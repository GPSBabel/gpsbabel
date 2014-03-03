/*

    Minimum support for Palm/OS database files
    Copyright (C) 2007 Olaf Klein, o.b.klein@gpsbabel.org

    Written after study the Coldsync project

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111 USA

 */

#include "defs.h"

#if PDBFMTS_ENABLED

#include "gbfile.h"
#include "pdbfile.h"
#include <stdio.h>
#include <string.h>


#define MYNAME "pdbfile"

static void
pdb_invalid_file(const pdbfile *pdb_in, const char *fmt, ...)
{
  char buff[128];
  va_list args;

  va_start(args, fmt);
  vsnprintf(buff, sizeof(buff), fmt, args);
  buff[sizeof(buff)-1] = '0';

  warning(MYNAME ": %s\n", buff);
  fatal(MYNAME ": Invalid or unsupported file (%s).\n", pdb_in->file->name);
}

/* try to read to EOF (avoid determining file-size) */

static void *
pdb_read_tail(gbfile *fin, int32_t *size)
{
  int count;
  char buff[256];
  char *res = NULL;
  int bytes = 0;

  while ((count = gbfread(buff, 1, sizeof(buff), fin))) {

    if (!res) {
      res = (char *) xmalloc(count);
      memcpy(res, buff, count);
    } else {
      res = (char*) xrealloc(res, bytes + count);
      memcpy(&res[bytes], buff, count);
    }
    bytes += count;
  }
  if (res) {
    res = (char*) xrealloc(res, bytes + 1);
  } else {
    res = (char*) xmalloc(1);
  }
  res[bytes] = '\0';

  if (size) {
    *size = bytes;
  }
  return (void *)res;
}

static void
pdb_load_data(pdbfile *fin)
{
  uint16_t i, ct;
  pdbrec_t *last_rec;
  uint32_t offs;
  pdbrec_t *rec;

  /* load the header */
  gbfread(fin->name, 1, PDB_DBNAMELEN, fin->file);
  fin->name[PDB_DBNAMELEN] = '\0';

  fin->attr = gbfgetuint16(fin->file);
  fin->version = gbfgetuint16(fin->file);
  fin->ctime = gbfgetuint32(fin->file);
  fin->mtime = gbfgetuint32(fin->file);
  fin->btime = gbfgetuint32(fin->file);
  fin->revision = gbfgetuint32(fin->file);
  fin->appinfo_offs = gbfgetint32(fin->file);
  fin->index_offs = gbfgetuint32(fin->file);
  fin->type = gbfgetuint32(fin->file);
  fin->creator = gbfgetuint32(fin->file);
  fin->uid = gbfgetuint32(fin->file);

  if (fin->appinfo_offs < 0) {
    pdb_invalid_file(fin, "Invalid application data offset (%0xh)", fin->appinfo_offs);
  }
  if (fin->index_offs < 0) {
    pdb_invalid_file(fin, "Invalid index offset (%0xh)", fin->index_offs);
  }

#if 0
  fprintf(stderr, "%s: dbname   \"%s\"\n", MYNAME, fin->name);
  fprintf(stderr, "%s: attr      %-8x\n", MYNAME, fin->attr);
  fprintf(stderr, "%s: creator   %-8x\n", MYNAME, fin->creator);
  fprintf(stderr, "%s: type      %-8x\n", MYNAME, fin->type);
  fprintf(stderr, "%s: ver       %-8u\n", MYNAME, fin->version);
  fprintf(stderr, "%s: app-ofs   %-8u\n", MYNAME, fin->appinfo_offs);
  fprintf(stderr, "%s: index-ofs %-8u\n", MYNAME, fin->index_offs);
#endif
  /* ID = */ (void) gbfgetuint32(fin->file);
  ct = fin->rec_ct = gbfgetint16(fin->file);
  if (ct >= 0x7FFF) {
    warning(MYNAME ": Probably invalid number of records (%0d)\n", fin->rec_ct);
  }

  offs = 78;

  last_rec = NULL;
  for (i = 0; i < ct; i++) {
    pdbrec_t *rec;

    rec = (pdbrec_t*) xcalloc(1, sizeof(*rec));
    if (fin->attr & PDB_FLAG_RESOURCE) {
      (void) gbfgetuint32(fin->file);	/* type */
      rec->id = gbfgetint16(fin->file);
      rec->offs = gbfgetuint32(fin->file);
      if ((int32_t)rec->offs < 0) {
        pdb_invalid_file(fin, "Invalid offset to record (%0d, id = %d)", rec->offs, rec->id);
      }
    } else {
      uint32_t x;

      rec->offs = gbfgetint32(fin->file);
      x = gbfgetuint32(fin->file);
      rec->id = x & 0x0ffff;
      rec->category = (x >> 24) & 0x0f;
      rec->flags = (x >> 24) & 0xf0;
      if ((int32_t)rec->offs < 0) {
        pdb_invalid_file(fin, "Invalid offset to resource record (%0d, id = %d)", rec->offs, rec->id);
      }
    }

    if (last_rec == NULL) {
      fin->rec_list = rec;
    } else {
      last_rec->next = rec;
    }
    last_rec = rec;
  }

  offs += (ct * 8);
  last_rec = fin->rec_list;

  if (fin->appinfo_offs != 0) {
    uint32_t top;

    /* seek to application info offset */
    while (offs < fin->appinfo_offs) {
      (void)gbfgetc(fin->file);
      offs++;
    }

    /* determine the length of application info */
    if (fin->index_offs != 0) {
      top = fin->index_offs;
    } else {
      top = 0x7FFFFFFU;
    }
    if (last_rec && (last_rec->offs < top)) {
      top = last_rec->offs;
    }

    if (top != 0x7FFFFFFU) {
      fin->appinfo = xmalloc(top - offs);
      fin->appinfo_len = gbfread(fin->appinfo, 1, top - offs, fin->file);
      offs += fin->appinfo_len;
    } else {
      int32_t size;
      fin->appinfo = pdb_read_tail(fin->file, &size);
      fin->appinfo_len = size;
      offs += size;
    }
  }

  for (rec = fin->rec_list; rec; rec = rec->next) {
    /* seek to current record */
    while (offs < rec->offs) {
      (void) gbfgetc(fin->file);
      offs++;
    }
    if (rec->next) {
      rec->size = (int32_t)rec->next->offs - (int32_t)offs;
      if (rec->size > 0) {
        rec->data = (char*) xmalloc(rec->size);
        rec->size = gbfread(rec->data, 1, rec->size, fin->file);
        offs += rec->size;
      } else if (rec->size < 0) {
        pdb_invalid_file(fin, "Wrong data size in record with id %d.\n", rec->id);
      }
    } else {
      rec->data = (char*) pdb_read_tail(fin->file, &rec->size);
      offs += rec->size;
    }
  }
}

pdbfile *
pdb_open(const char *filename, const char *module)
{
  pdbfile *res;

  res = (pdbfile*) xcalloc(1, sizeof(*res));
  res->file = gbfopen_be(filename, "rb", module);
  res->mode = 1;

  pdb_load_data(res);
  pdb_rewind(res);

  return res;
}

int
pdb_read_rec_by_id(pdbfile *fin, const uint32_t rec_id, uint8_t *flags, uint8_t *category, void **data)
{
  pdbrec_t *rec;

  for (rec = fin->rec_list; rec; rec = rec->next) {
    if (rec->id == rec_id) {
      if (data) {
        *data = rec->data;
      }
      if (flags) {
        *flags = rec->flags;
      }
      if (category) {
        *category = rec->category;
      }
      return rec->size;
    }
  }
  return -1;
}

pdbfile *
pdb_create(const char *filename, const char *module)
{
  pdbfile *res;

  res = (pdbfile*) xcalloc(1, sizeof(*res));
  strncpy(res->name, "Palm/OS Database", PDB_DBNAMELEN);
  res->file = gbfopen_be(filename, "wb", module);;
  res->mode = 2;

  return res;
}

void
pdb_write_rec(pdbfile *fout, const uint8_t flags, const uint8_t category, const uint32_t rec_id, const void *data, const uint32_t size)
{
  pdbrec_t *rec, *cur;

  rec = (pdbrec_t*) xcalloc(1, sizeof(*rec));
  rec->category = category;
  rec->flags = category;
  rec->id = rec_id;
  rec->size = size;
  if (size > 0) {
    rec->data = (char*) xmalloc(size);
    memcpy(rec->data, data, size);
  }

  /* insert rec into rec_list sorted by id */
  cur = fout->rec_list;
  if (cur == NULL) {
    fout->rec_list = rec;
  } else {
    pdbrec_t *prev = NULL;

    while (cur) {
      if (rec_id < cur->id) {
        rec->next = cur;
        if (prev == NULL) {
          fout->rec_list = rec;
        } else {
          prev->next = rec;
        }
        break;
      } else if (rec_id == cur->id) {	/* Overwrite record with id ... */
        rec->next = cur->next;
        if (prev == NULL) {
          fout->rec_list = rec;
        } else {
          prev->next = rec;
        }
        if (cur->data) {
          xfree(cur->data);
        }
        xfree(cur);
        cur = rec;
        break;
      }
      prev = cur;
      cur = cur->next;
    }
    if (! cur) {
      if (prev == NULL) {
        fout->rec_list = rec;
      } else {
        prev->next = rec;
      }
    }
  }
  fout->rec_ct++;
}

/* all data was buffered, write now to file */

static void
pdb_flush(pdbfile *file)
{
  pdbrec_t *rec;
  gbfile *fout = file->file;
  int len, offs;

  offs = 78;
  file->index_offs = 0;
  offs += (file->rec_ct * 8);

  offs += 2;

  if (file->appinfo && (file->appinfo_len > 0)) {
    file->appinfo_offs = offs;
    offs += file->appinfo_len;
  } else {
    file->appinfo_offs = 0;
  }

  rec = file->rec_list;
  while (rec) {			/* prepare data records */
    rec->offs = offs;
    offs += rec->size;
    rec = rec->next;
  }

  len = strlen(file->name);
  gbfwrite(file->name, 1, len, fout);
  while (len++ < PDB_DBNAMELEN) {
    gbfputc(0, fout);
  }

  gbfputuint16(file->attr, fout);
  gbfputuint16(file->version, fout);
  gbfputuint32(file->ctime, fout);
  gbfputuint32(file->mtime, fout);
  gbfputuint32(file->btime, fout);
  gbfputuint32(file->revision, fout);
  gbfputuint32(file->appinfo_offs, fout);
  gbfputuint32(file->index_offs, fout);
  gbfputuint32(file->type, fout);
  gbfputuint32(file->creator, fout);
  gbfputuint32(file->uid, fout);

  gbfputuint32(0, fout); /* ? ID ? */
  gbfputuint16(file->rec_ct, fout);

  for (rec = file->rec_list; rec; rec = rec->next) {
    uint32_t attr;

    gbfputint32(rec->offs, fout);
    attr = (rec->category & 0x0f) | (rec->flags & 0xf0);
    gbfputint32((rec->id & 0x0ffffff) | (attr << 24), fout);
  }
  gbfputint16(0, fout);

  if (file->appinfo && (file->appinfo_len > 0)) {
    gbfwrite(file->appinfo, 1, file->appinfo_len, fout);
  }

  for (rec = file->rec_list; rec; rec = rec->next) {
    if (rec->size > 0) {
      gbfwrite(rec->data, 1, rec->size, fout);
    }
  }
}

void
pdb_close(pdbfile *file)
{
  pdbrec_t *rec;

  if (! file) {
    return;
  }

  if (file->mode & 2) {
#if 0
    /* this can be done later */
    if (gpsbabel_time == 0) {	/*     !!! We are in testo !!!       */
      file->ctime = 0;	/* (now we also can do a bincompare) */
      file->mtime = 0;
      file->btime = 0;
    }
#endif
    pdb_flush(file);
  }

  gbfclose(file->file);

  if ((file->mode & 1) && file->appinfo) {
    xfree(file->appinfo);
  }

  rec = file->rec_list;
  while (rec) {
    pdbrec_t *tmp = rec;
    rec = rec->next;

    if (tmp->data) {
      xfree(tmp->data);
    }
    xfree(tmp);
  }
  xfree(file);
}

int
pdb_eof(pdbfile *fin)
{
  return (fin->rec_curr) ? 0 : 1;
}

int
pdb_read_rec(pdbfile *fin, uint8_t *flags, uint8_t *category, uint32_t *rec_id, void **data)
{
  if (pdb_eof(fin)) {
    return -1;
  } else {
    pdbrec_t *rec = fin->rec_curr;
    fin->rec_curr = rec->next;

    if (data) {
      *data = rec->data;
    }
    if (flags) {
      *flags = rec->flags;
    }
    if (category) {
      *category = rec->category;
    }
    if (rec_id) {
      *rec_id = rec->id;
    }

    return rec->size;
  }
}

void
pdb_rewind(pdbfile *fin)
{
  fin->rec_curr = fin->rec_list;
}

#endif
