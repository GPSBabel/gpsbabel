/*
    Read and write GPSPilot Tracker files.

    Copyright (C) 2002 Robert Lipe, robertlipe@usa.net

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
#include "pdbfile.h"

#define MYNAME "GPSPilot"
#define MYTYPE_POINTS 0x706f696e  	/* poin */
#define MYTYPE_AIRPORT 0x706f3030  	/* po00 */
#define MYTYPE_CITIES  0x706f3031  	/* po01 */
#define MYTYPE_LNDMRKS 0x706f3032  	/* po02 */
#define MYTYPE_NAVAIDS 0x706f3033  	/* po03 */
#define MYCREATOR 0x47704c69 	/* GpLi */

struct record {
  pdb_32 longitude; 	/* Big endian, long * 3.6e6 */
  pdb_32 latitude; 	/* similarly */
  pdb_16 elevation; 	/* meters */
  pdb_16 magvar; 		/* magnetic variation in degrees, neg = west */
};

struct runways {
  pdb_32 be_longitude; 	/* Big endian, long * 3.6e6 */
  pdb_32 be_latitude; 	/* similarly */
  pdb_32 en_longitude; 	/* Big endian, long * 3.6e6 */
  pdb_32 en_latitude; 	/* similarly */
};

static pdbfile *file_in, *file_out;
static const char *out_fname;
static char *dbname = NULL;
static int ct;

static
arglist_t gpspilot_args[] = {
  {"dbname", &dbname, "Database name", NULL, ARGTYPE_STRING, ARG_NOMINMAX},
  ARG_TERMINATOR
};

static void
rd_init(const char *fname)
{
  file_in = pdb_open(fname, MYNAME);
}

static void
rd_deinit(void)
{
  pdb_close(file_in);
  if (dbname) {
    xfree(dbname);
    dbname = NULL;
  }
}

static void
wr_init(const char *fname)
{
  file_out = pdb_create(fname, MYNAME);
  out_fname = fname;
  ct = 0;
}

static void
wr_deinit(void)
{
  pdb_close(file_out);
  if (dbname) {
    xfree(dbname);
    dbname = NULL;
  }
}

static void
data_read(void)
{
  struct record *rec;
  pdbrec_t *pdb_rec;

  if ((file_in->creator != MYCREATOR)) {
    fatal(MYNAME ": Not a gpspilot file.\n");
  }

  switch (file_in->type) {
  case MYTYPE_AIRPORT:
  case MYTYPE_POINTS:
  case MYTYPE_CITIES:
  case MYTYPE_LNDMRKS:
  case MYTYPE_NAVAIDS:
    break;
  default:
    fatal(MYNAME ": Not a gpspilot file.\n");
  }

  for (pdb_rec = file_in->rec_list; pdb_rec; pdb_rec = pdb_rec->next) {
    waypoint *wpt_tmp;
    char *vdata;

    wpt_tmp = waypt_new();

    rec = (struct record *) pdb_rec->data;
    wpt_tmp->longitude = be_read32(&rec->longitude) / 3.6e6;
    wpt_tmp->latitude = be_read32(&rec->latitude) / 3.6e6;
    wpt_tmp->altitude =
      be_read16(&rec->elevation);

    vdata = (char *) pdb_rec->data + sizeof(*rec);

    /*
    * skip runway records if an airport.
    */
    if (pdb_rec->category == 0) {
      int numRunways;
      numRunways = be_read16(vdata);
      vdata += 2;
      vdata += (sizeof(struct runways) * numRunways);
    }

    /*
     * This maping is a bit contrived.
     * Name is up to 36. ID is up to 9.
     * Since 'ID' maps more clearly to "shortname" (and this
     * more likely to be resemble  a wayoint name in another
     * receiver) we use that for shortname and use 'name' as
     * our description.
     */
    wpt_tmp->description = xstrdup(vdata);
    vdata = vdata + strlen(vdata) + 1;

    wpt_tmp->shortname = xstrdup(vdata);
    vdata = vdata + strlen(vdata) + 1;

    wpt_tmp->notes = xstrdup(vdata);

    waypt_add(wpt_tmp);

  }
}


static void
gpspilot_writewpt(const waypoint *wpt)
{
  struct record *rec;
  char *vdata;

  rec = (struct record*) xcalloc(sizeof(*rec)+206,1);

  be_write32(&rec->longitude, si_round(wpt->longitude * 3.6e6));
  be_write32(&rec->latitude, si_round(wpt->latitude * 3.6e6));
  be_write16(&rec->elevation, si_round(wpt->altitude));
  be_write16(&rec->magvar, 0);

  vdata = (char *)rec + sizeof(*rec);
  if (wpt->description) {
    strncpy(vdata, wpt->description, 36);
    vdata[35] = '\0';
  } else {
    vdata[0] ='\0';
  }
  vdata += strlen(vdata) + 1;
  if (wpt->shortname) {
    strncpy(vdata, wpt->shortname, 9);
    vdata[8] = '\0';
  } else {
    vdata[0] ='\0';
  }
  vdata += strlen(vdata) + 1;

  if (wpt->notes) {
    strncpy(vdata, wpt->notes, 161);
    vdata[160] = '\0';
  } else {
    vdata[0] ='\0';
  }
  vdata += strlen(vdata) + 1;

  pdb_write_rec(file_out, 0, 2, ct++, (void *)rec, (char *)vdata - (char *)rec);

  xfree(rec);
}

static void
data_write(void)
{
  if (dbname) {
    strncpy(file_out->name, dbname, PDB_DBNAMELEN);
  } else {
    strncpy(file_out->name, out_fname, PDB_DBNAMELEN);
  }
  file_out->name[PDB_DBNAMELEN-1] = 0;
  file_out->attr = PDB_FLAG_BACKUP;
  file_out->ctime = file_out->mtime = current_time() + 2082844800U;
  file_out->type = MYTYPE_POINTS;
  file_out->creator = MYCREATOR;
  file_out->version = 0;

  waypt_disp_all(gpspilot_writewpt);
}


ff_vecs_t gpspilot_vecs = {
  ff_type_file,
  FF_CAP_RW_WPT,
  rd_init,
  wr_init,
  rd_deinit,
  wr_deinit,
  data_read,
  data_write,
  NULL,
  gpspilot_args,
  CET_CHARSET_ASCII, 0	/* CET-REVIEW */
};
#endif
