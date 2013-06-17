/*
    Read and write Magellan Navigator Companion files.

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

#define MYNAME "Companion Waypoints"
#define MYTYPE  0x54777074  	/* Twpt */
#define MYCREATOR 0x4d47747a 	/* MGtz */

struct record {
  pdb_16 crt_sec; /* Big endian, creation time */
  pdb_16 crt_min;
  pdb_16 crt_hour;
  pdb_16 crt_mday;
  pdb_16 crt_mon; /* 1 = Jan */
  pdb_16 crt_year; /* includes century. */
  pdb_16 unknown;
  pdb_16 xx_sec; /* appears to be time, but we don't know what it is. */
  pdb_16 xx_min;
  pdb_16 xx_hour;
  pdb_16 xx_mday;
  pdb_16 xx_mon;
  pdb_16 xx_year;
  pdb_16 unknown2;
  pdb_32 latitude; /* lat * 1e5 */
  pdb_32 longitude; /* lon * 1e5 */
  pdb_32 elevation; /* meters */
  char plot; /* 1 = plot on map screen.   default = 0 */
  char unknown3; /* always 'a' */
};

static pdbfile* file_in;
static pdbfile* file_out;
static short_handle mkshort_handle;
static int ct;

static void
rd_init(const char* fname)
{
  file_in = pdb_open(fname, MYNAME);
}

static void
rd_deinit(void)
{
  pdb_close(file_in);
}

static void
wr_init(const char* fname)
{
  file_out = pdb_create(fname, MYNAME);
  mkshort_handle = mkshort_new_handle();
  setshort_length(mkshort_handle, 20);
  ct = 0;
}

static void
wr_deinit(void)
{
  pdb_close(file_out);
  mkshort_del_handle(&mkshort_handle);
}

static void
data_read(void)
{
  struct record* rec;
  pdbrec_t* pdb_rec;

  if ((file_in->creator != MYCREATOR) || (file_in->type != MYTYPE)) {
    fatal(MYNAME ": Not a Magellan Navigator file.\n");
  }

  for (pdb_rec = file_in->rec_list; pdb_rec; pdb_rec = pdb_rec->next) {
    waypoint* wpt_tmp;
    char* vdata;
    struct tm tm;

    memset(&tm, 0, sizeof(tm));
    wpt_tmp = waypt_new();
    rec = (struct record*) pdb_rec->data;
    wpt_tmp->altitude = be_read32(&rec->elevation);

    wpt_tmp->longitude = be_read32(&rec->longitude) / 1e5;
    wpt_tmp->latitude = be_read32(&rec->latitude) / 1e5;

    vdata = (char*) pdb_rec->data + sizeof(*rec);

    wpt_tmp->shortname = xstrdup(vdata);
    vdata += strlen(vdata) + 1;

    wpt_tmp->description = xstrdup(vdata);
    vdata += strlen(vdata) + 1;

    tm.tm_sec = be_read16(&rec->crt_sec);
    tm.tm_min = be_read16(&rec->crt_min);
    tm.tm_hour = be_read16(&rec->crt_hour);
    tm.tm_mday = be_read16(&rec->crt_mday);
    tm.tm_mon = be_read16(&rec->crt_mon) - 1;
    tm.tm_year = be_read16(&rec->crt_year) - 1900;
    if (mkgmtime(&tm) > 0) {
      wpt_tmp->SetCreationTime(mktime(&tm));
    }
    waypt_add(wpt_tmp);

  }
}


static void
my_writewpt(const waypoint* wpt)
{
  struct record* rec;
  char* vdata;
  const char* sn = global_opts.synthesize_shortnames ?
                   mkshort_from_wpt(mkshort_handle, wpt) :
                   wpt->shortname;

  rec = (struct record*) xcalloc(sizeof(*rec)+56,1);

  time_t tm_t;
  struct tm* tm;
  tm = NULL;
  if (wpt->creation_time) {
    const time_t tt = wpt->creation_time;
    tm = gmtime(&tt);
  }
  if (!tm) {
    tm_t = current_time();
    tm = gmtime(&tm_t);
  }

  be_write16(&rec->crt_sec, tm->tm_sec);
  be_write16(&rec->crt_min, tm->tm_min);
  be_write16(&rec->crt_hour, tm->tm_hour);
  be_write16(&rec->crt_mday, tm->tm_mday);
  be_write16(&rec->crt_mon, tm->tm_mon + 1);
  be_write16(&rec->crt_year, tm->tm_year + 1900);

  be_write16(&rec->unknown, 0);

  be_write16(&rec->xx_sec, tm->tm_sec);
  be_write16(&rec->xx_min, tm->tm_min);
  be_write16(&rec->xx_hour, tm->tm_hour);
  be_write16(&rec->xx_mday, tm->tm_mday);
  be_write16(&rec->xx_mon, tm->tm_mon + 1);
  be_write16(&rec->xx_year, tm->tm_year + 1900);

  be_write16(&rec->unknown2, 0);

  be_write32(&rec->longitude, si_round(wpt->longitude * 100000.0));
  be_write32(&rec->latitude, si_round(wpt->latitude * 100000.0));
  be_write32(&rec->elevation, (unsigned int)(wpt->altitude));

  rec->plot = 0;
  rec->unknown3 = 'a';

  vdata = (char*)rec + sizeof(*rec);
  if (sn) {
    strncpy(vdata, sn, 21);
    vdata[20] = '\0';
  } else {
    vdata[0] ='\0';
  }
  vdata += strlen(vdata) + 1;
  if (wpt->description) {
    strncpy(vdata, wpt->description, 33);
    vdata[32] = '\0';
  } else {
    vdata[0] = '\0';
  }
  vdata += strlen(vdata) + 1;
  vdata[0] = '\0';
  vdata[1] = '\0';
  vdata += 2;

  pdb_write_rec(file_out, 0, 0, ct++, rec, (char*)vdata - (char*)rec);

  xfree(rec);
}

static void
data_write(void)
{
  static const char* appinfo =
    "\0\x01"
    "User\0\0\0\0\0\0\0\0\0\0\0\0"
    "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
    "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
    "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
    "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
    "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
    "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
    "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
    "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
    "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
    "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
    "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
    "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
    "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
    "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
    "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
    "\0\x01\x02\x03\x04\x05\x06\x07\x08"
    "\x09\x0a\x0b\x0c\x0d\x0e\x0f\0\0";

  strncpy(file_out->name, "Companion Waypoints", PDB_DBNAMELEN);
  file_out->attr = PDB_FLAG_BACKUP;
  file_out->ctime = file_out->mtime = current_time() + 2082844800U;
  file_out->type = MYTYPE;  /* CWpt */
  file_out->creator = MYCREATOR; /* cGPS */
  file_out->version = 1;
  file_out->appinfo = (void*)appinfo;
  file_out->appinfo_len = 276;

  waypt_disp_all(my_writewpt);
}


ff_vecs_t magnav_vec = {
  ff_type_file,
  FF_CAP_RW_WPT,
  rd_init,
  wr_init,
  rd_deinit,
  wr_deinit,
  data_read,
  data_write,
  NULL,
  NULL,
  CET_CHARSET_ASCII, 0	/* CET-REVIEW */
};
#endif
