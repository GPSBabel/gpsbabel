/*
    Read and write Bushnell trail *.trl files.

    Copyright (C) 2009  Robert Lipe (robertlipe+source@gpsbabel.org)

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
#define MYNAME "Bushnell Trail"

static gbfile* file_in;
static gbfile* file_out;
static int trkpt_count;
static route_head* trk_head;

static
arglist_t bushnell_args[] = {
  ARG_TERMINATOR
};

static void
rd_init(const QString& fname)
{
  char h[0x14]; // Believed to be zero terminated.
  file_in = gbfopen_le(fname, "rb", MYNAME);
  gbfread(h, 1, sizeof(h), file_in);

  trk_head = route_head_alloc();
  track_add_head(trk_head);

  trk_head->rte_name = lrtrim(h);
}

static void
rd_deinit()
{
  gbfclose(file_in);
}

static void
wr_init(const QString& fname)
{
  int l = fname.length();
  char obuf[20] = { 0 } ;
  char* p = obuf;
  file_out = gbfopen_le(fname, "w", MYNAME);
  trkpt_count = 0;
  QString fnameu = fname.toUpper();
  for (int i = 0; (i < l) && (i < 20); i++) {
    char c = fnameu[i].toLatin1();
    if (isalnum(c)) {
      *p++ = c;
    }
    if (c == '.') {
      break;
    }
  }
  gbfwrite(&obuf, 1, 20, file_out);
}

static void
wr_deinit()
{
  int i = trkpt_count;
  while (i < 4502) {
    gbfputint32(0, file_out);
    gbfputint32(0, file_out);
    i++;
  }
  gbfputint32(0, file_out);
  gbfputint16(trkpt_count, file_out);
  gbfputc(1, file_out);

  gbfclose(file_out);
}

/*
 * Each file contains a single waypoint.
 */
static void
bushnell_read()
{
  while (true) {
    int lat_tmp = gbfgetint32(file_in);
    int lon_tmp = gbfgetint32(file_in);

    if (!lat_tmp && !lon_tmp) {
      break;
    }

    Waypoint* wpt_tmp = new Waypoint;
    wpt_tmp->latitude  = lat_tmp / 10000000.0;
    wpt_tmp->longitude = lon_tmp / 10000000.0;

    track_add_wpt(trk_head, wpt_tmp);
  }
}

static void
bushnell_write_one(const Waypoint* wpt)
{
  int32_t lat = wpt->latitude  * 10000000.0;
  int32_t lon = wpt->longitude * 10000000.0;
  trkpt_count++;
  if (trkpt_count > 4502) {
    fatal(MYNAME " too many trackpoints.  Max is 4502.");
  }

  gbfputint32(lat, file_out);
  gbfputint32(lon, file_out);
}

static void
bushnell_write()
{
  track_disp_all(nullptr, nullptr, bushnell_write_one);
}

ff_vecs_t bushnell_trl_vecs = {
  ff_type_file,
  { ff_cap_none, (ff_cap)(ff_cap_read | ff_cap_write), ff_cap_none },
  rd_init,
  wr_init,
  rd_deinit,
  wr_deinit,
  bushnell_read,
  bushnell_write,
  nullptr,
  bushnell_args,
  CET_CHARSET_MS_ANSI, 0,  /* Not really sure... */
  NULL_POS_OPS,
  nullptr
};
