/*
    Read and write Bushnell trail *.trl files.

    Copyright (C) 2009  Robert Lipe (robertlipe@gpsbabel.org)

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

static gbfile *file_in;
static gbfile *file_out;
static int trkpt_count;
static route_head* trk_head;

static
arglist_t bushnell_args[] = {
  ARG_TERMINATOR
};

static void
rd_init(const char *fname) {
  char h[0x14]; // Believed to be zero terminated.
  file_in = gbfopen_le(fname, "rb", MYNAME);
  gbfread(h, 1, sizeof(h), file_in);

  trk_head = route_head_alloc();
  track_add_head(trk_head);

  trk_head->rte_name = xstrdup(lrtrim(h));
}

static void
rd_deinit(void) {
  gbfclose(file_out);
}

static void
wr_init(const char *fname) {
  file_out = gbfopen_le(fname, "w", MYNAME);
  trkpt_count = 0;
  static char obuf[20] = {"TL003"};
  gbfwrite(&obuf, 1, 20, file_out);
}

static void
wr_deinit(void) {
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
bushnell_read(void) {
  int lat_tmp,lon_tmp;

  while (1) {
    waypoint *wpt_tmp;

    lat_tmp = gbfgetint32(file_in);
    lon_tmp = gbfgetint32(file_in);

    if (!lat_tmp && !lon_tmp)
      break;

    wpt_tmp = waypt_new();
    wpt_tmp->latitude  = lat_tmp / 10000000.0;
    wpt_tmp->longitude = lon_tmp / 10000000.0;

    track_add_wpt(trk_head, wpt_tmp);
  }
}

static void
bushnell_write_one(const waypoint *wpt) {
  trkpt_count++;
  if (trkpt_count > 4502)
    fatal(MYNAME " too many trackpoints.  Max is 4502.");

  gbint32 lat = wpt->latitude  * 10000000.0;
  gbint32 lon = wpt->longitude * 10000000.0;
  gbfputint32(lat, file_out);
  gbfputint32(lon, file_out);
}

static void
bushnell_write(void) {
  track_disp_all(NULL, NULL, bushnell_write_one);
}

ff_vecs_t bushnell_trl_vecs = {
  ff_type_file,
  { ff_cap_none, ff_cap_read | ff_cap_write, ff_cap_none },
  rd_init,
  wr_init,
  rd_deinit,
  wr_deinit,
  bushnell_read,
  bushnell_write,
  NULL,
  bushnell_args,
  CET_CHARSET_MS_ANSI, 0  /* Not really sure... */
};
