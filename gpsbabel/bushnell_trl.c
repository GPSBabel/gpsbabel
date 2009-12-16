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
  gbfclose(file_in);
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


ff_vecs_t bushnell_trl_vecs = {
  ff_type_file,
  { ff_cap_none, ff_cap_read, ff_cap_none },
  rd_init,
  NULL,
  rd_deinit,
  NULL,
  bushnell_read,
  NULL,
  NULL,
  bushnell_args,
  CET_CHARSET_MS_ANSI, 0  /* Not really sure... */
};
