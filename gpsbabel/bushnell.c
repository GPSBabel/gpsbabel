/*
    Read and write Bushnellfiles.

    Copyright (C) 2008  Robert Lipe (robertlipe@gpsbabel.org)

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
#define MYNAME "Bushnell"

static gbfile *file_in;
static gbfile *file_out;

static
arglist_t bushnell_args[] = {
  ARG_TERMINATOR
};


static void
rd_init(const char *fname) {
  file_in = gbfopen_le(fname, "rb", MYNAME);
}

static void
rd_deinit(void) {
  gbfclose(file_in);
}

static void
wr_init(const char *fname) {
  file_out = gbfopen_le(fname, "wb", MYNAME);
}

static void
wr_deinit(void) {
  gbfclose(file_out);
}

/*
 * Each file contains a single waypoint.
 */
static void
bushnell_read(void) {
  long lat_tmp,lon_tmp;
  waypoint *wpt_tmp = waypt_new();

  lat_tmp = gbfgetuint32(file_in);
  lon_tmp = gbfgetuint32(file_in);

  wpt_tmp->altitude = gbfgetuint16(file_in);
  wpt_tmp->latitude = lat_tmp / 10000000.0;
  wpt_tmp->longitude = lon_tmp / 10000000.0;

  // Apparently this is always zero terminated, though it's never been
  // observed to be longer than 19 bytes + a null terminator.
  wpt_tmp->shortname = xstrdup(gbfgetstr(file_in));

  waypt_add(wpt_tmp);
}

static void
bushnell_write_one(const waypoint *wpt) {
  char tbuf[22];

  gbfputint32(wpt->latitude  * 10000000, file_out);
  gbfputint32(wpt->longitude * 10000000, file_out);
  gbfputuint16(wpt->altitude, file_out);

  strncpy(tbuf, wpt->shortname, sizeof(tbuf));
  tbuf[sizeof(tbuf)-1] = 0;
  gbfwrite(tbuf, sizeof(tbuf), 1, file_out);
}

static void
bushnell_write(void) {
  waypt_disp_all(bushnell_write_one);
}

ff_vecs_t bushnell_vecs = {
  ff_type_file,
  FF_CAP_RW_WPT,
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
