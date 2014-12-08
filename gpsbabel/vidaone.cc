/*

    Support for VidaOne GPS for Pocket PC (.gpb) files

    Copyright (C) 2008 Olaf Klein, o.b.klein@gpsbabel.org

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

/*
   Simple layout:

struct
{
	double dLatitude
	double dLongitude
	float fReserved
};
*/

#include "defs.h"
#include <stdlib.h>
#include <math.h>

#define MYNAME "vidaone"

#define VIDAONE_VER	"ver"

static char* vidaone_opt_ver;
static int vidaone_ver;

static
arglist_t vidaone_args[] = {
  {
    VIDAONE_VER, &vidaone_opt_ver,
    "Version of VidaOne file to read or write (1 or 2)",
    "1", ARGTYPE_INT, "1", "2"
  },
  ARG_TERMINATOR
};

static gbfile* fin, *fout;

/*******************************************************************************
* %%%        global callbacks called by gpsbabel main process              %%% *
*******************************************************************************/

static void
vidaone_rd_init(const char* fname)
{
  vidaone_ver = atoi(vidaone_opt_ver);
  fin = gbfopen(fname, "rb", MYNAME);
}

static void
vidaone_rd_deinit(void)
{
  gbfclose(fin);
}

static void
vidaone_read(void)
{
  route_head* trk = NULL;

  while (! gbfeof(fin)) {
    Waypoint* wpt = new Waypoint;

    wpt->latitude = gbfgetdbl(fin);
    wpt->longitude = gbfgetdbl(fin);
    if (vidaone_ver >= 2) {
      wpt->altitude = gbfgetflt(fin);
    }
    (void) gbfgetflt(fin);

    /* Only one basic check of data integrity */
    if ((fabs(wpt->latitude) > 90) || (fabs(wpt->longitude) > 180)) {
      fatal(MYNAME ": Latitude and/or longitude out of range.\n");
    }

    if (!trk) {
      trk = route_head_alloc();
      track_add_head(trk);
    }

    track_add_wpt(trk, wpt);
  }
}

static void
vidaone_wr_init(const char* fname)
{
  vidaone_ver = atoi(vidaone_opt_ver);
  fout = gbfopen(fname, "wb", MYNAME);
}

static void
vidaone_wr_deinit(void)
{
  gbfclose(fout);
}

static void
vidaone_trkpt(const Waypoint* wpt)
{
  gbfputdbl(wpt->latitude, fout);
  gbfputdbl(wpt->longitude, fout);
  if (vidaone_ver >= 2) {
    gbfputflt(wpt->altitude, fout);
  }
  gbfputflt(0, fout);
}

static void
vidaone_write(void)
{
  track_disp_all(NULL, NULL, vidaone_trkpt);
}

/**************************************************************************/

ff_vecs_t vidaone_vecs = {
  ff_type_file,
  {
    ff_cap_none 			/* waypoints */,
    (ff_cap)(ff_cap_read | ff_cap_write)	/* tracks */,
    ff_cap_none 			/* routes */
  },
  vidaone_rd_init,
  vidaone_wr_init,
  vidaone_rd_deinit,
  vidaone_wr_deinit,
  vidaone_read,
  vidaone_write,
  NULL,
  vidaone_args,
  CET_CHARSET_UTF8, 1
};

/**************************************************************************/
