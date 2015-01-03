/*

    Support for Navitel binary tracks (.bin),
    copyright (C) 2008 Olaf.Klein@gpsbabel.org.

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
#include "gbfile.h"
#include "jeeps/gpsmath.h"

#define MYNAME "navitel"

static gbfile* fin, *fout;
static char new_track;
static int trkpts;

/*******************************************************************************
* %%%        global callbacks called by gpsbabel main process              %%% *
*******************************************************************************/

static void
navitel_rd_init(const char* fname)
{
  fin = gbfopen(fname, "rb", MYNAME);
}

static void
navitel_rd_deinit(void)
{
  gbfclose(fin);
}

static void
navitel_read_track(void)
{
  int points, i;
  route_head* trk = NULL;

  points = gbfgetint32(fin);
  (void) gbfgetint32(fin); /* unknown */

  for (i = 0; i < points; i++) {
    int lat, lon;
    Waypoint* wpt;

    lon = gbfgetint32(fin);
    lat = gbfgetint32(fin);

    wpt = new Waypoint;
    wpt->latitude = GPS_Math_Semi_To_Deg(lat & 0x7FFFFFFF);
    wpt->longitude = GPS_Math_Semi_To_Deg(lon);

    if ((lat >> 31) || (trk == NULL)) {
      trk = route_head_alloc();
      track_add_head(trk);
    }
    track_add_wpt(trk, wpt);
  }
}

static void
navitel_wr_init(const char* fname)
{
  fout = gbfopen(fname, "wb", MYNAME);
}

static void
navitel_wr_deinit(void)
{
  gbfclose(fout);
}

static void
navitel_enum_trkpts(const Waypoint* wpt)
{
  trkpts++;
}

static void
navitel_disp_trk_head(const route_head* trk)
{
  new_track = 1;
}

static void
navitel_disp_trkpts(const Waypoint* wpt)
{
  int lat, lon;

  lat = GPS_Math_Deg_To_Semi(wpt->latitude);
  lon = GPS_Math_Deg_To_Semi(wpt->longitude);

  if (new_track) {
    lat |= (1 << 31);
    new_track = 0;
  }

  gbfputint32(lon, fout);
  gbfputint32(lat, fout);
}

static void
navitel_write_track(void)
{
  trkpts = 0;
  track_disp_all(NULL, NULL, navitel_enum_trkpts);
  if (trkpts > 10000) {
    trkpts = 10000;
    warning(MYNAME ": Can store only 10000 points per file!\n");
  }

  gbfputint32(trkpts, fout);
  gbfputint32(1, fout);		/* ? */
  track_disp_all(navitel_disp_trk_head, NULL, navitel_disp_trkpts);
}

/**************************************************************************/

ff_vecs_t navitel_trk_vecs = {
  ff_type_file,
  {
    ff_cap_none			/* waypoints */,
    (ff_cap)(ff_cap_read | ff_cap_write)	/* tracks */,
    ff_cap_none			/* routes */
  },
  navitel_rd_init,
  navitel_wr_init,
  navitel_rd_deinit,
  navitel_wr_deinit,
  navitel_read_track,
  navitel_write_track,
  NULL,
  NULL,
  CET_CHARSET_UTF8, 1			/* Nothing to convert */
};

/**************************************************************************/
