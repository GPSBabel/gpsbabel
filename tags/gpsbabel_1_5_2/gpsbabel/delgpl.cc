/*
    DeLorme GPL Track Format.

    Copyright (C) 2003, 2009 Robert Lipe, robertlipe+source@gpsbabel.org

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

#define MYNAME "GPL"

typedef struct gpl_point {
  unsigned int status;
  unsigned int dummy1;
  double lat;
  double lon;
  double alt; /* in feet */
  double heading;
  double speed; /* mph */
  unsigned int tm;
  unsigned int dummy3;
} gpl_point_t;

static gbfile* gplfile_in;
static gbfile* gplfile_out;

static void
gpl_rd_init(const char* fname)
{
  gplfile_in = gbfopen_le(fname, "rb", MYNAME);
  if (sizeof(struct gpl_point) != 56) {
    fatal(MYNAME ": gpl_point is %lu instead of 56.\n",
          (unsigned long) sizeof(struct gpl_point));
  }
}

static void
gpl_read(void)
{
  Waypoint* wpt_tmp;
  route_head* track_head;
  gpl_point_t gp;
  double alt_feet;

  track_head = route_head_alloc();
  track_add_head(track_head);

  while (gbfread(&gp, sizeof(gp), 1, gplfile_in) > 0) {
    wpt_tmp = new Waypoint;
    wpt_tmp->latitude = le_read_double(&gp.lat);
    wpt_tmp->longitude = le_read_double(&gp.lon);
    alt_feet = le_read_double(&gp.alt);
    wpt_tmp->altitude = FEET_TO_METERS(alt_feet);
    if (wpt_tmp->altitude <= unknown_alt + 1) {
      wpt_tmp->altitude = unknown_alt;
    }
    wpt_tmp->SetCreationTime(le_read32(&gp.tm));

    switch (le_read32(&gp.status)) {
    case 1:
      wpt_tmp->fix = fix_none;
      break;
    case 2:
      wpt_tmp->fix = fix_2d;
      break;
    case 3:
      wpt_tmp->fix = fix_3d;
      break;
    case 5:
      wpt_tmp->fix = fix_dgps;
      break;
    }

    WAYPT_SET(wpt_tmp, course, le_read_double(&gp.heading));
    WAYPT_SET(wpt_tmp, speed, le_read_double(&gp.speed));
    WAYPT_SET(wpt_tmp, speed, MILES_TO_METERS(wpt_tmp->speed)/3600);
    // 2008 and 2009 seem to throw track points in that go back
    // in time.  The only thing I see "special" about those
    // trackpoints is that these fields are zeroed.  Toss them.
    if ((wpt_tmp->speed == 0.0) && (wpt_tmp->course == 0.0)) {
      delete wpt_tmp;
      continue;
    }
    track_add_wpt(track_head, wpt_tmp);
  }
}


static void
gpl_rd_deinit(void)
{
  gbfclose(gplfile_in);
}

static void
gpl_wr_init(const char* fname)
{
  gplfile_out = gbfopen_le(fname, "wb", MYNAME);
}

static void
gpl_wr_deinit(void)
{
  gbfclose(gplfile_out);
}

static void
gpl_trackpt(const Waypoint* wpt)
{
  double alt_feet = METERS_TO_FEET(wpt->altitude);
  int status = 3;
  gpl_point_t gp;
  double speed = 3600*METERS_TO_MILES(wpt->speed);
  double heading = wpt->course;

  switch (wpt->fix) {
  case fix_none:
    status = 1;
    break;
  case fix_2d:
    status = 2;
    break;
  case fix_3d:
    status = 3;
    break;
  case fix_dgps:
    status = 5;
    break;
  default:
    status = 3;   // a strategic lie for fix_unknown.
  }

  memset(&gp, 0, sizeof(gp));
  le_write32(&gp.status, status);
  le_write_double(&gp.lat, wpt->latitude);
  le_write_double(&gp.lon, wpt->longitude);
  le_write_double(&gp.alt, alt_feet);
  le_write_double(&gp.speed, speed);
  le_write_double(&gp.heading, heading);
  le_write32(&gp.tm, wpt->GetCreationTime().toTime_t());

  gbfwrite(&gp, sizeof(gp), 1, gplfile_out);
}

static void
gpl_write(void)
{
  track_disp_all(NULL, NULL, gpl_trackpt);
}

ff_vecs_t gpl_vecs = {
  ff_type_file,
  { ff_cap_none, (ff_cap)(ff_cap_read | ff_cap_write), ff_cap_none },
  gpl_rd_init,
  gpl_wr_init,
  gpl_rd_deinit,
  gpl_wr_deinit,
  gpl_read,
  gpl_write,
  NULL,
  NULL,
  CET_CHARSET_UTF8, 1	/* there is no need to convert anything | CET-REVIEW */
};
