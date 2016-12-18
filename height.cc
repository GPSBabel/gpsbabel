/*
	Copyright (C) 2008 Alexander Stapff, a.stapff@gmx.de

	Geoid separation code by Oleg Gusev, from data by Peter Dana.
	This code was published by the gpsd project (http://gpsd.berlios.de/)
	under the BSD license.

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
#include "filterdefs.h"
#include <math.h>
#include <stdlib.h>

#define MYNAME "height"

#if FILTERS_ENABLED
static char* addopt        = NULL;
static char* wgs84tomslopt = NULL;
static double addf;


static
arglist_t height_args[] = {
  {
    "add", &addopt, "Adds a constant value to every altitude (meter, append \"f\" (x.xxf) for feet)",
    NULL, ARGTYPE_BEGIN_REQ | ARGTYPE_FLOAT, ARG_NOMINMAX
  },
  {
    "wgs84tomsl", &wgs84tomslopt, "Converts WGS84 ellipsoidal height to orthometric height (MSL)",
    NULL, ARGTYPE_END_REQ | ARGTYPE_BOOL, ARG_NOMINMAX
  },
  ARG_TERMINATOR
};


static double bilinear(double x1, double y1, double x2, double y2, double x, double y, double z11, double z12, double z21, double z22)
{
  double delta;

  if (y1 == y2 && x1 == x2) {
    return (z11);
  }
  if (y1 == y2 && x1 != x2) {
    return (z22*(x-x1)+z11*(x2-x))/(x2-x1);
  }
  if (x1 == x2 && y1 != y2) {
    return (z22*(y-y1)+z11*(y2-y))/(y2-y1);
  }

  delta=(y2-y1)*(x2-x1);

  return (z22*(y-y1)*(x-x1)+z12*(y2-y)*(x-x1)+z21*(y-y1)*(x2-x)+z11*(y2-y)*(x2-x))/delta;
}


/* return geoid separation (MSL - WGS84) in meters, given a lat/lot in degrees */
static double wgs84_separation(double lat, double lon)
{
#include "height.h"
  int	ilat, ilon;
  int	ilat1, ilat2, ilon1, ilon2;

  /* sanity checks to prevent segfault on bad data */
  if ((lat > 90.0) || (lat < -90.0)) {
    fatal(MYNAME ": Invalid latitude value (%f)\n", lat);
  }
  if ((lon > 180.0) || (lon < -180.0)) {
    fatal(MYNAME ": Invalid longitude value (%f)\n", lon);
  }

  ilat=(int)floor((90.0+lat)/GEOID_GRID_DEG);
  ilon=(int)floor((180.0+lon)/GEOID_GRID_DEG);

  ilat1=ilat;
  ilon1=ilon;
  ilat2=(ilat < GEOID_ROW-1)? ilat+1:ilat;
  ilon2=(ilon < GEOID_COL-1)? ilon+1:ilon;

  return bilinear(
           ilon1*GEOID_GRID_DEG-180.0,ilat1*GEOID_GRID_DEG-90.0,
           ilon2*GEOID_GRID_DEG-180.0,ilat2*GEOID_GRID_DEG-90.0,
           lon, lat,
           (double)geoid_delta[ilon1+ilat1*GEOID_COL]/GEOID_SCALE,
           (double)geoid_delta[ilon2+ilat1*GEOID_COL]/GEOID_SCALE,
           (double)geoid_delta[ilon1+ilat2*GEOID_COL]/GEOID_SCALE,
           (double)geoid_delta[ilon2+ilat2*GEOID_COL]/GEOID_SCALE
         );
}


static void
correct_height(const Waypoint* wpt)
{
  Waypoint* waypointp = (Waypoint*) wpt;

  if (waypointp->altitude != unknown_alt) {
    if (addopt) {
      waypointp->altitude += addf;
    }
  
    if (wgs84tomslopt) {
        waypointp->altitude -= wgs84_separation(waypointp->latitude, waypointp->longitude);
    }
  }
}


static void
height_init(const char* args)
{
  char* unit;

  if (addopt) {
    addf = strtod(addopt, &unit);

    if (*unit == 'f' || *unit== 'F') {
      addf = FEET_TO_METERS(addf);
    } else if ((*unit != 'm') && (*unit != 'M') && (*unit != '\0'))  {
      fatal(MYNAME ": Invalid unit (\"%c\")! Please use \"m\" for meter or \"f\" for feet.\n", *unit);
    }
  } else {
    addf = 0.0;
  }
}


static void
height_process(void)	/* this procedure must be present in vecs */
{
  waypt_disp_all(correct_height);
  route_disp_all(NULL, NULL, correct_height);
  track_disp_all(NULL, NULL, correct_height);
}


filter_vecs_t height_vecs = {
  height_init,
  height_process,
  NULL,
  NULL,
  height_args
};


#endif // FILTERS_ENABLED
