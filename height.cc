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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

 */


#include "defs.h"
#include "height.h"
#include <cmath>    // for floor
#include <cstdint>  // for int8_t


#if FILTERS_ENABLED

double HeightFilter::bilinear(double x1, double y1, double x2, double y2, double x, double y, double z11, double z12, double z21, double z22)
{
  if (y1 == y2 && x1 == x2) {
    return (z11);
  }
  if (y1 == y2 && x1 != x2) {
    return (z22*(x-x1)+z11*(x2-x))/(x2-x1);
  }
  if (x1 == x2 && y1 != y2) {
    return (z22*(y-y1)+z11*(y2-y))/(y2-y1);
  }

  double delta = (y2-y1)*(x2-x1);

  return (z22*(y-y1)*(x-x1)+z12*(y2-y)*(x-x1)+z21*(y-y1)*(x2-x)+z11*(y2-y)*(x2-x))/delta;
}

/* return geoid separation (MSL - WGS84) in meters, given a lat/lot in degrees */
double HeightFilter::wgs84_separation(double lat, double lon)
{
  /* sanity checks to prevent segfault on bad data */
  if ((lat > 90.0) || (lat < -90.0)) {
    fatal("Invalid latitude value (%f)\n", lat);
  }
  if ((lon > 180.0) || (lon < -180.0)) {
    fatal("Invalid longitude value (%f)\n", lon);
  }

  auto ilat = static_cast<int>(floor((90.0+lat)/geoid_grid_deg));
  auto ilon = static_cast<int>(floor((180.0+lon)/geoid_grid_deg));

  int ilat1 = ilat;
  int ilon1 = ilon;
  int ilat2 = (ilat < geoid_row-1)? ilat+1:ilat;
  int ilon2 = (ilon < geoid_col-1)? ilon+1:ilon;

  return bilinear(
           ilon1*geoid_grid_deg-180.0, ilat1*geoid_grid_deg-90.0,
           ilon2*geoid_grid_deg-180.0, ilat2*geoid_grid_deg-90.0,
           lon, lat,
           static_cast<double>(geoid_delta[ilat1][ilon1])/geoid_scale,
           static_cast<double>(geoid_delta[ilat1][ilon2])/geoid_scale,
           static_cast<double>(geoid_delta[ilat2][ilon1])/geoid_scale,
           static_cast<double>(geoid_delta[ilat2][ilon2])/geoid_scale
         );
}

void HeightFilter::correct_height(const Waypoint* wpt)
{
  auto* waypointp = const_cast<Waypoint*>(wpt);

  if (waypointp->altitude != unknown_alt) {
    if (addopt) {
      waypointp->altitude += addf;
    }

    if (wgs84tomslopt) {
      waypointp->altitude -= wgs84_separation(waypointp->latitude, waypointp->longitude);
    }
  }
}

void HeightFilter::init()
{
  addf = 0.0;
  if (addopt) {
    if (parse_distance(addopt, &addf, 1.0) == 0) {
      fatal("No height specified with add option.");
    }
  }
}

void HeightFilter::process()
{
  WayptFunctor<HeightFilter> correct_height_f(this, &HeightFilter::correct_height);

  waypt_disp_all(correct_height_f);
  route_disp_all(nullptr, nullptr, correct_height_f);
  track_disp_all(nullptr, nullptr, correct_height_f);
}

#endif // FILTERS_ENABLED
