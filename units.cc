/*
    Display scaled distances in 'local' units.

    Copyright (C) 2006 Robert Lipe, robertlipe+source@gpsbabel.org

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
#include "units.h"

static fmt_units units = fmt_units::statute;

void
fmt_setunits(fmt_units u)
{
  switch (u) {
  case fmt_units::statute:
  case fmt_units::metric:
  case fmt_units::nautical:
  case fmt_units::aviation:
    units = u;
    break;
  default:
    fatal("not done yet");
    break;
  }
}

std::pair<double, QString>
fmt_distance(const double distance_meters)
{
  double d;
  const char* tag;

  switch (units) {
  case fmt_units::statute:
    d = METERS_TO_FEET(distance_meters);
    if (d < 5280) {
      tag = "ft";
    } else  {
      d = METERS_TO_MILES(distance_meters);
      tag = "mi";
    }
    break;
  case fmt_units::nautical:
  case fmt_units::aviation:
    d = METERS_TO_NMILES(distance_meters);
    tag = "NM";
    break;
  case fmt_units::metric:
    d = distance_meters;
    if (d < 1000) {
      tag = "meters";
    } else {
      d = d / 1000.0;
      tag = "km";
    }
    break;

  default:
    fatal("not done yet");
    break;
  }

  return {d, tag};
}

std::pair<double, QString>
fmt_altitude(const double distance_meters)
{
  double d;
  const char* tag;

  switch (units) {
  case fmt_units::statute:
  case fmt_units::aviation:
    d = METERS_TO_FEET(distance_meters);
    tag = "ft";
    break;
  case fmt_units::nautical:
    d = METERS_TO_NMILES(distance_meters);
    tag = "NM";
    break;
  case fmt_units::metric:
    d = distance_meters;
    tag = "meters";
    break;

  default:
    fatal("not done yet");
    break;
  }

  return {d, tag};
}

std::pair<double, QString>
fmt_speed(const double distance_meters_sec)
{
  double d;
  const char* tag;

  switch (units) {
  case fmt_units::statute:
    d = METERS_TO_MILES(distance_meters_sec) * SECONDS_PER_HOUR ;
    tag = "mph";
    break;
  case fmt_units::nautical:
  case fmt_units::aviation:
    d = METERS_TO_NMILES(distance_meters_sec) * SECONDS_PER_HOUR ;
    tag = "knts";
    break;
  case fmt_units::metric:
    d = distance_meters_sec * SECONDS_PER_HOUR;
    tag = "meters/hour";
    if (d > 1000.0) {
      d /= 1000.0;
      tag = "km/hour";
    }
    break;
  default:
    fatal("not done yet");

  }
  return {d, tag};
}
