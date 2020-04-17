/*
    Radius Filter

    Copyright (C) 2002 Robert Lipe, robertlipe+source@gpsbabel.org

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

#ifndef RADIUS_H_INCLUDED_
#define RADIUS_H_INCLUDED_

#include <QtCore/QVector>  // for QVector

#include "defs.h"    // for ARG_NOMINMAX, ARGTYPE_FLOAT, ARGTYPE_REQUIRED
#include "filter.h"  // for Filter

#if FILTERS_ENABLED

class RadiusFilter:public Filter
{
public:
  QVector<arglist_t>* get_args() override
  {
    return &args;
  }
  void init() override;
  void process() override;
  void deinit() override;

private:
  double pos_dist;
  char* distopt = nullptr;
  char* latopt = nullptr;
  char* lonopt = nullptr;
  char* exclopt = nullptr;
  char* nosort = nullptr;
  char* maxctarg = nullptr;
  char* routename = nullptr;
  int maxct;

  Waypoint* home_pos;

  struct extra_data {
    double distance;
  };

  QVector<arglist_t> args = {
    {
      "lat", &latopt,       "Latitude for center point (D.DDDDD)",
      nullptr, ARGTYPE_FLOAT | ARGTYPE_REQUIRED, ARG_NOMINMAX, nullptr
    },
    {
      "lon", &lonopt,       "Longitude for center point (D.DDDDD)",
      nullptr, ARGTYPE_FLOAT | ARGTYPE_REQUIRED, ARG_NOMINMAX, nullptr
    },
    {
      "distance", &distopt, "Maximum distance from center",
      nullptr, ARGTYPE_FLOAT | ARGTYPE_REQUIRED, ARG_NOMINMAX, nullptr
    },
    {
      "exclude", &exclopt,  "Exclude points close to center",
      nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    {
      "nosort", &nosort,    "Inhibit sort by distance to center",
      nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    {
      "maxcount", &maxctarg,"Output no more than this number of points",
      nullptr, ARGTYPE_INT, "1", nullptr, nullptr
    },
    {
      "asroute", &routename,"Put resulting waypoints in route of this name",
      nullptr, ARGTYPE_STRING, nullptr, nullptr, nullptr
    },
  };

  double gc_distance(double lat1, double lon1, double lat2, double lon2);

};
#endif // FILTERS_ENABLED
#endif // RADIUS_H_INCLUDED_
