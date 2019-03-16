/*
    Add route points before and after a bend.

    Copyright (C) 2011 Fernando Arbeiza, fernando.arbeiza@gmail.com
    Copyright (C) 2011 Robert Lipe, robertlipe+source@gpsbabel.org

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

#ifndef BEND_H_INCLUDED_
#define BEND_H_INCLUDED_

#include "defs.h"    // for route_head (ptr only), ARGTYPE_FLOAT, ARG_NOMINMAX
#include "filter.h"  // for Filter

#if FILTERS_ENABLED

class BendFilter:public Filter
{
public:
  arglist_t* get_args() override
  {
    return args;
  }
  void init() override;
  void process() override;
  void deinit() override;

private:
  char* distopt = nullptr;
  char* minangleopt = nullptr;

  double maxDist;
  double minAngle;

  RouteList* routes_orig = nullptr;

  arglist_t args[3] = {
    {
      "distance", &distopt, "Distance to the bend in meters where the new points will be added",
      "25", ARGTYPE_FLOAT, ARG_NOMINMAX, nullptr
    },
    {
      "minangle", &minangleopt, "Minimum bend angle in degrees", "5",
      ARGTYPE_FLOAT, ARG_NOMINMAX, nullptr
    },
    ARG_TERMINATOR
  };

  Waypoint* create_wpt_dest(const Waypoint* wpt_orig, double lat_orig,
                            double long_orig, double lat_orig_adj, double long_orig_adj);
  int is_small_angle(double lat_orig, double long_orig, double lat_orig_prev,
                     double long_orig_prev, double lat_orig_next,
                     double long_orig_next);
  void process_route(const route_head* route_orig, route_head* route_dest);
  void process_route_orig(const route_head* route_orig);

};

#endif // FILTERS_ENABLED
#endif // BEND_H_INCLUDED_
