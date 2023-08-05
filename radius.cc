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

#include "radius.h"

#include <cstdlib>          // for strtod

#include <QString>          // for QString
#include <QtGlobal>         // for qAsConst, QAddConst<>::Type, foreach

#include "defs.h"           // for Waypoint, del_marked_wpts, route_add_head, route_add_wpt, waypt_add, waypt_sort, waypt_swap, xstrtoi, route_head, WaypointList, kMilesPerKilometer


#if FILTERS_ENABLED

void RadiusFilter::process()
{
  foreach (Waypoint* waypointp, *global_waypoint_list) {
    double dist = gc_distance(waypointp->latitude, waypointp->longitude,
                              home_pos->latitude, home_pos->longitude);

    if ((dist >= pos_dist) == (exclopt == nullptr)) {
      waypointp->wpt_flags.marked_for_deletion = 1;
    } else {
      auto* ed = new extra_data;
      ed->distance = dist;
      waypointp->extra_data = ed;
    }
  }
  del_marked_wpts();

  if (nosort == nullptr) {
    auto dist_comp_lambda = [](const Waypoint* a, const Waypoint* b)->bool {
      const auto* aed = static_cast<const extra_data*>(a->extra_data);
      const auto* bed = static_cast<const extra_data*>(b->extra_data);
      return aed->distance < bed->distance;
    };
    waypt_sort(dist_comp_lambda);
  }

  route_head* rte_head = nullptr;
  if (routename != nullptr) {
    rte_head = new route_head;
    rte_head->rte_name = routename;
    route_add_head(rte_head);
  }

  /*
   * Create an list of remaining waypoints.
   * Delete them, add them to the global waypoint list, or add them
   * to a new route.
   */

  WaypointList comp;
  waypt_swap(comp);

  int i = 0;
  for (Waypoint* wp : qAsConst(comp)) {
    delete static_cast<extra_data*>(wp->extra_data);
    wp->extra_data = nullptr;

    if ((maxctarg != nullptr) && (i >= maxct)) {
      delete wp;
    } else {
      if (routename != nullptr) {
        route_add_wpt(rte_head, wp);
      } else {
        waypt_add(wp);
      }
    }
    ++i;
  }
}

void RadiusFilter::init()
{
  pos_dist = 0;

  if (distopt != nullptr) {
    char* fm;
    pos_dist = strtod(distopt, &fm);

    if ((*fm == 'k') || (*fm == 'K')) {
      /* distance is kilometers, convert to miles */
      pos_dist *= kMilesPerKilometer;
    }
  }

  if (maxctarg != nullptr) {
    maxct = xstrtoi(maxctarg, nullptr, 10);
  } else {
    maxct = 0;
  }

  home_pos = new Waypoint;

  if (latopt != nullptr) {
    home_pos->latitude = strtod(latopt, nullptr);
  }
  if (lonopt != nullptr) {
    home_pos->longitude = strtod(lonopt, nullptr);
  }
}

void RadiusFilter::deinit()
{
  delete home_pos;
}

#endif // FILTERS_ENABLED
