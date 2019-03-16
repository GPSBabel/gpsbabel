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

#include <cmath>            // macos wants abs from here!
#include <cstdlib>          // for strtod, abs

#include <QtCore/QString>   // for QString
#include <QtCore/QtGlobal>  // for qAsConst, QAddConst<>::Type, foreach

#include "defs.h"
#include "filterdefs.h"
#include "bend.h"
#include "grtcirc.h"        // for RAD, heading_true_degrees, gcdist, linepart, radtometers, DEG


#define MYNAME "bend"

#if FILTERS_ENABLED

void BendFilter::init()
{
  maxDist = 0.0;
  if (distopt) {
    maxDist = strtod(distopt, nullptr);
  }

  minAngle = 0.0;
  if (minangleopt) {
    minAngle = strtod(minangleopt, nullptr);
  }

  route_backup(&routes_orig);
  route_flush_all_routes();
}

Waypoint* BendFilter::create_wpt_dest(const Waypoint* wpt_orig, double lat_orig,
                                      double long_orig, double lat_orig_adj, double long_orig_adj)
{
  double distance = gcdist(lat_orig, long_orig,
                           lat_orig_adj, long_orig_adj);
  double lat_dest;
  double long_dest;
  distance = radtometers(distance);
  if (distance <= maxDist) {
    return nullptr;
  }

  double frac = maxDist / distance;

  linepart(lat_orig, long_orig, lat_orig_adj, long_orig_adj, frac,
           &lat_dest, &long_dest);

  Waypoint* wpt_dest = new Waypoint(*wpt_orig);
  wpt_dest->latitude = DEG(lat_dest);
  wpt_dest->longitude = DEG(long_dest);

  return wpt_dest;
}

int BendFilter::is_small_angle(double lat_orig, double long_orig, double lat_orig_prev,
                               double long_orig_prev, double lat_orig_next,
                               double long_orig_next)
{
  double heading_prev = heading_true_degrees(lat_orig, long_orig,
                        lat_orig_prev, long_orig_prev);
  double heading_next = heading_true_degrees(lat_orig, long_orig,
                        lat_orig_next, long_orig_next);

  double heading_diff = heading_next - heading_prev;

  return ((std::abs(heading_diff - 0.0) < minAngle)
          || (std::abs(heading_diff - 180.0) < minAngle)
          || (std::abs(heading_diff - 360.0) < minAngle));
}

void BendFilter::process_route(const route_head* route_orig, route_head* route_dest)
{
  const Waypoint* wpt_orig_prev = nullptr;
  const Waypoint* wpt_orig = nullptr;

  foreach (const Waypoint* wpt_orig_next, route_orig->waypoint_list) {

    if (wpt_orig_prev == nullptr) {
      if (wpt_orig != nullptr) {
        Waypoint* waypoint_dest = new Waypoint(*wpt_orig);
        route_add_wpt(route_dest, waypoint_dest);
      }
    } else {
      double lat_orig = RAD(wpt_orig->latitude);
      double long_orig = RAD(wpt_orig->longitude);

      double lat_orig_prev = RAD(wpt_orig_prev->latitude);
      double long_orig_prev = RAD(wpt_orig_prev->longitude);

      double lat_orig_next = RAD(wpt_orig_next->latitude);
      double long_orig_next = RAD(wpt_orig_next->longitude);

      if (is_small_angle(lat_orig, long_orig, lat_orig_prev,
                         long_orig_prev, lat_orig_next, long_orig_next)) {
        Waypoint* waypoint_dest = new Waypoint(*wpt_orig);
        route_add_wpt(route_dest, waypoint_dest);
      } else {
        Waypoint* wpt_dest_prev = create_wpt_dest(wpt_orig,
                                  lat_orig, long_orig, lat_orig_prev, long_orig_prev);
        if (wpt_dest_prev != nullptr) {
          route_add_wpt(route_dest, wpt_dest_prev);
        }

        Waypoint* wpt_dest_next = create_wpt_dest(wpt_orig,
                                                   lat_orig, long_orig, lat_orig_next, long_orig_next);
        if (wpt_dest_next != nullptr) {
          route_add_wpt(route_dest, wpt_dest_next);

          wpt_orig = wpt_dest_next;
        }
      }
    }

    wpt_orig_prev = wpt_orig;
    wpt_orig = wpt_orig_next;
  }

  if (wpt_orig != nullptr) {
    Waypoint* waypoint_dest = new Waypoint(*wpt_orig);
    route_add_wpt(route_dest, waypoint_dest);
  }
}

void BendFilter::process_route_orig(const route_head* route_orig)
{
  route_head* route_dest = route_head_alloc();
  route_dest->rte_name = route_orig->rte_name;
  route_dest->rte_desc = route_orig->rte_desc;
  route_dest->fs = fs_chain_copy(route_orig->fs);
  route_dest->rte_num = route_orig->rte_num;

  route_add_head(route_dest);

  process_route(route_orig, route_dest);
}

void BendFilter::process()
{
  for (const auto* route_orig : qAsConst(*routes_orig)) {
    process_route_orig(route_orig);
  }
}

void BendFilter::deinit()
{
  routes_orig->flush();
  delete routes_orig;
  routes_orig = nullptr;
}

#endif // FILTERS_ENABLED
