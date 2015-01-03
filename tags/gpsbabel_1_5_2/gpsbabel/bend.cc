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

#include "defs.h"
#include "filterdefs.h"
#include "grtcirc.h"

#include <stdlib.h>
#include <cmath>

#define MYNAME "bend"

#if FILTERS_ENABLED

static char* distopt = NULL;
static char* minangleopt = NULL;

static double maxDist;
static double minAngle;

static queue* routes_orig = NULL;
static int routes_orig_num = 0;

static
arglist_t bend_args[] = {
  {
    "distance", &distopt, "Distance to the bend in meters where the new points will be added",
    "25", ARGTYPE_FLOAT, ARG_NOMINMAX
  },
  {
    "minangle", &minangleopt, "Minimum bend angle in degrees", "5",
    ARGTYPE_FLOAT, ARG_NOMINMAX
  },
  ARG_TERMINATOR
};

static void
bend_init(const char* args)
{
  maxDist = 0.0;
  if (distopt) {
    maxDist = strtod(distopt, NULL);
  }

  minAngle = 0.0;
  if (minangleopt) {
    minAngle = strtod(minangleopt, NULL);
  }

  route_backup(&routes_orig_num, &routes_orig);
  route_flush_all_routes();
}

static Waypoint*
create_wpt_dest(const Waypoint* wpt_orig, double lat_orig,
                double long_orig, double lat_orig_adj, double long_orig_adj)
{
  double distance = gcdist(lat_orig, long_orig,
                           lat_orig_adj, long_orig_adj);
  double frac;
  double lat_dest;
  double long_dest;
  Waypoint* wpt_dest = NULL;
  distance = radtometers(distance);
  if (distance <= maxDist) {
    return NULL;
  }

  frac = maxDist / distance;

  linepart(lat_orig, long_orig, lat_orig_adj, long_orig_adj, frac,
           &lat_dest, &long_dest);

  wpt_dest = new Waypoint(*wpt_orig);
  wpt_dest->latitude = DEG(lat_dest);
  wpt_dest->longitude = DEG(long_dest);

  return wpt_dest;
}

static int
is_small_angle(double lat_orig, double long_orig, double lat_orig_prev,
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

static void
process_route(const route_head* route_orig, route_head* route_dest)
{
  Waypoint* wpt_orig_prev = NULL;
  Waypoint* wpt_orig = NULL;

  queue* elem, *tmp;
  QUEUE_FOR_EACH(&route_orig->waypoint_list, elem, tmp) {
    Waypoint* wpt_orig_next = (Waypoint*)elem;

    if (wpt_orig_prev == NULL) {
      if (wpt_orig != NULL) {
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
      Waypoint* wpt_dest_next = NULL;

      if (is_small_angle(lat_orig, long_orig, lat_orig_prev,
                         long_orig_prev, lat_orig_next, long_orig_next)) {
        Waypoint* waypoint_dest = new Waypoint(*wpt_orig);
        route_add_wpt(route_dest, waypoint_dest);
      } else {
        Waypoint* wpt_dest_prev = create_wpt_dest(wpt_orig,
                                  lat_orig, long_orig, lat_orig_prev, long_orig_prev);
        if (wpt_dest_prev != NULL) {
          route_add_wpt(route_dest, wpt_dest_prev);
        }

        wpt_dest_next = create_wpt_dest(wpt_orig,
                                        lat_orig, long_orig, lat_orig_next, long_orig_next);
        if (wpt_dest_next != NULL) {
          route_add_wpt(route_dest, wpt_dest_next);

          wpt_orig = wpt_dest_next;
        }
      }
    }

    wpt_orig_prev = wpt_orig;
    wpt_orig = wpt_orig_next;
  }

  if (wpt_orig != NULL) {
    Waypoint* waypoint_dest = new Waypoint(*wpt_orig);
    route_add_wpt(route_dest, waypoint_dest);
  }
}

static void
process_route_orig(const route_head* route_orig)
{
  route_head* route_dest = route_head_alloc();
  route_dest->rte_name = route_orig->rte_name;
  route_dest->rte_desc = route_orig->rte_desc;
  route_dest->fs = fs_chain_copy(route_orig->fs);
  route_dest->rte_num = route_orig->rte_num;

  route_add_head(route_dest);

  process_route(route_orig, route_dest);
}

static void
bend_process(void)
{
  queue* elem, *tmp;
  QUEUE_FOR_EACH(routes_orig, elem, tmp) {
    route_head* route_orig = (route_head*)elem;
    process_route_orig(route_orig);
  }
}

static void
bend_deinit(void)
{
  route_flush(routes_orig);
  xfree(routes_orig);
}

static void
bend_exit(void)
{
}

filter_vecs_t bend_vecs = {
  bend_init,
  bend_process,
  bend_deinit,
  bend_exit,
  bend_args
};

#endif // FILTERS_ENABLED
