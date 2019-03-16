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
    Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111 USA

 */

#include <cstdlib>          // for atof, atoi, qsort, strtod

#include <QtCore/QString>   // for QString
#include <QtCore/QtGlobal>  // for foreach

#include "defs.h"
#include "filterdefs.h"
#include "radius.h"
#include "grtcirc.h"        // for RAD, gcdist, radtomiles

#if FILTERS_ENABLED

double RadiusFilter::gc_distance(double lat1, double lon1, double lat2, double lon2)
{
  return gcdist(
           RAD(lat1),
           RAD(lon1),
           RAD(lat2),
           RAD(lon2)
         );
}

int RadiusFilter::dist_comp(const void* a, const void* b)
{
  const Waypoint* x1 = *(Waypoint**)a;
  const Waypoint* x2 = *(Waypoint**)b;
  extra_data* x1e = (extra_data*) x1->extra_data;
  extra_data* x2e = (extra_data*) x2->extra_data;

  if (x1e->distance > x2e->distance) {
    return 1;
  }
  if (x1e->distance < x2e->distance) {
    return -1;
  }
  return 0;

}

void RadiusFilter::process()
{
  Waypoint** comp;
  int i, wc;
  route_head* rte_head = nullptr;
  foreach (Waypoint* waypointp, *global_waypoint_list) {
    double dist = gc_distance(waypointp->latitude,
                       waypointp->longitude,
                       home_pos->latitude,
                       home_pos->longitude);

    /* convert radians to float point statute miles */
    dist = radtomiles(dist);

    if ((dist >= pos_dist) == (exclopt == nullptr)) {
      waypt_del(waypointp);
      delete waypointp;
      continue;
    }

    extra_data* ed = (extra_data*) xcalloc(1, sizeof(*ed));
    ed->distance = dist;
    waypointp->extra_data = ed;
  }

  wc = waypt_count();

  comp = (Waypoint**) xcalloc(wc, sizeof(*comp));

  i = 0;

  /*
   * Create an array of remaining waypoints, popping them off the
   * master queue as we go.   This gives us something reasonable
   * for qsort.
   */

  foreach (Waypoint* wp, *global_waypoint_list) {
    comp[i] = wp;
    waypt_del(wp);
    i++;
  }

  if (!nosort) {
    qsort(comp, wc, sizeof(Waypoint*), dist_comp);
  }

  if (routename) {
    rte_head = route_head_alloc();
    rte_head->rte_name = routename;
    route_add_head(rte_head);
  }

  /*
   * The comp array is now sorted by distance.   As we run through it,
   * we push them back onto the master wp list, letting us pass them
   * on through in the modified order.
   */
  for (i = 0; i < wc; i++) {
    Waypoint* wp = comp[i];

    xfree(wp->extra_data);
    wp->extra_data = nullptr;

    if (maxctarg && i >= maxct) {
      continue;
    }
    if (routename) {
      route_add_wpt(rte_head, wp);
    } else {
      waypt_add(wp);
    }
  }

  xfree(comp);
}

void RadiusFilter::init()
{
  char* fm;

  pos_dist = 0;

  if (distopt) {
    pos_dist = strtod(distopt, &fm);

    if ((*fm == 'k') || (*fm == 'K')) {
      /* distance is kilometers, convert to miles */
      pos_dist *= .6214;
    }
  }

  if (maxctarg) {
    maxct = atoi(maxctarg);
  } else {
    maxct = 0;
  }

  home_pos = new Waypoint;

  if (latopt) {
    home_pos->latitude = atof(latopt);
  }
  if (lonopt) {
    home_pos->longitude = atof(lonopt);
  }
}

void RadiusFilter::deinit()
{
  delete home_pos;
}

#endif // FILTERS_ENABLED
