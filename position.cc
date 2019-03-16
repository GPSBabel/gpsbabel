/*
    Distance Between Points Filter(s)

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

#include <cmath>            // for fabs
#include <cstdlib>          // for strtod

#include <QtCore/QtGlobal>  // for foreach

#include "defs.h"
#include "filterdefs.h"
#include "grtcirc.h"        // for RAD, gcdist, radtomiles
#include "position.h"

#if FILTERS_ENABLED

double PositionFilter::gc_distance(double lat1, double lon1, double lat2, double lon2)
{
  return gcdist(
           RAD(lat1),
           RAD(lon1),
           RAD(lat2),
           RAD(lon2)
         );
}

/* tear through a waypoint queue, processing points by distance */
void PositionFilter::position_runqueue(WaypointList* waypt_list, int nelems, int qtype)
{
  double dist, diff_time;
  int i = 0, anyitem;

  Waypoint** comp = (Waypoint**) xcalloc(nelems, sizeof(*comp));
  int* qlist = (int*) xcalloc(nelems, sizeof(*qlist));

  foreach (Waypoint* waypointp, *waypt_list) {
    comp[i] = waypointp;
    qlist[i] = 0;
    i++;
  }

  for (i = 0 ; i < nelems ; i++) {
    anyitem = 0;

    if (!qlist[i]) {
      for (int j = i + 1 ; j < nelems ; j++) {
        if (!qlist[j]) {
          dist = gc_distance(comp[j]->latitude,
                             comp[j]->longitude,
                             comp[i]->latitude,
                             comp[i]->longitude);

          /* convert radians to integer feet */
          dist = (int)(5280*radtomiles(dist));
          diff_time = fabs(waypt_time(comp[i]) - waypt_time(comp[j]));

          if (dist <= pos_dist) {
            if (check_time && diff_time >= max_diff_time) {
              continue;
            }

            qlist[j] = 1;
            switch (qtype) {
            case wptdata:
              waypt_del(comp[j]);
              delete comp[j];
              break;
            case trkdata:
              track_del_wpt(cur_rte, comp[j]);
              delete comp[j];
              break;
            case rtedata:
              route_del_wpt(cur_rte, comp[j]);
              delete comp[j];
              break;
            default:
              break;
            }
            anyitem = 1;
          }
        }
      }

      if (anyitem && !!purge_duplicates) {
        switch (qtype) {
        case wptdata:
          waypt_del(comp[i]);
          delete comp[i];
          break;
        case trkdata:
          track_del_wpt(cur_rte, comp[i]);
          delete comp[i];
          break;
        case rtedata:
          route_del_wpt(cur_rte, comp[i]);
          delete comp[i];
          break;
        default:
          break;
        }
      }
    }
  }

  if (comp) {
    xfree(comp);
  }

  if (qlist) {
    xfree(qlist);
  }
}

void PositionFilter::position_process_any_route(const route_head* rh, int type)
{
  int i = rh->rte_waypt_ct;

  if (i) {
    cur_rte = const_cast<route_head*>(rh);
    position_runqueue(&cur_rte->waypoint_list, i, type);
    cur_rte = nullptr;
  }

}

void PositionFilter::position_process_rte(const route_head* rh)
{
  position_process_any_route(rh, rtedata);
}

void PositionFilter::position_process_trk(const route_head* rh)
{
  position_process_any_route(rh, trkdata);
}

void PositionFilter::process()
{
  RteHdFunctor<PositionFilter> position_process_rte_f(this, &PositionFilter::position_process_rte);
  RteHdFunctor<PositionFilter> position_process_trk_f(this, &PositionFilter::position_process_trk);

  int i = waypt_count();

  if (i) {
    position_runqueue(global_waypoint_list, i, wptdata);
  }

  route_disp_all(position_process_rte_f, nullptr, nullptr);
  track_disp_all(position_process_trk_f, nullptr, nullptr);
}

void PositionFilter::init()
{
  char* fm;

  pos_dist = 0;
  max_diff_time = 0;
  check_time = 0;

  if (distopt) {
    pos_dist = strtod(distopt, &fm);

    if ((*fm == 'm') || (*fm == 'M')) {
      /* distance is meters */
      pos_dist *= 3.2802;
    }
  }

  if (timeopt) {
    check_time = 1;
    max_diff_time = strtod(timeopt, &fm);
  }
}

#endif // FILTERS_ENABLED
