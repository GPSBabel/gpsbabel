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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

 */

#include <cmath>            // for fabs
#include <cstdlib>          // for strtod

#include <QtCore/QList>     // for QList
#include <QtCore/QtGlobal>  // for qAsConst, QAddConst<>::Type

#include "defs.h"
#include "grtcirc.h"        // for RAD, gcdist, radtometers
#include "position.h"

#if FILTERS_ENABLED

double PositionFilter::gc_distance(double lat1, double lon1, double lat2, double lon2)
{
  return radtometers(gcdist(
                       RAD(lat1),
                       RAD(lon1),
                       RAD(lat2),
                       RAD(lon2)
                     ));
}

/* tear through a waypoint queue, processing points by distance */
void PositionFilter::position_runqueue(WaypointList* waypt_list, int qtype)
{
  QList<WptRecord> qlist;

  for (auto* const waypointp : qAsConst(*waypt_list)) {
    qlist.append(WptRecord(waypointp));
  }
  int nelems = qlist.size();

  for (int i = 0 ; i < nelems ; ++i) {
    if (!qlist.at(i).deleted) {
      bool something_deleted = false;

      for (int j = i + 1 ; j < nelems ; ++j) {
        if (!qlist.at(j).deleted) {
          double dist = gc_distance(qlist.at(j).wpt->latitude,
                                    qlist.at(j).wpt->longitude,
                                    qlist.at(i).wpt->latitude,
                                    qlist.at(i).wpt->longitude);

          if (dist <= pos_dist) {
            if (check_time) {
              double diff_time = fabs(waypt_time(qlist.at(i).wpt) - waypt_time(qlist.at(j).wpt));
              if (diff_time >= max_diff_time) {
                continue;
              }
            }

            qlist[j].deleted = true;
            switch (qtype) {
            case wptdata:
              waypt_del(qlist.at(j).wpt);
              delete qlist.at(j).wpt;
              break;
            case trkdata:
              track_del_wpt(cur_rte, qlist.at(j).wpt);
              delete qlist.at(j).wpt;
              break;
            case rtedata:
              route_del_wpt(cur_rte, qlist.at(j).wpt);
              delete qlist.at(j).wpt;
              break;
            default:
              break;
            }
            something_deleted = true;
          } else {
            // Unlike waypoints, routes and tracks are ordered paths.
            // Don't eliminate points from the return path when the
            // route or track loops back on itself.
            if ((qtype == trkdata) || (qtype == rtedata)) {
              break;
            }
          }
        }
      }

      if (something_deleted && (purge_duplicates != nullptr)) {
        switch (qtype) {
        case wptdata:
          waypt_del(qlist.at(i).wpt);
          delete qlist.at(i).wpt;
          break;
        case trkdata:
          track_del_wpt(cur_rte, qlist.at(i).wpt);
          delete qlist.at(i).wpt;
          break;
        case rtedata:
          route_del_wpt(cur_rte, qlist.at(i).wpt);
          delete qlist.at(i).wpt;
          break;
        default:
          break;
        }
      }
    }
  }

}

void PositionFilter::position_process_any_route(const route_head* rh, int type)
{
  if (rh->rte_waypt_ct != 0) {
    cur_rte = const_cast<route_head*>(rh);
    position_runqueue(&cur_rte->waypoint_list, type);
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

  if (waypt_count() != 0) {
    position_runqueue(global_waypoint_list, wptdata);
  }

  route_disp_all(position_process_rte_f, nullptr, nullptr);
  track_disp_all(position_process_trk_f, nullptr, nullptr);
}

void PositionFilter::init()
{
  char* fm;

  pos_dist = 0.0;
  max_diff_time = 0.0;
  check_time = false;

  if (distopt != nullptr) {
    pos_dist = strtod(distopt, &fm);

    if (!((*fm == 'm') || (*fm == 'M'))) {
      /* distance is feet */
      pos_dist = FEET_TO_METERS(pos_dist);
    }
  }

  if (timeopt != nullptr) {
    check_time = true;
    max_diff_time = strtod(timeopt, &fm);
  }
}

#endif // FILTERS_ENABLED
