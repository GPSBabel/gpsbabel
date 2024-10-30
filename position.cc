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

#include "position.h"

#include <cmath>                // for abs
#include <cstdlib>              // for abs

#include <QList>                // for QList
#include <QtGlobal>             // for qRound64, qint64

#include "defs.h"
#include "grtcirc.h"            // for gcdist, radtometers
#include "src/core/datetime.h"  // for DateTime

#if FILTERS_ENABLED
#define MYNAME "Position filter"

/* tear through a waypoint queue, processing points by distance */
void PositionFilter::position_runqueue(const WaypointList& waypt_list, int qtype)
{
  if (!waypt_list.empty()) {
    QList<WptRecord> qlist;

    for (auto* const waypointp : waypt_list) {
      qlist.append(WptRecord(waypointp));
    }
    int nelems = qlist.size();

    for (int i = 0 ; i < nelems ; ++i) {
      if (!qlist.at(i).deleted) {
        bool something_deleted = false;

        for (int j = i + 1 ; j < nelems ; ++j) {
          if (!qlist.at(j).deleted) {
            double dist = radtometers(gcdist(qlist.at(j).wpt->position(),
                                             qlist.at(i).wpt->position()));

            if (dist <= pos_dist) {
              if (check_time) {
                qint64 diff_time = std::abs(qlist.at(j).wpt->creation_time.msecsTo(qlist.at(i).wpt->creation_time));
                if (diff_time >= max_diff_time) {
                  continue;
                }
              }

              qlist[j].deleted = true;
              qlist.at(j).wpt->wpt_flags.marked_for_deletion = 1;
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

        if (something_deleted && purge_duplicates) {
          qlist.at(i).wpt->wpt_flags.marked_for_deletion = 1;
        }
      }
    }
  }
}

void PositionFilter::process()
{
  position_runqueue(*global_waypoint_list, wptdata);
  del_marked_wpts();

  auto position_process_rte_lambda = [this](const route_head* rte) ->void {
    position_runqueue(rte->waypoint_list, rtedata);
  };
  auto rte_trl_lambda = [](const route_head* rte) -> void {
    route_del_marked_wpts(const_cast<route_head*>(rte));
  };
  route_disp_all(position_process_rte_lambda, rte_trl_lambda, nullptr);

  auto position_process_trk_lambda = [this](const route_head* rte) ->void {
    position_runqueue(rte->waypoint_list, trkdata);
  };
  auto trk_trl_lambda = [](const route_head* rte) -> void {
    track_del_marked_wpts(const_cast<route_head*>(rte));
  };
  track_disp_all(position_process_trk_lambda, trk_trl_lambda, nullptr);
}

void PositionFilter::init()
{
  pos_dist = 0.0;
  max_diff_time = 0;
  check_time = false;

  if (distopt) {
    if (parse_distance(distopt, &pos_dist, kMetersPerFoot, MYNAME) == 0) {
      fatal(MYNAME ": No distance specified with distance option.\n");
    }
  }

  if (timeopt) {
    check_time = true;
    max_diff_time = qRound64(timeopt.get_result() * 1000.0);
  }
}

#endif // FILTERS_ENABLED
