/*
    Interpolate filter

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

#include <cstdlib>              // for atoi, strtod

#include <QtCore/QString>       // for QString
#include <QtCore/QtGlobal>      // for qAsConst, QAddConst<>::Type, foreach

#include "defs.h"
#include "filterdefs.h"
#include "interpolate.h"
#include "grtcirc.h"            // for linepart, RAD, gcdist, radtomiles
#include "src/core/datetime.h"  // for DateTime


#if FILTERS_ENABLED
#define MYNAME "Interpolate filter"

void InterpolateFilter::process()
{
  RouteList* backuproute = nullptr;
  double lat1 = 0, lon1 = 0;
  double altitude1 = unknown_alt;
  unsigned int time1 = 0;
  double frac;

  if (opt_route) {
    route_backup(&backuproute);
    route_flush_all_routes();
  } else {
    track_backup(&backuproute);
    route_flush_all_tracks();
  }

  if (backuproute->empty()) {
    fatal(MYNAME ": Found no routes or tracks to operate on.\n");
  }

  for (const auto* rte_old : qAsConst(*backuproute)) {

    route_head* rte_new = route_head_alloc();
    rte_new->rte_name = rte_old->rte_name;
    rte_new->rte_desc = rte_old->rte_desc;
    rte_new->fs = fs_chain_copy(rte_old->fs);
    rte_new->rte_num = rte_old->rte_num;
    if (opt_route) {
      route_add_head(rte_new);
    } else {
      track_add_head(rte_new);
    }
    bool first = true;
    foreach (const Waypoint* wpt, rte_old->waypoint_list) {
      if (first) {
        first = false;
      } else {
        if (opt_interval &&
            wpt->creation_time.toTime_t() - time1 > interval) {
          for (unsigned int timen = time1+interval;
               timen < wpt->creation_time.toTime_t();
               timen += interval) {
            Waypoint* wpt_new = new Waypoint(*wpt);
            wpt_new->SetCreationTime(timen);
            wpt_new->shortname = QString();
            wpt_new->description = QString();

            frac = (double)(timen - time1) / (double)(wpt->creation_time.toTime_t() - time1);
            linepart(lat1, lon1,
                     wpt->latitude, wpt->longitude,
                     frac,
                     &wpt_new->latitude,
                     &wpt_new->longitude);
            if (altitude1 != unknown_alt && wpt->altitude != unknown_alt) {
              wpt_new->altitude = altitude1 + frac * (wpt->altitude - altitude1);
            }
            if (opt_route) {
              route_add_wpt(rte_new, wpt_new);
            } else {
              track_add_wpt(rte_new, wpt_new);
            }
          }
        } else if (opt_dist) {
          double rt1 = RAD(lat1);
          double rn1 = RAD(lon1);
          double rt2 = RAD(wpt->latitude);
          double rn2 = RAD(wpt->longitude);
          double curdist = gcdist(rt1, rn1, rt2, rn2);
          curdist = radtomiles(curdist);
          if (curdist > dist) {
            for (double distn = dist;
                 distn < curdist;
                 distn += dist) {
              Waypoint* wpt_new = new Waypoint(*wpt);
              frac = distn / curdist;
              wpt_new->SetCreationTime(frac * (wpt->creation_time.toTime_t() - time1) + time1);
              wpt_new->shortname = QString();
              wpt_new->description = QString();
              linepart(lat1, lon1,
                       wpt->latitude, wpt->longitude,
                       frac,
                       &wpt_new->latitude,
                       &wpt_new->longitude);
              if (altitude1 != unknown_alt && wpt->altitude != unknown_alt) {
                wpt_new->altitude = altitude1 + frac * (wpt->altitude - altitude1);
              }
              if (opt_route) {
                route_add_wpt(rte_new, wpt_new);
              } else {
                track_add_wpt(rte_new, wpt_new);
              }
            }
          }
        }
      }
      if (opt_route) {
        route_add_wpt(rte_new, new Waypoint(*wpt));
      } else {
        track_add_wpt(rte_new, new Waypoint(*wpt));
      }

      lat1 = wpt->latitude;
      lon1 = wpt->longitude;
      altitude1 = wpt->altitude;
      time1 = wpt->creation_time.toTime_t();
    }
  }
  backuproute->flush();
  delete backuproute;
}

void InterpolateFilter::init()
{

  char* fm;
  if (opt_interval && opt_dist) {
    fatal(MYNAME ": Can't interpolate on both time and distance.\n");
  } else if (opt_interval && opt_route) {
    fatal(MYNAME ": Can't interpolate routes on time.\n");
  } else if (opt_interval) {
    interval = atoi(opt_interval);
  } else if (opt_dist) {
    dist = strtod(opt_dist, &fm);
    if ((*fm == 'k') || (*fm == 'K')) {
      /* distance is kilometers, convert to miles */
      dist *= .6214;
    }
  } else {
    fatal(MYNAME ": No interval specified.\n");
  }
}

#endif // FILTERS_ENABLED
