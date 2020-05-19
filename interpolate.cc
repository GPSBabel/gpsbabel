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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

 */

#include <climits>              // for INT_MAX
#include <cmath>                // for abs, ceil, isfinite, round
#include <cstdlib>              // for abs, atoi, strtod

#include <QtCore/QString>       // for QString
#include <QtCore/QtGlobal>      // for qAsConst, QAddConst<>::Type

#include "defs.h"
#include "interpolate.h"
#include "grtcirc.h"            // for linepart, RAD, gcdist, radtomiles
#include "src/core/datetime.h"  // for DateTime
#include "src/core/logging.h"   // for Fatal
#include "src/core/optional.h"  // for optional


#if FILTERS_ENABLED
#define MYNAME "Interpolate filter"

void InterpolateFilter::process()
{
  RouteList backuproute;
  if (opt_route != nullptr) {
    route_swap(backuproute);
  } else {
    track_swap(backuproute);
  }

  if (backuproute.empty()) {
    Fatal() << MYNAME ": Found no routes or tracks to operate on.";
  }

  for (const auto* rte_old : qAsConst(backuproute)) {
    // FIXME: Allocating a new route_head and copying the members one at a
    // time is not maintainable.  When new members are added it is likely
    // they will not be copied here!
    // We want a deep copy of everything but with an empty WaypointList.
    auto* rte_new = new route_head;
    rte_new->rte_name = rte_old->rte_name;
    rte_new->rte_desc = rte_old->rte_desc;
    rte_new->rte_urls = rte_old->rte_urls;
    rte_new->rte_num = rte_old->rte_num;
    rte_new->fs = rte_old->fs.FsChainCopy();
    rte_new->line_color = rte_old->line_color;
    rte_new->line_width = rte_old->line_width;
    rte_new->session = rte_old->session;
    if (opt_route != nullptr) {
      route_add_head(rte_new);
    } else {
      track_add_head(rte_new);
    }

    double lat1 = 0;
    double lon1 = 0;
    double altitude1 = unknown_alt;
    gpsbabel::DateTime time1;
    bool first = true;
    for (const Waypoint* wpt : rte_old->waypoint_list) {
      if (first) {
        first = false;
      } else {
        gpsbabel_optional::optional<qint64> timespan;
        if (wpt->creation_time.isValid() && time1.isValid()) {
          timespan = wpt->creation_time.toMSecsSinceEpoch() -
                     time1.toMSecsSinceEpoch();
        }
        gpsbabel_optional::optional<double> altspan;
        if (altitude1 != unknown_alt && wpt->altitude != unknown_alt) {
          altspan = wpt->altitude - altitude1;
        }

        // How many points need to be inserted?
        double npts = 0;
        if (opt_time != nullptr) {
          if (!timespan.has_value()) {
            Fatal() << MYNAME ": points must have valid times to interpolate by time!";
          }
          // interpolate even if time is running backwards.
          npts = std::abs(timespan.value()) / max_time_step;
        } else if (opt_dist != nullptr) {
          double distspan = radtomiles(gcdist(RAD(lat1),
                                              RAD(lon1),
                                              RAD(wpt->latitude),
                                              RAD(wpt->longitude)));
          npts = distspan / max_dist_step;
        }
        if (!std::isfinite(npts) || (npts >= INT_MAX)) {
          Fatal() << MYNAME ": interpolation interval too small!";
        }

        // Insert the required points
        int nmax = static_cast<int>(ceil(npts)) - 1; // # of points to insert
        for (int n = 0; n < nmax; ++n) {
          double frac = static_cast<double>(n + 1) /
                        static_cast<double>(nmax + 1);
          // We create the inserted point from the Waypoint at the end of the
          // span.  Another choice would be the Waypoint at the beginning of
          // the span.  We clear some fields but use a copy of the rest or the
          // interpolated value.
          auto* wpt_new = new Waypoint(*wpt);
          wpt_new->shortname = QString();
          wpt_new->description = QString();
          if (timespan.has_value()) {
            wpt_new->SetCreationTime(0, time1.toMSecsSinceEpoch() +
                                     round(frac * timespan.value()));
          } else {
            wpt_new->creation_time = gpsbabel::DateTime();
          }
          linepart(lat1, lon1,
                   wpt->latitude, wpt->longitude,
                   frac,
                   &wpt_new->latitude,
                   &wpt_new->longitude);
          if (altspan.has_value()) {
            wpt_new->altitude = altitude1 + (frac * altspan.value());
          } else {
            wpt_new->altitude = unknown_alt;
          }
          if (opt_route != nullptr) {
            route_add_wpt(rte_new, wpt_new);
          } else {
            track_add_wpt(rte_new, wpt_new);
          }
        }
      }
      if (opt_route != nullptr) {
        route_add_wpt(rte_new, new Waypoint(*wpt));
      } else {
        track_add_wpt(rte_new, new Waypoint(*wpt));
      }

      lat1 = wpt->latitude;
      lon1 = wpt->longitude;
      altitude1 = wpt->altitude;
      time1 = wpt->creation_time;
    }
  }
  backuproute.flush();
}

void InterpolateFilter::init()
{
  char* fm;
  if ((opt_time != nullptr) && (opt_dist != nullptr)) {
    Fatal() << MYNAME ": Can't interpolate on both time and distance.";
  } else if ((opt_time != nullptr) && (opt_route != nullptr)) {
    Fatal() << MYNAME ": Can't interpolate routes on time.";
  } else if (opt_time != nullptr) {
    max_time_step = 1000 * strtod(opt_time, nullptr); // milliseconds
    if (max_time_step <= 0) {
      Fatal() << MYNAME ": interpolation time should be positve!";
    }
  } else if (opt_dist != nullptr) {
    max_dist_step = strtod(opt_dist, &fm);
    if ((*fm == 'k') || (*fm == 'K')) {
      /* distance is kilometers, convert to miles */
      max_dist_step *= kMilesPerKilometer;
    }
    if (max_dist_step <= 0) {
      Fatal() << MYNAME ": interpolation distance should be positve!";
    }
  } else {
    Fatal() << MYNAME ": No interval specified.";
  }
}

#endif // FILTERS_ENABLED
