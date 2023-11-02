/*
    Interpolate filter

    Copyright (C) 2002,2023 Robert Lipe, robertlipe+source@gpsbabel.org

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

#include "interpolate.h"

#include <climits>              // for INT_MAX
#include <cmath>                // for abs, ceil, isfinite, round
#include <cstdlib>              // for abs, strtod
#include <optional>             // for optional

#include <QString>              // for QString
#include <QtGlobal>             // for qint64, qAsConst, qRound64

#include "defs.h"
#include "grtcirc.h"            // for linepart, RAD, gcdist, radtomiles
#include "src/core/datetime.h"  // for DateTime
#include "src/core/logging.h"   // for Fatal


#if FILTERS_ENABLED
#define MYNAME "Interpolate filter"

void InterpolateFilter::process()
{
  if (((opt_route != nullptr) && (route_count() == 0)) || ((opt_route == nullptr) && (track_count() == 0))) {
    fatal(FatalMsg() << MYNAME ": Found no routes or tracks to operate on.");
  }

  auto process_rte_lambda = [this](const route_head* rte)->void {
    process_rte(const_cast<route_head*>(rte));
  };
  if (opt_route != nullptr) {
    route_disp_all(process_rte_lambda, nullptr, nullptr);
  } else {
    track_disp_all(process_rte_lambda, nullptr, nullptr);
  }
}

void InterpolateFilter::process_rte(route_head* rte)
{
  // Steal all the wpts
  WaypointList wptlist;
  if (opt_route != nullptr) {
    route_swap_wpts(rte, wptlist);
  } else {
    track_swap_wpts(rte, wptlist);
  }

  // And add them back, with interpolated points interspersed.
  double lat1 = 0;
  double lon1 = 0;
  double altitude1 = unknown_alt;
  gpsbabel::DateTime time1;
  bool first = true;
  foreach (Waypoint* wpt, wptlist) {
    if (first) {
      first = false;
    } else {
      std::optional<qint64> timespan;
      if (wpt->creation_time.isValid() && time1.isValid()) {
        timespan = time1.msecsTo(wpt->creation_time);
      }
      std::optional<double> altspan;
      if (altitude1 != unknown_alt && wpt->altitude != unknown_alt) {
        altspan = wpt->altitude - altitude1;
      }

      // How many points need to be inserted?
      double npts = 0;
      if (opt_time != nullptr) {
        if (!timespan.has_value()) {
          fatal(FatalMsg() << MYNAME ": points must have valid times to interpolate by time!");
        }
        // interpolate even if time is running backwards.
        npts = std::abs(*timespan) / max_time_step;
      } else if (opt_dist != nullptr) {
        double distspan = radtomiles(gcdist(RAD(lat1),
                                            RAD(lon1),
                                            RAD(wpt->latitude),
                                            RAD(wpt->longitude)));
        npts = distspan / max_dist_step;
      }
      if (!std::isfinite(npts) || (npts >= INT_MAX)) {
        fatal(FatalMsg() << MYNAME ": interpolation interval too small!");
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
          wpt_new->SetCreationTime(time1.addMSecs(qRound64(frac * *timespan)));
        } else {
          wpt_new->creation_time = gpsbabel::DateTime();
        }
        linepart(lat1, lon1,
                 wpt->latitude, wpt->longitude,
                 frac,
                 &wpt_new->latitude,
                 &wpt_new->longitude);
        if (altspan.has_value()) {
          wpt_new->altitude = altitude1 + (frac * *altspan);
        } else {
          wpt_new->altitude = unknown_alt;
        }
        if (opt_route != nullptr) {
          route_add_wpt(rte, wpt_new);
        } else {
          track_add_wpt(rte, wpt_new);
        }
      }
    }
    if (opt_route != nullptr) {
      route_add_wpt(rte, wpt);
    } else {
      track_add_wpt(rte, wpt);
    }

    lat1 = wpt->latitude;
    lon1 = wpt->longitude;
    altitude1 = wpt->altitude;
    time1 = wpt->creation_time.toUTC();  // use utc to avoid tz conversions.
  }
}

void InterpolateFilter::init()
{
  char* fm;
  if ((opt_time != nullptr) && (opt_dist != nullptr)) {
    fatal(FatalMsg() << MYNAME ": Can't interpolate on both time and distance.");
  } else if ((opt_time != nullptr) && (opt_route != nullptr)) {
    fatal(FatalMsg() << MYNAME ": Can't interpolate routes on time.");
  } else if (opt_time != nullptr) {
    max_time_step = 1000 * strtod(opt_time, nullptr); // milliseconds
    if (max_time_step <= 0) {
      fatal(FatalMsg() << MYNAME ": interpolation time should be positive!");
    }
  } else if (opt_dist != nullptr) {
    max_dist_step = strtod(opt_dist, &fm);
    if ((*fm == 'k') || (*fm == 'K')) {
      /* distance is kilometers, convert to miles */
      max_dist_step *= kMilesPerKilometer;
    }
    if (max_dist_step <= 0) {
      fatal(FatalMsg() << MYNAME ": interpolation distance should be positive!");
    }
  } else {
    fatal(FatalMsg() << MYNAME ": No interval specified.");
  }
}

#endif // FILTERS_ENABLED
