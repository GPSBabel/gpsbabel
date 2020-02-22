/*
    Distance from point to arc filter

    Copyright (C) 2002-2014 Robert Lipe, robertlipe+source@gpsbabel.org

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

#include <cmath>                // for round
#include <cstdio>               // for printf, sscanf
#include <cstdlib>              // for strtod
#include <cstring>              // for strchr, strlen, strncmp, strspn

#include <QtCore/QByteArray>    // for QByteArray
#include <QtCore/QString>       // for QString
#include <QtCore/QtGlobal>      // for foreach, qPrintable, qint64

#include "defs.h"
#include "arcdist.h"
#include "gbfile.h"             // for gbfclose, gbfgetstr, gbfopen, gbfile
#include "grtcirc.h"            // for RAD, gcdist, linedistprj, radtomi
#include "src/core/datetime.h"  // for DateTime
#include "src/core/logging.h"   // for Fatal


#if FILTERS_ENABLED
#define MYNAME "Arc filter"

#define BADVAL 999999

void ArcDistanceFilter::arcdist_arc_disp_wpt_cb(const Waypoint* arcpt2)
{
  static const Waypoint* arcpt1 = nullptr;
  double prjlat;
  double prjlon;
  double frac;

  if (arcpt2 && arcpt2->latitude != BADVAL && arcpt2->longitude != BADVAL &&
      (ptsopt || (arcpt1 &&
                  (arcpt1->latitude != BADVAL && arcpt1->longitude != BADVAL)))) {
    foreach (Waypoint* waypointp, *global_waypoint_list) {
      extra_data* ed;
      if (waypointp->extra_data) {
        ed = (extra_data*) waypointp->extra_data;
      } else {
        ed = new extra_data;
        ed->distance = BADVAL;
      }
      if (ed->distance == BADVAL || projectopt || ed->distance >= pos_dist) {
        double dist;
        if (ptsopt) {
          dist = gcdist(RAD(arcpt2->latitude),
                        RAD(arcpt2->longitude),
                        RAD(waypointp->latitude),
                        RAD(waypointp->longitude));
          prjlat = arcpt2->latitude;
          prjlon = arcpt2->longitude;
          frac = 1.0;
        } else {
          if (waypointp == nullptr) {
            Fatal() << "Internal error. Attempt to project through a waypoint that doesn't exist";
          }
          if (arcpt1 == nullptr) {
            Fatal() << "Internal error: Attempt to project waypoint without predecessor";
          }

          dist = linedistprj(arcpt1->latitude,
                             arcpt1->longitude,
                             arcpt2->latitude,
                             arcpt2->longitude,
                             waypointp->latitude,
                             waypointp->longitude,
                             &prjlat, &prjlon, &frac);
        }

        /* convert radians to float point statute miles */
        dist = radtomiles(dist);

        if (ed->distance > dist) {
          ed->distance = dist;
          if (projectopt) {
            ed->prjlatitude = prjlat;
            ed->prjlongitude = prjlon;
            ed->frac = frac;
            ed->arcpt1 = arcpt1;
            ed->arcpt2 = arcpt2;
          }
        }
        waypointp->extra_data = ed;
      }
    }
  }
  arcpt1 = arcpt2;
}

void ArcDistanceFilter::arcdist_arc_disp_hdr_cb(const route_head*)
{
  /* Set arcpt1 to NULL */
  arcdist_arc_disp_wpt_cb(nullptr);
}

void ArcDistanceFilter::process()
{
  WayptFunctor<ArcDistanceFilter> arcdist_arc_disp_wpt_cb_f(this, &ArcDistanceFilter::arcdist_arc_disp_wpt_cb);
  RteHdFunctor<ArcDistanceFilter> arcdist_arc_disp_hdr_cb_f(this, &ArcDistanceFilter::arcdist_arc_disp_hdr_cb);

  if (arcfileopt) {
    int fileline = 0;
    char* line;

    gbfile* file_in = gbfopen(arcfileopt, "r", MYNAME);

    auto* arcpt1 = new Waypoint;
    auto* arcpt2 = new Waypoint;
    arcdist_arc_disp_hdr_cb(nullptr);

    arcpt2->latitude = arcpt2->longitude = BADVAL;
    while ((line = gbfgetstr(file_in))) {
      fileline++;

      char* pound = strchr(line, '#');
      if (pound) {
        if (0 == strncmp(pound, "#break", 6)) {
          arcdist_arc_disp_hdr_cb(nullptr);
        }
        *pound = '\0';
      }

      arcpt2->latitude = arcpt2->longitude = BADVAL;
      int argsfound = sscanf(line, "%lf %lf", &arcpt2->latitude, &arcpt2->longitude);

      if (argsfound != 2 && strspn(line, " \t\n") < strlen(line)) {
        warning(MYNAME ": Warning: Arc file contains unusable vertex on line %d.\n", fileline);
      } else {
        Waypoint* arcpttmp = arcpt1;
        arcdist_arc_disp_wpt_cb(arcpt2);
        arcpt1 = arcpt2;
        arcpt2 = arcpttmp;
      }
    }
    delete arcpt1;
    delete arcpt2;

    gbfclose(file_in);
  } else if (rteopt) {
    route_disp_all(arcdist_arc_disp_hdr_cb_f, nullptr, arcdist_arc_disp_wpt_cb_f);
  } else if (trkopt) {
    track_disp_all(arcdist_arc_disp_hdr_cb_f, nullptr, arcdist_arc_disp_wpt_cb_f);
  }

  unsigned removed = 0;
  foreach (Waypoint* wp, *global_waypoint_list) {
    auto* ed = (extra_data*) wp->extra_data;
    wp->extra_data = nullptr;
    if (ed) {
      if ((ed->distance >= pos_dist) == (exclopt == nullptr)) {
        waypt_del(wp);
        delete wp;
        removed++;
      } else if (projectopt) {
        wp->longitude = ed->prjlongitude;
        wp->latitude = ed->prjlatitude;
        wp->route_priority = 1;
        if (!arcfileopt &&
            (ed->arcpt2->altitude != unknown_alt) &&
            (ptsopt || (ed->arcpt1->altitude != unknown_alt))) {
          /* Interpolate altitude */
          if (ptsopt) {
            wp->altitude = ed->arcpt2->altitude;
          } else {
            wp->altitude = ed->arcpt1->altitude +
                           ed->frac * (ed->arcpt2->altitude - ed->arcpt1->altitude);
          }
        }
        if (trkopt &&
            (ed->arcpt2->GetCreationTime().isValid()) &&
            (ptsopt || (ed->arcpt1->GetCreationTime().isValid()))) {
          /* Interpolate time */
          if (ptsopt) {
            wp->SetCreationTime(ed->arcpt2->GetCreationTime());
          } else {
            // Apply the multiplier to the difference between the times
            // of the two points.  Add that to the first for the
            // interpolated time.
            qint64 span =
              ed->arcpt1->GetCreationTime().msecsTo(ed->arcpt2->GetCreationTime());
            qint64 offset = std::round(ed->frac * span);
            wp->SetCreationTime(ed->arcpt1->GetCreationTime().addMSecs(offset));
          }
        }
        if (global_opts.debug_level >= 1) {
          warning("Including waypoint %s at dist:%f lat:%f lon:%f\n",
                  qPrintable(wp->shortname), ed->distance, wp->latitude, wp->longitude);
        }
      }
      delete ed;
    }
  }
  if (global_opts.verbose_status > 0) {
    printf(MYNAME "-arc: %u waypoint(s) removed.\n", removed);
  }
}

void ArcDistanceFilter::init()
{
  char* fm;

  if ((!arcfileopt && !rteopt && !trkopt) ||
      (arcfileopt && (rteopt || trkopt)) ||
      (rteopt && trkopt)) {
    fatal(MYNAME ": Incompatible or incomplete option values!\n");
  }

  pos_dist = 0;

  if (distopt) {
    pos_dist = strtod(distopt, &fm);

    if ((*fm == 'k') || (*fm == 'K')) {
      /* distance is kilometers, convert to mile */
      pos_dist *= kMilesPerKilometer;
    }
  }
}

#endif // FILTERS_ENABLED
