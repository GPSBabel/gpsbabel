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

#include "arcdist.h"

#include <cmath>                  // for round
#include <cstdio>                 // for printf, sscanf
#include <cstdlib>                // for strtod
#include <tuple>                  // for tie, tuple

#include <QByteArray>             // for QByteArray
#include <QString>                // for QString
#include <QtGlobal>               // for foreach, qPrintable, qint64

#include "defs.h"
#include "grtcirc.h"              // for RAD, gcdist, linedistprj, radtomi
#include "src/core/datetime.h"    // for DateTime
#include "src/core/logging.h"     // for Fatal
#include "src/core/textstream.h"  // for TextStream


#if FILTERS_ENABLED
#define MYNAME "Arc filter"

#define BADVAL 999999

void ArcDistanceFilter::arcdist_arc_disp_wpt_cb(const Waypoint* arcpt2)
{
  static const Waypoint* arcpt1 = nullptr;
  PositionDeg prjpos;
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
          dist = gcdist(arcpt2->position(), waypointp->position());
          prjpos = arcpt2->position();
          frac = 1.0;
        } else {
          if (waypointp == nullptr) {
            fatal(FatalMsg() << "Internal error. Attempt to project through a waypoint that doesn't exist");
          }
          if (arcpt1 == nullptr) {
            fatal(FatalMsg() << "Internal error: Attempt to project waypoint without predecessor");
          }

          std::tie(dist, prjpos, frac) = linedistprj(arcpt1->position(),
                                                     arcpt2->position(),
                                                     waypointp->position());
        }

        /* convert radians to meters */
        dist = radtometers(dist);

        if (ed->distance > dist) {
          ed->distance = dist;
          if (projectopt) {
            ed->prjpos = prjpos;
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

void ArcDistanceFilter::arcdist_arc_disp_hdr_cb(const route_head* /*unused*/)
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
    QString line;

    gpsbabel::TextStream stream;
    stream.open(arcfileopt.get(), QIODevice::ReadOnly, MYNAME);

    auto* arcpt1 = new Waypoint;
    auto* arcpt2 = new Waypoint;
    arcdist_arc_disp_hdr_cb(nullptr);

    arcpt2->latitude = arcpt2->longitude = BADVAL;
    while (stream.readLineInto(&line)) {
      fileline++;

      auto pound = line.indexOf('#');
      if (pound >= 0) {
        if (line.mid(pound, 6) == u"#break") {
          arcdist_arc_disp_hdr_cb(nullptr);
        }
        line.truncate(pound);
      }

      arcpt2->latitude = arcpt2->longitude = BADVAL;
      int argsfound = sscanf(CSTR(line), "%lf %lf", &arcpt2->latitude, &arcpt2->longitude);

      if ((argsfound != 2) && (line.trimmed().size() > 0)) {
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

    stream.close();
  } else if (rteopt) {
    route_disp_all(arcdist_arc_disp_hdr_cb_f, nullptr, arcdist_arc_disp_wpt_cb_f);
  } else if (trkopt) {
    track_disp_all(arcdist_arc_disp_hdr_cb_f, nullptr, arcdist_arc_disp_wpt_cb_f);
  }

  unsigned removed = 0;
  foreach (Waypoint* wp, *global_waypoint_list) {
    if (wp->extra_data) {
      auto* ed = (extra_data*) wp->extra_data;
      wp->extra_data = nullptr;
      if ((ed->distance >= pos_dist) == !exclopt) {
        wp->wpt_flags.marked_for_deletion = 1;
        removed++;
      } else if (projectopt) {
        wp->SetPosition(ed->prjpos);
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
  del_marked_wpts();
  if (global_opts.verbose_status > 0) {
    printf(MYNAME "-arc: %u waypoint(s) removed.\n", removed);
  }
}

void ArcDistanceFilter::init()
{
  if ((!arcfileopt && !rteopt && !trkopt) ||
      (arcfileopt && (rteopt || trkopt)) ||
      (rteopt && trkopt)) {
    fatal(MYNAME ": Incompatible or incomplete option values!\n");
  }

  pos_dist = 0.0;

  if (distopt) {
    if (parse_distance(distopt, &pos_dist, kMetersPerMile , MYNAME) == 0) {
      fatal(MYNAME ": No distance specified with distance option.");
    }
  }
}

#endif // FILTERS_ENABLED
