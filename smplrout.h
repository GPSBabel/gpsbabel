/*
    Route / track simplification filter

    Copyright (C) 2002-2023 Robert Lipe, robertlipe+source@gpsbabel.org

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

/* The following comments are from an email I wrote to Paul Fox in November
 * 2005 in an attempt to explain how the cross track error minimization method
 * works  (RLP 2005):
 *
 * It's pretty simple, really: for each triplet of vertices A-B-C, we compute
 * how much cross-track error we'd introduce by going straight from A to C
 * (the maximum cross-track error for that segment is the height of the
 * triangle ABC, measured between vertex B and edge AC.)  If we need to remove
 * 40 points, we just sort the points by that metric and remove the 40
 * smallest ones.
 *
 * It's actually a little more complicated than that, because removing a
 * point changes the result for its two nearest neighbors.  When we remove
 * one, we recompute the neighbors and then sort them back into the list
 * at their new locations.
 *
 * As you can see, this hasn't been shown to be an optimal algorithm.  After
 * all, removing one high-xte point might create two very low-xte neighbors
 * that more than make up for the high xte of the original point.  I believe
 * the optimal algorithm would be NP-complete, but I haven't proven it.  This
 * is really more of a heuristic than anything, but it seems to work well for
 * the routes I've fed it.
 *
 * Not in that email was an explanation of how the pathlength-based calculation
 * works: instead of computing the height of the triangle, we just compute
 * the difference in pathlength from taking the direct route.  This case,
 * too, is only a heuristic, as it's possible that a different combination or
 * order of point removals could lead to a smaller number of points with less
 * reduction in path length.  In the case of pathlength, error is cumulative.
*/

/*
    History:

	2008/08/20: added "relative" option, (Carsten Allefeld, carsten.allefeld@googlemail.com)
*/

#ifndef SMPLROUT_H_INCLUDED_
#define SMPLROUT_H_INCLUDED_

#include <QList>     // for QList
#include <QString>               // for QString
#include <QVector>               // for QVector

#include "defs.h"
#include "filter.h"  // for Filter
#include "option.h"  // for OptionBool, OptionCString


#if FILTERS_ENABLED

class SimplifyRouteFilter:public Filter
{
public:

  /* Types */

  struct trackerror {
    double dist;
    WaypointList::size_type wptpos;
  };

  /* Member Functions */

  QVector<arglist_t>* get_args() override
  {
    return &args;
  }
  void init() override;
  void process() override;

private:

  /* Types */

  enum class limit_basis_t {count, error};
  enum class metric_t {crosstrack, length, relative};

  struct neighborhood {
    Waypoint* wpt;
    Waypoint* prev;
    Waypoint* next;
  };

  /* Constants */

  static constexpr double kHugeValue = 2000000000;

  /* Member Functions */

  double compute_track_error(const neighborhood& nb) const;
  void routesimple_head(const route_head* rte);

  /* Data Members */

  int count = 0;
  double error = 0;
  limit_basis_t limit_basis{limit_basis_t::error};
  metric_t metric{metric_t::crosstrack};

  OptionCString countopt;
  OptionCString erroropt;
  OptionBool xteopt;
  OptionBool lenopt;
  OptionBool relopt;

  QVector<arglist_t> args = {
    {
      "count", &countopt,  "Maximum number of points in route",
      nullptr, ARGTYPE_INT | ARGTYPE_BEGIN_REQ | ARGTYPE_BEGIN_EXCL, "1", nullptr, nullptr
    },
    {
      "error", &erroropt, "Maximum error", nullptr,
      ARGTYPE_STRING | ARGTYPE_END_REQ | ARGTYPE_END_EXCL, "0", nullptr, nullptr
    },
    {
      "crosstrack", &xteopt, "Use cross-track error (default)", nullptr,
      ARGTYPE_BOOL | ARGTYPE_BEGIN_EXCL, ARG_NOMINMAX, nullptr
    },
    {
      "length", &lenopt, "Use arclength error", nullptr,
      ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    {
      "relative", &relopt, "Use relative error", nullptr,
      ARGTYPE_BOOL | ARGTYPE_END_EXCL, ARG_NOMINMAX, nullptr
    },
  };

};

#endif // FILTERS_ENABLED
#endif // SMPLROUT_H_INCLUDED_
