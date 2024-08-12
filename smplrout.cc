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

#include <cassert>
#include <cstdlib>              // for strtod, strtol
#include <iterator>             // for prev

#include <QDateTime>            // for QDateTime
#include <QHash>                // for QHash
#include <QMap>                 // for QMap

#include "defs.h"
#include "smplrout.h"
#include "grtcirc.h"            // for gcdist, linedist, radtometers, radtomiles, linepart
#include "src/core/datetime.h"  // for DateTime


#if FILTERS_ENABLED
#define MYNAME "simplify"

inline bool operator<(const SimplifyRouteFilter::trackerror& lhs, const SimplifyRouteFilter::trackerror& rhs)
{
  return ((lhs.dist > rhs.dist) || ((lhs.dist == rhs.dist) && (lhs.wptpos < rhs.wptpos)));
}

double SimplifyRouteFilter::compute_track_error(const neighborhood& nb) const
{
  /* if no previous, this is an endpoint and must be preserved. */
  if (nb.prev == nullptr) {
    return kHugeValue;
  }
  const Waypoint* wpt1 = nb.prev;

  /* if no next, this is an endpoint and must be preserved. */
  if (nb.next == nullptr) {
    return kHugeValue;
  }
  const Waypoint* wpt2 = nb.next;

  const Waypoint* wpt3 = nb.wpt;
  double track_error;
  switch (metric) {
  case metric_t::crosstrack:
    track_error = radtomiles(linedist(
                               wpt1->latitude, wpt1->longitude,
                               wpt2->latitude, wpt2->longitude,
                               wpt3->latitude, wpt3->longitude));
    break;
  case metric_t::length:
    track_error = radtomiles(
                    gcdist(wpt1->position(), wpt3->position()) +
                    gcdist(wpt3->position(), wpt2->position()) -
                    gcdist(wpt1->position(), wpt2->position()));
    break;
  case metric_t::relative:
  default: // eliminate false positive warning with g++ 11.3.0: ‘error’ may be used uninitialized in this function [-Wmaybe-uninitialized]
    // if timestamps exist, distance to interpolated point
    if (wpt1->GetCreationTime().isValid() &&
        wpt2->GetCreationTime().isValid() &&
        wpt3->GetCreationTime().isValid() &&
        (wpt1->GetCreationTime() != wpt2->GetCreationTime())) {
      double frac = static_cast<double>(wpt1->GetCreationTime().msecsTo(wpt3->GetCreationTime())) /
                    static_cast<double>(wpt1->GetCreationTime().msecsTo(wpt2->GetCreationTime()));
      auto respos = linepartnew(wpt1->position(),
                                wpt2->position(),
                                frac);
      track_error = radtometers(gcdist(wpt3->position(), respos));
    } else { // else distance to connecting line
      track_error = radtometers(linedist(
                                  wpt1->latitude, wpt1->longitude,
                                  wpt2->latitude, wpt2->longitude,
                                  wpt3->latitude, wpt3->longitude));
    }
    // error relative to horizontal precision
    track_error /= (6 * wpt3->hdop);
    // (hdop->meters following to J. Person at <http://www.developerfusion.co.uk/show/4652/3/>)
    break;
  }
  return track_error;
}

void SimplifyRouteFilter::routesimple_head(const route_head* rte)
{
  /* short-circuit if we already have fewer than the max points */
  if ((limit_basis == limit_basis_t::count) && count >= rte->rte_waypt_ct()) {
    return;
  }

  /* short-circuit if the route is impossible to simplify, too. */
  if (2 >= rte->rte_waypt_ct()) {
    return;
  }

  /* compute all distances */
  /* sort XTE array, lowest XTE last */
  Waypoint* pwpt = nullptr;
  Waypoint* ppwpt = nullptr;
  WaypointList::size_type pos = 0;
  neighborhood nb;
  trackerror te;
  QMap<trackerror, neighborhood> errormap;
  QHash<Waypoint*, trackerror> wpthash;
  for (auto* wpt : rte->waypoint_list) {
    wpt->extra_data = nullptr;

    if (metric == metric_t::relative) {
      // check hdop is available for compute_track_error
      if (wpt->hdop == 0) {
        fatal(MYNAME ": relative needs hdop information.\n");
      }
    }

    if (pwpt != nullptr) {
      nb.wpt = pwpt;
      nb.prev = ppwpt;
      nb.next = wpt;

      te.dist = compute_track_error(nb);
      te.wptpos = pos;

      errormap.insert(te, nb);
      wpthash.insert(pwpt, te);
    }

    ppwpt = pwpt;
    pwpt = wpt;
    ++pos;
  }
  nb.wpt = pwpt;
  nb.prev = ppwpt;
  nb.next = nullptr;

  te.dist = compute_track_error(nb);
  te.wptpos = pos;

  errormap.insert(te, nb);
  wpthash.insert(pwpt, te);

  assert(!errormap.isEmpty());
  double totalerror = errormap.lastKey().dist;

  /* while we still have too many records... */
  while ((!errormap.isEmpty()) &&
         (((limit_basis == limit_basis_t::count) && (count < errormap.size())) ||
          ((limit_basis == limit_basis_t::error) && (totalerror < error)))) {

    /* remove the record with the lowest XTE */
    neighborhood goner = errormap.last();
    goner.wpt->wpt_flags.marked_for_deletion = 1;
    // errormap.remove(lastKey());  // with Qt 5.12.12, 5.15.2 results in asan heap-use-after-free errors in build_extra_tests.sh
    errormap.erase(std::prev(errormap.cend())); // in Qt6 can use cend().
    // wpthash.remove(goner.wpt); // removal not necessary

    /* recompute neighbors of point marked for deletion. */
    if (goner.prev != nullptr) {
      assert(wpthash.contains(goner.prev));
      trackerror terr = wpthash.value(goner.prev);
      assert(errormap.contains(terr));
      neighborhood nbh = errormap.value(terr);
      nbh.next = goner.next;
      errormap.remove(terr);
      terr.dist = compute_track_error(nbh);
      errormap.insert(terr, nbh);
      wpthash.insert(goner.prev, terr);
    }
    if (goner.next != nullptr) {
      assert(wpthash.contains(goner.next));
      trackerror terr = wpthash.value(goner.next);
      assert(errormap.contains(terr));
      neighborhood nbh = errormap.value(terr);
      nbh.prev = goner.prev;
      errormap.remove(terr);
      terr.dist = compute_track_error(nbh);
      errormap.insert(terr, nbh);
      wpthash.insert(goner.next, terr);
    }

    /* compute impact of deleting next point */
    if (limit_basis == limit_basis_t::error) {
      switch (metric) {
      case metric_t::crosstrack:
      case metric_t::relative:
        totalerror = errormap.lastKey().dist;
        break;
      case metric_t::length:
        totalerror += errormap.lastKey().dist;
        break;
      }
    }

  } /* end of too many records loop */
}

void SimplifyRouteFilter::process()
{
  auto common_head_lambda = [this](const route_head* rte)->void {
    routesimple_head(rte);
  };

  auto route_tail_lambda = [](const route_head* rte)->void {
    route_del_marked_wpts(const_cast<route_head*>(rte));
  };
  route_disp_all(common_head_lambda, route_tail_lambda, nullptr);

  auto track_tail_lambda = [](const route_head* rte)->void {
    track_del_marked_wpts(const_cast<route_head*>(rte));
  };
  track_disp_all(common_head_lambda, track_tail_lambda, nullptr);
}

void SimplifyRouteFilter::init()
{
  count = 0;

  if (!countopt && erroropt) {
    limit_basis = limit_basis_t::error;
  } else if (countopt && !erroropt) {
    limit_basis = limit_basis_t::count;
  } else {
    fatal(MYNAME ": You must specify either count or error, but not both.\n");
  }

  if (!lenopt && !relopt) {
    metric = metric_t::crosstrack; /* default */
  } else if (!xteopt && lenopt && !relopt) {
    metric = metric_t::length;
  } else if (!xteopt && !lenopt && relopt) {
    metric = metric_t::relative;
  } else {
    fatal(MYNAME ": You may specify only one of crosstrack, length, or relative.\n");
  }

  switch (limit_basis) {
  case limit_basis_t::count:
    count = strtol(countopt, nullptr, 10);
    break;
  case limit_basis_t::error: {
    if (metric == metric_t::relative) {
      error = strtod(erroropt, nullptr);
    } else {
      int res = parse_distance(erroropt, &error, 1.0, MYNAME);
      if (res == 0) {
        error = 0;
      } else if (res == 2) { /* parameter with unit */
        error = METERS_TO_MILES(error);
      }
    }
  }
  break;
  }
}

#endif // FILTERS_ENABLED
