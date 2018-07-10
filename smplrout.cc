/*
    Route / track simplification filter

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
    Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111 USA

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

#include "defs.h"
#include "filterdefs.h"
#include "grtcirc.h"
#include "smplrout.h"
#include <cstdlib>

#if FILTERS_ENABLED
#define MYNAME "simplify"

#define sqr(a) ((a)*(a))

void SimplifyRouteFilter::free_xte(struct xte* xte_rec)
{
  xfree(xte_rec->intermed);
}

#define HUGEVAL 2000000000

void SimplifyRouteFilter::routesimple_waypt_pr(const Waypoint* wpt)
{
  if (!cur_rte) {
    return;
  }
  xte_recs[xte_count].ordinal=xte_count;
  xte_recs[xte_count].intermed = (struct xte_intermed*) xmalloc(sizeof(struct xte_intermed));
  xte_recs[xte_count].intermed->wpt = wpt;
  xte_recs[xte_count].intermed->xte_rec = xte_recs+xte_count;
  xte_recs[xte_count].intermed->next = nullptr;
  xte_recs[xte_count].intermed->prev = tmpprev;
  if (tmpprev) {
    tmpprev->next = xte_recs[xte_count].intermed;
  }
  tmpprev = xte_recs[xte_count].intermed;
  xte_count++;
}

void SimplifyRouteFilter::compute_xte(struct xte* xte_rec)
{
  const Waypoint* wpt3 = xte_rec->intermed->wpt;
  double reslat, reslon;
  /* if no previous, this is an endpoint and must be preserved. */
  if (!xte_rec->intermed->prev) {
    xte_rec->distance = HUGEVAL;
    return;
  }
  const Waypoint* wpt1 = xte_rec->intermed->prev->wpt;

  /* if no next, this is an endpoint and must be preserved. */
  if (!xte_rec->intermed->next) {
    xte_rec->distance = HUGEVAL;
    return;
  }
  const Waypoint* wpt2 = xte_rec->intermed->next->wpt;

  if (xteopt) {
    xte_rec->distance = radtomiles(linedist(
                                     wpt1->latitude, wpt1->longitude,
                                     wpt2->latitude, wpt2->longitude,
                                     wpt3->latitude, wpt3->longitude));
  } else if (lenopt) {
    xte_rec->distance = radtomiles(
                          gcdist(wpt1->latitude, wpt1->longitude,
                                 wpt3->latitude, wpt3->longitude) +
                          gcdist(wpt3->latitude, wpt3->longitude,
                                 wpt2->latitude, wpt2->longitude) -
                          gcdist(wpt1->latitude, wpt1->longitude,
                                 wpt2->latitude, wpt2->longitude));
  } else if (relopt) {
    if (wpt3->hdop == 0) {
      fatal(MYNAME ": relative needs hdop information.\n");
    }
    // if timestamps exist, distance to interpolated point
    if (wpt1->GetCreationTime() != wpt2->GetCreationTime()) {
      double frac = (double)(wpt3->GetCreationTime().toTime_t() - wpt1->GetCreationTime().toTime_t()) /
        (wpt2->GetCreationTime().toTime_t() - wpt1->GetCreationTime().toTime_t());
      linepart(wpt1->latitude, wpt1->longitude,
               wpt2->latitude, wpt2->longitude,
               frac, &reslat, &reslon);
      xte_rec->distance = radtometers(gcdist(
                                        wpt3->latitude, wpt3->longitude,
                                        reslat, reslon));
    } else { // else distance to connecting line
      xte_rec->distance = radtometers(linedist(
                                        wpt1->latitude, wpt1->longitude,
                                        wpt2->latitude, wpt2->longitude,
                                        wpt3->latitude, wpt3->longitude));
    }
    // error relative to horizontal precision
    xte_rec->distance /= (6 * wpt3->hdop);
    // (hdop->meters following to J. Person at <http://www.developerfusion.co.uk/show/4652/3/>)

  }
}

int SimplifyRouteFilter::compare_xte(const void* a, const void* b)
{
  double distdiff = ((struct xte*)a)->distance -
                    ((struct xte*)b)->distance;
  int priodiff = ((struct xte*)a)->intermed->wpt->route_priority -
                 ((struct xte*)b)->intermed->wpt->route_priority;

  if (HUGEVAL == ((struct xte*)a)->distance) {
    return -1;
  }

  if (HUGEVAL == ((struct xte*)b)->distance) {
    return 1;
  }

  if (priodiff < 0) {
    return 1;
  }
  if (priodiff > 0) {
    return -1;
  }
  if (distdiff < 0) {
    return 1;
  }
  if (distdiff > 0) {
    return -1;
  }
  return 0;
}

void SimplifyRouteFilter::routesimple_head(const route_head* rte)
{
  cur_rte = nullptr;
  /* build array of XTE/wpt xref records */
  xte_count = 0;
  tmpprev = nullptr;
  totalerror = 0;

  /* short-circuit if we already have fewer than the max points */
  if (countopt && count >= rte->rte_waypt_ct) {
    return;
  }

  /* short-circuit if the route is impossible to simplify, too. */
  if (2 >= rte->rte_waypt_ct) {
    return;
  }

  xte_recs = (struct xte*) xcalloc(rte->rte_waypt_ct, sizeof(struct xte));
  cur_rte = rte;

}

void SimplifyRouteFilter::shuffle_xte(struct xte* xte_rec)
{
  struct xte tmp_xte;
  while (xte_rec > xte_recs && compare_xte(xte_rec, xte_rec-1) < 0) {
    tmp_xte.distance = xte_rec->distance;
    tmp_xte.ordinal = xte_rec->ordinal;
    tmp_xte.intermed = xte_rec->intermed;
    xte_rec->distance = xte_rec[-1].distance;
    xte_rec->ordinal = xte_rec[-1].ordinal;
    xte_rec->intermed = xte_rec[-1].intermed;
    xte_rec->intermed->xte_rec = xte_rec;
    xte_rec--;
    xte_rec->distance = tmp_xte.distance;
    xte_rec->ordinal = tmp_xte.ordinal;
    xte_rec->intermed = tmp_xte.intermed;
    xte_rec->intermed->xte_rec = xte_rec;
  }
  while (xte_rec - xte_recs < xte_count-2 &&
         compare_xte(xte_rec, xte_rec+1) > 0) {
    tmp_xte.distance = xte_rec->distance;
    tmp_xte.ordinal = xte_rec->ordinal;
    tmp_xte.intermed = xte_rec->intermed;
    xte_rec->distance = xte_rec[1].distance;
    xte_rec->ordinal = xte_rec[1].ordinal;
    xte_rec->intermed = xte_rec[1].intermed;
    xte_rec->intermed->xte_rec = xte_rec;
    xte_rec++;
    xte_rec->distance = tmp_xte.distance;
    xte_rec->ordinal = tmp_xte.ordinal;
    xte_rec->intermed = tmp_xte.intermed;
    xte_rec->intermed->xte_rec = xte_rec;
  }
}

void SimplifyRouteFilter::routesimple_tail(const route_head* rte)
{
  int i;
  if (!cur_rte) {
    return;
  }

  /* compute all distances */
  for (i = 0; i < xte_count ; i++) {
    compute_xte(xte_recs+i);
  }


  /* sort XTE array, lowest XTE last */
  qsort(xte_recs, xte_count, sizeof(struct xte), compare_xte);

  for (i = 0; i < xte_count; i++) {
    xte_recs[i].intermed->xte_rec = xte_recs+i;
  }
  // Ensure totalerror starts with the distance between first and second points
  // and not the zero-init.  From a June 25, 2014  thread titled "Simplify
  // Filter: GPSBabel removes one trackpoint..."  I never could repro it it
  // with the sample data, so there is no automated test case, but Steve's
  // fix is "obviously" right here.
  if (xte_count >= 1) {
    totalerror = xte_recs[xte_count-1].distance;
  }

  /* while we still have too many records... */
  while ((xte_count) && ((countopt && count < xte_count) || (erroropt && totalerror < error))) {
    i = xte_count - 1;
    /* remove the record with the lowest XTE */
    if (erroropt) {
      if (xteopt || relopt) {
        if (i > 1) {
          totalerror = xte_recs[i-1].distance;
        } else {
          totalerror = xte_recs[i].distance;
        }
      }
      if (lenopt) {
        totalerror += xte_recs[i].distance;
      }
    }
    (*waypt_del_fnp)((route_head*)(void*)rte,
                     (Waypoint*)(void*)(xte_recs[i].intermed->wpt));
    delete (Waypoint*)(void*)(xte_recs[i].intermed->wpt);

    if (xte_recs[i].intermed->prev) {
      xte_recs[i].intermed->prev->next = xte_recs[i].intermed->next;
      compute_xte(xte_recs[i].intermed->prev->xte_rec);
      shuffle_xte(xte_recs[i].intermed->prev->xte_rec);
    }
    if (xte_recs[i].intermed->next) {
      xte_recs[i].intermed->next->prev = xte_recs[i].intermed->prev;
      compute_xte(xte_recs[i].intermed->next->xte_rec);
      shuffle_xte(xte_recs[i].intermed->next->xte_rec);
    }
    xte_count--;
    free_xte(xte_recs+xte_count);
    /* end of loop */
  }
  if (xte_count) {
    do {
      xte_count--;
      free_xte(xte_recs+xte_count);
    } while (xte_count);
  }
  xfree(xte_recs);
}

void SimplifyRouteFilter::process()
{
  WayptFunctor<SimplifyRouteFilter> routesimple_waypt_pr_f(this, &SimplifyRouteFilter::routesimple_waypt_pr);
  RteHdFunctor<SimplifyRouteFilter> routesimple_head_f(this, &SimplifyRouteFilter::routesimple_head);
  RteHdFunctor<SimplifyRouteFilter> routesimple_tail_f(this, &SimplifyRouteFilter::routesimple_tail);

  waypt_del_fnp = route_del_wpt;
  route_disp_all(routesimple_head_f, routesimple_tail_f, routesimple_waypt_pr_f);

  waypt_del_fnp = track_del_wpt;
  track_disp_all(routesimple_head_f, routesimple_tail_f, routesimple_waypt_pr_f);
}

void SimplifyRouteFilter::init()
{
  count = 0;

  if (!!countopt == !!erroropt) {
    fatal(MYNAME ": You must specify either count or error, but not both.\n");
  }
  if ((!!xteopt + !!lenopt + !!relopt) > 1) {
    fatal(MYNAME ": You may specify only one of crosstrack, length, or relative.\n");
  }
  if (!xteopt && !lenopt && !relopt) {
    xteopt = (char*) "";
  }

  if (countopt) {
    count = atol(countopt);
  }
  if (erroropt) {
    int res = parse_distance(erroropt, &error, 1.0, MYNAME);
    if (res == 0) {
      error = 0;
    } else if (res == 2) { /* parameter with unit */
      error = METERS_TO_MILES(error);
    }
  }
}

#endif // FILTERS_ENABLED
