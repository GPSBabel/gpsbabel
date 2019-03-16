/*
    Route reversal filter.

    Copyright (C) 2003 Robert Lipe, robertlipe+source@gpsbabel.org

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
#include <algorithm>     // for reverse

#include <QtCore/QList>  // for QList<>::iterator

#include "defs.h"
#include "filterdefs.h"
#include "reverse_route.h"

#if FILTERS_ENABLED

#define MYNAME "Route reversal filter"

/*
 * reverse_route_wpt fixes up the waypoint flag new_trkseg
 */
void ReverseRouteFilter::reverse_route_wpt(const Waypoint* waypointp)
{

  /* Cast away const-ness */
  Waypoint* wpp = const_cast<Waypoint*>(waypointp);

  int curr_new_trkseg = waypointp->wpt_flags.new_trkseg;
  wpp->wpt_flags.new_trkseg = prev_new_trkseg;
  prev_new_trkseg = curr_new_trkseg;
}

void ReverseRouteFilter::reverse_route_head(const route_head* rte_hd)
{
  /* Cast away const-ness */
  auto rh = const_cast<route_head*>(rte_hd);
  std::reverse(rh->waypoint_list.begin(), rh->waypoint_list.end());
  prev_new_trkseg = 1;
}

void ReverseRouteFilter::process()
{
  WayptFunctor<ReverseRouteFilter> reverse_route_wpt_f(this, &ReverseRouteFilter::reverse_route_wpt);
  RteHdFunctor<ReverseRouteFilter> reverse_route_head_f(this, &ReverseRouteFilter::reverse_route_head);

  track_disp_all(reverse_route_head_f, nullptr, reverse_route_wpt_f);
  route_disp_all(reverse_route_head_f, nullptr, nullptr);
}

void ReverseRouteFilter::init()
{
  switch (global_opts.objective) {
  case rtedata:
    break;
  case trkdata:
    break;
  default:
    fatal(MYNAME ": This filter only works in track "
          "or route (-t or -r) mode.\n");
  }
}

#endif
