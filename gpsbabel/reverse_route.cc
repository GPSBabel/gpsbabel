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
#include "defs.h"
#include "filterdefs.h"

#if FILTERS_ENABLED

#define MYNAME "Route reversal filter"

static int prev_new_trkseg;

static
arglist_t reverse_route_args[] = {
  ARG_TERMINATOR
};

/*
 * reverse_route_wpt fixes up the waypoint flag new_trkseg
 */
static void reverse_route_wpt(const Waypoint* waypointp)
{

  /* Cast away const-ness */
  Waypoint* wpp = (Waypoint*) waypointp;

  int curr_new_trkseg;

  curr_new_trkseg = waypointp->wpt_flags.new_trkseg;
  wpp->wpt_flags.new_trkseg = prev_new_trkseg;
  prev_new_trkseg = curr_new_trkseg;
}

void
reverse_route_head(const route_head* rte)
{
  route_reverse(rte);
  prev_new_trkseg = 1;
}

void
reverse_route_process(void)
{
  track_disp_all(reverse_route_head, NULL, reverse_route_wpt);
  route_disp_all(reverse_route_head, NULL, NULL);
}

void
reverse_route_init(const char* args)
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

void
reverse_route_deinit(void)
{
  /* do nothing */
}

filter_vecs_t reverse_route_vecs = {
  reverse_route_init,
  reverse_route_process,
  reverse_route_deinit,
  NULL,
  reverse_route_args
};
#endif
