/*

    Transformation filter for GPS data.

    Copyright (C) 2006 Olaf Klein, o.b.klein@gpsbabel.org

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
#include <stdlib.h>

#if FILTERS_ENABLED

#include <ctype.h>

#define MYNAME "transform"

static char current_target;
static route_head* current_trk;
static route_head* current_rte;

static char* opt_routes, *opt_tracks, *opt_waypts, *opt_delete, *rpt_name_digits, *opt_rpt_name;
static QString current_namepart;

static int name_digits, use_src_name;

static char RPT[] = "RPT";

static
arglist_t transform_args[] = {
  {
    "wpt", &opt_waypts, "Transform track(s) or route(s) into waypoint(s) [R/T]", NULL,
    ARGTYPE_STRING, ARG_NOMINMAX
  },
  {
    "rte", &opt_routes, "Transform waypoint(s) or track(s) into route(s) [W/T]", NULL,
    ARGTYPE_STRING, ARG_NOMINMAX
  },
  {
    "trk", &opt_tracks, "Transform waypoint(s) or route(s) into tracks(s) [W/R]", NULL,
    ARGTYPE_STRING, ARG_NOMINMAX
  },
  {
    "rptdigits", &rpt_name_digits, "Number of digits in generated names", NULL,
    ARGTYPE_INT, "2", NULL
  },
  {
    "rptname", &opt_rpt_name, "Use source name for route point names", "N",
    ARGTYPE_BOOL, ARG_NOMINMAX
  },
  {
    "del", &opt_delete, "Delete source data after transformation", "N",
    ARGTYPE_BOOL, ARG_NOMINMAX
  },
  ARG_TERMINATOR
};

static void
transform_waypoints(void)
{
  route_head* rte;

  rte = route_head_alloc();
  switch (current_target) {
  case 'R':
    route_add_head(rte);
    break;
  case 'T':
    track_add_head(rte);
    break;
  }
#if NEWQ
  foreach(Waypoint* wpt, waypt_list) {
#else
  queue* elem, *tmp;
  QUEUE_FOR_EACH(&waypt_head, elem, tmp) {
    Waypoint* wpt = (Waypoint*) elem;
#endif

    wpt = new Waypoint(*wpt);
    switch (current_target) {
    case 'R':
      route_add_wpt_named(rte, wpt, RPT, name_digits);
      break;
    case 'T':
      track_add_wpt_named(rte, wpt, RPT, name_digits);
      break;
    }
  }
}

static void
transform_rte_disp_hdr_cb(const route_head* rte)
{
  current_namepart = RPT;
  if (!rte->rte_name.isEmpty() && use_src_name) {
    current_namepart = rte->rte_name;
  }
  if (current_target == 'T') {
    current_trk = route_head_alloc();
    track_add_head(current_trk);
    if (!rte->rte_name.isEmpty()) {
      current_trk->rte_desc = QString("Generated from route %1").arg(rte->rte_name);
      current_trk->rte_name = rte->rte_name; /* name the new trk */
    }
  }
}

static void
transform_trk_disp_hdr_cb(const route_head* trk)
{
  current_namepart = RPT;
  if (!trk->rte_name.isEmpty() && use_src_name) {
    current_namepart = trk->rte_name;
  }
  if (current_target == 'R') {
    current_rte = route_head_alloc();
    route_add_head(current_rte);
    if (!trk->rte_name.isEmpty()) {
      current_rte->rte_desc = "Generated from track ";
      current_rte->rte_desc += trk->rte_name;
      current_rte->rte_name = trk->rte_name; /* name the new rte */
    }
  }
}

static void
transform_any_disp_wpt_cb(const Waypoint* wpt)
{
  Waypoint* temp = new Waypoint(*wpt);
  if (current_target == 'R') {
    route_add_wpt_named(current_rte, temp, current_namepart, name_digits);
  } else if (current_target == 'T') {
    track_add_wpt_named(current_trk, temp, current_namepart, name_digits);
  } else {
    waypt_add(temp);
  }
}

static void
transform_routes(void)
{
  route_disp_all(transform_rte_disp_hdr_cb, NULL, transform_any_disp_wpt_cb);
}

static void
transform_tracks(void)
{
  track_disp_all(transform_trk_disp_hdr_cb, NULL, transform_any_disp_wpt_cb);
}

/*******************************************************************************
* %%%        global callbacks called by gpsbabel main process              %%% *
*******************************************************************************/

static void
transform_init(const char* args)
{
}

static void
transform_deinit(void)
{
}

static void
transform_process(void)
{
  int delete_after = (opt_delete && (*opt_delete == '1')) ? 1 : 0;

  use_src_name = (opt_rpt_name && (*opt_rpt_name == '1')) ? 1 : 0;

  name_digits = 3;
  if (rpt_name_digits && *rpt_name_digits) {
    name_digits = atoi(rpt_name_digits);
  }

  if (opt_waypts != NULL) {
    current_target = 'W';
    switch (toupper(*opt_waypts)) {
    case 'R':
      transform_routes();
      if (delete_after) {
        route_flush_all_routes();
      }
      break;
    case 'T':
      transform_tracks();
      if (delete_after) {
        route_flush_all_tracks();
      }
      break;
    default:
      fatal(MYNAME ": Invalid option value (%s)!\n", opt_waypts);
    }
  }
  if (opt_routes != NULL) {
    current_target = 'R';
    switch (toupper(*opt_routes)) {
    case 'W':
      transform_waypoints();
      if (delete_after) {
        waypt_flush_all();
      }
      break;
    case 'T':
      transform_tracks();
      if (delete_after) {
        route_flush_all_tracks();
      }
      break;
    default:
      fatal(MYNAME ": Invalid option value (%s)!\n", opt_routes);
    }
  }
  if (opt_tracks != NULL) {
    current_target = 'T';
    switch (toupper(*opt_tracks)) {
    case 'W':
      transform_waypoints();
      if (delete_after) {
        waypt_flush_all();
      }
      break;
    case 'R':
      transform_routes();
      if (delete_after) {
        route_flush_all_routes();
      }
      break;
    default:
      fatal(MYNAME ": Invalid option value (%s)!\n", opt_tracks);
    }
  }
}

/*******************************************************************************/

filter_vecs_t transform_vecs = {
  transform_init,
  transform_process,
  transform_deinit,
  NULL,
  transform_args
};

/*******************************************************************************/

#endif // FILTERS_ENABLED
