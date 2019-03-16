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

#include <cctype>           // for toupper
#include <cstdlib>          // for atoi

#include <QtCore/QtGlobal>  // for foreach

#include "defs.h"
#include "filterdefs.h"
#include "transform.h"


#if FILTERS_ENABLED

#include <cctype>

#define MYNAME "transform"

void TransformFilter::transform_waypoints()
{
  route_head* rte = route_head_alloc();
  switch (current_target) {
  case 'R':
    route_add_head(rte);
    break;
  case 'T':
    track_add_head(rte);
    break;
  }
  foreach (Waypoint* wpt, *global_waypoint_list) {

    wpt = new Waypoint(*wpt);
    switch (current_target) {
    case 'R':
      route_add_wpt(rte, wpt, RPT, name_digits);
      break;
    case 'T':
      track_add_wpt(rte, wpt, RPT, name_digits);
      break;
    }
  }
}

void TransformFilter::transform_rte_disp_hdr_cb(const route_head* rte)
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

void TransformFilter::transform_trk_disp_hdr_cb(const route_head* trk)
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

void TransformFilter::transform_any_disp_wpt_cb(const Waypoint* wpt)
{
  Waypoint* temp = new Waypoint(*wpt);
  if (current_target == 'R') {
    route_add_wpt(current_rte, temp, current_namepart, name_digits);
  } else if (current_target == 'T') {
    track_add_wpt(current_trk, temp, current_namepart, name_digits);
  } else {
    waypt_add(temp);
  }
}

void TransformFilter::transform_routes()
{
  WayptFunctor<TransformFilter> transform_any_disp_wpt_cb_f(this, &TransformFilter::transform_any_disp_wpt_cb);
  RteHdFunctor<TransformFilter> transform_rte_disp_hdr_cb_f(this, &TransformFilter::transform_rte_disp_hdr_cb);

  route_disp_all(transform_rte_disp_hdr_cb_f, nullptr, transform_any_disp_wpt_cb_f);
}

void TransformFilter::transform_tracks()
{
  WayptFunctor<TransformFilter> transform_any_disp_wpt_cb_f(this, &TransformFilter::transform_any_disp_wpt_cb);
  RteHdFunctor<TransformFilter> transform_trk_disp_hdr_cb_f(this, &TransformFilter::transform_trk_disp_hdr_cb);

  track_disp_all(transform_trk_disp_hdr_cb_f, nullptr, transform_any_disp_wpt_cb_f);
}

/*******************************************************************************
* %%%        global callbacks called by gpsbabel main process              %%% *
*******************************************************************************/

void TransformFilter::process()
{
  int delete_after = (opt_delete && (*opt_delete == '1')) ? 1 : 0;

  use_src_name = (opt_rpt_name && (*opt_rpt_name == '1')) ? 1 : 0;

  name_digits = 3;
  if (rpt_name_digits && *rpt_name_digits) {
    name_digits = atoi(rpt_name_digits);
  }

  if (opt_waypts != nullptr) {
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
  if (opt_routes != nullptr) {
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
  if (opt_tracks != nullptr) {
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

#endif // FILTERS_ENABLED
