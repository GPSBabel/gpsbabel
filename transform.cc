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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

 */

#include <cctype>           // for toupper

#include <QtGlobal>         // for foreach

#include "defs.h"
#include "transform.h"


#if FILTERS_ENABLED


void TransformFilter::transform_waypoints()
{
  auto* rte = new route_head;
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
    if (timeless) {
      wpt->SetCreationTime(gpsbabel::DateTime());
    }
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
    current_trk = new route_head;
    track_add_head(current_trk);
    if (!rte->rte_name.isEmpty()) {
      current_trk->rte_desc = QStringLiteral("Generated from route %1").arg(rte->rte_name);
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
    current_rte = new route_head;
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
  auto* temp = new Waypoint(*wpt);
  if (timeless) {
    temp->SetCreationTime(gpsbabel::DateTime());
  }
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
  timeless = opt_timeless;

  bool delete_after = opt_delete;

  use_src_name = opt_rpt_name;

  name_digits = 3;
  if (!rpt_name_digits.isEmpty()) {
    name_digits = rpt_name_digits.get_result();
  }

  if (opt_waypts) {
    current_target = 'W';
    if (opt_waypts.get().startsWith('R', Qt::CaseInsensitive)) {
      transform_routes();
      if (delete_after) {
        route_flush_all_routes();
      }
    } else if (opt_waypts.get().startsWith('T', Qt::CaseInsensitive)) {
      transform_tracks();
      if (delete_after) {
        route_flush_all_tracks();
      }
    } else {
      gbFatal("Invalid option value (%s)!\n", gbLogCStr(opt_waypts));
    }
  }
  if (opt_routes) {
    current_target = 'R';
    if (opt_routes.get().startsWith('W', Qt::CaseInsensitive)) {
      transform_waypoints();
      if (delete_after) {
        waypt_flush_all();
      }
    } else if (opt_routes.get().startsWith('T', Qt::CaseInsensitive)) {
      transform_tracks();
      if (delete_after) {
        route_flush_all_tracks();
      }
    } else {
      gbFatal("Invalid option value (%s)!\n", gbLogCStr(opt_routes));
    }
  }
  if (opt_tracks) {
    current_target = 'T';
    if (opt_tracks.get().startsWith('W', Qt::CaseInsensitive)) {
      transform_waypoints();
      if (delete_after) {
        waypt_flush_all();
      }
    } else if (opt_tracks.get().startsWith('R', Qt::CaseInsensitive)) {
      transform_routes();
      if (delete_after) {
        route_flush_all_routes();
      }
    } else {
      gbFatal("Invalid option value (%s)!\n", gbLogCStr(opt_tracks));
    }
  }
}

#endif // FILTERS_ENABLED
