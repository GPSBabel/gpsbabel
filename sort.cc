/*
    Arbitrary Sorting Filter(s)

    Copyright (C) 2004 Robert Lipe, robertlipe+source@gpsbabel.org

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

#include <QtCore/QDateTime>     // for QDateTime
#include <QtCore/QString>       // for operator<, QString

#include "defs.h"
#include "src/core/datetime.h"  // for DateTime
#include "filterdefs.h"
#include "sort.h"

#if FILTERS_ENABLED
#define MYNAME "sort"


bool SortFilter::sort_comp_wpt_by_description(const Waypoint* a, const Waypoint* b)
{
  return a->description < b->description;
}

bool SortFilter::sort_comp_wpt_by_gcid(const Waypoint* a, const Waypoint* b)
{
  return a->gc_data->id < b->gc_data->id;
}

bool SortFilter::sort_comp_wpt_by_shortname(const Waypoint* a, const Waypoint* b)
{
  return a->shortname < b->shortname;
}

bool SortFilter::sort_comp_wpt_by_time(const Waypoint* a, const Waypoint* b)
{
  return a->GetCreationTime() < b->GetCreationTime();
}

bool SortFilter::sort_comp_rh_by_description(const route_head* a, const route_head* b)
{
  return a->rte_desc < b->rte_desc;
}

bool SortFilter::sort_comp_rh_by_name(const route_head* a, const route_head* b)
{
  return a->rte_name < b->rte_name;
}

bool SortFilter::sort_comp_rh_by_number(const route_head* a, const route_head* b)
{
  return a->rte_num < b->rte_num;
}

void SortFilter::process()
{
  if (wpt_sort_mode != SortModeWpt::none) {
    switch (wpt_sort_mode) {
    case SortModeWpt::description:
      waypt_sort(sort_comp_wpt_by_description);
      break;
    case SortModeWpt::gcid:
      waypt_sort(sort_comp_wpt_by_gcid);
      break;
    case SortModeWpt::shortname:
      waypt_sort(sort_comp_wpt_by_shortname);
      break;
    case SortModeWpt::time:
      waypt_sort(sort_comp_wpt_by_time);
      break;
    default:
      fatal(MYNAME ": unknown waypoint sort mode.");
    }
  }
  
  if (rte_sort_mode != SortModeRteHd::none) {
    switch (rte_sort_mode)  {
    case SortModeRteHd::description:
      route_sort(SortFilter::sort_comp_rh_by_description);
      break;
    case SortModeRteHd::name:
      route_sort(sort_comp_rh_by_name);
      break;
    case SortModeRteHd::number:
      route_sort(sort_comp_rh_by_number);
      break;
    default:
      fatal(MYNAME ": unknown route sort mode.");
    }
  }
  if (trk_sort_mode != SortModeRteHd::none) {
    switch (trk_sort_mode)  {
    case SortModeRteHd::description:
      track_sort(sort_comp_rh_by_description);
      break;
    case SortModeRteHd::name:
      track_sort(sort_comp_rh_by_name);
      break;
    case SortModeRteHd::number:
      track_sort(sort_comp_rh_by_number);
      break;
    default:
      fatal(MYNAME ": unknown track sort mode.");
    }
  }
}

void SortFilter::init()
{
  // sort waypts by
  if (opt_sm_description) {
    wpt_sort_mode = SortModeWpt::description;
  }
  if (opt_sm_gcid) {
    wpt_sort_mode = SortModeWpt::gcid;
  }
  if (opt_sm_shortname) {
    wpt_sort_mode = SortModeWpt::shortname;
  }
  if (opt_sm_time) {
    wpt_sort_mode = SortModeWpt::time;
  }

  // sort routes by
  if (opt_sm_rtedesc) {
    rte_sort_mode = SortModeRteHd::description;
  }
  if (opt_sm_rtename) {
    rte_sort_mode = SortModeRteHd::name;
  }
  if (opt_sm_rtenum) {
    rte_sort_mode = SortModeRteHd::number;
  }

  // sort tracks by
  if (opt_sm_trkdesc) {
    trk_sort_mode = SortModeRteHd::description;
  }
  if (opt_sm_trkname) {
    trk_sort_mode = SortModeRteHd::name;
  }
  if (opt_sm_trknum) {
    trk_sort_mode = SortModeRteHd::number;
  }
}

#endif // FILTERS_ENABLED
