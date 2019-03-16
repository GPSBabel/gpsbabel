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

#ifndef SORT_H_INCLUDED_
#define SORT_H_INCLUDED_

#include "defs.h"    // for ARGTYPE_BOOL, ARG_NOMINMAX, arglist_t, ARG_TERMI...
#include "filter.h"  // for Filter

#if FILTERS_ENABLED

class SortFilter:public Filter
{
public:
  arglist_t* get_args() override
  {
    return args;
  }
  void init() override;
  void process() override;

private:
  enum class SortModeWpt {
    none,
    description,
    gcid,
    shortname,
    time
  };

  SortModeWpt wpt_sort_mode = SortModeWpt::none;	/* How are we sorting these? */

  enum class SortModeRteHd {
    none,
    description,
    name,
    number
  };

  SortModeRteHd rte_sort_mode = SortModeRteHd::none;	/* How are we sorting these? */
  SortModeRteHd trk_sort_mode = SortModeRteHd::none;	/* How are we sorting these? */

  char* opt_sm_gcid, *opt_sm_shortname, *opt_sm_description, *opt_sm_time;
  char* opt_sm_rtenum, *opt_sm_rtename, *opt_sm_rtedesc;
  char* opt_sm_trknum, *opt_sm_trkname, *opt_sm_trkdesc;

  arglist_t args[11] = {
    {
      "description", &opt_sm_description, "Sort waypoints by description",
      nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    {
      "gcid", &opt_sm_gcid, "Sort waypoints by numeric geocache ID",
      nullptr, ARGTYPE_BEGIN_EXCL | ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    {
      "shortname", &opt_sm_shortname, "Sort waypoints by short name",
      nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    {
      "time", &opt_sm_time, "Sort waypoints by time",
      nullptr, ARGTYPE_END_EXCL | ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    {
      "rtedesc", &opt_sm_rtedesc, "Sort routes by description",
      nullptr, ARGTYPE_END_EXCL | ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    {
      "rtename", &opt_sm_rtename, "Sort routes by name",
      nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    {
      "rtenum", &opt_sm_rtenum, "Sort routes by number",
      nullptr, ARGTYPE_BEGIN_EXCL | ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    {
      "trkdesc", &opt_sm_trkdesc, "Sort tracks by description",
      nullptr, ARGTYPE_END_EXCL | ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    {
      "trkname", &opt_sm_trkname, "Sort tracks by name",
      nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    {
      "trknum", &opt_sm_trknum, "Sort tracks by number",
      nullptr, ARGTYPE_BEGIN_EXCL | ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    ARG_TERMINATOR
  };

  static bool sort_comp_wpt_by_description(const Waypoint* a, const Waypoint* b);
  static bool sort_comp_wpt_by_gcid(const Waypoint* a, const Waypoint* b);
  static bool sort_comp_wpt_by_shortname(const Waypoint* a, const Waypoint* b);
  static bool sort_comp_wpt_by_time(const Waypoint* a, const Waypoint* b);
  static bool sort_comp_rh_by_description(const route_head* a, const route_head* b);
  static bool sort_comp_rh_by_name(const route_head* a, const route_head* b);
  static bool sort_comp_rh_by_number(const route_head* a, const route_head* b);

};
#endif // FILTERS_ENABLED
#endif // SORT_H_INCLUDED_
