/*
    Distance Between Points Filter(s)

    Copyright (C) 2002 Robert Lipe, robertlipe+source@gpsbabel.org

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

#ifndef POSITION_H_INCLUDED_
#define POSITION_H_INCLUDED_

#include "defs.h"    // for route_head (ptr only), ARG_NOMINMAX, ARGTYPE_FLOAT
#include "filter.h"  // for Filter

#if FILTERS_ENABLED

class PositionFilter:public Filter
{
public:
  arglist_t* get_args() override
  {
    return args;
  }
  void init() override;
  void process() override;

private:
  route_head* cur_rte = nullptr;

  double pos_dist;
  double max_diff_time;
  char* distopt = nullptr;
  char* timeopt = nullptr;
  char* purge_duplicates = nullptr;
  bool check_time;

  arglist_t args[4] = {
    {
      "distance", &distopt, "Maximum positional distance",
      nullptr, ARGTYPE_FLOAT | ARGTYPE_REQUIRED, ARG_NOMINMAX, nullptr
    },
    {
      "all", &purge_duplicates,
      "Suppress all points close to other points",
      nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    {
      "time", &timeopt, "Maximum time in seconds between two points",
      nullptr, ARGTYPE_FLOAT | ARGTYPE_REQUIRED, ARG_NOMINMAX, nullptr
    },
    ARG_TERMINATOR
  };

  class WptRecord
  {
  public:
    Waypoint* wpt{nullptr};
    bool deleted{false};

    explicit WptRecord(Waypoint* w) : wpt(w) {}
  };

  double gc_distance(double lat1, double lon1, double lat2, double lon2);
  void position_runqueue(WaypointList* waypt_list, int qtype);
  void position_process_any_route(const route_head* rh, int type);
  void position_process_rte(const route_head* rh);
  void position_process_trk(const route_head* rh);

};
#endif // FILTERS_ENABLED
#endif // POSITION_H_INCLUDED_
