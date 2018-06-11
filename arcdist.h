/*
    Distance from point to arc filter

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

#ifndef ARCDIST_H_INCLUDED_
#define ARCDIST_H_INCLUDED_

#include "defs.h"    // for ARG_NOMINMAX, ARGTYPE_BOOL, Waypoint (ptr only)
#include "filter.h"  // for Filter

#if FILTERS_ENABLED

class ArcDistanceFilter:public Filter
{
public:
  arglist_t* get_args() override
  {
    return args;
  }
  void init() override;
  void process() override;

private:
  double pos_dist;
  char* distopt = nullptr;
  char* arcfileopt = nullptr;
  char* rteopt = nullptr;
  char* trkopt = nullptr;
  char* exclopt = nullptr;
  char* ptsopt = nullptr;
  char* projectopt = nullptr;

  typedef struct {
    double distance;
    double prjlatitude, prjlongitude;
    double frac;
    Waypoint* arcpt1, * arcpt2;
  } extra_data;

  arglist_t args[8] = {
    {
      "file", &arcfileopt,  "File containing vertices of arc",
      nullptr, ARGTYPE_FILE, ARG_NOMINMAX, nullptr
    },
    {
      "rte", &rteopt, "Route(s) are vertices of arc",
      nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    {
      "trk", &trkopt, "Track(s) are vertices of arc",
      nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    {
      "distance", &distopt, "Maximum distance from arc",
      nullptr, ARGTYPE_FLOAT | ARGTYPE_REQUIRED, ARG_NOMINMAX, nullptr
    },
    {
      "exclude", &exclopt, "Exclude points close to the arc", nullptr,
      ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    {
      "points", &ptsopt, "Use distance from vertices not lines",
      nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    {
      "project", &projectopt, "Move waypoints to its projection on lines or vertices",
      nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    ARG_TERMINATOR
  };

  void arcdist_arc_disp_wpt_cb(const Waypoint* arcpt2);
  void arcdist_arc_disp_hdr_cb(const route_head*);

};
#endif // FILTERS_ENABLED
#endif // ARCDIST_H_INCLUDED_
