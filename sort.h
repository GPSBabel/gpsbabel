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
#include "queue.h"   // for queue

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
  typedef enum {
    sm_unknown = 0,
    sm_gcid,
    sm_shortname,
    sm_description,
    sm_time
  } sort_mode_;

  sort_mode_ sort_mode = sm_shortname;	/* How are we sorting these? */

  char* opt_sm_gcid, *opt_sm_shortname, *opt_sm_description, *opt_sm_time;

  arglist_t args[5] = {
    {
      "gcid", &opt_sm_gcid, "Sort by numeric geocache ID",
      nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    {
      "shortname", &opt_sm_shortname, "Sort by waypoint short name",
      nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    {
      "description", &opt_sm_description, "Sort by waypoint description",
      nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    {
      "time", &opt_sm_time, "Sort by time",
      nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    ARG_TERMINATOR
  };

  int sort_comp(const queue* a, const queue* b);

  static int sort_comp_glue(const queue* a, const queue* b)
  {
    return fObj->sort_comp(a, b);
  }
  static void setObj(SortFilter& obj)
  {
    fObj = &obj;
  }
  static SortFilter* fObj;
};
#endif // FILTERS_ENABLED
#endif // SORT_H_INCLUDED_
