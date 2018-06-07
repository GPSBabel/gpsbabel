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
#include "defs.h"
#include "filterdefs.h"
#include "sort.h"
#include <cstdlib>

#if FILTERS_ENABLED

SortFilter* SortFilter::fObj = nullptr; // definition required for odr-use.

int SortFilter::sort_comp(const queue* a, const queue* b)
{
  const Waypoint* x1 = (Waypoint*)a;
  const Waypoint* x2 = (Waypoint*)b;

  switch (sort_mode)  {
  case sm_gcid:
    return x1->gc_data->id - x2->gc_data->id;
  case sm_shortname:
    return x1->shortname.compare(x2->shortname);
  case sm_description:
    return x1->description.compare(x2->description);
  case sm_time:
    return x1->GetCreationTime().toTime_t() - x2->GetCreationTime().toTime_t();
  default:
    abort();
    return 0; /* Internal caller error. */
  }
}

void SortFilter::process()
{
  setObj(*this);

  sortqueue(&waypt_head, &sort_comp_glue);
}

void SortFilter::init(const char*)
{
  if (opt_sm_gcid) {
    sort_mode = sm_gcid;
  }
  if (opt_sm_shortname) {
    sort_mode = sm_shortname;
  }
  if (opt_sm_description) {
    sort_mode = sm_description;
  }
  if (opt_sm_time) {
    sort_mode = sm_time;
  }
}

#endif // FILTERS_ENABLED
