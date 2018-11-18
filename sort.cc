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
#include <QtCore/QString>    // for QString
#include <cstdlib>           // for abort

extern queue my_route_head;
extern queue my_track_head;

#if FILTERS_ENABLED

template <class T>
inline int sgn(T v)
{
// Returns 1 if v > 0, -1 if v < 0, and 0 if v is zero
  return (v > T(0)) - (v < T(0));
}

template <class T>
inline int cmp(T a, T b)
{
// Returns 1 if a > b, -1 if a < b, and 0 if a = b
// note possible overflow in computing sgn(a-b) is avoided.
  return (a > b) - (a < b);
}

int SortFilter::sort_comp_wpt(const queue* a, const queue* b)
{
  const Waypoint* x1 = reinterpret_cast<const Waypoint*>(a);
  const Waypoint* x2 = reinterpret_cast<const Waypoint*>(b);

  switch (wpt_sort_mode)  {
  case SortModeWpt::description:
    return x1->description.compare(x2->description);
  case SortModeWpt::gcid:
    return cmp(x1->gc_data->id, x2->gc_data->id);
  case SortModeWpt::shortname:
    return x1->shortname.compare(x2->shortname);
  case SortModeWpt::time:
    return sgn(x2->GetCreationTime().msecsTo(x1->GetCreationTime()));
  default:
    abort();
    return 0; /* Internal caller error. */
  }
}

int SortFilter::sort_comp_rh(const queue* a, const queue* b)
{
  const route_head* x1 = reinterpret_cast<const route_head*>(a);
  const route_head* x2 = reinterpret_cast<const route_head*>(b);

  switch (rh_sort_mode)  {
  case SortModeRteHd::description:
    return x1->rte_desc.compare(x2->rte_desc);
  case SortModeRteHd::name:
    return x1->rte_name.compare(x2->rte_name);
  case SortModeRteHd::number:
    return cmp(x1->rte_num, x2->rte_num);
  default:
    abort();
    return 0; /* Internal caller error. */
  }
}

int SortFilter::SortCompWptFunctor::operator()(const queue* a, const queue* b)
{
  return that->sort_comp_wpt(a, b);
}

int SortFilter::SortCompRteHdFunctor::operator()(const queue* a, const queue* b)
{
  return that->sort_comp_rh(a, b);
}

void SortFilter::process()
{
  SortCompWptFunctor sort_comp_wpt_f(*this);
  SortCompRteHdFunctor sort_comp_rh_f(*this);

  if (wpt_sort_mode != SortModeWpt::none) {
    sortqueue(&waypt_head, sort_comp_wpt_f);
  }
  if (rte_sort_mode != SortModeRteHd::none) {
    rh_sort_mode = rte_sort_mode;
    sortqueue(&my_route_head, sort_comp_rh_f);
  }
  if (trk_sort_mode != SortModeRteHd::none) {
    rh_sort_mode = trk_sort_mode;
    sortqueue(&my_track_head, sort_comp_rh_f);
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
