/*
    exact duplicate point filter utility.

    Copyright (C) 2002-2023 Robert Lipe, robertlipe+source@gpsbabel.org

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

#include "duplicate.h"

#include <algorithm>             // for stable_sort

#include <QDateTime>             // for QDateTime
#include <QList>                 // for QList, QList<>::iterator, QList<>::const_iterator
#include <QMultiHash>            // for QMultiHash

#include "defs.h"
#include "geocache.h"            // for Geocache
#include "src/core/datetime.h"   // for DateTime


#if FILTERS_ENABLED

#define MYNAME "duplicate"
/*

It looks odd that we have different comparisons for date and index.
	If exported if a < b return 1
	if index    if a < b return -1

The reason is that we want to sort in reverse order by date, but forward
order by index.  So if we have four records:

    date      index
    June 24    0
    June 25    1
    June 25    2
    June 24    3

we want to sort them like this:

    date      index
    June 25    1
    June 25    2
    June 24    0
    June 24    3

Thus, the first point we come across is the latest point, but if we
have two points with the same export date/time, we will first see the
one with the smaller index (i.e. the first of those two points that we
came across while importing waypoints.)

In the (common) case that we have no exported dates, the dates will all
be zero so the sort will end up being an expensive no-op.  However, the
complexity of this filter is dominated by other concerns.
*/

void DuplicateFilter::init()
{
  if (!lcopt && !snopt) {
    fatal(MYNAME ": one or both of the shortname and location options are required.\n");
  }
}

void DuplicateFilter::process()
{
  int delete_flag; // &delete_flag != nullptr

  auto wptlist = *global_waypoint_list;

  auto compare_lambda = [](const Waypoint* wa, const Waypoint* wb)->bool {
    return wa->gc_data->exported > wb->gc_data->exported;
  };
  std::stable_sort(wptlist.begin(), wptlist.end(), compare_lambda);

  QMultiHash<QString, Waypoint*> wpthash;
  for (Waypoint* waypointp : wptlist) {
    waypointp->extra_data = nullptr;

    QString key;
    if (lcopt) {
      /* The degrees2ddmm stuff is a feeble attempt to
       * get everything rounded the same way in a precision
       * that's "close enough" for determining duplicates.
       */
      key = QStringLiteral("%1%2")
        .arg(degrees2ddmm(waypointp->latitude), 11, 'f', 3)
        .arg(degrees2ddmm(waypointp->longitude), 11, 'f', 3);
    }
  
    if (snopt) {
      key.append(waypointp->shortname);
    }

    wpthash.insert(key, waypointp);
  }

  const QList<QString> keys = wpthash.uniqueKeys();
  for (const auto& key : keys) {
    const QList<Waypoint*> values = wpthash.values(key);
    if (values.size() > 1) {
      Waypoint* wptfirst = values.last(); // first inserted
      if (correct_coords) {
        Waypoint* wptlast = values.front(); // last inserted
        wptfirst->latitude = wptlast->latitude;
        wptfirst->longitude = wptlast->longitude;
      }
      for (auto it = values.cbegin(); it != values.cend(); ++it) {
        Waypoint* wpt = *it;
        if (purge_duplicates || (wpt != wptfirst)) {
          wpt->extra_data = &delete_flag;
        }
      }
    }
  }

  // For lineary complexity build a new list from the points we keep.
  WaypointList oldlist;
  waypt_swap(oldlist);
  
  for (Waypoint* wpt : qAsConst(oldlist)) {
    if (wpt->extra_data == nullptr) {
      waypt_add(wpt);
    } else {
      delete wpt;
    }
  }
}

#endif
