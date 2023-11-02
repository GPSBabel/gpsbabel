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

#include <QList>                 // for QList, QList<>::iterator, QList<>::const_iterator
#include <QMultiHash>            // for QMultiHash
#include <QtCore>                // for qAsConst

#include "defs.h"


#if FILTERS_ENABLED

#define MYNAME "duplicate"

void DuplicateFilter::init()
{
  if (!lcopt && !snopt) {
    fatal(MYNAME ": one or both of the shortname and location options are required.\n");
  }
}

void DuplicateFilter::process()
{
  QMultiHash<QString, Waypoint*> wpthash;
  for (Waypoint* waypointp : qAsConst(*global_waypoint_list)) {

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
          wpt->wpt_flags.marked_for_deletion = 1;
        }
      }
    }
  }
  del_marked_wpts();
}

#endif
