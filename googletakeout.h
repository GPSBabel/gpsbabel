/*
  Support reading Google Takeout Timeline Location History JSON format.

  Copyright (C) 2023 Tyler MacDonald, tyler@macdonald.name
  Copyright (C) 2023 Robert Lipe, robertlipe+source@gpsbabel.org

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
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301,
  USA.
*/
#ifndef _GOOGLETAKEOUT_H
#define _GOOGLETAKEOUT_H

#include <QString>
#include <QVector>

#include "defs.h"
#include "format.h"

/*
 * Reads Location History JSON files and return each timelineObject
 * that should be processed.
 *
 * TODO: Allow date ranges
 */
class GoogleTakeoutInputStream
{
public:
  GoogleTakeoutInputStream() = default;
  GoogleTakeoutInputStream(const QString& source) : sources(source) {}
  // Returns the next timelineObject, or a null QJsonValue if we're at the end
  QJsonValue next();
private:
  QList<QString> sources;
  QList<QJsonObject> timelineObjects;
  void loadSource(const QString& source);
};

/* Read-only Google Timeline Location History gpsbabel Format */
class GoogleTakeoutFormat : public Format
{
public:
  /* Member functions */
  QVector<arglist_t>* get_args() override
  {
    return &googletakeout_args;
  }

  ff_type get_type() const override
  {
   return ff_type_file; 
  }

  QVector<ff_cap> get_cap() const override
  {
    return { ff_cap_read, ff_cap_read, ff_cap_none };
  }

  void rd_init(const QString& fname) override;
  void read() override;
 
private:
  GoogleTakeoutInputStream inputStream;
  QVector<arglist_t> googletakeout_args;

  void add_place_visit(const QJsonObject& placeVisit);
  int add_activity_segment(const QJsonObject& activitySegment);
  void title_case(QString& title);

  const QString PLACE_VISIT = QStringLiteral("placeVisit");
  const QString ACTIVITY_SEGMENT = QStringLiteral("activitySegment");
  const QString ACTIVITY_TYPE = QStringLiteral("activityType");
  const QString LOCATION = QStringLiteral("location");
  const QString LOCATION_LATE7 = QStringLiteral("latitudeE7");
  const QString LOCATION_LONE7 = QStringLiteral("longitudeE7");
  const QString NAME = QStringLiteral("name");
  const QString ADDRESS = QStringLiteral("address");
  const QString DURATION = QStringLiteral("duration");
  const QString START_TIMESTAMP = QStringLiteral("startTimestamp");
  const QString START_LOCATION = QStringLiteral("startLocation");
  const QString END_TIMESTAMP = QStringLiteral("endTimestamp");
  const QString END_LOCATION = QStringLiteral("endLocation");
  const QString TIMESTAMP = QStringLiteral("timestamp");
  const QString SIMPLE_PATH = QStringLiteral("simplifiedRawPath");
  const QString POINTS = QStringLiteral("points");
  const QString WAYPOINT_PATH = QStringLiteral("waypointPath");
  const QString WAYPOINTS  = QStringLiteral("waypoints");
  // for some reason that probably only a former Google engineer knows,);
  // we use = QStringLiteral("latE7"/"lngE7" here instead of "latitudeE7"/"longitudeE7".);
  // +10 points for brevity, but -100 points for inconsistency.);
  const QString LATE7 = QStringLiteral("latE7");
  const QString LONE7 = QStringLiteral("lngE7");
};


#endif /* _GOOGLETAKEOUT_H */
