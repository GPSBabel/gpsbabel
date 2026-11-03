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
#ifndef GOOGLETAKEOUT_H_INCLUDED_
#define GOOGLETAKEOUT_H_INCLUDED_

#include <QJsonObject>        // for QJsonObject
#include <QJsonValue>         // for QJsonValue
#include <QLatin1StringView>  // for QLatin1StringView
#include <QList>              // for QList
#include <QString>            // for QString
#include <QVector>            // for QVector

#include "defs.h"
#include "format.h"           // for Format

using namespace Qt::StringLiterals;

/*
 * Reads Location History JSON files and return each timelineObject
 * that should be processed.
 *
 * TODO: Allow date ranges
 */
/* Read-only Google Timeline Location History gpsbabel Format */
class GoogleTakeoutFormat : public Format
{
public:
  using Format::Format;

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

  void rd_init(const QString& fname) override
  {}
  void read() override;

private:
  /* Constants */

  static constexpr QLatin1StringView TIMELINE_OBJECTS = "timelineObjects"_L1;
  static constexpr QLatin1StringView PLACE_VISIT = "placeVisit"_L1;
  static constexpr QLatin1StringView ACTIVITY_SEGMENT = "activitySegment"_L1;
  static constexpr QLatin1StringView ACTIVITY_TYPE = "activityType"_L1;
  static constexpr QLatin1StringView LOCATION = "location"_L1;
  static constexpr QLatin1StringView LOCATION_LATE7 = "latitudeE7"_L1;
  static constexpr QLatin1StringView LOCATION_LONE7 = "longitudeE7"_L1;
  static constexpr QLatin1StringView NAME = "name"_L1;
  static constexpr QLatin1StringView ADDRESS = "address"_L1;
  static constexpr QLatin1StringView DURATION = "duration"_L1;
  static constexpr QLatin1StringView START_TIMESTAMP = "startTimestamp"_L1;
  static constexpr QLatin1StringView START_LOCATION = "startLocation"_L1;
  static constexpr QLatin1StringView END_TIMESTAMP = "endTimestamp"_L1;
  static constexpr QLatin1StringView END_LOCATION = "endLocation"_L1;
  static constexpr QLatin1StringView TIMESTAMP = "timestamp"_L1;
  static constexpr QLatin1StringView SIMPLE_PATH = "simplifiedRawPath"_L1;
  static constexpr QLatin1StringView POINTS = "points"_L1;
  static constexpr QLatin1StringView WAYPOINT_PATH = "waypointPath"_L1;
  static constexpr QLatin1StringView WAYPOINTS  = "waypoints"_L1;
  // for some reason that probably only a former Google engineer knows,;
  // we use = "latE7"/"lngE7" here instead of "latitudeE7"/"longitudeE7".;
  // +10 points for brevity, but -100 points for inconsistency.;
  static constexpr QLatin1StringView LATE7 = "latE7"_L1;
  static constexpr QLatin1StringView LONE7 = "lngE7"_L1;

  /* Types */

  class GoogleTakeoutInputStream
  {
  public:
    /* Special Member Functions */
    GoogleTakeoutInputStream() = default;
    GoogleTakeoutInputStream(const QString& source) : sources({source}) {}

    /* Member Functions */

    // Returns the next timelineObject, or a null QJsonValue if we're at the end
    QJsonValue next();

  private:
    /* Member Functions */

    static QList<QJsonObject> readJson(const QString& source);
    static QList<QString> readDir(const QString& source);
    void loadSource(const QString& source);

    /* Data Members */

    QList<QString> sources;
    QList<QJsonObject> timelineObjects;
  };

  /* Member Functions */

  static void takeout_fatal(const QString& message);
  static void takeout_warning(const QString& message);
  static Waypoint* takeout_waypoint(int lat_e7, int lon_e7, const QString* shortname, const QString* description, const QString* start_str);
  static bool track_maybe_add_wpt(route_head* route, Waypoint* waypoint);
  static void title_case(QString& title);
  static void add_place_visit(const QJsonObject& placeVisit);
  static int add_activity_segment(const QJsonObject& activitySegment);

  /* Data Members */

  QVector<arglist_t> googletakeout_args;
};

#endif /* GOOGLETAKEOUT_H_INCLUDED_ */
