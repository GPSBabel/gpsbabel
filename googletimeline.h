/*
  Support reading the device-local Google Maps Timeline export ("Timeline.json").

  Copyright (C) 2026 Tyler MacDonald, tyler@macdonald.name

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
#ifndef GOOGLETIMELINE_H_INCLUDED_
#define GOOGLETIMELINE_H_INCLUDED_

#include <QJsonArray>           // for QJsonArray
#include <QJsonObject>          // for QJsonObject
#include <QLatin1StringView>    // for QLatin1StringView
#include <QString>              // for QString
#include <QVector>              // for QVector

#include "defs.h"
#include "format.h"             // for Format
#include "src/core/datetime.h"  // for DateTime

using namespace Qt::StringLiterals;

/*
 * Reads the device-local Google Maps Timeline export (a single "Timeline.json"
 * with a top-level "semanticSegments" array), the format Google switched to after
 * retiring the Takeout Location History download. For the older Takeout structure
 * (a "timelineObjects" array, or per-year folders) use the "googletakeout" format.
 *
 * Read-only.
 */
class GoogleTimelineFormat : public Format
{
public:
  using Format::Format;

  /* Member functions */
  QVector<arglist_t>* get_args() override
  {
    return &googletimeline_args;
  }

  ff_type get_type() const override
  {
    return ff_type_file;
  }

  QVector<ff_cap> get_cap() const override
  {
    /* waypoints (visits), tracks (paths/activities), no routes */
    return { ff_cap_read, ff_cap_read, ff_cap_none };
  }

  void rd_init(const QString& fname) override
  {}
  void read() override;

private:
  /* Constants — top level */
  static constexpr QLatin1StringView SEMANTIC_SEGMENTS = "semanticSegments"_L1;
  static constexpr QLatin1StringView START_TIME = "startTime"_L1;
  static constexpr QLatin1StringView END_TIME = "endTime"_L1;

  /* Constants — the three semanticSegment sub-types */
  static constexpr QLatin1StringView VISIT = "visit"_L1;
  static constexpr QLatin1StringView ACTIVITY = "activity"_L1;
  static constexpr QLatin1StringView TIMELINE_PATH = "timelinePath"_L1;

  /* Constants — fields within the sub-types */
  static constexpr QLatin1StringView TOP_CANDIDATE = "topCandidate"_L1;
  static constexpr QLatin1StringView PLACE_LOCATION = "placeLocation"_L1;
  static constexpr QLatin1StringView LATLNG = "latLng"_L1;
  static constexpr QLatin1StringView SEMANTIC_TYPE = "semanticType"_L1;
  static constexpr QLatin1StringView PLACE_ID = "placeId"_L1;
  static constexpr QLatin1StringView POINT = "point"_L1;
  static constexpr QLatin1StringView TIME = "time"_L1;
  static constexpr QLatin1StringView START = "start"_L1;
  static constexpr QLatin1StringView END = "end"_L1;
  static constexpr QLatin1StringView TYPE = "type"_L1;
  static constexpr QLatin1StringView PARKING = "parking"_L1;
  static constexpr QLatin1StringView LOCATION = "location"_L1;

  static const QString nullString;

  /* Member functions */
  static void timeline_fatal(const QString& message);
  static void timeline_warning(const QString& message);
  // Parse a "lat°, lng°" decimal-degree string. Returns false (and leaves lat/lon
  // untouched) on a malformed value.
  static bool parse_latlng(const QString& s, double& lat, double& lon);
  static gpsbabel::DateTime parse_time(const QString& s);
  static Waypoint* make_waypoint(double lat, double lon, const QString& shortname,
                                 const QString& description, const QString& time_str);
  static bool track_maybe_add_wpt(route_head* route, Waypoint* waypoint);
  static void title_case(QString& title);
  // Returns true if a waypoint was added, false if the visit had no usable
  // coordinate and was skipped.
  static bool add_visit(const QJsonObject& visit, const QString& start_time);
  static int add_activity(const QJsonObject& activity, const QString& start_time,
                          const QString& end_time);
  // Returns true if a parking waypoint was added, false if the activity had no
  // parking location or an unusable one.
  static bool add_parking(const QJsonObject& activity);
  static int add_timeline_path(const QJsonArray& path, const QString& track_name);

  /* Data Members */
  QVector<arglist_t> googletimeline_args;
};

#endif /* GOOGLETIMELINE_H_INCLUDED_ */
