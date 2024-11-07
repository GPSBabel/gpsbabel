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

#include <QJsonObject>     // for QJsonObject
#include <QJsonValue>      // for QJsonValue
#include <QList>           // for QList
#include <QString>         // for QString
#include <QVector>         // for QVector

#include "defs.h"
#include "format.h"        // for Format

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

  static constexpr char TIMELINE_OBJECTS[] = "timelineObjects";
  static constexpr char16_t PLACE_VISIT[] = u"placeVisit";
  static constexpr char16_t ACTIVITY_SEGMENT[] = u"activitySegment";
  static constexpr char16_t ACTIVITY_TYPE[] = u"activityType";
  static constexpr char16_t LOCATION[] = u"location";
  static constexpr char16_t LOCATION_LATE7[] = u"latitudeE7";
  static constexpr char16_t LOCATION_LONE7[] = u"longitudeE7";
  static constexpr char16_t NAME[] = u"name";
  static constexpr char16_t ADDRESS[] = u"address";
  static constexpr char16_t DURATION[] = u"duration";
  static constexpr char16_t START_TIMESTAMP[] = u"startTimestamp";
  static constexpr char16_t START_LOCATION[] = u"startLocation";
  static constexpr char16_t END_TIMESTAMP[] = u"endTimestamp";
  static constexpr char16_t END_LOCATION[] = u"endLocation";
  static constexpr char16_t TIMESTAMP[] = u"timestamp";
  static constexpr char16_t SIMPLE_PATH[] = u"simplifiedRawPath";
  static constexpr char16_t POINTS[] = u"points";
  static constexpr char16_t WAYPOINT_PATH[] = u"waypointPath";
  static constexpr char16_t WAYPOINTS [] = u"waypoints";
  // for some reason that probably only a former Google engineer knows,;
  // we use[] = u"latE7"/"lngE7" here instead of "latitudeE7"/"longitudeE7".;
  // +10 points for brevity, but -100 points for inconsistency.;
  static constexpr char16_t LATE7[] = u"latE7";
  static constexpr char16_t LONE7[] = u"lngE7";

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
