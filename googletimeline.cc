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

#include "googletimeline.h"

#include <QChar>                // for QChar
#include <QDateTime>            // for QDateTime
#include <QDebug>               // for QDebug
#include <QIODevice>            // for operator|, QIODevice
#include <QJsonArray>           // for QJsonArray, QJsonArray::const_iterator
#include <QJsonDocument>        // for QJsonDocument
#include <QJsonObject>          // for QJsonObject
#include <QJsonParseError>      // for QJsonParseError, QJsonParseError::NoError
#include <QJsonValue>           // for QJsonValue
#include <QStringList>          // for QStringList
#include <Qt>                   // for ISODateWithMs, CaseInsensitive

#include "src/core/datetime.h"  // for DateTime
#include "src/core/file.h"      // for File
#include "src/core/logging.h"   // for Debug, FatalMsg, Warning


void GoogleTimelineFormat::timeline_fatal(const QString& message)
{
  gbFatal(FatalMsg() << message);
}

void GoogleTimelineFormat::timeline_warning(const QString& message)
{
  Warning() << message;
}

/* Parse a "lat°, lng°" decimal-degree string (e.g. "37.4277646°, -122.1404993°").
 * Returns false and leaves lat/lon untouched on a malformed value.
 */
bool GoogleTimelineFormat::parse_latlng(const QString& s, double& lat, double& lon)
{
  QString cleaned = s;
  cleaned.remove(QChar(0x00B0)); // strip the degree signs
  const QStringList parts = cleaned.split(',');
  if (parts.size() != 2) {
    return false;
  }
  bool ok_lat = false;
  bool ok_lon = false;
  const double parsed_lat = parts.at(0).trimmed().toDouble(&ok_lat);
  const double parsed_lon = parts.at(1).trimmed().toDouble(&ok_lon);
  if (!ok_lat || !ok_lon) {
    return false;
  }
  lat = parsed_lat;
  lon = parsed_lon;
  return true;
}

gpsbabel::DateTime GoogleTimelineFormat::parse_time(const QString& s)
{
  // Timeline times are ISO-8601 with milliseconds and a UTC offset, e.g.
  // "2024-11-22T11:17:53.000-05:00". The absolute instant is unambiguous; gpsbabel
  // stores it as UTC.
  return QDateTime::fromString(s, Qt::ISODateWithMs);
}

Waypoint* GoogleTimelineFormat::make_waypoint(
  double lat,
  double lon,
  const QString* shortname,
  const QString* description,
  const QString& time_str)
{
  auto* waypoint = new Waypoint();
  waypoint->latitude = lat;
  waypoint->longitude = lon;
  if (shortname != nullptr && !shortname->isEmpty()) {
    waypoint->shortname = *shortname;
  }
  if (description != nullptr && !description->isEmpty()) {
    waypoint->description = *description;
  }
  if (!time_str.isEmpty()) {
    waypoint->SetCreationTime(parse_time(time_str));
  }
  return waypoint;
}

bool GoogleTimelineFormat::track_maybe_add_wpt(route_head* route, Waypoint* waypoint)
{
  if (waypoint->latitude == 0 && waypoint->longitude == 0) {
    if (global_opts.debug_level >= 2) {
      Debug(2) << "Track " << route->rte_name << "@" <<
        waypoint->creation_time.toPrettyString() <<
        ": Dropping point with no lat/long";
    }
    delete waypoint; // as we're dropping it, gpsbabel won't clean it up later
    return false;
  }
  track_add_wpt(route, waypoint);
  return true;
}

void GoogleTimelineFormat::title_case(QString& title)
{
  bool new_word = true;
  for (auto& chr : title) {
    if (chr == '_' || chr == ' ') {
      new_word = true;
      if (chr == '_') {
        chr = ' ';
      }
    } else if (new_word) {
      new_word = false;
      chr = chr.toUpper();
    } else {
      chr = chr.toLower();
    }
  }
}

void GoogleTimelineFormat::read()
{
  if (global_opts.debug_level >= 4) {
    Debug(4) << "reading " << fname;
  }
  auto* ifd = new gpsbabel::File(fname);
  ifd->open(QIODevice::ReadOnly | QIODevice::Text);
  const QString content = ifd->readAll();
  QJsonParseError error{};
  const QJsonDocument doc = QJsonDocument::fromJson(content.toUtf8(), &error);
  if (error.error != QJsonParseError::NoError) {
    timeline_fatal(
      QString("JSON parse error in ") + ifd->fileName() + ": " + error.errorString()
    );
  }

  const QJsonObject root = doc.object();
  const QJsonValue segmentsIn = root.value(SEMANTIC_SEGMENTS);
  if (!segmentsIn.isArray()) {
    timeline_fatal(
      ifd->fileName() + " is missing the required \"" + SEMANTIC_SEGMENTS +
      "\" array. If this is an older Google Takeout export (a \"timelineObjects\" "
      "array, or per-year folders), use the \"googletakeout\" format instead."
    );
  }
  ifd->close();
  delete ifd;

  const QJsonArray segments = segmentsIn.toArray();
  int visits = 0;
  int activities = 0;
  int paths = 0;
  int points = 0;
  for (const auto&& segmentRef : segments) {
    const QJsonObject segment = segmentRef.toObject();
    const QString startTime = segment[START_TIME].toString();
    const QString endTime = segment[END_TIME].toString();
    /*
     * A semanticSegment carries exactly one of: a "visit" (a place, -> waypoint),
     * an "activity" (a trip start/end, -> track), or a "timelinePath" (a raw
     * point trail, -> track). Some carry only "timelineMemory" and no coordinates.
     */
    if (segment.contains(VISIT)) {
      add_visit(segment[VISIT].toObject(), startTime);
      ++visits;
      ++points;
    } else if (segment.contains(ACTIVITY)) {
      points += add_activity(segment[ACTIVITY].toObject(), startTime, endTime);
      ++activities;
    } else if (segment.contains(TIMELINE_PATH)) {
      points += add_timeline_path(segment[TIMELINE_PATH].toArray(), startTime);
      ++paths;
    }
  }
  if (segments.isEmpty()) {
    timeline_warning(fname + " does not contain any semanticSegments");
  }
  if (global_opts.debug_level >= 1) {
    Debug(1) << "Processed " << segments.size() << " semanticSegments: " <<
      visits << " visits, " << activities << " activities, " << paths <<
      " timelinePaths (" << points << " points total)";
  }
}

void GoogleTimelineFormat::add_visit(const QJsonObject& visit, const QString& start_time)
{
  /*
   * A visit's coordinate lives in visit.topCandidate.placeLocation.latLng.
   * There's no street address in the device export, so the human-readable
   * "semanticType" (HOME, INFERRED_WORK, ...) becomes the waypoint name and the
   * Google placeId (if any) its description.
   */
  const QJsonObject topCandidate = visit[TOP_CANDIDATE].toObject();
  const QString latLng =
    topCandidate[PLACE_LOCATION].toObject()[LATLNG].toString();
  double lat = 0;
  double lon = 0;
  if (!parse_latlng(latLng, lat, lon)) {
    if (global_opts.debug_level >= 2) {
      Debug(2) << "visit @" << start_time <<
        ": unparsable placeLocation \"" << latLng << "\", skipping";
    }
    return;
  }
  QString shortname = topCandidate[SEMANTIC_TYPE].toString();
  title_case(shortname);
  const QString placeId = topCandidate[PLACE_ID].toString();
  Waypoint* waypoint = make_waypoint(
    lat, lon,
    shortname.isEmpty() ? nullptr : &shortname,
    placeId.isEmpty() ? nullptr : &placeId,
    start_time
  );
  waypt_add(waypoint);
}

/* add an "activity" as a track: its start and end points, plus (as a standalone
 * waypoint) a parking location if present. returns the number of TRACK points added.
 */
int GoogleTimelineFormat::add_activity(
  const QJsonObject& activity,
  const QString& start_time,
  const QString& end_time)
{
  int n_points = 0;
  auto* route = new route_head;
  QString name = activity[TOP_CANDIDATE][TYPE].toString(); // e.g. WALKING, CYCLING
  title_case(name);
  route->rte_name = name;
  track_add_head(route);

  double lat = 0;
  double lon = 0;
  if (parse_latlng(activity[START].toObject()[LATLNG].toString(), lat, lon)) {
    n_points += track_maybe_add_wpt(
      route, make_waypoint(lat, lon, nullptr, nullptr, start_time));
  }
  if (parse_latlng(activity[END].toObject()[LATLNG].toString(), lat, lon)) {
    n_points += track_maybe_add_wpt(
      route, make_waypoint(lat, lon, nullptr, nullptr, end_time));
  }
  if (n_points == 0) {
    if (global_opts.debug_level >= 2) {
      Debug(2) << "Track " << route->rte_name << ": Dropping track with no waypoints";
    }
    track_del_head(route);
  }

  /* A parked-vehicle activity carries a parking.location — emit it as a waypoint. */
  if (activity.contains(PARKING)) {
    const QJsonObject parking = activity[PARKING].toObject();
    double plat = 0;
    double plon = 0;
    if (parse_latlng(parking[LOCATION].toObject()[LATLNG].toString(), plat, plon)) {
      QString parking_name = QStringLiteral("Parking");
      waypt_add(make_waypoint(
        plat, plon, &parking_name, nullptr, parking[START_TIME].toString()));
    }
  }
  return n_points;
}

/* add a "timelinePath" (the raw point trail) as a track. returns the number of
 * points added.
 */
int GoogleTimelineFormat::add_timeline_path(
  const QJsonArray& path,
  const QString& track_name)
{
  int n_points = 0;
  auto* route = new route_head;
  route->rte_name = track_name.isEmpty() ? QStringLiteral("Timeline Path") : track_name;
  track_add_head(route);
  for (const auto&& pointRef : path) {
    const QJsonObject point = pointRef.toObject();
    double lat = 0;
    double lon = 0;
    if (!parse_latlng(point[POINT].toString(), lat, lon)) {
      continue;
    }
    n_points += track_maybe_add_wpt(
      route, make_waypoint(lat, lon, nullptr, nullptr, point[TIME].toString()));
  }
  if (n_points == 0) {
    track_del_head(route);
  }
  return n_points;
}
