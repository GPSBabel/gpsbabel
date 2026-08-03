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
#include <QIODevice>            // for QIODevice, QIODevice::ReadOnly
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

const QString GoogleTimelineFormat::nullString = QString();

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
  const QString& shortname,
  const QString& description,
  const QString& time_str)
{
  auto* waypoint = new Waypoint();
  waypoint->latitude = lat;
  waypoint->longitude = lon;
  if (!shortname.isEmpty()) {
    waypoint->shortname = shortname;
  }
  if (!description.isEmpty()) {
    waypoint->description = description;
  }
  if (!time_str.isEmpty()) {
    waypoint->SetCreationTime(parse_time(time_str));
  }
  return waypoint;
}

/* Add a point to a track, dropping a null-island (0, 0) coordinate.
 *
 * Unlike googletakeout, where a missing latitudeE7/longitudeE7 field reads back as 0, a
 * point only reaches here once parse_latlng() has accepted an explicit
 * coordinate string, so (0, 0) means the file really did say "0.0°, 0.0°".
 * That is open ocean in the Gulf of Guinea and, in a phone's timeline, a
 * placeholder rather than a visit.
 */
bool GoogleTimelineFormat::track_maybe_add_wpt(route_head* route, Waypoint* waypoint)
{
  if (waypoint->latitude == 0 && waypoint->longitude == 0) {
    if (global_opts.debug_level >= 2) {
      Debug(2) << "Track " << route->rte_name << "@" <<
        waypoint->creation_time.toPrettyString() <<
        ": Dropping null island (0, 0) point";
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
  /*
   * Deliberately not QIODevice::Text: it translates line endings on read, so
   * the parser would not see the bytes that are actually on disk. JSON treats
   * CR and LF alike as whitespace, so the translation gains nothing.
   */
  ifd->open(QIODevice::ReadOnly);
  /*
   * Hand the raw bytes to the JSON parser. Decoding to QString first and
   * re-encoding with toUtf8() both copies the whole buffer an extra time and,
   * more importantly, launders invalid UTF-8: the decode substitutes U+FFFD, so
   * a corrupt file parses "successfully" and the damage lands silently in the
   * output. Parsing the QByteArray directly reports QJsonParseError::IllegalUTF8String
   * instead.
   */
  QJsonParseError error{};
  const QJsonDocument doc = QJsonDocument::fromJson(ifd->readAll(), &error);
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
  int parking = 0;
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
      /* a visit with an unusable placeLocation adds nothing, so it must not be
       * counted towards either total */
      if (add_visit(segment[VISIT].toObject(), startTime)) {
        ++visits;
        ++points;
      }
    } else if (segment.contains(ACTIVITY)) {
      const QJsonObject activity = segment[ACTIVITY].toObject();
      /* likewise, an activity or timelinePath whose points were all unusable
       * or dropped emits no track and isn't counted */
      if (const int n = add_activity(activity, startTime, endTime); n > 0) {
        points += n;
        ++activities;
      }
      if (add_parking(activity)) {
        ++parking;
        ++points;
      }
    } else if (segment.contains(TIMELINE_PATH)) {
      if (const int n = add_timeline_path(segment[TIMELINE_PATH].toArray(), startTime);
          n > 0) {
        points += n;
        ++paths;
      }
    }
  }
  if (segments.isEmpty()) {
    timeline_warning(fname + " does not contain any semanticSegments");
  }
  if (global_opts.debug_level >= 1) {
    Debug(1) << "Processed " << segments.size() << " semanticSegments: " <<
      visits << " visits, " << activities << " activities, " << paths <<
      " timelinePaths, " << parking << " parking (" << points << " points total)";
  }
}

/* add a "visit" as a waypoint. returns true if a waypoint was added, false if the
 * visit had no usable coordinate and was skipped.
 */
bool GoogleTimelineFormat::add_visit(const QJsonObject& visit, const QString& start_time)
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
    return false;
  }
  QString shortname = topCandidate[SEMANTIC_TYPE].toString();
  title_case(shortname);
  const QString placeId = topCandidate[PLACE_ID].toString();
  Waypoint* waypoint = make_waypoint(
    lat, lon,
    shortname,
    placeId,
    start_time
  );
  waypt_add(waypoint);
  return true;
}

/* add an "activity" as a track: its start and end points. returns the number of
 * points added.
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
      route, make_waypoint(lat, lon, nullString, nullString, start_time));
  }
  if (parse_latlng(activity[END].toObject()[LATLNG].toString(), lat, lon)) {
    n_points += track_maybe_add_wpt(
      route, make_waypoint(lat, lon, nullString, nullString, end_time));
  }
  if (n_points == 0) {
    if (global_opts.debug_level >= 2) {
      Debug(2) << "Track " << route->rte_name << ": Dropping track with no waypoints";
    }
    track_del_head(route);
  }
  return n_points;
}

/* a parked-vehicle activity carries a parking.location — emit it as a waypoint.
 * returns true if one was added.
 */
bool GoogleTimelineFormat::add_parking(const QJsonObject& activity)
{
  if (!activity.contains(PARKING)) {
    return false;
  }
  const QJsonObject parking = activity[PARKING].toObject();
  double lat = 0;
  double lon = 0;
  if (!parse_latlng(parking[LOCATION].toObject()[LATLNG].toString(), lat, lon)) {
    return false;
  }
  waypt_add(make_waypoint(
    lat, lon, QStringLiteral("Parking"), nullString, parking[START_TIME].toString()));
  return true;
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
      route, make_waypoint(lat, lon, nullString, nullString, point[TIME].toString()));
  }
  if (n_points == 0) {
    track_del_head(route);
  }
  return n_points;
}
