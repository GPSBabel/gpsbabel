#include <QDir>
#include <QIODevice>
#include <QJsonObject>
#include <QJsonArray>
#include <QJsonParseError>

#include "src/core/file.h"

#include "googletakeout.h"

#define MYNAME "Google Takeout"
#define TIMELINE_OBJECTS "timelineObjects"

static const QList<QString> takeout_month_names{
  "JANUARY", "FEBRUARY", "MARCH", "APRIL", "MAY", "JUNE", "JULY",
  "AUGUST", "SEPTEMBER", "OCTOBER", "NOVEMBER", "DECEMBER"
};

static inline void takeout_fatal(const QString& message) {
  fatal(FatalMsg() << MYNAME << ": " << message);
}

static inline void takeout_warning(const QString& message) {
  Warning() << MYNAME << ": " << message;
}

/* create a waypoint from late7/lone7 and optional metadata */
static Waypoint* takeout_waypoint(
  int lat_e7,
  int lon_e7,
  const QString* shortname,
  const QString* description,
  const QString* start_str
)
{
  Waypoint* waypoint = new Waypoint();
  waypoint->latitude = lat_e7 / 1e7;
  waypoint->longitude = lon_e7 / 1e7;
  if (shortname && (*shortname).length() > 0) {
    waypoint->shortname = *shortname;
  }
  if (description && (*description).length() > 0) {
    waypoint->description = *description;
  }
  if (start_str && (*start_str).length() > 0) {
    gpsbabel::DateTime start = QDateTime::fromString(*start_str, Qt::ISODate);
    waypoint->SetCreationTime(start);
  }
  return waypoint;
}

static bool track_maybe_add_wpt(route_head* route, Waypoint* waypoint) {
  if (waypoint->latitude == 0 && waypoint->longitude == 0) {
    Debug(2) << "Track " << route->rte_name << "@" <<
      waypoint->creation_time.toPrettyString() <<
      ": Dropping point with no lat/long";
    delete waypoint; // as we're dropping it, gpsbabel won't clean it up later
    return false;
  }
  track_add_wpt(route, waypoint);
  return true;
}

static QList<QJsonObject> readJson(
    const QString& source)
{
  Debug(2) << "Reading from JSON " << source;
  auto* ifd = new gpsbabel::File(source);
  ifd->open(QIODevice::ReadOnly | QIODevice::Text);
  const QString content = ifd->readAll();
  QJsonParseError error{};
  const QJsonDocument doc = QJsonDocument::fromJson(content.toUtf8(), &error);
  if (error.error != QJsonParseError::NoError) {
    takeout_fatal(
      QString("JSON parse error in ") + ifd->fileName() + ": " +
      error.errorString()
    );
  }

  const QJsonObject root = doc.object();
  const QJsonValue timelineObjectsIn = root.value(TIMELINE_OBJECTS);
  if (timelineObjectsIn.isNull()) {
    takeout_fatal(
      ifd->fileName() + " is missing required \"" +
      TIMELINE_OBJECTS + "\" section"
    );
  }

  const QJsonArray timelineJson = timelineObjectsIn.toArray();
  QList<QJsonObject> timeline;
  for (QJsonValue&& val : timelineJson) {
    if (val.isObject()) {
      timeline.append(val.toObject());
    } else {
      takeout_fatal(ifd->fileName() + " has non-object in timelineObjects");
    }
  }
  ifd->close();
  delete ifd;
  if (timeline.isEmpty()) {
    takeout_warning(QString(source) + " does not contain any timelineObjects");
  }
  Debug(2) << "Saw " << timeline.size() << " timelineObjects in " << source;
  return timeline;
}

static QList<QString> readDir(
    const QString& source)
{
  Debug(2) << "Reading from folder " << source;
  const QDir dir{source};
  const QFileInfo sourceInfo{source};
  const QString baseName = sourceInfo.fileName();
  QList<QString> paths;
  /* If a directory's name is a 4-digit number, this is a "year" folder
   * and we will look for YYYY_MONTH.json files.
   * Otherwise, this is the all-time folder that _contains_ the month
   * folders
   */
  if (baseName.length() == 4 && baseName.toInt() > 0) {
    for (auto&& month : takeout_month_names) {
      const QString path = source + "/" + baseName + "_" + month + ".json";
      const QFileInfo info{path};
      if (info.exists()) {
        Debug(3) << "Adding file " << path;
        paths.append(path);
      } else {
        Debug(4) << "Did not find " << path;
      }
    }
  } else {
    for (auto&& entry : dir.entryList()) {
      const QString path = dir.filePath(entry);
      if (entry.length() == 4 && entry.toInt() > 0) {
        Debug(3) << "Adding directory " << path;
        paths.append(path);
      } else {
        takeout_warning(QString("Malformed folder name ") + path);
      }
    }
  }
  Debug(2) << "Saw " << paths.size() << " paths in " << source;
  return paths;
}

void
GoogleTakeoutFormat::title_case(QString& title)
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

void
GoogleTakeoutFormat::rd_init(const QString& fname) {
  Debug(4) << "rd_init(" << fname << ")";
  inputStream = GoogleTakeoutInputStream(fname);
}

void
GoogleTakeoutFormat::read()
{
  int items = 0;
  int points = 0;
  int place_visits = 0;
  int activity_segments = 0;
  QJsonValue iterator = inputStream.next();

  for (; !iterator.isNull(); iterator = inputStream.next()) {
    ++ items;
    /*
     * A timelineObject is stored in a single-element dictionary.
     * The key will be either a "placeVisit" (waypoint) or
     * "activitySegment" (movement), and the value is another dictionary
     * containing the timelineObject's details.
     *
     */
    const QJsonObject timelineObjectContainer = iterator.toObject();
    int len = timelineObjectContainer.size();
    if (len != 1) {
      takeout_fatal(
        QString("expected a single key dict, got ") + QString::number(len) +
        " keys"
      );
    }
    const QJsonObject::const_iterator timelineObjectIterator =
      timelineObjectContainer.constBegin();
    const QString& timelineObjectType = timelineObjectIterator.key();
    const QJsonObject& timelineObjectDetail =
      timelineObjectIterator.value().toObject();
    if (timelineObjectType == PLACE_VISIT) {
      add_place_visit(timelineObjectDetail);
      ++ place_visits;
      ++ points;
    } else if (timelineObjectType == ACTIVITY_SEGMENT) {
      points += add_activity_segment(timelineObjectDetail);
      ++ activity_segments;
    } else {
      takeout_fatal(
        QString("unknown timeline object type \"") + timelineObjectType +
        "\""
      );
    }
  }
  Debug(1) << MYNAME << ": Processed " << items << " items: " <<
    place_visits << " " << PLACE_VISIT << ", " << activity_segments <<
    " " << ACTIVITY_SEGMENT << " (" << points << " points total)";
}

void
GoogleTakeoutFormat::add_place_visit(const QJsonObject& placeVisit)
{
  /*
   * placeVisits:
   *   one lat/long, will always contain "location.address", may also
   *   contain "location.name"
   *   "duration" contains start and end times
   *
   *  TODO: capture end time/duration
   *  some placeVisits have a simplifiedRawPath.
   *  TODO: do something with simplifiedRawPath
   */
  const QJsonObject& loc = placeVisit[LOCATION].toObject();
  const QString address = loc[ADDRESS].toString();
  const QString timestamp = placeVisit[DURATION][START_TIMESTAMP].toString();
  Waypoint* waypoint;

  if (loc.contains(NAME) && loc[NAME].toString().length() > 0) {
    QString name = loc[NAME].toString();
    waypoint = takeout_waypoint(
      loc[LOCATION_LATE7].toInt(),
      loc[LOCATION_LONE7].toInt(),
      name.length() > 0 ? &name : nullptr,
      &address,
      &timestamp
    );
  } else {
    waypoint = takeout_waypoint(
      loc[LOCATION_LATE7].toInt(),
      loc[LOCATION_LONE7].toInt(),
      nullptr,
      &address,
      &timestamp
    );
  }

  waypt_add(waypoint);
}

/* add an "activitySegment" (track)
 * an activitySegment has at least two points (a start and an end) and
 * may have waypoints in-between.
 *
 * returns the total number of points added
 */
int
GoogleTakeoutFormat::add_activity_segment(const QJsonObject& activitySegment)
{
  /*
   * activitySegment:
   *   one startLocation and one endLocation. there are also waypoints
   *   in the waypointPath array. the "distance" field appears to be in
   *   metres. There is a "duration" section with "startTimestamp" and
   *   "endTimestamp" with "distance" and "duration", "speed" can be
   *   inferred.
   *   "activityType" is stuff like "IN_PASSENGER_VEHICLE", "WALKING", etc
   *
   *   some activitySegments also include a "parkingEvent".
   *   TODO: add parkingEvent as its own waypoint
   *   some activitySegments also have a simplifiedRawPath
   *   TODO: do something with simplifiedRawPath
   */
  int n_points = 0;
  Waypoint* waypoint = nullptr;
  auto* route = new route_head;
  const QJsonObject startLoc = activitySegment[START_LOCATION].toObject();
  const QJsonObject endLoc = activitySegment[END_LOCATION].toObject();
  QString activityType = activitySegment[ACTIVITY_TYPE].toString();
  title_case(activityType);
  route->rte_name = activityType;
  track_add_head(route);
  QString timestamp;
  timestamp = activitySegment[DURATION][START_TIMESTAMP].toString();
  waypoint = takeout_waypoint(
    startLoc[LOCATION_LATE7].toInt(),
    startLoc[LOCATION_LONE7].toInt(),
    nullptr, nullptr,
    &timestamp
  );
  n_points += track_maybe_add_wpt(route, waypoint);
  /* activitySegments give us three sets of waypoints.
   * 1. "waypoints" dict
   *    This is available on all tracks, but only includes the
   *    lat/lon - no timestamp.
   * 2. "simplifiedRawPath" dict
   *    these all have timestamps which provides richer metadata.
   *    This is not available on all tracks.
   * 3. An incredibly detailed "roadSegment" list that includes a
   *    bunch of google placeId's and durations, but no lat/long.
   *    To get the lat/lon you need to query the Google Places API
   *    https://maps.googleapis.com/maps/api/place/details/json with:
   *      placeid=placeId
   *      key=apiKey
   *    and then extract "lat" and "lng" from result.geometry.location
   *    This also costs $17USD per 1,000 location requests
   *    TODO: add an option to query places API, and an option to count
   *    how many Google Places requests it would take to process a file
   *
   *  For now, we use (1)
   */
  const QJsonArray points =
    activitySegment[WAYPOINT_PATH][WAYPOINTS].toArray();
  for (const auto&& pointRef: points) {
    const QJsonObject point = pointRef.toObject();
    waypoint = takeout_waypoint(
      point[LATE7].toInt(),
      point[LONE7].toInt(),
      nullptr,
      nullptr,
      nullptr
    );
    n_points += track_maybe_add_wpt(route, waypoint);
  }
  timestamp = activitySegment[DURATION][END_TIMESTAMP].toString();
  waypoint = takeout_waypoint(
    endLoc[LOCATION_LATE7].toInt(),
    endLoc[LOCATION_LONE7].toInt(),
    nullptr, nullptr, &timestamp
  );
  n_points += track_maybe_add_wpt(route, waypoint);
  if (!n_points) {
    Debug(2) << "Track " << route->rte_name <<
      ": Dropping track with no waypoints";
    track_del_head(route);
  }
  return n_points;
}

void GoogleTakeoutInputStream::loadSource(const QString& source) {
  const QFileInfo info{source};
  if (info.isDir()) {
    sources += readDir(source);
  } else if (info.exists()) {
    timelineObjects.append(readJson(source));
  } else {
    takeout_fatal(source + ": No such file or directory");
  }
}

QJsonValue GoogleTakeoutInputStream::next() {
  if (!timelineObjects.isEmpty()) {
    QJsonValue nextObject = timelineObjects.first();
    timelineObjects.removeFirst();
    return nextObject;
  }

  if (!sources.isEmpty()) {
    const QString filename = sources.first();
    sources.removeFirst();
    loadSource(filename);
    return next();
  }

  return QJsonValue();
}
