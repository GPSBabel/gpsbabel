/*
    Perform various operations on waypoints.

    Copyright (C) 2002-2013 Robert Lipe, robertlipe+source@gpsbabel.org

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

#include <cassert>              // for assert
#include <cmath>                // for fabs
#include <cstdio>               // for fflush, fprintf, stdout
#include <utility>              // for as_const

#include <QChar>                // for QChar
#include <QDateTime>            // for QDateTime
#include <QDebug>               // for QDebug
#include <QLatin1Char>          // for QLatin1Char
#include <QList>                // for QList<>::const_iterator
#include <QString>              // for QString, operator==
#include <QStringLiteral>       // for qMakeStringPrivate, QStringLiteral
#include <QStringView>          // for QStringView
#include <QTime>                // for QTime
#include <QtGlobal>             // for QForeachContainer, qMakeForeachContainer, foreach, qint64

#include "defs.h"
#include "formspec.h"           // for FormatSpecificDataList
#include "garmin_fs.h"          // for garmin_ilink_t, garmin_fs_t
#include "geocache.h"           // for Geocache
#include "grtcirc.h"            // for RAD, gcdist, heading_true_degrees, radtometers
#include "session.h"            // for curr_session, session_t
#include "src/core/datetime.h"  // for DateTime
#include "src/core/logging.h"   // for FatalMsg


WaypointList* global_waypoint_list;

Geocache Waypoint::empty_gc_data;

void
waypt_init()
{
  global_waypoint_list = new WaypointList;
}

void
waypt_add(Waypoint* wpt)
{
  global_waypoint_list->waypt_add(wpt);
}

void
waypt_del(Waypoint* wpt)
{
  global_waypoint_list->waypt_del(wpt);
}

void
del_marked_wpts()
{
  global_waypoint_list->del_marked_wpts();
}

int
waypt_count()
{
  return global_waypoint_list->count();
}

void
waypt_status_disp(int total_ct, int myct)
{
  fprintf(stdout, "%d/%d/%d\r", myct*100/total_ct, myct, total_ct);
  fflush(stdout);
}

void
waypt_init_bounds(bounds* bounds)
{
  /* Set data out of bounds so that even one waypoint will reset */
  bounds->max_lat = -9999;
  bounds->max_lon = -9999;
  bounds->min_lat = 9999;
  bounds->min_lon = 9999;
  bounds->max_alt = unknown_alt;
  bounds->min_alt = -unknown_alt;
}

bool
waypt_bounds_valid(bounds* bounds)
{
  /* Returns true if bb has any 'real' data in it */
  return bounds->max_lat > -9999;
}

/*
 * Recompute bounding box based on new position point.
 */
void
waypt_add_to_bounds(bounds* bounds, const Waypoint* waypointp)
{
  if (waypointp->latitude > bounds->max_lat) {
    bounds->max_lat = waypointp->latitude;
  }
  if (waypointp->longitude > bounds->max_lon) {
    bounds->max_lon = waypointp->longitude;
  }
  if (waypointp->latitude < bounds->min_lat) {
    bounds->min_lat = waypointp->latitude;
  }
  if (waypointp->longitude < bounds->min_lon) {
    bounds->min_lon = waypointp->longitude;
  }
  if (waypointp->altitude != unknown_alt) {
    if (waypointp->altitude < bounds->min_alt) {
      bounds->min_alt = waypointp->altitude;
    }
    if (waypointp->altitude > bounds->max_alt) {
      bounds->max_alt = waypointp->altitude;
    }
  }
}


/*
 *  Makes another pass over the data to compute bounding
 *  box data and populates bounding box information.
 */

void
waypt_compute_bounds(bounds* bounds)
{
  global_waypoint_list->waypt_compute_bounds(bounds);
}

Waypoint*
find_waypt_by_name(const QString& name)
{
  return global_waypoint_list->find_waypt_by_name(name);
}

void
waypt_flush_all()
{
  global_waypoint_list->flush();
}

void
waypt_deinit()
{
  waypt_flush_all();
  delete global_waypoint_list;
}

void
waypt_append(WaypointList* src)
{
  src->copy(&global_waypoint_list);
}

void
waypt_backup(WaypointList** head_bak)
{
  global_waypoint_list->copy(head_bak);
}

void
waypt_restore(WaypointList* head_bak)
{
  global_waypoint_list->restore(head_bak);
}

void
waypt_swap(WaypointList& other)
{
  global_waypoint_list->swap(other);
}

void
waypt_add_url(Waypoint* wpt, const QString& link, const QString& url_link_text)
{
  wpt->AddUrlLink(UrlLink(link, url_link_text));
}

void
waypt_add_url(Waypoint* wpt, const QString& link, const QString& url_link_text, const QString& url_link_type)
{
  wpt->AddUrlLink(UrlLink(link, url_link_text, url_link_type));
}

// TODO: change inputs to PositionDeg?
double
gcgeodist(const double lat1, const double lon1,
          const double lat2, const double lon2)
{
 return radtometers(gcdist(PositionDeg(lat1, lon1), PositionDeg(lat2, lon2)));
}

/*
 * returns full creation_time with parts of seconds in fractional portion
 */

double
waypt_time(const Waypoint* wpt)
{
  if (!wpt->creation_time.isValid()) {
    return 0.0;
  } else {
    return ((double)wpt->creation_time.toMSecsSinceEpoch()) / 1000.0;
  }
}

/*
 * Calculates the distance between points "A" and "B" including
 * special data (Garmin interstep links)
 * The result comes in meters.
 */

double
waypt_distance_ex(const Waypoint* A, const Waypoint* B)
{
  double res = 0;

  if ((A == nullptr) || (B == nullptr)) {
    return 0;
  }

  if (const garmin_fs_t* gmsd = garmin_fs_t::find(A); (gmsd != nullptr) && (!gmsd->ilinks.isEmpty())) {
    auto prev_lat = A->latitude;
    auto prev_lon = A->longitude;
    for (const auto& link : gmsd->ilinks) {
      res += gcgeodist(prev_lat, prev_lon, link.lat, link.lon);
      prev_lat = link.lat;
      prev_lon = link.lon;
    }
    res += gcgeodist(gmsd->ilinks.last().lat, gmsd->ilinks.last().lon, B->latitude, B->longitude);
  } else {
    res = gcgeodist(A->latitude, A->longitude, B->latitude, B->longitude);
  }

  return res;
}

double
waypt_distance(const Waypoint* A, const Waypoint* B)
{
  if ((A == nullptr) || (B == nullptr)) {
    return 0;
  } else {
    return gcgeodist(A->latitude, A->longitude, B->latitude, B->longitude);
  }
}

/*
 * Calculates the speed between points "A" and "B" including
 * special data (Garmin interstep links)
 * The result comes in meters per second and is always positive.
 */

double
waypt_speed_ex(const Waypoint* A, const Waypoint* B)
{
  double dist = waypt_distance_ex(A, B);
  if (dist == 0) {
    return 0;
  }

  double time = fabs((double)A->creation_time.msecsTo(B->creation_time)) / 1000.0;
  if (time > 0) {
    return (dist / time);
  } else {
    return 0;
  }
}

/*
 * Calculates the speed between points "A" and "B"
 * the result comes in meters per second and is always positive
 */

double
waypt_speed(const Waypoint* A, const Waypoint* B)
{
  double dist = waypt_distance(A, B);
  if (dist == 0) {
    return 0;
  }

  double time = fabs((double)A->creation_time.msecsTo(B->creation_time)) / 1000.0;
  if (time > 0) {
    return (dist / time);
  } else {
    return 0;
  }
}

/*
 * Calculates the vertical speed between points "A" and "B"
 * the result comes in meters per second and can be negative.
 */

double
waypt_vertical_speed(const Waypoint* A, const Waypoint* B)
{
  double altitude = A->altitude - B->altitude;
  if (altitude == 0) {
    return 0;
  }

  double time = fabs((double)A->creation_time.msecsTo(B->creation_time)) / 1000.0;
  if (time > 0) {
    return (altitude / time);
  } else {
    return 0;
  }
}

/*
 * Returns "Road Gradient" between A and B as a percentage of slope.
 * If there is no distance or either A or B have unknown altitude, return 0.
 */
double
waypt_gradient(const Waypoint* A, const Waypoint* B)
{
  double dist = waypt_distance(A, B);
  if (dist == 0) {
    return 0;
  }

  double altitude = A->altitude - B->altitude;
  if (altitude == 0 ||
      A->altitude == unknown_alt || B->altitude == unknown_alt) {
    return 0;
  }

  double gradient = (altitude / dist) * 100;
  return (gradient);
}

/*
 * Calculates "Course True" from A to B
 */
double
waypt_course(const Waypoint* A, const Waypoint* B)
{
  if (A && B) {
    return heading_true_degrees(A->position(), B->position());
  } else {
    return 0;
  }
}

Waypoint::Waypoint() :
  geoidheight(0),
  depth(0),
  proximity(0),
  course(0),
  speed(0),
  temperature(0),
  latitude(0),  // These should probably use some invalid data, but
  longitude(0), // it looks like we have code that relies on them being zero.
  altitude(unknown_alt),
  hdop(0),
  vdop(0),
  pdop(0),
  fix(fix_unknown),
  sat(-1),
  heartrate(0),
  cadence(0),
  power(0),
  odometer_distance(0),
  gc_data(&Waypoint::empty_gc_data),
  session(curr_session()),
  extra_data(nullptr)
{
}

Waypoint::~Waypoint()
{
  if (gc_data != &Waypoint::empty_gc_data) {
    delete gc_data;
  }
  fs.FsChainDestroy();
}

Waypoint::Waypoint(const Waypoint& other) :
  geoidheight(other.geoidheight),
  depth(other.depth),
  proximity(other.proximity),
  course(other.course),
  speed(other.speed),
  temperature(other.temperature),
  opt_flags(other.opt_flags),
  latitude(other.latitude),
  longitude(other.longitude),
  altitude(other.altitude),
  shortname(other.shortname),
  description(other.description),
  notes(other.notes),
  urls(other.urls),
  icon_descr(other.icon_descr),
  creation_time(other.creation_time),
  wpt_flags(other.wpt_flags),
  hdop(other.hdop),
  vdop(other.vdop),
  pdop(other.pdop),
  fix(other.fix),
  sat(other.sat),
  heartrate(other.heartrate),
  cadence(other.cadence),
  power(other.power),
  odometer_distance(other.odometer_distance),
  gc_data(other.gc_data),
  session(other.session),
  extra_data(other.extra_data)
{
  // deep copy geocache data unless it is the special static empty_gc_data.
  if (other.gc_data != &Waypoint::empty_gc_data) {
    gc_data = new Geocache(*other.gc_data);
  }

  // deep copy fs chain data.
  fs = other.fs.FsChainCopy();

  // note: session is not deep copied.
  // note: extra_data is not deep copied.
}

Waypoint& Waypoint::operator=(const Waypoint& rhs)
{
  if (this != &rhs) {

    // deallocate
    if (gc_data != &Waypoint::empty_gc_data) {
      delete gc_data;
    }
    fs.FsChainDestroy();

    // allocate and copy
    latitude = rhs.latitude;
    longitude = rhs.longitude;
    altitude = rhs.altitude;
    geoidheight = rhs.geoidheight;
    depth = rhs.depth;
    proximity = rhs.proximity;
    shortname = rhs.shortname;
    description = rhs.description;
    notes = rhs.notes;
    urls = rhs.urls;
    wpt_flags = rhs.wpt_flags;
    icon_descr = rhs.icon_descr;
    creation_time = rhs.creation_time;
    hdop = rhs.hdop;
    vdop = rhs.vdop;
    pdop = rhs.pdop;
    course = rhs.course;
    speed = rhs.speed;
    fix = rhs.fix;
    sat = rhs.sat;
    heartrate = rhs.heartrate;
    cadence = rhs.cadence;
    power = rhs.power;
    temperature = rhs.temperature;
    odometer_distance = rhs.odometer_distance;
    gc_data = rhs.gc_data;
    session = rhs.session;
    extra_data = rhs.extra_data;
    // deep copy geocache data unless it is the special static empty_gc_data.
    if (rhs.gc_data != &Waypoint::empty_gc_data) {
      gc_data = new Geocache(*rhs.gc_data);
    }

    // deep copy fs chain data.
    fs = rhs.fs.FsChainCopy();

    // note: session is not deep copied.
    // note: extra_data is not deep copied.
  }

  return *this;
}

bool
Waypoint::HasUrlLink() const
{
  return urls.HasUrlLink();
}

const UrlLink&
Waypoint::GetUrlLink() const
{
  return urls.GetUrlLink();
}

void
Waypoint::AddUrlLink(const UrlLink& l)
{
  urls.AddUrlLink(l);
}

QString
Waypoint::CreationTimeXML() const
{
  if (!creation_time.isValid()) {
    return nullptr;
  }

  QDateTime dt = GetCreationTime().toUTC();

  const char* format = "yyyy-MM-ddTHH:mm:ssZ";
  if (dt.time().msec()) {
    format = "yyyy-MM-ddTHH:mm:ss.zzzZ";
  }

  return dt.toString(format);
}

gpsbabel::DateTime
Waypoint::GetCreationTime() const
{
  return creation_time;
}

void
Waypoint::SetCreationTime(const gpsbabel::DateTime& t)
{
  creation_time = t;
}

void
Waypoint::SetCreationTime(qint64 t, qint64 ms)
{
  creation_time.setMSecsSinceEpoch((t * 1000) + ms);
}

Geocache*
Waypoint::AllocGCData()
{
  if (gc_data == &Waypoint::empty_gc_data) {
    gc_data = new Geocache;
  }
  return gc_data;
}

int
Waypoint::EmptyGCData() const
{
  return (gc_data == &Waypoint::empty_gc_data);
}

void
WaypointList::waypt_add(Waypoint* wpt)
{
  double lat_orig = wpt->latitude;
  double lon_orig = wpt->longitude;
  append(wpt);

  if (wpt->latitude < -90) {
    wpt->latitude += 180;
  } else if (wpt->latitude > +90) {
    wpt->latitude -= 180;
  }
  if (wpt->longitude < -180) {
    wpt->longitude += 360;
  } else if (wpt->longitude > +180) {
    wpt->longitude -= 360;
  }

  if ((wpt->latitude < -90) || (wpt->latitude > 90.0))
    fatal(FatalMsg() << wpt->session->name
          << "Invalid latitude" << lat_orig << "in waypoint"
          << wpt->shortname);
  if ((wpt->longitude < -180) || (wpt->longitude > 180.0))
    fatal(FatalMsg() << "Invalid longitude" << lon_orig << "in waypoint"
          << wpt->shortname);

  /*
   * Some input may not have one or more of these types so we
   * try to be sure that we have these fields even if just by
   * copying them from elsewhere.
   */

  // Note tests for isNull here as some formats intentionally set "".
  // This is kind of goofy, but it emulates the C string implementation.
  if (wpt->shortname.isNull()) {
    if (!wpt->description.isNull()) {
      wpt->shortname = wpt->description;
    } else if (!wpt->notes.isNull()) {
      wpt->shortname = wpt->notes;
    } else {
      wpt->shortname = QStringLiteral("WPT%1").arg(waypt_count(), 3, 10, QLatin1Char('0'));
    }
  }

  if (wpt->description.isEmpty()) {
    if (!wpt->notes.isNull()) {
      wpt->description = wpt->notes;
    } else {
      if (!wpt->shortname.isNull()) {
        wpt->description = wpt->shortname;
      }
    }
  }

}

void
WaypointList::add_rte_waypt(int waypt_ct, Waypoint* wpt, bool synth, QStringView namepart, int number_digits)
{
  append(wpt);

  if (synth && wpt->shortname.isEmpty()) {
    wpt->shortname = QStringLiteral("%1%2").arg(namepart).arg(waypt_ct, number_digits, 10, QChar('0'));
    wpt->wpt_flags.shortname_is_synthetic = 1;
  }
}

void
WaypointList::waypt_del(Waypoint* wpt)
{
  const int idx = this->indexOf(wpt);
  assert(idx >= 0);
  removeAt(idx);
}

void
WaypointList::del_marked_wpts()
{
  // For lineary complexity build a new list from the points we keep.
  WaypointList oldlist;
  swap(oldlist);

  for (Waypoint* wpt : std::as_const(oldlist)) {
    if (wpt->wpt_flags.marked_for_deletion) {
      delete wpt;
    } else {
      waypt_add(wpt);
    }
  }
}

void
WaypointList::del_rte_waypt(Waypoint* wpt)
{
  const int idx = indexOf(wpt);
  assert(idx >= 0);
  if (wpt->wpt_flags.new_trkseg && ((idx+1) < size())) {
    auto* wpt_next = at(idx+1);
    wpt_next->wpt_flags.new_trkseg = 1;
  }
  wpt->wpt_flags.new_trkseg = 0;
  removeAt(idx);
}

/*
 *  Makes another pass over the data to compute bounding
 *  box data and populates bounding box information.
 */

void
WaypointList::waypt_compute_bounds(bounds* bounds) const
{
  waypt_init_bounds(bounds);
  foreach (const Waypoint* waypointp, *this) {
    waypt_add_to_bounds(bounds, waypointp);
  }
}

Waypoint*
WaypointList::find_waypt_by_name(const QString& name) const
{
  foreach (Waypoint* waypointp, *this) {
    if (waypointp->shortname == name) {
      return waypointp;
    }
  }

  return nullptr;
}

void
WaypointList::flush()
{
  while (!isEmpty()) {
    delete takeFirst();
  }
}

void
WaypointList::copy(WaypointList** dst) const
{
  if (*dst == nullptr) {
    *dst = new WaypointList;
  }

  foreach (const Waypoint* wpt_old, *this) {
    (*dst)->waypt_add(new Waypoint(*wpt_old));
  }
}

void
WaypointList::restore(WaypointList* src)
{
  if (src == nullptr) {
    return;
  }
  flush();

  *this = *src;
  src->clear();
}

void WaypointList::swap(WaypointList& other)
{
  const WaypointList tmp_list = *this;
  *this = other;
  other = tmp_list;
}

PositionDeg::PositionDeg(const PositionRad& posr) :
  lat(posr.lat * kDegreesPerRadian),
  lon(posr.lon * kDegreesPerRadian) {}

PositionRad::PositionRad(const PositionDeg& posd) :
  lat(posd.lat * kRadiansPerDegree),
  lon(posd.lon * kRadiansPerDegree) {}
