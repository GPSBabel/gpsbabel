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
    Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111 USA

 */

#include <cassert>              // for assert
#include <cmath>                // for fabs
#include <cstdio>               // for printf, fflush, fprintf, stdout
#include <ctime>                // for time_t
#include <algorithm>            // for stable_sort

#include <QtCore/QByteArray>    // for QByteArray
#include <QtCore/QChar>         // for QChar
#include <QtCore/QDateTime>     // for QDateTime
#include <QtCore/QList>         // for QList
#include <QtCore/QString>       // for QString, operator==
#include <QtCore/QTime>         // for QTime
#include <QtCore/QtGlobal>      // for qPrintable

#include "defs.h"
#include "garmin_fs.h"          // for garmin_ilink_t, garmin_fs_s, GMSD_FIND, garmin_fs_p
#include "grtcirc.h"            // for RAD, gcdist, heading_true_degrees, radtometers
#include "session.h"            // for curr_session, session_t
#include "src/core/datetime.h"  // for DateTime
#include "src/core/logging.h"   // for Warning, Fatal

WaypointList* global_waypoint_list;

static short_handle mkshort_handle;
geocache_data Waypoint::empty_gc_data;
static global_trait traits;

const global_trait* get_traits()
{
  return &traits;
}

void
waypt_init()
{
  mkshort_handle = mkshort_new_handle();
  global_waypoint_list = new WaypointList;
}

void update_common_traits(const Waypoint* wpt)
{
  /* This is a bit tacky, but it allows a hint whether we've seen
   * this data or not in the life cycle of this run.   Of course,
   * the caches could have been filtered out of existance and not
   * all waypoints may have this and a few other pitfalls, but it's
   * an easy and fast test here.
   */
  traits.trait_geocaches |= (wpt->gc_data->diff && wpt->gc_data->terr);
  traits.trait_heartrate |= wpt->heartrate > 0;
  traits.trait_cadence |= wpt->cadence > 0;
  traits.trait_power |= wpt->power > 0;
  traits.trait_depth |= WAYPT_HAS(wpt, depth);
  traits.trait_temperature |= WAYPT_HAS(wpt, temperature);
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

unsigned int
waypt_count()
{
  return global_waypoint_list->count();
}

// TODO: should this, and mkshort_handle, be part of main, which is the only user?
void
waypt_disp(const Waypoint* wpt)
{
  if (wpt->GetCreationTime().isValid()) {
    printf("%s ", qPrintable(wpt->creation_time.toString()));
  }
  printposn(wpt->latitude,1);
  printposn(wpt->longitude,0);
  if (!wpt->description.isEmpty()) {
    printf("%s/%s",
           global_opts.synthesize_shortnames ?
           qPrintable(mkshort(mkshort_handle, wpt->description)) :
           qPrintable(wpt->shortname),
           qPrintable(wpt->description));
  }

  if (wpt->altitude != unknown_alt) {
    printf(" %f", wpt->altitude);
  }
  printf("\n");
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

int
waypt_bounds_valid(bounds* bounds)
{
  /* Returns true if bb has any 'real' data in it */
  return bounds->max_lat > -9999;
}

/*
 * Recompund bounding box based on new position point.
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
  if (mkshort_handle) {
    mkshort_del_handle(&mkshort_handle);
  }
  global_waypoint_list->flush();
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
waypt_sort(WaypointList::Compare cmp)
{
  global_waypoint_list->sort(cmp);
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

double
gcgeodist(const double lat1, const double lon1,
          const double lat2, const double lon2)
{
  double res = radtometers(gcdist(RAD(lat1), RAD(lon1), RAD(lat2), RAD(lon2)));
  if (res < 0.1) {
    res = 0;  /* calc. diffs on 32- and 64-bit hosts */
  }

  return res;
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
  garmin_fs_p gmsd;

  if ((A == nullptr) || (B == nullptr)) {
    return 0;
  }

  if ((gmsd = GMSD_FIND(A)) && (gmsd->ilinks != nullptr)) {
    garmin_ilink_t* link = gmsd->ilinks;

    res = gcgeodist(A->latitude, A->longitude, link->lat, link->lon);
    while (link->next != nullptr) {
      garmin_ilink_t* prev = link;
      link = link->next;
      res += gcgeodist(prev->lat, prev->lon, link->lat, link->lon);
    }
    res += gcgeodist(link->lat, link->lon, B->latitude, B->longitude);
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
    return heading_true_degrees(RAD(A->latitude), RAD(A->longitude), RAD(B->latitude), RAD(B->longitude));
  } else {
    return 0;
  }
}

Waypoint::Waypoint() :
  latitude(0),  // These should probably use some invalid data, but
  longitude(0), // it looks like we have code that relies on them being zero.
  altitude(unknown_alt),
  geoidheight(0),
  depth(0),
  proximity(0),
  route_priority(0),
  hdop(0),
  vdop(0),
  pdop(0),
  course(0),
  speed(0),
  fix(fix_unknown),
  sat(-1),
  heartrate(0),
  cadence(0),
  power(0),
  temperature(0),
  odometer_distance(0),
  gc_data(&Waypoint::empty_gc_data),
  fs(nullptr),
  session(curr_session()),
  extra_data(nullptr)
{
}

Waypoint::~Waypoint()
{
  if (gc_data != &Waypoint::empty_gc_data) {
    delete gc_data;
  }
  fs_chain_destroy(fs);
}

Waypoint::Waypoint(const Waypoint& other) :
  latitude(other.latitude),
  longitude(other.longitude),
  altitude(other.altitude),
  geoidheight(other.geoidheight),
  depth(other.depth),
  proximity(other.proximity),
  shortname(other.shortname),
  description(other.description),
  notes(other.notes),
  urls(other.urls),
  wpt_flags(other.wpt_flags),
  icon_descr(other.icon_descr),
  creation_time(other.creation_time),
  route_priority(other.route_priority),
  hdop(other.hdop),
  vdop(other.vdop),
  pdop(other.pdop),
  course(other.course),
  speed(other.speed),
  fix(other.fix),
  sat(other.sat),
  heartrate(other.heartrate),
  cadence(other.cadence),
  power(other.power),
  temperature(other.temperature),
  odometer_distance(other.odometer_distance),
  gc_data(other.gc_data),
  fs(other.fs),
  session(other.session),
  extra_data(other.extra_data)
{
  // deep copy geocache data unless it is the specail static empty_gc_data.
  if (other.gc_data != &Waypoint::empty_gc_data) {
    gc_data = new geocache_data(*other.gc_data);
  }

  // deep copy fs chain data.
  fs = fs_chain_copy(other.fs);

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
    fs_chain_destroy(fs);

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
    route_priority = rhs.route_priority;
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
    fs = rhs.fs;
    session = rhs.session;
    extra_data = rhs.extra_data;
    // deep copy geocache data unless it is the specail static empty_gc_data.
    if (rhs.gc_data != &Waypoint::empty_gc_data) {
      gc_data = new geocache_data(*rhs.gc_data);
    }

    // deep copy fs chain data.
    fs = fs_chain_copy(rhs.fs);

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

[[deprecated]] const QList<UrlLink>
Waypoint::GetUrlLinks() const
{
  return urls;
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
Waypoint::SetCreationTime(time_t t)
{
  creation_time = QDateTime::fromTime_t(t);
}

void
Waypoint::SetCreationTime(time_t t, int ms)
{
  creation_time.setTime_t(t);
  creation_time = creation_time.addMSecs(ms);
}

geocache_data*
Waypoint::AllocGCData()
{
  if (gc_data == &Waypoint::empty_gc_data) {
    gc_data = new geocache_data;
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
    Fatal() << wpt->session->name
            << "Invalid latitude" << lat_orig << "in waypoint"
            << wpt->shortname;
  if ((wpt->longitude < -180) || (wpt->longitude > 180.0))
    Fatal() << "Invalid longitude" << lon_orig << "in waypoint"
            << wpt->shortname;

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
      QString n;
      n.sprintf("%03d", waypt_count());
      wpt->shortname = QString("WPT%1").arg(n);
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

  if (this == global_waypoint_list) {
    update_common_traits(wpt);
  }

}

void
WaypointList::add_rte_waypt(int waypt_ct, Waypoint* wpt, bool synth, const QString& namepart, int number_digits)
{
  append(wpt);

   if (synth && wpt->shortname.isEmpty()) {
     wpt->shortname = QString("%1%2").arg(namepart).arg(waypt_ct, number_digits, 10, QChar('0'));
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
WaypointList::del_rte_waypt(Waypoint* wpt)
{
  const int idx = indexOf(wpt);
  assert(idx >= 0);
  if (wpt->wpt_flags.new_trkseg && ((idx+1) < size())) {
    auto wpt_next = at(idx+1);
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

void WaypointList::sort(Compare cmp)
{
  std::stable_sort(begin(), end(), cmp);
}
