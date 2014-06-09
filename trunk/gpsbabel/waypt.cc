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

#include <stdio.h>
#include <math.h>
#include <QtCore/QDebug>
#include <QtCore/QList>

#include "defs.h"
#include "cet_util.h"
#include "grtcirc.h"
#include "garmin_fs.h"
#include "session.h"
#include "src/core/logging.h"

#if NEWQ
QList<Waypoint*> waypt_list;
queue waypt_head; // This is here solely to freak out the formats that are
// looking into what should be a private members.
#else
queue waypt_head;
#endif

static unsigned int waypt_ct;
static short_handle mkshort_handle;
geocache_data Waypoint::empty_gc_data;
static global_trait traits;

const global_trait* get_traits(void)
{
  return &traits;
}

void
waypt_init(void)
{
  mkshort_handle = mkshort_new_handle();
#if NEWQ
  waypt_list.clear();
#else
  QUEUE_INIT(&waypt_head);
#endif
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
  double lat_orig = wpt->latitude;
  double lon_orig = wpt->longitude;
#if NEWQ
  waypt_list.append(wpt);
#else
  ENQUEUE_TAIL(&waypt_head, &wpt->Q);
  waypt_ct++;
#endif


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

  update_common_traits(wpt);

}

void
waypt_del(Waypoint* wpt)
{
  // the wpt must be on waypt_list, and is assumed unique.
#if NEWQ
  waypt_list.removeOne(wpt);
#else
  dequeue(&wpt->Q);
  waypt_ct--;
#endif
}

unsigned int
waypt_count(void)
{
#if NEWQ
  return waypt_list.size();
#else
  return waypt_ct;
#endif
}

void
set_waypt_count(unsigned int nc)
{
  waypt_ct = nc;
}

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
waypt_disp_all(waypt_cb cb)
{
  waypt_disp_session(NULL, cb);
}

void
waypt_disp_session(const session_t* se, waypt_cb cb)
{
  int i = 0;
#if NEWQ
  foreach(Waypoint* waypointp, waypt_list) {
#else
  queue* elem, *tmp;
  Waypoint* waypointp;
  QUEUE_FOR_EACH(&waypt_head, elem, tmp) {
    waypointp = (Waypoint*) elem;
#endif
    if ((se == NULL) || (waypointp->session == se)) {
      if (global_opts.verbose_status) {
        i++;
        waypt_status_disp(waypt_count(), i);
      }
      (*cb)(waypointp);
    }
  }
  if (global_opts.verbose_status) {
    fprintf(stdout, "\r\n");
  }
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
  waypt_init_bounds(bounds);
#if NEWQ
  foreach(Waypoint* waypointp, waypt_list) {
#else
  queue* elem, *tmp;
  Waypoint* waypointp;
  QUEUE_FOR_EACH(&waypt_head, elem, tmp) {
    waypointp = (Waypoint*) elem;
#endif
    waypt_add_to_bounds(bounds, waypointp);
  }
}

Waypoint*
find_waypt_by_name(const QString& name)
{
#if NEWQ
  foreach(Waypoint* waypointp, waypt_list) {
#else
  queue* elem, *tmp;
  Waypoint* waypointp;

  QUEUE_FOR_EACH(&waypt_head, elem, tmp) {
    waypointp = (Waypoint*) elem;
#endif
    if (waypointp->shortname == name) {
      return waypointp;
    }
  }

  return NULL;
}

#if NEWQ
void
waypt_flush(queue* head)
{
// TODO: This is incorrect when head != &waypt_head
// We need to pass in a QList<Waypoint*> instead of a queue* that we ignore!
  if (head != &waypt_head) {
    if (global_opts.debug_level >= 1) {
      warning("NEWQ version of waypt_flush is unimplemented for this list.\n");
    }
  } else {
    while (!waypt_list.isEmpty()) {
      delete waypt_list.takeFirst();
    }
  }
}
#else
void
waypt_flush(queue* head)
{
  queue* elem, *tmp;

  QUEUE_FOR_EACH(head, elem, tmp) {
    Waypoint* q = (Waypoint*) dequeue(elem);
    delete q;
    if (head == &waypt_head) {
      waypt_ct--;
    }
  }
}
#endif

void
waypt_flush_all()
{
  if (mkshort_handle) {
    mkshort_del_handle(&mkshort_handle);
  }
#if NEWQ
  // TODO: eventually we shoud pass the list instead of the queue.
  waypt_flush(&waypt_head);
#else
  waypt_flush(&waypt_head);
#endif
}

void
waypt_backup(signed int* count, queue** head_bak)
{
  queue* elem, *tmp, *qbackup;
  Waypoint* wpt;
  int no = 0;

  qbackup = (queue*) xcalloc(1, sizeof(*qbackup));
  QUEUE_INIT(qbackup);
#if NEWQ
// Why does this code exist?
//abort();
#else
  QUEUE_MOVE(qbackup, &waypt_head);
  QUEUE_INIT(&waypt_head);
#endif

  waypt_ct = 0;

  QUEUE_FOR_EACH(qbackup, elem, tmp) {
    wpt = (Waypoint*)elem;
    waypt_add(new Waypoint(*wpt));
    no++;
  }

  *head_bak = qbackup;
  *count = no;
}

void
waypt_restore(signed int count, queue* head_bak)
{
  if (head_bak == NULL) {
    return;
  }

#if NEWQ
//abort();
#else
  waypt_flush(&waypt_head);
  QUEUE_INIT(&waypt_head);
  QUEUE_MOVE(&waypt_head, head_bak);
#endif
  waypt_ct = count;
  xfree(head_bak);
}

void
waypt_add_url(Waypoint* wpt, const QString& link, const QString& url_link_text)
{
  wpt->url_link_list_.push_back(UrlLink(link, url_link_text));
}

void
waypt_add_url(Waypoint* wpt, const QString& link, const QString& url_link_text, const QString& url_link_type)
{
  wpt->url_link_list_.push_back(UrlLink(link, url_link_text, url_link_type));
}

double
gcgeodist(const double lat1, const double lon1,
          const double lat2, const double lon2)
{
  double res;

  res = radtometers(gcdist(RAD(lat1), RAD(lon1), RAD(lat2), RAD(lon2)));
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
    return (double) 0;
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

  if ((A == NULL) || (B == NULL)) {
    return 0;
  }

  if ((gmsd = GMSD_FIND(A)) && (gmsd->ilinks != NULL)) {
    garmin_ilink_t* link = gmsd->ilinks;

    res = gcgeodist(A->latitude, A->longitude, link->lat, link->lon);
    while (link->next != NULL) {
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
  if ((A == NULL) || (B == NULL)) {
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
  double dist, time;

  dist = waypt_distance_ex(A, B);
  if (dist == 0) {
    return 0;
  }

  time = fabs((double)A->creation_time.msecsTo(B->creation_time)) / 1000.0;
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
  double dist, time;

  dist = waypt_distance(A, B);
  if (dist == 0) {
    return 0;
  }

  time = fabs((double)A->creation_time.msecsTo(B->creation_time)) / 1000.0;
  if (time > 0) {
    return (dist / time);
  } else {
    return 0;
  }
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
  // Q(),
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
  fs(NULL),
  session(curr_session()),
  extra_data(NULL)
{
  QUEUE_INIT(&Q);
}

Waypoint::~Waypoint()
{
  if (gc_data != &Waypoint::empty_gc_data) {
    delete gc_data;
  }
  fs_chain_destroy(fs);
}

Waypoint::Waypoint(const Waypoint& other) :
  // Q(other.Q),
  latitude(other.latitude),
  longitude(other.longitude),
  altitude(other.altitude),
  geoidheight(other.geoidheight),
  depth(other.depth),
  proximity(other.proximity),
  shortname(other.shortname),
  description(other.description),
  notes(other.notes),
  url_link_list_(other.url_link_list_),
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

  /*
   * It's important that this duplicated waypoint not appear
   * on the master Q.
   */
  QUEUE_INIT(&Q);

  // deep copy fs chain data.
  fs = fs_chain_copy(other.fs);

  // note: session is not deep copied.
  // note: extra_data is not deep copied.
}

Waypoint& Waypoint::operator=(const Waypoint& other)
{
  // the default assignment operator is not appropriate as we do deep copy of some members,
  // and we haven't bothered to write an appropriate one.
  // this is a dummy so the compiler can catch attempts to use the assignment operator.
  return *this;
}

bool
Waypoint::HasUrlLink() const
{
  return !url_link_list_.isEmpty();
}

const UrlLink&
Waypoint::GetUrlLink() const
{
  return url_link_list_[0];
}

const QList<UrlLink>
Waypoint::GetUrlLinks() const
{
  return url_link_list_;
}

void
Waypoint::AddUrlLink(const UrlLink l)
{
  url_link_list_.push_back(l);
}

QString
Waypoint::CreationTimeXML() const
{
  if (!creation_time.isValid()) {
    return NULL;
  }

  QDateTime dt = GetCreationTime().toUTC();
// qDebug() << dt.toString("dd.MM.yyyy hh:mm:ss.zzz")  << " CML " << microseconds;

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
Waypoint::SetCreationTime(gpsbabel::DateTime t)
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
