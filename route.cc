/*
    Copyright (C) 2002-2010 Robert Lipe, robertlipe+source@gpsbabel.org

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
#include <cstddef>              // for nullptr_t
#include <algorithm>            // for sort
#include <iterator>

#include <QtCore/QDateTime>     // for QDateTime
#include <QtCore/QList>         // for QList<>::iterator
#include <QtCore/QString>       // for QString
#include <QtCore/QtGlobal>      // for foreach

#include "defs.h"
#include "grtcirc.h"            // for RAD, gcdist, heading_true_degrees, radtometers
#include "session.h"            // for curr_session, session_t (ptr only)
#include "src/core/datetime.h"  // for DateTime
#include "src/core/optional.h"  // for optional, operator>, operator<


RouteList* global_route_list;
RouteList* global_track_list;

extern void update_common_traits(const Waypoint* wpt);

void
route_init()
{
  global_route_list = new RouteList;
  global_track_list = new RouteList;
}

unsigned int
route_waypt_count()
{
  /* total waypoint count -- all routes */
  return global_route_list->waypt_count();
}

unsigned int
route_count()
{
  return global_route_list->count();	/* total # of routes */
}

unsigned int
track_waypt_count()
{
  /* total waypoint count -- all tracks */
  return global_track_list->waypt_count();
}

unsigned int
track_count()
{
  return global_track_list->count();	/* total # of tracks */
}

// FIXME: provide a method to deallocate a head that isn't added onto a route list,
// or just let the users allocate with new and deallocate with delete.
route_head*
route_head_alloc()
{
  return new route_head;
}

void
route_add_head(route_head* rte)
{
  global_route_list->add_head(rte);
}

void
route_del_head(route_head* rte)
{
  global_route_list->del_head(rte);
}

void
track_add_head(route_head* rte)
{
  global_track_list->add_head(rte);
}

void
track_del_head(route_head* rte)
{
  global_track_list->del_head(rte);
}

void
track_insert_head(route_head* rte, route_head* predecessor)
{
  global_track_list->insert_head(rte, predecessor);
}

void
route_add_wpt(route_head* rte, Waypoint* wpt, const QString& namepart, int number_digits)
{
  // First point in a route is always a new segment.
  // This improves compatibility when reading from
  // segment-unaware formats.
  if (rte->waypoint_list.empty()) {
    wpt->wpt_flags.new_trkseg = 1;
  }

  global_route_list->add_wpt(rte, wpt, true, namepart, number_digits);
}

void
track_add_wpt(route_head* rte, Waypoint* wpt, const QString& namepart, int number_digits)
{
  // First point in a track is always a new segment.
  // This improves compatibility when reading from
  // segment-unaware formats.
  if (rte->waypoint_list.empty()) {
    wpt->wpt_flags.new_trkseg = 1;
  }

  // FIXME: It is misleading to accept namepart and number_digits parameters which
  // are ignored because synth is set to false.
  global_track_list->add_wpt(rte, wpt, false, namepart, number_digits);
}

void
route_del_wpt(route_head* rte, Waypoint* wpt)
{
  global_route_list->del_wpt(rte, wpt);
}

void
track_del_wpt(route_head* rte, Waypoint* wpt)
{
  global_track_list->del_wpt(rte, wpt);
}

void
route_disp(const route_head* /* rh */, std::nullptr_t /* wc */)
{
// wc == nullptr
}

void
route_disp_session(const session_t* se, route_hdr rh, route_trl rt, waypt_cb wc)
{
  global_route_list->common_disp_session(se, rh, rt, wc);
}

void
track_disp_session(const session_t* se, route_hdr rh, route_trl rt, waypt_cb wc)
{
  global_track_list->common_disp_session(se, rh, rt, wc);
}

void
route_flush_all_routes()
{
  global_route_list->flush();
}

void
route_flush_all_tracks()
{
  global_track_list->flush();
}

void
route_deinit()
{
  route_flush_all_routes();
  route_flush_all_tracks();
  delete global_route_list;
  delete global_track_list;
}

void
route_append(RouteList* src)
{
  src->copy(&global_route_list);
}

void
track_append(RouteList* src)
{
  src->copy(&global_track_list);
}

void
route_backup(RouteList** head_bak)
{
  global_route_list->copy(head_bak);
}

void
route_restore(RouteList* head_bak)
{
  global_route_list->restore(head_bak);
}

void
route_swap(RouteList& other)
{
  global_route_list->swap(other);
}

void
route_sort(RouteList::Compare cmp)
{
  global_route_list->sort(cmp);
}

void
track_backup(RouteList** head_bak)
{
  global_track_list->copy(head_bak);
}

void
track_restore(RouteList* head_bak)
{
  global_track_list->restore(head_bak);
}

void
track_swap(RouteList& other)
{
  global_track_list->swap(other);
}

void
track_sort(RouteList::Compare cmp)
{
  global_track_list->sort(cmp);
}

/*
 * This really makes more sense for tracks than routes.
 * Run over all the trackpoints, computing heading (course), speed, and
 * and so on.
 *
 * If trkdatap is non-null upon entry, a pointer to an allocated collection
 * (hopefully interesting) statistics about the track will be placed there.
 */
computed_trkdata track_recompute(const route_head* trk)
{
  Waypoint first;
  const Waypoint* prev = &first;
  int tkpt = 0;
  int pts_hrt = 0;
  double tot_hrt = 0.0;
  int pts_cad = 0;
  double tot_cad = 0.0;
  computed_trkdata tdata;

//  first.latitude = 0;
//  first.longitude = 0;
//  first.creation_time = 0;

  foreach (Waypoint* thisw, trk->waypoint_list) {

    /*
     * gcdist and heading want radians, not degrees.
     */
    double tlat = RAD(thisw->latitude);
    double tlon = RAD(thisw->longitude);
    double plat = RAD(prev->latitude);
    double plon = RAD(prev->longitude);
    WAYPT_SET(thisw, course, heading_true_degrees(plat, plon,
              tlat, tlon));
    double dist = radtometers(gcdist(plat, plon, tlat, tlon));

    /*
     * Avoid that 6300 mile jump as we move from 0,0.
     */
    if (plat && plon) {
      tdata.distance_meters += dist;
    }

    /*
     * If we've moved as much as a meter,
     * conditionally recompute speeds.
     */
    if (!WAYPT_HAS(thisw, speed) && (dist > 1)) {
      // Only recompute speed if the waypoint
      // didn't already have a speed
      if (thisw->GetCreationTime().isValid() &&
          prev->GetCreationTime().isValid() &&
          thisw->GetCreationTime() > prev->GetCreationTime()) {
        double timed =
          prev->GetCreationTime().msecsTo(thisw->GetCreationTime()) / 1000.0;
        WAYPT_SET(thisw, speed, dist / timed);
      }
    }
    if (WAYPT_HAS(thisw, speed)) {
      if ((!tdata.min_spd) || (thisw->speed < tdata.min_spd)) {
        tdata.min_spd = thisw->speed;
      }
      if ((!tdata.max_spd) || (thisw->speed > tdata.max_spd)) {
        tdata.max_spd = thisw->speed;
      }
    }

    if (thisw->altitude != unknown_alt) {
      if ((!tdata.min_alt) || (thisw->altitude < tdata.min_alt)) {
        tdata.min_alt = thisw->altitude;
      }
      if ((!tdata.max_alt) || (thisw->altitude > tdata.max_alt)) {
        tdata.max_alt = thisw->altitude;
      }
    }

    if (thisw->heartrate > 0) {
      pts_hrt++;
      tot_hrt += thisw->heartrate;
    }

    if (thisw->heartrate > 0) {
      if ((!tdata.min_hrt) || (thisw->heartrate < tdata.min_hrt)) {
        tdata.min_hrt = thisw->heartrate;
      }
      if ((!tdata.max_hrt) || (thisw->heartrate > tdata.max_hrt)) {
        tdata.max_hrt = thisw->heartrate;
      }
    }

    if (thisw->cadence > 0) {
      pts_cad++;
      tot_cad += thisw->cadence;
    }

    if ((thisw->cadence > 0) && ((!tdata.max_cad) || (thisw->cadence > tdata.max_cad))) {
      tdata.max_cad = thisw->cadence;
    }

    if (thisw->GetCreationTime().isValid()) {
      if (!tdata.start.isValid() || (thisw->GetCreationTime() < tdata.start)) {
        tdata.start = thisw->GetCreationTime();
      }

      if (!tdata.end.isValid() || (thisw->GetCreationTime() > tdata.end)) {
        tdata.end = thisw->GetCreationTime();
      }
    }

    if (thisw->shortname.isEmpty()) {
      thisw->shortname = QString("%1-%2").arg(trk->rte_name).arg(tkpt);
    }
    tkpt++;
    prev = thisw;
  }

  if (pts_hrt > 0) {
    tdata.avg_hrt = tot_hrt / pts_hrt;
  }

  if (pts_cad > 0) {
    tdata.avg_cad = tot_cad / pts_cad;
  }

  return tdata;
}

route_head::route_head() :
  rte_num(0),
  rte_waypt_ct(0),
  fs(nullptr),
  cet_converted(0),
  // line_color(),
  line_width(-1),
  session(curr_session())
{
};

route_head::~route_head()
{
  waypoint_list.flush();
  if (fs) {
    fs_chain_destroy(fs);
  }
}

int RouteList::waypt_count() const
{
  return waypt_ct;
}

// rte may or may not contain waypoints in it's waypoint_list.
// FIXME: In the case that it does, our count of total waypoints won't
// match until after rte is added.
// examples are in tests for garmin_txt, gdb, ggv_log, ik3d, navitel, osm.
void
RouteList::add_head(route_head* rte)
{
  this->append(rte);
}

void
RouteList::del_head(route_head* rte)
{
  waypt_ct -= rte->rte_waypt_ct;
  const int idx = this->indexOf(rte);
  assert(idx >= 0);
  removeAt(idx);
  delete rte;
}

void
RouteList::insert_head(route_head* rte, route_head* predecessor)
{
  const int idx = this->indexOf(predecessor);
  assert(idx >= 0);
  this->insert(idx + 1, rte);
}

// Synthesizing names based on the total number of waypoints in the RouteList makes
// it advantageous to keep a count of the total number of waypoints in all the routes
// in the RouteList AND any routes that have had waypoints added but haven't been
// added themselves yet.
void
RouteList::add_wpt(route_head* rte, Waypoint* wpt, bool synth, const QString& namepart, int number_digits)
{
  rte->rte_waypt_ct++;	/* waypoints in this route */
  ++waypt_ct;
  rte->waypoint_list.add_rte_waypt(waypt_ct, wpt, synth, namepart, number_digits);
  if ((this == global_route_list) || (this == global_track_list)) {
    update_common_traits(wpt);
  }
}

void
RouteList::del_wpt(route_head* rte, Waypoint* wpt)
{
  rte->waypoint_list.del_rte_waypt(wpt);
  rte->rte_waypt_ct--;
  --waypt_ct;
}

void
RouteList::common_disp_session(const session_t* se, route_hdr rh, route_trl rt, waypt_cb wc)
{
  foreach (const route_head* rhp, *this) {
    if (rhp->session == se) {
      if (rh) {
        (*rh)(rhp);
      }
      route_disp(rhp, wc);
      if (rt) {
        (*rt)(rhp);
      }
    }
  }
}

void
RouteList::flush()
{
  while (!isEmpty()) {
    delete takeFirst();
  }
  waypt_ct = 0;
}

void
RouteList::copy(RouteList** dst) const
{
  if (*dst == nullptr) {
    *dst = new RouteList;
  }

  const char RPT[] = "RPT";
  foreach (const route_head* rte_old, *this) {
    route_head* rte_new = route_head_alloc();
    rte_new->rte_name = rte_old->rte_name;
    rte_new->rte_desc = rte_old->rte_desc;
    rte_new->rte_urls = rte_old->rte_urls;
    rte_new->fs = fs_chain_copy(rte_old->fs);
    rte_new->rte_num = rte_old->rte_num;
    (*dst)->add_head(rte_new);
    foreach (const Waypoint* old_wpt, rte_old->waypoint_list) {
      (*dst)->add_wpt(rte_new, new Waypoint(*old_wpt), false, RPT, 3);
    }
  }
}

void
RouteList::restore(RouteList* src)
{
  if (src == nullptr) {
    return;
  }
  flush();

  *this = *src;
  src->clear();
  src->waypt_ct = 0;
}

void RouteList::swap(RouteList& other)
{
  const RouteList tmp_list = *this;
  *this = other;
  other = tmp_list;
}

void RouteList::sort(Compare cmp)
{
  std::sort(begin(), end(), cmp);
}
