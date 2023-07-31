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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

 */

#include <cassert>              // for assert
#include <cstddef>              // for nullptr_t
#include <optional>             // for optional, operator>, operator<

#include <QDateTime>            // for operator>, QDateTime, operator<
#include <QList>                // for QList<>::const_iterator
#include <QString>              // for QString
#include <QStringLiteral>       // for qMakeStringPrivate, QStringLiteral
#include <QStringView>          // for QStringView
#include <QtGlobal>             // for QForeachContainer, qMakeForeachContainer, foreach

#include "defs.h"
#include "formspec.h"           // for FormatSpecificDataList
#include "grtcirc.h"            // for RAD, gcdist, heading_true_degrees, radtometers
#include "session.h"            // for curr_session, session_t (ptr only)
#include "src/core/datetime.h"  // for DateTime


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
route_add_wpt(route_head* rte, Waypoint* wpt, QStringView namepart, int number_digits)
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
track_add_wpt(route_head* rte, Waypoint* wpt, QStringView namepart, int number_digits)
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
route_swap_wpts(route_head* rte, WaypointList& other)
{
  global_route_list->swap_wpts(rte, other);
}

void
track_swap_wpts(route_head* rte, WaypointList& other)
{
  global_track_list->swap_wpts(rte, other);
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

/*
 * This really makes more sense for tracks than routes.
 * Run over all the trackpoints, computing heading (course), speed, and
 * and so on.
 *
 * return a collection of (hopefully interesting) statistics about the track.
 */
computed_trkdata track_recompute(const route_head* trk)
{
  const Waypoint* prev = nullptr;
  int tkpt = 0;
  int pts_hrt = 0;
  double tot_hrt = 0.0;
  int pts_cad = 0;
  double tot_cad = 0.0;
  int pts_pwr = 0;
  double tot_pwr = 0.0;
  computed_trkdata tdata;

  foreach (Waypoint* thisw, trk->waypoint_list) {

    if (prev != nullptr) {
      /*
       * gcdist and heading want radians, not degrees.
       */
      double tlat = RAD(thisw->latitude);
      double tlon = RAD(thisw->longitude);
      double plat = RAD(prev->latitude);
      double plon = RAD(prev->longitude);
      if (!thisw->course_has_value()) {
        // Only recompute course if the waypoint
        // didn't already have a course.
        thisw->set_course(heading_true_degrees(plat, plon, tlat, tlon));
      }
      double dist = radtometers(gcdist(plat, plon, tlat, tlon));
      tdata.distance_meters += dist;

      /*
       * If we've moved as much as a meter,
       * conditionally recompute speeds.
       */
      if (!thisw->speed_has_value() && (dist > 1)) {
        // Only recompute speed if the waypoint
        // didn't already have a speed
        if (thisw->GetCreationTime().isValid() &&
            prev->GetCreationTime().isValid() &&
            thisw->GetCreationTime() > prev->GetCreationTime()) {
          double timed =
            prev->GetCreationTime().msecsTo(thisw->GetCreationTime()) / 1000.0;
          thisw->set_speed(dist / timed);
        }
      }
    }

    if (thisw->speed_has_value()) {
      if ((!tdata.min_spd) || (thisw->speed_value() < tdata.min_spd)) {
        tdata.min_spd = thisw->speed_value();
      }
      if ((!tdata.max_spd) || (thisw->speed_value() > tdata.max_spd)) {
        tdata.max_spd = thisw->speed_value();
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

    if (thisw->power > 0) {
      pts_pwr++;
      tot_pwr += thisw->power;
    }

    if ((thisw->power > 0) && ((!tdata.max_pwr) || (thisw->power > tdata.max_pwr))) {
      tdata.max_pwr = thisw->power;
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
      thisw->shortname = QStringLiteral("%1-%2").arg(trk->rte_name).arg(tkpt);
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

  if (pts_pwr > 0) {
    tdata.avg_pwr = tot_pwr / pts_pwr;
  }

  return tdata;
}

route_head::route_head() :
  rte_num(0),
  // line_color(),
  line_width(-1),
  session(curr_session())
{
}

route_head::~route_head()
{
  waypoint_list.flush();
  fs.FsChainDestroy();
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
  waypt_ct -= rte->rte_waypt_ct();
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
RouteList::add_wpt(route_head* rte, Waypoint* wpt, bool synth, QStringView namepart, int number_digits)
{
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

  for (const auto& rte_old : *this) {
    auto* rte_new = new route_head;
    // waypoint_list created below with add_wpt.
    rte_new->rte_name = rte_old->rte_name;
    rte_new->rte_desc = rte_old->rte_desc;
    rte_new->rte_urls = rte_old->rte_urls;
    rte_new->rte_num = rte_old->rte_num;
    rte_new->fs = rte_old->fs.FsChainCopy();
    rte_new->line_color = rte_old->line_color;
    rte_new->line_width = rte_old->line_width;
    rte_new->session = rte_old->session;
    (*dst)->add_head(rte_new);
    const auto& old_list = rte_old->waypoint_list;
    for (const auto& old_wpt : old_list) {
      (*dst)->add_wpt(rte_new, new Waypoint(*old_wpt), false, u"RPT", 3);
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

void RouteList::swap_wpts(route_head* rte, WaypointList& other)
{
  this->waypt_ct -= rte->rte_waypt_ct();
  this->waypt_ct += other.count();
  rte->waypoint_list.swap(other);
}
