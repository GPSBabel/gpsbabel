/*
    Copyright (C) 2002-2010 Robert Lipe, robertlipe@gpsbabel.org

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
#include "defs.h"
#include "grtcirc.h"
#include "session.h"

static queue my_route_head;
static queue my_track_head;
static int rte_head_ct;
static int rte_waypts;
static int trk_head_ct;
static int trk_waypts;

extern void update_common_traits(const waypoint* wpt);

void
route_init(void)
{
  QUEUE_INIT(&my_route_head);
  QUEUE_INIT(&my_track_head);
}

unsigned int
route_waypt_count(void)
{
  /* total wapoint count -- all routes */
  return rte_waypts;
}

unsigned int
route_count(void)
{
  return rte_head_ct;	/* total # of routes */
}

unsigned int
track_waypt_count(void)
{
  /* totaly waypoint count -- all tracks */
  return trk_waypts;
}

unsigned int
track_count(void)
{
  return trk_head_ct;	/* total # of tracks */
}

route_head *
route_head_alloc(void)
{
  route_head* rte_head = new route_head;
  QUEUE_INIT(&rte_head->Q);
  QUEUE_INIT(&rte_head->waypoint_list);
  rte_head->session = curr_session();
  return rte_head;
}

static void
any_route_free(route_head *rte)
{
  if (rte->rte_name) {
    xfree(rte->rte_name);
  }
  if (rte->rte_desc) {
    xfree(rte->rte_desc);
  }
  waypt_flush(&rte->waypoint_list);
  if (rte->fs) {
    fs_chain_destroy(rte->fs);
  }
  delete rte;
  rte = NULL;
}


static void
any_route_add_head(route_head *rte, queue *head)
{
  ENQUEUE_TAIL(head, &rte->Q);
}

static void
any_route_del_head(route_head *rte)
{
  dequeue(&rte->Q);
  any_route_free(rte);
}

void
route_add_head(route_head *rte)
{
  any_route_add_head(rte, &my_route_head);
  rte_head_ct++;
}

void
route_del_head(route_head *rte)
{
  any_route_del_head(rte);
  rte_head_ct--;
}

void
track_add_head(route_head *rte)
{
  any_route_add_head(rte, &my_track_head);
  trk_head_ct++;
}

void
track_del_head(route_head *rte)
{
  any_route_del_head(rte);
  trk_head_ct--;
}

void
track_insert_head(route_head *rte, route_head *predecessor)
{
  ENQUEUE_AFTER(&predecessor->Q, &rte->Q);
  trk_head_ct++;
}

static
route_head *
common_route_by_name(queue *routes, const char *name)
{
  queue *elem, *tmp;
  route_head *rte;

  QUEUE_FOR_EACH(routes, elem, tmp) {
    rte = (route_head *) elem;
    if (0 == strcmp(rte->rte_name, name)) {
      return rte;
    }
  }

  return NULL;
}

route_head *
route_find_route_by_name(const char *name)
{
  return common_route_by_name(&my_route_head, name);
}

route_head *
route_find_track_by_name(const char *name)
{
  return common_route_by_name(&my_track_head, name);
}

static void
any_route_add_wpt(route_head *rte, waypoint *wpt, int *ct, int synth)
{
  ENQUEUE_TAIL(&rte->waypoint_list, &wpt->Q);
  rte->rte_waypt_ct++;	/* waypoints in this route */
  if (ct) {
    (*ct)++;
  }
  if (synth && !wpt->shortname) {
    char tmpnam[10];
    snprintf(tmpnam, sizeof(tmpnam), "RPT%03d",*ct);
    wpt->shortname = xstrdup(tmpnam);
    wpt->wpt_flags.shortname_is_synthetic = 1;
  }
  update_common_traits(wpt);
}

void
route_add_wpt(route_head *rte, waypoint *wpt)
{
  // First point in a route is always a new segment.
  // This improves compatibility when reading from
  // segment-unaware formats.
  if (QUEUE_EMPTY(&rte->waypoint_list)) {
    wpt->wpt_flags.new_trkseg = 1;
  }

  any_route_add_wpt(rte, wpt, &rte_waypts, 1);
}

void
track_add_wpt(route_head *rte, waypoint *wpt)
{
  // First point in a track is always a new segment.
  // This improves compatibility when reading from
  // segment-unaware formats.
  if (QUEUE_EMPTY(&rte->waypoint_list)) {
    wpt->wpt_flags.new_trkseg = 1;
  }

  any_route_add_wpt(rte, wpt, &trk_waypts, 0);
}

waypoint *
route_find_waypt_by_name(route_head *rh, const char *name)
{
  queue *elem, *tmp;

  QUEUE_FOR_EACH(&rh->waypoint_list, elem, tmp) {
    waypoint *waypointp = (waypoint *) elem;
    if (0 == strcmp(waypointp->shortname, name)) {
      return waypointp;
    }
  }
  return NULL;
}

static void
any_route_del_wpt(route_head *rte, waypoint *wpt, int *ct)
{
  if (wpt->wpt_flags.new_trkseg && wpt != (waypoint*)QUEUE_LAST(&rte->waypoint_list)) {
    waypoint* wpt_next = (waypoint*)QUEUE_NEXT(&wpt->Q);
    wpt_next->wpt_flags.new_trkseg = 1;
  }
  wpt->wpt_flags.new_trkseg = 0;
  dequeue(&wpt->Q);
  rte->rte_waypt_ct--;
  if (ct) {
    (*ct)--;
  }
}

void
route_del_wpt(route_head *rte, waypoint *wpt)
{
  any_route_del_wpt(rte, wpt, &rte_waypts);
}

void
track_del_wpt(route_head *rte, waypoint *wpt)
{
  any_route_del_wpt(rte, wpt, &trk_waypts);
}

void
route_disp(const route_head *rh, waypt_cb cb)
{
  queue *elem, *tmp;
  if (!cb)  {
    return;
  }
  QUEUE_FOR_EACH(&rh->waypoint_list, elem, tmp) {
    waypoint *waypointp;
    waypointp = (waypoint *) elem;
    (*cb)(waypointp);
  }

}

void
route_reverse(const route_head *rte_hd)
{
  /* Cast away const-ness */
  route_head *rh = (route_head *) rte_hd;
  queue *elem, *tmp;
  QUEUE_FOR_EACH(&rh->waypoint_list, elem, tmp) {
    ENQUEUE_HEAD(&rh->waypoint_list, dequeue(elem));
  }
}

static void
common_disp_all(queue *qh, route_hdr rh, route_trl rt, waypt_cb wc)
{
  queue *elem, *tmp;
  QUEUE_FOR_EACH(qh, elem, tmp) {
    const route_head *rhp;
    rhp = (route_head *) elem;
    if (rh) {
      (*rh)(rhp);
    }
    route_disp(rhp, wc);
    if (rt) {
      (*rt)(rhp);
    }
  }
}

static void
common_disp_session(const session_t *se, queue *qh, route_hdr rh, route_trl rt, waypt_cb wc)
{
  queue *elem, *tmp;
  QUEUE_FOR_EACH(qh, elem, tmp) {
    const route_head *rhp;
    rhp = (route_head *) elem;
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
route_disp_all(route_hdr rh, route_trl rt, waypt_cb wc)
{
  common_disp_all(&my_route_head, rh, rt, wc);
}

void
route_disp_session(const session_t *se, route_hdr rh, route_trl rt, waypt_cb wc)
{
  common_disp_session(se, &my_route_head, rh, rt, wc);
}

void
track_disp_all(route_hdr rh, route_trl rt, waypt_cb wc)
{
  common_disp_all(&my_track_head, rh, rt, wc);
}

void
track_disp_session(const session_t *se, route_hdr rh, route_trl rt, waypt_cb wc)
{
  common_disp_session(se, &my_track_head, rh, rt, wc);
}

static void
route_flush_q(queue *head)
{
  queue *elem, *tmp;
  queue *q;

  QUEUE_FOR_EACH(head, elem, tmp) {
    q = dequeue(elem);
    any_route_free((route_head *) q);
  }
}

void
route_flush_all_routes(void)
{
  route_flush_q(&my_route_head);
  rte_head_ct = 0;
  rte_waypts = 0;
}

void
route_flush_all_tracks(void)
{
  route_flush_q(&my_track_head);
  trk_head_ct = 0;
  trk_waypts = 0;
}

void
route_flush_all()
{
  route_flush_all_tracks();
  route_flush_all_routes();
}

void
route_flush(queue *head)
{
  queue *elem, *tmp;
  queue *q;
  QUEUE_FOR_EACH(head, elem, tmp) {
    q = dequeue(elem);
    any_route_free((route_head *)q);
  }
}

void
route_copy(int *dst_count, int *dst_wpt_count, queue **dst, queue *src)
{
  queue *elem, *tmp, *elem2, *tmp2;
  route_head *rte_new;
  int junk;
  if (!dst_wpt_count) {
    dst_wpt_count = &junk;
  }

  if (!*dst) {
    *dst = (queue *)xcalloc(1, sizeof(queue));
    QUEUE_INIT(*dst);
    *dst_count = 0;
    *dst_wpt_count = 0;
  }
  QUEUE_FOR_EACH(src, elem, tmp) {
    route_head *rte_old = (route_head *)elem;

    rte_new = route_head_alloc();
    rte_new->rte_name = xstrdup(rte_old->rte_name);
    rte_new->rte_desc = xstrdup(rte_old->rte_desc);
    rte_new->rte_url = rte_old->rte_url;
    rte_new->fs = fs_chain_copy(rte_old->fs);
    rte_new->rte_num = rte_old->rte_num;
    any_route_add_head(rte_new, *dst);
    QUEUE_FOR_EACH(&rte_old->waypoint_list, elem2, tmp2) {
      any_route_add_wpt(rte_new, waypt_dupe((waypoint *)elem2), dst_wpt_count, 0);
    }
    (*dst_count)++;
  }
}

void
route_append(queue *src)
{
  queue *dst = &my_route_head;
  route_copy(&rte_head_ct, &rte_waypts, &dst, src);
}

void
track_append(queue *src)
{
  queue *dst = &my_track_head;
  route_copy(&trk_head_ct, &trk_waypts, &dst, src);
}

void
route_backup(signed int *count, queue **head_bak)
{
  route_copy(count, NULL, head_bak, &my_route_head);
}

static void
route_restore_hdr(const route_head *rte)
{
  rte_head_ct++;
}

static void
track_restore_hdr(const route_head *trk)
{
  trk_head_ct++;
}

static void
route_restore_tlr(const route_head *rte)
{
}

static void
route_restore_wpt(const waypoint *wpt)
{
  rte_waypts++;
}

static void
track_restore_wpt(const waypoint *wpt)
{
  trk_waypts++;
}

static void
common_restore_finish(void)
{
  rte_head_ct = 0;
  trk_head_ct = 0;
  rte_waypts = 0;
  trk_waypts = 0;
  route_disp_all(route_restore_hdr, route_restore_tlr, route_restore_wpt);
  track_disp_all(track_restore_hdr, route_restore_tlr, track_restore_wpt);
}

void
route_restore(queue *head_bak)
{
  if (head_bak == NULL) {
    return;
  }

  route_flush_q(&my_route_head);
  QUEUE_INIT(&my_route_head);
  QUEUE_MOVE(&my_route_head, head_bak);

  common_restore_finish();
}

void
track_backup(signed int *count, queue **head_bak)
{
  route_copy(count, NULL, head_bak, &my_track_head);
}

void
track_restore(queue *head_bak)
{
  if (head_bak == NULL) {
    return;
  }

  route_flush_q(&my_track_head);
  QUEUE_INIT(&my_track_head);
  QUEUE_MOVE(&my_track_head, head_bak);

  common_restore_finish();
}

/*
 * Move the entire track queue onto the route queue making no attempt
 * at all to "fix" anything in the process.
 */
void
routes_to_tracks(void)
{
  queue *elem, *tmp;

  QUEUE_FOR_EACH(&my_route_head, elem, tmp) {
    route_head *trk = (route_head *) elem;
    dequeue(&trk->Q);
    ENQUEUE_TAIL(&my_track_head, &trk->Q);
  }
}

/*
 * Same, but in opposite direction.
 */
void
tracks_to_routes(void)
{
  queue *elem, *tmp;

  QUEUE_FOR_EACH(&my_track_head, elem, tmp) {
    route_head *trk = (route_head *) elem;
    dequeue(&trk->Q);
    ENQUEUE_TAIL(&my_route_head, &trk->Q);
  }
}


/*
 * This really makes more sense for tracks than routes.
 * Run over all the trackpoints, computing heading (course), speed, and
 * and so on.
 *
 * If trkdatap is non-null upon entry, a pointer to an allocated collection
 * (hopefully interesting) statistics about the track will be placed there.
 */
void track_recompute(const route_head *trk, computed_trkdata **trkdatap)
{
  waypoint first;
  waypoint *thisw;
  waypoint *prev = &first;
  queue *elem, *tmp;
  int tkpt = 0;
  int pts_hrt = 0;
  double tot_hrt = 0.0;
  int pts_cad = 0;
  double tot_cad = 0.0;
  char tkptname[100];
  computed_trkdata *tdata = (computed_trkdata *)xcalloc(1, sizeof(computed_trkdata));

  if (trkdatap) {
    *trkdatap = tdata;
  }

  first.latitude = 0;
  first.longitude = 0;
  first.creation_time = 0;
  tdata->min_hrt =  9999;
  tdata->min_alt = -unknown_alt;
  tdata->max_alt =  unknown_alt;

  QUEUE_FOR_EACH((queue *)&trk->waypoint_list, elem, tmp) {
    time_t timed;
    double tlat, tlon, plat, plon, dist;

    thisw = (waypoint *)elem;
    timed = thisw->creation_time - prev->creation_time;

    /*
     * gcdist and heading want radians, not degrees.
     */
    tlat = RAD(thisw->latitude);
    tlon = RAD(thisw->longitude);
    plat = RAD(prev->latitude);
    plon = RAD(prev->longitude);
    WAYPT_SET(thisw, course, heading_true_degrees(plat, plon,
              tlat, tlon));
    dist = radtometers(gcdist(plat, plon, tlat, tlon));

    /*
     * Avoid that 6300 mile jump as we move from 0,0.
     */
    if (plat && plon) {
      tdata->distance_meters += dist;
    }

    /*
     * If we've moved as much as a meter,
     * conditionally recompute speeds.
     */
    if (timed && (dist > 1)) {
      if (!WAYPT_HAS(thisw, speed)) {
        // Only recompute speed if the waypoint
        // didn't already have a speed
        WAYPT_SET(thisw, speed, dist / labs(timed));
      }
      if (thisw->speed > tdata->max_spd) {
        tdata->max_spd = thisw->speed;
      }
      if (thisw->speed < tdata->min_spd) {
        tdata->min_spd = thisw->speed;
      }
    }

    if (thisw->altitude != unknown_alt) {
      if (thisw->altitude < tdata->min_alt) {
        tdata->min_alt = thisw->altitude;
      }
      if (thisw->altitude > tdata->max_alt) {
        tdata->max_alt = thisw->altitude;
      }
    }

    if (thisw->heartrate > 0) {
      pts_hrt++;
      tot_hrt += (float) thisw->heartrate;
    }

    if ((thisw->heartrate > 0) && (thisw->heartrate < tdata->min_hrt)) {
      tdata->min_hrt = (int) thisw->heartrate;
    }

    if ((thisw->heartrate > 0) && (thisw->heartrate > tdata->max_hrt)) {
      tdata->max_hrt = (int) thisw->heartrate;
    }

    if (thisw->cadence > 0) {
      pts_cad++;
      tot_cad += (float) thisw->cadence;
    }

    if ((thisw->cadence > 0) && (thisw->cadence > tdata->max_cad)) {
      tdata->max_cad = (int) thisw->cadence;
    }

    if (thisw->creation_time && (thisw->creation_time < tdata->start)) {
      tdata->start = thisw->creation_time;
    }

    if (thisw->creation_time > tdata->end) {
      tdata->end = thisw->creation_time;
      if (tdata->start == 0) {
        tdata->start = tdata->end;
      }
    }
    prev = thisw;
    if (!thisw->shortname || !thisw->shortname[0]) {
      snprintf(tkptname, sizeof(tkptname), "%s-%d",
               trk->rte_name ? trk->rte_name : "" , tkpt);
      thisw->shortname = xstrdup(tkptname);
    }
    tkpt++;
  }

  if (pts_hrt > 0) {
    tdata->avg_hrt = tot_hrt / (float) pts_hrt;
  }

  if (pts_cad > 0) {
    tdata->avg_cad = tot_cad / (float) pts_cad;
  }

  if (!trkdatap) {
    xfree(tdata);
  }
}
