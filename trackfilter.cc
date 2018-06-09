/*

    Track manipulation filter
    Copyright (c) 2009 - 2013 Robert Lipe, robertlipe+source@gpsbabel.org
    Copyright (C) 2005-2006 Olaf Klein, o.b.klein@gpsbabel.org

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


#include "defs.h"
#include "filterdefs.h"
#include "grtcirc.h"
#include "strptime.h"
#include "trackfilter.h"
#include "xmlgeneric.h"
#include <QtCore/QRegExp>
#include <QtCore/QXmlStreamAttributes>
#include <cmath>
#include <cstdio> /* for snprintf */
#include <cstdlib> /* for qsort */

#if FILTERS_ENABLED || MINIMAL_FILTERS
#define MYNAME "trackfilter"

#undef TRACKF_DBG

TrackFilter* TrackFilter::fObj = nullptr; // definition required for odr-use.

/*******************************************************************************
* helpers
*******************************************************************************/

int TrackFilter::trackfilter_opt_count()
{
  int res = 0;
  arglist_t* a = args;

  while (a->argstring) {
    if (*a->argval != nullptr) {
      res++;
    }
    a++;
  }
  return res;
}

int TrackFilter::trackfilter_parse_time_opt(const char* arg)
{
  time_t t0, t1;
  int sign = 1;
  char* cin = (char*)arg;
  char c;

  t0 = t1 = 0;

  while ((c = *cin++)) {
    time_t seconds;

    if (c >= '0' && c <= '9') {
      t1 = (t1 * 10) + (c - '0');
      continue;
    }
    switch (tolower(c)) {
    case 'd':
      seconds = SECONDS_PER_DAY;
      break;
    case 'h':
      seconds = SECONDS_PER_HOUR;
      break;
    case 'm':
      seconds = 60;
      break;
    case 's':
      seconds = 1;
      break;
    case '+':
      sign = +1;
      continue;
    case '-':
      sign = -1;
      continue;
    default:
      fatal(MYNAME "-time: invalid character in time option!\n");
    }
    t0 += (t1 * seconds * sign);
    sign = +1;
    t1 = 0;
  }
  t0 += t1;
  return t0;
}

int TrackFilter::trackfilter_init_qsort_cb(const void* a, const void* b)
{
  const trkflt_t* ra = (const trkflt_t*) a;
  const trkflt_t* rb = (const trkflt_t*) b;
  const QDateTime dta = ra->first_time;
  const QDateTime dtb = rb->first_time;

  if (dta > dtb) {
    return 1;
  }
  if (dta == dtb) {
    return 0;
  }
  return -1;
}

int TrackFilter::trackfilter_merge_qsort_cb(const void* a, const void* b)
{
  const Waypoint* wa = *(Waypoint**)a;
  const Waypoint* wb = *(Waypoint**)b;
  const QDateTime dta = wa->GetCreationTime();
  const QDateTime dtb = wb->GetCreationTime();

  if (dta > dtb) {
    return 1;
  }
  if (dta == dtb) {
    int seqno_a = gb_ptr2int(wa->extra_data);
    int seqno_b = gb_ptr2int(wb->extra_data);
    if (seqno_a > seqno_b) {
      return 1;
    } else if (seqno_a == seqno_b) {
      return 0;
    } else {
      return -1;
    }
  }
  return -1;
}

fix_type TrackFilter::trackfilter_parse_fix(int* nsats)
{
  if (!opt_fix) {
    return fix_unknown;
  }
  if (!case_ignore_strcmp(opt_fix, "pps")) {
    *nsats = 4;
    return fix_pps;
  }
  if (!case_ignore_strcmp(opt_fix, "dgps")) {
    *nsats = 4;
    return fix_dgps;
  }
  if (!case_ignore_strcmp(opt_fix, "3d")) {
    *nsats = 4;
    return fix_3d;
  }
  if (!case_ignore_strcmp(opt_fix, "2d")) {
    *nsats = 3;
    return fix_2d;
  }
  if (!case_ignore_strcmp(opt_fix, "none")) {
    *nsats = 0;
    return fix_none;
  }
  fatal(MYNAME ": invalid fix type\n");
  return fix_unknown;
}

void TrackFilter::trackfilter_fill_track_list_cb(const route_head* track) 	/* callback for track_disp_all */
{
  int i;
  Waypoint* wpt, *prev;
  queue* elem, *tmp;

  if (track->rte_waypt_ct == 0) {
    track_del_head((route_head*)track);
    return;
  }

  if (opt_name != nullptr) {
    if (!QRegExp(opt_name, Qt::CaseInsensitive, QRegExp::WildcardUnix).exactMatch(track->rte_name)) {
      QUEUE_FOR_EACH((queue*)&track->waypoint_list, elem, tmp) {
        Waypoint* wpt = (Waypoint*)elem;
        track_del_wpt((route_head*)track, wpt);
        delete wpt;
      }
      track_del_head((route_head*)track);
      return;
    }
  }

  track_list[track_ct].track = (route_head*)track;

  i = 0;
  prev = nullptr;

  QUEUE_FOR_EACH((queue*)&track->waypoint_list, elem, tmp) {
    track_pts++;

    wpt = (Waypoint*)elem;
    if (!wpt->creation_time.isValid()) {
      timeless_pts++;
    }
    if (!(opt_merge && opt_discard) && (need_time != 0) && (!wpt->creation_time.isValid())) {
      fatal(MYNAME "-init: Found track point at %f,%f without time!\n",
            wpt->latitude, wpt->longitude);
    }

    i++;
    if (i == 1) {
      track_list[track_ct].first_time = wpt->GetCreationTime();
    } else if (i == track->rte_waypt_ct) {
      track_list[track_ct].last_time = wpt->GetCreationTime();
    }

    if ((need_time != 0) && (prev != nullptr) && (prev->GetCreationTime() > wpt->GetCreationTime())) {
      if (opt_merge == nullptr) {
        QString t1 = prev->CreationTimeXML();
        QString t2 = wpt->CreationTimeXML();
        fatal(MYNAME "-init: Track points badly ordered (timestamp %s > %s)!\n", qPrintable(t1), qPrintable(t2));
      }
    }
    prev = wpt;
  }
  track_ct++;
}

void TrackFilter::trackfilter_minpoint_list_cb(const route_head* track)
{
  int minimum_points = atoi(opt_minpoints);
  if (track->rte_waypt_ct < minimum_points) {
    track_del_head((route_head*)track);
    return;
  }
}

/*******************************************************************************
* track title producers
*******************************************************************************/

void TrackFilter::trackfilter_split_init_rte_name(route_head* track, const QDateTime dt)
{
  QString datetimestring;

  if (opt_interval != 0) {
    datetimestring = dt.toUTC().toString("yyyyMMddhhmmss");
  } else {
    datetimestring = dt.toUTC().toString("yyyyMMdd");
  }

  if ((opt_title != nullptr) && (strlen(opt_title) > 0)) {
    if (strchr(opt_title, '%') != nullptr) {
      // Uggh.  strftime format exposed to user.
      char buff[128];

      time_t time = dt.toUTC().toTime_t();
      struct tm tm = *gmtime(&time);

      strftime(buff, sizeof(buff), opt_title, &tm);
      track->rte_name = buff;
    } else {
      track->rte_name = QString("%1-%2").arg(opt_title).arg(datetimestring);
    }
  } else if (!track->rte_name.isEmpty()) {
    track->rte_name = QString("%1-%2").arg(track->rte_name).arg(datetimestring);
  } else {
    track->rte_name = datetimestring;
  }
}

void TrackFilter::trackfilter_pack_init_rte_name(route_head* track, const time_t default_time)
{
  if (strchr(opt_title, '%') != nullptr) {
    struct tm tm;
    Waypoint* wpt;

    if (track->rte_waypt_ct == 0) {
      tm = *localtime(&default_time);
    } else {
      wpt = (Waypoint*) QUEUE_FIRST((queue*)&track->waypoint_list);
      time_t t = wpt->GetCreationTime().toTime_t();
      tm = *localtime(&t);
    }
    char buff[128];
    strftime(buff, sizeof(buff), opt_title, &tm);
    track->rte_name = buff;
  } else {
    track->rte_name = opt_title;
  }
}

/*******************************************************************************
* option "title"
*******************************************************************************/

void TrackFilter::trackfilter_title()
{
  int i;

  if (opt_title == nullptr) {
    return;
  }

  if (strlen(opt_title) == 0) {
    fatal(MYNAME "-title: Missing your title!\n");
  }
  for (i = 0; i < track_ct; i++) {
    route_head* track = track_list[i].track;
    trackfilter_pack_init_rte_name(track, 0);
  }
}

/*******************************************************************************
* option "pack" (default)
*******************************************************************************/

void TrackFilter::trackfilter_pack()
{
  int i, j;
  trkflt_t prev;
  route_head* master;

  for (i = 1, j = 0; i < track_ct; i++, j++) {
    prev = track_list[j];
    if (prev.last_time >= track_list[i].first_time) {
      fatal(MYNAME "-pack: Tracks overlap in time! %s >= %s at %d\n",
            qPrintable(prev.last_time.toString()),
            qPrintable(track_list[i].first_time.toString()), i);
    }
  }

  /* we fill up the first track by all other track points */

  master = track_list[0].track;

  for (i = 1; i < track_ct; i++) {
    queue* elem, *tmp;
    route_head* curr = track_list[i].track;

    QUEUE_FOR_EACH((queue*)&curr->waypoint_list, elem, tmp) {
      Waypoint* wpt = (Waypoint*)elem;
      track_del_wpt(curr, wpt);
      track_add_wpt(master, wpt);
    }
    track_del_head(curr);
    track_list[i].track = nullptr;
  }
  track_ct = 1;
}

/*******************************************************************************
* option "merge"
*******************************************************************************/

void TrackFilter::trackfilter_merge()
{
  int i, j, dropped;

  queue* elem, *tmp;
  Waypoint** buff;
  Waypoint* prev, *wpt;
  route_head* master = track_list[0].track;

  if (track_pts-timeless_pts < 1) {
    return;
  }

  buff = (Waypoint**)xcalloc(track_pts-timeless_pts, sizeof(*buff));

  j = 0;
  for (i = 0; i < track_ct; i++) {	/* put all points into temp buffer */
    route_head* track = track_list[i].track;
    QUEUE_FOR_EACH((queue*)&track->waypoint_list, elem, tmp) {
      wpt = (Waypoint*)elem;
      if (wpt->creation_time.isValid()) {
        buff[j] = new Waypoint(*wpt);
        // augment sort key so a stable sort is possible.
        buff[j]->extra_data = gb_int2ptr(j);
        j++;
        // we will put the merged points in one track segment,
        // as it isn't clear how track segments in the original tracks
        // should relate to the merged track.
        // track_add_wpt will set new_trkseg for the first point
        // after the sort.
        wpt->wpt_flags.new_trkseg = 0;
      }
      track_del_wpt(track, wpt); // copies any new_trkseg flag forward.
      delete wpt;
    }
    if (track != master) {	/* i > 0 */
      track_del_head(track);
    }
  }
  track_ct = 1;

  qsort(buff, track_pts-timeless_pts, sizeof(*buff), trackfilter_merge_qsort_cb);

  dropped = timeless_pts;
  prev = nullptr;

  for (i = 0; i < track_pts-timeless_pts; i++) {
    buff[i]->extra_data = nullptr;
    wpt = buff[i];
    if ((prev == nullptr) || (prev->GetCreationTime() != wpt->GetCreationTime())) {
      track_add_wpt(master, wpt);
      prev = wpt;
    } else {
      delete wpt;
      dropped++;
    }
  }
  xfree(buff);

  if (global_opts.verbose_status > 0) {
    printf(MYNAME "-merge: %d track point(s) merged, %d dropped.\n", track_pts - dropped, dropped);
  }
}

/*******************************************************************************
* option "split"
*******************************************************************************/

void TrackFilter::trackfilter_split()
{
  route_head* curr;
  route_head* master = track_list[0].track;
  int count = master->rte_waypt_ct;

  Waypoint** buff;
  Waypoint* wpt;
  queue* elem, *tmp;
  int i, j;
  double interval = -1;
  double distance = -1;

  if (count <= 1) {
    return;
  }

  /* check additional options */

  opt_interval = (opt_split && (strlen(opt_split) > 0) && (0 != strcmp(opt_split, TRACKFILTER_SPLIT_OPTION)));
  opt_distance = (opt_sdistance && (strlen(opt_sdistance) > 0) && (0 != strcmp(opt_sdistance, TRACKFILTER_SDIST_OPTION)));

  if (opt_interval != 0) {
    double base;
    char   unit;

    switch (strlen(opt_split)) {
    case 0:
      fatal(MYNAME ": No time interval specified.\n");
      break; /* ? */

    case 1:
      unit = *opt_split;
      interval = 1;
      break;

    default:
      i = sscanf(opt_split,"%lf%c", &interval, &unit);
      if (i == 0) {
        /* test reverse order */
        i = sscanf(opt_split,"%c%lf", &unit, &interval);
      }
      if ((i != 2) || (interval <= 0)) {
        fatal(MYNAME ": invalid time interval specified, must be one a positive number.\n");
      }
      break;
    }

    switch (tolower(unit)) {
    case 's':
      base = 1;
      break;
    case 'm':
      base = 60;
      break;
    case 'h':
      base = 60 * 60;
      break;
    case 'd':
      base = 24 * 60 * 60;
      break;
    default:
      fatal(MYNAME ": invalid time interval specified, must be one of [dhms].\n");
      break;
    }
#ifdef TRACKF_DBG
    printf(MYNAME ": unit \"%c\", interval %g -> %g\n", unit, interval, base * interval);
#endif
    interval *= base;
  }

  if (opt_distance != 0) {
    double base;
    char   unit;

    switch (strlen(opt_sdistance)) {
    case 0:
      fatal(MYNAME ": No distance specified.\n");
      break; /* ? */

    case 1:
      unit = *opt_sdistance;
      distance = 1;
      break;

    default:
      i = sscanf(opt_sdistance,"%lf%c", &distance, &unit);
      if (i == 0) {
        /* test reverse order */
        i = sscanf(opt_sdistance,"%c%lf", &unit, &distance);
      }
      if ((i != 2) || (distance <= 0)) {
        fatal(MYNAME ": invalid distance specified, must be one a positive number.\n");
      }
      break;
    }

    switch (tolower(unit)) {
    case 'k': /* kilometers */
      base = 0.6214;
      break;
    case 'm': /* miles */
      base = 1;
      break;
    default:
      fatal(MYNAME ": invalid distance specified, must be one of [km].\n");
      break;
    }
#ifdef TRACKF_DBG
    printf(MYNAME ": unit \"%c\", distance %g -> %g\n", unit, distance, base * distance);
#endif
    distance *= base;
  }

  trackfilter_split_init_rte_name(master, track_list[0].first_time);

  buff = (Waypoint**) xcalloc(count, sizeof(*buff));

  i = 0;
  QUEUE_FOR_EACH((queue*)&master->waypoint_list, elem, tmp) {
    wpt = (Waypoint*)elem;
    buff[i++] = wpt;
  }

  curr = nullptr;	/* will be set by first new track */

  for (i=0, j=1; j<count; i++, j++) {
    int new_track_flag;

    if ((opt_interval == 0) && (opt_distance == 0)) {
      struct tm t1, t2;
// FIXME: This whole function needs to be reconsidered for arbitrary time.
      time_t tt1 = buff[i]->GetCreationTime().toTime_t();
      time_t tt2 = buff[j]->GetCreationTime().toTime_t();

      t1 = *localtime(&tt1);
      t2 = *localtime(&tt2);

      new_track_flag = ((t1.tm_year != t2.tm_year) || (t1.tm_mon != t2.tm_mon) ||
                        (t1.tm_mday != t2.tm_mday));
#ifdef TRACKF_DBG
      if (new_track_flag != 0) {
        printf(MYNAME ": new day %02d.%02d.%04d\n", t2.tm_mday, t2.tm_mon+1, t2.tm_year+1900);
      }
#endif
    } else {
      new_track_flag = 1;

      if (distance > 0) {
        double rt1 = RAD(buff[i]->latitude);
        double rn1 = RAD(buff[i]->longitude);
        double rt2 = RAD(buff[j]->latitude);
        double rn2 = RAD(buff[j]->longitude);
        double curdist = gcdist(rt1, rn1, rt2, rn2);
        curdist = radtomiles(curdist);
        if (curdist <= distance) {
          new_track_flag = 0;
        }
#ifdef TRACKF_DBG
        else {
          printf(MYNAME ": sdistance, %g > %g\n", curdist, distance);
        }
#endif
      }

      if (interval > 0) {
        double tr_interval = difftime(buff[j]->GetCreationTime().toTime_t(), buff[i]->GetCreationTime().toTime_t());
        if (tr_interval <= interval) {
          new_track_flag = 0;
        }
#ifdef TRACKF_DBG
        else {
          printf(MYNAME ": split, %g > %g\n", tr_interval, interval);
        }
#endif
      }

    }
    if (new_track_flag != 0) {
#ifdef TRACKF_DBG
      printf(MYNAME ": splitting new track\n");
#endif
      curr = (route_head*) route_head_alloc();
      trackfilter_split_init_rte_name(curr, buff[j]->GetCreationTime());
      track_add_head(curr);
    }
    if (curr != nullptr) {
      wpt = buff[j];
      track_del_wpt(master, wpt);
      track_add_wpt(curr, wpt);
      buff[j] = wpt;
    }
  }
  xfree(buff);
}

/*******************************************************************************
* option "move"
*******************************************************************************/

void TrackFilter::trackfilter_move()
{
  int i;
  queue* elem, *tmp;
  Waypoint* wpt;
  time_t delta;

  delta = trackfilter_parse_time_opt(opt_move);
  if (delta == 0) {
    return;
  }

  for (i = 0; i < track_ct; i++) {
    route_head* track = track_list[i].track;
    QUEUE_FOR_EACH((queue*)&track->waypoint_list, elem, tmp) {
      wpt = (Waypoint*)elem;
      wpt->creation_time += delta;
    }

    track_list[i].first_time = track_list[i].first_time.addSecs(delta);
    track_list[i].last_time = track_list[i].last_time.addSecs(delta);
  }
}

/*******************************************************************************
* options "fix", "course", "speed"
*******************************************************************************/

void TrackFilter::trackfilter_synth()
{
  int i;
  queue* elem, *tmp;
  Waypoint* wpt;

  double last_course_lat;
  double last_course_lon;
  double last_speed_lat;
  double last_speed_lon;
  time_t last_speed_time;
  int first;
  fix_type fix;
  int nsats = 0;

  fix = trackfilter_parse_fix(&nsats);

  for (i = 0; i < track_ct; i++) {
    route_head* track = track_list[i].track;
    first = 1;
    QUEUE_FOR_EACH((queue*)&track->waypoint_list, elem, tmp) {
      wpt = (Waypoint*)elem;
      if (opt_fix) {
        wpt->fix = fix;
        if (wpt->sat == 0) {
          wpt->sat = nsats;
        }
      }
      if (first) {
        if (opt_course) {
          // TODO: the course value 0 isn't valid, wouldn't it be better to UNSET course?
          WAYPT_SET(wpt, course, 0);
        }
        if (opt_speed) {
          // TODO: the speed value 0 isn't valid, wouldn't it be better to UNSET speed?
          WAYPT_SET(wpt, speed, 0);
        }
        first = 0;
        last_course_lat = wpt->latitude;
        last_course_lon = wpt->longitude;
        last_speed_lat = wpt->latitude;
        last_speed_lon = wpt->longitude;
        last_speed_time = wpt->GetCreationTime().toTime_t();
      } else {
        if (opt_course) {
          WAYPT_SET(wpt, course, heading_true_degrees(RAD(last_course_lat),
                    RAD(last_course_lon),RAD(wpt->latitude),
                    RAD(wpt->longitude)));
          last_course_lat = wpt->latitude;
          last_course_lon = wpt->longitude;
        }
        if (opt_speed) {
          if (last_speed_time != wpt->GetCreationTime().toTime_t()) {
            // If we have mutliple points with the same time and
            // we use the pair of points about which the time ticks then we will
            // underestimate the distance and compute low speeds on average.
            // Therefore, if we have multiple points with the same time use the
            // first ones with the new times to compute speed.
            // Note that points with the same time can occur because the input
            // has truncated times, or because we are truncating times with
            // toTime_t().
            WAYPT_SET(wpt, speed, radtometers(gcdist(
                                                RAD(last_speed_lat), RAD(last_speed_lon),
                                                RAD(wpt->latitude),
                                                RAD(wpt->longitude))) /
                      labs(wpt->GetCreationTime().toTime_t()-last_speed_time));
            last_speed_lat = wpt->latitude;
            last_speed_lon = wpt->longitude;
            last_speed_time = wpt->GetCreationTime().toTime_t();
          } else {
            WAYPT_UNSET(wpt, speed);
          }
        }
      }
    }
  }
}


/*******************************************************************************
* option: "start" / "stop"
*******************************************************************************/

time_t TrackFilter::trackfilter_range_check(const char* timestr)
{
  int i;
  char fmt[20];
  char c;
  char* cin;
  struct tm time;


  i = 0;
  strncpy(fmt, "00000101000000", sizeof(fmt));
  cin = (char*)timestr;

  while ((c = *cin++)) {
    if (fmt[i] == '\0') {
      fatal(MYNAME "-range: parameter too long \"%s\"!\n", timestr);
    }
    if (isdigit(c) == 0) {
      fatal(MYNAME "-range: invalid character \"%c\"!\n", c);
    }
    fmt[i++] = c;
  }
  cin = strptime(fmt, "%Y%m%d%H%M%S", &time);
  if ((cin != nullptr) && (*cin != '\0')) {
    fatal(MYNAME "-range-check: Invalid time stamp (stopped at %s of %s)!\n", cin, fmt);
  }

  return mkgmtime(&time);
}

int TrackFilter::trackfilter_range()		/* returns number of track points left after filtering */
{
  time_t start, stop;
  queue* elem, *tmp;
  int i, dropped, inside = 0;

  if (opt_start != nullptr) {
    start = trackfilter_range_check(opt_start);
  } else {
    start = 0;
  }

  if (opt_stop != nullptr) {
    stop = trackfilter_range_check(opt_stop);
  } else {
    stop = 0x7FFFFFFF;
  }

  dropped = inside = 0;

  for (i = 0; i < track_ct; i++) {
    route_head* track = track_list[i].track;

    QUEUE_FOR_EACH((queue*)&track->waypoint_list, elem, tmp) {
      Waypoint* wpt = (Waypoint*)elem;
      if (wpt->creation_time.isValid()) {
        inside = ((wpt->GetCreationTime().toTime_t() >= start) && (wpt->GetCreationTime().toTime_t() <= stop));
      }
      // If the time is mangled so horribly that it's
      // negative, toss it.
      if (!wpt->creation_time.isValid()) {
        inside = 0;
      }

      if (! inside) {
        track_del_wpt(track, wpt);
        delete wpt;
        dropped++;
      }
    }

    if (track->rte_waypt_ct == 0) {
      track_del_head(track);
      track_list[i].track = nullptr;
    }
  }

  if ((track_pts > 0) && (dropped == track_pts)) {
    warning(MYNAME "-range: All %d track points have been dropped!\n", track_pts);
  }

  return track_pts - dropped;
}

/*******************************************************************************
* option "seg2trk"
*******************************************************************************/

void TrackFilter::trackfilter_seg2trk()
{
  int i;

  for (i = 0; i < track_ct; i++) {
    queue* elem, *tmp;
    route_head* src = track_list[i].track;
    route_head* dest = nullptr;
    route_head* insert_point = src;
    int trk_seg_num = 1, first = 1;

    QUEUE_FOR_EACH((queue*)&src->waypoint_list, elem, tmp) {
      Waypoint* wpt = (Waypoint*)elem;
      if (wpt->wpt_flags.new_trkseg && !first) {

        dest = route_head_alloc();
        dest->rte_num = src->rte_num;
        /* name in the form TRACKNAME #n */
        if (!src->rte_name.isEmpty()) {
          dest->rte_name = QString("%1 #%2").arg(src->rte_name).arg(++trk_seg_num);
        }

        /* Insert after original track or after last newly
         * created track */
        track_insert_head(dest, insert_point);
        insert_point = dest;
      }

      /* If we found a track separator, transfer from original to
       * new track. We have to reset new_trkseg temporarily to
       * prevent track_del_wpt() from copying it to the next track
       * point.
       */
      if (dest) {
        int orig_new_trkseg = wpt->wpt_flags.new_trkseg;
        wpt->wpt_flags.new_trkseg = 0;
        track_del_wpt(src, wpt);
        wpt->wpt_flags.new_trkseg = orig_new_trkseg;
        track_add_wpt(dest, wpt);
      }
      first = 0;
    }
  }
}

/*******************************************************************************
* option "trk2seg"
*******************************************************************************/

void TrackFilter::trackfilter_trk2seg()
{
  int i, first;
  route_head* master;

  master = track_list[0].track;

  for (i = 1; i < track_ct; i++) {
    queue* elem, *tmp;
    route_head* curr = track_list[i].track;

    first = 1;
    QUEUE_FOR_EACH((queue*)&curr->waypoint_list, elem, tmp) {
      Waypoint* wpt = (Waypoint*)elem;


      int orig_new_trkseg = wpt->wpt_flags.new_trkseg;
      wpt->wpt_flags.new_trkseg = 0;
      track_del_wpt(curr, wpt);
      wpt->wpt_flags.new_trkseg = orig_new_trkseg;
      track_add_wpt(master, wpt);
      if (first) {
        wpt->wpt_flags.new_trkseg = 1;
        first = 0;
      }
    }
    track_del_head(curr);
    track_list[i].track = nullptr;
  }
  track_ct = 1;
}

/*******************************************************************************
* option: "faketime"
*******************************************************************************/

TrackFilter::faketime_t TrackFilter::trackfilter_faketime_check(const char* timestr)
{
  int i, j;
  char fmtstart[20];
  char fmtstep[20];
  char c;
  const char* cin;
  struct tm time;
  int timeparse = 1;
  faketime_t result;
  result.force = 0;

  i = j = 0;
  strncpy(fmtstart, "00000101000000", sizeof(fmtstart));
  strncpy(fmtstep,  "00000000000000", sizeof(fmtstep));
  cin = timestr;

  while ((c = *cin++)) {
    if (c=='f') {
      result.force = 1;
      continue;
    }

    if (c!='+' && isdigit(c) == 0) {
      fatal(MYNAME "-faketime: invalid character \"%c\"!\n", c);
    }

    if (timeparse) {
      if (c == '+') {
        fmtstart[i++] = '\0';
        timeparse = 0;
      } else {
        if (fmtstart[i] == '\0') {
          fatal(MYNAME "-faketime: parameter too long \"%s\"!\n", timestr);
        }
        fmtstart[i++] = c;
      }
    } else {
      if (fmtstep[j] == '\0') {
        fatal(MYNAME "-faketime: parameter too long \"%s\"!\n", timestr);
      }
      fmtstep[j++] = c;
    }
  }
  fmtstep[j++] = '\0';

  cin = strptime(fmtstart, "%Y%m%d%H%M%S", &time);
  result.step = atoi(fmtstep);
  if ((cin != nullptr) && (*cin != '\0')) {
    fatal(MYNAME "-faketime-check: Invalid time stamp (stopped at %s of %s)!\n", cin, fmtstart);
  }

  result.start = mkgmtime(&time);
  return result;
}

int TrackFilter::trackfilter_faketime()             /* returns number of track points left after filtering */
{
  faketime_t faketime;

  queue* elem, *tmp;
  int i, dropped, inside = 0;

  if (opt_faketime != nullptr) {
    faketime = trackfilter_faketime_check(opt_faketime);
  }

  dropped = inside = 0;

  for (i = 0; i < track_ct; i++) {
    route_head* track = track_list[i].track;

    QUEUE_FOR_EACH((queue*)&track->waypoint_list, elem, tmp) {
      Waypoint* wpt = (Waypoint*)elem;

      if (opt_faketime != nullptr && (!wpt->creation_time.isValid() || faketime.force)) {
        wpt->creation_time = QDateTime::fromTime_t(faketime.start);
        faketime.start += faketime.step;
      }
    }
  }

  return track_pts - dropped;
}

int TrackFilter::trackfilter_points_are_same(const Waypoint* wpta, const Waypoint* wptb)
{
  // We use a simpler (non great circle) test for lat/lon here as this
  // is used for keeping the 'bookends' of non-moving points.
  //
  // Latitude spacing is about 27 feet per .00001 degree.
  // Longitude spacing varies, but the reality is that anything closer
  // than 27 feet does little but clutter the output.
  // As this is about the limit of consumer grade GPS, it seems a
  // reasonable tradeoff.

  return
    std::abs(wpta->latitude - wptb->latitude) < .00001 &&
    std::abs(wpta->longitude - wptb->longitude) < .00001 &&
    std::abs(wpta->altitude - wptb->altitude) < 20 &&
    (WAYPT_HAS(wpta,course) == WAYPT_HAS(wptb,course)) &&
    (wpta->course == wptb->course) &&
    (wpta->speed == wptb->speed) &&
    (wpta->heartrate == wptb->heartrate) &&
    (wpta->cadence == wptb->cadence) &&
    (wpta->temperature == wptb->temperature);
}

void TrackFilter::trackfilter_segment_head(const route_head* rte)
{
  queue* elem, *tmp;
  double avg_dist = 0;
  int index = 0;
  Waypoint* prev_wpt = nullptr;
  // Consider tossing trackpoints closer than this in radians.
  // (Empirically determined; It's a few dozen feet.)
  const double ktoo_close = 0.000005;

  QUEUE_FOR_EACH(&rte->waypoint_list, elem, tmp) {
    Waypoint* wpt = (Waypoint*)elem;
    if (index > 0) {
      double cur_dist = gcdist(RAD(prev_wpt->latitude),
                               RAD(prev_wpt->longitude),
                               RAD(wpt->latitude),
                               RAD(wpt->longitude));
      // Denoise points that are on top of each other.
      if (avg_dist == 0) {
        avg_dist = cur_dist;
      }

      if (cur_dist < ktoo_close) {
        if (wpt != (Waypoint*) QUEUE_LAST(&rte->waypoint_list)) {
          Waypoint* next_wpt = (Waypoint*) QUEUE_NEXT(&wpt->Q);
          if (trackfilter_points_are_same(prev_wpt, wpt) &&
              trackfilter_points_are_same(wpt, next_wpt)) {
            track_del_wpt((route_head*)rte, wpt);
            continue;
          }
        }
      }
      if (cur_dist > .001 && cur_dist > 1.2* avg_dist) {
        avg_dist = cur_dist = 0;
        wpt->wpt_flags.new_trkseg = 1;
      }
      // Update weighted moving average;
      avg_dist = (cur_dist + 4.0 * avg_dist) / 5.0;
    }
    prev_wpt = wpt;
    index++;
  }
}

/*******************************************************************************
* global cb's
*******************************************************************************/

void TrackFilter::init(const char*)
{
  setObj(*this);

  int count = track_count();

  /*
   * check time presence only if required. Options that NOT require time:
   *
   * - opt_title (!!! only if no format specifier is present !!!)
   * - opt_course
   * - opt_name
   */
  need_time = (
                opt_merge || opt_pack || opt_split || opt_sdistance ||
                opt_move || opt_fix || opt_speed ||
                (trackfilter_opt_count() == 0)	/* do pack by default */
              );
  /* in case of a formated title we also need valid timestamps */
  if ((opt_title != nullptr) && (strchr(opt_title, '%') != nullptr)) {
    need_time = 1;
  }

  track_ct = 0;
  track_pts = 0;

  // Perform segmenting first.
  if (opt_segment) {
    track_disp_all(&trackfilter_segment_head_glue, nullptr, nullptr);
  }

  if (count > 0) {
    track_list = new trkflt_t[count];

    /* check all tracks for time and order (except merging) */

    track_disp_all(&trackfilter_fill_track_list_cb_glue, nullptr, nullptr);
    if (need_time) {
      qsort(track_list, track_ct, sizeof(*track_list), trackfilter_init_qsort_cb);
    }
  }
}

void TrackFilter::deinit()
{
  delete[] track_list;
  track_ct = 0;
  track_pts = 0;
}

/*******************************************************************************
* trackfilter_process: called from gpsbabel central engine
*******************************************************************************/

void TrackFilter::process()
{
  setObj(*this);

  int opts, something_done;

  if (track_ct == 0) {
    return;  /* no track(s), no fun */
  }

  opts = trackfilter_opt_count();
  if (opts == 0) {
    opts = -1;  /* flag for do "pack" by default */
  }

  if (opt_name != nullptr) {
    if (--opts == 0) {
      return;
    }
  }

  if (opt_move != nullptr) {		/* Correct timestamps before any other op */
    trackfilter_move();
    if (--opts == 0) {
      return;
    }
  }

  if (opt_speed || opt_course || opt_fix) {
    trackfilter_synth();
    if (opt_speed) {
      opts--;
    }
    if (opt_course) {
      opts--;
    }
    if (opt_fix) {
      opts--;
    }
    if (!opts) {
      return;
    }
  }

  if ((opt_faketime != nullptr)) {
    opts--;

    trackfilter_faketime();

    if (opts == 0) {
      return;
    }

    deinit();       /* reinitialize */
    init(nullptr);

    if (track_ct == 0) {
      return;  /* no more track(s), no more fun */
    }
  }

  if ((opt_stop != nullptr) || (opt_start != nullptr)) {
    if (opt_start != nullptr) {
      opts--;
    }
    if (opt_stop != nullptr) {
      opts--;
    }

    trackfilter_range();

    if (opts == 0) {
      return;
    }

    deinit();	/* reinitialize */
    init(nullptr);

    if (track_ct == 0) {
      return;  /* no more track(s), no more fun */
    }

  }

  if (opt_seg2trk != nullptr) {
    trackfilter_seg2trk();
    if (--opts == 0) {
      return;
    }

    deinit();	/* reinitialize */
    init(nullptr);
  }

  if (opt_trk2seg != nullptr) {
    trackfilter_trk2seg();
    if (--opts == 0) {
      return;
    }
  }

  if (opt_title != nullptr) {
    if (--opts == 0) {
      trackfilter_title();
      return;
    }
  }

  something_done = 0;

  if ((opt_pack != nullptr) || (opts == -1)) {	/* call our default option */
    trackfilter_pack();
    something_done = 1;
  } else if (opt_merge != nullptr) {
    trackfilter_merge();
    something_done = 1;
  }

  if ((something_done == 1) && (--opts <= 0)) {
    if (opt_title != nullptr) {
      trackfilter_title();
    }
    return;
  }

  if ((opt_split != nullptr) || (opt_sdistance != nullptr)) {
    if (track_ct > 1) {
      fatal(MYNAME "-split: Cannot split more than one track, please pack (or merge) before!\n");
    }

    trackfilter_split();
  }

  // Performed last as previous options may have created "small" tracks.
  if ((opt_minpoints != nullptr) && atoi(opt_minpoints) > 0) {
    track_disp_all(&trackfilter_minpoint_list_cb_glue, nullptr, nullptr);
  }
}

#endif // FILTERS_ENABLED
