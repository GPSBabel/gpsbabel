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

#undef TRACKF_DBG

#include <cassert>                         // for assert
#include <cmath>                           // for nan
#include <cstdio>                          // for printf
#include <cstdlib>                         // for abs
#include <cstring>                         // for strlen, strchr, strcmp
#include <ctime>                           // for gmtime, strftime
#include <iterator>                        // for next

#include <algorithm>                       // for sort, stable_sort

#include <QtCore/QByteArray>               // for QByteArray
#include <QtCore/QChar>                    // for QChar
#include <QtCore/QDate>                    // for QDate
#include <QtCore/QDateTime>                // for QDateTime
#ifdef TRACKF_DBG
#include <QtCore/QDebug>
#endif
#include <QtCore/QList>                    // for QList<>::iterator, QList, QList<>::const_iterator
#include <QtCore/QRegExp>                  // for QRegExp, QRegExp::WildcardUnix
#include <QtCore/QRegularExpression>       // for QRegularExpression, QRegularExpression::CaseInsensitiveOption, QRegularExpression::PatternOptions
#include <QtCore/QRegularExpressionMatch>  // for QRegularExpressionMatch
#include <QtCore/QString>                  // for QString
#include <QtCore/Qt>                       // for UTC, CaseInsensitive
#include <QtCore/QtGlobal>                 // for qAsConst, foreach, qPrintable, QAddConst<>::Type, qint64

#include "defs.h"
#include "filterdefs.h"
#include "trackfilter.h"

#include "grtcirc.h"                       // for RAD, gcdist, radtometers, heading_true_degrees
#include "src/core/datetime.h"             // for DateTime


#if FILTERS_ENABLED || MINIMAL_FILTERS
#define MYNAME "trackfilter"

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

qint64 TrackFilter::trackfilter_parse_time_opt(const char* arg)
{
  qint64 result;

  QRegularExpression re("^([+-]?\\d+)([dhms])$", QRegularExpression::CaseInsensitiveOption);
  assert(re.isValid());
  QRegularExpressionMatch match = re.match(arg);
  if (match.hasMatch()) {
    bool ok;
    result = match.captured(1).toLong(&ok);
    if (!ok) {
      fatal(MYNAME "-time: invalid quantity in move option \"%s\"!\n", qPrintable(match.captured(1)));
    }

    switch (match.captured(2).at(0).toLower().toLatin1()) {
    case 'd':
      result *= SECONDS_PER_DAY;
      break;
    case 'h':
      result *= SECONDS_PER_HOUR;
      break;
    case 'm':
      result *= 60;
      break;
    case 's':
      break;
    default:
      fatal(MYNAME "-time: invalid unit in move option \"%s\"!\n", qPrintable(match.captured(2)));
    }

#ifdef TRACKF_DBG
    qDebug() << MYNAME "-time option: shift =" << result << "seconds";
#endif
  } else {
    fatal(MYNAME "-time: invalid value in move option \"%s\"!\n", arg);
  }

  return result;
}

bool TrackFilter::trackfilter_init_sort_cb(const route_head* ha, const route_head* hb)
{
  return trackfilter_get_first_time(ha) < trackfilter_get_first_time(hb);
}

bool TrackFilter::trackfilter_merge_sort_cb(const Waypoint* wa, const Waypoint* wb)
{
  return wa->GetCreationTime() < wb->GetCreationTime();
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

QDateTime TrackFilter::trackfilter_get_first_time(const route_head* track)
{
  if (track->waypoint_list.empty()) {
    return QDateTime();
  } else {
    return track->waypoint_list.front()->GetCreationTime();
  }
}

QDateTime TrackFilter::trackfilter_get_last_time(const route_head* track)
{
  if (track->waypoint_list.empty()) {
    return QDateTime();
  } else {
    return track->waypoint_list.back()->GetCreationTime();
  }
}


void TrackFilter::trackfilter_fill_track_list_cb(const route_head* track) 	/* callback for track_disp_all */
{
  if (track->rte_waypt_ct == 0) {
    track_del_head(const_cast<route_head*>(track));
    return;
  }

  if (opt_name != nullptr) {
    if (!QRegExp(opt_name, Qt::CaseInsensitive, QRegExp::WildcardUnix).exactMatch(track->rte_name)) {
      foreach (Waypoint* wpt, track->waypoint_list) {
        track_del_wpt(const_cast<route_head*>(track), wpt);
        delete wpt;
      }
      track_del_head(const_cast<route_head*>(track));
      return;
    }
  }

  const Waypoint* prev = nullptr;

  foreach (const Waypoint* wpt, track->waypoint_list) {
    if (!(opt_merge && opt_discard) && need_time && (!wpt->creation_time.isValid())) {
      fatal(MYNAME "-init: Found track point at %f,%f without time!\n",
            wpt->latitude, wpt->longitude);
    }

    if (need_time && (prev != nullptr) && (prev->GetCreationTime() > wpt->GetCreationTime())) {
      if (opt_merge == nullptr) {
        QString t1 = prev->CreationTimeXML();
        QString t2 = wpt->CreationTimeXML();
        fatal(MYNAME "-init: Track points badly ordered (timestamp %s > %s)!\n", qPrintable(t1), qPrintable(t2));
      }
    }
    prev = wpt;
  }

  track_list.append(const_cast<route_head*>(track));
}

void TrackFilter::trackfilter_minpoint_list_cb(const route_head* track)
{
  if (track->rte_waypt_ct < minimum_points) {
    track_del_head(const_cast<route_head*>(track));
    return;
  }
}

/*******************************************************************************
* track title producers
*******************************************************************************/

void TrackFilter::trackfilter_split_init_rte_name(route_head* track, const QDateTime& dt)
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

      time_t time = dt.toTime_t();
      struct tm tm = *gmtime(&time);
      char buff[128];
      strftime(buff, sizeof(buff), opt_title, &tm);
      track->rte_name = buff;
    } else {
      track->rte_name = QString("%1-%2").arg(opt_title, datetimestring);
    }
  } else if (!track->rte_name.isEmpty()) {
    track->rte_name = QString("%1-%2").arg(track->rte_name, datetimestring);
  } else {
    track->rte_name = datetimestring;
  }
}

void TrackFilter::trackfilter_pack_init_rte_name(route_head* track, const QDateTime& default_time)
{
  if (strchr(opt_title, '%') != nullptr) {
    // Uggh.  strftime format exposed to user.

    QDateTime dt;
    if (track->rte_waypt_ct == 0) {
      dt = default_time;
    } else {
      auto wpt = track->waypoint_list.front();
      dt = wpt->GetCreationTime();
    }
    time_t t = dt.toTime_t();
    struct tm tm = *gmtime(&t);
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
  if (opt_title == nullptr) {
    return;
  }

  if (strlen(opt_title) == 0) {
    fatal(MYNAME "-title: Missing your title!\n");
  }
  for (auto track : qAsConst(track_list)) {
    trackfilter_pack_init_rte_name(track, QDateTime::fromMSecsSinceEpoch(0, Qt::UTC));
  }
}

/*******************************************************************************
* option "pack" (default)
*******************************************************************************/

void TrackFilter::trackfilter_pack()
{
  if (!track_list.isEmpty()) {
    int i, j;

    for (i = 1, j = 0; i < track_list.size(); i++, j++) {
      auto prev_last_time = trackfilter_get_last_time(track_list.at(j));
      auto curr_first_time = trackfilter_get_first_time(track_list.at(i));
      if (prev_last_time >= curr_first_time) {
        fatal(MYNAME "-pack: Tracks overlap in time! %s >= %s at %d\n",
              qPrintable(prev_last_time.toString()),
              qPrintable(curr_first_time.toString()), i);
      }
    }

    /* we fill up the first track by all other track points */

    route_head* master = track_list.first();

    while (track_list.size() > 1) {
      route_head* curr = track_list.takeAt(1);

      foreach (Waypoint* wpt, curr->waypoint_list) {
        track_del_wpt(curr, wpt);
        track_add_wpt(master, wpt);
      }
      track_del_head(curr);
    }
  }
}

/*******************************************************************************
* option "merge"
*******************************************************************************/

void TrackFilter::trackfilter_merge()
{
  if (!track_list.isEmpty()) {
    route_head* master = track_list.first();

    int original_waypt_count = track_waypt_count();

    QList<Waypoint*> buff;

    auto it = track_list.begin();
    while (it != track_list.end()) { /* put all points into temp buffer */
      route_head* track = *it;
      foreach (Waypoint* wpt, track->waypoint_list) {
        track_del_wpt(track, wpt); /* copies any new_trkseg flag forward, and clears new_trkseg flag. */
        if (wpt->creation_time.isValid()) {
          // we will put the merged points in one track segment,
          // as it isn't clear how track segments in the original tracks
          // should relate to the merged track.
          // track_del_wpt cleared new_trkseg flag for wpt.
          // track_add_wpt will set new_trkseg for the first point
          // added to a track.
          buff.append(wpt);
        } else {
          delete wpt;
        }
      }
      if (it != track_list.begin()) {
        track_del_head(track);
        it = track_list.erase(it);
      } else {
        ++it;
      }
    }

    std::stable_sort(buff.begin(), buff.end(), trackfilter_merge_sort_cb);

    Waypoint* prev = nullptr;

    for (auto wpt : buff) {
      if ((prev == nullptr) || (prev->GetCreationTime() != wpt->GetCreationTime())) {
        track_add_wpt(master, wpt);
        prev = wpt;
      } else {
        delete wpt;
      }
    }

    if (master->rte_waypt_ct == 0) {
      track_del_head(master);
      track_list.clear();
    }

    if (global_opts.verbose_status > 0) {
      printf(MYNAME "-merge: %d track point(s) merged, %d dropped.\n", track_waypt_count(), original_waypt_count - track_waypt_count());
    }
    if ((original_waypt_count > 0) && (track_waypt_count() == 0)) {
      warning(MYNAME "-merge: All %d track points have been dropped!\n", original_waypt_count);
    }
  }
}

/*******************************************************************************
* option "split"
*******************************************************************************/

void TrackFilter::trackfilter_split()
{
  if (track_list.size() > 1) {
    fatal(MYNAME "-split: Cannot split more than one track, please pack (or merge) before!\n");
  } else if (!track_list.isEmpty()) {
    route_head* master = track_list.first();
    int count = master->rte_waypt_ct;

    int i, j;
    double interval = -1; /* seconds */
    double distance = -1; /* meters */

    if (count <= 1) {
      return;
    }

    /* check additional options */

    opt_interval = (opt_split && (strlen(opt_split) > 0) && (0 != strcmp(opt_split, TRACKFILTER_SPLIT_OPTION)));
    if (opt_interval != 0) {
      QRegularExpression re(R"(^([+-]?(?:\d+(?:\.\d*)?|\.\d+))([dhms])$)", QRegularExpression::CaseInsensitiveOption);
      assert(re.isValid());
      QRegularExpressionMatch match = re.match(opt_split);
      if (match.hasMatch()) {
        bool ok;
        interval = match.captured(1).toDouble(&ok);
        if (!ok || interval <= 0.0) {
          fatal(MYNAME ": invalid time interval specified \"%s\", must be a positive number.\n", qPrintable(match.captured(1)));
        }

        switch (match.captured(2).at(0).toLower().toLatin1()) {
        case 'd':
          interval *= SECONDS_PER_DAY;
          break;
        case 'h':
          interval *= SECONDS_PER_HOUR;
          break;
        case 'm':
          interval *= 60;
          break;
        case 's':
          break;
        default:
          fatal(MYNAME ": invalid time interval unit specified.\n");
        }

#ifdef TRACKF_DBG
        printf(MYNAME ": interval %f seconds\n", interval);
#endif
      } else {
        fatal(MYNAME ": invalid timer interval specified \"%s\", must be a positive number, followed by 'd' for days, 'h' for hours, 'm' for minutes or 's' for seconds.\n", opt_split);
      }
    }

    opt_distance = (opt_sdistance && (strlen(opt_sdistance) > 0) && (0 != strcmp(opt_sdistance, TRACKFILTER_SDIST_OPTION)));
    if (opt_distance != 0) {
      QRegularExpression re(R"(^([+-]?(?:\d+(?:\.\d*)?|\.\d+))([km])$)", QRegularExpression::CaseInsensitiveOption);
      assert(re.isValid());
      QRegularExpressionMatch match = re.match(opt_sdistance);
      if (match.hasMatch()) {
        bool ok;
        distance = match.captured(1).toDouble(&ok);
        if (!ok || distance <= 0.0) {
          fatal(MYNAME ": invalid time distance specified \"%s\", must be a positive number.\n", qPrintable(match.captured(1)));
        }

        switch (match.captured(2).at(0).toLower().toLatin1()) {
        case 'k': /* kilometers */
          distance *= 1000.0;
          break;
        case 'm': /* miles */
          distance *= 1609.344;
          break;
        default:
          fatal(MYNAME ": invalid distance unit specified.\n");
        }

#ifdef TRACKF_DBG
        printf(MYNAME ": distance %f meters\n", distance);
#endif
      } else {
        fatal(MYNAME ": invalid distance specified \"%s\", must be a positive number followed by 'k' for kilometers or 'm' for miles.\n", opt_sdistance);
      }
    }

    QList<Waypoint*> buff;

    foreach (Waypoint* wpt, master->waypoint_list) {
      buff.append(wpt);
    }

    trackfilter_split_init_rte_name(master, buff.at(0)->GetCreationTime());

    route_head* curr = nullptr;	/* will be set by first new track */

    for (i=0, j=1; j<count; i++, j++) {
      bool new_track_flag;

      if ((opt_interval == 0) && (opt_distance == 0)) {
// FIXME: This whole function needs to be reconsidered for arbitrary time.
        new_track_flag = buff.at(i)->GetCreationTime().toLocalTime().date() !=
                         buff.at(j)->GetCreationTime().toLocalTime().date();
#ifdef TRACKF_DBG
        if (new_track_flag) {
          printf(MYNAME ": new day %s\n", qPrintable(buff.at(j)->GetCreationTime().toLocalTime().date().toString(Qt::ISODate)));
        }
#endif
      } else {
        new_track_flag = true;

        if (distance > 0) {
          double rt1 = RAD(buff.at(i)->latitude);
          double rn1 = RAD(buff.at(i)->longitude);
          double rt2 = RAD(buff.at(j)->latitude);
          double rn2 = RAD(buff.at(j)->longitude);
          double curdist = gcdist(rt1, rn1, rt2, rn2);
          curdist = radtometers(curdist);
          if (curdist <= distance) {
            new_track_flag = false;
          }
#ifdef TRACKF_DBG
          else {
            printf(MYNAME ": sdistance, %g > %g\n", curdist, distance);
          }
#endif
        }

        if (interval > 0) {
          double tr_interval = 0.001 * buff.at(i)->GetCreationTime().msecsTo(buff.at(j)->GetCreationTime());
          if (tr_interval <= interval) {
            new_track_flag = false;
          }
#ifdef TRACKF_DBG
          else {
            printf(MYNAME ": split, %g > %g\n", tr_interval, interval);
          }
#endif
        }

      }
      if (new_track_flag) {
#ifdef TRACKF_DBG
        printf(MYNAME ": splitting new track\n");
#endif
        curr = route_head_alloc();
        trackfilter_split_init_rte_name(curr, buff.at(j)->GetCreationTime());
        track_add_head(curr);
        track_list.append(curr);
      }
      if (curr != nullptr) {
        track_del_wpt(master, buff.at(j));
        track_add_wpt(curr, buff.at(j));
      }
    }
  }
}

/*******************************************************************************
* option "move"
*******************************************************************************/

void TrackFilter::trackfilter_move()
{
  qint64 delta = trackfilter_parse_time_opt(opt_move);
  if (delta == 0) {
    return;
  }

  for (auto track : qAsConst(track_list)) {
    foreach (Waypoint* wpt, track->waypoint_list) {
      wpt->creation_time = wpt->creation_time.addSecs(delta);
    }
  }
}

/*******************************************************************************
* options "fix", "course", "speed"
*******************************************************************************/

void TrackFilter::trackfilter_synth()
{
  double last_course_lat;
  double last_course_lon;
  double last_speed_lat = std::nan(""); /* Quiet gcc 7.3.0 -Wmaybe-uninitialized */
  double last_speed_lon = std::nan(""); /* Quiet gcc 7.3.0 -Wmaybe-uninitialized */
  gpsbabel::DateTime last_speed_time;
  int nsats = 0;

  fix_type fix = trackfilter_parse_fix(&nsats);

  for (auto track : qAsConst(track_list)) {
    bool first = true;
    foreach (Waypoint* wpt, track->waypoint_list) {
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
        first = false;
        last_course_lat = wpt->latitude;
        last_course_lon = wpt->longitude;
        last_speed_lat = wpt->latitude;
        last_speed_lon = wpt->longitude;
        last_speed_time = wpt->GetCreationTime();
      } else {
        if (opt_course) {
          WAYPT_SET(wpt, course, heading_true_degrees(RAD(last_course_lat),
                    RAD(last_course_lon),RAD(wpt->latitude),
                    RAD(wpt->longitude)));
          last_course_lat = wpt->latitude;
          last_course_lon = wpt->longitude;
        }
        if (opt_speed) {
          if (last_speed_time.msecsTo(wpt->GetCreationTime()) != 0) {
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
                      (0.001 * std::abs(last_speed_time.msecsTo(wpt->GetCreationTime())))
                     );
            last_speed_lat = wpt->latitude;
            last_speed_lon = wpt->longitude;
            last_speed_time = wpt->GetCreationTime();
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

QDateTime TrackFilter::trackfilter_range_check(const char* timestr)
{
  QDateTime result;

  QRegularExpression re("^(\\d{0,14})$");
  assert(re.isValid());
  QRegularExpressionMatch match = re.match(timestr);
  if (match.hasMatch()) {
    QString start = match.captured(1);
    QString fmtstart("00000101000000");
    fmtstart.replace(0, start.size(), start);
    result = QDateTime::fromString(fmtstart, "yyyyMMddHHmmss");
    result.setTimeSpec(Qt::UTC);
    if (!result.isValid()) {
      fatal(MYNAME "-range-check: Invalid timestamp \"%s\"!\n", qPrintable(start));
    }

#ifdef TRACKF_DBG
    qDebug() << MYNAME "-range-check: " << result;
#endif
  } else {
    fatal(MYNAME "-range-check: Invalid value for option \"%s\"!\n", timestr);
  }

  return result;
}

void TrackFilter::trackfilter_range()
{
  QDateTime start, stop; // constructed such that isValid() is false, unlike gpsbabel::DateTime!

  if (opt_start != nullptr) {
    start = trackfilter_range_check(opt_start);
  }

  if (opt_stop != nullptr) {
    stop = trackfilter_range_check(opt_stop);
  }

  int original_waypt_count = track_waypt_count();

  auto it = track_list.begin();
  while (it != track_list.end()) {
    route_head* track = *it;

    foreach (Waypoint* wpt, track->waypoint_list) {
      bool inside;
      if (wpt->creation_time.isValid()) {
        bool after_start = !start.isValid() || (wpt->GetCreationTime() >= start);
        bool before_stop = !stop.isValid() || (wpt->GetCreationTime() <= stop);
        inside = after_start && before_stop;
      } else {
        // If the time is mangled so horribly that it's
        // negative, toss it.
        inside = false;
      }

      if (!inside) {
        track_del_wpt(track, wpt);
        delete wpt;
      }
    }

    if (track->rte_waypt_ct == 0) {
      track_del_head(track);
      it = track_list.erase(it);
    } else {
      ++it;
    }
  }

  if ((original_waypt_count > 0) && (track_waypt_count() == 0)) {
    warning(MYNAME "-range: All %d track points have been dropped!\n", original_waypt_count);
  }
}

/*******************************************************************************
* option "seg2trk"
*******************************************************************************/

void TrackFilter::trackfilter_seg2trk()
{
  if (!track_list.isEmpty()) {
    QList<route_head*> new_track_list;
    for (auto src : qAsConst(track_list)) {
      new_track_list.append(src);
      route_head* dest = nullptr;
      route_head* insert_point = src;
      int trk_seg_num = 1;
      bool first = true;

      foreach (Waypoint* wpt, src->waypoint_list) {
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
          new_track_list.append(dest);
        }

        /* If we found a track separator, transfer from original to
         * new track. We have to reset new_trkseg temporarily to
         * prevent track_del_wpt() from copying it to the next track
         * point.
         */
        if (dest) {
          unsigned orig_new_trkseg = wpt->wpt_flags.new_trkseg;
          wpt->wpt_flags.new_trkseg = 0;
          track_del_wpt(src, wpt);
          wpt->wpt_flags.new_trkseg = orig_new_trkseg;
          track_add_wpt(dest, wpt);
        }
        first = false;
      }
    }
    track_list = new_track_list;
  }
}

/*******************************************************************************
* option "trk2seg"
*******************************************************************************/

void TrackFilter::trackfilter_trk2seg()
{
  if (!track_list.isEmpty()) {
    route_head* master = track_list.first();

    while (track_list.size() > 1) {
      route_head* curr = track_list.takeAt(1);

      bool first = true;
      foreach (Waypoint* wpt, curr->waypoint_list) {

        unsigned orig_new_trkseg = wpt->wpt_flags.new_trkseg;
        wpt->wpt_flags.new_trkseg = 0;
        track_del_wpt(curr, wpt);
        wpt->wpt_flags.new_trkseg = orig_new_trkseg;
        track_add_wpt(master, wpt);
        if (first) {
          wpt->wpt_flags.new_trkseg = 1;
          first = false;
        }
      }
      track_del_head(curr);
    }
  }
}

/*******************************************************************************
* option: "faketime"
*******************************************************************************/

TrackFilter::faketime_t TrackFilter::trackfilter_faketime_check(const char* timestr)
{
  faketime_t result;

  QRegularExpression re(R"(^(f?)(\d{0,14})(?:\+(\d{1,10}))?$)");
  assert(re.isValid());
  QRegularExpressionMatch match = re.match(timestr);
  if (match.hasMatch()) {
    result.force = match.capturedLength(1) > 0;

    QString start = match.captured(2);
    QString fmtstart("00000101000000");
    fmtstart.replace(0, start.size(), start);
    result.start = QDateTime::fromString(fmtstart, "yyyyMMddHHmmss");
    result.start.setTimeSpec(Qt::UTC);
    if (!result.start.isValid()) {
      fatal(MYNAME "-faketime-check: Invalid timestamp \"%s\"!\n", qPrintable(start));
    }

    if (match.capturedLength(3) > 0) {
      bool ok;
      result.step = match.captured(3).toInt(&ok);
      if (!ok) {
        fatal(MYNAME "-faketime-check: Invalid step \"%s\"!\n", qPrintable(match.captured(3)));
      }
    } else {
      result.step = 0;
    }

#ifdef TRACKF_DBG
    qDebug() << MYNAME "-faketime option: force =" << result.force << ", timestamp =" << result.start << ", step =" << result.step;
#endif
  } else {
    fatal(MYNAME "-faketime-check: Invalid value for faketime option \"%s\"!\n", timestr);
  }

  return result;
}

void TrackFilter::trackfilter_faketime()
{
  assert(opt_faketime != nullptr);
  faketime_t faketime = trackfilter_faketime_check(opt_faketime);

  for (auto track : qAsConst(track_list)) {
    foreach (Waypoint* wpt, track->waypoint_list) {

      if (!wpt->creation_time.isValid() || faketime.force) {
        wpt->creation_time = faketime.start;
        faketime.start = faketime.start.addSecs(faketime.step);
      }
    }
  }
}

bool TrackFilter::trackfilter_points_are_same(const Waypoint* wpta, const Waypoint* wptb)
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
  double avg_dist = 0;
  int index = 0;
  Waypoint* prev_wpt = nullptr;
  // Consider tossing trackpoints closer than this in radians.
  // (Empirically determined; It's a few dozen feet.)
  const double ktoo_close = 0.000005;

  for (auto it = rte->waypoint_list.cbegin(); it != rte->waypoint_list.cend(); ++it) {
    auto wpt = *it;
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
        if (wpt != rte->waypoint_list.back()) {
          auto next_wpt = *std::next(it);
          if (trackfilter_points_are_same(prev_wpt, wpt) &&
              trackfilter_points_are_same(wpt, next_wpt)) {
            track_del_wpt(const_cast<route_head*>(rte), wpt);
            delete wpt;
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

void TrackFilter::init()
{
  RteHdFunctor<TrackFilter> trackfilter_segment_head_f(this, &TrackFilter::trackfilter_segment_head);
  RteHdFunctor<TrackFilter> trackfilter_fill_track_list_cb_f(this, &TrackFilter::trackfilter_fill_track_list_cb);

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
    need_time = true;
  }

  // Perform segmenting first.
  if (opt_segment) {
    track_disp_all(trackfilter_segment_head_f, nullptr, nullptr);
  }

  track_list.clear();
  if (track_count() > 0) {
    /* check all tracks for time and order (except merging) */

    track_disp_all(trackfilter_fill_track_list_cb_f, nullptr, nullptr);
    if (need_time) {
      std::sort(track_list.begin(), track_list.end(), trackfilter_init_sort_cb);
    }
  }
}

void TrackFilter::deinit()
{
  track_list.clear();
}

/*******************************************************************************
* trackfilter_process: called from gpsbabel central engine
*******************************************************************************/

void TrackFilter::process()
{
  RteHdFunctor<TrackFilter> trackfilter_minpoint_list_cb_f(this, &TrackFilter::trackfilter_minpoint_list_cb);

  if (track_list.isEmpty()) {
    return;  /* no track(s), no fun */
  }

  int opts = trackfilter_opt_count();
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
    // tracks and points within tracks may now be out of order.

    if (opts == 0) {
      return;
    }

    deinit();       /* reinitialize */
    init();

    if (track_list.isEmpty()) {
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
    // track_list may? now be invalid!

    if (opts == 0) {
      return;
    }

    // TODO: Is this needed if range maintains the track_list?
    deinit();	/* reinitialize */
    init();

    if (track_list.isEmpty()) {
      return;  /* no more track(s), no more fun */
    }
  }

  if (opt_seg2trk != nullptr) {
    trackfilter_seg2trk();
    // track_list may? now be invalid!
    if (--opts == 0) {
      return;
    }

    // TODO: Is this needed if seg2trk maintains the track_list?
    deinit();	/* reinitialize */
    init();
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

  bool something_done = false;

  if ((opt_pack != nullptr) || (opts == -1)) {	/* call our default option */
    trackfilter_pack();
    something_done = true;
  } else if (opt_merge != nullptr) {
    trackfilter_merge();
    something_done = true;
  }

  if (something_done && (--opts <= 0)) {
    if (opt_title != nullptr) {
      trackfilter_title();
    }
    return;
  }

  if ((opt_split != nullptr) || (opt_sdistance != nullptr)) {
    trackfilter_split();
    // track_list may? now be invalid!
  }

  // Performed last as previous options may have created "small" tracks.
  if (opt_minpoints != nullptr) {
    bool ok;
    minimum_points = QString(opt_minpoints).toInt(&ok);
    if (!ok || minimum_points <= 0) {
      fatal(MYNAME "-minimum_points: option value must be a positive integer!\n");
    }
    track_disp_all(trackfilter_minpoint_list_cb_f, nullptr, nullptr);
  }
}

#endif // FILTERS_ENABLED
