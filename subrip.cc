/*
    Write points to SubRip subtitle file (for video geotagging)

    Copyright (C) 2010 Michael von Glasow, michael @t vonglasow d.t com
    Copyright (C) 2014 Gleb Smirnoff, glebius @t FreeBSD d.t org

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
#include <QDate>                // for QDate
#include <QDateTime>            // for QDateTime, operator<<
#include <QDebug>               // for QDebug
#include <QString>              // for QString
#include <QTime>                // for QTime
#include <Qt>                   // for UTC

#include "defs.h"
#include "subrip.h"
#include "gbfile.h"             // for gbfprintf, gbfclose, gbfopen, gbfwrite, gbfile
#include "src/core/datetime.h"  // for DateTime
#include "src/core/logging.h"   // for Fatal


#define MYNAME "subrip"

/* internal helper functions */

QTime
SubripFormat::video_time(const QDateTime& dt) const
{
  return QTime::fromMSecsSinceStartOfDay(video_datetime.msecsTo(dt));
}

void
SubripFormat::subrip_prevwp_pr(const Waypoint* waypointp)
{
  static long long deltaoffset;

  /* Now that we have the next waypoint, we can write out the subtitle for
   * the previous one.
   */

  /* If this condition is not true, the waypoint is before the beginning of
   * the video and will be ignored
   */
  if (prevwpp->GetCreationTime() < video_datetime) {
    return;
  }

  gbfprintf(fout, "%d\n", stnum++);

  /* Writes start and end time for subtitle display to file. */
  QDateTime end_datetime;
  if (!waypointp) {
    // prevwpp is the last waypoint, so we don't have a datetime for the
    // next waypoint.  Instead, estimate it from length of the previous
    // video frame.
    end_datetime = prevwpp->GetCreationTime().addMSecs(deltaoffset);
  } else {
    end_datetime = waypointp->GetCreationTime();
    deltaoffset = prevwpp->GetCreationTime().msecsTo(waypointp->GetCreationTime());
  }
  QTime starttime = video_time(prevwpp->GetCreationTime());
  QTime endtime = video_time(end_datetime);
  gbfprintf(fout, "%02d:%02d:%02d,%03d --> %02d:%02d:%02d,%03d\n",
            starttime.hour(), starttime.minute(), starttime.second(), starttime.msec(),
            endtime.hour(), endtime.minute(), endtime.second(), endtime.msec());

  for (char* c = opt_format; *c != '\0' ; c++) {
    char fmt;

    switch (*c) {
    case '%':
      fmt = *++c;
      if (fmt == '\0') {
        fatal("No character after %% in subrip format");
      }

      switch (fmt) {
      case 's':
        if WAYPT_HAS(prevwpp, speed) {
          gbfprintf(fout, "%2.1f", MPS_TO_KPH(prevwpp->speed));
        } else {
          gbfprintf(fout, "--.-");
        }
        break;
      case 'e':
        if (prevwpp->altitude != unknown_alt) {
          gbfprintf(fout, "%4.0f", prevwpp->altitude);
        } else {
          gbfprintf(fout, "   -");
        }
        break;
      case 'v':
        gbfprintf(fout, "%2.2f", vspeed);
        break;
      case 'g':
        gbfprintf(fout, "%2.1f%%", gradient);
        break;
      case 't': {
        QTime t = prevwpp->GetCreationTime().toUTC().time();
        gbfprintf(fout, "%02d:%02d:%02d", t.hour(), t.minute(), t.second());
        break;
      }
      case 'l':
        gbfprintf(fout, "Lat=%0.5lf Lon=%0.5lf",
                  prevwpp->latitude, prevwpp->longitude);
        break;
      case 'c':
        if (prevwpp->cadence != 0) {
          gbfprintf(fout, "%3u", prevwpp->cadence);
        } else {
          gbfprintf(fout, "  -");
        }
        break;
      case 'h':
        if (prevwpp->heartrate != 0) {
          gbfprintf(fout, "%3u", prevwpp->heartrate);
        } else {
          gbfprintf(fout, "  -");
        }
        break;
      }

      break;

    case '\\':
      fmt = *++c;
      if (fmt == '\0') {
        fatal("No character after \\ in subrip format");
      }
      switch (fmt) {
      case 'n':
        gbfprintf(fout, "\n");
        break;
      }
      break;
    default:
      gbfwrite(c, 1, 1, fout);
    }
  }
  gbfprintf(fout, "\n\n");
}

/* callback functions */

void
SubripFormat::subrip_trkpt_pr(const Waypoint* waypointp)
{
  /*
   * To determine the duration of the subtitle, we need the timestamp of the
   * associated waypoint plus that of the following one.
   * Since we get waypoints one at a time, the only way is to store one and
   * defer processing until we get the next one.
   *
   * To determine vertical speed we need to have not only previous waypoint,
   * but also pre-previous, so we calculate vspeed right before forgetting
   * the previous.
   */
  if (!video_datetime.isValid()) {
    if (!gps_datetime.isValid()) {
      // If gps_date and gps_time options weren't used, then we use the
      // datetime of the first waypoint to sync to the video.
      gps_datetime = waypointp->GetCreationTime().toUTC();
    }
    video_datetime = gps_datetime.addMSecs(-video_offset_ms).toUTC();
    if (global_opts.debug_level >= 2) {
      qDebug().noquote() << "GPS track start is           "
                         << waypointp->GetCreationTime().toUTC().toString(Qt::ISODateWithMs);
      qDebug().noquote() << "Synchronizing"
                         << video_time(gps_datetime).toString("HH:mm:ss,zzz")
                         << "to" << gps_datetime.toString(Qt::ISODateWithMs);
      qDebug().noquote() << "Video start   00:00:00,000 is"
                         << video_datetime.toString(Qt::ISODateWithMs);
    }
  }

  if (prevwpp) {
    subrip_prevwp_pr(waypointp);
    vspeed = waypt_vertical_speed(waypointp, prevwpp);
    gradient = waypt_gradient(waypointp, prevwpp);
  }
  prevwpp = waypointp;
}

/* global callback (exported) functions */

void
SubripFormat::wr_init(const QString& fname)
{
  stnum = 1;
  prevwpp = nullptr;
  vspeed = 0;
  gradient = 0;

  if ((opt_gpstime == nullptr) != (opt_gpsdate == nullptr)) {
    fatal(FatalMsg() << MYNAME ": Either both or neither of the gps_date and gps_time options must be supplied!");
  }
  gps_datetime = QDateTime();
  if ((opt_gpstime != nullptr) && (opt_gpsdate != nullptr)) {
    QDate gps_date = QDate::fromString(opt_gpsdate, "yyyyMMdd");
    if (!gps_date.isValid()) {
      fatal(FatalMsg().nospace() << MYNAME ": option gps_date value (" << opt_gpsdate << ") is invalid.  Expected yyyymmdd.");
    }
    QTime gps_time = QTime::fromString(opt_gpstime, "HHmmss");
    if (!gps_time.isValid()) {
      gps_time = QTime::fromString(opt_gpstime, "HHmmss.z");
      if (!gps_time.isValid()) {
        fatal(FatalMsg().nospace() << MYNAME ": option gps_time value (" << opt_gpstime << ") is invalid.  Expected hhmmss[.sss]");
      }
    }
    gps_datetime = QDateTime(gps_date, gps_time, Qt::UTC);
  }

  video_offset_ms = 0;
  if (opt_videotime != nullptr) {
    QTime video_time = QTime::fromString(opt_videotime, "HHmmss");
    if (!video_time.isValid()) {
      video_time = QTime::fromString(opt_videotime, "HHmmss.z");
      if (!video_time.isValid()) {
        fatal(FatalMsg().nospace() << MYNAME ": option video_time value (" << opt_videotime << ") is invalid.  Expected hhmmss[.sss].");
      }
    }
    video_offset_ms = video_time.msecsSinceStartOfDay();
  }

  video_datetime = QDateTime();

  fout = gbfopen(fname, "wb", MYNAME);
}

void
SubripFormat::wr_deinit()
{
  gbfclose(fout);
}

void
SubripFormat::write()
{
  auto subrip_trkpt_pr_lambda = [this](const Waypoint* waypointp)->void {
    subrip_trkpt_pr(waypointp);
  };
  track_disp_all(nullptr, nullptr, subrip_trkpt_pr_lambda);

  /*
   * Due to the necessary hack, one waypoint is still in memory (unless we
   * did not get any waypoints). Check if there is one and, if so, write it.
   */
  if (prevwpp) {
    subrip_prevwp_pr(nullptr);
  }
}
