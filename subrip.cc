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
#include "src/core/datetime.h"  // for DateTime
#include "src/core/logging.h"   // for Fatal

#include <vector>

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

  *fout << QString::number(stnum++) << "\n";

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
  *fout << QStringLiteral("%1:%2:%3,%4 --> %5:%6:%7,%8\n")
            .arg(starttime.hour(), 2, 10, QChar('0')).arg(starttime.minute(), 2, 10, QChar('0')).arg(starttime.second(), 2, 10, QChar('0')).arg(starttime.msec(), 3, 10, QChar('0'))
            .arg(endtime.hour(), 2, 10, QChar('0')).arg(endtime.minute(), 2, 10, QChar('0')).arg(endtime.second(), 2, 10, QChar('0')).arg(endtime.msec(), 3, 10, QChar('0'));

  *fout << subtitle_content() << "\n\n";
}

QString
SubripFormat::subtitle_content() const
{
  // prepare fields and values
  const double nan = std::numeric_limits<double>::quiet_NaN();
  const QTime t = opt_localtime ? prevwpp->GetCreationTime().toLocalTime().time() : prevwpp->GetCreationTime().toUTC().time();
  const double speed_factor = opt_speedfactor.has_value() ? opt_speedfactor.get_result() : 1.0;
  const double altitude_factor = opt_altitudefactor.has_value() ? opt_altitudefactor.get_result() : 1.0;

  const std::vector<FormatString::NamedField> fields = {
    {"hour", t.hour()},
    {"minute", t.minute()},
    {"second", t.second()},
    {"longitude", prevwpp->longitude},
    {"latitude", prevwpp->latitude},
    {"altitude", prevwpp->altitude != unknown_alt ? prevwpp->altitude * altitude_factor : nan},
    {"speed", prevwpp->speed_has_value() ? speed_factor * prevwpp->speed_value() : nan},
    {"course", prevwpp->course_has_value() ? prevwpp->course_value() : nan},
    {"vspeed", vspeed.has_value() ? *vspeed * altitude_factor : nan},
    {"gradient", gradient.has_value() ? *gradient : nan},
    {"cadence", prevwpp->cadence != 0 ? prevwpp->cadence : nan},
    {"heartrate", prevwpp->heartrate != 0 ? prevwpp->heartrate : nan},
  };

  try
  {
    // must replace any literal "\n" (2 chars) in fmt string with actual newline
    auto fmt_qstring = opt_format.get(); // make it nonconst for replace()
    const auto fmt_string = fmt_qstring.replace("\\n", "\n").toStdString();
    const auto nan_inf_replacement = opt_nodata.get().toStdString();
    const auto subtitle = FormatString(fmt_string, nan_inf_replacement).format(fields);
    return QString::fromStdString(subtitle);
  }
  catch (const std::format_error &e)
  {
    gbFatal(FatalMsg().nospace() << e.what());
  }
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
                         << video_time(gps_datetime).toString(u"HH:mm:ss,zzz")
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
  vspeed.reset();
  gradient.reset();

  if (opt_gpstime != opt_gpsdate) {
    gbFatal(FatalMsg() << "Either both or neither of the gps_date and gps_time options must be supplied!");
  }
  gps_datetime = QDateTime();
  if (opt_gpstime && opt_gpsdate) {
    QDate gps_date = QDate::fromString(opt_gpsdate, u"yyyyMMdd");
    if (!gps_date.isValid()) {
      gbFatal(FatalMsg().nospace() << "option gps_date value (" << opt_gpsdate.get() << ") is invalid.  Expected yyyymmdd.");
    }
    QTime gps_time = QTime::fromString(opt_gpstime, u"HHmmss");
    if (!gps_time.isValid()) {
      gps_time = QTime::fromString(opt_gpstime, u"HHmmss.z");
      if (!gps_time.isValid()) {
        gbFatal(FatalMsg().nospace() << "option gps_time value (" << opt_gpstime.get() << ") is invalid.  Expected hhmmss[.sss]");
      }
    }
    gps_datetime = QDateTime(gps_date, gps_time, QtUTC);
  }

  video_offset_ms = 0;
  if (opt_videotime) {
    QTime video_time = QTime::fromString(opt_videotime, u"HHmmss");
    if (!video_time.isValid()) {
      video_time = QTime::fromString(opt_videotime, u"HHmmss.z");
      if (!video_time.isValid()) {
        gbFatal(FatalMsg().nospace() << "option video_time value (" << opt_videotime.get() << ") is invalid.  Expected hhmmss[.sss].");
      }
    }
    video_offset_ms = video_time.msecsSinceStartOfDay();
  }

  video_datetime = QDateTime();

  fout = new gpsbabel::TextStream;
  fout->open(fname, QIODevice::WriteOnly);
}

void
SubripFormat::wr_deinit()
{
  fout->close();
  delete fout;
  fout = nullptr;
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
