/*

    Support for GlobalSat DG-388 .gpl binary track log files.

    Copyright (C) 2026 Jack Irby, jbirby@gmail.com
    Copyright (C) 2026 Robert Lipe, robertlipe+source@gpsbabel.org

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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301,
    USA.

 */

#include "dg-388.h"

#include <QChar>               // for QChar
#include <QDate>               // for QDate
#include <QDataStream>         // for QDataStream
#include <QDateTime>           // for QDateTime
#include <QIODevice>           // for QIODevice
#include <QLatin1Char>         // for QLatin1Char
#include <QString>             // for QString
#include <QStringLiteral>      // for QStringLiteral
#include <QTime>               // for QTime

#include "defs.h"              // for Waypoint, route_head, waypt_add,
                               //     track_add_head, track_add_wpt,
                               //     KPH_TO_MPS, QtUTC
#include "src/core/file.h"     // for File
#include "src/core/logging.h"  // for Warning


void
Dg388Format::rd_init(const QString& fname)
{
  fin = new gpsbabel::File(fname);
  fin->open(QIODevice::ReadOnly);
}

void
Dg388Format::rd_deinit()
{
  fin->close();
  delete fin;
  fin = nullptr;
}

/*
 * Validate a 28-byte record buffer.
 *
 * Rejects uninitialized flash (all zeros or garbage) by checking that
 * the date and time fields form a valid QDateTime and that coordinates
 * fall within valid bounds.
 */
bool
Dg388Format::is_valid_record(const Dg388Format::record& buf)
{
  /* Reject null records */
  if (buf.date_int == 0 || (buf.lat_raw == 0 && buf.lon_raw == 0)) {
    return false;
  }

  /* Validate date and time via QDateTime */
  int year   = buf.date_int / 10000;
  int month  = (buf.date_int / 100) % 100;
  int day    = buf.date_int % 100;
  int hour   = buf.time_int / 10000;
  int minute = (buf.time_int / 100) % 100;
  int second = buf.time_int % 100;

  QDate date(year, month, day);
  QTime time(hour, minute, second);
  QDateTime dt(date, time, QtUTC);
  if (!dt.isValid()) {
    return false;
  }

  /* Coordinates: 1e-7 degrees, +/-90 lat, +/-180 lon */
  if (buf.lat_raw < -900000000 || buf.lat_raw > 900000000) {
    return false;
  }
  if (buf.lon_raw < -1800000000 || buf.lon_raw > 1800000000) {
    return false;
  }

  return true;
}

/*
 * Decode a single 28-byte record into a GPSBabel Waypoint.
 * Caller must have validated the record with is_valid_record() first.
 */
Waypoint*
Dg388Format::record_to_waypoint(const Dg388Format::record& buf)
{
  /* Decode date and time */
  int year   = buf.date_int / 10000;
  int month  = (buf.date_int / 100) % 100;
  int day    = buf.date_int % 100;
  int hour   = buf.time_int / 10000;
  int minute = (buf.time_int / 100) % 100;
  int second = buf.time_int % 100;

  QDate date(year, month, day);
  QTime time(hour, minute, second);
  QDateTime dt(date, time, QtUTC);

  auto* wpt = new Waypoint;

  wpt->latitude  = buf.lat_raw / 1.0e7;
  wpt->longitude = buf.lon_raw / 1.0e7;
  wpt->altitude  = buf.alt_raw / 10.0;          /* decimeters -> meters */

  wpt->SetCreationTime(dt);

  wpt->set_speed(KPH_TO_MPS(buf.speed_raw / 100.0));   /* cm-kph -> m/s */
  wpt->set_course(buf.hdg_raw / 100.0);                 /* centi-degrees -> degrees */

  return wpt;
}

/*
 * Main read entry point.
 *
 * Reads the entire .gpl file as a sequence of 28-byte records.
 * Records with flag=2 (automatic log) become track points in a single
 * track segment.  Records with flag=3 (manual waypoint button) become
 * standalone waypoints.
 */
void
Dg388Format::read()
{
  route_head* track = nullptr;
  record buf;
  int waypoint_count = 0;

  QDataStream stream(fin);
  stream.setByteOrder(QDataStream::LittleEndian);

  while (!stream.atEnd()) {
    stream >> buf.date_int;
    stream >> buf.time_int;
    stream >> buf.lat_raw;
    stream >> buf.lon_raw;
    stream >> buf.alt_raw;
    stream >> buf.speed_raw;
    stream >> buf.hdg_raw;
    stream >> buf.flag;

    // Make sure we didn't read passed the end.
    if (auto sts = stream.status(); sts != QDataStream::Ok) {
      if (sts == QDataStream::ReadPastEnd) {
        Warning() << "Warning: Partial record at end of input ignored!";
      } else {
        Warning() << "Read Error!";
      }
      break;
    }

    if (!is_valid_record(buf)) {
      continue;
    }

    Waypoint* wpt = record_to_waypoint(buf);

    if (buf.flag == FLAG_WAYPOINT) {
      /* Manual waypoint: give it a sequential name */
      waypoint_count++;
      wpt->shortname = QStringLiteral("WPT-%1")
                          .arg(waypoint_count, 3, 10, QLatin1Char('0'));
      waypt_add(wpt);
    } else {
      /* Track point (flag == FLAG_AUTO or any other value) */
      if (track == nullptr) {
        track = new route_head;
        track->rte_name = QStringLiteral("DG-388 Track");
        track_add_head(track);
      }
      track_add_wpt(track, wpt);
    }
  }
}
