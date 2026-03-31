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

#include <cstdint>       // for uint8_t, uint16_t, int32_t

#include <QChar>         // for QChar
#include <QDate>         // for QDate
#include <QDateTime>     // for QDateTime
#include <QLatin1Char>   // for QLatin1Char
#include <QString>       // for QString
#include <QStringLiteral> // for QStringLiteral
#include <QTime>         // for QTime

#include "defs.h"        // for Waypoint, route_head, waypt_add,
                         //     track_add_head, track_add_wpt,
                         //     le_read32, le_readu16, KPH_TO_MPS,
                         //     QtUTC


void
Dg388Format::rd_init(const QString& fname)
{
  fin = gbfopen(fname, "rb");
}

void
Dg388Format::rd_deinit()
{
  gbfclose(fin);
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
Dg388Format::is_valid_record(const uint8_t* buf)
{
  int32_t date_int = le_read32(buf + 0);
  int32_t time_int = le_read32(buf + 4);
  int32_t lat_raw  = le_read32(buf + 8);
  int32_t lon_raw  = le_read32(buf + 12);

  /* Reject null records */
  if (date_int == 0 || (lat_raw == 0 && lon_raw == 0)) {
    return false;
  }

  /* Validate date and time via QDateTime */
  int year   = date_int / 10000;
  int month  = (date_int / 100) % 100;
  int day    = date_int % 100;
  int hour   = time_int / 10000;
  int minute = (time_int / 100) % 100;
  int second = time_int % 100;

  QDate date(year, month, day);
  QTime time(hour, minute, second);
  QDateTime dt(date, time, QtUTC);
  if (!dt.isValid()) {
    return false;
  }

  /* Coordinates: 1e-7 degrees, +/-90 lat, +/-180 lon */
  if (lat_raw < -900000000 || lat_raw > 900000000) {
    return false;
  }
  if (lon_raw < -1800000000 || lon_raw > 1800000000) {
    return false;
  }

  return true;
}

/*
 * Decode a single 28-byte record into a GPSBabel Waypoint.
 * Caller must have validated the record with is_valid_record() first.
 */
Waypoint*
Dg388Format::record_to_waypoint(const uint8_t* buf)
{
  int32_t date_int    = le_read32(buf + 0);
  int32_t time_int    = le_read32(buf + 4);
  int32_t lat_raw     = le_read32(buf + 8);
  int32_t lon_raw     = le_read32(buf + 12);
  int32_t alt_raw     = le_read32(buf + 16);
  int32_t speed_raw   = le_read32(buf + 20);
  uint16_t hdg_raw    = le_readu16(buf + 24);

  /* Decode date and time */
  int year   = date_int / 10000;
  int month  = (date_int / 100) % 100;
  int day    = date_int % 100;
  int hour   = time_int / 10000;
  int minute = (time_int / 100) % 100;
  int second = time_int % 100;

  QDate date(year, month, day);
  QTime time(hour, minute, second);
  QDateTime dt(date, time, QtUTC);

  auto* wpt = new Waypoint;

  wpt->latitude  = lat_raw / 1.0e7;
  wpt->longitude = lon_raw / 1.0e7;
  wpt->altitude  = alt_raw / 10.0;          /* decimeters -> meters */

  wpt->SetCreationTime(dt);

  wpt->set_speed(KPH_TO_MPS(speed_raw / 100.0));   /* cm-kph -> m/s */
  wpt->set_course(hdg_raw / 100.0);                 /* centi-degrees -> degrees */

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
  uint8_t buf[RECORD_SIZE];
  int waypoint_count = 0;

  while (gbfread(buf, 1, RECORD_SIZE, fin) == RECORD_SIZE) {

    if (!is_valid_record(buf)) {
      continue;
    }

    uint16_t flag = le_readu16(buf + 26);

    Waypoint* wpt = record_to_waypoint(buf);

    if (flag == FLAG_WAYPOINT) {
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
