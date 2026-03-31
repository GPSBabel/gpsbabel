/*

    Support for GlobalSat DG-388 .gpl binary track log files.

    The DG-388 is a USB mass-storage GPS data logger built around the
    Airoha AG3335M multi-GNSS chipset.  It stores track logs as fixed
    28-byte binary records in .gpl files on its internal flash, with no
    file header.  The format was reverse-engineered from device output
    and validated against 7,130 timestamp-matched records.

    Record layout (28 bytes, all little-endian):
      Offset  0-3:  int32   Date        YYYYMMDD integer
      Offset  4-7:  int32   Time (UTC)  HHMMSS integer
      Offset  8-11: int32   Latitude    1e-7 degrees (signed)
      Offset 12-15: int32   Longitude   1e-7 degrees (signed)
      Offset 16-19: int32   Altitude    decimeters above MSL
      Offset 20-23: int32   Speed       km/h * 100
      Offset 24-25: uint16  Heading     degrees * 100
      Offset 26-27: uint16  Record flag 2=auto log, 3=manual waypoint

    Copyright (C) 2026 Jack Irby, jbirby@gmail.com
    Copyright (C) 2001-2026 Robert Lipe, robertlipe+source@gpsbabel.org

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
#ifndef DG388_H_INCLUDED_
#define DG388_H_INCLUDED_

#include <QDateTime>     // for QDateTime
#include <QString>       // for QString
#include <QTimeZone>     // for QTimeZone
#include <QVector>       // for QVector

#include "defs.h"        // for arglist_t, ff_cap, ff_type, route_head, Waypoint
#include "format.h"      // for Format
#include "gbfile.h"      // for gbfile

class Dg388Format : public Format
{
public:
  QVector<arglist_t>* get_args() override
  {
    return &dg388_args;
  }

  ff_type get_type() const override
  {
    return ff_type_file;
  }

  QVector<ff_cap> get_cap() const override
  {
    return {
      ff_cap_read           /* waypoints */,
      ff_cap_read           /* tracks */,
      ff_cap_none           /* routes */
    };
  }

  void rd_init(const QString& fname) override;
  void read() override;
  void rd_deinit() override;

private:
  /* Binary record constants */
  static constexpr int RECORD_SIZE = 28;
  static constexpr uint16_t FLAG_AUTO = 2;
  static constexpr uint16_t FLAG_WAYPOINT = 3;

  /* Helpers */
  bool is_valid_record(const uint8_t* buf) const;
  Waypoint* record_to_waypoint(const uint8_t* buf) const;

  /* State */
  gbfile* fin{nullptr};

  /* No user-configurable options for this format */
  QVector<arglist_t> dg388_args = {};
};

#endif // DG388_H_INCLUDED_
