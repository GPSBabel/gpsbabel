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
#ifndef DG388_H_INCLUDED_
#define DG388_H_INCLUDED_

#include <QString>          // for QString
#include <QVector>          // for QVector
#include <QtGlobal>         // for qint32, quint16

#include "defs.h"           // for ff_cap, ff_type, Waypoint
#include "format.h"         // for Format
#include "src/core/file.h"  // for File

class Dg388Format : public Format
{
public:
  using Format::Format;

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
  /* Types */

  struct record {
    qint32 date_int{};
    qint32 time_int{};
    qint32 lat_raw{};
    qint32 lon_raw{};
    qint32 alt_raw{};
    qint32 speed_raw{};
    quint16 hdg_raw{};
    quint16 flag{};
  };

  /* Binary record constants */
  static constexpr quint16 FLAG_AUTO = 2;
  static constexpr quint16 FLAG_WAYPOINT = 3;

  /* Helpers */
  static bool is_valid_record(const record& buf);
  static Waypoint* record_to_waypoint(const record& buf);

  /* State */
  gpsbabel::File* fin{nullptr};

};

#endif // DG388_H_INCLUDED_
