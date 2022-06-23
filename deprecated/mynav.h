/*
    Handle MyNav TRC format .trc and .ftn files

    Copyright (c) 2014-2020 Ralf Horstmann <ralf@ackstorm.de>
    Copyright (C) 2014-2020 Robert Lipe, robertlipe+source@gpsbabel.org

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

#ifndef MYNAV_H_INCLUDED_
#define MYNAV_H_INCLUDED_

#include <QString>
#include <QVector>

#include "defs.h"
#include "format.h"

class MyNavFormat : public Format
{
public:
  ff_type get_type() const override
  {
    return ff_type_file;
  }

  QVector<ff_cap> get_cap() const override
  {
    return {
      ff_cap_none,  // waypoints
      ff_cap_read,  // tracks
      ff_cap_none   // routes
    };
  }

  QString get_encode() const override
  {
    return CET_CHARSET_ASCII;
  }

  int get_fixed_encode() const override
  {
    return 0;
  }

  void rd_init(const QString& fname) override;
  void read() override;
  void rd_deinit() override;

private:
  enum field_e {
    fld_type = 0,
    fld_lon,
    fld_lat,
    fld_direction,
    fld_speed,
    fld_altitude,
    fld_timestamp,
    fld_duration,
    fld_gps_valid,
    fld_distance,
    fld_ascent,
    fld_cadence,
    fld_heart_rate,
    fld_id,
    fld_total_duration,
    fld_terminator
  };

  enum line_e {
    line_header = 0,
    line_sensors = 1,
    line_geonote = 2,
    line_gps = 5,
    line_lap_pause = 7,
    line_lap_restart = 8,
    line_total = 9,
    line_lap_start = 10,
    line_lap_end = 11,
  };

  static void read_line(const QString& line, route_head* track);

  QString read_fname;

};

#endif
