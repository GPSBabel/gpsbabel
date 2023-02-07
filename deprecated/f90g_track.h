/*
  Map file reader for F90G Automobile DVR.

    Copyright (C) 2014 Jim Keeler, James.L.Keeler@gmail.com
    Copyright (C) 2001-2013 Robert Lipe, robertlipe+source@gpsbabel.org

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

    Read the map file contents picking out the defined record types.

    The map file contains a constant 30 byte header record followed by a variable number of
    TT records.  The TT records start with the two characters "TT" and are 251 bytes long.
    The TT records contain values for time, position and velocity.

 */
#ifndef F90G_TRACK_H_INCLUDED_
#define F90G_TRACK_H_INCLUDED_

#include <QString>   // for QString
#include <QVector>   // for QVector

#include "defs.h"    // for ff_cap, arglist_t, ff_cap_none, CET_CHARSET_UTF8, ff_cap_read, ff_type, ff_type_file, route_head
#include "format.h"  // for Format
#include "gbfile.h"  // for gbfile


class F90gTrackFormat : public Format
{
public:
  QVector<arglist_t>* get_args() override
  {
    return &f90g_track_args;
  }

  ff_type get_type() const override
  {
    return ff_type_file;
  }

  QVector<ff_cap> get_cap() const override
  {
    //         waypoints,                tracks,      routes
    return { ff_cap_none, (ff_cap)(ff_cap_read), ff_cap_none };
  }

  QString get_encode() const override
  {
    return CET_CHARSET_UTF8;
  }

  int get_fixed_encode() const override
  {
    return 0;
  }

  void rd_init(const QString& fname) override;
  void read() override;
  void rd_deinit() override;

private:
  /* Constants */

  // needed conversion factors
  static constexpr double MIN_PER_DEGREE  = 600000.0;
  static constexpr float  SPEED_CONVERSION = (10.0f)/(36.0f); // convert KPH to meters per second

  /* Data Members */

  gbfile* fin = nullptr;
  route_head* track = nullptr;

  QVector<arglist_t> f90g_track_args = {
  };

};
#endif // F90G_TRACK_H_INCLUDED_
