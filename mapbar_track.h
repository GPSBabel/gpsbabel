/*
    China mapbar navigation track reader for sonim xp3300
       it's maybe can used in other demo devices of mapbar navigation

    Copyright (C) 2013 xiao jian cheng, azuresky.xjc@gmail.com
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

 */
#ifndef MAPBAR_TRACK_H_INCLUDED_
#define MAPBAR_TRACK_H_INCLUDED_

#include <QString>              // for QString
#include <QVector>              // for QVector

#include "defs.h"               // for ff_cap, arglist_t, ff_cap_none, CET_CHARSET_UTF8, Waypoint, ff_cap_read, ff_type, ff_type_file
#include "format.h"             // for Format
#include "gbfile.h"             // for gbfile
#include "src/core/datetime.h"  // for DateTime


class MapbarTrackFormat : public Format
{
public:
  QVector<arglist_t>* get_args() override
  {
    return &mapbar_track_args;
  }

  ff_type get_type() const override
  {
    return ff_type_file;
  }

  QVector<ff_cap> get_cap() const override
  {
    /*         waypoints,                tracks,      routes */
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

  static constexpr double DIV_RATE  = 100000.0f;

  /* Member Functions */

  gpsbabel::DateTime read_datetime();
  Waypoint* read_waypoint();

  /* Data Members */

  gbfile* fin{};

  QVector<arglist_t> mapbar_track_args = {
  };
};
#endif // MAPBAR_TRACK_H_INCLUDED_
