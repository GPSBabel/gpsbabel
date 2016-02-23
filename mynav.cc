/*
    Handle MyNav TRC format .trc and .ftn files

    Copyright (c) 2014 Ralf Horstmann <ralf@ackstorm.de>
    Copyright (C) 2014 Robert Lipe, robertlipe+source@gpsbabel.org

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

#include "defs.h"
#include <QtCore/QDebug>
#include <QtCore/QStringList>

#define MYNAME "mynav"

typedef enum {
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
} field_e;

static route_head* mynav_track;
static gbfile* fin;

//***************************************************************************
//           local helper functions
//***************************************************************************
static void
mynav_rd_line(QString line)
{
  QStringList fields = line.split("|");

  if (global_opts.debug_level > 1) {
    qDebug() << "line: " << line;
    for (int i = 0; i < fields.size(); i++) {
      qDebug() << "field" << i << fields.at(i);
    }
  }

  // don't consider lines without latitude/longitude
  if (fields.size() <= fld_lat) {
    return;
  }

  // only type 1 and type 5 lines contain coordinates
  bool ok = false;
  int val_type = fields.at(fld_type).trimmed().toInt(&ok);
  if (!ok) {
    return;
  }
  if (val_type != 1 && val_type != 5) {
    return;
  }

  // This field is not present in .trc files, only in .ftn, so
  // ignore line if present and != 1
  if (fields.size() > fld_gps_valid) {
    int val_gps_valid = fields.at(fld_gps_valid).trimmed().toInt(&ok);
    if (!ok || val_gps_valid != 1) {
      return;
    }
  }

  double val_lon = fields.at(fld_lon).trimmed().toDouble(&ok) / 3600000.0;
  if (!ok) {
    return;
  }
  double val_lat = fields.at(fld_lat).trimmed().toDouble(&ok) / 3600000.0;
  if (!ok) {
    return;
  }

  Waypoint* wpt = new Waypoint;
  wpt->latitude = val_lat;
  wpt->longitude = val_lon;

  if (fields.size() > fld_altitude) {
    double val_alt = fields.at(fld_altitude).trimmed().toDouble(&ok);
    if (ok) {
      wpt->altitude = val_alt;
    }
  }

  if (fields.size() > fld_timestamp) {
    int val_time = fields.at(fld_timestamp).trimmed().toInt(&ok);
    if (ok) {
      wpt->SetCreationTime(val_time);
    }
  }

  track_add_wpt(mynav_track, wpt);
}


//***************************************************************************
//           global callbacks called by gpsbabel main process
//***************************************************************************

static void
mynav_rd_init(const QString& fname)
{
  fin = gbfopen(fname, "rb", MYNAME);
  mynav_track = route_head_alloc();
  track_add_head(mynav_track);
}

static void
mynav_rd_deinit(void)
{
  gbfclose(fin);
}

static void
mynav_rd(void)
{
  QString buff;

  while ((buff = gbfgetstr(fin)), !buff.isNull()) {
    buff = buff.trimmed();
    if ((buff.isEmpty()) || (buff[0] == '#')) {
      continue;
    }
    mynav_rd_line(buff);
  }
}

ff_vecs_t mynav_vecs = {
  ff_type_file,
  {
    ff_cap_none,  // waypoints
    ff_cap_read,  // tracks
    ff_cap_none   // routes
  },
  mynav_rd_init,    // rd_init
  NULL,           // wr_init
  mynav_rd_deinit,  // rd_deinit
  NULL,           // wr_deinit
  mynav_rd,         // read
  NULL,           // write
  NULL,           // exit
  NULL,           //args
  CET_CHARSET_ASCII, 0  //encode,fixed_encode
  //NULL                //name dynamic/internal?
};
