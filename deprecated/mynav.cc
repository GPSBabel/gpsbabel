/*
    Handle MyNav TRC format .trc and .ftn files

    For information on the data format see
    http://www.mynav.it/hwdoc/dev/TRC_Format_Spec.pdf

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

#include <QChar>
#include <QDebug>
#include <QIODevice>
#include <QString>
#include <QStringList>
#include <QtGlobal>

#include "src/core/textstream.h"

#include "mynav.h"

/***************************************************************************
 *              local helper functions                                     *
 ***************************************************************************/

void
MyNavFormat::read_line(const QString& line, route_head* track)
{
  const QStringList fields = line.split('|');

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
  int line_type = fields.at(fld_type).trimmed().toInt(&ok);
  if (!ok) {
    return;
  }
  if (line_type != line_sensors && line_type != line_gps) {
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

  auto* wpt = new Waypoint;
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

  track_add_wpt(track, wpt);
}

/***************************************************************************
 *              entry points called by gpsbabel main process               *
 ***************************************************************************/

void
MyNavFormat::rd_init(const QString& fname)
{
  read_fname = fname;
}

void
MyNavFormat::rd_deinit()
{
  read_fname.clear();
}

void
MyNavFormat::read()
{
  gpsbabel::TextStream stream;
  stream.open(read_fname, QIODevice::ReadOnly, "mynav");

  auto* track = new route_head;
  track_add_head(track);

  QString buf;
  while (stream.readLineInto(&buf)) {
    buf = buf.trimmed();
    if ((buf.isEmpty()) || buf.startsWith('#')) {
      continue;
    }
    read_line(buf, track);
  }

  stream.close();
}
