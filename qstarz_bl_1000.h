/*
    Handle Qstarz BL-1000 .BIN files.

    Saved in the SESSION/GPSLog folder. There will 3 files sharing the same base name: .BIN, .POI, and .DAT.
    Only the .BIN file is of interest to us.

    Copyright (C) 2020 Pierre Bernard, pierre.bernard@houdah.com
    Copyright (C) 2001-2020 Robert Lipe, robertlipe+source@gpsbabel.org

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

#ifndef QSTARZ_BL1000_H_INCLUDED_
#define QSTARZ_BL1000_H_INCLUDED_

#include <QtCore/QString>
#include <QtCore/QVector>

#include "defs.h"
#include "format.h"

struct qstarz_bl_1000_fsdata : FormatSpecificData {
  qstarz_bl_1000_fsdata() : FormatSpecificData(kFsQstarzBl1000) {}

  qstarz_bl_1000_fsdata* clone() const override
  {
    return new qstarz_bl_1000_fsdata(*this);
  }

  char rcr; // record reason. possible values are listed in switch-case in .cc file
  float accelerationX; // horizonal acceleration value measured in acceleration due to gravity or g
  float accelerationY; // vertical acceleration value measured in acceleration due to gravity or g
  float accelerationZ; // depth acceleration value measured in acceleration due to gravity or g
  quint16 maxSNR;
  qint8 satTotal; // satellite count (view)
  quint8 batteryPercent;
};

class QstarzBL1000Format : public Format
{
public:
  ff_type get_type() const override
  {
    return ff_type_file;
  }

  QVector<ff_cap> get_cap() const override
  {
    return {
      ff_cap_read,  // waypoints
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
  void qstarz_bl_1000_read(QDataStream& stream);
  void qstarz_bl_1000_read_record(QDataStream& stream, route_head* track_route);

private:
  QString read_fname;
};


#endif
