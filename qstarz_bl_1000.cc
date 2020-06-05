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

#include "qstarz_bl_1000.h"

#include <cmath>               // for round
#include <QtCore/QChar>        // for QChar
#include <QtCore/QDataStream>  // for QDataStream, QDataStream::SinglePrecision, QDataStream::DoublePrecision, QDataStream::LittleEndian, QDataStream::Ok
#include <QtCore/QDebug>       // for QDebug
#include <QtCore/QFile>        // for QFile
#include <QtCore/QIODevice>    // for QIODevice, QIODevice::ReadOnly
#include "defs.h"              // for Waypoint, ddmm2degrees, route_head, track_add_head, track_add_wpt, waypt_add, waypt_count, wp_flags, fix_unknown, fix_2d, fix_3d, fix_dgps, fix_none, fix_pps, fix_type, global_options, global_opts
#include "src/core/logging.h"  // for Fatal


#define MYNAME "Qstarz BL-1000"


enum BL1000_POINT_TYPE {
  BL1000_POINT_TYPE_UNKNOWN = '-',
  BL1000_POINT_TYPE_WAY_POINT = 'B', // Button push
  BL1000_POINT_TYPE_TRACK_POINT_BY_TIME = 'T',
  BL1000_POINT_TYPE_TRACK_POINT_BY_DISTANCE = 'D',
  BL1000_POINT_TYPE_TRACK_POINT_BY_SPEED = 'S',
};


/***************************************************************************
 *           local helper functions                                        *
 ***************************************************************************/

inline bool qstarz_bl_1000_is_waypoint_type(BL1000_POINT_TYPE type)
{
  return (type == BL1000_POINT_TYPE_WAY_POINT);
}

inline bool qstarz_bl_1000_is_trackpoint_type(BL1000_POINT_TYPE type)
{
  return (
           (type == BL1000_POINT_TYPE_TRACK_POINT_BY_TIME) ||
           (type == BL1000_POINT_TYPE_TRACK_POINT_BY_DISTANCE) ||
           (type == BL1000_POINT_TYPE_TRACK_POINT_BY_SPEED)
         );
}

void
QstarzBL1000Format::qstarz_bl_1000_read(QDataStream& stream)
{
  auto* track_route = new route_head;

  track_add_head(track_route);

  while (!stream.atEnd()) {
    qstarz_bl_1000_read_record(stream, track_route);
  }
}

void
QstarzBL1000Format::qstarz_bl_1000_read_record(QDataStream& stream, route_head* track_route)
{
  // Value fields in 64-byte log entry
  quint8                fixStatus;                // 0
  qint8                 rcr;                      // 1
  quint16               milliseconds;             // 2-3
  double                dLatitude;                // 4-11
  double                dLongitude;               // 12-19
  quint32               time;                     // 20-23
  float                 speed;                    // 24-27
  float                 altitude;                 // 28-31
  float                 heading;                  // 32-35
  qint16                gx;                       // 36-37
  qint16                gy;                       // 38-39
  qint16                gz;                       // 40-41
  quint16               maxSNR;                   // 42-43
  float                 hdop;                     // 44-47
  float                 vdop;                     // 48-51
  qint8                 satelliteCountView;       // 52
  qint8                 satelliteCountUsed;       // 53
  quint8                fixQuality;               // 54
  quint8              	batteryPercent;           // 55
  quint32               unused1;                  // 56-59
  quint32               unused2;                  // 60-64

  stream >> fixStatus;
  stream >> rcr;
  stream >> milliseconds;

  stream.setFloatingPointPrecision(QDataStream::DoublePrecision);
  stream >> dLatitude;
  stream >> dLongitude;

  stream >> time;

  stream.setFloatingPointPrecision(QDataStream::SinglePrecision);
  stream >> speed;
  stream >> altitude;
  stream >> heading;

  stream >> gx;
  stream >> gy;
  stream >> gz;
  stream >> maxSNR;

  stream.setFloatingPointPrecision(QDataStream::SinglePrecision);
  stream >> hdop;
  stream >> vdop;

  stream >> satelliteCountView;
  stream >> satelliteCountUsed;
  stream >> fixQuality;
  stream >> batteryPercent;
  stream >> unused1;
  stream >> unused2;

  if (stream.status() != QDataStream::Ok) {
    fatal(FatalMsg() << MYNAME << ": File format error on " << read_fname << ". Perhaps this isn't a Qstarz BL-1000 file");
  }

  BL1000_POINT_TYPE type;

  switch (rcr) {
  case 'B':
    type = BL1000_POINT_TYPE_WAY_POINT;
    break;
  case 'T':
    type = BL1000_POINT_TYPE_TRACK_POINT_BY_TIME;
    break;
  case 'D':
    type = BL1000_POINT_TYPE_TRACK_POINT_BY_DISTANCE;
    break;
  case 'S':
    type = BL1000_POINT_TYPE_TRACK_POINT_BY_SPEED;
    break;
  case 'W': // Power On
  case 'X': // Power Off
  case 'Y': // USB plug in
  case 'Z': // USB removed
  case 'C': // Cali G-sensor
  case 'A': // Enter Smart Mode(Power Saving)
  case 'R': // Reserved
  case 'N': // Reserved
  case 'M': // Reserved
  case 'G': // Reserved
  case 'H': // Reserved
  case 'I': // Reserved
  case 'J': // Reserved
    type = BL1000_POINT_TYPE_UNKNOWN;
    break;
  default:
    type = BL1000_POINT_TYPE_UNKNOWN;

    fatal(FatalMsg() << MYNAME << ": File format error on " << read_fname << ". Unexpected value for RCR (record reason): " << rcr);

    break;
  }

  fix_type fix;

  switch (fixQuality) {
  case 1:
    switch (fixStatus) {
    case 1:
      fix = fix_none;
      break;
    case 2:
      fix = fix_2d;
      break;
    case 3:
      fix = fix_3d;
      break;
    default:
      fix = fix_unknown;
      break;
    }
    break;
  case 2:
    fix = fix_dgps;
    break;
  case 3:
    fix = fix_pps;
    break;
  default:
    fix = fix_unknown;

    if (type != BL1000_POINT_TYPE_UNKNOWN) {
      fatal(FatalMsg() << MYNAME << ": File format error on " << read_fname << ". Unexpected value for fix quality: " << fixQuality);
    }

    break;
  }

  auto* waypoint = new Waypoint();

  waypoint->latitude = ddmm2degrees(dLatitude);
  waypoint->longitude = ddmm2degrees(dLongitude);

  // qDebug(waypoint)

  if ((waypoint->latitude < -90) || (waypoint->latitude > 90)) {
    fatal(FatalMsg() << MYNAME << ": File format error on " << read_fname << ". Unexpected value for latitude: " << waypoint->latitude);
  }

  if ((waypoint->longitude < -180) || (waypoint->longitude > 180)) {
    fatal(FatalMsg() << MYNAME << ": File format error on " << read_fname << ". Unexpected value for longitude: " << waypoint->longitude);
  }

  waypoint->altitude = altitude;

  waypoint->hdop = round(hdop * 1000) / 1000;
  waypoint->vdop = round(vdop * 1000) / 1000;
  waypoint->fix = fix;
  waypoint->sat = satelliteCountUsed;

  waypoint->speed = KPH_TO_MPS(speed);
  waypoint->wpt_flags.speed = 1;

  waypoint->course = heading;
  waypoint->wpt_flags.course = 1;
  waypoint->SetCreationTime(time, milliseconds);

  auto* fsdata = new qstarz_bl_1000_fsdata;

  waypoint->fs.FsChainAdd(fsdata);

  fsdata->accelerationX = gx / 256.0;
  fsdata->accelerationY = gy / 256.0;
  fsdata->accelerationZ = gz / 256.0;

  fsdata->satTotal = satelliteCountView;
  fsdata->maxSNR = maxSNR;
  fsdata->batteryPercent = batteryPercent;

  if (qstarz_bl_1000_is_waypoint_type(type)) {
    if (global_opts.synthesize_shortnames) {
      waypoint->shortname = QString("WP%2").arg(waypt_count() + 1, 3, 10, QChar('0'));
      waypoint->wpt_flags.shortname_is_synthetic = 1;
    }
    waypt_add(waypoint);
  } else if (qstarz_bl_1000_is_trackpoint_type(type)) {
    track_add_wpt(track_route, waypoint, "TP", 3);
  } else {
    delete waypoint;
  }
}


/***************************************************************************
 *              entry points called by gpsbabel main process               *
 ***************************************************************************/

void
QstarzBL1000Format::rd_init(const QString& fname)
{
  read_fname = fname;
}

void
QstarzBL1000Format::rd_deinit()
{
  read_fname.clear();
}

void
QstarzBL1000Format::read()
{
  QFile file(read_fname);
  if (!file.open(QIODevice::ReadOnly)) {
    fatal(FatalMsg() << MYNAME << ": Error opening file " << read_fname);
  }

  QDataStream stream(&file);

  stream.setByteOrder(QDataStream::LittleEndian);

  qstarz_bl_1000_read(stream);

  file.close();
}
