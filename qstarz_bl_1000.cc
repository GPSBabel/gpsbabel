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

#include <QtCore/QDataStream>
#include <QtCore/QFile>

#include "defs.h"
#include "gbfile.h"

#include "qstarz_bl_1000.h"

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

void
QstarzBL1000Format::qstarz_bl_1000_read(QDataStream& stream)
{
  route_head* track_route = route_head_alloc();

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
  qint8              	  batteryPercent;           // 55
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
    fatal(MYNAME ":  error on '%s'. Perhaps this isn't a Qstarz BL-1000 file\n", qPrintable(read_fname));
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

    fatal(MYNAME ": File format error on '%s'. Unexpected value for RCR (record reason): %c\n", qPrintable(read_fname), (char)rcr);

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
      fatal(MYNAME ": File format error on '%s'. Unexpected value for fix quality: %d\n", qPrintable(read_fname), fixQuality);
    }

    break;
  }

  auto* waypoint = new Waypoint();

  int tmp_lat = dLatitude / 100;
  int tmp_lon = dLongitude / 100;

  waypoint->latitude = tmp_lat + (dLatitude - tmp_lat * 100) / 60.0;
  waypoint->longitude = tmp_lon + (dLongitude - tmp_lon * 100) / 60.0;

  // printf(MYNAME ": waypoint->latitude: %f\n", waypoint->latitude);
  // printf(MYNAME ": waypoint->longitude: %f\n", waypoint->longitude);

  if ((waypoint->latitude < -90) || (waypoint->latitude > 90)) {
    fatal(MYNAME ": File format error on '%s'. Unexpected value for latitude: %f\n", qPrintable(read_fname), waypoint->latitude);
  }

  if ((waypoint->longitude < -180) || (waypoint->longitude > 180)) {
    fatal(MYNAME ": File format error on '%s'. Unexpected value for longitude: %f\n", qPrintable(read_fname), waypoint->longitude);
  }

  waypoint->altitude = altitude;

  waypoint->hdop = round(hdop * 1000) / 1000;
  waypoint->vdop = round(vdop * 1000) / 1000;
  waypoint->fix = fix;
  waypoint->sat = satelliteCountUsed;

  waypoint->speed = speed;
  waypoint->wpt_flags.speed = 1;

  waypoint->course = heading;
  waypoint->wpt_flags.course = 1;

  double timeIntervalSince1970 = time + milliseconds / 1000.0;
  QDateTime dateTime = QDateTime::fromSecsSinceEpoch(timeIntervalSince1970);

  waypoint->SetCreationTime(gpsbabel::DateTime(dateTime));

  qstarz_bl_1000_fsdata* fsdata = qstarz_bl_1000_alloc_fsdata();

  fs_chain_add(&(waypoint->fs), (format_specific_data*) fsdata);

  fsdata->accelerationOffset = 0;
  fsdata->accelerationX = gx / 256.0;
  fsdata->accelerationY = gy / 256.0;
  fsdata->accelerationZ = gz / 256.0;

  fsdata->satTotal = satelliteCountView;
  fsdata->maxSNR = maxSNR;
  fsdata->batteryPercent = batteryPercent;

  if (type == BL1000_POINT_TYPE_WAY_POINT) {
    if (global_opts.synthesize_shortnames) {
      waypoint->shortname = QString("WP%2").arg(waypt_count() + 1, 3, 10, QChar('0'));
      waypoint->wpt_flags.shortname_is_synthetic = 1;
    }
    waypt_add(waypoint);
  } else if ((type == BL1000_POINT_TYPE_TRACK_POINT_BY_TIME) || (type == BL1000_POINT_TYPE_TRACK_POINT_BY_DISTANCE) || (type == BL1000_POINT_TYPE_TRACK_POINT_BY_SPEED)) {
    track_add_wpt(track_route, waypoint, "TP", 3);
  } else {
    delete waypoint;
  }
}

/* fsdata manipulation functions */
void
qstarz_bl_1000_free_fsdata(void* fsdata)
{
  xfree(fsdata);
}

void
qstarz_bl_1000_copy_fsdata(qstarz_bl_1000_fsdata** dest, qstarz_bl_1000_fsdata* src)
{
  *dest = (qstarz_bl_1000_fsdata*)xmalloc(sizeof(*src));
  ** dest = *src;
  (*dest)->fs.next = nullptr;
}

qstarz_bl_1000_fsdata*
qstarz_bl_1000_alloc_fsdata()
{
  auto* fsdata = (qstarz_bl_1000_fsdata*) xcalloc(1, sizeof(qstarz_bl_1000_fsdata));
  fsdata->fs.copy = (fs_copy) qstarz_bl_1000_copy_fsdata;
  fsdata->fs.destroy = qstarz_bl_1000_free_fsdata;

  return fsdata;
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
    fatal(MYNAME ": Error opening file %s\n", qPrintable(read_fname));
  }

  QDataStream stream(&file);

  stream.setByteOrder(QDataStream::LittleEndian);

  qstarz_bl_1000_read(stream);

  file.close();
}
