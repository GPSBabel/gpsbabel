/*

    Support for FIT track files.

    Copyright (C) 2011 Paul Brook, paul@nowt.org
    Copyright (C) 2003-2011  Robert Lipe, robertlipe+source@gpsbabel.org
    Copyright (C) 2019 Martin Buck, mb-tmp-tvguho.pbz@gromit.dyndns.org

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

#include <cstdint>             // for uint8_t, uint16_t, uint32_t, int32_t, int8_t, uint64_t
#include <cstdio>              // for EOF, SEEK_SET, snprintf
#include <deque>               // for deque, _Deque_iterator, operator!=
#include <memory>              // for allocator_traits<>::value_type
#include <string>              // for operator+, to_string, char_traits
#include <utility>             // for pair
#include <vector>              // for vector

#include <QByteArray>          // for QByteArray
#include <QDateTime>           // for QDateTime
#include <QFileInfo>           // for QFileInfo
#include <QLatin1Char>         // for QLatin1Char
#include <QString>             // for QString
#include <Qt>                  // for CaseInsensitive
#include <QtGlobal>            // for uint, qint64

#include "defs.h"
#include "garmin_fit.h"
#include "gbfile.h"            // for gbfputc, gbfputuint16, gbfputuint32, gbfgetc, gbfread, gbfseek, gbfclose, gbfgetuint16, gbfopen_le, gbfputint32, gbfflush, gbfgetuint32, gbfputs, gbftell, gbfwrite, gbfile, gbsize_t
#include "jeeps/gpsmath.h"     // for GPS_Math_Semi_To_Deg, GPS_Math_Gtime_To_Utime, GPS_Math_Deg_To_Semi, GPS_Math_Utime_To_Gtime
#include "src/core/logging.h"  // for Warning, Fatal


#define MYNAME "fit"

/*******************************************************************************
* %%%        global callbacks called by gpsbabel main process              %%% *
*******************************************************************************/

void
GarminFitFormat::rd_init(const QString& fname)
{
  fin = gbfopen_le(fname, "rb", MYNAME);
}

void
GarminFitFormat::rd_deinit()
{
  fit_data = fit_data_t();

  gbfclose(fin);
}

void
GarminFitFormat::wr_init(const QString& fname)
{
  fout = gbfopen_le(fname, "w+b", MYNAME);
}

void
GarminFitFormat::wr_deinit()
{
  gbfclose(fout);
}

/*******************************************************************************
* fit_parse_header- parse the global FIT header
*******************************************************************************/
void
GarminFitFormat::fit_parse_header()
{
  char sig[4];

  int len = gbfgetc(fin);
  if (len == EOF || len < 12) {
    fatal(MYNAME ": Bad header\n");
  }
  if (global_opts.debug_level >= 1) {
    Debug(1) << MYNAME ": header len=" << len;
  }

  int ver = gbfgetc(fin);
  if (ver == EOF || (ver >> 4) > 2)
    fatal(MYNAME ": Unsupported protocol version %d.%d\n",
          ver >> 4, ver & 0xf);
  if (global_opts.debug_level >= 1) {
    Debug(1) << MYNAME ": protocol version=" << ver;
  }

  // profile version
  ver = gbfgetuint16(fin);
  // data length
  fit_data.len = gbfgetuint32(fin);
  // File signature
  if (gbfread(sig, 4, 1, fin) != 1) {
    fatal(MYNAME ": Unexpected end of file\n");
  }
  if (sig[0] != '.' || sig[1] != 'F' || sig[2] != 'I' || sig[3] != 'T') {
    fatal(MYNAME ": .FIT signature missing\n");
  }

  if (global_opts.debug_level >= 1) {
    Debug(1) << MYNAME ": profile version=" << ver;
    Debug(1) << MYNAME ": fit_data.len=" << fit_data.len;
  }

  // Header CRC may be omitted entirely
  if (len >= kReadHeaderCrcLen) {
    uint16_t hdr_crc = gbfgetuint16(fin);
    // Header CRC may be set to 0, or contain the CRC over previous bytes.
    if (hdr_crc != 0) {
      // Check the header CRC
      uint16_t crc = 0;
      gbfseek(fin, 0, SEEK_SET);
      for (unsigned int i = 0; i < kReadHeaderCrcLen; ++i) {
        int data = gbfgetc(fin);
        if (data == EOF) {
          fatal(MYNAME ": File %s truncated\n", qPrintable(fin->name));
        }
        crc = fit_crc16(data, crc);
      }
      if (crc != 0) {
        Warning().nospace() << MYNAME ": Header CRC mismatch in file " <<  fin->name << ".";
        if (!opt_recoverymode) {
          fatal(FatalMsg().nospace() << MYNAME ": File " << fin->name << " is corrupt.  Use recoverymode option at your risk.");
        }
      } else if (global_opts.debug_level >= 1) {
        Debug(1) << MYNAME ": Header CRC verified.";
      }
    }
  }

  QFileInfo fi(fin->name);
  qint64 size = fi.size();
  if ((len + fit_data.len + 2) != size) {
    Warning().nospace() << MYNAME ": File size " << size << " is not expected given header len " << len << ", data length " << fit_data.len << " and a 2 byte file CRC.";
  } else if (global_opts.debug_level >= 1) {
    Debug(1) << MYNAME ": File size matches expectations from information in the header.";
  }

  gbfseek(fin, len, SEEK_SET);

  fit_data.global_utc_offset = 0;
}

uint8_t
GarminFitFormat::fit_getuint8()
{
  if (fit_data.len < 1) {
    throw ReaderException("record truncated: expecting char[1], but only got " + std::to_string(fit_data.len) + ".");
  }
  int val = gbfgetc(fin);
  if (val == EOF) {
    throw ReaderException("unexpected end of file with fit_data.len=" + std::to_string(fit_data.len) + ".");
  }
  --fit_data.len;
  return static_cast<uint8_t>(val);
}

uint16_t
GarminFitFormat::fit_getuint16()
{
  char buf[2];

  if (fit_data.len < 2) {
    throw ReaderException("record truncated: expecting char[2], but only got " + std::to_string(fit_data.len) + ".");
  }
  gbsize_t count = gbfread(buf, 2, 1, fin);
  if (count != 1) {
    throw ReaderException("unexpected end of file with fit_data.len=" + std::to_string(fit_data.len) + ".");
  }
  fit_data.len -= 2;
  if (fit_data.endian) {
    return be_read16(buf);
  } else {
    return le_read16(buf);
  }
}

uint32_t
GarminFitFormat::fit_getuint32()
{
  char buf[4];

  if (fit_data.len < 4) {
    throw ReaderException("record truncated: expecting char[4], but only got " + std::to_string(fit_data.len) + ".");
  }
  gbsize_t count = gbfread(buf, 4, 1, fin);
  if (count != 1) {
    throw ReaderException("unexpected end of file with fit_data.len=" + std::to_string(fit_data.len) + ".");
  }
  fit_data.len -= 4;
  if (fit_data.endian) {
    return be_read32(buf);
  } else {
    return le_read32(buf);
  }
}

QString
GarminFitFormat::fit_getstring(int size)
{
  if (fit_data.len < size) {
    throw ReaderException("record truncated: expecting " + std::to_string(size) + " bytes, but only got " + std::to_string(fit_data.len) + ".");
  }
  QByteArray buf(size + 1, 0); // almost certainly an extra byte, QByteArray should guarnatee a terminator.
  gbsize_t count = gbfread(buf.data(), size, 1, fin);
  if (count != 1) {
    throw ReaderException("unexpected end of file with fit_data.len=" + std::to_string(fit_data.len) + ".");
  }
  fit_data.len -= size;
  return QString(buf.constData());
}

void
GarminFitFormat::fit_parse_definition_message(uint8_t header)
{
  int local_id = header & 0x0f;
  fit_message_def def;

  // first byte is reserved.  It's usually 0 and we don't know what it is,
  // but we've seen some files that are 0x40.  So we just read it and toss it.
  (void) fit_getuint8();

  // second byte is endianness
  def.endian = fit_getuint8();
  if (def.endian > 1) {
    throw ReaderException(QStringLiteral("Bad endian field 0x%1 at file position 0x%2.").arg(def.endian, 0, 16).arg(gbftell(fin) - 1, 0, 16).toStdString());
  }
  fit_data.endian = def.endian;

  // next two bytes are the global message number
  def.global_id = fit_getuint16();

  // byte 5 has the number of records in the remainder of the definition message
  int num_fields = fit_getuint8();
  if (global_opts.debug_level >= 8) {
    Debug(8) << MYNAME ": definition message contains " << num_fields << " records";
  }

  // remainder of the definition message is data at one byte per field * 3 fields
  for (int i = 0; i < num_fields; ++i) {
    int id   = fit_getuint8();
    int size = fit_getuint8();
    int type = fit_getuint8();
    fit_field_t field = {id, size, type};
    if (global_opts.debug_level >= 8) {
      Debug(8) << MYNAME ": record " << i << "  ID: " << field.id << "  SIZE: "
               << field.size << "  TYPE: " << field.type << "  fit_data.len="
               << fit_data.len;
    }
    def.fields.append(field);
  }

  // If we have developer fields (since version 2.0) they must be read too
  // These is one byte containing the number of fields and 3 bytes for every field.
  // So this is identical to the normal fields but the meaning of the content is different.
  //
  // Currently we just want to ignore the developer fields because they are not meant
  // to hold relevant data we need (currently handle) for the conversion.

  // For simplicity using the existing infrastructure we do it in the following way:
  //   * We read it in as normal fields
  //   * We set the field id to kFieldInvalid so that it do not interfere with valid id's from
  //     the normal fields.
  //       -In our opinion in practice this will not happen, because we do not expect
  //        developer fields e.g. inside lap or record records. But we want to be safe here.
  //   * We do not have to change the type as we did for the id above, because fit_read_field()
  //     already uses the size information to read the data, if the type does not match the size.
  //
  // If we want to change this or if we want to avoid the xrealloc call, we can change
  // it in the future by e.g. extending the fit_message_def struct.

  // Bit 5 of the header specify if we have developer fields in the data message
  bool hasDevFields = static_cast<bool>(header & 0x20);

  if (hasDevFields) {
    int numOfDevFields = fit_getuint8();
    if (global_opts.debug_level >= 8) {
      Debug(8) << MYNAME ": definition message contains " << numOfDevFields << " developer records";
    }
    if (numOfDevFields > 0) {
      int numOfFields = num_fields + numOfDevFields;
      for (int i = num_fields; i < numOfFields; ++i) {
        int id   = fit_getuint8();
        int size = fit_getuint8();
        int type = fit_getuint8();
        fit_field_t field = {id, size, type};
        if (global_opts.debug_level >= 8) {
          Debug(8) << MYNAME ": developer record " << i - num_fields <<
                   "  ID: " << field.id << "  SIZE: " << field.size <<
                   "  TYPE: " << field.type << "  fit_data.len=" << fit_data.len;
        }
        // Because we parse developer fields like normal fields and we do not want
        // that the field id interfere which valid id's from the normal fields
        field.id = kFieldInvalid;
        def.fields.append(field);
      }
    }
  }

  fit_data.message_def.insert(local_id, def);
}

QVariant
GarminFitFormat::fit_read_field(const fit_field_t& f)
{
  /* https://forums.garmin.com/showthread.php?223645-Vivoactive-problems-plus-suggestions-for-future-firmwares&p=610929#post610929
   * Per section 4.2.1.4.2 of the FIT Protocol the size of a field may be a
   * multiple of the size of the underlying type, indicating the field
   * contains multiple elements represented as an array.
   *
   * Garmin Product Support
   */
  // In the case that the field contains one value of the indicated type we return that value,
  // otherwise we just skip over the data.

  if (global_opts.debug_level >= 8) {
    Debug(8) << MYNAME ": fit_read_field: read data field with f.type=0x" <<
             Qt::hex << f.type << " and f.size=" <<
             Qt::dec << f.size << " fit_data.len=" << fit_data.len;
  }
  switch (f.type) {
  case 0: // enum
  case 1: // sint8
  case 2: // uint8
    if (f.size == 1) {
      return fit_getuint8();
    } else { // ignore array data
      for (int i = 0; i < f.size; ++i) {
        fit_getuint8();
      }
      if (global_opts.debug_level >= 8) {
        Debug(8) << MYNAME ": fit_read_field: skipping 1-byte array data";
      }
      return -1;
    }
  case 0x7:
    return fit_getstring(f.size);

  case 0x83: // sint16
  case 0x84: // uint16
    if (f.size == 2) {
      return fit_getuint16();
    } else { // ignore array data
      for (int i = 0; i < f.size; ++i) {
        fit_getuint8();
      }
      if (global_opts.debug_level >= 8) {
        Debug(8) << MYNAME ": fit_read_field: skipping 2-byte array data";
      }
      return -1;
    }
  case 0x85: // sint32
  case 0x86: // uint32
    if (f.size == 4) {
      return fit_getuint32();
    } else { // ignore array data
      for (int i = 0; i < f.size; ++i) {
        fit_getuint8();
      }
      if (global_opts.debug_level >= 8) {
        Debug(8) << MYNAME ": fit_read_field: skipping 4-byte array data";
      }
      return -1;
    }
  default: // Ignore everything else for now.
    for (int i = 0; i < f.size; ++i) {
      fit_getuint8();
    }
    if (global_opts.debug_level >= 8) {
      Debug(8) << MYNAME ": fit_read_field: skipping unrecognized data type";
    }
    return -1;
  }
}

void
GarminFitFormat::fit_parse_data(const fit_message_def& def, int time_offset)
{
  uint32_t timestamp = fit_data.last_timestamp + time_offset;
  int32_t lat = 0x7fffffff;
  int32_t lon = 0x7fffffff;
  uint16_t alt = 0xffff;
  uint16_t speed = 0xffff;
  uint8_t heartrate = 0xff;
  uint8_t cadence = 0xff;
  uint16_t power = 0xffff;
  int8_t temperature = 0x7f;
  //int32_t startlat = 0x7fffffff;
  //int32_t startlon = 0x7fffffff;
  int32_t endlat = 0x7fffffff;
  int32_t endlon = 0x7fffffff;
  //uint32_t starttime = 0; // ??? default ?
  uint8_t event = 0xff;
  uint8_t eventtype = 0xff;
  QString name;
  QString description;

  if (global_opts.debug_level >= 7) {
    Debug(7) << MYNAME ": parsing fit data ID " << def.global_id << " with num_fields=" << def.fields.size();
  }
  for (int i = 0; i < def.fields.size(); ++i) {
    if (global_opts.debug_level >= 7) {
      Debug(7) << MYNAME ": parsing field " << i;
    }
    const fit_field_t& f = def.fields.at(i);
    QVariant field = fit_read_field(f);
    uint32_t val = -1;
    if (field.canConvert<uint>()) {
      val = field.toUInt();
    }
    if (f.id == kFieldTimestamp) {
      if (global_opts.debug_level >= 7) {
        Debug(7) << MYNAME ": parsing fit data: timestamp=" << static_cast<int32_t>(val);
      }
      timestamp = val;
      // if the timestamp is < 0x10000000, this value represents
      // system time; to convert it to UTC, add the global utc offset to it
      if (timestamp < 0x10000000) {
        timestamp += fit_data.global_utc_offset;
      }
      fit_data.last_timestamp = timestamp;
    } else {
      switch (def.global_id) {
      case kIdDeviceSettings: // device settings message
        switch (f.id) {
        case kFieldGlobalUtcOffset:
          if (global_opts.debug_level >= 7) {
            Debug(7) << MYNAME ": parsing fit data: global utc_offset=" << static_cast<int32_t>(val);
          }
          fit_data.global_utc_offset = val;
          break;
        default:
          if (global_opts.debug_level >= 1) {
            Debug(1) << MYNAME ": unrecognized data type in GARMIN FIT device settings: f.id=" << f.id;
          }
          break;
        } // switch (f.id)
        // end of case def.global_id = kIdDeviceSettings
        break;

      case kIdRecord: // record message - trkType is a track
        switch (f.id) {
        case kFieldLatitude:
          if (global_opts.debug_level >= 7) {
            Debug(7) << MYNAME ": parsing fit data: lat=" << static_cast<int32_t>(val);
          }
          lat = val;
          break;
        case kFieldLongitude:
          if (global_opts.debug_level >= 7) {
            Debug(7) << MYNAME ": parsing fit data: lon=" << static_cast<int32_t>(val);
          }
          lon = val;
          break;
        case kFieldAltitude:
          if (global_opts.debug_level >= 7) {
            Debug(7) << MYNAME ": parsing fit data: alt=" << static_cast<int32_t>(val);
          }
          if (val != 0xffff) {
            alt = val;
          }
          break;
        case kFieldHeartRate:
          if (global_opts.debug_level >= 7) {
            Debug(7) << MYNAME ": parsing fit data: heartrate=" << static_cast<int32_t>(val);
          }
          heartrate = val;
          break;
        case kFieldCadence:
          if (global_opts.debug_level >= 7) {
            Debug(7) << MYNAME ": parsing fit data: cadence=" << static_cast<int32_t>(val);
          }
          cadence = val;
          break;
        case kFieldDistance:
          // NOTE: 5 is DISTANCE in cm ... unused.
          if (global_opts.debug_level >= 7) {
            Debug(7) << MYNAME ": unrecognized data type in GARMIN FIT record: f.id=" << f.id;
          }
          break;
        case kFieldSpeed:
          if (global_opts.debug_level >= 7) {
            Debug(7) << MYNAME ": parsing fit data: speed=" << static_cast<int32_t>(val);
          }
          if (val != 0xffff) {
            speed = val;
          }
          break;
        case kFieldPower:
          if (global_opts.debug_level >= 7) {
            Debug(7) << MYNAME ": parsing fit data: power=" << static_cast<int32_t>(val);
          }
          power = val;
          break;
        case kFieldTemperature:
          if (global_opts.debug_level >= 7) {
            Debug(7) << MYNAME ": parsing fit data: temperature=" << static_cast<int32_t>(val);
          }
          temperature = val;
          break;
        case kFieldEnhancedSpeed:
          if (global_opts.debug_level >= 7) {
            Debug(7) << MYNAME ": parsing fit data: enhanced_speed=" << static_cast<int32_t>(val);
          }
          if (val != 0xffff) {
            speed = val;
          }
          break;
        case kFieldEnhancedAltitude:
          if (global_opts.debug_level >= 7) {
            Debug(7) << MYNAME ": parsing fit data: enhanced_altitude=" << static_cast<int32_t>(val);
          }
          if (val != 0xffff) {
            alt = val;
          }
          break;
        default:
          if (global_opts.debug_level >= 1) {
            Debug(1) << MYNAME ": unrecognized data type in GARMIN FIT record: f.id=" << f.id;
          }
          break;
        } // switch (f.id)
        // end of case def.global_id = kIdRecord
        break;

      case kIdLap: // lap wptType , endlat+lon is wpt
        switch (f.id) {
        case kFieldStartTime:
          if (global_opts.debug_level >= 7) {
            Debug(7) << MYNAME ": parsing fit data: starttime=" << static_cast<int32_t>(val);
          }
          //starttime = val;
          break;
        case kFieldStartLatitude:
          if (global_opts.debug_level >= 7) {
            Debug(7) << MYNAME ": parsing fit data: startlat=" << static_cast<int32_t>(val);
          }
          //startlat = val;
          break;
        case kFieldStartLongitude:
          if (global_opts.debug_level >= 7) {
            Debug(7) << MYNAME ": parsing fit data: startlon=" << static_cast<int32_t>(val);
          }
          //startlon = val;
          break;
        case kFieldEndLatitude:
          if (global_opts.debug_level >= 7) {
            Debug(7) << MYNAME ": parsing fit data: endlat=" << static_cast<int32_t>(val);
          }
          endlat = val;
          break;
        case kFieldEndLongitude:
          if (global_opts.debug_level >= 7) {
            Debug(7) << MYNAME ": parsing fit data: endlon=" << static_cast<int32_t>(val);
          }
          endlon = val;
          break;
        case kFieldElapsedTime:
          if (global_opts.debug_level >= 7) {
            Debug(7) << MYNAME ": parsing fit data: elapsedtime=" << static_cast<int32_t>(val);
          }
          //elapsedtime = val;
          break;
        case kFieldTotalDistance:
          if (global_opts.debug_level >= 7) {
            Debug(7) << MYNAME ": parsing fit data: totaldistance=" << static_cast<int32_t>(val);
          }
          //totaldistance = val;
          break;
        default:
          if (global_opts.debug_level >= 1) {
            Debug(1) << MYNAME ": unrecognized data type in GARMIN FIT lap: f.id=" << f.id;
          }
          break;
        } // switch (f.id)
        // end of case def.global_id = kIdLap
        break;

      case kIdEvent:
        switch (f.id) {
        case kFieldEvent:
          if (global_opts.debug_level >= 7) {
            Debug(7) << MYNAME ": parsing fit data: event=" << static_cast<int32_t>(val);
          }
          event = val;
          break;
        case kFieldEventType:
          if (global_opts.debug_level >= 7) {
            Debug(7) << MYNAME ": parsing fit data: eventtype=" << static_cast<int32_t>(val);
          }
          eventtype = val;
          break;
        } // switch (f.id)
        // end of case def.global_id = kIdEvent
        break;

      case kIdLocations:
        switch (f.id) {
        case kFieldLocLatitude:
          if (global_opts.debug_level >= 7) {
            Debug(7) << MYNAME ": parsing fit data: lat=" << static_cast<int32_t>(val);
          }
          lat = val;
          break;
        case kFieldLocLongitude:
          if (global_opts.debug_level >= 7) {
            Debug(7) << MYNAME ": parsing fit data: lon=" << static_cast<int32_t>(val);
          }
          lon = val;
          break;
        case kFieldLocAltitude:
          if (global_opts.debug_level >= 7) {
            Debug(7) << MYNAME ": parsing fit data: alt=" << static_cast<int32_t>(val);
          }
          if (val != 0xffff) {
            alt = val;
          }
          break;
        case kFieldLocationName:
          name = field.toString();
          if (global_opts.debug_level >= 7) {
            Debug(7) << MYNAME ": parsing fit data: location name=" << name;
          }
          break;
        case kFieldLocationDescription:
          description = field.toString();
          if (global_opts.debug_level >= 7) {
            Debug(7) << MYNAME ": parsing fit data: location description=" << description;
          }
          break;
        default:
          if (global_opts.debug_level >= 1) {
            Debug(1) << MYNAME ": unrecognized data type in GARMIN FIT locations: f.id=" << f.id;
          }
          break;
        } // switch (f.id)
        // end of case def.global_id = kIdLocations
        break;
      default:
        if (global_opts.debug_level >= 1) {
          Debug(1) << MYNAME ": unrecognized/unhandled global ID for GARMIN FIT: " << def.global_id;
        }
        break;
      } // switch (def.global_id)
    }
  }

  if (global_opts.debug_level >= 7) {
    Debug(7) << MYNAME ": storing fit data with num_fields=" << def.fields.size();
  }
  switch (def.global_id) {
  case kIdLap: { // lap message
    if (endlat == 0x7fffffff || endlon == 0x7fffffff) {
      break;
    }
    if (global_opts.debug_level >= 7) {
      Debug(7) << MYNAME ": storing fit data LAP " << def.global_id;
    }
    auto* lappt = new Waypoint;
    lappt->latitude = GPS_Math_Semi_To_Deg(endlat);
    lappt->longitude = GPS_Math_Semi_To_Deg(endlon);
    lappt->shortname = QStringLiteral("LAP%1").arg(++lap_ct, 3, 10, QLatin1Char('0'));
    waypt_add(lappt);
  }
  break;
  case kIdRecord: { // record message
    if ((lat == 0x7fffffff || lon == 0x7fffffff) && !opt_allpoints) {
      break;
    }

    auto* waypt = new Waypoint;
    if (lat != 0x7fffffff) {
      waypt->latitude = GPS_Math_Semi_To_Deg(lat);
    }
    if (lon != 0x7fffffff) {
      waypt->longitude = GPS_Math_Semi_To_Deg(lon);
    }
    if (alt != 0xffff) {
      waypt->altitude = (alt / 5.0) - 500;
    }
    waypt->SetCreationTime(GPS_Math_Gtime_To_Utime(timestamp));
    if (speed != 0xffff) {
      waypt->set_speed(speed / 1000.0f);
    }
    if (heartrate != 0xff) {
      waypt->heartrate = heartrate;
    }
    if (cadence != 0xff) {
      waypt->cadence = cadence;
    }
    if (power != 0xffff) {
      waypt->power = power;
    }
    if (temperature != 0x7f) {
      waypt->set_temperature(temperature);
    }
    if (new_trkseg) {
      waypt->wpt_flags.new_trkseg = 1;
      new_trkseg = false;
    }
    track_add_wpt(fit_data.track, waypt);
  }
  break;
  case kIdEvent: // event message
    if (event == kEnumEventTimer && eventtype == kEnumEventTypeStart) {
      // Start event, start new track segment. Note: We don't do this
      // on stop events because some GPS devices seem to generate a last
      // trackpoint after the stop event and that would erroneously get
      // assigned to the next segment.
      new_trkseg = true;
    }
    break;
  case kIdLocations: { // locations message
    if (lat == 0x7fffffff || lon == 0x7fffffff) {
      break;
    }
    if (global_opts.debug_level >= 7) {
      Debug(7) << MYNAME ": storing fit data location " << def.global_id;
    }
    auto* locpt = new Waypoint;
    locpt->latitude = GPS_Math_Semi_To_Deg(lat);
    locpt->longitude = GPS_Math_Semi_To_Deg(lon);
    if (alt != 0xffff) {
      locpt->altitude = (alt / 5.0) - 500;
    }
    locpt->shortname = name;
    locpt->description = description;
    waypt_add(locpt);
  }
  break;
  }
}

void
GarminFitFormat::fit_parse_data_message(uint8_t header)
{
  int local_id = header & 0x0f;
  if (fit_data.message_def.contains(local_id)) {
    fit_parse_data(fit_data.message_def.value(local_id), 0);
  } else {
    throw ReaderException(
      QString("Message %1 hasn't been defined before being used at file position 0x%2.").
      arg(local_id).arg(gbftell(fin) - 1, 0, 16).toStdString());
  }
}

void
GarminFitFormat::fit_parse_compressed_message(uint8_t header)
{
  int local_id = (header >> 5) & 3;
  if (fit_data.message_def.contains(local_id)) {
    fit_parse_data(fit_data.message_def.value(local_id), header & 0x1f);
  } else {
    throw ReaderException(
      QString("Compressed message %1 hasn't been defined before being used at file position 0x%2.").
      arg(local_id).arg(gbftell(fin) - 1, 0, 16).toStdString());
  }
}

/*******************************************************************************
* fit_parse_record- parse each record in the file
*******************************************************************************/
void
GarminFitFormat::fit_parse_record()
{
  gbsize_t position = gbftell(fin);
  uint8_t header = fit_getuint8();
  // high bit 7 set -> compressed message (0 for normal)
  // second bit 6 set -> 0 for data message, 1 for definition message
  // bit 5 -> message type specific
  //    definition message: Bit set means that we have additional Developer Field definitions
  //                        behind the field definitions inside the record content
  //    data message: currently not used
  // bit 4 -> reserved
  // bits 3..0 -> local message type
  if (header & 0x80) {
    if (global_opts.debug_level >= 6) {
      Debug(6) << MYNAME ": got compressed message at file position 0x" <<
               Qt::hex << position << ", fit_data.len=" << Qt::dec << fit_data.len
               << " ...local message type 0x" << Qt::hex << (header & 0x0f);
    }
    fit_parse_compressed_message(header);
  } else if (header & 0x40) {
    if (global_opts.debug_level >= 6) {
      Debug(6) << MYNAME ": got definition message at file position 0x" <<
               Qt::hex << position << ", fit_data.len=" << Qt::dec << fit_data.len
               << " ...local message type 0x" << Qt::hex << (header & 0x0f);
    }
    fit_parse_definition_message(header);
  } else {
    if (global_opts.debug_level >= 6) {
      Debug(6) << MYNAME ": got data message at file position 0x" <<
               Qt::hex << position << ", fit_data.len=" << Qt::dec << fit_data.len
               << " ...local message type 0x" << Qt::hex << (header & 0x0f);
    }
    fit_parse_data_message(header);
  }
}

void
GarminFitFormat::fit_check_file_crc() const
{
  // Check file CRC

  gbsize_t position = gbftell(fin);

  uint16_t crc = 0;
  gbfseek(fin, 0, SEEK_SET);
  while (true) {
    int data = gbfgetc(fin);
    if (data == EOF) {
      break;
    }
    crc = fit_crc16(data, crc);
  }
  if (crc != 0) {
    Warning().nospace() << MYNAME ": File CRC mismatch in file " <<  fin->name << ".";
    if (!opt_recoverymode) {
      fatal(FatalMsg().nospace() << MYNAME ": File " << fin->name << " is corrupt.  Use recoverymode option at your risk.");
    }
  } else if (global_opts.debug_level >= 1) {
    Debug(1) << MYNAME ": File CRC verified.";
  }

  gbfseek(fin, position, SEEK_SET);
}

/*******************************************************************************
* fit_read- global entry point
* - parse the header
* - parse all the records in the file
*******************************************************************************/
void
GarminFitFormat::read()
{
  fit_check_file_crc();

  fit_parse_header();

  fit_data.track = new route_head;
  track_add_head(fit_data.track);
  if (global_opts.debug_level >= 1) {
    Debug(1) << MYNAME ": starting to read data with fit_data.len=" << fit_data.len;
  }
  try {
    while (fit_data.len) {
      fit_parse_record();
    }
  } catch (ReaderException& e) {
    if (opt_recoverymode) {
      warning(MYNAME ": %s\n",e.what());
      warning(MYNAME ": Aborting read and continuing processing.\n");
    } else {
      fatal(MYNAME ": %s  Use recoverymode option at your risk.\n",e.what());
    }
  }
}

/*******************************************************************************
* FIT writing
*******************************************************************************/

void
GarminFitFormat::fit_write_message_def(uint8_t local_id, uint16_t global_id, const std::vector<fit_field_t>& fields) const
{
  gbfputc(0x40 | local_id, fout); // Local ID
  gbfputc(0, fout); // Reserved
  gbfputc(0, fout); // Little endian
  gbfputuint16(global_id, fout); // Global ID
  gbfputc(fields.size(), fout); // Number of fields
  for (auto&& field : fields) {
    gbfputc(field.id, fout); // Field definition number
    gbfputc(field.size, fout); // Field size in bytes
    gbfputc(field.type, fout); // Field type
  }
}

uint16_t
GarminFitFormat::fit_crc16(uint8_t data, uint16_t crc)
{
  static const uint16_t crc_table[] = {
    0x0000, 0xcc01, 0xd801, 0x1400, 0xf001, 0x3c00, 0x2800, 0xe401,
    0xa001, 0x6c00, 0x7800, 0xb401, 0x5000, 0x9c01, 0x8801, 0x4400
  };

  crc = (crc >> 4) ^ crc_table[crc & 0xf] ^ crc_table[data & 0xf];
  crc = (crc >> 4) ^ crc_table[crc & 0xf] ^ crc_table[(data >> 4) & 0xf];
  return crc;
}

void
GarminFitFormat::fit_write_timestamp(const gpsbabel::DateTime& t) const
{
  uint32_t t_fit;
  if (t.isValid() && t.toTime_t() >= (unsigned int)GPS_Math_Gtime_To_Utime(0)) {
    t_fit = GPS_Math_Utime_To_Gtime(t.toTime_t());
  } else {
    t_fit = 0xffffffff;
  }
  gbfputuint32(t_fit, fout);
}

void
GarminFitFormat::fit_write_fixed_string(const QString& s, unsigned int len) const
{
  QString trimmed(s);
  QByteArray u8buf;

  // Truncate if too long, making sure not to chop in the middle of a UTF-8
  // character (i.e. we chop the unicode string and then check whether its
  // UTF-8 representation fits)
  while (true) {
    u8buf = trimmed.toUtf8();
    if (static_cast<unsigned int>(u8buf.size()) < len) {
      break;
    }
    trimmed.chop(1);
  }
  // If the string was too short initially or we had to chop multibyte
  // characters, the UTF-8 representation might be too short now, so pad
  // it.
  u8buf.append(len - u8buf.size(), '\0');
  gbfwrite(u8buf.data(), len, 1, fout);
}

void
GarminFitFormat::fit_write_position(double pos) const
{
  if (pos >= -180 && pos < 180) {
    gbfputint32(GPS_Math_Deg_To_Semi(pos), fout);
  } else {
    gbfputint32(0xffffffff, fout);
  }
}

// Note: The data fields written using fit_write_msg_*() below need to match
// the message field definitions in fit_msg_fields_* above!
void
GarminFitFormat::fit_write_msg_file_id(uint8_t type, uint16_t manufacturer, uint16_t product,
                                       const gpsbabel::DateTime& time_created) const
{
  gbfputc(kWriteLocalIdFileId, fout);
  gbfputc(type, fout);
  gbfputuint16(manufacturer, fout);
  gbfputuint16(product, fout);
  fit_write_timestamp(time_created);
}

void
GarminFitFormat::fit_write_msg_course(const QString& name, uint8_t sport) const
{
  gbfputc(kWriteLocalIdCourse, fout);
  fit_write_fixed_string(name, 0x10);
  gbfputc(sport, fout);
}

void
GarminFitFormat::fit_write_msg_lap(const gpsbabel::DateTime& timestamp, const gpsbabel::DateTime& start_time,
                                   double start_position_lat, double start_position_long,
                                   double end_position_lat, double end_position_long,
                                   uint32_t total_elapsed_time_s, double total_distance_m,
                                   double avg_speed_ms, double max_speed_ms) const
{
  gbfputc(kWriteLocalIdLap, fout);
  fit_write_timestamp(timestamp);
  fit_write_timestamp(start_time);
  fit_write_position(start_position_lat);
  fit_write_position(start_position_long);
  fit_write_position(end_position_lat);
  fit_write_position(end_position_long);
  if (total_elapsed_time_s < 4294967) {
    gbfputuint32(total_elapsed_time_s * 1000, fout);
    gbfputuint32(total_elapsed_time_s * 1000, fout);
  } else {
    gbfputuint32(0xffffffff, fout);
    gbfputuint32(0xffffffff, fout);
  }
  if (total_distance_m >= 0 && total_distance_m < 42949672.94) {
    gbfputuint32(total_distance_m * 100, fout);
  } else {
    gbfputuint32(0xffffffff, fout);
  }
  if (avg_speed_ms >= 0 && avg_speed_ms < 65.534) {
    gbfputuint16(avg_speed_ms * 1000, fout);
  } else {
    gbfputuint16(0xffff, fout);
  }
  if (max_speed_ms >= 0 && max_speed_ms < 65.534) {
    gbfputuint16(max_speed_ms * 1000, fout);
  } else {
    gbfputuint16(0xffff, fout);
  }
}

void
GarminFitFormat::fit_write_msg_event(const gpsbabel::DateTime& timestamp,
                                     uint8_t event, uint8_t event_type, uint8_t event_group) const
{
  gbfputc(kWriteLocalIdEvent, fout);
  fit_write_timestamp(timestamp);
  gbfputc(event, fout);
  gbfputc(event_type, fout);
  gbfputc(event_group, fout);
}

void
GarminFitFormat::fit_write_msg_course_point(const gpsbabel::DateTime& timestamp,
    double position_lat, double position_long,
    double distance_m, const QString& name,
    uint8_t type) const
{
  gbfputc(kWriteLocalIdCoursePoint, fout);
  fit_write_timestamp(timestamp);
  fit_write_position(position_lat);
  fit_write_position(position_long);
  if (distance_m >= 0 && distance_m < 42949672.94) {
    gbfputuint32(distance_m * 100, fout);
  } else {
    gbfputuint32(0xffffffff, fout);
  }
  fit_write_fixed_string(name, 0x10);
  gbfputc(type, fout);
}

void
GarminFitFormat::fit_write_msg_record(const gpsbabel::DateTime& timestamp,
                                      double position_lat, double position_long,
                                      double distance_m, double altitude,
                                      double speed_ms) const
{
  gbfputc(kWriteLocalIdRecord, fout);
  fit_write_timestamp(timestamp);
  fit_write_position(position_lat);
  fit_write_position(position_long);
  if (distance_m >= 0 && distance_m < 42949672.94) {
    gbfputuint32(distance_m * 100, fout);
  } else {
    gbfputuint32(0xffffffff, fout);
  }
  if (altitude != unknown_alt && altitude >= -500 && altitude < 12606.8) {
    gbfputuint16((altitude + 500) * 5, fout);
  } else {
    gbfputuint16(0xffff, fout);
  }
  if (speed_ms >= 0 && speed_ms < 65.534) {
    gbfputuint16(speed_ms * 1000, fout);
  } else {
    gbfputuint16(0xffff, fout);
  }
}

void
GarminFitFormat::fit_write_file_header(uint32_t file_size, uint16_t crc) const
{
  gbfputc(kWriteHeaderCrcLen, fout); // Header+CRC length
  gbfputc(0x10, fout);               // Protocol version
  gbfputuint16(0x811, fout);         // Profile version
  gbfputuint32(file_size, fout);     // Length of data records (little endian)
  gbfputs(".FIT", fout);             // Signature
  gbfputuint16(crc, fout);           // CRC
}

void
GarminFitFormat::fit_write_header_msgs(const gpsbabel::DateTime& ctime, const QString& name) const
{
  fit_write_message_def(kWriteLocalIdFileId, kIdFileId, fit_msg_fields_file_id);
  fit_write_message_def(kWriteLocalIdCourse, kIdCourse, fit_msg_fields_course);
  fit_write_message_def(kWriteLocalIdLap, kIdLap, fit_msg_fields_lap);
  fit_write_message_def(kWriteLocalIdEvent, kIdEvent, fit_msg_fields_event);
  fit_write_message_def(kWriteLocalIdCoursePoint, kIdCoursePoint, fit_msg_fields_course_point);
  fit_write_message_def(kWriteLocalIdRecord, kIdRecord, fit_msg_fields_record);

  fit_write_msg_file_id(kFileCourse, 1, 0x3e9, ctime);
  fit_write_msg_course(name, 0);
}

void
GarminFitFormat::fit_write_file_finish() const
{
  // Update data records size in file header
  gbsize_t file_size = gbftell(fout);
  if (file_size < kWriteHeaderCrcLen) {
    fatal(MYNAME ": File %s truncated\n", qPrintable(fout->name));
  }
  gbfseek(fout, 0, SEEK_SET);
  fit_write_file_header(file_size - kWriteHeaderCrcLen, 0);

  // Update file header CRC
  uint16_t crc = 0;
  gbfseek(fout, 0, SEEK_SET);
  for (unsigned int i = 0; i < kWriteHeaderLen; ++i) {
    int data = gbfgetc(fout);
    if (data == EOF) {
      fatal(MYNAME ": File %s truncated\n", qPrintable(fout->name));
    }
    crc = fit_crc16(data, crc);
  }
  gbfseek(fout, 0, SEEK_SET);
  fit_write_file_header(file_size - kWriteHeaderCrcLen, crc);

  // Write file CRC
  gbfflush(fout);
  crc = 0;
  while (true) {
    int data = gbfgetc(fout);
    if (data == EOF) {
      break;
    }
    crc = fit_crc16(data, crc);
  }
  gbfputuint16(crc, fout);
}

void
GarminFitFormat::fit_collect_track_hdr(const route_head* rte)
{
  (void)rte;
  course.clear();
}

void
GarminFitFormat::fit_collect_trackpt(const Waypoint* waypointp)
{
  course.push_back(FitCourseRecordPoint(*waypointp, false));
}

void
GarminFitFormat::fit_collect_track_tlr(const route_head* rte)
{
  // Prepare for writing a course corresponding to a track.
  // For this, we need to check for/synthesize missing information
  // and convert waypoints to coursepoints (i.e. insert them at the right
  // place between course records).

  // Recalculate odometer_distance for the whole track unless already
  // (properly, i.e. monotonically increasing) set
  double dist_sum = 0;
  double prev_lat = 999;
  double prev_lon = 999;
  double max_speed = 0;
  gpsbabel::DateTime prev_time;
  for (auto& crpt: course) {
    // Distance to prev. point
    double dist;
    if (crpt.odometer_distance && crpt.odometer_distance >= dist_sum) {
      dist = crpt.odometer_distance - dist_sum;
      dist_sum = crpt.odometer_distance;
    } else {
      if (prev_lat >= -90 && prev_lat <= 90 && prev_lon >= -180 && prev_lon <= 180) {
        dist = gcgeodist(prev_lat, prev_lon, crpt.lat, crpt.lon);
      } else {
        dist = 0;
      }
      dist_sum += dist;
      crpt.odometer_distance = dist_sum;
    }
    prev_lat = crpt.lat;
    prev_lon = crpt.lon;

    // Check/set timestamp/speed
    if (!crpt.creation_time.isValid() ||
        (prev_time.isValid() && prev_time >= crpt.creation_time)) {
      if (crpt.speed < 1e-3) {
        crpt.speed = kSynthSpeed;
      }
      crpt.creation_time = prev_time.addSecs(dist / crpt.speed);
    } else if (crpt.speed < 1e-3) {
      uint64_t duration = prev_time.secsTo(crpt.creation_time);
      if (!duration) {
        duration = 1;
      }
      crpt.speed = dist / duration;
    }
    prev_time = crpt.creation_time;

    if (crpt.speed > max_speed) {
      max_speed = crpt.speed;
    }
  }

  // Insert course points at the right place between track points (with
  // minimum distance to next track point)
  while (!waypoints.empty()) {
    auto& wpt = waypoints.front();
    double best_distance = -1;
    auto best_distance_it = course.begin();
    double best_odometer_distance = 0;
    for (auto cit = course.begin(); cit != course.end(); ++cit) {
      if (!cit->is_course_point) {
        double distance = gcgeodist(cit->lat, cit->lon, wpt.lat, wpt.lon);
        if (best_distance < 0 || distance < best_distance) {
          best_distance = distance;
          best_distance_it = cit;
          best_odometer_distance = cit->odometer_distance;
        }
      }
    }
    wpt.odometer_distance = best_odometer_distance;
    course.insert(best_distance_it, wpt);
    waypoints.pop_front();
  }

  // Use current time as creation time if we have nothing better
  gpsbabel::DateTime track_date_time;
  gpsbabel::DateTime track_end_date_time;
  gpsbabel::DateTime creation_time;
  double first_lat = 999;
  double first_lon = 999;
  double last_lat = 999;
  double last_lon = 999;
  if (!course.empty()) {
    track_date_time = creation_time = course.front().creation_time;
    track_end_date_time = course.back().creation_time;
    first_lat = course.front().lat;
    first_lon = course.front().lon;
    last_lat = course.back().lat;
    last_lon = course.back().lon;
  } else {
    creation_time = gpsbabel::DateTime::currentDateTimeUtc();
  }
  uint32_t total_time = track_date_time.secsTo(track_end_date_time);

  // Write file-level header messages here because we need a name and date
  // and take these from the first track
  if (write_header_msgs) {
    fit_write_header_msgs(creation_time, rte->rte_name);
    write_header_msgs = false;
  }

  // Write track header messages (lap+start event)
  double avg_speed = 0;
  if (total_time) {
    avg_speed = dist_sum / total_time;
  }
  fit_write_msg_lap(track_date_time, track_date_time,
                    first_lat, first_lon, last_lat, last_lon, total_time, dist_sum,
                    avg_speed, max_speed);
  fit_write_msg_event(track_date_time, kEventTimer, kEventTypeStart, 0);

  // Write track/course points for the whole track
  for (const auto& crpt: course) {
    if (crpt.is_course_point) {
      fit_write_msg_course_point(crpt.creation_time,
                                 crpt.lat,
                                 crpt.lon,
                                 crpt.odometer_distance,
                                 crpt.shortname,
                                 crpt.course_point_type);
    } else {
      fit_write_msg_record(crpt.creation_time,
                           crpt.lat,
                           crpt.lon,
                           crpt.odometer_distance,
                           crpt.altitude,
                           crpt.speed);
    }
  }

  fit_write_msg_event(track_end_date_time, kEventTimer, kEventTypeStopDisableAll, 0);
}

void
GarminFitFormat::fit_collect_waypt(const Waypoint* waypointp)
{
  FitCourseRecordPoint crpt(*waypointp, true);

  // Try to find a better course point type than "generic", based on the
  // course point name
  for (const auto& cptm: kCoursePointTypeMapping) {
    if (crpt.shortname.contains(cptm.first, Qt::CaseInsensitive)) {
      crpt.course_point_type = cptm.second;
      break;
    }
  }

  waypoints.push_back(crpt);
}

/*******************************************************************************
* fit_write- global entry point
*******************************************************************************/
void
GarminFitFormat::write()
{
  fit_write_file_header(0, 0);
  write_header_msgs = true;

  auto fit_collect_waypt_lambda = [this](const Waypoint* waypointp)->void {
    fit_collect_waypt(waypointp);
  };
  waypt_disp_all(fit_collect_waypt_lambda);

  auto fit_collect_track_hdr_lambda = [this](const route_head* rte)->void {
    fit_collect_track_hdr(rte);
  };
  auto fit_collect_track_tlr_lambda = [this](const route_head* rte)->void {
    fit_collect_track_tlr(rte);
  };
  auto fit_collect_trackpt_lambda = [this](const Waypoint* waypointp)->void {
    fit_collect_trackpt(waypointp);
  };
  track_disp_all(fit_collect_track_hdr_lambda, fit_collect_track_tlr_lambda, fit_collect_trackpt_lambda);
  fit_write_file_finish();
}
