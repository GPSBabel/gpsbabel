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

#include <cstdint>
#include <cstdio>               // for EOF, snprintf
#include <vector>
#include <deque>
#include <utility>

#include <QtCore/QDateTime>     // for QDateTime
#include <QtCore/QString>       // for QString

#include "defs.h"
#include "gbfile.h"             // for gbfgetc, gbfread, gbfclose, gbfgetuint16, gbfgetuint32, gbfile, gbfopen_le
#include "jeeps/gpsmath.h"      // for GPS_Math_Semi_To_Deg


#define MYNAME "fit"

// constants for global IDs
const int kIdFileId = 0;
const int kIdDeviceSettings = 0;
const int kIdLap = 19;
const int kIdRecord = 20;
const int kIdEvent = 21;
const int kIdCourse = 31;
const int kIdCoursePoint = 32;

// constants for local IDs (for writing)
const int kWriteLocalIdFileId = 0;
const int kWriteLocalIdCourse = 1;
const int kWriteLocalIdLap = 2;
const int kWriteLocalIdEvent = 3;
const int kWriteLocalIdCoursePoint = 4;
const int kWriteLocalIdRecord = 5;

// constants for message fields
// for all global IDs
const int kFieldTimestamp = 253;
const int kFieldMessageIndex = 254;
// for global ID: file id
const int kFieldType = 0;
const int kFieldManufacturer = 1;
const int kFieldProduct = 2;
const int kFieldTimeCreated = 4;
// for global ID: device settings
const int kFieldGlobalUtcOffset = 4;
// for global ID: lap
const int kFieldStartTime = 2;
const int kFieldStartLatitude = 3;
const int kFieldStartLongitude = 4;
const int kFieldEndLatitude = 5;
const int kFieldEndLongitude = 6;
const int kFieldElapsedTime = 7;
const int kFieldTotalTimerTime = 8;
const int kFieldTotalDistance = 9;
const int kFieldAvgSpeed = 13;
const int kFieldMaxSpeed = 14;
// for global ID: record
const int kFieldLatitude = 0;
const int kFieldLongitude = 1;
const int kFieldAltitude = 2;
const int kFieldHeartRate = 3;
const int kFieldCadence = 4;
const int kFieldDistance = 5;
const int kFieldSpeed = 6;
const int kFieldPower = 7;
const int kFieldTemperature = 13;
const int kFieldEnhancedSpeed = 73;
const int kFieldEnhancedAltitude = 78;
// for global ID: event
const int kFieldEvent = 0;
const int kEnumEventTimer = 0;
const int kFieldEventType = 1;
const int kEnumEventTypeStart = 0;
const int kFieldEventGroup = 4;
// for global ID: course
const int kFieldSport = 4;
const int kFieldName = 5;
// for global ID: course point
const int kFieldCPTimeStamp = 1;
const int kFieldCPPositionLat = 2;
const int kFieldCPPositionLong = 3;
const int kFieldCPDistance = 4;
const int kFieldCPName = 6;
const int kFieldCPType = 5;

// For developer fields as a non conflicting id
const int kFieldInvalid = 255;

// types for message definitions
const int kTypeEnum = 0x00;
const int kTypeUint8 = 0x02;
const int kTypeString = 0x07;
const int kTypeUint16 = 0x84;
const int kTypeSint32 = 0x85;
const int kTypeUint32 = 0x86;

// misc. constants for message fields
const int kFileCourse = 0x06;
const int kEventTimer = 0x00;
const int kEventTypeStart = 0x00;
const int kEventTypeStopDisableAll = 0x09;
const int kCoursePointTypeGeneric = 0x00;
const int kCoursePointTypeLeft = 0x06;
const int kCoursePointTypeRight = 0x07;

const int kWriteHeaderLen = 12;
const int kWriteHeaderCrcLen = 14;

const double kSynthSpeed = 10.0 * 1000 / 3600; /* speed in m/s */

static char* opt_allpoints = nullptr;
static int lap_ct = 0;
static bool new_trkseg = false;
static bool write_header_msgs = false;


static
QVector<arglist_t> fit_args = {
  {
    "allpoints", &opt_allpoints,
    "Read all points even if latitude or longitude is missing",
    nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
  },
};

const std::vector<std::pair<QString, int> > kCoursePointTypeMapping = {
  {"left", kCoursePointTypeLeft},
  {"links", kCoursePointTypeLeft},
  {"gauche", kCoursePointTypeLeft},
  {"izquierda", kCoursePointTypeLeft},
  {"sinistra", kCoursePointTypeLeft},

  {"right", kCoursePointTypeRight},
  {"rechts", kCoursePointTypeRight},
  {"droit", kCoursePointTypeRight},
  {"derecha", kCoursePointTypeRight},
  {"destro", kCoursePointTypeRight},
};


struct fit_field_t {
  int id;
  int size;
  int type;
};

struct fit_message_def {
  int endian;
  int global_id;
  int num_fields;
  fit_field_t* fields;
};

static struct {
  int len;
  int endian;
  route_head* track;
  uint32_t last_timestamp;
  uint32_t global_utc_offset;
  fit_message_def message_def[16];
} fit_data;

struct FitCourseRecordPoint {
  FitCourseRecordPoint(const Waypoint &wpt, bool is_course_point, unsigned int course_point_type = kCoursePointTypeGeneric)
      : lat(wpt.latitude),
        lon(wpt.longitude),
        altitude(wpt.altitude),
        speed(WAYPT_HAS((&wpt), speed) ? wpt.speed : -1),
        odometer_distance(wpt.odometer_distance),
        creation_time(wpt.creation_time),
        shortname(wpt.shortname),
        is_course_point(is_course_point),
        course_point_type(course_point_type) { }
  double lat, lon, altitude;
  double speed, odometer_distance;
  gpsbabel::DateTime creation_time;
  QString shortname;
  bool is_course_point;
  unsigned int course_point_type;
};

std::deque<FitCourseRecordPoint> course, waypoints;


static	gbfile* fin;
static	gbfile* fout;

/*******************************************************************************
* %%%        global callbacks called by gpsbabel main process              %%% *
*******************************************************************************/

static void
fit_rd_init(const QString& fname)
{
  fin = gbfopen_le(fname, "rb", MYNAME);
}

static void
fit_rd_deinit()
{
  for (auto &local_id : fit_data.message_def) {
    fit_message_def* def = &local_id;
    if (def->fields) {
      xfree(def->fields);
      def->fields = nullptr;
    }
  }

  gbfclose(fin);
}

static void
fit_wr_init(const QString& fname)
{
  fout = gbfopen_le(fname, "w+b", MYNAME);
}

static void
fit_wr_deinit()
{
  gbfclose(fout);
}


/*******************************************************************************
* fit_parse_header- parse the global FIT header
*******************************************************************************/
static void
fit_parse_header()
{
  char sig[4];

  int len = gbfgetc(fin);
  if (len == EOF || len < 12) {
    fatal(MYNAME ": Bad header\n");
  }
  if (global_opts.debug_level >= 1) {
    debug_print(1,"%s: header len=%d\n", MYNAME, len);
  }

  int ver = gbfgetc(fin);
  if (ver == EOF || (ver >> 4) > 2)
    fatal(MYNAME ": Unsupported protocol version %d.%d\n",
          ver >> 4, ver & 0xf);
  if (global_opts.debug_level >= 1) {
    debug_print(1,"%s: protocol version=%d\n", MYNAME, ver);
  }

  // profile version
  ver = gbfgetuint16(fin);
  // data length
  fit_data.len = gbfgetuint32(fin);
  // File signature
  is_fatal(gbfread(sig, 4, 1, fin) != 1,
           MYNAME ": Unexpected end of file\n");
  if (sig[0] != '.' || sig[1] != 'F' || sig[2] != 'I' || sig[3] != 'T') {
    fatal(MYNAME ": .FIT signature missing\n");
  }

  if (global_opts.debug_level >= 1) {
    debug_print(1,"%s: profile version=%d\n", MYNAME, ver);
    debug_print(1,"%s: fit_data.len=%d\n", MYNAME, fit_data.len);
  }

  if (len > 12) {
    // Unused according to Ingo Arndt
    gbfgetuint16(fin);
  }

  fit_data.global_utc_offset = 0;
}

static uint8_t
fit_getuint8()
{
  if (fit_data.len == 0) {
    // fail gracefully for GARMIN Edge 800 with newest firmware, seems to write a wrong record length
    // for the last record.
    //fatal(MYNAME ": record truncated: fit_data.len=0\n");
    if (global_opts.debug_level >= 1) {
      warning("%s: record truncated: fit_data.len=0\n", MYNAME);
    }
    return 0;
  }
  int val = gbfgetc(fin);
  if (val == EOF) {
    fatal(MYNAME ": unexpected end of file with fit_data.len=%d\n",fit_data.len);
  }
  fit_data.len--;
  return (uint8_t)val;

}

static uint16_t
fit_getuint16()
{
  char buf[2];

  if (fit_data.len < 2) {
    fatal(MYNAME ": record truncated: expecting char[2], but only got %d\n",fit_data.len);
  }
  is_fatal(gbfread(buf, 2, 1, fin) != 1,
           MYNAME ": unexpected end of file with fit_data.len=%d\n",fit_data.len);
  fit_data.len -= 2;
  if (fit_data.endian) {
    return be_read16(buf);
  } else {
    return le_read16(buf);
  }

}

static uint32_t
fit_getuint32()
{
  char buf[4];

  if (fit_data.len < 4) {
    fatal(MYNAME ": record truncated: expecting char[4], but only got %d\n",fit_data.len);
  }
  is_fatal(gbfread(buf, 4, 1, fin) != 1,
           MYNAME ": unexpected end of file with fit_data.len=%d\n",fit_data.len);
  fit_data.len -= 4;
  if (fit_data.endian) {
    return be_read32(buf);
  } else {
    return le_read32(buf);
  }

}

static void
fit_parse_definition_message(uint8_t header)
{
  int local_id = header & 0x0f;
  fit_message_def* def = &fit_data.message_def[local_id];

  if (def->fields) {
    xfree(def->fields);
  }

  // first byte is reserved.  It's usually 0 and we don't know what it is,
  // but we've seen some files that are 0x40.  So we just read it and toss it.
  int i = fit_getuint8();

  // second byte is endianness
  def->endian = fit_getuint8();
  if (def->endian > 1) {
    warning(MYNAME ": Unusual endian field (interpreting as big endian): %d\n",def->endian);
  }
  fit_data.endian = def->endian;

  // next two bytes are the global message number
  def->global_id = fit_getuint16();

  // byte 5 has the number of records in the remainder of the definition message
  def->num_fields = fit_getuint8();
  if (global_opts.debug_level >= 8) {
    debug_print(8,"%s: definition message contains %d records\n",MYNAME, def->num_fields);
  }
  if (def->num_fields == 0) {
    def->fields = (fit_field_t*) xmalloc(sizeof(fit_field_t));
  }

  // remainder of the definition message is data at one byte per field * 3 fields
  if (def->num_fields > 0) {
    def->fields = (fit_field_t*) xmalloc(def->num_fields * sizeof(fit_field_t));
    for (i = 0; i < def->num_fields; i++) {
      def->fields[i].id   = fit_getuint8();
      def->fields[i].size = fit_getuint8();
      def->fields[i].type = fit_getuint8();
      if (global_opts.debug_level >= 8) {
        debug_print(8,"%s: record %d  ID: %d  SIZE: %d  TYPE: %d  fit_data.len=%d\n",
                    MYNAME, i, def->fields[i].id, def->fields[i].size, def->fields[i].type,fit_data.len);
      }

    }
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
      debug_print(8,"%s: definition message contains %d developer records\n",MYNAME, numOfDevFields);
    }
    if (numOfDevFields == 0) {
      return;
    }

    int numOfFields = def->num_fields+numOfDevFields;
    def->fields = (fit_field_t*) xrealloc(def->fields, numOfFields * sizeof(fit_field_t));
    for (i = def->num_fields; i < numOfFields; i++) {
      def->fields[i].id   = fit_getuint8();
      def->fields[i].size = fit_getuint8();
      def->fields[i].type = fit_getuint8();
      if (global_opts.debug_level >= 8) {
        debug_print(8,"%s: developer record %d  ID: %d  SIZE: %d  TYPE: %d  fit_data.len=%d\n",
                    MYNAME, i-def->num_fields, def->fields[i].id, def->fields[i].size, def->fields[i].type,fit_data.len);
      }
      // Because we parse developer fields like normal fields and we do not want 
      // that the field id interfere which valid id's from the normal fields
      def->fields[i].id = kFieldInvalid;

    }
    def->num_fields = numOfFields;
  }
}

static uint32_t
fit_read_field(fit_field_t* f)
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
  int i;

  if (global_opts.debug_level >= 8) {
    debug_print(8,"%s: fit_read_field: read data field with f->type=0x%X and f->size=%d fit_data.len=%d\n",
                MYNAME, f->type, f->size, fit_data.len);
  }
  switch (f->type) {
  case 0: // enum
  case 1: // sint8
  case 2: // uint8
    if (f->size == 1) {
      return fit_getuint8();
    } else { // ignore array data
      for (i = 0; i < f->size; i++) {
        fit_getuint8();
      }
      if (global_opts.debug_level >= 8) {
        debug_print(8, "%s: fit_read_field: skipping 1-byte array data\n", MYNAME);
      }
      return -1;
    }
  case 0x83: // sint16
  case 0x84: // uint16
    if (f->size == 2) {
      return fit_getuint16();
    } else { // ignore array data
      for (i = 0; i < f->size; i++) {
        fit_getuint8();
      }
      if (global_opts.debug_level >= 8) {
        debug_print(8, "%s: fit_read_field: skipping 2-byte array data\n", MYNAME);
      }
      return -1;
    }
  case 0x85: // sint32
  case 0x86: // uint32
    if (f->size == 4) {
      return fit_getuint32();
    } else { // ignore array data
      for (i = 0; i < f->size; i++) {
        fit_getuint8();
      }
      if (global_opts.debug_level >= 8) {
        debug_print(8, "%s: fit_read_field: skipping 4-byte array data\n", MYNAME);
      }
      return -1;
    }
  default: // Ignore everything else for now.
    for (i = 0; i < f->size; i++) {
      fit_getuint8();
    }
    if (global_opts.debug_level >= 8) {
      debug_print(8, "%s: fit_read_field: skipping unrecognized data type\n", MYNAME);
    }
    return -1;
  }
}

static void
fit_parse_data(fit_message_def* def, int time_offset)
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
  Waypoint* waypt;
  int32_t startlat = 0x7fffffff;
  int32_t startlon = 0x7fffffff;
  int32_t endlat = 0x7fffffff;
  int32_t endlon = 0x7fffffff;
  uint32_t starttime = 0; // ??? default ?
  uint8_t event = 0xff;
  uint8_t eventtype = 0xff;
  char cbuf[10];
  Waypoint* lappt;  // WptPt in gpx

  if (global_opts.debug_level >= 7) {
    debug_print(7,"%s: parsing fit data ID %d with num_fields=%d\n", MYNAME, def->global_id, def->num_fields);
  }
  for (int i = 0; i < def->num_fields; i++) {
    if (global_opts.debug_level >= 7) {
      debug_print(7,"%s: parsing field %d\n", MYNAME, i);
    }
    fit_field_t* f = &def->fields[i];
    uint32_t val = fit_read_field(f);
    if (f->id == kFieldTimestamp) {
      if (global_opts.debug_level >= 7) {
        debug_print(7,"%s: parsing fit data: timestamp=%d\n", MYNAME, val);
      }
      timestamp = val;
      // if the timestamp is < 0x10000000, this value represents
      // system time; to convert it to UTC, add the global utc offset to it
      if (timestamp < 0x10000000)
        timestamp += fit_data.global_utc_offset;
      fit_data.last_timestamp = timestamp;
    } else {
      switch (def->global_id) {
      case kIdDeviceSettings: // device settings message
        switch (f->id) {
        case kFieldGlobalUtcOffset:
          if (global_opts.debug_level >= 7) {
            debug_print(7,"%s: parsing fit data: global utc_offset=%d\n", MYNAME, val);
          }
          fit_data.global_utc_offset = val;
          break;
        default:
          if (global_opts.debug_level >= 1) {
            debug_print(1, "%s: unrecognized data type in GARMIN FIT device settings: f->id=%d\n", MYNAME, f->id);
          }
          break;
        } // switch (f->id)
        // end of case def->global_id = kIdDeviceSettings
        break;

      case kIdRecord: // record message - trkType is a track
        switch (f->id) {
        case kFieldLatitude:
          if (global_opts.debug_level >= 7) {
            debug_print(7,"%s: parsing fit data: lat=%d\n", MYNAME, val);
          }
          lat = val;
          break;
        case kFieldLongitude:
          if (global_opts.debug_level >= 7) {
            debug_print(7,"%s: parsing fit data: lon=%d\n", MYNAME, val);
          }
          lon = val;
          break;
        case kFieldAltitude:
          if (global_opts.debug_level >= 7) {
            debug_print(7,"%s: parsing fit data: alt=%d\n", MYNAME, val);
          }
          if (val != 0xffff) {
              alt = val;
          }
          break;
        case kFieldHeartRate:
          if (global_opts.debug_level >= 7) {
            debug_print(7,"%s: parsing fit data: heartrate=%d\n", MYNAME, val);
          }
          heartrate = val;
          break;
        case kFieldCadence:
          if (global_opts.debug_level >= 7) {
            debug_print(7,"%s: parsing fit data: cadence=%d\n", MYNAME, val);
          }
          cadence = val;
          break;
        case kFieldDistance:
          // NOTE: 5 is DISTANCE in cm ... unused.
          if (global_opts.debug_level >= 7) {
            debug_print(7, "%s: unrecognized data type in GARMIN FIT record: f->id=%d\n", MYNAME, f->id);
          }
          break;
        case kFieldSpeed:
          if (global_opts.debug_level >= 7) {
            debug_print(7,"%s: parsing fit data: speed=%d\n", MYNAME, val);
          }
          if (val != 0xffff) {
              speed = val;
          }
          break;
        case kFieldPower:
          if (global_opts.debug_level >= 7) {
            debug_print(7,"%s: parsing fit data: power=%d\n", MYNAME, val);
          }
          power = val;
          break;
        case kFieldTemperature:
          if (global_opts.debug_level >= 7) {
            debug_print(7,"%s: parsing fit data: temperature=%d\n", MYNAME, val);
          }
          temperature = val;
          break;
        case kFieldEnhancedSpeed:
          if (global_opts.debug_level >= 7) {
            debug_print(7,"%s: parsing fit data: enhanced_speed=%d\n", MYNAME, val);
          }
          if (val != 0xffff) {
              speed = val;
          }
          break;
        case kFieldEnhancedAltitude:
          if (global_opts.debug_level >= 7) {
            debug_print(7,"%s: parsing fit data: enhanced_altitude=%d\n", MYNAME, val);
          }
          if (val != 0xffff) {
              alt = val;
          }
          break;
        default:
          if (global_opts.debug_level >= 1) {
            debug_print(1, "%s: unrecognized data type in GARMIN FIT record: f->id=%d\n", MYNAME, f->id);
          }
          break;
        } // switch (f->id)
        // end of case def->global_id = kIdRecord
        break;

      case kIdLap: // lap wptType , endlat+lon is wpt
        switch (f->id) {
        case kFieldStartTime:
          if (global_opts.debug_level >= 7) {
            debug_print(7,"%s: parsing fit data: starttime=%d\n", MYNAME, val);
          }
          starttime = val;
          break;
        case kFieldStartLatitude:
          if (global_opts.debug_level >= 7) {
            debug_print(7,"%s: parsing fit data: startlat=%d\n", MYNAME, val);
          }
          startlat = val;
          break;
        case kFieldStartLongitude:
          if (global_opts.debug_level >= 7) {
            debug_print(7,"%s: parsing fit data: startlon=%d\n", MYNAME, val);
          }
          startlon = val;
          break;
        case kFieldEndLatitude:
          if (global_opts.debug_level >= 7) {
            debug_print(7,"%s: parsing fit data: endlat=%d\n", MYNAME, val);
          }
          endlat = val;
          break;
        case kFieldEndLongitude:
          if (global_opts.debug_level >= 7) {
            debug_print(7,"%s: parsing fit data: endlon=%d\n", MYNAME, val);
          }
          endlon = val;
          break;
        case kFieldElapsedTime:
          if (global_opts.debug_level >= 7) {
            debug_print(7,"%s: parsing fit data: elapsedtime=%d\n", MYNAME, val);
          }
          //elapsedtime = val;
          break;
        case kFieldTotalDistance:
          if (global_opts.debug_level >= 7) {
            debug_print(7,"%s: parsing fit data: totaldistance=%d\n", MYNAME, val);
          }
          //totaldistance = val;
          break;
        default:
          if (global_opts.debug_level >= 1) {
            debug_print(1, "%s: unrecognized data type in GARMIN FIT lap: f->id=%d\n", MYNAME, f->id);
          }
          break;
        } // switch (f->id)
        // end of case def->global_id = kIdLap
        break;

      case kIdEvent:
        switch (f->id) {
        case kFieldEvent:
          if (global_opts.debug_level >= 7) {
            debug_print(7,"%s: parsing fit data: event=%d\n", MYNAME, val);
          }
          event = val;
          break;
        case kFieldEventType:
          if (global_opts.debug_level >= 7) {
            debug_print(7,"%s: parsing fit data: eventtype=%d\n", MYNAME, val);
          }
          eventtype = val;
          break;
        } // switch (f->id)
        // end of case def->global_id = kIdEvent
        break;
      default:
        if (global_opts.debug_level >= 1) {
          debug_print(1, "%s: unrecognized/unhandled global ID for GARMIN FIT: %d\n", MYNAME, def->global_id);
        }
        break;
      } // switch (def->global_id)
    }
  }

  if (global_opts.debug_level >= 7) {
    debug_print(7,"%s: storing fit data with num_fields=%d\n", MYNAME, def->num_fields);
  }
  switch (def->global_id) {
  case kIdLap: // lap message
    if (endlat == 0x7fffffff || endlon == 0x7fffffff) {
      break;
    }
    if (global_opts.debug_level >= 7) {
      debug_print(7,"%s: storing fit data LAP %d\n", MYNAME, def->global_id);
    }
    lappt = new Waypoint;
    lappt->latitude = GPS_Math_Semi_To_Deg(endlat);
    lappt->longitude = GPS_Math_Semi_To_Deg(endlon);
    lap_ct++;
    snprintf(cbuf, sizeof(cbuf), "LAP%03d", lap_ct);
    lappt->shortname = cbuf;
    waypt_add(lappt);
    break;
  case kIdRecord: // record message
    if ((lat == 0x7fffffff || lon == 0x7fffffff) && !opt_allpoints) {
      break;
    }

    waypt = new Waypoint;
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
      WAYPT_SET(waypt, speed, speed / 1000.0f);
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
      WAYPT_SET(waypt, temperature, temperature);
    }
    if (new_trkseg) {
      waypt->wpt_flags.new_trkseg = 1;
      new_trkseg = false;
    }
    track_add_wpt(fit_data.track, waypt);
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
  }
}

static void
fit_parse_data_message(uint8_t header)
{
  int local_id = header & 0x0f;
  fit_message_def* def = &fit_data.message_def[local_id];
  fit_parse_data(def, 0);
}

static void
fit_parse_compressed_message(uint8_t header)
{
  int local_id = (header >> 5) & 3;
  fit_message_def* def = &fit_data.message_def[local_id];
  fit_parse_data(def, header & 0x1f);
}

/*******************************************************************************
* fit_parse_record- parse each record in the file
*******************************************************************************/
static void
fit_parse_record()
{
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
      debug_print(6,"%s: got compressed message at fit_data.len=%d", MYNAME, fit_data.len);
      debug_print(0," ...local message type 0x%X\n", header&0x0f);
    }
    fit_parse_compressed_message(header);
  } else if (header & 0x40) {
    if (global_opts.debug_level >= 6) {
      debug_print(6,"%s: got definition message at fit_data.len=%d", MYNAME, fit_data.len);
      debug_print(0," ...local message type 0x%X\n", header&0x0f);
    }
    fit_parse_definition_message(header);
  } else {
    if (global_opts.debug_level >= 6) {
      debug_print(6,"%s: got data message at fit_data.len=%d", MYNAME, fit_data.len);
      debug_print(0," ...local message type 0x%X\n", header&0x0f);
    }
    fit_parse_data_message(header);
  }
}

/*******************************************************************************
* fit_read- global entry point
* - parse the header
* - parse all the records in the file
*******************************************************************************/
static void
fit_read()
{
  fit_parse_header();

  fit_data.track = route_head_alloc();
  track_add_head(fit_data.track);
  if (global_opts.debug_level >= 1) {
    debug_print(1,"%s: starting to read data with fit_data.len=%d\n", MYNAME, fit_data.len);
  }
  while (fit_data.len) {
    fit_parse_record();
  }
}

/*******************************************************************************
* FIT writing
*******************************************************************************/

const static std::vector<fit_field_t> fit_msg_fields_file_id = {
  // field id,            size, type
  { kFieldType,           0x01, kTypeEnum   },
  { kFieldManufacturer,   0x02, kTypeUint16 },
  { kFieldProduct,        0x02, kTypeUint16 },
  { kFieldTimeCreated,    0x04, kTypeUint32 },
};
const static std::vector<fit_field_t> fit_msg_fields_course = {
  { kFieldName,           0x10, kTypeString },
  { kFieldSport,          0x01, kTypeEnum   },
};
const static std::vector<fit_field_t> fit_msg_fields_lap = {
  { kFieldTimestamp,      0x04, kTypeUint32 },
  { kFieldStartTime,      0x04, kTypeUint32 },
  { kFieldStartLatitude,  0x04, kTypeSint32 },
  { kFieldStartLongitude, 0x04, kTypeSint32 },
  { kFieldEndLatitude,    0x04, kTypeSint32 },
  { kFieldEndLongitude,   0x04, kTypeSint32 },
  { kFieldElapsedTime,    0x04, kTypeUint32 },
  { kFieldTotalTimerTime, 0x04, kTypeUint32 },
  { kFieldTotalDistance,  0x04, kTypeUint32 },
  { kFieldAvgSpeed,       0x02, kTypeUint16 },
  { kFieldMaxSpeed,       0x02, kTypeUint16 },
};
const static std::vector<fit_field_t> fit_msg_fields_event = {
  { kFieldTimestamp,      0x04, kTypeUint32 },
  { kFieldEvent,          0x01, kTypeEnum   },
  { kFieldEventType,      0x01, kTypeEnum   },
  { kFieldEventGroup,     0x01, kTypeUint8  },
};
const static std::vector<fit_field_t> fit_msg_fields_course_point = {
  { kFieldCPTimeStamp,    0x04, kTypeUint32 },
  { kFieldCPPositionLat,  0x04, kTypeSint32 },
  { kFieldCPPositionLong, 0x04, kTypeSint32 },
  { kFieldCPDistance,     0x04, kTypeUint32 },
  { kFieldCPName,         0x10, kTypeString },
  { kFieldCPType,         0x01, kTypeEnum   },
};
const static std::vector<fit_field_t> fit_msg_fields_record = {
  { kFieldTimestamp,      0x04, kTypeUint32 },
  { kFieldLatitude,       0x04, kTypeSint32 },
  { kFieldLongitude,      0x04, kTypeSint32 },
  { kFieldDistance,       0x04, kTypeUint32 },
  { kFieldAltitude,       0x02, kTypeUint16 },
  { kFieldSpeed,          0x02, kTypeUint16 },
};


static void
fit_write_message_def(uint8_t local_id, uint16_t global_id, const std::vector<fit_field_t> &fields) {
  gbfputc(0x40 | local_id, fout); // Local ID
  gbfputc(0, fout); // Reserved
  gbfputc(0, fout); // Little endian
  gbfputuint16(global_id, fout); // Global ID
  gbfputc(fields.size(), fout); // Number of fields
  for (auto &&field : fields) {
    gbfputc(field.id, fout); // Field definition number
    gbfputc(field.size, fout); // Field size in bytes
    gbfputc(field.type, fout); // Field type
  }
}


static uint16_t
fit_crc16(uint8_t data, uint16_t crc) {
  static const uint16_t crc_table[] = {
    0x0000, 0xcc01, 0xd801, 0x1400, 0xf001, 0x3c00, 0x2800, 0xe401,
    0xa001, 0x6c00, 0x7800, 0xb401, 0x5000, 0x9c01, 0x8801, 0x4400
  };

  crc = (crc >> 4) ^ crc_table[crc & 0xf] ^ crc_table[data & 0xf];
  crc = (crc >> 4) ^ crc_table[crc & 0xf] ^ crc_table[(data >> 4) & 0xf];
  return crc;
}


static void
fit_write_timestamp(const gpsbabel::DateTime &t) {
  uint32_t t_fit;
  if (t.isValid() && t.toTime_t() >= (unsigned int)GPS_Math_Gtime_To_Utime(0)) {
    t_fit = GPS_Math_Utime_To_Gtime(t.toTime_t());
  } else {
    t_fit = 0xffffffff;
  }
  gbfputuint32(t_fit, fout);
}


static void
fit_write_fixed_string(const QString &s, unsigned int len) {
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


static void
fit_write_position(double pos) {
  if (pos >= -180 && pos < 180) {
    gbfputint32(GPS_Math_Deg_To_Semi(pos), fout);
  } else {
    gbfputint32(0xffffffff, fout);
  }
}


// Note: The data fields written using fit_write_msg_*() below need to match
// the message field definitions in fit_msg_fields_* above!
static void
fit_write_msg_file_id(uint8_t type, uint16_t manufacturer, uint16_t product,
                      const gpsbabel::DateTime &time_created) {
  gbfputc(kWriteLocalIdFileId, fout);
  gbfputc(type, fout);
  gbfputuint16(manufacturer, fout);
  gbfputuint16(product, fout);
  fit_write_timestamp(time_created);
}

static void
fit_write_msg_course(const QString &name, uint8_t sport) {
  gbfputc(kWriteLocalIdCourse, fout);
  fit_write_fixed_string(name, 0x10);
  gbfputc(sport, fout);
}

static void
fit_write_msg_lap(const gpsbabel::DateTime &timestamp, const gpsbabel::DateTime &start_time,
                  double start_position_lat, double start_position_long,
                  double end_position_lat, double end_position_long,
                  uint32_t total_elapsed_time_s, double total_distance_m,
                  double avg_speed_ms, double max_speed_ms) {
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


static void
fit_write_msg_event(const gpsbabel::DateTime &timestamp,
                    uint8_t event, uint8_t event_type, uint8_t event_group) {
  gbfputc(kWriteLocalIdEvent, fout);
  fit_write_timestamp(timestamp);
  gbfputc(event, fout);
  gbfputc(event_type, fout);
  gbfputc(event_group, fout);
}


static void
fit_write_msg_course_point(const gpsbabel::DateTime &timestamp,
                           double position_lat, double position_long,
                           double distance_m, const QString &name,
                           uint8_t type) {
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


static void
fit_write_msg_record(const gpsbabel::DateTime &timestamp,
                     double position_lat, double position_long,
                     double distance_m, double altitude,
                     double speed_ms) {
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


static void
fit_write_file_header(uint32_t file_size, uint16_t crc)
{
  gbfputc(kWriteHeaderCrcLen, fout); // Header+CRC length
  gbfputc(0x10, fout);               // Protocol version
  gbfputuint16(0x811, fout);         // Profile version
  gbfputuint32(file_size, fout);     // Length of data records (little endian)
  gbfputs(".FIT", fout);             // Signature
  gbfputuint16(crc, fout);           // CRC
}


static void
fit_write_header_msgs(const gpsbabel::DateTime& ctime, const QString& name)
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


static void
fit_write_file_finish()
{
  // Update data records size in file header
  gbsize_t file_size = gbftell(fout);
  if (file_size < kWriteHeaderCrcLen) {
    fatal(MYNAME ": File %s truncated\n", fout->name);
  }
  gbfseek(fout, 0, SEEK_SET);
  fit_write_file_header(file_size - kWriteHeaderCrcLen, 0);

  // Update file header CRC
  uint16_t crc = 0;
  gbfseek(fout, 0, SEEK_SET);
  for (unsigned int i = 0; i < kWriteHeaderLen; i++) {
    int data = gbfgetc(fout);
    if (data == EOF) {
      fatal(MYNAME ": File %s truncated\n", fout->name);
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

static void
fit_collect_track_hdr(const route_head *rte)
{
  (void)rte;
  course.clear();
}

static void
fit_collect_trackpt(const Waypoint* waypointp)
{
  course.push_back(FitCourseRecordPoint(*waypointp, false));
}

static void
fit_collect_track_tlr(const route_head *rte)
{
  // Prepare for writing a course corresponding to a track.
  // For this, we need to check for/synthesize missing information
  // and convert waypoints to coursepoints (i.e. insert them at the right
  // place between course records).

  // Recalculate odometer_distance for the whole track unless already
  // (properly, i.e. monotonically increasing) set
  double dist_sum = 0;
  double prev_lat = 999, prev_lon = 999;
  double max_speed = 0;
  gpsbabel::DateTime prev_time;
  for (auto &crpt: course) {
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
    auto &wpt = waypoints.front();
    double best_distance = -1;
    auto best_distance_it = course.begin();
    double best_odometer_distance = 0;
    for (auto cit = course.begin(); cit != course.end(); cit++) {
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
  gpsbabel::DateTime track_date_time, track_end_date_time, creation_time;
  double first_lat = 999, first_lon = 999, last_lat = 999, last_lon = 999;
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
  for (auto &crpt: course) {
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

static void
fit_collect_waypt(const Waypoint* waypointp)
{
  FitCourseRecordPoint crpt(*waypointp, true);

  // Try to find a better course point type than "generic", based on the
  // course point name
  for (auto &cptm: kCoursePointTypeMapping) {
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
static void
fit_write()
{
  fit_write_file_header(0, 0);
  write_header_msgs = true;
  waypt_disp_all(fit_collect_waypt);
  track_disp_all(fit_collect_track_hdr, fit_collect_track_tlr, fit_collect_trackpt);
  fit_write_file_finish();
}

/**************************************************************************/

// capabilities below means: we can only read and write waypoints
// please change this depending on your new module

ff_vecs_t format_fit_vecs = {
  ff_type_file,
  {
    ff_cap_write				/* waypoints */,
    (ff_cap)(ff_cap_read | ff_cap_write)	/* tracks */,
    ff_cap_none 				/* routes */
  },
  fit_rd_init,
  fit_wr_init,
  fit_rd_deinit,
  fit_wr_deinit,
  fit_read,
  fit_write,
  nullptr,
  &fit_args,
  CET_CHARSET_ASCII, 0		/* ascii is the expected character set */
  /* not fixed, can be changed through command line parameter */
  , NULL_POS_OPS,
  nullptr
};
/**************************************************************************/
