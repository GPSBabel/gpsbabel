/*

    Support for FIT track files.

    Copyright (C) 2011 Paul Brook, paul@nowt.org
    Copyright (C) 2003-2011  Robert Lipe, robertlipe+source@gpsbabel.org

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
#include <cstdio>

#define MYNAME "fit"

// constants for global IDs
const int kIdDeviceSettings = 0;
const int kIdLap = 19;
const int kIdRecord = 20;
const int kIdEvent = 21;

// constants for message fields
// for all global IDs
const int kFieldTimestamp = 253;
// for global ID: device settings
const int kFieldGlobalUtcOffset = 4;
// for global ID: lap
const int kFieldStartTime = 2;
const int kFieldStartLatitude = 3;
const int kFieldStartLongitude = 4;
const int kFieldEndLatitude = 5;
const int kFieldEndLongitude = 6;
const int kFieldElapsedTime = 7;
const int kFieldTotalDistance = 9;
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

// For developer fields as a non conflicting id
const int kFieldInvalid = 255;

static char* opt_allpoints = nullptr;
static int lap_ct = 0;
static bool new_trkseg = false;

static
arglist_t fit_args[] = {
  {
    "allpoints", &opt_allpoints,
    "Read all points even if latitude or longitude is missing",
    nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
  },
  ARG_TERMINATOR
};

typedef struct {
  int id;
  int size;
  int type;
} fit_field_t;

typedef struct {
  int endian;
  int global_id;
  int num_fields;
  fit_field_t* fields;
} fit_message_def;

static struct {
  int len;
  int endian;
  route_head* track;
  uint32_t last_timestamp;
  uint32_t global_utc_offset;
  fit_message_def message_def[16];
} fit_data;

static	gbfile* fin;

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
    fatal(MYNAME ": Bad endian field\n");
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
    lappt->latitude = (endlat / (double)0x7fffffff) * 180;
    lappt->longitude = (endlon / (double)0x7fffffff) * 180;
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
      waypt->latitude = (lat / (double)0x7fffffff) * 180;
    }
    if (lon != 0x7fffffff) {
      waypt->longitude = (lon / (double)0x7fffffff) * 180;
    }
    if (alt != 0xffff) {
      waypt->altitude = (alt / 5.0) - 500;
    }
    waypt->SetCreationTime(QDateTime::fromTime_t(timestamp + 631065600));
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
  int local_id = header & 0x1f;
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

/**************************************************************************/

// capabilities below means: we can only read and write waypoints
// please change this depending on your new module

ff_vecs_t format_fit_vecs = {
  ff_type_file,
  {
    ff_cap_none			/* waypoints */,
    ff_cap_read 		/* tracks */,
    ff_cap_none 		/* routes */
  },
  fit_rd_init,
  nullptr,
  fit_rd_deinit,
  nullptr,
  fit_read,
  nullptr,
  nullptr,
  fit_args,
  CET_CHARSET_ASCII, 0		/* ascii is the expected character set */
  /* not fixed, can be changed through command line parameter */
  , NULL_POS_OPS,
  nullptr
};
/**************************************************************************/
