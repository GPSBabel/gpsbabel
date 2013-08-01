/*

    Support for FIT track files.

    Copyright (C) 2011 Paul Brook, paul@nowt.org
    Copyright (C) 2003-2011  Robert Lipe, robertlipe@gpsbabel.org

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
#include <ctype.h>

#define MYNAME "fit"


static
arglist_t fit_args[] = {
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
  fit_message_def message_def[16];
} fit_data;

static	gbfile* fin;

/*******************************************************************************
* %%%        global callbacks called by gpsbabel main process              %%% *
*******************************************************************************/

static void
fit_rd_init(const char* fname)
{
  fin = gbfopen_le(fname, "rb", MYNAME);
}

static void
fit_rd_deinit(void)
{
  int local_id;

  for (local_id=0; local_id<16; local_id++) {
    fit_message_def* def = &fit_data.message_def[local_id];
    if (def->fields) {
      xfree(def->fields);
    }
  }

  gbfclose(fin);
}


/*******************************************************************************
* fit_parse_header- parse the global FIT header
*******************************************************************************/
static void
fit_parse_header(void)
{
  int len;
  int ver;
  char sig[4];

  len = gbfgetc(fin);
  if (len == EOF || len < 12) {
    fatal(MYNAME ": Bad header\n");
  }
  if (global_opts.debug_level >= 1) {
    debug_print(1,"%s: header len=%d\n", MYNAME, len);
  }

  ver = gbfgetc(fin);
  if (ver == EOF || (ver >> 4) > 1)
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
}

static uint8_t
fit_getuint8(void)
{
  int val;

  if (fit_data.len == 0) {
    // fail gracefully for GARMIN Edge 800 with newest firmware, seems to write a wrong record length
    // for the last record.
    //fatal(MYNAME ": record truncated: fit_data.len=0\n");
    if (global_opts.debug_level >= 1) {
      warning("%s: record truncated: fit_data.len=0\n", MYNAME);
    }
    return 0;
  }
  val = gbfgetc(fin);
  if (val == EOF) {
    fatal(MYNAME ": unexpected end of file with fit_data.len=%d\n",fit_data.len);
  }
  fit_data.len--;
  return (uint8_t)val;

}

static uint16_t
fit_getuint16(void)
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
fit_getuint32(void)
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
  int i;

  if (def->fields) {
    xfree(def->fields);
  }

  // first byte is reserved.  It's usually 0 and we don't know what it is,
  // but we've seen some files that are 0x40.  So we just read it and toss it.
  i = fit_getuint8();

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
    return;
  }

  // remainder of the definition message is data at one byte per field * 3 fields
  def->fields = (fit_field_t*) xmalloc(def->num_fields * sizeof(fit_field_t));
  for (i = 0; i < def->num_fields; i++) {
    def->fields[i].id = fit_getuint8();
    def->fields[i].size = fit_getuint8();
    def->fields[i].type = fit_getuint8();
    if (global_opts.debug_level >= 8) {
      debug_print(8,"%s: record %d  ID: %d  SIZE: %d  TYPE: %d  fit_data.len=%d\n",
                  MYNAME, i, def->fields[i].id, def->fields[i].size, def->fields[i].type,fit_data.len);
    }
  }
}

static uint32_t
fit_read_field(fit_field_t* f)
{
  int i;

  if (global_opts.debug_level >= 8) {
    debug_print(8,"%s: fit_read_field: read data field with f->type=0x%X and f->size=%d fit_data.len=%d\n",
                MYNAME, f->type, f->size, fit_data.len);
  }
  switch (f->type) {
  case 1: // sint8
  case 2: // uint8
    is_fatal(f->size != 1,
             MYNAME ": Bad field size in data message\n");
    return fit_getuint8();
  case 0x83: // sint16
  case 0x84: // uint16
    is_fatal(f->size != 2,
             MYNAME ": Bad field size in data message\n");
    return fit_getuint16();
  case 0x85: // sint32
  case 0x86: // uint32
    is_fatal(f->size != 4,
             MYNAME ": Bad field size in data message\n");
    return fit_getuint32();
  default: // Ignore everything else for now.
    for (i = 0; i < f->size; i++) {
      fit_getuint8();
    }
    return -1;
  }
}

static void
fit_parse_data(fit_message_def* def, int time_offset)
{
  fit_field_t* f;
  uint32_t timestamp = fit_data.last_timestamp + time_offset;
  uint32_t val;
  int32_t lat = 0x7fffffff;
  int32_t lon = 0x7fffffff;
  uint16_t alt = 0xffff;
  uint16_t speed = 0xffff;
  uint8_t heartrate = 0xff;
  uint8_t cadence = 0xff;
  uint16_t power = 0xffff;
  int8_t temperature = 0x7f;
  int i;
  waypoint* waypt;

  if (global_opts.debug_level >= 7) {
    debug_print(7,"%s: parsing fit data ID %d with num_fields=%d\n", MYNAME, def->global_id, def->num_fields);
  }
  for (i = 0; i < def->num_fields; i++) {
    if (global_opts.debug_level >= 7) {
      debug_print(7,"%s: parsing field %d\n", MYNAME, i);
    }
    f = &def->fields[i];
    val = fit_read_field(f);
    if (f->id == 253) {
      if (global_opts.debug_level >= 7) {
        debug_print(7,"%s: parsing fit data: timestamp=%d\n", MYNAME, val);
      }
      fit_data.last_timestamp = timestamp = val;
    } else {
      switch (def->global_id) {
      case 20: // record message
        switch (f->id) {
        case 0:
          if (global_opts.debug_level >= 7) {
            debug_print(7,"%s: parsing fit data: lat=%d\n", MYNAME, val);
          }
          lat = val;
          break;
        case 1:
          if (global_opts.debug_level >= 7) {
            debug_print(7,"%s: parsing fit data: lon=%d\n", MYNAME, val);
          }
          lon = val;
          break;
        case 2:
          if (global_opts.debug_level >= 7) {
            debug_print(7,"%s: parsing fit data: alt=%d\n", MYNAME, val);
          }
          alt = val;
          break;
        case 3:
          if (global_opts.debug_level >= 7) {
            debug_print(7,"%s: parsing fit data: heartrate=%d\n", MYNAME, val);
          }
          heartrate = val;
          break;
        case 4:
          if (global_opts.debug_level >= 7) {
            debug_print(7,"%s: parsing fit data: cadence=%d\n", MYNAME, val);
          }
          cadence = val;
          break;
        case 5:
          // NOTE: 5 is DISTANCE in cm ... unused.
          if (global_opts.debug_level >= 7) {
            debug_print(7, "%s: unrecognized data type in GARMIN FIT record: f->id=%d\n", MYNAME, f->id);
          }
          break;
        case 6:
          if (global_opts.debug_level >= 7) {
            debug_print(7,"%s: parsing fit data: speed=%d\n", MYNAME, val);
          }
          speed = val;
          break;
        case 7:
          if (global_opts.debug_level >= 7) {
            debug_print(7,"%s: parsing fit data: power=%d\n", MYNAME, val);
          }
          power = val;
          break;
        case 13:
          if (global_opts.debug_level >= 7) {
            debug_print(7,"%s: parsing fit data: temperature=%d\n", MYNAME, val);
          }
          temperature = val;
          break;
        default:
          if (global_opts.debug_level >= 1) {
            debug_print(1, "%s: unrecognized data type in GARMIN FIT record: f->id=%d\n", MYNAME, f->id);
          }
          break;
        }
      }
    }
  }

  if (global_opts.debug_level >= 7) {
    debug_print(7,"%s: storing fit data with num_fields=%d\n", MYNAME, def->num_fields);
  }
  switch (def->global_id) {
  case 20: // record message
    if (lat == 0x7fffffff || lon == 0x7fffffff) {
      break;
    }

    waypt = waypt_new();
    waypt->latitude = (lat / (double)0x7fffffff) * 180;
    waypt->longitude = (lon / (double)0x7fffffff) * 180;
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
    track_add_wpt(fit_data.track, waypt);
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
fit_parse_record(void)
{
  uint8_t header;

  header = fit_getuint8();
  // high bit 7 set -> compressed message (0 for normal)
  // second bit 6 set -> 0 for data message, 1 for definition message
  // bits 5, 4 -> reserved
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
fit_read(void)
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
  NULL,
  fit_rd_deinit,
  NULL,
  fit_read,
  NULL,
  NULL,
  fit_args,
  CET_CHARSET_ASCII, 0		/* ascii is the expected character set */
  /* not fixed, can be changed through command line parameter */
};
/**************************************************************************/
