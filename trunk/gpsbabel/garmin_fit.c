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
  gbuint32 last_timestamp;
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
  gbfclose(fin);
}

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
  ver = gbfgetc(fin);
  if (ver == EOF || (ver >> 4) > 1)
    fatal(MYNAME ": Unsupported protocol version %d.%d\n",
          ver >> 4, ver & 0xf);
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

  // Read in rest of header (if any)
  len -= 12;
  while( len-- ) {
    gbfgetc(fin); // throw away unknown header data
  }
}

static gbuint8
fit_getuint8(void)
{
  int val;

  if (fit_data.len == 0) {
    fatal(MYNAME ": record truncated\n");
  }
  val = gbfgetc(fin);
  if (val == EOF) {
    fatal(MYNAME ": unexpected end of file\n");
  }
  fit_data.len--;
  return (gbuint8)val;

}

static gbuint16
fit_getuint16(void)
{
  char buf[2];

  if (fit_data.len < 2) {
    fatal(MYNAME ": record truncated\n");
  }
  is_fatal(gbfread(buf, 2, 1, fin) != 1,
           MYNAME ": unexpected end of file\n");
  fit_data.len -= 2;
  if (fit_data.endian) {
    return be_read16(buf);
  } else {
    return le_read16(buf);
  }

}

static gbuint32
fit_getuint32(void)
{
  char buf[4];

  if (fit_data.len < 4) {
    fatal(MYNAME ": record truncated\n");
  }
  is_fatal(gbfread(buf, 4, 1, fin) != 1,
           MYNAME ": unexpected end of file\n");
  fit_data.len -= 4;
  if (fit_data.endian) {
    return be_read32(buf);
  } else {
    return le_read32(buf);
  }

}

static void
fit_parse_definition_message(gbuint8 header)
{
  int local_id = header & 0x1f;
  fit_message_def* def = &fit_data.message_def[local_id];
  int i;

  if (def->fields) {
    free(def->fields);
  }

  is_fatal(fit_getuint8() != 0,
           MYNAME ": Definition message reserved bits not zero\n");
  def->endian = fit_getuint8();
  if (def->endian > 1) {
    fatal(MYNAME ": Bad endian field\n");
  }
  fit_data.endian = def->endian;
  def->global_id = fit_getuint16();
  def->num_fields = fit_getuint8();
  if (def->num_fields == 0) {
    def->fields = (fit_field_t*) xmalloc(sizeof(fit_field_t));
    return;
  }
  def->fields = (fit_field_t*) xmalloc(def->num_fields * sizeof(fit_field_t));
  for (i = 0; i < def->num_fields; i++) {
    def->fields[i].id = fit_getuint8();
    def->fields[i].size = fit_getuint8();
    def->fields[i].type = fit_getuint8();
  }
}

static gbuint32
fit_read_field(fit_field_t* f)
{
  int i;

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
  gbuint32 timestamp = fit_data.last_timestamp + time_offset;
  gbuint32 val;
  gbint32 lat = 0x7fffffff;
  gbint32 lon = 0x7fffffff;
  gbuint16 alt = 0xffff;
  gbuint16 speed = 0xffff;
  gbuint8 heartrate = 0xff;
  gbuint8 cadence = 0xff;
  gbuint16 power = 0xffff;
  gbint8 temperature = 0x7f;
  int i;
  waypoint* waypt;

  for (i = 0; i < def->num_fields; i++) {
    f = &def->fields[i];
    val = fit_read_field(f);
    if (f->id == 253) {
      fit_data.last_timestamp = timestamp = val;
    } else {
      switch (def->global_id) {
      case 20: // record mesage
        switch (f->id) {
        case 0:
          lat = val;
          break;
        case 1:
          lon = val;
          break;
        case 2:
          alt = val;
          break;
        case 3:
          heartrate = val;
          break;
        case 4:
          cadence = val;
          break;
        case 6:
          speed = val;
          break;
        case 7:
          power = val;
          break;
        case 13:
          temperature = val;
          break;
        }
      }
    }
  }
  switch (def->global_id) {
  case 20: // record mesage
    if (lat == 0x7fffffff || lon == 0x7fffffff) {
      break;
    }

    waypt = waypt_new();
    waypt->latitude = (lat / (float)0x7fffffff) * 180;
    waypt->longitude = (lon / (float)0x7fffffff) * 180;
    if (alt != 0xffff) {
      waypt->altitude = (alt / 5.0) - 500;
    }
    waypt->creation_time = timestamp + 631065600;
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
fit_parse_data_message(gbuint8 header)
{
  int local_id = header & 0x1f;
  fit_message_def* def = &fit_data.message_def[local_id];
  fit_parse_data(def, 0);
}

static void
fit_parse_compressed_message(gbuint8 header)
{
  int local_id = (header >> 5) & 3;
  fit_message_def* def = &fit_data.message_def[local_id];
  fit_parse_data(def, header & 0x1f);
}

static void
fit_parse_record(void)
{
  gbuint8 header;

  header = fit_getuint8();
  if (header & 0x80) {
    fit_parse_compressed_message(header);
  } else if (header & 0x40) {
    fit_parse_definition_message(header);
  } else {
    fit_parse_data_message(header);
  }
}

static void
fit_read(void)
{
  fit_parse_header();

  fit_data.track = route_head_alloc();
  track_add_head(fit_data.track);
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
