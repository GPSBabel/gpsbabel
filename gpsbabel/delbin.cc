/*
	DeLorme PN-20/40 USB "DeLBin" protocol

    Copyright (C) 2009 Paul Cornett, pc-gpsb at bullseye.com
    Copyright (C) 2005, 2009  Robert Lipe, robertlipe@gpsbabel.org

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
#include <assert.h>

#define MYNAME "delbin"
static short_handle mkshort_handle;

/*
Device documentation:
"DeLorme Binary GPS Format", delbin_user_interface_format_176.pdf
obtained here: http://forum.delorme.com/viewtopic.php?t=13846

Notes:
Initial development was done with a PN-40, firmware 2.4.123299. The test
device was upgraded to firmware 2.5.165506 during development.

The "data size" in the message header includes the 4 trailer bytes, so it
is really the size of the whole message minus the header.

Messages do not always start at the beginning of a packet. Every once in a
while, the start of the next message directly follows the end of the previous
one, in the same packet.

The time before an unacknowledged message will be retransmitted by the
device is on the order of 2 to 4 seconds.

Retrieving all tracks at once (using code 0 in message 0xb031) does not
seem to work, it hangs after the first track, maybe waiting for some
undocumented response message.

Character encoding is not documented, appears to be 8859-1.

The undocumented messages 0xaa01, 0xb015, 0xb016 and the use of the
"reserved" byte in message 0xb012 were discovered by examining the data
transferred between the device and DeLorme Topo 8.0. They may have been
added in the PN-40 2.5 firmware.
*/

//-----------------------------------------------------------------------------
// interface to platform-specific device I/O
typedef struct {
  void (*init)(const char* name);
  void (*deinit)(void);
  unsigned(*packet_read)(void*);
  unsigned(*packet_write)(const void*, unsigned);
} delbin_os_ops_t;

// really static, only extern so it can be forward declared
extern delbin_os_ops_t delbin_os_ops;

static unsigned delbin_os_packet_size;
//-----------------------------------------------------------------------------

// number of times to attempt a transfer before giving up
#define ATTEMPT_MAX 2
// seconds to wait for expected message (actual time will be somewhat
// indeterminate, but at least READ_TIMEOUT - 1)
#define READ_TIMEOUT 6

// debug output: low, medium, high, higher
#define DBGLVL_L 1
#define DBGLVL_M 2
#define DBGLVL_H 3
#define DBGLVL_H2 4

// Multiple unit support.
#define DELBIN_MAX_UNITS 32
static struct {
  unsigned int unit_number;
  const char* unit_serial_number;
  const char* unit_name;
} delbin_unit_info[DELBIN_MAX_UNITS];
static int n_delbin_units;

#define UNKNOWN_ELEV -2000000

#define sizeofarray(x) (sizeof(x) / sizeof(x[0]))

static char* opt_getposn = NULL;
static char* opt_logs = NULL;
static char* opt_long_notes = NULL;
static char* opt_nuke_wpt = NULL;
static char* opt_nuke_trk = NULL;
static char* opt_nuke_rte = NULL;
/* If true, Order hint to match Cache Register and Topo 7 */
static char* opt_hint_at_end = NULL;
static char* opt_gcsym = NULL;


static arglist_t delbin_args[] = {
  {
    "get_posn", &opt_getposn, "Return current position as a waypoint",
    NULL, ARGTYPE_BOOL, ARG_NOMINMAX
  },
  {
    "logs", &opt_logs, "Include groundspeak logs when writing",
    NULL, ARGTYPE_BOOL, ARG_NOMINMAX
  },
  {
    "long_notes", &opt_long_notes, "Use long waypoint notes regardless of PN version",
    NULL, ARGTYPE_BOOL, ARG_NOMINMAX
  },
  {
    "nukewpt", &opt_nuke_wpt, "Delete all waypoints before sending", NULL, ARGTYPE_BOOL,
    ARG_NOMINMAX
  },
  {
    "nuketrk", &opt_nuke_trk, "Delete all tracks before sending", NULL, ARGTYPE_BOOL,
    ARG_NOMINMAX
  },
  {
    "nukerte", &opt_nuke_rte, "Delete all routes before sending", NULL, ARGTYPE_BOOL,
    ARG_NOMINMAX
  },
  {"hint_at_end", &opt_hint_at_end, "If true, geocache hint at end of text", NULL, ARGTYPE_BOOL, ARG_NOMINMAX },
  {"gcsym", &opt_gcsym, "If set to 0, prefer user-provided symbols over Groundspeaks ones for geocaches", NULL, ARGTYPE_BOOL, ARG_NOMINMAX, (char *) "1" },
  ARG_TERMINATOR
};

// Whether device understands message 0xb016
static int use_extended_notes;

// Device capabilities
static unsigned device_max_waypoint;

static const char* waypoint_symbol(unsigned index);
static unsigned waypoint_symbol_index(const char* name);
static int track_color(unsigned index);
static unsigned track_color_index(int bgr);

static unsigned waypoint_i;
static unsigned waypoint_n;
static waypoint** wp_array;

//-----------------------------------------------------------------------------
// Message ids and sizes. Only the needed ones are here.
// Note that "in" and "out" ids are named as in the device documentation,
// so "in" means to the device, "out" means from.
#define MSG_ACK 0xaa00
#define MSG_BREAK 0xaa02
#define MSG_BREAK_SIZE 33
#define MSG_CAPABILITIES 0xb001
#define MSG_DELETE 0xb005
#define MSG_DELETE_SIZE 67
#define MSG_ERROR 0xa003
#define MSG_NAVIGATION 0xa010
#define MSG_REQUEST_ROUTES 0xb051
#define MSG_REQUEST_ROUTES_SIZE 65
#define MSG_REQUEST_TRACKS 0xb031
#define MSG_REQUEST_TRACKS_SIZE 33
#define MSG_REQUEST_WAYPOINTS 0xb012
#define MSG_REQUEST_WAYPOINTS_SIZE 15
#define MSG_ROUTE_COUNT 0xb050
#define MSG_ROUTE_HEADER_IN 0xb055
#define MSG_ROUTE_HEADER_OUT 0xb052
#define MSG_ROUTE_POINT_IN 0xb056
#define MSG_ROUTE_POINT_OUT 0xb053
#define MSG_ROUTE_SHAPE_IN 0xb057
#define MSG_ROUTE_SHAPE_OUT 0xb054
#define MSG_SATELLITE_INFO 0xa020
#define MSG_TRACK_COUNT 0xb030
#define MSG_TRACK_HEADER_IN 0xb035
#define MSG_TRACK_HEADER_OUT 0xb032
#define MSG_TRACK_POINT_IN 0xb036
#define MSG_TRACK_POINT_OUT 0xb033
#define MSG_TRANSFER_COMPLETE 0xaa04
#define MSG_VERSION 0xa001
#define MSG_WAYPOINT_COUNT 0xb010
#define MSG_WAYPOINT_IN 0xb014
#define MSG_WAYPOINT_OUT 0xb013
// Undocumented:
// This one looks like MSG_ACK, except it also has a string in it that says
// something like "device is busy". The expected MSG_ACK usually immediately
// follows it, so the point of this one is unclear.
#define MSG_NACK 0xaa01
// Long waypoint notes
#define MSG_WAYPOINT_NOTE_IN 0xb016
#define MSG_WAYPOINT_NOTE_OUT 0xb015

//-----------------------------------------------------------------------------
// Message structures

// Input Delete Message
// Message ID: 0xB005
typedef enum {
  nuke_type_wpt = 0,
  nuke_type_trk = 1,
  nuke_type_rte = 2,
  // int nuke_map = 3;
} nuke_type;

typedef enum {
  nuke_mode_all = 0,
  nuke_mode_single = 1
} nuke_mode;

typedef enum {
  nuke_dest_internal = 0,
  nuke_dest_sd = 1
} nuke_dest;

typedef struct {
  gbuint8 type;
  gbuint8 mode;
  gbuint8 location;
  char object_name[64];
} msg_delete_t;

// Output Waypoint Message
// Message ID: 0xB013
// Input Waypoint Message
// Message ID: 0xB014
typedef struct {
  gbuint8 total[4]; // U32
  gbuint8 index[4]; // U32
  gbuint8 year;
  gbuint8 month;
  gbuint8 day;
  gbuint8 hour;
  gbuint8 minute;
  gbuint8 second;
  gbuint8 latitude[4]; // S32 rad * 100000000
  gbuint8 longitude[4]; // S32 rad * 100000000
  gbuint8 elevation[4]; // F32 meters
  gbuint8 color;
  gbuint8 symbol;
  gbuint8 name_size;
  char name[1];
  // note_size[2] U16
  // note[note_size]
} msg_waypoint_t;

// undocumented, seen with PN-40 2.5 firmware
// output waypoint note
// Message ID: 0xB015
// input waypoint note
// Message ID: 0xB016
typedef struct {
  gbuint8 index[2];
  gbuint8 total[2];
  gbuint8 name_size;
  char name[1];
  // note_size[2]
  // note[note_size]
} msg_waypoint_note_t;

// Output Track Point Message
// Message ID: 0xB033
// Input Track Point Message
// Message ID: 0xB036
typedef struct {
  gbuint8 total[4]; // U32
  gbuint8 index[4]; // U32
  gbuint8 number;
  struct {
    gbuint8 year;
    gbuint8 month;
    gbuint8 day;
    gbuint8 hour;
    gbuint8 minute;
    gbuint8 second;
    gbuint8 latitude[4]; // S32 rad * 100000000
    gbuint8 longitude[4]; // S32 rad * 100000000
    gbuint8 elevation[4]; // F32 meters
    gbuint8 speed[2]; // U16 km/h * 10
    gbuint8 heading[2]; // U16 deg * 100
    gbuint8 status;
  } point[1];
} msg_track_point_t;

// Output Track Header (Name) Message
// Message ID: 0xB032
typedef struct {
  gbuint8 total_tracks[2]; // U16
  gbuint8 number[2]; // U16
  char name[32];
  gbuint8 total_points[4]; // U32
  gbuint8 year;
  gbuint8 month;
  gbuint8 day;
  gbuint8 hour;
  gbuint8 minute;
  gbuint8 second;
  gbuint8 color[2]; // U16
  gbuint8 distance[4]; // U32 m
  gbuint8 duration[4]; // U32 sec
  gbuint8 comment_size[2]; // U16
  char comment[1];
} msg_track_header_t;

// Input Upload Track Header Message
// Message ID: 0xB035
typedef struct {
  char name[32];
  gbuint8 total_points[4]; // U32
  gbuint8 year;
  gbuint8 month;
  gbuint8 day;
  gbuint8 hour;
  gbuint8 minute;
  gbuint8 second;
  gbuint8 color[2]; // U16
  gbuint8 comment_size[2]; // U16
  char comment[1];
} msg_track_header_in_t;

// Output Route Shape Message
// Message ID: 0xB054
typedef struct {
  gbuint8 total[4]; // U32
  gbuint8 index[4]; // U32
  gbuint8 number;
  gbuint8 reserved;
  struct {
    gbuint8 latitude[4]; // S32 rad * 100000000
    gbuint8 longitude[4]; // S32 rad * 100000000
  } point[1];
} msg_route_shape_t;

// Output Route Point Message
// Message ID: 0xB053
// Input Route Itin Point Message
// Message ID: 0xB056
typedef struct {
  gbuint8 total[4]; // U32
  gbuint8 index[4]; // U32
  char name[32];
  gbuint8 latitude[4]; // S32 rad * 100000000
  gbuint8 longitude[4]; // S32 rad * 100000000
  gbuint8 time_from_start[4]; // U32 sec
  gbuint8 distance_from_start[4]; // F32 km
  gbuint8 bearing_in[2]; // U16 deg * 100
  gbuint8 bearing_out[2]; // U16 deg * 100
  gbuint8 bearing_next[2]; // U16 deg * 100
  gbuint8 itinerary_type;
  gbuint8 turn_type;
  gbuint8 road_class[2]; // U16
  gbuint8 feature_code[4]; // U32
  gbuint8 exit_label_size;
  char exit_label[1];
  // comment_size U8
  // comment[comment_size]
  // shape_pt_count U32
} msg_route_point_t;

// Output Route Header (Name) Message
// Message ID: 0xB052
typedef struct {
  gbuint8 total[2]; // U16
  gbuint8 index[2]; // U16
  char name[64];
  gbuint8 type;
  gbuint8 total_route_point[4]; // U32
  gbuint8 total_shape_point[4]; // U32
} msg_route_header_t;

// Input Upload Route Header Message
// Message ID: 0xB055
typedef struct {
  char name[64];
  gbuint8 type;
  gbuint8 total_route_point[4]; // U32
  gbuint8 total_shape_point[4]; // U32
} msg_route_header_in_t;

// Output Navigation Message
// Message ID: 0xA010
typedef struct {
  gbuint8 gps_week[2]; // U16
  gbuint8 time_of_week[8]; // D64 sec
  gbuint8 year[2]; // U16
  gbuint8 month;
  gbuint8 day;
  gbuint8 hour;
  gbuint8 minute;
  gbuint8 second;
  gbuint8 satellites;
  gbuint8 latitude[8]; // D64 deg
  gbuint8 longitude[8]; // D64 deg
  gbuint8 elevation[8]; // D64 meters
  gbuint8 geoid_offset[2]; // S16 meters * 10
  gbuint8 speed[4]; // F32 km/h
  gbuint8 heading[2]; // U16 deg * 100
  gbuint8 magnetic_variation[2]; // S16 deg * 100
  gbuint8 fix_status;
} msg_navigation_t;

// Output Satellite Info Message
// Message ID: 0xA020
typedef struct {
  gbuint8 gps_week[2]; // U16
  gbuint8 time_of_week[8]; // D64 sec
  gbuint8 hdop[2]; // U16
  gbuint8 vdop[2]; // U16
  gbuint8 pdop[2]; // U16
  gbuint8 number;
  struct {
    gbuint8 prn;
    gbuint8 azimuth[2]; // S16 deg? * 100
    gbuint8 elevation[2]; // S16 deg? * 100
    gbuint8 Cn0[2]; // U16 snr * 100
    gbuint8 status;
  } sat[1];
} msg_satellite_t;

// Output Version Message
// Message ID: 0xA001
typedef struct {
  gbuint8 firmware_version[4];
  char company[32];
  char product[32];
  char firmware[32];
  char gps_firmware[48];
  char serial[16];
  char extra[16];
} msg_version_t;

// Output Device Capabilities Message
// Message ID: 0xB001
typedef struct {
  gbuint8 max_waypoints[4]; // U32
  gbuint8 max_tracks[2]; // U16
  gbuint8 max_track_points[4]; // U32
  gbuint8 max_routes[2]; // U16
  gbuint8 max_route_points[4]; // U32
  gbuint8 max_route_shape_points[4]; // U32
  gbuint8 max_maps[2]; // U16
  gbuint8 min_map_version[2]; // U16
  gbuint8 max_map_version[2]; // U16
  gbuint8 total_internal_file_memory[4]; // U32
  gbuint8 avail_internal_file_memory[4]; // U32
  gbuint8 total_external_file_memory[4]; // U32
  gbuint8 avail_external_file_memory[4]; // U32
} msg_capabilities_t;

//-----------------------------------------------------------------------------

#if __APPLE__ || __linux
#include <sys/time.h>
#endif

static void
debug_out(const char* fmt, ...)
{
  va_list ap;
  va_start(ap, fmt);
  fputs(MYNAME ": ", stderr);
  vfprintf(stderr, fmt, ap);
  va_end(ap);
}

static void
debug_out_time(const char* s)
{
#if __APPLE__ || __linux
  struct timeval tv;
  gettimeofday(&tv, NULL);
  debug_out("%u.%03u %s", (unsigned)tv.tv_sec & 0xf, (unsigned)tv.tv_usec / 1000, s);
#else
  debug_out("%u %s", (unsigned)time(NULL) & 0xf, s);
#endif
}

//-----------------------------------------------------------------------------

static gbuint16
checksum(const gbuint8* p, unsigned n)
{
  int x = 0;
  unsigned i;
  for (i = n / 2; i > 0; i--) {
    x += *p++;
    x += *p++ << 8;
  }
  if (n & 1) {
    x += *p;
  }
  return (gbuint16)-x;
}

//-----------------------------------------------------------------------------
// OS packet read/write wrappers

static unsigned
packet_read(void* buf)
{
  unsigned n = delbin_os_ops.packet_read(buf);
  if (n == 0) {
    fatal(MYNAME ": read 0\n");
  }
  if (global_opts.debug_level >= DBGLVL_H) {
    unsigned j;
    const gbuint8* p = (const gbuint8*) buf;

    debug_out_time("pcktrd");
    for (j = 0; j < n; j++) {
      warning(" %02x", p[j]);
    }
    if (global_opts.debug_level >= DBGLVL_H2) {
      warning("  ");
      for (j = 0; j < n; j++) {
        int c = p[j];
        warning("%c", isprint(c) ? c : '.');
      }
    }
    warning("\n");
  }
  return n;
}

static void
packet_write(const void* buf, unsigned size)
{
  unsigned n;
  if (global_opts.debug_level >= DBGLVL_H) {
    unsigned j;
    const gbuint8* p = (const gbuint8*) buf;

    debug_out_time("pcktwr");
    for (j = 0; j < size; j++) {
      warning(" %02x", p[j]);
    }
    if (global_opts.debug_level >= DBGLVL_H2) {
      warning("  ");
      for (j = 0; j < size; j++) {
        int c = p[j];
        warning("%c", isprint(c) ? c : '.');
      }
    }
    warning("\n");
  }
  n = delbin_os_ops.packet_write(buf, size);
  if (n != size) {
    fatal(MYNAME ": short write %u %u\n", size, n);
  }
}

//-----------------------------------------------------------------------------

// dynamically sized buffer with space reserved for message header and trailer
typedef struct {
  // message data size
  unsigned size;
  // buffer size
  unsigned capacity;
  gbuint8* buf;
  // convenience pointer to message data area
  void* data;
} message_t;

static void
message_init(message_t* m)
{
  m->capacity = 100;
  m->buf = (gbuint8*)xmalloc(m->capacity);
  m->data = m->buf + 2 + 8;
}

static void
message_init_size(message_t* m, unsigned size)
{
  m->size = size;
  m->capacity = 2 + 8 + size + 4;
  m->buf = (gbuint8*)xmalloc(m->capacity);
  m->data = m->buf + 2 + 8;
}

static void
message_free(message_t* m)
{
  xfree(m->buf);
  m->buf = NULL;
  m->data = NULL;
}

static void
message_ensure_size(message_t* m, unsigned size)
{
  m->size = size;
  if (m->capacity < 2 + 8 + size + 4) {
    m->capacity = 2 + 8 + size + 4;
    xfree(m->buf);
    m->buf = (gbuint8*)xmalloc(m->capacity);
    m->data = m->buf + 2 + 8;
  }
}

static unsigned
message_get_id(const message_t* m)
{
  return le_readu16(m->buf + 4);
}

//-----------------------------------------------------------------------------

static void
message_write(unsigned msg_id, message_t* m)
{
  unsigned chksum;
  unsigned count;
  unsigned n;
  gbuint8* p = m->buf;

  // header (2 start bytes filled in later)
  p[2] = 0xdb;
  p[3] = 0xfe;
  le_write16(p + 4, msg_id);
  // "data size" includes 4 trailer bytes
  le_write16(p + 6, m->size + 4);
  chksum = checksum(p + 2, 6);
  le_write16(p + 8, chksum);
  // message data (filled in by caller)
  chksum = checksum((gbuint8*) m->data, m->size);
  n = 2 + 8 + m->size;
  // trailer (checksum and marker bytes)
  le_write16(p + n, chksum);
  p[n + 2] = 0xad;
  p[n + 3] = 0xbc;
  // size of message not counting packet start bytes
  count = 8 + m->size + 4;
  do {
    const gbuint8 save0 = p[0];
    const gbuint8 save1 = p[1];
    n = delbin_os_packet_size - 2;
    if (n > count) {
      n = count;
    }
    // doc. says 0x20, device sends 0, probably ignored
    p[0] = 0x20;
    // valid bytes in packet after first 2
    p[1] = n;
    packet_write(p, 2 + n);
    p[0] = save0;
    p[1] = save1;
    p += n;
    count -= n;
  } while (count != 0);
  if (global_opts.debug_level >= DBGLVL_M) {
    warning(MYNAME ": sent %x\n", msg_id);
  }
}

// read from the payload of a single packet
static unsigned
read_depacketize_1(gbuint8** p, unsigned n, int new_packet)
{
  static gbuint8 buf[256];
  static unsigned buf_i, buf_n;
  if (new_packet) {
    buf_n = 0;
  }
  while (buf_n == 0) {
    packet_read(buf);
    if (buf[1] <= delbin_os_packet_size - 2) {
      buf_n = buf[1];
      buf_i = 2;
    }
  }
  *p = buf + buf_i;
  if (n > buf_n) {
    n = buf_n;
  }
  buf_n -= n;
  buf_i += n;
  return n;
}

// read from packet payloads until request is fulfilled
static void
read_depacketize(gbuint8* buf, unsigned n)
{
  while (n) {
    gbuint8* p;
    unsigned nn = read_depacketize_1(&p, n, FALSE);
    memcpy(buf, p, nn);
    n -= nn;
    buf += nn;
  }
}

// Get one valid message.
// If a corrupted message with the right id is seen, return failure (0).
static unsigned
message_read_1(unsigned msg_id, message_t* m)
{
  unsigned id;
  for (;;) {
    unsigned total;
    unsigned n;
    gbuint8 buf[8];
    gbuint8* p;

    n = read_depacketize_1(&p, 8, FALSE);
    memset(buf, 0, 8);
    memcpy(buf, p, n);
    while (buf[0] != 0xdb || buf[1] != 0xfe || checksum(buf, 6) != le_readu16(buf + 6)) {
      // try for a message start at the beginning of next packet
      n = read_depacketize_1(&p, 8, TRUE);
      memset(buf, 0, 8);
      memcpy(buf, p, n);
    }
    id = le_readu16(buf + 2);
    total = le_readu16(buf + 4);
    message_ensure_size(m, total - 4);
    // copy in message head, really only need id field, do the rest for debugging
    m->buf[0] = m->buf[1] = 0;
    memcpy(m->buf + 2, buf, 8);
    // read message body and trailer
    read_depacketize((gbuint8*) m->data, total);
    p = (gbuint8*)m->data + m->size;
    if (checksum((gbuint8*) m->data, m->size) == le_readu16(p) &&
        p[2] == 0xad && p[3] == 0xbc) {
      if (global_opts.debug_level >= DBGLVL_M) {
        warning(MYNAME ": received %x\n", id);
      }
      break;
    }
    if (global_opts.debug_level >= DBGLVL_L) {
      warning(MYNAME ": corrupted message %x\n", id);
    }
    if (id == msg_id) {
      id = 0;
      break;
    }
  }
  return id;
}

// Send MSG_ACK for given message
static void
message_ack(unsigned id, const message_t* m)
{
  message_t ack;
  char* p1;
  const char* p2 = (const char*) m->data;
  switch (id) {
  case MSG_ACK:
  case MSG_NACK:
  case MSG_NAVIGATION:
  case MSG_SATELLITE_INFO:
    // don't ack these
    return;
  }
  message_init_size(&ack, 4);
  p1 = (char*) ack.data;
  // ack payload is id and body checksum of acked message
  le_write16(p1, id);
  p1[2] = p2[m->size];
  p1[3] = p2[m->size + 1];
  message_write(MSG_ACK, &ack);
  message_free(&ack);
}

// Get specific message, ignoring others. Sends ACK for non-interval messages.
// Gives up after at least READ_TIMEOUT-1 seconds have passed.
static int
message_read(unsigned msg_id, message_t* m)
{
  unsigned id;
  time_t time_start = time(NULL);

  if (global_opts.debug_level >= DBGLVL_M) {
    warning(MYNAME ": looking for %x\n", msg_id);
  }
  for (;;) {
    id = message_read_1(msg_id, m);
    if (id == 0) {
      break;
    }
    if (id == MSG_ERROR) {
      const gbuint8* p = (const gbuint8*) m->data;
      fatal(MYNAME ": device error %u: \"%s\"\n", *p, p + 1);
    }
    message_ack(id, m);
    if (id == msg_id || time(NULL) - time_start >= READ_TIMEOUT) {
      break;
    }
  }
  return id == msg_id;
}

// Read a sequence of messages, up to a MSG_TRANSFER_COMPLETE
static int
get_batch(message_t** array, unsigned* n)
{
  int success = 1;
  unsigned array_max = 100;
  message_t* a = (message_t*) xmalloc(array_max * sizeof(message_t));
  unsigned i = 0;
  unsigned id;
  if (global_opts.debug_level >= DBGLVL_M) {
    warning(MYNAME ": begin get_batch\n");
  }
  do {
    time_t time_start = time(NULL);
    if (i == array_max) {
      message_t* old_a = a;
      array_max += array_max;
      a = (message_t*) xmalloc(array_max * sizeof(message_t));
      memcpy(a, old_a, i * sizeof(message_t));
      xfree(old_a);
    }
    message_init(&a[i]);
    for (;;) {
      id = message_read_1(0, &a[i]);
      switch (id) {
      case MSG_NAVIGATION:
        if (time(NULL) - time_start >= READ_TIMEOUT) {
          success = 0;
          break;
        }
        // fall through
      case MSG_ACK:
      case MSG_NACK:
      case MSG_SATELLITE_INFO:
        continue;
      }
      break;
    }
    message_ack(id, &a[i]);
    i++;
  } while (success && id != MSG_TRANSFER_COMPLETE);
  if (success) {
    *array = a;
    *n = i - 1;
    message_free(&a[*n]);
    if (global_opts.debug_level >= DBGLVL_M) {
      warning(MYNAME ": end get_batch, %u messages\n", *n);
    }
  } else {
    while (i--) {
      message_free(&a[i]);
    }
    xfree(a);
    *array = NULL;
    *n = 0;
    if (global_opts.debug_level >= DBGLVL_M) {
      warning(MYNAME ": end get_batch, failed\n");
    }
  }
  return success;
}

typedef struct {
  unsigned msg_id;
  message_t msg;
} batch_array_t;

static batch_array_t* batch_array;

static unsigned batch_array_max;
static unsigned batch_array_i;

// add a message to sequence that will later be sent all at once
static void
add_to_batch(unsigned id, message_t* m)
{
  if (batch_array_i == batch_array_max) {
    char* old = (char*) batch_array;
    if (batch_array_max == 0) {
      batch_array_max = 50;
    }
    batch_array_max += batch_array_max;
    batch_array = (batch_array_t*) xmalloc(batch_array_max * sizeof(*batch_array));
    if (batch_array_i) {
      memcpy(batch_array, old, batch_array_i * sizeof(*batch_array));
      xfree(old);
    }
  }
  batch_array[batch_array_i].msg_id = id;
  batch_array[batch_array_i].msg = *m;
  batch_array_i++;
  memset(m, 0, sizeof(*m));
}

// send an accumulated sequence of messages
static void
send_batch(void)
{
  message_t m;
  const unsigned n = batch_array_i;
  unsigned i;
  unsigned progress = 0;

  message_init(&m);
  if (global_opts.debug_level >= DBGLVL_M) {
    warning(MYNAME ": begin send_batch, %u messages\n", n);
  }
  for (i = 0; i < n; i++) {
    unsigned timeout_count = 0;
    time_t time_start = time(NULL);

    // Can't really trigger this off either i or n as we don't
    // know how the various packets map to actual waypts.
    if (global_opts.verbose_status &&
        (batch_array[i].msg_id == MSG_WAYPOINT_IN)) {
      waypt_status_disp(waypoint_n, ++progress);
    }

    message_write(batch_array[i].msg_id, &batch_array[i].msg);
    for (;;) {
      unsigned id = message_read_1(0, &m);
      switch (id) {
      case MSG_ACK:
        break;
      case MSG_NAVIGATION:
        if (time(NULL) - time_start >= 2) {
          if (timeout_count) {
            fatal(MYNAME ": send_batch timed out\n");
          }
          timeout_count++;
          if (global_opts.debug_level >= DBGLVL_M) {
            warning(MYNAME ": re-sending %x\n", batch_array[i].msg_id);
          }
          message_write(batch_array[i].msg_id, &batch_array[i].msg);
          time_start = time(NULL);
        }
        // fall through
      case MSG_NACK:
      case MSG_SATELLITE_INFO:
        continue;
      default:
        warning(MYNAME ": unexpected response message %x during send_batch\n", id);
        continue;
      }
      break;
    }
  }
  message_read(MSG_TRANSFER_COMPLETE, &m);
  if (global_opts.debug_level >= DBGLVL_M) {
    warning(MYNAME ": end send_batch\n");
  }
  for (i = n; i--;) {
    message_free(&batch_array[i].msg);
  }
  xfree(batch_array);
  message_free(&m);
  batch_array_i = batch_array_max = 0;
}

//-----------------------------------------------------------------------------
// Coordinate conversion

static double
delbin_rad2deg(gbint32 x)
{
  return x * ((180 / M_PI) / 100000000);
}

static gbint32
delbin_deg2rad(double x)
{
  return (gbint32)(x * ((M_PI / 180) * 100000000));
}

//-----------------------------------------------------------------------------
// Waypoint reading

static time_t
decode_time(const gbuint8* p)
{
  struct tm t;
  t.tm_year = p[0];
  t.tm_mon  = p[1] - 1;
  t.tm_mday = p[2];
  t.tm_hour = p[3];
  t.tm_min  = p[4];
  t.tm_sec  = p[5];
  return mkgmtime(&t);
}

static waypoint*
decode_waypoint(const void* data)
{
  waypoint* wp = waypt_new();
  const msg_waypoint_t* p = (const msg_waypoint_t*)data;
  const char* s;
  float f;

  wp->SetCreationTime(decode_time(&p->year));
  wp->latitude = delbin_rad2deg(le_read32(p->latitude));
  wp->longitude = delbin_rad2deg(le_read32(p->longitude));
  f = le_read_float(p->elevation);
  if (f > UNKNOWN_ELEV) {
    wp->altitude = f;
  }
  wp->icon_descr = waypoint_symbol(p->symbol);
//  if (!wp->icon_descr.isNull()) {
//    wp->icon_descr = wp->icon_descr;
//  }
  if (p->name_size && p->name[0]) {
    wp->description = xstrdup(p->name);
  }
  s = p->name + p->name_size;
  if (le_readu16(s) &&  s[2]) {
    wp->notes = xstrdup(s + 2);
  }
  return wp;
}

static void
read_waypoints(void)
{
  message_t m;
  message_t* msg_array;
  unsigned msg_array_n;
  waypoint* wp = NULL;
  unsigned n_point;
  unsigned notes_i = 0;
  unsigned notes_max = 0;
  unsigned i;
  int attempt = ATTEMPT_MAX;

  message_init(&m);
  // get number of waypoints
  for (;;) {
    m.size = 0;
    message_write(MSG_WAYPOINT_COUNT, &m);
    if (message_read(MSG_WAYPOINT_COUNT, &m)) {
      break;
    }
    if (--attempt == 0) {
      fatal(MYNAME ": reading waypoint count failed\n");
    }
  }
  n_point = le_readu32(m.data);
  if (global_opts.debug_level >= DBGLVL_L) {
    warning(MYNAME ": %u waypoints\n", n_point);
  }
  if (n_point == 0) {
    message_free(&m);
    return;
  }
  // get waypoint messages
  attempt = ATTEMPT_MAX;
  for (;;) {
    m.size = MSG_REQUEST_WAYPOINTS_SIZE;
    memset(m.data, 0, m.size);
    // This byte is documented as reserved. Setting it to 3 is required to get
    // extended notes (message 0xb015) with PN-40 firmware 2.5.
    // Whether it has any effect with earlier firmware or the PN-20 is unknown.
    ((char*)m.data)[1] = 3;
    message_write(MSG_REQUEST_WAYPOINTS, &m);
    if (get_batch(&msg_array, &msg_array_n)) {
      break;
    }
    if (--attempt == 0) {
      fatal(MYNAME ": reading waypoints failed\n");
    }
    if (global_opts.debug_level >= DBGLVL_M) {
      warning(MYNAME ": timed out reading waypoints, retrying\n");
    }
    m.size = MSG_BREAK_SIZE;
    memset(m.data, 0, m.size);
    message_write(MSG_BREAK, &m);
  }
  message_free(&m);
  // process waypoint messages
  for (i = 0; i < msg_array_n; i++) {
    unsigned id = message_get_id(&msg_array[i]);
    if (id == MSG_WAYPOINT_OUT) {
      wp = decode_waypoint(msg_array[i].data);
      waypt_add(wp);
      notes_i = 0;
      notes_max = 0;
      if (global_opts.debug_level >= DBGLVL_L) {
        warning(MYNAME ": read waypoint '%s'\n", wp->description);
      }
    } else if (wp && id == MSG_WAYPOINT_NOTE_OUT) {
      const msg_waypoint_note_t* p = (const msg_waypoint_note_t*) msg_array[i].data;
      const char* s = p->name + p->name_size;
      unsigned nn = le_readu16(s);
      if (notes_max < notes_i + nn) {
        char* old = wp->notes;
        if (notes_max == 0) {
          notes_max = nn;
        }
        do {
          notes_max += notes_max;
        } while (notes_max < notes_i + nn);
        wp->notes = (char*) xmalloc(notes_max);
        if (old) {
          memcpy(wp->notes, old, notes_i);
          xfree(old);
        }
      }
      if (nn) {
        memcpy(wp->notes + notes_i, s + 2, nn);
        notes_i += nn;
        if (wp->notes[notes_i - 1] == 0) {
          notes_i--;
        }
      }
    } else {
      fatal(MYNAME ": unexpected message %x while reading waypoints\n", id);
    }
    message_free(&msg_array[i]);
  }
  xfree(msg_array);
}

//-----------------------------------------------------------------------------
// Waypoint writing

static void
encode_time(time_t time_, gbuint8* p)
{
  const struct tm* t = gmtime(&time_);
  p[0] = t->tm_year;
  p[1] = t->tm_mon + 1;
  p[2] = t->tm_mday;
  p[3] = t->tm_hour;
  p[4] = t->tm_min;
  p[5] = t->tm_sec;
}

static void
get_gc_notes(const waypoint* wp, int* symbol, char** notes, unsigned* notes_size)
{
  fs_xml* fs_gpx;
  xml_tag* root = NULL;
  gbfile* fd = gbfopen(NULL, "w", MYNAME);
  const char* size = NULL;
  int gc_sym = 0;

  switch (wp->gc_data->type) {
  case gt_traditional:
    gc_sym = 160;
    break;
  case gt_multi:
    gc_sym = 161;
    break;
  case gt_virtual:
    gc_sym = 169;
    break;
  case gt_letterbox:
    gc_sym = 163;
    break;
  case gt_event:
    gc_sym = 165;
    break;
  case gt_suprise:
    gc_sym = 162;
    break;
  case gt_webcam:
    gc_sym = 170;
    break;
  case gt_earth:
    gc_sym = 168;
    break;
  case gt_benchmark:
    gc_sym = 172;
    break;
  case gt_cito:
    gc_sym = 167;
    break;
  case gt_mega:
    gc_sym = 166;
    break;
  case gt_wherigo:
    gc_sym = 164;
    break;
  case gt_unknown:
  case gt_locationless:
  case gt_ape:
    break;
  }
  if (0 == (wp->icon_descr.compare("Geocache Found"))) {
    gc_sym = 124;
  }
  if (wp->description) {
    gbfputs(wp->description, fd);
    if (!wp->gc_data->placer.isEmpty()) {
      gbfprintf(fd, " by %s", wp->gc_data->placer.toUtf8().data());
    }
    gbfputc('\n', fd);
  }

  gbfprintf(fd, "Cache ID: %s\n", wp->shortname);
  if (gc_sym && opt_gcsym && atoi(opt_gcsym)) {
    gbfprintf(fd, "%s\n", waypoint_symbol(gc_sym));
    *symbol = gc_sym;
  } else if (!wp->icon_descr.isNull()) {
    gbfprintf(fd, "%s\n", wp->icon_descr.toUtf8().data());
  }
  switch (wp->gc_data->container) {
  case gc_micro:
    size = "Micro";
    break;
  case gc_small:
    size = "Small";
    break;
  case gc_regular:
    size = "Regular";
    break;
  case gc_large:
    size = "Large";
    break;
  case gc_unknown:
    size = "Not Chosen" ;
    break;
  case gc_other:
    size = "Other";
    break;
    // Device has no symbol for this, but this is what Topo sends.
  case gc_virtual:
    size = "Virtual";
    break;
  default:
    break;
  }
  if (size) {
    gbfprintf(fd, "SIZE: %s\n", size);
  }
  if (wp->gc_data->diff % 10) {
    gbfprintf(fd, "D%.1f", wp->gc_data->diff / 10.0);
  } else {
    gbfprintf(fd, "D%u", wp->gc_data->diff / 10);
  }
  if (wp->gc_data->terr % 10) {
    gbfprintf(fd, "/T%.1f\n", wp->gc_data->terr / 10.0);
  } else {
    gbfprintf(fd, "/T%u\n", wp->gc_data->terr / 10);
  }
  if (!wp->gc_data->hint.isEmpty() && !opt_hint_at_end) {
    gbfprintf(fd, "HINT: %s\n", wp->gc_data->hint.toUtf8().data());
  }
  if (!wp->gc_data->desc_short.utfstring.isEmpty() || !wp->gc_data->desc_long.utfstring.isEmpty()) {
    gbfputs("DESC: ", fd);
    if (!wp->gc_data->desc_short.utfstring.isEmpty()) {
      char* s1 = strip_html(&wp->gc_data->desc_short);
      char* s2 = cet_str_utf8_to_any(s1, global_opts.charset);
      gbfprintf(fd, "%s\n", s2);
      xfree(s2);
      xfree(s1);
    }
    if (!wp->gc_data->desc_long.utfstring.isEmpty()) {
      char* s1 = strip_html(&wp->gc_data->desc_long);
      char* s2 = cet_str_utf8_to_any(s1, global_opts.charset);
      gbfputs(s2, fd);
      xfree(s2);
      xfree(s1);
    }
  }
  fs_gpx = (fs_xml*)fs_chain_find(wp->fs, FS_GPX);
  if (opt_logs && fs_gpx && fs_gpx->tag) {
    root = xml_findfirst(fs_gpx->tag, "groundspeak:logs");
  }
  if (root) {
    xml_tag* curlog = xml_findfirst(root, "groundspeak:log");
    if (curlog) {
      gbfputs("\nLOG:\n", fd);
    }
    for (; curlog; curlog = xml_findnext(root, curlog, "groundspeak:log")) {
      xml_tag* logpart = xml_findfirst(curlog, "groundspeak:type");
      if (logpart) {
        gbfprintf(fd, "%s\n", logpart->cdata);
      }
      logpart = xml_findfirst(curlog, "groundspeak:date");
      if (logpart) {
        time_t logtime = xml_parse_time(logpart->cdata);
        const struct tm* logtm = gmtime(&logtime);
        gbfprintf(fd, "%d-%02d-%02d ", logtm->tm_year + 1900, logtm->tm_mon + 1, logtm->tm_mday);
      }
      logpart = xml_findfirst(curlog, "groundspeak:finder");
      if (logpart) {
        char* s = cet_str_utf8_to_any(logpart->cdata, global_opts.charset);
        gbfputs(s, fd);
        xfree(s);
      }
      logpart = xml_findfirst(curlog, "groundspeak:text");
      if (logpart) {
        char* s = cet_str_utf8_to_any(logpart->cdata, global_opts.charset);
        gbfprintf(fd, ", %s", s);
        xfree(s);
      }
      gbfputc('\n', fd);
    }
  }
  if (!wp->gc_data->hint.isEmpty() && opt_hint_at_end) {
    gbfprintf(fd, "\nHINT: %s\n", wp->gc_data->hint.toUtf8().data());
  }
  gbfputc(0, fd);
  *notes_size = fd->memlen;
  *notes = (char*) xmalloc(*notes_size);
  memcpy(*notes, fd->handle.mem, *notes_size);
  gbfclose(fd);
}

static void
write_waypoint_notes(const char* notes, unsigned size, const char* name)
{
  message_t m;
  const unsigned name_size = strlen(name) + 1;
  const unsigned bytes_per_msg = (10 * (delbin_os_packet_size - 2)) - name_size - 20;
  const unsigned msg_count = (size + (bytes_per_msg - 1)) / bytes_per_msg;
  unsigned i = 1;

  do {
    char* pp;
    unsigned n = bytes_per_msg;
    msg_waypoint_note_t* p;
    message_init_size(&m, 2 + 2 + 1 + name_size + 2 + bytes_per_msg);
    p = (msg_waypoint_note_t*) m.data;
    le_write16(p->index, i++);
    le_write16(p->total, msg_count);
    p->name_size = name_size;
    memcpy(p->name, name, p->name_size);
    pp = p->name + p->name_size;
    if (n > size) {
      n = size;
    }
    le_write16(pp, n);
    pp += 2;
    memcpy(pp, notes, n);
    pp += n;
    if (*(pp - 1)) {
      *pp++ = 0;
    }
    notes += n;
    size -= n;
    m.size = pp - (char*)p;
    add_to_batch(MSG_WAYPOINT_NOTE_IN, &m);
  } while (size != 0);
}

static void
add_nuke(nuke_type type)
{
  message_t m;
  msg_delete_t* p;

  message_init_size(&m, MSG_DELETE_SIZE);
  p = (msg_delete_t*) m.data;
  p->type = type;
  p->mode = nuke_mode_all;
  p->location = nuke_dest_internal;
  memset(p->object_name, 0, sizeof(p->object_name));

  // MSG_DELETE generates a MSG_TRANSFER_COMPLETE,
  // so use the batch facility to wait for it
  add_to_batch(MSG_DELETE, &m);
  send_batch();
}

static void
write_waypoint(const waypoint* wp)
{
  message_t m;
  msg_waypoint_t* p;
  const char* name = wp->shortname;
  unsigned name_size;
  char* notes;
  unsigned notes_size = 0;
  unsigned extended_notes_size = 0;
  char* notes_freeable = NULL;
  int symbol = -1;
  float elev = UNKNOWN_ELEV;
  char* pp;

  if (waypt_empty_gc_data(wp)) {
    notes = wp->notes;
    if (notes == NULL && wp->description && strcmp(wp->shortname, wp->description)) {
      notes = wp->description;
    }
    if (notes) {
      notes_size = strlen(notes) + 1;
    }
  } else {
    get_gc_notes(wp, &symbol, &notes, &notes_size);
    notes_freeable = notes;
    if (wp->description) {
      name = mkshort(mkshort_handle, wp->description);
    }
  }

  if (notes_size > 800) {
    if (use_extended_notes) {
      extended_notes_size = notes_size;
      notes_size = 1;
    } else {
      notes_size = 800;
    }
  }

  name_size = strlen(name) + 1;
  if (name_size > 255) {
    name_size = 255;
  }
  message_init_size(&m, 31 + name_size + notes_size);
  p = (msg_waypoint_t*) m.data;

  waypoint_i++;
  le_write32(p->total, waypoint_n);
  le_write32(p->index, waypoint_i);
  encode_time(wp->GetCreationTime(), &p->year);
  le_write32(p->latitude, delbin_deg2rad(wp->latitude));
  le_write32(p->longitude, delbin_deg2rad(wp->longitude));
  if (wp->altitude > unknown_alt) {
    elev = wp->altitude;
  }
  le_write_float(p->elevation, elev);
  if (symbol < 0) {
    symbol = 0;
    if (!wp->icon_descr.isNull()) {
      symbol = waypoint_symbol_index(wp->icon_descr.toUtf8().data());
    }
  }
  p->symbol = symbol;
  p->name_size = name_size;
  memcpy(p->name, name, name_size - 1);
  p->name[name_size - 1] = 0;
  pp = p->name + name_size;
  m.size = (pp + 2 + notes_size) - (char*)p;
  if (extended_notes_size) {
    le_write16(pp, 0xffff);
    pp[2] = 0;
  } else {
    le_write16(pp, notes_size);
    if (notes) {
      memcpy(pp + 2, notes, notes_size - 1);
      pp[2 + notes_size - 1] = 0;
    }
  }

  add_to_batch(MSG_WAYPOINT_IN, &m);

  if (extended_notes_size) {
    write_waypoint_notes(notes, extended_notes_size, name);
  }
  if (notes_freeable) {
    xfree(notes_freeable);
  }
  if (global_opts.debug_level >= DBGLVL_L) {
    warning(MYNAME ": wrote waypoint %u '%s'\n", waypoint_i, name);
  }
}

static void
write_waypoints(void)
{
  message_t m;
  unsigned device_n = 0;

  waypoint_i = 0;
  waypoint_n = waypt_count();
  if (waypoint_n > device_max_waypoint) {
    fatal(MYNAME ": waypoint count (%u) exceeds device limit (%u)\n",
          waypoint_n, device_max_waypoint);
  }

  message_init_size(&m, 0);
  message_write(MSG_WAYPOINT_COUNT, &m);
  if (message_read(MSG_WAYPOINT_COUNT, &m)) {
    device_n = le_readu32(m.data);
  }

  waypt_disp_all(write_waypoint);
  send_batch();

  if (device_n + waypoint_n > device_max_waypoint) {
    m.size = 0;
    message_write(MSG_WAYPOINT_COUNT, &m);
    if (message_read(MSG_WAYPOINT_COUNT, &m) &&
        le_readu32(m.data) == device_max_waypoint) {
      warning(MYNAME ": waypoint count (%u already on device + %u added = %u)"
              " exceeds device limit (%u), some may have been discarded\n",
              device_n, waypoint_n, device_n + waypoint_n, device_max_waypoint);
    }
  }
  message_free(&m);
}

//-----------------------------------------------------------------------------
// Track reading

static void
decode_sat_fix(waypoint* wp, const gbuint8 status)
{
  switch (status & 3) {
  case 1:
    wp->fix = fix_none;
    break;
  case 2:
    wp->fix = fix_2d;
    break;
  case 3:
    wp->fix = fix_3d;
    if (status & 4) {
      wp->fix = fix_dgps;
    }
    break;
  }
}

static void
decode_track_point(const void* data, unsigned* wp_array_i, unsigned max_point)
{
  const msg_track_point_t* p = (const msg_track_point_t*) data;
  const unsigned n = p->number;
  unsigned i;
  unsigned j = *wp_array_i;

  if (j + n > max_point) {
    fatal(MYNAME ": read too many track points\n");
  }
  for (i = 0; i < n; i++, j++) {
    waypoint* wp = waypt_new();
    float elev = le_read_float(p->point[i].elevation);
    wp_array[j] = wp;
    wp->SetCreationTime(decode_time(&p->point[i].year));
    wp->latitude = delbin_rad2deg(le_read32(p->point[i].latitude));
    wp->longitude = delbin_rad2deg(le_read32(p->point[i].longitude));
    if (elev > UNKNOWN_ELEV) {
      wp->altitude = elev;
    }
    wp->speed = le_readu16(p->point[i].speed);
    wp->speed *= (100.0f / (60 * 60));
    wp->wpt_flags.speed = 1;
    decode_sat_fix(wp, p->point[i].status);
    wp->wpt_flags.new_trkseg = (p->point[i].status & 0x10) != 0;
  }
  *wp_array_i = j;
}

static void
read_track(route_head* track)
{
  message_t m;
  message_t* msg_array;
  const msg_track_header_t* p;
  unsigned msg_array_n;
  unsigned wp_array_i = 0;
  unsigned n_point;
  unsigned i;
  int attempt = ATTEMPT_MAX;

  message_init(&m);
  // read track messages
  for (;;) {
    m.size = MSG_REQUEST_TRACKS_SIZE;
    memset(m.data, 0, m.size);
    ((char*)m.data)[0] = 1;  // Download single track
    strcpy((char*)m.data + 1, track->rte_name);
    message_write(MSG_REQUEST_TRACKS, &m);
    if (get_batch(&msg_array, &msg_array_n)) {
      break;
    }
    if (--attempt == 0) {
      fatal(MYNAME ": reading track '%s' failed\n", track->rte_name);
    }
    if (global_opts.debug_level >= DBGLVL_M) {
      warning(MYNAME ": timed out reading track '%s', retrying\n", track->rte_name);
    }
    m.size = MSG_BREAK_SIZE;
    memset(m.data, 0, m.size);
    message_write(MSG_BREAK, &m);
  }
  message_free(&m);
  if (msg_array_n == 0 || message_get_id(&msg_array[0]) != MSG_TRACK_HEADER_OUT) {
    fatal(MYNAME ": reading track '%s' failed (missing track header)\n", track->rte_name);
  }
  // process track messages
  p = (const msg_track_header_t*) msg_array[0].data;
  if (le_readu16(p->comment_size)) {
    track->rte_desc = xstrdup(p->comment);
  }
  track->line_color.bbggrr = track_color(p->color[0]);
  n_point = le_readu32(p->total_points);
  wp_array = (waypoint**) xcalloc(n_point, sizeof(*wp_array));
  message_free(&msg_array[0]);
  for (i = 1; i < msg_array_n; i++) {
    unsigned id = message_get_id(&msg_array[i]);
    if (id == MSG_TRACK_POINT_OUT) {
      decode_track_point(msg_array[i].data, &wp_array_i, n_point);
    } else {
      fatal(MYNAME ": unexpected message %x while reading track '%s'\n", id, track->rte_name);
    }
    message_free(&msg_array[i]);
  }
  xfree(msg_array);
  if (n_point != wp_array_i) {
    fatal(MYNAME ": track point count mismatch, expected %u, got %u\n", n_point, wp_array_i);
  }
  if (global_opts.debug_level >= DBGLVL_L) {
    warning(MYNAME ": read track '%s' %u points\n", track->rte_name, n_point);
  }
  for (i = 0; i < n_point; i++) {
    track_add_wpt(track, wp_array[i]);
  }
  track_add_head(track);
  xfree(wp_array);
}

static void
read_tracks(void)
{
  message_t m;
  message_t* msg_array;
  unsigned msg_array_n;
  route_head** track_array;
  unsigned total;
  unsigned i;
  int attempt = ATTEMPT_MAX;

  message_init(&m);
  // get number of tracks
  for (;;) {
    m.size = 0;
    message_write(MSG_TRACK_COUNT, &m);
    if (message_read(MSG_TRACK_COUNT, &m)) {
      break;
    }
    if (--attempt == 0) {
      fatal(MYNAME ": reading track count failed\n");
    }
  }
  total = le_readu32(m.data);
  if (global_opts.debug_level >= DBGLVL_L) {
    warning(MYNAME ": %u tracks\n", total);
  }
  if (total == 0) {
    message_free(&m);
    return;
  }

  // First get track headers, then request each track with non-zero number of points
  attempt = ATTEMPT_MAX;
  for (;;) {
    m.size = MSG_REQUEST_TRACKS_SIZE;
    memset(m.data, 0, m.size);
    ((char*)m.data)[0] = 2;  // Download all track headers
    message_write(MSG_REQUEST_TRACKS, &m);
    if (get_batch(&msg_array, &msg_array_n)) {
      break;
    }
    if (--attempt == 0) {
      fatal(MYNAME ": reading track headers failed\n");
    }
    if (global_opts.debug_level >= DBGLVL_M) {
      warning(MYNAME ": timed out reading track headers, retrying\n");
    }
    m.size = MSG_BREAK_SIZE;
    memset(m.data, 0, m.size);
    message_write(MSG_BREAK, &m);
  }
  message_free(&m);
  track_array = (route_head**) xcalloc(total, sizeof(*track_array));
  for (i = 0; i < msg_array_n; i++) {
    unsigned id = message_get_id(&msg_array[i]);
    if (id == MSG_TRACK_HEADER_OUT) {
      const msg_track_header_t* p = (msg_track_header_t*) msg_array[i].data;
      if (le_readu32(p->total_points)) {
        track_array[i] = route_head_alloc();
        track_array[i]->rte_name = xstrdup(p->name);
      }
    } else {
      fatal(MYNAME ": unexpected message %x while reading track headers\n", id);
    }
    message_free(&msg_array[i]);
  }
  xfree(msg_array);
  // get each track
  for (i = 0; i < total; i++) {
    if (track_array[i]) {
      read_track(track_array[i]);
    }
  }
  xfree(track_array);
}

//-----------------------------------------------------------------------------
// Track writing

static void
write_track_points(void)
{
  message_t m;
  const unsigned pt_per_msg = 10;
  msg_track_point_t* p = NULL;
  unsigned i = 0;
  unsigned j = 0;

  do {
    const waypoint* wp = wp_array[i];
    float f;

    if (j == 0) {
      message_init_size(&m, 9 + 23 * pt_per_msg);
      p =(msg_track_point_t*) m.data;
      le_write32(p->total, waypoint_n);
      le_write32(p->index, i + 1);
    }
    assert(p);
    encode_time(wp->GetCreationTime(), &p->point[j].year);
    le_write32(p->point[j].latitude, delbin_deg2rad(wp->latitude));
    le_write32(p->point[j].longitude, delbin_deg2rad(wp->longitude));
    f = UNKNOWN_ELEV;
    if (wp->altitude > unknown_alt) {
      f = wp->altitude;
    }
    le_write_float(p->point[j].elevation, f);
    f = WAYPT_GET(wp, speed, 0);
    f *= (60 * 60) / 100;
    le_write16(p->point[j].speed, (gbuint16)f);
    f = WAYPT_GET(wp, course, 0);
    f *= 100;
    le_write16(p->point[j].heading, (gbuint16)f);
    switch (wp->fix) {
    default:
      p->point[j].status = 0;
      break;
    case fix_none:
      p->point[j].status = 1;
      break;
    case fix_2d:
      p->point[j].status = 2;
      break;
    case fix_3d:
      p->point[j].status = 3;
      break;
    case fix_dgps:
      p->point[j].status = 4 | 3;
      break;
    }
    if (wp->wpt_flags.new_trkseg) {
      p->point[j].status |= 0x10;
    }
    i++;
    j++;
    if (j == pt_per_msg || i == waypoint_n) {
      p->number = j;
      m.size = 9 + 23 * j;
      add_to_batch(MSG_TRACK_POINT_IN, &m);
      j = 0;
    }
  } while (i < waypoint_n);
}

static void
write_track_begin(const route_head* track)
{
  waypoint_i = 0;
  waypoint_n = track->rte_waypt_ct;
  if (waypoint_n) {
    wp_array = (waypoint**) xmalloc(waypoint_n * sizeof(*wp_array));
  }
}

static void
write_track_point(const waypoint* wp)
{
  wp_array[waypoint_i++] = (waypoint*)wp;
}

static void
write_track_end(const route_head* track)
{
  message_t m;
  msg_track_header_in_t* p;
  unsigned comment_size = 0;

  if (waypoint_n == 0) {
    return;
  }
  if (track->rte_desc) {
    comment_size = strlen(track->rte_desc) + 1;
  }
  message_init_size(&m, sizeof(msg_track_header_in_t) - 1 + comment_size);
  p = (msg_track_header_in_t*) m.data;
  memset(p->name, 0, sizeof(p->name));
  if (track->rte_name) {
    strncpy(p->name, track->rte_name, sizeof(p->name) - 1);
  } else {
    sprintf(p->name, "%lu", (long)wp_array[0]->GetCreationTime());
  }
  le_write32(p->total_points, waypoint_n);
  encode_time(current_time(), &p->year);
  le_write16(p->color, track_color_index(track->line_color.bbggrr));
  le_write16(p->comment_size, comment_size);
  if (comment_size) {
    memcpy(p->comment, track->rte_desc, comment_size);
  }
  add_to_batch(MSG_TRACK_HEADER_IN, &m);
  write_track_points();
  send_batch();
  xfree(wp_array);
}

static void
write_tracks(void)
{
  track_disp_all(write_track_begin, write_track_end, write_track_point);
}

//-----------------------------------------------------------------------------
// Route reading

static void
decode_route_shape(const void* data, unsigned* wp_array_i)
{
  const msg_route_shape_t* p = (msg_route_shape_t*) data;
  const unsigned n = p->number;
  unsigned i;
  unsigned j = *wp_array_i;

  for (i = 0; i < n; i++, j++) {
    char buf[32];
    waypoint* wp = waypt_new();
    wp_array[j] = wp;
    wp->latitude = delbin_rad2deg(le_read32(p->point[i].latitude));
    wp->longitude = delbin_rad2deg(le_read32(p->point[i].longitude));
    sprintf(buf, "SHP%03u", j);
    wp->shortname = xstrdup(buf);
  }
  *wp_array_i = j;
}

static waypoint*
decode_route_point(const void* data)
{
  const msg_route_point_t* p = (const msg_route_point_t*) data;
  const char* s = NULL;
  gbfile* fd = gbfopen(NULL, "w", MYNAME);
  waypoint* wp = waypt_new();
  if (p->name[0]) {
    wp->shortname = xstrdup(p->name);
  }
  // give these a higher priority than the shape points
  wp->route_priority = 1;
  wp->latitude = delbin_rad2deg(le_read32(p->latitude));
  wp->longitude = delbin_rad2deg(le_read32(p->longitude));
  switch (p->itinerary_type) {
  case 1:
    s = "Start";
    break;
  case 2:
    s = "Stop";
    break;
  case 3:
    s = "Finish";
    break;
  case 4:
    s = "Via";
    break;
  case 5:
    s = "Via Hidden";
    break;
  case 6:
    switch (p->turn_type) {
    case 1:
      s = "Turn, Straight";
      break;
    case 2:
      s = "Turn, Right";
      break;
    case 3:
      s = "Turn, Bear Right";
      break;
    case 4:
      s = "Turn, Keep Right";
      break;
    case 5:
      s = "Turn, Left";
      break;
    case 6:
      s = "Turn, Bear Left";
      break;
    case 7:
      s = "Turn, Keep Left";
      break;
    case 8:
      s = "Turn, Reverse Direction";
      break;
    case 9:
      s = "Turn, Street Name Change";
      break;
    }
    break;
  }
  if (s) {
    gbfprintf(fd, "Type: %s", s);
  }
  if (p->exit_label_size && p->exit_label[0]) {
    gbfprintf(fd, "\nExit: %s", p->exit_label);
  }
  s = p->exit_label + p->exit_label_size;
  if (s[0] && s[1]) {
    gbfprintf(fd, "\n%s", s + 1);
  }
  if (fd->memlen) {
    gbfputc(0, fd);
    wp->notes = (char*) xmalloc(fd->memlen);
    memcpy(wp->notes, fd->handle.mem, fd->memlen);
  }
  gbfclose(fd);
  return wp;
}

static void
read_route(route_head* route)
{
  message_t m;
  message_t* msg_array;
  const msg_route_header_t* p;
  unsigned msg_array_n;
  unsigned wp_array_i = 0;
  unsigned route_total, shape_total, total;
  unsigned i;
  int attempt = ATTEMPT_MAX;

  message_init(&m);
  for (;;) {
    m.size = MSG_REQUEST_ROUTES_SIZE;
    memset(m.data, 0, m.size);
    ((char*)m.data)[0] = 1;  // Download single route
    strcpy((char*)m.data + 1, route->rte_name);
    message_write(MSG_REQUEST_ROUTES, &m);
    if (get_batch(&msg_array, &msg_array_n)) {
      break;
    }
    if (--attempt == 0) {
      fatal(MYNAME ": reading route '%s' failed (timed out)\n", route->rte_name);
    }
    if (global_opts.debug_level >= DBGLVL_M) {
      warning(MYNAME ": timed out reading route route '%s', retrying\n", route->rte_name);
    }
    m.size = MSG_BREAK_SIZE;
    memset(m.data, 0, m.size);
    message_write(MSG_BREAK, &m);
  }
  message_free(&m);
  if (msg_array_n == 0 || message_get_id(&msg_array[0]) != MSG_ROUTE_HEADER_OUT) {
    fatal(MYNAME ": missing route header\n");
  }
  p = (const msg_route_header_t*) msg_array[0].data;
  route_total = le_readu32(p->total_route_point);
  shape_total = le_readu32(p->total_shape_point);
  total = route_total + shape_total;
  wp_array = (waypoint**) xcalloc(total, sizeof(*wp_array));
  if (global_opts.debug_level >= DBGLVL_L) {
    warning(MYNAME ": route '%s' %u points, %u shape points\n",
            route->rte_name, route_total, shape_total);
  }
  message_free(&msg_array[0]);
  for (i = 1; i < msg_array_n; i++) {
    unsigned id = message_get_id(&msg_array[i]);
    if (id == MSG_ROUTE_POINT_OUT) {
      wp_array[wp_array_i] = decode_route_point(msg_array[i].data);
      if (global_opts.debug_level >= DBGLVL_L) {
        warning(MYNAME ": route point '%s'\n", wp_array[wp_array_i]->shortname);
      }
      wp_array_i++;
    } else if (id == MSG_ROUTE_SHAPE_OUT) {
      decode_route_shape(msg_array[i].data, &wp_array_i);
    } else {
      fatal(MYNAME ": unexpected message %x while reading route '%s'\n", id, route->rte_name);
    }
    message_free(&msg_array[i]);
  }
  xfree(msg_array);
  if (total != wp_array_i) {
    fatal(MYNAME ": route point count mismatch, expected %u, got %u\n", total, wp_array_i);
  }
  for (i = 0; i < total; i++) {
    route_add_wpt(route, wp_array[i]);
  }
  xfree(wp_array);
  route_add_head(route);
}

static void
read_routes(void)
{
  message_t m;
  message_t* msg_array;
  unsigned msg_array_n;
  route_head** route_array;
  unsigned total;
  unsigned i;
  int attempt = ATTEMPT_MAX;

  message_init(&m);
  // get number of routes
  for (;;) {
    m.size = 0;
    message_write(MSG_ROUTE_COUNT, &m);
    if (message_read(MSG_ROUTE_COUNT, &m)) {
      break;
    }
    if (--attempt == 0) {
      fatal(MYNAME ": reading route count failed\n");
    }
  }
  total = le_readu32(m.data);
  if (global_opts.debug_level >= DBGLVL_L) {
    warning(MYNAME ": %u routes\n", total);
  }
  if (total == 0) {
    message_free(&m);
    return;
  }

  // First get route headers, then request each route
  attempt = ATTEMPT_MAX;
  for (;;) {
    m.size = MSG_REQUEST_ROUTES_SIZE;
    memset(m.data, 0, m.size);
    ((char*)m.data)[0] = 2;  // Download all route headers
    message_write(MSG_REQUEST_ROUTES, &m);
    if (get_batch(&msg_array, &msg_array_n)) {
      break;
    }
    if (--attempt == 0) {
      fatal(MYNAME ": reading route headers failed\n");
    }
    if (global_opts.debug_level >= DBGLVL_M) {
      warning(MYNAME ": timed out reading route headers, retrying\n");
    }
    m.size = MSG_BREAK_SIZE;
    memset(m.data, 0, m.size);
    message_write(MSG_BREAK, &m);
  }
  message_free(&m);
  route_array = (route_head**) xcalloc(total, sizeof(*route_array));
  for (i = 0; i < msg_array_n; i++) {
    unsigned id = message_get_id(&msg_array[i]);
    if (id == MSG_ROUTE_HEADER_OUT) {
      route_array[i] = route_head_alloc();
      route_array[i]->rte_name = xstrdup(((msg_route_header_t*)msg_array[i].data)->name);
    } else {
      fatal(MYNAME ": unexpected message %x while reading route headers\n", id);
    }
    message_free(&msg_array[i]);
  }
  xfree(msg_array);
  // get each route
  for (i = 0; i < total; i++) {
    read_route(route_array[i]);
  }
  xfree(route_array);
}

//-----------------------------------------------------------------------------
// Route writing

static unsigned route_point_n;
static unsigned shape_point_n;
static unsigned* shape_point_counts;

static void
write_route_shape_points(waypoint** array, unsigned n)
{
  message_t m;
  const unsigned pt_per_msg = 25;
  msg_route_shape_t* p = NULL;
  unsigned i = 0;
  unsigned j = 0;

  do {
    if (j == 0) {
      message_init_size(&m, 10 + 8 * pt_per_msg);
      p = (msg_route_shape_t*) m.data;
      le_write32(p->total, n);
      le_write32(p->index, i + 1);
      p->reserved = 0;
    }
    assert(p);
    le_write32(p->point[j].latitude, delbin_deg2rad(array[i]->latitude));
    le_write32(p->point[j].longitude, delbin_deg2rad(array[i]->longitude));
    i++;
    j++;
    if (j == pt_per_msg || i == n) {
      p->number = j;
      m.size = 10 + 8 * j;
      add_to_batch(MSG_ROUTE_SHAPE_IN, &m);
      j = 0;
    }
  } while (i < n);
}

static void
write_route_points(void)
{
  unsigned route_point_i = 0;
  unsigned i = 0;

  while (i < waypoint_n) {
    message_t m;
    unsigned shape_n;
    const waypoint* wp = wp_array[i];
    msg_route_point_t* p;
    char* s;

    message_init_size(&m, sizeof(msg_route_point_t) + 1 + 1 + 4);
    p = (msg_route_point_t*) m.data;
    memset(m.data, 0, m.size);
    route_point_i++;
    shape_n = shape_point_counts[route_point_i];
    le_write32(p->total, route_point_n);
    le_write32(p->index, route_point_i);
    if (wp->shortname) {
      strncpy(p->name, wp->shortname, sizeof(p->name) - 1);
    } else {
      sprintf(p->name, "RPT%u", route_point_i);
    }
    le_write32(p->latitude, delbin_deg2rad(wp->latitude));
    le_write32(p->longitude, delbin_deg2rad(wp->longitude));
    p->exit_label_size = 1;
    s = p->exit_label + p->exit_label_size;
    s[0] = 1;  // comment size
    le_write32(s + 2, shape_n);
    if (route_point_i == 1) {
      p->itinerary_type = 1; // start
    } else if (route_point_i == route_point_n) {
      p->itinerary_type = 3; // finish
    }
    add_to_batch(MSG_ROUTE_POINT_IN, &m);
    i++;
    if (shape_n) {
      write_route_shape_points(&wp_array[i], shape_n);
      i += shape_n;
    }
  }
}

static void
write_route_begin(const route_head* track)
{
  waypoint_i = 0;
  route_point_n = 0;
  shape_point_n = 0;
  waypoint_n = track->rte_waypt_ct;
  if (waypoint_n) {
    wp_array = (waypoint**) xmalloc(waypoint_n * sizeof(*wp_array));
    shape_point_counts = (unsigned int*) xcalloc(waypoint_n, sizeof(*shape_point_counts));
  }
}

static void
write_route_point(const waypoint* wp)
{
  const char* s = wp->shortname;
  wp_array[waypoint_i++] = (waypoint*)wp;
  if (s && s[0] == 'S' && s[1] == 'H' && s[2] == 'P' && s[3] >= '0' && s[3] <= '9') {
    shape_point_n++;
    shape_point_counts[route_point_n]++;
  } else {
    route_point_n++;
  }
}

static void
write_route_end(const route_head* route)
{
  message_t m;
  msg_route_header_in_t* p;

  if (waypoint_n == 0) {
    return;
  }
  message_init_size(&m, sizeof(msg_route_header_in_t));
  p = (msg_route_header_in_t*) m.data;
  memset(p->name, 0, sizeof(p->name));
  if (route->rte_name) {
    strncpy(p->name, route->rte_name, sizeof(p->name) - 1);
  } else {
    sprintf(p->name, "%lu", (long)wp_array[0]->GetCreationTime());
  }
  p->type = 0;
  le_write32(p->total_route_point, route_point_n);
  le_write32(p->total_shape_point, shape_point_n);
  add_to_batch(MSG_ROUTE_HEADER_IN, &m);
  write_route_points();
  send_batch();
  if (wp_array) {
    xfree(wp_array);
    xfree(shape_point_counts);
  }
}

static void
write_routes(void)
{
  route_disp_all(write_route_begin, write_route_end, write_route_point);
}

//-----------------------------------------------------------------------------
// Current position

static waypoint*
decode_navmsg(const void* data)
{
  waypoint* wp = waypt_new();
  const msg_navigation_t* p = (const msg_navigation_t*) data;
  struct tm t;

  t.tm_year = le_readu16(p->year) - 1900;
  t.tm_mon = p->month - 1;
  t.tm_mday = p->day;
  t.tm_hour = p->hour;
  t.tm_min = p->minute;
  t.tm_sec = p->second;
  wp->SetCreationTime(mkgmtime(&t));
  wp->sat = p->satellites;
  wp->latitude = le_read_double(p->latitude);
  wp->longitude = le_read_double(p->longitude);
  wp->altitude = le_read_double(p->elevation);
  wp->speed = le_read_float(p->speed);
  wp->speed *= (1000.0f / (60 * 60));
  wp->wpt_flags.speed = 1;
  wp->course = le_readu16(p->heading);
  wp->course /= 100;
  wp->wpt_flags.course = 1;
  decode_sat_fix(wp, p->fix_status);
  wp->shortname = xstrdup("Position");
  return wp;
}

static waypoint*
read_position(void)
{
  waypoint* wp;
  message_t m;

  message_init(&m);
  message_read(MSG_NAVIGATION, &m);
  wp = decode_navmsg(m.data);
  if (wp->fix > fix_none &&
      message_read_1(MSG_SATELLITE_INFO, &m) == MSG_SATELLITE_INFO) {
    const msg_satellite_t* p = (const msg_satellite_t*) m.data;
    wp->hdop = le_readu16(p->hdop);
    wp->hdop /= 100;
    wp->vdop = le_readu16(p->vdop);
    wp->vdop /= 100;
    wp->pdop = le_readu16(p->pdop);
    wp->pdop /= 100;
  }
  message_free(&m);
  return wp;
}

//-----------------------------------------------------------------------------

static void
delbin_list_units()
{
  int i;
  for (i = 0; i < n_delbin_units; i++) {
    printf("%u %s %s\n",
           delbin_unit_info[i].unit_number,
           delbin_unit_info[i].unit_serial_number,
           delbin_unit_info[i].unit_name);
  }
}

static void
delbin_rw_init(const char* fname)
{
  message_t m;
  char buf[256];

  if (!mkshort_handle) {
    mkshort_handle = mkshort_new_handle();
  }
  //  Contrary to doc, it looks like there's a limit of 32 bytes
  // and a null terminator is required, at least in F/W 2.6.210726
  // on a PN-40.
  setshort_length(mkshort_handle, 31);
  setshort_whitespace_ok(mkshort_handle, 1);
  setshort_badchars(mkshort_handle, "");
  setshort_mustuniq(mkshort_handle, 1);

  delbin_os_ops.init(fname);

  // Often the first packet is part of an old message, sometimes it can
  // confuse the first message read if we don't get rid of it
  packet_read(buf);
  // Send a break to clear any state from a previous failure
  message_init_size(&m, MSG_BREAK_SIZE);
  memset(m.data, 0, m.size);
  message_write(MSG_BREAK, &m);
  // get version info
  m.size = 0;
  message_write(MSG_VERSION, &m);
  if (message_read(MSG_VERSION, &m)) {
    const msg_version_t* p = (const msg_version_t*) m.data;
    if (global_opts.debug_level >= DBGLVL_L) {
      warning(MYNAME ": device %s %s\n", p->product, p->firmware);
    }
    if (opt_long_notes) {
      use_extended_notes = TRUE;
    } else if (strstr(p->product, "PN-20")) {
      use_extended_notes = p->firmware[0] > '1' ||
                           (p->firmware[0] == '1' && p->firmware[2] >= '6');
    } else if (strstr(p->product, "PN-30") || strstr(p->product, "PN-40")) {
      use_extended_notes = p->firmware[0] > '2' ||
                           (p->firmware[0] == '2' && p->firmware[2] >= '5');
    } else {
      // assume PN-60 or later
      use_extended_notes = TRUE;
    }
    delbin_unit_info[n_delbin_units].unit_number = n_delbin_units;
    delbin_unit_info[n_delbin_units].unit_serial_number = xstrndup(p->serial, sizeof(p->serial));
    delbin_unit_info[n_delbin_units].unit_name = xstrndup(p->product, sizeof(p->product));
    n_delbin_units++;
  }
  message_free(&m);

  if (strlen(fname) > 4) {
    if (0 == strcmp(fname+4, "list")) {
      delbin_list_units();
      exit(1);
    }
  }
}

static void
delbin_rw_deinit(void)
{
  if (mkshort_handle) {
    mkshort_del_handle(&mkshort_handle);
  }
  delbin_os_ops.deinit();
}

static void
delbin_read(void)
{
  if (doing_wpts) {
    if (opt_getposn) {
      waypt_add(read_position());
    } else {
      read_waypoints();
    }
  }
  if (doing_trks) {
    read_tracks();
  }
  if (doing_rtes) {
    read_routes();
  }
}

static void
delbin_write(void)
{
  if (doing_wpts) {
    message_t m;
    device_max_waypoint = 1000;
    message_init_size(&m, 0);
    message_write(MSG_CAPABILITIES, &m);
    if (message_read(MSG_CAPABILITIES, &m)) {
      const msg_capabilities_t* p = (const msg_capabilities_t*) m.data;
      device_max_waypoint = le_readu32(p->max_waypoints);
    }
    message_free(&m);

    if (opt_nuke_wpt) {
      add_nuke(nuke_type_wpt);
    }
    write_waypoints();
  }
  if (doing_trks) {
    if (opt_nuke_trk) {
      add_nuke(nuke_type_trk);
    }
    write_tracks();
  }
  if (doing_rtes) {
    if (opt_nuke_rte) {
      add_nuke(nuke_type_rte);
    }
    write_routes();
  }
}

static waypoint*
delbin_rd_position(posn_status* status)
{
  return read_position();
}

ff_vecs_t delbin_vecs = {
  ff_type_serial,
  FF_CAP_RW_ALL,
  delbin_rw_init,
  delbin_rw_init,
  delbin_rw_deinit,
  delbin_rw_deinit,
  delbin_read,
  delbin_write,
  NULL,
  delbin_args,
  CET_CHARSET_LATIN1, 1,
  { delbin_rw_init, delbin_rd_position, delbin_rw_deinit }
};

//=============================================================================
// OS device I/O implementations

#define VENDOR_ID 0x1163
#define PRODUCT_ID 0x2020

//-----------------------------------------------------------------------------
// Windows
#ifdef HAVE_WDK

#undef HAVE_LIBUSB

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <setupapi.h>
// If hidsdi.h is not found, you need to download the Windows Driver Kit,
// from http://www.microsoft.com/whdc/Devtools/wdk/default.mspx
// You need to install 'build environments' and 'tools' from the SDK and
// follow the instructions in the Install.html to get MSVC to find the right
// headers and libraries.
#include <specstrings.h>
#include <hidsdi.h>

static HANDLE hid_handle;

static void
win_os_init(const char* fname)
{
  GUID hid_guid;
  HDEVINFO dev_info;
  SP_DEVICE_INTERFACE_DATA dev_int_data;
  PHIDP_PREPARSED_DATA hid_ppd;
  HIDP_CAPS hid_caps;
  const char* busy = "";
  unsigned i;

  hid_handle = INVALID_HANDLE_VALUE;
  HidD_GetHidGuid(&hid_guid);
  dev_info = SetupDiGetClassDevs(&hid_guid, NULL, NULL, DIGCF_DEVICEINTERFACE | DIGCF_PRESENT);
  if (dev_info == INVALID_HANDLE_VALUE) {
    fatal(MYNAME ": SetupDiGetClassDevs failed %u\n", GetLastError());
  }
  dev_int_data.cbSize = sizeof(dev_int_data);
  for (i = 0; SetupDiEnumDeviceInterfaces(dev_info, NULL, &hid_guid, i, &dev_int_data); i++) {
    union {
      SP_DEVICE_INTERFACE_DETAIL_DATA detail_data;
      char buf[300];
    } u;
    u.detail_data.cbSize = sizeof(u.detail_data);
    if (SetupDiGetDeviceInterfaceDetail(dev_info,
                                        &dev_int_data, &u.detail_data, sizeof(u.buf), NULL, NULL)) {
      HANDLE h = CreateFile(u.detail_data.DevicePath,
                            FILE_READ_DATA | FILE_WRITE_DATA, 0, NULL, OPEN_EXISTING, 0, NULL);
      if (h != INVALID_HANDLE_VALUE) {
        HIDD_ATTRIBUTES hid_attr;
        hid_attr.Size = sizeof(hid_attr);
        if (HidD_GetAttributes(h, &hid_attr) &&
            hid_attr.VendorID == VENDOR_ID && hid_attr.ProductID == PRODUCT_ID) {
          hid_handle = h;
          break;
        }
        CloseHandle(h);
      } else if (GetLastError() == ERROR_SHARING_VIOLATION &&
                 strstr(u.detail_data.DevicePath, "1163") &&
                 strstr(u.detail_data.DevicePath, "2020")) {
        busy = " (device busy?)";
      }
    }
  }
  SetupDiDestroyDeviceInfoList(dev_info);
  if (hid_handle == INVALID_HANDLE_VALUE) {
    fatal(MYNAME ": no DeLorme PN found%s\n", busy);
  }
  if (!HidD_GetPreparsedData(hid_handle, &hid_ppd)) {
    fatal(MYNAME ": HidD_GetPreparsedData failed %u\n", GetLastError());
  }
  if (!HidP_GetCaps(hid_ppd, &hid_caps)) {
    fatal(MYNAME ": HidP_GetCaps failed %u\n", GetLastError());
  }
  // report length includes report id
  delbin_os_packet_size = hid_caps.InputReportByteLength - 1;
  HidD_FreePreparsedData(hid_ppd);
}

static void
win_os_deinit(void)
{
  CloseHandle(hid_handle);
}

static unsigned
win_os_packet_read(void* buf)
{
  DWORD n;
  char buf1[257];
  // first byte is report id
  if (ReadFile(hid_handle, buf1, delbin_os_packet_size + 1, &n, NULL) == 0) {
    unsigned err = GetLastError();
    fatal(MYNAME ": ReadFile failed %u\n", err);
  }
  if (n > 0) {
    n--;
  }
  memcpy(buf, buf1 + 1, n);
  return n;
}

static unsigned
win_os_packet_write(const void* buf, unsigned size)
{
  DWORD n;
  char buf1[257];
  // first byte is report id
  buf1[0] = 0;
  memcpy(buf1 + 1, buf, size);
  if (WriteFile(hid_handle, buf1, delbin_os_packet_size + 1, &n, NULL) == 0) {
    unsigned err = GetLastError();
    fatal(MYNAME ": WriteFile of %u bytes failed with %u.  Size: %u Wrote: %d\n",
          delbin_os_packet_size + 1, err, size, (int) n);
  }
  if (n > size) {
    n = size;
  }
  return n;
}

delbin_os_ops_t delbin_os_ops = {
  win_os_init,
  win_os_deinit,
  win_os_packet_read,
  win_os_packet_write
};

#endif // HAVE_WDK

//-----------------------------------------------------------------------------
// MacOS X
#if __APPLE__

#undef HAVE_LIBUSB

#include <pthread.h>
#include <IOKit/IOKitLib.h>
#include <IOKit/IOCFPlugIn.h>
#include <IOKit/hid/IOHIDLib.h>
#include <mach/mach_error.h>

// IOHIDDeviceInterface121::getReport() does not work, it hangs the process
// in some sort of unkillable state.  So reading is done via a separate thread
// with a run loop and interrupt report callback. Yuck.

static IOHIDDeviceInterface122** device;
static pthread_t thread;
static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t cond = PTHREAD_COND_INITIALIZER;
static char* report_buf;
static char* packet_array[32];
static unsigned packet_array_head;
static unsigned packet_array_tail;
static CFRunLoopRef run_loop;

static void*
thread_func(void* run_loop_source)
{
  run_loop = CFRunLoopGetCurrent();
#if __cplusplus
  CFRunLoopAddSource(run_loop, (__CFRunLoopSource*) run_loop_source, kCFRunLoopDefaultMode);
#else
  CFRunLoopAddSource(run_loop, run_loop_source, kCFRunLoopDefaultMode);
#endif
  CFRunLoopRun();
  return NULL;
}

static void
interrupt_report_cb(void* target, IOReturn result, void* refcon, void* sender, UInt32 bufferSize)
{
  memcpy(packet_array[packet_array_head], report_buf, delbin_os_packet_size);
  pthread_mutex_lock(&mutex);
  if (packet_array_head == packet_array_tail) {
    pthread_cond_signal(&cond);
  }
  packet_array_head++;
  packet_array_head &= sizeofarray(packet_array) - 1;
  if (packet_array_head == packet_array_tail && global_opts.debug_level >= DBGLVL_M) {
    warning(MYNAME ": packet_array overrun, packets lost\n");
  }
  pthread_mutex_unlock(&mutex);
}

static void
mac_os_init(const char* fname)
{
  CFMutableDictionaryRef dict = IOServiceMatching(kIOHIDDeviceKey);
  io_service_t service;
  IOCFPlugInInterface** plugin;
  CFNumberRef cf_num;
  CFRunLoopSourceRef run_loop_source;
  int i;
  kern_return_t kr;
  HRESULT hr;
  IOReturn ir;
  SInt32 unused;

  i = VENDOR_ID;
  cf_num = CFNumberCreate(kCFAllocatorDefault, kCFNumberIntType, &i);
  CFDictionaryAddValue(dict, CFSTR(kIOHIDVendorIDKey), cf_num);
  CFRelease(cf_num);
  i = PRODUCT_ID;
  cf_num = CFNumberCreate(kCFAllocatorDefault, kCFNumberIntType, &i);
  CFDictionaryAddValue(dict, CFSTR(kIOHIDProductIDKey), cf_num);
  CFRelease(cf_num);
  service = IOServiceGetMatchingService(kIOMasterPortDefault, dict);
  if (service == 0) {
    fatal(MYNAME ": no DeLorme PN found\n");
  }
  kr = IOCreatePlugInInterfaceForService(
         service, kIOHIDDeviceUserClientTypeID, kIOCFPlugInInterfaceID, &plugin, &unused);
  if (kr) {
    fatal(MYNAME ": IOCreatePlugInInterfaceForService failed 0x%x\n", (int)kr);
  }
  IOObjectRelease(service);
  hr = (*plugin)->QueryInterface(plugin, CFUUIDGetUUIDBytes(kIOHIDDeviceInterfaceID122), (void**)&device);
  if (hr) {
    fatal(MYNAME ": QueryInterface failed 0x%x\n", (int)hr);
  }
  (*plugin)->Release(plugin);
  ir = (*device)->open(device, kIOHIDOptionsTypeSeizeDevice);
  if (ir)
    fatal(MYNAME ": device open failed 0x%x - %s\n", (int)ir,
          mach_error_string(ir));
  ir = (*device)->createAsyncEventSource(device, &run_loop_source);
  if (ir) {
    fatal(MYNAME ": createAsyncEventSource failed 0x%x\n", (int)ir);
  }
  delbin_os_packet_size = 64;
  report_buf = (char*)xmalloc(delbin_os_packet_size);
  for (i = sizeofarray(packet_array); i--;) {
    packet_array[i] = (char*)xmalloc(delbin_os_packet_size);
  }
  ir = (*device)->setInterruptReportHandlerCallback(
         device, report_buf, delbin_os_packet_size, interrupt_report_cb, NULL, NULL);
  if (ir) {
    fatal(MYNAME ": setInterruptReportHandlerCallback failed 0x%x\n", (int)ir);
  }
  i = pthread_create(&thread, NULL, thread_func, run_loop_source);
  if (i) {
    fatal(MYNAME ": pthread_create failed %d\n", i);
  }
}

static void
mac_os_deinit(void)
{
  void* unused;
  unsigned i;
  CFRunLoopStop(run_loop);
  pthread_join(thread, &unused);
  (*device)->Release(device);
  xfree(report_buf);
  for (i = sizeofarray(packet_array); i--;) {
    xfree(packet_array[i]);
  }
}

static unsigned
mac_os_packet_read(void* buf)
{
  pthread_mutex_lock(&mutex);
  while (packet_array_head == packet_array_tail) {
    pthread_cond_wait(&cond, &mutex);
  }
  memcpy(buf, packet_array[packet_array_tail++], delbin_os_packet_size);
  packet_array_tail &= sizeofarray(packet_array) - 1;
  pthread_mutex_unlock(&mutex);
  return delbin_os_packet_size;
}

static unsigned
mac_os_packet_write(const void* buf, unsigned size)
{
  IOReturn r = (*device)->setReport(
                 device, kIOHIDReportTypeOutput, 0, (void*)buf, size, 2000, NULL, NULL, NULL);
  if (r) {
    fatal("setReport failed 0x%x\n", (int)r);
  }
  return size;
}

delbin_os_ops_t delbin_os_ops = {
  mac_os_init,
  mac_os_deinit,
  mac_os_packet_read,
  mac_os_packet_write
};

#endif // __APPLE__

//-----------------------------------------------------------------------------
// libusb
#if HAVE_LIBUSB

#include <usb.h>

static struct usb_device* usb_dev;
static usb_dev_handle* usb_handle;
static int endpoint_in;
static int endpoint_out;

static void
libusb_os_init(const char* fname)
{
  struct usb_bus* bus;
  const struct usb_endpoint_descriptor* endpoint_desc;

  usb_init();
  usb_find_busses();
  usb_find_devices();
  for (bus = usb_busses; usb_dev == NULL && bus; bus = bus->next) {
    struct usb_device* d;
    for (d = bus->devices; d; d = d->next) {
      if (d->descriptor.idVendor == VENDOR_ID && d->descriptor.idProduct == PRODUCT_ID) {
        usb_dev = d;
        break;
      }
    }
  }
  if (usb_dev == NULL) {
    fatal(MYNAME ": no DeLorme PN found\n");
  }
  usb_handle = usb_open(usb_dev);
  if (usb_handle == NULL) {
    fatal(MYNAME ": %s\n", usb_strerror());
  }

  // Device has 1 configuration, 1 interface, 2 interrupt endpoints
  if (usb_claim_interface(usb_handle, 0) < 0) {
#if LIBUSB_HAS_DETACH_KERNEL_DRIVER_NP
    if (usb_detach_kernel_driver_np(usb_handle, 0) < 0) {
      warning(MYNAME ": %s\n", usb_strerror());
    }
    if (usb_claim_interface(usb_handle, 0) < 0)
#endif
    {
      const char* s = usb_strerror();
      usb_close(usb_handle);
      fatal(MYNAME ": %s\n", s);
    }
  }
  endpoint_desc = usb_dev->config[0].interface[0].altsetting[0].endpoint;
  delbin_os_packet_size = endpoint_desc[0].wMaxPacketSize;
  endpoint_in = endpoint_desc[0].bEndpointAddress;
  endpoint_out = endpoint_desc[1].bEndpointAddress;
  if ((endpoint_in & USB_ENDPOINT_DIR_MASK) == USB_ENDPOINT_OUT) {
    int t = endpoint_in;
    endpoint_in = endpoint_out;
    endpoint_out = t;
  }
}

static void
libusb_os_deinit(void)
{
  usb_release_interface(usb_handle, 0);
  usb_close(usb_handle);
}

static unsigned
libusb_os_packet_read(void* buf)
{
  int n = usb_interrupt_read(usb_handle, endpoint_in, (char *) buf, delbin_os_packet_size, 2000);
  if (n < 0) {
    fatal(MYNAME ": %s\n", usb_strerror());
  }
  return n;
}

static unsigned
libusb_os_packet_write(const void* buf, unsigned size)
{
  int n = usb_interrupt_write(usb_handle, endpoint_out, (char*)buf, size, 2000);
  if (n < 0) {
    fatal(MYNAME ": %s\n", usb_strerror());
  }
  return n;
}

#if HAVE_LINUX_HID
static const delbin_os_ops_t libusb_os_ops =
#else
delbin_os_ops_t delbin_os_ops =
#endif
{
  libusb_os_init,
  libusb_os_deinit,
  libusb_os_packet_read,
  libusb_os_packet_write
};

#endif // HAVE_LIBUSB

//-----------------------------------------------------------------------------
// Linux
#if HAVE_LINUX_HID

#include <unistd.h>
#include <fcntl.h>
#include <dirent.h>
#include <errno.h>
#include <sys/ioctl.h>
#include <linux/types.h>
#include <linux/hiddev.h>
#include <linux/hidraw.h>

// Read from hidraw, write to hiddev. Reading from hiddev does not work,
// and neither does writing to hidraw.
static int fd_hidraw;
static int fd_hiddev;

static int linuxhid_os_init_status;

static void
linuxhid_os_init(const char* fname)
{
  struct hidraw_devinfo info;
  struct hiddev_field_info finfo;
  DIR* dir = NULL;
  struct dirent* d;

  fd_hidraw = fd_hiddev = -1;
  if (fname && memcmp(fname, "hid:", 4) == 0) {
    char* raw_name = xstrdup(fname + 4);
    char* dev_name = strchr(raw_name, ',');
    if (dev_name == NULL) {
      fatal(MYNAME ": missing hiddev path\n");
    }
    *dev_name++ = 0;
    fd_hidraw = open(raw_name, O_RDONLY);
    if (fd_hidraw < 0) {
      fatal(MYNAME ": %s: %s\n", raw_name, strerror(errno));
    }
    fd_hiddev = open(dev_name, O_WRONLY);
    if (fd_hiddev < 0) {
      fatal(MYNAME ": %s: %s\n", dev_name, strerror(errno));
    }
    xfree(raw_name);
  } else {
    dir = opendir("/dev");
  }
  while (dir && (d = readdir(dir)) != NULL) {
    if (strncmp(d->d_name, "hidraw", 6) == 0) {
      int fd1, fd2;
      char raw_name[32];
      char dev_name[32];
      sprintf(raw_name, "/dev/%s", d->d_name);
      fd1 = open(raw_name, O_RDONLY);
      if (fd1 < 0) {
        if (global_opts.debug_level >= DBGLVL_M) {
          warning(MYNAME ": %s: %s\n", raw_name, strerror(errno));
        }
        continue;
      }
      sprintf(dev_name, "/dev/usb/hiddev%s", raw_name + sizeof("/dev/hidraw") - 1);
      fd2 = open(dev_name, O_WRONLY);
      if (fd2 < 0 && errno == ENOENT) {
        sprintf(dev_name, "/dev/hiddev%s", raw_name + sizeof("/dev/hidraw") - 1);
        fd2 = open(dev_name, O_WRONLY);
      }
      if (fd2 < 0) {
        if (global_opts.debug_level >= DBGLVL_M) {
          warning(MYNAME ": %s: %s\n", dev_name, strerror(errno));
        }
        close(fd1);
        continue;
      }
      if (ioctl(fd1, HIDIOCGRAWINFO, &info) == 0 &&
          info.vendor == VENDOR_ID && info.product == PRODUCT_ID) {
        fd_hidraw = fd1;
        fd_hiddev = fd2;
        break;
      }
      close(fd1);
      close(fd2);
    }
  }
  if (dir) {
    closedir(dir);
  }
  if (fd_hidraw < 0) {
    if (linuxhid_os_init_status == 0) {
      fatal(MYNAME ": no DeLorme PN found\n");
    }
    return;
  }
  finfo.report_type = HID_REPORT_TYPE_INPUT;
  finfo.report_id = 0;
  finfo.field_index = 0;
  if (ioctl(fd_hiddev, HIDIOCGFIELDINFO, &finfo) < 0) {
    warning(MYNAME ": HIDIOCGFIELDINFO: %s\n", strerror(errno));
    if (linuxhid_os_init_status == 0) {
      exit(1);
    }
    return;
  }
  delbin_os_packet_size = finfo.maxusage;
  linuxhid_os_init_status = 0;
}

static void
linuxhid_os_deinit(void)
{
  close(fd_hidraw);
  close(fd_hiddev);
}

static unsigned
linuxhid_os_packet_read(void* buf)
{
  int n = read(fd_hidraw, buf, delbin_os_packet_size);
  if (n < 0) {
    fatal(MYNAME ": %s\n", strerror(errno));
  }
  return n;
}

static unsigned
linuxhid_os_packet_write(const void* buf, unsigned size)
{
  struct hiddev_usage_ref_multi urefm;
  struct hiddev_report_info rinfo;
  const gbuint8* p = (const gbuint8*) buf;
  unsigned i;

  for (i = 0; i < size; i++) {
    urefm.values[i] = p[i];
  }
  urefm.num_values = size;
  memset(&urefm.uref, 0, sizeof(urefm.uref));
  urefm.uref.report_type = HID_REPORT_TYPE_OUTPUT;
  if (ioctl(fd_hiddev, HIDIOCSUSAGES, &urefm)) {
    fatal(MYNAME ": HIDIOCSUSAGES: %s\n", strerror(errno));
  }
  memset(&rinfo, 0, sizeof(rinfo));
  rinfo.report_type = HID_REPORT_TYPE_OUTPUT;
  if (ioctl(fd_hiddev, HIDIOCSREPORT, &rinfo)) {
    fatal(MYNAME ": HIDIOCSREPORT: %s\n", strerror(errno));
  }
  return size;
}

static const delbin_os_ops_t linuxhid_os_ops = {
  linuxhid_os_init,
  linuxhid_os_deinit,
  linuxhid_os_packet_read,
  linuxhid_os_packet_write
};

static void
linux_os_init(const char* fname)
{
  // tell linuxhid_os_init not to exit
  linuxhid_os_init_status = 1;
  linuxhid_os_init(fname);
  if (linuxhid_os_init_status == 0) {
    delbin_os_ops = linuxhid_os_ops;
  } else {
#if HAVE_LIBUSB
    if (global_opts.debug_level >= DBGLVL_M) {
      warning(MYNAME ": HID init failed, falling back to libusb\n");
    }
    delbin_os_ops = libusb_os_ops;
    delbin_os_ops.init(fname);
#else
    fatal(MYNAME ": no DeLorme PN found\n");
#endif
  }
}

delbin_os_ops_t delbin_os_ops = {
  linux_os_init,
  NULL,
  NULL,
  NULL
};

#endif // HAVE_LINUX_HID

//-----------------------------------------------------------------------------
// stubs
#if !(HAVE_WDK || __APPLE__ || HAVE_LIBUSB || HAVE_LINUX_HID)
static void
stub_os_init(const char* fname)
{
  fatal(MYNAME ": OS not supported\n");
}
static void
stub_os_deinit(void)
{
}
static unsigned
stub_os_packet_read(void* buf)
{
  return 0;
}
static unsigned
stub_os_packet_write(const void* buf, unsigned size)
{
  return 0;
}
delbin_os_ops_t delbin_os_ops = {
  stub_os_init,
  stub_os_deinit,
  stub_os_packet_read,
  stub_os_packet_write
};
#endif
// end OS device I/O implementations section
//=============================================================================

static const int track_color_bgr[] = {
  0x0000ff, // red
  0x00ffff, // yellow
  0x008000, // green
  0xff0000, // blue
  0x808080, // gray
  0xffffff, // white
  0,        // black
  0xffff00, // cyan
  0xff00ff, // magenta
  0x00a5ff, // orange
  0x82004b, // indigo
  0xeea5ee  // violet
};

static int track_color(unsigned i)
{
  int bgr = -1;
  if (i < sizeofarray(track_color_bgr)) {
    bgr = track_color_bgr[i];
  }
  return bgr;
}

static unsigned track_color_index(int bgr)
{
  unsigned i = sizeofarray(track_color_bgr);
  do {
    i--;
  } while (i != 0 && track_color_bgr[i] != bgr);
  return i;
}

static const char* const waypoint_symbol_name[] = {
  // 0
  "Red Map Pin",
  "Dark Red Map Pin",
  "Yellow Map Pin",
  "Dark Yellow Map Pin",
  "Green Map Pin",
  "Dark Green Map Pin",
  "Turquoise Map Pin",
  "Dark Turquoise Map Pin",
  "Blue Map Pin",
  "Dark Blue Map Pin",
  // 10
  "Gray Map Pin",
  "Dark Gray Map Pin",
  "Red Flag",
  "Dark Red Flag",
  "Yellow Flag",
  "Dark Yellow Flag",
  "Green Flag",
  "Dark Green Flag",
  "Turquoise Flag",
  "Dark Turquoise Flag",
  // 20
  "Blue Flag",
  "Dark Blue Flag",
  "Gray Flag",
  "Dark Gray Flag",
  "Red Dot",
  "Dark Red Dot",
  "Yellow Dot",
  "Dark Yellow Dot",
  "Green Dot",
  "Dark Green Dot",
  // 30
  "Turquoise Dot",
  "Dark Turquoise Dot",
  "Blue Dot",
  "Dark Blue Dot",
  "Gray Dot",
  "Dark Gray Dot",
  "Small Red Dot",
  "Small Dark Red Dot",
  "Small Yellow Dot",
  "Small Dark Yellow Dot",
  // 40
  "Small Green Dot",
  "Small Dark Green Dot",
  "Small Turquoise Dot",
  "Small Dark Turquoise Dot",
  "Small Blue Dot",
  "Small Dark Blue Dot",
  "Small Gray Dot",
  "Small Dark Gray Dot",
  "Arrow Up",
  "Arrow Down",
  // 50
  "Arrow Left",
  "Arrow Right",
  "Arrow Up Left",
  "Arrow Up Right",
  "Arrow Down Left",
  "Arrow Down Right",
  "Green Star",
  "Yellow Square",
  "Red X",
  "Turquoise Circle",
  // 60
  "Purple Triangle",
  "American Flag",
  "Stop",
  "Parking",
  "First Aid",
  "Dining",
  "Railroad Crossing",
  "Heliport",
  "Restroom",
  "Information",
  // 70
  "Diver Down",
  "Exit",
  "Health Facility",
  "Police",
  "Post Office",
  "Mining",
  "Danger",
  "Money",
  "Exclamation",
  "Car",
  // 80
  "Jeep",
  "Truck",
  "Tow Truck",
  "Motor Home",
  "School Bus",
  "Four-wheeler",
  "Snowmobile",
  "Sailboat",
  "Powerboat",
  "Boat Launch",
  // 90
  "Anchor",
  "Buoy",
  "Shipwreck",
  "Glider Area",
  "Private Airport",
  "Public Airport",
  "Military Airport",
  "Military Base",
  "House",
  "Church",
  // 100
  "Building",
  "School",
  "Lighthouse",
  "Bridge",
  "Radio Tower",
  "Dam",
  "Tunnel",
  "Toll Booth",
  "Gas Station",
  "Lodging",
  // 110
  "Telephone",
  "Traffic Light",
  "Fire Hydrant",
  "Cemetery",
  "Picnic Table",
  "Tent",
  "Shelter",
  "Camper",
  "Fire",
  "Shower",
  // 120
  "Drinking Water",
  "Binoculars",
  "Camera",
  "Geocache",
  "Geocache Found",
  "Fishing Pole",
  "Ice Fishing Trap Set",
  "Ice Fishing Trap Up",
  "Moose",
  "Deer",
  // 130
  "Bear",
  "Bird",
  "Duck",
  "Fish",
  "Deer Tracks",
  "Animal Tracks",
  "Bird Tracks",
  "Birch Tree",
  "Evergreen Tree",
  "Deciduous Tree",
  // 140
  "Flower Garden",
  "Mountain",
  "Cave",
  "Beach",
  "Hiking",
  "Swimming",
  "Bicycling",
  "Kayaking",
  "Canoeing",
  "Water Skiing",
  // 150
  "Cross-country Skiing",
  "Downhill Skiing",
  "Ice Skating",
  "Dogsledding",
  "Shooting",
  "Golf Course",
  "Ballpark",
  // 157-182 added in PN-40 2.5 firmware
  "Cache Found",
  "Didn't Find It",
  "My Cache",
  // 160
  "Traditional Cache",
  "Multi-Cache",
  "Unknown Cache",
  "Letterbox Hybrid",
  "Whereigo Cache",
  "Event Cache",
  "Mega-Event Cache",
  "Cache In Trash Out Event",
  "EarthCache",
  "Virtual Cache",
  // 170
  "Webcam Cache",
  "Waymark",
  "NGS Benchmark",
  "Write Note",
  "Needs Maintenance",
  "Final Location",
  "Parking Area",
  "Question to Answer",
  "Reference Point",
  "Stages of a Multicache",
  // 180
  "Trailhead",
  "Temporarily Disable Listing",
  "Enable Listing",
  // 183-222 added in PN-40 2.7 firmware
  "Crane Truck",
  "Forest Fire",
  "Oil Derrick",
  "Wind Turbine",
  "Letter A",
  "Letter B",
  "Letter C",
  // 190
  "Letter D",
  "Letter E",
  "Letter F",
  "Letter G",
  "Letter H",
  "Letter I",
  "Letter J",
  "Letter K",
  "Letter L",
  "Letter M",
  // 200
  "Letter N",
  "Letter O",
  "Letter P",
  "Letter Q",
  "Letter R",
  "Letter S",
  "Letter T",
  "Letter U",
  "Letter V",
  "Letter W",
  // 210
  "Letter X",
  "Letter Y",
  "Letter Z",
  "Numeral 0",
  "Numeral 1",
  "Numeral 2",
  "Numeral 3",
  "Numeral 4",
  "Numeral 5",
  "Numeral 6",
  // 220
  "Numeral 7",
  "Numeral 8",
  "Numeral 9"
};

static const char*
waypoint_symbol(unsigned i)
{
  const char* p = NULL;
  if (i < sizeofarray(waypoint_symbol_name)) {
    p = waypoint_symbol_name[i];
  }
  return p;
}

static unsigned
waypoint_symbol_index(const char* name)
{
  static unsigned last_result;
  static char last_name[32];
  unsigned i = last_result;

  if (strncmp(name, last_name, sizeof(last_name)) != 0) {
    i = sizeofarray(waypoint_symbol_name);
    do {
      i--;
    } while (i != 0 && case_ignore_strcmp(name, waypoint_symbol_name[i]) != 0);
    strncpy(last_name, name, sizeof(last_name));
    last_result = i;
  }
  return i;
}

// vi: ts=4 sw=4 noexpandtab
