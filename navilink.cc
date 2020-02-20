/*
    NaviGPS serial protocol.

    Copyright (C) 2007 Tom Hughes, tom@compton.nu
    Copyright (C) 2008 Rodney Lorrimar, rodney@rodney.id.au

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


/* Based on description at http://wiki.splitbrain.org/navilink */
#include "defs.h"
#include "gbser.h"
#include "jeeps/gpsmath.h"
#include "navilink.h"
#include <QtCore/QThread>

#define MYNAME "NAVILINK"

static char* nuketrk = nullptr;
static char* nukerte = nullptr;
static char* nukewpt = nullptr;
static char* nukedlg = nullptr;
static char* poweroff = nullptr;
static char* datalog = nullptr;

static void* serial_handle = nullptr;
static gbfile* file_handle = nullptr;

static unsigned char* track_data;
static unsigned char* track_data_ptr;
static unsigned char* track_data_end;
static unsigned track_serial;
static Waypoint** route_waypts;
static unsigned* route_ids;
static unsigned route_id_ptr;

static enum {
  READING,
  WRITING
} operation = READING;

#define SERIAL_TIMEOUT        8000
#define CLEAR_DATALOG_TIME    7000

#define MAX_WAYPOINTS         1000
#define MAX_SUBROUTES         9
#define MAX_SUBROUTE_LENGTH   14
#define MAX_ROUTE_LENGTH      (MAX_SUBROUTES * MAX_SUBROUTE_LENGTH - 1)
#define MAX_READ_TRACKPOINTS  512
#define MAX_WRITE_TRACKPOINTS 127
#define MAX_READ_LOGPOINTS    256

#define PID_SYNC              0xd6
#define PID_ACK               0x0c
#define PID_NAK               0x00
#define PID_QRY_INFORMATION   0x20
#define PID_QRY_FW_VERSION    0xfe
#define PID_DATA              0x03
#define PID_ADD_A_WAYPOINT    0x3c
#define PID_QRY_WAYPOINTS     0x28
#define PID_QRY_ROUTE         0x24
#define PID_DEL_WAYPOINT      0x36
#define PID_DEL_ALL_WAYPOINT  0x37
#define PID_DEL_ROUTE         0x34
#define PID_DEL_ALL_ROUTE     0x35
#define PID_ADD_A_ROUTE       0x3d
#define PID_ERASE_TRACK       0x11
#define PID_READ_TRACKPOINTS  0x14
#define PID_WRITE_TRACKPOINTS 0x16
#define PID_CMD_OK            0xf3
#define PID_CMD_FAIL          0xf4
#define PID_QUIT              0xf2
#define PID_INFO_DATALOG      0x1c
#define PID_READ_DATALOG      0x14
#define PID_CLEAR_DATALOG     0x1b

static
const char* const icon_table[] = {
  "Star",
  "Flag",
  "House",
  "Left Sign",
  "Telegraph Pole",
  "People",
  "Fuel",
  "Phone",
  "Pole",
  "Mountain",
  "Water",
  "Tree",
  "Road Narrows",
  "Crossroads",
  "Road Fork",
  "Turn Right",
  "Turn Left",
  "Bird",
  "3D House",
  "Trig Point",
  "Tower",
  "Cable Car",
  "Church",
  "Telegraph Pole",
  "Skier",
  "Anchor",
  "Fish",
  "Fishes",
  "Rain",
  "Fisherman",
  "Tower",
  "Boats",
  "Boat",
  "Bicycle",
  "Railway Track",
  "Dollar Sign",
  "Bus",
  "Camera",
  "Fuel Pump",
  "Cup",
  "Merging Road",
  "Plane",
  "Red Cross",
  "House",
  "Parking"
};

static
QVector<arglist_t> navilink_args = {
  {
    "nuketrk", &nuketrk, "Delete all track points", nullptr, ARGTYPE_BOOL,
    ARG_NOMINMAX, nullptr
  },
  {
    "nukerte", &nukerte, "Delete all routes", nullptr, ARGTYPE_BOOL,
    ARG_NOMINMAX, nullptr
  },
  {
    "nukewpt", &nukewpt, "Delete all waypoints", nullptr, ARGTYPE_BOOL,
    ARG_NOMINMAX, nullptr
  },
  {
    "nukedlg", &nukedlg, "Clear the datalog", nullptr, ARGTYPE_BOOL,
    ARG_NOMINMAX, nullptr
  },
  {
    "datalog", &datalog, "Read from datalogger buffer",
    nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
  },
  {
    "power_off", &poweroff, "Command unit to power itself down",
    nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
  },
};

static void (*write_waypoint)(const Waypoint*) = nullptr;
static void (*write_track_start)(const route_head* track) = nullptr;
static void (*write_track_point)(const Waypoint* waypt) = nullptr;
static void (*write_track_end)(const route_head* track) = nullptr;
static void (*write_route_start)(const route_head* track) = nullptr;
static void (*write_route_point)(const Waypoint* waypt) = nullptr;
static void (*write_route_end)(const route_head* track) = nullptr;

static int
find_icon_from_descr(const QString& descr)
{
  for (unsigned int i = 0; i < sizeof(icon_table) / sizeof(const char*); i++) {
    if (0 == descr.compare(icon_table[i])) {
      return i;
    }
  }

  return 0;
}

static void
free_waypoints(Waypoint** waypts)
{
  for (Waypoint** wayptp = waypts; wayptp < waypts + MAX_WAYPOINTS; wayptp++) {
    if (*wayptp) {
      delete *wayptp;
    }
  }

  xfree(waypts);
}

static unsigned
compare_waypoints(const Waypoint* waypt1, const Waypoint* waypt2)
{
  return waypt1->latitude == waypt2->latitude &&
         waypt1->longitude == waypt2->longitude &&
         waypt1->altitude == waypt2->altitude &&
         waypt1->shortname == waypt2->shortname;
}

unsigned
navilink_checksum_packet(const unsigned char* packet, unsigned length)
{
  unsigned checksum = 0;

  while (length-- > 0) {
    checksum += *packet++;
  }

  return checksum & 0x7fff;
}

#ifdef NAVILINK_DEBUG

static void
dump_packet(char* prefix, unsigned char* packet, unsigned length)
{
  unsigned i;

  for (i = 0; i < length; i++) {
    if ((i % 16) == 0) {
      fprintf(stderr, "%s %08x :", prefix, i);
    }
    fprintf(stderr, " %02x", packet[i]);
    if ((i % 16) == 15 || i == length - 1) {
      fprintf(stderr, "\n");
    }
  }

  fprintf(stderr, "\n");
}

#endif

static void
write_packet(unsigned type, const void* payload, unsigned length)
{
  auto* packet = (unsigned char*) xmalloc(length + 9);

  packet[0] = 0xa0;
  packet[1] = 0xa2;
  le_write16(packet + 2, length + 1);
  packet[4] = type;
  memcpy(packet + 5, payload, length);
  le_write16(packet + length + 5, navilink_checksum_packet(packet + 4, length + 1));
  packet[length + 7] = 0xb0;
  packet[length + 8] = 0xb3;

#ifdef NAVILINK_DEBUG
  dump_packet(">>>", packet + 4, length + 1);
#endif

  if (gbser_write(serial_handle, packet, length + 9) != gbser_OK) {
    fatal(MYNAME ": Write error\n");
  }

  xfree(packet);
}

static unsigned
read_word()
{
  unsigned char buffer[2];

  if (gbser_read_wait(serial_handle, buffer, 2, SERIAL_TIMEOUT) != 2) {
    fatal(MYNAME ": Read error\n");
  }

  return (buffer[1] << 8) | buffer[0];
}

/*
 * Read a protocol packet into payload.
 *
 * handle_nak determines behaviour when a PID_NAK packet is read from
 * the device:
 *  - if handle_nak is false, a fatal error will be raised.
 *  - if handle_nak is true, read_packet will simply return false.
 *
 * Returns true if the packet was successfully read into payload.
 */
static bool
read_packet(unsigned type, void* payload,
            int minlength, int maxlength,
            bool handle_nak)
{
  if (read_word() != 0xa2a0) {
    fatal(MYNAME ": Protocol error: Bad packet header."
          " Is your NaviGPS in NAVILINK mode?\n");
  }

  int size;
  if ((size = read_word()) <= minlength) {
    fatal(MYNAME ": Protocol error: Packet too short\n");
  }

  auto* data = (unsigned char*) xmalloc(size);

  if (gbser_read_wait(serial_handle, data, size, SERIAL_TIMEOUT) != size) {
    fatal(MYNAME ": Read error reading %d byte payload\n", size);
  }

#ifdef NAVILINK_DEBUG
  dump_packet("<<<", data, size);
#endif

  if (data[0] != type) {
    if (handle_nak && data[0] == PID_NAK) {
      return false;
    }

    fatal(MYNAME ": Protocol error: Bad packet type (expected 0x%02x but got 0x%02x)\n", type, data[0]);
  }

  unsigned checksum;
  if ((checksum = read_word()) != navilink_checksum_packet(data, size)) {
    fatal(MYNAME ": Checksum error - expected %x got %x\n",
          navilink_checksum_packet(data, size), checksum);
  }

  if (read_word() != 0xb3b0) {
    fatal(MYNAME ": Protocol error: Bad packet trailer\n");
  }

  if (size - 1 > maxlength) {
    memcpy(payload, data + 1, maxlength);
  } else {
    memcpy(payload, data + 1, size - 1);
  }

  xfree(data);

  return true;
}

static QDateTime
decode_datetime(const unsigned char* buffer)
{
  QTime tm(buffer[3], buffer[4], buffer[5]);
  QDate dt(2000 + buffer[0], buffer[1], buffer[2]);
  return QDateTime(dt, tm, Qt::UTC);
}

static void
encode_datetime(time_t datetime, unsigned char* buffer)
{
  struct tm* tm;

  if ((tm = gmtime(&datetime)) != nullptr) {
    buffer[0] = tm->tm_year - 100;
    buffer[1] = tm->tm_mon + 1;
    buffer[2] = tm->tm_mday;
    buffer[3] = tm->tm_hour;
    buffer[4] = tm->tm_min;
    buffer[5] = tm->tm_sec;
  } else {
    memset(buffer, 0, 6);
  }
}

static void
decode_position(const unsigned char* buffer, Waypoint* waypt)
{
  waypt->latitude = le_read32(buffer + 0) / 10000000.0;
  waypt->longitude = le_read32(buffer + 4) / 10000000.0;
  waypt->altitude = FEET_TO_METERS(le_read16(buffer + 8));
}

static void
encode_position(const Waypoint* waypt, unsigned char* buffer)
{
  le_write32(buffer + 0, (int)(waypt->latitude * 10000000));
  le_write32(buffer + 4, (int)(waypt->longitude * 10000000));
  le_write16(buffer + 8, METERS_TO_FEET(waypt->altitude));
}

static unsigned
decode_waypoint_id(const unsigned char* buffer)
{
  unsigned id = le_read16(buffer + 2);

  if (id >= MAX_WAYPOINTS) {
    fatal(MYNAME ": Invalid waypoint ID\n");
  }

  return id;
}

static Waypoint*
decode_waypoint(const unsigned char* buffer)
{
  auto* waypt = new Waypoint;

  decode_position(buffer + 12, waypt);
  char* s = xstrdup((char*)buffer + 4);
  waypt->shortname = s;
  xfree(s);
  waypt->icon_descr = icon_table[buffer[28]];
  waypt->SetCreationTime(decode_datetime(buffer + 22));

  return waypt;
}

static void
encode_waypoint(const Waypoint* waypt, unsigned char* buffer)
{
  buffer[0] = 0x00;
  buffer[1] = 0x40;
  le_write16(buffer + 2, 0);
  strncpy((char*)buffer + 4, CSTRc(waypt->shortname), 6);
  buffer[10] = 0;
  buffer[11] = 0;
  encode_position(waypt, buffer + 12);
  encode_datetime(waypt->GetCreationTime().toTime_t(), buffer + 22);
  buffer[28] = find_icon_from_descr(waypt->icon_descr);
  buffer[29] = 0;
  buffer[30] = 0x00;
  buffer[31] = 0x7e;
}

static Waypoint*
decode_trackpoint(const unsigned char* buffer)
{
  auto* waypt = new Waypoint;

  decode_position(buffer + 12, waypt);
  waypt->SetCreationTime(decode_datetime(buffer + 22));
  WAYPT_SET(waypt, course, le_read16(buffer + 2));
  WAYPT_SET(waypt, speed, KPH_TO_MPS(buffer[29] * 2));

  return waypt;
}

static void
encode_trackpoint(const Waypoint* waypt, unsigned serial, unsigned char* buffer)
{
  double x;
  double y;
  int32  z;
  char   zc;

  GPS_Math_WGS84_To_UTM_EN(waypt->latitude, waypt->longitude, &x, &y, &z, &zc);

  le_write16(buffer + 0, serial);
  le_write16(buffer + 2, WAYPT_GET(waypt, course, 0));
  le_write32(buffer + 4, x);
  le_write32(buffer + 8, y);
  encode_position(waypt, buffer + 12);
  encode_datetime(waypt->GetCreationTime().toTime_t(), buffer + 22);
  buffer[28] = z;
  buffer[29] = MPS_TO_KPH(WAYPT_GET(waypt, speed, 0) / 2);
  buffer[30] = 0x5a;
  buffer[31] = 0x7e;
}

static Waypoint**
serial_read_waypoints()
{
  Waypoint**       waypts = nullptr;
  unsigned char  information[32];

  if (global_opts.masked_objective & RTEDATAMASK) {
    waypts = (Waypoint**) xcalloc(MAX_WAYPOINTS, sizeof(Waypoint*));
  }

  write_packet(PID_QRY_INFORMATION, nullptr, 0);
  read_packet(PID_DATA, information,
              sizeof(information), sizeof(information),
              false);

  unsigned short total = le_read16(information + 0);

  for (unsigned short start = 0; start < total; start += 32) {
    unsigned short count = total - start;
    unsigned char  payload[7];

    if (count > 32) {
      count = 32;
    }

    le_write32(payload + 0, start);
    le_write16(payload + 4, count);
    payload[6] = 1;

    write_packet(PID_QRY_WAYPOINTS, payload, sizeof(payload));

    auto*  waypoints = (unsigned char*) xmalloc(count * 32);

    read_packet(PID_DATA, waypoints, count * 32, count * 32, false);

    for (unsigned char*  w = waypoints; w < waypoints + count * 32; w = w + 32) {
      if (global_opts.masked_objective & WPTDATAMASK) {
        waypt_add(decode_waypoint(w));
      }
      if (global_opts.masked_objective & RTEDATAMASK) {
        waypts[decode_waypoint_id(w)] = decode_waypoint(w);
      }
    }

    xfree(waypoints);

    if (global_opts.verbose_status) {
      waypt_status_disp(total, start + count);
    }
  }

  return waypts;
}

static unsigned int
serial_write_waypoint_packet(const Waypoint* waypt)
{
  unsigned char data[32];
  unsigned char id[2];

  encode_waypoint(waypt, data);
  write_packet(PID_ADD_A_WAYPOINT, data, sizeof(data));
  if (!read_packet(PID_DATA, id, sizeof(id), sizeof(id), true)) {
    fatal(MYNAME ": Could not write waypoint.\n");
  }

  return le_read16(id);
}

static void
serial_write_waypoint(const Waypoint* waypt)
{
  serial_write_waypoint_packet(waypt);
}

static void
serial_read_track()
{
  unsigned char  information[32];

  write_packet(PID_QRY_INFORMATION, nullptr, 0);
  read_packet(PID_DATA, information,
              sizeof(information), sizeof(information),
              false);

  unsigned int address = le_read32(information + 4);
  unsigned short total = le_read16(information + 12);

  auto* track = new route_head;
  track_add_head(track);

  while (total > 0) {
    unsigned short count = total < MAX_READ_TRACKPOINTS ? total : MAX_READ_TRACKPOINTS;
    unsigned char  payload[7];

    le_write32(payload + 0, address);
    le_write16(payload + 4, count * 32);
    payload[6] = 0x00;

    write_packet(PID_READ_TRACKPOINTS, payload, sizeof(payload));

    auto*  trackpoints = (unsigned char*) xmalloc(count * 32);

    read_packet(PID_DATA, trackpoints, count * 32, count * 32, false);
    write_packet(PID_ACK, nullptr, 0);

    for (unsigned char*  t = trackpoints; t < trackpoints + count * 32; t = t + 32) {
      track_add_wpt(track, decode_trackpoint(t));
    }

    xfree(trackpoints);

    address = address + count * 32;
    total = total - count;
  }
}

static void
serial_write_track()
{
  unsigned char  information[32];
  unsigned char  data[7];

  write_packet(PID_QRY_INFORMATION, nullptr, 0);
  read_packet(PID_DATA, information,
              sizeof(information), sizeof(information),
              false);

  unsigned int address = le_read32(information + 4);
  unsigned short total = le_read16(information + 12);

  le_write32(data + 0, address + total * 32);
  le_write16(data + 4, track_data_ptr - track_data);
  data[6] = 0x00;

  write_packet(PID_WRITE_TRACKPOINTS, data, sizeof(data));
  QThread::usleep(10000);
  write_packet(PID_DATA, track_data, track_data_ptr - track_data);
  read_packet(PID_CMD_OK, nullptr, 0, 0, false);

  track_data_ptr = track_data;
}

static void
serial_write_track_start(const route_head*)
{
  track_data = (unsigned char*) xmalloc(MAX_WRITE_TRACKPOINTS * 32);
  track_data_ptr = track_data;
  track_data_end = track_data + MAX_WRITE_TRACKPOINTS * 32;
}

static void
serial_write_track_point(const Waypoint* waypt)
{
  if (track_data_ptr >= track_data_end) {
    serial_write_track();
  }

  encode_trackpoint(waypt, 0, track_data_ptr);

  track_data_ptr += 32;
}

static void
serial_write_track_end(const route_head*)
{
  if (track_data_ptr > track_data) {
    serial_write_track();
  }

  xfree(track_data);
}

static void
serial_read_routes(Waypoint** waypts)
{
  unsigned char information[32];

  write_packet(PID_QRY_INFORMATION, nullptr, 0);
  read_packet(PID_DATA, information,
              sizeof(information), sizeof(information),
              false);

  unsigned char routec = information[2];

  for (unsigned char r = 0; r < routec; r++) {
    unsigned char payload[7];
    unsigned char routedata[320];

    le_write32(payload + 0, r);
    le_write16(payload + 2, 0);
    payload[6] = 0x01;

    write_packet(PID_QRY_ROUTE, payload, sizeof(payload));
    read_packet(PID_DATA, routedata, 64, sizeof(routedata), false);

    auto* route = new route_head;
    route->rte_num = routedata[2];
    route->rte_name = (char*)routedata + 4;
    route_add_head(route);

    for (int sr = 0; sr < MAX_SUBROUTES; sr++) {
      for (int w = 0; w < MAX_SUBROUTE_LENGTH; w++) {
        unsigned short id = le_read16(routedata + 34 + 32 * sr + 2 *w);

        if (id == 0xffffu) {
          w = MAX_SUBROUTE_LENGTH;
          sr = MAX_SUBROUTES;
        } else if (id >= MAX_WAYPOINTS) {
          fatal(MYNAME ": Invalid waypoint ID in route\n");
        } else if (waypts[id] == nullptr) {
          fatal(MYNAME ": Non-existent waypoint in route\n");
        } else {
          route_add_wpt(route, new Waypoint(*waypts[id]));
        }
      }
    }
  }
}

static void
serial_write_route_start(const route_head* route)
{
  route_ids = (unsigned int*) xmalloc(route->rte_waypt_ct * sizeof(unsigned));
  route_id_ptr = 0;
}

static void
serial_write_route_point(const Waypoint* waypt)
{
  unsigned w;

  for (w = 0; w < MAX_WAYPOINTS; w++) {
    if (route_waypts[w] && compare_waypoints(waypt, route_waypts[w])) {
      break;
    }
  }

  if (w == MAX_WAYPOINTS) {
    w = serial_write_waypoint_packet(waypt);
    route_waypts[w] = new Waypoint(*waypt);
  }

  route_ids[route_id_ptr++] = w;
}

static void
serial_write_route_end(const route_head* route)
{
  unsigned char id[1];

  QString rte_name = route->rte_name;
  if (rte_name == nullptr) {
    rte_name = "NO NAME";
  }
  if (route_id_ptr > MAX_ROUTE_LENGTH) {
    fatal(MYNAME ": Route %s too long\n", qPrintable(route->rte_name));
  }

  unsigned src = (route_id_ptr + MAX_SUBROUTE_LENGTH) / MAX_SUBROUTE_LENGTH;
  auto* data = (unsigned char*) xmalloc(32 + src * 32);

  le_write16(data + 0, 0x2000);
  data[2] = 0;
  data[3] = 0x20;
  memset(data + 4, 0, 14);
  strncpy((char*)data + 4, CSTR(rte_name), 13);
  data[18] = 0;
  data[19] = 0;
  le_write32(data + 20, 0);
  le_write32(data + 24, 0);
  le_write16(data + 28, 0);
  data[30] = 0x7b;
  data[31] = 0x77;

  for (unsigned sr = 0; sr < src; sr++) {
    unsigned char* srdata = data + 32 + 32 * sr;
    unsigned      pt_offset = MAX_SUBROUTE_LENGTH * sr;

    le_write16(srdata + 0, 0x2010);

    for (unsigned pt = 0; pt < MAX_SUBROUTE_LENGTH; pt++) {
      if (pt_offset + pt < route_id_ptr) {
        le_write16(srdata + 2 + 2 * pt, route_ids[pt_offset + pt]);
      } else {
        le_write16(srdata + 2 + 2 * pt, 0xffffu);
      }
    }

    srdata[30] = 0x7f;
    srdata[31] = 0x77;
  }

  write_packet(PID_ADD_A_ROUTE, data, 32 + src * 32);
  if (!read_packet(PID_DATA, id, sizeof(id), sizeof(id), true)) {
    fatal(MYNAME ": Could not add route.\n");
  }

  xfree(data);
  xfree(route_ids);
}

static int
decode_sbp_msec(const unsigned char* buffer)
{
  int msec = le_read16(buffer);
  return (msec % 1000);
}

static time_t
decode_sbp_datetime_packed(const unsigned char* buffer)
{
  /*
   * Packed_Date_Time_UTC:
   *   bit 31..22 :year*12+month (10 bits) :   real year= year+2000
   *   bit17.21: day (5bits)
   *   bit12.16: hour (5bits)
   *   bit6..11: min  (6bits)
   *   bit0..5 : sec  (6bits)
   *
   * 0        1        2        3
   * 01234567 01234567 01234567 01234567
   * ........ ........ ........ ........
   * SSSSSSMM MMMMHHHH Hdddddmm mmmmmmmm
   */

  struct tm tm;

  memset(&tm, 0, sizeof(tm));

  tm.tm_sec = buffer[0] & 0x3F;
  tm.tm_min = ((buffer[0] & 0xC0) >> 6) | ((buffer[1] & 0x0F) << 2);
  tm.tm_hour = ((buffer[1] & 0xF0) >> 4) | ((buffer[2] & 0x01) << 4);
  tm.tm_mday = (buffer[2] & 0x3E) >> 1;
  int months = ((buffer[2] & 0xC0) >> 6) | buffer[3] << 2;
  tm.tm_mon = months % 12 - 1;
  tm.tm_year = 100 + months / 12;

  return mkgmtime(&tm);
}

static void
decode_sbp_position(const unsigned char* buffer, Waypoint* waypt)
{
  waypt->latitude = le_read32(buffer + 0) / 10000000.0;
  waypt->longitude = le_read32(buffer + 4) / 10000000.0;
  waypt->altitude = le_read32(buffer + 8) / 100.0;
}

Waypoint*
navilink_decode_logpoint(const unsigned char* buffer)
{
  auto* waypt = new Waypoint;

  waypt->hdop = (buffer[0]) * 0.2f;
  waypt->sat = buffer[1];
  waypt->SetCreationTime(decode_sbp_datetime_packed(buffer + 4),
                         decode_sbp_msec(buffer + 2));
  decode_sbp_position(buffer + 12, waypt);
  WAYPT_SET(waypt, speed, le_read16(buffer + 24) * 0.01f);
  WAYPT_SET(waypt, course, le_read16(buffer + 26) * 0.01f);

  return waypt;
}

/*
 * The datalog is a circular buffer, so it may be necessary to glue
 * together two segments. This function queries the device for the
 * circular buffer pointers, and returns two pairs of address/length.
 * If there is only one segment (i.e. the datalog has not yet wrapped
 * around), then seg2_addr and seg2_len will be zero.
 */
static void
read_datalog_info(unsigned int* seg1_addr, unsigned int* seg1_len,
                  unsigned int* seg2_addr, unsigned int* seg2_len)
{
  unsigned char  info[16];

  write_packet(PID_INFO_DATALOG, nullptr, 0);
  read_packet(PID_DATA, info, sizeof(info), sizeof(info), false);

  unsigned int flash_start_addr = le_read32(info);
  unsigned int flash_length = le_read32(info + 4);
  unsigned int data_start_addr = le_read32(info + 8);
  unsigned int next_blank_addr = le_read32(info + 12);

  if (data_start_addr > next_blank_addr) {
    /* usually there are two segments to be read */
    *seg1_addr = data_start_addr;
    *seg1_len = flash_start_addr + flash_length - *seg1_addr;
    *seg2_addr = flash_start_addr;
    *seg2_len = next_blank_addr - flash_start_addr;
  } else {
    /* hasn't wrapped around yet, only one segment */
    *seg1_addr = data_start_addr;
    *seg1_len = next_blank_addr - data_start_addr;
    *seg2_addr = 0;
    *seg2_len = 0;
  }

  if (*seg1_len & 0x1F || *seg2_len & 0x1F) {
    fatal(MYNAME ": Protocol error: datalog lengths %u, %u "
          "not aligned to 32 bytes\n", *seg1_len, *seg2_len);
  }
}

static void
read_datalog_records(route_head* track,
                     unsigned int start_addr, unsigned int len)
{
  unsigned char  payload[7];

  /* The protocol only supports reading 256 logpoints at once, so
   * read small chunks until none left. */
  while (len > 0) {
  unsigned char  logpoints[MAX_READ_LOGPOINTS * SBP_RECORD_LEN];
  unsigned int   logpoints_len = len > MAX_READ_LOGPOINTS ? MAX_READ_LOGPOINTS : len;

    le_write32(payload, start_addr);
    le_write16(payload + 4, logpoints_len);
    payload[6] = 0x01;

    write_packet(PID_READ_DATALOG, payload, sizeof(payload));
    read_packet(PID_DATA, logpoints, logpoints_len, logpoints_len, false);
    write_packet(PID_ACK, nullptr, 0);

    for (unsigned char* p = logpoints; p < logpoints + logpoints_len; p += 32) {
      track_add_wpt(track, navilink_decode_logpoint(p));
    }

    len -= logpoints_len;
    start_addr += logpoints_len;
  }
}

static void
serial_read_datalog()
{
  unsigned int seg1_addr;
  unsigned int seg1_len;
  unsigned int seg2_addr;
  unsigned int seg2_len;

  read_datalog_info(&seg1_addr, &seg1_len, &seg2_addr, &seg2_len);

  auto* track = new route_head;
  track_add_head(track);

  if (seg1_len) {
    read_datalog_records(track, seg1_addr, seg1_len);
  }

  if (seg2_len) {
    read_datalog_records(track, seg2_addr, seg2_len);
  }
}

static void
file_read()
{
  unsigned char data[32];
  route_head*    track = nullptr;

  while (gbfread(data, sizeof(data), 1, file_handle) == 1) {
    switch (le_read16(data)) {
    case 0x2000:
      fatal(MYNAME ": Route objects not supported in file sources\n");
      break;
    case 0x2010:
      fatal(MYNAME ": Subroute objects not supported in file sources\n");
      break;
    case 0x4000:
      if (global_opts.masked_objective & WPTDATAMASK) {
        waypt_add(decode_waypoint(data));
      }
      break;
    default:
      if (global_opts.masked_objective & TRKDATAMASK) {
        if (track == nullptr) {
          track = new route_head;
          track_add_head(track);
        }

        track_add_wpt(track, decode_trackpoint(data));
      }
      break;
    }
  }
}

static void
file_write_waypoint(const Waypoint* waypt)
{
  unsigned char data[32];

  encode_waypoint(waypt, data);
  gbfwrite(data, sizeof(data), 1, file_handle);
}

static void
file_write_track_start(const route_head*)
{
  track_serial = 1;
}

static void
file_write_track_point(const Waypoint* waypt)
{
  unsigned char data[32];

  encode_trackpoint(waypt, track_serial++, data);
  gbfwrite(data, sizeof(data), 1, file_handle);
}

static void
file_write_track_end(const route_head*)
{
}

static void
file_write_route_start(const route_head*)
{
  fatal(MYNAME ": Can't write routes to a file\n");
}

static void
file_write_route_point(const Waypoint*)
{
}

static void
file_write_route_end(const route_head*)
{
}

static void
nuke()
{
  if (nuketrk) {
    unsigned char information[32];
    unsigned char data[7];

    write_packet(PID_QRY_INFORMATION, nullptr, 0);
    read_packet(PID_DATA, information,
                sizeof(information), sizeof(information),
                false);

    le_write32(data + 0, le_read32(information + 4));
    le_write16(data + 4, 0);
    data[6] = 0;

    write_packet(PID_ERASE_TRACK, data, sizeof(data));
    read_packet(PID_CMD_OK, nullptr, 0, 0, false);
  }

  if (nukerte) {
    unsigned char data[4];

    le_write32(data, 0x00f00000);
    write_packet(PID_DEL_ALL_ROUTE, data, sizeof(data));
    if (!read_packet(PID_ACK, nullptr, 0, 0, true)) {
      fatal(MYNAME ": Could not nuke all routes.\n");
    }
  }

  if (nukewpt) {
    unsigned char data[4];

    le_write32(data, 0x00f00000);
    write_packet(PID_DEL_ALL_WAYPOINT, data, sizeof(data));
    if (!read_packet(PID_ACK, nullptr, 0, 0, true)) {
      fatal(MYNAME ": You must nuke all routes before nuking waypoints.\n");
      /* perhaps a better action would be to nuke routes for user.
       * i.e. set nukerte when nukewpt is set */
    }
  }

  if (nukedlg) {
    write_packet(PID_CLEAR_DATALOG, nullptr, 0);
    /* The flash erase operation is time-consuming. Each sector (64KB)
     * takes around 1 second.  The total sectors for SBP is 10.
     * So give the device some time to clear its datalog, in addition
     * to SERIAL_TIMEOUT, which applies to read_packet() */
    QThread::usleep(CLEAR_DATALOG_TIME * 1000);
    read_packet(PID_ACK, nullptr, 0, 0, false);
  }
}

static void
navilink_common_init(const QString& name)
{
  if (gbser_is_serial(qPrintable(name))) {
    if ((serial_handle = gbser_init(qPrintable(name))) == nullptr) {
      fatal(MYNAME ": Could not open serial port %s\n", qPrintable(name));
    }

    if (gbser_set_port(serial_handle, 115200, 8, 0, 1) != gbser_OK) {
      fatal(MYNAME ": Can't configure port\n");
    }

    write_packet(PID_SYNC, nullptr, 0);
    read_packet(PID_ACK, nullptr, 0, 0, false);

    /* nuke data before writing */
    if (operation == WRITING) {
      nuke();
    }

    write_waypoint = serial_write_waypoint;
    write_track_start = serial_write_track_start;
    write_track_point = serial_write_track_point;
    write_track_end = serial_write_track_end;
    write_route_start = serial_write_route_start;
    write_route_point = serial_write_route_point;
    write_route_end = serial_write_route_end;
  } else {
    const char* mode = operation == READING ? "r" : "w+";
    file_handle = gbfopen(name, mode, MYNAME);

    write_waypoint = file_write_waypoint;
    write_track_start = file_write_track_start;
    write_track_point = file_write_track_point;
    write_track_end = file_write_track_end;
    write_route_start = file_write_route_start;
    write_route_point = file_write_route_point;
    write_route_end = file_write_route_end;
  }

}

static void
navilink_rd_init(const QString& name)
{
  operation = READING;
  navilink_common_init(name);
}

static void
navilink_wr_init(const QString& name)
{
  operation = WRITING;
  navilink_common_init(name);
}

static void
navilink_deinit()
{
  if (serial_handle) {
    /* nuke data after reading */
    if (operation == READING) {
      nuke();
    }

    if (poweroff) {
      write_packet(PID_QUIT, nullptr, 0);
    }

    gbser_deinit(serial_handle);
  }

  if (file_handle) {
    gbfclose(file_handle);
  }

}

static void
navilink_read()
{
  if (datalog) {
    if (global_opts.masked_objective & TRKDATAMASK) {
      if (serial_handle) {
        serial_read_datalog();
      } else if (file_handle) {
        fatal(MYNAME ": Not supported. Use SBP format.\n");
      }
    }
  } else {
    if (serial_handle) {
      Waypoint** waypts = nullptr;

      if (global_opts.masked_objective & (WPTDATAMASK|RTEDATAMASK)) {
        waypts = serial_read_waypoints();
      }

      if (global_opts.masked_objective & TRKDATAMASK) {
        serial_read_track();
      }

      if (global_opts.masked_objective & RTEDATAMASK) {
        serial_read_routes(waypts);
      }

      if (waypts) {
        free_waypoints(waypts);
      }
    } else if (file_handle) {
      file_read();
    }
  }
}

static void
navilink_write()
{
  if (datalog)  {
    fatal(MYNAME ": Writing to datalog not supported.\n");
  }

  switch (global_opts.objective) {
  case trkdata:
    track_disp_all(write_track_start,
                   write_track_end,
                   write_track_point);
    break;
  case wptdata:
    waypt_disp_all(write_waypoint);
    break;
  case rtedata:
    if (serial_handle) {
      route_waypts = serial_read_waypoints();
    }
    route_disp_all(write_route_start,
                   write_route_end,
                   write_route_point);
    if (route_waypts) {
      free_waypoints(route_waypts);
      route_waypts = nullptr;
    }
    break;
  default:
    fatal(MYNAME ": Unknown objective.\n");
  }
}

ff_vecs_t navilink_vecs = {
  ff_type_serial,
  FF_CAP_RW_ALL,
  navilink_rd_init,
  navilink_wr_init,
  navilink_deinit,
  navilink_deinit,
  navilink_read,
  navilink_write,
  nullptr,
  &navilink_args,
  CET_CHARSET_ASCII, 0	/* CET-REVIEW */
  , NULL_POS_OPS,
  nullptr
};
