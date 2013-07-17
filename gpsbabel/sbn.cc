/*
    Locosys NaviGPS GT-31/BGT-31 SiRF binary logging format (SBN)

    Copyright (C) 2008  Rodney Lorrimar <rodney@rodney.id.au>
    Copyright (C) 2005  Robert Lipe, robertlipe@usa.net

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
#include "navilink.h"
#include <ctype.h>

#define MYNAME "sbn"

static gbfile *file_handle = NULL;

static
arglist_t sbn_args[] = {
  ARG_TERMINATOR
};


/**********************************************************************/

/* Packets are encoded according to the SiRF Binary Protocol.
 *
 *   http://www.navmanwirelessoem.com/oem/customer-support/oem-ne
 *   ws/product-briefs-and-data-sheets/jupiter-32-xlp-new2/sirf-b
 *   inary-protocol-reference-manual
 *
 * The important packet is "Geodetic Navigation Data" (Message ID 41).
 */

#define PID_SBN              41
#define SBN_RECORD_LEN       97 /* 91 plus three two shorts added by Locosys */
/* V1.3 of the s/w added SDOP and and VSDOP bytes */
#define PID_VISIBLE_LIST     13
#define PID_QRY_INFORMATION  253
#define QRY_INFORMATION_LEN  41
#define INFO_USERNAME_LEN    13
#define INFO_SERIAL_NUM_LEN  9
#define INFO_LOG_RATE_LEN    3
#define INFO_VERSION_LEN     12
#define INFO_SEP             ','


/*
 * Very similar to read_packet in navilink.c, except reads from file
 * instead of serial, and integers are read in big endian order.
 */
static size_t
read_packet(int *type, void *payload, size_t max_len)
{
  size_t size, data_size;
  unsigned char start[4];
  unsigned int  checksum_exp, checksum_act;
  unsigned char *data;

  if (gbfread(start, sizeof(start), 1, file_handle) != 1) {
    if (gbfeof(file_handle)) {
      return 0;
    }

    fatal(MYNAME ": Format error: No packet start.\n");
  }

  if (start[0] != 0xa0 || start[1] != 0xa2) {
    fatal(MYNAME ": Format error: Bad packet start.\n");
  }

  size = be_readu16(start + 2);

  if (size < 1 || max_len < size) {
    fatal(MYNAME ": Format error: unexpected size: %d.\n", (int) size);
  }

  /* allocate space for checksum and trailing 0xb0b3 */
  data_size = size + 4;

  /* data_size can be up to about 64k */
  data = (unsigned char *) xmalloc(data_size);

  if (gbfread(data, data_size, 1, file_handle) != 1) {
    fatal(MYNAME ": Format error: could not read %d bytes.\n",
          (int) data_size);
  }

  *type = data[0];

  checksum_exp = be_readu16(data + size);
  checksum_act = navilink_checksum_packet(data, size);

  if (checksum_exp != checksum_act) {
    fatal(MYNAME ": Checksum error - expected %x got %x\n",
          checksum_exp, checksum_act);
  }

  if (data[size + 2] != 0xb0 || data[size + 3] != 0xb3) {
    fatal(MYNAME ": Format error: Bad packet trailer\n");
  }

  --size;

  memcpy(payload, data + 1, size);
  xfree(data);

  return size;
}

#ifdef LOCOSYS_PARSE_FILE_ID
static size_t
hdrcpy(char *dest, const char *src, size_t max_len)
{
  size_t i;

  for (i = 0; i < max_len && *src != INFO_SEP; i++) {
    *dest++ = *src++;
  }
  *dest++ = 0;

  return ++i;
}
#endif /* LOCOSYS_PARSE_FILE_ID */

int
locosys_decode_file_id(char *header, size_t len)
{
#ifdef LOCOSYS_PARSE_FILE_ID
  /*
   * MID_FILE_ID(0xfd) contains the following payload :
   *   User Name, Serial Number, Log Rate, Firmware Version
   *     >field separator:","
   *     >User Name : MAX CHAR(13)
   *     >Serial Number : MAX CHAR(8)
   *     >Log Rate : MAX CHAR 3, 0..255 in seconds
   *     >Firmware Version  :  MAX CHAR (13)
   */

  char username[INFO_USERNAME_LEN + 1];
  char serial_num[INFO_SERIAL_NUM_LEN + 1];
  char log_rate[INFO_LOG_RATE_LEN + 1];
  char version[INFO_VERSION_LEN + 1];
  char *p = header;

  p += hdrcpy(username,   p, INFO_USERNAME_LEN);
  p += hdrcpy(serial_num, p, INFO_SERIAL_NUM_LEN);
  p += hdrcpy(log_rate,   p, INFO_LOG_RATE_LEN);
  p += hdrcpy(version,    p, INFO_VERSION_LEN);

  printf(MYNAME ": Username: %s\n", username);
  printf(MYNAME ": Serial Number: %s\n", serial_num);
  printf(MYNAME ": Log rate (seconds): %s\n", log_rate);
  printf(MYNAME ": Firmware version: %s\n", version);
#endif /* LOCOSYS_PARSE_FILE_ID */

  return TRUE;
}

static void
read_sbn_header(route_head *track)
{
  char header[QRY_INFORMATION_LEN];
  size_t len;
  int type = 0;

  len = read_packet(&type, header, sizeof(header));

  if (len == 0 || type != PID_QRY_INFORMATION ||
      !locosys_decode_file_id(header, len)) {
    fatal(MYNAME ": Format error: Couldn't read SBN header."
          "This probably isn't a SBN file.\n");
  }
}

static int
is_sbn_valid(const unsigned char *buffer)
{
  /* valid navigation (any bit set implies navigation solution is not
   * optimal) */
  return !buffer[0] && !buffer[1];
}

static fix_type
decode_sbn_mode(const unsigned char *mode)
{
  static const fix_type fixes[8] = {
    fix_none,     /* 000 No Nav */
    fix_none,     /* 001 1 SV solution */
    fix_none,     /* 010 2 SV solution */
    fix_2d,       /* 011 3 SV solution (2D) */
    fix_3d,       /* 100 4 or more SV solution (3D) */
    fix_2d,       /* 101 Least Square 2D solution */
    fix_3d,       /* 110 Least Square 3D solution */
    fix_none      /* 111 DR solution (no SV) */
  };
  int dgps_correction = *mode & 0x80;
  fix_type fix = fixes[*mode & 7];

  return dgps_correction && fix == fix_3d ? fix_dgps : fix;
}

static void
decode_sbn_datetime(const unsigned char *buffer, waypoint *waypt)
{
  struct tm tm;
  int ms = be_readu16(buffer + 6);

  tm.tm_sec = ms / 1000;
  tm.tm_min = buffer[5];
  tm.tm_hour = buffer[4];
  tm.tm_mday = buffer[3];
  tm.tm_mon = buffer[2] - 1;
  tm.tm_year = be_readu16(buffer) - 1900;

  waypt->SetCreationTime(mkgmtime(&tm), (ms % 1000) * 1000);
}

static void
decode_sbn_position(const unsigned char *buffer, waypoint *waypt)
{
  waypt->latitude = be_read32(buffer + 0) * 0.0000001;
  waypt->longitude = be_read32(buffer + 4) * 0.0000001;
  waypt->altitude = be_read32(buffer + 12) * 0.01;
}

static waypoint *
decode_sbn_record(unsigned char *buffer)
{
  waypoint *waypt = NULL;
  waypt = waypt_new();

  if (is_sbn_valid(buffer)) {
    waypt->fix = decode_sbn_mode(buffer + 3);
  } else {
    waypt->fix = fix_none;
  }

  decode_sbn_datetime(buffer + 10, waypt);
  decode_sbn_position(buffer + 22, waypt);
  WAYPT_SET(waypt, speed, be_read16(buffer + 39) * 0.01f);
  WAYPT_SET(waypt, course, be_read16(buffer + 41) * 0.01f);
  waypt->sat = buffer[87];
  waypt->hdop = buffer[88] * 0.2f;

  return waypt;
}

static void
add_logpoints(route_head *track)
{
  unsigned char buffer[SBN_RECORD_LEN];
  int type = 0;

  while (read_packet(&type, buffer, sizeof(buffer))) {
    if (type == PID_SBN) {
      track_add_wpt(track, decode_sbn_record(buffer));
    } else if (type == PID_VISIBLE_LIST) {
      /* A list of visible SVs, their IDs, azimuths, elevations.
       * Not storing this info for now. */
    } else {
      warning(MYNAME ": Format error: Unknown packet type 0x%02x.\n", type);
    }
  }
}

/**********************************************************************/

static void
sbn_rd_init(const char *fname)
{
  file_handle = gbfopen(fname, "r", MYNAME);
}

static void
sbn_rd_deinit(void)
{
  gbfclose(file_handle);
}

static void
sbn_read(void)
{
  if (global_opts.masked_objective & TRKDATAMASK) {
    route_head     *track;

    track = route_head_alloc();
    track_add_head(track);

    read_sbn_header(track);

    add_logpoints(track);
  }
}

static void
sbn_exit(void)
{
}

/**********************************************************************/

ff_vecs_t sbn_vecs = {
  ff_type_file,
  {
    ff_cap_none         /* waypoints */,
    ff_cap_read					/* tracks */,
    ff_cap_none					/* routes */
  },
  sbn_rd_init,
  NULL,
  sbn_rd_deinit,
  NULL,
  sbn_read,
  NULL,
  sbn_exit,
  sbn_args,
  /* Characters are always encoded in ASCII. Even if the unit is set
   * to Chinese language, only ASCII characters can be entered. */
  CET_CHARSET_ASCII, 0
};
/**********************************************************************/
