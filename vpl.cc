/*

    Reader for Honda/Acura (Alpine) Navigation System VP Log (VPL) files

	Copyright (C) 2009	Chris Tracy, gpsbabel@adiemus.org
    Copyright (C) 2005  Robert Lipe, robertlipe+source@gpsbabel.org

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

/*
	With logging enabled (Diagnostic Menu | Functional Setup | Log Data), VPL
	files are written to the PC Card.  The files themselves are written out as
	ASCII files, mostly containing lines of hexadecimal characters.  The format
	seems similar to NMEA, with various sentences carrying different types of
	information.  For our purposes, the only sentence that matters is the '75'
	sentence.  For example:

75700241FA59FB242CD500CF041984991E0B0A0C09060613064509060613064424824D68FF00000800051C000271
00--1111111122222222333344445555--667788999999999999AAAAAAAAAAAA--------------------------XX

0 - Sentence type
1 - Latitude in hex (signed, divide by 0xE1000 for decimal degrees)
	0241FA59 -> 37878361 / 0xE1000 = 41.100652
2 - Longitude in hex (signed, divide by 0xE1000 for decimal degrees)
	FB242CD5 -> -81515307 / 0xE1000 = -88.449769
3 - Altitude (signed, meters)
	00CF -> 207
4 - Speed (divide by 0x10 for MPH)
	0419 -> 1049 / 0x10 = 65.5625
5 - Heading (multiply by 360/65535 to constrain to 0 - 360 degrees)
	8499 -> 33945 * (360/65535) = 186.47
6 - Number of sats
	0B -> 11
7 - HDOP (divide by 8)
	0A -> 10 / 8 = 1.25
8 - VDOP (divide by 8)
	0C -> 12 / 8 = 1.5
9 - Date and Time (YYMMDDHHMMSS)
	090606130645 = June 6, 2009 13:06:45
A - Previous line date and time (?)
	090606130644 = June 6, 2009 13:06:44
X - Checksum (xor, ala NMEA)

***********************************
* Unused, but (at least partially) decoded sentences

******
0D - Yaw Gyro (This field is not currently decoded herein)
	This field is written 25 times per second.  It contains the raw values
	from the yaw gyro.  Positive values for right turns, negative values
	for left turns.

0D00FEFA09
00??1111CC

0 - Sentence Type
? - An unknown field with observed values between 0 and 3.
1 - Yaw Gyro value
C - Checksum

******
31 - Distance Traveled
	This field is written once a second.  It contains the number of
	meters traveled since the navigation system was last turned on.

310000117050
0011111111CC

0 - Sentence Type
1 - Distance Traveled in Meters
C - Checksum

******
35 - Raw Position
	This field is written 5 times per second.  It contains Latitude
	and Longitude, as well as two currently unknown angular values.

35CFBB5CBC1744CB1BD9023308C2
00111111112222222233334444CC

0 - Sentence Type
1 - Longitude (divide by 0x8CA000 for decimal degrees)
2 - Latitude (divide by 0x8CA000 for decimal degrees)
3 - Unknown angular value (multiply by 360/65535)
4 - Unknown angular value (multiply by 360/65535)
C - Checksum

*/

/*
	TODO:
		- Implement checksum verification
 */

#include "defs.h"
#include <stdio.h> /* for sscanf */

#define MYNAME "vpl"

void vpl_parse_75_sentence(const char*);

static
arglist_t vpl_args[] = {
  ARG_TERMINATOR
};

static gbfile* vpl_file_in;
static route_head* track_head;

/*******************************************************************************
* %%%        global callbacks called by gpsbabel main process              %%% *
*******************************************************************************/

static void
vpl_rd_init(const QString& fname)
{
  vpl_file_in = gbfopen(fname, "r", MYNAME);
}

static void
vpl_rd_deinit(void)
{
  gbfclose(vpl_file_in);
}

static void
vpl_read(void)
{
  char* ibuf;

  // Set up a track
  if (track_head == NULL) {
    track_head = route_head_alloc();
    track_add_head(track_head);
  }

  while ((ibuf = gbfgetstr(vpl_file_in))) {
    if (strncmp(ibuf, "75", 2) == 0) {
      vpl_parse_75_sentence(ibuf);
    }
  }
}

static void
vpl_wr_init(const QString& fname)
{
  fatal("Writing file of type %s is not support\n", MYNAME);
}

/*******************************************************************************
* Local Functions
*******************************************************************************/

void
vpl_parse_75_sentence(const char* ibuf)
{
  uint32_t ymd, hms;
  int32_t lat_raw, lon_raw;
  int16_t alt, speed_raw;
  uint16_t hdg_raw;
  uint8_t sats, hdop_raw, vdop_raw;
  Waypoint* waypt;
  struct tm tm;

  // The files have DOS line endings (CR/LF) but we don't care, because we
  // don't read to the end.
  sscanf(ibuf, "75%*2c%8X%8X%4hX%4hX%4hX%*2c%2hhX%2hhX%2hhX%6u%6u",
         &lat_raw, &lon_raw, &alt, &speed_raw, &hdg_raw, &sats, &hdop_raw, &vdop_raw,
         &ymd, &hms);

  tm.tm_sec = hms % 100;
  hms /= 100;
  tm.tm_min = hms % 100;
  hms /= 100;
  tm.tm_hour = hms % 100;

  tm.tm_mday = ymd % 100;
  ymd /= 100;
  tm.tm_mon = ymd % 100;
  ymd /= 100;
  tm.tm_year = ymd % 100 + 100;

  waypt = new Waypoint;

  // Lat/Lon are both stored *0xE1000 which we have to divide out
  // for decimal degrees
  waypt->latitude  = lat_raw / (double) 0xE1000;
  waypt->longitude = lon_raw / (double) 0xE1000;
  waypt->altitude  = alt;
  waypt->sat       = sats;
  // Speed comes in (MPH x 0x10) which we have to convert to m/s
  WAYPT_SET(waypt, speed, (speed_raw / (double) 0x10) * 0.44704);
  waypt->course    = hdg_raw * (double)(360/65535);
  waypt->hdop      = hdop_raw / (double) 8;
  waypt->vdop      = vdop_raw / (double) 8;

  waypt->SetCreationTime(mkgmtime(&tm));

  track_add_wpt(track_head, waypt);
}

/**************************************************************************/

ff_vecs_t vpl_vecs = {
  ff_type_file,
  {
    ff_cap_none		/* waypoints */,
    ff_cap_read		/* tracks */,
    ff_cap_none		/* routes */
  },
  vpl_rd_init,
  vpl_wr_init,
  vpl_rd_deinit,
  NULL,
  vpl_read,
  NULL,
  NULL,
  vpl_args,
  CET_CHARSET_ASCII, /* ascii is the expected character set */
  1	               /* fixed, can't be changed through command line parameter */
};
/**************************************************************************/
