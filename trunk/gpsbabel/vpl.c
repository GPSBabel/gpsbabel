/*

    Reader for Honda/Acura (Alpine) Navigation System VP Log (VPL) files

	Copyright (C) 2009	Chris Tracy, gpsbabel@adiemus.org
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

/*
	With logging enabled (Diagnostic Menu | Functional Setup | Log Data), VPL
	files are written to the PC Card.  The files themselves are written out as
	ASCII files, mostly containing lines of hexadecimal characters.  The format
	seems similar to NMEA, with various sentences carrying different types of
	information.  For our purposes, the only sentence that matters is the '75'
	sentence.  For example:

75700241FA59FB242CD500CF041984991E0B0A0C09060613064509060613064424824D68FF00000800051C000271
00--1111111122222222333344445555--66----777777777777--------------------------------------CC

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
7 - Date and Time (YYMMDDHHMMSS)
	090606130645 = June 6, 2009 13:06:45
C - Checksum (xor, ala NMEA)

*/

/* 
	TODO:
		- Implement checksum verification
 */

#include "defs.h"
#include <ctype.h>

#define MYNAME "vpl"


void vpl_parse_75_sentence(const char*);

static
arglist_t vpl_args[] = {
	ARG_TERMINATOR
};

static gbfile *vpl_file_in;
static route_head *track_head;

/*******************************************************************************
* %%%        global callbacks called by gpsbabel main process              %%% *
*******************************************************************************/

static void
vpl_rd_init(const char *fname)
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
	char *ibuf;

	// Set up a track
	if(track_head == NULL) {
		track_head = route_head_alloc();
		track_add_head(track_head);
	}

	while((ibuf = gbfgetstr(vpl_file_in))) {
		if(strncmp(ibuf, "75", 2)) {
			// Only bother with '75' sentences
			continue;
		}
		vpl_parse_75_sentence(ibuf);
	}
}

static void
vpl_wr_init(const char *fname)
{
	fatal("Writing file of type %s is not support\n", MYNAME);
}

/*******************************************************************************
* Local Functions
*******************************************************************************/

void
vpl_parse_75_sentence(const char *ibuf)
{
	gbuint32 ymd, hms;
	gbint32 lat_raw, lon_raw;
	gbint16 alt, speed_raw;
	gbuint16 hdg_raw;
	gbuint8 sats;
	waypoint *waypt;
	struct tm tm;

	// The files have DOS line endings (CR/LF) but we don't care, because we
	// don't read to the end.
	sscanf(ibuf, "75%*2c%8X%8X%4hX%4hX%4hX%*2c%2hhX%*4c%6u%6u",
		&lat_raw, &lon_raw, &alt, &speed_raw, &hdg_raw, &sats, &ymd, &hms);

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

	waypt = waypt_new();

	// Lat/Lon are both stored *0xE1000 which we have to divide out
	// for decimal degrees
	waypt->latitude  = lat_raw / (double) 0xE1000;
	waypt->longitude = lon_raw / (double) 0xE1000;
	waypt->altitude  = alt;
	waypt->sat       = sats;
	// Speed comes in (MPH x 0x10) which we have to convert to m/s
	WAYPT_SET(waypt, speed, (speed_raw / (double) 0x10) * 0.44704);
	waypt->course    = hdg_raw * (double) (360/65535);

	waypt->creation_time = mkgmtime(&tm);

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
