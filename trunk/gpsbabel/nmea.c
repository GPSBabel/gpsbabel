/*
    Read files containing selected NMEA 0183 sentences.
    Based on information by Eino Uikkanenj

    Copyright (C) 2004 Robert Lipe, robertlipe@usa.net

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

#include <ctype.h>
#include <time.h>

#include "defs.h"

/**********************************************************

   ' 1      2      3        4 5         6 7 8  9   10   11 12  13 14 15
   ' $GPGGA - Global Positioning System Fix Data
   ' $GPGGA,155537,6006.718,N,02426.290,E,1,05,2.4,50.5,M,19.7,M,,*79
   '  2    123519       Fix taken at 12:35:19 UTC
   '  3,4  4807.038,N   Latitude 48 deg 07.038' N
   '  5,6  01131.324,E  Longitude 11 deg 31.324' E
   '  7    1            Fix quality: 0 = invalid
   '                                 1 = GPS fix
   '                                 2 = DGPS fix
   '  8    08           Number of satellites being tracked
   '  9    0.9          Horizontal dilution of position
   ' 10,11 545.4,M      Altitude, Metres, above mean sea level
   ' 12,13 46.9,M       Height of geoid (mean sea level) above WGS84 ellipsoid
   ' 14    (empty field) time in seconds since last DGPS update
   ' 15    (empty field) DGPS station ID number

   ' $GPWPL - waypoint location
   ' $GPWPL,4917.16,N,12310.64,W,003*65
   '  2,3  4917.16,N    Latitude of waypoint
   '  4,5  12310.64,W   Longitude of waypoint
   '  6    003          Waypoint ID

   ' $GPGLL - Geographic position, Latitude and Longitude
   ' $GPGLL,4916.45,N,12311.12,W,225444,A
   '  2,3  4916.46,N    Latitude 49 deg. 16.45 min. North
   '  4,5  12311.12,W   Longitude 123 deg. 11.12 min. West
   '  6    225444       Fix taken at 22:54:44 UTC
   '  7    A            Data valid

   ' The optional checksum field consists of a "*" and two hex digits repre-
   ' senting the exclusive OR of all characters between, but not including,
   ' the "$" and "*".  A checksum is required on some sentences.

****************************************/

static FILE *file_in;
static FILE *file_out;
static route_head *trk_head;

#define MYNAME "nmea"

static void
nmea_rd_init(const char *fname)
{
	file_in = xfopen(fname, "r", MYNAME);
}
static  void
nmea_rd_deinit(void)
{
	fclose(file_in);
}

static void
nmea_wr_init(const char *portname)
{
	file_out = xfopen(portname, "w+", MYNAME);
}

static  void
nmea_wr_deinit(void)
{
	fclose(file_out);
}

void
gpgll_parse(char *ibuf)
{
	double latdeg, lngdeg;
	char lngdir, latdir;
	int hms;
	char valid;
	struct tm tm;
	waypoint *waypt;

	if (trk_head == NULL) {
		trk_head = route_head_alloc();
		track_add_head(trk_head);
	}

	waypt = waypt_new();

	memset(&tm, 0, sizeof(tm));

	sscanf(ibuf,"$GPGLL,%lf,%c,%lf,%c,%d,%c,",
		&latdeg,&latdir,
		&lngdeg,&lngdir,
		&hms,&valid);

	tm.tm_sec = hms % 100;
	hms = hms / 100;
	tm.tm_min = hms % 100;
	hms = hms / 100;
	tm.tm_hour = hms % 100;

	waypt->creation_time = mktime(&tm) + get_tz_offset() + time(0);;

	if (latdir == 'S') latdeg = -latdeg;
	waypt->latitude = ddmm2degrees(latdeg);

	if (lngdir == 'W') lngdeg = -lngdeg;
	waypt->longitude = ddmm2degrees(lngdeg);

	route_add_wpt(trk_head, waypt);
}

static void
gpgga_parse(char *ibuf)
{
	double latdeg, lngdeg;
	char lngdir, latdir;
	int hms;
	int fix;
	int nsats;
	double hdop;
	struct tm tm;
	double alt;
	char altunits;
	waypoint *waypt;

	if (trk_head == NULL) {
		trk_head = route_head_alloc();
		track_add_head(trk_head);
	}

	waypt  = waypt_new();

	memset(&tm, 0, sizeof(tm));

	sscanf(ibuf,"$GPGGA,%d,%lf,%c,%lf,%c,%d,%d,%lf,%lf,%c",
		&hms, &latdeg,&latdir,
		&lngdeg,&lngdir,
		&fix,&nsats,&hdop,&alt,&altunits);

	tm.tm_sec = hms % 100;
	hms = hms / 100;
	tm.tm_min = hms % 100;
	hms = hms / 100;
	tm.tm_hour = hms % 100;

	waypt->creation_time = mktime(&tm) + get_tz_offset() + time(NULL);

	if (latdir == 'S') latdeg = -latdeg;
	waypt->latitude = ddmm2degrees(latdeg);

	if (lngdir == 'W') lngdeg = -lngdeg;
	waypt->longitude = ddmm2degrees(lngdeg);

	waypt->altitude = alt;
	route_add_wpt(trk_head, waypt);

}

static void
nmea_read(void)
{
	char ibuf[1024];

	while (fgets(ibuf, sizeof(ibuf), file_in)) {
		if (0 == strncmp(ibuf, "$GPGGA,", 7)) {
			gpgga_parse(ibuf);
		}
//		if (0 == strncmp(ibuf, "$GPGLL,", 7)) {
//			gpgll_parse(ibuf);
//		}
	}
}

static void
nmea_write(void)
{
}

ff_vecs_t nmea_vecs = {
	nmea_rd_init,	
	nmea_wr_init,	
	nmea_rd_deinit,	
	nmea_wr_deinit,	
	nmea_read,
	NULL,
	NULL
};
