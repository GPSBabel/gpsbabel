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

   ' $GPRMC - Recommended minimum specific GNSS Data
   ' $GPRMC,085721.194,A,5917.7210,N,01103.9227,E,21.42,50.33,300504,,*07
   '  2    085721       Fix taken at 08:57:21 UTC
   '  3    A				Fix valid (this field reads V if fix is not valid)
   '  4,5  5917.7210,N   Latitude 59 deg 17.7210' N
   '  6,7  01103.9227,E  Longitude 11 deg 03.9227' E
   '  8    21.42			Speed over ground (knots)
   '  9    50.33			Course over ground (true)
   '	10   300504			Date 30/05-2004
   '  11   Empty field	Magnetic variation


   ' The optional checksum field consists of a "*" and two hex digits repre-
   ' senting the exclusive OR of all characters between, but not including,
   ' the "$" and "*".  A checksum is required on some sentences.

****************************************/

/*
 * An input file may have both GGA and GLL and RMC sentences for the exact 
 * same position fix. If we see a single GGA, start ignoring GLL's and RMC's.
 *	GLL's will also be ignored if RMC's are found and GGA's not found.
 */
 
typedef enum {
	gp_unknown = 0,
	gpgga,
	gplgll,
	gprmc
} preferred_posn_type;

static FILE *file_in;
static FILE *file_out;
static route_head *trk_head;
static void *mkshort_handle;
static preferred_posn_type posn_type;
static time_t creation_time;

#define MYNAME "nmea"

/*
 * Slightly different than the Magellan checksum fn.
 */
static int
nmea_cksum(const char *const buf)
{
	int x = 0 ;
	const char *p;

	for (p = buf; *p; p++) {
		x ^= *p;
	}
	return x;
}

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
	
	mkshort_handle = mkshort_new_handle();
	setshort_length(mkshort_handle, 6);
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

	memset(&tm, 0, sizeof(tm));

	sscanf(ibuf,"$GPGLL,%lf,%c,%lf,%c,%d,%c,",
		&latdeg,&latdir,
		&lngdeg,&lngdir,
		&hms,&valid);

	if (valid != 'A')
		return;
	tm.tm_sec = hms % 100;
	hms = hms / 100;
	tm.tm_min = hms % 100;
	hms = hms / 100;
	tm.tm_hour = hms % 100;

	waypt = waypt_new();

	waypt->creation_time = creation_time;

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
	double hms;
	int fix = 0;
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

	memset(&tm, 0, sizeof(tm));

	sscanf(ibuf,"$GPGGA,%lf,%lf,%c,%lf,%c,%d,%d,%lf,%lf,%c",
		&hms, &latdeg,&latdir,
		&lngdeg,&lngdir,
		&fix,&nsats,&hdop,&alt,&altunits);

	if (fix == 0)
		return;

	tm.tm_sec = (long) hms % 100;
	hms = hms / 100;
	tm.tm_min = (long) hms % 100;
	hms = hms / 100;
	tm.tm_hour = (long) hms % 100;

	waypt  = waypt_new();

	waypt->creation_time = creation_time;

	if (latdir == 'S') latdeg = -latdeg;
	waypt->latitude = ddmm2degrees(latdeg);

	if (lngdir == 'W') lngdeg = -lngdeg;
	waypt->longitude = ddmm2degrees(lngdeg);

	waypt->altitude = alt;
	route_add_wpt(trk_head, waypt);

}

static void
gprmc_parse(char *ibuf)
{
	double latdeg, lngdeg;
	char lngdir, latdir;
	double hms;
	double speed, course;
	char fix;
	unsigned int dmy;
	struct tm tm;
	waypoint *waypt;

	if (trk_head == NULL) {
		trk_head = route_head_alloc();
		track_add_head(trk_head);
	}

	memset(&tm, 0, sizeof(tm));

	sscanf(ibuf,"$GPRMC,%lf,%c,%lf,%c,%lf,%c,%lf,%lf,%u",
		&hms, &fix, &latdeg, &latdir,
		&lngdeg, &lngdir,
		&speed, &course, &dmy);

	if (fix != 'A')
		return;
	tm.tm_sec = (long) hms % 100;
	hms = hms / 100;
	tm.tm_min = (long) hms % 100;
	hms = hms / 100;
	tm.tm_hour = (long) hms % 100;

	tm.tm_year = dmy % 100 + 100;
	dmy = dmy / 100;
	tm.tm_mon  = dmy % 100 - 1;
	dmy = dmy / 100;
	tm.tm_mday = dmy;
	creation_time = mkgmtime(&tm);

	if (posn_type == gpgga)
		return;

	waypt  = waypt_new();
	waypt->creation_time = creation_time;

	if (latdir == 'S') latdeg = -latdeg;
	waypt->latitude = ddmm2degrees(latdeg);

	if (lngdir == 'W') lngdeg = -lngdeg;
	waypt->longitude = ddmm2degrees(lngdeg);

	route_add_wpt(trk_head, waypt);
}

static void
gpwpl_parse(char *ibuf)
{
	waypoint *waypt;
	double latdeg, lngdeg;
	char latdir, lngdir;
	char sname[7];

	sscanf(ibuf,"$GPWPL,%lf,%c,%lf,%c,%[^*]",
		&latdeg,&latdir,
		&lngdeg,&lngdir,
		&sname);

	waypt  = waypt_new();

	if (latdir == 'S') latdeg = -latdeg;
	waypt->latitude = ddmm2degrees(latdeg);
	if (lngdir == 'W') lngdeg = -lngdeg;
	waypt->longitude = ddmm2degrees(lngdeg);

	waypt->shortname = xstrdup(sname);

	waypt_add(waypt);

}

static void
gpzda_parse(char *ibuf)
{
	double hms;
	int dd, mm, yy, lclhrs, lclmins;
	struct tm tm;

	memset(&tm, 0, sizeof(tm));
	sscanf(ibuf,"$GPZDA,%lf,%d,%d,%d,%d,%d", 
		&hms, &dd, &mm, &yy, &lclhrs, &lclmins);
	tm.tm_sec  = (int) hms % 100;
	tm.tm_min  = (((int) hms - tm.tm_sec) / 100) % 100;
	tm.tm_hour = (int) hms / 10000;
	tm.tm_mday = dd;
	tm.tm_mon  = mm - 1;
	tm.tm_year = yy - 1900;
	creation_time = mkgmtime(&tm);
}

static void
nmea_read(void)
{
	char ibuf[1024];
	char *ck;
	int ckval, ckcmp;
	struct tm tm;

	creation_time = mkgmtime(&tm) + current_time();

	while (fgets(ibuf, sizeof(ibuf), file_in)) {
		ck = strrchr(ibuf, '*');
		if (ck != NULL) {
			*ck = '\0';
			ckval = nmea_cksum(&ibuf[1]);
			*ck = '*';
			ck++;
			sscanf(ck, "%2X", &ckcmp);
			if (ckval != ckcmp) {
#if 0
				printf("ckval %X, %X, %s\n", ckval, ckcmp, ck);
				printf("NMEA %s\n", ibuf);
#endif
				continue;
			}
		}
		if (0 == strncmp(ibuf, "$GPWPL,", 7)) {
			gpwpl_parse(ibuf);
		} else
		if (0 == strncmp(ibuf, "$GPGGA,", 7)) {
			posn_type = gpgga;
			gpgga_parse(ibuf);
		} else
		if (0 == strncmp(ibuf, "$GPRMC,", 7)) {
			if (posn_type != gpgga) {
				posn_type = gprmc;
			}			
			/*			
			 * Allways call gprmc_parse() because like GPZDA
			 * it contains the full date.
			 */			
			gprmc_parse(ibuf);
		} else
		if (0 == strncmp(ibuf, "$GPGLL,", 7)) {
			if ((posn_type != gpgga) && (posn_type != gprmc)) {
				gpgll_parse(ibuf);
			}
		} else
		if (0 == strncmp(ibuf, "$GPZDA,",7)) {
			gpzda_parse(ibuf);
		}
	}
}

static void
nmea_wayptpr(const waypoint *wpt)
{
	char obuf[200];
	double lat,lon;
	char *s;
	int cksum;

	lat = degrees2ddmm(wpt->latitude);
	lon = degrees2ddmm(wpt->longitude);
	s = mkshort(mkshort_handle, wpt->shortname);

	snprintf(obuf, sizeof(obuf),  "GPWPL,%08.3f,%c,%09.3f,%c,%s", 
			fabs(lat), lat < 0 ? 'S' : 'N',
			fabs(lon), lon < 0 ? 'W' : 'E', s

	);
	cksum = nmea_cksum(obuf);
	fprintf(file_out, "$%s*%02X\n", obuf, cksum);
	
	xfree(s);
	
}

void
nmea_trackpt_pr(const waypoint *wpt)
{
	char obuf[200];
	double lat,lon;
	int cksum;
	struct tm *tm;
	int hms;

	lat = degrees2ddmm(wpt->latitude);
	lon = degrees2ddmm(wpt->longitude);

	tm = gmtime(&wpt->creation_time);
	if ( tm ) {
		hms = tm->tm_hour * 10000 + tm->tm_min  * 100 +
		tm->tm_sec;
	} else {
		hms = 0;
	}

	snprintf(obuf, sizeof(obuf), "GPGGA,%06d,%08.3f,%c,%09.3f,%c,04,0,0,%.3f,M,0.0,M,,",
			hms,
			fabs(lat), lat < 0 ? 'S' : 'N',
			fabs(lon), lon < 0 ? 'W' : 'E',
			wpt->altitude == unknown_alt ? 0 : wpt->altitude);

	cksum = nmea_cksum(obuf);
	fprintf(file_out, "$%s*%02X\n", obuf, cksum);
}

static void
nmea_write(void)
{
	waypt_disp_all(nmea_wayptpr);
	track_disp_all(NULL, NULL, nmea_trackpt_pr);
}

ff_vecs_t nmea_vecs = {
	ff_type_file,
	{ ff_cap_read | ff_cap_write, ff_cap_read | ff_cap_write, ff_cap_none},
	nmea_rd_init,	
	nmea_wr_init,	
	nmea_rd_deinit,	
	nmea_wr_deinit,	
	nmea_read,
	nmea_write,
	NULL,
	NULL
};
