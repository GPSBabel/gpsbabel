/*
	Read files containing selected NMEA 0183 sentences.
	Based on information by Eino Uikkanenj

	Copyright (C) 2004-2006 Robert Lipe, robertlipe@usa.net

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
#include "strptime.h"

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

	  GSA - GPS DOP and active satellites
	  $GPGSA,A,3,04,05,,09,12,,,24,,,,,2.5,1.3,2.1*39
	       A            Auto selection of 2D or 3D fix (M = manual)
	       3            3D fix
	       04,05...     PRNs of satellites used for fix (space for 12)
	       2.5          PDOP (dilution of precision)
	       1.3          Horizontal dilution of precision (HDOP)
	       2.1          Vertical dilution of precision (VDOP)
	         DOP is an indication of the effect of satellite geometry on
	         the accuracy of the fix.

	  VTG - Track made good and ground speed
	  $GPVTG,054.7,T,034.4,M,005.5,N,010.2,K
	       054.7,T      True track made good
	       034.4,M      Magnetic track made good
	       005.5,N      Ground speed, knots
	       010.2,K      Ground speed, Kilometers per hour

	  WPL - waypoint location
	  $GPWPL,4917.16,N,12310.64,W,003*65
	       4917.16,N    Latitude of waypoint
	       12310.64,W   Longitude of waypoint
	       003          Waypoint ID
	         When a route is active, this sentence is sent once for each
	         waypoint in the route, in sequence. When all waypoints have
	         been reported, GPR00 is sent in the next data set. In any
	         group of sentences, only one WPL sentence, or an R00
	         sentence, will be sent.


   ' The optional checksum field consists of a "*" and two hex digits repre-
   ' senting the exclusive OR of all characters between, but not including,
   ' the "$" and "*".  A checksum is required on some sentences.

****************************************/

/*
 * An input file may have both GGA and GLL and RMC sentences for the exact 
 * same position fix. If we see a single GGA, start ignoring GLL's and RMC's.
 *	GLL's will also be ignored if RMC's are found and GGA's not found.
 */

/* 
Zmarties notes:

In practice, all fields of the NMEA sentences should be treated as optional -
if the data is not available, then the field can be omitted (hence leading
to the output of two consecutive commas).

An NMEA recording can start anywhere in the stream of data.  It is therefore
necessary to discard sentences until sufficient data has been processed to
have all the necessary data to construct a waypoint.  In practice, this means
discarding data until we have had the first sentence that provides the date.
(We could scan forwards in the stream of data to find the first date, and
then back apply it to all previous sentences, but that is probably more
complexity that is necessary - the lost of one waypoint at the start of the
stream can normally be tolerated.)

If a sentence is received without a checksum, but previous sentences have
had checksums, it is best to discard that sentence.  In practice, the only
time I have seen this is when the recording stops suddenly, where the last
sentence is truncated - and missing part of the line, including the checksum.
*/

#define SECONDS_PER_DAY (24L*60*60)
 
typedef enum {
	gp_unknown = 0,
	gpgga,
	gplgll,
	gprmc
} preferred_posn_type;

static FILE *file_in;
static FILE *file_out;
static route_head *trk_head;
static short_handle mkshort_handle;
static preferred_posn_type posn_type;
static struct tm tm;
static waypoint * curr_waypt = NULL;

static int without_date;	/* number of created trackpoints without a valid date */
static struct tm opt_tm;	/* converted "date" parameter */

#define MYNAME "nmea"

static const double kts2mps =0.51444444444444444; /* knots to m/s */
static const double kmh2mps =0.27777777777777778; /* km/h to m/s  */ 

static char *dogprmc = NULL;
static char *nogpgga = NULL;
static char *nogpvtg = NULL;
static char *nogpgsa = NULL;
static char *snlenopt = NULL;
static char *optdate = NULL;

arglist_t nmea_args[] = {
	{"gprmc", &dogprmc, "Write GPRMC sentences", NULL, ARGTYPE_BOOL, ARG_NOMINMAX },
	{"snlen", &snlenopt, "Max length of waypoint name to write", "6", ARGTYPE_INT, "1", "64" },
	{"nogpgga", &nogpgga, "Don't write GPGGA sentences", NULL, ARGTYPE_BOOL, ARG_NOMINMAX },
	{"nogpvtg", &nogpvtg, "Don't write GPVTG sentences", NULL, ARGTYPE_BOOL, ARG_NOMINMAX },
	{"nogpgsa", &nogpgsa, "Don't write GPGSA sentences", NULL, ARGTYPE_BOOL, ARG_NOMINMAX },
	{"date", &optdate, "Complete date-free tracks with given date (YYYYMMDD).", NULL, ARGTYPE_INT, ARG_NOMINMAX },
	ARG_TERMINATOR
};

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
	setshort_length(mkshort_handle, atoi(snlenopt));
}

static  void
nmea_wr_deinit(void)
{
	fclose(file_out);
	mkshort_del_handle(&mkshort_handle);
}

static void
nmea_set_waypoint_time(waypoint *wpt, struct tm *time)
{
	if (time->tm_year == 0)
	{
		wpt->creation_time = ((((time_t)time->tm_hour * 60) + time->tm_min) * 60) + time->tm_sec;
		if (wpt->centiseconds == 0)
		{
			 wpt->centiseconds++;
			 without_date++;
		}
	}
	else
	{
		wpt->creation_time = mkgmtime(time);
		if (wpt->centiseconds != 0)
		{
			wpt->centiseconds = 0;
			without_date--;
		}
	}
}

static void
gpgll_parse(char *ibuf)
{
	double latdeg, lngdeg;
	char lngdir, latdir;
	int hms;
	char valid;
	waypoint *waypt;

	if (trk_head == NULL) {
		trk_head = route_head_alloc();
		track_add_head(trk_head);
	}

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

	nmea_set_waypoint_time(waypt, &tm);

	if (latdir == 'S') latdeg = -latdeg;
	waypt->latitude = ddmm2degrees(latdeg);

	if (lngdir == 'W') lngdeg = -lngdeg;
	waypt->longitude = ddmm2degrees(lngdeg);

	curr_waypt = waypt;
	track_add_wpt(trk_head, waypt);
}

static void
gpgga_parse(char *ibuf)
{
	double latdeg, lngdeg;
	char lngdir, latdir;
	double hms;
	double alt;
	int fix;
	int nsats;
	double hdop;
	char altunits;
	waypoint *waypt;

	if (trk_head == NULL) {
		trk_head = route_head_alloc();
		track_add_head(trk_head);
	}

	sscanf(ibuf,"$GPGGA,%lf,%lf,%c,%lf,%c,%d,%d,%lf,%lf,%c",
		&hms, &latdeg,&latdir,
		&lngdeg,&lngdir,
		&fix,&nsats,&hdop,&alt,&altunits);

	if (fix == 0) {
		return;
	}

	tm.tm_sec = (long) hms % 100;
	hms = hms / 100;
	tm.tm_min = (long) hms % 100;
	hms = hms / 100;
	tm.tm_hour = (long) hms % 100;

	waypt  = waypt_new();

	nmea_set_waypoint_time(waypt, &tm);

	if (latdir == 'S') latdeg = -latdeg;
	waypt->latitude = ddmm2degrees(latdeg);

	if (lngdir == 'W') lngdeg = -lngdeg;
	waypt->longitude = ddmm2degrees(lngdeg);

	waypt->altitude = alt;

	waypt->sat 	= nsats;

	waypt->hdop 	= hdop;

	switch (fix) {
		case 1:
			waypt->fix  = (nsats>3)?(fix_3d):(fix_2d);
			break;
		case 2:
			waypt->fix = fix_dgps;
			break;
		case 3:
			waypt->fix = fix_pps;
			break;
	}

	curr_waypt = waypt;
	track_add_wpt(trk_head, waypt);
}

static void
gprmc_parse(char *ibuf)
{
	double latdeg, lngdeg;
	char lngdir, latdir;
	double hms;
	char fix;
	unsigned int dmy;
	double speed,course;
	waypoint *waypt;

	if (trk_head == NULL) {
		trk_head = route_head_alloc();
		track_add_head(trk_head);
	}

	sscanf(ibuf,"$GPRMC,%lf,%c,%lf,%c,%lf,%c,%lf,%lf,%u",
		&hms, &fix, &latdeg, &latdir,
		&lngdeg, &lngdir,
		&speed, &course, &dmy);

	if (fix != 'A') {
		/* ignore this fix - it is invalid */
		return;
	}
	
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

	if (posn_type == gpgga) {
		/* capture useful data update and exit */
		if (curr_waypt) {
			if (curr_waypt->speed<=0) 
				curr_waypt->speed 	= speed*kts2mps;
			if (curr_waypt->course<=0)
				curr_waypt->course 	= course;
			/* The change of date wasn't recorded when 
			 * going from 235959 to 000000. */
			 nmea_set_waypoint_time(curr_waypt, &tm);
		}
		return;
	}
		
	waypt  = waypt_new();

	waypt->speed 	= speed*kts2mps;

	waypt->course 	= course;
	
	nmea_set_waypoint_time(waypt, &tm);

	if (latdir == 'S') latdeg = -latdeg;
	waypt->latitude = ddmm2degrees(latdeg);

	if (lngdir == 'W') lngdeg = -lngdeg;
	waypt->longitude = ddmm2degrees(lngdeg);

	curr_waypt = waypt;
	track_add_wpt(trk_head, waypt);
}

static void
gpwpl_parse(char *ibuf)
{
	waypoint *waypt;
	double latdeg, lngdeg;
	char latdir, lngdir;
	char sname[99];

	sscanf(ibuf,"$GPWPL,%lf,%c,%lf,%c,%[^*]",
		&latdeg,&latdir,
		&lngdeg,&lngdir,
		sname);

	waypt  = waypt_new();

	if (latdir == 'S') latdeg = -latdeg;
	waypt->latitude = ddmm2degrees(latdeg);
	if (lngdir == 'W') lngdeg = -lngdeg;
	waypt->longitude = ddmm2degrees(lngdeg);

	waypt->shortname = xstrdup(sname);

	curr_waypt = NULL; /* waypoints won't be updated with GPS fixes */
	waypt_add(waypt);

}

static void
gpzda_parse(char *ibuf)
{
	double hms;
	int dd, mm, yy, lclhrs, lclmins;

	sscanf(ibuf,"$GPZDA,%lf,%d,%d,%d,%d,%d", 
		&hms, &dd, &mm, &yy, &lclhrs, &lclmins);
	tm.tm_sec  = (int) hms % 100;
	tm.tm_min  = (((int) hms - tm.tm_sec) / 100) % 100;
	tm.tm_hour = (int) hms / 10000;
	tm.tm_mday = dd;
	tm.tm_mon  = mm - 1;
	tm.tm_year = yy - 1900;
}

static void
gpgsa_parse(char *ibuf)
{
	char fixauto;
	char fix;
	int  prn[12];
	int  scn,cnt;
	float pdop=0,hdop=0,vdop=0;
	char*	tok=0;

	memset(prn,0xff,sizeof(prn));

	scn = sscanf(ibuf,"$GPGSA,%c,%c,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d",
		&fixauto, &fix,
		&prn[0],&prn[1],&prn[2],&prn[3],&prn[4],&prn[5],
		&prn[6],&prn[7],&prn[8],&prn[9],&prn[10],&prn[11]);

	/* 
		sscanf has scanned all the leftmost elements
		we'll rescan by skipping 15 commas to the dops
	*/
	tok = ibuf;
	for (cnt=0;(tok)&&(cnt<15);cnt++)
	{
		tok = strchr(tok,',');
		if (!tok) break;
		tok++;
	}
	if (tok) sscanf(tok,"%f,%f,%f",&pdop,&hdop,&vdop);
	

	if (curr_waypt) {

		if (curr_waypt->fix!=fix_dgps) {
			if 	(fix=='3')	curr_waypt->fix=fix_3d;
			else if (fix=='2')	curr_waypt->fix=fix_2d;
		}
	
		curr_waypt->pdop = pdop;
		curr_waypt->hdop = hdop;
		curr_waypt->vdop = vdop;
		
		if (curr_waypt->sat  <= 0)	{
			for (cnt=0;cnt<12;cnt++)
				curr_waypt->sat += (prn[cnt]>0)?(1):(0);
		}
	}	
	
}

static void
gpvtg_parse(char *ibuf)
{
	float 	course;
	char	ct;
	float	magcourse;
	char	cm;
	double	speed_n;
	char	cn;
	double	speed_k;
	char	ck;	

	sscanf(ibuf,"$GPVTG,%f,%c,%f,%c,%lf,%c,%lf,%c",
		&course,&ct,&magcourse,&cm,&speed_n,&cn,&speed_k,&ck);
		
	if (curr_waypt) {
		curr_waypt->course = course;		
		
		if (speed_k>0)
			curr_waypt->speed = speed_k*kmh2mps;
		else
			curr_waypt->speed = speed_n*kts2mps;

	}	
	
}

static void
nmea_fix_timestamps(route_head *track)
{
	if ((trk_head == NULL) || (without_date == 0)) return;
	
	if (tm.tm_year == 0)
	{
		queue *elem, *temp;
		waypoint *prev = NULL;
		time_t delta_tm;
		
		if (optdate == NULL)
		{
			warning(MYNAME ": No date found within track (all points dropped)!\n");
			warning(MYNAME ": Please use option \"date\" to preset a valid date for thoose tracks.\n");
			track_del_head(track);
			return;
		}
		delta_tm = mkgmtime(&opt_tm);
		
		QUEUE_FOR_EACH(&track->waypoint_list, elem, temp)
		{
			waypoint *wpt = (waypoint *)elem;
			
			wpt->creation_time += delta_tm;
			if ((prev != NULL) && (prev->creation_time > wpt->creation_time))	/* go over midnight ? */
			{
				delta_tm += SECONDS_PER_DAY;
				wpt->creation_time += SECONDS_PER_DAY;
			}
			prev = wpt;
		}
	}
	else
	{
		time_t prev;
		queue *elem;
		
		tm.tm_hour = 23;	/* last date found */
		tm.tm_min = 59;
		tm.tm_sec = 59;
		
		prev = mkgmtime(&tm);
		
		/* go backward through the track and complete timestamps */

		for (elem = QUEUE_LAST(&track->waypoint_list); elem != &track->waypoint_list; elem=elem->prev)
		{
			waypoint *wpt = (waypoint *)elem;
			
			if (wpt->centiseconds != 0)
			{
				time_t dt;
				
				wpt->centiseconds = 0;		/* reset flag */

				dt = (prev / SECONDS_PER_DAY) * SECONDS_PER_DAY;
				wpt->creation_time += dt;
				if (wpt->creation_time > prev)
						wpt->creation_time+=SECONDS_PER_DAY;
			}
			prev = wpt->creation_time;
		}
	}
}

static void
nmea_read(void)
{
	char ibuf[1024];
	char *ck;
	int ckval, ckcmp;
	int had_checksum = 0;

	posn_type = gp_unknown;
	trk_head = NULL;
	without_date = 0;
	memset(&tm, 0, sizeof(tm));
	opt_tm = tm;
	
	if (optdate)
	{
		memset(&opt_tm, 0, sizeof(opt_tm));
		
		ck = (char *)strptime(optdate, "%Y%m%d", &opt_tm);
		if ((ck == NULL) || (*ck != '\0') || (strlen(optdate) != 8))
			fatal(MYNAME ": Invalid date \"%s\"!\n", optdate);
		else if (opt_tm.tm_year < 70)
			fatal(MYNAME ": Date \"%s\" is out of range (have to be 19700101 or later)!\n", optdate);
	}

	curr_waypt = NULL; 

	while (fgets(ibuf, sizeof(ibuf), file_in)) 
	{
		char *tbuf = lrtrim(ibuf);
		if (*tbuf != '$') continue;
		
		ck = strrchr(tbuf, '*');
		if (ck != NULL) {
			*ck = '\0';
			ckval = nmea_cksum(&tbuf[1]);
			*ck = '*';
			ck++;
			sscanf(ck, "%2X", &ckcmp);
			if (ckval != ckcmp) {
#if 0
				printf("ckval %X, %X, %s\n", ckval, ckcmp, ck);
				printf("NMEA %s\n", tbuf);
#endif
				continue;
			}

			had_checksum = 1;
		}
		else if (had_checksum) {
			/* we have had a checksum on all previous sentences, but not on this
			one, which probably indicates this line is truncated */
			had_checksum = 0;
			continue;
		}

		/* @@@ zmarties: The parse routines all assume all fields are present, but 
		   the NMEA format allows any field to be missed out if there is no data
		   for that field.  Rather than change all the parse routines, we first
		   substitute a default value of zero for any missing field.
		*/
		if (strstr(tbuf, ",,"))
		{
			tbuf = gstrsub(tbuf, ",,", ",0,");
		}

		if (0 == strncmp(tbuf, "$GPWPL,", 7)) {
			gpwpl_parse(tbuf);
		} else
		if (0 == strncmp(tbuf, "$GPGGA,", 7)) {
			posn_type = gpgga;
			gpgga_parse(tbuf);
		} else
		if (0 == strncmp(tbuf, "$GPRMC,", 7)) {
			if (posn_type != gpgga) {
				posn_type = gprmc;
			}			
			/*			
			 * Always call gprmc_parse() because like GPZDA
			 * it contains the full date.
			 */			
			gprmc_parse(tbuf);
		} else
		if (0 == strncmp(tbuf, "$GPGLL,", 7)) {
			if ((posn_type != gpgga) && (posn_type != gprmc)) {
				gpgll_parse(tbuf);
			}
		} else
		if (0 == strncmp(tbuf, "$GPZDA,",7)) {
			gpzda_parse(tbuf);
		} else
		if (0 == strncmp(tbuf, "$GPVTG,",7)) {
			gpvtg_parse(tbuf); /* speed and course */
		} else
		if (0 == strncmp(tbuf, "$GPGSA,",7)) {
			gpgsa_parse(tbuf); /* GPS fix */
		}

		if (tbuf != ibuf) {
		  /* clear up the dynamic buffer we used because substition was required */
			xfree(tbuf);
		}
	}
	/* try to complete date-less trackpoints */
	nmea_fix_timestamps(trk_head);
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
	if (global_opts.synthesize_shortnames) 
		s = mkshort_from_wpt(mkshort_handle, wpt);
	else {
		s = mkshort(mkshort_handle, wpt->shortname);
	}

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
	char fix='0';
	double lat,lon;
	int cksum;
	struct tm *tm;
	time_t hms;
	time_t ymd;

	lat = degrees2ddmm(wpt->latitude);
	lon = degrees2ddmm(wpt->longitude);

	tm = gmtime(&wpt->creation_time);
	if ( tm ) {
		hms = tm->tm_hour * 10000 + tm->tm_min * 100 + tm->tm_sec;
		ymd = tm->tm_mday * 10000 + tm->tm_mon * 100 + tm->tm_year;
	} else {
		hms = 0;
		ymd = 0;
	}

	switch (wpt->fix) 
	{
	case fix_dgps: 
		fix='2';
		/* or */
	case fix_3d:
	case fix_2d:
		fix='1';
		break;
	case fix_pps:
		fix='3';
		break;
	default:
		fix='0';
	}

	if (dogprmc) {
		snprintf(obuf, sizeof(obuf), "GPRMC,%06d,%c,%08.3f,%c,%09.3f,%c,%.2f,%.2f,%06d,,",
				hms,
				fix=='0' ? 'V' : 'A',
				fabs(lat), lat < 0 ? 'S' : 'N',
				fabs(lon), lon < 0 ? 'W' : 'E',
				(wpt->speed>0)?(wpt->speed / kts2mps):(0),
				(wpt->course>=0)?(wpt->course):(0),
				ymd);
		cksum = nmea_cksum(obuf);
		fprintf(file_out, "$%s*%02X\n", obuf, cksum);
	}
	if (!nogpgga) {
		snprintf(obuf, sizeof(obuf), "GPGGA,%06d,%08.3f,%c,%09.3f,%c,%c,%02d,%.1f,%.3f,M,0.0,M,,",
				hms,
				fabs(lat), lat < 0 ? 'S' : 'N',
				fabs(lon), lon < 0 ? 'W' : 'E',
				fix,
				(wpt->sat>0)?(wpt->sat):(0),
				(wpt->hdop>0)?(wpt->hdop):(0.0),
				wpt->altitude == unknown_alt ? 0 : wpt->altitude);
		cksum = nmea_cksum(obuf);
		fprintf(file_out, "$%s*%02X\n", obuf, cksum);
	}
	if ((!nogpvtg) && ((wpt->course>=0) || (wpt->speed>0))) {
		snprintf(obuf,sizeof(obuf),"GPVTG,%.3f,T,0,M,%.3f,N,%.3f,K",
			(wpt->course>=0)?(wpt->course):(0),	
			(wpt->speed>0)?(wpt->speed / kts2mps):(0),
			(wpt->speed>0)?(wpt->speed / kmh2mps):(0) );

		cksum = nmea_cksum(obuf);
		fprintf(file_out, "$%s*%02X\n", obuf, cksum);
	}
			
	if ((!nogpgsa) && (wpt->fix!=fix_unknown)) {

		switch (wpt->fix) 
		{
		case fix_dgps: 
			/* or */
		case fix_3d:
			fix='3';
			break;
		case fix_2d:
			fix='2';
			break;
		default:
			fix=0;
		}
		snprintf(obuf,sizeof(obuf),"GPGSA,A,%c,,,,,,,,,,,,,%.1f,%.1f,%.1f",
			fix,
			(wpt->pdop>0)?(wpt->pdop):(0),
			(wpt->hdop>0)?(wpt->hdop):(0),
			(wpt->vdop>0)?(wpt->vdop):(0) );
		cksum = nmea_cksum(obuf);
		fprintf(file_out, "$%s*%02X\n", obuf, cksum);
	}
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
	nmea_args,
	CET_CHARSET_ASCII, 0	/* CET-REVIEW */
};
