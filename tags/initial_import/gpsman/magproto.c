/*
    Communicate Thales/Magellan serial protocol.

    Copyright (C) 2002 Robert Lipe, robertlipe@usa.net

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

#include <termios.h>
#include <ctype.h>

#include "defs.h"
#include "magellan.h"

#define debug 1

typedef enum {
	mrs_handoff = 0,
	mrs_handon
} mag_rxstate;

static FILE *magfile;
static int magfd;
static mag_rxstate magrxstate;
static struct termios orig_tio;
static int last_rx_csum;
static int found_done;
static icon_mapping_t *icon_mapping;

static waypoint * mag_wptparse(char *);

static icon_mapping_t gps315_icon_table[] = {
	{ "a", "filled circle" },
	{ "b", "box" },
	{ "c", "red buoy" },
	{ "d", "green buoy" },
	{ "e", "buoy" },
	{ "f", "rocks" },
	{ "g", "red daymark" },
	{ "h", "green daymark" },
	{ "i", "bell" },
	{ "j", "danger" },
	{ "k", "diver down" },
	{ "l", "fish" },
	{ "m", "house" },
	{ "n", "mark" },
	{ "o", "car" },
	{ "p", "tent" },
	{ "q", "boat" },
	{ "r", "food" },
	{ "s", "fuel" },
	{ "t", "tree" },
	{ NULL, NULL }
};

static icon_mapping_t map330_icon_table[] = {
	{ "a", "crossed square" },
	{ "b", "box" },
	{ "c", "house" },
	{ "d", "aerial" },
	{ "e", "airport" },
	{ "f", "amusement park" },
	{ "g", "ATM" },
	{ "h", "auto repair" },
	{ "i", "boating" },
	{ "j", "camping" },
	{ "k", "exit ramp" },
	{ "l", "first aid" },
	{ "m", "nav aid" },
	{ "n", "buoy" },
	{ "o", "fuel" },
	{ "p", "garden" },
	{ "q", "golf" },
	{ "r", "hotel" },
	{ "s", "hunting/fishing" },
	{ "t", "large city" },
	{ "u", "lighthouse" },
	{ "v", "major city" },
	{ "w", "marina" },
	{ "x", "medium city" },
	{ "y", "museum" },
	{ "z", "obstruction" },
	{ "aa", "park" },
	{ "ab", "resort" },
	{ "ac", "restaraunt" },
	{ "ad", "rock" },
	{ "ae", "scuba" },
	{ "af", "RV service" },
	{ "ag", "shooting" },
	{ "ah", "sight seeing" },
	{ "ai", "small city" },
	{ "aj", "sounding" },
	{ "ak", "sports arena" },
	{ "al", "tourist info" },
	{ "am", "truck service" },
	{ "an", "winery" },
	{ "ao", "wreck" },
	{ "ap", "zoo" },
	{ NULL, NULL } 
};

pid_to_model_t pid_to_model[] = 
{
	{ mm_gps315320, 24, "GPS 315/320" },
	{ mm_map410, 25, "Map 410" },
	{ mm_map330, 30, "Map 330" },
	{ mm_gps310, 31, "GPS 310" },
	{ mm_meridian, 33, "Meridian" },
	{ 0, 0, NULL }
};


/*
 * Given a protocol message, compute the checksum as needed by 
 * the Magellan protocol.
 */
static unsigned int 
mag_checksum(const char * const buf)
{
	int csum = 0;
	const char *p;

	for(p = buf; *p; p++) {
		csum  ^= *p;
	}
	
	return csum;
}
static unsigned int
mag_pchecksum(const char * const buf, int len)
{
	int csum = 0;
	const char *p = buf;
	for (; len ; len--) {
		csum ^= *p++;
	}
	return csum;
}

static void
mag_writemsg(const char * const buf)
{
	unsigned int osum = mag_checksum(buf);

	if (debug) {
		fprintf(stderr,"WRITE: $%s*%02X\r\n",buf, osum);
	}
#if 0
	retry:
#endif
	fprintf(magfile, "$%s*%02X\r\n",buf, osum);
#if 0
	if (magrxstate == mrs_handon) {
		mag_readmsg();
		if (last_rx_csum != osum) {
fprintf(stderr, "E");
			goto retry;
		}
	}
#endif
} 
static void
mag_writeack(int osum)
{
	char obuf[100];
	snprintf(obuf, sizeof(obuf), "PMGNCSM,%02X", osum);
	mag_writemsg(obuf);
}

static void
mag_handon(void)
{
	magrxstate = mrs_handon;
	mag_writemsg("PMGNCMD,HANDON");
}

static void
mag_handoff(void)
{
	magrxstate = mrs_handoff;
	mag_writemsg("PMGNCMD,HANDOFF");
}

void
mag_verparse(char *ibuf)
{
	int prodid = mm_unknown; 
	char version[1024];
	pid_to_model_t *pp = pid_to_model;
	sscanf(ibuf,"$PMGNVER,%d,%[^,]", &prodid, version);

	for (pp = pid_to_model; pp->model ; pp++) {
		if (pp->pid == prodid) {
			break;
		}
	}
	switch (pp->model) {
		case mm_gps315320:
		case mm_map410:
			icon_mapping = gps315_icon_table;
			break;
		case mm_map330:
		case mm_meridian:
			icon_mapping = map330_icon_table;
			break;
		default:
			abort();
	}
}

#define IS_TKN(x) (strncmp(ibuf,x, sizeof(x)-1) == 0)

static void
mag_readmsg(void)
{
	char ibuf[100];
	int isz;
	unsigned int isum;
	char *isump;
	fgets(ibuf, sizeof(ibuf), magfile);
	isz = strlen(ibuf);
	if (isz < 5) {
if (debug)
fprintf(stderr, "SHORT READ %d\n", isz);
		return;
	}
	while (!isprint(ibuf[isz]))
		isz--;
	isump = &ibuf[isz-1];
	isum  = strtoul(isump, NULL,16); 
	if (isum != mag_pchecksum(&ibuf[1], isz-3)) {
if (debug)
		fprintf(stderr, "RXERR %02x/%02x: '%s'\n", isum, mag_pchecksum(&ibuf[1],isz-5), ibuf);
	}
if (debug)
fprintf(stderr, "READ: %s\n", ibuf);
	if (IS_TKN("$PMGNCSM,")) {
		last_rx_csum = strtoul(&ibuf[9], NULL, 16);
		return;
	} 
	if (strncmp(ibuf, "$PMGNWPT,", 7) == 0) {
		waypoint *wpt = mag_wptparse(ibuf);
		waypt_add(wpt);
	} 
	if (IS_TKN("$PMGNVER,")) {
		mag_verparse(ibuf);
return;
	} 
	if (IS_TKN("$PMGNCMD,END")) {
		found_done = 1;
return;
	} 
	mag_writeack(isum);
}

static void
mag_rd_init(const char *portname)
{
	struct termios new_tio;
	magfile = fopen(portname, "rw+b");
	
	if (magfile == NULL) {
		perror("Open failed");
		return;
	}

	magfd = fileno(magfile);
	tcgetattr(magfd, &orig_tio);
	new_tio = orig_tio;
	new_tio.c_iflag &= ~(IGNBRK|BRKINT|PARMRK|ISTRIP|INLCR|IGNCR|ICRNL|
		IXON);
	new_tio.c_oflag &= ~OPOST;
	new_tio.c_lflag &= ~(ECHO|ECHONL|ICANON|ISIG|IEXTEN);
	new_tio.c_cflag &= ~(CSIZE|PARENB);
	new_tio.c_cflag |= CS8;

	cfsetospeed(&new_tio, B4800);
	cfsetispeed(&new_tio, B4800);
	tcsetattr(magfd, TCSAFLUSH, &new_tio);
	
	mag_handon();
	mag_writemsg("PMGNCMD,VERSION");
	mag_writemsg("PMGNCMD,NMEAOFF");
	return;
}

static void
mag_deinit(void)
{
	mag_handoff();
	tcsetattr(magfd, TCSANOW, &orig_tio);
	fclose(magfile);
}

/*
 * Given an incoming track messages of the form:
 * $PMGNTRK,3605.259,N,08644.389,W,00151,M,201444.61,A,,020302*66
 * create and return a populated waypoint.
 */
waypoint * 
mag_trkparse(char *trkmsg)
{
	double latdeg, lngdeg;
	int alt;
	char altunits;
	char lngdir, latdir;
	int dmy;
	int hms;
	int fracsecs;
	struct tm tm;
	waypoint *waypt;

	waypt  = calloc(sizeof *waypt, 1);
	if (waypt == NULL) 
		return NULL;

	printf("%s\n", trkmsg);
	memset(&tm, 0, sizeof(tm));
#if 0
	sscanf(trkmsg,"$PMGNTRK,%lf,%c,%lf,%c,%d,%c,%d.%d,A,,%d", 
		&latdeg,&latdir,
		&lngdeg,&lngsecs,&lngdir,
		&alt,&altunits,&hms,&fracsecs,&dmy);
#else
latdeg = 123;
#endif

	tm.tm_sec = hms % 100;
	hms = hms / 100;
	tm.tm_min = hms % 100;
	hms = hms / 100;
	tm.tm_hour = hms % 100;

	tm.tm_mon =  dmy % 100;
	dmy = dmy / 100;
	tm.tm_mday = dmy % 100; 
	dmy = dmy / 100;
	tm.tm_year = 100 + dmy % 100;

	/*
	 * FIXME: mktime assumes the struct tm is in local time, which 
	 * ours is not...
 	 */
	waypt->time_created = mktime(&tm);

	waypt->position.latitude.degrees = latdeg / 100.0;
	waypt->position.longitude.degrees = lngdeg / 100.0;
	waypt->position.altitude.altitude_meters = alt;

	return waypt;
	
}

const char *
mag_find_descr_from_token(const char *token)
{
	icon_mapping_t *i = icon_mapping;

	if (icon_mapping == NULL) {
		return "unknown";
	}

	for (i = icon_mapping; i->token; i++) {
		if (strcmp(token, i->token) == 0)
			return i->icon;
	}
	return icon_mapping[0].icon;
}

const char *
mag_find_token_from_descr(const char *icon)
{
	icon_mapping_t *i = icon_mapping;

	if (i == NULL || icon == NULL) {
		return "a";
	}

	for (i = icon_mapping; i->token; i++) {
		if (strcmp(icon, i->icon) == 0)
			return i->token;
	}
	return icon_mapping[0].token;
}


/*
 * Given an incoming waypoint messages of the form:
 * $PMGNWPL,3549.499,N,08650.827,W,0000257,M,HOME,HOME,c*4D
 * create and return a populated waypoint.
 */
waypoint * 
mag_wptparse(char *trkmsg)
{
	double latdeg, lngdeg;
	char latdir;
	char lngdir;
	int alt;
	char altunits;
	char shortname[100];
	char descr[100];
	char icon_token[100];
	waypoint *waypt;
	char *icons;
	char *icone;
	char *blah;
	int i = 0;

	descr[0] = 0;
	icon_token[0] = 0;

	waypt  = calloc(sizeof *waypt, 1);
	if (waypt == NULL) 
		fatal("Magproto: Cannot allocate memory\n");

	sscanf(trkmsg,"$PMGNWPL,%lf,%c,%lf,%c,%d,%c,%[^,],%[^,]", 
		&latdeg,&latdir,
		&lngdeg,&lngdir,
		&alt,&altunits,shortname,descr);
	icone = strrchr(trkmsg, '*');
	icons = strrchr(trkmsg, ',')+1;
	
	for (blah = icons ; blah < icone; blah++)
		icon_token[i++] = *blah;

	if (latdir == 'S') latdeg = -latdeg;
	waypt->position.latitude.degrees = latdeg / 100.0;

	if (lngdir == 'W') lngdeg = -lngdeg;
	waypt->position.longitude.degrees = lngdeg / 100.0;

	waypt->position.altitude.altitude_meters = alt;
	waypt->shortname = strdup(shortname);
	waypt->description = strdup(descr);
	waypt->icon_descr = mag_find_descr_from_token(icon_token);

	return waypt;
}

#if BLERF

int main(void)
{
char trk1[] = "$PMGNTRK,3605.259,N,08644.389,W,00151,M,201444.61,A,,020302*66";
char trk2[] = "$PMGNTRK,3605.315,N,08644.625,W,00153,M,211618.68,A,,020302*6D";
char trk3[] = "$PMGNTRK,3605.320,N,08644.619,W,00164,M,212121.87,A,,020302*6F";
char wpt1[] = "$PMGNWPL,3604.831,N,08659.737,W,0000000,M,PowerPln,Power Plant by White Dog Pack,a*50";
char wpt2[] = "$PMGNWPL,3614.645,N,08641.885,W,0000000,M,CmbrlndR,Cumberland River Cache by GISG,a*7B";
char wpt3[] = "$PMGNWPL,3608.210,N,08648.389,W,0000000,M,Puff!,Puff! by White Dog Pack,a*61";
waypt_init();
route_init();

#if 1
mag_trkparse(trk1);
mag_trkparse(trk2);
mag_trkparse(trk3);

exit(0);
#endif

#if 1
mag_wptparse(wpt1);
mag_wptparse(wpt2);
mag_wptparse(wpt3);
exit(0);
#endif
	mag_init();

#if 1
	mag_writemsg("PMGNCMD,WAYPOINT");
while (!found_done) {
 mag_readmsg();
}
exit(0);
#endif
#if 0
	mag_writemsg("PMGNCMD,TRACK,2");
while (!found_done) {
 mag_readmsg();
}
#endif
#if 1
	mag_writemsg("PMGNCMD,ROUTE");
while (!found_done) {
 mag_readmsg();
}
exit(0);
#endif
#if 0
	mag_writemsg("PMGNCMD,FIL,FIRST,?");
sleep(2);
 mag_readmsg();
while (!found_done) {
  mag_writemsg("PMGNCMD,FIL,NEXT");
 mag_readmsg();
sleep(2);
}
#endif
	mag_deinit();
	return 0;
}

#if SOON
/* ROUTES */
READ: $PMGNRTE,3,1,c,19,HOME,c,KidsAtPl,a*0F
READ: $PMGNRTE,3,2,c,19,TrainWrc,a,Leipers,a*6F
READ: $PMGNRTE,3,3,c,19,Nashvill,a,HOME,c*1A

/* WAYPOINTS */
READ: $PMGNWPL,3604.831,N,08659.737,W,0000000,M,PowerPln,Power Plant by White Do
READ: $PMGNWPL,3614.645,N,08641.885,W,0000000,M,CmbrlndR,Cumberland River Cache
READ: $PMGNWPL,3608.210,N,08648.389,W,0000000,M,Puff!,Puff! by White Dog Pack,a*
61

#endif
#endif

void
mag_readwpt(void)
{
	mag_writemsg("PMGNCMD,WAYPOINT");
	while (!found_done) {
		mag_readmsg();
	}
}

static
void
mag_waypt_pr(waypoint *waypointp)
{
	double lon, lat;
	double ilon, ilat;
	char obuf[200];
	ilat = waypointp->position.latitude.degrees;
	ilon = waypointp->position.longitude.degrees;

	lon =fabs(ilon * 100.0);
	lat =fabs(ilat * 100.0);

	sprintf(obuf, "PMGNWPL,%4.3f,%c,%05.3f,%c,%.lf,M,%-.8s,%-.20s,%s",
		lat, ilon < 0 ? 'N' : 'S',
		lon, ilat < 0 ? 'E' : 'W',
		waypointp->position.altitude.altitude_meters,
		waypointp->shortname,
		waypointp->description,
		waypointp->icon_descr);
	mag_writemsg(obuf);
	mag_readmsg();
}


void
mag_write(void)
{
#if 1
		mag_readmsg();
		mag_readmsg();
		mag_readmsg();
		mag_readmsg();
#endif
	waypt_disp_all(mag_waypt_pr);
	
}

ff_vecs_t mag_vecs = {
	mag_rd_init,	
	mag_rd_init,	
	mag_deinit,	
	mag_deinit,	
	mag_readwpt,
	mag_write,
};
