/*
    Support for GPS TrackMaker data file.

    Copyright (C) 2005  Gustavo Niemeyer <gustavo@niemeyer.net>.

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
#include "jeeps/gpsmath.h"

static FILE *fd, *ofd;
static int indatum;
static int wp_count;
static int ws_count;
static int tr_count;
static int ts_count;
static int rt_count;
static int im_count;
static const route_head *rte_active;
static int start_new;

#define MYNAME "GTM"
#define EPOCH89DIFF 631065600
/* was 631076400 but that seems to include a three-hour bias */
#define WAYPOINTSTYLES \
	"\xf5\xff\xff\xff\x0f\x00Times New Roman\x00\x00\x00\x00\x00\x90\x01"\
	"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"\
	"\xf5\xff\xff\xff\x0f\x00Times New Roman\x01\x00\x00\x00\x00\x90\x01"\
	"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"\
	"\xf5\xff\xff\xff\x0f\x00Times New Roman\x02\x00\x00\x00\x00\x90\x01"\
	"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"\
	"\xf5\xff\xff\xff\x0f\x00Times New Roman\x03\x00\x00\x00\x00\x90\x01"\
	"\x00\x00\x00\x00\x00\x00\x8b\xff\xff\xff\xff\x00\x00\x00\x00\x00\x01"

#define unknown_alt_gtm -10000000

/* Read functions, according to specification. */

static void
fread_discard(FILE *fd, int len)
{
	char buf[1024];
	fread(buf, 1, len, fd);
}

static unsigned char
fread_byte(FILE *fd)
{
	unsigned char buf[1];
	fread(buf, 1, 1, fd);
	return buf[0];
}

static short int
fread_bool(FILE *fd)
{
	char buf[2];
	fread(buf, 2, 1, fd);
	return le_read16(buf) ? 1 : 0;
}

static short int
fread_integer(FILE *fd)
{
	char buf[2];
	fread(buf, 2, 1, fd);
	return le_read16(buf);
}

static int
fread_long(FILE *fd)
{
	char buf[4];
	fread(buf, 4, 1, fd);
	return le_read32(buf);
}

static float
fread_single(FILE *fd)
{
	unsigned char buf[4];
	float f;
	int i;
	fread(buf, 4, 1, fd);
	i = le_read32(buf);
	memcpy(&f, &i, 4);
	return f;
}

static double
fread_double(FILE *fd)
{
	char buf[8];
	double d;
	fread(buf, 8, 1, fd);
	le_read64(&d, buf);
	return d;
}

static char *
fread_string(FILE *fd)
{
	int len = fread_integer(fd);
	char *val;
	
	if (len == 0) return NULL;
	
	val = xmalloc(len+1);
	fread(val, 1, len, fd);
	while (len != 0 && val[len-1] == ' ')
		len--;
	val[len] = 0;
	return val;
}

static void
fread_string_discard(FILE *fd)
{
	char *temp = fread_string(fd);
	if (temp != NULL) {
		xfree(temp);
	}
}

static char *
fread_fixedstring(FILE *fd, int len)
{
	char *val = xmalloc(len+1);
	fread(val, 1, len, fd);
	while (len != 0 && val[len-1] == ' ')
		len--;
	val[len] = 0;
	return val;
}

/* Write functions, according to specification. */

static void
fwrite_null(FILE *fd, int len)
{
	char buf[1024];
	memset(buf, 0, len);
	fwrite(buf, 1, len, fd);
}

static void
fwrite_byte(FILE *fd, unsigned char val)
{
	fwrite(&val, 1, 1, fd);
}

static void
fwrite_bool(FILE *fd, short int val)
{
	char buf[2];
	buf[0] = buf[1] = val ? 0xff : 0x00;
	fwrite(buf, 2, 1, fd);
}

static void
fwrite_integer(FILE *fd, short int val)
{
	char buf[2];
	le_write16(buf, val);
	fwrite(buf, 2, 1, fd);
}

static void
fwrite_long(FILE *fd, int val)
{
	char buf[4];
	le_write32(buf, val);
	fwrite(buf, 4, 1, fd);
}

static void
fwrite_single(FILE *fd, float val)
{
	char buf[4];
	int i;
	memcpy(&i, &val, 4);
	le_write32(buf, i);
	fwrite(buf, 4, 1, fd);
}

static void
fwrite_double(FILE *fd, double val)
{
	char buf[8];
	le_read64(buf, &val);
	fwrite(buf, 8, 1, fd);
}

static void
fwrite_string(FILE *fd, const char *str)
{
	if (str && str[0]) {
		int len = strlen(str);
		fwrite_integer(fd, len);
		fwrite(str, 1, len, fd);
	}
	else {
		fwrite_integer(fd, 0);
	}
}

void
fwrite_fixedstring(FILE *fd, const char *str, int fieldlen)
{
	int len = str ? strlen(str) : 0;
	if (len > fieldlen)
		len = fieldlen;
	if (str)
		fwrite(str, 1, len, fd);
	for (; len != fieldlen; len++)
		fputc(' ', fd);
}

/* Auxiliar functions */

void
set_datum(int n)
{
	indatum = -1;
	if (n < 1) {}
	else if (n < 8)  { indatum = 0; } /* Adindan */
	else if (n < 9)  { indatum = 1; } /* Afgooye */
	else if (n < 10) { indatum = 2; } /* Ain el Abd */
	else if (n < 14) {}
	else if (n < 23) { indatum = 6; } /* ARC 1950 */
	else if (n < 26) { indatum = 7; } /* ARC 1960 */
	else if (n < 27) { indatum = 8; } /* Ascension Island 58 */
	else if (n < 32) {}
	else if (n < 33) { indatum = 13; } /* Australian Geo 84 */
	else if (n < 34) {}
	else if (n < 35) { indatum = 15; } /* Bellevue IGN */
	else if (n < 36) { indatum = 16; } /* Bermuda 1957 */
	else if (n < 38) {}
	else if (n < 39) { indatum = 17; } /* Bukit Rimpah */
	else if (n < 40) { indatum = 18; } /* Camp Area Astro */
	else if (n < 41) { indatum = 19; } /* Campo Inchauspe */
	else if (n < 42) { indatum = 22; } /* Canton Islan 1966 */
	else if (n < 43) { indatum = 23; } /* Cape */
	else if (n < 44) { indatum = 24; } /* Cape Canaveral */
	else if (n < 45) { indatum = 26; } /* Carthe */
	else if (n < 46) { indatum = 28; } /* Chatham */
	else if (n < 47) { indatum = 29; } /* Chua Astro */
	else if (n < 48) { indatum = 30; } /* Corrego Alegre*/
	else if (n < 50) {}
	else if (n < 51) { indatum = 33; } /* Djakarta (Batavia) */
	else if (n < 52) { indatum = 34; } /* DOS 1968 */
	else if (n < 53) { indatum = 35; } /* Easter Island 1967 */
	else if (n < 54) {}
	else if (n < 69) { indatum = 38; } /* European 1950 Mean */
	else if (n < 70) { indatum = 39; } /* European 1979 Mean */
	else if (n < 71) {}
	else if (n < 72) { indatum = 41; } /* Gandajika */
	else if (n < 73) { indatum = 42; } /* Geodetic Datum 49 */
	else if (n < 74) {}
	else if (n < 75) { indatum = 45; } /* Guam 1963 */
	else if (n < 76) { indatum = 46; } /* Gunung Segara */
	else if (n < 77) {}
	else if (n < 78) { indatum = 49; } /* Hearth North */
	else if (n < 79) {}
	else if (n < 80) { indatum = 50; } /* Hjorsey 1955 */
	else if (n < 81) { indatum = 51; } /* Hong Kong 1963 */
	else if (n < 82) { indatum = 52; } /* Hu-Tzu-Shan */
	else if (n < 89) { indatum = 53; } /* Indian */
	else if (n < 90) {}
	else if (n < 91) { indatum = 55; } /* Ireland 1965 */
	else if (n < 92) {}
	else if (n < 93) { indatum = 56; } /* ISTS 073 69 */
	else if (n < 94) { indatum = 57; } /* Johnston Island 61 */
	else if (n < 95) { indatum = 58; } /* Kandawala */
	else if (n < 96) { indatum = 59; } /* Kerguelen Island */
	else if (n < 97) { indatum = 60; } /* Kertau 48 */
	else if (n < 99) {}
	else if (n < 100) { indatum = 61; } /* L.C. 5 Astro */
	else if (n < 101) {}
	else if (n < 102) { indatum = 63; } /* Liberia 1964 */
	else if (n < 104) { indatum = 64; } /* Luzon */
	else if (n < 105) {}
	else if (n < 106) { indatum = 65; } /* Mahe 1971 */
	else if (n < 107) {}
	else if (n < 108) { indatum = 69; } /* Merchich */
	else if (n < 109) { indatum = 71; } /* Midway Astro 61 */
	else if (n < 111) { indatum = 73; } /* Minna */
	else if (n < 112) {}
	else if (n < 115) { indatum = 75; } /* Nahrwan */
	else if (n < 116) { indatum = 76; } /* Naparima BWI  */
	else if (n < 116) {}
	else if (n < 119) { indatum = 3;  } /* Alaska NAD27 */
	else if (n < 121) { indatum = 14; } /* Bahamas NAD27 */
	else if (n < 126) { indatum = 20; } /* Canada Mean NAD27 */
	else if (n < 127) { indatum = 21; } /* Canal Zone NAD27 */
	else if (n < 128) { indatum = 31; } /* Cuba NAD27 */
	else if (n < 129) { indatum = 44; } /* Greenland NAD27 */
	else if (n < 131) {}
	else if (n < 132) { indatum = 20; } /* Canada Mean NAD27 */
	else if (n < 135) {}
	else if (n < 136) { indatum = 70; } /* Mexico NAD27 */
	else if (n < 144) {}
	else if (n < 145) { indatum = 80; } /* Old Egyptian */
	else if (n < 146) { indatum = 81; } /* Old Hawaiian */
	else if (n < 147) { indatum = 82; } /* Old Hawaiian Kauai */
	else if (n < 148) { indatum = 83; } /* Old Hawaiian Maui */
	else if (n < 149) { indatum = 81; } /* Old Hawaiian Mean */
	else if (n < 150) { indatum = 84; } /* Old Hawaiian Oahu */
	else if (n < 151) { indatum = 85; } /* Oman */
	else if (n < 156) { indatum = 86; } /* OSG Britain */
	else if (n < 157) { indatum = 87; } /* Pico de Las Nieves */
	else if (n < 158) { indatum = 88; } /* Pitcairn Astro 67 */
	else if (n < 171) {}
	else if (n < 172) { indatum = 91; } /* Puerto Rico */
	else if (n < 173) { indatum = 92; } /* Pulkovo 1942 */
	else if (n < 174) { indatum = 94; } /* Quatar National */
	else if (n < 176) {}
	else if (n < 177) { indatum = 95; } /* Rome 1940 */
	else if (n < 184) { indatum = 96; } /* S-42 (Pulkovo 1942) */
	else if (n < 185) {}
	else if (n < 186) { indatum = 100; } /* Santo DOS */
	else if (n < 187) { indatum = 99; } /* Sao Braz */
	else if (n < 191) {}
	else if (n < 193) { indatum = 105; } /* SAD-69/Mean */
	else if (n < 194) { indatum = 98;  } /* SAD-69/Brazil */
	else if (n < 204) { indatum = 105; } /* SAD-69/Mean */
	else if (n < 205) { indatum = 106; } /* South Asia */
	else if (n < 206) { indatum = 109; } /* Tananarive 1926 */
	else if (n < 207) { indatum = 111; } /* Timbalai 1948 */
	else if (n < 211) { indatum = 112; } /* Tokyo mean */
	else if (n < 212) { indatum = 113; } /* Tristan Astro 1968 */
	else if (n < 213) { indatum = 115; } /* Viti Levu 1916 */
	else if (n < 215) {}
	else if (n < 216) { indatum = 116; } /* Wake Eniwetok 1960 */
	else if (n < 217) { indatum = 117; } /* WGS 72 */
	else if (n < 218) { indatum = 118; } /* WGS 84 */
	else if (n < 219) { indatum = 119; } /* Yacare */
	else if (n < 220) { indatum = 120; } /* Zanderij */
	else if (n < 231) {}
	else if (n < 232) { indatum = 98;  } /* SAD-69/Brazil*/
	else if (n < 234) {}
	else if (n < 235) { indatum = 117; } /* WGS 72 */
	else if (n < 236) { indatum = 0;   } /* Adindan */
	else if (n < 237) { indatum = 2;   } /* Ain el Abd */
	else if (n < 238) { indatum = 7;   } /* ARC 1960 */
	else if (n < 239) { indatum = 8;   } /* Ascension Island 58 */
	else if (n < 241) {}
	else if (n < 242) { indatum = 52;  } /* Hu-Tzu-Shan */
	else if (n < 245) { indatum = 53;  } /* Indian */
	else if (n < 246) {}
	else if (n < 247) { indatum = 57;  } /* Johnston Island 61 */
	else if (n < 248) { indatum = 64;  } /* Luzon */
	else if (n < 249) {}
	else if (n < 250) { indatum = 75; } /* Nahrwan */
	else if (n < 251) { indatum = 76; } /* Naparima BWI  */
	else if (n < 254) {}
	else if (n < 255) { indatum = 82; } /* Old Hawaiian Kauai */
	else if (n < 256) { indatum = 83; } /* Old Hawaiian Maui */
	else if (n < 257) { indatum = 84; } /* Old Hawaiian Oahu */
	else if (n < 259) {}
	else if (n < 260) { indatum = 101; } /* Sapper Hill 43 */
	else if (n < 261) { indatum = 111; } /* Timbalai 1948 */
	else if (n < 262) { indatum = 112; } /* Tokyo mean */
	else if (n < 263) { indatum = 116; } /* Wake Eniwetok 1960 */

	if (indatum == -1)
		warning(MYNAME ": Unsupported datum (%d), won't convert to WGS84\n", n);
}

static const char *icon_descr[] = {
"", "Airport", "Ball Park", "Bank", "Bar", "Boat Ramp", "Campground", "Car",
"City (Large)", "City (Medium)", "City (Small)", "Dam", "Danger Area",
"Drinking Water", "Fishing Area", "Gas Station", "Glider Area", "Golf Course",
"Heliport", "Hotel", "Animals", "Information", "Man Overboard", "Marina",
"Mine", "Medical Facility", "Parachute Area", "Park", "Parking Area",
"Picnic Area", "Private Field", "Residence", "Restaurant", "Restroom",
"Scenic Area", "School", "Seaplane Base", "Shipwreck", "Shopping Center",
"Short Tower", "Policy Station", "Ski Resort", "Soft Field", "Swimming Area",
"Tall Tower", "Telephone", "Tracback Point", "Ultralight Area", "Waypoint",
"Boat", "Exit", "Flag", "Duck", "Buoy", "Back Track", "Beach", "Bridge",
"Building", "Car Repair", "Cemetery", "Church", "Civil", "Convenience Store",
"Crossing", "Fast Food", "Forest", "Ghost Town", "Levee", "Military",
"Oil Field", "Post Office", "Rv Park", "Scales", "Summit", "Toll Booth",
"Trail Head", "Truck Stop", "Tunnel", "Highway", "Gate", "Fall", "Fence",
"Mata-Burro", "Fitness Center", "Movie Theater", "Live Theater", "Zoo", "Horn",
"Bowling", "Car Rental", "City (Capitol)", "Controlled Area", "Stadium",
"Museum", "Amusement Park", "Skull", "Department Store", "Pharmacy", "Pizza",
"Diver Down Flag 1", "Light", "Pin", "", "Pigsty", "Tree", "Bamboo",
"Banana Plant", "Arrow-Down", "Bifurcation", "Cavern", "River", "Rock",
"Arrow-Up", "Trunk", "Soccer Field", "Sporting Court", "Flag, Green", "Trench",
"Ship-Yellow", "Green Sign", "Swamp", "Lake", "Stop!",
"Fishing Hot Spot Facility", "Speed Reducer", "Stairway", "Cactus", "Ship-Red",
"Letter - S", "Letter - D", "Letter - N",
"Crossing", "Cross", "Flag, Red", "Curve1", "Curve2", "Curve3", "Curve4",
"Letter - W", "Letter - L", "Letter - R", "Radio Beacon", "Road Sign",
"Geocache", "Geocache Found", "Traffic Light", "Bus Station", "Train Station",
"School", "Mile Marker", "Conservation Area", "Waypoint", "Box", "Aerial",
"Auto Repair", "Boat", "Exit Ramp", "Fixed Nav Aid", "Floating Buoy", "Garden",
"Fish Farm", "Lighthouse", "Truck Service", "Resort", "Scuba", "Shooting",
"Sight Seeing", "Sounding", "Winery", "Navaid, Amber", "Navaid, Black",
"Navaid, Blue", "Navaid, Green", "Navaid, Green/Red", "Navaid, Green/White",
"Navaid, Orange", "Navaid, Red", "Navaid, Red/Green", "Navaid, Red/White",
"Navaid, Violet", "Navaid, White", "Navaid, White/Green", "Navaid, White/Red",
"Buoy, White", "Dot, White", "Red Square", "Red Diamond", "Green Square",
"Green Diamond", "Restricted Area", "Navaid (unlit)", "Dot (Small)", "Libraries", "Waypoint", "Waypoint1",
"Waypoint2", "Mark (1)", "Mark (2)", "Mark (3)", "Cross (Red)", "Store",
"Exclamation", "Flag (EUA)", "Flag (CAN)", "Flag (BRA)", "Man", "Animals",
"Deer Tracks", "Tree Stand", "Bridge", "Fence", "Intersection",
"Non Direct Beacon", "VHF Omni Range", "Vor/Tacan", "Vor-Dme",
"1st Approach Fix", "Localizer Outer", "Missed Appr. Pt", "Tacan",
"CheckPoint", NULL
};


void convert_datum(double *lat, double *lon)
{
	double amt;
	if (indatum != -1 && indatum != 118) {
            GPS_Math_Known_Datum_To_WGS84_M(*lat, *lon, 0.0,
			    		    lat, lon, &amt, indatum);
	}
}

/* Callbacks */

static void
gtm_rd_init(const char *fname)
{
	int version;
	char *name;
	fd = xfopen(fname, "rb", MYNAME);
	version = fread_integer(fd);
	name = fread_fixedstring(fd, 10);
	if (version == -29921)
		fatal(MYNAME ": Uncompress the file first\n");
	if (strcmp(name, "TrackMaker") != 0)
		fatal(MYNAME ": Invalid file format\n");
	if (version != 211)
		fatal(MYNAME ": Invalid format version\n");
	xfree(name);

	/* Header */
	fread_discard(fd, 15);
	ws_count = fread_long(fd);
	fread_discard(fd, 4);
	wp_count = fread_long(fd);
	tr_count = fread_long(fd);
	rt_count = fread_long(fd);
	fread_discard(fd, 16);
	im_count = fread_long(fd);
	ts_count = fread_long(fd);
	fread_discard(fd, 28);
	fread_string_discard(fd);
	fread_string_discard(fd);
	fread_string_discard(fd);
	fread_string_discard(fd);

	/* User Grid and Datum */
	fread_discard(fd, 34);
	set_datum(fread_integer(fd));
	fread_discard(fd, 22);
}

static void 
gtm_rd_deinit(void) 
{
	fclose(fd);
}

static void count_route_waypts(const waypoint *wpt) { rt_count++; }
static void count_track_waypts(const waypoint *wpt) { tr_count++; }

static void
gtm_wr_init(const char *fname)
{
	rt_count = tr_count = 0;
	track_disp_all(NULL, NULL, count_track_waypts);
	route_disp_all(NULL, NULL, count_route_waypts);

	ofd = xfopen(fname, "wb", MYNAME);

	/* Header */
	fwrite_integer(ofd, 211);
	fwrite_fixedstring(ofd, "TrackMaker", 10);
	fwrite_byte(ofd, 0);
	fwrite_byte(ofd, 0);
	fwrite_byte(ofd, 8);
	fwrite_byte(ofd, 0);
	fwrite_byte(ofd, 0);
	fwrite_byte(ofd, 0);
	fwrite_byte(ofd, 0);
	fwrite_long(ofd, 0);
	fwrite_long(ofd, 16777215);
	fwrite_long(ofd, waypt_count() ? 4 : 0); /* num waypoint styles */
	fwrite_long(ofd, 0);
	fwrite_long(ofd, waypt_count()); /* num waypoints */
	fwrite_long(ofd, tr_count);
	fwrite_long(ofd, rt_count);
	fwrite_single(ofd, 0); /* maxlon */
	fwrite_single(ofd, 0); /* minlon */
	fwrite_single(ofd, 0); /* maxlat */
	fwrite_single(ofd, 0); /* minlat */
	fwrite_long(ofd, 0);
	fwrite_long(ofd, track_count()); /* num tracklog styles */
	fwrite_single(ofd, 0);
	fwrite_single(ofd, 0);
	fwrite_bool(ofd, 0);
	fwrite_bool(ofd, 0);
	fwrite_bool(ofd, 0);
	fwrite_bool(ofd, 0);
	fwrite_bool(ofd, 0);
	fwrite_bool(ofd, 0);
	fwrite_bool(ofd, 0);
	fwrite_bool(ofd, 0);
	fwrite_bool(ofd, 0);
	fwrite_bool(ofd, 0);
	fwrite_string(ofd, "Times New Roman");
	fwrite_string(ofd, "");
	fwrite_string(ofd, "");
	fwrite_string(ofd, "");

	/* User Grid and Datum */
	fwrite_null(ofd, 34);
	fwrite_integer(ofd, 217); /* WGS84 */
	fwrite_null(ofd, 22);
}

static void
gtm_wr_deinit(void)
{
	fclose(ofd);
}

static void
gtm_read(void)
{
	route_head *first_trk_head = NULL;
	route_head *trk_head = NULL;
	route_head *rte_head = NULL;
	waypoint *wpt;
	int real_tr_count = 0;
	char *route_name;
	unsigned int icon;
	int i;

	/* Image information */
	for (i = 0; i != im_count; i++) {
		fread_string_discard(fd);
		fread_string_discard(fd);
		fread_discard(fd, 30);
	}

	/* Waypoints */
	for (i = 0; i != wp_count; i++) {
		wpt = waypt_new();
		wpt->latitude = fread_double(fd);
		wpt->longitude = fread_double(fd);
		convert_datum(&wpt->latitude, &wpt->longitude);
		wpt->shortname = fread_fixedstring(fd, 10);
		wpt->description = fread_string(fd);
		icon = fread_integer(fd);
		if (icon < sizeof(icon_descr)/sizeof(char*))
			wpt->icon_descr = icon_descr[icon];
		fread_discard(fd, 1);
		wpt->creation_time = fread_long(fd);
		if (wpt->creation_time)
			wpt->creation_time += EPOCH89DIFF;
		fread_discard(fd, 2);
		wpt->altitude = fread_single(fd);
		if (wpt->altitude == unknown_alt_gtm)
			wpt->altitude = unknown_alt;
		fread_discard(fd, 2);
		waypt_add(wpt);
	}

	/* Waypoint Styles */
	if (wp_count) {
		for (i = 0; i != ws_count; i++) {
			fread_discard(fd, 4);
			fread_string_discard(fd);
			fread_discard(fd, 24);
		}
	}

	/* Tracklogs */
	for (i = 0; i != tr_count; i++) {
		wpt = waypt_new();
		wpt->latitude = fread_double(fd);
		wpt->longitude = fread_double(fd);
		convert_datum(&wpt->latitude, &wpt->longitude);
		wpt->creation_time = fread_long(fd);
		if (wpt->creation_time)
			wpt->creation_time += EPOCH89DIFF;
		start_new = fread_byte(fd);
		wpt->altitude = fread_single(fd);
		if (wpt->altitude == unknown_alt_gtm)
			wpt->altitude = unknown_alt;
		if (start_new || !trk_head) {
			trk_head = route_head_alloc();
			track_add_head(trk_head);
			real_tr_count++;
			if (!first_trk_head)
				first_trk_head = trk_head;
		}
		route_add_wpt(trk_head, wpt);
	}

	/* Tracklog styles */
	trk_head = first_trk_head;
	for (i = 0; i != ts_count && i != real_tr_count; i++) {
		trk_head->rte_name = fread_string(fd);
		fread_discard(fd, 12);
		trk_head = (route_head *)QUEUE_NEXT(&trk_head->Q);
	}

	/* Routes */
	for (i = 0; i != rt_count; i++) {
		wpt = waypt_new();
		wpt->latitude = fread_double(fd);
		wpt->longitude = fread_double(fd);
		convert_datum(&wpt->latitude, &wpt->longitude);
		wpt->shortname = fread_fixedstring(fd, 10);
		wpt->description = fread_string(fd);
		route_name = fread_string(fd);
		icon = fread_integer(fd);
		if (icon < sizeof(icon_descr)/sizeof(char*))
			wpt->icon_descr = icon_descr[icon];
		fread_discard(fd, 1);
		start_new = fread_byte(fd);
		fread_discard(fd, 6);
		wpt->altitude = fread_single(fd);
		if (wpt->altitude == unknown_alt_gtm)
			wpt->altitude = unknown_alt;
		fread_discard(fd, 2);

		if (start_new || !rte_head) {
			rte_head = route_head_alloc();
			rte_head->rte_name = route_name;
			route_add_head(rte_head);
		}
		else {
			xfree(route_name);
		}
		route_add_wpt(rte_head, wpt);
	}
}

int icon_from_descr(const char *descr)
{
	int i = 0;
	if (descr) {
		for (; i != sizeof(icon_descr)/sizeof(char*); i++)
			if (strcmp(icon_descr[i], descr) == 0)
				return i;
	}
	return 48;
}

static void write_waypt(const waypoint *wpt)
{
	fwrite_double(ofd, wpt->latitude);
	fwrite_double(ofd, wpt->longitude);
	fwrite_fixedstring(ofd, wpt->shortname, 10);
	fwrite_string(ofd, wpt->description);
	fwrite_integer(ofd, icon_from_descr(wpt->icon_descr));
	fwrite_byte(ofd, 3);
	if (wpt->creation_time)
		fwrite_long(ofd, wpt->creation_time-EPOCH89DIFF);
	else
		fwrite_long(ofd, 0);
	fwrite_integer(ofd, 0);
	if (wpt->altitude == unknown_alt)
		fwrite_single(ofd, unknown_alt_gtm);
	else
		fwrite_single(ofd, wpt->altitude);
	fwrite_integer(ofd, 0);
}

static void start_rte(const route_head *rte)
{
	rte_active = rte;
	start_new = 1;
}

static void write_trk_waypt(const waypoint *wpt)
{
	fwrite_double(ofd, wpt->latitude);
	fwrite_double(ofd, wpt->longitude);
	fwrite_long(ofd, wpt->creation_time-EPOCH89DIFF);
	fwrite_byte(ofd, start_new);
	if (wpt->altitude == unknown_alt)
		fwrite_single(ofd, unknown_alt_gtm);
	else
		fwrite_single(ofd, wpt->altitude);
	start_new = 0;
}

static void write_trk_style(const route_head *trk)
{
	fwrite_string(ofd, trk->rte_name);
	fwrite_byte(ofd, 1);
	fwrite_long(ofd, 0);
	fwrite_single(ofd, 0);
	fwrite_byte(ofd, 0);
	fwrite_integer(ofd, 0);
}

static void write_rte_waypt(const waypoint *wpt)
{
	fwrite_double(ofd, wpt->latitude);
	fwrite_double(ofd, wpt->longitude);
	fwrite_fixedstring(ofd, wpt->shortname, 10);
	fwrite_string(ofd, wpt->description);
	fwrite_string(ofd, rte_active->rte_name);
	fwrite_integer(ofd, icon_from_descr(wpt->icon_descr));
	fwrite_byte(ofd, 3);
	fwrite_byte(ofd, start_new);
	fwrite_long(ofd, 0);
	fwrite_integer(ofd, 0);
	if (wpt->altitude == unknown_alt)
		fwrite_single(ofd, unknown_alt_gtm);
	else
		fwrite_single(ofd, wpt->altitude);
	fwrite_integer(ofd, 0);
	start_new = 0;
}

static void
gtm_write(void)
{
	waypt_disp_all(write_waypt);
	if (waypt_count())
		fwrite(WAYPOINTSTYLES, 1, sizeof(WAYPOINTSTYLES)-1, ofd);
	track_disp_all(start_rte, NULL, write_trk_waypt);
	track_disp_all(write_trk_style, NULL, NULL);
	route_disp_all(start_rte, NULL, write_rte_waypt);
}

static
arglist_t gtm_args[] = {
	ARG_TERMINATOR
};

ff_vecs_t gtm_vecs = {
	ff_type_file,
	FF_CAP_RW_ALL,
	gtm_rd_init,	
	gtm_wr_init,	
	gtm_rd_deinit,	
	gtm_wr_deinit,	
	gtm_read,
	gtm_write,
	NULL, 
	gtm_args,
};
