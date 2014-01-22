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

/*
 * Documentation can be found at http://www.trackmaker.com/download/ref_guide_eng.pdf
 */

#include "defs.h"
#include "jeeps/gpsmath.h"

static gbfile* file_in, *file_out;
static int indatum;
static int wp_count;
static int ws_count;
static int tr_count;
static int ts_count;
static int rt_count;
static int im_count;
static const route_head* rte_active;
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

#define fread_discard(a,b) gbfseek(a, (b), SEEK_CUR)
#define fread_byte(a) (unsigned char) gbfgetc(a)

#if 0
/* not used */
static short int
fread_bool(gbfile* fd)
{
  char buf[2];
  gbfread(buf, 2, 1, fd);
  return le_read16(buf) ? 1 : 0;
}
#endif

#define fread_integer(a) gbfgetint16(a)
#define fread_long(a) gbfgetint32(a)
#define fread_single(a) gbfgetflt(a)
#define fread_double(a) gbfgetdbl(a)

QString
fread_string(gbfile* fd)
{
  char* val;
  int len = fread_integer(fd);

  if (len == 0) {
    return NULL;
  }

  val = (char*) xmalloc(len+1);
  gbfread(val, 1, len, fd);
  while (len != 0 && val[len-1] == ' ') {
    len--;
  }
  val[len] = 0;
  QString v(val);
  xfree(val);
  return v;
}

static void
fread_string_discard(gbfile* fd)
{
  fread_string(fd);
}

QString
fread_fixedstring(gbfile* fd, int len)
{
  char* val = (char*) xmalloc(len+1);

  gbfread(val, 1, len, fd);
  while (len != 0 && val[len-1] == ' ') {
    len--;
  }
  val[len] = 0;
  QString v(val);
  xfree(val);

  return v;
}

/* Write functions, according to specification. */

static void
fwrite_null(gbfile* fd, int len)
{
  char buf[1024];

  memset(buf, 0, len);
  gbfwrite(buf, 1, len, fd);
}

#define fwrite_byte(a,b) gbfputc((signed char)(b), a)
#define fwrite_bool(a,b) gbfputuint16((b) ? 0xffff : 0, a)
#define fwrite_integer(a,b) gbfputint16((b), a)
#define fwrite_long(a,b) gbfputint32((b), a)
#define fwrite_single(a,b) gbfputflt((b), a)
#define fwrite_double(a,b) gbfputdbl((b), a)

static void
fwrite_string(gbfile* fd, const char* str)
{
  if (str && str[0]) {
    int len = strlen(str);
    fwrite_integer(fd, len);
    gbfwrite(str, 1, len, fd);
  } else {
    fwrite_integer(fd, 0);
  }
}
static void
fwrite_string(gbfile* fd, const QString& str)
{
  if (str.isEmpty()) {
    fwrite_integer(fd, 0);
  } else {
    fwrite_integer(fd, str.length());
    gbfwrite(str.toLatin1().data(), 1, str.length(), fd);
  }
}

void
fwrite_fixedstring(gbfile* fd, const char* str, int fieldlen)
{
  int len = str ? strlen(str) : 0;

  if (len > fieldlen) {
    len = fieldlen;
  }
  if (str) {
    gbfwrite(str, 1, len, fd);
  }
  for (; len != fieldlen; len++) {
    gbfputc(' ', fd);
  }
}

void
fwrite_fixedstring(gbfile* fd, const QString& str, int fieldlen)
{
  fwrite_fixedstring(fd, CSTR(str), fieldlen);
}

/* Auxiliar functions */

#define MAX_INDATUM_INDEX 263

static const int indatum_array[MAX_INDATUM_INDEX] = {
  -1, // < 1
  0, 0, 0, 0, 0, 0, 0, // < 8 : Adindan
  1, // < 9 : Afgooye
  2, // < 10 : Ain el Abd
  -1, -1, -1, -1, // < 14
  6, 6, 6, 6, 6, 6, 6, 6, 6, // < 23 : ARC 1950
  7, 7, 7, // < 26 : ARC 1960
  8, // < 27 : Ascension Island 58
  -1, -1, -1, -1, -1, // < 32
  13, // < 33 : Australian Geo 84
  -1, // < 34
  15, // < 35 : Bellevue IGN
  16, // < 36 : Bermuda 1957
  -1, -1, // < 38
  17, // < 39 : Bukit Rimpah
  18, // < 40 : Camp Area Astro
  19, // < 41 : Campo Inchauspe
  22, // < 42 : Canton Islan 1966
  23, // < 43 : Cape
  24, // < 44 : Cape Canaveral
  26, // < 45 : Carthe
  28, // < 46 : Chatham
  29, // < 47 : Chua Astro
  30, // < 48 : Corrego Alegre
  -1, -1, // < 50
  33, // < 51 : Djakarta (Batavia)
  34, // < 52 : DOS 1968
  35, // < 53 : Easter Island 1967
  -1, // < 54
  38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, // < 69 : European 1950 Mean
  39, // < 70 : European 1979 Mean
  -1, // < 71
  41, // < 72 : Gandajika
  42, // < 73 : Geodetic Datum 49
  -1, // < 74
  45, // < 75 : Guam 1963
  46, // < 76 : Gunung Segara
  -1, // < 77
  49, // < 78 : Hearth North
  -1, // < 79
  50, // < 80 : Hjorsey 1955
  51, // < 81 : Hong Kong 1963
  52, // < 82 : Hu-Tzu-Shan
  53, 53, 53, 53, 53, 53, 53, // < 89 : Indian
  -1, // < 90
  55, // < 91 : Ireland 1965
  -1, // < 92
  56, // < 93 : ISTS 073 69
  57, // < 94 : Johnston Island 61
  58, // < 95 : Kandawala
  59, // < 96 : Kerguelen Island
  60, // < 97 : Kertau 48
  -1, -1, // < 99
  61, // < 100 : L.C. 5 Astro
  -1, // < 101
  63, // < 102 : Liberia 1964
  64, 64, // < 104 : Luzon
  -1, // < 105
  65, // < 106 : Mahe 1971
  -1, // < 107
  69, // < 108 : Merchich
  71, // < 109 : Midway Astro 61
  73, 73, // < 111 : Minna
  -1, // < 112
  75, 75, 75, // < 115 : Nahrwan
  76, // < 116 : Naparima BWI
  3, 3, 3, // < 119 : Alaska NAD27
  14, 14, // < 121 : Bahamas NAD27
  20, 20, 20, 20, 20, // < 126 : Canada Mean NAD27
  21, // < 127 : Canal Zone NAD27
  31, // < 128 : Cuba NAD27
  44, // < 129 : Greenland NAD27
  -1, -1, // < 131
  20, // < 132 : Canada Mean NAD27
  -1, -1, -1, // < 135
  70, // < 136 : Mexico NAD27
  -1, -1, -1, -1, -1, -1, -1, -1, // < 144
  80, // < 145 : Old Egyptian
  81, // < 146 : Old Hawaiian
  82, // < 147 : Old Hawaiian Kauai
  83, // < 148 : Old Hawaiian Maui
  81, // < 149 : Old Hawaiian Mean
  84, // < 150 : Old Hawaiian Oahu
  85, // < 151 : Oman
  86, 86, 86, 86, 86, // < 156 : OSG Britain
  87, // < 157 : Pico de Las Nieves
  88, // < 158 : Pitcairn Astro 67
  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, // < 171
  91, // < 172 : Puerto Rico
  92, // < 173 : Pulkovo 1942
  94, // < 174 : Quatar National
  -1, -1, // < 176
  95, // < 177 : Rome 1940
  96, 96, 96, 96, 96, 96, 96, // < 184 : S-42 (Pulkovo 1942)
  -1, // < 185
  100, // < 186 : Santo DOS
  99, // < 187 : Sao Braz
  -1, -1, -1, -1, // < 191
  105, 105, // < 193 : SAD-69/Mean
  98, // < 194 : SAD-69/Brazil
  105, 105, 105, 105, 105, 105, 105, 105, 105, 105, // < 204 : SAD-69/Mean
  106, // < 205 : South Asia
  109, // < 206 : Tananarive 1926
  111, // < 207 : Timbalai 1948
  112, 112, 112, 112, // < 211 : Tokyo mean
  113, // < 212 : Tristan Astro 1968
  115, // < 213 : Viti Levu 1916
  -1, -1, // < 215
  116, // < 216 : Wake Eniwetok 1960
  117, // < 217 : WGS 72
  118, // < 218 : WGS 84
  119, // < 219 : Yacare
  120, // < 220 : Zanderij
  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, // < 231
  98, // < 232 : SAD-69/Brazil
  -1, -1, // < 234
  117, // < 235 : WGS 72
  0, // < 236 : Adindan
  2, // < 237 : Ain el Abd
  7, // < 238 : ARC 1960
  8, // < 239 : Ascension Island 58
  -1, -1, // < 241
  52, // < 242 : Hu-Tzu-Shan
  53, 53, 53, // < 245 : Indian
  -1, // < 246
  57, // < 247 : Johnston Island 61
  64, // < 248 : Luzon
  -1, // < 249
  75, // < 250 : Nahrwan
  76, // < 251 : Naparima BWI
  -1, -1, -1, // < 254
  82, // < 255 : Old Hawaiian Kauai
  83, // < 256 : Old Hawaiian Maui
  84, // < 257 : Old Hawaiian Oahu
  -1, -1, // < 259
  101, // < 260 : Sapper Hill 43
  111, // < 261 : Timbalai 1948
  112, // < 262 : Tokyo mean
  116 // < 263 : Wake Eniwetok 1960
};

void set_datum(int n)
{
  indatum = -1;
  if (n > 0 && n < MAX_INDATUM_INDEX) {
    indatum = indatum_array[n];
  }

  if (indatum == -1) {
    warning(MYNAME ": Unsupported datum (%d), won't convert to WGS84\n", n);
  }
}

static const char* icon_descr[] = {
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


void convert_datum(double* lat, double* lon)
{
  double amt;
  if (indatum != -1 && indatum != 118) {
    GPS_Math_Known_Datum_To_WGS84_M(*lat, *lon, 0.0,
                                    lat, lon, &amt, indatum);
  }
}

/* Callbacks */

static void
gtm_rd_init(const char* fname)
{
  int version;
  file_in = gbfopen_le(fname, "rb", MYNAME);
  version = fread_integer(file_in);
  QString name = fread_fixedstring(file_in, 10);
  if (version == -29921) {
    fatal(MYNAME ": Uncompress the file first\n");
  }
  if (name != "TrackMaker") {
    fatal(MYNAME ": Invalid file format\n");
  }
  if (version != 211) {
    fatal(MYNAME ": Invalid format version\n");
  }

  /* Header */
  fread_discard(file_in, 15);
  ws_count = fread_long(file_in);
  fread_discard(file_in, 4);
  wp_count = fread_long(file_in);
  tr_count = fread_long(file_in);
  rt_count = fread_long(file_in);
  fread_discard(file_in, 16);
  im_count = fread_long(file_in);
  ts_count = fread_long(file_in);
  fread_discard(file_in, 28);
  fread_string_discard(file_in);
  fread_string_discard(file_in);
  fread_string_discard(file_in);
  fread_string_discard(file_in);

  /* User Grid and Datum */
  fread_discard(file_in, 34);
  set_datum(fread_integer(file_in));
  fread_discard(file_in, 22);
}

static void
gtm_rd_deinit(void)
{
  gbfclose(file_in);
}

static void count_route_waypts(const Waypoint* wpt)
{
  rt_count++;
}
static void count_track_waypts(const Waypoint* wpt)
{
  tr_count++;
}

static void
gtm_wr_init(const char* fname)
{
  rt_count = tr_count = 0;
  track_disp_all(NULL, NULL, count_track_waypts);
  route_disp_all(NULL, NULL, count_route_waypts);

  file_out = gbfopen_le(fname, "wb", MYNAME);	/* little endian */

  /* Header */
  fwrite_integer(file_out, 211);
  fwrite_fixedstring(file_out, "TrackMaker", 10);
  fwrite_byte(file_out, 0);
  fwrite_byte(file_out, 0);
  fwrite_byte(file_out, 8);
  fwrite_byte(file_out, 0);
  fwrite_byte(file_out, 0);
  fwrite_byte(file_out, 0);
  fwrite_byte(file_out, 0);
  fwrite_long(file_out, 0);
  fwrite_long(file_out, 16777215);
  fwrite_long(file_out, waypt_count() ? 4 : 0); /* num waypoint styles */
  fwrite_long(file_out, 0);
  fwrite_long(file_out, waypt_count()); /* num waypoints */
  fwrite_long(file_out, tr_count);
  fwrite_long(file_out, rt_count);
  fwrite_single(file_out, 0); /* maxlon */
  fwrite_single(file_out, 0); /* minlon */
  fwrite_single(file_out, 0); /* maxlat */
  fwrite_single(file_out, 0); /* minlat */
  fwrite_long(file_out, 0);
  fwrite_long(file_out, track_count()); /* num tracklog styles */
  fwrite_single(file_out, 0);
  fwrite_single(file_out, 0);
  fwrite_bool(file_out, 0);
  fwrite_bool(file_out, 0);
  fwrite_bool(file_out, 0);
  fwrite_bool(file_out, 0);
  fwrite_bool(file_out, 0);
  fwrite_bool(file_out, 0);
  fwrite_bool(file_out, 0);
  fwrite_bool(file_out, 0);
  fwrite_bool(file_out, 0);
  fwrite_bool(file_out, 0);
  fwrite_string(file_out, "Times New Roman");
  fwrite_string(file_out, "");
  fwrite_string(file_out, "");
  fwrite_string(file_out, "");

  /* User Grid and Datum */
  fwrite_null(file_out, 34);
  fwrite_integer(file_out, 217); /* WGS84 */
  fwrite_null(file_out, 22);
}

static void
gtm_wr_deinit(void)
{
  gbfclose(file_out);
}

static void
gtm_read(void)
{
  route_head* first_trk_head = NULL;
  route_head* trk_head = NULL;
  route_head* rte_head = NULL;
  Waypoint* wpt;
  int real_tr_count = 0;
  unsigned int icon;
  int i;

  /* Image information */
  for (i = 0; i != im_count; i++) {
    fread_string_discard(file_in);
    fread_string_discard(file_in);
    fread_discard(file_in, 30);
  }

  /* Waypoints */
  for (i = 0; i != wp_count; i++) {
    wpt = new Waypoint;
    wpt->latitude = fread_double(file_in);
    wpt->longitude = fread_double(file_in);
    convert_datum(&wpt->latitude, &wpt->longitude);
    wpt->shortname = fread_fixedstring(file_in, 10);
    wpt->description = fread_string(file_in);
    icon = fread_integer(file_in);
    if (icon < sizeof(icon_descr)/sizeof(char*)) {
      wpt->icon_descr = icon_descr[icon];
    }
    fread_discard(file_in, 1);
    wpt->SetCreationTime(fread_long(file_in));
    if (wpt->creation_time.isValid()) {
      wpt->creation_time += EPOCH89DIFF;
    }
    fread_discard(file_in, 2);
    wpt->altitude = fread_single(file_in);
    if (wpt->altitude == unknown_alt_gtm) {
      wpt->altitude = unknown_alt;
    }
    fread_discard(file_in, 2);
    waypt_add(wpt);
  }

  /* Waypoint Styles */
  if (wp_count) {
    for (i = 0; i != ws_count; i++) {
      fread_discard(file_in, 4);
      fread_string_discard(file_in);
      fread_discard(file_in, 24);
    }
  }

  /* Tracklogs */
  for (i = 0; i != tr_count; i++) {
    wpt = new Waypoint;
    wpt->latitude = fread_double(file_in);
    wpt->longitude = fread_double(file_in);
    convert_datum(&wpt->latitude, &wpt->longitude);
    wpt->SetCreationTime(fread_long(file_in));
    if (wpt->creation_time.isValid()) {
      wpt->creation_time += EPOCH89DIFF;
    }
    start_new = fread_byte(file_in);
    wpt->altitude = fread_single(file_in);
    if (wpt->altitude == unknown_alt_gtm) {
      wpt->altitude = unknown_alt;
    }
    if (start_new || !trk_head) {
      trk_head = route_head_alloc();
      track_add_head(trk_head);
      real_tr_count++;
      if (!first_trk_head) {
        first_trk_head = trk_head;
      }
    }
    track_add_wpt(trk_head, wpt);
  }

  /* Tracklog styles */
  trk_head = first_trk_head;
  for (i = 0; i != ts_count && i != real_tr_count; i++) {
    trk_head->rte_name = fread_string(file_in);
    fread_discard(file_in, 12);
    trk_head = (route_head*)QUEUE_NEXT(&trk_head->Q);
  }

  /* Routes */
  for (i = 0; i != rt_count; i++) {
    wpt = new Waypoint;
    wpt->latitude = fread_double(file_in);
    wpt->longitude = fread_double(file_in);
    convert_datum(&wpt->latitude, &wpt->longitude);
    wpt->shortname = fread_fixedstring(file_in, 10);
    wpt->description = fread_string(file_in);
    QString route_name = fread_string(file_in);
    icon = fread_integer(file_in);
    if (icon < sizeof(icon_descr)/sizeof(char*)) {
      wpt->icon_descr = icon_descr[icon];
    }
    fread_discard(file_in, 1);
    start_new = fread_byte(file_in);
    fread_discard(file_in, 6);
    wpt->altitude = fread_single(file_in);
    if (wpt->altitude == unknown_alt_gtm) {
      wpt->altitude = unknown_alt;
    }
    fread_discard(file_in, 2);

    if (start_new || !rte_head) {
      rte_head = route_head_alloc();
      rte_head->rte_name = route_name;
      route_add_head(rte_head);
    }
    route_add_wpt(rte_head, wpt);
  }
}

static int icon_from_descr(const QString& descr)
{
  for (int i = 0; icon_descr[i]; i++) {
    if (descr.compare(icon_descr[i]) == 0) {
      return i;
    }
  }
  return 48;
}

static void write_waypt(const Waypoint* wpt)
{
  fwrite_double(file_out, wpt->latitude);
  fwrite_double(file_out, wpt->longitude);
  fwrite_fixedstring(file_out, wpt->shortname, 10);
  fwrite_string(file_out, wpt->description);
  fwrite_integer(file_out, icon_from_descr(wpt->icon_descr));
  fwrite_byte(file_out, 3);
  if (wpt->creation_time.isValid()) {
    fwrite_long(file_out, wpt->GetCreationTime().toTime_t()-EPOCH89DIFF);
  } else {
    fwrite_long(file_out, 0);
  }
  fwrite_integer(file_out, 0);
  if (wpt->altitude == unknown_alt) {
    fwrite_single(file_out, unknown_alt_gtm);
  } else {
    fwrite_single(file_out, wpt->altitude);
  }
  fwrite_integer(file_out, 0);
}

static void start_rte(const route_head* rte)
{
  rte_active = rte;
  start_new = 1;
}

static void write_trk_waypt(const Waypoint* wpt)
{
  fwrite_double(file_out, wpt->latitude);
  fwrite_double(file_out, wpt->longitude);
  fwrite_long(file_out, wpt->GetCreationTime().toTime_t()-EPOCH89DIFF);
  fwrite_byte(file_out, start_new);
  if (wpt->altitude == unknown_alt) {
    fwrite_single(file_out, unknown_alt_gtm);
  } else {
    fwrite_single(file_out, wpt->altitude);
  }
  start_new = 0;
}

static void write_trk_style(const route_head* trk)
{
  fwrite_string(file_out, trk->rte_name);
  fwrite_byte(file_out, 1);
  fwrite_long(file_out, 0);
  fwrite_single(file_out, 0);
  fwrite_byte(file_out, 0);
  fwrite_integer(file_out, 0);
}

static void write_rte_waypt(const Waypoint* wpt)
{
  fwrite_double(file_out, wpt->latitude);
  fwrite_double(file_out, wpt->longitude);
  fwrite_fixedstring(file_out, wpt->shortname, 10);
  fwrite_string(file_out, wpt->description);
  fwrite_string(file_out, rte_active->rte_name);
  fwrite_integer(file_out, icon_from_descr(wpt->icon_descr));
  fwrite_byte(file_out, 3);
  fwrite_byte(file_out, start_new);
  fwrite_long(file_out, 0);
  fwrite_integer(file_out, 0);
  if (wpt->altitude == unknown_alt) {
    fwrite_single(file_out, unknown_alt_gtm);
  } else {
    fwrite_single(file_out, wpt->altitude);
  }
  fwrite_integer(file_out, 0);
  start_new = 0;
}

static void
gtm_write(void)
{
  waypt_disp_all(write_waypt);
  if (waypt_count()) {
    gbfwrite(WAYPOINTSTYLES, 1, sizeof(WAYPOINTSTYLES)-1, file_out);
  }
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
