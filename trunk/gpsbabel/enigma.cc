/*
	Enigma route and waypoint file format.
        http://www.mglavionics.co.za/Docs/Enigma%20Waypoint%20format.pdf
        Binary data are stored in little endian (Intel)

	Copyright (C) 2009 Tobias Kretschmar, tobias.kretschmar@gmx.de

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
#include <math.h>

#define MYNAME "Enigma binary route and waypoint file format"

#define WTYPE_WAYPOINT        0 // Waypoint of unspecified type
#define WTYPE_AIRPORT         1 // Typical assignment for medium sized airports
#define WTYPE_MAJORAIRPORT    2 // Typical assignment for large and international airports
#define WTYPE_SEAPLANEBASE    3
#define WTYPE_AIRFIELD        4 // Typical assignment for smaller municipal airfields, glider fields etc
#define WTYPE_PRIVATEAIRFIELD 5
#define WTYPE_ULTRALIGHTFIELD 6
#define WTYPE_INTERSECTION    7 // (reporting point, boundary crossing)
#define WTYPE_HELIPORT        8
#define WTYPE_TACAN           9
#define WTYPE_NDBDME          10
#define WTYPE_NDB             11
#define WTYPE_VORDME          12
#define WTYPE_VORTAC          13
#define WTYPE_FANMARKER       14
#define WTYPE_VOR             15
#define WTYPE_REPPT           16
#define WTYPE_LFR             17
#define WTYPE_UHFNDB          18
#define WTYPE_MNDB            19
#define WTYPE_MNDBDME         20
#define WTYPE_LOM             21
#define WTYPE_LMM             22
#define WTYPE_LOCSDF          23
#define WTYPE_MLSISMLS        24
#define WTYPE_OTHERNAV        25 // Navaid not falling into any of the above types
#define WTYPE_ALTITUDECHANGE  26 // Location at which altitude should be changed

union wpt_data {
  int32_t     wp_altitude;  // Waypoint type 0-6,8: waypoint altitude in feet
  int32_t     tg_altitude;  // Waypoint type 26: target altitude in feet
  uint32_t    frequency;    // Waypoint type 9-25: freq in steps of 1000Hz (118Mhz = 180000)
  int32_t     dummy;        // waypoint type 7, unused
};

typedef struct enigma_wpt {
  int32_t			latitude;
  int32_t			longitude;
  union wpt_data  data;
  uint8_t         waypoint_type;
  uint8_t         shortname_len;
  char            shortname[6];
  uint8_t         longname_len;
  char            longname[27];
} ENIGMA_WPT;

static gbfile* file_in, *file_out;

static void
rd_init(const char* fname)
{
  file_in = gbfopen_le(fname, "rb", MYNAME);
}

int32_t decToEnigmaPosition(double val)
{
  int degrees = fabs(val);
  double frac = fabs(val) - degrees;
  int enigmadeg = degrees * 180000;
  int enigmafrac = 180000 * frac;
  int sign = (val < 0) ? -1 : +1;
  return sign * (enigmadeg + enigmafrac);
}

float enigmaPositionToDec(int32_t val)
{
  int deg = abs(val) / 180000;
  int enigmafrac = abs(val) % 180000;
  double frac = (double)enigmafrac / 180000;
  int sign = (val < 0) ? -1 : +1;
  return sign * (deg + frac);
}

static void
data_read(void)
{
  struct enigma_wpt ewpt;
  route_head* route = route_head_alloc();
  route_add_head(route);

  while (1 == gbfread(&ewpt, sizeof(ewpt), 1, file_in)) {
    Waypoint* wpt = new Waypoint;
    wpt->latitude = enigmaPositionToDec(le_read32(&ewpt.latitude));
    wpt->longitude = enigmaPositionToDec(le_read32(&ewpt.longitude));
    char*sn = xstrndup(ewpt.shortname, ewpt.shortname_len);
    wpt->shortname = sn;
    xfree(sn);

    char* ds = xstrndup(ewpt.longname, ewpt.longname_len);
    wpt->description = ds;
    xfree(ds);

    switch (ewpt.waypoint_type) {
    case WTYPE_WAYPOINT:        // 0
    case WTYPE_AIRPORT:         // 1
    case WTYPE_MAJORAIRPORT:    // 2
    case WTYPE_SEAPLANEBASE:    // 3
    case WTYPE_AIRFIELD:        // 4
    case WTYPE_PRIVATEAIRFIELD: // 5
    case WTYPE_ULTRALIGHTFIELD: // 6
    case WTYPE_HELIPORT:        // 8
      // waypoint altitude
      wpt->altitude = FEET_TO_METERS(le_read32(&ewpt.data.wp_altitude) - 1000);
      break;
    case WTYPE_ALTITUDECHANGE:  // 26
      // target altitude
      wpt->altitude = FEET_TO_METERS(le_read32(&ewpt.data.tg_altitude) - 1000);
      break;
    case WTYPE_INTERSECTION:    // 7
      // unused
      break;
    default:
      // frequency
      // wpt->frequency = wpt.le_readu32(ewpt.data.frequency);
      ;
    }
    route_add_wpt(route, wpt);
  }
}

static void
rd_deinit(void)
{
  gbfclose(file_in);
}

static void
wr_init(const char* fname)
{
  file_out = gbfopen_le(fname, "wb", MYNAME);
}

#ifndef min
#define min(a,b) ((a) < (b)) ? (a) : (b)
#endif
#ifndef max
#define max(a,b) ((a) > (b)) ? (a) : (b)
#endif

static void
enigma_waypt_disp(const Waypoint* wpt)
{
  struct enigma_wpt ewpt;

  memset(&ewpt, 0, sizeof(ewpt));

  le_write32(&ewpt.latitude, decToEnigmaPosition(wpt->latitude));
  le_write32(&ewpt.longitude, decToEnigmaPosition(wpt->longitude));
  ewpt.waypoint_type = WTYPE_WAYPOINT;
  if (wpt->altitude != unknown_alt) {
    le_write32(&ewpt.data.wp_altitude, METERS_TO_FEET(wpt->altitude) + 1000);
  }
  if (wpt->shortname != NULL) {
    ewpt.shortname_len = min(6, strlen(CSTRc(wpt->shortname)));
    strncpy(ewpt.shortname, CSTRc(wpt->shortname), 6);
  }
  if (wpt->description != NULL) {
    ewpt.longname_len = min(27, strlen(CSTRc(wpt->description)));
    strncpy(ewpt.longname, CSTRc(wpt->description), 27);
  }
  gbfwrite(&ewpt, sizeof(ewpt), 1, file_out);
}

static void
data_write(void)
{
  route_disp_all(NULL, NULL, enigma_waypt_disp);
}

static void
wr_deinit(void)
{
  gbfclose(file_out);
}

ff_vecs_t enigma_vecs = {
  ff_type_file,
  {
    ff_cap_none,                    /* waypoints */
    ff_cap_none,                    /* tracks */
    (ff_cap)(ff_cap_read | ff_cap_write) 	/* routes */
  },
  rd_init,
  wr_init,
  rd_deinit,
  wr_deinit,
  data_read,
  data_write,
  NULL,
  NULL,
  CET_CHARSET_ASCII, 0	/* CET-REVIEW */
};
