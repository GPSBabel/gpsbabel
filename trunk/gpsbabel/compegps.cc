/*

    Support for CompeGPS waypoint (.wpt), route (.rte) and track (.trk) files,

    Copyright (C) 2005 Olaf Klein, o.b.klein@gpsbabel.org

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
    History:
    		10/23/2005: First release; only a reader
    		10/25/2005: becomes a writer too
		10/26/2005: received documention from CompeGPS team
		            added fatals for "G" and "U" if not WGS84 and lat/lon
		08/13/2006: switch to gbfile api
*/

/*

    the meaning of leading characters in CompeGPS data lines (enhanced PCX):

    header lines:

	"G": WGS 84			- Datum of the map
	"N": Anybody			- Name of the user
	"L": -02:00:00			- Difference to UTC
	"M": ...			- Any comments
	"R": 16711680 , xxxx , 1 	- Route header
	"U": 1				- System of coordinates (0=UTM 1=Latitude/Longitude)

	"C":  0 0 255 2 -1.000000	- ???
	"V":  0.0 0.0 0 0 0 0 0.0	- ???
	"E": 0|1|00-NUL-00 00:00:00|00:00:00|0 - ???

    data lines:

	"W": if(route) routepoint; else waypoint
	"T": trackpoint
    	"t": if(track) additionally track info
	     if(!track) additionally trackpoint info
	"a": link to ...
	"w": waypoint additional info

*/

#include "defs.h"
#include "csv_util.h"

#if CSVFMTS_ENABLED
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "jeeps/gpsmath.h"

#define MYNAME "CompeGPS"

#define SHORT_NAME_LENGTH 16

static gbfile* fin, *fout;
static int target_index, curr_index;
static int track_info_flag;
static short_handle sh;
static int snlen;
static int radius;
static int input_datum;

static route_head* curr_track;
static route_head* curr_route;

/* placeholders for options */

static char* option_icon;
static char* option_index;
static char* option_radius;
static char* option_snlen;

static
arglist_t compegps_args[] = {
  {
    "deficon", &option_icon, "Default icon name",
    NULL, ARGTYPE_STRING, ARG_NOMINMAX
  },
  {
    "index", &option_index, "Index of route/track to write (if more than one in source)",
    NULL, ARGTYPE_INT, "1", NULL
  },
  {
    "radius", &option_radius, "Give points (waypoints/route points) a default radius (proximity)",
    NULL, ARGTYPE_FLOAT, "0", NULL
  },
  {
    "snlen", &option_snlen, "Length of generated shortnames (default 16)",
    "16", ARGTYPE_INT, "1", NULL
  },
  ARG_TERMINATOR
};

static
void fix_datum(double* lat, double* lon)
{
  double amt;

  /*
   * Avoid FP jitter in the common case.
   */
  if (input_datum != DATUM_WGS84) {
    GPS_Math_Known_Datum_To_WGS84_M(*lat, *lon, 0.0, lat, lon,
                                    &amt, input_datum);
  }
}

static void
compegps_parse_date(const char* c, struct tm* tm)
{
  char month[4];
  int year;
  tm->tm_mday = atoi(c);
  strncpy(month, c+3, 3);
  month[3] = 0;
  tm->tm_mon = month_lookup(month);
  year = atoi(c + 7);
  if (year < 70) {
    year += 100;
  }
  if (year > 1900) {
    year -= 1900;
  }
  tm->tm_year = year;
  // if (tm->tm_year < 70) tm->tm_year += 100;
}

static void
compegps_parse_time(const char* c, struct tm* tm)
{
  tm->tm_hour = atoi(c);
  tm->tm_min = atoi(c+3);
  tm->tm_sec = atoi(c+6);
}

/* specialized readers */

static waypoint*
parse_wpt(char* buff)
{
  int col = -1;
  char* c, *cx;
  waypoint* wpt = waypt_new();
  struct tm tm;
  int has_time = 0;
  memset(&tm, 0, sizeof(tm));

  c = strstr(buff, "A ");
  if (c == buff) {
    col++;
  }

  c = csv_lineparse(buff, " ", "", col++);
  while (c != NULL) {
    c = lrtrim(c);
    if (*c != '\0') {
#if 0
      printf(MYNAME "_read_wpt: col(%d)=%s\n", col, c);
#endif
      switch (col) {
      case 0:

        cx = c + strlen(c) - 1;		/* trim trailing underscores */
        while ((cx >= c) && (*cx == '_')) {
          *cx-- = '\0';
        }
        if (*c != '\0') {
          wpt->shortname = xstrdup(c);
        }
        break;
      case 2:
        human_to_dec(c, &wpt->latitude, NULL, 1);
        break;
      case 3:
        human_to_dec(c, NULL, &wpt->longitude, 2);
        break;
        // Older compegps used a dumb constant.
        // Report are that 2010-era writes a sensible
        // value here.
        /* always "27-MAR-62 00:00:00" */
      case 4:
        if (strcmp(c, "27-MAR-62")) {
          has_time = 1;
          compegps_parse_date(c, &tm);
        }
        break;
      case 5:
        if (has_time) {
          compegps_parse_time(c, &tm);
          wpt->creation_time = mkgmtime(&tm);
        }
      case 6:
        wpt->altitude = atof(c);
        break;
      case 7:
        wpt->description = xstrdup(c);
        break;
      default:
        if (col > 7) {
          wpt->description = xstrappend(wpt->description, " ");
          wpt->description = xstrappend(wpt->description, c);
        }
      }
    }
    c = csv_lineparse(NULL, " ", "", col++);
  }
  fix_datum(&wpt->latitude, &wpt->longitude);
  return wpt;
}

static void
parse_wpt_info(const char* buff, waypoint* wpt)		/* "w" */
{
  char* c;
  int col = -1;
  double fx;

  c = csv_lineparse(buff, ",", "", col++);
  while (c != NULL) {
    c = lrtrim(c);
    if (*c != '\0') {
#if 0
      printf(MYNAME "_read_wpt_info: col(%d)=%s\n", col, c);
#endif
      switch (col) {
      case 0:
        wpt->icon_descr = xstrdup(c);
        wpt->wpt_flags.icon_descr_is_dynamic = 1;
        break;
      case 1:
        break;			/* Text postion */
      case 2:
        break;			/* Lens zoom level */
      case 3:
        break;			/* Text colour */
      case 4:
        break;			/* Background colour */
      case 5:
        break;			/* Transparent text  (0=transparent, 1=no transparent) */
      case 6:
        break;			/* ??? */
      case 7:
        break;			/* ??? */
      case 8: 			/* radius */
        fx = atof(c);
        if (fx > 0) {
          WAYPT_SET(wpt, proximity, fx);
        }
        break;
      }
    }
    c = csv_lineparse(NULL, ",", "", col++);
  }
}

static waypoint*
parse_trkpt(char* buff)
{
  int col = -1;
  char* c;
  struct tm tm;
  waypoint* wpt = waypt_new();

  c = strstr(buff, "A ");
  if (c == buff) {
    col++;
  }

  memset(&tm, 0, sizeof(tm));
  c = csv_lineparse(buff, " ", "", col++);
  while (c != NULL) {
    c = lrtrim(c);
    if (*c != '\0') {
#if 0
      printf(MYNAME "_read_trkpt: col(%d)=%s\n", col, c);
#endif
      switch (col) {
      case 2:
        human_to_dec(c, &wpt->latitude, NULL, 1);
        break;
      case 3:
        human_to_dec(c, NULL, &wpt->longitude, 2);
        break;
      case 4:
        compegps_parse_date(c, &tm);
        break;
      case 5:
        compegps_parse_time(c, &tm);
        wpt->creation_time = mkgmtime(&tm);
        break;
      case 7:
        wpt->altitude = atof(c);
        break;
      }
    }
    c = csv_lineparse(NULL, " ", "", col++);
  }
  fix_datum(&wpt->latitude, &wpt->longitude);
  return wpt;
}

static void
parse_track_info(const char* buff, route_head* track)	/* "t" */
{
  char* c;
  int col = -1;

  c = csv_lineparse(buff, "|", "", col++);
  while (c != NULL) {
    c = lrtrim(c);
    if (*c != '\0') {
#if 0
      printf(MYNAME "_read_track_info: col(%d)=%s\n", col, c);
#endif
      switch (col) {
      case 0:
        break;	/* unknown field */
      case 1:
        track->rte_name = xstrdup(c);
        break;
      case 2:
        break;	/* unknown field */
      case 3:
        break;	/* unknown field */
      }
    }
    c = csv_lineparse(NULL, "|", "", col++);
  }
}

static void
parse_rte_info(const char* buff, route_head* route)	/* "R" */
{
  char* c;
  int col = -1;

  c = csv_lineparse(buff, ",", "", col++);
  while (c != NULL) {
    c = lrtrim(c);
    if (*c != '\0') {
#if 0
      printf(MYNAME "_read_rte_info: col(%d)=%s\n", col, c);
#endif
      switch (col) {
      case 0:
        break;				/* unknown field (colour?) */
      case 1:
        route->rte_name = xstrdup(c);
        break;
      case 2:
        break;				/* unknown field */

      }
    }
    c = csv_lineparse(NULL, ",", "", col++);
  }
}

/* main functions */

static void
compegps_rd_init(const char* fname)
{
  fin = gbfopen(fname, "rb", MYNAME);
  input_datum = DATUM_WGS84;
}

static void
compegps_rd_deinit(void)
{
  gbfclose(fin);
}

static void
compegps_data_read(void)
{
  char* buff;
  int line = 0;
  int input_datum;
  waypoint* wpt = NULL;
  route_head* route = NULL;
  route_head* track = NULL;

  while ((buff = gbfgetstr(fin))) {
    char* cin = buff;
    char* ctail;

    if ((line++ == 0) && fin->unicode) {
      cet_convert_init(CET_CHARSET_UTF8, 1);
    }
    cin = lrtrim(buff);
    if (strlen(cin) == 0) {
      continue;
    }

    ctail = strchr(cin, ' ');
    if (ctail == NULL) {
      continue;
    }
    ctail = lrtrim(ctail);

    switch (*cin) {
    case 'G':
      input_datum = GPS_Lookup_Datum_Index(ctail);
      if (input_datum < 0) {
        fatal(MYNAME ": Unsupported datum \"%s\"!", ctail);
      }
      break;
    case 'U':
      switch (*ctail) {
      case '1': /* lat/lon, that's we want to see */
        break;
      case '0': /* UTM not supported yet */
        fatal(MYNAME "Sorry, UTM is not supported yet!\n");
      default:
        fatal(MYNAME "Invalid system of coordinates (%s)!\n", cin);
      }
      break;
    case 'R':
      route = route_head_alloc();
      route_add_head(route);
      parse_rte_info(ctail, route);
      break;
    case 'M':
      break;
    case 'W':
      wpt = parse_wpt(ctail);
      if (wpt != NULL) {
        if (route != NULL) {
          route_add_wpt(route, wpt);
        } else {
          waypt_add(wpt);
        }
      }
      break;
    case 'w':
      is_fatal((wpt == NULL), MYNAME ": No waypoint data before \"%s\"!", cin);
      parse_wpt_info(ctail, wpt);
      break;
    case 'T':
      wpt = parse_trkpt(ctail);
      if (wpt != NULL) {
        if (track == NULL) {
          track = route_head_alloc();
          track_add_head(track);
        }
        track_add_wpt(track, wpt);
      }
      break;
    case 't':
      if (track != NULL) {
        parse_track_info(ctail, track);
      }
      break;
    }
  }
}

/* ----------------------------------------------------------- */

static void
write_waypt_cb(const waypoint* wpt)
{
  char* name;

  if (curr_index != target_index) {
    return;
  }

  name = (snlen > 0) ? mkshort_from_wpt(sh, wpt) : csv_stringclean(wpt->shortname, " ");

  gbfprintf(fout, "W  %s A ", name);
  gbfprintf(fout, "%.10f%c%c ",
            fabs(wpt->latitude), 0xBA, (wpt->latitude >= 0) ? 'N' : 'S');
  gbfprintf(fout, "%.10f%c%c ",
            fabs(wpt->longitude), 0xBA, (wpt->longitude >= 0) ? 'E' : 'W');
  gbfprintf(fout, "27-MAR-62 00:00:00 %.6f",
            (wpt->altitude != unknown_alt) ? wpt->altitude : 0.0);
  if (wpt->description != NULL) {
    gbfprintf(fout, " %s", wpt->description);
  }
  gbfprintf(fout, "\n");

  if ((wpt->icon_descr != NULL) || (wpt->wpt_flags.proximity) || \
      (option_icon != NULL)) {
    char* icon = option_icon;

    if (wpt->icon_descr != NULL) {
      icon = (char*) wpt->icon_descr;
    }

    gbfprintf(fout, "w  %s,0,0.0,16777215,255,1,7,,%.1f\n",
              (icon != NULL) ? icon : "Waypoint",
              WAYPT_GET(wpt, proximity, 0));
  }
  xfree(name);
}

static void
write_route_hdr_cb(const route_head* rte)
{
  char* name;
  curr_route = (route_head*) rte;
  curr_index++;
  if (curr_index != target_index) {
    return;
  }

  name = rte->rte_name;
  if (name != NULL) {
    name = csv_stringclean(name, ",");
  } else {
    name = xstrdup(" ");
  }
  gbfprintf(fout, "R  16711680,%s,1,-1\n", name);
  xfree(name);
}

static void
write_route(void)
{
  curr_index = 0;
  route_disp_all(write_route_hdr_cb, NULL, write_waypt_cb);
}

static void
write_track_hdr_cb(const route_head* trk)
{
  track_info_flag = 0;
  curr_track = (route_head*) trk;

  curr_index++;
  if (curr_index != target_index) {
    return;
  }

  track_info_flag = 1;
}

static void
write_trkpt_cb(const waypoint* wpt)
{
  char buff[128];
  struct tm tm;

  if ((curr_index != target_index) || (wpt == NULL)) {
    return;
  }

  buff[0] = '\0';

  if (wpt->creation_time != 0) {
    tm = *gmtime(&wpt->creation_time);
    strftime(buff, sizeof(buff), "%d-%b-%y %H:%M:%S", &tm);
    strupper(buff);
  } else {
    strncpy(buff, "01-JAN-70 00:00:00", sizeof(buff));
  }

  gbfprintf(fout, "T  A %.10f%c%c %.10f%c%c ",
            fabs(wpt->latitude), 0xBA, (wpt->latitude >= 0) ? 'N' : 'S',
            fabs(wpt->longitude), 0xBA, (wpt->longitude >= 0) ? 'E' : 'W');
  gbfprintf(fout, "%s s %.1f %.1f %.1f %.1f %d ",
            buff,
            wpt->altitude,
            0.0,
            0.0,
            0.0,
            0);
  gbfprintf(fout, "%.1f %.1f %.1f %.1f %.1f\n",
            -1000.0,
            -1.0,
            -1.0,
            -1.0,
            -1.0);
  if (track_info_flag != 0) {
    track_info_flag = 0;
    if (curr_track->rte_name != NULL) {
      char* name;

      name = csv_stringclean(curr_track->rte_name, "|");
      gbfprintf(fout, "t 4294967295|%s|-1|-1\n", name);
      xfree(name);
    }
  }
}

static void
write_track(void)
{
  curr_index = 0;

//	gbfprintf(fout, "L  +02:00:00\n");
  track_disp_all(write_track_hdr_cb, NULL, write_trkpt_cb);
  gbfprintf(fout, "F  1234\n");
}

static void
write_waypoints(void)
{
  waypt_disp_all(write_waypt_cb);
}

/* --------------------------------------------------------------------------- */

static void
compegps_wr_init(const char* fname)
{
  fout = gbfopen(fname, "w", MYNAME);
  sh = mkshort_new_handle();
}

static void
compegps_wr_deinit(void)
{
  mkshort_del_handle(&sh);
  gbfclose(fout);
}

static void
compegps_data_write(void)
{
  /* because of different file extensions we can only write one GPS data type at time */

  gbfprintf(fout, "G  WGS 84\n");
  gbfprintf(fout, "U  1\n");

  /* process options */

  target_index = 1;
  if (option_index != NULL) {
    target_index = atoi(option_index);
  }

  snlen = 0;
  if (global_opts.synthesize_shortnames != 0) {
    if (option_snlen != NULL) {
      snlen = atoi(option_snlen);
    } else {
      snlen = SHORT_NAME_LENGTH;
    }

    is_fatal((snlen < 1), MYNAME "Invalid length for generated shortnames!");

    setshort_whitespace_ok(sh, 0);
    setshort_length(sh, snlen);
  }

  radius = -1;
  if (option_radius != 0) {
    radius = atof(option_radius);
    is_fatal((radius <= 0.0), MYNAME "Invalid value for radius!");
  }

  if (option_icon != NULL) {
    if (*option_icon == '\0') {
      option_icon = NULL;
    } else if (case_ignore_strcmp(option_icon, "deficon") == 0) {
      option_icon = NULL;
    }
  }

  switch (global_opts.objective) {
  case wptdata:
  case unknown_gpsdata:
    curr_index = target_index = 0;
    write_waypoints();
    break;
  case trkdata:
    write_track();
    break;
  case rtedata:
    write_route();
    break;
  case posndata:
    fatal(MYNAME ": Realtime positioning not supported.\n");
    break;
  }
}

/* --------------------------------------------------------------------------- */

ff_vecs_t compegps_vecs = {
  ff_type_file,
  FF_CAP_RW_ALL,
  compegps_rd_init,
  compegps_wr_init,
  compegps_rd_deinit,
  compegps_wr_deinit,
  compegps_data_read,
  compegps_data_write,
  NULL,
  compegps_args,
  CET_CHARSET_MS_ANSI, 1
};
#endif /* CSVFMTS_ENABLED */
