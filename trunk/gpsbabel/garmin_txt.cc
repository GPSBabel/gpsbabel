/*

    Support for MapSource Text Export (Tab delimited) files.

    Copyright (C) 2006 Olaf Klein, o.b.klein@gpsbabel.org

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

#if CSVFMTS_ENABLED
#include <math.h>
#include <time.h>
#include <ctype.h>
#include <errno.h>
#include <stdarg.h>
#include "cet_util.h"
#include "csv_util.h"
#include "garmin_fs.h"
#include "garmin_tables.h"
#include "grtcirc.h"
#include "inifile.h"
#include "jeeps/gpsmath.h"
#include "strptime.h"

#define MYNAME "garmin_txt"

typedef struct gtxt_flags_s {
  unsigned int metric:1;
  unsigned int celsius:1;
  unsigned int utc:1;
  unsigned int enum_waypoints:1;
  unsigned int route_header_written:1;
  unsigned int track_header_written:1;
} gtxt_flags_t;

static gbfile* fin, *fout;
static route_head* current_trk, *current_rte;
static int waypoints;
static int routepoints;
static waypoint** wpt_a;
static int wpt_a_ct;
static grid_type grid_index;
static int datum_index;
static const char* datum_str;
static int current_line;
static char* date_time_format = NULL;
static int precision = 3;
static time_t utc_offs = 0;

static gtxt_flags_t gtxt_flags;

typedef enum {
  waypt_header = 0,
  rtept_header,
  trkpt_header,
  route_header,
  track_header,
  unknown_header
} header_type;

#if __cplusplus
inline header_type operator++(header_type& rs, int)
{
  return rs = (header_type)((int)rs + 1);
}

inline gt_display_modes_e  operator++(gt_display_modes_e& rs, int)
{
  return rs = (gt_display_modes_e)((int)rs + 1);
}
#endif

#define MAX_HEADER_FIELDS 36

static char* header_lines[unknown_header + 1][MAX_HEADER_FIELDS];
static int header_fields[unknown_header + 1][MAX_HEADER_FIELDS];
static int header_ct[unknown_header + 1];

#define GARMIN_UNKNOWN_ALT 1.0e25f
#define DEFAULT_DISPLAY garmin_display_symbol_and_name
#define DEFAULT_DATE_FORMAT "dd/mm/yyyy"
#define DEFAULT_TIME_FORMAT "HH:mm:ss"

/* macros */

#define IS_VALID_ALT(a) (((a) != unknown_alt) && ((a) < GARMIN_UNKNOWN_ALT))
#define DUPSTR(a) (((a) != NULL) && ((a)[0] != 0)) ? ((a)) : NULL

static char* opt_datum = NULL;
static char* opt_dist = NULL;
static char* opt_temp = NULL;
static char* opt_date_format = NULL;
static char* opt_time_format = NULL;
static char* opt_precision = NULL;
static char* opt_utc = NULL;
static char* opt_grid = NULL;

static
arglist_t garmin_txt_args[] = {
  {"date",  &opt_date_format, "Read/Write date format (i.e. yyyy/mm/dd)", NULL, ARGTYPE_STRING, ARG_NOMINMAX},
  {"datum", &opt_datum, 	    "GPS datum (def. WGS 84)", "WGS 84", ARGTYPE_STRING, ARG_NOMINMAX},
  {"dist",  &opt_dist,        "Distance unit [m=metric, s=statute]", "m", ARGTYPE_STRING, ARG_NOMINMAX},
  {"grid",  &opt_grid,        "Write position using this grid.", NULL, ARGTYPE_STRING, ARG_NOMINMAX},
  {"prec",  &opt_precision,   "Precision of coordinates", "3", ARGTYPE_INT, ARG_NOMINMAX},
  {"temp",  &opt_temp,        "Temperature unit [c=Celsius, f=Fahrenheit]", "c", ARGTYPE_STRING, ARG_NOMINMAX},
  {"time",  &opt_time_format, "Read/Write time format (i.e. HH:mm:ss xx)", NULL, ARGTYPE_STRING, ARG_NOMINMAX},
  {"utc",   &opt_utc,         "Write timestamps with offset x to UTC time", NULL, ARGTYPE_INT, "-23", "+23"},
  ARG_TERMINATOR
};

typedef struct info_s {
  double length;
  time_t start;
  time_t time;
  double speed;
  double total;
  int count;
  waypoint* prev_wpt;
  waypoint* first_wpt;
  waypoint* last_wpt;
} info_t;

static info_t* route_info;
static int route_idx;
static info_t* cur_info;

static const char* headers[] = {
  "Name\tDescription\tType\tPosition\tAltitude\tDepth\tProximity\tTemperature\t"
  "Display Mode\tColor\tSymbol\tFacility\tCity\tState\tCountry\t"
  "Date Modified\tLink\tCategories",
  "Waypoint Name\tDistance\tLeg Length\tCourse",
  "Position\tTime\tAltitude\tDepth\tTemperature\tLeg Length\tLeg Time\tLeg Speed\tLeg Course",
  "Name\tLength\tCourse\tWaypoints\tLink",
  "Name\tStart Time\tElapsed Time\tLength\tAverage Speed\tLink",
  NULL
};

/* helpers */

static const char*
get_option_val(const char* option, const char* def)
{
  const char* c = (option != NULL) ? option : def;
  return c;
}

static void
init_date_and_time_format(void)
{
  const char* f;
  const char* c;

  f = get_option_val(opt_date_format, DEFAULT_DATE_FORMAT);
  date_time_format = convert_human_date_format(f);

  date_time_format = xstrappend(date_time_format, " ");

  f = get_option_val(opt_time_format, DEFAULT_TIME_FORMAT);
  c = convert_human_time_format(f);
  date_time_format = xstrappend(date_time_format, c);
  xfree((void*) c);
}

static void
convert_datum(const waypoint* wpt, double* dest_lat, double* dest_lon)
{
  double alt;

  if (datum_index == DATUM_WGS84) {
    *dest_lat = wpt->latitude;
    *dest_lon = wpt->longitude;
  } else GPS_Math_WGS84_To_Known_Datum_M(wpt->latitude, wpt->longitude, 0.0,
                                           dest_lat, dest_lon, &alt, datum_index);
}

/* WRITER *****************************************************************/

/* Waypoint preparation */

static void
enum_waypt_cb(const waypoint* wpt)
{
  garmin_fs_p gmsd;
  int wpt_class;

  gmsd = GMSD_FIND(wpt);
  wpt_class = GMSD_GET(wpt_class, 0);
  if (wpt_class < 0x80) {
    int i;

    if (gtxt_flags.enum_waypoints) {		/* enumerate only */
      waypoints++;
      return;
    }
    for (i = 0; i < wpt_a_ct; i++) {		/* check for duplicates */
      waypoint* tmp = wpt_a[i];
      if (case_ignore_strcmp(tmp->shortname, wpt->shortname) == 0) {
        wpt_a[i] = (waypoint*)wpt;
        waypoints--;
        return;

      }
    }
    wpt_a[wpt_a_ct++] = (waypoint*)wpt;
  }

}

static int
sort_waypt_cb(const void* a, const void* b)
{
  const waypoint* wa = *(waypoint**)a;
  const waypoint* wb = *(waypoint**)b;
#if NEW_STRINGS
  return wa->shortname.compare(wb->shortname, Qt::CaseInsensitive);
#else
  return case_ignore_strcmp(QString::fromLatin1(wa->shortname), QString::fromLatin1(wb->shortname));
#endif
}


/* common route and track pre-work */

static void
prework_hdr_cb(const route_head* rte)
{
  cur_info = &route_info[route_idx];
  cur_info->prev_wpt = NULL;
  cur_info->length = 0;
  cur_info->time = 0;
}

static void
prework_tlr_cb(const route_head* rte)
{
  cur_info->last_wpt = cur_info->prev_wpt;
  route_idx++;
}

static void
prework_wpt_cb(const waypoint* wpt)
{
  waypoint* prev = cur_info->prev_wpt;

  if (prev != NULL) {
    cur_info->time += (wpt->GetCreationTime().toTime_t() - prev->GetCreationTime().toTime_t());
    cur_info->length += waypt_distance_ex(prev, wpt);
  } else {
    cur_info->first_wpt = (waypoint*)wpt;
    cur_info->start = wpt->GetCreationTime().toTime_t();
  }
  cur_info->prev_wpt = (waypoint*)wpt;
  cur_info->count++;
  routepoints++;
}


/* output helpers */

static void
print_position(const waypoint* wpt)
{
  int valid = 1;
  double lat, lon, north, east;
  char latsig, lonsig;
  double  latmin, lonmin, latsec, lonsec;
  int     latint, lonint, zone;
  char map[3], zonec;

  convert_datum(wpt, &lat, &lon);

  /* ----------------------------------------------------------------------------*/
  /*            the following code is from pretty_deg_format (util.c)            */
  /* ----------------------------------------------------------------------------*/
  /* !ToDo! generate common code for calculating of degrees, minutes and seconds */
  /* ----------------------------------------------------------------------------*/

  latsig = lat < 0 ? 'S':'N';
  lonsig = lon < 0 ? 'W':'E';
  latint = abs((int) lat);
  lonint = abs((int) lon);
  latmin = 60.0 * (fabs(lat) - latint);
  lonmin = 60.0 * (fabs(lon) - lonint);
  latsec = 60.0 * (latmin - floor(latmin));
  lonsec = 60.0 * (lonmin - floor(lonmin));

  switch (grid_index) {

  case grid_lat_lon_ddd:

    gbfprintf(fout, "%c%0.*f %c%0.*f\t",
              latsig, precision, fabs(lat),
              lonsig, precision, fabs(lon));
    break;

  case grid_lat_lon_dmm:

    gbfprintf(fout, "%c%d %0*.*f %c%d %0*.*f\t",
              latsig, latint, precision + 3, precision, latmin,
              lonsig, lonint, precision + 3, precision, lonmin);
    break;

  case grid_lat_lon_dms:

    gbfprintf(fout, "%c%d %d %.*f %c%d %d %.*f\t",
              latsig, latint, (int)latmin, precision, latsec,
              lonsig, lonint, (int)lonmin, precision, lonsec);
    break;

  case grid_bng:

    valid = GPS_Math_WGS84_To_UKOSMap_M(wpt->latitude, wpt->longitude, &east, &north, map);
    if (valid) {
      gbfprintf(fout, "%s %5.0f %5.0f\t", map, east, north);
    }
    break;

  case grid_utm:

    valid = GPS_Math_Known_Datum_To_UTM_EN(lat, lon,
                                           &east, &north, &zone, &zonec, datum_index);
    if (valid) {
      gbfprintf(fout, "%02d %c %.0f %.0f\t", zone, zonec, east, north);
    }
    break;

  case grid_swiss:

    valid = GPS_Math_WGS84_To_Swiss_EN(wpt->latitude, wpt->longitude, &east, &north);
    if (valid) {
      gbfprintf(fout, "%.f %.f\t", east, north);
    }
    break;

  default:
    fatal("ToDo\n");
  }

  if (! valid) {
    gbfprintf(fout, "#####\n");
    fatal(MYNAME ": %s (%s) is outside of convertable area \"%s\"!\n",
#if NEW_STRINGS
          wpt->shortname.isEmpty() ? "Waypoint" : CSTR(wpt->shortname),
#else
          wpt->shortname ? CSTRc(wpt->shortname) : "Waypoint",
#endif
          pretty_deg_format(wpt->latitude, wpt->longitude, 'd', NULL, 0),
          gt_get_mps_grid_longname(grid_index, MYNAME));
  }
}

static void
print_date_and_time(const time_t time, const int time_only)
{
  struct tm tm;
  char tbuf[32];

  if (time < 0) {
    gbfprintf(fout, "\t");
    return;
  }
  if (time_only) {
    tm = *gmtime(&time);
    snprintf(tbuf, sizeof(tbuf), "%d:%02d:%02d", tm.tm_hour, tm.tm_min, tm.tm_sec);
    gbfprintf(fout, "%s", tbuf);
  } else if (time != 0) {
    if (gtxt_flags.utc) {
      time_t t = time + utc_offs;
      tm = *gmtime(&t);
    } else {
      tm = *localtime(&time);
    }
    strftime(tbuf, sizeof(tbuf), date_time_format, &tm);
    gbfprintf(fout, "%s ", tbuf);
  }
  gbfprintf(fout, "\t");
}

static void
print_categories(uint16_t categories)
{
  int i, count;
  char* c;

  if (categories == 0) {
    return;
  }

  count = 0;
  for (i = 0; i < 16; i++) {
    if ((categories & 1) != 0) {
      if (global_opts.inifile != NULL) {
        char key[3];
        snprintf(key, sizeof(key), "%d", i + 1);
        c = inifile_readstr(global_opts.inifile, GMSD_SECTION_CATEGORIES, key);
      } else {
        c = NULL;
      }

      gbfprintf(fout, "%s", (count++ > 0) ? "," : "");
      if (c == NULL) {
        gbfprintf(fout, "Category %d", i+1);
      }
//				gbfprintf(fout, "%s", gps_categories[i]);
      else {
        gbfprintf(fout, "%s", c);
      }

    }
    categories = categories >> 1;
  }
}

static void
print_course(const waypoint* A, const waypoint* B)		/* seems to be okay */
{
  if ((A != NULL) && (B != NULL) && (A != B)) {
    int course;
    course = si_round(waypt_course(A, B));
    cet_gbfprintf(fout, &cet_cs_vec_cp1252, "%d%c true", course, 0xB0);
  }
}

static void
print_distance(const double distance, const int no_scale, const int with_tab, const int decis)
{
  double dist = distance;

  if (gtxt_flags.metric == 0) {
    dist = METERS_TO_FEET(dist);

    if ((dist < 5280) || no_scale) {
      gbfprintf(fout, "%.*f ft", decis, dist);
    } else {
      dist = METERS_TO_MILES(distance);
      if (dist < (double)100) {
        gbfprintf(fout, "%.1f mi", dist);
      } else {
        gbfprintf(fout, "%d mi", si_round(dist));
      }
    }
  } else {
    if ((dist < 1000) || no_scale) {
      gbfprintf(fout, "%.*f m", decis, dist);
    } else {
      dist = dist / (double)1000.0;
      if (dist < (double)100) {
        gbfprintf(fout, "%.1f km", dist);
      } else {
        gbfprintf(fout, "%d km", si_round(dist));
      }
    }
  }
  if (with_tab) {
    gbfprintf(fout, "\t");
  }
}

static void
print_speed(double* distance, time_t* time)
{
  int idist;
  double dist = *distance;
  const char* unit;

  if (!gtxt_flags.metric) {
    dist = METERS_TO_MILES(dist) * 1000.0;
    unit = "mph";
  } else {
    unit = "kph";
  }
  idist = si_round(dist);

  if ((*time != 0) && (idist > 0)) {
    double speed = MPS_TO_KPH(dist / (double)*time);
    int ispeed = si_round(speed);

    if (speed < (double)0.01) {
      gbfprintf(fout, "0 %s", unit);
    } else if (ispeed < 2) {
      gbfprintf(fout, "%.1f %s", speed, unit);
    } else {
      gbfprintf(fout, "%d %s", ispeed, unit);
    }
  } else {
    gbfprintf(fout, "0 %s", unit);
  }
  gbfprintf(fout, "\t");
}

static void
print_temperature(const float temperature)
{
  if (gtxt_flags.celsius) {
    gbfprintf(fout, "%.f C", temperature);
  } else {
    gbfprintf(fout, "%.f F", (temperature * 1.8) + 32);
  }
}

static void
print_string(const char* fmt, const char* string)
{
  char* c;
  char* buff;

  buff = xstrdup(string);
  /* remove unwanted characters from source string */
  for (c = buff; *c; c++) {
    if (iscntrl(*c)) {
      *c = ' ';
    }
  }
  gbfprintf(fout, fmt, buff);
  xfree(buff);
}

static void
print_string(const char* fmt, const QString& string)
{
  print_string(fmt, string.toUtf8().data());
}


/* main cb's */

static void
write_waypt(const waypoint* wpt)
{
  unsigned char wpt_class;
  garmin_fs_p gmsd;
  const char* wpt_type;
  const char* dspl_mode;
  const char* country;
  double x;
  int i, icon, dynamic;
  const char* icon_descr;

  gmsd = GMSD_FIND(wpt);

  i = GMSD_GET(display, 0);
  if (i > GT_DISPLAY_MODE_MAX) {
    i = 0;
  }
  dspl_mode = gt_display_mode_names[i];

  wpt_class = GMSD_GET(wpt_class, 0);
  if (wpt_class <= gt_waypt_class_map_line) {
    wpt_type = gt_waypt_class_names[wpt_class];
  } else {
    wpt_type = gt_waypt_class_names[0];
  }

#if NEW_STRINGS
  gbfprintf(fout, "Waypoint\t%s\t", CSTRc(wpt->shortname));
#else
  gbfprintf(fout, "Waypoint\t%s\t", (wpt->shortname) ? CSTRc(wpt->shortname) : "");
#endif
  if (wpt_class <= gt_waypt_class_airport_ndb) {
    QString temp = wpt->notes;
    if (temp.isEmpty()) {
#if NEW_STRINGS
      if (wpt->description != wpt->shortname) {
#else
      if (wpt->description && (strcmp(wpt->description, wpt->shortname) != 0)) {
#endif
        temp = wpt->description;
      } else {
        temp = "";
      }
    }
    print_string("%s\t", temp);
  } else {
    gbfprintf(fout, "\t");
  }
  gbfprintf(fout, "%s\t", wpt_type);

  print_position(wpt);

  if IS_VALID_ALT(wpt->altitude) {
    print_distance(wpt->altitude, 1, 0, 0);
  }
  gbfprintf(fout, "\t");

  x = WAYPT_GET(wpt, depth, unknown_alt);
  if (x != unknown_alt) {
    print_distance(x, 1, 0, 1);
  }
  gbfprintf(fout, "\t");

  x = WAYPT_GET(wpt, proximity, unknown_alt);
  if (x != unknown_alt) {
    print_distance(x, 0, 0, 0);
  }
  gbfprintf(fout, "\t");

  x = WAYPT_GET(wpt, temperature, -999);
  if (x != -999) {
    print_temperature(x);
  }
  gbfprintf(fout, "\t%s\t", dspl_mode);

  gbfprintf(fout, "Unknown\t"); 				/* Color is fixed: Unknown */

  icon = GMSD_GET(icon, -1);
  if (icon == -1) {
    icon = gt_find_icon_number_from_desc(wpt->icon_descr, GDB);
  }
  icon_descr = gt_find_desc_from_icon_number(icon, GDB, &dynamic);
  print_string("%s\t", icon_descr);
  if (dynamic) {
    // sleaze alert: cast away constness.
    xfree((char*) icon_descr);
  }

  print_string("%s\t", GMSD_GET(facility, ""));
  print_string("%s\t", GMSD_GET(city, ""));
  print_string("%s\t", GMSD_GET(state, ""));
  country = gt_get_icao_country(GMSD_GET(cc, ""));
  print_string("%s\t", (country != NULL) ? country : "");
  print_date_and_time(wpt->GetCreationTime().toTime_t(), 0);
  if (wpt->HasUrlLink()) {
    UrlLink l = wpt->GetUrlLink();
    print_string("%s\t", l.url_);
  } else {
    print_string("%s\t", "");
  }
  print_categories(GMSD_GET(category, 0));

  gbfprintf(fout, "\r\n");
}

static void
route_disp_hdr_cb(const route_head* rte)
{
  current_trk = (route_head*)rte;
  cur_info = &route_info[route_idx];
  cur_info->prev_wpt = NULL;
  cur_info->total = 0;
  if (rte->rte_waypt_ct <= 0) {
    return;
  }

  if (!gtxt_flags.route_header_written) {
    gtxt_flags.route_header_written = 1;
    gbfprintf(fout, "\r\n\r\nHeader\t%s\r\n", headers[route_header]);
  }
#if NEW_STRINGS
  print_string("\r\nRoute\t%s\t", current_trk->rte_name);
#else
  print_string("\r\nRoute\t%s\t", current_trk->rte_name ? CSTRc(current_trk->rte_name) : "");
#endif
  print_distance(cur_info->length, 0, 1, 0);
  print_course(cur_info->first_wpt, cur_info->last_wpt);
  gbfprintf(fout, "\t%d waypoints\t", cur_info->count);
  print_string("%s\r\n", rte->rte_url);
  gbfprintf(fout, "\r\nHeader\t%s\r\n\r\n", headers[rtept_header]);
}

static void
route_disp_tlr_cb(const route_head* rte)
{
  route_idx++;
}

static void
route_disp_wpt_cb(const waypoint* wpt)
{
  waypoint* prev = cur_info->prev_wpt;

  gbfprintf(fout, "Route Waypoint\t");
  gbfprintf(fout, "%s\t", CSTRc(wpt->shortname));

  if (prev != NULL) {
    double dist = waypt_distance_ex(prev, wpt);
    cur_info->total += dist;
    print_distance(cur_info->total, 0, 1, 0);
    print_distance(dist, 0, 1, 0);
    print_course(prev, wpt);
  } else {
    print_distance(0, 1, 0, 0);
  }

  gbfprintf(fout, "\r\n");

  cur_info->prev_wpt = (waypoint*)wpt;
}

static void
track_disp_hdr_cb(const route_head* track)
{
  cur_info = &route_info[route_idx];
  cur_info->prev_wpt = NULL;
  cur_info->total = 0;
  current_trk = (route_head*)track;
  if (track->rte_waypt_ct <= 0) {
    return;
  }

  if (!gtxt_flags.track_header_written) {
    gtxt_flags.track_header_written = 1;
    gbfprintf(fout, "\r\n\r\nHeader\t%s\r\n", headers[track_header]);
  }
#if NEW_STRINGS
  print_string("\r\nTrack\t%s\t", current_trk->rte_name);
#else
  print_string("\r\nTrack\t%s\t", current_trk->rte_name ? CSTRc(current_trk->rte_name) : "");
#endif
  print_date_and_time(cur_info->start, 0);
  print_date_and_time(cur_info->time, 1);
  print_distance(cur_info->length, 0, 1, 0);
  print_speed(&cur_info->length, &cur_info->time);
  print_string("%s", track->rte_url);
  gbfprintf(fout, "\r\n\r\nHeader\t%s\r\n\r\n", headers[trkpt_header]);
}

static void
track_disp_tlr_cb(const route_head* track)
{
  route_idx++;
}

static void
track_disp_wpt_cb(const waypoint* wpt)
{
  waypoint* prev = cur_info->prev_wpt;
  time_t delta;
  double dist, depth;

  gbfprintf(fout, "Trackpoint\t");

  print_position(wpt);
  print_date_and_time(wpt->GetCreationTime().toTime_t(), 0);
  if IS_VALID_ALT(wpt->altitude) {
    print_distance(wpt->altitude, 1, 0, 0);
  }

  gbfprintf(fout, "\t");
  depth = WAYPT_GET(wpt, depth, unknown_alt);
  if (depth != unknown_alt) {
    print_distance(depth, 1, 0, 1);
  }

  if (prev != NULL) {
    float temp;
    gbfprintf(fout, "\t");
    delta = wpt->GetCreationTime().toTime_t() - prev->GetCreationTime().toTime_t();
    temp = WAYPT_GET(wpt, temperature, -999);
    if (temp != -999) {
      print_temperature(temp);
    }
    gbfprintf(fout, "\t");
    dist = waypt_distance_ex(prev, wpt);
    print_distance(dist, 0, 1, 0);
    print_date_and_time(delta, 1);
    print_speed(&dist, &delta);
    print_course(prev, wpt);
  }
  gbfprintf(fout, "\r\n");

  cur_info->prev_wpt = (waypoint*)wpt;
}

/*******************************************************************************
* %%%        global callbacks called by gpsbabel main process              %%% *
*******************************************************************************/

static void
garmin_txt_wr_init(const char* fname)
{
  const char* grid_str;

  memset(&gtxt_flags, 0, sizeof(gtxt_flags));

  fout = gbfopen(fname, "wb", MYNAME);

  gtxt_flags.metric = (toupper(*get_option_val(opt_dist, "m")) == 'M');
  gtxt_flags.celsius = (toupper(*get_option_val(opt_temp, "c")) == 'C');
  init_date_and_time_format();
  if (opt_precision) {
    precision = atoi(opt_precision);
    is_fatal(precision < 0, MYNAME ": Invalid precision (%s)!", opt_precision);
  }

  datum_str = get_option_val(opt_datum, NULL);
  grid_str = get_option_val(opt_grid, NULL);

  grid_index = grid_lat_lon_dmm;
  if (grid_str != NULL) {
    int i;

    if (sscanf(grid_str, "%d", &i)) {
      grid_index = (grid_type) i;
      if ((grid_index < GRID_INDEX_MIN) || (grid_index > GRID_INDEX_MAX))
        fatal(MYNAME ": Grid index out of range (%d..%d)!",
              (int)GRID_INDEX_MIN, (int)GRID_INDEX_MAX);
    } else {
      grid_index = gt_lookup_grid_type(grid_str, MYNAME);
    }
  }

  switch (grid_index) {
  case grid_bng: /* force datum to "Ord Srvy Grt Britn" */
    datum_index = DATUM_OSGB36;
    break;
  case grid_swiss: /* force datum to "Ord Srvy Grt Britn" */
    datum_index = DATUM_WGS84;
    break;
  default:
    datum_index = gt_lookup_datum_index(datum_str, MYNAME);
  }

  if (opt_utc != NULL) {
    if (case_ignore_strcmp(opt_utc, "utc") == 0) {
      utc_offs = 0;
    } else {
      utc_offs = atoi(opt_utc);
    }
    utc_offs *= (60 * 60);
    gtxt_flags.utc = 1;
  }
}

static void
garmin_txt_wr_deinit(void)
{
  gbfclose(fout);
  xfree(date_time_format);
}

static void
garmin_txt_write(void)
{
  char* grid_str, *c;
  const char* datum_str;

  grid_str = xstrdup(gt_get_mps_grid_longname(grid_index, MYNAME));
  while ((c = strchr(grid_str, '*'))) {
    *c = 0xB0;  /* degree sign */
  }
  cet_gbfprintf(fout, &cet_cs_vec_cp1252, "Grid\t%s\r\n", grid_str);
  xfree(grid_str);

  datum_str = gt_get_mps_datum_name(datum_index);
  gbfprintf(fout, "Datum\t%s\r\n\r\n", datum_str);

  waypoints = 0;
  gtxt_flags.enum_waypoints = 1;			/* enum all waypoints */
  waypt_disp_all(enum_waypt_cb);
  route_disp_all(NULL, NULL, enum_waypt_cb);
  gtxt_flags.enum_waypoints = 0;

  if (waypoints > 0) {
    int i;

    wpt_a_ct = 0;
    wpt_a = (waypoint**)xcalloc(waypoints, sizeof(*wpt_a));
    waypt_disp_all(enum_waypt_cb);
    route_disp_all(NULL, NULL, enum_waypt_cb);
    qsort(wpt_a, waypoints, sizeof(*wpt_a), sort_waypt_cb);

    gbfprintf(fout, "Header\t%s\r\n\r\n", headers[waypt_header]);
    for (i = 0; i < waypoints; i++) {
      waypoint* wpt = wpt_a[i];
      write_waypt(wpt);
    }
    xfree(wpt_a);

    route_idx = 0;
    route_info = (info_t*) xcalloc(route_count(), sizeof(struct info_s));
    routepoints = 0;
    route_disp_all(prework_hdr_cb, prework_tlr_cb, prework_wpt_cb);
    if (routepoints > 0) {
      route_idx = 0;
      route_disp_all(route_disp_hdr_cb, route_disp_tlr_cb, route_disp_wpt_cb);
    }
    xfree(route_info);
  }

  route_idx = 0;
  route_info = (info_t*) xcalloc(track_count(), sizeof(struct info_s));
  routepoints = 0;
  track_disp_all(prework_hdr_cb, prework_tlr_cb, prework_wpt_cb);

  if (routepoints > 0) {
    route_idx = 0;
    track_disp_all(track_disp_hdr_cb, track_disp_tlr_cb, track_disp_wpt_cb);
  }
  xfree(route_info);
}

/* READER *****************************************************************/

/* helpers */

static void
free_header(const header_type ht)
{
  int i;

  for (i = 0; i < MAX_HEADER_FIELDS; i++) {
    char* c = header_lines[ht][i];
    if (c != NULL) {
      xfree(c);
      header_lines[ht][i] = NULL;
    }
  }
  header_ct[ht] = 0;
  memset(header_fields[ht], 0, sizeof(header_fields[ht]));
}

/* data parsers */

static int
parse_date_and_time(char* str, time_t* value)
{
  struct tm tm;
  char* cerr, *cin;

  memset(&tm, 0, sizeof(tm));
  cin = lrtrim(str);
  if (*cin == '\0') {
    return 0;
  }

  cerr = strptime(cin, date_time_format, &tm);
  if (cerr == NULL) {
    cerr = strptime(cin, "%m/%d/%Y %I:%M:%S %p", &tm);
    is_fatal(cerr == NULL, MYNAME ": Invalid date or/and time \"%s\" at line %d!", cin, current_line);
  }

//	printf(MYNAME "_parse_date_and_time: %02d.%02d.%04d, %02d:%02d:%02d\n",
//		tm.tm_mday, tm.tm_mon+1, tm.tm_year+1900, tm.tm_hour, tm.tm_min, tm.tm_sec);

  *value = mklocaltime(&tm);
  return 1;
}

static uint16_t
parse_categories(const char* str)
{
  char buff[256];
  uint16_t val;
  uint16_t res = 0;
  char* cin, *cx;

  if (*str == '\0') {
    return 0;
  }

  strncpy(buff, str, sizeof(buff));
  cin = lrtrim(buff);
  if (*cin == '\0') {
    return 0;
  }

  strcat(cin, ",");

  while ((cx = strchr(cin, ','))) {
    *cx++ = '\0';
    cin = lrtrim(cin);
    if (*cin != '\0') {
      if (!garmin_fs_convert_category(cin, &val)) {
        warning(MYNAME ": Unable to convert category \"%s\" at line %d!\n", cin, current_line);
      } else {
        res = res | val;
      }
    }
    cin = cx;
  }
  return res;
}

static int
parse_temperature(const char* str, double* temperature)
{
  double value;
  unsigned char unit;

  if ((str == NULL) || (*str == '\0')) {
    return 0;
  }

  if (sscanf(str, "%lf %c", &value, &unit) == 2) {
    unit = toupper(unit);
    switch (unit) {
    case 'C':
      *temperature = value;
      break;
    case 'F':
      *temperature = FAHRENHEIT_TO_CELSIUS(value);
      break;
    default:
      fatal(MYNAME ": Unknown temperature unit \"%c\" at line %d!\n", unit, current_line);
    }
    return 1;
  } else {
    fatal(MYNAME ": Invalid temperature \"%s\" at line %d!\n", str, current_line);
  }
  return 0;
}

static void
parse_header(void)
{
  char* str;
  int column = -1;

  free_header(unknown_header);

  while ((str = csv_lineparse(NULL, "\t", "", column++))) {
    header_lines[unknown_header][column] = strupper(xstrdup(str));
    header_ct[unknown_header]++;
    if (header_ct[unknown_header] >= MAX_HEADER_FIELDS) {
      break;
    }
  }
}

static int
parse_display(const char* str, int* val)
{
  gt_display_modes_e i;

  if ((str == NULL) || (*str == '\0')) {
    return 0;
  }

  for (i = GT_DISPLAY_MODE_MIN; i <= GT_DISPLAY_MODE_MAX; i++) {
    if (case_ignore_strcmp(str, gt_display_mode_names[i]) == 0) {
      *val = i;
      return 1;
    }
  }
  warning(MYNAME ": Unknown display mode \"%s\" at line %d.\n", str, current_line);
  return 0;
}

static void
bind_fields(const header_type ht)
{
  int i;
  char* fields, *c;

  is_fatal((grid_index < 0) || (datum_index < 0), MYNAME ": Incomplete or invalid file header!");

  if (header_ct[unknown_header] <= 0) {
    return;
  }
  free_header(ht);

  /* make a copy of headers[ht], uppercase, replace "\t" with "\0" */

  i = strlen(headers[ht]);
  fields = (char*) xmalloc(i + 2);
  strcpy(fields, headers[ht]);
  strcat(fields, "\t");
  c = strupper(fields);
  while ((c = strchr(c, '\t'))) {
    *c++ = '\0';
  }

  for (i = 0; i < header_ct[unknown_header]; i++) {
    char* name;
    int field_no;
    name = header_lines[ht][i] = header_lines[unknown_header][i];
    header_lines[unknown_header][i] = NULL;

    c = fields;
    field_no = 1;
    while (*c) {
      if (strcmp(c, name) == 0) {
        header_fields[ht][i] = field_no;
#if 0
        printf("Binding field \"%s\" to internal number %d (%d,%d)\n", name, field_no, ht, i);
#endif
        break;
      }
      field_no++;
      c = c + strlen(c) + 1;
    }
  }
  header_ct[unknown_header] = 0;
  xfree(fields);
}

static void
parse_grid(void)
{
  char* str = csv_lineparse(NULL, "\t", "", 1);

  if (str != NULL) {
    if (strstr(str, "dd.ddddd") != 0) {
      grid_index = grid_lat_lon_ddd;
    } else if (strstr(str, "mm.mmm") != 0) {
      grid_index = grid_lat_lon_dmm;
    } else if (strstr(str, "mm'ss.s") != 0) {
      grid_index = grid_lat_lon_dms;
    } else {
      grid_index = gt_lookup_grid_type(str, MYNAME);
    }
  } else {
    fatal(MYNAME ": Missing grid headline!\n");
  }
}

static void
parse_datum(void)
{
  char* str = csv_lineparse(NULL, "\t", "", 1);

  if (str != NULL) {
    datum_index = gt_lookup_datum_index(str, MYNAME);
  } else {
    fatal(MYNAME ": Missing GPS datum headline!\n");
  }
}

static void
parse_waypoint(void)
{
  char* str;
  int column = -1;
  waypoint* wpt;
  garmin_fs_p gmsd = NULL;

  bind_fields(waypt_header);

  wpt = waypt_new();
  gmsd = garmin_fs_alloc(-1);
  fs_chain_add(&wpt->fs, (format_specific_data*) gmsd);

  while ((str = csv_lineparse(NULL, "\t", "", column++))) {
    int i, dynamic;
    double d;
    int field_no = header_fields[waypt_header][column];

    switch (field_no) {
    case  1:
      wpt->shortname = DUPSTR(str);
      break;
    case  2:
      wpt->notes = DUPSTR(str);
      break;
    case  3:
      for (i = 0; i <= gt_waypt_class_map_line; i++) {
        if (case_ignore_strcmp(str, gt_waypt_class_names[i]) == 0) {
          GMSD_SET(wpt_class, i);
          break;
        }
      }
      break;
    case  4:
      parse_coordinates(str, datum_index, grid_index,
                        &wpt->latitude, &wpt->longitude, MYNAME);
      break;
    case  5:
      if (parse_distance(str, &d, 1, MYNAME)) {
        wpt->altitude = d;
      }
      break;
    case  6:
      if (parse_distance(str, &d, 1, MYNAME)) {
        WAYPT_SET(wpt, depth, d);
      }
      break;
    case  7:
      if (parse_distance(str, &d, 1, MYNAME)) {
        WAYPT_SET(wpt, proximity, d);
      }
      break;
    case  8:
      if (parse_temperature(str, &d)) {
        WAYPT_SET(wpt, temperature, d);
      }
      break;
    case  9:
      if (parse_display(str, &i)) {
        GMSD_SET(display, i);
      }
      break;
    case 10:
      break;	/* skip color */
    case 11:
      i = gt_find_icon_number_from_desc(str, GDB);
      GMSD_SET(icon, i);
      wpt->icon_descr = gt_find_desc_from_icon_number(i, GDB, &dynamic);
      break;
    case 12:
      GMSD_SETSTR(facility, str);
      break;
    case 13:
      GMSD_SETSTR(city, str);
      break;
    case 14:
      GMSD_SETSTR(state, str);
      break;
    case 15:
      GMSD_SETSTR(country, str);
      GMSD_SETSTR(cc, gt_get_icao_cc(str, wpt->shortname));
      break;
    case 16: {
      time_t ct;
      if (parse_date_and_time(str, &ct)) {
        wpt->SetCreationTime(ct);
      }
    }
    break;
    case 17: {
      wpt->AddUrlLink(str);
    }
    break;
    case 18:
      GMSD_SET(category, parse_categories(str));
      break;
    default:
      break;
    }
  }
  waypt_add(wpt);
}

static void
parse_route_header(void)
{
  char* str;
  int column = -1;
  route_head* rte;

  rte = route_head_alloc();

  bind_fields(route_header);
  while ((str = csv_lineparse(NULL, "\t", "", column++))) {
    int field_no = header_fields[route_header][column];
    switch (field_no) {
    case 1:
      rte->rte_name = DUPSTR(str);
      break;
    case 5:
      rte->rte_url = str;
      break;
    }
  }
  route_add_head(rte);
  current_rte = rte;
}

static void
parse_track_header(void)
{
  char* str;
  int column = -1;
  route_head* trk;

  bind_fields(track_header);
  trk = route_head_alloc();
  while ((str = csv_lineparse(NULL, "\t", "", column++))) {
    int field_no = header_fields[track_header][column];
    switch (field_no) {
    case 1:
      trk->rte_name = DUPSTR(str);
      break;
    case 6:
      trk->rte_url = str;
      break;
    }
  }
  track_add_head(trk);
  current_trk = trk;
}

static void
parse_route_waypoint(void)
{
  char* str;
  int column = -1;
  waypoint* wpt = NULL;

  bind_fields(rtept_header);

  while ((str = csv_lineparse(NULL, "\t", "", column++))) {
    int field_no = header_fields[rtept_header][column];
    switch (field_no) {
    case 1:
      is_fatal((*str == '\0'), MYNAME ": Route waypoint without name at line %d!\n", current_line);
      wpt = find_waypt_by_name(str);
      is_fatal((wpt == NULL), MYNAME ": Route waypoint \"%s\" not in waypoint list (line %d)!\n", str, current_line);
      wpt = waypt_dupe(wpt);
      break;
    }
  }
  if (wpt != NULL) {
    route_add_wpt(current_rte, wpt);
  }
}

static void
parse_track_waypoint(void)
{
  char* str;
  int column = -1;
  waypoint* wpt;

  bind_fields(trkpt_header);
  wpt = waypt_new();

  while ((str = csv_lineparse(NULL, "\t", "", column++))) {
    int field_no;
    double x;

    if (! *str) {
      continue;
    }

    field_no = header_fields[trkpt_header][column];
    switch (field_no) {
    case 1:
      parse_coordinates(str, datum_index, grid_index,
                        &wpt->latitude, &wpt->longitude, MYNAME);
      break;
    case 2: {
      time_t ct;
      if (parse_date_and_time(str, &ct)) {
        wpt->SetCreationTime(ct);
      }
    }
    break;
    case 3:
      if (parse_distance(str, &x, 1, MYNAME)) {
        wpt->altitude = x;
      }
      break;
    case 4:
      if (parse_distance(str, &x, 1, MYNAME)) {
        WAYPT_SET(wpt, depth, x);
      }
      break;
    case 5:
      if (parse_temperature(str, &x)) {
        WAYPT_SET(wpt, temperature, x);
      }
      break;
    case 8:
      if (parse_speed(str, &x, 1, MYNAME)) {
        WAYPT_SET(wpt, speed, x);
      }
      break;
    case 9:
      WAYPT_SET(wpt, course, atoi(str));
      break;
    }
  }
  track_add_wpt(current_trk, wpt);
}

/***************************************************************/

static void
garmin_txt_rd_init(const char* fname)
{
  memset(&gtxt_flags, 0, sizeof(gtxt_flags));

  fin = gbfopen(fname, "rb", MYNAME);
  memset(&header_ct, 0, sizeof(header_ct));

  datum_index = -1;
  grid_index = (grid_type) -1;

  init_date_and_time_format();
}

static void
garmin_txt_rd_deinit(void)
{
  header_type h;

  for (h = waypt_header; h <= unknown_header; h++) {
    free_header(h);
  }
  gbfclose(fin);
  xfree(date_time_format);
}

static void
garmin_txt_read(void)
{
  char* buff;

  current_line = 0;

  while ((buff = gbfgetstr(fin))) {
    char* cin;

    if ((current_line++ == 0) && fin->unicode) {
      cet_convert_init(CET_CHARSET_UTF8, 1);
    }

    cin = lrtrim(buff);
    if (*cin == '\0') {
      continue;
    }

    cin = csv_lineparse(cin, "\t", "", 0);

    if (cin == NULL) {
      continue;
    }

    if (case_ignore_strcmp(cin, "Header") == 0) {
      parse_header();
    } else if (case_ignore_strcmp(cin, "Grid") == 0) {
      parse_grid();
    } else if (case_ignore_strcmp(cin, "Datum") == 0) {
      parse_datum();
    } else if (case_ignore_strcmp(cin, "Waypoint") == 0) {
      parse_waypoint();
    } else if (case_ignore_strcmp(cin, "Route Waypoint") == 0) {
      parse_route_waypoint();
    } else if (case_ignore_strcmp(cin, "Trackpoint") == 0) {
      parse_track_waypoint();
    } else if (case_ignore_strcmp(cin, "Route") == 0) {
      parse_route_header();
    } else if (case_ignore_strcmp(cin, "Track") == 0) {
      parse_track_header();
    } else if (case_ignore_strcmp(cin, "Map") == 0) /* do nothing */ ;
    else {
      fatal(MYNAME ": Unknwon identifier (%s) at line %d!\n", cin, current_line);
    }

    /* flush pending data */
    while (csv_lineparse(NULL, "\t", "", 0));
  }
}

ff_vecs_t garmin_txt_vecs = {
  ff_type_file,
  FF_CAP_RW_ALL,
  garmin_txt_rd_init,
  garmin_txt_wr_init,
  garmin_txt_rd_deinit,
  garmin_txt_wr_deinit,
  garmin_txt_read,
  garmin_txt_write,
  NULL,
  garmin_txt_args,
  CET_CHARSET_MS_ANSI, 0
};

#endif // CSVFMTS_ENABLED
