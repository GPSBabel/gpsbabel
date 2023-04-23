/*

    Support for MapSource Text Export (Tab delimited) files.

    Copyright (C) 2006 Olaf Klein, o.b.klein@gpsbabel.org
    Copyright (C) 2004-2022 Robert Lipe, robertlipe+source@gpsbabel.org

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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

 */

#include "defs.h"

#if CSVFMTS_ENABLED
#include <algorithm>               // for sort
#include <cctype>                  // for toupper
#include <cmath>                   // for fabs, floor
#include <cstdio>                  // for NULL, snprintf, sscanf
#include <cstdint>
#include <cstdlib>                 // for abs
#include <cstring>                 // for memset, strstr, strcat, strchr, strlen, strcmp, strcpy, strncpy
#include <ctime>                   // for gmtime, localtime, strftime

#include <QByteArray>              // for QByteArray
#include <QChar>                   // for QChar, QChar::Other_Control
#include <QDateTime>               // for QDateTime
#include <QIODevice>               // for QIODevice, QIODevice::ReadOnly, QIODevice::WriteOnly
#include <QList>                   // for QList, QList<>::const_iterator
#include <QString>                 // for QString, operator!=
#include <QStringList>             // for QStringList
#include <QTextStream>             // for QTextStream
#include <QVector>                 // for QVector
#include <Qt>                      // for CaseInsensitive
#include <QtGlobal>                // for qRound, qPrintable

#include "csv_util.h"              // for csv_linesplit
#include "formspec.h"              // for FormatSpecificDataList
#include "garmin_fs.h"             // for garmin_fs_t, garmin_fs_alloc, garmin_fs_convert_category, GMSD_SECTION_CATEGORIES
#include "garmin_tables.h"         // for gt_display_modes_e, gt_find_desc_from_icon_number, gt_find_icon_number_from_desc, gt_get_mps_grid_longname, gt_lookup_datum_index, gt_lookup_grid_type, GDB, gt_get_icao_cc, gt_get_icao_country, gt_get_mps_datum_name, gt_waypt_class_names, GT_DISPLAY_MODE...
#include "inifile.h"               // for inifile_readstr
#include "jeeps/gpsmath.h"         // for GPS_Math_Known_Datum_To_UTM_EN, GPS_Math_WGS84_To_Known_Datum_M, GPS_Math_WGS84_To_Swiss_EN, GPS_Math_WGS84_To_UKOSMap_M
#include "src/core/datetime.h"     // for DateTime
#include "src/core/logging.h"      // for Fatal
#include "src/core/textstream.h"   // for TextStream


#define MYNAME "garmin_txt"

struct gtxt_flags_t {
  unsigned int metric:1;
  unsigned int celsius:1;
  unsigned int utc:1;
  unsigned int enum_waypoints:1;
  unsigned int route_header_written:1;
  unsigned int track_header_written:1;
};

static gpsbabel::TextStream* fin = nullptr;
static gpsbabel::TextStream* fout = nullptr;
static route_head* current_trk;
static route_head* current_rte;
static int waypoints;
static int routepoints;
static const Waypoint** wpt_a;
static int wpt_a_ct;
static grid_type grid_index;
static int datum_index;
static const char* datum_str;
static int current_line;
static char* date_time_format = nullptr;
static int precision = 3;
static QString current_line_text;
static time_t utc_offs = 0;
static gtxt_flags_t gtxt_flags;

enum header_type {
  waypt_header = 0,
  rtept_header,
  trkpt_header,
  route_header,
  track_header,
  unknown_header
};

inline header_type& operator++(header_type& s) // prefix
{
  return s = static_cast<header_type>(s + 1);
}
inline header_type operator++(header_type& s, int) // postfix
{
  header_type ret(s);
  ++s;
  return ret;
}

inline gt_display_modes_e& operator++(gt_display_modes_e& s) // prefix
{
  return s = static_cast<gt_display_modes_e>(s + 1);
}
inline gt_display_modes_e operator++(gt_display_modes_e& s, int) // postfix
{
  gt_display_modes_e ret(s);
  ++s;
  return ret;
}

static constexpr int kMaxHeaderFields = 36;

static QString header_lines[unknown_header + 1][kMaxHeaderFields];
static int header_fields[unknown_header + 1][kMaxHeaderFields];
static int header_ct[unknown_header + 1];

static constexpr double kGarminUnknownAlt = 1.0e25;
static constexpr char kDefaultDateFormat[] = "dd/mm/yyyy";
static constexpr char kDefaultTimeFormat[] = "HH:mm:ss";

static inline bool is_valid_alt(double alt)
{
  return (alt != unknown_alt) && (alt < kGarminUnknownAlt);
}

static char* opt_datum = nullptr;
static char* opt_dist = nullptr;
static char* opt_temp = nullptr;
static char* opt_date_format = nullptr;
static char* opt_time_format = nullptr;
static char* opt_precision = nullptr;
static char* opt_utc = nullptr;
static char* opt_grid = nullptr;

static
QVector<arglist_t> garmin_txt_args = {
  {"date",  &opt_date_format, "Read/Write date format (i.e. yyyy/mm/dd)", nullptr, ARGTYPE_STRING, ARG_NOMINMAX, nullptr},
  {"datum", &opt_datum, 	    "GPS datum (def. WGS 84)", "WGS 84", ARGTYPE_STRING, ARG_NOMINMAX, nullptr},
  {"dist",  &opt_dist,        "Distance unit [m=metric, s=statute]", "m", ARGTYPE_STRING, ARG_NOMINMAX, nullptr},
  {"grid",  &opt_grid,        "Write position using this grid.", nullptr, ARGTYPE_STRING, ARG_NOMINMAX, nullptr},
  {"prec",  &opt_precision,   "Precision of coordinates", "3", ARGTYPE_INT, ARG_NOMINMAX, nullptr},
  {"temp",  &opt_temp,        "Temperature unit [c=Celsius, f=Fahrenheit]", "c", ARGTYPE_STRING, ARG_NOMINMAX, nullptr},
  {"time",  &opt_time_format, "Read/Write time format (i.e. HH:mm:ss xx)", nullptr, ARGTYPE_STRING, ARG_NOMINMAX, nullptr},
  {"utc",   &opt_utc,         "Write timestamps with offset x to UTC time", nullptr, ARGTYPE_INT, "-23", "+23", nullptr},
};

class PathInfo
{
public:
  double length {0};
  time_t start {0};
  time_t time {0};
  double speed {0};
  double total {0};
  int count {0};
  const Waypoint* prev_wpt {nullptr};
  const Waypoint* first_wpt {nullptr};
  const Waypoint* last_wpt {nullptr};
};

static PathInfo* route_info;
static int route_idx;
static PathInfo* cur_info;

static const QVector<QString> headers = {
  "Name\tDescription\tType\tPosition\tAltitude\tDepth\tProximity\tTemperature\t"
  "Display Mode\tColor\tSymbol\tFacility\tCity\tState\tCountry\t"
  "Date Modified\tLink\tCategories",
  "Waypoint Name\tDistance\tLeg Length\tCourse",
  "Position\tTime\tAltitude\tDepth\tTemperature\tLeg Length\tLeg Time\tLeg Speed\tLeg Course",
  "Name\tLength\tCourse\tWaypoints\tLink",
  "Name\tStart Time\tElapsed Time\tLength\tAverage Speed\tLink"
};

/* helpers */

static const char*
get_option_val(const char* option, const char* def)
{
  const char* c = (option != nullptr) ? option : def;
  return c;
}

static void
init_date_and_time_format()
{
  // This is old, and weird, code.. date_time_format is a global that's
  // explicitly malloced and freed elsewhere. This isn't very C++ at all,
  // but this format is on its deathbead for deprecation.
  const char* d = get_option_val(opt_date_format, kDefaultDateFormat);
  QString d1 = convert_human_date_format(d);

  const char* t = get_option_val(opt_time_format, kDefaultTimeFormat);
  QString t1 = convert_human_time_format(t);

  xasprintf(&date_time_format, "%s %s", CSTR(d1), CSTR(t1));
}

static void
convert_datum(const Waypoint* wpt, double* dest_lat, double* dest_lon)
{
  double alt;

  if (datum_index == kDautmWGS84) {
    *dest_lat = wpt->latitude;
    *dest_lon = wpt->longitude;
  } else GPS_Math_WGS84_To_Known_Datum_M(wpt->latitude, wpt->longitude, 0.0,
                                           dest_lat, dest_lon, &alt, datum_index);
}

/* WRITER *****************************************************************/

/* Waypoint preparation */

static void
enum_waypt_cb(const Waypoint* wpt)
{
  garmin_fs_t* gmsd = garmin_fs_t::find(wpt);
  int wpt_class = garmin_fs_t::get_wpt_class(gmsd, 0);
  if (wpt_class < 0x80) {
    if (gtxt_flags.enum_waypoints) {		/* enumerate only */
      waypoints++;
      return;
    }
    for (int i = 0; i < wpt_a_ct; i++) {		/* check for duplicates */
      const Waypoint* tmp = wpt_a[i];
      if (tmp->shortname.compare(wpt->shortname, Qt::CaseInsensitive) == 0) {
        wpt_a[i] = wpt;
        waypoints--;
        return;

      }
    }
    wpt_a[wpt_a_ct++] = wpt;
  }

}

/* common route and track pre-work */

static void
prework_hdr_cb(const route_head*)
{
  cur_info = &route_info[route_idx];
}

static void
prework_tlr_cb(const route_head*)
{
  cur_info->last_wpt = cur_info->prev_wpt;
  route_idx++;
}

static void
prework_wpt_cb(const Waypoint* wpt)
{
  const Waypoint* prev = cur_info->prev_wpt;

  if (prev != nullptr) {
    cur_info->time += (wpt->GetCreationTime().toTime_t() - prev->GetCreationTime().toTime_t());
    cur_info->length += waypt_distance_ex(prev, wpt);
  } else {
    cur_info->first_wpt = wpt;
    cur_info->start = wpt->GetCreationTime().toTime_t();
  }
  cur_info->prev_wpt = wpt;
  cur_info->count++;
  routepoints++;
}


/* output helpers */

static void
print_position(const Waypoint* wpt)
{
  int valid = 1;
  double lat, lon, north, east;
  int zone;
  char map[3], zonec;

  convert_datum(wpt, &lat, &lon);

  /* ----------------------------------------------------------------------------*/
  /*            the following code is from pretty_deg_format (util.c)            */
  /* ----------------------------------------------------------------------------*/
  /* !ToDo! generate common code for calculating of degrees, minutes and seconds */
  /* ----------------------------------------------------------------------------*/

  char latsig = lat < 0 ? 'S':'N';
  char lonsig = lon < 0 ? 'W':'E';
  int latint = abs((int) lat);
  int lonint = abs((int) lon);
  double latmin = 60.0 * (fabs(lat) - latint);
  double lonmin = 60.0 * (fabs(lon) - lonint);
  double latsec = 60.0 * (latmin - floor(latmin));
  double lonsec = 60.0 * (lonmin - floor(lonmin));

  switch (grid_index) {

  case grid_lat_lon_ddd:

    *fout << QString::asprintf("%c%0.*f %c%0.*f\t",
                               latsig, precision, fabs(lat),
                               lonsig, precision, fabs(lon));
    break;

  case grid_lat_lon_dmm:

    *fout << QString::asprintf("%c%d %0*.*f %c%d %0*.*f\t",
                               latsig, latint, precision + 3, precision, latmin,
                               lonsig, lonint, precision + 3, precision, lonmin);
    break;

  case grid_lat_lon_dms:

    *fout << QString::asprintf("%c%d %d %.*f %c%d %d %.*f\t",
                               latsig, latint, (int)latmin, precision, latsec,
                               lonsig, lonint, (int)lonmin, precision, lonsec);
    break;

  case grid_bng:

    valid = GPS_Math_WGS84_To_UKOSMap_M(wpt->latitude, wpt->longitude, &east, &north, map);
    if (valid) {
      *fout << QString::asprintf("%s %5.0f %5.0f\t", map, east, north);
    }
    break;

  case grid_utm:

    valid = GPS_Math_Known_Datum_To_UTM_EN(lat, lon,
                                           &east, &north, &zone, &zonec, datum_index);
    if (valid) {
      *fout << QString::asprintf("%02d %c %.0f %.0f\t", zone, zonec, east, north);
    }
    break;

  case grid_swiss:

    valid = GPS_Math_WGS84_To_Swiss_EN(wpt->latitude, wpt->longitude, &east, &north);
    if (valid) {
      *fout << QString::asprintf("%.f %.f\t", east, north);
    }
    break;

  default:
    fatal("ToDo\n");
  }

  if (! valid) {
    *fout << "#####\n";
    fatal(MYNAME ": %s (%s) is outside of convertible area \"%s\"!\n",
          wpt->shortname.isEmpty() ? "Waypoint" : qPrintable(wpt->shortname),
          qPrintable(pretty_deg_format(wpt->latitude, wpt->longitude, 'd', nullptr, false)),
          gt_get_mps_grid_longname(grid_index, MYNAME));
  }
}

static void
print_date_and_time(const time_t time, const int time_only)
{
  struct tm tm;
  char tbuf[32];

  if (time < 0) {
    *fout << "\t";
    return;
  }
  if (time_only) {
    tm = *gmtime(&time);
    snprintf(tbuf, sizeof(tbuf), "%d:%02d:%02d", tm.tm_hour, tm.tm_min, tm.tm_sec);
    *fout << QString::asprintf("%s", tbuf);
  } else if (time != 0) {
    if (gtxt_flags.utc) {
      time_t t = time + utc_offs;
      tm = *gmtime(&t);
    } else {
      tm = *localtime(&time);
    }
    strftime(tbuf, sizeof(tbuf), date_time_format, &tm);
    *fout << QString::asprintf("%s ", tbuf);
  }
  *fout << "\t";
}

static void
print_categories(uint16_t categories)
{
  if (categories == 0) {
    return;
  }

  int count = 0;
  for (int i = 0; i < 16; i++) {
    if ((categories & 1) != 0) {
      QString c;
      if (global_opts.inifile != nullptr) {
        QString key = QString::number(i + 1);
        c = inifile_readstr(global_opts.inifile, GMSD_SECTION_CATEGORIES, key);
      }

      *fout << QString::asprintf("%s", (count++ > 0) ? "," : "");
      if (c.isNull()) {
        *fout << QString::asprintf("Category %d", i+1);
      }
//				*fout << QString::asprintf("%s", gps_categories[i]);
      else {
        *fout << c;
      }

    }
    categories = categories >> 1;
  }
}

static void
print_course(const Waypoint* A, const Waypoint* B)		/* seems to be okay */
{
  if ((A != nullptr) && (B != nullptr) && (A != B)) {
    int course = qRound(waypt_course(A, B));
    *fout << QString::asprintf("%d° true", course);
  }
}

static void
print_distance(const double distance, const int no_scale, const int with_tab, const int decis)
{
  double dist = distance;

  if (gtxt_flags.metric == 0) {
    dist = METERS_TO_FEET(dist);

    if ((dist < 5280) || no_scale) {
      *fout << QString::asprintf("%.*f ft", decis, dist);
    } else {
      dist = METERS_TO_MILES(distance);
      if (dist < 100.0) {
        *fout << QString::asprintf("%.1f mi", dist);
      } else {
        *fout << QString::asprintf("%d mi", qRound(dist));
      }
    }
  } else {
    if ((dist < 1000) || no_scale) {
      *fout << QString::asprintf("%.*f m", decis, dist);
    } else {
      dist = dist / 1000.0;
      if (dist < 100.0) {
        *fout << QString::asprintf("%.1f km", dist);
      } else {
        *fout << QString::asprintf("%d km", qRound(dist));
      }
    }
  }
  if (with_tab) {
    *fout << "\t";
  }
}

static void
print_speed(const double* distance, const time_t* time)
{
  double dist = *distance;
  const char* unit;

  if (!gtxt_flags.metric) {
    dist = METERS_TO_MILES(dist) * 1000.0;
    unit = "mph";
  } else {
    unit = "kph";
  }
  int idist = qRound(dist);

  if ((*time != 0) && (idist > 0)) {
    double speed = MPS_TO_KPH(dist / (double)*time);
    int ispeed = qRound(speed);

    if (speed < 0.01) {
      *fout << QString::asprintf("0 %s", unit);
    } else if (ispeed < 2) {
      *fout << QString::asprintf("%.1f %s", speed, unit);
    } else {
      *fout << QString::asprintf("%d %s", ispeed, unit);
    }
  } else {
    *fout << QString::asprintf("0 %s", unit);
  }
  *fout << "\t";
}

static void
print_temperature(const float temperature)
{
  if (gtxt_flags.celsius) {
    *fout << QString::asprintf("%.f C", temperature);
  } else {
    *fout << QString::asprintf("%.f F", (temperature * 1.8) + 32);
  }
}

static void
print_string(const char* fmt, const QString& string)
{
  /* remove unwanted characters from source string */
  QString cleanstring;
  for (const auto& chr : string) {
    if (chr.category() != QChar::Other_Control) {
      cleanstring.append(chr);
    } else {
      cleanstring.append(' ');
    }
  }
  *fout << QString::asprintf(fmt, CSTR(cleanstring));
}


/* main cb's */

static void
write_waypt(const Waypoint* wpt)
{
  const char* wpt_type;

  garmin_fs_t* gmsd = garmin_fs_t::find(wpt);

  int i = garmin_fs_t::get_display(gmsd, 0);
  if (i > GT_DISPLAY_MODE_MAX) {
    i = 0;
  }
  const char* dspl_mode = gt_display_mode_names[i];

  unsigned char wpt_class = garmin_fs_t::get_wpt_class(gmsd, 0);
  if (wpt_class <= gt_waypt_class_map_line) {
    wpt_type = gt_waypt_class_names[wpt_class];
  } else {
    wpt_type = gt_waypt_class_names[0];
  }

  *fout << "Waypoint\t" << wpt->shortname << "\t";
  if (wpt_class <= gt_waypt_class_airport_ndb) {
    QString temp = wpt->notes;
    if (temp.isEmpty()) {
      if (wpt->description != wpt->shortname) {
        temp = wpt->description;
      } else {
        temp = "";
      }
    }
    print_string("%s\t", temp);
  } else {
    *fout << "\t";
  }
  *fout << QString::asprintf("%s\t", wpt_type);

  print_position(wpt);

  if (is_valid_alt(wpt->altitude)) {
    print_distance(wpt->altitude, 1, 0, 0);
  }
  *fout << "\t";

  double x = wpt->depth_value_or(unknown_alt);
  if (x != unknown_alt) {
    print_distance(x, 1, 0, 1);
  }
  *fout << "\t";

  x = wpt->proximity_value_or(unknown_alt);
  if (x != unknown_alt) {
    print_distance(x, 0, 0, 0);
  }
  *fout << "\t";

  x = wpt->temperature_value_or(-999);
  if (x != -999) {
    print_temperature(x);
  }
  *fout << QString::asprintf("\t%s\t", dspl_mode);

  *fout << "Unknown\t"; 				/* Color is fixed: Unknown */

  int icon = garmin_fs_t::get_icon(gmsd, -1);
  if (icon == -1) {
    icon = gt_find_icon_number_from_desc(wpt->icon_descr, GDB);
  }
  print_string("%s\t", gt_find_desc_from_icon_number(icon, GDB));

  print_string("%s\t", garmin_fs_t::get_facility(gmsd, ""));
  print_string("%s\t", garmin_fs_t::get_city(gmsd, ""));
  print_string("%s\t", garmin_fs_t::get_state(gmsd, ""));
  const char* country = gt_get_icao_country(garmin_fs_t::get_cc(gmsd, ""));
  print_string("%s\t", (country != nullptr) ? country : "");
  print_date_and_time(wpt->GetCreationTime().toTime_t(), 0);
  if (wpt->HasUrlLink()) {
    UrlLink l = wpt->GetUrlLink();
    print_string("%s\t", l.url_);
  } else {
    print_string("%s\t", "");
  }
  print_categories(garmin_fs_t::get_category(gmsd, 0));

  *fout << "\r\n";
}

static void
route_disp_hdr_cb(const route_head* rte)
{
  cur_info = &route_info[route_idx];
  cur_info->prev_wpt = nullptr;
  cur_info->total = 0;
  if (rte->rte_waypt_empty()) {
    return;
  }

  if (!gtxt_flags.route_header_written) {
    gtxt_flags.route_header_written = 1;
    *fout << "\r\n\r\nHeader\t" << headers[route_header] << "\r\n";
  }
  print_string("\r\nRoute\t%s\t", rte->rte_name);
  print_distance(cur_info->length, 0, 1, 0);
  print_course(cur_info->first_wpt, cur_info->last_wpt);
  *fout << QString::asprintf("\t%d waypoints\t", cur_info->count);
  if (rte->rte_urls.HasUrlLink()) {
    print_string("%s\r\n", rte->rte_urls.GetUrlLink().url_);
  } else {
    print_string("%s\r\n", "");
  }
  *fout << "\r\nHeader\t" << headers[rtept_header] << "\r\n\r\n";
}

static void
route_disp_tlr_cb(const route_head*)
{
  route_idx++;
}

static void
route_disp_wpt_cb(const Waypoint* wpt)
{
  const Waypoint* prev = cur_info->prev_wpt;

  *fout << "Route Waypoint\t" << wpt->shortname << "\t";

  if (prev != nullptr) {
    double dist = waypt_distance_ex(prev, wpt);
    cur_info->total += dist;
    print_distance(cur_info->total, 0, 1, 0);
    print_distance(dist, 0, 1, 0);
    print_course(prev, wpt);
  } else {
    print_distance(0, 1, 0, 0);
  }

  *fout << "\r\n";

  cur_info->prev_wpt = wpt;
}

static void
track_disp_hdr_cb(const route_head* track)
{
  cur_info = &route_info[route_idx];
  cur_info->prev_wpt = nullptr;
  cur_info->total = 0;
  if (track->rte_waypt_empty()) {
    return;
  }

  if (!gtxt_flags.track_header_written) {
    gtxt_flags.track_header_written = 1;
    *fout << "\r\n\r\nHeader\t" << headers[track_header] << "\r\n";
  }
  print_string("\r\nTrack\t%s\t", track->rte_name);
  print_date_and_time(cur_info->start, 0);
  print_date_and_time(cur_info->time, 1);
  print_distance(cur_info->length, 0, 1, 0);
  print_speed(&cur_info->length, &cur_info->time);
  if (track->rte_urls.HasUrlLink()) {
    print_string("%s", track->rte_urls.GetUrlLink().url_);
  } else {
    print_string("%s", "");
  }
  *fout << "\r\n\r\nHeader\t" << headers[trkpt_header] << "\r\n\r\n";
}

static void
track_disp_tlr_cb(const route_head*)
{
  route_idx++;
}

static void
track_disp_wpt_cb(const Waypoint* wpt)
{
  const Waypoint* prev = cur_info->prev_wpt;
  time_t delta;
  double dist;

  *fout << "Trackpoint\t";

  print_position(wpt);
  print_date_and_time(wpt->GetCreationTime().toTime_t(), 0);
  if (is_valid_alt(wpt->altitude)) {
    print_distance(wpt->altitude, 1, 0, 0);
  }

  *fout << "\t";
  double depth = wpt->depth_value_or(unknown_alt);
  if (depth != unknown_alt) {
    print_distance(depth, 1, 0, 1);
  }

  if (prev != nullptr) {
    *fout << "\t";
    delta = wpt->GetCreationTime().toTime_t() - prev->GetCreationTime().toTime_t();
    float temp = wpt->temperature_value_or(-999);
    if (temp != -999) {
      print_temperature(temp);
    }
    *fout << "\t";
    dist = waypt_distance_ex(prev, wpt);
    print_distance(dist, 0, 1, 0);
    print_date_and_time(delta, 1);
    print_speed(&dist, &delta);
    print_course(prev, wpt);
  }
  *fout << "\r\n";

  cur_info->prev_wpt = wpt;
}

/*******************************************************************************
* %%%        global callbacks called by gpsbabel main process              %%% *
*******************************************************************************/

static void
garmin_txt_wr_init(const QString& fname)
{
  memset(&gtxt_flags, 0, sizeof(gtxt_flags));

  fout = new gpsbabel::TextStream;
  fout->open(fname, QIODevice::WriteOnly, MYNAME, "windows-1252");

  gtxt_flags.metric = (toupper(*get_option_val(opt_dist, "m")) == 'M');
  gtxt_flags.celsius = (toupper(*get_option_val(opt_temp, "c")) == 'C');
  init_date_and_time_format();
  if (opt_precision) {
    precision = xstrtoi(opt_precision, nullptr, 10);
    if (precision < 0) {
      fatal(MYNAME ": Invalid precision (%s)!", opt_precision);
    }
  }

  datum_str = get_option_val(opt_datum, nullptr);
  const char* grid_str = get_option_val(opt_grid, nullptr);

  grid_index = grid_lat_lon_dmm;
  if (grid_str != nullptr) {
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
    datum_index = kDatumOSGB36;
    break;
  case grid_swiss: /* force datum to WGS84 */
    datum_index = kDautmWGS84;
    break;
  default:
    datum_index = gt_lookup_datum_index(datum_str, MYNAME);
  }

  if (opt_utc != nullptr) {
    if (case_ignore_strcmp(opt_utc, "utc") == 0) {
      utc_offs = 0;
    } else {
      utc_offs = xstrtoi(opt_utc, nullptr, 10);
    }
    utc_offs *= (60 * 60);
    gtxt_flags.utc = 1;
  }
}

static void
garmin_txt_wr_deinit()
{
  fout->close();
  delete fout;
  fout = nullptr;
  xfree(date_time_format);
}

static void
garmin_txt_write()
{
  QString grid_str = gt_get_mps_grid_longname(grid_index, MYNAME);
  grid_str = grid_str.replace('*', "°");
  *fout << "Grid\t" << grid_str << "\r\n";

  datum_str = gt_get_mps_datum_name(datum_index);
  *fout << QString::asprintf("Datum\t%s\r\n\r\n", datum_str);

  waypoints = 0;
  gtxt_flags.enum_waypoints = 1;			/* enum all waypoints */
  waypt_disp_all(enum_waypt_cb);
  route_disp_all(nullptr, nullptr, enum_waypt_cb);
  gtxt_flags.enum_waypoints = 0;

  if (waypoints > 0) {
    wpt_a_ct = 0;
    wpt_a = new const Waypoint*[waypoints] {};
    waypt_disp_all(enum_waypt_cb);
    route_disp_all(nullptr, nullptr, enum_waypt_cb);
    auto sort_waypt_lambda = [](const Waypoint* wa, const Waypoint* wb)->bool {
      return wa->shortname.compare(wb->shortname, Qt::CaseInsensitive) < 0;
    };
    std::sort(wpt_a, wpt_a + waypoints, sort_waypt_lambda);

    *fout << "Header\t" << headers[waypt_header] << "\r\n\r\n";
    for (int i = 0; i < waypoints; i++) {
      write_waypt(wpt_a[i]);
    }
    delete[] wpt_a;

    route_idx = 0;
    route_info = new PathInfo[route_count()];
    routepoints = 0;
    route_disp_all(prework_hdr_cb, prework_tlr_cb, prework_wpt_cb);
    if (routepoints > 0) {
      route_idx = 0;
      route_disp_all(route_disp_hdr_cb, route_disp_tlr_cb, route_disp_wpt_cb);
    }
    delete[] route_info;
    route_info = nullptr;
  }

  route_idx = 0;
  route_info = new PathInfo[track_count()];
  routepoints = 0;
  track_disp_all(prework_hdr_cb, prework_tlr_cb, prework_wpt_cb);

  if (routepoints > 0) {
    route_idx = 0;
    track_disp_all(track_disp_hdr_cb, track_disp_tlr_cb, track_disp_wpt_cb);
  }
  delete[] route_info;
}

/* READER *****************************************************************/

/* helpers */

static void
free_header(const header_type ht)
{
  for (int i = 0; i < kMaxHeaderFields; i++) {
    header_lines[ht][i].clear();
  }
  header_ct[ht] = 0;
  memset(header_fields[ht], 0, sizeof(header_fields[ht]));
}

// Super simple attempt to convert strftime/strptime spec to Qt spec.
// This misses a LOT of cases and vagaries, but the reality is that we
// see very few date formats here.
static QString
strftime_to_timespec(const char* s)
{
  QString q;
  int l = strlen(s);
  q.reserve(l * 2); // no penalty if our guess is wrong.

  for (int i = 0; i < l; i++) {
    switch (s[i]) {
    case '%':
      if (i < l-1) {
        switch (s[++i]) {
        case 'd':
          q += "dd";
          continue;
        case 'm':
          q += "MM";
          continue;
        case 'y':
          q += "yy";
          continue;
        case 'Y':
          q += "yyyy";
          continue;
        case 'H':
          q += "hh";
          continue;
        case 'M':
          q += "mm";
          continue;
        case 'S':
          q += "ss";
          continue;
        case 'A':
          q += "dddd";
          continue;
        case 'a':
          q += "ddd";
          continue;
        case 'B':
          q += "MMMM";
          continue;
        case 'C':
          q += "yy";
          continue;
        case 'D':
          q += "MM/dd/yyyy";
          continue;
        case 'T':
          q += "hh:mm:ss";
          continue;
        case 'F':
          q += "yyyy-MM-dd";
          continue;
        default:
          q += s[i+1];
          break;
        }
      }
      break;
    default:
      q += s[i];
      break;
    }
  }
  return q;
}


/* data parsers */

// This could return an optional QDateTime instead or a pair.
static bool
parse_date_and_time(const QString& str, QDateTime* value)
{
  QString timespec = strftime_to_timespec(date_time_format);
  QDateTime dt;
  dt = QDateTime::fromString(QString(str).trimmed(), timespec);
  bool dt_is_valid = dt.isValid();
  if (dt_is_valid) {
    *value = dt;
  }
  return dt_is_valid;
}

static uint16_t
parse_categories(const QString& str)
{
  uint16_t res = 0;

  const QStringList catstrings = str.split(',');
  for (const auto& catstring : catstrings) {
    QString cin = catstring.trimmed();
    if (!cin.isEmpty()) {
      uint16_t val;
      if (!garmin_fs_convert_category(CSTR(cin), &val)) {
        warning(MYNAME ": Unable to convert category \"%s\" at line %d!\n", qPrintable(cin), current_line);
      } else {
        res = res | val;
      }
    }
  }
  return res;
}

static bool
parse_temperature(const QString& str, double* temperature)
{
  double value;
  unsigned char unit;

  if (str.isEmpty()) {
    return false;
  }

  if (sscanf(CSTR(str), "%lf %c", &value, &unit) == 2) {
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
    return true;
  } else {
    fatal(MYNAME ": Invalid temperature \"%s\" at line %d!\n", qPrintable(str), current_line);
  }
  return false;
}

static void
parse_header(const QStringList& lineparts)
{
  int column = -1;

  free_header(unknown_header);

  for (const auto& str : lineparts) {
    column++;
    header_lines[unknown_header][column] = str.toUpper();
    header_ct[unknown_header]++;
    if (header_ct[unknown_header] >= kMaxHeaderFields) {
      break;
    }
  }
}

static bool
parse_display(const QString& str, int* val)
{
  if (str.isEmpty()) {
    return false;
  }

  for (gt_display_modes_e i = GT_DISPLAY_MODE_MIN; i <= GT_DISPLAY_MODE_MAX; ++i) {
    if (case_ignore_strcmp(str, gt_display_mode_names[i]) == 0) {
      *val = i;
      return true;
    }
  }
  warning(MYNAME ": Unknown display mode \"%s\" at line %d.\n", qPrintable(str), current_line);
  return false;
}

static void
bind_fields(const header_type ht)
{
  if ((grid_index < 0) || (datum_index < 0)) {
    fatal(MYNAME ": Incomplete or invalid file header!");
  }

  if (header_ct[unknown_header] <= 0) {
    return;
  }
  free_header(ht);

  /* make a copy of headers[ht], uppercase, split on "\t" */
  const QStringList altheader = headers.at(ht).toUpper().split('\t');

  for (int i = 0; i < header_ct[unknown_header]; i++) {
    auto name = header_lines[ht][i] = header_lines[unknown_header][i];
    header_lines[unknown_header][i].clear();

    int field_idx = altheader.indexOf(name);
    if (field_idx >= 0) {
      int field_no = field_idx + 1;
      header_fields[ht][i] = field_no;
#if 0
      printf("Binding field \"%s\" to internal number %d (%d,%d)\n", qPrintable(name), field_no, ht, i);
#endif
    }
  }
  header_ct[unknown_header] = 0;
}

static void
parse_grid(const QStringList& lineparts)
{
  if (lineparts.empty()) {
    fatal(MYNAME ": Missing grid headline!\n");
  }

  const QByteArray ba = lineparts.at(0).toUtf8();
  const char* str = ba.constData();
  if (strstr(str, "dd.ddddd") != nullptr) {
    grid_index = grid_lat_lon_ddd;
  } else if (strstr(str, "mm.mmm") != nullptr) {
    grid_index = grid_lat_lon_dmm;
  } else if (strstr(str, "mm'ss.s") != nullptr) {
    grid_index = grid_lat_lon_dms;
  } else {
    grid_index = gt_lookup_grid_type(str, MYNAME);
  }
}

static void
parse_datum(const QStringList& lineparts)
{
  if (lineparts.empty()) {
    fatal(MYNAME ": Missing GPS datum headline!\n");
  }

  const auto& str = lineparts.at(0);
  datum_index = gt_lookup_datum_index(CSTR(str), MYNAME);
}

static void
parse_waypoint(const QStringList& lineparts)
{
  int column = -1;

  bind_fields(waypt_header);

  auto* wpt = new Waypoint;
  garmin_fs_t* gmsd = garmin_fs_alloc(-1);
  wpt->fs.FsChainAdd(gmsd);

  for (const auto& str : lineparts) {
    column++;
    int i;
    double d;
    int field_no = header_fields[waypt_header][column];

    switch (field_no) {
    case  1:
      if (!str.isEmpty()) {
        wpt->shortname = str;
      }
      break;
    case  2:
      if (!str.isEmpty()) {
        wpt->notes = str;
      }
      break;
    case  3:
      for (i = 0; i <= gt_waypt_class_map_line; i++) {
        if (case_ignore_strcmp(str, gt_waypt_class_names[i]) == 0) {
          garmin_fs_t::set_wpt_class(gmsd, i);
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
        wpt->set_depth(d);
      }
      break;
    case  7:
      if (parse_distance(str, &d, 1, MYNAME)) {
        wpt->set_proximity(d);
      }
      break;
    case  8:
      if (parse_temperature(str, &d)) {
        wpt->set_temperature(d);
      }
      break;
    case  9:
      if (parse_display(str, &i)) {
        garmin_fs_t::set_display(gmsd, i);
      }
      break;
    case 10:
      break;	/* skip color */
    case 11:
      i = gt_find_icon_number_from_desc(str, GDB);
      garmin_fs_t::set_icon(gmsd, i);
      wpt->icon_descr = gt_find_desc_from_icon_number(i, GDB);
      break;
    case 12:
      garmin_fs_t::set_facility(gmsd, str);
      break;
    case 13:
      garmin_fs_t::set_city(gmsd, str);
      break;
    case 14:
      garmin_fs_t::set_state(gmsd, str);
      break;
    case 15:
      garmin_fs_t::set_country(gmsd, str);
      garmin_fs_t::set_cc(gmsd, gt_get_icao_cc(str, wpt->shortname));
      break;
    case 16: {
      QDateTime ct;
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
      garmin_fs_t::set_category(gmsd, parse_categories(str));
      break;
    default:
      break;
    }
  }
  waypt_add(wpt);
}

static void
parse_route_header(const QStringList& lineparts)
{
  int column = -1;

  auto* rte = new route_head;

  bind_fields(route_header);
  for (const auto& str : lineparts) {
    column++;
    int field_no = header_fields[route_header][column];
    switch (field_no) {
    case 1:
      if (!str.isEmpty()) {
        rte->rte_name = str;
      }
      break;
    case 5:
      rte->rte_urls.AddUrlLink(UrlLink(str));
      break;
    }
  }
  route_add_head(rte);
  current_rte = rte;
}

static void
parse_track_header(const QStringList& lineparts)
{
  int column = -1;

  bind_fields(track_header);
  auto* trk = new route_head;
  for (const auto& str : lineparts) {
    column++;
    int field_no = header_fields[track_header][column];
    switch (field_no) {
    case 1:
      if (!str.isEmpty()) {
        trk->rte_name = str;
      }
      break;
    case 6:
      trk->rte_urls.AddUrlLink(UrlLink(str));
      break;
    }
  }
  track_add_head(trk);
  current_trk = trk;
}


static void
parse_route_waypoint(const QStringList& lineparts)
{
  int column = -1;
  Waypoint* wpt = nullptr;

  bind_fields(rtept_header);

  for (const auto& str : lineparts) {
    column++;
    int field_no = header_fields[rtept_header][column];
    switch (field_no) {
    case 1:
      if (str.isEmpty()) {
        fatal(MYNAME ": Route waypoint without name at line %d!\n", current_line);
      }
      wpt = find_waypt_by_name(str);
      if (wpt == nullptr) {
        fatal(FatalMsg() << MYNAME << ": Route waypoint " << str << " not in waypoint list (line " << current_line<< ")!\n");
      }
      wpt = new Waypoint(*wpt);
      break;
    }
  }
  if (wpt != nullptr) {
    route_add_wpt(current_rte, wpt);
  }
}

static void
parse_track_waypoint(const QStringList& lineparts)
{
  int column = -1;

  bind_fields(trkpt_header);
  auto* wpt = new Waypoint;

  for (const auto& str : lineparts) {
    column++;
    double x;

    if (str.isEmpty()) {
      continue;
    }

    int field_no = header_fields[trkpt_header][column];
    switch (field_no) {
    case 1:
      parse_coordinates(str, datum_index, grid_index,
                        &wpt->latitude, &wpt->longitude, MYNAME);
      break;
    case 2: {
      QDateTime ct;
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
        wpt->set_depth(x);
      }
      break;
    case 5:
      if (parse_temperature(str, &x)) {
        wpt->set_temperature(x);
      }
      break;
    case 8:
      if (parse_speed(str, &x, 1, MYNAME)) {
        wpt->set_speed(x);
      }
      break;
    case 9:
      wpt->set_course(xstrtoi(CSTR(str), nullptr, 10));
      break;
    }
  }
  track_add_wpt(current_trk, wpt);
}

/***************************************************************/

static void
garmin_txt_rd_init(const QString& fname)
{
  memset(&gtxt_flags, 0, sizeof(gtxt_flags));

  fin = new gpsbabel::TextStream;
  fin->open(fname, QIODevice::ReadOnly, MYNAME, "windows-1252");
  memset(&header_ct, 0, sizeof(header_ct));

  datum_index = -1;
  grid_index = (grid_type) -1;

  init_date_and_time_format();
}

static void
garmin_txt_rd_deinit()
{
  for (header_type h = waypt_header; h <= unknown_header; ++h) {
    free_header(h);
  }
  fin->close();
  delete fin;
  fin = nullptr;
  xfree(date_time_format);
}

static void
garmin_txt_read()
{
  QString buff;

  current_line = 0;
  while ((buff = fin->readLine(), !buff.isNull())) {
    ++current_line;
    buff = buff.trimmed();

    if (buff.isEmpty()) {
      continue;
    }

    QStringList lineparts = csv_linesplit(buff, "\t", "", 0);

    if (lineparts.empty()) {
      continue;
    }
    auto linetype = lineparts.at(0);
    lineparts.removeFirst();

    if (linetype.compare(u"Header", Qt::CaseInsensitive) == 0) {
      parse_header(lineparts);
    } else if (linetype.compare(u"Grid", Qt::CaseInsensitive) == 0) {
      parse_grid(lineparts);
    } else if (linetype.compare(u"Datum", Qt::CaseInsensitive) == 0) {
      parse_datum(lineparts);
    } else if (linetype.compare(u"Waypoint", Qt::CaseInsensitive) == 0) {
      parse_waypoint(lineparts);
    } else if (linetype.compare(u"Route Waypoint", Qt::CaseInsensitive) == 0) {
      parse_route_waypoint(lineparts);
    } else if (linetype.compare(u"Trackpoint", Qt::CaseInsensitive) == 0) {
      parse_track_waypoint(lineparts);
    } else if (linetype.compare(u"Route", Qt::CaseInsensitive) == 0) {
      parse_route_header(lineparts);
    } else if (linetype.compare(u"Track", Qt::CaseInsensitive) == 0) {
      parse_track_header(lineparts);
    } else if (linetype.compare(u"Map", Qt::CaseInsensitive) == 0) /* do nothing */ ;
    else {
      fatal(MYNAME ": Unknown identifier (%s) at line %d!\n", qPrintable(linetype), current_line);
    }

  }
}

/*
 * The file encoding is windows-1252.
 * Conversion between windows-1252 and utf-16 is handled by the stream.
 * Conversion between utf-16 and utf-8 is handled by this format.
 * Let main know char strings have already been converted to utf-8
 * so it doesn't attempt to re-convert any char strings including gmsd data.
 */

ff_vecs_t garmin_txt_vecs = {
  ff_type_file,
  FF_CAP_RW_ALL,
  garmin_txt_rd_init,
  garmin_txt_wr_init,
  garmin_txt_rd_deinit,
  garmin_txt_wr_deinit,
  garmin_txt_read,
  garmin_txt_write,
  nullptr,
  &garmin_txt_args,
  NULL_POS_OPS
};

#endif // CSVFMTS_ENABLED
