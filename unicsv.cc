/*
    Universal CSV - support for csv files, divining field order from the header.

    Copyright (C) 2006-2013 Robert Lipe, robertlipe+source@gpsbabel.org
    copyright (C) 2007,2008 Olaf Klein, o.b.klein@gpsbabel.org

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

#include <cmath>                   // for fabs, lround
#include <cstdio>                  // for NULL, sscanf
#include <cstdint>
#include <cstdlib>                 // for atoi
#include <cstring>                 // for memset, strchr, strncpy
#include <ctime>                   // for gmtime

#include <QtCore/QByteArray>       // for QByteArray
#include <QtCore/QChar>            // for QChar
#include <QtCore/QCharRef>         // for QCharRef
#include <QtCore/QDateTime>        // for QDateTime
#include <QtCore/QLatin1String>    // for QLatin1String
#include <QtCore/QString>          // for QString, operator!=, operator==
#include <QtCore/QStringList>      // for QStringList
#include <QtCore/QTime>            // for QTime
#include <QtCore/QVector>          // for QVector
#include <QtCore/Qt>               // for CaseInsensitive
#include <QtCore/QtGlobal>         // for qPrintable

#include "defs.h"
#include "cet_util.h"              // for cet_convert_init
#include "csv_util.h"              // for csv_linesplit, human_to_dec
#include "garmin_fs.h"             // for garmin_fs_flags_t, garmin_fs_t, GMSD_GET, GMSD_HAS, GMSD_SETQSTR, GMSD_FIND, garmin_fs_alloc
#include "garmin_tables.h"         // for gt_lookup_datum_index, gt_get_mps_grid_longname, gt_lookup_grid_type
#include "gbfile.h"                // for gbfprintf, gbfputs, gbfclose, gbfgetstr, gbfopen, gbfile
#include "jeeps/gpsmath.h"         // for GPS_Math_UKOSMap_To_WGS84_M, GPS_Math_EN_To_UKOSNG_Map, GPS_Math_Known_Datum_To_UTM_EN, GPS_Math_Known_Datum_To_WGS84_M, GPS_Math_Swiss_EN_To_WGS84, GPS_Math_UTM_EN_To_Known_Datum, GPS_Math_WGS84_To_Known_Datum_M, GPS_Math_WGS84_To_Swiss_EN, GPS_Math_WGS...
#include "session.h"               // for session_t
#include "src/core/datetime.h"     // for DateTime
#include "src/core/logging.h"      // for Warning, Fatal


#define MYNAME "unicsv"

/* "UNICSV_FIELD_SEP" and "UNICSV_LINE_SEP" are only used by the writer */

#define UNICSV_FIELD_SEP	","
#define UNICSV_LINE_SEP		"\r\n"
#define UNICSV_QUOT_CHAR	'"'

/* GPSBabel internal and calculated fields */

typedef enum {
  fld_shortname = 0,
  fld_latitude,
  fld_longitude,
  fld_description,
  fld_notes,
  fld_url,
  fld_altitude,
  fld_utm_zone,
  fld_utm_zone_char,
  fld_utm_northing,
  fld_utm_easting,
  fld_utm,
  fld_bng,
  fld_bng_zone,
  fld_bng_northing,
  fld_bng_easting,
  fld_swiss,
  fld_swiss_northing,
  fld_swiss_easting,
  fld_hdop,
  fld_pdop,
  fld_vdop,
  fld_sat,
  fld_fix,
  fld_utc_date,
  fld_utc_time,
  fld_course,
  fld_speed,
  fld_temperature,
  fld_temperature_f,
  fld_heartrate,
  fld_cadence,
  fld_power,
  fld_proximity,
  fld_depth,
  fld_symbol,
  fld_date,
  fld_time,
  fld_datetime,
  fld_iso_time,
  fld_year,
  fld_month,
  fld_day,
  fld_hour,
  fld_min,
  fld_sec,
  fld_ns,
  fld_ew,

  fld_garmin_city,
  fld_garmin_postal_code,
  fld_garmin_state,
  fld_garmin_country,
  fld_garmin_addr,
  fld_garmin_phone_nr,
  fld_garmin_phone_nr2,
  fld_garmin_fax_nr,
  fld_garmin_email,
  fld_garmin_facility,
  fld_gc_id,
  fld_gc_type,
  fld_gc_container,
  fld_gc_terr,
  fld_gc_diff,
  fld_gc_is_archived,
  fld_gc_is_available,
  fld_gc_exported,
  fld_gc_last_found,
  fld_gc_placer,
  fld_gc_placer_id,
  fld_gc_hint,
  fld_terminator
} field_e;

#define STR_LEFT	1
#define STR_RIGHT	2
#define STR_ANY		4
#define STR_EQUAL	8
#define STR_CASE	16

#define unicsv_unknown	1e25

typedef struct {
  const char* name;
  field_e type;
  uint32_t options;
} field_t;

/*
 * ! Please use always underscores in field names !
 * we check a second time after replacing underscores with spaces
 */
static field_t fields_def[] = {
  /* unhandled columns */
  { "index",	fld_terminator, STR_ANY },
  { "no",		fld_terminator, STR_EQUAL },
  { "mini",	fld_terminator, STR_ANY },	/* maybe minimum anything, so
							   avoid detection as 'min' for minute */
  /* handled columns */
  { "name",	fld_shortname, STR_ANY },
  { "title",	fld_shortname, STR_ANY },
  { "desc",	fld_description, STR_ANY },
  { "notes",	fld_notes, STR_ANY },
  { "omment",	fld_notes, STR_ANY },		/* works also for German "Kommentar" */
  { "text",	fld_notes, STR_ANY },
  { "url",	fld_url, STR_ANY },
  { "icon",	fld_symbol, STR_ANY },
  { "symb",	fld_symbol, STR_ANY },
  { "lat",	fld_latitude, STR_ANY },
  { "lon",	fld_longitude, STR_ANY },
  { "lng",	fld_longitude, STR_ANY },
  { "x",		fld_longitude, STR_EQUAL },
  { "y",		fld_latitude, STR_EQUAL },
  { "z",		fld_altitude, STR_EQUAL },
  { "x_pos",	fld_longitude, STR_ANY },
  { "y_pos",	fld_latitude, STR_ANY },
  { "alt",	fld_altitude, STR_ANY },
  { "ele",	fld_altitude, STR_ANY },
  { "height",	fld_altitude, STR_ANY },
  { "utm_z",	fld_utm_zone, STR_ANY },
  { "utm_c",	fld_utm_zone_char, STR_ANY },
  { "utm_zc",	fld_utm_zone_char, STR_ANY },
  { "utm_n",	fld_utm_northing, STR_ANY },
  { "utm_e",	fld_utm_easting, STR_ANY },
  { "utm",	fld_utm, STR_EQUAL },
  { "utm_coo",	fld_utm, STR_ANY },
  { "utm_pos",	fld_utm, STR_ANY },
  { "bng_z",	fld_bng_zone, STR_ANY },
  { "bng_n",	fld_bng_northing, STR_ANY },
  { "bng_e",	fld_bng_easting, STR_ANY },
  { "bng",	fld_bng, STR_EQUAL },
  { "bng_coo",	fld_bng, STR_ANY },
  { "bng_pos",	fld_bng, STR_ANY },
  { "swiss_e",	fld_swiss_easting, STR_ANY },
  { "swiss_n",	fld_swiss_northing, STR_ANY },
  { "swiss",	fld_swiss, STR_EQUAL },
  { "swiss_coo",	fld_swiss, STR_ANY },
  { "swiss_pos",	fld_swiss, STR_ANY },
  { "hdop",	fld_hdop, STR_ANY },
  { "pdop",	fld_pdop, STR_ANY },
  { "vdop",	fld_vdop, STR_ANY },
  { "sat",	fld_sat, STR_ANY },
  { "fix",	fld_fix, STR_ANY },
  { "utc_d",	fld_utc_date, STR_ANY },
  { "utc_t",	fld_utc_time, STR_ANY },
  { "head",	fld_course, STR_ANY },
  { "cour",	fld_course, STR_ANY },
  { "speed",	fld_speed, STR_ANY },
  { "velo",	fld_speed, STR_ANY },
  { "geschw",	fld_speed, STR_ANY },		/* speed in german */
  { "tempf",	fld_temperature_f, STR_EQUAL },	/* degrees fahrenheit */
  { "temp",	fld_temperature, STR_ANY },	/* degrees celsius by default */
  { "heart",	fld_heartrate, STR_ANY },
  { "caden",	fld_cadence, STR_ANY },
  { "power",	fld_power, STR_ANY },
  { "prox",	fld_proximity, STR_ANY },
  { "depth",	fld_depth, STR_ANY },
  { "date",	fld_date, STR_ANY },
  { "datum",	fld_date, STR_ANY },
  { "time",	fld_time, STR_ANY },
  { "zeit",	fld_time, STR_ANY },
  { "hour",	fld_hour, STR_LEFT },
  { "min",	fld_min, STR_LEFT },
  { "sec",	fld_sec, STR_LEFT },
  { "year",	fld_year, STR_LEFT },
  { "month",	fld_month, STR_LEFT },
  { "day",	fld_day, STR_LEFT },
  { "n/s",	fld_ns, STR_ANY },
  { "e/w",	fld_ew, STR_ANY },

  /* garmin specials */
  { "addr",	fld_garmin_addr, STR_ANY },
  { "street",	fld_garmin_addr, STR_ANY },
  { "city",	fld_garmin_city, STR_ANY },
  { "country",	fld_garmin_country, STR_ANY },
  { "post",	fld_garmin_postal_code, STR_ANY },
  { "zip",	fld_garmin_postal_code, STR_ANY },
  { "phone",	fld_garmin_phone_nr, STR_ANY },
  { "phone2",	fld_garmin_phone_nr2, STR_ANY },
  { "fax",	fld_garmin_fax_nr, STR_ANY },
  { "email",	fld_garmin_email, STR_ANY },
  { "state",	fld_garmin_state, STR_ANY },
  { "faci",	fld_garmin_facility, STR_ANY },
  /* geocache details */
  { "gcid",	fld_gc_id, STR_ANY },
  { "type",	fld_gc_type, STR_ANY },
  { "cont",	fld_gc_container, STR_ANY },
  { "terr",	fld_gc_terr, STR_ANY },
  { "diff",	fld_gc_diff, STR_ANY },
  { "arch",	fld_gc_is_archived, STR_ANY },
  { "avail",	fld_gc_is_available, STR_ANY },
  { "exported",	fld_gc_exported, STR_ANY },
  { "found",	fld_gc_last_found, STR_ANY },
  { "placer_id",	fld_gc_placer_id, STR_ANY },
  { "placer",	fld_gc_placer, STR_ANY },
  { "hint",	fld_gc_hint, STR_ANY },
  { nullptr,		fld_terminator, 0 }
};

static QVector<field_e> unicsv_fields_tab;
static double unicsv_altscale, unicsv_depthscale, unicsv_proximityscale
;
static const char* unicsv_fieldsep;
static gbfile* fin, *fout;
static gpsdata_type unicsv_data_type;
static route_head* unicsv_track, *unicsv_route;
static char unicsv_outp_flags[(fld_terminator + 8) / 8];
static grid_type unicsv_grid_idx;
static int unicsv_datum_idx;
static char* opt_datum;
static char* opt_grid;
static char* opt_utc;
static char* opt_filename;
static char* opt_format;
static char* opt_prec;
static char* opt_fields;
static int unicsv_waypt_ct;
static char unicsv_detect;
static int llprec;

static arglist_t unicsv_args[] = {
  {
    "datum", &opt_datum, "GPS datum (def. WGS 84)",
    "WGS 84", ARGTYPE_STRING, ARG_NOMINMAX, nullptr
  },
  {
    "grid",  &opt_grid,  "Write position using this grid.",
    nullptr, ARGTYPE_STRING, ARG_NOMINMAX, nullptr
  },
  {
    "utc",   &opt_utc,   "Write timestamps with offset x to UTC time",
    nullptr, ARGTYPE_INT, "-23", "+23", nullptr
  },
  {
    "format", &opt_format,   "Write name(s) of format(s) from input session(s)",
    nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
  },
  {
    "filename", &opt_filename,   "Write filename(s) from input session(s)",
    nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
  },
  {
    "prec", &opt_prec,   "Precision of numerical coordinates (no grid set)",
    "6", ARGTYPE_INT | ARGTYPE_HIDDEN, "0", "15", nullptr
  },
  {
    "fields",  &opt_fields,  "Name and order of input fields, separated by '+'",
    nullptr, ARGTYPE_STRING, ARG_NOMINMAX, nullptr
  },
  ARG_TERMINATOR
};


/* helpers */

// There is no test coverage of this and it's been wrong for years and
// nobody has noticed...
static int
unicsv_parse_gc_id(const QString& str)
{
  int res = 0;
  const QString kBase35 = "0123456789ABCDEFGHJKMNPQRTVWXYZ"; //  ILOSU are omitted.
  if (str.startsWith("GC")) {
    int base35 = str.size() > 6; // above GCFFFF?
    QString s = str.mid(2);
    while (!s.isEmpty()) {
      res = res * 16 + kBase35.indexOf(s[0]);
      s = str.mid(1);
    }
    if (base35) {
      res -= 411120;
    }
  }
  return res;
}

static time_t
unicsv_parse_date(const char* str, int* consumed)
{
  int p1, p2, p3;
  char sep[2];
  struct tm tm;
  int lconsumed = 0;

  memset(&tm, 0, sizeof(tm));
  int ct = sscanf(str, "%d%1[-.//]%d%1[-.//]%d%n", &p1, sep, &p2, sep, &p3, &lconsumed);
  if (consumed && lconsumed) {
    *consumed = lconsumed;
  }
  if (ct != 5) {
    if (consumed) {		/* don't stop here; it's only sniffing */
      *consumed = 0;	/* for a possible date */
      return 0;
    }
    Fatal() << MYNAME << ": Could not parse date string (" << str << ").\n";
  }

  if ((p1 > 99) || (sep[0] == '-')) { /* Y-M-D (iso like) */
    tm.tm_year = p1;
    tm.tm_mon = p2;
    tm.tm_mday = p3;
  } else if (sep[0] == '.') {	/* Germany and any other countries */
    tm.tm_mday = p1;	/* have a fixed D.M.Y format */
    tm.tm_mon = p2;
    tm.tm_year = p3;
  } else {
    tm.tm_mday = p2;
    tm.tm_mon = p1;
    tm.tm_year = p3;
  }
  if ((p1 < 100) && (p2 < 100) && (p3 < 100)) {
    if (tm.tm_year < 70) {
      tm.tm_year += 2000;
    } else {
      tm.tm_year += 1900;
    }
  }
  /* some low-level checks */
  if ((tm.tm_mon > 12) || (tm.tm_mon < 1) || (tm.tm_mday > 31) || (tm.tm_mday < 1)) {
    if (consumed) {
      *consumed = 0;
      return 0;	/* don't stop here */
    }
    Fatal() << MYNAME << ": Could not parse date string (" << str << ").\n";
  }

  tm.tm_year -= 1900;
  tm.tm_mon -= 1;

  return mkgmtime(&tm);
}

static time_t
unicsv_parse_time(const char* str, int* usec, time_t* date)
{
  int hour, min, sec;
  int consumed = 0;
  double us;
  char sep[2];

  /* If we have somethine we're pretty sure is a date, parse that
   * first, skip over it, and pass that back to the caller)
   */
  time_t ldate = unicsv_parse_date(str, &consumed);
  if (consumed && ldate) {
    str += consumed;
    if (date) {
      *date = ldate;
    }
  }
  int ct = sscanf(str, "%d%1[.://]%d%1[.://]%d%lf", &hour, sep, &min, sep, &sec, &us);
  is_fatal(ct < 5, MYNAME ": Could not parse time string (%s).\n", str);
  if (ct == 6) {
    *usec = lround((us * 1000000));
    if (*usec > 999999) {
      *usec = 0;
      sec++;
    }
  } else {
    *usec = 0;
  }

  return ((hour * SECONDS_PER_HOUR) + (min * 60) + sec);
}

static time_t
unicsv_parse_time(const QString& str, int* msec, time_t* date)
{
  return unicsv_parse_time(CSTR(str), msec, date);
}

static status_type
unicsv_parse_status(const QString& str)
{
  if (str.compare(QLatin1String("true"), Qt::CaseInsensitive) == 0 ||
      str.compare(QLatin1String("yes"), Qt::CaseInsensitive) == 0 ||
      str == "1") {
    return status_true;
  }
  if (str.compare(QLatin1String("false"), Qt::CaseInsensitive) == 0 ||
      str.compare(QLatin1String("no"), Qt::CaseInsensitive) == 0 ||
      str == "0") {
    return status_false;
  }
  return status_unknown;
}

static QDateTime
unicsv_adjust_time(const time_t time, const time_t* date)
{
  time_t res = time;
  if (date) {
    res += *date;
  }
  if (opt_utc) {
    res += atoi(opt_utc) * SECONDS_PER_HOUR;
  } else {
    struct tm tm = *gmtime(&res);
    res = mklocaltime(&tm);
  }
  return QDateTime::fromTime_t(res);
}

static bool
unicsv_compare_fields(const QString& s, const field_t* f)
{
  QString name = f->name;
  QString test = s;
  bool result = false;

  if (!(f->options & STR_CASE)) {
    test = test.toUpper();
    name = name.toUpper();
  }

  if (f->options & STR_EQUAL) {
    result = test == name;
  } else if (f->options & STR_ANY) {
    result = test.contains(name);
  } else if (f->options & STR_LEFT) {
    result = test.startsWith(name);
  } else if (f->options & STR_RIGHT) {
    result = test.endsWith(name);
  }

  if ((! result) && test.contains(' ')) {
    /* replace  ' ' with '_' and try again */
    result = unicsv_compare_fields(test.replace(' ', '_'), f);
  }
  if ((! result) && test.contains('-')) {
    /* replace  '-' with '_' and try again */
    result = unicsv_compare_fields(test.replace('-', '_'), f);
  }

  return result;
}

static void
unicsv_fondle_header(QString header)
{
  /* Convert the entire header to lower case for convenience.
   * If we see a tab in that header, we decree it to be tabsep.
   */
  unicsv_fieldsep = ",";
  if (header.contains('\t')) {
    unicsv_fieldsep = "\t";
  } else if (header.contains(';')) {
    unicsv_fieldsep = ";";
  } else if (header.contains('|')) {
    unicsv_fieldsep = "|";
  }
  header = header.toLower();

  const QStringList values = csv_linesplit(header, unicsv_fieldsep, "\"", 0);
  for (auto value : values) {
    value = value.trimmed();

    field_t* f = &fields_def[0];

    unicsv_fields_tab.append(fld_terminator);
    while (f->name) {
      if (unicsv_compare_fields(value, f)) {
        unicsv_fields_tab.last() = f->type;
        break;
      }
      f++;
    }
    if ((! f->name) && global_opts.debug_level) {
      warning(MYNAME ": Unhandled column \"%s\".\n", qPrintable(value));
    }

    /* handle some special items */
    if (f->type == fld_altitude) {
      if (value.contains("ft") || value.contains("feet")) {
        unicsv_altscale = FEET_TO_METERS(1);
      }
    }
    if (f->type == fld_depth) {
      if (value.contains("ft") || value.contains("feet")) {
        unicsv_depthscale = FEET_TO_METERS(1);
      }
    }
    if (f->type == fld_proximity) {
      if (value.contains("ft") || value.contains("feet")) {
        unicsv_proximityscale = FEET_TO_METERS(1);
      }
    }
    if ((f->type == fld_time) || (f->type == fld_date)) {
      if (value.contains("iso")) {
        f->type = fld_iso_time;
      }
    }
  }
}

static void
unicsv_rd_init(const QString& fname)
{
  char* c;
  unicsv_altscale = 1.0;
  unicsv_depthscale = 1.0;
  unicsv_proximityscale = 1.0;

  unicsv_fields_tab.clear();
  unicsv_data_type = global_opts.objective;
  unicsv_detect = (!(global_opts.masked_objective & (WPTDATAMASK | TRKDATAMASK | RTEDATAMASK | POSNDATAMASK)));

  unicsv_track = unicsv_route = nullptr;
  unicsv_datum_idx = gt_lookup_datum_index(opt_datum, MYNAME);

  fin = gbfopen(fname, "rb", MYNAME);
  if (opt_fields) {
    QString fields = QString(opt_fields).replace("+", ",");
    unicsv_fondle_header(fields);
  } else if ((c = gbfgetstr(fin))) {
    unicsv_fondle_header(c);
  } else {
    unicsv_fieldsep = nullptr;
  }
  if (fin->unicode) {
    cet_convert_init(CET_CHARSET_UTF8, 1);
  }
}

static void
unicsv_rd_deinit()
{
  gbfclose(fin);
  unicsv_fields_tab.clear();
}

static void
unicsv_parse_one_line(char* ibuf)
{
  int  utm_zone = -9999;
  double utm_easting = 0;
  double utm_northing = 0;
  char utm_zc = 'N';
  // Zones are always two bytes.  Spare one for null termination..
  char bng_zone[3] = "";
  double bng_easting = unicsv_unknown;
  double bng_northing = unicsv_unknown;
  double swiss_easting = unicsv_unknown;
  double swiss_northing = unicsv_unknown;
  int checked = 0;
  time_t date = -1;
  time_t time = -1;
  int usec = -1;
  char is_localtime = 0;
  garmin_fs_t* gmsd;
  double d;
  struct tm ymd;
  int src_datum = unicsv_datum_idx;
  int ns = 1;
  int ew = 1;
  geocache_data* gc_data = nullptr;
  Waypoint* wpt = new Waypoint;
  wpt->latitude = unicsv_unknown;
  wpt->longitude = unicsv_unknown;
  memset(&ymd, 0, sizeof(ymd));

  int column = -1;
  const QStringList values = csv_linesplit(ibuf, unicsv_fieldsep, "\"", 0);
  for (auto value : values) {
    if (++column >= unicsv_fields_tab.size()) {
      break;  /* ignore extra fields on line */
    }

    ibuf = nullptr;

    checked++;
    value = value.trimmed();
    if (value.isEmpty()) {
      continue;  /* skip empty columns */
    }
    switch (unicsv_fields_tab[column]) {

    case fld_time:
    case fld_date:
    case fld_datetime:
      /* switch column type if it looks like an iso time string */
      if (value.contains('T')) {
        unicsv_fields_tab[column] = fld_iso_time;
      }
      break;
    default:
      ;
    }


    switch (unicsv_fields_tab[column]) {

    case fld_latitude:
      human_to_dec(CSTR(value), &wpt->latitude, nullptr, 1);
      wpt->latitude = wpt->latitude * ns;
      break;

    case fld_longitude:
      human_to_dec(CSTR(value), nullptr, &wpt->longitude, 2);
      wpt->longitude = wpt->longitude * ew;
      break;

    case fld_shortname:
      wpt->shortname = value;
      break;

    case fld_description:
      wpt->description = value;
      break;

    case fld_notes:
      wpt->notes = value;
      break;

    case fld_url: {
      wpt->AddUrlLink(value);
    }
    break;

    case fld_altitude:
      if (parse_distance(value, &d, unicsv_altscale, MYNAME)) {
        if (fabs(d) < fabs(unknown_alt)) {
          wpt->altitude = d;
        }
      }
      break;

    case fld_utm_zone:
      utm_zone = value.toInt();
      break;

    case fld_utm_easting:
      utm_easting = value.toDouble();
      break;

    case fld_utm_northing:
      utm_northing = value.toDouble();
      break;

    case fld_utm_zone_char:
      utm_zc = value.at(0).toUpper().toLatin1();
      break;

    case fld_utm:
      parse_coordinates(value, unicsv_datum_idx, grid_utm,
                        &wpt->latitude, &wpt->longitude, MYNAME);
      /* coordinates from parse_coordinates are in WGS84
         don't convert a second time */
      src_datum = DATUM_WGS84;
      break;

    case fld_bng:
      parse_coordinates(value, DATUM_OSGB36, grid_bng,
                        &wpt->latitude, &wpt->longitude, MYNAME);
      /* coordinates from parse_coordinates are in WGS84
         don't convert a second time */
      src_datum = DATUM_WGS84;
      break;

    case fld_bng_zone:
      strncpy(bng_zone, CSTR(value), sizeof(bng_zone) - 1);
      strupper(bng_zone);
      break;

    case fld_bng_northing:
      bng_northing = value.toDouble();
      break;

    case fld_bng_easting:
      bng_easting = value.toDouble();
      break;

    case fld_swiss:
      parse_coordinates(value, DATUM_WGS84, grid_swiss,
                        &wpt->latitude, &wpt->longitude, MYNAME);
      /* coordinates from parse_coordinates are in WGS84
         don't convert a second time */
      src_datum = DATUM_WGS84;
      break;

    case fld_swiss_easting:
      swiss_easting = value.toDouble();
      break;

    case fld_swiss_northing:
      swiss_northing = value.toDouble();
      break;

    case fld_hdop:
      wpt->hdop = value.toDouble();
      if (unicsv_detect) {
        unicsv_data_type = trkdata;
      }
      break;

    case fld_pdop:
      wpt->pdop = value.toDouble();
      if (unicsv_detect) {
        unicsv_data_type = trkdata;
      }
      break;

    case fld_vdop:
      wpt->vdop = value.toDouble();
      if (unicsv_detect) {
        unicsv_data_type = trkdata;
      }
      break;

    case fld_sat:
      wpt->sat = value.toInt();
      if (unicsv_detect) {
        unicsv_data_type = trkdata;
      }
      break;

    case fld_fix:
      if (unicsv_detect) {
        unicsv_data_type = trkdata;
      }
      if (case_ignore_strcmp(value, "none") == 0) {
        wpt->fix = fix_none;
      } else if (case_ignore_strcmp(value, "2d") == 0) {
        wpt->fix = fix_2d;
      } else if (case_ignore_strcmp(value, "3d") == 0) {
        wpt->fix = fix_3d;
      } else if (case_ignore_strcmp(value, "dgps") == 0) {
        wpt->fix = fix_dgps;
      } else if (case_ignore_strcmp(value, "pps") == 0) {
        wpt->fix = fix_pps;
      } else {
        wpt->fix = fix_unknown;
      }
      break;

    case fld_utc_date:
      if ((is_localtime < 2) && (date < 0)) {
        date = unicsv_parse_date(CSTR(value), nullptr);
        is_localtime = 0;
      }
      break;

    case fld_utc_time:
      if ((is_localtime < 2) && (time < 0)) {
        time = unicsv_parse_time(value, &usec, &date);
        is_localtime = 0;
      }
      break;

    case fld_speed:
      if (parse_speed(value, &d, 1.0, MYNAME)) {
        WAYPT_SET(wpt, speed, d);
        if (unicsv_detect) {
          unicsv_data_type = trkdata;
        }
      }
      break;

    case fld_course:
      WAYPT_SET(wpt, course, value.toDouble());
      if (unicsv_detect) {
        unicsv_data_type = trkdata;
      }
      break;

    case fld_temperature:
      d = value.toDouble();
      if (fabs(d) < 999999) {
        WAYPT_SET(wpt, temperature, d);
      }
      break;

    case fld_temperature_f:
      d = value.toDouble();
      if (fabs(d) < 999999) {
        WAYPT_SET(wpt, temperature, FAHRENHEIT_TO_CELSIUS(d));
      }
      break;

    case fld_heartrate:
      wpt->heartrate = value.toInt();
      if (unicsv_detect) {
        unicsv_data_type = trkdata;
      }
      break;

    case fld_cadence:
      wpt->cadence = value.toInt();
      if (unicsv_detect) {
        unicsv_data_type = trkdata;
      }
      break;

    case fld_power:
      wpt->power = value.toDouble();
      if (unicsv_detect) {
        unicsv_data_type = trkdata;
      }
      break;

    case fld_proximity:
      if (parse_distance(value, &d, unicsv_proximityscale, MYNAME)) {
        WAYPT_SET(wpt, proximity, d);
      }
      break;

    case fld_depth:
      if (parse_distance(value, &d, unicsv_depthscale, MYNAME)) {
        WAYPT_SET(wpt, depth, d);
      }
      break;

    case fld_symbol:
      wpt->icon_descr = value;
      break;

    case fld_iso_time:
      is_localtime = 2;	/* fix result */
      wpt->SetCreationTime(xml_parse_time(value));
      break;

    case fld_time:
      if ((is_localtime < 2) && (time < 0)) {
        time = unicsv_parse_time(value, &usec, &date);
        is_localtime = 1;
      }
      break;

    case fld_date:
      if ((is_localtime < 2) && (date < 0)) {
        date = unicsv_parse_date(CSTR(value), nullptr);
        is_localtime = 1;
      }
      break;

    case fld_year:
      ymd.tm_year = value.toInt();
      break;

    case fld_month:
      ymd.tm_mon = value.toInt();
      break;

    case fld_day:
      ymd.tm_mday = value.toInt();
      break;

    case fld_hour:
      ymd.tm_hour = value.toInt();
      break;

    case fld_min:
      ymd.tm_min = value.toInt();
      break;

    case fld_sec:
      ymd.tm_sec = value.toInt();
      break;

    case fld_datetime:
      if ((is_localtime < 2) && (date < 0) && (time < 0)) {
        time = unicsv_parse_time(value, &usec, &date);
        is_localtime = 1;
      }
      break;

    case fld_ns:
      ns = value.startsWith('n', Qt::CaseInsensitive) ? 1 : -1;
      wpt->latitude *= ns;
      break;

    case fld_ew:
      ew = value.startsWith('e', Qt::CaseInsensitive) ? 1 : -1;
      wpt->longitude *= ew;
      break;

    case fld_garmin_city:
    case fld_garmin_postal_code:
    case fld_garmin_state:
    case fld_garmin_country:
    case fld_garmin_addr:
    case fld_garmin_phone_nr:
    case fld_garmin_phone_nr2:
    case fld_garmin_fax_nr:
    case fld_garmin_email:
    case fld_garmin_facility:
      gmsd = GMSD_FIND(wpt);
      if (! gmsd) {
        gmsd = garmin_fs_alloc(-1);
        fs_chain_add(&wpt->fs, (format_specific_data*) gmsd);
      }
      switch (unicsv_fields_tab[column]) {
      case fld_garmin_city:
        GMSD_SETQSTR(city, value);
        break;
      case fld_garmin_postal_code:
        GMSD_SETQSTR(postal_code, value);
        break;
      case fld_garmin_state:
        GMSD_SETQSTR(state, value);
        break;
      case fld_garmin_country:
        GMSD_SETQSTR(country, value);
        break;
      case fld_garmin_addr:
        GMSD_SETQSTR(addr, value);
        break;
      case fld_garmin_phone_nr:
        GMSD_SETQSTR(phone_nr, value);
        break;
      case fld_garmin_phone_nr2:
        GMSD_SETQSTR(phone_nr2, value);
        break;
      case fld_garmin_fax_nr:
        GMSD_SETQSTR(fax_nr, value);
        break;
      case fld_garmin_email:
        GMSD_SETQSTR(email, value);
        break;
      case fld_garmin_facility:
        GMSD_SETQSTR(facility, value);
        break;
      default:
        break;
      }
      break;
    case fld_gc_id:
    case fld_gc_type:
    case fld_gc_container:
    case fld_gc_terr:
    case fld_gc_diff:
    case fld_gc_is_archived:
    case fld_gc_is_available:
    case fld_gc_exported:
    case fld_gc_last_found:
    case fld_gc_placer:
    case fld_gc_placer_id:
    case fld_gc_hint:

      gc_data = wpt->AllocGCData();

      switch (unicsv_fields_tab[column]) {

      case fld_gc_id:
        gc_data->id = value.toInt();
        if (gc_data->id == 0) {
          gc_data->id = unicsv_parse_gc_id(value);
        }
        break;
      case fld_gc_type:
        gc_data->type = gs_mktype(value);
        break;
      case fld_gc_container:
        gc_data->container = gs_mkcont(value);
        break;
      case fld_gc_terr:
        gc_data->terr = value.toDouble() * 10;
        break;
      case fld_gc_diff:
        gc_data->diff = value.toDouble() * 10;
        break;
      case fld_gc_is_archived:
        gc_data->is_archived = unicsv_parse_status(value);
        break;
      case fld_gc_is_available:
        gc_data->is_available = unicsv_parse_status(value);
        break;
      case fld_gc_exported: {
        time_t time, date;
        int usec;
        time = unicsv_parse_time(value, &usec, &date);
        if (date || time) {
          gc_data->exported = unicsv_adjust_time(time, &date);
        }
      }
      break;
      case fld_gc_last_found: {
        time_t time, date;
        int usec;
        time = unicsv_parse_time(value, &usec, &date);
        if (date || time) {
          gc_data->last_found = unicsv_adjust_time(time, &date);
        }
      }
      break;
      case fld_gc_placer:
        gc_data->placer = value;
        break;
      case fld_gc_placer_id:
        gc_data->placer_id = value.toInt();
        break;
      case fld_gc_hint:
        gc_data->hint = value;
        break;

      default:
        break;
      }
      break;
    case fld_terminator: /* dummy */
      checked--;
      break;
    }
  }

  if (checked == 0) {
    delete wpt;
    return;
  }

  if (is_localtime < 2) {	/* not fixed */
    if ((time >= 0) && (date >= 0)) {
      time_t t = date + time;

      if (is_localtime) {
        struct tm tm;
        tm = *gmtime(&t);
        if (opt_utc) {
          wpt->SetCreationTime(mkgmtime(&tm));
        } else {
          wpt->SetCreationTime(mklocaltime(&tm));
        }
      } else {
        wpt->SetCreationTime(t);
      }
    } else if (time >= 0) {
      wpt->SetCreationTime(time);
    } else if (date >= 0) {
      wpt->SetCreationTime(date);
    } else if (ymd.tm_year || ymd.tm_mon || ymd.tm_mday) {
      if (ymd.tm_year < 100) {
        if (ymd.tm_year <= 70) {
          ymd.tm_year += 2000;
        } else {
          ymd.tm_year += 1900;
        }
      }
      ymd.tm_year -= 1900;

      if (ymd.tm_mon == 0) {
        ymd.tm_mon = 1;
      }
      if (ymd.tm_mday == 0) {
        ymd.tm_mday = 1;
      }

      ymd.tm_mon--;
      if (opt_utc) {
        wpt->SetCreationTime(mkgmtime(&ymd));
      } else {
        wpt->SetCreationTime(mklocaltime(&ymd));
      }
    } else if (ymd.tm_hour || ymd.tm_min || ymd.tm_sec) {
      if (opt_utc) {
        wpt->SetCreationTime(mkgmtime(&ymd));
      } else {
        wpt->SetCreationTime(mklocaltime(&ymd));
      }
    }

    if (usec >= 0) {
      wpt->creation_time = wpt->creation_time.addMSecs(MICRO_TO_MILLI(usec));
    }

    if (opt_utc) {
      wpt->creation_time += atoi(opt_utc) * SECONDS_PER_HOUR;
    }
  }

  /* utm/bng/swiss can be optional */

  if ((wpt->latitude == unicsv_unknown) && (wpt->longitude == unicsv_unknown)) {
    if (utm_zone != -9999) {
      GPS_Math_UTM_EN_To_Known_Datum(&wpt->latitude, &wpt->longitude,
                                     utm_easting, utm_northing, utm_zone, utm_zc, unicsv_datum_idx);
    } else if ((bng_easting != unicsv_unknown) && (bng_northing != unicsv_unknown)) {
      if (bng_zone[0] == '\0') { // OS easting northing
        // Grid references may also be quoted as a pair of numbers: eastings then northings in metres, measured from the southwest corner of the SV square.
        double bnge;
        double bngn;
        char bngz[3];
        if (! GPS_Math_EN_To_UKOSNG_Map(
              bng_easting, bng_northing,
              &bnge, &bngn, bngz)) {
          fatal(MYNAME ": Unable to convert BNG coordinates (%.f %.f)!\n",
                bng_easting, bng_northing);
        }
        if (! GPS_Math_UKOSMap_To_WGS84_M(
              bngz, bnge, bngn,
              &wpt->latitude, &wpt->longitude))
          fatal(MYNAME ": Unable to convert BNG coordinates (%s %.f %.f)!\n",
                bngz, bnge, bngn);
      } else { // traditional zone easting northing
        if (! GPS_Math_UKOSMap_To_WGS84_M(
              bng_zone, bng_easting, bng_northing,
              &wpt->latitude, &wpt->longitude))
          fatal(MYNAME ": Unable to convert BNG coordinates (%s %.f %.f)!\n",
                bng_zone, bng_easting, bng_northing);
      }
      src_datum = DATUM_WGS84;	/* don't convert afterwards */
    } else if ((swiss_easting != unicsv_unknown) && (swiss_northing != unicsv_unknown)) {
      GPS_Math_Swiss_EN_To_WGS84(swiss_easting, swiss_northing,
                                 &wpt->latitude, &wpt->longitude);
      src_datum = DATUM_WGS84;	/* don't convert afterwards */
    }
  }

  if ((src_datum != DATUM_WGS84) &&
      (wpt->latitude != unicsv_unknown) && (wpt->longitude != unicsv_unknown)) {
    double alt;
    GPS_Math_Known_Datum_To_WGS84_M(wpt->latitude, wpt->longitude, 0.0,
                                    &wpt->latitude, &wpt->longitude, &alt, src_datum);
  }

  switch (unicsv_data_type) {
  case rtedata:
    if (! unicsv_route) {
      unicsv_route = route_head_alloc();
      route_add_head(unicsv_route);
    }
    route_add_wpt(unicsv_route, wpt);
    break;
  case trkdata:
    if (! unicsv_track) {
      unicsv_track = route_head_alloc();
      track_add_head(unicsv_track);
    }
    track_add_wpt(unicsv_track, wpt);
    break;
  default:
    waypt_add(wpt);
  }
}

static void
unicsv_rd()
{
  char* buff;

  if (unicsv_fieldsep == nullptr) {
    return;
  }

  while ((buff = gbfgetstr(fin))) {
    buff = lrtrim(buff);
    if ((*buff == '\0') || (*buff == '#')) {
      continue;
    }
    unicsv_parse_one_line(buff);
  }
}

/* =========================================================================== */

static void
unicsv_fatal_outside(const Waypoint* wpt)
{
  gbfprintf(fout, "#####\n");
  fatal(MYNAME ": %s (%s) is outside of convertable area of grid \"%s\"!\n",
        wpt->shortname.isEmpty() ? "Waypoint" : qPrintable(wpt->shortname),
        pretty_deg_format(wpt->latitude, wpt->longitude, 'd', nullptr, 0),
        gt_get_mps_grid_longname(unicsv_grid_idx, MYNAME));
}

static void
unicsv_print_str(const QString& s)
{
  gbfputs(unicsv_fieldsep, fout);
  QString t;
  if (!s.isEmpty()) {
    t = strenquote(s, UNICSV_QUOT_CHAR);
    // I'm not sure these three replacements are necessary; they're just a
    // slavish re-implementation of (what I think) the original C code
    // was doing.
    t.replace("\r\n", ",");
    t.replace("\r", ",");
    t.replace("\n", ",");
  }
  gbfputs(t.trimmed(), fout);
}

static void
unicsv_print_data_time(const QDateTime& idt)
{
  if (!idt.isValid()) {
    return;
  }
  QDateTime dt = idt;
  if (opt_utc) {
    //time += atoi(opt_utc) * SECONDS_PER_HOUR;
    dt = dt.addSecs(atoi(opt_utc) * SECONDS_PER_HOUR);
    dt = dt.toUTC();
  }

  unicsv_print_str(dt.toString("yyyy/MM/dd hh:mm:ss"));
}

#define FIELD_USED(a) (gb_getbit(&unicsv_outp_flags, a))

static void
unicsv_waypt_enum_cb(const Waypoint* wpt)
{
  const QString& shortname = wpt->shortname;
  garmin_fs_t* gmsd = GMSD_FIND(wpt);

  if (!shortname.isEmpty()) {
    gb_setbit(&unicsv_outp_flags, fld_shortname);
  }
  if (wpt->altitude != unknown_alt) {
    gb_setbit(&unicsv_outp_flags, fld_altitude);
  }
  if (!wpt->icon_descr.isNull()) {
    gb_setbit(&unicsv_outp_flags, fld_symbol);
  }
  if (!wpt->description.isEmpty() && shortname != wpt->description) {
    gb_setbit(&unicsv_outp_flags, fld_description);
  }
  if (!wpt->notes.isEmpty() && shortname != wpt->notes) {
    if ((wpt->description.isEmpty()) || (wpt->description != wpt->notes)) {
      gb_setbit(&unicsv_outp_flags, fld_notes);
    }
  }
  if (wpt->HasUrlLink()) {
    gb_setbit(&unicsv_outp_flags, fld_url);
  }
  if (wpt->creation_time.isValid()) {
    gb_setbit(&unicsv_outp_flags, fld_time);
    if (wpt->creation_time.toTime_t() >= SECONDS_PER_DAY) {
      gb_setbit(&unicsv_outp_flags, fld_date);
    }
  }

  if (wpt->fix != fix_unknown) {
    gb_setbit(&unicsv_outp_flags, fld_fix);
  }
  if (wpt->vdop > 0) {
    gb_setbit(&unicsv_outp_flags, fld_vdop);
  }
  if (wpt->hdop > 0) {
    gb_setbit(&unicsv_outp_flags, fld_hdop);
  }
  if (wpt->pdop > 0) {
    gb_setbit(&unicsv_outp_flags, fld_pdop);
  }
  if (wpt->sat > 0) {
    gb_setbit(&unicsv_outp_flags, fld_sat);
  }
  if (wpt->heartrate != 0) {
    gb_setbit(&unicsv_outp_flags, fld_heartrate);
  }
  if (wpt->cadence != 0) {
    gb_setbit(&unicsv_outp_flags, fld_cadence);
  }
  if (wpt->power > 0) {
    gb_setbit(&unicsv_outp_flags, fld_power);
  }

  /* "flagged" waypoint members */
  if WAYPT_HAS(wpt, course) {
    gb_setbit(&unicsv_outp_flags, fld_course);
  }
  if WAYPT_HAS(wpt, depth) {
    gb_setbit(&unicsv_outp_flags, fld_depth);
  }
  if WAYPT_HAS(wpt, speed) {
    gb_setbit(&unicsv_outp_flags, fld_speed);
  }
  if WAYPT_HAS(wpt, proximity) {
    gb_setbit(&unicsv_outp_flags, fld_proximity);
  }
  if WAYPT_HAS(wpt, temperature) {
    gb_setbit(&unicsv_outp_flags, fld_temperature);
  }

  if (gmsd) {
    if GMSD_HAS(addr) {
      gb_setbit(&unicsv_outp_flags, fld_garmin_addr);
    }
    if GMSD_HAS(city) {
      gb_setbit(&unicsv_outp_flags, fld_garmin_city);
    }
    if GMSD_HAS(country) {
      gb_setbit(&unicsv_outp_flags, fld_garmin_country);
    }
    if GMSD_HAS(phone_nr) {
      gb_setbit(&unicsv_outp_flags, fld_garmin_phone_nr);
    }
    if GMSD_HAS(phone_nr2) {
      gb_setbit(&unicsv_outp_flags, fld_garmin_phone_nr2);
    }
    if GMSD_HAS(fax_nr) {
      gb_setbit(&unicsv_outp_flags, fld_garmin_fax_nr);
    }
    if GMSD_HAS(email) {
      gb_setbit(&unicsv_outp_flags, fld_garmin_email);
    }
    if GMSD_HAS(postal_code) {
      gb_setbit(&unicsv_outp_flags, fld_garmin_postal_code);
    }
    if GMSD_HAS(state) {
      gb_setbit(&unicsv_outp_flags, fld_garmin_state);
    }
    if GMSD_HAS(facility) {
      gb_setbit(&unicsv_outp_flags, fld_garmin_facility);
    }
  }

  if (! wpt->EmptyGCData()) {
    const geocache_data* gc_data = wpt->gc_data;

    if (gc_data->id) {
      gb_setbit(&unicsv_outp_flags, fld_gc_id);
    }
    if (gc_data->type) {
      gb_setbit(&unicsv_outp_flags, fld_gc_type);
    }
    if (gc_data->container) {
      gb_setbit(&unicsv_outp_flags, fld_gc_container);
    }
    if (gc_data->terr) {
      gb_setbit(&unicsv_outp_flags, fld_gc_terr);
    }
    if (gc_data->diff) {
      gb_setbit(&unicsv_outp_flags, fld_gc_diff);
    }
    if (gc_data->is_archived) {
      gb_setbit(&unicsv_outp_flags, fld_gc_is_archived);
    }
    if (gc_data->is_available) {
      gb_setbit(&unicsv_outp_flags, fld_gc_is_available);
    }
    if (gc_data->exported.isValid()) {
      gb_setbit(&unicsv_outp_flags, fld_gc_exported);
    }
    if (gc_data->last_found.isValid()) {
      gb_setbit(&unicsv_outp_flags, fld_gc_last_found);
    }
    if (!gc_data->placer.isEmpty()) {
      gb_setbit(&unicsv_outp_flags, fld_gc_placer);
    }
    if (gc_data->placer_id) {
      gb_setbit(&unicsv_outp_flags, fld_gc_placer_id);
    }
    if (!gc_data->hint.isEmpty()) {
      gb_setbit(&unicsv_outp_flags, fld_gc_hint);
    }
  }
}

static void
unicsv_waypt_disp_cb(const Waypoint* wpt)
{
  double lat, lon, alt;
  char* cout = nullptr;
  const geocache_data* gc_data = nullptr;
  unicsv_waypt_ct++;

  QString shortname = wpt->shortname;
  garmin_fs_t* gmsd = GMSD_FIND(wpt);

  if (unicsv_datum_idx == DATUM_WGS84) {
    lat = wpt->latitude;
    lon = wpt->longitude;
    alt = wpt->altitude;
  } else {
    GPS_Math_WGS84_To_Known_Datum_M(wpt->latitude, wpt->longitude, 0.0,
                                    &lat, &lon, &alt, unicsv_datum_idx);
  }

  gbfprintf(fout, "%d%s", unicsv_waypt_ct, unicsv_fieldsep);

  switch (unicsv_grid_idx) {

  case grid_lat_lon_ddd:
    cout = pretty_deg_format(lat, lon, 'd', unicsv_fieldsep, 0);
    gbfputs(cout, fout);
    break;

  case grid_lat_lon_dmm:
    cout = pretty_deg_format(lat, lon, 'm', unicsv_fieldsep, 0);
    gbfputs(cout, fout);
    break;

  case grid_lat_lon_dms: {
    cout = pretty_deg_format(lat, lon, 's', unicsv_fieldsep, 0);
    char* sep = strchr(cout, ',');
    *sep = '\0';
    QString tmp = strenquote(cout, UNICSV_QUOT_CHAR);
    gbfprintf(fout, "%s%s", CSTR(tmp), unicsv_fieldsep);
    tmp = strenquote(sep+1, UNICSV_QUOT_CHAR);
    gbfputs(tmp, fout);
  }
  break;

  case grid_bng: {
    char map[3];
    double north, east;

    if (! GPS_Math_WGS84_To_UKOSMap_M(wpt->latitude, wpt->longitude, &east, &north, map)) {
      unicsv_fatal_outside(wpt);
    }
    gbfprintf(fout, "%s%s%5.0f%s%5.0f",
              map, unicsv_fieldsep,
              east, unicsv_fieldsep,
              north);
    break;
  }
  case grid_utm: {
    int zone;
    char zonec;
    double north, east;

    if (! GPS_Math_Known_Datum_To_UTM_EN(lat, lon,
                                         &east, &north, &zone, &zonec, unicsv_datum_idx)) {
      unicsv_fatal_outside(wpt);
    }
    gbfprintf(fout, "%02d%s%c%s%.0f%s%.0f",
              zone, unicsv_fieldsep,
              zonec, unicsv_fieldsep,
              east, unicsv_fieldsep,
              north);
    break;
  }
  case grid_swiss: {
    double north, east;

    if (! GPS_Math_WGS84_To_Swiss_EN(wpt->latitude, wpt->longitude, &east, &north)) {
      unicsv_fatal_outside(wpt);
    }
    gbfprintf(fout, "%.f%s%.f",
              east, unicsv_fieldsep, north);
    break;

  }
  default:
    gbfprintf(fout, "%.*f%s%.*f", llprec, lat, unicsv_fieldsep, llprec, lon);
    break;
  }

  if (cout) {
    xfree(cout);
  }

  if FIELD_USED(fld_shortname) {
    unicsv_print_str(shortname);
  }
  if FIELD_USED(fld_altitude) {
    if (wpt->altitude != unknown_alt) {
      gbfprintf(fout, "%s%.1f", unicsv_fieldsep, wpt->altitude);
    } else {
      gbfputs(unicsv_fieldsep, fout);
    }
  }
  if FIELD_USED(fld_description) {
    unicsv_print_str(wpt->description);
  }
  if FIELD_USED(fld_notes) {
    unicsv_print_str(wpt->notes);
  }
  if FIELD_USED(fld_symbol) {
    unicsv_print_str(wpt->icon_descr.isNull() ? "Waypoint" : wpt->icon_descr);
  }
  if FIELD_USED(fld_depth) {
    if WAYPT_HAS(wpt, depth) {
      gbfprintf(fout, "%s%.3f", unicsv_fieldsep, wpt->depth);
    } else {
      gbfputs(unicsv_fieldsep, fout);
    }
  }
  if FIELD_USED(fld_proximity) {
    if WAYPT_HAS(wpt, proximity) {
      gbfprintf(fout, "%s%.f", unicsv_fieldsep, wpt->proximity);
    } else {
      gbfputs(unicsv_fieldsep, fout);
    }
  }
  if FIELD_USED(fld_temperature) {
    if WAYPT_HAS(wpt, temperature) {
      gbfprintf(fout, "%s%.3f", unicsv_fieldsep, wpt->temperature);
    } else {
      gbfputs(unicsv_fieldsep, fout);
    }
  }
  if FIELD_USED(fld_speed) {
    if WAYPT_HAS(wpt, speed) {
      gbfprintf(fout, "%s%.2f", unicsv_fieldsep, wpt->speed);
    } else {
      gbfputs(unicsv_fieldsep, fout);
    }
  }
  if FIELD_USED(fld_course) {
    if WAYPT_HAS(wpt, course) {
      gbfprintf(fout, "%s%.1f", unicsv_fieldsep, wpt->course);
    } else {
      gbfputs(unicsv_fieldsep, fout);
    }
  }
  if FIELD_USED(fld_fix) {
    const char* fix;
    switch (wpt->fix) {
    case fix_none:
      fix = "none";
      break;
    case fix_2d:
      fix = "2d";
      break;
    case fix_3d:
      fix = "3d";
      break;
    case fix_dgps:
      fix = "dgps";
      break;
    case fix_pps:
      fix = "pps";
      break;
    default:
      fix = nullptr;
    }
    if (fix) {
      unicsv_print_str(fix);
    } else {
      gbfputs(unicsv_fieldsep, fout);
    }
  }
  if FIELD_USED(fld_hdop) {
    if (wpt->hdop > 0) {
      gbfprintf(fout, "%s%.2f", unicsv_fieldsep, wpt->hdop);
    } else {
      gbfputs(unicsv_fieldsep, fout);
    }
  }
  if FIELD_USED(fld_vdop) {
    if (wpt->vdop > 0) {
      gbfprintf(fout, "%s%.2f", unicsv_fieldsep, wpt->vdop);
    } else {
      gbfputs(unicsv_fieldsep, fout);
    }
  }
  if FIELD_USED(fld_pdop) {
    if (wpt->pdop > 0) {
      gbfprintf(fout, "%s%.2f", unicsv_fieldsep, wpt->pdop);
    } else {
      gbfputs(unicsv_fieldsep, fout);
    }
  }
  if FIELD_USED(fld_sat) {
    if (wpt->sat > 0) {
      gbfprintf(fout, "%s%d", unicsv_fieldsep, wpt->sat);
    } else {
      gbfputs(unicsv_fieldsep, fout);
    }
  }
  if FIELD_USED(fld_heartrate) {
    if (wpt->heartrate != 0) {
      gbfprintf(fout, "%s%u", unicsv_fieldsep, wpt->heartrate);
    } else {
      gbfputs(unicsv_fieldsep, fout);
    }
  }
  if FIELD_USED(fld_cadence) {
    if (wpt->cadence != 0) {
      gbfprintf(fout, "%s%u", unicsv_fieldsep, wpt->cadence);
    } else {
      gbfputs(unicsv_fieldsep, fout);
    }
  }
  if FIELD_USED(fld_power) {
    if (wpt->power > 0) {
      gbfprintf(fout, "%s%.1f", unicsv_fieldsep, wpt->power);
    } else {
      gbfputs(unicsv_fieldsep, fout);
    }
  }
  if FIELD_USED(fld_date) {
    if (wpt->creation_time.toTime_t() >= SECONDS_PER_DAY) {
      QDateTime dt;
      if (opt_utc) {
        dt = wpt->GetCreationTime().toUTC();
        // We might wrap to a different day by overriding the TZ offset.
        dt = dt.addSecs(atoi(opt_utc) * SECONDS_PER_HOUR);
      } else {
        dt = wpt->GetCreationTime();
      }
      QString date = dt.toString("yyyy/MM/dd");
      gbfputs(unicsv_fieldsep, fout);
      gbfputs(date, fout);
    } else {
      gbfputs(unicsv_fieldsep, fout);
    }
  }
  if FIELD_USED(fld_time) {
    if (wpt->creation_time.isValid()) {
      QTime t;
      if (opt_utc) {
        t = wpt->GetCreationTime().toUTC().time();
        t = t.addSecs(atoi(opt_utc) * SECONDS_PER_HOUR);
      } else {
        t = wpt->GetCreationTime().time();
      }
      QString out;
      if (t.msec() > 0) {
        out = t.toString("hh:mm:ss.zzz");
      } else {
        out = t.toString("hh:mm:ss");
      }
      gbfputs(unicsv_fieldsep, fout);
      gbfputs(out, fout);
    } else {
      gbfputs(unicsv_fieldsep, fout);
    }
  }
  if (FIELD_USED(fld_url)) {
    if (!wpt->HasUrlLink()) {
      unicsv_print_str("");
    } else {
      UrlLink l = wpt->GetUrlLink();
      unicsv_print_str(l.url_);
    }
  }

  if FIELD_USED(fld_garmin_facility) {
    unicsv_print_str(GMSD_GET(facility, NULL));
  }
  if FIELD_USED(fld_garmin_addr) {
    unicsv_print_str(GMSD_GET(addr, NULL));
  }
  if FIELD_USED(fld_garmin_city) {
    unicsv_print_str(GMSD_GET(city, NULL));
  }
  if FIELD_USED(fld_garmin_postal_code) {
    unicsv_print_str(GMSD_GET(postal_code, NULL));
  }
  if FIELD_USED(fld_garmin_state) {
    unicsv_print_str(GMSD_GET(state, NULL));
  }
  if FIELD_USED(fld_garmin_country) {
    unicsv_print_str(GMSD_GET(country, NULL));
  }
  if FIELD_USED(fld_garmin_phone_nr) {
    unicsv_print_str(GMSD_GET(phone_nr, NULL));
  }
  if FIELD_USED(fld_garmin_phone_nr2) {
    unicsv_print_str(GMSD_GET(phone_nr2, NULL));
  }
  if FIELD_USED(fld_garmin_fax_nr) {
    unicsv_print_str(GMSD_GET(fax_nr, NULL));
  }
  if FIELD_USED(fld_garmin_email) {
    unicsv_print_str(GMSD_GET(email, NULL));
  }

  if (wpt->EmptyGCData()) {
    gc_data = nullptr;
  } else {
    gc_data = wpt->gc_data;
  }

  if FIELD_USED(fld_gc_id) {
    gbfputs(unicsv_fieldsep, fout);
    if (gc_data && gc_data->id) {
      gbfprintf(fout, "%d", gc_data->id);
    }
  }
  if FIELD_USED(fld_gc_type) {
    if (gc_data) {
      unicsv_print_str(gs_get_cachetype(gc_data->type));
    } else {
      gbfputs(unicsv_fieldsep, fout);
    }
  }
  if FIELD_USED(fld_gc_container) {
    if (gc_data) {
      unicsv_print_str(gs_get_container(gc_data->container));
    } else {
      gbfputs(unicsv_fieldsep, fout);
    }
  }
  if FIELD_USED(fld_gc_terr) {
    gbfputs(unicsv_fieldsep, fout);
    if (gc_data && gc_data->terr) {
      gbfprintf(fout, "%.1f", (double)gc_data->terr / 10);
    }
  }
  if FIELD_USED(fld_gc_diff) {
    gbfputs(unicsv_fieldsep, fout);
    if (gc_data && gc_data->diff) {
      gbfprintf(fout, "%.1f", (double)gc_data->diff / 10);
    }
  }
  if FIELD_USED(fld_gc_is_archived) {
    if (gc_data && gc_data->is_archived) {
      unicsv_print_str((gc_data->is_archived == status_true) ? "True" : "False");
    } else {
      gbfputs(unicsv_fieldsep, fout);
    }
  }
  if FIELD_USED(fld_gc_is_available) {
    if (gc_data && gc_data->is_available) {
      unicsv_print_str((gc_data->is_available == status_true) ? "True" : "False");
    } else {
      gbfputs(unicsv_fieldsep, fout);
    }
  }
  if FIELD_USED(fld_gc_exported) {
    if (gc_data) {
      unicsv_print_data_time(gc_data->exported);
    } else {
      gbfputs(unicsv_fieldsep, fout);
    }
  }
  if FIELD_USED(fld_gc_last_found) {
    if (gc_data) {
      unicsv_print_data_time(gc_data->last_found);
    } else {
      gbfputs(unicsv_fieldsep, fout);
    }
  }
  if FIELD_USED(fld_gc_placer) {
    if (gc_data) {
      unicsv_print_str(gc_data->placer);
    } else {
      gbfputs(unicsv_fieldsep, fout);
    }
  }
  if FIELD_USED(fld_gc_placer_id) {
    gbfputs(unicsv_fieldsep, fout);
    if (gc_data && gc_data->placer_id) {
      gbfprintf(fout, "%d", gc_data->placer_id);
    }
  }
  if FIELD_USED(fld_gc_hint) {
    if (gc_data) {
      unicsv_print_str(gc_data->hint);
    } else {
      gbfputs(unicsv_fieldsep, fout);
    }
  }
  if (opt_format) {
    unicsv_print_str(wpt->session->name);
  }
  if (opt_filename) {
    unicsv_print_str(wpt->session->filename);
  }

  gbfputs(UNICSV_LINE_SEP, fout);
}

/* --------------------------------------------------------------------------- */


static void
unicsv_wr_init(const QString& filename)
{
  fout = gbfopen(filename, "wb", MYNAME);

  memset(&unicsv_outp_flags, 0, sizeof(unicsv_outp_flags));
  unicsv_grid_idx = grid_unknown;
  unicsv_datum_idx = DATUM_WGS84;
  unicsv_fieldsep = UNICSV_FIELD_SEP;
  unicsv_waypt_ct = 0;

  if (opt_grid != nullptr) {
    int i;

    if (sscanf(opt_grid, "%d", &i)) {
      unicsv_grid_idx = (grid_type) i;
      if ((unicsv_grid_idx < GRID_INDEX_MIN) || (unicsv_grid_idx > GRID_INDEX_MAX))
        fatal(MYNAME ": Grid index out of range (%d..%d)!\n",
              (int)GRID_INDEX_MIN, (int)GRID_INDEX_MAX);
    } else {
      unicsv_grid_idx = gt_lookup_grid_type(opt_grid, MYNAME);
    }
  }

  if (unicsv_grid_idx == grid_bng)
    /* force datum to "Ord Srvy Grt Britn" / OSGB36 */
    /* ! ignore parameter "Datum" ! */
  {
    unicsv_datum_idx = DATUM_OSGB36;
  } else if (unicsv_grid_idx == grid_swiss)
    /* ! ignore parameter "Datum" ! */
  {
    unicsv_datum_idx = DATUM_WGS84;  /* internal, becomes CH1903 */
  } else {
    unicsv_datum_idx = gt_lookup_datum_index(opt_datum, MYNAME);
  }

  llprec = atoi(opt_prec);
}

static void
unicsv_wr_deinit()
{
  gbfclose(fout);
}

// Waypoints are default-on and there's no way to turn them off. This is
// used to see if a user specified (-t OR -r) in addition to the default
// of -w.  It's pretty weak, but it's better than letting the last flag
// 'win' which can result in no data silently being displayed.

static void
unicsv_check_modes(bool test)
{
  if (test) {
    Fatal() << MYNAME <<
            " : Invalid combination of -w, -t, -r selected. Use only one.";
  }
}


static void
unicsv_wr()
{
  switch (global_opts.objective) {
  case wptdata:
  case unknown_gpsdata:
    unicsv_check_modes(doing_rtes || doing_trks);
    waypt_disp_all(unicsv_waypt_enum_cb);
    break;
  case trkdata:
    unicsv_check_modes(doing_rtes);
    track_disp_all(nullptr, nullptr, unicsv_waypt_enum_cb);
    break;
  case rtedata:
    unicsv_check_modes(doing_trks);
    route_disp_all(nullptr, nullptr, unicsv_waypt_enum_cb);
    break;
  case posndata:
    Fatal() << MYNAME << ": Realtime positioning not supported.";
  }

  gbfprintf(fout, "No%s", unicsv_fieldsep);

  switch (unicsv_grid_idx) {
  case grid_bng:
    /*		indexed parameters doesn't work under __win32__ (mingw)
    		gbfprintf(fout, "BNG-Zone%1$sBNG-East%1$sBNG-North", unicsv_fieldsep);
    */
    gbfprintf(fout, "BNG-Zone%sBNG-East%sBNG-North",
              unicsv_fieldsep, unicsv_fieldsep);
    break;
  case grid_utm:
    /*		indexed parameters doesn't work under __win32__ (mingw)
    		gbfprintf(fout, "BNG-Zone%1$sBNG-East%1$sBNG-North", unicsv_fieldsep);
    */
    gbfprintf(fout, "UTM-Zone%sUTM-Ch%sUTM-East%sUTM-North",
              unicsv_fieldsep, unicsv_fieldsep, unicsv_fieldsep);
    break;
  case grid_swiss:
    gbfprintf(fout, "Swiss-East%sSwiss-North",
              unicsv_fieldsep);
    break;
  default:
    gbfprintf(fout, "Latitude%sLongitude", unicsv_fieldsep);
  }

  if FIELD_USED(fld_shortname) {
    gbfprintf(fout, "%sName", unicsv_fieldsep);
  }
  if FIELD_USED(fld_altitude) {
    gbfprintf(fout, "%sAltitude", unicsv_fieldsep);
  }
  if FIELD_USED(fld_description) {
    gbfprintf(fout, "%sDescription", unicsv_fieldsep);
  }
  if FIELD_USED(fld_notes) {
    gbfprintf(fout, "%sNotes", unicsv_fieldsep);
  }
  if FIELD_USED(fld_symbol) {
    gbfprintf(fout, "%sSymbol", unicsv_fieldsep);
  }
  if FIELD_USED(fld_depth) {
    gbfprintf(fout, "%sDepth", unicsv_fieldsep);
  }
  if FIELD_USED(fld_proximity) {
    gbfprintf(fout, "%sProximity", unicsv_fieldsep);
  }
  if FIELD_USED(fld_temperature) {
    gbfprintf(fout, "%sTemperature", unicsv_fieldsep);
  }
  if FIELD_USED(fld_speed) {
    gbfprintf(fout, "%sSpeed", unicsv_fieldsep);
  }
  if FIELD_USED(fld_course) {
    gbfprintf(fout, "%sCourse", unicsv_fieldsep);
  }
  if FIELD_USED(fld_fix) {
    gbfprintf(fout, "%sFIX", unicsv_fieldsep);
  }
  if FIELD_USED(fld_hdop) {
    gbfprintf(fout, "%sHDOP", unicsv_fieldsep);
  }
  if FIELD_USED(fld_vdop) {
    gbfprintf(fout, "%sVDOP", unicsv_fieldsep);
  }
  if FIELD_USED(fld_pdop) {
    gbfprintf(fout, "%sPDOP", unicsv_fieldsep);
  }
  if FIELD_USED(fld_sat) {
    gbfprintf(fout, "%sSatellites", unicsv_fieldsep);
  }
  if FIELD_USED(fld_heartrate) {
    gbfprintf(fout, "%sHeartrate", unicsv_fieldsep);
  }
  if FIELD_USED(fld_cadence) {
    gbfprintf(fout, "%sCadence", unicsv_fieldsep);
  }
  if FIELD_USED(fld_power) {
    gbfprintf(fout, "%sPower", unicsv_fieldsep);
  }
  if FIELD_USED(fld_date) {
    gbfprintf(fout, "%sDate", unicsv_fieldsep);
  }
  if FIELD_USED(fld_time) {
    gbfprintf(fout, "%sTime", unicsv_fieldsep);
  }
  if FIELD_USED(fld_url) {
    gbfprintf(fout, "%sURL", unicsv_fieldsep);
  }

  if FIELD_USED(fld_garmin_facility) {
    gbfprintf(fout, "%sFacility", unicsv_fieldsep);
  }
  if FIELD_USED(fld_garmin_addr) {
    gbfprintf(fout, "%sAddress", unicsv_fieldsep);
  }
  if FIELD_USED(fld_garmin_city) {
    gbfprintf(fout, "%sCity", unicsv_fieldsep);
  }
  if FIELD_USED(fld_garmin_postal_code) {
    gbfprintf(fout, "%sPostalCode", unicsv_fieldsep);
  }
  if FIELD_USED(fld_garmin_state) {
    gbfprintf(fout, "%sState", unicsv_fieldsep);
  }
  if FIELD_USED(fld_garmin_country) {
    gbfprintf(fout, "%sCountry", unicsv_fieldsep);
  }
  if FIELD_USED(fld_garmin_phone_nr) {
    gbfprintf(fout, "%sPhone", unicsv_fieldsep);
  }
  if FIELD_USED(fld_garmin_phone_nr2) {
    gbfprintf(fout, "%sPhone2", unicsv_fieldsep);
  }
  if FIELD_USED(fld_garmin_fax_nr) {
    gbfprintf(fout, "%sFax", unicsv_fieldsep);
  }
  if FIELD_USED(fld_garmin_email) {
    gbfprintf(fout, "%sEmail", unicsv_fieldsep);
  }

  if FIELD_USED(fld_gc_id) {
    gbfprintf(fout, "%sGCID", unicsv_fieldsep);
  }
  if FIELD_USED(fld_gc_type) {
    gbfprintf(fout, "%sType", unicsv_fieldsep);
  }
  if FIELD_USED(fld_gc_container) {
    gbfprintf(fout, "%sContainer", unicsv_fieldsep);
  }
  if FIELD_USED(fld_gc_terr) {
    gbfprintf(fout, "%sTerrain", unicsv_fieldsep);
  }
  if FIELD_USED(fld_gc_diff) {
    gbfprintf(fout, "%sDifficulty", unicsv_fieldsep);
  }
  if FIELD_USED(fld_gc_is_archived) {
    gbfprintf(fout, "%sArchived", unicsv_fieldsep);
  }
  if FIELD_USED(fld_gc_is_available) {
    gbfprintf(fout, "%sAvailable", unicsv_fieldsep);
  }
  if FIELD_USED(fld_gc_exported) {
    gbfprintf(fout, "%sExported", unicsv_fieldsep);
  }
  if FIELD_USED(fld_gc_last_found) {
    gbfprintf(fout, "%sLast Found", unicsv_fieldsep);
  }
  if FIELD_USED(fld_gc_placer) {
    gbfprintf(fout, "%sPlacer", unicsv_fieldsep);
  }
  if FIELD_USED(fld_gc_placer_id) {
    gbfprintf(fout, "%sPlacer ID", unicsv_fieldsep);
  }
  if FIELD_USED(fld_gc_hint) {
    gbfprintf(fout, "%sHint", unicsv_fieldsep);
  }
  if (opt_format) {
    gbfprintf(fout, "%sFormat", unicsv_fieldsep);
  }
  if (opt_filename) {
    gbfprintf(fout, "%sFilename", unicsv_fieldsep);
  }

  gbfputs(UNICSV_LINE_SEP, fout);

  switch (global_opts.objective) {
  case wptdata:
    waypt_disp_all(unicsv_waypt_disp_cb);
    break;
  case trkdata:
    track_disp_all(nullptr, nullptr, unicsv_waypt_disp_cb);
    break;
  case rtedata:
    route_disp_all(nullptr, nullptr, unicsv_waypt_disp_cb);
    break;
  default:
    break;
  }
}

/* --------------------------------------------------------------------------- */

ff_vecs_t unicsv_vecs = {
  ff_type_file,
  FF_CAP_RW_ALL,
  unicsv_rd_init,
  unicsv_wr_init,
  unicsv_rd_deinit,
  unicsv_wr_deinit,
  unicsv_rd,
  unicsv_wr,
  nullptr,
  unicsv_args,
  CET_CHARSET_ASCII, 0	/* can be changed with -c ... */
  , NULL_POS_OPS,
  nullptr
};
