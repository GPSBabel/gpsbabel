/*
    XCSV - X Character Separated Values (.???)

    A hopefully not too feeble attempt at parsing whatever separated values
    files into the waypoint structure and back out again.  This is a config-
    file wrapper around csv_util.c.

    Copyright (C) 2002 Alex Mottram (geo_alexm at cox-internet.com)

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

#include <cctype>                  // for isdigit, tolower
#include <cmath>                   // for fabs, pow
#include <cstdio>                  // for snprintf, sscanf
#include <cstdlib>                 // for atof, atoi, strtod, atol
#include <cstring>                 // for strlen, strncmp, strcmp, strncpy, memset
#include <ctime>                   // for gmtime, localtime, mktime, strftime

#include <QtCore/QByteArray>       // for QByteArray
#include <QtCore/QChar>            // for QChar
#include <QtCore/QCharRef>         // for QCharRef
#include <QtCore/QDate>            // for QDate
#include <QtCore/QDateTime>        // for QDateTime
#include <QtCore/QFile>            // for QFile
#include <QtCore/QHash>            // for QHash
#include <QtCore/QIODevice>        // for operator|, QIODevice::ReadOnly, QIODevice::Text, QIODevice::WriteOnly
#include <QtCore/QList>            // for QList
#include <QtCore/QRegExp>          // for QRegExp
#include <QtCore/QString>          // for QString, operator+, operator==, QByteArray::append
#include <QtCore/QStringList>      // for QStringList
#include <QtCore/QTextCodec>       // for QTextCodec
#include <QtCore/QTextStream>      // for QTextStream
#include <QtCore/QTime>            // for QTime
#include <QtCore/Qt>               // for UTC
#include <QtCore/QtGlobal>         // for qAsConst, QAddConst<>::Type, qPrintable

#include "defs.h"
#include "csv_util.h"              // for csv_stringtrim, dec_to_human, csv_stringclean, human_to_dec, ddmmdir_to_degrees, dec_to_intdeg, decdir_to_dec, intdeg_to_dec, csv_linesplit
#include "garmin_fs.h"             // for garmin_fs_t, garmin_fs_flags_t, GMSD_FIND, GMSD_GET, GMSD_SET, garmin_fs_alloc
#include "gbfile.h"                // for gbfgetstr, gbfclose, gbfopen, gbfile
#include "grtcirc.h"               // for RAD, gcdist, radtomiles
#include "jeeps/gpsmath.h"         // for GPS_Math_WGS84_To_UTM_EN, GPS_Lookup_Datum_Index, GPS_Math_Known_Datum_To_WGS84_M, GPS_Math_UTM_EN_To_Known_Datum, GPS_Math_WGS84_To_Known_Datum_M, GPS_Math_WGS84_To_UKOSMap_M
#include "jeeps/gpsport.h"         // for int32
#include "session.h"               // for session_t
#include "src/core/datetime.h"     // for DateTime
#include "src/core/file.h"         // for File
#include "src/core/logging.h"      // for Warning, Fatal
#include "src/core/optional.h"     // for optional
#include "strptime.h"              // for strptime
#include "xcsv.h"

#define MYNAME	"XCSV"

/* macros */
#define LAT_DIR(a) a < 0.0 ? 'S' : 'N'
#define LON_DIR(a) a < 0.0 ? 'W' : 'E'
#define NONULL(a) a.isNull() ? "" : CSTRc(a)

/* convert excel time (days since 1900) to time_t and back again */
#define EXCEL_TO_TIMET(a) ((a - 25569.0) * 86400.0)
#define TIMET_TO_EXCEL(a) ((a / 86400.0) + 25569.0)

#define GPS_DATUM_WGS84		118


/*
 * Internal numeric value to associate with each keyword in a style file.
 * To add new keywords, just add an entry here, handle it in the switch
 * statements below, add it to xcsv_tokens.in, and rebuild on a system
 * that has GNU gperf on it.
 */
typedef enum {
  XT_unused = 0,
  XT_ALT_FEET,
  XT_ALT_METERS,
  XT_ANYNAME,
  XT_CADENCE,
  XT_CITY,
  XT_CONSTANT,
  XT_COUNTRY,
  XT_DESCRIPTION,
  XT_EXCEL_TIME,
  XT_FACILITY,
  XT_FILENAME,
  XT_FORMAT,
  XT_GEOCACHE_CONTAINER,
  XT_GEOCACHE_DIFF,
  XT_GEOCACHE_HINT,
  XT_GEOCACHE_LAST_FOUND,
  XT_GEOCACHE_PLACER,
  XT_GEOCACHE_TERR,
  XT_GEOCACHE_TYPE,
  XT_GEOCACHE_ISAVAILABLE,
  XT_GEOCACHE_ISARCHIVED,
  XT_GMT_TIME,
  XT_GPS_FIX,
  XT_GPS_HDOP,
  XT_GPS_PDOP,
  XT_GPS_SAT,
  XT_GPS_VDOP,
  XT_HEART_RATE,
  XT_HMSG_TIME,
  XT_HMSL_TIME,
  XT_ICON_DESCR,
  XT_IGNORE,
  XT_INDEX,
  XT_ISO_TIME,
  XT_ISO_TIME_MS,
  XT_LATLON_HUMAN_READABLE,
  XT_LAT_DECIMAL,
  XT_LAT_DECIMALDIR,
  XT_LAT_DIR,
  XT_LAT_DIRDECIMAL,
  XT_LAT_HUMAN_READABLE,
  XT_LAT_INT32DEG,
  XT_LAT_DDMMDIR,
  XT_LAT_NMEA,
  XT_LOCAL_TIME,
  XT_LON_DECIMAL,
  XT_LON_DECIMALDIR,
  XT_LON_DIR,
  XT_LON_DIRDECIMAL,
  XT_LON_HUMAN_READABLE,
  XT_LON_INT32DEG,
  XT_LON_DDMMDIR,
  XT_LON_NMEA,
  XT_MAP_EN_BNG,
  XT_NOTES,
  XT_NET_TIME,
  XT_PATH_COURSE,
  XT_PATH_DISTANCE_KM,
  XT_PATH_DISTANCE_METERS,
  XT_PATH_DISTANCE_MILES,
  XT_PATH_SPEED,
  XT_PATH_SPEED_KNOTS,
  XT_PATH_SPEED_KPH,
  XT_PATH_SPEED_MPH,
  XT_PHONE_NR,
  XT_POSTAL_CODE,
  XT_POWER,
  XT_ROUTE_NAME,
  XT_SHORTNAME,
  XT_STATE,
  XT_STREET_ADDR,
  XT_TEMPERATURE,
  XT_TEMPERATURE_F,
  XT_TIMET_TIME,
  XT_TIMET_TIME_MS,
  XT_TRACK_NAME,
  XT_TRACK_NEW,
  XT_URL,
  XT_UTM,
  XT_UTM_ZONE,
  XT_UTM_ZONEC,
  XT_UTM_ZONEF,
  XT_UTM_EASTING,
  XT_UTM_NORTHING,
  XT_URL_LINK_TEXT,
  XT_YYYYMMDD_TIME
} xcsv_token;

#include "xcsv_tokens.gperf"       // for Perfect_Hash, xt_mapping

#if CSVFMTS_ENABLED
/****************************************************************************/
/* obligatory global struct                                                 */
/****************************************************************************/

extern char* xcsv_urlbase;
extern char* prefer_shortnames;

XcsvFile xcsv_file;
static double pathdist = 0;
static double oldlon = 999;
static double oldlat = 999;

static int waypt_out_count;
static route_head* csv_track, *csv_route;

struct xcsv_parse_data {
  QString rte_name;
  QString trk_name;
  bool new_track{false};
  double utm_northing{0};
  double utm_easting{0};
  double utm_zone{0};
  char utm_zonec{'N'};
  UrlLink* link_{nullptr};
  gpsbabel_optional::optional<bool> lat_dir_positive;
  gpsbabel_optional::optional<bool> lon_dir_positive;
};

static char* styleopt = nullptr;
static char* snlenopt = nullptr;
static char* snwhiteopt = nullptr;
static char* snupperopt = nullptr;
static char* snuniqueopt = nullptr;
char* prefer_shortnames = nullptr;
char* xcsv_urlbase = nullptr;
static char* opt_datum;

static const char* intstylebuf = nullptr;

static
arglist_t xcsv_args[] = {
  {
    "style", &styleopt, "Full path to XCSV style file", nullptr,
    ARGTYPE_FILE | ARGTYPE_REQUIRED, ARG_NOMINMAX, nullptr
  },
  {
    "snlen", &snlenopt, "Max synthesized shortname length", nullptr,
    ARGTYPE_INT, "1", nullptr, nullptr
  },
  {
    "snwhite", &snwhiteopt, "Allow whitespace synth. shortnames",
    nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
  },
  {
    "snupper", &snupperopt, "UPPERCASE synth. shortnames",
    nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
  },
  {
    "snunique", &snuniqueopt, "Make synth. shortnames unique",
    nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
  },
  {
    "urlbase", &xcsv_urlbase, "Basename prepended to URL on output",
    nullptr, ARGTYPE_STRING, ARG_NOMINMAX, nullptr
  },
  {
    "prefer_shortnames", &prefer_shortnames,
    "Use shortname instead of description",
    nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
  },
  {
    "datum", &opt_datum, "GPS datum (def. WGS 84)",
    "WGS 84", ARGTYPE_STRING, ARG_NOMINMAX, nullptr
  },
  ARG_TERMINATOR
};

/* something to map config file constants to chars */
typedef struct char_map {
  const QString key;
  const QString chars;
} char_map_t;

/* a table of config file constants mapped to chars */
static
char_map_t xcsv_char_table[] = {
  { "COMMA",		"," 	},
  { "COMMASPACE",		", " 	},
  { "SINGLEQUOTE",	"'"	},
  { "DOUBLEQUOTE",	"\""	},
  { "COLON",		":"	},
  { "SEMICOLON",		";"	},
  { "NEWLINE",		"\n"	},
  { "CR",			"\n"	},
  { "CRNEWLINE",  	"\r\n"	},
  { "TAB",  		"\t"	},
  { "SPACE",  		" "	},
  { "HASH",  		"#"	},
  { "WHITESPACE",		"\\w"	},
  { "PIPE",		"|"	},
  { nullptr, 		nullptr	}
};

static void
xcsv_destroy_style()
{

  /*
   * If this xcsv_file struct came from a file we can free it all.
   * If not, we can at least free the queue elements.
   */

  /* destroy the prologue */
  xcsv_file.prologue.clear();

  /* destroy the epilogue */
  xcsv_file.epilogue.clear();

  /* destroy the ifields */
  xcsv_file.ifields.clear();
  /* destroy the ofields */
  xcsv_file.ofields.clear();

  /* other alloc'd glory */
  xcsv_file.field_delimiter = QString();
  xcsv_file.field_encloser = QString();
  xcsv_file.record_delimiter = QString();
  xcsv_file.badchars = QString();
  xcsv_file.description = QString();
  xcsv_file.extension = QString();

  if (xcsv_file.mkshort_handle) {
    mkshort_del_handle(&xcsv_file.mkshort_handle);
  }
}

// Given a keyword of "COMMASPACE", return ", ".
static QString
xcsv_get_char_from_constant_table(const QString& key)
{
  static QHash<QString, QString> substitutions;
  if (substitutions.empty()) {
    for (char_map_t* cm = xcsv_char_table; !cm->key.isNull(); cm++) {
      substitutions.insert(cm->key, cm->chars);
    }
  }
  if (substitutions.contains(key)) {
    return substitutions[key];
  }
  // No substition found? Just return original.
  return key;
}

// Remove outer quotes.
// Should probably be in csv_util.
static QString dequote(const QString& in) {
  QString r = in.simplified();
  if (r.startsWith("\"")) r = r.mid(1);
  if (r.endsWith("\"")) r.chop(1);
  return r;
}

/*****************************************************************************/
/* xcsv_file_init() - prepare xcsv_file for first use.                       */
/*****************************************************************************/
void xcsv_file_init()
{
  xcsv_file.is_internal = false;
  xcsv_file.field_delimiter = QString();
  xcsv_file.field_encloser = QString();
  xcsv_file.record_delimiter = QString();
  xcsv_file.badchars = QString();
  xcsv_file.file = nullptr;
  xcsv_file.stream = nullptr;
  xcsv_file.codec = nullptr;
  xcsv_file.fname = QString();
  xcsv_file.description = nullptr;
  xcsv_file.extension = nullptr;

  xcsv_file.prologue.clear();
  xcsv_file.epilogue.clear();

  xcsv_file.ifields.clear();
  xcsv_file.ofields.clear();
  /*
   * Provide a sane default for CSV _files_.
   */
  xcsv_file.type = ff_type_file;

  xcsv_file.mkshort_handle = mkshort_new_handle();
  xcsv_file.gps_datum = GPS_DATUM_WGS84;
}

XcsvFile::XcsvFile() {
  is_internal = false;
  extension  = nullptr;
//   xcsv_file_init();
}

static void validate_fieldmap(const field_map& fmp, bool is_output) {
  if (fmp.key.isEmpty()) {
    Fatal() << MYNAME << ": xcsv style is missing" <<
            (is_output ? "output" : "input") << "field type.";
  }
  if (fmp.val.isNull()) {
    Fatal() << MYNAME << ": xcsv style" << fmp.key.constData() << "is missing default.";
  }
  if (is_output && fmp.printfc.isNull()) {
    Fatal() << MYNAME << ": xcsv style" << fmp.key.constData() << "output is missing format specifier.";
  }
}

/*****************************************************************************/
/* xcsv_ifield_add() - add input field to ifield queue.                      */
/* usage: xcsv_ifield_add("DESCRIPTION", "", "%s")                           */
/*****************************************************************************/
static void
xcsv_ifield_add(const QString& qkey, const QString& qval, const QString& qpfc)
{
  QByteArray key = qkey.toUtf8();
  QByteArray val = qval.toUtf8();
  QByteArray pfc = qpfc.toUtf8();

  struct xt_mapping* xm = Perfect_Hash::in_word_set(key.constData(), strlen(key.constData()));

  field_map fmp(key, val, pfc, xm ? xm->xt_token : -1);
  validate_fieldmap(fmp, false);

  xcsv_file.ifields.append(fmp);
}

/*****************************************************************************/
/* xcsv_ofield_add() - add output field to ofield queue.                     */
/* usage: xcsv_ofield_add("LAT_DECIMAL", "", "%08.5lf")                      */
/*****************************************************************************/
static void
xcsv_ofield_add(const QString& qkey, const QString& qval, const QString& qpfc, unsigned options)
{
  QByteArray key = qkey.toUtf8();
  QByteArray val = qval.toUtf8();
  QByteArray pfc = qpfc.toUtf8();

  struct xt_mapping* xm = Perfect_Hash::in_word_set(key.constData(), strlen(key.constData()));

  field_map fmp(key, val, pfc, xm ? xm->xt_token : -1, options);
  validate_fieldmap(fmp, true);

  xcsv_file.ofields.append(fmp);
}

/*****************************************************************************/
/* xcsv_prologue_add() - add prologue line to prologue queue                 */
/* usage: xcsv_prologue_add("Four score and seven years ago today,")         */
/*****************************************************************************/
static void
xcsv_prologue_add(const QString& prologue)
{
  xcsv_file.prologue.append(prologue);
}

/*****************************************************************************/
/* xcsv_epilogue_add() - add epilogue line to epilogue queue                 */
/* usage: xcsv_epilogue_add("shall not perish from the earth.")              */
/*****************************************************************************/
static void
xcsv_epilogue_add(const QString& epilogue)
{
  xcsv_file.epilogue.append(epilogue);
}

static
QDateTime
yyyymmdd_to_time(const char* s)
{
  QDate d = QDate::fromString(s, "yyyyMMdd");
  return QDateTime(d);
}


/*
 * sscanftime - Parse a date buffer using strftime format
 */
static
time_t
sscanftime(const char* s, const char* format, const int gmt)
{
  struct tm stm;
  memset(&stm, 0, sizeof(stm));

  if (strptime(s, format, &stm)) {
    if ((stm.tm_mday == 0) && (stm.tm_mon == 0) && (stm.tm_year == 0)) {
      stm.tm_mday = 1;
      stm.tm_mon = 0;
      stm.tm_year = 70;
    }
    stm.tm_isdst = -1;
    if (gmt) {
      return mkgmtime(&stm);
    } else {
      return mktime(&stm);
    }
  }
  // Don't fuss for empty strings.
  if (*s) {
    warning("date parse of string '%s' with format '%s' failed.\n",
            s, format);
  }
  return 0;
}

static
time_t
addhms(const char* s, const char* format)
{
  time_t tt = 0;
  int hour = 0;
  int min = 0;
  int sec = 0;

  char* ampm = (char*) xmalloc(strlen(s) + 1);
  int ac = sscanf(s, format, &hour, &min, &sec, ampm);
  /* If no time format in arg string, assume AM */
  if (ac < 4) {
    ampm[0] = 0;
  }
  if (ac) {
    tt = ((tolower(ampm[0])=='p') ? 43200 : 0) + 3600 * hour + 60 * min + sec;
  }
  xfree(ampm);

  return tt;
}

static
QString
writetime(const char* format, time_t t, bool gmt)
{
  static struct tm* stmp;

  if (gmt) {
    stmp = gmtime(&t);
  } else {
    stmp = localtime(&t);
  }

  // It's unfortunate that we publish the definition of "strftime specifiers"
  // in the style definitions.  For this reason, we have to bust everything
  // down to a time_t and then let strftime handle them.

  char tbuff[1024];
  strftime(tbuff, sizeof tbuff, format, stmp);
  return QString(tbuff);
}

static
QString
writetime(const char* format, const gpsbabel::DateTime& t, bool gmt)
{
  return writetime(format, t.toTime_t(), gmt);
}

QString
writehms(const char* format, time_t t, int gmt)
{
  static struct tm no_time = tm();
  static struct tm* stmp = &no_time;

  if (gmt) {
    stmp = gmtime(&t);
  } else {
    stmp = localtime(&t);
  }

  if (stmp == nullptr) {
    stmp = &no_time;
  }

  return QString().sprintf(format,
                            stmp->tm_hour, stmp->tm_min, stmp->tm_sec,
                            (stmp->tm_hour >= 12 ? "PM" : "AM"));
}

QString
writehms(const char* format, const gpsbabel::DateTime& t, int gmt)
{
  return writehms(format, t.toTime_t(), gmt);
}

static
long
time_to_yyyymmdd(const QDateTime& t)
{
  QDate d = t.date();
  return d.year() * 10000 + d.month() * 100 + d.day();
}

static garmin_fs_t*
gmsd_init(Waypoint* wpt)
{
  garmin_fs_t* gmsd = GMSD_FIND(wpt);
  if (gmsd == nullptr) {
    gmsd = garmin_fs_alloc(-1);
    fs_chain_add(&wpt->fs, (format_specific_data*) gmsd);
  }
  return gmsd;
}

/*****************************************************************************/
/* xcsv_parse_val() - parse incoming data into the waypt structure.          */
/* usage: xcsv_parse_val("-123.34", *waypt, *field_map)                      */
/*****************************************************************************/
static void 
xcsv_parse_val(const char* s, Waypoint* wpt, const field_map& fmp,
               xcsv_parse_data* parse_data, const int line_no)
{
  const char* enclosure = "";
  geocache_data* gc_data = nullptr;

  if (fmp.printfc.isNull()) {
    fatal(MYNAME ": xcsv style '%s' is missing format specifier", fmp.key.constData());
  }

  if (0 == strcmp(fmp.printfc.constData(), "\"%s\"")) {
    enclosure = "\"";
  }
  switch (fmp.hashed_key) {
  case XT_IGNORE:
    /* IGNORE -- Categorically ignore this... */
    break;
  case XT_CONSTANT:
    /* CONSTANT -- Ignore on Input... */
    break;
  case XT_ANYNAME:
    /* ANYNAME -- Ignore -- this is output magic. */
    break;
  case XT_INDEX:
    /* IGNORE -- Calculated Sequence # For Ouput*/
    break;
  case XT_SHORTNAME:
    wpt->shortname = csv_stringtrim(s, enclosure);
    break;
  case XT_DESCRIPTION:
    wpt->description = csv_stringtrim(s, enclosure);
    break;
  case XT_NOTES:
    wpt->notes = csv_stringtrim(s, "");
    break;
  case XT_URL:
    if (!parse_data->link_) {
      parse_data->link_ = new UrlLink;
    }
    parse_data->link_->url_ = QString(s).trimmed();
    break;
  case XT_URL_LINK_TEXT:
    if (!parse_data->link_) {
      parse_data->link_ = new UrlLink;
    }
    parse_data->link_->url_link_text_ = QString(s).trimmed();
    break;
  case XT_ICON_DESCR:
    wpt->icon_descr = QString(s).trimmed();
    break;

    /* LATITUDE CONVERSIONS**************************************************/
  case XT_LAT_DECIMAL:
    /* latitude as a pure decimal value */
    wpt->latitude = atof(s);
    break;
  case XT_LAT_DECIMALDIR:
  case XT_LAT_DIRDECIMAL:
    /* latitude as a decimal with N/S in it. */
    wpt->latitude = decdir_to_dec(s);
    break;
  case XT_LAT_INT32DEG:
    /* latitude as a 32 bit integer offset */
    wpt->latitude = intdeg_to_dec((int) atof(s));
    break;
  case XT_LAT_HUMAN_READABLE:
    human_to_dec(s, &wpt->latitude, &wpt->longitude, 1);
    break;
  case XT_LAT_DDMMDIR:
    wpt->latitude = ddmmdir_to_degrees(s);
    break;
  case XT_LAT_NMEA:
    wpt->latitude = ddmm2degrees(atof(s));
    break;
    // XT_LAT_10E is handled outside the switch.
    /* LONGITUDE CONVERSIONS ***********************************************/
  case XT_LON_DECIMAL:
    /* longitude as a pure decimal value */
    wpt->longitude = atof(s);
    break;
  case XT_LON_DECIMALDIR:
  case XT_LON_DIRDECIMAL:
    /* longitude as a decimal with N/S in it. */
    wpt->longitude = decdir_to_dec(s);
    break;
  case XT_LON_INT32DEG:
    /* longitude as a 32 bit integer offset  */
    wpt->longitude = intdeg_to_dec((int) atof(s));
    break;
  case XT_LON_HUMAN_READABLE:
    human_to_dec(s, &wpt->latitude, &wpt->longitude, 2);
    break;
  case XT_LON_DDMMDIR:
    wpt->longitude = ddmmdir_to_degrees(s);
    break;
  case XT_LON_NMEA:
    wpt->longitude = ddmm2degrees(atof(s));
    break;
    // case XT_LON_10E is handled outside the switch.
    /* LAT AND LON CONVERSIONS ********************************************/
  case XT_LATLON_HUMAN_READABLE:
    human_to_dec(s, &wpt->latitude, &wpt->longitude, 0);
    break;
    /* DIRECTIONS **********************************************************/
  case XT_LAT_DIR:
    /* latitude N/S. */
    if (*s == 'n' || *s == 'N') {
      parse_data->lat_dir_positive = true;
    } else if (*s == 's' || *s == 'S') {
      parse_data->lat_dir_positive = false;
    } else {
      warning("parse of string '%s' on line number %d as LAT_DIR failed.  Expected 'n', 'N', 's' or 'S'.\n", s, line_no);
    }
    break;
  case XT_LON_DIR:
    /* longitude E/W. */
    if (*s == 'e' || *s == 'E') {
      parse_data->lon_dir_positive = true; 
    } else if (*s == 'w' || *s == 'W') {
      parse_data->lon_dir_positive = false;
    } else {
      warning("parse of string '%s' on line number %d as LON_DIR failed.  Expected 'e', 'E', 'w' or 'W'.\n", s, line_no);
    }
    break;
    /* SPECIAL COORDINATES/GRID */
  case XT_MAP_EN_BNG:
    parse_coordinates(s, DATUM_OSGB36, grid_bng,
                      &wpt->latitude, &wpt->longitude, MYNAME);
    break;
  case XT_UTM_ZONE:
    parse_data->utm_zone = atoi(s);
    break;
  case XT_UTM_ZONEC:
    parse_data->utm_zonec = s[0];
    break;
  case XT_UTM_ZONEF:
    parse_data->utm_zone = atoi(s);
    parse_data->utm_zonec = s[strlen(s) - 1];
    break;
  case XT_UTM_EASTING:
    parse_data->utm_easting = atof(s);
    break;
  case XT_UTM_NORTHING:
    parse_data->utm_northing = atof(s);
    break;
  case XT_UTM: {
    char* ss;
    int i = 0;

    parse_data->utm_zone = strtod(s, &ss);
    parse_data->utm_zonec = ss[i];
    ss++;
    parse_data->utm_easting = strtod(ss, &ss);
    while (*ss && !isdigit(*ss)) {
      ss++;
    }
    parse_data->utm_northing = strtod(ss, nullptr);
  }
  break;
  /* ALTITUDE CONVERSIONS ************************************************/
  case XT_ALT_FEET: {
    char *endptr;
    double val = strtod(s, &endptr);
    if ((val == 0 && s==endptr)) {
      wpt->altitude = unknown_alt;
    } else {
      wpt->altitude = FEET_TO_METERS(val);
      if (wpt->altitude < unknown_alt + 1) {
        wpt->altitude = unknown_alt;
      }
    }
  }
  break;
  case XT_ALT_METERS: {
    char *endptr;
    double val = strtod(s, &endptr);
    if ((val == 0 && s==endptr)) {
      wpt->altitude = unknown_alt;
    } else {
      wpt->altitude = val;
      if (wpt->altitude < unknown_alt + 1) {
        wpt->altitude = unknown_alt;
      }
    }
  }
  break;

    /* PATH CONVERSIONS ************************************************/
  case XT_PATH_SPEED:
    WAYPT_SET(wpt, speed, atof(s));
    break;
  case XT_PATH_SPEED_KPH:
    WAYPT_SET(wpt, speed, KPH_TO_MPS(atof(s)));
    break;
  case XT_PATH_SPEED_MPH:
    WAYPT_SET(wpt, speed, MPH_TO_MPS(atof(s)));
    break;
  case XT_PATH_SPEED_KNOTS:
    WAYPT_SET(wpt, speed, KNOTS_TO_MPS(atof(s)));
    break;
  case XT_PATH_COURSE:
    WAYPT_SET(wpt, course, atof(s));
    break;

    /* TIME CONVERSIONS ***************************************************/
  case XT_EXCEL_TIME:
    /* Time as Excel Time  */
    wpt->SetCreationTime(EXCEL_TO_TIMET(atof(s)));
    break;
  case XT_TIMET_TIME:
    /* Time as time_t */
    wpt->SetCreationTime((time_t) atol(s));
    break;
  case XT_TIMET_TIME_MS: {
    /* Time as time_t in milliseconds */
    int s_len = strlen(s);
    if (s_len < 4) {
      /* less than 1 epochsecond, an unusual case */
      wpt->SetCreationTime(0, atoi(s));
    } else {
      char buff[32];
      int off = s_len - 3;
      strncpy(buff, s, off);
      buff[off] = '\0';
      time_t t = (time_t) atol(buff);
      s += off;
      strncpy(buff, s, 3);
      buff[3] = '\0';
      wpt->SetCreationTime(t, atoi(buff));
    }
  }
  break;
  case XT_YYYYMMDD_TIME:
    wpt->SetCreationTime(yyyymmdd_to_time(s));
    break;
  case XT_GMT_TIME:
    wpt->SetCreationTime(sscanftime(s, fmp.printfc.constData(), 1));
    break;
  case XT_LOCAL_TIME:
    if (ugetenv("GPSBABEL_FREEZE_TIME").isNull()) {
      wpt->creation_time += sscanftime(s, fmp.printfc.constData(), 0);
    } else {
      /* Force constant time zone for test */
      wpt->creation_time += sscanftime(s, fmp.printfc.constData(), 1);
    }
    break;
    /* Useful when time and date are in separate fields
    	GMT / Local offset is handled by the two cases above */
  case XT_HMSG_TIME:
  case XT_HMSL_TIME:
    wpt->creation_time += addhms(s, fmp.printfc.constData());
    break;
  case XT_ISO_TIME:
  case XT_ISO_TIME_MS:
    wpt->SetCreationTime(xml_parse_time(s));
    break;
  case XT_NET_TIME: {
    fatal("XT_NET_TIME can't have possibly ever worked.");
//    time_t tt = wpt->GetCreationTime();
//    dotnet_time_to_time_t(atof(s), &tt, &wpt->microseconds);
  }
  break;
  case XT_GEOCACHE_LAST_FOUND:
    wpt->AllocGCData()->last_found = yyyymmdd_to_time(s);
    break;

    /* GEOCACHING STUFF ***************************************************/
  case XT_GEOCACHE_DIFF:
    /* Geocache Difficulty as an int */
    wpt->AllocGCData()->diff = atof(s) * 10;
    break;
  case XT_GEOCACHE_TERR:
    /* Geocache Terrain as an int */
    wpt->AllocGCData()->terr = atof(s) * 10;
    break;
  case XT_GEOCACHE_TYPE:
    /* Geocache Type */
    wpt->AllocGCData()->type = gs_mktype(s);
    break;
  case XT_GEOCACHE_CONTAINER:
    wpt->AllocGCData()->container = gs_mkcont(s);
    break;
  case XT_GEOCACHE_HINT:
    wpt->AllocGCData()->hint = QString(s).trimmed();
    break;
  case XT_GEOCACHE_PLACER:
    wpt->AllocGCData()->placer = QString(s).trimmed();
    break;
  case XT_GEOCACHE_ISAVAILABLE:
    gc_data = wpt->AllocGCData();
    if (case_ignore_strcmp(csv_stringtrim(s, ""), "False") == 0) {
      gc_data->is_available = status_false;
    } else if (case_ignore_strcmp(csv_stringtrim(s, ""), "True") == 0) {
      gc_data->is_available = status_true;
    } else {
      gc_data->is_available = status_unknown;
    }
    break;
  case XT_GEOCACHE_ISARCHIVED:
    gc_data = wpt->AllocGCData();
    if (case_ignore_strcmp(csv_stringtrim(s, ""), "False") == 0) {
      gc_data->is_archived = status_false;
    } else if (case_ignore_strcmp(csv_stringtrim(s, ""), "True") == 0) {
      gc_data->is_archived = status_true;
    } else {
      gc_data->is_archived = status_unknown;
    }
    break;

    /* GPS STUFF *******************************************************/
  case XT_GPS_HDOP:
    wpt->hdop = atof(s);
    break;
  case XT_GPS_VDOP:
    wpt->vdop = atof(s);
    break;
  case XT_GPS_PDOP:
    wpt->pdop = atof(s);
    break;
  case XT_GPS_SAT:
    wpt->sat = atoi(s);
    break;
  case XT_GPS_FIX:
    wpt->fix = (fix_type)(atoi(s)-(fix_type)1);
    if (wpt->fix < fix_2d) {
      if (!case_ignore_strcmp(s, "none")) {
        wpt->fix = fix_none;
      } else if (!case_ignore_strcmp(s, "dgps")) {
        wpt->fix = fix_dgps;
      } else if (!case_ignore_strcmp(s, "pps")) {
        wpt->fix = fix_pps;
      } else {
        wpt->fix = fix_unknown;
      }
    }
    break;
    /* Tracks and routes *********************************************/
  case XT_ROUTE_NAME:
    parse_data->rte_name = csv_stringtrim(s, enclosure);
    break;
  case XT_TRACK_NEW:
    parse_data->new_track = atoi(s);
    break;
  case XT_TRACK_NAME:
    parse_data->trk_name = csv_stringtrim(s, enclosure);
    break;

    /* OTHER STUFF ***************************************************/
  case XT_PATH_DISTANCE_METERS:
    wpt->odometer_distance = atof(s);
    break;
  case XT_PATH_DISTANCE_KM:
    wpt->odometer_distance = atof(s) * 1000.0;
    break;
  case XT_PATH_DISTANCE_MILES:
    wpt->odometer_distance = MILES_TO_METERS(atof(s));
    break;
  case XT_HEART_RATE:
    wpt->heartrate = atoi(s);
    break;
  case XT_CADENCE:
    wpt->cadence = atoi(s);
    break;
  case XT_POWER:
    wpt->power = atof(s);
    break;
  case XT_TEMPERATURE:
    wpt->temperature = atof(s);
    break;
  case XT_TEMPERATURE_F:
    wpt->temperature = (FAHRENHEIT_TO_CELSIUS(atof(s)));
    break;
    /* GMSD ****************************************************************/
  case XT_COUNTRY: {
    garmin_fs_t* gmsd = gmsd_init(wpt);
    GMSD_SET(country, csv_stringtrim(s, enclosure, 0));
  }
  break;
  case XT_STATE: {
    garmin_fs_t* gmsd = gmsd_init(wpt);
    GMSD_SET(state, csv_stringtrim(s, enclosure, 0));
  }
  break;
  case XT_CITY: {
    garmin_fs_t* gmsd = gmsd_init(wpt);
    GMSD_SET(city, csv_stringtrim(s, enclosure, 0));
  }
  break;
  case XT_STREET_ADDR: {
    garmin_fs_t* gmsd = gmsd_init(wpt);
    GMSD_SET(addr, csv_stringtrim(s, enclosure, 0));
  }
  break;
  case XT_POSTAL_CODE: {
    garmin_fs_t* gmsd = gmsd_init(wpt);
    GMSD_SET(postal_code, csv_stringtrim(s, enclosure, 0));
  }
  break;
  case XT_PHONE_NR: {
    garmin_fs_t* gmsd = gmsd_init(wpt);
    GMSD_SET(phone_nr, csv_stringtrim(s, enclosure, 0));
  }
  break;
  case XT_FACILITY: {
    garmin_fs_t* gmsd = gmsd_init(wpt);
    GMSD_SET(facility, csv_stringtrim(s, enclosure, 0));
  }
  break;
  case -1:
    if (strncmp(fmp.key.constData(), "LON_10E", 7) == 0) {
      wpt->longitude = atof(s) / pow(10.0, atof(fmp.key.constData()+7));
    } else if (strncmp(fmp.key.constData(), "LAT_10E", 7) == 0) {
      wpt->latitude = atof(s) / pow(10.0, atof(fmp.key.constData()+7));
    } else {
      warning(MYNAME ": Unknown style directive: %s\n", fmp.key.constData());
    }
    break;

  default:
    fatal("This can't happen\n");
    break;
  }
}

/*****************************************************************************/
/* xcsv_data_read() - read input file, parsing lines, fields and handling    */
/*                   any data conversion (the input meat)                    */
/*****************************************************************************/
static void
xcsv_data_read()
{
  int linecount = 0;
  route_head* rte = nullptr;
  route_head* trk = nullptr;

  while (true) {
    QString buff = xcsv_file.stream->readLine();
    if (buff.isNull()) { 
      break;
    }
    linecount++;
    /* Whack trailing space; leading space may matter if our field sep
     * is whitespace and we have leading whitespace.
     */
    // This could be hoisted out as a generic rtrim() if we need such a thing.
    while(buff.size() > 0 && buff.at(buff.size() - 1).isSpace()) {
      buff.chop(1); 
    }

    /* skip over x many lines on the top for the prologue... */
    if ((linecount - 1) < xcsv_file.prologue.count()) {
      continue;
    }

    /* We should skip over epilogue lines also.  Since we don't want to
     * pre-read the file to know how many data lines we should be seeing,
     * we take this cheap shot at the data and cross our fingers.
     */
    for(const auto& ogp : qAsConst(xcsv_file.epilogue)) {
       if (ogp.startsWith(buff)) {
         buff.clear();
         break;
       }
    }
    if (!buff.isEmpty()) {
      Waypoint* wpt_tmp = new Waypoint;
      // initialize parse data for accumulation of line results from all fields in this line.
      xcsv_parse_data parse_data;
      const QStringList values = csv_linesplit(buff, xcsv_file.field_delimiter,
                        xcsv_file.field_encloser, linecount);

      if (xcsv_file.ifields.isEmpty()) {
        fatal(MYNAME ": attempt to read, but style '%s' has no IFIELDs in it.\n", CSTR(xcsv_file.description)? CSTR(xcsv_file.description) : "unknown");
      }

      int ifield_idx = 0;

      /* now rip the line apart */
      for (const auto& value : values) {
        const field_map& fmp = xcsv_file.ifields.at(ifield_idx++);
        xcsv_parse_val(CSTR(value), wpt_tmp, fmp, &parse_data, linecount);

        if (ifield_idx >= xcsv_file.ifields.size()) {
          /* no more fields, stop parsing! */
          break;
        }
      }

      // If XT_LAT_DIR(XT_LON_DIR) was an input field, and the latitude(longitude) is positive,
      // assume the latitude(longitude) was the absolute value and take the sign from XT_LAT_DIR(XT_LON_DIR).
      if (parse_data.lat_dir_positive.has_value() && !parse_data.lat_dir_positive.value() && (wpt_tmp->latitude > 0.0)) {
        wpt_tmp->latitude = -wpt_tmp->latitude;
      }
      if (parse_data.lon_dir_positive.has_value() && !parse_data.lon_dir_positive.value() && (wpt_tmp->longitude > 0.0)) {
        wpt_tmp->longitude = -wpt_tmp->longitude;
      }

      if ((xcsv_file.gps_datum > -1) && (xcsv_file.gps_datum != GPS_DATUM_WGS84)) {
        double alt;
        GPS_Math_Known_Datum_To_WGS84_M(wpt_tmp->latitude, wpt_tmp->longitude, 0.0,
                                        &wpt_tmp->latitude, &wpt_tmp->longitude, &alt, xcsv_file.gps_datum);
      }

      if (parse_data.utm_easting || parse_data.utm_northing) {
        GPS_Math_UTM_EN_To_Known_Datum(&wpt_tmp->latitude,
                                       &wpt_tmp->longitude,
                                       parse_data.utm_easting, parse_data.utm_northing,
                                       parse_data.utm_zone, parse_data.utm_zonec,
                                       DATUM_WGS84);
      }

      if (parse_data.link_) {
        wpt_tmp->AddUrlLink(*parse_data.link_);
        delete parse_data.link_;
        parse_data.link_ = nullptr;
      }

      switch (xcsv_file.datatype) {
      case unknown_gpsdata:
      case wptdata:
        waypt_add(wpt_tmp);
        break;
      case trkdata:
        if ((trk == nullptr) || parse_data.new_track) {
          trk = route_head_alloc();
          track_add_head(trk);
        }
        if (!parse_data.trk_name.isEmpty()) {
          trk->rte_name = parse_data.trk_name;
        }
        track_add_wpt(trk, wpt_tmp);
        break;
      case rtedata:
        if (rte == nullptr) {
          rte = route_head_alloc();
          route_add_head(rte);
        }
        if (!parse_data.rte_name.isEmpty()) {
          rte->rte_name = parse_data.rte_name;
        }
        route_add_wpt(rte, wpt_tmp);
        break;
      default:
        ;
      }
    }
  }
}

static void
xcsv_resetpathlen(const route_head* head)
{
  pathdist = 0;
  oldlat = 999;
  oldlon = 999;
  csv_route = csv_track = nullptr;
  switch (xcsv_file.datatype) {
  case trkdata:
    csv_track = const_cast<route_head*>(head);
    break;
  case rtedata:
    csv_route = const_cast<route_head*>(head);
    break;
  default:
    break;
  }
}

/*****************************************************************************/
/* xcsv_waypt_pr() - write output file, handling output conversions          */
/*                  (the output meat)                                        */
/*****************************************************************************/
static void
xcsv_waypt_pr(const Waypoint* wpt)
{
  QString buff;
  double latitude, longitude;
  int32 utmz;
  double utme, utmn;
  char utmzc;

  buff[0] = '\0';

  if (oldlon < 900) {
    pathdist += radtomiles(gcdist(RAD(oldlat),RAD(oldlon),
                                  RAD(wpt->latitude),RAD(wpt->longitude)));
  }
  longitude = oldlon = wpt->longitude;
  latitude = oldlat = wpt->latitude;

  QString write_delimiter;
  if (xcsv_file.field_delimiter == "\\w") {
    write_delimiter = " ";
  } else {
    write_delimiter = xcsv_file.field_delimiter;
  }

  QString description;
  QString shortname;
  if (wpt->shortname.isEmpty() || global_opts.synthesize_shortnames) {
    if (!wpt->description.isEmpty()) {
      if (global_opts.synthesize_shortnames) {
        shortname = mkshort_from_wpt(xcsv_file.mkshort_handle, wpt);
      } else {
        shortname = csv_stringclean(wpt->description, xcsv_file.badchars);
      }
    } else {
      /* no shortname available -- let shortname default on output */
    }
  } else {
    shortname = csv_stringclean(wpt->shortname, xcsv_file.badchars);
  }
  if (wpt->description.isEmpty()) {
    if (!shortname.isEmpty()) {
      description = csv_stringclean(shortname, xcsv_file.badchars);
    } else {
      /* no description -- let description default on output */
    }
  } else {
    description = csv_stringclean(wpt->description, xcsv_file.badchars);
  }

  if (prefer_shortnames) {
    description = shortname;
  }

  if ((xcsv_file.gps_datum > -1) && (xcsv_file.gps_datum != GPS_DATUM_WGS84)) {
    double alt;
    GPS_Math_WGS84_To_Known_Datum_M(latitude, longitude, 0.0,
                                    &latitude, &longitude, &alt, xcsv_file.gps_datum);
  }

  int i = 0;
  for (const auto& fmp : qAsConst(xcsv_file.ofields)) {
    double lat = latitude;
    double lon = longitude;
    /*
     * A klunky concept.   This should evaluate to true for any
     * field if we think we don't have realistic value for it.
     * This is used by the 'optional' attribute for suppressing
     * fields on output.
     */
    int field_is_unknown = 0;

    if ((i != 0) && !(fmp.options & OPTIONS_NODELIM)) {
      *xcsv_file.stream << write_delimiter;
    }

    if (fmp.options & OPTIONS_ABSOLUTE) {
      lat = fabs(lat);
      lon = fabs(lon);
    }

    i++;

    switch (fmp.hashed_key) {
    case XT_IGNORE:
      /* IGNORE -- Write the char printf conversion */
      buff = QString().sprintf(fmp.printfc.constData(), "");
      break;
    case XT_INDEX:
      buff = QString().sprintf(fmp.printfc.constData(), waypt_out_count + atoi(fmp.val.constData()));
      break;
    case XT_CONSTANT: {
      auto cp = xcsv_get_char_from_constant_table(fmp.val.constData());
      if (!cp.isEmpty()) {
        buff = QString().sprintf(fmp.printfc.constData(), CSTR(cp));
      } else {
        buff = QString().sprintf(fmp.printfc.constData(), fmp.val.constData());
      }
    }
    break;
    case XT_SHORTNAME:
		buff = QString().sprintf(fmp.printfc.constData(),
                shortname.isEmpty() ? fmp.val.constData() : CSTR(shortname));

      break;
    case XT_ANYNAME:
      {
      QString anyname = wpt->shortname;
      if (anyname.isEmpty()) {
        anyname = mkshort(xcsv_file.mkshort_handle, wpt->description);
      }
      if (anyname.isEmpty()) {
        anyname = mkshort(xcsv_file.mkshort_handle, wpt->description);
      }
      if (anyname.isEmpty()) {
        anyname = wpt->notes;
      }
      if (anyname.isEmpty()) {
        anyname = fmp.val.constData();
      }
      buff = QString().sprintf(fmp.printfc.constData(), CSTR(anyname));
      }

      break;
    case XT_DESCRIPTION:
      buff = QString().sprintf(fmp.printfc.constData(),
                description.isEmpty() ? fmp.val.constData() : CSTR(description));
      break;
    case XT_NOTES:
      buff = QString().sprintf(fmp.printfc.constData(),
                wpt->notes.isEmpty() ? fmp.val.constData() : CSTR(wpt->notes));
      break;
    case XT_URL: {
      if (xcsv_urlbase) {
        buff = xcsv_urlbase;
      }
      if (wpt->HasUrlLink()) {
        UrlLink l = wpt->GetUrlLink();
        buff += QString().sprintf(fmp.printfc.constData(), CSTR(l.url_));
      } else {
        buff += QString().sprintf(fmp.printfc.constData(), fmp.val.constData() && *fmp.val.constData() ? fmp.val.constData() : "\"\"");
      }
    }
    break;
    case XT_URL_LINK_TEXT:
      if (wpt->HasUrlLink()) {
        UrlLink l = wpt->GetUrlLink();
        buff = QString().sprintf(fmp.printfc.constData(),
                 !l.url_link_text_.isEmpty() ? CSTR(l.url_link_text_) : fmp.val.constData());
      }
      break;
    case XT_ICON_DESCR:
      buff = QString().sprintf(fmp.printfc.constData(),
                (!wpt->icon_descr.isNull()) ?
                CSTR(wpt->icon_descr) : fmp.val.constData());
      break;

      /* LATITUDE CONVERSION***********************************************/
    case XT_LAT_DECIMAL:
      /* latitude as a pure decimal value */
      buff = QString().sprintf(fmp.printfc.constData(), lat);
      break;
    case XT_LAT_DECIMALDIR:
      /* latitude as a decimal value with N/S after it */
      buff = QString().sprintf(fmp.printfc.constData(), fabs(lat),
               LAT_DIR(lat));
      break;
    case XT_LAT_DIRDECIMAL:
      /* latitude as a decimal value with N/S before it */
      buff = QString().sprintf(fmp.printfc.constData(),
               LAT_DIR(lat),
               fabs(lat));
      break;
    case XT_LAT_INT32DEG:
      /* latitude as an integer offset from 0 degrees */
      buff = QString().sprintf(fmp.printfc.constData(),
                dec_to_intdeg(lat));
      break;
    case XT_LAT_DDMMDIR:
      /*latitude as (degrees * 100) + decimal minutes, with N/S after it */
      buff = dec_to_human(fmp.printfc.constData(), "SN", degrees2ddmm(lat));
      break;
    case XT_LAT_HUMAN_READABLE:
      buff = dec_to_human(fmp.printfc.constData(), "SN", lat);
      break;
    case XT_LAT_NMEA:
      buff = QString().sprintf(fmp.printfc.constData(), degrees2ddmm(lat));
      break;
      // case XT_LAT_10E is handled outside the switch.
      /* LONGITUDE CONVERSIONS*********************************************/
    case XT_LON_DECIMAL:
      /* longitude as a pure decimal value */
      buff = QString().sprintf(fmp.printfc.constData(), lon);
      break;
    case XT_LON_DECIMALDIR:
      /* latitude as a decimal value with N/S after it */
      buff = QString().sprintf(fmp.printfc.constData(),
               fabs(lon),
               LON_DIR(lon));
      break;
    case XT_LON_DIRDECIMAL:
      /* latitude as a decimal value with N/S before it */
      buff = QString().sprintf(fmp.printfc.constData(),
               LON_DIR(lon),
               fabs(lon));
      break;
    case XT_LON_INT32DEG:
      /* longitudee as an integer offset from 0 degrees */
      buff = QString().sprintf(fmp.printfc.constData(),
                dec_to_intdeg(lon));
      break;
    case XT_LON_DDMMDIR:
      /* longidute as (degrees * 100) + decimal minutes, with W/E after it*/
      buff = dec_to_human(fmp.printfc.constData(), "WE", degrees2ddmm(lon));
      break;
    case XT_LON_HUMAN_READABLE:
      buff = dec_to_human(fmp.printfc.constData(), "WE", lon);
      break;
    case XT_LATLON_HUMAN_READABLE:
      buff = dec_to_human(fmp.printfc.constData(), "SN", lat);
        buff += " ";
      buff += dec_to_human(fmp.printfc.constData(), "WE", lon);
      // Tidy up leading, trailing, middle whitespace.
      buff = buff.simplified();
      break;
    case XT_LON_NMEA:
      buff = QString().sprintf(fmp.printfc.constData(), degrees2ddmm(lon));
      break;
      // case XT_LON_10E is handled outside the switch.
      /* DIRECTIONS *******************************************************/
    case XT_LAT_DIR:
      /* latitude N/S as a char */
      buff = QString().sprintf(fmp.printfc.constData(),
                LAT_DIR(lat));
      break;
    case XT_LON_DIR:
      /* longitude E/W as a char */
      buff = QString().sprintf(fmp.printfc.constData(),
                LON_DIR(lon));
      break;

      /* SPECIAL COORDINATES */
    case XT_MAP_EN_BNG: {
      char map[3];
      double north, east;
      if (! GPS_Math_WGS84_To_UKOSMap_M(wpt->latitude, wpt->longitude, &east, &north, map))
        fatal(MYNAME ": Position (%.5f/%.5f) outside of BNG.\n",
              wpt->latitude, wpt->longitude);
      buff = QString().sprintf(fmp.printfc.constData(), map, (int)(east + 0.5), (int)(north + 0.5));
    }
    break;
    case XT_UTM: {
      char tbuf[100];
      GPS_Math_WGS84_To_UTM_EN(wpt->latitude, wpt->longitude,
                               &utme, &utmn, &utmz, &utmzc);
      snprintf(tbuf, sizeof(tbuf), "%d%c %6.0f %7.0f",
               utmz, utmzc, utme, utmn);
      buff = QString().sprintf(fmp.printfc.constData(), tbuf);
    }
    break;
    case XT_UTM_ZONE:
      GPS_Math_WGS84_To_UTM_EN(wpt->latitude, wpt->longitude,
                               &utme, &utmn, &utmz, &utmzc);
      buff = QString().sprintf(fmp.printfc.constData(), utmz);
      break;
    case XT_UTM_ZONEC:
      GPS_Math_WGS84_To_UTM_EN(wpt->latitude, wpt->longitude,
                               &utme, &utmn, &utmz, &utmzc);
      buff = QString().sprintf(fmp.printfc.constData(), utmzc);
      break;
    case XT_UTM_ZONEF: {
      char tbuf[10];
      GPS_Math_WGS84_To_UTM_EN(wpt->latitude, wpt->longitude,
                               &utme, &utmn, &utmz, &utmzc);
      tbuf[0] = 0;
      snprintf(tbuf, sizeof(tbuf), "%d%c", utmz, utmzc);
      buff = QString().sprintf(fmp.printfc.constData(), tbuf);
    }
    break;
    case XT_UTM_NORTHING:
      GPS_Math_WGS84_To_UTM_EN(wpt->latitude, wpt->longitude,
                               &utme, &utmn, &utmz, &utmzc);
      buff = QString().sprintf(fmp.printfc.constData(), utmn);
      break;
    case XT_UTM_EASTING:
      GPS_Math_WGS84_To_UTM_EN(wpt->latitude, wpt->longitude,
                               &utme, &utmn, &utmz, &utmzc);
      buff = QString().sprintf(fmp.printfc.constData(), utme);
      break;

      /* ALTITUDE CONVERSIONS**********************************************/
    case XT_ALT_FEET:
      /* altitude in feet as a decimal value */
      if (wpt->altitude != unknown_alt) {
        buff = QString().sprintf(fmp.printfc.constData(),
                  METERS_TO_FEET(wpt->altitude));
      }
      break;
    case XT_ALT_METERS:
      /* altitude in meters as a decimal value */
      if (wpt->altitude != unknown_alt) {
        buff = QString().sprintf(fmp.printfc.constData(),
                  wpt->altitude);
      }
      break;

      /* DISTANCE CONVERSIONS**********************************************/
      /* prefer odometer distance. */
      /* if not available, use calculated distance from positions */
    case XT_PATH_DISTANCE_MILES:
      /* path (route/track) distance in miles */
      if (wpt->odometer_distance) {
        buff = QString().sprintf(fmp.printfc.constData(), METERS_TO_MILES(wpt->odometer_distance));
      } else {
        buff = QString().sprintf(fmp.printfc.constData(), pathdist);
      }
      break;
    case XT_PATH_DISTANCE_METERS:
      /* path (route/track) distance in meters */
      if (wpt->odometer_distance) {
        buff = QString().sprintf(fmp.printfc.constData(), wpt->odometer_distance);
      } else {
        buff = QString().sprintf(fmp.printfc.constData(), MILES_TO_METERS(pathdist));
      }
      break;
    case XT_PATH_DISTANCE_KM:
      /* path (route/track) distance in kilometers */
      if (wpt->odometer_distance) {
        buff = QString().sprintf(fmp.printfc.constData(), wpt->odometer_distance / 1000.0);
      } else {
        buff = QString().sprintf(fmp.printfc.constData(), MILES_TO_METERS(pathdist) / 1000.0);
      }
      break;
    case XT_PATH_SPEED:
      buff = QString().sprintf(fmp.printfc.constData(), wpt->speed);
      break;
    case XT_PATH_SPEED_KPH:
      buff = QString().sprintf(fmp.printfc.constData(), MPS_TO_KPH(wpt->speed));
      break;
    case XT_PATH_SPEED_MPH:
      buff = QString().sprintf(fmp.printfc.constData(), MPS_TO_MPH(wpt->speed));
      break;
    case XT_PATH_SPEED_KNOTS:
      buff = QString().sprintf(fmp.printfc.constData(), MPS_TO_KNOTS(wpt->speed));
      break;
    case XT_PATH_COURSE:
      buff = QString().sprintf(fmp.printfc.constData(), wpt->course);
      break;

      /* HEART RATE CONVERSION***********************************************/
    case XT_HEART_RATE:
      buff = QString().sprintf(fmp.printfc.constData(), wpt->heartrate);
      break;
      /* CADENCE CONVERSION***********************************************/
    case XT_CADENCE:
      buff = QString().sprintf(fmp.printfc.constData(), wpt->cadence);
      break;
      /* POWER CONVERSION***********************************************/
    case XT_POWER:
      buff = QString().sprintf(fmp.printfc.constData(), wpt->power);
      break;
    case XT_TEMPERATURE:
      buff = QString().sprintf(fmp.printfc.constData(), wpt->temperature);
      break;
    case XT_TEMPERATURE_F:
      buff = QString().sprintf(fmp.printfc.constData(), CELSIUS_TO_FAHRENHEIT(wpt->temperature));
      break;
      /* TIME CONVERSIONS**************************************************/
    case XT_EXCEL_TIME:
      /* creation time as an excel (double) time */
      buff = QString().sprintf(fmp.printfc.constData(), TIMET_TO_EXCEL(wpt->GetCreationTime().toTime_t()));
      break;
    case XT_TIMET_TIME:
      /* time as a time_t variable */
    {
      time_t tt = wpt->GetCreationTime().toTime_t();
      buff = QString().sprintf(fmp.printfc.constData(), tt);
    }
    break;

    case XT_TIMET_TIME_MS: {
      /* time as a time_t variable in milliseconds */
      buff = writetime("%ld", wpt->GetCreationTime().toTime_t(), false);
      buff += QString().sprintf("%03d", wpt->GetCreationTime().time().msec());

    }
    break;
    case XT_YYYYMMDD_TIME:
      buff = QString().sprintf(fmp.printfc.constData(), time_to_yyyymmdd(wpt->GetCreationTime()));
      break;
    case XT_GMT_TIME:
      buff = writetime(fmp.printfc.constData(), wpt->GetCreationTime(), true);
      break;
    case XT_LOCAL_TIME:
      buff = writetime(fmp.printfc.constData(), wpt->GetCreationTime(), false);
      break;
    case XT_HMSG_TIME:
      buff = writehms(fmp.printfc.constData(), wpt->GetCreationTime(), 1);
      break;
    case XT_HMSL_TIME:
      buff = writehms(fmp.printfc.constData(), wpt->GetCreationTime(), 0);
      break;
    case XT_ISO_TIME:
      buff = writetime("%Y-%m-%dT%H:%M:%SZ", wpt->GetCreationTime(), true);
      break;
    case XT_ISO_TIME_MS:
      buff = wpt->GetCreationTime().toPrettyString();
      break;
    case XT_GEOCACHE_LAST_FOUND:
      buff = QString().sprintf(fmp.printfc.constData(), time_to_yyyymmdd(wpt->gc_data->last_found));
      break;
      /* GEOCACHE STUFF **************************************************/
    case XT_GEOCACHE_DIFF:
      /* Geocache Difficulty as a double */
      buff = QString().sprintf(fmp.printfc.constData(), wpt->gc_data->diff / 10.0);
      field_is_unknown = !wpt->gc_data->diff;
      break;
    case XT_GEOCACHE_TERR:
      /* Geocache Terrain as a double */
      buff = QString().sprintf(fmp.printfc.constData(), wpt->gc_data->terr / 10.0);
      field_is_unknown = !wpt->gc_data->terr;
      break;
    case XT_GEOCACHE_CONTAINER:
      /* Geocache Container */
      buff = QString().sprintf(fmp.printfc.constData(), gs_get_container(wpt->gc_data->container));
      field_is_unknown = wpt->gc_data->container == gc_unknown;
      break;
    case XT_GEOCACHE_TYPE:
      /* Geocache Type */
      buff = QString().sprintf(fmp.printfc.constData(), gs_get_cachetype(wpt->gc_data->type));
      field_is_unknown = wpt->gc_data->type == gt_unknown;
      break;
    case XT_GEOCACHE_HINT:
      buff = QString().sprintf(fmp.printfc.constData(), CSTR(wpt->gc_data->hint));
      field_is_unknown = !wpt->gc_data->hint.isEmpty();
      break;
    case XT_GEOCACHE_PLACER:
      buff = QString().sprintf(fmp.printfc.constData(), CSTR(wpt->gc_data->placer));
      field_is_unknown = !wpt->gc_data->placer.isEmpty();
      break;
    case XT_GEOCACHE_ISAVAILABLE:
      if (wpt->gc_data->is_available == status_false) {
        buff = QString().sprintf(fmp.printfc.constData(), "False");
      } else if (wpt->gc_data->is_available == status_true) {
        buff = QString().sprintf(fmp.printfc.constData(), "True");
      } else {
        buff = QString().sprintf(fmp.printfc.constData(), "Unknown");
      }
      break;
    case XT_GEOCACHE_ISARCHIVED:
      if (wpt->gc_data->is_archived == status_false) {
        buff = QString().sprintf(fmp.printfc.constData(), "False");
      } else if (wpt->gc_data->is_archived == status_true) {
        buff = QString().sprintf(fmp.printfc.constData(), "True");
      } else {
        buff = QString().sprintf(fmp.printfc.constData(), "Unknown");
      }
      break;
      /* Tracks and Routes ***********************************************/
    case XT_TRACK_NEW:
      if (csv_track) {
        if (WAYPT_HAS(wpt,new_trkseg)) {
          buff = QString().sprintf(fmp.printfc.constData(), 1);
        } else {
          buff = QString().sprintf(fmp.printfc.constData(), 0);
        }
      }
      break;
    case XT_TRACK_NAME:
      if (csv_track) {
        QString r = csv_track->rte_name;
        buff = QString().sprintf(fmp.printfc.constData(), NONULL(r));
      }
      break;
    case XT_ROUTE_NAME:
      if (csv_route) {
        QString r = csv_route->rte_name;
        buff = QString().sprintf(fmp.printfc.constData(), NONULL(r));
      }
      break;

      /* GPS STUFF *******************************************************/
    case XT_GPS_HDOP:
      buff = QString().sprintf(fmp.printfc.constData(), wpt->hdop);
      field_is_unknown = !wpt->hdop;
      break;
    case XT_GPS_VDOP:
      buff = QString().sprintf(fmp.printfc.constData(), wpt->vdop);
      field_is_unknown = !wpt->vdop;
      break;
    case XT_GPS_PDOP:
      buff = QString().sprintf(fmp.printfc.constData(), wpt->pdop);
      field_is_unknown = !wpt->pdop;
      break;
    case XT_GPS_SAT:
      buff = QString().sprintf(fmp.printfc.constData(), wpt->sat);
      field_is_unknown = !wpt->sat;
      break;
    case XT_GPS_FIX: {
      const char* fix = nullptr;
      switch (wpt->fix) {
      case fix_unknown:
        field_is_unknown = 1;
        fix = "Unknown";
        break;
      case fix_none:
        fix = "None";
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
      }
      buff = QString().sprintf(fmp.printfc.constData(), fix);
    }
    break;
    /* GMSD ************************************************************/
    case XT_COUNTRY: {
      garmin_fs_t* gmsd = GMSD_FIND(wpt);
      buff = QString().sprintf(fmp.printfc.constData(), GMSD_GET(country, ""));
    }
    break;
    case XT_STATE: {
      garmin_fs_t* gmsd = GMSD_FIND(wpt);
      buff = QString().sprintf(fmp.printfc.constData(), GMSD_GET(state, ""));
    }
    break;
    case XT_CITY: {
      garmin_fs_t* gmsd = GMSD_FIND(wpt);
      buff = QString().sprintf(fmp.printfc.constData(), GMSD_GET(city, ""));
    }
    break;
    case XT_POSTAL_CODE: {
      garmin_fs_t* gmsd = GMSD_FIND(wpt);
      buff = QString().sprintf(fmp.printfc.constData(), GMSD_GET(postal_code, ""));
    }
    break;
    case XT_STREET_ADDR: {
      garmin_fs_t* gmsd = GMSD_FIND(wpt);
      buff = QString().sprintf(fmp.printfc.constData(), GMSD_GET(addr, ""));
    }
    break;
    case XT_PHONE_NR: {
      garmin_fs_t* gmsd = GMSD_FIND(wpt);
      buff = QString().sprintf(fmp.printfc.constData(), GMSD_GET(phone_nr, ""));
    }
    break;
    case XT_FACILITY: {
      garmin_fs_t* gmsd = GMSD_FIND(wpt);
      buff = QString().sprintf(fmp.printfc.constData(), GMSD_GET(facility, ""));
    }
    break;
    /* specials */
    case XT_FILENAME:
      buff = QString().sprintf(fmp.printfc.constData(), CSTR(wpt->session->filename));
      break;
    case XT_FORMAT:
      buff = QString().sprintf(fmp.printfc.constData(), CSTR(wpt->session->name));
      break;
    case -1:
      if (strncmp(fmp.key.constData(), "LON_10E", 7) == 0) {
        buff = QString().sprintf(fmp.printfc.constData(), lon * pow(10.0, atof(fmp.key.constData()+7)));
      } else if (strncmp(fmp.key.constData(), "LAT_10E", 7) == 0) {
        buff = QString().sprintf(fmp.printfc.constData(), lat * pow(10.0, atof(fmp.key.constData()+7)));
      }
      break;
    default:
      warning(MYNAME ": Unknown style directive: %s\n", fmp.key.constData());
      break;
    }
    QString obuff = csv_stringclean(buff, xcsv_file.badchars);

    if (field_is_unknown && fmp.options & OPTIONS_OPTIONAL) {
      continue;
    }

    if (!xcsv_file.field_encloser.isEmpty()) {
      /* print the enclosing character(s) */
      *xcsv_file.stream << xcsv_file.record_delimiter;
    }

    /* As a special case (pronounced "horrible hack") we allow
     * ""%s"" to smuggle bad characters through.
     */
    if (0 == strcmp(fmp.printfc.constData(), "\"%s\"")) {
      obuff = '"' + obuff + '"';
    }
    *xcsv_file.stream << obuff;

    if (!xcsv_file.field_encloser.isEmpty()) {
      /* print the enclosing character(s) */
      *xcsv_file.stream << xcsv_file.record_delimiter;
    }
    buff.clear();
  }

  *xcsv_file.stream << xcsv_file.record_delimiter;

  /* increment the index counter */
  waypt_out_count++;
}

static void
xcsv_noop(const route_head* wp)
{
  (void)wp;
  /* no-op */
}

// return |original| after performing token replacement.
static QString
xcsv_replace_tokens(const QString& original) {
  QString replacement = original;
    // Don't do potentially expensive replacements if token prefix
    // isn't present;
    if (original.contains("__")) {
      time_t my_time = gpsbabel_time;

      replacement.replace("__FILE__", xcsv_file.fname);
      replacement.replace("__VERSION__", my_time == 0 ? "" : gpsbabel_version);

      QDateTime dt = QDateTime::fromTime_t(my_time);
      dt = dt.toTimeSpec(Qt::UTC);

      QString dts = dt.toString("ddd MMM dd hh:mm:ss yyyy");
      replacement.replace("__DATE_AND_TIME__", dts);

      QString d = dt.toString("MM/dd/yyyy");
      replacement.replace("__DATE__", d);

      QString t = dt.toString("hh:mm:ss");
      replacement.replace("__TIME__", t);
    }
  return replacement;
}

/*****************************************************************************/
/* xcsv_data_write(void) - write prologues, spawn the output loop, and write */
/*                         epilogues.                                        */
/*****************************************************************************/
static void
xcsv_data_write()
{
  /* reset the index counter */
  waypt_out_count = 0;

  /* output prologue lines, if any. */
  for (const auto& line : qAsConst(xcsv_file.prologue)) {
   QString line_to_write = xcsv_replace_tokens(line);
    *xcsv_file.stream << line_to_write <<  xcsv_file.record_delimiter;
  }

  if ((xcsv_file.datatype == 0) || (xcsv_file.datatype == wptdata)) {
    waypt_disp_all(xcsv_waypt_pr);
  }
  if ((xcsv_file.datatype == 0) || (xcsv_file.datatype == rtedata)) {
    route_disp_all(xcsv_resetpathlen,xcsv_noop,xcsv_waypt_pr);
  }
  if ((xcsv_file.datatype == 0) || (xcsv_file.datatype == trkdata)) {
    track_disp_all(xcsv_resetpathlen,xcsv_noop,xcsv_waypt_pr);
  }

  /* output epilogue lines, if any. */
  for (const auto& line : qAsConst(xcsv_file.epilogue)) {
    QString line_to_write = xcsv_replace_tokens(line);
    *xcsv_file.stream << line_to_write << xcsv_file.record_delimiter;
  }
}

static void
xcsv_parse_style_line(QString line)
{
  // The lines to be parsed have a leading operation |op| that is
  // separated by whitespace from the rest. Each op may have zero or
  // more comma separated tokens  |token[]|.

  // Handle comments, with an escape. Probably not optimal.
  int escape_idx = line.indexOf('\\');
  int comment_idx = line.indexOf('#');
  if (comment_idx > 0 && escape_idx +1 != comment_idx) {
    line = line.mid(0, line.indexOf("#")).trimmed();
  } else {
    line = line.replace("\\#", "#");
  }

  // Separate op and tokens.
  int sep = line.indexOf(QRegExp("\\s+"));

  // the first token is the operation, e.g. "IFIELD"
  QString op = line.mid(0, sep).trimmed().toUpper();
  QString tokenstr = line.mid(sep).trimmed();
  QStringList tokens = tokenstr.split(",");

  if (op == "FIELD_DELIMITER") {
    auto cp = xcsv_get_char_from_constant_table(tokens[0]);
    xcsv_file.field_delimiter = cp;

    char* p = csv_stringtrim(CSTR(xcsv_file.field_delimiter), " ", 0);
      /* field delimiters are always bad characters */
    if (0 == strcmp(p, "\\w")) {
      xcsv_file.badchars = " \n\r";
    } else {
      xcsv_file.badchars += p;
    }
    xfree(p);

  } else

  if (op == "FIELD_ENCLOSER") {
    auto cp = xcsv_get_char_from_constant_table(tokens[0]);
    xcsv_file.field_encloser = cp;

    char* p = csv_stringtrim(CSTR(xcsv_file.field_encloser), " ", 0);
    xcsv_file.badchars += p;
    xfree(p);
  } else

  if (op == "RECORD_DELIMITER") {
    auto cp = xcsv_get_char_from_constant_table(tokens[0]);
    xcsv_file.record_delimiter = cp;

      // Record delimiters are always bad characters.
    auto p = csv_stringtrim(CSTR(xcsv_file.record_delimiter), " ", 0);
    xcsv_file.badchars += p;
    xfree(p);

  } else

  if (op == "FORMAT_TYPE") {
    if (tokens[0] == "INTERNAL") {
      xcsv_file.type = ff_type_internal;
    }
      // this is almost inconcievable...
    if (tokens[0] == "SERIAL") {
      xcsv_file.type = ff_type_serial;
    }
  } else

  if (op == "DESCRIPTION") {
    xcsv_file.description = tokens[0];
  } else

  if (op == "EXTENSION") {
    xcsv_file.extension = tokens[0];
  } else

  if (op == "SHORTLEN") {
    if (xcsv_file.mkshort_handle) {
      setshort_length(xcsv_file.mkshort_handle, tokens[0].toInt());
    }
  } else

  if (op == "SHORTWHITE") {
    if (xcsv_file.mkshort_handle) {
      setshort_whitespace_ok(xcsv_file.mkshort_handle, tokens[0].toInt());
    }
  } else

  if (op == "BADCHARS") {
    char* sp = csv_stringtrim(CSTR(tokenstr), "\"", 1);
    QString cp = xcsv_get_char_from_constant_table(sp);
    xcsv_file.badchars += cp;
    xfree(sp);
  } else

  if (op =="PROLOGUE") {
    xcsv_prologue_add(tokenstr);
  } else

  if (op == "EPILOGUE") {
    xcsv_epilogue_add(tokenstr);
  } else

  if (op == "ENCODING") {
    QByteArray ba;
    ba.append(tokens[0]);
    xcsv_file.codec = QTextCodec::codecForName(ba);
    if (!xcsv_file.codec) {
      Fatal() << "Unsupported character set '" << QString(tokens[0]) << "'.";
    }
  } else

  if (op == "DATUM") {
    xcsv_file.gps_datum = GPS_Lookup_Datum_Index(tokens[0]);
    is_fatal(xcsv_file.gps_datum < 0, MYNAME ": datum \"%s\" is not supported.", CSTR(tokens[0]));
  } else

  if (op == "DATATYPE") {
    QString p = tokens[0].toUpper();
    if (p == "TRACK") {
      xcsv_file.datatype = trkdata;
    } else if (p == "ROUTE") {
      xcsv_file.datatype = rtedata;
    } else if (p == "WAYPOINT") {
      xcsv_file.datatype = wptdata;
    } else {
      Fatal() << MYNAME << ": Unknown data type" << p;
    }
  } else

  if (op == "IFIELD") {
    if (tokens.size() < 3) {
      Fatal() << "Invalid IFIELD line: " << tokenstr;
    }

    // The key ("LAT_DIR") should never contain quotes.

    const QString key = tokens[0].simplified();
    const QString val = dequote(tokens[1]);
    const QString pfc = dequote(tokens[2]);
    xcsv_ifield_add(key, val, pfc);
  } else

      //
      //  as OFIELDs are implemented as an after-thought, I'll
      //  leave this as it's own parsing for now.  We could
      //  change the world on ifield vs ofield format later..
      //
  if (op == "OFIELD") {
    unsigned options = 0;
      // Note: simplifieid() has to run after split().
    if (tokens.size() < 3) {
      Fatal() << "Invalid OFIELD line: " << tokenstr;
    }

    // The key ("LAT_DIR") should never contain quotes.
    const QString key = tokens[0].simplified();
    const QString val = dequote(tokens[1]);
    const QString pfc = dequote(tokens[2]);

    // This is pretty lazy way to parse write options.
    // They've very rarely used, so we'll go for simple.
    // We may have split the optional fourth and final field which can contain
    // option[s], so look at all the remaining tokens.
    for (int token_idx = 3; token_idx < tokens.size(); ++token_idx) {
      QString options_string = tokens[token_idx].simplified();
      if (options_string.contains("no_delim_before")) {
        options |= OPTIONS_NODELIM;
      }
      if (options_string.contains("absolute")) {
        options |= OPTIONS_ABSOLUTE;
      }
      if (options_string.contains("optional")) {
        options |= OPTIONS_OPTIONAL;
      }
    }
    xcsv_ofield_add(key, val, pfc, options);
  }
}


/*
 * A wrapper for xcsv_parse_style_line that reads until it hits
 * a terminating null.   Makes multiple calls to that function so
 * that "ignore to end of line" comments work right.
 */
static void
xcsv_parse_style_buff(const char* sbuff)
{
  QStringList lines = QString(sbuff).split('\n');
  for (const auto& line : lines) {
    xcsv_parse_style_line(line);
  }
}

static void
xcsv_read_style(const char* fname)
{
  xcsv_file_init();

  gbfile* fp = gbfopen(fname, "rb", MYNAME);
  for  (QString sbuff = gbfgetstr(fp); !sbuff.isNull(); sbuff = gbfgetstr(fp)) {
    sbuff = sbuff.trimmed();
    xcsv_parse_style_line(sbuff);
  }

  /* if we have no output fields, use input fields as output fields */
  if (xcsv_file.ofields.isEmpty()) {
    xcsv_file.ofields = xcsv_file.ifields;
  }
  gbfclose(fp);
}

/*
 * Passed a pointer to an internal buffer that would be identical
 * to the series of bytes that would be in a style file, we set up
 * the xcsv parser and make it ready for general use.
 */
void
xcsv_read_internal_style(const char* style_buf)
{
  xcsv_file_init();
  xcsv_file.is_internal = true;

  xcsv_parse_style_buff(style_buf);

  /* if we have no output fields, use input fields as output fields */
  if (xcsv_file.ofields.isEmpty()) {
    xcsv_file.ofields = xcsv_file.ifields;
  }
}

void
xcsv_setup_internal_style(const char* style_buf)
{
  xcsv_file_init();
  xcsv_destroy_style();
  xcsv_file.is_internal = !!style_buf;
  intstylebuf = style_buf;
}


static void
xcsv_rd_init(const QString& fname)
{

  /*
   * if we don't have an internal style defined, we need to
   * read it from a user-supplied style file, or die trying.
   */
  if (xcsv_file.is_internal) {
    xcsv_read_internal_style(intstylebuf);
  } else {
    if (!styleopt) {
      fatal(MYNAME ": XCSV input style not declared.  Use ... -i xcsv,style=path/to/file.style\n");
    }

    xcsv_read_style(styleopt);
  }

  if ((xcsv_file.datatype == 0) || (xcsv_file.datatype == wptdata)) {
    if (global_opts.masked_objective & (TRKDATAMASK|RTEDATAMASK)) {
      warning(MYNAME " attempt to read %s as a track or route, but this format only supports waypoints on read.  Reading as waypoints instead.\n", qPrintable(fname));
    }
  }

  xcsv_file.file = new gpsbabel::File(fname);
  xcsv_file.file->open(QFile::ReadOnly);
  xcsv_file.stream = new QTextStream(xcsv_file.file);
  if (xcsv_file.codec) {
    xcsv_file.stream->setCodec(xcsv_file.codec);
  } else {
    // default to UTF-8.
    xcsv_file.stream->setCodec("UTF-8");
    xcsv_file.stream->setAutoDetectUnicode(true);
  }
  xcsv_file.gps_datum = GPS_Lookup_Datum_Index(opt_datum);
  is_fatal(xcsv_file.gps_datum < 0, MYNAME ": datum \"%s\" is not supported.", opt_datum);
}

static void
xcsv_rd_deinit()
{
  xcsv_file.file->close();
  delete xcsv_file.file;
  xcsv_file.file = nullptr;
  delete xcsv_file.stream;
  xcsv_file.stream = nullptr;
  xcsv_file.codec = nullptr;

  xcsv_destroy_style();
}

static void
xcsv_wr_init(const QString& fname)
{
  /* if we don't have an internal style defined, we need to
   * read it from a user-supplied style file, or die trying.
   * 8/19 - add test for styleopt to ensure that a write of a style
   * after a read of a style works.
   */
  if (xcsv_file.is_internal && !styleopt) {
    xcsv_read_internal_style(intstylebuf);
  } else {

    if (!styleopt) {
      fatal(MYNAME ": XCSV output style not declared.  Use ... -o xcsv,style=path/to/file.style\n");
    }

    xcsv_read_style(styleopt);
  }

  xcsv_file.file = new gpsbabel::File(fname);
  xcsv_file.file->open(QFile::WriteOnly | QFile::Text);
  xcsv_file.stream = new QTextStream(xcsv_file.file);
  if (xcsv_file.codec) {
    xcsv_file.stream->setCodec(xcsv_file.codec);
    // enable bom for all UTF codecs except UTF-8
    if (xcsv_file.codec->mibEnum() != 106) {
      xcsv_file.stream->setGenerateByteOrderMark(true);
    }
  } else {
    // emulate gbfputs which assumes UTF-8.
    xcsv_file.stream->setCodec("UTF-8");
  }
  xcsv_file.fname = fname;

  /* set mkshort options from the command line */
  if (global_opts.synthesize_shortnames) {

    if (snlenopt) {
      setshort_length(xcsv_file.mkshort_handle, atoi(snlenopt));
    }

    if (snwhiteopt) {
      setshort_whitespace_ok(xcsv_file.mkshort_handle, atoi(snwhiteopt));
    }

    if (snupperopt) {
      setshort_mustupper(xcsv_file.mkshort_handle, atoi(snupperopt));
    }

    if (snuniqueopt) {
      setshort_mustuniq(xcsv_file.mkshort_handle, atoi(snuniqueopt));
    }

    setshort_badchars(xcsv_file.mkshort_handle, CSTR(xcsv_file.badchars));

  }
  xcsv_file.gps_datum = GPS_Lookup_Datum_Index(opt_datum);
  is_fatal(xcsv_file.gps_datum < 0, MYNAME ": datum \"%s\" is not supported.", opt_datum);
}

static void
xcsv_wr_position_init(const QString& fname)
{
  xcsv_wr_init(fname);
}

static void
xcsv_wr_deinit()
{
  xcsv_file.stream->flush();
  xcsv_file.file->close();
  delete xcsv_file.file;
  xcsv_file.file = nullptr;
  delete xcsv_file.stream;
  xcsv_file.stream = nullptr;
  xcsv_file.codec = nullptr;

  xcsv_destroy_style();
}

static void
xcsv_wr_position_deinit()
{
  xcsv_wr_deinit();
}


static void
xcsv_wr_position(Waypoint* wpt)
{
  /* Tweak incoming name if we don't have a fix */
  switch (wpt->fix) {
  case fix_none:
    wpt->shortname = "ESTIMATED Position";
    break;
  default:
    break;
  }

  waypt_add(wpt);
  xcsv_data_write();
  waypt_del(wpt);

  xcsv_file.stream->flush();
}

ff_vecs_t xcsv_vecs = {
  ff_type_internal,
  FF_CAP_RW_WPT, /* This is a bit of a lie for now... */
  xcsv_rd_init,
  xcsv_wr_init,
  xcsv_rd_deinit,
  xcsv_wr_deinit,
  xcsv_data_read,
  xcsv_data_write,
  nullptr,
  xcsv_args,
  CET_CHARSET_ASCII, 0,	/* CET-REVIEW */
  { nullptr, nullptr, nullptr, xcsv_wr_position_init, xcsv_wr_position, xcsv_wr_position_deinit },
  nullptr

};
#else
void xcsv_read_internal_style(const char* style_buf) {}
void xcsv_setup_internal_style(const char* style_buf) {}
#endif //CSVFMTS_ENABLED
