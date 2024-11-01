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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

 */

#include "xcsv.h"

#include <cctype>                  // for isdigit, tolower
#include <cmath>                   // for fabs, pow
#include <cstdio>                  // for snprintf, sscanf
#include <cstdint>                 // for uint32_t
#include <cstdlib>                 // for strtod
#include <cstring>                 // for strlen, strncmp, strcmp
#include <ctime>                   // for gmtime, localtime, time_t, mktime, strftime
#include <optional>                // for optional
#include <utility>                 // for as_const

#include <QByteArray>              // for QByteArray
#include <QChar>                   // for QChar
#include <QDate>                   // for QDate
#include <QDateTime>               // for QDateTime
#include <QDebug>                  // for QDebug
#include <QHash>                   // for QHash
#include <QIODevice>               // for QIODevice, operator|, QIODevice::ReadOnly, QIODevice::Text, QIODevice::WriteOnly
#include <QList>                   // for QList
#include <QRegularExpression>      // for QRegularExpression
#include <QString>                 // for QString, operator+, operator==
#include <QStringList>             // for QStringList
#include <QTextStream>             // for QTextStream
#include <Qt>                      // for CaseInsensitive
#include <QtGlobal>                // for qRound, qPrintable

#include "defs.h"
#include "csv_util.h"              // for csv_stringtrim, dec_to_human, csv_stringclean, human_to_dec, ddmmdir_to_degrees, dec_to_intdeg, decdir_to_dec, intdeg_to_dec, csv_linesplit
#include "formspec.h"              // for FormatSpecificDataList
#include "garmin_fs.h"             // for garmin_fs_t
#include "geocache.h"              // for Geocache, Geocache::status_t, Geoc...
#include "grtcirc.h"               // for RAD, gcdist, radtometers
#include "jeeps/gpsmath.h"         // for GPS_Math_WGS84_To_UTM_EN, GPS_Lookup_Datum_Index, GPS_Math_Known_Datum_To_WGS84_M, GPS_Math_UTM_EN_To_Known_Datum, GPS_Math_WGS84_To_Known_Datum_M, GPS_Math_WGS84_To_UKOSMap_H
#include "jeeps/gpsport.h"         // for int32
#include "session.h"               // for session_t
#include "src/core/datetime.h"     // for DateTime
#include "src/core/logging.h"      // for FatalMsg
#include "src/core/textstream.h"   // for TextStream
#include "strptime.h"              // for strptime


const QHash<QString, XcsvStyle::xcsv_token> XcsvStyle::xcsv_tokens {
  { "ALT_FEET", XT_ALT_FEET },
  { "ALT_METERS", XT_ALT_METERS },
  { "ANYNAME", XT_ANYNAME },
  { "CADENCE", XT_CADENCE },
  { "CITY", XT_CITY },
  { "CONSTANT", XT_CONSTANT },
  { "COUNTRY", XT_COUNTRY },
  { "DESCRIPTION", XT_DESCRIPTION },
  { "EMAIL", XT_EMAIL },
  { "EXCEL_TIME", XT_EXCEL_TIME },
  { "FACILITY", XT_FACILITY },
  { "FILENAME", XT_FILENAME },
  { "FORMAT", XT_FORMAT },
  { "GEOCACHE_CONTAINER", XT_GEOCACHE_CONTAINER },
  { "GEOCACHE_DIFF", XT_GEOCACHE_DIFF },
  { "GEOCACHE_HINT", XT_GEOCACHE_HINT },
  { "GEOCACHE_ISARCHIVED", XT_GEOCACHE_ISARCHIVED },
  { "GEOCACHE_ISAVAILABLE", XT_GEOCACHE_ISAVAILABLE },
  { "GEOCACHE_LAST_FOUND", XT_GEOCACHE_LAST_FOUND },
  { "GEOCACHE_PLACER", XT_GEOCACHE_PLACER },
  { "GEOCACHE_TERR", XT_GEOCACHE_TERR },
  { "GEOCACHE_TYPE", XT_GEOCACHE_TYPE },
  { "GMT_TIME", XT_GMT_TIME },
  { "GPS_FIX", XT_GPS_FIX },
  { "GPS_HDOP", XT_GPS_HDOP },
  { "GPS_PDOP", XT_GPS_PDOP },
  { "GPS_SAT", XT_GPS_SAT },
  { "GPS_VDOP", XT_GPS_VDOP },
  { "HEART_RATE", XT_HEART_RATE },
  { "ICON_DESCR", XT_ICON_DESCR },
  { "IGNORE", XT_IGNORE },
  { "INDEX", XT_INDEX },
  { "ISO_TIME", XT_ISO_TIME },
  { "ISO_TIME_MS", XT_ISO_TIME_MS },
  { "LATLON_HUMAN_READABLE", XT_LATLON_HUMAN_READABLE },
  { "LAT_DDMMDIR", XT_LAT_DDMMDIR },
  { "LAT_DECIMAL", XT_LAT_DECIMAL },
  { "LAT_DECIMALDIR", XT_LAT_DECIMALDIR },
  { "LAT_DIR", XT_LAT_DIR },
  { "LAT_DIRDECIMAL", XT_LAT_DIRDECIMAL },
  { "LAT_HUMAN_READABLE", XT_LAT_HUMAN_READABLE },
  { "LAT_INT32DEG", XT_LAT_INT32DEG },
  { "LAT_NMEA", XT_LAT_NMEA },
  { "LOCAL_TIME", XT_LOCAL_TIME },
  { "LON_DDMMDIR", XT_LON_DDMMDIR },
  { "LON_DECIMAL", XT_LON_DECIMAL },
  { "LON_DECIMALDIR", XT_LON_DECIMALDIR },
  { "LON_DIR", XT_LON_DIR },
  { "LON_DIRDECIMAL", XT_LON_DIRDECIMAL },
  { "LON_HUMAN_READABLE", XT_LON_HUMAN_READABLE },
  { "LON_INT32DEG", XT_LON_INT32DEG },
  { "LON_NMEA", XT_LON_NMEA },
  { "MAP_EN_BNG", XT_MAP_EN_BNG },
  { "NET_TIME", XT_NET_TIME },
  { "NOTES", XT_NOTES },
  { "PATH_COURSE", XT_PATH_COURSE },
  { "PATH_DISTANCE_KM", XT_PATH_DISTANCE_KM },
  { "PATH_DISTANCE_METERS", XT_PATH_DISTANCE_METERS },
  { "PATH_DISTANCE_MILES", XT_PATH_DISTANCE_MILES },
  { "PATH_DISTANCE_NAUTICAL_MILES", XT_PATH_DISTANCE_NAUTICAL_MILES },
  { "PATH_SPEED", XT_PATH_SPEED },
  { "PATH_SPEED_KNOTS", XT_PATH_SPEED_KNOTS },
  { "PATH_SPEED_KPH", XT_PATH_SPEED_KPH },
  { "PATH_SPEED_MPH", XT_PATH_SPEED_MPH },
  { "PHONE_NR", XT_PHONE_NR },
  { "POSTAL_CODE", XT_POSTAL_CODE },
  { "POWER", XT_POWER },
  { "ROUTE_NAME", XT_ROUTE_NAME },
  { "SHORTNAME", XT_SHORTNAME },
  { "STATE", XT_STATE },
  { "STREET_ADDR", XT_STREET_ADDR },
  { "TEMPERATURE", XT_TEMPERATURE },
  { "TEMPERATURE_F", XT_TEMPERATURE_F },
  { "TIMET_TIME", XT_TIMET_TIME },
  { "TIMET_TIME_MS", XT_TIMET_TIME_MS },
  { "TRACK_NAME", XT_TRACK_NAME },
  { "TRACK_NEW", XT_TRACK_NEW },
  { "URL", XT_URL },
  { "URL_LINK_TEXT", XT_URL_LINK_TEXT },
  { "UTM", XT_UTM },
  { "UTM_EASTING", XT_UTM_EASTING },
  { "UTM_NORTHING", XT_UTM_NORTHING },
  { "UTM_ZONE", XT_UTM_ZONE },
  { "UTM_ZONEC", XT_UTM_ZONEC },
  { "UTM_ZONEF", XT_UTM_ZONEF },
  { "YYYYMMDD_TIME", XT_YYYYMMDD_TIME }
};

/* a table of config file constants mapped to chars */
const QHash<QString, QString> XcsvStyle::xcsv_char_table {
  { "COMMA",		"," 	},
  { "COMMASPACE",		", " 	},
  { "SINGLEQUOTE",	"'"	},
  { "DOUBLEQUOTE",	"\""	},
  { "COLON",		":"	},
  { "SEMICOLON",		";"	},
  { "NEWLINE",		"\n"	},
  { "CR",			"\r"	},
  { "CRNEWLINE",  	"\r\n"	},
  { "TAB",  		"\t"	},
  { "SPACE",  		" "	},
  { "HASH",  		"#"	},
  { "WHITESPACE",		"\\w"	},
  { "PIPE",		"|"	}
};

// Given a keyword of "COMMASPACE", return ", ".
QString
XcsvStyle::xcsv_get_char_from_constant_table(const QString& key)
{
  // No substitution found? Just return original.
  return xcsv_char_table.value(key, key);
}

// Remove outer quotes.
// Should probably be in csv_util.
QString XcsvStyle::dequote(const QString& in)
{
  QString r = in.simplified();
  if (r.startsWith("\"")) {
    r = r.mid(1);
  }
  if (r.endsWith("\"")) {
    r.chop(1);
  }
  return r;
}

void XcsvStyle::validate_fieldmap(const field_map& fmp, bool is_output)
{
  if (fmp.key.isEmpty()) {
    fatal(FatalMsg() << "xcsv style is missing" <<
          (is_output ? "output" : "input") << "field type.");
  }
  if (fmp.val.isNull()) {
    fatal(FatalMsg() << "xcsv style" << fmp.key.constData() << "is missing default.");
  }
  if (is_output && fmp.printfc.isNull()) {
    fatal(FatalMsg() << "xcsv style" << fmp.key.constData() << "output is missing format specifier.");
  }
}

/*****************************************************************************/
/* xcsv_ifield_add() - add input field to ifield queue.                      */
/* usage: xcsv_ifield_add("DESCRIPTION", "", "%s")                           */
/*****************************************************************************/
void
XcsvStyle::xcsv_ifield_add(XcsvStyle* style, const QString& qkey, const QString& qval, const QString& qpfc)
{
  QByteArray key = qkey.toUtf8();
  QByteArray val = qval.toUtf8();
  QByteArray pfc = qpfc.toUtf8();

  field_map fmp(key, val, pfc, xcsv_tokens.value(qkey, XT_unused));
  validate_fieldmap(fmp, false);

  style->ifields.append(fmp);
}

/*****************************************************************************/
/* xcsv_ofield_add() - add output field to ofield queue.                     */
/* usage: xcsv_ofield_add("LAT_DECIMAL", "", "%08.5lf")                      */
/*****************************************************************************/
void
XcsvStyle::xcsv_ofield_add(XcsvStyle* style, const QString& qkey, const QString& qval, const QString& qpfc, unsigned options)
{
  QByteArray key = qkey.toUtf8();
  QByteArray val = qval.toUtf8();
  QByteArray pfc = qpfc.toUtf8();

  field_map fmp(key, val, pfc, xcsv_tokens.value(qkey, XT_unused), options);
  validate_fieldmap(fmp, true);

  style->ofields.append(fmp);
}

QDate
XcsvFormat::yyyymmdd_to_time(const QString& s)
{
  return QDate::fromString(s, u"yyyyMMdd");
}

QDateTime
XcsvFormat::xcsv_adjust_time(const QDate date, const QTime time, bool is_localtime) const
{
  return make_datetime(date, time, is_localtime, opt_utc, utc_offset);
}

/*
 * sscanftime - Parse a date buffer using strftime format
 */
void
XcsvFormat::sscanftime(const char* s, const char* format, QDate& date, QTime& time)
{
  std::tm stm{};
  stm.tm_sec = -1;
  stm.tm_min = -1;
  stm.tm_hour = -1;
  stm.tm_mday = -1;
  stm.tm_mon = -1;
  stm.tm_year = -1;
  stm.tm_wday = -1;
  stm.tm_yday = -1;
  stm.tm_isdst = -1;

  if (strptime(s, format, &stm)) {

    std::optional<QTime> time_result;
    bool bad_time_parse = false;
    if (stm.tm_hour >= 0 && stm.tm_min >= 0 && stm.tm_sec >= 0) {
      time_result = QTime(stm.tm_hour, stm.tm_min, stm.tm_sec);
    } else if (stm.tm_hour >= 0 && stm.tm_min >= 0) {
      time_result = QTime(stm.tm_hour, stm.tm_min, 0);
    } else if (stm.tm_hour >= 0) {
      time_result = QTime(stm.tm_hour, 0, 0);
    } else if (!(stm.tm_hour == -1 && stm.tm_min == -1 && stm.tm_sec == -1)) {
      bad_time_parse = true;
    }
    if ((time_result.has_value() && !time_result->isValid()) || bad_time_parse) {
      fatal("couldn't parse time from string '%s' with format '%s'.\n",
            s, format);
    }
    if (time_result.has_value()) {
      time = *time_result;
    }

    std::optional<QDate> date_result;
    bool bad_date_parse = false;
    int year_result = (stm.tm_year >= 70)? stm.tm_year + 1900 : stm.tm_year + 2000;
    if (stm.tm_year >= 0 && stm.tm_mon >= 0 && stm.tm_mday >= 0) {
      date_result = QDate(year_result, stm.tm_mon + 1, stm.tm_mday);
    } else if (stm.tm_year >= 0 && stm.tm_mon >= 0) {
      date_result = QDate(year_result, stm.tm_mon + 1, 1);
    } else if (stm.tm_year >= 0) {
      date_result = QDate(year_result, 1, 1);
    } else if (!(stm.tm_year == -1 && stm.tm_mon == -1 && stm.tm_mday == -1)) {
      bad_date_parse = true;
    }
    if ((date_result.has_value() && !date_result->isValid()) || bad_date_parse) {
      fatal("couldn't parse date from string '%s' with format '%s'.\n",
            s, format);
    }
    if (date_result.has_value()) {
      date = *date_result;
    }
  } else {
    // Don't fuss for empty strings.
    if (*s) {
      warning("date parse of string '%s' with format '%s' failed.\n",
              s, format);
    }
  }
}

QString
XcsvFormat::writetime(const char* format, time_t t, bool gmt)
{
  static const std::tm* stmp;

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

QString
XcsvFormat::writetime(const char* format, const gpsbabel::DateTime& t, bool gmt)
{
  uint32_t tt = t.toTime_t();
  return (tt == 0xffffffffU)? QString() : writetime(format, tt, gmt);
}

long
XcsvFormat::time_to_yyyymmdd(const QDateTime& t)
{
  QDate d = t.toUTC().date();
  return d.year() * 10000 + d.month() * 100 + d.day();
}

garmin_fs_t*
XcsvFormat::gmsd_init(Waypoint* wpt)
{
  garmin_fs_t* gmsd = garmin_fs_t::find(wpt);
  if (gmsd == nullptr) {
    gmsd = new garmin_fs_t(-1);
    wpt->fs.FsChainAdd(gmsd);
  }
  return gmsd;
}

/*****************************************************************************/
/* xcsv_parse_val() - parse incoming data into the waypt structure.          */
/* usage: xcsv_parse_val("-123.34", *waypt, *field_map)                      */
/*****************************************************************************/
void
XcsvFormat::xcsv_parse_val(const QString& value, Waypoint* wpt, const XcsvStyle::field_map& fmp,
                           xcsv_parse_data* parse_data, const int line_no)
{
  QString enclosure = "";
  Geocache* gc_data = nullptr;

  if (fmp.printfc.isNull()) {
    fatal("xcsv style '%s' is missing format specifier", fmp.key.constData());
  }

  if (0 == strcmp(fmp.printfc.constData(), "\"%s\"")) {
    enclosure = "\"";
  }

  // TODO: eliminate this char string usage.
  QByteArray value_utf8 = value.toUtf8();
  const char* s = value_utf8.constData();

  switch (fmp.hashed_key) {
  case XcsvStyle::XT_IGNORE:
    /* IGNORE -- Categorically ignore this... */
    break;
  case XcsvStyle::XT_CONSTANT:
    /* CONSTANT -- Ignore on Input... */
    break;
  case XcsvStyle::XT_ANYNAME:
    /* ANYNAME -- Ignore -- this is output magic. */
    break;
  case XcsvStyle::XT_INDEX:
    /* IGNORE -- Calculated Sequence # For Output*/
    break;
  case XcsvStyle::XT_SHORTNAME:
    wpt->shortname = csv_stringtrim(value, enclosure, 0);
    break;
  case XcsvStyle::XT_DESCRIPTION:
    wpt->description = csv_stringtrim(value, enclosure, 0);
    break;
  case XcsvStyle::XT_NOTES:
    wpt->notes = value.trimmed();
    break;
  case XcsvStyle::XT_URL:
    if (!parse_data->link_) {
      parse_data->link_ = new UrlLink;
    }
    parse_data->link_->url_ = value.trimmed();
    break;
  case XcsvStyle::XT_URL_LINK_TEXT:
    if (!parse_data->link_) {
      parse_data->link_ = new UrlLink;
    }
    parse_data->link_->url_link_text_ = value.trimmed();
    break;
  case XcsvStyle::XT_ICON_DESCR:
    wpt->icon_descr = value.trimmed();
    break;

  /* LATITUDE CONVERSIONS**************************************************/
  case XcsvStyle::XT_LAT_DECIMAL:
    /* latitude as a pure decimal value */
    wpt->latitude = strtod(s, nullptr);
    break;
  case XcsvStyle::XT_LAT_DECIMALDIR:
  case XcsvStyle::XT_LAT_DIRDECIMAL:
    /* latitude as a decimal with N/S in it. */
    wpt->latitude = decdir_to_dec(s);
    break;
  case XcsvStyle::XT_LAT_INT32DEG:
    /* latitude as a 32 bit integer offset */
    wpt->latitude = intdeg_to_dec((int) strtod(s, nullptr));
    break;
  case XcsvStyle::XT_LAT_HUMAN_READABLE:
    human_to_dec(value, &wpt->latitude, &wpt->longitude, 1);
    break;
  case XcsvStyle::XT_LAT_DDMMDIR:
    wpt->latitude = ddmmdir_to_degrees(s);
    break;
  case XcsvStyle::XT_LAT_NMEA:
    wpt->latitude = ddmm2degrees(strtod(s, nullptr));
    break;
  // XT_LAT_10E is handled outside the switch.
  /* LONGITUDE CONVERSIONS ***********************************************/
  case XcsvStyle::XT_LON_DECIMAL:
    /* longitude as a pure decimal value */
    wpt->longitude = strtod(s, nullptr);
    break;
  case XcsvStyle::XT_LON_DECIMALDIR:
  case XcsvStyle::XT_LON_DIRDECIMAL:
    /* longitude as a decimal with N/S in it. */
    wpt->longitude = decdir_to_dec(s);
    break;
  case XcsvStyle::XT_LON_INT32DEG:
    /* longitude as a 32 bit integer offset  */
    wpt->longitude = intdeg_to_dec((int) strtod(s, nullptr));
    break;
  case XcsvStyle::XT_LON_HUMAN_READABLE:
    human_to_dec(value, &wpt->latitude, &wpt->longitude, 2);
    break;
  case XcsvStyle::XT_LON_DDMMDIR:
    wpt->longitude = ddmmdir_to_degrees(s);
    break;
  case XcsvStyle::XT_LON_NMEA:
    wpt->longitude = ddmm2degrees(strtod(s, nullptr));
    break;
  // case XcsvStyle::XT_LON_10E is handled outside the switch.
  /* LAT AND LON CONVERSIONS ********************************************/
  case XcsvStyle::XT_LATLON_HUMAN_READABLE:
    human_to_dec(value, &wpt->latitude, &wpt->longitude, 0);
    break;
  /* DIRECTIONS **********************************************************/
  case XcsvStyle::XT_LAT_DIR:
    /* latitude N/S. */
    if (*s == 'n' || *s == 'N') {
      parse_data->lat_dir_positive = true;
    } else if (*s == 's' || *s == 'S') {
      parse_data->lat_dir_positive = false;
    } else {
      warning("parse of string '%s' on line number %d as LAT_DIR failed.  Expected 'n', 'N', 's' or 'S'.\n", s, line_no);
    }
    break;
  case XcsvStyle::XT_LON_DIR:
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
  case XcsvStyle::XT_MAP_EN_BNG:
    parse_coordinates(s, kDatumOSGB36, grid_bng,
                      &wpt->latitude, &wpt->longitude);
    break;
  case XcsvStyle::XT_UTM_ZONE:
    parse_data->utm_zone = xstrtoi(s, nullptr, 10);
    break;
  case XcsvStyle::XT_UTM_ZONEC:
    parse_data->utm_zonec = s[0];
    break;
  case XcsvStyle::XT_UTM_ZONEF:
    parse_data->utm_zone = xstrtoi(s, nullptr, 10);
    parse_data->utm_zonec = s[strlen(s) - 1];
    break;
  case XcsvStyle::XT_UTM_EASTING:
    parse_data->utm_easting = strtod(s, nullptr);
    break;
  case XcsvStyle::XT_UTM_NORTHING:
    parse_data->utm_northing = strtod(s, nullptr);
    break;
  case XcsvStyle::XT_UTM: {
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
  case XcsvStyle::XT_ALT_FEET: {
    char* endptr;
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
  case XcsvStyle::XT_ALT_METERS: {
    char* endptr;
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
  case XcsvStyle::XT_PATH_SPEED:
    wpt->set_speed(strtod(s, nullptr));
    break;
  case XcsvStyle::XT_PATH_SPEED_KPH:
    wpt->set_speed(KPH_TO_MPS(strtod(s, nullptr)));
    break;
  case XcsvStyle::XT_PATH_SPEED_MPH:
    wpt->set_speed(MPH_TO_MPS(strtod(s, nullptr)));
    break;
  case XcsvStyle::XT_PATH_SPEED_KNOTS:
    wpt->set_speed(KNOTS_TO_MPS(strtod(s, nullptr)));
    break;
  case XcsvStyle::XT_PATH_COURSE:
    wpt->set_course(strtod(s, nullptr));
    break;

  /* TIME CONVERSIONS ***************************************************/
  case XcsvStyle::XT_EXCEL_TIME: {
    /* Time as Excel Time  */
    bool ok;
    double et = value.toDouble(&ok);
    if (ok) {
      wpt->SetCreationTime(0, excel_to_timetms(et));
      parse_data->need_datetime = false;
    } else if (!value.isEmpty()) {
      warning("parse of string '%s' on line number %d as EXCEL_TIME failed.\n", s, line_no);
    }
  }
  break;
  case XcsvStyle::XT_TIMET_TIME: {
    /* Time as time_t */
    bool ok;
    long long tt = value.toLongLong(&ok);
    if (ok) {
      wpt->SetCreationTime(tt);
      parse_data->need_datetime = false;
    } else if (!value.isEmpty()) {
      warning("parse of string '%s' on line number %d as TIMET_TIME failed.\n", s, line_no);
    }
  }
  break;
  case XcsvStyle::XT_TIMET_TIME_MS: {
    /* Time as time_t in milliseconds */
    bool ok;
    long long tt = value.toLongLong(&ok);
    if (ok) {
      wpt->SetCreationTime(0, tt);
      parse_data->need_datetime = false;
    } else if (!value.isEmpty()) {
      warning("parse of string '%s' on line number %d as TIMET_TIME_MS failed.\n", s, line_no);
    }
  }
  break;
  case XcsvStyle::XT_YYYYMMDD_TIME:
    parse_data->utc_date = yyyymmdd_to_time(value);
    break;
  case XcsvStyle::XT_GMT_TIME:
    sscanftime(s, fmp.printfc.constData(), parse_data->utc_date, parse_data->utc_time);
    break;
  case XcsvStyle::XT_LOCAL_TIME:
    sscanftime(s, fmp.printfc.constData(), parse_data->local_date, parse_data->local_time);
    break;
  case XcsvStyle::XT_ISO_TIME:
  case XcsvStyle::XT_ISO_TIME_MS:
    wpt->SetCreationTime(QDateTime::fromString(value, Qt::ISODateWithMs));
    parse_data->need_datetime = false;
    break;
  case XcsvStyle::XT_NET_TIME: {
    bool ok;
    long long dnt = value.toLongLong(&ok);
    if (ok) {
      wpt->SetCreationTime(dotnet_time_to_qdatetime(dnt));
      parse_data->need_datetime = false;
    } else if (!value.isEmpty()) {
      warning("parse of string '%s' on line number %d as NET_TIME failed.\n", s, line_no);
    }
  }
  break;
  case XcsvStyle::XT_GEOCACHE_LAST_FOUND: {
    QDate date = yyyymmdd_to_time(value);
    wpt->AllocGCData()->last_found = date.startOfDay();
    break;
  }

  /* GEOCACHING STUFF ***************************************************/
  case XcsvStyle::XT_GEOCACHE_DIFF:
    /* Geocache Difficulty as an int */
    wpt->AllocGCData()->diff = strtod(s, nullptr) * 10;
    break;
  case XcsvStyle::XT_GEOCACHE_TERR:
    /* Geocache Terrain as an int */
    wpt->AllocGCData()->terr = strtod(s, nullptr) * 10;
    break;
  case XcsvStyle::XT_GEOCACHE_TYPE:
    /* Geocache Type */
    wpt->AllocGCData()->set_type(value);
    break;
  case XcsvStyle::XT_GEOCACHE_CONTAINER:
    wpt->AllocGCData()->set_container(value);
    break;
  case XcsvStyle::XT_GEOCACHE_HINT:
    wpt->AllocGCData()->hint = value.trimmed();
    break;
  case XcsvStyle::XT_GEOCACHE_PLACER:
    wpt->AllocGCData()->placer = value.trimmed();
    break;
  case XcsvStyle::XT_GEOCACHE_ISAVAILABLE:
    gc_data = wpt->AllocGCData();
    if (value.trimmed().compare(u"False", Qt::CaseInsensitive) == 0) {
      gc_data->is_available = Geocache::status_t::gs_false;
    } else if (value.trimmed().compare(u"True", Qt::CaseInsensitive) == 0) {
      gc_data->is_available = Geocache::status_t::gs_true;
    } else {
      gc_data->is_available = Geocache::status_t::gs_unknown;
    }
    break;
  case XcsvStyle::XT_GEOCACHE_ISARCHIVED:
    gc_data = wpt->AllocGCData();
    if (value.trimmed().compare(u"False", Qt::CaseInsensitive) == 0) {
      gc_data->is_archived = Geocache::status_t::gs_false;
    } else if (value.trimmed().compare(u"True", Qt::CaseInsensitive) == 0) {
      gc_data->is_archived = Geocache::status_t::gs_true;
    } else {
      gc_data->is_archived = Geocache::status_t::gs_unknown;
    }
    break;

  /* GPS STUFF *******************************************************/
  case XcsvStyle::XT_GPS_HDOP:
    wpt->hdop = strtod(s, nullptr);
    break;
  case XcsvStyle::XT_GPS_VDOP:
    wpt->vdop = strtod(s, nullptr);
    break;
  case XcsvStyle::XT_GPS_PDOP:
    wpt->pdop = strtod(s, nullptr);
    break;
  case XcsvStyle::XT_GPS_SAT:
    wpt->sat = xstrtoi(s, nullptr, 10);
    break;
  case XcsvStyle::XT_GPS_FIX:
    wpt->fix = (fix_type)(xstrtoi(s, nullptr, 10)-(fix_type)1);
    if (wpt->fix < fix_2d) {
      if (!value.compare(u"none", Qt::CaseInsensitive)) {
        wpt->fix = fix_none;
      } else if (!value.compare(u"dgps", Qt::CaseInsensitive)) {
        wpt->fix = fix_dgps;
      } else if (!value.compare(u"pps", Qt::CaseInsensitive)) {
        wpt->fix = fix_pps;
      } else {
        wpt->fix = fix_unknown;
      }
    }
    break;
  /* Tracks and routes *********************************************/
  case XcsvStyle::XT_ROUTE_NAME:
    parse_data->rte_name = csv_stringtrim(value, enclosure, 0);
    break;
  case XcsvStyle::XT_TRACK_NEW:
    parse_data->new_track = xstrtoi(s, nullptr, 10);
    break;
  case XcsvStyle::XT_TRACK_NAME:
    parse_data->trk_name = csv_stringtrim(value, enclosure, 0);
    break;

  /* OTHER STUFF ***************************************************/
  case XcsvStyle::XT_PATH_DISTANCE_METERS:
    wpt->odometer_distance = strtod(s, nullptr);
    break;
  case XcsvStyle::XT_PATH_DISTANCE_KM:
    wpt->odometer_distance = strtod(s, nullptr) * 1000.0;
    break;
  case XcsvStyle::XT_PATH_DISTANCE_MILES:
    wpt->odometer_distance = MILES_TO_METERS(strtod(s, nullptr));
    break;
  case XcsvStyle::XT_PATH_DISTANCE_NAUTICAL_MILES:
    wpt->odometer_distance = NMILES_TO_METERS(strtod(s, nullptr));
    break;
  case XcsvStyle::XT_HEART_RATE:
    wpt->heartrate = xstrtoi(s, nullptr, 10);
    break;
  case XcsvStyle::XT_CADENCE:
    wpt->cadence = xstrtoi(s, nullptr, 10);
    break;
  case XcsvStyle::XT_POWER:
    wpt->power = strtod(s, nullptr);
    break;
  case XcsvStyle::XT_TEMPERATURE:
    wpt->set_temperature(strtod(s, nullptr));
    break;
  case XcsvStyle::XT_TEMPERATURE_F:
    wpt->set_temperature(FAHRENHEIT_TO_CELSIUS(strtod(s, nullptr)));
    break;
  /* GMSD ****************************************************************/
  case XcsvStyle::XT_COUNTRY: {
    garmin_fs_t* gmsd = gmsd_init(wpt);
    garmin_fs_t::set_country(gmsd, csv_stringtrim(value, enclosure, 0));
  }
  break;
  case XcsvStyle::XT_STATE: {
    garmin_fs_t* gmsd = gmsd_init(wpt);
    garmin_fs_t::set_state(gmsd, csv_stringtrim(value, enclosure, 0));
  }
  break;
  case XcsvStyle::XT_CITY: {
    garmin_fs_t* gmsd = gmsd_init(wpt);
    garmin_fs_t::set_city(gmsd, csv_stringtrim(value, enclosure, 0));
  }
  break;
  case XcsvStyle::XT_STREET_ADDR: {
    garmin_fs_t* gmsd = gmsd_init(wpt);
    garmin_fs_t::set_addr(gmsd, csv_stringtrim(value, enclosure, 0));
  }
  break;
  case XcsvStyle::XT_POSTAL_CODE: {
    garmin_fs_t* gmsd = gmsd_init(wpt);
    garmin_fs_t::set_postal_code(gmsd, csv_stringtrim(value, enclosure, 0));
  }
  break;
  case XcsvStyle::XT_PHONE_NR: {
    garmin_fs_t* gmsd = gmsd_init(wpt);
    garmin_fs_t::set_phone_nr(gmsd, csv_stringtrim(value, enclosure, 0));
  }
  break;
  case XcsvStyle::XT_FACILITY: {
    garmin_fs_t* gmsd = gmsd_init(wpt);
    garmin_fs_t::set_facility(gmsd, csv_stringtrim(value, enclosure, 0));
  }
  break;
  case XcsvStyle::XT_EMAIL: {
    garmin_fs_t* gmsd = gmsd_init(wpt);
    garmin_fs_t::set_email(gmsd, csv_stringtrim(value, enclosure, 0));
  }
  break;
  case XcsvStyle::XT_unused:
    if (strncmp(fmp.key.constData(), "LON_10E", 7) == 0) {
      wpt->longitude = strtod(s, nullptr) / pow(10.0, strtod(fmp.key.constData()+7, nullptr));
    } else if (strncmp(fmp.key.constData(), "LAT_10E", 7) == 0) {
      wpt->latitude = strtod(s, nullptr) / pow(10.0, strtod(fmp.key.constData()+7, nullptr));
    } else {
      warning("Unknown style directive: %s\n", fmp.key.constData());
    }
    break;

  default:
    fatal("Unknown style directive: %s - %d\n", fmp.key.constData(), fmp.hashed_key);
    break;
  }
}

/*****************************************************************************/
/* read() - read input file, parsing lines, fields and handling              */
/*                   any data conversion (the input meat)                    */
/*****************************************************************************/
void
XcsvFormat::read()
{
  int linecount = 0;
  route_head* rte = nullptr;
  route_head* trk = nullptr;

  while (true) {
    QString buff = xcsv_file->stream.readLine();
    if (buff.isNull()) {
      break;
    }
    linecount++;
    /* Whack trailing space; leading space may matter if our field sep
     * is whitespace and we have leading whitespace.
     */
    // This could be hoisted out as a generic rtrim() if we need such a thing.
    while (buff.size() > 0 && buff.at(buff.size() - 1).isSpace()) {
      buff.chop(1);
    }

    /* skip over x many lines on the top for the prologue... */
    if ((linecount - 1) < xcsv_style->prologue.count()) {
      continue;
    }

    /* We should skip over epilogue lines also.  Since we don't want to
     * pre-read the file to know how many data lines we should be seeing,
     * we take this cheap shot at the data and cross our fingers.
     */
    for (const auto& ogp : std::as_const(xcsv_style->epilogue)) {
      if (ogp.startsWith(buff)) {
        buff.clear();
        break;
      }
    }
    if (!buff.isEmpty()) {
      auto* wpt_tmp = new Waypoint;
      // initialize parse data for accumulation of line results from all fields in this line.
      xcsv_parse_data parse_data;
      const QStringList values = csv_linesplit(buff, xcsv_style->field_delimiter,
                                 xcsv_style->field_encloser, linecount);

      if (xcsv_style->ifields.isEmpty()) {
        fatal("attempt to read, but style '%s' has no IFIELDs in it.\n", qPrintable(xcsv_style->description)? qPrintable(xcsv_style->description) : "unknown");
      }

      int ifield_idx = 0;

      /* now rip the line apart */
      for (const auto& value : values) {
        const XcsvStyle::field_map& fmp = xcsv_style->ifields.at(ifield_idx++);
        xcsv_parse_val(value, wpt_tmp, fmp, &parse_data, linecount);

        if (ifield_idx >= xcsv_style->ifields.size()) {
          /* no more fields, stop parsing! */
          break;
        }
      }


      if (parse_data.need_datetime) {
        if (parse_data.utc_date.isValid() && parse_data.utc_time.isValid()) {
          wpt_tmp->SetCreationTime(xcsv_adjust_time(parse_data.utc_date, parse_data.utc_time, false));
        } else if (parse_data.local_date.isValid() && parse_data.local_time.isValid()) {
          wpt_tmp->SetCreationTime(xcsv_adjust_time(parse_data.local_date, parse_data.local_time, true));
        } else if (parse_data.utc_date.isValid()) {
          wpt_tmp->SetCreationTime(xcsv_adjust_time(parse_data.utc_date, parse_data.utc_time, false));
        } else if (parse_data.local_date.isValid()) {
          wpt_tmp->SetCreationTime(xcsv_adjust_time(parse_data.local_date, parse_data.local_time, true));
        } else if (parse_data.utc_time.isValid()) {
          wpt_tmp->SetCreationTime(xcsv_adjust_time(parse_data.utc_date, parse_data.utc_time, false));
        } else if (parse_data.local_time.isValid()) {
          wpt_tmp->SetCreationTime(xcsv_adjust_time(parse_data.local_date, parse_data.local_time, true));
        }
      }

      // If XT_LAT_DIR(XT_LON_DIR) was an input field, and the latitude(longitude) is positive,
      // assume the latitude(longitude) was the absolute value and take the sign from XT_LAT_DIR(XT_LON_DIR).
      if (parse_data.lat_dir_positive.has_value() && !(*parse_data.lat_dir_positive) && (wpt_tmp->latitude > 0.0)) {
        wpt_tmp->latitude = -wpt_tmp->latitude;
      }
      if (parse_data.lon_dir_positive.has_value() && !(*parse_data.lon_dir_positive) && (wpt_tmp->longitude > 0.0)) {
        wpt_tmp->longitude = -wpt_tmp->longitude;
      }

      if ((xcsv_file->gps_datum_idx > -1) && (xcsv_file->gps_datum_idx != kDatumWGS84)) {
        double alt;
        GPS_Math_Known_Datum_To_WGS84_M(wpt_tmp->latitude, wpt_tmp->longitude, 0.0,
                                        &wpt_tmp->latitude, &wpt_tmp->longitude, &alt, xcsv_file->gps_datum_idx);
      }

      if (parse_data.utm_easting || parse_data.utm_northing) {
        GPS_Math_UTM_EN_To_Known_Datum(&wpt_tmp->latitude,
                                       &wpt_tmp->longitude,
                                       parse_data.utm_easting, parse_data.utm_northing,
                                       parse_data.utm_zone, parse_data.utm_zonec,
                                       kDatumWGS84);
      }

      if (parse_data.link_) {
        wpt_tmp->AddUrlLink(*parse_data.link_);
        delete parse_data.link_;
        parse_data.link_ = nullptr;
      }

      switch (xcsv_style->datatype) {
      case unknown_gpsdata:
      case wptdata:
        waypt_add(wpt_tmp);
        break;
      case trkdata:
        if ((trk == nullptr) || parse_data.new_track) {
          trk = new route_head;
          track_add_head(trk);
        }
        if (!parse_data.trk_name.isEmpty()) {
          trk->rte_name = parse_data.trk_name;
        }
        track_add_wpt(trk, wpt_tmp);
        break;
      case rtedata:
        if (rte == nullptr) {
          rte = new route_head;
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

void
XcsvFormat::xcsv_resetpathlen(const route_head* head)
{
  pathdist = 0;
  old_position.reset();
  csv_route = csv_track = nullptr;
  switch (xcsv_style->datatype) {
  case trkdata:
    csv_track = head;
    break;
  case rtedata:
    csv_route = head;
    break;
  default:
    break;
  }
}

/*****************************************************************************/
/* xcsv_waypt_pr() - write output file, handling output conversions          */
/*                  (the output meat)                                        */
/*****************************************************************************/
void
XcsvFormat::xcsv_waypt_pr(const Waypoint* wpt)
{
  QString buff;
  double latitude;
  double longitude;
  int32_t utmz;
  double utme;
  double utmn;
  char utmzc;

  if (old_position) {
    pathdist += radtometers(gcdist(old_position.value(),
                                   wpt->position()));
  }
  old_position = wpt->position();
  latitude = wpt->latitude;
  longitude = wpt->longitude;

  QString write_delimiter;
  if (xcsv_style->field_delimiter == u"\\w") {
    write_delimiter = " ";
  } else {
    write_delimiter = xcsv_style->field_delimiter;
  }

  QString description;
  QString shortname;
  if (wpt->shortname.isEmpty() || global_opts.synthesize_shortnames) {
    if (!wpt->description.isEmpty()) {
      if (global_opts.synthesize_shortnames) {
        shortname = xcsv_file->mkshort_handle.mkshort_from_wpt(wpt);
      } else {
        shortname = csv_stringclean(wpt->description, xcsv_style->badchars);
      }
    } else {
      /* no shortname available -- let shortname default on output */
    }
  } else {
    shortname = csv_stringclean(wpt->shortname, xcsv_style->badchars);
  }
  if (wpt->description.isEmpty()) {
    if (!shortname.isEmpty()) {
      description = csv_stringclean(shortname, xcsv_style->badchars);
    } else {
      /* no description -- let description default on output */
    }
  } else {
    description = csv_stringclean(wpt->description, xcsv_style->badchars);
  }

  if (prefer_shortnames) {
    description = shortname;
  }

  if ((xcsv_file->gps_datum_idx > -1) && (xcsv_file->gps_datum_idx != kDatumWGS84)) {
    double alt;
    GPS_Math_WGS84_To_Known_Datum_M(latitude, longitude, 0.0,
                                    &latitude, &longitude, &alt, xcsv_file->gps_datum_idx);
  }

  int i = 0;
  for (const auto& fmp : std::as_const(xcsv_style->ofields)) {
    double lat = latitude;
    double lon = longitude;
    /*
     * A klunky concept.   This should evaluate to true for any
     * field if we think we don't have realistic value for it.
     * This is used by the 'optional' attribute for suppressing
     * fields on output.
     */
    int field_is_unknown = 0;

    if ((i != 0) && !(fmp.options & XcsvStyle::options_nodelim)) {
      xcsv_file->stream << write_delimiter;
    }

    if (fmp.options & XcsvStyle::options_absolute) {
      lat = fabs(lat);
      lon = fabs(lon);
    }

    i++;

    switch (fmp.hashed_key) {
    case XcsvStyle::XT_IGNORE:
      /* IGNORE -- Write the char printf conversion */
      buff = QString::asprintf(fmp.printfc.constData(), "");
      break;
    case XcsvStyle::XT_INDEX:
      buff = QString::asprintf(fmp.printfc.constData(), waypt_out_count + xstrtoi(fmp.val.constData(), nullptr, 10));
      break;
    case XcsvStyle::XT_CONSTANT: {
      auto cp = XcsvStyle::xcsv_get_char_from_constant_table(fmp.val.constData());
      if (!cp.isEmpty()) {
        buff = QString::asprintf(fmp.printfc.constData(), CSTR(cp));
      } else {
        buff = QString::asprintf(fmp.printfc.constData(), fmp.val.constData());
      }
    }
    break;
    case XcsvStyle::XT_SHORTNAME:
      buff = QString::asprintf(fmp.printfc.constData(),
                               shortname.isEmpty() ? fmp.val.constData() : CSTR(shortname));

      break;
    case XcsvStyle::XT_ANYNAME: {
      QString anyname = wpt->shortname;
      if (anyname.isEmpty()) {
        anyname = xcsv_file->mkshort_handle.mkshort(wpt->description);
      }
      if (anyname.isEmpty()) {
        anyname = xcsv_file->mkshort_handle.mkshort(wpt->description);
      }
      if (anyname.isEmpty()) {
        anyname = wpt->notes;
      }
      if (anyname.isEmpty()) {
        anyname = fmp.val.constData();
      }
      buff = QString::asprintf(fmp.printfc.constData(), CSTR(anyname));
    }

    break;
    case XcsvStyle::XT_DESCRIPTION:
      buff = QString::asprintf(fmp.printfc.constData(),
                               description.isEmpty() ? fmp.val.constData() : CSTR(description));
      break;
    case XcsvStyle::XT_NOTES:
      buff = QString::asprintf(fmp.printfc.constData(),
                               wpt->notes.isEmpty() ? fmp.val.constData() : CSTR(wpt->notes));
      break;
    case XcsvStyle::XT_URL: {
      if (xcsv_urlbase) {
        buff = xcsv_urlbase;
      }
      if (wpt->HasUrlLink()) {
        const UrlLink& l = wpt->GetUrlLink();
        buff += QString::asprintf(fmp.printfc.constData(), CSTR(l.url_));
      } else {
        buff += QString::asprintf(fmp.printfc.constData(), fmp.val.constData() && *fmp.val.constData() ? fmp.val.constData() : "\"\"");
      }
    }
    break;
    case XcsvStyle::XT_URL_LINK_TEXT:
      if (wpt->HasUrlLink()) {
        const UrlLink& l = wpt->GetUrlLink();
        buff = QString::asprintf(fmp.printfc.constData(),
                                 !l.url_link_text_.isEmpty() ? CSTR(l.url_link_text_) : fmp.val.constData());
      }
      break;
    case XcsvStyle::XT_ICON_DESCR:
      buff = QString::asprintf(fmp.printfc.constData(),
                               (!wpt->icon_descr.isNull()) ?
                               CSTR(wpt->icon_descr) : fmp.val.constData());
      break;

    /* LATITUDE CONVERSION***********************************************/
    case XcsvStyle::XT_LAT_DECIMAL:
      /* latitude as a pure decimal value */
      buff = QString::asprintf(fmp.printfc.constData(), lat);
      break;
    case XcsvStyle::XT_LAT_DECIMALDIR:
      /* latitude as a decimal value with N/S after it */
      buff = QString::asprintf(fmp.printfc.constData(), fabs(lat),
                               lat_dir(lat));
      break;
    case XcsvStyle::XT_LAT_DIRDECIMAL:
      /* latitude as a decimal value with N/S before it */
      buff = QString::asprintf(fmp.printfc.constData(),
                               lat_dir(lat),
                               fabs(lat));
      break;
    case XcsvStyle::XT_LAT_INT32DEG:
      /* latitude as an integer offset from 0 degrees */
      buff = QString::asprintf(fmp.printfc.constData(),
                               dec_to_intdeg(lat));
      break;
    case XcsvStyle::XT_LAT_DDMMDIR:
      /*latitude as (degrees * 100) + decimal minutes, with N/S after it */
      buff = dec_to_human(fmp.printfc.constData(), "SN", degrees2ddmm(lat));
      break;
    case XcsvStyle::XT_LAT_HUMAN_READABLE:
      buff = dec_to_human(fmp.printfc.constData(), "SN", lat);
      break;
    case XcsvStyle::XT_LAT_NMEA:
      buff = QString::asprintf(fmp.printfc.constData(), degrees2ddmm(lat));
      break;
    // case XcsvStyle::XT_LAT_10E is handled outside the switch.
    /* LONGITUDE CONVERSIONS*********************************************/
    case XcsvStyle::XT_LON_DECIMAL:
      /* longitude as a pure decimal value */
      buff = QString::asprintf(fmp.printfc.constData(), lon);
      break;
    case XcsvStyle::XT_LON_DECIMALDIR:
      /* latitude as a decimal value with N/S after it */
      buff = QString::asprintf(fmp.printfc.constData(),
                               fabs(lon),
                               lon_dir(lon));
      break;
    case XcsvStyle::XT_LON_DIRDECIMAL:
      /* latitude as a decimal value with N/S before it */
      buff = QString::asprintf(fmp.printfc.constData(),
                               lon_dir(lon),
                               fabs(lon));
      break;
    case XcsvStyle::XT_LON_INT32DEG:
      /* longitude as an integer offset from 0 degrees */
      buff = QString::asprintf(fmp.printfc.constData(),
                               dec_to_intdeg(lon));
      break;
    case XcsvStyle::XT_LON_DDMMDIR:
      /* longitude as (degrees * 100) + decimal minutes, with W/E after it*/
      buff = dec_to_human(fmp.printfc.constData(), "WE", degrees2ddmm(lon));
      break;
    case XcsvStyle::XT_LON_HUMAN_READABLE:
      buff = dec_to_human(fmp.printfc.constData(), "WE", lon);
      break;
    case XcsvStyle::XT_LATLON_HUMAN_READABLE:
      buff = dec_to_human(fmp.printfc.constData(), "SN", lat);
      buff += " ";
      buff += dec_to_human(fmp.printfc.constData(), "WE", lon);
      // Tidy up leading, trailing, middle whitespace.
      buff = buff.simplified();
      break;
    case XcsvStyle::XT_LON_NMEA:
      buff = QString::asprintf(fmp.printfc.constData(), degrees2ddmm(lon));
      break;
    // case XcsvStyle::XT_LON_10E is handled outside the switch.
    /* DIRECTIONS *******************************************************/
    case XcsvStyle::XT_LAT_DIR:
      /* latitude N/S as a char */
      buff = QString::asprintf(fmp.printfc.constData(),
                               lat_dir(lat));
      break;
    case XcsvStyle::XT_LON_DIR:
      /* longitude E/W as a char */
      buff = QString::asprintf(fmp.printfc.constData(),
                               lon_dir(lon));
      break;

    /* SPECIAL COORDINATES */
    case XcsvStyle::XT_MAP_EN_BNG: {
      char map[3];
      double north;
      double east;
      if (! GPS_Math_WGS84_To_UKOSMap_H(wpt->latitude, wpt->longitude, &east, &north, map))
        fatal("Position (%.5f/%.5f) outside of BNG.\n",
              wpt->latitude, wpt->longitude);
      buff = QString::asprintf(fmp.printfc.constData(), map, qRound(east), qRound(north));
    }
    break;
    case XcsvStyle::XT_UTM: {
      char tbuf[100];
      GPS_Math_WGS84_To_UTM_EN(wpt->latitude, wpt->longitude,
                               &utme, &utmn, &utmz, &utmzc);
      snprintf(tbuf, sizeof(tbuf), "%d%c %6.0f %7.0f",
               utmz, utmzc, utme, utmn);
      buff = QString::asprintf(fmp.printfc.constData(), tbuf);
    }
    break;
    case XcsvStyle::XT_UTM_ZONE:
      GPS_Math_WGS84_To_UTM_EN(wpt->latitude, wpt->longitude,
                               &utme, &utmn, &utmz, &utmzc);
      buff = QString::asprintf(fmp.printfc.constData(), utmz);
      break;
    case XcsvStyle::XT_UTM_ZONEC:
      GPS_Math_WGS84_To_UTM_EN(wpt->latitude, wpt->longitude,
                               &utme, &utmn, &utmz, &utmzc);
      buff = QString::asprintf(fmp.printfc.constData(), utmzc);
      break;
    case XcsvStyle::XT_UTM_ZONEF: {
      char tbuf[10];
      GPS_Math_WGS84_To_UTM_EN(wpt->latitude, wpt->longitude,
                               &utme, &utmn, &utmz, &utmzc);
      tbuf[0] = 0;
      snprintf(tbuf, sizeof(tbuf), "%d%c", utmz, utmzc);
      buff = QString::asprintf(fmp.printfc.constData(), tbuf);
    }
    break;
    case XcsvStyle::XT_UTM_NORTHING:
      GPS_Math_WGS84_To_UTM_EN(wpt->latitude, wpt->longitude,
                               &utme, &utmn, &utmz, &utmzc);
      buff = QString::asprintf(fmp.printfc.constData(), utmn);
      break;
    case XcsvStyle::XT_UTM_EASTING:
      GPS_Math_WGS84_To_UTM_EN(wpt->latitude, wpt->longitude,
                               &utme, &utmn, &utmz, &utmzc);
      buff = QString::asprintf(fmp.printfc.constData(), utme);
      break;

    /* ALTITUDE CONVERSIONS**********************************************/
    case XcsvStyle::XT_ALT_FEET:
      /* altitude in feet as a decimal value */
      if (wpt->altitude != unknown_alt) {
        buff = QString::asprintf(fmp.printfc.constData(),
                                 METERS_TO_FEET(wpt->altitude));
      }
      break;
    case XcsvStyle::XT_ALT_METERS:
      /* altitude in meters as a decimal value */
      if (wpt->altitude != unknown_alt) {
        buff = QString::asprintf(fmp.printfc.constData(),
                                 wpt->altitude);
      }
      break;

    /* DISTANCE CONVERSIONS**********************************************/
    /* prefer odometer distance. */
    /* if not available, use calculated distance from positions */
    case XcsvStyle::XT_PATH_DISTANCE_MILES:
      /* path (route/track) distance in miles */
      if (wpt->odometer_distance) {
        buff = QString::asprintf(fmp.printfc.constData(), METERS_TO_MILES(wpt->odometer_distance));
      } else {
        buff = QString::asprintf(fmp.printfc.constData(), METERS_TO_MILES(pathdist));
      }
      break;
    case XcsvStyle::XT_PATH_DISTANCE_NAUTICAL_MILES:
      /* path (route/track) distance in miles */
      if (wpt->odometer_distance) {
        buff = QString::asprintf(fmp.printfc.constData(), METERS_TO_NMILES(wpt->odometer_distance));
      } else {
        buff = QString::asprintf(fmp.printfc.constData(), METERS_TO_NMILES(pathdist));
      }
      break;
    case XcsvStyle::XT_PATH_DISTANCE_METERS:
      /* path (route/track) distance in meters */
      if (wpt->odometer_distance) {
        buff = QString::asprintf(fmp.printfc.constData(), wpt->odometer_distance);
      } else {
        buff = QString::asprintf(fmp.printfc.constData(), pathdist);
      }
      break;
    case XcsvStyle::XT_PATH_DISTANCE_KM:
      /* path (route/track) distance in kilometers */
      if (wpt->odometer_distance) {
        buff = QString::asprintf(fmp.printfc.constData(), wpt->odometer_distance / 1000.0);
      } else {
        buff = QString::asprintf(fmp.printfc.constData(), pathdist / 1000.0);
      }
      break;
    case XcsvStyle::XT_PATH_SPEED:
      if (wpt->speed_has_value()) {
        buff = QString::asprintf(fmp.printfc.constData(), wpt->speed_value());
      }
      break;
    case XcsvStyle::XT_PATH_SPEED_KPH:
      if (wpt->speed_has_value()) {
        buff = QString::asprintf(fmp.printfc.constData(), MPS_TO_KPH(wpt->speed_value()));
      }
      break;
    case XcsvStyle::XT_PATH_SPEED_MPH:
      if (wpt->speed_has_value()) {
        buff = QString::asprintf(fmp.printfc.constData(), MPS_TO_MPH(wpt->speed_value()));
      }
      break;
    case XcsvStyle::XT_PATH_SPEED_KNOTS:
      if (wpt->speed_has_value()) {
        buff = QString::asprintf(fmp.printfc.constData(), MPS_TO_KNOTS(wpt->speed_value()));
      }
      break;
    case XcsvStyle::XT_PATH_COURSE:
      if (wpt->course_has_value()) {
        buff = QString::asprintf(fmp.printfc.constData(), wpt->course_value());
      }
      break;

    /* HEART RATE CONVERSION***********************************************/
    case XcsvStyle::XT_HEART_RATE:
      if (wpt->heartrate) {
        buff = QString::asprintf(fmp.printfc.constData(), wpt->heartrate);
      }
      break;
    /* CADENCE CONVERSION***********************************************/
    case XcsvStyle::XT_CADENCE:
      if (wpt->cadence) {
        buff = QString::asprintf(fmp.printfc.constData(), wpt->cadence);
      }
      break;
    /* POWER CONVERSION***********************************************/
    case XcsvStyle::XT_POWER:
      if (wpt->power) {
        buff = QString::asprintf(fmp.printfc.constData(), wpt->power);
      }
      break;
    case XcsvStyle::XT_TEMPERATURE:
      if (wpt->temperature_has_value()) {
        buff = QString::asprintf(fmp.printfc.constData(), wpt->temperature_value());
      }
      break;
    case XcsvStyle::XT_TEMPERATURE_F:
      if (wpt->temperature_has_value()) {
        buff = QString::asprintf(fmp.printfc.constData(), CELSIUS_TO_FAHRENHEIT(wpt->temperature_value()));
      }
      break;
    /* TIME CONVERSIONS**************************************************/
    case XcsvStyle::XT_EXCEL_TIME:
      /* creation time as an excel (double) time */
      if (wpt->GetCreationTime().isValid()) {
        buff = QString::asprintf(fmp.printfc.constData(), timetms_to_excel(wpt->GetCreationTime().toMSecsSinceEpoch()));
      }
      break;
    case XcsvStyle::XT_TIMET_TIME:
      /* time as a time_t variable in seconds */
      if (wpt->GetCreationTime().isValid()) {
        buff = QString::asprintf(fmp.printfc.constData(), wpt->GetCreationTime().toSecsSinceEpoch());
      }
      break;
    case XcsvStyle::XT_TIMET_TIME_MS:
      /* time as a time_t variable in milliseconds */
      if (wpt->GetCreationTime().isValid()) {
        buff = QString::asprintf(fmp.printfc.constData(), wpt->GetCreationTime().toMSecsSinceEpoch());
      }
      break;
    case XcsvStyle::XT_YYYYMMDD_TIME:
      if (wpt->GetCreationTime().isValid()) {
        buff = QString::asprintf(fmp.printfc.constData(), time_to_yyyymmdd(wpt->GetCreationTime()));
      }
      break;
    case XcsvStyle::XT_GMT_TIME:
      buff = writetime(fmp.printfc.constData(), wpt->GetCreationTime(), true);
      break;
    case XcsvStyle::XT_LOCAL_TIME:
      buff = writetime(fmp.printfc.constData(), wpt->GetCreationTime(), false);
      break;
    case XcsvStyle::XT_ISO_TIME:
      if (wpt->GetCreationTime().isValid()) {
        buff = wpt->GetCreationTime().toUTC().toString(Qt::ISODate);
      }
      break;
    case XcsvStyle::XT_ISO_TIME_MS:
      if (wpt->GetCreationTime().isValid()) {
        buff = wpt->GetCreationTime().toPrettyString();
      }
      break;
    case XcsvStyle::XT_NET_TIME:
      if (wpt->GetCreationTime().isValid()) {
        buff = QString::number(qdatetime_to_dotnet_time(wpt->GetCreationTime()));
      }
      break;
    case XcsvStyle::XT_GEOCACHE_LAST_FOUND:
      buff = QString::asprintf(fmp.printfc.constData(), time_to_yyyymmdd(wpt->gc_data->last_found));
      break;
    /* GEOCACHE STUFF **************************************************/
    case XcsvStyle::XT_GEOCACHE_DIFF:
      /* Geocache Difficulty as a double */
      buff = QString::asprintf(fmp.printfc.constData(), wpt->gc_data->diff / 10.0);
      field_is_unknown = !wpt->gc_data->diff;
      break;
    case XcsvStyle::XT_GEOCACHE_TERR:
      /* Geocache Terrain as a double */
      buff = QString::asprintf(fmp.printfc.constData(), wpt->gc_data->terr / 10.0);
      field_is_unknown = !wpt->gc_data->terr;
      break;
    case XcsvStyle::XT_GEOCACHE_CONTAINER:
      /* Geocache Container */
      buff = QString::asprintf(fmp.printfc.constData(), CSTR(wpt->gc_data->get_container()));
      field_is_unknown = wpt->gc_data->container == Geocache::container_t::gc_unknown;
      break;
    case XcsvStyle::XT_GEOCACHE_TYPE:
      /* Geocache Type */
      buff = QString::asprintf(fmp.printfc.constData(), CSTR(wpt->gc_data->get_type()));
      field_is_unknown = wpt->gc_data->type == Geocache::type_t::gt_unknown;
      break;
    case XcsvStyle::XT_GEOCACHE_HINT:
      buff = QString::asprintf(fmp.printfc.constData(), CSTR(wpt->gc_data->hint));
      field_is_unknown = !wpt->gc_data->hint.isEmpty();
      break;
    case XcsvStyle::XT_GEOCACHE_PLACER:
      buff = QString::asprintf(fmp.printfc.constData(), CSTR(wpt->gc_data->placer));
      field_is_unknown = !wpt->gc_data->placer.isEmpty();
      break;
    case XcsvStyle::XT_GEOCACHE_ISAVAILABLE:
      if (wpt->gc_data->is_available == Geocache::status_t::gs_false) {
        buff = QString::asprintf(fmp.printfc.constData(), "False");
      } else if (wpt->gc_data->is_available == Geocache::status_t::gs_true) {
        buff = QString::asprintf(fmp.printfc.constData(), "True");
      } else {
        buff = QString::asprintf(fmp.printfc.constData(), "Unknown");
      }
      break;
    case XcsvStyle::XT_GEOCACHE_ISARCHIVED:
      if (wpt->gc_data->is_archived == Geocache::status_t::gs_false) {
        buff = QString::asprintf(fmp.printfc.constData(), "False");
      } else if (wpt->gc_data->is_archived == Geocache::status_t::gs_true) {
        buff = QString::asprintf(fmp.printfc.constData(), "True");
      } else {
        buff = QString::asprintf(fmp.printfc.constData(), "Unknown");
      }
      break;
    /* Tracks and Routes ***********************************************/
    case XcsvStyle::XT_TRACK_NEW:
      if (csv_track) {
        if (wpt->wpt_flags.new_trkseg) {
          buff = QString::asprintf(fmp.printfc.constData(), 1);
        } else {
          buff = QString::asprintf(fmp.printfc.constData(), 0);
        }
      }
      break;
    case XcsvStyle::XT_TRACK_NAME:
      if (csv_track) {
        buff = QString::asprintf(fmp.printfc.constData(), CSTR(csv_track->rte_name));
      }
      break;
    case XcsvStyle::XT_ROUTE_NAME:
      if (csv_route) {
        buff = QString::asprintf(fmp.printfc.constData(), CSTR(csv_route->rte_name));
      }
      break;

    /* GPS STUFF *******************************************************/
    case XcsvStyle::XT_GPS_HDOP:
      buff = QString::asprintf(fmp.printfc.constData(), wpt->hdop);
      field_is_unknown = !wpt->hdop;
      break;
    case XcsvStyle::XT_GPS_VDOP:
      buff = QString::asprintf(fmp.printfc.constData(), wpt->vdop);
      field_is_unknown = !wpt->vdop;
      break;
    case XcsvStyle::XT_GPS_PDOP:
      buff = QString::asprintf(fmp.printfc.constData(), wpt->pdop);
      field_is_unknown = !wpt->pdop;
      break;
    case XcsvStyle::XT_GPS_SAT:
      buff = QString::asprintf(fmp.printfc.constData(), wpt->sat);
      field_is_unknown = !wpt->sat;
      break;
    case XcsvStyle::XT_GPS_FIX: {
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
      buff = QString::asprintf(fmp.printfc.constData(), fix);
    }
    break;
    /* GMSD ************************************************************/
    case XcsvStyle::XT_COUNTRY: {
      const garmin_fs_t* gmsd = garmin_fs_t::find(wpt);
      buff = QString::asprintf(fmp.printfc.constData(), CSTR(garmin_fs_t::get_country(gmsd, "")));
    }
    break;
    case XcsvStyle::XT_STATE: {
      const garmin_fs_t* gmsd = garmin_fs_t::find(wpt);
      buff = QString::asprintf(fmp.printfc.constData(), CSTR(garmin_fs_t::get_state(gmsd, "")));
    }
    break;
    case XcsvStyle::XT_CITY: {
      const garmin_fs_t* gmsd = garmin_fs_t::find(wpt);
      buff = QString::asprintf(fmp.printfc.constData(), CSTR(garmin_fs_t::get_city(gmsd, "")));
    }
    break;
    case XcsvStyle::XT_POSTAL_CODE: {
      const garmin_fs_t* gmsd = garmin_fs_t::find(wpt);
      buff = QString::asprintf(fmp.printfc.constData(), CSTR(garmin_fs_t::get_postal_code(gmsd, "")));
    }
    break;
    case XcsvStyle::XT_STREET_ADDR: {
      const garmin_fs_t* gmsd = garmin_fs_t::find(wpt);
      buff = QString::asprintf(fmp.printfc.constData(), CSTR(garmin_fs_t::get_addr(gmsd, "")));
    }
    break;
    case XcsvStyle::XT_PHONE_NR: {
      const garmin_fs_t* gmsd = garmin_fs_t::find(wpt);
      buff = QString::asprintf(fmp.printfc.constData(), CSTR(garmin_fs_t::get_phone_nr(gmsd, "")));
    }
    break;
    case XcsvStyle::XT_FACILITY: {
      const garmin_fs_t* gmsd = garmin_fs_t::find(wpt);
      buff = QString::asprintf(fmp.printfc.constData(), CSTR(garmin_fs_t::get_facility(gmsd, "")));
    }
    break;
    case XcsvStyle::XT_EMAIL: {
      const garmin_fs_t* gmsd = garmin_fs_t::find(wpt);
      buff = QString::asprintf(fmp.printfc.constData(), CSTR(garmin_fs_t::get_email(gmsd, "")));
    }
    break;
    /* specials */
    case XcsvStyle::XT_FILENAME:
      buff = QString::asprintf(fmp.printfc.constData(), CSTR(wpt->session->filename));
      break;
    case XcsvStyle::XT_FORMAT:
      buff = QString::asprintf(fmp.printfc.constData(), CSTR(wpt->session->name));
      break;
    case XcsvStyle::XT_unused:
      if (strncmp(fmp.key.constData(), "LON_10E", 7) == 0) {
        buff = QString::asprintf(fmp.printfc.constData(), lon * pow(10.0, strtod(fmp.key.constData()+7, nullptr)));
      } else if (strncmp(fmp.key.constData(), "LAT_10E", 7) == 0) {
        buff = QString::asprintf(fmp.printfc.constData(), lat * pow(10.0, strtod(fmp.key.constData()+7, nullptr)));
      }
      break;
    default:
      warning("Unknown style directive: %s\n", fmp.key.constData());
      break;
    }
    QString obuff = csv_stringclean(buff, xcsv_style->badchars);

    if (field_is_unknown && fmp.options & XcsvStyle::options_optional) {
      continue;
    }

    if (!xcsv_style->field_encloser.isEmpty()) {
      /* print the enclosing character(s) */
      xcsv_file->stream << xcsv_style->field_encloser;
    }

    /* As a special case (pronounced "horrible hack") we allow
     * ""%s"" to smuggle bad characters through.
     */
    if (0 == strcmp(fmp.printfc.constData(), "\"%s\"")) {
      obuff = '"' + obuff + '"';
    }
    xcsv_file->stream << obuff;

    if (!xcsv_style->field_encloser.isEmpty()) {
      /* print the enclosing character(s) */
      xcsv_file->stream << xcsv_style->field_encloser;
    }
    buff.clear();
  }

  xcsv_file->stream << xcsv_style->record_delimiter;

  /* increment the index counter */
  waypt_out_count++;
}

// return |original| after performing token replacement.
QString
XcsvFormat::xcsv_replace_tokens(const QString& original) const
{
  QString replacement = original;
  // Don't do potentially expensive replacements if token prefix
  // isn't present;
  if (original.contains("__")) {
    replacement.replace("__FILE__", xcsv_file->fname);
    replacement.replace("__VERSION__", gpsbabel_testmode()? "" : gpsbabel_version);

    QDateTime dt = current_time().toUTC();

    QString dts = dt.toString(u"ddd MMM dd hh:mm:ss yyyy");
    replacement.replace("__DATE_AND_TIME__", dts);

    QString d = dt.toString(u"MM/dd/yyyy");
    replacement.replace("__DATE__", d);

    QString t = dt.toString(u"hh:mm:ss");
    replacement.replace("__TIME__", t);
  }
  return replacement;
}

/*****************************************************************************/
/* write(void) - write prologues, spawn the output loop, and write           */
/*                         epilogues.                                        */
/*****************************************************************************/
void
XcsvFormat::write()
{
  /* reset the index counter */
  waypt_out_count = 0;

  /* output prologue lines, if any. */
  for (const auto& line : std::as_const(xcsv_style->prologue)) {
    QString line_to_write = xcsv_replace_tokens(line);
    xcsv_file->stream << line_to_write <<  xcsv_style->record_delimiter;
  }

  auto xcsv_waypt_pr_lambda = [this](const Waypoint* wpt)->void {
    xcsv_waypt_pr(wpt);
  };
  auto xcsv_resetpathlen_lambda = [this](const route_head* rte)->void {
    xcsv_resetpathlen(rte);
  };

  if ((xcsv_style->datatype == 0) || (xcsv_style->datatype == wptdata)) {
    waypt_disp_all(xcsv_waypt_pr_lambda);
  }
  if ((xcsv_style->datatype == 0) || (xcsv_style->datatype == rtedata)) {
    route_disp_all(xcsv_resetpathlen_lambda, nullptr, xcsv_waypt_pr_lambda);
  }
  if ((xcsv_style->datatype == 0) || (xcsv_style->datatype == trkdata)) {
    track_disp_all(xcsv_resetpathlen_lambda, nullptr, xcsv_waypt_pr_lambda);
  }

  /* output epilogue lines, if any. */
  for (const auto& line : std::as_const(xcsv_style->epilogue)) {
    QString line_to_write = xcsv_replace_tokens(line);
    xcsv_file->stream << line_to_write << xcsv_style->record_delimiter;
  }
}

void
XcsvStyle::xcsv_parse_style_line(XcsvStyle* style, QString line)
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
  static const QRegularExpression re(R"(\s+)");
  int sep = line.indexOf(re);

  // the first token is the operation, e.g. "IFIELD"
  QString op = line.mid(0, sep).trimmed().toUpper();
  QString tokenstr = line.mid(sep).trimmed();
  const QStringList tokens = tokenstr.split(',');

  if (op == u"FIELD_DELIMITER") {
    auto sp = csv_stringtrim(tokenstr, "\"", 1);
    auto cp = xcsv_get_char_from_constant_table(sp);
    style->field_delimiter = cp;

    /* field delimiters are always bad characters */
    if (cp == u"\\w") {
      style->badchars += " \n\r";
    } else {
      style->badchars += cp;
    }

  } else if (op == u"FIELD_ENCLOSER") {
    auto sp = csv_stringtrim(tokenstr, "\"", 1);
    auto cp = xcsv_get_char_from_constant_table(sp);
    style->field_encloser = cp;

    style->badchars += cp;

  } else if (op == u"RECORD_DELIMITER") {
    auto sp = csv_stringtrim(tokenstr, "\"", 1);
    auto cp = xcsv_get_char_from_constant_table(sp);
    style->record_delimiter = cp;

    // Record delimiters are always bad characters.
    style->badchars += cp;

  } else if (op == u"FORMAT_TYPE") {
    if (tokens[0] == u"INTERNAL") {
      style->type = ff_type_internal;
    }
    // this is almost inconceivable...
    if (tokens[0] == u"SERIAL") {
      style->type = ff_type_serial;
    }

  } else if (op == u"DESCRIPTION") {
    style->description = tokens[0];

  } else if (op == u"EXTENSION") {
    style->extension = tokens[0];

  } else if (op == u"SHORTLEN") {
    style->shortlen = tokens[0].toInt();

  } else if (op == u"SHORTWHITE") {
    style->whitespace_ok = tokens[0].toInt();

  } else if (op == u"BADCHARS") {
    auto sp = csv_stringtrim(tokenstr, "\"", 1);
    auto cp = xcsv_get_char_from_constant_table(sp);
    style->badchars += cp;

  } else if (op =="PROLOGUE") {
    style->prologue.append(tokenstr);

  } else if (op == u"EPILOGUE") {
    style->epilogue.append(tokenstr);

  } else if (op == u"ENCODING") {
    style->codecname = tokens[0];

  } else if (op == u"DATUM") {
    style->gps_datum_name = tokens[0];

  } else if (op == u"DATATYPE") {
    QString p = tokens[0].toUpper();
    if (p == u"TRACK") {
      style->datatype = trkdata;
    } else if (p == u"ROUTE") {
      style->datatype = rtedata;
    } else if (p == u"WAYPOINT") {
      style->datatype = wptdata;
    } else {
      fatal(FatalMsg() << "Unknown data type" << p);
    }

  } else if (op == u"IFIELD") {
    if (tokens.size() < 3) {
      fatal(FatalMsg() << "Invalid IFIELD line: " << tokenstr);
    }

    // The key ("LAT_DIR") should never contain quotes.

    const QString key = tokens[0].simplified();
    const QString val = dequote(tokens[1]);
    const QString pfc = dequote(tokens[2]);
    xcsv_ifield_add(style, key, val, pfc);

  } else if (op == u"OFIELD") {
    //
    //  as OFIELDs are implemented as an after-thought, I'll
    //  leave this as it's own parsing for now.  We could
    //  change the world on ifield vs ofield format later..
    //
    unsigned options = 0;
    // Note: simplified() has to run after split().
    if (tokens.size() < 3) {
      fatal(FatalMsg() << "Invalid OFIELD line: " << tokenstr);
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
        options |= options_nodelim;
      }
      if (options_string.contains("absolute")) {
        options |= options_absolute;
      }
      if (options_string.contains("optional")) {
        options |= options_optional;
      }
    }
    xcsv_ofield_add(style, key, val, pfc, options);
  }
}

XcsvStyle
XcsvStyle::xcsv_read_style(const QString& fname)
{
  XcsvStyle style;

  gpsbabel::TextStream stream;
  stream.open(fname, QIODevice::ReadOnly);
  QString sbuff;
  while (stream.readLineInto(&sbuff)) {
    xcsv_parse_style_line(&style, sbuff.trimmed());
  }
  stream.close();

  /* if we have no output fields, use input fields as output fields */
  if (style.ofields.isEmpty()) {
    style.ofields = style.ifields;
  }

  return style;
}

void
XcsvFormat::xcsv_setup_internal_style(const QString& style_filename)
{
  intstylefile = style_filename;
}

void
XcsvFormat::rd_init(const QString& fname)
{
  /*
   * if we don't have an internal style defined, we need to
   * read it from a user-supplied style file, or die trying.
   */
  if (!intstylefile.isEmpty()) {
    xcsv_style = new XcsvStyle(XcsvStyle::xcsv_read_style(intstylefile));
  } else {
    if (!styleopt) {
      fatal("XCSV input style not declared.  Use ... -i xcsv,style=path/to/file.style\n");
    }

    xcsv_style = new XcsvStyle(XcsvStyle::xcsv_read_style(styleopt));
  }

  if ((xcsv_style->datatype == 0) || (xcsv_style->datatype == wptdata)) {
    if (global_opts.masked_objective & (TRKDATAMASK|RTEDATAMASK)) {
      warning("attempt to read %s as a track or route, but this format only supports waypoints on read.  Reading as waypoints instead.\n", qPrintable(fname));
    }
  }

  xcsv_file = new XcsvFile;
  if (xcsv_style->codecname.isEmpty()) {
    xcsv_file->stream.open(fname, QIODevice::ReadOnly);
  } else {
    xcsv_file->stream.open(fname, QIODevice::ReadOnly, CSTR(xcsv_style->codecname));
  }
  xcsv_file->fname = fname;

  QString datum_name;
  if (opt_datum) {
    datum_name = opt_datum;
  } else if (!xcsv_style->gps_datum_name.isEmpty()) {
    datum_name = xcsv_style->gps_datum_name;
  } else {
    datum_name = "WGS 84";
  }
  xcsv_file->gps_datum_idx = GPS_Lookup_Datum_Index(datum_name);
  if (xcsv_file->gps_datum_idx < 0) {
    fatal("datum \"%s\" is not supported.", qPrintable(datum_name));
  }

  utc_offset = opt_utc? opt_utc.get_result() * SECONDS_PER_HOUR : 0;
}

void
XcsvFormat::rd_deinit()
{
  xcsv_file->stream.close();
  delete xcsv_file;
  xcsv_file = nullptr;

  delete xcsv_style;
  xcsv_style = nullptr;
}

void
XcsvFormat::wr_init(const QString& fname)
{
  /*
   * if we don't have an internal style defined, we need to
   * read it from a user-supplied style file, or die trying.
   */
  if (!intstylefile.isEmpty()) {
    xcsv_style = new XcsvStyle(XcsvStyle::xcsv_read_style(intstylefile));
  } else {
    if (!styleopt) {
      fatal("XCSV output style not declared.  Use ... -o xcsv,style=path/to/file.style\n");
    }

    xcsv_style = new XcsvStyle(XcsvStyle::xcsv_read_style(styleopt));
  }

  xcsv_file = new XcsvFile;
  if (xcsv_style->codecname.isEmpty()) {
    xcsv_file->stream.open(fname, QIODevice::WriteOnly | QIODevice::Text);
  } else {
    xcsv_file->stream.open(fname, QIODevice::WriteOnly | QIODevice::Text, CSTR(xcsv_style->codecname));
  }
  xcsv_file->fname = fname;

  if (xcsv_style->shortlen) {
    xcsv_file->mkshort_handle.set_length(*xcsv_style->shortlen);
  }
  if (xcsv_style->whitespace_ok) {
    xcsv_file->mkshort_handle.set_whitespace_ok(*xcsv_style->whitespace_ok);
  }

  /* set mkshort options from the command line */
  if (global_opts.synthesize_shortnames) {

    if (snlenopt) {
      xcsv_file->mkshort_handle.set_length(snlenopt.get_result());
    }

    if (snwhiteopt.has_value()) {
      xcsv_file->mkshort_handle.set_whitespace_ok(snwhiteopt);
    }

    if (snupperopt.has_value()) {
      xcsv_file->mkshort_handle.set_mustupper(snupperopt);
    }

    if (snuniqueopt.has_value()) {
      xcsv_file->mkshort_handle.set_mustuniq(snuniqueopt);
    }

    xcsv_file->mkshort_handle.set_badchars(CSTR(xcsv_style->badchars));

  }

  QString datum_name;
  if (opt_datum) {
    datum_name = opt_datum;
  } else if (!xcsv_style->gps_datum_name.isEmpty()) {
    datum_name = xcsv_style->gps_datum_name;
  } else {
    datum_name = "WGS 84";
  }
  xcsv_file->gps_datum_idx = GPS_Lookup_Datum_Index(datum_name);
  if (xcsv_file->gps_datum_idx < 0) {
    fatal("datum \"%s\" is not supported.", qPrintable(datum_name));
  }
}

void
XcsvFormat::wr_position_init(const QString& fname)
{
  wr_init(fname);
}

void
XcsvFormat::wr_deinit()
{
  xcsv_file->stream.close();
  delete xcsv_file;
  xcsv_file = nullptr;

  delete xcsv_style;
  xcsv_style = nullptr;
}

void
XcsvFormat::wr_position_deinit()
{
  wr_deinit();
}

void
XcsvFormat::wr_position(Waypoint* wpt)
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
  write();
  waypt_del(wpt);

  xcsv_file->stream.flush();
}
