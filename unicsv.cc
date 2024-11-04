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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
*/

#include "unicsv.h"

#include <algorithm>               // for find_if
#include <cmath>                   // for fabs, lround
#include <cstdio>                  // for NULL, sscanf
#include <ctime>                   // for tm
#include <utility>                 // for as_const

#include <QByteArray>              // for QByteArray
#include <QChar>                   // for QChar
#include <QDateTime>               // for QDateTime
#include <QIODevice>               // for QIODevice, QIODevice::ReadOnly, QIODevice::WriteOnly
#include <QLatin1Char>             // for QLatin1Char
#include <QList>                   // for QList, QList<>::const_iterator
#include <QString>                 // for QString, operator!=, operator==
#include <QStringList>             // for QStringList
#include <QTextStream>             // for QTextStream, operator<<, qSetRealNumberPrecision, qSetFieldWidth, QTextStream::FixedNotation
#include <QTime>                   // for QTime
#include <QVector>                 // for QVector
#include <Qt>                      // for CaseInsensitive
#include <QtGlobal>                // for qPrintable

#include "defs.h"
#include "csv_util.h"              // for csv_linesplit, human_to_dec
#include "formspec.h"              // for FormatSpecificDataList
#include "garmin_fs.h"             // for garmin_fs_t
#include "garmin_tables.h"         // for gt_lookup_datum_index, gt_get_mps_grid_longname, gt_lookup_grid_type
#include "geocache.h"              // for Geocache, Geocache::status_t, Geoc...
#include "jeeps/gpsmath.h"         // for GPS_Math_UKOSMap_To_WGS84_H, GPS_Math_EN_To_UKOSNG_Map, GPS_Math_Known_Datum_To_UTM_EN, GPS_Math_Known_Datum_To_WGS84_M, GPS_Math_Swiss_EN_To_WGS84, GPS_Math_UTM_EN_To_Known_Datum, GPS_Math_WGS84_To_Known_Datum_M, GPS_Math_WGS84_To_Swiss_EN, GPS_Math_WGS...
#include "session.h"               // for session_t
#include "src/core/datetime.h"     // for DateTime
#include "src/core/logging.h"      // for Warning, Fatal
#include "src/core/textstream.h"   // for TextStream


/*
 * ! Please use always underscores in field names !
 * we check a second time after replacing underscores with spaces
 */
const QVector<UnicsvFormat::field_t> UnicsvFormat::fields_def = {
  /* unhandled columns */
  { "index",	fld_terminator, kStrAny },
  { "no",		fld_terminator, kStrEqual },
  { "mini",	fld_terminator, kStrAny },	/* maybe minimum anything, so
							   avoid detection as 'min' for minute */
  /* handled columns */
  { "name",	fld_shortname, kStrAny },
  { "title",	fld_shortname, kStrAny },
  { "desc",	fld_description, kStrAny },
  { "notes",	fld_notes, kStrAny },
  { "omment",	fld_notes, kStrAny },		/* works also for German "Kommentar" */
  { "text",	fld_notes, kStrAny },
  { "url",	fld_url, kStrAny },
  { "icon",	fld_symbol, kStrAny },
  { "symb",	fld_symbol, kStrAny },
  { "lat",	fld_latitude, kStrAny },
  { "lon",	fld_longitude, kStrAny },
  { "lng",	fld_longitude, kStrAny },
  { "x",		fld_longitude, kStrEqual },
  { "y",		fld_latitude, kStrEqual },
  { "z",		fld_altitude, kStrEqual },
  { "x_pos",	fld_longitude, kStrAny },
  { "y_pos",	fld_latitude, kStrAny },
  { "alt",	fld_altitude, kStrAny },
  { "ele",	fld_altitude, kStrAny },
  { "height",	fld_altitude, kStrAny },
  { "utm_z",	fld_utm_zone, kStrAny },
  { "utm_c",	fld_utm_zone_char, kStrAny },
  { "utm_zc",	fld_utm_zone_char, kStrAny },
  { "utm_n",	fld_utm_northing, kStrAny },
  { "utm_e",	fld_utm_easting, kStrAny },
  { "utm",	fld_utm, kStrEqual },
  { "utm_coo",	fld_utm, kStrAny },
  { "utm_pos",	fld_utm, kStrAny },
  { "bng_z",	fld_bng_zone, kStrAny },
  { "bng_n",	fld_bng_northing, kStrAny },
  { "bng_e",	fld_bng_easting, kStrAny },
  { "bng",	fld_bng, kStrEqual },
  { "bng_coo",	fld_bng, kStrAny },
  { "bng_pos",	fld_bng, kStrAny },
  { "swiss_e",	fld_swiss_easting, kStrAny },
  { "swiss_n",	fld_swiss_northing, kStrAny },
  { "swiss",	fld_swiss, kStrEqual },
  { "swiss_coo",	fld_swiss, kStrAny },
  { "swiss_pos",	fld_swiss, kStrAny },
  { "hdop",	fld_hdop, kStrAny },
  { "pdop",	fld_pdop, kStrAny },
  { "vdop",	fld_vdop, kStrAny },
  { "sat",	fld_sat, kStrAny },
  { "fix",	fld_fix, kStrAny },
  { "utc_d",	fld_utc_date, kStrAny },
  { "utc_t",	fld_utc_time, kStrAny },
  { "head",	fld_course, kStrAny },
  { "cour",	fld_course, kStrAny },
  { "speed",	fld_speed, kStrAny },
  { "velo",	fld_speed, kStrAny },
  { "geschw",	fld_speed, kStrAny },		/* speed in german */
  { "tempf",	fld_temperature_f, kStrEqual },	/* degrees fahrenheit */
  { "temp",	fld_temperature, kStrAny },	/* degrees celsius by default */
  { "heart",	fld_heartrate, kStrAny },
  { "caden",	fld_cadence, kStrAny },
  { "power",	fld_power, kStrAny },
  { "prox",	fld_proximity, kStrAny },
  { "depth",	fld_depth, kStrAny },
  { "date",	fld_date, kStrAny },
  { "time",	fld_time, kStrAny },
  { "zeit",	fld_time, kStrAny },
  { "hour",	fld_hour, kStrLeft },
  { "min",	fld_min, kStrLeft },
  { "sec",	fld_sec, kStrLeft },
  { "year",	fld_year, kStrLeft },
  { "month",	fld_month, kStrLeft },
  { "day",	fld_day, kStrLeft },
  { "n/s",	fld_ns, kStrAny },
  { "e/w",	fld_ew, kStrAny },

  /* garmin specials */
  { "addr",	fld_garmin_addr, kStrAny },
  { "street",	fld_garmin_addr, kStrAny },
  { "city",	fld_garmin_city, kStrAny },
  { "country",	fld_garmin_country, kStrAny },
  { "post",	fld_garmin_postal_code, kStrAny },
  { "zip",	fld_garmin_postal_code, kStrAny },
  { "phone",	fld_garmin_phone_nr, kStrAny },
  { "phone2",	fld_garmin_phone_nr2, kStrAny },
  { "fax",	fld_garmin_fax_nr, kStrAny },
  { "email",	fld_garmin_email, kStrAny },
  { "state",	fld_garmin_state, kStrAny },
  { "faci",	fld_garmin_facility, kStrAny },
  /* geocache details */
  { "gcid",	fld_gc_id, kStrAny },
  { "type",	fld_gc_type, kStrAny },
  { "cont",	fld_gc_container, kStrAny },
  { "terr",	fld_gc_terr, kStrAny },
  { "diff",	fld_gc_diff, kStrAny },
  { "arch",	fld_gc_is_archived, kStrAny },
  { "avail",	fld_gc_is_available, kStrAny },
  { "found",	fld_gc_last_found, kStrAny },
  { "placer_id",	fld_gc_placer_id, kStrAny },
  { "placer",	fld_gc_placer, kStrAny },
  { "hint",	fld_gc_hint, kStrAny }
};
/* helpers */

// Parse GC-Code / geo cache reference code into int64 (GC-ID)
// (see also https://api.groundspeak.com/documentation#referencecodes)
long long
UnicsvFormat::unicsv_parse_gc_code(const QString& str)
{
  if (! str.startsWith("GC")) {
    return 0;
  }

  // Remove "GC" prefix
  QString s = str.mid(2);
  // Replacements according to groundspeak api documentation
  s.replace('S', '5');
  s.replace('O', '0');
  // Remove leading zeros. some online converters do that as well.
  while (s.startsWith('0')) {
    s.remove(0, 1);
  }

  // We have these cases:
  // *  1-3 digits                   => base 16
  // *    4 digits, first one is 0-F => base 16
  // *    4 digits, first one G-Z    => base 31
  // * 5-12 digits                   => base 31
  // *  13- digits                   => exceeds int64_t
  //
  int base;
  const QString kBase31 = "0123456789ABCDEFGHJKMNPQRTVWXYZ"; //  ILOSU are omitted.
  if (s.size() >= 1 && s.size() <= 3) {
    base = 16;
  } else if (s.size() == 4) {
    if (kBase31.indexOf(s[0]) < 16) {
      base = 16;
    } else {
      base = 31;
    }
  } else if (s.size() >= 5 && s.size() <= 12) {
    base = 31;
  } else {
    return 0;
  }

  long long res = 0;
  for (auto c : std::as_const(s)) {
    int val = kBase31.indexOf(c);
    if (val < 0 || (base == 16 && val > 15)) {
      return 0;
    }
    res = res * base + val;
  }
  if (base == 31) {
    res -= 411120;
  }
  if (res < 0) {
    res = 0;
  }
  return res;
}

QDate
UnicsvFormat::unicsv_parse_date(const char* str, int* consumed)
{
  int p1;
  int p2;
  int p3;
  char sep[2];
  std::tm tm{};
  int lconsumed = 0;

  int ct = sscanf(str, "%d%1[-.//]%d%1[-.//]%d%n", &p1, sep, &p2, sep, &p3, &lconsumed);
  if (consumed && lconsumed) {
    *consumed = lconsumed;
  }
  if (ct != 5) {
    if (consumed) {		/* don't stop here; it's only sniffing */
      *consumed = 0;	/* for a possible date */
      return {};
    }
    gbFatal(FatalMsg() << "Could not parse date string (" << str << ").");
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
      return {};	/* don't stop here */
    }
    gbFatal(FatalMsg() << "Could not parse date string (" << str << ").");
  }

  QDate result{tm.tm_year, tm.tm_mon, tm.tm_mday};
  if (!result.isValid()) {
    gbFatal(FatalMsg() << "Invalid date parsed from string (" << str << ").");
  }
  return result;
}

QTime
UnicsvFormat::unicsv_parse_time(const char* str, QDate& date)
{
  int hour;
  int min;
  int sec;
  int msec;
  int consumed = 0;
  double frac_sec;

  /* If we have something we're pretty sure is a date, parse that
   * first, skip over it, and pass that back to the caller)
   */
  QDate ldate = unicsv_parse_date(str, &consumed);
  if (consumed && ldate.isValid()) {
    str += consumed;
    date = ldate;
  }
  int ct = sscanf(str, "%d%*1[.://]%d%*1[.://]%d%lf", &hour, &min, &sec, &frac_sec);
  if (ct < 3) {
    gbFatal(FatalMsg() << "Could not parse time string (" << str << ").");
  }
  if (ct >= 4) {
    // Don't round up and ripple through seconds, minutes, hours.
    // 23:59:59.9999999 -> 24:00:00.000 which is an invalid QTime.
    msec = frac_sec * 1000.0;
  } else {
    msec = 0;
  }

  QTime result{hour, min, sec, msec};
  if (!result.isValid()) {
    gbFatal(FatalMsg() << "Invalid time parsed from string (" << str << ").");
  }
  return result;
}

QTime
UnicsvFormat::unicsv_parse_time(const QString& str, QDate& date)
{
  return unicsv_parse_time(CSTR(str), date);
}

Geocache::status_t
UnicsvFormat::unicsv_parse_status(const QString& str)
{
  if (str.compare(u"true", Qt::CaseInsensitive) == 0 ||
      str.compare(u"yes", Qt::CaseInsensitive) == 0 ||
      str == '1') {
    return Geocache::status_t::gs_true;
  }
  if (str.compare(u"false", Qt::CaseInsensitive) == 0 ||
      str.compare(u"no", Qt::CaseInsensitive) == 0 ||
      str == '0') {
    return Geocache::status_t::gs_false;
  }
  return Geocache::status_t::gs_unknown;
}

QDateTime
UnicsvFormat::unicsv_adjust_time(const QDate date, const QTime time, bool is_localtime) const
{
  return make_datetime(date, time, is_localtime, opt_utc, utc_offset);
}

bool
UnicsvFormat::unicsv_compare_fields(const QString& s, const field_t& f)
{
  QString name = f.name;
  QString test = s;
  bool result = false;

  if (!(f.options & kStrCase)) {
    test = test.toUpper();
    name = name.toUpper();
  }

  if (f.options & kStrEqual) {
    result = test == name;
  } else if (f.options & kStrAny) {
    result = test.contains(name);
  } else if (f.options & kStrLeft) {
    result = test.startsWith(name);
  } else if (f.options & kStrRight) {
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

void
UnicsvFormat::unicsv_fondle_header(QString header)
{
  /* Convert the entire header to lower case for convenience.
   */
  header = header.toLower();

  /* Find the separator and split the line into fields.
   * If we see an unenclosd tab that is the separator.
   * Otherwise, if we see an unenclosed semicolon that is the separator.
   * Otherwise, if we see an unenclosed vertical bar that is the separator.
   * Otherwise the separator is a comma.
   */
  const QList<const char*> delimiters = {"\t", ";", "|", ","};
  unicsv_fieldsep = delimiters.last();
  QStringList values;
  bool delimiter_detected;
  for (const auto* delimiter : delimiters) {
    values = csv_linesplit(header, delimiter, kUnicsvQuoteChar, unicsv_lineno, CsvQuoteMethod::rfc4180, &delimiter_detected);
    if (delimiter_detected) {
      unicsv_fieldsep = delimiter;
      break;
    }
  }

  for (auto value : values) {
    value = value.trimmed();

    unicsv_fields_tab.append(fld_terminator); // default

    const auto it = std::find_if(fields_def.cbegin(), fields_def.cend(), [&value](const field_t& f)->bool {
      return unicsv_compare_fields(value, f);
    });

    if (it == fields_def.cend()) { // not found
      if (global_opts.debug_level) {
        gbWarning("Unhandled column \"%s\".\n", qPrintable(value));
      }
    } else { // found
      const field_t f = *it;
      unicsv_fields_tab.last() = f.type;

      if (global_opts.debug_level) {
        gbWarning("Interpreting column \"%s\" as %s(%d).\n", qPrintable(value), qPrintable(f.name), f.type);
      }

      /* handle some special items */
      if (f.type == fld_altitude) {
        if (value.contains("ft") || value.contains("feet")) {
          unicsv_altscale = FEET_TO_METERS(1);
        }
      }
      if (f.type == fld_depth) {
        if (value.contains("ft") || value.contains("feet")) {
          unicsv_depthscale = FEET_TO_METERS(1);
        }
      }
      if (f.type == fld_proximity) {
        if (value.contains("ft") || value.contains("feet")) {
          unicsv_proximityscale = FEET_TO_METERS(1);
        }
      }
      if ((f.type == fld_time) || (f.type == fld_date)) {
        if (value.contains("iso")) {
          unicsv_fields_tab.last() = fld_iso_time;
        }
      }
    }
  }
}

void
UnicsvFormat::rd_init(const QString& fname)
{
  QString buff;
  unicsv_altscale = 1.0;
  unicsv_depthscale = 1.0;
  unicsv_proximityscale = 1.0;

  unicsv_fields_tab.clear();
  unicsv_data_type = global_opts.objective;
  unicsv_detect = (!(global_opts.masked_objective & (WPTDATAMASK | TRKDATAMASK | RTEDATAMASK | POSNDATAMASK)));

  unicsv_track = unicsv_route = nullptr;
  unicsv_datum_idx = gt_lookup_datum_index(opt_datum);

  fin = new gpsbabel::TextStream;
  fin->open(fname, QIODevice::ReadOnly, opt_codec.get().toUtf8());
  unicsv_lineno = 0;
  if (opt_fields) {
    QString fields = opt_fields;
    fields.replace('+', ',');
    unicsv_fondle_header(fields);
  } else if (buff = fin->readLine(); !buff.isNull()) {
    ++unicsv_lineno;
    unicsv_fondle_header(buff);
  } else {
    unicsv_fieldsep = nullptr;
  }

  utc_offset = opt_utc? opt_utc.get_result() * SECONDS_PER_HOUR : 0;
}

void
UnicsvFormat::rd_deinit()
{
  fin->close();
  delete fin;
  fin = nullptr;
  unicsv_fields_tab.clear();
}

void
UnicsvFormat::unicsv_parse_one_line(const QString& ibuf)
{
  int  utm_zone = -9999;
  double utm_easting = 0;
  double utm_northing = 0;
  char utm_zc = 'N';
  // Zones are always two bytes.
  QString bng_zone;
  double bng_easting = kUnicsvUnknown;
  double bng_northing = kUnicsvUnknown;
  double swiss_easting = kUnicsvUnknown;
  double swiss_northing = kUnicsvUnknown;
  int checked = 0;
  QDate local_date;
  QTime local_time;
  QDate utc_date;
  QTime utc_time;
  bool need_datetime = true;
  garmin_fs_t* gmsd;
  double d;
  std::tm ymd{};
  int src_datum = unicsv_datum_idx;
  int ns = 1;
  int ew = 1;
  Geocache* gc_data = nullptr;
  auto* wpt = new Waypoint;
  wpt->latitude = kUnicsvUnknown;
  wpt->longitude = kUnicsvUnknown;

  int column = -1;
  const QStringList values = csv_linesplit(ibuf, unicsv_fieldsep, kUnicsvQuoteChar, unicsv_lineno, CsvQuoteMethod::rfc4180);
  for (auto value : values) {
    if (++column >= unicsv_fields_tab.size()) {
      break;  /* ignore extra fields on line */
    }

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
      if (parse_distance(value, &d, unicsv_altscale)) {
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
                        &wpt->latitude, &wpt->longitude);
      /* coordinates from parse_coordinates are in WGS84
         don't convert a second time */
      src_datum = kDatumWGS84;
      break;

    case fld_bng:
      parse_coordinates(value, kDatumOSGB36, grid_bng,
                        &wpt->latitude, &wpt->longitude);
      /* coordinates from parse_coordinates are in WGS84
         don't convert a second time */
      src_datum = kDatumWGS84;
      break;

    case fld_bng_zone:
      bng_zone = value.toUpper();
      break;

    case fld_bng_northing:
      bng_northing = value.toDouble();
      break;

    case fld_bng_easting:
      bng_easting = value.toDouble();
      break;

    case fld_swiss:
      parse_coordinates(value, kDatumWGS84, grid_swiss,
                        &wpt->latitude, &wpt->longitude);
      /* coordinates from parse_coordinates are in WGS84
         don't convert a second time */
      src_datum = kDatumWGS84;
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
      if (value.compare(u"none", Qt::CaseInsensitive) == 0) {
        wpt->fix = fix_none;
      } else if (value.compare(u"2d", Qt::CaseInsensitive) == 0) {
        wpt->fix = fix_2d;
      } else if (value.compare(u"3d", Qt::CaseInsensitive) == 0) {
        wpt->fix = fix_3d;
      } else if (value.compare(u"dgps", Qt::CaseInsensitive) == 0) {
        wpt->fix = fix_dgps;
      } else if (value.compare(u"pps", Qt::CaseInsensitive) == 0) {
        wpt->fix = fix_pps;
      } else {
        wpt->fix = fix_unknown;
      }
      break;

    case fld_utc_date:
      if (need_datetime && !utc_date.isValid()) {
        utc_date = unicsv_parse_date(CSTR(value), nullptr);
      }
      break;

    case fld_utc_time:
      if (need_datetime && !utc_time.isValid()) {
        utc_time = unicsv_parse_time(value, utc_date);
      }
      break;

    case fld_speed:
      if (parse_speed(value, &d, 1.0)) {
        wpt->set_speed(d);
        if (unicsv_detect) {
          unicsv_data_type = trkdata;
        }
      }
      break;

    case fld_course:
      wpt->set_course(value.toDouble());
      if (unicsv_detect) {
        unicsv_data_type = trkdata;
      }
      break;

    case fld_temperature:
      d = value.toDouble();
      if (fabs(d) < 999999) {
        wpt->set_temperature(d);
      }
      break;

    case fld_temperature_f:
      d = value.toDouble();
      if (fabs(d) < 999999) {
        wpt->set_temperature(FAHRENHEIT_TO_CELSIUS(d));
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
      if (parse_distance(value, &d, unicsv_proximityscale)) {
        wpt->set_proximity(d);
      }
      break;

    case fld_depth:
      if (parse_distance(value, &d, unicsv_depthscale)) {
        wpt->set_depth(d);
      }
      break;

    case fld_symbol:
      wpt->icon_descr = value;
      break;

    case fld_iso_time:
      need_datetime = false;	/* fix result */
      wpt->SetCreationTime(QDateTime::fromString(value, Qt::ISODateWithMs));
      break;

    case fld_time:
      if (need_datetime && !local_time.isValid()) {
        local_time = unicsv_parse_time(value, local_date);
      }
      break;

    case fld_date:
      if (need_datetime && !local_date.isValid()) {
        local_date = unicsv_parse_date(CSTR(value), nullptr);
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
      if (need_datetime && !local_date.isValid() && !local_time.isValid()) {
        local_time = unicsv_parse_time(value, local_date);
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
      gmsd = garmin_fs_t::find(wpt);
      if (! gmsd) {
        gmsd = new garmin_fs_t(-1);
        wpt->fs.FsChainAdd(gmsd);
      }
      switch (unicsv_fields_tab[column]) {
      case fld_garmin_city:
        garmin_fs_t::set_city(gmsd, value);
        break;
      case fld_garmin_postal_code:
        garmin_fs_t::set_postal_code(gmsd, value);
        break;
      case fld_garmin_state:
        garmin_fs_t::set_state(gmsd, value);
        break;
      case fld_garmin_country:
        garmin_fs_t::set_country(gmsd, value);
        break;
      case fld_garmin_addr:
        garmin_fs_t::set_addr(gmsd, value);
        break;
      case fld_garmin_phone_nr:
        garmin_fs_t::set_phone_nr(gmsd, value);
        break;
      case fld_garmin_phone_nr2:
        garmin_fs_t::set_phone_nr2(gmsd, value);
        break;
      case fld_garmin_fax_nr:
        garmin_fs_t::set_fax_nr(gmsd, value);
        break;
      case fld_garmin_email:
        garmin_fs_t::set_email(gmsd, value);
        break;
      case fld_garmin_facility:
        garmin_fs_t::set_facility(gmsd, value);
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
    case fld_gc_last_found:
    case fld_gc_placer:
    case fld_gc_placer_id:
    case fld_gc_hint:

      gc_data = wpt->AllocGCData();

      switch (unicsv_fields_tab[column]) {

      case fld_gc_id:
        // First try to decode as numeric GC-ID (e.g. "575006").
        // If that doesn't succedd, try to decode as GC-Code
        // (e.g. "GC1234G").
        bool ok;
        gc_data->id = value.toLongLong(&ok, 10);
        if (!ok) {
          gc_data->id = unicsv_parse_gc_code(value);
        }
        break;
      case fld_gc_type:
        gc_data->set_type(value);
        break;
      case fld_gc_container:
        gc_data->set_container(value);
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
      case fld_gc_last_found: {
        QTime ftime;
        QDate fdate;
        ftime = unicsv_parse_time(value, fdate);
        if (fdate.isValid() || ftime.isValid()) {
          gc_data->last_found = unicsv_adjust_time(fdate, ftime, true);
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

  if (need_datetime) {	/* not fixed */
    if (utc_date.isValid() && utc_time.isValid()) {
      wpt->SetCreationTime(unicsv_adjust_time(utc_date, utc_time, false));
    } else if (local_date.isValid() && local_time.isValid()) {
      wpt->SetCreationTime(unicsv_adjust_time(local_date, local_time, true));
    } else if (utc_date.isValid()) {
      wpt->SetCreationTime(unicsv_adjust_time(utc_date, utc_time, false));
    } else if (local_date.isValid()) {
      wpt->SetCreationTime(unicsv_adjust_time(local_date, local_time, true));
    } else if (utc_time.isValid()) {
      wpt->SetCreationTime(unicsv_adjust_time(utc_date, utc_time, false));
    } else if (local_time.isValid()) {
      wpt->SetCreationTime(unicsv_adjust_time(local_date, local_time, true));
    } else if (ymd.tm_year || ymd.tm_mon || ymd.tm_mday) {
      if (ymd.tm_year < 100) {
        if (ymd.tm_year <= 70) {
          ymd.tm_year += 2000;
        } else {
          ymd.tm_year += 1900;
        }
      }

      if (ymd.tm_mon == 0) {
        ymd.tm_mon = 1;
      }
      if (ymd.tm_mday == 0) {
        ymd.tm_mday = 1;
      }

      wpt->SetCreationTime(unicsv_adjust_time(
                             QDate(ymd.tm_year, ymd.tm_mon, ymd.tm_mday),
                             QTime(ymd.tm_hour, ymd.tm_min, ymd.tm_sec),
                             true));
    } else if (ymd.tm_hour || ymd.tm_min || ymd.tm_sec) {
      wpt->SetCreationTime(unicsv_adjust_time(
                             QDate(),
                             QTime(ymd.tm_hour, ymd.tm_min, ymd.tm_sec),
                             true));
    }

  }

  /* utm/bng/swiss can be optional */

  if ((wpt->latitude == kUnicsvUnknown) && (wpt->longitude == kUnicsvUnknown)) {
    if (utm_zone != -9999) {
      GPS_Math_UTM_EN_To_Known_Datum(&wpt->latitude, &wpt->longitude,
                                     utm_easting, utm_northing, utm_zone, utm_zc, unicsv_datum_idx);
    } else if ((bng_easting != kUnicsvUnknown) && (bng_northing != kUnicsvUnknown)) {
      if (bng_zone.isEmpty()) { // OS easting northing
        // Grid references may also be quoted as a pair of numbers: eastings then northings in metres, measured from the southwest corner of the SV square.
        double bnge;
        double bngn;
        char bngz[3];
        if (! GPS_Math_EN_To_UKOSNG_Map(
              bng_easting, bng_northing,
              &bnge, &bngn, bngz)) {
          gbFatal("Unable to convert BNG coordinates (%.f %.f)!\n",
                bng_easting, bng_northing);
        }
        if (! GPS_Math_UKOSMap_To_WGS84_H(
              bngz, bnge, bngn,
              &wpt->latitude, &wpt->longitude))
          gbFatal("Unable to convert BNG coordinates (%s %.f %.f)!\n",
                bngz, bnge, bngn);
      } else { // traditional zone easting northing
        if (! GPS_Math_UKOSMap_To_WGS84_H(
              CSTR(bng_zone), bng_easting, bng_northing,
              &wpt->latitude, &wpt->longitude))
          gbFatal("Unable to convert BNG coordinates (%s %.f %.f)!\n",
                CSTR(bng_zone), bng_easting, bng_northing);
      }
      src_datum = kDatumWGS84;	/* don't convert afterwards */
    } else if ((swiss_easting != kUnicsvUnknown) && (swiss_northing != kUnicsvUnknown)) {
      GPS_Math_Swiss_EN_To_WGS84(swiss_easting, swiss_northing,
                                 &wpt->latitude, &wpt->longitude);
      src_datum = kDatumWGS84;	/* don't convert afterwards */
    }
  }

  if ((src_datum != kDatumWGS84) &&
      (wpt->latitude != kUnicsvUnknown) && (wpt->longitude != kUnicsvUnknown)) {
    double alt;
    GPS_Math_Known_Datum_To_WGS84_M(wpt->latitude, wpt->longitude, 0.0,
                                    &wpt->latitude, &wpt->longitude, &alt, src_datum);
  }

  switch (unicsv_data_type) {
  case rtedata:
    if (! unicsv_route) {
      unicsv_route = new route_head;
      route_add_head(unicsv_route);
    }
    route_add_wpt(unicsv_route, wpt);
    break;
  case trkdata:
    if (! unicsv_track) {
      unicsv_track = new route_head;
      track_add_head(unicsv_track);
    }
    track_add_wpt(unicsv_track, wpt);
    break;
  default:
    waypt_add(wpt);
  }
}

void
UnicsvFormat::read()
{
  QString buff;

  if (unicsv_fieldsep == nullptr) {
    return;
  }

  while ((buff = fin->readLine(), !buff.isNull())) {
    ++unicsv_lineno;
    buff = buff.trimmed();
    if (buff.isEmpty() || buff.startsWith('#')) {
      continue;
    }
    unicsv_parse_one_line(buff);
  }
}

/* =========================================================================== */

[[noreturn]] void UnicsvFormat::unicsv_fatal_outside(const Waypoint* wpt) const
{
  *fout << "#####\n";
  gbFatal("%s (%s) is outside of convertible area of grid \"%s\"!\n",
        wpt->shortname.isEmpty() ? "Waypoint" : qPrintable(wpt->shortname),
        qPrintable(pretty_deg_format(wpt->latitude, wpt->longitude, 'd', nullptr, false)),
        qPrintable(gt_get_mps_grid_longname(unicsv_grid_idx)));
}

void
UnicsvFormat::unicsv_print_str(const QString& s) const
{
  *fout << unicsv_fieldsep;
  QString t;
  if (!s.isEmpty()) {
    t = csv_enquote(s, kUnicsvQuoteChar);
    // I'm not sure these three replacements are necessary; they're just a
    // slavish re-implementation of (what I think) the original C code
    // was doing.
    t.replace("\r\n", ",");
    t.replace('\r', ',');
    t.replace('\n', ',');
  }
  *fout << t.trimmed();
}

void
UnicsvFormat::unicsv_print_date_time(const QDateTime& idt) const
{
  if (!idt.isValid()) {
    return;
  }
  QDateTime dt;
  if (opt_utc) {
    dt = idt.toOffsetFromUtc(utc_offset);
  } else {
    dt = idt.toLocalTime();
  }

  unicsv_print_str(dt.toString(u"yyyy/MM/dd hh:mm:ss"));
}

void
UnicsvFormat::unicsv_waypt_enum_cb(const Waypoint* wpt)
{
  const QString& shortname = wpt->shortname;
  const garmin_fs_t* gmsd = garmin_fs_t::find(wpt);

  if (!shortname.isEmpty()) {
    unicsv_outp_flags[fld_shortname] = true;
  }
  if (wpt->altitude != unknown_alt) {
    unicsv_outp_flags[fld_altitude] = true;
  }
  if (!wpt->icon_descr.isNull()) {
    unicsv_outp_flags[fld_symbol] = true;
  }
  if (!wpt->description.isEmpty() && shortname != wpt->description) {
    unicsv_outp_flags[fld_description] = true;
  }
  if (!wpt->notes.isEmpty() && shortname != wpt->notes) {
    if ((wpt->description.isEmpty()) || (wpt->description != wpt->notes)) {
      unicsv_outp_flags[fld_notes] = true;
    }
  }
  if (wpt->HasUrlLink()) {
    unicsv_outp_flags[fld_url] = true;
  }
  if (wpt->creation_time.isValid()) {
    unicsv_outp_flags[fld_time] = true;
    if (wpt->creation_time.toTime_t() >= 2 * SECONDS_PER_DAY) {
      unicsv_outp_flags[fld_date] = true;
    }
  }

  if (wpt->fix != fix_unknown) {
    unicsv_outp_flags[fld_fix] = true;
  }
  if (wpt->vdop > 0) {
    unicsv_outp_flags[fld_vdop] = true;
  }
  if (wpt->hdop > 0) {
    unicsv_outp_flags[fld_hdop] = true;
  }
  if (wpt->pdop > 0) {
    unicsv_outp_flags[fld_pdop] = true;
  }
  if (wpt->sat > 0) {
    unicsv_outp_flags[fld_sat] = true;
  }
  if (wpt->heartrate != 0) {
    unicsv_outp_flags[fld_heartrate] = true;
  }
  if (wpt->cadence != 0) {
    unicsv_outp_flags[fld_cadence] = true;
  }
  if (wpt->power > 0) {
    unicsv_outp_flags[fld_power] = true;
  }

  /* "flagged" waypoint members */
  if (wpt->course_has_value()) {
    unicsv_outp_flags[fld_course] = true;
  }
  if (wpt->depth_has_value()) {
    unicsv_outp_flags[fld_depth] = true;
  }
  if (wpt->speed_has_value()) {
    unicsv_outp_flags[fld_speed] = true;
  }
  if (wpt->proximity_has_value()) {
    unicsv_outp_flags[fld_proximity] = true;
  }
  if (wpt->temperature_has_value()) {
    unicsv_outp_flags[fld_temperature] = true;
  }

  if (gmsd) {
    if (garmin_fs_t::has_addr(gmsd)) {
      unicsv_outp_flags[fld_garmin_addr] = true;
    }
    if (garmin_fs_t::has_city(gmsd)) {
      unicsv_outp_flags[fld_garmin_city] = true;
    }
    if (garmin_fs_t::has_country(gmsd)) {
      unicsv_outp_flags[fld_garmin_country] = true;
    }
    if (garmin_fs_t::has_phone_nr(gmsd)) {
      unicsv_outp_flags[fld_garmin_phone_nr] = true;
    }
    if (garmin_fs_t::has_phone_nr2(gmsd)) {
      unicsv_outp_flags[fld_garmin_phone_nr2] = true;
    }
    if (garmin_fs_t::has_fax_nr(gmsd)) {
      unicsv_outp_flags[fld_garmin_fax_nr] = true;
    }
    if (garmin_fs_t::has_email(gmsd)) {
      unicsv_outp_flags[fld_garmin_email] = true;
    }
    if (garmin_fs_t::has_postal_code(gmsd)) {
      unicsv_outp_flags[fld_garmin_postal_code] = true;
    }
    if (garmin_fs_t::has_state(gmsd)) {
      unicsv_outp_flags[fld_garmin_state] = true;
    }
    if (garmin_fs_t::has_facility(gmsd)) {
      unicsv_outp_flags[fld_garmin_facility] = true;
    }
  }

  if (! wpt->EmptyGCData()) {
    const Geocache* gc_data = wpt->gc_data;

    if (gc_data->id) {
      unicsv_outp_flags[fld_gc_id] = true;
    }
    if (gc_data->type != Geocache::type_t::gt_unknown) {
      unicsv_outp_flags[fld_gc_type] = true;
    }
    if (gc_data->container != Geocache::container_t::gc_unknown) {
      unicsv_outp_flags[fld_gc_container] = true;
    }
    if (gc_data->terr) {
      unicsv_outp_flags[fld_gc_terr] = true;
    }
    if (gc_data->diff) {
      unicsv_outp_flags[fld_gc_diff] = true;
    }
    if (gc_data->is_archived != Geocache::status_t::gs_unknown) {
      unicsv_outp_flags[fld_gc_is_archived] = true;
    }
    if (gc_data->is_available != Geocache::status_t::gs_unknown) {
      unicsv_outp_flags[fld_gc_is_available] = true;
    }
    if (gc_data->last_found.isValid()) {
      unicsv_outp_flags[fld_gc_last_found] = true;
    }
    if (!gc_data->placer.isEmpty()) {
      unicsv_outp_flags[fld_gc_placer] = true;
    }
    if (gc_data->placer_id) {
      unicsv_outp_flags[fld_gc_placer_id] = true;
    }
    if (!gc_data->hint.isEmpty()) {
      unicsv_outp_flags[fld_gc_hint] = true;
    }
  }
}

void
UnicsvFormat::unicsv_waypt_disp_cb(const Waypoint* wpt)
{
  double lat;
  double lon;
  double alt;
  const Geocache* gc_data = nullptr;
  unicsv_waypt_ct++;

  QString shortname = wpt->shortname;
  const garmin_fs_t* gmsd = garmin_fs_t::find(wpt);

  if (unicsv_datum_idx == kDatumWGS84) {
    lat = wpt->latitude;
    lon = wpt->longitude;
    alt = wpt->altitude;
  } else {
    GPS_Math_WGS84_To_Known_Datum_M(wpt->latitude, wpt->longitude, 0.0,
                                    &lat, &lon, &alt, unicsv_datum_idx);
  }

  *fout << unicsv_waypt_ct << unicsv_fieldsep;

  switch (unicsv_grid_idx) {

  case grid_lat_lon_ddd:
    *fout << pretty_deg_format(lat, lon, 'd', unicsv_fieldsep, false);
    break;

  case grid_lat_lon_dmm:
    *fout << pretty_deg_format(lat, lon, 'm', unicsv_fieldsep, false);
    break;

  case grid_lat_lon_dms: {
    QString position = pretty_deg_format(lat, lon, 's', unicsv_fieldsep, false);
    auto sep = position.indexOf(unicsv_fieldsep);
    QString tmp = csv_enquote(position.left(sep), kUnicsvQuoteChar);
    *fout << tmp << unicsv_fieldsep;
    tmp = csv_enquote(position.mid(sep+1), kUnicsvQuoteChar);
    *fout << tmp;
  }
  break;

  case grid_bng: {
    char map[3];
    double north;
    double east;

    if (! GPS_Math_WGS84_To_UKOSMap_H(wpt->latitude, wpt->longitude, &east, &north, map)) {
      unicsv_fatal_outside(wpt);
    }
    auto fieldWidth = fout->fieldWidth();
    *fout << map << unicsv_fieldsep
          << qSetFieldWidth(5) << qSetRealNumberPrecision(0) << east << qSetFieldWidth(fieldWidth)
          << unicsv_fieldsep
          << qSetFieldWidth(5) << north << qSetFieldWidth(fieldWidth);
    break;
  }
  case grid_utm: {
    int zone;
    char zonec;
    double north;
    double east;

    if (! GPS_Math_Known_Datum_To_UTM_EN(lat, lon,
                                         &east, &north, &zone, &zonec, unicsv_datum_idx)) {
      unicsv_fatal_outside(wpt);
    }
    *fout << QStringLiteral("%1").arg(zone, 2, 10, QLatin1Char('0')) << unicsv_fieldsep
          << zonec  << unicsv_fieldsep
          << qSetRealNumberPrecision(0) << east << unicsv_fieldsep
          << north;
    break;
  }
  case grid_swiss: {
    double north;
    double east;

    if (! GPS_Math_WGS84_To_Swiss_EN(wpt->latitude, wpt->longitude, &east, &north)) {
      unicsv_fatal_outside(wpt);
    }
    *fout << qSetRealNumberPrecision(0) << east << unicsv_fieldsep
          << north;
    break;

  }
  default:
    *fout << qSetRealNumberPrecision(llprec) << lat << unicsv_fieldsep
          << lon;
    break;
  }

  if (unicsv_outp_flags[fld_shortname]) {
    unicsv_print_str(shortname);
  }
  if (unicsv_outp_flags[fld_altitude]) {
    if (wpt->altitude != unknown_alt) {
      *fout << unicsv_fieldsep
            << qSetRealNumberPrecision(1) <<  wpt->altitude;
    } else {
      *fout << unicsv_fieldsep;
    }
  }
  if (unicsv_outp_flags[fld_description]) {
    unicsv_print_str(wpt->description);
  }
  if (unicsv_outp_flags[fld_notes]) {
    unicsv_print_str(wpt->notes);
  }
  if (unicsv_outp_flags[fld_symbol]) {
    unicsv_print_str(wpt->icon_descr.isNull() ? "Waypoint" : wpt->icon_descr);
  }
  if (unicsv_outp_flags[fld_depth]) {
    if (wpt->depth_has_value()) {
      *fout << unicsv_fieldsep
            << qSetRealNumberPrecision(3) << wpt->depth_value();
    } else {
      *fout << unicsv_fieldsep;
    }
  }
  if (unicsv_outp_flags[fld_proximity]) {
    if (wpt->proximity_has_value()) {
      *fout << unicsv_fieldsep
            << qSetRealNumberPrecision(0) << wpt->proximity_value();
    } else {
      *fout << unicsv_fieldsep;
    }
  }
  if (unicsv_outp_flags[fld_temperature]) {
    if (wpt->temperature_has_value()) {
      *fout << unicsv_fieldsep
            << qSetRealNumberPrecision(3) << wpt->temperature_value();
    } else {
      *fout << unicsv_fieldsep;
    }
  }
  if (unicsv_outp_flags[fld_speed]) {
    if (wpt->speed_has_value()) {
      *fout << unicsv_fieldsep
            << qSetRealNumberPrecision(2) << wpt->speed_value();
    } else {
      *fout << unicsv_fieldsep;
    }
  }
  if (unicsv_outp_flags[fld_course]) {
    if (wpt->course_has_value()) {
      *fout << unicsv_fieldsep
            << qSetRealNumberPrecision(1) << wpt->course_value();
    } else {
      *fout << unicsv_fieldsep;
    }
  }
  if (unicsv_outp_flags[fld_fix]) {
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
      *fout << unicsv_fieldsep;
    }
  }
  if (unicsv_outp_flags[fld_hdop]) {
    if (wpt->hdop > 0) {
      *fout << unicsv_fieldsep
            << qSetRealNumberPrecision(2) << wpt->hdop;
    } else {
      *fout << unicsv_fieldsep;
    }
  }
  if (unicsv_outp_flags[fld_vdop]) {
    if (wpt->vdop > 0) {
      *fout << unicsv_fieldsep
            << qSetRealNumberPrecision(2) << wpt->vdop;
    } else {
      *fout << unicsv_fieldsep;
    }
  }
  if (unicsv_outp_flags[fld_pdop]) {
    if (wpt->pdop > 0) {
      *fout << unicsv_fieldsep
            << qSetRealNumberPrecision(2) << wpt->pdop;
    } else {
      *fout << unicsv_fieldsep;
    }
  }
  if (unicsv_outp_flags[fld_sat]) {
    if (wpt->sat > 0) {
      *fout << unicsv_fieldsep << wpt->sat;
    } else {
      *fout << unicsv_fieldsep;
    }
  }
  if (unicsv_outp_flags[fld_heartrate]) {
    if (wpt->heartrate != 0) {
      *fout << unicsv_fieldsep << wpt->heartrate;
    } else {
      *fout << unicsv_fieldsep;
    }
  }
  if (unicsv_outp_flags[fld_cadence]) {
    if (wpt->cadence != 0) {
      *fout << unicsv_fieldsep << wpt->cadence;
    } else {
      *fout << unicsv_fieldsep;
    }
  }
  if (unicsv_outp_flags[fld_power]) {
    if (wpt->power > 0) {
      *fout << unicsv_fieldsep
            << qSetRealNumberPrecision(1) << wpt->power;
    } else {
      *fout << unicsv_fieldsep;
    }
  }
  if (unicsv_outp_flags[fld_date]) {
    if (wpt->creation_time.toTime_t() >= 2 * SECONDS_PER_DAY) {
      QDateTime dt;
      if (opt_utc) {
        dt = wpt->GetCreationTime().toOffsetFromUtc(utc_offset);
      } else {
        dt = wpt->GetCreationTime().toLocalTime();
      }
      QString date = dt.toString(u"yyyy/MM/dd");
      *fout << unicsv_fieldsep << date;
    } else {
      *fout << unicsv_fieldsep;
    }
  }
  if (unicsv_outp_flags[fld_time]) {
    if (wpt->creation_time.isValid()) {
      QDateTime dt;
      if (opt_utc) {
        dt = wpt->GetCreationTime().toOffsetFromUtc(utc_offset);
      } else {
        dt = wpt->GetCreationTime().toLocalTime();
      }
      QString out;
      if (dt.time().msec() > 0) {
        out = dt.toString(u"hh:mm:ss.zzz");
      } else {
        out = dt.toString(u"hh:mm:ss");
      }
      *fout << unicsv_fieldsep << out;
    } else {
      *fout << unicsv_fieldsep;
    }
  }
  if (unicsv_outp_flags[fld_url]) {
    if (!wpt->HasUrlLink()) {
      unicsv_print_str("");
    } else {
      const UrlLink& l = wpt->GetUrlLink();
      unicsv_print_str(l.url_);
    }
  }

  if (unicsv_outp_flags[fld_garmin_facility]) {
    unicsv_print_str(garmin_fs_t::get_facility(gmsd, nullptr));
  }
  if (unicsv_outp_flags[fld_garmin_addr]) {
    unicsv_print_str(garmin_fs_t::get_addr(gmsd, nullptr));
  }
  if (unicsv_outp_flags[fld_garmin_city]) {
    unicsv_print_str(garmin_fs_t::get_city(gmsd, nullptr));
  }
  if (unicsv_outp_flags[fld_garmin_postal_code]) {
    unicsv_print_str(garmin_fs_t::get_postal_code(gmsd, nullptr));
  }
  if (unicsv_outp_flags[fld_garmin_state]) {
    unicsv_print_str(garmin_fs_t::get_state(gmsd, nullptr));
  }
  if (unicsv_outp_flags[fld_garmin_country]) {
    unicsv_print_str(garmin_fs_t::get_country(gmsd, nullptr));
  }
  if (unicsv_outp_flags[fld_garmin_phone_nr]) {
    unicsv_print_str(garmin_fs_t::get_phone_nr(gmsd, nullptr));
  }
  if (unicsv_outp_flags[fld_garmin_phone_nr2]) {
    unicsv_print_str(garmin_fs_t::get_phone_nr2(gmsd, nullptr));
  }
  if (unicsv_outp_flags[fld_garmin_fax_nr]) {
    unicsv_print_str(garmin_fs_t::get_fax_nr(gmsd, nullptr));
  }
  if (unicsv_outp_flags[fld_garmin_email]) {
    unicsv_print_str(garmin_fs_t::get_email(gmsd, nullptr));
  }

  if (wpt->EmptyGCData()) {
    gc_data = nullptr;
  } else {
    gc_data = wpt->gc_data;
  }

  if (unicsv_outp_flags[fld_gc_id]) {
    *fout << unicsv_fieldsep;
    if (gc_data && gc_data->id) {
      *fout << gc_data->id;
    }
  }
  if (unicsv_outp_flags[fld_gc_type]) {
    if (gc_data) {
      unicsv_print_str(gc_data->get_type());
    } else {
      *fout << unicsv_fieldsep;
    }
  }
  if (unicsv_outp_flags[fld_gc_container]) {
    if (gc_data) {
      unicsv_print_str(gc_data->get_container());
    } else {
      *fout << unicsv_fieldsep;
    }
  }
  if (unicsv_outp_flags[fld_gc_terr]) {
    *fout << unicsv_fieldsep;
    if (gc_data && gc_data->terr) {
      *fout << qSetRealNumberPrecision(1) << ((double)gc_data->terr / 10);
    }
  }
  if (unicsv_outp_flags[fld_gc_diff]) {
    *fout << unicsv_fieldsep;
    if (gc_data && gc_data->diff) {
      *fout << qSetRealNumberPrecision(1) << ((double)gc_data->diff / 10);
    }
  }
  if (unicsv_outp_flags[fld_gc_is_archived]) {
    if (gc_data && (gc_data->is_archived != Geocache::status_t::gs_unknown)) {
      unicsv_print_str((gc_data->is_archived == Geocache::status_t::gs_true) ? "True" : "False");
    } else {
      *fout << unicsv_fieldsep;
    }
  }
  if (unicsv_outp_flags[fld_gc_is_available]) {
    if (gc_data && (gc_data->is_available != Geocache::status_t::gs_unknown)) {
      unicsv_print_str((gc_data->is_available == Geocache::status_t::gs_true) ? "True" : "False");
    } else {
      *fout << unicsv_fieldsep;
    }
  }
  if (unicsv_outp_flags[fld_gc_last_found]) {
    if (gc_data) {
      unicsv_print_date_time(gc_data->last_found);
    } else {
      *fout << unicsv_fieldsep;
    }
  }
  if (unicsv_outp_flags[fld_gc_placer]) {
    if (gc_data) {
      unicsv_print_str(gc_data->placer);
    } else {
      *fout << unicsv_fieldsep;
    }
  }
  if (unicsv_outp_flags[fld_gc_placer_id]) {
    *fout << unicsv_fieldsep;
    if (gc_data && gc_data->placer_id) {
      *fout << gc_data->placer_id;
    }
  }
  if (unicsv_outp_flags[fld_gc_hint]) {
    if (gc_data) {
      unicsv_print_str(gc_data->hint);
    } else {
      *fout << unicsv_fieldsep;
    }
  }
  if (opt_format) {
    unicsv_print_str(wpt->session->name);
  }
  if (opt_filename) {
    unicsv_print_str(wpt->session->filename);
  }

  *fout << kUnicsvLineSep;
}

/* --------------------------------------------------------------------------- */


void
UnicsvFormat::wr_init(const QString& fname)
{
  if (opt_fields) {
    gbFatal(FatalMsg() <<
          "option 'fields' is not supported on output");
  }
  fout = new gpsbabel::TextStream;
  fout->open(fname, QIODevice::WriteOnly, opt_codec.get().toUtf8());
  fout->setRealNumberNotation(QTextStream::FixedNotation);

  unicsv_outp_flags.reset();
  unicsv_grid_idx = grid_unknown;
  unicsv_datum_idx = kDatumWGS84;
  unicsv_fieldsep = kUnicsvFieldSep;
  unicsv_waypt_ct = 0;

  if (!opt_grid.isEmpty()) {
    bool ok;

    if (int i = opt_grid.toInt(&ok); ok) {
      unicsv_grid_idx = (grid_type) i;
      if ((unicsv_grid_idx < GRID_INDEX_MIN) || (unicsv_grid_idx > GRID_INDEX_MAX))
        gbFatal("Grid index out of range (%d..%d)!\n",
              (int)GRID_INDEX_MIN, (int)GRID_INDEX_MAX);
    } else {
      unicsv_grid_idx = gt_lookup_grid_type(opt_grid);
    }
  }

  if (unicsv_grid_idx == grid_bng)
    /* force datum to "Ord Srvy Grt Britn" / OSGB36 */
    /* ! ignore parameter "Datum" ! */
  {
    unicsv_datum_idx = kDatumOSGB36;
  } else if (unicsv_grid_idx == grid_swiss)
    /* ! ignore parameter "Datum" ! */
  {
    unicsv_datum_idx = kDatumWGS84;  /* internal, becomes CH1903 */
  } else {
    unicsv_datum_idx = gt_lookup_datum_index(opt_datum);
  }

  llprec = opt_prec.get_result();
  utc_offset = opt_utc? opt_utc.get_result() * SECONDS_PER_HOUR : 0;
}

void
UnicsvFormat::wr_deinit()
{
  fout->close();
  delete fout;
  fout = nullptr;
}

// Waypoints are default-on and there's no way to turn them off. This is
// used to see if a user specified (-t OR -r) in addition to the default
// of -w.  It's pretty weak, but it's better than letting the last flag
// 'win' which can result in no data silently being displayed.

void
UnicsvFormat::unicsv_check_modes(bool test)
{
  if (test) {
    gbFatal(FatalMsg() <<
          "Invalid combination of -w, -t, -r selected. Use only one.");
  }
}

void
UnicsvFormat::write()
{
  auto unicsv_waypt_enum_cb_lambda = [this](const Waypoint* waypointp)->void {
    unicsv_waypt_enum_cb(waypointp);
  };
  switch (global_opts.objective) {
  case wptdata:
  case unknown_gpsdata:
    unicsv_check_modes(doing_rtes || doing_trks);
    waypt_disp_all(unicsv_waypt_enum_cb_lambda);
    break;
  case trkdata:
    unicsv_check_modes(doing_rtes);
    track_disp_all(nullptr, nullptr, unicsv_waypt_enum_cb_lambda);
    break;
  case rtedata:
    unicsv_check_modes(doing_trks);
    route_disp_all(nullptr, nullptr, unicsv_waypt_enum_cb_lambda);
    break;
  case posndata:
    gbFatal(FatalMsg() << "Realtime positioning not supported.");
  }

  *fout << "No" << unicsv_fieldsep;

  switch (unicsv_grid_idx) {
  case grid_bng:
    *fout << "BNG-Zone" << unicsv_fieldsep
          << "BNG-East" << unicsv_fieldsep
          << "BNG-North";
    break;
  case grid_utm:
    *fout << "UTM-Zone" << unicsv_fieldsep
          << "UTM-Ch" << unicsv_fieldsep
          << "UTM-East" << unicsv_fieldsep
          << "UTM-North";
    break;
  case grid_swiss:
    *fout << "Swiss-East" << unicsv_fieldsep
          << "Swiss-North";
    break;
  default:
    *fout << "Latitude" << unicsv_fieldsep
          << "Longitude";
  }

  if (unicsv_outp_flags[fld_shortname]) {
    *fout << unicsv_fieldsep << "Name";
  }
  if (unicsv_outp_flags[fld_altitude]) {
    *fout << unicsv_fieldsep << "Altitude";
  }
  if (unicsv_outp_flags[fld_description]) {
    *fout << unicsv_fieldsep << "Description";
  }
  if (unicsv_outp_flags[fld_notes]) {
    *fout << unicsv_fieldsep << "Notes";
  }
  if (unicsv_outp_flags[fld_symbol]) {
    *fout << unicsv_fieldsep << "Symbol";
  }
  if (unicsv_outp_flags[fld_depth]) {
    *fout << unicsv_fieldsep << "Depth";
  }
  if (unicsv_outp_flags[fld_proximity]) {
    *fout << unicsv_fieldsep << "Proximity";
  }
  if (unicsv_outp_flags[fld_temperature]) {
    *fout << unicsv_fieldsep << "Temperature";
  }
  if (unicsv_outp_flags[fld_speed]) {
    *fout << unicsv_fieldsep << "Speed";
  }
  if (unicsv_outp_flags[fld_course]) {
    *fout << unicsv_fieldsep << "Course";
  }
  if (unicsv_outp_flags[fld_fix]) {
    *fout << unicsv_fieldsep << "FIX";
  }
  if (unicsv_outp_flags[fld_hdop]) {
    *fout << unicsv_fieldsep << "HDOP";
  }
  if (unicsv_outp_flags[fld_vdop]) {
    *fout << unicsv_fieldsep << "VDOP";
  }
  if (unicsv_outp_flags[fld_pdop]) {
    *fout << unicsv_fieldsep << "PDOP";
  }
  if (unicsv_outp_flags[fld_sat]) {
    *fout << unicsv_fieldsep << "Satellites";
  }
  if (unicsv_outp_flags[fld_heartrate]) {
    *fout << unicsv_fieldsep << "Heartrate";
  }
  if (unicsv_outp_flags[fld_cadence]) {
    *fout << unicsv_fieldsep << "Cadence";
  }
  if (unicsv_outp_flags[fld_power]) {
    *fout << unicsv_fieldsep << "Power";
  }
  if (unicsv_outp_flags[fld_date]) {
    *fout << unicsv_fieldsep << "Date";
  }
  if (unicsv_outp_flags[fld_time]) {
    *fout << unicsv_fieldsep << "Time";
  }
  if (unicsv_outp_flags[fld_url]) {
    *fout << unicsv_fieldsep << "URL";
  }

  if (unicsv_outp_flags[fld_garmin_facility]) {
    *fout << unicsv_fieldsep << "Facility";
  }
  if (unicsv_outp_flags[fld_garmin_addr]) {
    *fout << unicsv_fieldsep << "Address";
  }
  if (unicsv_outp_flags[fld_garmin_city]) {
    *fout << unicsv_fieldsep << "City";
  }
  if (unicsv_outp_flags[fld_garmin_postal_code]) {
    *fout << unicsv_fieldsep << "PostalCode";
  }
  if (unicsv_outp_flags[fld_garmin_state]) {
    *fout << unicsv_fieldsep << "State";
  }
  if (unicsv_outp_flags[fld_garmin_country]) {
    *fout << unicsv_fieldsep << "Country";
  }
  if (unicsv_outp_flags[fld_garmin_phone_nr]) {
    *fout << unicsv_fieldsep << "Phone";
  }
  if (unicsv_outp_flags[fld_garmin_phone_nr2]) {
    *fout << unicsv_fieldsep << "Phone2";
  }
  if (unicsv_outp_flags[fld_garmin_fax_nr]) {
    *fout << unicsv_fieldsep << "Fax";
  }
  if (unicsv_outp_flags[fld_garmin_email]) {
    *fout << unicsv_fieldsep << "Email";
  }

  if (unicsv_outp_flags[fld_gc_id]) {
    *fout << unicsv_fieldsep << "GCID";
  }
  if (unicsv_outp_flags[fld_gc_type]) {
    *fout << unicsv_fieldsep << "Type";
  }
  if (unicsv_outp_flags[fld_gc_container]) {
    *fout << unicsv_fieldsep << "Container";
  }
  if (unicsv_outp_flags[fld_gc_terr]) {
    *fout << unicsv_fieldsep << "Terrain";
  }
  if (unicsv_outp_flags[fld_gc_diff]) {
    *fout << unicsv_fieldsep << "Difficulty";
  }
  if (unicsv_outp_flags[fld_gc_is_archived]) {
    *fout << unicsv_fieldsep << "Archived";
  }
  if (unicsv_outp_flags[fld_gc_is_available]) {
    *fout << unicsv_fieldsep << "Available";
  }
  if (unicsv_outp_flags[fld_gc_last_found]) {
    *fout << unicsv_fieldsep << "Last Found";
  }
  if (unicsv_outp_flags[fld_gc_placer]) {
    *fout << unicsv_fieldsep << "Placer";
  }
  if (unicsv_outp_flags[fld_gc_placer_id]) {
    *fout << unicsv_fieldsep << "Placer ID";
  }
  if (unicsv_outp_flags[fld_gc_hint]) {
    *fout << unicsv_fieldsep << "Hint";
  }
  if (opt_format) {
    *fout << unicsv_fieldsep << "Format";
  }
  if (opt_filename) {
    *fout << unicsv_fieldsep << "Filename";
  }

  *fout << kUnicsvLineSep;

  auto unicsv_waypt_disp_cb_lambda =  [this](const Waypoint* waypointp)->void {
    unicsv_waypt_disp_cb(waypointp);
  };
  switch (global_opts.objective) {
  case wptdata:
    waypt_disp_all(unicsv_waypt_disp_cb_lambda);
    break;
  case trkdata:
    track_disp_all(nullptr, nullptr, unicsv_waypt_disp_cb_lambda);
    break;
  case rtedata:
    route_disp_all(nullptr, nullptr, unicsv_waypt_disp_cb_lambda);
    break;
  default:
    break;
  }
}

/* --------------------------------------------------------------------------- */
