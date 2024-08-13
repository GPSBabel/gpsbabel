/*
    Copyright (C) 2002 Alex Mottram (geo_alexm at cox-internet.com)
    Copyright (C) 2002-2014 Robert Lipe

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

#ifndef XCSV_H_INCLUDED_
#define XCSV_H_INCLUDED_

#include <ctime>
#include <optional>               // for optional
#include <utility>                // for move

#include <QByteArray>             // for QByteArray
#include <QDate>                  // for QDate
#include <QDateTime>              // for QDateTime
#include <QHash>                  // for QHash
#include <QList>                  // for QList
#include <QString>                // for QString
#include <QStringList>            // for QStringList
#include <QTime>                  // for QTime
#include <QVector>                // for QVector
#include <QtGlobal>               // for qRound64

#include "defs.h"
#include "format.h"               // for Format
#include "garmin_fs.h"            // for garmin_fs_t
#include "mkshort.h"              // for MakeShort
#include "src/core/datetime.h"    // for DateTime
#include "src/core/textstream.h"  // for TextStream

#if CSVFMTS_ENABLED

/*
 * Class describing an xcsv format.
 */

class XcsvStyle
{
public:
  /* Types */

  /*
   * Internal numeric value to associate with each keyword in a style file.
   * To add new keywords, just add an entry here, add it to xcsv_tokens, and
   * handle it in the switch statements in xcsv.cc.
   */
  enum xcsv_token {
    XT_unused = 0,
    XT_ALT_FEET,
    XT_ALT_METERS,
    XT_ANYNAME,
    XT_CADENCE,
    XT_CITY,
    XT_CONSTANT,
    XT_COUNTRY,
    XT_DESCRIPTION,
    XT_EMAIL,
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
    XT_PATH_DISTANCE_NAUTICAL_MILES,
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
  };

  /* something to map fields to waypts */
  struct field_map {
    // We use QByteArrays because consumers want char* data and QByteArrays supply this through constData().
    // If we used QStrings, then we would have to convert to QByteArrays to get the char* data.
    // If we use char* then we have to manage memory allocation/deallocation.
    // TODO: when consumers use QStrings then we can store QStrings instead of QByteArrays.
    QByteArray key;
    QByteArray val;
    QByteArray printfc;
    xcsv_token hashed_key{XT_unused};
    unsigned options{0};

    field_map() = default;
    field_map(QByteArray k, QByteArray v, QByteArray p, xcsv_token hk) :
      key{std::move(k)}, val{std::move(v)}, printfc{std::move(p)}, hashed_key{hk} {}
    field_map(QByteArray k, QByteArray v, QByteArray p, xcsv_token hk, unsigned o) :
      key{std::move(k)}, val{std::move(v)}, printfc{std::move(p)}, hashed_key{hk}, options{o} {}
  };

  /* Constants */

  static constexpr unsigned options_nodelim = 1;
  static constexpr unsigned options_absolute = 2;
  static constexpr unsigned options_optional = 4;

  /* Member Functions */

  static QString xcsv_get_char_from_constant_table(const QString& key);
  static XcsvStyle xcsv_read_style(const QString& fname);

  /* Data Members */

  /* PROLOGUE from style file */
  /* header lines for writing at the top of the file. */
  QStringList prologue;

  /* EPILOGUE from style file */
  /* footer lines for writing at the bottom of the file. */
  QStringList epilogue;

  /* FIELD_DELIMITER from style file */
  /* comma, quote, etc... */
  QString field_delimiter;

  /* FIELD_ENCLOSER from style file */
  /* doublequote, etc... */
  QString field_encloser;

  /* RECORD_DELIMITER from style file */
  /* newline, c/r, etc... */
  QString record_delimiter;

  /* BADCHARS from style file */
  /* characters we never write to output */
  QString badchars;

  /* IFIELDS from style file */
  /* input field mapping */
  QList<field_map> ifields;

  /* OFIELDS from style file */
  /* output field mapping */
  QList<field_map> ofields;

  /* ENCODING from style file */
  QString codecname;

  /* DESCRIPTION from style file */
  /* for help text */
  QString description;

  /* EXTENSION from style file */
  /* preferred filename extension (for wrappers)*/
  QString extension;

  /* FORMAT_TYPE from style file */
  /* format type for GUI wrappers. */
  ff_type type{ff_type_file};

  /* DATUM from style file */
  QString gps_datum_name;

  /* DATATYPE from style file */
  /* can be wptdata, rtedata or trkdata */
  /* ... or ZERO to keep the old behaviour */
  gpsdata_type datatype{unknown_gpsdata};

  /* SHORTLEN from style file */
  std::optional<int> shortlen;

  /* SHORTWHITE from style file */
  std::optional<bool> whitespace_ok;

private:
  /* Types */

  /* Member Functions */

  static QString dequote(const QString& in);
  static void validate_fieldmap(const field_map& fmp, bool is_output);
  static void xcsv_ifield_add(XcsvStyle* style, const QString& qkey, const QString& qval, const QString& qpfc);
  static void xcsv_ofield_add(XcsvStyle* style, const QString& qkey, const QString& qval, const QString& qpfc, unsigned int options);
  static void xcsv_parse_style_line(XcsvStyle* style, QString line);

  /* Data Members */

  static const QHash<QString, xcsv_token> xcsv_tokens;

  /* a table of config file constants mapped to chars */
  static const QHash<QString, QString> xcsv_char_table;
};

class XcsvFormat : public Format
{
public:
  using Format::Format;
  /* Member Functions */
  QVector<arglist_t>* get_args() override
  {
    return &xcsv_args;
  }

  ff_type get_type() const override
  {
    return ff_type_internal;
  }

  QVector<ff_cap> get_cap() const override
  {
    return FF_CAP_RW_WPT; /* This is a bit of a lie for now... */
  }

  void rd_init(const QString& fname) override;
  void read() override;
  void rd_deinit() override;
  void wr_init(const QString& fname) override;
  void write() override;
  void wr_deinit() override;
  void wr_position_init(const QString& fname) override;
  void wr_position(Waypoint* wpt) override;
  void wr_position_deinit() override;

  void xcsv_setup_internal_style(const QString& style_filename);

private:
  /* Types */

  class XcsvFile
  {
  public:

    /* Data Members */

    gpsbabel::TextStream stream;
    QString fname;
    int gps_datum_idx{-1};		/* result of GPS_Lookup_Datum_Index */
    MakeShort mkshort_handle;
  };

  struct xcsv_parse_data {
    QString rte_name;
    QString trk_name;
    bool new_track{false};
    double utm_northing{0};
    double utm_easting{0};
    double utm_zone{0};
    char utm_zonec{'N'};
    UrlLink* link_{nullptr};
    std::optional<bool> lat_dir_positive;
    std::optional<bool> lon_dir_positive;
    QDate local_date;
    QTime local_time;
    QDate utc_date;
    QTime utc_time;
    bool need_datetime{true};
  };

  /* Constants */

  static constexpr char lat_dir(double a)
  {
    return a < 0.0 ? 'S' : 'N';
  }
  static constexpr char lon_dir(double a)
  {
    return a < 0.0 ? 'W' : 'E';
  }

  /* convert excel time (days since 1900) to time_t and back again */
  static constexpr qint64 excel_to_timetms(double a)
  {
    return qRound64((a - 25569.0) * 86400000.0);
  }
  static constexpr double timetms_to_excel(qint64 a)
  {
    return (a / 86400000.0) + 25569.0;
  }

  /* Member Functions */

  static QDate yyyymmdd_to_time(const QString& s);
  QDateTime xcsv_adjust_time(const QDate date, const QTime time, bool is_localtime) const;
  static void sscanftime(const char* s, const char* format, QDate& date, QTime& time);
  static QString writetime(const char* format, time_t t, bool gmt);
  static QString writetime(const char* format, const gpsbabel::DateTime& t, bool gmt);
  static long int time_to_yyyymmdd(const QDateTime& t);
  static garmin_fs_t* gmsd_init(Waypoint* wpt);
  static void xcsv_parse_val(const QString& value, Waypoint* wpt, const XcsvStyle::field_map& fmp, xcsv_parse_data* parse_data, int line_no);
  void xcsv_resetpathlen(const route_head* head);
  void xcsv_waypt_pr(const Waypoint* wpt);
  QString xcsv_replace_tokens(const QString& original) const;

  /* Data Members */

  XcsvFile* xcsv_file{nullptr};
  const XcsvStyle* xcsv_style{nullptr};
  double pathdist = 0;
  std::optional<PositionDeg> old_position;

  int waypt_out_count = 0;
  const route_head* csv_track = nullptr;
  const route_head* csv_route = nullptr;

  char* styleopt = nullptr;
  char* snlenopt = nullptr;
  char* snwhiteopt = nullptr;
  char* snupperopt = nullptr;
  char* snuniqueopt = nullptr;
  char* prefer_shortnames = nullptr;
  char* xcsv_urlbase = nullptr;
  char* opt_datum = nullptr;
  char* opt_utc = nullptr;
  int utc_offset{};

  QString intstylefile;

  QVector<arglist_t> xcsv_args = {
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
      nullptr, ARGTYPE_STRING, ARG_NOMINMAX, nullptr
    },
    {
      "utc",   &opt_utc,   "Write timestamps with offset x to UTC time",
      nullptr, ARGTYPE_INT, "-14", "+14", nullptr
    },
  };

};

#endif // CSVFMTS_ENABLED
#endif // XCSV_H_INCLUDED_
