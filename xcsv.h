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
#include <utility>                // for move

#include <QtCore/QByteArray>      // for QByteArray
#include <QtCore/QDateTime>       // for QDateTime
#include <QtCore/QList>           // for QList
#include <QtCore/QString>         // for QString
#include <QtCore/QStringList>     // for QStringList
#include <QtCore/QVector>         // for QVector

#include "defs.h"
#include "format.h"
#include "garmin_fs.h"
#include "src/core/datetime.h"    // for DateTime
#include "src/core/optional.h"    // for optional
#include "src/core/textstream.h"  // for TextStream

#if CSVFMTS_ENABLED

class XcsvFormat : public Format
{
public:
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

  QString get_encode() const override
  {
    return CET_CHARSET_UTF8;
  }

  int get_fixed_encode() const override
  {
    return 0;
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

  /* something to map fields to waypts */
  static constexpr unsigned options_nodelim = 1;
  static constexpr unsigned options_absolute = 2;
  static constexpr unsigned options_optional = 4;

  struct field_map {
    // We use QByteArrays because consumers want char* data and QByteArrays supply this through constData().
    // If we used QStrings, then we would have to convert to QByteArrays to get the char* data.
    // If we use char* then we have to manage memory allocation/deallocation.
    // TODO: when consumers use QStrings then we can store QStrings instead of QByteArrays.
    QByteArray key;
    QByteArray val;
    QByteArray printfc;
    int hashed_key{0};
    unsigned options{0};

    field_map() = default;
    field_map(QByteArray k, QByteArray v, QByteArray p, int hk) : key{std::move(k)},val{std::move(v)},printfc{std::move(p)},hashed_key{hk} {}
    field_map(QByteArray k, QByteArray v, QByteArray p, int hk, unsigned o) : key{std::move(k)},val{std::move(v)},printfc{
      std::move(p)},hashed_key{hk},options{o} {}
  };

  /*
   * Class describing an xcsv format.
   */
  class XcsvStyle
  {
  public:
    static XcsvStyle xcsv_read_internal_style(const char* style_buf);
    static XcsvStyle xcsv_read_style(const char* fname);
  private:
    static QString dequote(const QString& in);
    static void validate_fieldmap(const field_map& fmp, bool is_output);
    static void xcsv_ifield_add(XcsvStyle* style, const QString& qkey, const QString& qval, const QString& qpfc);
    static void xcsv_ofield_add(XcsvStyle* style, const QString& qkey, const QString& qval, const QString& qpfc, unsigned int options);
    static void xcsv_parse_style_line(XcsvStyle* style, QString line);
    static XcsvStyle xcsv_parse_style_buff(const char* sbuff);

  public:
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
    gpsbabel_optional::optional<int> shortlen;

    /* SHORTWHITE from style file */
    gpsbabel_optional::optional<int> whitespace_ok;
  };

  void xcsv_setup_internal_style(const char* style_buf);

private:
  /* macros */
  static constexpr char lat_dir(double a)
  {
    return a < 0.0 ? 'S' : 'N';
  }
  static constexpr char lon_dir(double a)
  {
    return a < 0.0 ? 'W' : 'E';
  }

  /* convert excel time (days since 1900) to time_t and back again */
  static constexpr double excel_to_timet(double a)
  {
    return (a - 25569.0) * 86400.0;
  }
  static constexpr double timet_to_excel(double a)
  {
    return (a / 86400.0) + 25569.0;
  }

  static constexpr int gps_datum_wgs84 = 118; // GPS_Lookup_Datum_Index("WGS 84")

  class XcsvFile
  {
  public:
    XcsvFile() : mkshort_handle(mkshort_new_handle()) {}
    // delete copy and move constructors and assignment operators.
    // The defaults are not appropriate, and we haven't implemented proper ones.
    XcsvFile(const XcsvFile&) = delete;
    XcsvFile& operator=(const XcsvFile&) = delete;
    XcsvFile(XcsvFile&&) = delete;
    XcsvFile& operator=(XcsvFile&&) = delete;
    ~XcsvFile()
    {
      if (mkshort_handle != nullptr) {
        mkshort_del_handle(&mkshort_handle);
      }
    }

    gpsbabel::TextStream stream;
    QString fname;
    int gps_datum_idx{-1};		/* result of GPS_Lookup_Datum_Index */
    short_handle mkshort_handle{nullptr};
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
    gpsbabel_optional::optional<bool> lat_dir_positive;
    gpsbabel_optional::optional<bool> lon_dir_positive;
  };

  static QString xcsv_get_char_from_constant_table(const QString& key);
  static QDateTime yyyymmdd_to_time(const char* s);
  static time_t sscanftime(const char* s, const char* format, int gmt);
  static time_t addhms(const char* s, const char* format);
  static QString writetime(const char* format, time_t t, bool gmt);
  static QString writetime(const char* format, const gpsbabel::DateTime& t, bool gmt);
  static QString writehms(const char* format, time_t t, int gmt);
  static QString writehms(const char* format, const gpsbabel::DateTime& t, int gmt);
  static long int time_to_yyyymmdd(const QDateTime& t);
  static garmin_fs_t* gmsd_init(Waypoint* wpt);
  static void xcsv_parse_val(const QString& value, Waypoint* wpt, const field_map& fmp, xcsv_parse_data* parse_data, int line_no);
  void xcsv_resetpathlen(const route_head* head);
  void xcsv_waypt_pr(const Waypoint* wpt);
  QString xcsv_replace_tokens(const QString& original) const;

  XcsvFile* xcsv_file{nullptr};
  const XcsvStyle* xcsv_style{nullptr};
  double pathdist = 0;
  double oldlon = 999;
  double oldlat = 999;

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

  const char* intstylebuf = nullptr;

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
  };

  /* something to map config file constants to chars */
  struct char_map_t {
    const QString key;
    const QString chars;
  };

  /* a table of config file constants mapped to chars */
  static const char_map_t xcsv_char_table[];

};

#endif // CSVFMTS_ENABLED
#endif // XCSV_H_INCLUDED_
