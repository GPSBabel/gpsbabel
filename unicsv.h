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
#ifndef UNICSV_H_INCLUDED_
#define UNICSV_H_INCLUDED_

#include <bitset>                 // for bitset
#include <cstdint>                // for uint32_t

#include <QDate>                  // for QDate
#include <QDateTime>              // for QDateTime
#include <QList>                  // for QList
#include <QString>                // for QString
#include <QTime>                  // for QTime
#include <QVector>                // for QVector

#include "defs.h"
#include "format.h"               // for Format
#include "geocache.h"             // for Geocache, Geocache::status_t
#include "option.h"               // for OptionString, OptionBool
#include "src/core/textstream.h"  // for TextStream


class UnicsvFormat : public Format
{
public:
  QVector<arglist_t>* get_args() override
  {
    return &unicsv_args;
  }

  ff_type get_type() const override
  {
    return ff_type_file;
  }

  QVector<ff_cap> get_cap() const override
  {
    return FF_CAP_RW_ALL;
  }

  void rd_init(const QString& fname) override;
  void read() override;
  void rd_deinit() override;
  void wr_init(const QString& fname) override;
  void write() override;
  void wr_deinit() override;

private:
  /* Types */

  /* GPSBabel internal and calculated fields */

  enum field_e {
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
    fld_gc_last_found,
    fld_gc_placer,
    fld_gc_placer_id,
    fld_gc_hint,
    fld_terminator
  };

  struct field_t {
    const QString name;
    field_e type;
    uint32_t options;
  };

  /* Constants */

  /* "kUnicsvFieldSep" and "kUnicsvLineSep" are only used by the writer */

  static constexpr const char* kUnicsvFieldSep = ",";
  static constexpr const char* kUnicsvLineSep = "\r\n";
  static constexpr const char* kUnicsvQuoteChar = "\"";

  static constexpr uint32_t kStrLeft = 1;
  static constexpr uint32_t kStrRight = 2;
  static constexpr uint32_t kStrAny = 4;
  static constexpr uint32_t kStrEqual = 8;
  static constexpr uint32_t kStrCase = 16;

  static constexpr double kUnicsvUnknown = 1e25;

  /* Member Functions */

  static long long int unicsv_parse_gc_code(const QString& str);
  static QDate unicsv_parse_date(const char* str, int* consumed);
  static QTime unicsv_parse_time(const char* str, QDate& date);
  static QTime unicsv_parse_time(const QString& str, QDate& date);
  static Geocache::status_t unicsv_parse_status(const QString& str);
  QDateTime unicsv_adjust_time(QDate date, QTime time, bool is_localtime) const;
  static bool unicsv_compare_fields(const QString& s, const field_t& f);
  void unicsv_fondle_header(QString header);
  void unicsv_parse_one_line(const QString& ibuf);
  [[noreturn]] void unicsv_fatal_outside(const Waypoint* wpt) const;
  void unicsv_print_str(const QString& s) const;
  void unicsv_print_date_time(const QDateTime& idt) const;
  void unicsv_waypt_enum_cb(const Waypoint* wpt);
  void unicsv_waypt_disp_cb(const Waypoint* wpt);
  static void unicsv_check_modes(bool test);

  /* Data Members */

  static const QVector<field_t> fields_def;

  QVector<field_e> unicsv_fields_tab;
  double unicsv_altscale{};
  double unicsv_depthscale{};
  double unicsv_proximityscale{};
  const char* unicsv_fieldsep{nullptr};
  int unicsv_lineno{0};
  gpsbabel::TextStream* fin{nullptr};
  gpsbabel::TextStream* fout{nullptr};
  gpsdata_type unicsv_data_type{unknown_gpsdata};
  route_head* unicsv_track{nullptr};
  route_head* unicsv_route{nullptr};
  std::bitset<fld_terminator> unicsv_outp_flags;
  grid_type unicsv_grid_idx{grid_unknown};
  int unicsv_datum_idx{};
  OptionString opt_datum;
  OptionString opt_grid;
  OptionInt opt_utc;
  OptionBool opt_filename;
  OptionBool opt_format;
  OptionInt opt_prec;
  OptionString opt_fields;
  OptionString opt_codec;
  int unicsv_waypt_ct{};
  char unicsv_detect{};
  int llprec{};
  int utc_offset{};

  QVector<arglist_t> unicsv_args = {
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
      nullptr, ARGTYPE_INT, "-14", "+14", nullptr
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
    {
      "codec", &opt_codec, "codec to use for reading and writing strings (default UTF-8)",
      "UTF-8", ARGTYPE_STRING, ARG_NOMINMAX, nullptr
    },
  };

};
#endif // UNICSV_H_INCLUDED_
