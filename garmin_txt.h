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
#ifndef GARMIN_TXT_H_INCLUDED_
#define GARMIN_TXT_H_INCLUDED_

#include <array>                  // for array
#include <cstdint>                // for uint16_t
#include <ctime>                  // for time_t
#include <utility>                // for pair

#include <QDateTime>              // for QDateTime
#include <QList>                  // for QList
#include <QString>                // for QString
#include <QStringList>            // for QStringList
#include <QVector>                // for QVector

#include "defs.h"
#include "format.h"               // for Format
#include "src/core/textstream.h"  // for TextStream


class GarminTxtFormat : public Format
{
public:
  QVector<arglist_t>* get_args() override
  {
    return &garmin_txt_args;
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
  /* Constants */

  static constexpr double kGarminUnknownAlt = 1.0e25;
  static constexpr char kDefaultDateFormat[] = "dd/mm/yyyy";
  static constexpr char kDefaultTimeFormat[] = "HH:mm:ss";

  static const QVector<QString> headers;

  /* Types */

  struct gtxt_flags_t {
    unsigned int metric:1;
    unsigned int celsius:1;
    unsigned int utc:1;
    unsigned int enum_waypoints:1;
    unsigned int route_header_written:1;
    unsigned int track_header_written:1;
  };

  enum header_type {
    waypt_header = 0,
    rtept_header,
    trkpt_header,
    route_header,
    track_header,
    unknown_header
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

  /* Member Functions */

  static bool is_valid_alt(double alt);
  static const char* get_option_val(const char* option, const char* def);
  void init_date_and_time_format();
  void convert_datum(const Waypoint* wpt, double* dest_lat, double* dest_lon) const;
  void enum_waypt_cb(const Waypoint* wpt);
  void prework_hdr_cb(const route_head* unused);
  void prework_tlr_cb(const route_head* unused);
  void prework_wpt_cb(const Waypoint* wpt);
  void print_position(const Waypoint* wpt);
  void print_date_and_time(time_t time, bool time_only);
  void print_categories(uint16_t categories);
  void print_course(const Waypoint* A, const Waypoint* B);
  void print_distance(double distance, bool no_scale, bool with_tab, int decis);
  void print_speed(double distance, time_t time);
  void print_temperature(float temperature);
  void print_string(const char* fmt, const QString& string);
  void write_waypt(const Waypoint* wpt);
  void route_disp_hdr_cb(const route_head* rte);
  void route_disp_tlr_cb(const route_head* unused);
  void route_disp_wpt_cb(const Waypoint* wpt);
  void track_disp_hdr_cb(const route_head* track);
  void track_disp_tlr_cb(const route_head* unused);
  void track_disp_wpt_cb(const Waypoint* wpt);
  void garmin_txt_utc_option();
  void garmin_txt_adjust_time(QDateTime& dt) const;
  void free_headers();
  static QString strftime_to_timespec(const char* s);
  QDateTime parse_date_and_time(const QString& str);
  uint16_t parse_categories(const QString& str) const;
  bool parse_temperature(const QString& str, double* temperature) const;
  void parse_header(const QStringList& lineparts);
  bool parse_display(const QString& str, int* val) const;
  void bind_fields(header_type ht);
  void parse_grid(const QStringList& lineparts);
  void parse_datum(const QStringList& lineparts);
  void parse_waypoint(const QStringList& lineparts);
  void parse_route_header(const QStringList& lineparts);
  void parse_track_header(const QStringList& lineparts);
  void parse_route_waypoint(const QStringList& lineparts);
  void parse_track_waypoint(const QStringList& lineparts);

  /* Data Members */

  gpsbabel::TextStream* fin = nullptr;
  gpsbabel::TextStream* fout = nullptr;
  route_head* current_trk{};
  route_head* current_rte{};
  int waypoints{};
  int routepoints{};
  const Waypoint** wpt_a{};
  int wpt_a_ct{};
  grid_type grid_index{};
  int datum_index{};
  const char* datum_str{};
  int current_line{};
  QString date_time_format;
  int precision = 3;
  time_t utc_offs = 0;
  gtxt_flags_t gtxt_flags{};

  std::array<QList<std::pair<QString, int>>, unknown_header> header_mapping_info;
  QStringList header_column_names;

  char* opt_datum = nullptr;
  char* opt_dist = nullptr;
  char* opt_temp = nullptr;
  char* opt_date_format = nullptr;
  char* opt_time_format = nullptr;
  char* opt_precision = nullptr;
  char* opt_utc = nullptr;
  char* opt_grid = nullptr;

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

  PathInfo* route_info{};
  int route_idx{};
  PathInfo* cur_info{};
};

#endif // GARMIN_TXT_H_INCLUDED_
