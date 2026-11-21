/*
    Write points to SubRip subtitle file (for video geotagging)

    Copyright (C) 2010 Michael von Glasow, michael @t vonglasow d.t com
    Copyright (C) 2014 Gleb Smirnoff, glebius @t FreeBSD d.t org

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
#ifndef SUBRIP_H_INCLUDED_
#define SUBRIP_H_INCLUDED_

#include <concepts>   // for same_as
#include <optional>   // for optional
#include <ranges>     // for ranges
#include <regex>      // for regex, regex_replace
#include <string>     // for string
#include <utility>    // for pair
#include <variant>    // for variant, visit

#include <QList>      // for QList
#include <QDateTime>  // for QDateTime, operator<<
#include <QString>    // for QString
#include <QTime>      // for QTime
#include <QVector>    // for QVector

#include "defs.h"
#include "format.h"   // for Format
#include "option.h"   // for OptionString
#include "src/core/textstream.h"  // for TextStream


class SubripFormat : public Format
{
public:
  QVector<arglist_t>* get_args() override
  {
    return &subrip_args;
  }

  ff_type get_type() const override
  {
    return ff_type_file;
  }

  QVector<ff_cap> get_cap() const override
  {
    return { ff_cap_none, ff_cap_write, ff_cap_none }; // waypoints, track, route; for now, we just do tracks
  }

  void wr_init(const QString& fname) override;
  void write() override;
  void wr_deinit() override;

private:
  /* Member Functions */

  QTime video_time(const QDateTime& dt) const;
  void subrip_prevwp_pr(const Waypoint* waypointp);
  void subrip_trkpt_pr(const Waypoint* waypointp);

  /* 
  functions to prepare each component of a subtitle.
  each subtitle consists of 3 parts:
  */

  // 1. A numeric counter identifying each sequential subtitle
  QString subtitle_counter() { return QString::number(stnum++); };
  // 2. The times that the subtitle should appear on and disappear from the screen
  QString subtitle_onscreen_period(const Waypoint *waypointp) const;
  // 3. Subtitle text itself on one or more lines
  QString subtitle_content() const;

  /* Data Members */

  OptionString opt_videotime;
  OptionString opt_gpstime;
  OptionString opt_gpsdate;
  OptionString opt_format;
  OptionString opt_nodata;
  OptionDouble opt_speedfactor;
  OptionDouble opt_altitudefactor;
  OptionBool opt_localtime;

  QDateTime gps_datetime;    // Date time corresponding to video video_offset_ms
  QDateTime video_datetime;  // Date time corresponding to video time 00:00:00,000.
  int video_offset_ms{0};
  int stnum{1};
  gpsbabel::TextStream* fout{nullptr};
  const Waypoint* prevwpp{nullptr};
  std::optional<double> vspeed;
  std::optional<double> gradient;

  QVector<arglist_t> subrip_args = {
    {"video_time", &opt_videotime, "Video position for which exact GPS time is known (hhmmss[.sss], default is 00:00:00,000)", nullptr, ARGTYPE_STRING, ARG_NOMINMAX, nullptr },
    {"gps_time", &opt_gpstime, "GPS time at position video_time (hhmmss[.sss], default is first timestamp of track)", nullptr, ARGTYPE_STRING, ARG_NOMINMAX, nullptr },
    {"gps_date", &opt_gpsdate, "GPS date at position video_time (yyyymmdd, default is first timestamp of track)", nullptr, ARGTYPE_STRING, ARG_NOMINMAX, nullptr },
    {"format", &opt_format, "Format for subtitles", "{speed:4.1f} m/s {altitude:4.0f} m\\n{hour:02d}:{minute:02d}:{second:02d} Lat={latitude:.5f} Lon={longitude:.5f}", ARGTYPE_STRING, ARG_NOMINMAX, nullptr },
    {"no_data", &opt_nodata, "Substitution for no-data value", "---", ARGTYPE_STRING, ARG_NOMINMAX, nullptr },
    {"speed_factor", &opt_speedfactor, "Speed unit conversion factor", "1.0", ARGTYPE_FLOAT, ARG_NOMINMAX, nullptr },
    {"altitude_factor", &opt_altitudefactor, "Altitude unit conversion factor", "1.0", ARGTYPE_FLOAT, ARG_NOMINMAX, nullptr },
    {"local_time", &opt_localtime, "Hour, minute, second in subtitle is local time instead of UTC", "0", ARGTYPE_BOOL, ARG_NOMINMAX, nullptr },
  };
};



/// @brief Performs placement and formatting of values into a string.
/// These format strings are generally like those expected by std::format(),
/// but should normally have a field name specified similar to python's
/// f-strings. Unlike std::format or python, curly braces do not need to
/// be "doubled" to insert a single brace in the final formatted string.
/// A format string must specify a name for each field, such as
/// `{field_name}`, or `{field_name:0.2f}`.
class FormatString
{
public:
  using Value = std::variant<int64_t, double, std::string, bool>;
  using NamedField = std::pair<const std::string, Value>;

  /// @param fmt_string A string with replacement fields.
  /// @param nan_inf_replacement A string that will replace "nan" or "inf"
  /// when a double is converted to string.
  FormatString(const std::string &fmt_string, const std::string &nan_inf_replacement = "---")
      : original_fmt_string{fmt_string},
        nan_inf_replacement{nan_inf_replacement} {}

  /// @brief Replaces named fields in format string using the mapping of field name to value.
  /// @param fields A mapping of field names to their replacement values. This
  /// can be anything that provides an iterable of `NamedField`.
  /// @return The format string with fields replaced.
  /// @throws std::format_error An error during field replacement.
  template <typename T>
    requires std::ranges::forward_range<T> && std::same_as<std::ranges::range_value_t<T>, NamedField>
  std::string
  format(const T &fields) const
  {
    std::string format_str{original_fmt_string};

    for (const auto &[field_name, field_value] : fields)
    {
      const auto field_spec{std::format("\\{{{}(?::.*?)?\\}}", field_name)};
      const std::regex field_spec_re{field_spec};
      for (std::smatch matched_spec; std::regex_search(format_str, matched_spec, field_spec_re);)
      {
        const auto replacement = format_single_field(matched_spec.str(), field_name, field_value);
        format_str = matched_spec.prefix().str() + replacement + matched_spec.suffix().str();
      }
    }

    return format_str;
  }

private:
  const std::string original_fmt_string;

  const std::string nan_inf_replacement;
  const std::regex nan_inf_re{"nan|-?inf", std::regex_constants::icase};

  /// @brief Replace a single field, possibly containing a field name, with the
  /// formatted version of its value.
  /// @param field_spec A field such as `{}`, `{:0.2f}`, `{field_name}`, or `{field_name:0.2f}`
  /// @param field_name The name of a variable that may appear in `field_spec`.
  /// @param field_value A value to be formatted according to `field_spec`.
  /// @return The final formatted value as a string.
  /// @throws std::format_error An error during field replacement.
  std::string
  format_single_field(std::string field_spec, const std::string &field_name, const Value &field_value) const
  {
    try
    {
      field_spec = std::regex_replace(field_spec, std::regex{field_name}, "");
      auto format_variant = [&](auto &v){ return std::vformat(field_spec, std::make_format_args(v)); };
      auto formatted = std::visit(format_variant, field_value);
      formatted = std::regex_replace(formatted, nan_inf_re, nan_inf_replacement);
      return formatted;
    }
    catch (const std::format_error &e)
    {
      const std::string error = std::format("format error for {}: {}", field_name, e.what());
      throw std::format_error{error};
    }
  }
};


#endif // SUBRIP_H_INCLUDED_
