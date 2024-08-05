/*
    Output only format for Human Readable formats.

    Copyright (C) 2004 Scott Brynen, scott (at) brynen.com
    Copyright (C) 2002 Robert Lipe, robertlipe+source@gpsbabel.org

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
#ifndef HTML_H_INCLUDED_
#define HTML_H_INCLUDED_

#include <QString>                // for QString
#include <QVector>                // for QVector

#include "defs.h"
#include "format.h"               // for Format
#include "mkshort.h"              // for MakeShort
#include "src/core/textstream.h"  // for TextStream


class HtmlFormat : public Format
{
public:
  [[nodiscard]] QVector<arglist_t>* get_args() override
  {
    return &html_args;
  }

  [[nodiscard]] ff_type get_type() const override
  {
    return ff_type_file;
  }

  [[nodiscard]] QVector<ff_cap> get_cap() const override
  {
    /*          waypoints,      tracks,      routes */
    return { ff_cap_write, ff_cap_none, ff_cap_none };
  }

  void wr_init(const QString& fname) override;
  void write() override;
  void wr_deinit() override;

private:
  /* Member Functions */

  static QString create_id(int sequence_number);
  void html_disp(const Waypoint* wpt) const;
  void html_index(const Waypoint* wpt) const;

  /* Data Members */

  gpsbabel::TextStream* file_out{nullptr};
  MakeShort* mkshort_handle{};

  int waypoint_number{};

  char* stylesheet = nullptr;
  char* html_encrypt = nullptr;
  char* includelogs = nullptr;
  char* degformat = nullptr;
  char* altunits = nullptr;

  QVector<arglist_t> html_args = {
    {
      "stylesheet", &stylesheet,
      "Path to HTML style sheet", nullptr, ARGTYPE_STRING, ARG_NOMINMAX, nullptr
    },
    {
      "encrypt", &html_encrypt,
      "Encrypt hints using ROT13", nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    {
      "logs", &includelogs,
      "Include groundspeak logs if present", nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    {
      "degformat", &degformat,
      "Degrees output as 'ddd', 'dmm'(default) or 'dms'", "dmm", ARGTYPE_STRING, ARG_NOMINMAX, nullptr
    },
    {
      "altunits", &altunits,
      "Units for altitude (f)eet or (m)etres", "m", ARGTYPE_STRING, ARG_NOMINMAX, nullptr
    },
  };

};
#endif // HTML_H_INCLUDED_
