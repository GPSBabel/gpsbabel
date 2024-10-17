/*
    Output only format for Human Readable formats.

    Copyright (C) 2004 Scott Brynen, scott (at) brynen.com
    Copyright (C) 2002-2014 Robert Lipe, robertlipe+source@gpsbabel.org

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
#ifndef TEXT_H_INCLUDED_
#define TEXT_H_INCLUDED_

#include <QList>                  // for QList
#include <QString>                // for QString
#include <QVector>                // for QVector

#include "defs.h"
#include "format.h"               // for Format
#include "mkshort.h"              // for MakeShort
#include "option.h"               // for OptionBool, OptionCString
#include "src/core/textstream.h"  // for TextStream


class TextFormat : public Format
{
public:
  QVector<arglist_t>* get_args() override
  {
    return &text_args;
  }

  ff_type get_type() const override
  {
    return ff_type_file;
  }

  QVector<ff_cap> get_cap() const override
  {
    /*          waypoints,      tracks,      routes */
    return { ff_cap_write, ff_cap_none, ff_cap_none};
  }

  void wr_init(const QString& fname) override;
  void write() override;
  void wr_deinit() override;

private:
  /* Member Functions */

  void text_disp(const Waypoint* wpt);

  /* Data Members */

  gpsbabel::TextStream* file_out{nullptr};
  MakeShort* mkshort_handle{};

  OptionBool suppresssep;
  OptionBool txt_encrypt;
  OptionBool includelogs;
  OptionCString degformat;
  OptionCString altunits;
  OptionBool split_output;
  int waypoint_count{};
  QString output_name;

  QVector<arglist_t> text_args = {
    {
      "nosep", &suppresssep,
      "Suppress separator lines between waypoints",
      nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    {
      "encrypt", &txt_encrypt,
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
    {
      "splitoutput", &split_output,
      "Write each waypoint in a separate file", nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    }

  };
};
#endif // TEXT_H_INCLUDED_
