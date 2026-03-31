/*
    Copyright (C) 2026 Robert Lipe, robertlipe+source@gpsbabel.org

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
#ifndef FORMAT_SKELETON_H_INCLUDED_
#define FORMAT_SKELETON_H_INCLUDED_

#include <QVector>           // for QVector

#include "defs.h"
#include "format.h"          // for Format
#include "option.h"          // for OptionBool, OptionString


class FormatSkeletonFormat : public Format
{
public:
  using Format::Format;

  /* Member Functions */

  QVector<arglist_t>* get_args() override
  {
    return &format_skeleton_args;
  }

  ff_type get_type() const override
  {
    return ff_type_file;
  }

// capabilities below means: we can only read and write waypoints
// please change this depending on your new module

  QVector<ff_cap> get_cap() const override
  {
    return {
      (ff_cap)(ff_cap_read | ff_cap_write)  /* waypoints */,
      ff_cap_none       /* tracks */,
      ff_cap_none       /* routes */
    };
  }

  void rd_init(const QString& fname) override;
  void read() override;
  void rd_deinit() override;
  void wr_init(const QString& fname) override;
  void write() override;
  void wr_deinit() override;
  void exit() override;

private:
  /* Types */

  /* Constants */

  /* Special Member Functions */

  /* Member Functions */

  /* Data Members */

  OptionString opt_icon;
  OptionInt opt_utc;
  OptionBool opt_white;
  OptionDouble opt_lat;

// Any arg in this list will appear in command line help and will be
// populated for you.
// Values for ARGTYPE_xxx can be found in defs.h and are used to
// select the type of option.

  QVector<arglist_t> format_skeleton_args = {
    {"icon", &opt_icon, "Icon name", nullptr, ARGTYPE_STRING, ARG_NOMINMAX, nullptr},
    {"utc", &opt_utc, "Offset from UTC in hours", nullptr, ARGTYPE_INT, "-14", "+14", nullptr},
    {"white", &opt_white, "Allow whitespace synth. shortnames", nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr},
    {"lat", &opt_lat, "Latitude for center point (D.DDDDD)", nullptr, ARGTYPE_FLOAT | ARGTYPE_REQUIRED, ARG_NOMINMAX, nullptr},
  };
};
#endif // FORMAT_SKELETON_H_INCLUDED_
