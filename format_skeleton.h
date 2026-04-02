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

// This is needed only if you have command line options for your
// format.
  QVector<arglist_t>* get_args() override
  {
    return &format_skeleton_args;
  }

// This is required.
  ff_type get_type() const override
  {
    return ff_type_file;
  }

// capabilities below means: we can only read and write waypoints
// please change this depending on your new module

// This is required.
  QVector<ff_cap> get_cap() const override
  {
    return {
      (ff_cap)(ff_cap_read | ff_cap_write)  /* waypoints */,
      ff_cap_none       /* tracks */,
      ff_cap_none       /* routes */
    };
  }

// These are all optional, but you will need at least one of them
// to do anything.
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

//  If you have any options you will need a data member of the appropriate type for each.
//  Notice these are referred to in format_skeleton_args below.
//  These variables will be set for you before your format methods are called based on
//  what the user enters on the command line and your defaults.
//  These are just examples.
//    OptionString opt_icon;
//    OptionInt opt_utc;
//    OptionBool opt_white;
//    OptionDouble opt_lat;


// Add entries for each option to a vector.  If you don't have any options you don't
// need this at all. These are just examples.
// Any element in this vector will appear in command line help.
// Values for ARGTYPE_xxx can be found in defs.h and are used to select the type of option.
//  QVector<arglist_t> format_skeleton_args = {
//    {"icon", &opt_icon, "Icon name", nullptr, ARGTYPE_STRING, ARG_NOMINMAX, nullptr},
//    {"utc", &opt_utc, "Offset from UTC in hours", nullptr, ARGTYPE_INT, "-14", "+14", nullptr},
//    {"white", &opt_white, "Allow whitespace synth. shortnames", nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr},
//    {"lat", &opt_lat, "Latitude for center point (D.DDDDD)", nullptr, ARGTYPE_FLOAT | ARGTYPE_REQUIRED, ARG_NOMINMAX, nullptr},
//  };
};
#endif // FORMAT_SKELETON_H_INCLUDED_
