/*
    OziExplorer Waypoints/Tracks/Routes
    Comma Delimited

    As described in OziExplorer Help File

    Copyright (C) 2002-2005 Robert Lipe, robertlipe+source@gpsbabel.org

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

    Reference:
    https://www.oziexplorer4.com/eng/help/fileformats.html

    According to the OZI Explorer developer:
    "There is no specified character set, it defaults to whatever 8 bit
     character set "Windows" defaults to - normally CP-1252 but can vary
     depending on Windows regional settings."

    According to the reference, for some text fields:
    "comma's not allowed in text fields, character 209 can be used instead
     and a comma will be substituted."
    This could work for windows-1252, but not for utf-8.
    We don't support any special handling for character 209.

 */
#ifndef OZI_H_INCLUDED_
#define OZI_H_INCLUDED_

#include <QIODeviceBase>          // for QIODeviceBase, QIODeviceBase::OpenModeFlag
#include <QString>                // for QString
#include <QVector>                // for QVector

#include "defs.h"
#include "format.h"               // for Format
#include "formspec.h"             // for FormatSpecificData, kFsOzi
#include "mkshort.h"              // for MakeShort
#include "src/core/textstream.h"  // for TextStream


class OziFormat : public Format
{
public:
  QVector<arglist_t>* get_args() override
  {
    return &ozi_args;
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

  struct ozi_fsdata : FormatSpecificData {
    ozi_fsdata() : FormatSpecificData(kFsOzi) {}

    ozi_fsdata* clone() const override
    {
      return new ozi_fsdata(*this);
    }

    int fgcolor{0};
    int bgcolor{65535};
  };

  /* Member Functions */

  void ozi_open_io(const QString& fname, QIODeviceBase::OpenModeFlag mode);
  void ozi_close_io();
  ozi_fsdata* ozi_alloc_fsdata();
  static QString ozi_get_time_str(const Waypoint* waypointp);
  static void ozi_set_time_str(const QString& str, Waypoint* waypointp);
  void ozi_convert_datum(Waypoint* wpt) const;
  void ozi_openfile(const QString& fname);
  void ozi_track_hdr(const route_head* rte);
  void ozi_track_disp(const Waypoint* waypointp);
  void ozi_track_pr();
  void ozi_route_hdr(const route_head* rte);
  void ozi_route_disp(const Waypoint* waypointp);
  void ozi_route_pr();
  void ozi_init_units(int direction);
  void ozi_parse_waypt(int field, const QString& str, Waypoint* wpt_tmp, ozi_fsdata* fsdata) const;
  void ozi_parse_track(int field, const QString& str, Waypoint* wpt_tmp, char* trk_name);
  static void ozi_parse_routepoint(int field, const QString& str, Waypoint* wpt_tmp);
  void ozi_parse_routeheader(int field, const QString& str);
  void data_read();
  void ozi_waypt_pr(const Waypoint* wpt, int index);
  void data_write();

  /* Data Members */

  gpsbabel::TextStream* stream = nullptr;

  MakeShort* mkshort_handle{};
  route_head* trk_head{};
  route_head* rte_head{};

  int track_out_count{};
  int route_out_count{};
  int route_wpt_count{};
  int new_track{};

  char* snlenopt = nullptr;
  char* snwhiteopt = nullptr;
  char* snupperopt = nullptr;
  char* snuniqueopt = nullptr;
  char* wptfgcolor = nullptr;
  char* wptbgcolor = nullptr;
  char* pack_opt = nullptr;
  int datum{};
  char* proximityarg = nullptr;
  double proximity{};
  char* altunit_opt{};
  char* proxunit_opt{};
  char altunit{};
  char proxunit{};
  double alt_scale{};
  double prox_scale{};
  char* opt_codec{};

  QVector<arglist_t> ozi_args = {
    {
      "pack", &pack_opt, "Write all tracks into one file",
      nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    {
      "snlen", &snlenopt, "Max synthesized shortname length",
      "32", ARGTYPE_INT, "1", nullptr, nullptr
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
      "wptfgcolor", &wptfgcolor, "Waypoint foreground color",
      "black", ARGTYPE_STRING, ARG_NOMINMAX, nullptr
    },
    {
      "wptbgcolor", &wptbgcolor, "Waypoint background color",
      "yellow", ARGTYPE_STRING, ARG_NOMINMAX, nullptr
    },
    {
      "proximity", &proximityarg, "Proximity distance",
      "0", ARGTYPE_STRING, ARG_NOMINMAX, nullptr
    },
    {
      "altunit", &altunit_opt, "Unit used in altitude values",
      "feet", ARGTYPE_STRING, ARG_NOMINMAX, nullptr
    },
    {
      "proxunit", &proxunit_opt, "Unit used in proximity values",
      "miles", ARGTYPE_STRING, ARG_NOMINMAX, nullptr
    },
    {
      "codec", &opt_codec, "codec to use for reading and writing strings (default windows-1252)",
      "windows-1252", ARGTYPE_STRING, ARG_NOMINMAX, nullptr
    },
  };

  gpsdata_type ozi_objective{unknown_gpsdata};

  QString ozi_ofname;
};
#endif // OZI_H_INCLUDED_
