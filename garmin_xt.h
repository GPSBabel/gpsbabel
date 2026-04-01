/*

   Copyright (C) 2010  Eriks Zelenka, isindir@users.sourceforge.net
   Copyright (C) 2009  jekaeff,
   GMXT2GPX ( http://www.geocaching.hu/users.geo?id=9508 ; http://sites.google.com/site/jekaeff/eng-1 )
   The original code written in Pascal and does not include specific License, however on the project
   webpage it is said to be OpenSource/Libre software
   Copyright (C) 2005  Robert Lipe, robertlipe+source@gpsbabel.org

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
#ifndef GARMIN_XT_H_INCLUDED_
#define GARMIN_XT_H_INCLUDED_

#include <cstdint>   // for uint8_t, uint16_t, uint32_t

#include <QList>     // for QList
#include <QString>   // for QString
#include <QVector>   // for QVector

#include "defs.h"
#include "format.h"  // for Format
#include "gbfile.h"  // for gbfile
#include "option.h"  // for OptionString


class GarminXTFormat : public Format
{
public:
  QVector<arglist_t>* get_args() override
  {
    return &format_garmin_xt_args;
  }

  ff_type get_type() const override
  {
    return ff_type_file;
  }

  QVector<ff_cap> get_cap() const override
  {
    return {ff_cap_none, ff_cap_read, ff_cap_none};
  }

  void rd_init(const QString& fname) override;
  void read() override;
  void rd_deinit() override;

private:
  /* Constants */

  static constexpr int colors[] = {
    0x000000, // Black
    0x00008b, // DarkRed
    0x006400, // DarkGreen
    0x00d7ff, // Gold
    0x8b0000, // DarkBlue
    0x8b008b, // DarkMagenta
    0x8b8b00, // DarkCyan
    0xd3d3d3, // LightGray
    0xa9a9a9, // DarkGray
    0x0000ff, // Red
    0x00ff00, // Green
    0x00ffff, // Yellow
    0xff0000, // Blue
    0xff00ff, // Magenta
    0xffff00, // Cyan
    0xffffff // White
  };

  /* Member Functions */

  uint16_t format_garmin_xt_rd_st_attrs(char* p_trk_name, uint8_t* p_track_color);
  static void format_garmin_xt_decrypt_trk_blk(int Count, uint8_t* TrackBlock);
  static void format_garmin_xt_decomp_trk_blk(uint8_t ii, const uint8_t* TrackBlock, double* Ele, double* Lat, double* Lon, uint32_t* Time);
  static void format_garmin_xt_decomp_last_ele(uint8_t ii, double* PrevEle, const uint8_t* TrackBlock);
  void format_garmin_xt_proc_strk();
  void format_garmin_xt_proc_atrk();

  /* Data Members */

  gbfile* fin{};
  route_head* track{};
  OptionString	opt_xt_ftype;
  OptionInt	opt_trk_header;

  QVector<arglist_t> format_garmin_xt_args = {
    {"ftype", &opt_xt_ftype, "Garmin Mobile XT ([ATRK]/STRK)", "ATRK", ARGTYPE_STRING | ARGTYPE_REQUIRED, ARG_NOMINMAX, nullptr},
    // TODO: SHIFT - can't test behaviour, do not have appropriate files
    //{"trk_header_opt", &opt_trk_header, "Track name processing option ([0]-nrm/1-ign/2-sht)", "0", ARGTYPE_INT, ARG_NOMINMAX},
    {"trk_header", &opt_trk_header, "Track name processing option ([0]-nrm/1-ign)", "0", ARGTYPE_INT, ARG_NOMINMAX, nullptr},
  };
};
#endif // GARMIN_XT_H_INCLUDED_
