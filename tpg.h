/*
    National Geographic Topo! TPG file support (Waypoints/Routes)
    Contributed to gpsbabel by Alex Mottram

    For Topo! version 2.x.  Routes are currently not implemented.

    Copyright (C) 2002 Alex Mottram, geo_alexm at cox-internet.com

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
#ifndef TPG_H_INCLUDED_
#define TPG_H_INCLUDED_

#include <QList>      // for QList
#include <QString>    // for QString
#include <QVector>    // for QVector

#include "defs.h"
#include "format.h"   // for Format
#include "gbfile.h"   // for gbfile
#include "mkshort.h"  // for MakeShort
#include "option.h"   // for OptionString


class TpgFormat : public Format
{
public:
  using Format::Format;

  QVector<arglist_t>* get_args() override
  {
    return &tpg_args;
  }

  ff_type get_type() const override
  {
    return ff_type_file;
  }

  QVector<ff_cap> get_cap() const override
  {
    return FF_CAP_RW_WPT;
  }

  void rd_init(const QString& fname) override;
  void read() override;
  void rd_deinit() override;
  void wr_init(const QString& fname) override;
  void write() override;
  void wr_deinit() override;

private:
  /* Member Functions */

  static int valid_tpg_header(char* header, int len);
  void tpg_common_init();
  void tpg_waypt_pr(const Waypoint* wpt);

  /* Data Members */

  gbfile* tpg_file_in{};
  gbfile* tpg_file_out{};
  MakeShort* mkshort_handle{};
  OptionString tpg_datum_opt;
  int tpg_datum_idx{};

  int waypt_out_count{};

  QVector<arglist_t> tpg_args = {
    {"datum", &tpg_datum_opt, "Datum (default=NAD27)", "N. America 1927 mean", ARGTYPE_STRING, ARG_NOMINMAX, nullptr},
  };
};
#endif // TPG_H_INCLUDED_
