/*

    teletype .way module

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
#ifndef TELETYPE_H_INCLUDED_
#define TELETYPE_H_INCLUDED_

#include <QString>   // for QString
#include <QVector>   // for QVector

#include <cstdint>   // for uint32_t

#include "defs.h"    // for ff_cap, arglist_t, ff_cap_none, CET_CHARSET_ASCII, ff_cap_read, ff_type, ff_type_file
#include "format.h"  // for Format
#include "gbfile.h"  // for gbfile


class TeletypeFormat : public Format
{
public:
  QVector<arglist_t>* get_args() override
  {
    return &teletype_args;
  }

  ff_type get_type() const override
  {
    return ff_type_file;
  }

  QVector<ff_cap> get_cap() const override
  {
    /*                  waypoints,      tracks,      routes */
    return {(ff_cap)(ff_cap_read), ff_cap_none, ff_cap_none};
  }

  QString get_encode() const override
  {
    return CET_CHARSET_ASCII;
  }

  int get_fixed_encode() const override
  {
    return 0;
  }

  void rd_init(const QString& fname) override;
  void read() override;
  void rd_deinit() override;

private:
  /* Data Members */

  uint32_t tty_wpt_count{};
  gbfile* fin{};

  QVector<arglist_t> teletype_args = {
  };

};
#endif // TELETYPE_H_INCLUDED_
