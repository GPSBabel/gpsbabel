/*

    Legacy format shim.

    Copyright (C) 2019 Robert Lipe, robertlipe+source@gpsbabel.org

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
#ifndef LEGACYFORMAT_H_INCLUDED_
#define LEGACYFORMAT_H_INCLUDED_

#include "defs.h"
#include "format.h"


class LegacyFormat : public Format
{
public:
  LegacyFormat() = default;
  explicit LegacyFormat(const ff_vecs_t& v) : vec(v) {}

  /*******************************************************************************
  * %%%        global callbacks called by gpsbabel main process              %%% *
  *******************************************************************************/

  void rd_init(const QString& fname) override
  {
    if (vec.rd_init != nullptr) {
      vec.rd_init(fname);
    }
  }

  void rd_deinit() override
  {
    if (vec.rd_deinit != nullptr) {
      vec.rd_deinit();
    }
  }

  void read() override
  {
    if (vec.read != nullptr) {
      vec.read();
    }
  }

  void wr_init(const QString& fname) override
  {
    if (vec.wr_init != nullptr) {
      vec.wr_init(fname);
    }
  }

  void wr_deinit() override
  {
    if (vec.wr_deinit != nullptr) {
      vec.wr_deinit();
    }
  }

  void write() override
  {
    if (vec.write != nullptr) {
      vec.write();
    }
  }

  void exit() override
  {
    if (vec.exit != nullptr) {
      vec.exit();
    }
  }

  void rd_position_init(const QString& fname) override
  {
    if (vec.position_ops.rd_init != nullptr) {
      vec.position_ops.rd_init(fname);
    }
  }

  Waypoint* rd_position(posn_status* status) override
  {
    if (vec.position_ops.rd_position != nullptr) {
      return vec.position_ops.rd_position(status);
    }
    return nullptr;
  }

  void rd_position_deinit() override
  {
    if (vec.position_ops.rd_deinit != nullptr) {
      vec.position_ops.rd_deinit();
    }
  }

  void wr_position_init(const QString& fname) override
  {
    if (vec.position_ops.wr_init != nullptr) {
      vec.position_ops.wr_init(fname);
    }
  }

  void wr_position(Waypoint* wpt) override
  {
    if (vec.position_ops.wr_position != nullptr) {
      vec.position_ops.wr_position(wpt);
    }
  }

  void wr_position_deinit() override
  {
    if (vec.position_ops.wr_deinit != nullptr) {
      vec.position_ops.wr_deinit();
    }
  }

  /*******************************************************************************
  * %%%                          Accessors                                   %%% *
  *******************************************************************************/

  QVector<arglist_t>* get_args() override
  {
    return vec.args;
  }

  ff_type get_type() const override
  {
    return vec.type;
  }

  QVector<ff_cap> get_cap() const override
  {
    return vec.cap;
  }

private:
  ff_vecs_t vec;

};
#endif // LEGACYFORMAT_H_INCLUDED_
