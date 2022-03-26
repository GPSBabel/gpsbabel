/*
    Copyright (C) 2014 Robert Lipe

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
#ifndef MAPFACTOR_H_INCLUDED_
#define MAPFACTOR_H_INCLUDED_

#include <QString>           // for QString
#include <QVector>           // for QVector
#include <QXmlStreamReader>  // for QXmlStreamReader
#include <QXmlStreamWriter>  // for QXmlStreamWriter

#include "defs.h"            // for ff_cap, arglist_t, ff_cap_none, CET_CHARSET_UTF8, Waypoint, ff_cap_read, ff_cap_write, ff_type, ff_type_file
#include "format.h"          // for Format
#include "src/core/file.h"   // for File


class MapfactorFormat : public Format
{
public:
  QVector<arglist_t>* get_args() override
  {
    return &mapfactor_args;
  }

  ff_type get_type() const override
  {
    return ff_type_file;
  }

  QVector<ff_cap> get_cap() const override
  {
    /*                                  waypoints,      tracks,      routes */
    return { (ff_cap)(ff_cap_read | ff_cap_write), ff_cap_none, ff_cap_none };
  }

  QString get_encode() const override
  {
    return CET_CHARSET_UTF8;
  }

  int get_fixed_encode() const override
  {
    return 0;
  }

  void rd_init(const QString& fname) override;
  void read() override;
  void wr_init(const QString& fname) override;
  void write() override;
  void wr_deinit() override;

private:
  /* Constants */

  static constexpr double milliarcseconds = 60.0 * 60.0 * 1000.0;

  /* Member Functions */

  void MapfactorRead();
  void mapfactor_waypt_pr(const Waypoint* waypointp) const;

  /* Data Members */

  gpsbabel::File* oqfile{};
  QXmlStreamWriter* writer{};

  QVector<arglist_t> mapfactor_args = {
  };

  QXmlStreamReader reader;
  QString mapfactor_read_fname;
};
#endif // MAPFACTOR_H_INCLUDED_
