/*
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
#ifndef GEO_H_INCLUDED_
#define GEO_H_INCLUDED_

#include <QList>             // for QList
#include <QString>           // for QString
#include <QVector>           // for QVector
#include <QXmlStreamReader>  // for QXmlStreamReader
#include <QXmlStreamWriter>  // for QXmlStreamWriter

#include "defs.h"
#include "format.h"          // for Format
#include "geocache.h"        // for Geocache, Geocache::container_t
#include "option.h"          // for OptionBool, OptionString


class GeoFormat : public Format
{
public:
  using Format::Format;

  QVector<arglist_t>* get_args() override
  {
    return &geo_args;
  }

  ff_type get_type() const override
  {
    return ff_type_file;
  }

  QVector<ff_cap> get_cap() const override
  {
    return { (ff_cap)(ff_cap_read | ff_cap_write), ff_cap_none, ff_cap_none };
  }

  void rd_init(const QString& fname) override
  {}
  void read() override;
  void wr_init(const QString& fname) override
  {}
  void write() override;

private:

  /* Member Functions */

  static void GeoReadLoc(QXmlStreamReader& reader);
  void geo_waypt_pr(const Waypoint* waypointp, QXmlStreamWriter& writer);
  static Geocache::container_t wpt_container(const QString& args);

  /* Data Members */

  OptionString deficon;

  QVector<arglist_t> geo_args = {
    {"deficon", &deficon, "Default icon name", nullptr, ARGTYPE_STRING, ARG_NOMINMAX, nullptr },
  };
};
#endif // GEO_H_INCLUDED_
