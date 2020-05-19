/*
    Read Yahoo Geocoded files.

    Copyright (C) 2002-2020 Robert Lipe, gpsbabel.org

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
#ifndef YAHOO_H_INCLUDED_
#define YAHOO_H_INCLUDED_

#include <QtCore/QList>                 // for QList
#include <QtCore/QString>               // for QString
#include <QtCore/QVector>               // for QVector
#include <QtCore/QXmlStreamAttributes>  // for QXmlStreamAttributes

#include "defs.h"
#include "format.h"                     // for Format
#include "xmlgeneric.h"                 // for xg_tag_map_entry, cb_cdata, XgFunctor, cb_end, cb_start

class YahooFormat : public Format
{
public:
  QVector<arglist_t>* get_args() override
  {
    return &yahoo_args;
  }

  ff_type get_type() const override
  {
    return ff_type_file;
  }

  QVector<ff_cap> get_cap() const override
  {
    return {
      ff_cap_read,  // waypoints
      ff_cap_none,  // tracks
      ff_cap_none   // routes
    };
  }

  QString get_encode() const override
  {
    return CET_CHARSET_ASCII;
  }

  int get_fixed_encode() const override
  {
    return 0;
  }

  void rd_init(const QString&) override;
  void read() override;
  void rd_deinit() override;

private:
  Waypoint* wpt_tmp;
  char* as;

  QVector<arglist_t> yahoo_args = {
    {
      "addrsep", &as,
      "String to separate concatenated address fields (default=\", \")",
      ", ", ARGTYPE_STRING, ARG_NOMINMAX, nullptr
    },
  };

  void wpt_s(const QString&, const QXmlStreamAttributes*);
  void wpt_e(const QString&, const QXmlStreamAttributes*);
  void wpt_lat(const QString&, const QXmlStreamAttributes*);
  void wpt_lon(const QString&, const QXmlStreamAttributes*);
  void wpt_addr(const QString&, const QXmlStreamAttributes*);

  QList<xg_functor_map_entry<YahooFormat>> gl_map = {
    {&YahooFormat::wpt_s, cb_start, "/ResultSet/Result"},
    {&YahooFormat::wpt_lat, cb_cdata, "/ResultSet/Result/Latitude"},
    {&YahooFormat::wpt_lon, cb_cdata, "/ResultSet/Result/Longitude"},
    {&YahooFormat::wpt_addr, cb_cdata, "/ResultSet/Result/Address"},
    {&YahooFormat::wpt_addr, cb_cdata, "/ResultSet/Result/City"},
    {&YahooFormat::wpt_addr, cb_cdata, "/ResultSet/Result/State"},
    {&YahooFormat::wpt_addr, cb_cdata, "/ResultSet/Result/Zip"},
    {&YahooFormat::wpt_addr, cb_cdata, "/ResultSet/Result/Country"},
    {&YahooFormat::wpt_e, cb_end, "/ResultSet/Result"}
  };
};

#endif //  YAHOO_H_INCLUDED_
