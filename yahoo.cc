/*
    Read Yahoo Geocoded files.

    Copyright (C) 2005 Robert Lipe, robertlipe+source@gpsbabel.org

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


#include "defs.h"
#include "yahoo.h"
#include "xmlgeneric.h"
#include <QtCore/QXmlStreamAttributes>


#define MYNAME "yahoo"

// static xg_callback	wpt_s, wpt_lat, wpt_lon, wpt_e;
// static xg_callback 	wpt_addr /*, wpt_city, wpt_state, wpt_zip, wpt_country*/;
#if 0
static xg_tag_mapping gl_map[] = {
  { wpt_s,	cb_start, "/ResultSet/Result" },
  { wpt_lat,	cb_cdata, "/ResultSet/Result/Latitude" },
  { wpt_lon,	cb_cdata, "/ResultSet/Result/Longitude" },
  { wpt_addr,	cb_cdata, "/ResultSet/Result/Address" },
  { wpt_addr,	cb_cdata, "/ResultSet/Result/City" },
  { wpt_addr,	cb_cdata, "/ResultSet/Result/State" },
  { wpt_addr,	cb_cdata, "/ResultSet/Result/Zip" },
  { wpt_addr,	cb_cdata, "/ResultSet/Result/Country" },
  { wpt_e,	cb_end,   "/ResultSet/Result" },
  { nullptr,	(xg_cb_type)0,         nullptr}
};
#endif

void
YahooFormat::rd_init(const QString& fname)
{
abort();
//  xml_init(fname, gl_map, nullptr);
}

void
YahooFormat::read()
{
  xml_read();
}

void
YahooFormat::rd_deinit()
{
  xml_deinit();
}

void
YahooFormat::wpt_s(xg_string, const QXmlStreamAttributes*)
{
  wpt_tmp = new Waypoint;
}

void
YahooFormat::wpt_e(xg_string, const QXmlStreamAttributes*)
{
  waypt_add(wpt_tmp);
  wpt_tmp = nullptr;
}

void
YahooFormat::wpt_lat(xg_string args, const QXmlStreamAttributes*)
{
  wpt_tmp->latitude = args.toDouble();
}

void
YahooFormat::wpt_lon(xg_string args, const QXmlStreamAttributes*)
{
  wpt_tmp->longitude = args.toDouble();
}

void
YahooFormat::wpt_addr(xg_string args, const QXmlStreamAttributes*)
{
  if (!wpt_tmp->notes.isEmpty()) {
    wpt_tmp->notes += as;
  }
  wpt_tmp->notes += args;
}
#if 0
ff_vecs_t yahoo_vecs = {
  ff_type_file,
  { ff_cap_read, ff_cap_none, ff_cap_none },
  yahoo_rd_init,
  nullptr,
  yahoo_rd_deinit,
  nullptr,
  yahoo_read,
  nullptr,
  nullptr,
  &yahoo_args,
  CET_CHARSET_ASCII, 0	/* CET-REVIEW */
  , NULL_POS_OPS,
  nullptr
};
#endif
