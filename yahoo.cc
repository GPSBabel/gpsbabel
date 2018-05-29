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
    Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111 USA

 */

#include "defs.h"
#include "xmlgeneric.h"
#include <QtCore/QXmlStreamAttributes>

static Waypoint* wpt_tmp;
static char* as;

#define MYNAME "yahoo"

static
arglist_t yahoo_args[] = {
  {
    "addrsep", &as,
    "String to separate concatenated address fields (default=\", \")",
    ", ", ARGTYPE_STRING, ARG_NOMINMAX, nullptr
  },
  ARG_TERMINATOR
};

static xg_callback	wpt_s, wpt_lat, wpt_lon, wpt_e;
static xg_callback 	wpt_addr /*, wpt_city, wpt_state, wpt_zip, wpt_country*/;

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

static void
yahoo_rd_init(const QString& fname)
{
  xml_init(fname, gl_map, nullptr);
}

static void
yahoo_read()
{
  xml_read();
}

static void
yahoo_rd_deinit()
{
  xml_deinit();
}

static void
yahoo_wr_init(const QString&)
{
  fatal("Writing file of type %s is not supported\n", MYNAME);
}

void	wpt_s(xg_string, const QXmlStreamAttributes*)
{
  wpt_tmp = new Waypoint;
}

void	wpt_e(xg_string, const QXmlStreamAttributes*)
{
  waypt_add(wpt_tmp);
  wpt_tmp = nullptr;
}

void	wpt_lat(xg_string args, const QXmlStreamAttributes*)
{
  wpt_tmp->latitude = args.toDouble();
}

void	wpt_lon(xg_string args, const QXmlStreamAttributes*)
{
  wpt_tmp->longitude = args.toDouble();
}

void	wpt_addr(xg_string args, const QXmlStreamAttributes*)
{
  if (!wpt_tmp->notes.isEmpty()) {
    wpt_tmp->notes += as;
  }
  wpt_tmp->notes += args;
}

ff_vecs_t yahoo_vecs = {
  ff_type_file,
  { ff_cap_read },
  yahoo_rd_init,
  yahoo_wr_init,
  yahoo_rd_deinit,
  nullptr,
  yahoo_read,
  nullptr,
  nullptr,
  yahoo_args,
  CET_CHARSET_ASCII, 0	/* CET-REVIEW */
  , NULL_POS_OPS,
  nullptr
};
