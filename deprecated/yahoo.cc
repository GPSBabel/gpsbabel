/*
    Read Yahoo Geocoded files.

    Copyright (C) 2005 Robert Lipe

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


#include <QXmlStreamAttributes>         // for QXmlStreamAttributes

#include "defs.h"
#include "yahoo.h"
#include "xmlgeneric.h"                 // for xg_string, build_xg_tag_map, xml_deinit, xml_init, xml_read


#define MYNAME "yahoo"

void
YahooFormat::rd_init(const QString& fname)
{
  xml_init(fname, build_xg_tag_map(this, gl_map), nullptr, nullptr, nullptr, true);
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
