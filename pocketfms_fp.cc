/*
	Read PocketFMS flightplan files.

	Copyright (C) 2009 Tobias Kretschmar, tobias.kretschmar@gmx.de

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

#include <QtCore/QString>               // for QString
#include <QtCore/QStringRef>            // for QStringRef
#include <QtCore/QXmlStreamAttributes>  // for QXmlStreamAttributes

#include "defs.h"
#include "xmlgeneric.h"                 // for cb_cdata, xg_callback, xg_string, cb_start, cb_end, xg_cb_type, xml_deinit, xml_init, xml_read, xg_tag_mapping


static int isFirst = 1;
static route_head* route = nullptr;
static Waypoint* wpt_to, *wpt_from;
static double dest_altitude;

#define MYNAME "PocketFMS FlightPlan"

static xg_callback	wpt_s, wpt_from_lat, wpt_from_lon, wpt_from_name, wpt_from_elev;
static xg_callback	wpt_e, wpt_to_lat, wpt_to_lon, wpt_to_name, wpt_to_elev, wpt_altitude;

static xg_tag_mapping gl_map[] = {
  { wpt_s,			cb_start, "/PocketFMSFlightplan/LIB" },
  { wpt_from_lat,	cb_cdata, "/PocketFMSFlightplan/LIB/FromPoint/Latitude" },
  { wpt_from_lon,	cb_cdata, "/PocketFMSFlightplan/LIB/FromPoint/Longitude" },
  { wpt_from_elev,	cb_cdata, "/PocketFMSFlightplan/LIB/FromPoint/Elevation" },
  { wpt_from_name,	cb_cdata, "/PocketFMSFlightplan/LIB/FromPoint/FriendlyShortname" },
  { wpt_to_lat,		cb_cdata, "/PocketFMSFlightplan/LIB/ToPoint/Latitude" },
  { wpt_to_lon,		cb_cdata, "/PocketFMSFlightplan/LIB/ToPoint/Longitude" },
  { wpt_to_name,		cb_cdata, "/PocketFMSFlightplan/LIB/ToPoint/FriendlyShortname" },
  { wpt_to_elev,		cb_cdata, "/PocketFMSFlightplan/LIB/ToPoint/Elevation" },
  { wpt_altitude,	cb_start, "/PocketFMSFlightplan/LIB/PlannedAltitude" },
  { wpt_e,			cb_end,   "/PocketFMSFlightplan/LIB" },
  { nullptr,	(xg_cb_type)0,		nullptr}
};

static void
rd_init(const QString& fname)
{
  xml_init(fname, gl_map, nullptr);
}

static void
data_read()
{
  xml_read();
}

static void
rd_deinit()
{
  if (route != nullptr) {
    Waypoint* head = route->waypoint_list.front();
    Waypoint* tail = route->waypoint_list.back();
    if (head != nullptr) {
      route->rte_name = head->shortname;
    }
    route->rte_name += " - ";
    if (tail != nullptr) {
      route->rte_name += tail->shortname;
      tail->altitude = dest_altitude;
    }
  }
  xml_deinit();
}

static void
wr_init(const QString&)
{
  fatal("Writing file of type %s is not supported\n", MYNAME);
}

void	wpt_s(xg_string, const QXmlStreamAttributes*)
{
  if (isFirst == 1) {
    wpt_from = new Waypoint;
    route = route_head_alloc();
    route->rte_desc="PocketFMS flightplan";
    route_add_head(route);
  }
  wpt_to = new Waypoint;
}

void	wpt_e(xg_string, const QXmlStreamAttributes*)
{
  if (isFirst == 1) {
    route_add_wpt(route, wpt_from);
    if (doing_wpts) {
      waypt_add(new Waypoint(*wpt_from));
    }
    wpt_from = nullptr;
    isFirst = 0;
  }
  route_add_wpt(route, wpt_to);
  if (doing_wpts) {
    waypt_add(new Waypoint(*wpt_to));
  }
  wpt_to = nullptr;
}

void	wpt_from_lat(xg_string args, const QXmlStreamAttributes*)
{
  if (wpt_from != nullptr) {
    wpt_from->latitude = args.toDouble();
  }
}

void	wpt_from_lon(xg_string args, const QXmlStreamAttributes*)
{
  if (wpt_from != nullptr) {
    wpt_from->longitude = args.toDouble();
  }
}

void	wpt_from_name(xg_string args, const QXmlStreamAttributes*)
{
  if (wpt_from != nullptr) {
    wpt_from->shortname += args;
  }
}

void	wpt_from_elev(xg_string args, const QXmlStreamAttributes*)
{
  if (wpt_from != nullptr) {
    wpt_from->altitude = FEET_TO_METERS(args.toDouble());
  }
}

void	wpt_to_lat(xg_string args, const QXmlStreamAttributes*)
{
  wpt_to->latitude = args.toDouble();
}

void	wpt_to_lon(xg_string args, const QXmlStreamAttributes*)
{
  wpt_to->longitude = args.toDouble();
}

void	wpt_to_name(xg_string args, const QXmlStreamAttributes*)
{
  wpt_to->shortname += args;
}

void	wpt_to_elev(xg_string args, const QXmlStreamAttributes*)
{
  dest_altitude = FEET_TO_METERS(args.toDouble());
}

void	wpt_altitude(xg_string, const QXmlStreamAttributes* attrv)
{
  int isFeet = 0;

  if (attrv->hasAttribute("Value")) {
    wpt_to->altitude = attrv->value("Value").toString().toDouble();
  }
  if (attrv->hasAttribute("Unit")) {
    isFeet = (attrv->value("Unit") == "ft");
  }
  if (isFeet) {
    wpt_to->altitude = FEET_TO_METERS(wpt_to->altitude);
  }
}

ff_vecs_t pocketfms_fp_vecs = {
  ff_type_file,
  {
    ff_cap_read  	/* waypoints */,
    ff_cap_none 	/* tracks */,
    ff_cap_read  	/* routes */
  },
  rd_init,
  wr_init,
  rd_deinit,
  nullptr,
  data_read,
  nullptr,
  nullptr,
  nullptr,
  CET_CHARSET_ASCII, 0	/* CET-REVIEW */
  , NULL_POS_OPS,
  nullptr
};
