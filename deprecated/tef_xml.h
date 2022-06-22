/*
	Support for XML based "TourExchangeFormat",
	found in Map & Guide Motorrad-Tourenplaner 2005/06

	Copyright (C) 2005 Olaf Klein, o.b.klein@gpsbabel.org

	Based on kml.c, Keyhole "kml" format.
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
#ifndef TEF_XML_H_INCLUDED_
#define TEF_XML_H_INCLUDED_

#include <QList>                 // for QList
#include <QString>               // for QString
#include <QStringView>           // for QStringView
#include <QVector>               // for QVector
#include <QXmlStreamAttributes>  // for QXmlStreamAttributes

#include "defs.h"                // for arglist_t, ff_cap, ff_cap_none, ARGTYPE_BOOL, ARG_NOMINMAX, CET_CHARSET_UTF8, Waypoint, ff_cap_read, ff_type, ff_type_file, route_head
#include "format.h"              // for Format
#include "xmlgeneric.h"          // for xg_functor_map_entry, xg_string, cb_start, cb_end


class TefXMLFormat : public Format
{
public:
  QVector<arglist_t>* get_args() override
  {
    return &tef_xml_args;
  }

  ff_type get_type() const override
  {
    return ff_type_file;
  }

  QVector<ff_cap> get_cap() const override
  {
    /*         waypoints,      tracks,      routes */
    return { ff_cap_none, ff_cap_none, ff_cap_read };
  }

  QString get_encode() const override
  {
    return CET_CHARSET_UTF8;
  }

  int get_fixed_encode() const override
  {
    return 1;
  }

  void rd_init(const QString& fname) override;
  void read() override;
  void rd_deinit() override;

private:
  /* Member Functions */

  static QString fix_notes(const QString& /*unused*/, const QString& notes);
  void waypoint_final();
  static double tef_read_comma_float(QStringView value);
  void tef_start(xg_string /*unused*/, const QXmlStreamAttributes* attrv);
  void tef_header(xg_string /*unused*/, const QXmlStreamAttributes* attrv);
  void tef_list_start(xg_string /*unused*/, const QXmlStreamAttributes* attrv);
  void tef_item_end(xg_string /*unused*/, const QXmlStreamAttributes* /*unused*/);
  void tef_list_end(xg_string /*unused*/, const QXmlStreamAttributes* /*unused*/);
  void tef_item_start(xg_string /*unused*/, const QXmlStreamAttributes* attrv);
  void tef_point(xg_string /*unused*/, const QXmlStreamAttributes* attrv);

  /* Data Members */

  Waypoint* wpt_tmp{};
  int item_count{};
  int waypoints{};
  double version{};
  route_head* route = nullptr;

  char* routevia = nullptr;

  QVector<arglist_t> tef_xml_args = {
    {
      "routevia", &routevia, "Include only via stations in route",
      nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    }
  };

  QList<xg_functor_map_entry<TefXMLFormat>>  tef_xml_map = {
    { &TefXMLFormat::tef_start,		cb_start,	"/TEF" },
    { &TefXMLFormat::tef_header,		cb_start,	"/TEF/Header" },
    { &TefXMLFormat::tef_list_start,	cb_start,	"/TEF/WaypointList" },
    { &TefXMLFormat::tef_item_start,	cb_start,	"/TEF/WaypointList/Item" },
    { &TefXMLFormat::tef_point,		cb_start,	"/TEF/WaypointList/Item/Point" },
    { &TefXMLFormat::tef_item_end,		cb_end,		"/TEF/WaypointList/Item" },
    { &TefXMLFormat::tef_list_end,		cb_end,		"/TEF/WaypointList" }
  };
};
#endif // TEF_XML_H_INCLUDED_
