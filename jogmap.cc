/*

	Support for XML files from jogmap.de

	Copyright (C) 2009 Robert Lipe  robertlipe+source@gpsbabel.org

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
#include "garmin_tables.h"
#include "jeeps/gpsmath.h"
#include "xmlgeneric.h"
#include <QtCore/QXmlStreamAttributes>

static route_head* trk;

static QVector<arglist_t> jogmap_args = {
};

#define MYNAME "xol"

// static xg_callback	jogmap_shape, xol_shape_end;
// static xg_callback	jogmap_waypt, xol_overlay;

#define XOL "/overlays/overlay"



static void
jogmap_markers(xg_string, const QXmlStreamAttributes*)
{
  trk = new route_head;
  track_add_head(trk);
}

static void
jogmap_marker(xg_string, const QXmlStreamAttributes* attrv)
{
  auto* wpt = new Waypoint;

  if (attrv->hasAttribute("lat")) {
    wpt->latitude = attrv->value("lat").toString().toDouble();
  }

  if (attrv->hasAttribute("lng")) {
    wpt->longitude = attrv->value("lng").toString().toDouble();
  }

  if (trk) {
    track_add_wpt(trk, wpt);
  }
}

static
xg_tag_mapping jogmap_map[] = {
  { jogmap_markers,	cb_start,	"/markers" },
  { jogmap_marker,	cb_start,	"/markers/marker" },
  { nullptr,	(xg_cb_type)0,		nullptr }
};

static void
jogmap_rd_init(const QString& fname)
{
  trk = nullptr;
  xml_init(fname, jogmap_map, nullptr);
}

static void
jogmap_read()
{
  xml_read();
}

static void
jogmap_rd_deinit()
{
  xml_deinit();
}

ff_vecs_t jogmap_vecs = {
  ff_type_file,
  {
    ff_cap_none,		/* waypoints */
    ff_cap_read,		/* tracks */
    ff_cap_none
  },	/* routes */
  jogmap_rd_init,
  nullptr,
  jogmap_rd_deinit,
  nullptr,
  jogmap_read,
  nullptr,
  nullptr,
  &jogmap_args,
  CET_CHARSET_UTF8, 0
  , NULL_POS_OPS,
  nullptr
};
