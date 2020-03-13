/*

	Support for "MagicMaps" project files (.ikt)

	Copyright (C) 2008 Olaf Klein, o.b.klein@gpsbabel.org

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
#include "xmlgeneric.h"
#include <QtCore/QXmlStreamAttributes>

static QVector<arglist_t> ikt_args = {
};

#define MYNAME "ikt"

static QString name, text;

static route_head* track;
static Waypoint* waypt;

static xg_callback	iktobj_waypt, iktobj_type, iktobj_name, iktobj_trkpt, iktobj_text;

#define IKTOBJ   "/Root/Content/MMGeoObjects/MMGeoObject"

/* Here we are working with wildcards in the tag list.
   Please ensure that the longest entries comes first */

static
xg_tag_mapping ikt_map[] = {
  { iktobj_trkpt,	cb_start,	IKTOBJ "_*/PathPoints/Point_*/GeoPosition" },
  { iktobj_type,	cb_cdata,	IKTOBJ "_*/GeoObjectType" },
  { iktobj_waypt,	cb_start,	IKTOBJ "_*/GeoPosition" },
  { iktobj_name,	cb_cdata,	IKTOBJ "_*/Name" },
  { iktobj_text,	cb_cdata,	IKTOBJ "_*/POIDrawable2D/Text" },
  { nullptr,	(xg_cb_type)0,		nullptr }
};

static void
ikt_object_end()
{
  if (track) {
    track->rte_name = name;
    track_add_head(track);
  } else if (waypt) {
    waypt->shortname = name;
    waypt->description = text;
    waypt_add(waypt);
  }

  name = QString();
  text = QString();
  track = nullptr;
  waypt = nullptr;
}

static void
iktobj_waypt(xg_string, const QXmlStreamAttributes* attrv)
{
  if (attrv->hasAttribute("X")) {
    waypt->longitude = attrv->value("X").toString().toDouble();
  }
  if (attrv->hasAttribute("Y")) {
    waypt->latitude = attrv->value("Y").toString().toDouble();
  }
}

static void
iktobj_trkpt(xg_string args, const QXmlStreamAttributes* attrv)
{
  waypt = new Waypoint;
  iktobj_waypt(args, attrv);
  track_add_wpt(track, waypt);
  waypt = nullptr;
}

static void
iktobj_name(xg_string args, const QXmlStreamAttributes*)
{
  name = args;
}

static void
iktobj_text(xg_string args, const QXmlStreamAttributes*)
{
  text = args;
}

static void
iktobj_type(xg_string args, const QXmlStreamAttributes*)
{
  ikt_object_end();
  switch (args.toInt()) {
  case 0:
    waypt = new Waypoint;
    break;
  case 1:
    track = new route_head;
    break;
  default:
    fatal(MYNAME ": Unknown object type %s!\n", qPrintable(args));
  }
}

static void
ikt_rd_init(const QString& fname)
{
  xml_init(fname, ikt_map, nullptr);

  track = nullptr;
  waypt = nullptr;
  name = QString();
  text = QString();
}

static void
ikt_read()
{
  xml_read();
}

static void
ikt_rd_deinit()
{
  ikt_object_end();
  xml_deinit();
}

ff_vecs_t ik3d_vecs = {
  ff_type_file,
  {
    ff_cap_read,	/* waypoints */
    ff_cap_read, 	/* tracks */
    ff_cap_none	/* routes */
  },
  ikt_rd_init,
  nullptr,
  ikt_rd_deinit,
  nullptr,
  ikt_read,
  nullptr,
  nullptr,
  &ikt_args,
  CET_CHARSET_UTF8, 1
  , NULL_POS_OPS,
  nullptr
};
