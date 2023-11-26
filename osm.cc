/*

	Support for "OpenStreetMap" data files (.xml)

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

#include <cstring>                     // for strlen, strchr, st

#include <QByteArray>                  // for QByteArray
#include <QIODevice>                   // for operator|, QIODevice, QIODevice::Text, QIODevice::WriteOnly
#include <QLatin1String>               // for QLatin1String
#include <QPair>                       // for QPair, operator==
#include <QString>                     // for QString, operator==, operator+
#include <QXmlStreamAttributes>        // for QXmlStreamAttributes
#include <QtGlobal>                    // for qMax, qPrintable

#include "defs.h"
#include "osm.h"
#include "src/core/datetime.h"         // for DateTime
#include "src/core/xmlstreamwriter.h"  // for XmlStreamWriter
#include "xmlgeneric.h"                // for xg_string, build_xg_tag_map, xml_deinit, xml_init, xml_read


#define MYNAME "osm"

const char* const OsmFormat::osm_features[] = {
  "- dummy -",	/*  0 */
  "aeroway",	/*  1 */
  "amenity",	/*  2 */
  "building",	/*  3 */
  "cycleway",	/*  4 */
  "railway",	/*  5 */
  "highway",	/*  6 */
  "historic",	/*  7 */
  "landuse",	/*  8 */
  "leisure",	/*  9 */
  "man_made",	/* 10 */
  "military",	/* 11 */
  "natural",	/* 12 */
  "place",	/* 13 */
  "power",	/* 14 */
  "shop",		/* 15 */
  "sport",	/* 16 */
  "tourism",	/* 17 */
  "waterway",	/* 18 */
  "aerialway",	/* 19 */
  nullptr
};

/* based on <http://wiki.openstreetmap.org/index.php/Map_Features> */

const OsmFormat::osm_icon_mapping_t OsmFormat::osm_icon_mappings[] = {

  /* cycleway ...*/

  /* highway ...*/

//	{ 6, "mini_roundabout",		"?" },
//	{ 6, "stop",			"?" },
//	{ 6, "traffic_signals",		"?" },
//	{ 6, "crossing",		"?" },
//	{ 6, "gate",			"?" },
//	{ 6, "stile",			"?" },
//	{ 6, "cattle_grid",		"?" },
//	{ 6, "toll_booth",		"?" },
//	{ 6, "incline",			"?" },
//	{ 6, "incline_steep",		"?" },
//	{ 6, "viaduct",			"?" },
//	{ 6, "motorway_junction",	"?" },
//	{ 6, "services",		"?" },
//	{ 6, "ford",			"?" },
//	{ 6, "bus_stop",		"?" },
//	{ 6, "turning_circle",		"?" },
//	{ 6, "User Defined",		"?" },

  /* waterway ... */

  { 18, "dock",			"Dock" },
//	{ 18, "lock_gate",		"?" },
//	{ 18, "turning_point",		"?" },
//	{ 18, "aqueduct",		"?" },
//	{ 18, "boatyard",		"?" },
//	{ 18, "water_point",		"?" },
//	{ 18, "waste_disposal",		"?" },
//	{ 18, "mooring",		"?" },
//	{ 18, "weir",			"?" },
//	{ 18, "User Defined",		"?" },

  /* railway ... */

//	{ 5, "station",			"?" },
//	{ 5, "halt",			"?" },
//	{ 5, "tram_stop",		"?" },
//	{ 5, "viaduct",			"?" },
  { 5, "crossing",		"Crossing" },
//	{ 5, "level_crossing",		"?" },
//	{ 5, "subway_entrance",		"?" },
//	{ 5, "turntable",		"?" },
//	{ 5, "User Defined",		"?" },

  /* aeroway ... */

  { 1, "aerodrome",		"Airport" },
  { 1, "terminal",		"Airport" },
  { 1, "helipad",			"Heliport" },
//	{ 1, "User Defined",		"?" },

  /* aerialway ... */

//	{ 19, "User Defined",		"?" },

  /* power ... */

//	{ 14, "tower",			"?" },
//	{ 14, "sub_station",		"?" },
//	{ 14, "generator",		"?" },

  /* man_made ... */

//	{ 10, "works",			"?" },
//	{ 10, "beacon",			"?" },
//	{ 10, "survey_point",		"?" },
//	{ 10, "power_wind",		"?" },
//	{ 10, "power_hydro",		"?" },
//	{ 10, "power_fossil",		"?" },
//	{ 10, "power_nuclear",		"?" },
//	{ 10, "tower",			"?" },
//	{ 10, "water_tower",		"?" },
//	{ 10, "gasometer",		"?" },
//	{ 10, "reservoir_covered",	"?" },
//	{ 10, "lighthouse",		"?" },
//	{ 10, "windmill",		"?" },
//	{ 10, "wastewater_plant",	"?" },
//	{ 10, "crane",			"?" },
//	{ 10, "User Defined",		"?" },

  /* building ... */

  { 3, "yes",			"Building" },
//	{ 3, "User Defined",		"?" },

  /* leisure ... */

//	{ 9, "sports_centre",		"?" },
  { 9, "golf_course",		"Golf Course" },
  { 9, "stadium",			"Stadium" },
//	{ 9, "track",			"?" },
//	{ 9, "pitch",			"?" },
//	{ 9, "water_park",		"?" },
  { 9, "marina",			"Marina" },
//	{ 9, "slipway",			"?" },
  { 9, "fishing",			"Fishing Area" },
//	{ 9, "nature_reserve",		"?" },
  { 9, "park",			"Park" },
//	{ 9, "playground",		"?" },
//	{ 9, "garden",			"?" },
//	{ 9, "common",			"?" },
//	{ 9, "User Defined",		"?" },

  /* amenity ... */

  { 2, "pub",			"Bar" },
//	{ 2, "biergarten",		"?" },
  { 2, "nightclub",		"Bar" },
//	{ 2, "cafe",			"?" },
  { 2, "restaurant",		"Restaurant" },
  { 2, "fast_food",		"Fast Food" },
  { 2, "parking",			"Parking Area" },
//	{ 2, "bicycle_parking",		"?" },
//	{ 2, "bicycle_rental",		"?" },
  { 2, "car_rental",		"Car Rental" },
//	{ 2, "car_sharing",		"?" },
//	{ 2, "taxi",			"?" },
  { 2, "fuel",			"Gas Station" },
  { 2, "telephone",		"Telephone" },
  { 2, "toilets",			"Restroom" },
//	{ 2, "recycling",		"?" },
//	{ 2, "public_building",		"?" },
  { 2, "townhall",		"City Hall" },
//	{ 2, "place_of_worship",	"?" },
//	{ 2, "grave_yard",		"?" },
  { 2, "post_office",		"Post Office" },
//	{ 2, "post_box",		"?" },
  { 2, "school",			"School" },
//	{ 2, "university",		"?" },
//	{ 2, "college",			"?" },
  { 2, "pharmacy",		"Pharmacy" },
  { 2, "hospital",		"Medical Facility" },
//	{ 2, "library",			"?" },
  { 2, "police",			"Police Station" },
//	{ 2, "fire_station",		"?" },
//	{ 2, "bus_station",		"?" },
//	{ 2, "theatre",			"?" },
//	{ 2, "cinema",			"?" },
//	{ 2, "arts_centre",		"?" },
//	{ 2, "courthouse",		"?" },
//	{ 2, "prison",			"?" },
  { 2, "bank",			"Bank" },
//	{ 2, "bureau_de_change",	"?" },
//	{ 2, "atm",			"?" },
//	{ 2, "fountain",		"?" },
//	{ 2, "User Defined",		"?" },

  /* shop ... */

//	{ 15, "supermarket",		"?" },
  { 15, "convenience",		"Convenience Store" },
//	{ 15, "butcher",		"?" },
//	{ 15, "bicycle",		"?" },
//	{ 15, "doityourself",		"?" },
//	{ 15, "dry_cleaning",		"?" },
//	{ 15, "laundry",		"?" },
//	{ 15, "outdoor",		"?" },
//	{ 15, "kiosk",			"?" },
//	{ 15, "User Defined",		"?" },

  /* tourism ... */

  { 17, "information",		"Information" },
  { 17, "hotel",			"Hotel" },
  { 17, "motel",			"Lodging" },
  { 17, "guest_house",		"Lodging" },
  { 17, "hostel",			"Lodging" },
  { 17, "camp_site",		"Campground" },
  { 17, "caravan_site",		"RV Park" },
  { 17, "picnic_site",		"Picnic Area" },
  { 17, "viewpoint",		"Scenic Area" },
//	{ 17, "theme_park",		"?" },
//	{ 17, "attraction",		"?" },
  { 17, "zoo",			"Zoo" },
//	{ 17, "artwork",		"?" },
  { 17, "museum",			"Museum" },
//	{ 17, "User Defined",		"?" },

  /* historic ... */

//	{ 7, "castle",			"?" },
//	{ 7, "monument",		"?" },
//	{ 7, "memorial",		"?" },
//	{ 7, "archaeological_site",	"?" },
//	{ 7, "ruins",			"?" },
//	{ 7, "battlefield",		"?" },
//	{ 7, "User Defined",		"?" },

  /* landuse ... */

//	{ 8, "farm",			"?" },
//	{ 8, "quarry",			"?" },
//	{ 8, "landfill",		"?" },
//	{ 8, "basin",			"?" },
//	{ 8, "reservoir",		"?" },
  { 8, "forest",			"Forest" },
//	{ 8, "allotments",		"?" },
//	{ 8, "residential",		"?" },
//	{ 8, "retail",			"?" },
//	{ 8, "commercial",		"?" },
//	{ 8, "industrial",		"?" },
//	{ 8, "brownfield",		"?" },
//	{ 8, "greenfield",		"?" },
//	{ 8, "railway",			"?" },
//	{ 8, "construction",		"?" },
  { 8, "military",		"Military" },
  { 8, "cemetery",		"Cemetery" },
//	{ 8, "village_green",		"?" },
//	{ 8, "recreation_ground",	"?" },
//	{ 8, "User Defined",		"?" },

  /* military ... */

//	{ 11, "airfield",		"?" },
//	{ 11, "bunker",			"?" },
//	{ 11, "barracks",		"?" },
//	{ 11, "danger_area",		"?" },
//	{ 11, "range",			"?" },
//	{ 11, "naval_base",		"?" },
//	{ 11, "User Defined",		"?" },

  /* natural ... */

//	{ 12, "spring",			"?" },
//	{ 12, "peak",			"?" },
//	{ 12, "glacier",		"?" },
//	{ 12, "volcano",		"?" },
//	{ 12, "cliff",			"?" },
//	{ 12, "scree",			"?" },
//	{ 12, "scrub",			"?" },
//	{ 12, "fell",			"?" },
//	{ 12, "heath",			"?" },
//	{ 12, "wood",			"?" },
//	{ 12, "marsh",			"?" },
//	{ 12, "water",			"?" },
//	{ 12, "coastline",		"?" },
//	{ 12, "mud",			"?" },
  { 12, "beach",			"Beach" },
//	{ 12, "bay",			"?" },
//	{ 12, "land",			"?" },
//	{ 12, "cave_entrance",		"?" },
//	{ 12, "User Defined",		"?" },

  /* sport ... */

//	{ 16, "10pin",			"?" },
//	{ 16, "athletics",		"?" },
//	{ 16, "australian_football",	"?" },
//	{ 16, "baseball",		"?" },
//	{ 16, "basketball",		"?" },
//	{ 16, "boules",			"?" },
//	{ 16, "bowls",			"?" },
//	{ 16, "climbing",		"?" },
//	{ 16, "cricket",		"?" },
//	{ 16, "cricket_nets",		"?" },
//	{ 16, "croquet",		"?" },
//	{ 16, "cycling",		"?" },
//	{ 16, "dog_racing",		"?" },
//	{ 16, "equestrian",		"?" },
//	{ 16, "football",		"?" },
//	{ 16, "golf",			"?" },
//	{ 16, "gymnastics",		"?" },
//	{ 16, "hockey",			"?" },
//	{ 16, "horse_racing",		"?" },
//	{ 16, "motor",			"?" },
//	{ 16, "multi",			"?" },
//	{ 16, "pelota",			"?" },
//	{ 16, "racquet",		"?" },
//	{ 16, "rugby",			"?" },
//	{ 16, "skating",		"?" },
//	{ 16, "skateboard",		"?" },
//	{ 16, "soccer",			"?" },
  { 16, "swimming",		"Swimming Area" },
  { 16, "skiing",			"Skiing Area" },
//	{ 16, "table_tennis",		"?" },
//	{ 16, "tennis",			"?" },
//	{ 16, "orienteering",		"?" },
//	{ 16, "User Defined",		"?" },

  /* place ... */

//	{ 13, "continent",		"?" },
//	{ 13, "country",		"?" },
//	{ 13, "state",			"?" },
//	{ 13, "region",			"?" },
//	{ 13, "county",			"?" },
  { 13, "city",			"City (Large)" },
  { 13, "town",			"City (Medium)" },
  { 13, "village",		"City (Small)" },
//	{ 13, "hamlet",			"?" },
//	{ 13, "suburb",			"?" },
//	{ 13, "locality",		"?" },
//	{ 13, "island",			"?" },
//	{ 13, "User Defined",		"?" },

  { -1, nullptr, nullptr }
};

/*******************************************************************************/
/*                                   READER                                    */
/*-----------------------------------------------------------------------------*/

void
OsmFormat::osm_features_init()
{
  /* the first of osm_features is a place holder */
  for (int i = 1; osm_features[i]; ++i) {
    keys.insert(osm_features[i], i);
  }

  for (int i = 0; osm_icon_mappings[i].value; ++i) {
    QPair<int, QString> key(osm_icon_mappings[i].key, osm_icon_mappings[i].value);
    values.insert(key, &osm_icon_mappings[i]);
  }
}

char
OsmFormat::osm_feature_ikey(const QString& key) const
{
  return keys.value(key, -1);
}

QString
OsmFormat::osm_feature_symbol(const int ikey, const char* value) const
{
  QPair<int, QString> key(ikey, value);

  QString result;
  if (values.contains(key)) {
    result = values.value(key)->icon;
  } else {
    result = QStringLiteral("%1:%2").arg(osm_features[ikey], value);
  }
  return result;
}

QString
OsmFormat::osm_strip_html(const QString& str)
{
  return strip_html(str);	// util.cc
}

void
OsmFormat::osm_node_end(xg_string /*unused*/, const QXmlStreamAttributes* /*unused*/)
{
  if (wpt) {
    if (wpt->wpt_flags.fmt_use) {
      waypt_add(wpt);
    } else {
      delete wpt;
    }
    wpt = nullptr;
  }
}

void
OsmFormat::osm_node(xg_string /*unused*/, const QXmlStreamAttributes* attrv)
{
  wpt = new Waypoint;

  if (attrv->hasAttribute("id")) {
    QString atstr = attrv->value("id").toString();
    wpt->description =  "osm-id " + atstr;
    if (waypoints.contains(atstr)) {
      warning(MYNAME ": Duplicate osm-id %s!\n", qPrintable(atstr));
    } else {
      waypoints.insert(atstr, wpt);
      wpt->wpt_flags.fmt_use = 1;
    }
  }

  // if (attrv->hasAttribute("user")) ; // ignored

  if (attrv->hasAttribute("lat")) {
    wpt->latitude = attrv->value("lat").toDouble();
  }
  if (attrv->hasAttribute("lon")) {
    wpt->longitude = attrv->value("lon").toDouble();
  }

  if (attrv->hasAttribute("timestamp")) {
    QString ts = attrv->value("timestamp").toString();
    wpt->creation_time = xml_parse_time(ts);
  }
}

void
OsmFormat::osm_node_tag(xg_string /*unused*/, const QXmlStreamAttributes* attrv)
{
  QString key, value;
  signed char ikey;

  if (attrv->hasAttribute("k")) {
    key = attrv->value("k").toString();
  }
  if (attrv->hasAttribute("v")) {
    value = attrv->value("v").toString();
  }

  QString str = osm_strip_html(value);

  if (key == QLatin1String("name")) {
    if (wpt->shortname.isEmpty()) {
      wpt->shortname = str;
    }
  } else if (key == QLatin1String("name:en")) {
    wpt->shortname = str;
  } else if ((ikey = osm_feature_ikey(key)) >= 0) {
    wpt->icon_descr = osm_feature_symbol(ikey, CSTR(value));
  } else if (key == QLatin1String("note")) {
    if (wpt->notes.isEmpty()) {
      wpt->notes = str;
    } else {
      wpt->notes += "; ";
      wpt->notes += str;
    }
  } else if (key == QLatin1String("gps:hdop")) {
    wpt->hdop = str.toDouble();
  } else if (key == QLatin1String("gps:vdop")) {
    wpt->vdop = str.toDouble();
  } else if (key == QLatin1String("gps:pdop")) {
    wpt->pdop = str.toDouble();
  } else if (key == QLatin1String("gps:sat")) {
    wpt->sat = str.toDouble();
  } else if (key == QLatin1String("gps:fix")) {
    if (str == QLatin1String("2d")) {
      wpt->fix = fix_2d;
    } else if (str == QLatin1String("3d")) {
      wpt->fix = fix_3d;
    } else if (str == QLatin1String("dgps")) {
      wpt->fix = fix_dgps;
    } else if (str == QLatin1String("pps")) {
      wpt->fix = fix_pps;
    } else if (str == QLatin1String("none")) {
      wpt->fix = fix_none;
    }
  }
}

void
OsmFormat::osm_way(xg_string /*unused*/, const QXmlStreamAttributes* attrv)
{
  rte = new route_head;
  // create a wpt to represent the route center if it has a center tag
  wpt = new Waypoint;
  if (attrv->hasAttribute("id")) {
    rte->rte_desc =  "osm-id " + attrv->value("id").toString();
  }
}

void
OsmFormat::osm_way_nd(xg_string /*unused*/, const QXmlStreamAttributes* attrv)
{
  if (attrv->hasAttribute("ref")) {
    QString atstr = attrv->value("ref").toString();

    if (waypoints.contains(atstr)) {
      const Waypoint* ctmp = waypoints.value(atstr);
      auto* tmp = new Waypoint(*ctmp);
      route_add_wpt(rte, tmp);
    } else {
      warning(MYNAME ": Way reference id \"%s\" wasn't listed under nodes!\n", qPrintable(atstr));
    }
  }
}

void
OsmFormat::osm_way_tag(xg_string /*unused*/, const QXmlStreamAttributes* attrv)
{
  QString key, value;
  signed char ikey;

  if (attrv->hasAttribute("k")) {
    key = attrv->value("k").toString();
  }
  if (attrv->hasAttribute("v")) {
    value = attrv->value("v").toString();
  }

  QString str = osm_strip_html(value);

  if (key == QLatin1String("name")) {
    if (rte->rte_name.isEmpty()) {
      rte->rte_name = str;
      wpt->shortname = str;
    }
  } else if (key == QLatin1String("name:en")) {
    rte->rte_name = str;

    wpt->shortname = str;
    // The remaining cases only apply to the center node
  } else if ((ikey = osm_feature_ikey(key)) >= 0) {
    wpt->icon_descr = osm_feature_symbol(ikey, CSTR(value));
  } else if (key == "note") {
    if (wpt->notes.isEmpty()) {
      wpt->notes = str;
    } else {
      wpt->notes += "; ";
      wpt->notes += str;
    }
  }
}

void
OsmFormat::osm_way_center(xg_string /*unused*/, const QXmlStreamAttributes* attrv)
{
  wpt->wpt_flags.fmt_use = 1;

  if (attrv->hasAttribute("lat")) {
    wpt->latitude = attrv->value("lat").toDouble();
  }
  if (attrv->hasAttribute("lon")) {
    wpt->longitude = attrv->value("lon").toDouble();
  }
}

void
OsmFormat::osm_way_end(xg_string /*unused*/, const QXmlStreamAttributes* /*unused*/)
{
  if (rte) {
    route_add_head(rte);
    rte = nullptr;
  }

  if (wpt) {
    if (wpt->wpt_flags.fmt_use) {
      waypt_add(wpt);
    } else {
      delete wpt;
      wpt = nullptr;
    }
  }
}

void
OsmFormat::rd_init(const QString& fname)
{
  wpt = nullptr;
  rte = nullptr;

  waypoints.clear();
  if (keys.isEmpty()) {
    osm_features_init();
  }

  xml_reader = new XmlGenericReader;
  xml_reader->xml_init(fname, this, osm_map);
}

void
OsmFormat::read()
{
  xml_reader->xml_read();
}

void
OsmFormat::rd_deinit()
{
  delete xml_reader;
  xml_reader = nullptr;

  waypoints.clear();
}

/*******************************************************************************/
/*                                   WRITER                                    */
/*-----------------------------------------------------------------------------*/

void
OsmFormat::osm_init_icons()
{
  if (!icons.isEmpty()) {
    return;
  }

  for (int i = 0; osm_icon_mappings[i].value; ++i) {
    icons.insert(osm_icon_mappings[i].icon, &osm_icon_mappings[i]);
  }
}

void
OsmFormat::osm_write_tag(const QString& key, const QString& value) const
{
  if (!value.isEmpty()) {
    fout->writeEmptyElement(QStringLiteral("tag"));
    fout->writeAttribute(QStringLiteral("k"), key);
    fout->writeAttribute(QStringLiteral("v"), value);
  }
}

void
OsmFormat::osm_disp_feature(const Waypoint* waypoint) const
{
  if (icons.contains(waypoint->icon_descr)) {
    const osm_icon_mapping_t* map = icons.value(waypoint->icon_descr);
    osm_write_tag(osm_features[map->key], map->value);
  }
}

void
OsmFormat::osm_write_opt_tag(const QString& atag)
{
  const QStringList tags = atag.split(';');
  for (const auto& tag : tags) {
    auto idx = tag.indexOf(':');
    if (idx >= 0) {
      osm_write_tag(tag.left(idx), tag.mid(idx+1));
    }
  }
}

void
OsmFormat::osm_release_ids(const Waypoint* waypoint)
{
  if (waypoint && waypoint->extra_data) {
    delete static_cast<int*>(waypoint->extra_data);
    const_cast<Waypoint*>(waypoint)->extra_data = nullptr;
  }
}

QString
OsmFormat::osm_name_from_wpt(const Waypoint* waypoint)
{
  QString name = QString("%1\01%2\01%3")
                 .arg(waypoint->shortname)
                 .arg(waypoint->latitude, 0, 'f', 6)
                 .arg(waypoint->longitude, 0, 'f', 6);
  return name;
}

void
OsmFormat::osm_waypt_disp(const Waypoint* waypoint)
{
  QString name = osm_name_from_wpt(waypoint);

  if (waypoints.contains(name)) {
    return;
  }

  waypoints.insert(name, waypoint);

  auto* id = new int;
  *id = --node_id;
  const_cast<Waypoint*>(waypoint)->extra_data = id;

  fout->writeStartElement(QStringLiteral("node"));
  fout->writeAttribute(QStringLiteral("id"), QString::number(*id));
  fout->writeAttribute(QStringLiteral("visible"), QStringLiteral("true"));
  fout->writeAttribute(QStringLiteral("lat"), QString::number(waypoint->latitude, 'f', 7));
  fout->writeAttribute(QStringLiteral("lon"), QString::number(waypoint->longitude, 'f', 7));
  if (waypoint->creation_time.isValid()) {
    // osm readers don't uniformally support fractional seconds, and may only accept time zone designation Z.
    fout->writeAttribute(QStringLiteral("timestamp"), waypoint->creation_time.toUTC().toString(Qt::ISODate));
  }

  if (waypoint->hdop) {
    osm_write_tag(QStringLiteral("gps:hdop"), QString::number(waypoint->hdop, 'f'));
  }
  if (waypoint->vdop) {
    osm_write_tag(QStringLiteral("gps:vdop"), QString::number(waypoint->vdop, 'f'));
  }
  if (waypoint->pdop) {
    osm_write_tag(QStringLiteral("gps:pdop"), QString::number(waypoint->pdop, 'f'));
  }
  if (waypoint->sat > 0) {
    osm_write_tag(QStringLiteral("gps:sat"), QString::number(waypoint->sat));
  }

  switch (waypoint->fix) {
  case fix_2d:
    osm_write_tag(QStringLiteral("gps:fix"), QStringLiteral("2d"));
    break;
  case fix_3d:
    osm_write_tag(QStringLiteral("gps:fix"), QStringLiteral("3d"));
    break;
  case fix_dgps:
    osm_write_tag(QStringLiteral("gps:fix"), QStringLiteral("dgps"));
    break;
  case fix_pps:
    osm_write_tag(QStringLiteral("gps:fix"), QStringLiteral("pps"));
    break;
  case fix_none:
    osm_write_tag(QStringLiteral("gps:fix"), QStringLiteral("none"));
    break;
  case fix_unknown:
  default:
    break;
  }

  if (strlen(created_by) !=0) {
    QString value(created_by);
    if (!gpsbabel_testmode()) {
      if (strcmp("GPSBabel",created_by)==0) {
        value += '-';
        value += gpsbabel_version;
      }
    }
    osm_write_tag(QStringLiteral("created_by"), value);
  }

  osm_write_tag("name", waypoint->shortname);
  osm_write_tag("note", waypoint->notes.isEmpty() ? waypoint->description : waypoint->notes);
  if (!waypoint->icon_descr.isNull()) {
    osm_disp_feature(waypoint);
  }

  osm_write_opt_tag(opt_tagnd);

  fout->writeEndElement(); // node
}

void
OsmFormat::osm_rte_disp_head(const route_head* route)
{
  skip_rte = route->rte_waypt_empty();

  if (skip_rte) {
    return;
  }

  fout->writeStartElement(QStringLiteral("way"));
  fout->writeAttribute(QStringLiteral("id"), QString::number(--node_id));
  fout->writeAttribute(QStringLiteral("visible"), QStringLiteral("true"));
}

void
OsmFormat::osm_rtept_disp(const Waypoint* wpt_ref) const
{
  QString name = osm_name_from_wpt(wpt_ref);

  if (skip_rte) {
    return;
  }

  if (waypoints.contains(name)) {
    const Waypoint* waypoint = waypoints.value(name);
    auto* id = static_cast<int*>(waypoint->extra_data);
    fout->writeEmptyElement(QStringLiteral("nd"));
    fout->writeAttribute(QStringLiteral("ref"), QString::number(*id));
  }
}

void
OsmFormat::osm_rte_disp_trail(const route_head* route)
{
  if (skip_rte) {
    return;
  }

  if (strlen(created_by) !=0) {
    QString value(created_by);
    if (!gpsbabel_testmode()) {
      if (strcmp("GPSBabel",created_by)==0) {
        value += '-';
        value += gpsbabel_version;
      }
    }
    osm_write_tag(QStringLiteral("created_by"), value);
  }

  osm_write_tag("name", route->rte_name);
  osm_write_tag("note", route->rte_desc);

  if (opt_tag && (case_ignore_strncmp(opt_tag, "tagnd", 5) != 0)) {
    osm_write_opt_tag(opt_tag);
  }

  fout->writeEndElement(); // way
}

/*-----------------------------------------------------------------------------*/

void
OsmFormat::wr_init(const QString& fname)
{
  ofile = new gpsbabel::File(fname);
  ofile->open(QIODevice::WriteOnly | QIODevice::Text);

  fout = new gpsbabel::XmlStreamWriter(ofile);
  fout->setAutoFormatting(true);
  fout->setAutoFormattingIndent(2);

  osm_init_icons();
  waypoints.clear();
  node_id = 0;
}

void
OsmFormat::write()
{
  fout->writeStartDocument();
  fout->writeStartElement(QStringLiteral("osm"));
  fout->writeAttribute(QStringLiteral("version"), QStringLiteral("0.6"));
  QString value(QStringLiteral("GPSBabel"));
  if (!gpsbabel_testmode()) {
    value += '-';
    value += gpsbabel_version;
  }
  fout->writeAttribute(QStringLiteral("generator"), value);

  auto osm_waypt_disp_lambda = [this](const Waypoint* waypointp)->void {
    osm_waypt_disp(waypointp);
  };
  waypt_disp_all(osm_waypt_disp_lambda);
  route_disp_all(nullptr, nullptr, osm_waypt_disp_lambda);
  track_disp_all(nullptr, nullptr, osm_waypt_disp_lambda);

  auto osm_rte_disp_head_lambda = [this](const route_head* rte)->void {
    osm_rte_disp_head(rte);
  };
  auto osm_rte_disp_trail_lambda = [this](const route_head* rte)->void {
    osm_rte_disp_trail(rte);
  };
  auto osm_rtept_disp_lambda = [this](const Waypoint* waypointp)->void {
    osm_rtept_disp(waypointp);
  };
  route_disp_all(osm_rte_disp_head_lambda, osm_rte_disp_trail_lambda, osm_rtept_disp_lambda);
  track_disp_all(osm_rte_disp_head_lambda, osm_rte_disp_trail_lambda, osm_rtept_disp_lambda);

  fout->writeEndElement(); // osm
}

void
OsmFormat::wr_deinit()
{
  fout->writeEndDocument();
  delete fout;
  fout = nullptr;
  ofile->close();
  delete ofile;
  ofile = nullptr;

  waypt_disp_all(osm_release_ids);
  route_disp_all(nullptr, nullptr, osm_release_ids);
  track_disp_all(nullptr, nullptr, osm_release_ids);

  waypoints.clear();
}

void
OsmFormat::exit()
{
  keys.clear();
  values.clear();
  icons.clear();
}

/*-----------------------------------------------------------------------------*/
