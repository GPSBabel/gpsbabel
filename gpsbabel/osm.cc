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
	Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111 USA

*/

#include <QtCore/QXmlStreamAttributes>

#include "defs.h"
#include "xmlgeneric.h"

static char* opt_tag, *opt_tagnd, *created_by;

static arglist_t osm_args[] = {
  { "tag", &opt_tag, 	"Write additional way tag key/value pairs", NULL, ARGTYPE_STRING, ARG_NOMINMAX },
  { "tagnd", &opt_tagnd,	"Write additional node tag key/value pairs", NULL, ARGTYPE_STRING, ARG_NOMINMAX },
  { "created_by", &created_by, "Use this value as custom created_by value","GPSBabel", ARGTYPE_STRING, ARG_NOMINMAX },
  ARG_TERMINATOR
};

#define MYNAME "osm"

static QHash<QString, const Waypoint*> waypoints;

static QHash<QString, int> keys;
static QHash<QString, const struct osm_icon_mapping_s*> values;
static QHash<QString, const struct osm_icon_mapping_s*> icons;

static gbfile* fout;
static int node_id;
static int skip_rte;

static route_head* rte;
static Waypoint* wpt;

static xg_callback	osm_node, osm_node_tag, osm_node_end;
static xg_callback	osm_way, osm_way_nd, osm_way_tag, osm_way_end;

static
xg_tag_mapping osm_map[] = {
  { osm_node,	cb_start,	"/osm/node" },
  { osm_node_tag,	cb_start,	"/osm/node/tag" },
  { osm_node_end,	cb_end,		"/osm/node" },
  { osm_way,	cb_start,	"/osm/way" },
  { osm_way_nd,	cb_start,	"/osm/way/nd" },
  { osm_way_tag,	cb_start,	"/osm/way/tag" },
  { osm_way_end,	cb_end,		"/osm/way" },
  { NULL,	(xg_cb_type)0,		NULL }
};

static const char* osm_features[] = {
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
  NULL
};

typedef struct osm_icon_mapping_s {
  const int key;
  const char* value;
  const char* icon;
} osm_icon_mapping_t;


/* based on <http://wiki.openstreetmap.org/index.php/Map_Features> */

static osm_icon_mapping_t osm_icon_mappings[] = {

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

  { -1, NULL, NULL }
};


/*******************************************************************************/
/*                                   READER                                    */
/*-----------------------------------------------------------------------------*/

static void
osm_features_init(void)
{
  int i;

  /* the first of osm_features is a place holder */
  for (i = 1; osm_features[i]; i++) {
    keys.insert(QString::fromUtf8(osm_features[i]), i);
  }

  for (i = 0; osm_icon_mappings[i].value; i++) {
    char buff[128];

    buff[0] = osm_icon_mappings[i].key;
    strncpy(&buff[1], osm_icon_mappings[i].value, sizeof(buff) - 1);

    values.insert(QString::fromUtf8(buff), &osm_icon_mappings[i]);
  }
}


static char
osm_feature_ikey(const QString& key)
{
  return keys.value(key, -1);
}


static QString
osm_feature_symbol(const int ikey, const char* value)
{
  char buff[128];
  QString key;

  buff[0] = ikey;
  strncpy(&buff[1], value, sizeof(buff) - 1);

  key = QString::fromUtf8(buff);

  QString result;
  if (values.contains(key)) {
    result = values.value(key)->icon;
  } else {
    result = QString("%1:%2").arg(osm_features[ikey]).arg(value);
  }
  return result;
}


static char*
osm_strip_html(const char* str)
{
  utf_string utf;
  utf.is_html = 1;
  utf.utfstring = (char*)str;

  return strip_html(&utf);	// util.cc
}

static QString
osm_strip_html(const QString& str)
{
  char* r = osm_strip_html(CSTR(str));
  QString rv(r);
  xfree(r);
  return rv;
}


static void
osm_node_end(xg_string args, const QXmlStreamAttributes*)
{
  if (wpt) {
    if (wpt->wpt_flags.fmt_use) {
      waypt_add(wpt);
    } else {
      delete wpt;
    }
    wpt = NULL;
  }
}


static void
osm_node(xg_string args, const QXmlStreamAttributes* attrv)
{
  wpt = new Waypoint;

  if (attrv->hasAttribute("id")) {
    QString atstr = attrv->value("id").toString();
    wpt->description =  "osm-id " + atstr;
    if (waypoints.contains(atstr)) {
      warning(MYNAME ": Duplicate osm-id %s!\n", CSTR(atstr));
    } else {
      waypoints.insert(atstr, wpt);
      wpt->wpt_flags.fmt_use = 1;
    }
  }

  // if (attrv->hasAttribute("user")) ; // ignored

  if (attrv->hasAttribute("lat")) {
    wpt->latitude = attrv->value("lat").toString().toDouble();
  }
  if (attrv->hasAttribute("lon")) {
    wpt->longitude = attrv->value("lon").toString().toDouble();
  }

  if (attrv->hasAttribute("timestamp")) {
    QString ts = attrv->value("timestamp").toString();
    wpt->creation_time = xml_parse_time(ts);
  }
}


static void
osm_node_tag(xg_string args, const QXmlStreamAttributes* attrv)
{
  QString key, value;
  QString str;
  signed char ikey;

  if (attrv->hasAttribute("k")) {
    key = attrv->value("k").toString();
  }
  if (attrv->hasAttribute("v")) {
    value = attrv->value("v").toString();
  }

  str = osm_strip_html(value);

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


static void
osm_way(xg_string args, const QXmlStreamAttributes* attrv)
{
  rte = route_head_alloc();
  if (attrv->hasAttribute("id")) {
    rte->rte_desc =  "osm-id " + attrv->value("id").toString();
  }
}

static void
osm_way_nd(xg_string args, const QXmlStreamAttributes* attrv)
{
  if (attrv->hasAttribute("ref")) {
    QString atstr = attrv->value("ref").toString();
    Waypoint* tmp;
    const Waypoint* ctmp;

    if (waypoints.contains(atstr)) {
      ctmp = waypoints.value(atstr);
      tmp = new Waypoint(*ctmp);
      route_add_wpt(rte, tmp);
    } else {
      warning(MYNAME ": Way reference id \"%s\" wasn't listed under nodes!\n", CSTR(atstr));
    }
  }
}

static void
osm_way_tag(xg_string args, const QXmlStreamAttributes* attrv)
{
  QString key, value;
  QString str;

  if (attrv->hasAttribute("k")) {
    key = attrv->value("k").toString();
  }
  if (attrv->hasAttribute("v")) {
    value = attrv->value("v").toString();
  }

  str = osm_strip_html(value);

  if (key == QLatin1String("name")) {
    if (rte->rte_name.isEmpty()) {
      rte->rte_name = str;
    }
  } else if (key == QLatin1String("name:en")) {
    rte->rte_name = str;
  }
}

static void
osm_way_end(xg_string args, const QXmlStreamAttributes*)
{
  if (rte) {
    route_add_head(rte);
    rte = NULL;
  }
}

static void
osm_rd_init(const char* fname)
{
  wpt = NULL;
  rte = NULL;

  waypoints.clear();
  if (keys.isEmpty()) {
    osm_features_init();
  }

  xml_init(fname, osm_map, NULL);
}

static void
osm_read(void)
{
  xml_read();
}

static void
osm_rd_deinit(void)
{
  xml_deinit();
  waypoints.clear();
}

/*******************************************************************************/
/*                                   WRITER                                    */
/*-----------------------------------------------------------------------------*/

static void
osm_init_icons(void)
{
  int i;

  if (!icons.isEmpty()) {
    return;
  }

  for (i = 0; osm_icon_mappings[i].value; i++) {
    icons.insert(QString::fromUtf8(osm_icon_mappings[i].icon),
                 &osm_icon_mappings[i]);
  }
}

static void
osm_write_tag(const QString& key, const QString& value)
{
  if (!value.isEmpty()) {
    char* str = xml_entitize(CSTR(value));
    gbfprintf(fout, "    <tag k='%s' v='%s'/>\n", CSTR(key), str);
    xfree(str);
  }
}

static void
osm_disp_feature(const Waypoint* wpt)
{
  const osm_icon_mapping_t* map;

  if (icons.contains(wpt->icon_descr)) {
    map = icons.value(wpt->icon_descr);
    osm_write_tag(osm_features[map->key], map->value);
  }
}

static void
osm_write_opt_tag(const char* atag)
{
  char* tag, *cin, *ce;

  if (!atag) {
    return;
  }

  tag = cin = xstrdup(atag);
  ce = cin + strlen(cin);

  while (cin < ce) {
    char* sc, *dp;

    if ((sc = strchr(cin, ';'))) {
      *sc = '\0';
    }

    if ((dp = strchr(cin, ':'))) {
      *dp++ = '\0';
      osm_write_tag(cin, dp);
    }
    cin += strlen(cin) + 1;
  }

  xfree(tag);
}

static void
osm_release_ids(const Waypoint* wpt)
{
  if (wpt && wpt->extra_data) {
    Waypoint* tmp = (Waypoint*)wpt;
    xfree(tmp->extra_data);
    tmp->extra_data = NULL;
  }
}

static QString
osm_name_from_wpt(const Waypoint* wpt)
{
  QString name = QString("%1\01%2\01%3")
                 .arg(wpt->shortname)
                 .arg(wpt->latitude)
                 .arg(wpt->longitude);
  return name;
}

static void
osm_waypt_disp(const Waypoint* wpt)
{
  QString name = osm_name_from_wpt(wpt);

  if (waypoints.contains(name)) {
    return;
  }

  waypoints.insert(name, wpt);

  int* id;

  id = (int*) xmalloc(sizeof(*id));
  *id = --node_id;
  ((Waypoint*)(wpt))->extra_data = id;

  gbfprintf(fout, "  <node id='%d' visible='true' lat='%0.7f' lon='%0.7f'", *id, wpt->latitude, wpt->longitude);
  if (wpt->creation_time.isValid()) {
    QString time_string = wpt->CreationTimeXML();
    gbfprintf(fout, " timestamp='%s'", qPrintable(time_string));
  }
  gbfprintf(fout, ">\n");

  if (wpt->hdop) {
    gbfprintf(fout, "    <tag k='gps:hdop' v='%f' />\n", wpt->hdop);
  }
  if (wpt->vdop) {
    gbfprintf(fout, "    <tag k='gps:vdop' v='%f' />\n", wpt->vdop);
  }
  if (wpt->pdop) {
    gbfprintf(fout, "    <tag k='gps:pdop' v='%f' />\n", wpt->pdop);
  }
  if (wpt->sat > 0) {
    gbfprintf(fout, "    <tag k='gps:sat' v='%d' />\n", wpt->sat);
  }

  switch (wpt->fix) {
  case fix_2d:
    gbfprintf(fout, "    <tag k='gps:fix' v='2d' />\n");
    break;
  case fix_3d:
    gbfprintf(fout, "    <tag k='gps:fix' v='3d' />\n");
    break;
  case fix_dgps:
    gbfprintf(fout, "    <tag k='gps:fix' v='dgps' />\n");
    break;
  case fix_pps:
    gbfprintf(fout, "    <tag k='gps:fix' v='pps' />\n");
    break;
  case fix_none:
    gbfprintf(fout, "    <tag k='gps:fix' v='none' />\n");
    break;
  case fix_unknown:
  default:
    break;
  }

  if (strlen(created_by) !=0) {
    gbfprintf(fout, "    <tag k='created_by' v='%s",created_by);
    if (gpsbabel_time != 0)
      if (strcmp("GPSBabel",created_by)==0) {
        gbfprintf(fout, "-%s", gpsbabel_version);
      }
    gbfprintf(fout, "'/>\n");
  }

  osm_write_tag("name", wpt->shortname);
  osm_write_tag("note", (wpt->notes.isEmpty()) ? wpt->description : wpt->notes);
  if (!wpt->icon_descr.isNull()) {
    osm_disp_feature(wpt);
  }

  osm_write_opt_tag(opt_tagnd);

  gbfprintf(fout, "  </node>\n");
}

static void
osm_rte_disp_head(const route_head* rte)
{
  skip_rte = (rte->rte_waypt_ct <= 0);

  if (skip_rte) {
    return;
  }

  gbfprintf(fout, "  <way id='%d' visible='true'>\n", --node_id);
}

static void
osm_rtept_disp(const Waypoint* wpt_ref)
{
  QString name = osm_name_from_wpt(wpt_ref);
  const Waypoint* wpt;

  if (skip_rte) {
    return;
  }

  if (waypoints.contains(name)) {
    wpt = waypoints.value(name);
    int* id = (int*) wpt->extra_data;
    gbfprintf(fout, "    <nd ref='%d'/>\n", *id);
  }
}

static void
osm_rte_disp_trail(const route_head* rte)
{
  if (skip_rte) {
    return;
  }

  if (strlen(created_by) !=0) {
    gbfprintf(fout, "    <tag k='created_by' v='%s",created_by);
    if (gpsbabel_time != 0)
      if (strcmp("GPSBabel",created_by)==0) {
        gbfprintf(fout, "-%s", gpsbabel_version);
      }
    gbfprintf(fout, "'/>\n");
  }

  osm_write_tag("name", rte->rte_name);
  osm_write_tag("note", rte->rte_desc);

  if (opt_tag && (case_ignore_strncmp(opt_tag, "tagnd", 5) != 0)) {
    osm_write_opt_tag(opt_tag);
  }

  gbfprintf(fout, "  </way>\n");
}

/*-----------------------------------------------------------------------------*/

static void
osm_wr_init(const char* fname)
{
  fout = gbfopen(fname, "w", MYNAME);

  osm_init_icons();
  waypoints.clear();
  node_id = 0;
}

static void
osm_write(void)
{
  gbfprintf(fout, "<?xml version='1.0' encoding='UTF-8'?>\n");
  gbfprintf(fout, "<osm version='0.5' generator='GPSBabel");
  if (gpsbabel_time != 0) {
    gbfprintf(fout, "-%s", gpsbabel_version);
  }
  gbfprintf(fout, "'>\n");

  waypt_disp_all(osm_waypt_disp);
  route_disp_all(NULL, NULL, osm_waypt_disp);
  track_disp_all(NULL, NULL, osm_waypt_disp);

  route_disp_all(osm_rte_disp_head, osm_rte_disp_trail, osm_rtept_disp);
  track_disp_all(osm_rte_disp_head, osm_rte_disp_trail, osm_rtept_disp);

  gbfprintf(fout, "</osm>\n");
}

static void
osm_wr_deinit(void)
{
  gbfclose(fout);

  waypt_disp_all(osm_release_ids);
  route_disp_all(NULL, NULL, osm_release_ids);
  track_disp_all(NULL, NULL, osm_release_ids);

  waypoints.clear();
}

static void
osm_exit(void)
{
  keys.clear();
  values.clear();
  icons.clear();
}

/*-----------------------------------------------------------------------------*/

ff_vecs_t osm_vecs = {
  ff_type_file,
  {
    (ff_cap)(ff_cap_read | ff_cap_write)	/* waypoints */,
    ff_cap_write 			/* tracks */,
    (ff_cap)(ff_cap_read | ff_cap_write) 	/* routes */,
  },
  osm_rd_init,
  osm_wr_init,
  osm_rd_deinit,
  osm_wr_deinit,
  osm_read,
  osm_write,
  osm_exit,
  osm_args,
  CET_CHARSET_UTF8, 0
};
