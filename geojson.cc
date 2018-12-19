/*
    Copyright (C) 2016 Robert Lipe, robertlipe+source@gpsbabel.org

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
#include "src/core/file.h"
#include <QtCore/QJsonArray>
#include <QtCore/QJsonDocument>
#include <QtCore/QJsonObject>

static gbfile* ofd;
static QString input_file_name;
static const char MYNAME[] = "geojson";
static char* compact_opt = nullptr;
static QJsonObject* track_object = nullptr;
static QJsonArray* track_coords = nullptr;

static const QString FEATURE_COLLECTION = QStringLiteral("FeatureCollection");
static const QString FEATURE = QStringLiteral("Feature");
static const QString POINT = QStringLiteral("Point");
static const QString MULTIPOINT = QStringLiteral("MultiPoint");
static const QString LINESTRING = QStringLiteral("LineString");
static const QString MULTILINESTRING = QStringLiteral("MultiLineString");
static const QString POLYGON = QStringLiteral("Polygon");
static const QString MULTIPOLYGON = QStringLiteral("MultiPolygon");
static const QString TYPE = QStringLiteral("type");
static const QString FEATURES = QStringLiteral("features");
static const QString COORDINATES = QStringLiteral("coordinates");
static const QString GEOMETRY = QStringLiteral("geometry");
static const QString PROPERTIES = QStringLiteral("properties");
static const QString NAME = QStringLiteral("name");
static const QString DESCRIPTION = QStringLiteral("description");
static const QString URL = QStringLiteral("url");
static const QString URLNAME = QStringLiteral("urlname");

static arglist_t geojson_args[] = {
  {"compact", &compact_opt, "Compact Output. Default is off.", 
    nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr } ,
  ARG_TERMINATOR
};

static void
geojson_rd_init(const QString& fname) {
	input_file_name = fname;
}

static QJsonArray* feature_collection = nullptr;

static void
geojson_wr_init(const QString& fname) {
  feature_collection = new QJsonArray;
  ofd = gbfopen(fname, "w", MYNAME);
}

static void
geojson_waypt_pr(const Waypoint* waypoint) {
  QJsonObject geometry;
  geometry[TYPE] = POINT;
  QJsonArray coordinates;
  coordinates.append(waypoint->longitude);
  coordinates.append(waypoint->latitude);
  if (waypoint->altitude != unknown_alt && waypoint->altitude != 0) {
    coordinates.append(waypoint->altitude);
  }
  geometry[COORDINATES] = coordinates;

  QJsonObject feature;
  feature[TYPE] = FEATURE;
  feature[GEOMETRY] = geometry;

  // Build up the properties.
  QJsonObject properties;
  if (!waypoint->shortname.isEmpty()) {
    properties[NAME] = waypoint->shortname;
  }
  if (!waypoint->description.isEmpty()) {
    properties[DESCRIPTION] = waypoint->description;
  }
  if (waypoint->HasUrlLink()) {
    UrlLink link = waypoint->GetUrlLink();
    if (!link.url_.isEmpty()) {
      properties[URL] = link.url_;
    }
    if (!link.url_link_text_.isEmpty()) {
      properties[URLNAME] = link.url_link_text_;
    }
  }
  if (!properties.empty()) {
    feature[PROPERTIES] = properties;
  }
  
  feature_collection->append(feature);
}

static void
geojson_rd_deinit() {
	gbfclose(ofd);
	ofd = nullptr;
}

static void
geojson_wr_deinit() {
  QJsonObject object;
  object[TYPE] = FEATURE_COLLECTION;
  object[FEATURES]  = *feature_collection;

  QJsonDocument save(object);
  QJsonDocument::JsonFormat style = compact_opt ? QJsonDocument::Compact : QJsonDocument::Indented;
  gbfputs(save.toJson(style),ofd);

  gbfclose(ofd);
  ofd = nullptr;
  delete feature_collection;
  feature_collection = nullptr;
}

static Waypoint* 
waypoint_from_coordinates(const QJsonArray& coordinates)
{
	auto waypoint = new Waypoint();
	waypoint->latitude = coordinates.at(1).toDouble();
	waypoint->longitude = coordinates.at(0).toDouble();
	if (coordinates.size() > 2)
	{
		waypoint->altitude = coordinates.at(3).toDouble();
	}
	return waypoint;
}

static void 
routes_from_polygon_coordinates(const QJsonArray& polygon)
{
	for (auto && lineStringIterator : polygon)
	{
		QJsonArray coordinates = (lineStringIterator).toArray();
		auto route = route_head_alloc();
		route_add_head(route);
		for (auto && coordinate : coordinates)
		{
			auto waypoint = waypoint_from_coordinates(coordinate.toArray());
			route_add_wpt(route, waypoint);
		}
	}
}

static void
geojson_read() {
	QFile file;
	file.setFileName(input_file_name);
	file.open(QIODevice::ReadOnly | QIODevice::Text);
	QString file_content = file.readAll();
	file.close();
	QJsonParseError error{};
	QJsonDocument document = QJsonDocument::fromJson(file_content.toUtf8(), &error);
	QJsonObject rootObject = document.object();

	if (rootObject[TYPE] != FEATURE_COLLECTION)
	{
		return;
	}
	QJsonArray features = rootObject.value(FEATURES).toArray();
	for (auto && iterator : features)
	{
		QJsonObject feature = iterator.toObject();
		QJsonObject properties = (feature.value(PROPERTIES)).toObject();
		QString name;
		QString description;
		if (!properties.empty())
		{
			if (properties.contains(NAME))
			{
				name = properties[NAME].toString();
			}
			if (properties.contains(DESCRIPTION))
			{
				description = properties[DESCRIPTION].toString();
			}
		}
		
		QJsonObject geometry = feature.value(GEOMETRY).toObject();
		auto geometry_type = geometry[TYPE];
		if (geometry_type == POINT)
		{
			QJsonArray coordinates = geometry.value(COORDINATES).toArray();
			auto waypoint = waypoint_from_coordinates(coordinates);
			waypoint->shortname = name;
			waypoint->description = description;
			if (properties.contains(URL))
			{
				QString url = properties[URL].toString();
				if (properties.contains(URLNAME))
				{
					QString url_text = properties[URLNAME].toString();
					waypoint->AddUrlLink(UrlLink(url, url_text));
				}
				else
				{
					waypoint->AddUrlLink(UrlLink(url));
				}
			}
			waypt_add(waypoint);
		}
		else if (geometry_type == MULTIPOINT)
		{
			QJsonArray coordinates = geometry.value(COORDINATES).toArray();
			for (auto && coordinate : coordinates)
			{
				auto waypoint = waypoint_from_coordinates(coordinate.toArray());
				waypt_add(waypoint);
			}
		}
		else if (geometry_type == LINESTRING)
		{
			QJsonArray coordinates = geometry.value(COORDINATES).toArray();
			auto route = route_head_alloc();
			route->rte_name = name;
			route_add_head(route);
			for (auto && coordinate : coordinates)
			{
				auto waypoint = waypoint_from_coordinates(coordinate.toArray());
				route_add_wpt(route, waypoint);
			}
		}
		else if (geometry_type == POLYGON)
		{
			QJsonArray polygon = geometry.value(COORDINATES).toArray();
			routes_from_polygon_coordinates(polygon);
		}
		else if (geometry_type == MULTIPOLYGON)
		{
			QJsonArray polygons = geometry.value(COORDINATES).toArray();
			for (auto && polygons_iterator : polygons)
			{
				QJsonArray polygon = polygons_iterator.toArray();
				routes_from_polygon_coordinates(polygon);
			}
		}
		else if (geometry_type == MULTILINESTRING)
		{
			QJsonArray line_strings = geometry.value(COORDINATES).toArray();
			for (auto && line_string : line_strings)
			{
				QJsonArray coordinates = line_string.toArray();
				auto route = route_head_alloc();
				track_add_head(route);
				for (auto && coordinate : coordinates)
				{
					auto waypoint = waypoint_from_coordinates(coordinate.toArray());
					route_add_wpt(route, waypoint);
				}
			}
		}
	}
}


static void geojson_track_hdr(const route_head* track) {
  track_object = new QJsonObject();

  (*track_object)[TYPE] = FEATURE;
  track_coords = new QJsonArray();

  QJsonObject properties;
  if (!track->rte_name.isEmpty()) {
    properties[NAME] = track->rte_name;
  }
  (*track_object)[PROPERTIES] = properties;
}

static void geojson_track_disp(const Waypoint* trackpoint) {

  QJsonArray coords;
  coords.append(trackpoint->longitude);
  coords.append(trackpoint->latitude);
  if (trackpoint->altitude != unknown_alt && trackpoint->altitude != 0) {
    coords.append(trackpoint->altitude);
  }
  (*track_coords).append(coords);
}

static void geojson_track_tlr(const route_head*) {
  QJsonObject geometry;
  geometry[TYPE] = LINESTRING;
  geometry[COORDINATES] = *track_coords;
  (*track_object)[GEOMETRY] = geometry;
  feature_collection->append(*track_object);
  delete track_object;
  track_object = nullptr;
  delete track_coords;
  track_coords = nullptr;
}

static void
geojson_write() {
  waypt_disp_all(geojson_waypt_pr);
  track_disp_all(geojson_track_hdr, geojson_track_tlr, geojson_track_disp);
}

ff_vecs_t geojson_vecs = {
  ff_type_file,
  { 
  	(ff_cap)(ff_cap_read | ff_cap_write) /* waypoints */,
	(ff_cap)(ff_cap_read | ff_cap_write) /* tracks */,
	(ff_cap)(ff_cap_read | ff_cap_write) /* routes */, 
  },
  geojson_rd_init,
  geojson_wr_init,
  geojson_rd_deinit,
  geojson_wr_deinit,
  geojson_read,
  geojson_write,
  nullptr,
  geojson_args,
  CET_CHARSET_UTF8, 0	/* CET-REVIEW */
  , NULL_POS_OPS,
  nullptr  
};
