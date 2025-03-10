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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

 */

#include <QByteArray>              // for QByteArray
#include <QIODevice>               // for operator|, QIODevice, QIODevice::ReadOnly, QIODevice::Text
#include <QJsonArray>              // for QJsonArray
#include <QJsonDocument>           // for QJsonDocument, QJsonDocument::Compact, QJsonDocument::Indented, QJsonDocument::JsonFormat
#include <QJsonObject>             // for QJsonObject
#include <QJsonParseError>         // for QJsonParseError
#include <QJsonValue>              // for QJsonValue
#include <QJsonValueRef>           // for QJsonValueRef

#include "defs.h"
#include "geojson.h"
#include "src/core/file.h"         // for File
#include "src/core/logging.h"      // for Fatal


void
GeoJsonFormat::rd_init(const QString& fname)
{
  ifd = new gpsbabel::File(fname);
  ifd->open(QIODevice::ReadOnly | QIODevice::Text);
}

void
GeoJsonFormat::wr_init(const QString& fname)
{
  feature_collection = new QJsonArray;
  ofd = new gpsbabel::File(fname);
  ofd->open(QIODevice::WriteOnly);
}

void
GeoJsonFormat::geojson_waypt_pr(const Waypoint* waypoint) const
{
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
    properties[name_opt] = waypoint->shortname;
  }
  if (!waypoint->description.isEmpty()) {
    properties[desc_opt] = waypoint->description;
  }
  if (waypoint->HasUrlLink()) {
    const UrlLink& link = waypoint->GetUrlLink();
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

void
GeoJsonFormat::rd_deinit()
{
  ifd->close();
  delete ifd;
  ifd = nullptr;
}

void
GeoJsonFormat::wr_deinit()
{
  QJsonObject object;
  object[TYPE] = FEATURE_COLLECTION;
  object[FEATURES]  = *feature_collection;

  QJsonDocument save(object);
  QJsonDocument::JsonFormat style = compact_opt ? QJsonDocument::Compact : QJsonDocument::Indented;
  ofd->write(save.toJson(style));
  ofd->close();

  delete ofd;
  ofd = nullptr;
  delete feature_collection;
  feature_collection = nullptr;
}

Waypoint*
GeoJsonFormat::waypoint_from_coordinates(const QJsonArray& coordinates)
{
  auto* waypoint = new Waypoint();
  waypoint->latitude = coordinates.at(1).toDouble();
  waypoint->longitude = coordinates.at(0).toDouble();
  if (coordinates.size() > 2) {
    waypoint->altitude = coordinates.at(3).toDouble();
  }
  return waypoint;
}

void
GeoJsonFormat::routes_from_polygon_coordinates(const QJsonArray& polygon)
{
  for (auto&& lineStringIterator : polygon) {
    QJsonArray coordinates = (lineStringIterator).toArray();
    auto* route = new route_head;
    route_add_head(route);
    for (auto&& coordinate : coordinates) {
      auto* waypoint = waypoint_from_coordinates(coordinate.toArray());
      route_add_wpt(route, waypoint);
    }
  }
}

void
GeoJsonFormat::read()
{
  QString file_content = ifd->readAll();
  QJsonParseError error{};
  QJsonDocument document = QJsonDocument::fromJson(file_content.toUtf8(), &error);
  if (error.error != QJsonParseError::NoError) {
    gbFatal(FatalMsg().nospace() << "GeoJSON parse error in " << ifd->fileName() << ": " << error.errorString());
  }
  QJsonObject rootObject = document.object();

  if (rootObject[TYPE] != FEATURE_COLLECTION) {
    return;
  }
  QJsonArray features = rootObject.value(FEATURES).toArray();
  for (auto&& iterator : features) {
    QJsonObject feature = iterator.toObject();
    QJsonObject properties = (feature.value(PROPERTIES)).toObject();
    QString name;
    QString description;
    if (!properties.empty()) {
      if (properties.contains(name_opt)) {
        name = properties[name_opt].toString();
      }
      if (properties.contains(desc_opt)) {
        description = properties[desc_opt].toString();
      }
    }

    QJsonObject geometry = feature.value(GEOMETRY).toObject();
    auto geometry_type = geometry[TYPE];
    if (geometry_type == POINT) {
      QJsonArray coordinates = geometry.value(COORDINATES).toArray();
      auto* waypoint = waypoint_from_coordinates(coordinates);
      waypoint->shortname = name;
      waypoint->description = description;
      if (properties.contains(URL)) {
        QString url = properties[URL].toString();
        if (properties.contains(URLNAME)) {
          QString url_text = properties[URLNAME].toString();
          waypoint->AddUrlLink(UrlLink(url, url_text));
        } else {
          waypoint->AddUrlLink(UrlLink(url));
        }
      }
      waypt_add(waypoint);
    } else if (geometry_type == MULTIPOINT) {
      QJsonArray coordinates = geometry.value(COORDINATES).toArray();
      for (auto&& coordinate : coordinates) {
        auto* waypoint = waypoint_from_coordinates(coordinate.toArray());
        waypt_add(waypoint);
      }
    } else if (geometry_type == LINESTRING) {
      QJsonArray coordinates = geometry.value(COORDINATES).toArray();
      auto* route = new route_head;
      route->rte_name = name;
      route_add_head(route);
      for (auto&& coordinate : coordinates) {
        auto* waypoint = waypoint_from_coordinates(coordinate.toArray());
        route_add_wpt(route, waypoint);
      }
    } else if (geometry_type == POLYGON) {
      QJsonArray polygon = geometry.value(COORDINATES).toArray();
      routes_from_polygon_coordinates(polygon);
    } else if (geometry_type == MULTIPOLYGON) {
      QJsonArray polygons = geometry.value(COORDINATES).toArray();
      for (auto&& polygons_iterator : polygons) {
        QJsonArray polygon = polygons_iterator.toArray();
        routes_from_polygon_coordinates(polygon);
      }
    } else if (geometry_type == MULTILINESTRING) {
      QJsonArray line_strings = geometry.value(COORDINATES).toArray();
      for (auto&& line_string : line_strings) {
        QJsonArray coordinates = line_string.toArray();
        auto* route = new route_head;
        route->rte_name = name;
        track_add_head(route);
        for (auto&& coordinate : coordinates) {
          auto* waypoint = waypoint_from_coordinates(coordinate.toArray());
          route_add_wpt(route, waypoint);
        }
      }
    }
  }
}


void GeoJsonFormat::geojson_track_hdr(const route_head* track)
{
  track_object = new QJsonObject();

  (*track_object)[TYPE] = FEATURE;
  track_coords = new QJsonArray();

  QJsonObject properties;
  if (!track->rte_name.isEmpty()) {
    properties[name_opt] = track->rte_name;
  }
  (*track_object)[PROPERTIES] = properties;
}

void GeoJsonFormat::geojson_track_disp(const Waypoint* trackpoint) const
{

  QJsonArray coords;
  coords.append(trackpoint->longitude);
  coords.append(trackpoint->latitude);
  if (trackpoint->altitude != unknown_alt && trackpoint->altitude != 0) {
    coords.append(trackpoint->altitude);
  }
  track_coords->append(coords);
}

void GeoJsonFormat::geojson_track_tlr(const route_head* /*unused*/)
{
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

void
GeoJsonFormat::write()
{
  auto geojson_waypt_pr_lambda = [this](const Waypoint* waypointp)->void {
    geojson_waypt_pr(waypointp);
  };
  waypt_disp_all(geojson_waypt_pr_lambda);

  auto geojson_track_hdr_lambda = [this](const route_head* rte)->void {
    geojson_track_hdr(rte);
  };
  auto geojson_track_tlr_lambda = [this](const route_head* rte)->void {
    geojson_track_tlr(rte);
  };
  auto geojson_track_disp_lambda = [this](const Waypoint* waypointp)->void {
    geojson_track_disp(waypointp);
  };
  track_disp_all(geojson_track_hdr_lambda, geojson_track_tlr_lambda, geojson_track_disp_lambda);
}

