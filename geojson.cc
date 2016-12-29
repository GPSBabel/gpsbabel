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
#include <QtCore/QJsonDocument>
#include <QtCore/QJsonObject>
#include <QtCore/QJsonArray>
#include "src/core/file.h"

static gbfile* ofd;
static const char MYNAME[] = "geojson";
static char* compact_opt = NULL;
static QJsonObject* track_object = NULL;
static QJsonArray* track_coords = NULL;

static arglist_t geojson_args[] = {
  {"compact", &compact_opt, "Compact Output. Default is off.", 
    NULL, ARGTYPE_BOOL, ARG_NOMINMAX } ,
  ARG_TERMINATOR
};

static void
geojson_rd_init(const QString& fname) {
}

QJsonArray* feature_collection = nullptr;

static void
geojson_wr_init(const QString& fname) {
  feature_collection = new QJsonArray;
  ofd = gbfopen(fname, "w", MYNAME);
}

static void
geojson_waypt_pr(const Waypoint* waypoint) {
  QJsonObject object;
  static const QString kType = QStringLiteral("type");
  object[kType] = QStringLiteral("Feature");
  
  QJsonObject geometry;
  geometry[kType] = QStringLiteral("Point");

  QJsonArray coords;
  coords.append(waypoint->longitude);
  coords.append(waypoint->latitude);
  if (waypoint->altitude != unknown_alt && waypoint->altitude != 0) {
    coords.append(waypoint->altitude);
  }

  geometry[kType] = QStringLiteral("Point");
  geometry[QStringLiteral("coordinates")] = coords;
  object[QStringLiteral("geometry")] = geometry;

  // Build up the properties.
  QJsonObject properties;
  if (!waypoint->shortname.isEmpty()) {
    properties["name"] = waypoint->shortname;
  }
  if (!waypoint->description.isEmpty()) {
    properties["description"] = waypoint->description;
  }
  if (waypoint->HasUrlLink()) {
    UrlLink link = waypoint->GetUrlLink();
    if (!link.url_.isEmpty()) {
      properties["url"] = link.url_;
    }
    if (!link.url_link_text_.isEmpty()) {
      properties["urlname"] = link.url_link_text_;
    }
  }
  if (!properties.empty()) {
    object["properties"] = properties;
  }
  
  feature_collection->append(object);
}

static void
geojson_rd_deinit() {
}

static void
geojson_wr_deinit(void) {
  QJsonObject object;
  object[QStringLiteral("type")] = QStringLiteral("FeatureCollection");
  object[QStringLiteral("features")]  = *feature_collection;

  QJsonDocument save(object);
  QJsonDocument::JsonFormat style;
  style = compact_opt ? QJsonDocument::Compact : QJsonDocument::Indented;
  gbfputs(save.toJson(style),ofd);

  gbfclose(ofd);
  ofd = NULL;
  delete feature_collection;
  feature_collection = nullptr;
}

static void
geojson_read(void) {
}

static void geojson_track_hdr(const route_head* track) {
  track_object = new QJsonObject();

  (*track_object)[QStringLiteral("type")] = QStringLiteral("Feature");
  track_coords = new QJsonArray();

  QJsonObject properties;
  if (!track->rte_name.isEmpty()) {
    properties["name"] = track->rte_name;
  }
  (*track_object)["properties"] = properties;
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

static void geojson_track_tlr(const route_head* track) {
  QJsonObject geometry;
  geometry[QStringLiteral("type")] = QStringLiteral("LineString");
  geometry[QStringLiteral("coordinates")] = *track_coords;
  (*track_object)[QStringLiteral("geometry")] = geometry;
  feature_collection->append(*track_object);
  delete track_object;
  track_object = NULL;
  delete track_coords;
  track_coords = NULL;
}

static void
geojson_write(void) {
  waypt_disp_all(geojson_waypt_pr);
  track_disp_all(geojson_track_hdr, geojson_track_tlr, geojson_track_disp);
}

ff_vecs_t geojson_vecs = {
  ff_type_file,
  { (ff_cap)(/*ff_cap_read | */ff_cap_write), ff_cap_write, ff_cap_none },
  geojson_rd_init,
  geojson_wr_init,
  geojson_rd_deinit,
  geojson_wr_deinit,
  geojson_read,
  geojson_write,
  NULL,
  geojson_args,
  CET_CHARSET_UTF8, 0	/* CET-REVIEW */
};
