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
#ifndef GEOJSON_H_INCLUDED_
#define GEOJSON_H_INCLUDED_

#include <QJsonArray>                // for QJsonArray
#include <QJsonObject>               // for QJsonObject
#include <QString>                   // for QString, QStringLiteral
#include <QVector>                   // for QVector

#include "defs.h"
#include "format.h"                  // for Format
#include "src/core/file.h"

class GeoJsonFormat : public Format
{
public:
  /* Member Functions */

  QVector<arglist_t>* get_args() override
  {
    return &geojson_args;
  }

  ff_type get_type() const override
  {
    return ff_type_file;
  }

  QVector<ff_cap> get_cap() const override
  {
    return FF_CAP_RW_ALL;
  }

  QString get_encode() const override
  {
    return CET_CHARSET_UTF8;
  }

  int get_fixed_encode() const override
  {
    return 0;
  }

  void rd_init(const QString& fname) override;
  void read() override;
  void rd_deinit() override;
  void wr_init(const QString& fname) override;
  void write() override;
  void wr_deinit() override;

private:
  /* Member Functions */

  void geojson_waypt_pr(const Waypoint* waypoint) const;
  static Waypoint* waypoint_from_coordinates(const QJsonArray& coordinates);
  static void routes_from_polygon_coordinates(const QJsonArray& polygon);
  void geojson_track_hdr(const route_head* track);
  void geojson_track_disp(const Waypoint* trackpoint) const;
  void geojson_track_tlr(const route_head* unused);

  /* Data Members */

  gpsbabel::File* ifd{nullptr};
  gpsbabel::File* ofd{nullptr};
  const char* MYNAME = "geojson";
  char* compact_opt = nullptr;
  QJsonObject* track_object = nullptr;
  QJsonArray* track_coords = nullptr;

  const QString FEATURE_COLLECTION = QStringLiteral("FeatureCollection");
  const QString FEATURE = QStringLiteral("Feature");
  const QString POINT = QStringLiteral("Point");
  const QString MULTIPOINT = QStringLiteral("MultiPoint");
  const QString LINESTRING = QStringLiteral("LineString");
  const QString MULTILINESTRING = QStringLiteral("MultiLineString");
  const QString POLYGON = QStringLiteral("Polygon");
  const QString MULTIPOLYGON = QStringLiteral("MultiPolygon");
  const QString TYPE = QStringLiteral("type");
  const QString FEATURES = QStringLiteral("features");
  const QString COORDINATES = QStringLiteral("coordinates");
  const QString GEOMETRY = QStringLiteral("geometry");
  const QString PROPERTIES = QStringLiteral("properties");
  const QString NAME = QStringLiteral("name");
  const QString DESCRIPTION = QStringLiteral("description");
  const QString URL = QStringLiteral("url");
  const QString URLNAME = QStringLiteral("urlname");

  QVector<arglist_t> geojson_args = {
    {
      "compact", &compact_opt, "Compact Output. Default is off.",
      nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
  };

  QJsonArray* feature_collection = nullptr;

};
#endif // GEOJSON_H_INCLUDED_
