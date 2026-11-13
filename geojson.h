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

#include <QJsonArray>         // for QJsonArray
#include <QJsonObject>        // for QJsonObject
#include <QLatin1StringView>  // for QLatin1StringView
#include <QList>              // for QList
#include <QString>            // for QString
#include <QVector>            // for QVector

#include "defs.h"             // for arglist_t, Waypoint, route_head, ARG_NOMINMAX, ARGTYPE_STRING, ff_type, ARGTYPE_BOOL, FF_CAP_RW_ALL, ff_cap
#include "format.h"           // for Format
#include "option.h"           // for OptionString, OptionBool
#include "src/core/file.h"    // for File

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
  void geojson_route_hdr(const route_head* route);
  void geojson_route_disp(const Waypoint* routepoint) const;
  void geojson_route_tlr(const route_head* unused);

  /* Data Members */

  gpsbabel::File* ifd{nullptr};
  gpsbabel::File* ofd{nullptr};
  OptionBool compact_opt;
  OptionString name_opt;
  OptionString desc_opt;
  QJsonObject* track_object = nullptr;
  QJsonArray* track_coords = nullptr;
  QJsonObject* route_object = nullptr;
  QJsonArray* route_coords = nullptr;

  static constexpr QLatin1StringView FEATURE_COLLECTION{"FeatureCollection"};
  static constexpr QLatin1StringView FEATURE{"Feature"};
  static constexpr QLatin1StringView POINT{"Point"};
  static constexpr QLatin1StringView MULTIPOINT{"MultiPoint"};
  static constexpr QLatin1StringView LINESTRING{"LineString"};
  static constexpr QLatin1StringView MULTILINESTRING{"MultiLineString"};
  static constexpr QLatin1StringView POLYGON{"Polygon"};
  static constexpr QLatin1StringView MULTIPOLYGON{"MultiPolygon"};
  static constexpr QLatin1StringView TYPE{"type"};
  static constexpr QLatin1StringView FEATURES{"features"};
  static constexpr QLatin1StringView COORDINATES{"coordinates"};
  static constexpr QLatin1StringView GEOMETRY{"geometry"};
  static constexpr QLatin1StringView PROPERTIES{"properties"};
  static constexpr QLatin1StringView URL{"url"};
  static constexpr QLatin1StringView URLNAME{"urlname"};

  QVector<arglist_t> geojson_args = {
    {
      "compact", &compact_opt, "Compact Output. Default is off",
      nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    {
      "name", &name_opt, "Property key to use for name",
      "name", ARGTYPE_STRING, ARG_NOMINMAX, nullptr
    },
    {
      "desc", &desc_opt, "Property key to use for description",
      "description", ARGTYPE_STRING, ARG_NOMINMAX, nullptr
    },
  };

  QJsonArray* feature_collection = nullptr;

};
#endif // GEOJSON_H_INCLUDED_
