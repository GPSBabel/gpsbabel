/*

    ESRI shp/shx shapefiles.

    Copyright (C) 2003 Robert Lipe, robertlipe+source@gpsbabel.org

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
#ifndef SHAPE_H_INCLUDED_
#define SHAPE_H_INCLUDED_

#include <QtCore/QString>       // for QString
#include <QtCore/QVector>       // for QVector

#include "defs.h"               // for arglist_t, ARGTYPE_STRING, Waypoint, route_head, CET_CHARSET_ASCII, FF_CAP_RW_ALL, ff_cap, ff_type, ff_type_file
#include "format.h"             // for Format
#include "shapelib/shapefil.h"  // for DBFHandle, SHPAPI_CALL, SHPHandle


#if SHAPELIB_ENABLED
class ShapeFormat : public Format
{
public:
  QVector<arglist_t>* get_args() override
  {
    return &shp_args;
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
    return CET_CHARSET_ASCII;
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
  static SHPHandle SHPAPI_CALL SHPOpenGpsbabel(const QString& pszLayer, const char* pszAccess);
  static SHPHandle SHPAPI_CALL SHPCreateGpsbabel(const QString& pszLayer, int nShapeType);
  static DBFHandle SHPAPI_CALL DBFOpenGpsbabel(const QString& pszFilename, const char* pszAccess);
  static DBFHandle SHPAPI_CALL DBFCreateExGpsbabel(const QString& pszFilename, const char* pszCodePage);
  void dump_fields() const;
  void check_field_index(int fieldIdx) const;
  int get_field_index(const QString& fieldName) const;
  void write_wpt(const Waypoint* wpt) const;
  void poly_init(const route_head* rte);
  void poly_point(const Waypoint* wpt);
  void poly_deinit(const route_head* rte);

  SHPHandle ihandle;
  DBFHandle ihandledb;
  SHPHandle ohandle;
  DBFHandle ohandledb;

  unsigned poly_count;
  double* polybufx;
  double* polybufy;
  double* polybufz;
  QString ifname;
  QString ofname;
  int nameFieldIdx;	// the field index of the field with fieldName "name" in the output DBF.

  char* opt_name = nullptr;
  char* opt_url = nullptr;

  QVector<arglist_t> shp_args = {
    {
      "name", &opt_name, "Source for name field in .dbf",
      nullptr, ARGTYPE_STRING, "0", nullptr, nullptr
    },
    {
      "url", &opt_url, "Source for URL field in .dbf",
      nullptr, ARGTYPE_STRING, "0", nullptr, nullptr
    },
  };
#endif /* SHAPELIB_ENABLED */
};
#endif // SHAPE_H_INCLUDED_
