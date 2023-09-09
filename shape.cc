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

#include <cassert>               // for assert

#include <QByteArray>            // for QByteArray
#include <QLatin1String>         // for QLatin1String
#include <QString>               // for QString, Qt::SkipEmptyParts
#include <QStringList>           // for QStringList
#include <QVector>               // for QVector
#include <Qt>                    // for CaseInsensitive
#include <QtGlobal>              // for qPrintable

#include "defs.h"
#include "shape.h"
#if SHAPELIB_ENABLED
#if HAVE_LIBSHAPE
#  include <shapefil.h>
#else
#  include "shapelib/shapefil.h"
#endif


#define MYNAME "shape"

/************************************************************************/
/*                              SHPOpenGpsbabel()                       */
/************************************************************************/

SHPHandle SHPAPI_CALL
ShapeFormat::SHPOpenGpsbabel(const QString& pszLayer, const char* pszAccess)

{
  SAHooks sHooks;

#ifdef SHPAPI_UTF8_HOOKS
  SASetupUtf8Hooks(&sHooks);
  return SHPOpenLL(pszLayer.toUtf8().constData(), pszAccess, &sHooks);
#else
  SASetupDefaultHooks(&sHooks);
  return SHPOpenLL(qPrintable(pszLayer), pszAccess, &sHooks);
#endif

}

/************************************************************************/
/*                             SHPCreateGpsbabel()                      */
/*                                                                      */
/*      Create a new shape file and return a handle to the open         */
/*      shape file with read/write access.                              */
/************************************************************************/

SHPHandle SHPAPI_CALL
ShapeFormat::SHPCreateGpsbabel(const QString& pszLayer, int nShapeType)

{
  SAHooks sHooks;

#ifdef SHPAPI_UTF8_HOOKS
  SASetupUtf8Hooks(&sHooks);
  return SHPCreateLL(pszLayer.toUtf8().constData(), nShapeType, &sHooks);
#else
  SASetupDefaultHooks(&sHooks);
  return SHPCreateLL(qPrintable(pszLayer), nShapeType, &sHooks);
#endif

}

/************************************************************************/
/*                              DBFOpenGpsbabel()                       */
/*                                                                      */
/*      Open a .dbf file.                                               */
/************************************************************************/

DBFHandle SHPAPI_CALL
ShapeFormat::DBFOpenGpsbabel(const QString& pszFilename, const char* pszAccess)

{
  SAHooks sHooks;

#ifdef SHPAPI_UTF8_HOOKS
  SASetupUtf8Hooks(&sHooks);
  return DBFOpenLL(pszFilename.toUtf8().constData(), pszAccess, &sHooks);
#else
  SASetupDefaultHooks(&sHooks);
  return DBFOpenLL(qPrintable(pszFilename), pszAccess, &sHooks);
#endif

}

/************************************************************************/
/*                            DBFCreateExGpsbabel()                     */
/*                                                                      */
/*      Create a new .dbf file.                                         */
/************************************************************************/

DBFHandle SHPAPI_CALL
ShapeFormat::DBFCreateExGpsbabel(const QString& pszFilename, const char* pszCodePage)

{
  SAHooks sHooks;

#ifdef SHPAPI_UTF8_HOOKS
  SASetupUtf8Hooks(&sHooks);
  return DBFCreateLL(pszFilename.toUtf8().constData(), pszCodePage, &sHooks);
#else
  SASetupDefaultHooks(&sHooks);
  return DBFCreateLL(qPrintable(pszFilename), pszCodePage, &sHooks);
#endif

}

#ifdef DEAD_CODE_IS_REBORN
/************************************************************************/
/*                             DBFCreateGpsbabel()                      */
/*                                                                      */
/* Create a new .dbf file with default code page LDID/87 (0x57)         */
/************************************************************************/

DBFHandle SHPAPI_CALL
ShapeFormat::DBFCreateGpsbabel(const QString& pszFilename)

{
  return DBFCreateExGpsbabel(pszFilename, "LDID/87");   // 0x57
}
#endif

[[noreturn]] void ShapeFormat::dump_fields() const
{
  char name[12];
  warning(MYNAME ": Database fields:\n");
  const int nFields = DBFGetFieldCount(ihandledb);
  for (int i = 0; i < nFields; i++) {
    DBFFieldType type = DBFGetFieldInfo(ihandledb, i, name, nullptr, nullptr);
    warning(MYNAME ": Field Index: %2d, Field Name: %12s, Field Type %d\n", i, name, type);
  }
  fatal("\n");
}

void ShapeFormat::check_field_index(const int fieldIdx) const
{
  const int maxFields = DBFGetFieldCount(ihandledb);
  if (fieldIdx < 0 || fieldIdx >= maxFields) {
    warning(MYNAME ": dbf file for %s doesn't have a field index number %d.\n", qPrintable(ifname), fieldIdx);
    warning(MYNAME ": the minimum field number is 0 and the maximum field number is %d.\n",maxFields-1);
    dump_fields();
  }
}

int ShapeFormat::get_field_index(const QString& fieldName) const
{
  const int fieldIdx = DBFGetFieldIndex(ihandledb, CSTR(fieldName));
  if (fieldIdx < 0) {
    warning(MYNAME ": dbf file for %s doesn't have a field named '%s'.\n", qPrintable(ifname), qPrintable(fieldName));
    dump_fields();
  }
  return fieldIdx;
}

void
ShapeFormat::rd_init(const QString& fname)
{
  ifname = fname;
  // TODO: The .prj file can define the the coordinate system and projection information.
  ihandle = SHPOpenGpsbabel(fname, "rb");
  if (ihandle == nullptr) {
    fatal(MYNAME ": Cannot open shp file %s for reading\n", qPrintable(ifname));
  }

  ihandledb = DBFOpenGpsbabel(fname, "rb");
  if (ihandledb == nullptr) {
    fatal(MYNAME ": Cannot open dbf file %s for reading\n", qPrintable(ifname));
  }
  const char* codepage = DBFGetCodePage(ihandledb);
  if (codepage) {
    QString qcodepage(codepage);
    if (qcodepage.compare(u"UTF-8", Qt::CaseInsensitive)) {
      warning(MYNAME ": dbf file %s is in code page %s, but we always process dbf files as UTF-8.\n",qPrintable(ifname),codepage);
    }
  } else {
    warning(MYNAME ": dbf file %s uses unknown code page, assuming UTF-8.\n", qPrintable(ifname));
  }
}

void
ShapeFormat::read()
{
  // option processing here instead of in rd_init
  // lets the results of option processing be automatic.
  int nameidx;
  int urlidx;
  QVector<int> nameindices;

  if (opt_name) {
    const QString qopt_name(opt_name);
    if (qopt_name.startsWith('?')) {
      dump_fields();
    }
    if (qopt_name.contains('+')) {
      // form a compound name from one or more fields.
      nameidx = -2;
#if (QT_VERSION < QT_VERSION_CHECK(5, 15, 0))
      const QStringList opt_name_fields = qopt_name.split('+', QString::SkipEmptyParts);
#else
      const QStringList opt_name_fields = qopt_name.split('+', Qt::SkipEmptyParts);
#endif
      nameindices.reserve(opt_name_fields.size());
      for (int oidx=0; oidx<opt_name_fields.size(); oidx++) {
        bool ok;
        int fieldIdx = opt_name_fields.at(oidx).toInt(&ok);
        if (ok) {
          // retrieve name component from given field number
          check_field_index(fieldIdx);
        } else {
          // retrieve name component from given field name.
          fieldIdx = get_field_index(opt_name_fields.at(oidx));
        }
        nameindices.append(fieldIdx);
      }
    } else {
      // if we can convert it to a number assume it is a field index
      // otherwise assume it is a field name.
      bool ok;
      nameidx = qopt_name.toInt(&ok);
      if (ok) {
        check_field_index(nameidx);
      } else {
        nameidx = get_field_index(qopt_name);
      }
    }
  } else {
    // will return -1 if "NAME" is not a field (case insensitive).
    nameidx = DBFGetFieldIndex(ihandledb, "NAME");
  }

  if (opt_url) {
    const QString qopt_url(opt_url);
    // if we can convert it to a number assume it is a field index
    // otherwise assume it is a field name.
    bool ok;
    urlidx = qopt_url.toInt(&ok);
    if (ok) {
      check_field_index(urlidx);
    } else {
      urlidx = get_field_index(qopt_url);
    }
  } else {
    // will return -1 if "URL" is not a field (case insensitive).
    urlidx = DBFGetFieldIndex(ihandledb, "URL");
  }

  int npts;
  const char* etype = "unknown";

  SHPGetInfo(ihandle, &npts, nullptr, nullptr, nullptr);
  for (int iShape=0; iShape<npts; iShape++) {
    Waypoint* wpt;
    QString name;
    QString url;

    SHPObject* shp = SHPReadObject(ihandle, iShape);
    if (nameidx >= 0) {
      name = DBFReadStringAttribute(ihandledb, iShape, nameidx);
//  } else if (nameidx == -1) {
//      leave name as a null QString.
    } else if (nameidx == -2) {
      for (int nameindice : nameindices) {
        // form a compound name from one or more dbf fields.
        QString namecomponent = DBFReadStringAttribute(
                                  ihandledb, iShape, nameindice);
        if (!name.isEmpty() && !namecomponent.isEmpty()) {
          name.append(QLatin1String(" / "));
        }
        name.append(namecomponent);
      }
    }
    if (urlidx >= 0) {
      url = DBFReadStringAttribute(ihandledb, iShape, urlidx);
    }
    const bool hasZ = shp->nSHPType == SHPT_ARCZ
                      || shp->nSHPType == SHPT_POINTZ
                      || shp->nSHPType == SHPT_POLYGONZ
                      || shp->nSHPType == SHPT_MULTIPOINTZ
                      || shp->nSHPType == SHPT_MULTIPATCH;
    switch (shp->nSHPType) {
    case SHPT_ARC:
    case SHPT_ARCZ:
    case SHPT_ARCM: {
// A part is a connected sequence of two or more points.
// Parts may or may not be connected to one another.
// Parts may or may not intersect one another.
      for (int part=0; part < shp->nParts; part++) {
        auto* routehead = new route_head;
        routehead->rte_name = name;
        route_add_head(routehead);
        int endVertex;
        if (part < (shp->nParts - 1)) {
          endVertex = shp->panPartStart[part+1];
        } else {
          endVertex = shp->nVertices;
        }
        for (int j = shp->panPartStart[part]; j < endVertex; j++) {
          wpt = new Waypoint;
          wpt->latitude = shp->padfY[j];
          wpt->longitude = shp->padfX[j];
          if (hasZ) {
            wpt->altitude = shp->padfZ[j];
          }
          route_add_wpt(routehead, wpt);
        }
      }
      break;
    }
    case SHPT_POINT:
    case SHPT_POINTZ:
    case SHPT_POINTM:
      wpt = new Waypoint;
      wpt->latitude = shp->dfYMin;
      wpt->longitude = shp->dfXMin;
      if (hasZ) {
        wpt->altitude = shp->dfZMin;
      }
      wpt->shortname = name;
      if (!url.isEmpty()) {
        wpt->AddUrlLink(url);
      }
      waypt_add(wpt);
      break;
    case SHPT_POLYGON:
      etype = "polygon";
      goto err;
    case SHPT_MULTIPOINT:
      etype = "multipoint";
      goto err;
    case SHPT_POLYGONZ:
      etype = "polygonz" ;
      goto err;
    case SHPT_MULTIPOINTZ:
      etype = "multipointz" ;
      goto err;
    case SHPT_POLYGONM:
      etype = "polygonm" ;
      goto err;
    case SHPT_MULTIPOINTM:
      etype = "multipointm" ;
      goto err;
    case SHPT_MULTIPATCH:
      etype = "multipatch" ;
      goto err;
    default:

err:
      warning("This file contains shapefile geometry type %s that does not naturally convert\nCustom programming is likely required.\n",
              etype);
      break;
    }

    SHPDestroyObject(shp);

  }

}

void
ShapeFormat::rd_deinit()
{
  SHPClose(ihandle);
  DBFClose(ihandledb);
  ifname.clear();
}

void
ShapeFormat::wr_init(const QString& fname)
{
  ofname = fname;
}

void
ShapeFormat::wr_deinit()
{
  SHPClose(ohandle);
  DBFClose(ohandledb);
  ofname.clear();
}

void
ShapeFormat::write_wpt(const Waypoint* wpt) const
{
  // note that the z coordinate (&wpt->altitude) does not apply
  // to SHPT_POINT.
  // We could potentially write SHPT_POINTZ, but we would have
  // to address what to do when we don't have altitude data.
  SHPObject* shpobject = SHPCreateSimpleObject(SHPT_POINT, 1,
                         &wpt->longitude,
                         &wpt->latitude,
                         &wpt->altitude);
  int iShape = SHPWriteObject(ohandle, -1, shpobject);
  SHPDestroyObject(shpobject);
  DBFWriteStringAttribute(ohandledb, iShape, nameFieldIdx,
                          CSTR(wpt->shortname));
}

void
ShapeFormat::poly_init(const route_head* rte)
{
  const int ct = rte->rte_waypt_ct();
  poly_count = 0;
  polybufx = new double[ct];
  polybufy = new double[ct];
  polybufz = new double[ct];
}


void
ShapeFormat::poly_point(const Waypoint* wpt)
{
  polybufx[poly_count] = wpt->longitude;
  polybufy[poly_count] = wpt->latitude;
  polybufz[poly_count] = wpt->altitude;
  poly_count++;
}

void
ShapeFormat::poly_deinit(const route_head* rte)
{
  // note that the z coordinate (polybufz) does not apply
  // to SHPT_ARC.
  // We could potentially write SHPT_ARCZ, but we would have
  // to address what to do when we don't have altitude data.
  assert(rte->rte_waypt_ct() == poly_count);
  SHPObject* shpobject = SHPCreateSimpleObject(SHPT_ARC, poly_count,
                         polybufx, polybufy, polybufz);
  int iShape = SHPWriteObject(ohandle, -1,  shpobject);
  SHPDestroyObject(shpobject);
  DBFWriteStringAttribute(ohandledb, iShape, nameFieldIdx,
                          CSTR(rte->rte_name));
  delete[] polybufx;
  delete[] polybufy;
  delete[] polybufz;
  poly_count = 0;
}


void
ShapeFormat::write()
{
  // shape files can only contain one shape type in addition
  // to the null shape type.
  // Therefore we must pick whether to output waypoint or
  // route/track data.
  switch (global_opts.objective) {
  case wptdata:
  case unknown_gpsdata: {
    ohandle = SHPCreateGpsbabel(ofname, SHPT_POINT);

    if (ohandle == nullptr) {
      fatal(MYNAME ": Cannot open shp file %s for writing\n",
            qPrintable(ofname));
    }
    ohandledb = DBFCreateExGpsbabel(ofname, "UTF-8\n");
    if (ohandledb == nullptr) {
      fatal(MYNAME ": Cannot open dbf file %s for writing\n",
            qPrintable(ofname));
    }
    nameFieldIdx=DBFAddField(ohandledb,"name",FTString,100,0);
    auto write_wpt_lambda = [this](const Waypoint* wpt)->void {
      write_wpt(wpt);
    };
    waypt_disp_all(write_wpt_lambda);
    break;
  }
  case rtedata:
  case trkdata: {
    ohandle = SHPCreateGpsbabel(ofname, SHPT_ARC);

    if (ohandle == nullptr) {
      fatal(MYNAME ": Cannot open shp file %s for writing\n",
            qPrintable(ofname));
    }
    ohandledb = DBFCreateExGpsbabel(ofname, "UTF-8\n");
    if (ohandledb == nullptr) {
      fatal(MYNAME ": Cannot open dbf file %s for writing\n",
            qPrintable(ofname));
    }
    nameFieldIdx=DBFAddField(ohandledb,"name",FTString,100,0);
    auto poly_init_lambda = [this](const route_head* rte)->void {
      poly_init(rte);
    };
    auto poly_deinit_lambda = [this](const route_head* rte)->void {
      poly_deinit(rte);
    };
    auto poly_point_lambda = [this](const Waypoint* wpt)->void {
      poly_point(wpt);
    };
    if (global_opts.objective == trkdata) {
      track_disp_all(poly_init_lambda, poly_deinit_lambda, poly_point_lambda);
    } else { // rtedata
      route_disp_all(poly_init_lambda, poly_deinit_lambda, poly_point_lambda);
    }
    break;
  }
  case posndata:
    fatal(MYNAME ": Realtime positioning not supported\n");
    break;
  }
}
#endif /* SHAPELIB_ENABLED */
