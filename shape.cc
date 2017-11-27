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
    Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111 USA

 */
#include <QtCore/QLatin1String>
#include <QtCore/QString>
#include <QtCore/QStringList>
#include <QtCore/QVector>

#include "defs.h"
#include "shapelib/shapefil.h"
#include <cstdlib>

#if SHAPELIB_ENABLED
static SHPHandle ihandle;
static DBFHandle ihandledb;
static SHPHandle ohandle;
static DBFHandle ohandledb;
#define MYNAME "shape"

static unsigned poly_count;
static double* polybufx;
static double* polybufy;
static double* polybufz;
static QString ifname;
static QString ofname;
static int nameFieldIdx;	// the field index of the field with fieldName "name" in the output DBF.

static char* opt_name = NULL;
static char* opt_url = NULL;

static
arglist_t shp_args[] = {
  {
    "name", &opt_name, "Source for name field in .dbf",
    NULL, ARGTYPE_STRING, "0", NULL, nullptr
  },
  {
    "url", &opt_url, "Source for URL field in .dbf",
    NULL, ARGTYPE_STRING, "0", NULL, nullptr
  },
  ARG_TERMINATOR
};

static
void dump_fields(void)
{
  char name[12];
  warning(MYNAME ": Database fields:\n");
  const int nFields = DBFGetFieldCount(ihandledb);
  for (int i = 0; i < nFields; i++) {
    DBFFieldType type = DBFGetFieldInfo(ihandledb, i, name, NULL, NULL);
    warning(MYNAME ": Field Index: %2d, Field Name: %12s, Field Type %d\n", i, name, type);
  }
  fatal("\n");
}

static
void check_field_index(const int fieldIdx)
{
  const int maxFields = DBFGetFieldCount(ihandledb);
  if (fieldIdx < 0 || fieldIdx >= maxFields) {
    warning(MYNAME ": dbf file for %s doesn't have a field index number %d.\n", qPrintable(ifname), fieldIdx);
    warning(MYNAME ": the minimum field number is 0 and the maximum field number is %d.\n",maxFields-1);
    dump_fields();
  }
}

static
int get_field_index(const QString& fieldName)
{
  const int fieldIdx = DBFGetFieldIndex(ihandledb, CSTR(fieldName));
  if (fieldIdx < 0) {
    warning(MYNAME ": dbf file for %s doesn't have a field named '%s'.\n", qPrintable(ifname), qPrintable(fieldName));
    dump_fields();
  }
  return fieldIdx;
}

static void
my_rd_init(const QString& fname)
{
  ifname = fname;
  // TODO: The .prj file can define the the coordinate system and projection information.
  ihandle = SHPOpen(qPrintable(fname), "rb");
  if (ihandle == NULL) {
    fatal(MYNAME ": Cannot open shp file %s for reading\n", qPrintable(ifname));
  }

  ihandledb = DBFOpen(fname.toUtf8(), "rb");
  if (ihandledb == NULL) {
    fatal(MYNAME ": Cannot open dbf file %s for reading\n", qPrintable(ifname));
  }
  const char* codepage = DBFGetCodePage(ihandledb);
  if (codepage) {
    QString qcodepage(codepage);
    if (qcodepage.compare(QLatin1String("UTF-8"), Qt::CaseInsensitive)) {
      warning(MYNAME ": dbf file %s is in code page %s, but we always process dbf files as UTF-8.\n",qPrintable(ifname),codepage);
    }
  } else {
    warning(MYNAME ": dbf file %s uses unknown code page, assuming UTF-8.\n", qPrintable(ifname));
  }
}

void
my_read(void)
{
  // option processing here instead of in my_rd_init
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
      QStringList opt_name_fields = qopt_name.split('+', QString::SkipEmptyParts);
      nameindices.reserve(opt_name_fields.size());
      for (int oidx=0; oidx<opt_name_fields.size(); oidx++) {
        bool ok;
        int fieldIdx = opt_name_fields.at(oidx).toInt(&ok);
        if (ok) {
          // retrieve name commponent from given field number
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

  SHPGetInfo(ihandle, &npts, NULL, NULL, NULL);
  for (int iShape=0; iShape<npts; iShape++) {
    SHPObject* shp;
    Waypoint* wpt;
    QString name;
    QString url;

    shp = SHPReadObject(ihandle, iShape);
    if (nameidx >= 0) {
      name = DBFReadStringAttribute(ihandledb, iShape, nameidx);
//  } else if (nameidx == -1) {
//      leave name as a null QString.
    } else if (nameidx == -2) {
      for (int oidx=0; oidx<nameindices.size(); oidx++) {
        // form a compound name from one or more dbf fields.
        QString namecomponent = DBFReadStringAttribute(
                                  ihandledb, iShape, nameindices.at(oidx));
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
      route_head* routehead = route_head_alloc();
      routehead->rte_name = name;
      route_add_head(routehead);
      for (int j = 0; j < shp->nVertices; j++) {
        wpt = new Waypoint;
        wpt->latitude = shp->padfY[j];
        wpt->longitude = shp->padfX[j];
        if (hasZ) {
          wpt->altitude = shp->padfZ[j];
        }
        route_add_wpt(routehead, wpt);
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
my_rd_deinit(void)
{
  SHPClose(ihandle);
  DBFClose(ihandledb);
  ifname.clear();
}

void
my_wr_init(const QString& fname)
{
  ofname = fname;
}

void
my_wr_deinit(void)
{
  SHPClose(ohandle);
  DBFClose(ohandledb);
  ofname.clear();
}

void
my_write_wpt(const Waypoint* wpt)
{
  SHPObject* shpobject;

  // note that the z coordinate (&wpt->altitude) does not apply
  // to SHPT_POINT.
  // We could potentially write SHPT_POINTZ, but we would have
  // to address what to do when we don't have altitude data.
  shpobject = SHPCreateSimpleObject(SHPT_POINT, 1,
                                    (double*)(void*)&wpt->longitude,
                                    (double*)(void*)&wpt->latitude,
                                    (double*)(void*)&wpt->altitude);
  int iShape = SHPWriteObject(ohandle, -1, shpobject);
  SHPDestroyObject(shpobject);
  DBFWriteStringAttribute(ohandledb, iShape, nameFieldIdx,
                          CSTR(wpt->shortname));
}

void
poly_init(const route_head* rte)
{
  const int ct = rte->rte_waypt_ct;
  poly_count = 0;
  polybufx = (double*) xcalloc(ct, sizeof(double));
  polybufy = (double*) xcalloc(ct, sizeof(double));
  polybufz = (double*) xcalloc(ct, sizeof(double));
}


void
poly_point(const Waypoint* wpt)
{
  polybufx[poly_count] = wpt->longitude;
  polybufy[poly_count] = wpt->latitude;
  polybufz[poly_count] = wpt->altitude;
  poly_count++;
}

void
poly_deinit(const route_head* rte)
{
  SHPObject* shpobject;
  // note that the z coordinate (polybufz) does not apply
  // to SHPT_ARC.
  // We could potentially write SHPT_ARCZ, but we would have
  // to address what to do when we don't have altitude data.
  shpobject = SHPCreateSimpleObject(SHPT_ARC, poly_count,
                                    polybufx, polybufy, polybufz);
  int iShape = SHPWriteObject(ohandle, -1,  shpobject);
  SHPDestroyObject(shpobject);
  DBFWriteStringAttribute(ohandledb, iShape, nameFieldIdx,
                          CSTR(rte->rte_name));
  xfree(polybufx);
  xfree(polybufy);
  xfree(polybufz);
  poly_count = 0;
}


void
my_write(void)
{
  // shape files can only contain one shape type in addition
  // to the null shape type.
  // Therefore we must pick whether to output waypoint or
  // route/track data.
  switch (global_opts.objective) {
  case wptdata:
  case unknown_gpsdata:
    ohandle = SHPCreate(ofname.toUtf8(), SHPT_POINT);

    if (ohandle == NULL) {
      fatal(MYNAME ": Cannot open shp file %s for writing\n",
            qPrintable(ofname));
    }
    ohandledb = DBFCreateEx(qPrintable(ofname), "UTF-8\n");
    if (ohandledb == NULL) {
      fatal(MYNAME ": Cannot open dbf file %s for writing\n",
            qPrintable(ofname));
    }
    nameFieldIdx=DBFAddField(ohandledb,"name",FTString,100,0);
    waypt_disp_all(my_write_wpt);
    break;
  case rtedata:
  case trkdata:
    ohandle = SHPCreate(ofname.toUtf8(), SHPT_ARC);

    if (ohandle == NULL) {
      fatal(MYNAME ": Cannot open shp file %s for writing\n",
            qPrintable(ofname));
    }
    ohandledb = DBFCreateEx(qPrintable(ofname), "UTF-8\n");
    if (ohandledb == NULL) {
      fatal(MYNAME ": Cannot open dbf file %s for writing\n",
            qPrintable(ofname));
    }
    nameFieldIdx=DBFAddField(ohandledb,"name",FTString,100,0);
    if (global_opts.objective == trkdata) {
      track_disp_all(poly_init, poly_deinit, poly_point);
    } else { // rtedata
      route_disp_all(poly_init, poly_deinit, poly_point);
    }
    break;
  case posndata:
    fatal(MYNAME ": Realtime positioning not supported\n");
    break;
  }
}

ff_vecs_t shape_vecs = {
  ff_type_file,
  FF_CAP_RW_ALL,
  my_rd_init,
  my_wr_init,
  my_rd_deinit,
  my_wr_deinit,
  my_read,
  my_write,
  NULL,
  shp_args,
  CET_CHARSET_ASCII, 0	/* CET-REVIEW */
  , NULL_POS_OPS,
  nullptr
};
#endif /* SHAPELIB_ENABLED */
