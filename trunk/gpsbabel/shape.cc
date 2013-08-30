/*

    ESRI shp/shx shapefiles.

    Copyright (C) 2003 Robert Lipe, robertlipe@usa.net

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
#include "shapelib/shapefil.h"

#if SHAPELIB_ENABLED
static SHPHandle ihandle;
static DBFHandle ihandledb;
static SHPHandle ohandle;
#define MYNAME "shape"

static unsigned poly_count;
static double* polybufx;
static double* polybufy;
static double* polybufz;
static const char* ofname;
static int nameidx;
static int urlidx;

static char* opt_name = NULL;
static char* opt_url = NULL;

static
arglist_t shp_args[] = {
  {
    "name", &opt_name, "Index of name field in .dbf",
    NULL, ARGTYPE_STRING, "0", NULL
  },
  {
    "url", &opt_url, "Index of URL field in .dbf",
    NULL, ARGTYPE_INT, "0", NULL
  },
  ARG_TERMINATOR
};

static void
my_rd_init(const char* fname)
{
  ihandle = SHPOpen(fname, "rb");
  if (ihandle == NULL) {
    fatal(MYNAME ":Cannot open shp file %s for reading\n", fname);
  }

  ihandledb = DBFOpen(fname, "rb");
  if (ihandledb == NULL) {
    fatal(MYNAME ":Cannot open dbf file %s for reading\n", fname);
  }

  if (opt_name) {
    if (opt_name[0] == '?') {
      int nFields = 0;
      int i = 0;
      char name[12];
      char* txt = xstrdup("  Database fields\n");
      nFields = DBFGetFieldCount(ihandledb);
      for (i = 0; i < nFields; i++) {
        char txtName[50];
        DBFGetFieldInfo(ihandledb, i, name, NULL, NULL);
        sprintf(txtName,"%2d %s\n", i, name);
        txt = xstrappend(txt, txtName);
      }
      txt = xstrappend(txt,  "\n");
      fatal("%s",  txt);
    }
    if (strchr(opt_name, '+')) {
      nameidx = -2;
    } else if (opt_name[0] >= '0' && opt_name[0] <= '9') {
      nameidx = atoi(opt_name);
    } else {
      nameidx = DBFGetFieldIndex(ihandledb, opt_name);
      if (nameidx == -1) {
        fatal(MYNAME ":dbf file for %s doesn't have '%s' field.\n", fname, opt_name);
      }
    }
  } else {
    nameidx = DBFGetFieldIndex(ihandledb, "NAME");
    if (nameidx == -1) {
//			fatal(MYNAME ":dbf file for %s doesn't have 'NAME' field.\n  Please specify the name index with the 'name' option.\n", fname);
    }
  }
  if (opt_url) {
    if (opt_url[0] >= '0' && opt_url[0] <= '9') {
      urlidx = atoi(opt_url);
    } else {
      urlidx = DBFGetFieldIndex(ihandledb, opt_url);
    }
  } else {
    urlidx = DBFGetFieldIndex(ihandledb, "URL");
  }
}

void
my_read(void)
{
  int npts;
  const char* etype = "unknown";

  SHPGetInfo(ihandle, &npts, NULL, NULL, NULL);

  while (npts) {
    SHPObject* shp;
    waypoint* wpt;
    const char* name = "";
    const char* url;
    char* tmpName = NULL;
    char* tmpIndex = opt_name;

    shp = SHPReadObject(ihandle, npts-1);
    if (nameidx >0) {
      name = DBFReadStringAttribute(ihandledb, npts-1, nameidx);
    } else {
      if (nameidx == -1) {
        name = "";
      } else if (nameidx == -2) {
        tmpName = xstrdup("");
        tmpIndex = opt_name;
        while (tmpIndex) {
          char* tmp2 = tmpIndex;
          tmpIndex = strchr(tmpIndex,'+');
          if (tmpIndex) {
            *tmpIndex = '\0';
            tmpIndex++;
          }
          if (tmp2[0]>='0' && tmp2[0]<='9') {
            name = DBFReadStringAttribute(
                     ihandledb, npts-1, atoi(tmp2));
          } else {
            int idx = 0;
            idx = DBFGetFieldIndex(ihandledb, tmp2);
            if (idx >= 0) {
              name = DBFReadStringAttribute(
                       ihandledb, npts-1, idx);
            }
          }

          tmpName = xstrappend(tmpName, name);
          if (tmpIndex) {
            tmpName = xstrappend(tmpName, " / ");
          }
        }
        name = tmpName;
      }
    }
    if (urlidx > 0) {
      url = DBFReadStringAttribute(ihandledb, npts-1, urlidx);
    } else {
      url = NULL;
    }
    switch (shp->nSHPType) {
    case SHPT_ARC: {
      int j;
      route_head* routehead = route_head_alloc();
      routehead->rte_name = xstrdup(name);
      route_add_head(routehead);
      for (j = 0; j < shp->nVertices; j++) {
        wpt = waypt_new();
        wpt->latitude = shp->padfY[j];
        wpt->longitude = shp->padfX[j];
        wpt->altitude = shp->padfZ[j];
        route_add_wpt(routehead, wpt);
      }
    }
    break;

    case SHPT_POINT:
      wpt = waypt_new();
      wpt->latitude = shp->dfYMin;
      wpt->longitude = shp->dfXMin;
      wpt->shortname = xstrdup(name);
      if (url) {
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
    case SHPT_POINTZ:
      etype = "pointz" ;
      goto err;
    case SHPT_ARCZ:
      etype = "arcz" ;
      goto err;
    case SHPT_POLYGONZ:
      etype = "polygonz" ;
      goto err;
    case SHPT_MULTIPOINTZ:
      etype = "multipointz" ;
      goto err;
    case SHPT_POINTM:
      etype = "pointm" ;
      goto err;
    case SHPT_ARCM:
      etype = "arcm" ;
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

    npts--;
    if (tmpName) {
      xfree(tmpName);
      tmpName = NULL;
    }
  }

}

void
my_rd_deinit(void)
{
  SHPClose(ihandle);
}

void
my_wr_init(const char* fname)
{
  ofname = fname;
}

void
my_wr_deinit(void)
{
  SHPClose(ohandle);
}

void
my_write_wpt(const waypoint* wpt)
{
  SHPObject* shpobject;

  shpobject = SHPCreateSimpleObject(SHPT_POINT, 1,
                                    (double*)(void*)&wpt->longitude,
                                    (double*)(void*)&wpt->latitude,
                                    (double*)(void*)&wpt->altitude);
  SHPWriteObject(ohandle, -1, shpobject);
  SHPDestroyObject(shpobject);
}

void
poly_init(const route_head* h)
{
  int ct = track_waypt_count();
  polybufx = (double*) xcalloc(ct, sizeof(double));
  polybufy = (double*) xcalloc(ct, sizeof(double));
  polybufz = (double*) xcalloc(ct, sizeof(double));
}


void
poly_point(const waypoint* wpt)
{
  polybufx[poly_count] = wpt->longitude;
  polybufy[poly_count] = wpt->latitude;
  polybufz[poly_count] = wpt->altitude;
  poly_count++;
}

void
poly_deinit(const route_head* h)
{
  SHPObject* shpobject;
  shpobject = SHPCreateSimpleObject(SHPT_ARC, track_waypt_count(),
                                    polybufx, polybufy, polybufz);
  SHPWriteObject(ohandle, -1,  shpobject);
  SHPDestroyObject(shpobject);
  xfree(polybufx);
  xfree(polybufy);
  xfree(polybufz);
  fprintf(stderr, "Done\n");
  poly_count = 0;
}


void
my_write(void)
{
  switch (global_opts.objective) {
  case wptdata:
  case unknown_gpsdata:
    ohandle = SHPCreate(ofname, SHPT_POINT);

    if (ohandle == NULL) {
      fatal(MYNAME ":Cannot open %s for writing\n",
            ofname);
    }
    waypt_disp_all(my_write_wpt);
    break;
  case trkdata:
    ohandle = SHPCreate(ofname, SHPT_ARC);

    if (ohandle == NULL) {
      fatal(MYNAME ":Cannot open %s for writing\n",
            ofname);
    }
    route_disp_all(poly_init, poly_deinit, poly_point);
    break;
  case rtedata:
    fatal(MYNAME ": Routes are not supported\n");
    break;
  case posndata:
    fatal(MYNAME ": Realtime positioning not supported\n");
    break;
  }
}

ff_vecs_t shape_vecs = {
  ff_type_internal,
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
};
#endif /* SHAPELIB_ENABLED */
