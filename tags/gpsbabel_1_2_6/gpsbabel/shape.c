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

static SHPHandle ihandle;
static DBFHandle ihandledb;
static SHPHandle ohandle;
#define MYNAME "shape"

static unsigned poly_count;
static double *polybufx;
static double *polybufy;
static double *polybufz;
static const char *ofname;
static int nameidx;

static void
my_rd_init(const char *fname)
{
	ihandle = SHPOpen(fname, "rb" );
	if (ihandle == NULL) {
		fatal(MYNAME ":Cannot open shp file %s for reading\n", fname);
	}

	ihandledb = DBFOpen(fname, "rb" );
	if (ihandledb == NULL) {
		fatal(MYNAME ":Cannot open dbf file %s for reading\n", fname);
	}

	nameidx = DBFGetFieldIndex( ihandledb, "NAME" );
	if (nameidx == -1) {
//		fatal(MYNAME ":dbf file for %s doesn't have 'NAME'\n", fname);
	}
}

void
my_read(void)
{
	int npts;

	SHPGetInfo(ihandle, &npts, NULL, NULL, NULL);

	while (npts) {
		SHPObject *shp;
		waypoint *wpt;
		const char *name;

		shp = SHPReadObject(ihandle, npts-1);
		name = DBFReadStringAttribute(ihandledb, npts-1, nameidx);
		if (shp->nSHPType == SHPT_ARC) {
			int j;
			route_head *track_head = route_head_alloc();
			route_add_head(track_head);
			for (j = 0; j < shp->nVertices; j++) {
				wpt = waypt_new();
				wpt->latitude = shp->padfY[j];
				wpt->longitude = shp->padfX[j];
				wpt->altitude = shp->padfZ[j];
				route_add_wpt(track_head, wpt);
			}
		}

		if (shp->nSHPType == SHPT_POINT) {
			wpt = waypt_new();
			wpt->latitude = shp->dfYMin;
			wpt->longitude = shp->dfXMin;
			wpt->shortname = strdup(name);
			waypt_add(wpt);
		}

		SHPDestroyObject(shp);

		npts--;
	}

}

void
my_rd_deinit(void)
{
	SHPClose (ihandle);
}

void
my_wr_init(const char *fname)
{
	ofname = fname;
}

void
my_wr_deinit(void)
{
	SHPClose (ohandle);
}

void
my_write_wpt(const waypoint *wpt)
{
	SHPObject *shpobject;

	shpobject = SHPCreateSimpleObject(SHPT_POINT, 1, 
			(double *)(void *)&wpt->longitude, 
			(double *)(void *)&wpt->latitude, 
			(double *)(void *)&wpt->altitude);
	SHPWriteObject(ohandle, -1, shpobject);
	SHPDestroyObject(shpobject);
}

void
poly_init(const route_head *h)
{
	int ct = route_waypt_count();
	polybufx = xcalloc(ct, sizeof(double));
	polybufy = xcalloc(ct, sizeof(double));
	polybufz = xcalloc(ct, sizeof(double));
}


void 
poly_point(const waypoint *wpt)
{
	polybufx[poly_count] = wpt->longitude;
	polybufy[poly_count] = wpt->latitude;
	polybufz[poly_count] = wpt->altitude;
	poly_count++;
}

void
poly_deinit(const route_head *h)
{
	SHPObject *shpobject;
	shpobject = SHPCreateSimpleObject(SHPT_ARC, route_waypt_count(), 
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
	switch(global_opts.objective) {
		case wptdata:
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
			fatal(MYNAME ":Routes are not supported\n");
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
	NULL
};
