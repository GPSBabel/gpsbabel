/*
    Describe vectors containing file operations.
 
    Copyright (C) 2002 Robert Lipe, robertlipe@usa.net

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

#include <stdio.h>
#include "defs.h"

typedef struct {
	ff_vecs_t *vec;
	char *name;
	char *desc;
} vecs_t;

extern ff_vecs_t geo_vecs;
extern ff_vecs_t gpsman_vecs;
extern ff_vecs_t gpx_vecs;
extern ff_vecs_t mag_vecs;
extern ff_vecs_t mapsend_vecs;
extern ff_vecs_t mapsource_vecs;
extern ff_vecs_t gpsutil_vecs;
extern ff_vecs_t tiger_vecs;
extern ff_vecs_t pcx_vecs;
extern ff_vecs_t csv_vecs;
extern ff_vecs_t cetus_vecs;
extern ff_vecs_t gpspilot_vecs;
extern ff_vecs_t psp_vecs;
extern ff_vecs_t garmin_vecs;
extern ff_vecs_t mxf_vecs;
extern ff_vecs_t holux_vecs;
extern ff_vecs_t ozi_vecs;
extern ff_vecs_t tpg_vecs;
extern ff_vecs_t dna_vecs;
extern ff_vecs_t magnav_vec;
extern ff_vecs_t xmap_vecs;
extern ff_vecs_t tmp_vecs;

static
vecs_t vec_list[] = {
	{
		&geo_vecs, 
		"geo",
		"Geocaching.com .loc"
	}, 
	{
		&gpsman_vecs,
		"gpsman",
		"GPSman"
	},
	{
		&gpx_vecs,
		"gpx",
		"GPX XML"
	},
	{
		&mag_vecs,
		"magellan",
		"Magellan protocol"
	},
	{
		&mapsend_vecs,
		"mapsend",
		"Magellan Mapsend"
	},
	{
		&pcx_vecs,
		"pcx",
		"Garmin PCX5"
	},
	{
		&mapsource_vecs,
		"mapsource",
		"Garmin Mapsource"
	},
	{
		&gpsutil_vecs,
		"gpsutil",
		"gpsutil"
	},
	{
		&tiger_vecs,
		"tiger",
		"U.S. Census Bureau Tiger Mapping Service"
	},
	{
		&csv_vecs,
		"csv",
		"Comma separated values"
	},
	{
		&xmap_vecs,
		"xmap",
		"Delorme Topo USA4/XMap Conduit"
	},
	{
		&dna_vecs,
		"dna",
		"Navitrak DNA marker format"
	},
	{
		&psp_vecs,
		"psp",
		"MS PocketStreets 2002 Pushpin"
	},
	{
		&cetus_vecs,
		"cetus",
		"Cetus for Palm/OS"
	},
	{
		&gpspilot_vecs,
		"gpspilot",
		"GPSPilot Tracker for Palm/OS"
	},
	{
		&magnav_vec,
		"magnav",
		"Magellan NAV Companion for PalmOS"
	},
	{
		&garmin_vecs,
		"garmin",
		"Garmin serial protocol"
	},
	{
		&mxf_vecs,
		"mxf",
		"MapTech Exchange Format"
	},
	{
		&holux_vecs,
		"holux",
		"Holux (gm-100) .wpo Format"
	},
	{
		&ozi_vecs,
		"ozi",
		"OziExplorer Waypoint"
	},
	{
		&tpg_vecs,
		"tpg",
		"National Geographic Topo .tpg"
	},
	{
		&tmp_vecs,
		"tmp",
		"TopoMapPro"
	},

        {
		NULL,
		NULL,
		NULL
	}
};

ff_vecs_t *
find_vec(char *const vecname, char **opts)
{
	vecs_t *vec = vec_list;
	char *v = xstrdup(vecname);
	char *svecname = strtok(v, ",");

	while (vec->vec) {
		if (strcmp(svecname, vec->name) == 0) {
			char * res = strchr(vecname, ',');
			if (res)
				*opts = strchr(vecname, ',')+1;
			else
				*opts = NULL;
			free(v);
			return vec->vec;
		}
		vec++;
	}
	free(v);
	return NULL;
}

/*
 * Find and return a specific argument in an arg list.
 * Modelled approximately after getenv.
 */
char *
get_option(const char *iarglist, const char *argname)
{
	size_t arglen = strlen(argname);
	char *arglist;
	char *rval = NULL;
	char *arg;
	char *argp;

	if (!iarglist) {
		return NULL;
	}

	arglen = strlen(argname);
	arglist = xstrdup(iarglist);

	for (arg = arglist; argp = strtok(arg, ","); arg = NULL) {
		if (0 == strncmp(argp, argname, arglen)) {
			/*
			 * If we have something of the form "foo=bar"
			 * return "bar".   Otherwise, we assume we have
			 * simply "foo" so we return that.
			 */
			if (argp[arglen] == '=')
				rval = argp + arglen + 1;
			else
				rval = argp;
			break;
		}
	}
	/*
	 * Return an offset into the allocated copy.
	 * The caller mustn't free or otherwise get froggy with 
	 * this data.
	 */
	return rval;
}

void
disp_vecs(void)
{
	vecs_t *vec;
	for (vec = vec_list; vec->vec; vec++) {
		printf("%-20.20s  %-50.50s\n",
			vec->name, vec->desc);
	}
}
