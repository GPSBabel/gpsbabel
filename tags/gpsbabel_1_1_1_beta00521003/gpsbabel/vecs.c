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
#include "csv_util.h"

typedef struct {
	ff_vecs_t *vec;
	const char *name;
	const char *desc;
	const char *extension;
} vecs_t;

extern ff_vecs_t geo_vecs;
extern ff_vecs_t gpx_vecs;
extern ff_vecs_t mag_vecs;
extern ff_vecs_t mapsend_vecs;
extern ff_vecs_t mps_vecs;
extern ff_vecs_t gpsutil_vecs;
extern ff_vecs_t tiger_vecs;
extern ff_vecs_t pcx_vecs;
extern ff_vecs_t cetus_vecs;
extern ff_vecs_t gpspilot_vecs;
extern ff_vecs_t copilot_vecs;
extern ff_vecs_t psp_vecs;
extern ff_vecs_t garmin_vecs;
extern ff_vecs_t holux_vecs;
extern ff_vecs_t xcsv_vecs;
extern ff_vecs_t tpg_vecs;
extern ff_vecs_t magnav_vec;
extern ff_vecs_t tmpro_vecs;
extern ff_vecs_t gcdb_vecs;
extern ff_vecs_t easygps_vecs;

static
vecs_t vec_list[] = {
	/* XCSV must be the first entry in this table. */
	{
		&xcsv_vecs,
		"xcsv",
		"? Character Separated Values",
		NULL
	},
	{
		&geo_vecs, 
		"geo",
		"Geocaching.com .loc",
		"loc"
	}, 
	{
		&gpx_vecs,
		"gpx",
		"GPX XML",
		"gpx"
	},
	{
		&mag_vecs,
		"magellan",
		"Magellan protocol", 
		NULL
	},
	{
		&mapsend_vecs,
		"mapsend",
		"Magellan Mapsend", 
		NULL
	},
	{
		&pcx_vecs,
		"pcx",
		"Garmin PCX5",
		"pcx"
	},
	{
		&mps_vecs,
		"mapsource",
		"Garmin Mapsource",
		NULL
	},
	{
		&gpsutil_vecs,
		"gpsutil",
		"gpsutil", 
		NULL
	},
	{
		&psp_vecs,
		"psp",
		"MS PocketStreets 2002 Pushpin",
		"psp"
	},
	{
		&cetus_vecs,
		"cetus",
		"Cetus for Palm/OS", 
		NULL
	},
	{
		&copilot_vecs,
		"copilot",
		"CoPilot Flight Planner for Palm/OS", 
		NULL
	},
	{
		&gpspilot_vecs,
		"gpspilot",
		"GPSPilot Tracker for Palm/OS", 
		NULL
	},
	{
		&magnav_vec,
		"magnav",
		"Magellan NAV Companion for PalmOS", 
		NULL
	},
	{
		&garmin_vecs,
		"garmin",
		"Garmin serial protocol", 
		NULL
	},
	{
		&holux_vecs,
		"holux",
		"Holux (gm-100) .wpo Format",
		"wpo"
	},
	{
		&tpg_vecs,
		"tpg",
		"National Geographic Topo .tpg",
		"tpg"
	},
	{
		&tmpro_vecs,
		"tmpro",
		"TopoMapPro Places File",
		"tmpro"
	},
	{
		&gcdb_vecs,
		"gcdb",
		"Geocaching Database", 
		NULL
	},
	{
		&tiger_vecs,
		"tiger",
		"U.S. Census Bureau Tiger Mapping Service",
		NULL
	},
	{
		&easygps_vecs,
		"easygps",
		"EasyGPS",
		NULL
	},
	{
		NULL,
		NULL,
		NULL,
		NULL
	}
};

ff_vecs_t *
find_vec(char *const vecname, char **opts)
{
	vecs_t *vec = vec_list;
	style_vecs_t *svec = style_list;
	char *v = xstrdup(vecname);
	char *svecname = strtok(v, ",");

	while (vec->vec) {
		arglist_t *ap;
		char *res;

		if (strcmp(svecname, vec->name)) {
			vec++;
			continue;
		}

		res = strchr(vecname, ',');
		if (res) {
			*opts = strchr(vecname, ',')+1;

			if (vec->vec->args) {
				for (ap = vec->vec->args; ap->argstring; ap++){
					void *av;
					av = get_option(*opts, ap->argstring);
					if (av) {
						*ap->argval = av;
					}
				}
			}
		} else {
			*opts = NULL;
		}

		xfree(v);
		return vec->vec;
		
	}

	/* 
	 * Didn't find it in the table of "real" file types, so plan B
	 * is to search the list of xcsv styles.
	 */
	while (svec->name) {
		arglist_t *ap;
		char *res;

		if (strcmp(svecname, svec->name)) {
			svec++;
			continue;
		}

		res = strchr(vecname, ',');
		if (res) {
			*opts = strchr(vecname, ',') + 1;
			if (vec_list[0].vec->args) {
				for (ap = vec_list[0].vec->args; ap->argstring; ap++) {
					*ap->argval = get_option(*opts, ap->argstring);
				}
			}
		} else {
			*opts = NULL;
		}
		xcsv_read_internal_style(svec->style_buf);

		xfree(v);

		return vec_list[0].vec;
	}
	
	/*
	 * Not found.
	 */
	xfree(v);
	return NULL;
}

/*
 * Find and return a specific argument in an arg list.
 * Modelled approximately after getenv.
 */
char *
#ifdef DEBUG_MEM
GET_OPTION(const char *iarglist, const char *argname, DEBUG_PARAMS)
#else
get_option(const char *iarglist, const char *argname)
#endif
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
	if ( rval ) {
		rval = xxstrdup(rval,file, line);
	}
	xfree(arglist);
	return rval;
}

/*
 *  Display the available formats in a format that's easy for humans to
 *  parse for help on available command line options.
 */
void
disp_vecs(void)
{
	vecs_t *vec;
	style_vecs_t *svec;
	arglist_t *ap;

#define VEC_FMT "	%-20.20s  %-.50s\n"

	for (vec = vec_list; vec->vec; vec++) {
		printf(VEC_FMT, vec->name, vec->desc);
		for (ap = vec->vec->args; ap && ap->argstring; ap++) {
		printf("	  %-18.18s    %-.50s\n",
			ap->argstring, ap->helpstring);
		}
	}

	/*
	 * Parse each internal style table in sequence to get the
	 * help strings from it.
	 */
	for (svec = style_list; svec->name; svec++) {
		xcsv_read_internal_style(svec->style_buf);
		printf(VEC_FMT, svec->name, xcsv_file.description);
	}
}

/*
 *  Display the available formats in a format that's easy to machine
 *  parse.   Typically invoked by programs like graphical wrappers to
 *  determine what formats are supported.
 */
void
disp_formats(int version)
{
	vecs_t *vec;
	style_vecs_t *svec;

	switch(version) {
	case 0:
		for (vec = vec_list; vec->vec; vec++) {
			printf("%s\t%s\t%s\n", vec->name, 
				vec->extension? vec->extension : "", 
				vec->desc);
		}
		for (svec = style_list; svec->name; svec++) {
			xcsv_read_internal_style(svec->style_buf);
			printf("%s\t%s\t%s\n", svec->name, xcsv_file.extension ? 
				xcsv_file.extension : "", xcsv_file.description);
		}
		break;
	default:
		;
	}
}
