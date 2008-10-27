/*
    Describe vectors containing filter operations.
 
    Copyright (C) 2002,2004,2005,2006,2007 Robert Lipe, robertlipe@usa.net

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
#include "filterdefs.h"
#include "inifile.h"
#include "gbversion.h"

typedef struct {
	filter_vecs_t *vec;
	const char *name;
	const char *desc;
} fl_vecs_t;

extern filter_vecs_t position_vecs;
extern filter_vecs_t radius_vecs;
extern filter_vecs_t duplicate_vecs;
extern filter_vecs_t arcdist_vecs;
extern filter_vecs_t polygon_vecs;
extern filter_vecs_t routesimple_vecs;
extern filter_vecs_t reverse_route_vecs;
extern filter_vecs_t sort_vecs;
extern filter_vecs_t stackfilt_vecs;
extern filter_vecs_t trackfilter_vecs;
extern filter_vecs_t discard_vecs;
extern filter_vecs_t nuke_vecs;
extern filter_vecs_t interpolatefilt_vecs;
extern filter_vecs_t transform_vecs;
extern filter_vecs_t height_vecs;
extern filter_vecs_t swapdata_vecs;

static
fl_vecs_t filter_vec_list[] = {
#if FILTERS_ENABLED
        {
		&arcdist_vecs,
	        "arc",
	        "Include Only Points Within Distance of Arc",
	},
	{
		&discard_vecs,
		"discard",
		"Remove unreliable points with high hdop or vdop"
	},
	{
		&duplicate_vecs, 
		"duplicate",
		"Remove Duplicates",
	},
	{
		&interpolatefilt_vecs,
		"interpolate",
		"Interpolate between trackpoints"
	},
	{
		&nuke_vecs,
		"nuketypes",
		"Remove all waypoints, tracks, or routes"
	},
	{
		&polygon_vecs,
		"polygon",
		"Include Only Points Inside Polygon",
	},
	{
		&position_vecs, 
		"position",
		"Remove Points Within Distance",
	}, 
	{
		&radius_vecs, 
		"radius",
		"Include Only Points Within Radius",
	}, 
	{
		&routesimple_vecs,
		"simplify",
		"Simplify routes",
	},
	{
		&sort_vecs,
		"sort",
		"Rearrange waypoints by resorting",
	},
	{
		&stackfilt_vecs,
		"stack",
		"Save and restore waypoint lists"
	},
	{
		&reverse_route_vecs,
		"reverse",
		"Reverse stops within routes",
	},
	{
		&trackfilter_vecs,
		"track",
		"Manipulate track lists"
	},
	{
		&transform_vecs,
		"transform",
		"Transform waypoints into a route, tracks into routes, ..."
	},
	{
		&height_vecs,
		"height",
		"Manipulate altitudes"
	},
	{
		&swapdata_vecs,
		"swap",
		"Swap latitude and longitude of all loaded points"
	},
	
#endif
        {
		NULL,
		NULL,
		NULL
	}
};

filter_vecs_t *
find_filter_vec(char *const vecname, char **opts)
{
	fl_vecs_t *vec = filter_vec_list;
	char *v = xstrdup(vecname);
	char *svecname = strtok(v, ",");
	int found = 0;

	while (vec->vec) {
		arglist_t *ap;
		char *res;

		if (case_ignore_strcmp(svecname, vec->name)) {
			vec++;
			continue;
		}

		/* step 1: initialize by inifile or default values */
		if (vec->vec->args) {
			for (ap = vec->vec->args; ap->argstring; ap++) {
				const char *temp;
				
				temp = inifile_readstr(global_opts.inifile, vec->name, ap->argstring);
				if (temp == NULL) temp = inifile_readstr(global_opts.inifile, "Common filter settings", ap->argstring);
				if (temp == NULL) temp = ap->defaultvalue;
				assign_option(vec->name, ap, temp);
			}
		}
		
		/* step 2: override settings with command-line values */
		res = strchr(vecname, ',');
		if (res) {
			*opts = res+1;

			if (vec->vec->args) {
				for (ap = vec->vec->args; ap->argstring; ap++){
					char *opt;
					
					opt = get_option(*opts, ap->argstring);
					if ( opt ) {
						found = 1;
						assign_option(vec->name, ap, opt);
						xfree(opt);
					}
				}
			}
		}
		if (opts && opts[0] && !found) {
			warning("'%s' is an unknown option to %s.\n", *opts, vec->name);
		}

		if (global_opts.debug_level >= 1)
			disp_vec_options(vec->name, vec->vec->args);
			
		xfree(v);
		return vec->vec;
		
	}
	xfree(v);
	return NULL;
}

void
free_filter_vec( filter_vecs_t *fvec )
{
	arglist_t *ap;
	
	if ( fvec->args ) {
		for ( ap = fvec->args; ap->argstring; ap++) {
			if (ap->argvalptr) {
				xfree(ap->argvalptr);
				ap->argvalptr = *ap->argval = NULL;
			}
		}
	}
}

void 
init_filter_vecs(void)
{
	fl_vecs_t *vec = filter_vec_list;
	while ( vec->vec ) {
		arglist_t *ap;
		if ( vec->vec->args ) {
			for ( ap = vec->vec->args; ap->argstring; ap++ ) {
				ap->argvalptr = NULL;
			}
		}
		vec++;
	}
}

void 
exit_filter_vecs( void )
{
	fl_vecs_t *vec = filter_vec_list;
	while ( vec->vec ) {
		if ( vec->vec->f_exit ) {
			(vec->vec->f_exit)();
		}
		vec++;
	}
}

/*
 *  Display the available formats in a format that's easy for humans to
 *  parse for help on available command line options.
 */
void
disp_filter_vecs(void)
{
	fl_vecs_t *vec;
	arglist_t *ap;

	for (vec = filter_vec_list; vec->vec; vec++) {
		printf("	%-20.20s  %-50.50s\n",
			vec->name, vec->desc);
		for (ap = vec->vec->args; ap && ap->argstring; ap++) {
			if ( !(ap->argtype & ARGTYPE_HIDDEN )) 
				printf("	  %-18.18s    %-.50s %s\n",
				ap->argstring, ap->helpstring,
				(ap->argtype&ARGTYPE_REQUIRED)?"(required)":"");
		}
	}
}

void
disp_filter_vec( const char *vecname )
{
	fl_vecs_t *vec;
	arglist_t *ap;

	for (vec = filter_vec_list; vec->vec; vec++) {
		if ( case_ignore_strcmp( vec->name, vecname )) {
			continue;
		}
		printf("	%-20.20s  %-50.50s\n",
			vec->name, vec->desc);
		for (ap = vec->vec->args; ap && ap->argstring; ap++) {
			if ( !(ap->argtype & ARGTYPE_HIDDEN )) 
				printf("	  %-18.18s    %-.50s %s\n",
				ap->argstring, ap->helpstring,
				(ap->argtype&ARGTYPE_REQUIRED)?"(required)":"");
		}
	}
}

static signed int
alpha (const void *a, const void *b)
{
        const fl_vecs_t *const ap = a;
        const fl_vecs_t *const bp = b;

        return case_ignore_strcmp(ap->desc , bp->desc);
}

static 
void disp_help_url(const fl_vecs_t *vec, arglist_t *arg)
{
	printf("\t" WEB_DOC_DIR "/fmt_%s.html", vec->name);
	if (arg) {
		printf("#fmt_%s_o_%s",vec->name, arg->argstring);
	}
}

static void
disp_v1(const fl_vecs_t *vec)
{
	arglist_t *ap;
	
	disp_help_url(vec, NULL);
	printf("\n");
	for (ap = vec->vec->args; ap && ap->argstring; ap++) {
		if ( !(ap->argtype & ARGTYPE_HIDDEN)) {
			printf("option\t%s\t%s\t%s\t%s\t%s\t%s\t%s",
				vec->name, 
				ap->argstring, 
				ap->helpstring, 
				name_option(ap->argtype),
				ap->defaultvalue? ap->defaultvalue : "",
				ap->minvalue? ap->minvalue : "",
				ap->maxvalue? ap->maxvalue : "");
			disp_help_url(vec, ap);
			printf("\n");
		}
	}
}

/*
 *  Display the available formats in a format that's easy to machine
 *  parse.   Typically invoked by programs like graphical wrappers to
 *  determine what formats are supported.
 */
void
disp_filters(int version)
{
	fl_vecs_t *vec;

	qsort(filter_vec_list, 
		sizeof(filter_vec_list) / sizeof(filter_vec_list[0]) - 1,
		sizeof(filter_vec_list[0]),
		alpha);
		

	switch(version) {
	case 0:
	case 1:
		for (vec = filter_vec_list; vec->vec; vec++) {
			if (version == 0) {
				printf("%s\t%s\n", vec->name, vec->desc);
			} else {
				printf("%s\t%s", vec->name, vec->desc);
				disp_v1(vec);
			}
		}
		break;
	default:
		;
	}
}
