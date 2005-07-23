/*
    Describe vectors containing filter operations.
 
    Copyright (C) 2002,2004,2005 Robert Lipe, robertlipe@usa.net

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
extern filter_vecs_t fix_vecs;

static
fl_vecs_t filter_vec_list[] = {
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
		&duplicate_vecs, 
		"duplicate",
		"Remove Duplicates",
	},
        {
		&arcdist_vecs,
	        "arc",
	        "Include Only Points Within Distance of Arc",
	},
	{
		&polygon_vecs,
		"polygon",
		"Include Only Points Inside Polygon",
	},
	{
		&routesimple_vecs,
		"simplify",
		"Simplify routes",
	},
	{
		&reverse_route_vecs,
		"reverse",
		"Reverse stops within routes",
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
		&trackfilter_vecs,
		"track",
		"Manipulate track lists"
	},
	{
		&fix_vecs,
		"dop",
		"Remove unreliable points with high hdop or vdop."
	},
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

	while (vec->vec) {
		arglist_t *ap;
		char *res;

		if (strcmp(svecname, vec->name)) {
			vec++;
			continue;
		}

		res = strchr(vecname, ',');
		if (res) {
			*opts = res+1;

			if (vec->vec->args) {
				for (ap = vec->vec->args; ap->argstring; ap++){
					char *opt = get_option(*opts, 
							ap->argstring);
					if ( opts ) {
						*ap->argval = opt;
					}
					else if ( ap->defaultvalue ) {
						*ap->argval = xstrdup(
							ap->defaultvalue);
					}
					else {
						*ap->argval = NULL;
					}
				}
			}
		} else {
			*opts = NULL;
			if (vec->vec->args) {
				for (ap = vec->vec->args; ap->argstring; ap++){
					if ( ap->defaultvalue ) {
						*ap->argval = xstrdup(
							ap->defaultvalue);
					}
					else {
						*ap->argval = NULL;
					}
				}
			}
		}

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
			if (ap->argval && *ap->argval) xfree(*ap->argval);
		}
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

static signed int
alpha (const void *a, const void *b)
{
        const fl_vecs_t *const ap = a;
        const fl_vecs_t *const bp = b;

        return case_ignore_strcmp(ap->desc , bp->desc);
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
		for (vec = filter_vec_list; vec->vec; vec++) {
			printf("%s\t%s\n", vec->name, vec->desc);
		}
		break;
	default:
		;
	}
}
