/*
    Describe vectors containing filter operations.
 
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
	filter_vecs_t *vec;
	const char *name;
	const char *desc;
} fl_vecs_t;

extern filter_vecs_t position_vecs;
extern filter_vecs_t radius_vecs;
extern filter_vecs_t duplicate_vecs;
extern filter_vecs_t arcdist_vecs;

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
					*ap->argval = get_option(*opts, ap->argstring);
				}
			}
		} else {
			*opts = NULL;
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
	if ( fvec->args && fvec->args->argval && *(fvec->args->argval) ) {
		xfree(*(fvec->args->argval));
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
			printf("	  %-18.18s    %-.50s\n",
				ap->argstring, ap->helpstring);
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
