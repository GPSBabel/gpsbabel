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
extern filter_vecs_t duplicate_vecs;

static
fl_vecs_t filter_vec_list[] = {
	{
		&position_vecs, 
		"position",
		"Remove Points Within Distance",
	}, 
	{
		&duplicate_vecs, 
		"duplicate",
		"Remove Duplicates",
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
 *  Display the available formats in a format that's easy for humans to
 *  parse for help on available command line options.
 */
void
disp_filter_vecs(void)
{
	fl_vecs_t *vec;
	for (vec = filter_vec_list; vec->vec; vec++) {
		printf("	%-20.20s  %-50.50s\n",
			vec->name, vec->desc);
	}
}

/*
 *  Display the available formats in a format that's easy to machine
 *  parse.   Typically invoked by programs like graphical wrappers to
 *  determine what formats are supported.
 */
void
disp_filters(void)
{
	fl_vecs_t *vec;
	for (vec = filter_vec_list; vec->vec; vec++) {
		printf("%s\t%s\n", vec->name, vec->desc);
	}
}
