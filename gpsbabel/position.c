/*
    Distance Between Points Filter

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
#include <math.h>
#include "defs.h"

#ifndef M_PI
#  define M_PI 3.14159265358979323846
#endif

extern queue waypt_head;

static double pos_dist;

static double
gc_distance(double lat1, double lon1, double lat2, double lon2)
{
	double rlat1, rlat2, rlon1, rlon2;

	/* convert to radians */
	rlat1 = (lat1 * M_PI) / 180.0;
	rlon1 = (lon1 * M_PI) / 180.0;
	rlat2 = (lat2 * M_PI) / 180.0;
	rlon2 = (lon2 * M_PI) / 180.0;

	return (acos((sin(rlat1) * sin(rlat2)) +
		(cos(rlat1) * cos(rlat2) * cos(rlon1 - rlon2))));
}

static int
position_comp(const void * a, const void * b)
{
	const waypoint *x1 = *(waypoint **)a;
	const waypoint *x2 = *(waypoint **)b;
	double latdiff, londiff, max;

	/*
	 * this compare makes the assumption that things will fall into
	 * order by declaring their biggest single axial difference.
	 * It is much less math than distance and bearing from some random
	 * point.
	 */

	londiff = (x1->position.longitude.degrees -
		   x2->position.longitude.degrees) * 1000000.0;
	latdiff = (x1->position.latitude.degrees -
		   x2->position.latitude.degrees) * 1000000.0;

	max = fabs(londiff) >= fabs(latdiff) ? floor(londiff) : floor(latdiff);

	if (max < 0)
		return (-1);
	else if (max > 0)
		return (1);

	return(0);
}

void
position_process(void)
{
	queue * elem, * tmp;
	waypoint ** comp;
	double dist;
	int i, wc;

	wc = waypt_count();

	comp = xcalloc(wc, sizeof(*comp));

	i = 0;

	QUEUE_FOR_EACH(&waypt_head, elem, tmp) {
		comp[i] = (waypoint *)elem;
		i++;
	}

	qsort(comp, wc, sizeof(waypoint *), position_comp);

	for (i = 0 ; i < (wc - 1) ; i++) {
		dist = gc_distance(comp[i]->position.latitude.degrees,
				   comp[i]->position.longitude.degrees,
				   comp[i+1]->position.latitude.degrees,
				   comp[i+1]->position.longitude.degrees);

		/* convert radians to integer feet */
		dist = (int)((((dist * 180.0 * 60.0) / M_PI) / 1.1516) * 5280.0);

		if (dist <= pos_dist) 
			waypt_del(comp[i]);
	}

	if (comp)
		free (comp);
}

void
position_init(const char *args) {
	char *fm;
	const char *p;

	p = get_option(args, "distance");

	if (p) {
		pos_dist = strtod(p, &fm);

		if ((*fm == 'm') || (*fm == 'M')) {
			 /* distance is meters */
			pos_dist *= 3.2802;
		}
	}
}

void
position_deinit(void) {
}

filter_vecs_t position_vecs = {
	position_init,
	position_process,
	position_deinit
};
