/*
    Distance Between Points Filter(s)

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
static char *distopt;
static char *latopt;
static char *lonopt;
static char *exclopt;

waypoint * home_pos;

typedef struct {
	double distance;
} extra_data;

static
arglist_t position_args[] = {
	{"distance", &distopt, "Maximum positional distance (required)"},
	{0, 0, 0}
};

static
arglist_t radius_args[] = {
	{"lat", &latopt,       "Latitude for center point (D.DDDDD)"},
	{"lon", &lonopt,       "Longitude for center point (D.DDDDD)"},
	{"distance", &distopt, "Maximum distance from center"},
	{"exclude", &exclopt,  "Exclude points close to center"},
	{0, 0, 0}
};

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

static int
dist_comp(const void * a, const void * b)
{
	const waypoint *x1 = *(waypoint **)a;
	const waypoint *x2 = *(waypoint **)b;
	extra_data *x1e = x1->extra_data;
	extra_data *x2e = x2->extra_data;

	if (x1e->distance > x2e->distance)
		return 1;
	if (x1e->distance < x2e->distance)
		return -1;
	return 0;

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
		dist = (int)((((dist * 180.0 * 60.0) / M_PI) * 1.1516) * 5280.0);

		if (dist <= pos_dist) {
			waypt_del(comp[i]);
			waypt_free(comp[i]);
		}
	}

	if (comp)
		xfree(comp);
}

void
position_init(const char *args) {
	char *fm;

	pos_dist = 0;

	if (distopt) {
		pos_dist = strtod(distopt, &fm);

		if ((*fm == 'm') || (*fm == 'M')) {
			 /* distance is meters */
			pos_dist *= 3.2802;
		}
	}
}

void
position_deinit(void) {
}

void 
radius_process(void)
{
	queue * elem, * tmp;
	waypoint * waypointp;
	double dist;
	waypoint ** comp;
	int i, wc;
	queue temp_head;

	QUEUE_FOR_EACH(&waypt_head, elem, tmp) {
		extra_data *ed;

		waypointp = (waypoint *)elem;
		dist = gc_distance(waypointp->position.latitude.degrees,
				   waypointp->position.longitude.degrees,
				   home_pos->position.latitude.degrees,
				   home_pos->position.longitude.degrees);

		/* convert radians to float point statute miles */
		dist = (((dist * 180.0 * 60.0) / M_PI) * 1.1516);

		if ((dist >= pos_dist) == (exclopt == NULL)) {
			waypt_del(waypointp);
			waypt_free(waypointp);
			continue;
		}

		ed = xcalloc(1, sizeof(*ed));
		ed->distance = dist;
		waypointp->extra_data = ed;
	}

	wc = waypt_count();
	QUEUE_INIT(&temp_head);

	comp = xcalloc(wc, sizeof(*comp));

	i = 0;

	/*
	 * Create an array of remaining waypoints, popping them off the
	 * master queue as we go.   This gives us something reasonable 
	 * for qsort.
	 */

	QUEUE_FOR_EACH(&waypt_head, elem, tmp) {
		waypoint *wp = (waypoint *) elem;
		comp[i] = wp;
		waypt_del(wp);
		i++;
	}

	qsort(comp, wc, sizeof(waypoint *), dist_comp);

	/*
	 * The comp array is now sorted by distance.   As we run through it,
	 * we push them back onto the master wp list, letting us pass them
	 * on through in the modified order.
 	 */
	for (i = 0; i < wc; i++) {
		waypoint * wp = comp[i];
		waypt_add(wp);
		xfree(wp->extra_data);
	}

	free(comp);
}

void
radius_init(const char *args) {
	char *fm;

	pos_dist = 0;

	if (distopt) {
		pos_dist = strtod(distopt, &fm);

		if ((*fm == 'k') || (*fm == 'K')) {
			 /* distance is kilometers, convert to feet */
			pos_dist *= .6214;
		}
	}

	home_pos = xcalloc(sizeof(*home_pos), 1);

	if (latopt)
		home_pos->position.latitude.degrees = atof(latopt);
	if (lonopt)
		home_pos->position.longitude.degrees = atof(lonopt);
}

void
radius_deinit(void) {
	if (home_pos)
		xfree(home_pos);
}

filter_vecs_t position_vecs = {
	position_init,
	position_process,
	position_deinit,
	position_args
};

filter_vecs_t radius_vecs = {
	radius_init,
	radius_process,
	radius_deinit,
	radius_args
};
