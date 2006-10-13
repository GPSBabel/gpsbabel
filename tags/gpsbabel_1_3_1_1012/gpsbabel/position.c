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
#include "defs.h"
#include "filterdefs.h"
#include "grtcirc.h"

#if FILTERS_ENABLED

#ifndef M_PI
#  define M_PI 3.14159265358979323846
#endif

static route_head *cur_rte = NULL;

static double pos_dist;
static char *distopt = NULL;
static char *purge_duplicates = NULL;

typedef struct {
	double distance;
} extra_data;

static
arglist_t position_args[] = {
	{"distance", &distopt, "Maximum positional distance",
		NULL, ARGTYPE_FLOAT | ARGTYPE_REQUIRED, ARG_NOMINMAX },
	{"all", &purge_duplicates, 
		"Suppress all points close to other points", 
		NULL, ARGTYPE_BOOL, ARG_NOMINMAX }, 
	ARG_TERMINATOR
};

static double
gc_distance(double lat1, double lon1, double lat2, double lon2)
{
	return gcdist( 
	    RAD(lat1),
	    RAD(lon1),
	    RAD(lat2),
	    RAD(lon2)
	    );
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

	londiff = (x1->longitude -
		   x2->longitude) * 1000000.0;
	latdiff = (x1->latitude -
		   x2->latitude) * 1000000.0;

	max = fabs(londiff) >= fabs(latdiff) ? floor(londiff) : floor(latdiff);

	if (max < 0)
		return (-1);
	else if (max > 0)
		return (1);

	return(0);
}

/* tear through a waypoint queue, processing points by distance */
static void 
position_runqueue(queue *q, int nelems, int qtype)
{
	queue * elem, * tmp;
	waypoint ** comp;
	double dist;
	int i, j;
	int del = 0;

	comp = (waypoint **) xcalloc(nelems, sizeof(*comp));

	i = 0;

	QUEUE_FOR_EACH(q, elem, tmp) {
		comp[i] = (waypoint *)elem;
		i++;
	}

	if (qtype == wptdata)
		qsort(comp, nelems, sizeof(waypoint *), position_comp);

	for (i = 1, j = 0 ; i < nelems ; i++) {
		dist = gc_distance(comp[j]->latitude,
				   comp[j]->longitude,
				   comp[i]->latitude,
				   comp[i]->longitude);

		/* convert radians to integer feet */
		dist = (int)(5280*radtomiles(dist));
		
		if (dist <= pos_dist) {
			switch (qtype) {
				case wptdata:
					waypt_del(comp[i]);
					waypt_free(comp[i]);
					del = !!purge_duplicates;
					break;
				case trkdata:
					track_del_wpt(cur_rte, comp[i]);
					del = !!purge_duplicates;
					break;
				case rtedata:
					route_del_wpt(cur_rte, comp[i]);
					del = !!purge_duplicates;
					break;
				default:
					break;
			}
		}
		else if (del ) {
			switch (qtype) {
				case wptdata:
					waypt_del(comp[i]);
					waypt_free(comp[i]);
					del = 0;
					break;
				case trkdata:
					track_del_wpt(cur_rte, comp[i]);
					del = !!purge_duplicates;
					break;
				case rtedata:
					route_del_wpt(cur_rte, comp[i]);
					del = 0;
					break;
				default:
					break;
			}
		} else {
			j = i; /* advance last use point */
		}
	}
	if ( del ) {
		switch (qtype) {
			case wptdata:
				waypt_del(comp[nelems-1]);
				waypt_free(comp[nelems-1]);
				break;
			case trkdata:
				track_del_wpt(cur_rte, comp[i]);
				break;
			case rtedata:
				route_del_wpt(cur_rte, comp[i]);
				break;
			default:
				break;
		}
	}

	if (comp)
		xfree(comp);
}

static void
position_process_route(const route_head * rh) { 
    int i = rh->rte_waypt_ct;

    if (i) {
    	cur_rte = (route_head *)rh;
        position_runqueue((queue *)&rh->waypoint_list, i, rtedata);
        cur_rte = NULL;
    }

}

static void 
position_noop_w(const waypoint *w)
{
}

static void 
position_noop_t(const route_head *h)
{
}

void position_process(void) 
{
	int i = waypt_count();
	
	if (i)
		position_runqueue(&waypt_head, i, wptdata);
	
	route_disp_all(position_process_route, position_noop_t, position_noop_w);
	track_disp_all(position_process_route, position_noop_t, position_noop_w);
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

filter_vecs_t position_vecs = {
	position_init,
	position_process,
	position_deinit,
	NULL,
	position_args
};

#endif // FILTERS_ENABLED
