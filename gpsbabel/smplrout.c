/*
   Route simplification filter 

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

/* The following comments are from an email I wrote to Paul Fox in November
 * 2005 in an attempt to explain how the cross track error minimization method
 * works  (RLP 2005):
 * 
 * It's pretty simple, really: for each triplet of vertices A-B-C, we compute 
 * how much cross-track error we'd introduce by going straight from A to C 
 * (the maximum cross-track error for that segment is the height of the 
 * triangle ABC, measured between vertex B and edge AC.)  If we need to remove 
 * 40 points, we just sort the points by that metric and remove the 40 
 * smallest ones.
 * 
 * It's actually a little more complicated than that, because removing a 
 * point changes the result for its two nearest neighbors.  When we remove 
 * one, we recompute the neighbors and then sort them back into the list 
 * at their new locations.
 * 
 * As you can see, this hasn't been shown to be an optimal algorithm.  After 
 * all, removing one high-xte point might create two very low-xte neighbors 
 * that more than make up for the high xte of the original point.  I believe 
 * the optimal algorithm would be NP-complete, but I haven't proven it.  This 
 * is really more of a heuristic than anything, but it seems to work well for 
 * the routes I've fed it.
 *
 * Not in that email was an explanation of how the pathlength-based calculation
 * works: instead of computing the height of the triangle, we just compute
 * the difference in pathlength from taking the direct route.  This case,
 * too, is only a heuristic, as it's possible that a different combination or
 * order of point removals could lead to a smaller number of points with less
 * reduction in path length.  In the case of pathlength, error is cumulative.
 */


#include "defs.h"
#include "filterdefs.h"
#include "grtcirc.h"

#define MYNAME "Route simplification filter"

static int count = 0;
static double totalerror = 0;
static double error = 0;

static char *countopt = NULL;
static char *erroropt = NULL;
static char *xteopt = NULL;
static char *lenopt = NULL;
void (*waypt_del_fnp) (route_head *rte, waypoint *wpt);

static
arglist_t routesimple_args[] = {
	{"count", &countopt,  "Maximum number of points in route", 
		NULL, ARGTYPE_INT | ARGTYPE_BEGIN_REQ | ARGTYPE_BEGIN_EXCL, "1", NULL},
	{"error", &erroropt, "Maximum error", NULL,
		ARGTYPE_STRING | ARGTYPE_END_REQ | ARGTYPE_END_EXCL, "0", NULL},
	{"crosstrack", &xteopt, "Use cross-track error (default)", NULL, 
		ARGTYPE_BOOL | ARGTYPE_BEGIN_EXCL, ARG_NOMINMAX },
	{"length", &lenopt, "Use arclength error", NULL, 
		ARGTYPE_BOOL | ARGTYPE_END_EXCL, ARG_NOMINMAX },
	ARG_TERMINATOR
};

struct xte_intermed;

struct xte {
	double distance;
	int ordinal;
	struct xte_intermed *intermed;
};

struct xte_intermed {
	struct xte *xte_rec;
	struct xte_intermed *next;
	struct xte_intermed *prev;
	const waypoint *wpt;
};

void
free_xte( struct xte *xte_rec ) 
{
	xfree(xte_rec->intermed);
}

#define HUGEVAL 2000000000

static struct xte_intermed *tmpprev = NULL;
static int xte_count = 0;
static const route_head *cur_rte = NULL;
static struct xte *xte_recs = NULL;

void 
routesimple_waypt_pr( const waypoint *wpt ) 
{
	if ( !cur_rte ) return;
	xte_recs[xte_count].ordinal=xte_count;
	xte_recs[xte_count].intermed = (struct xte_intermed *) xmalloc( sizeof(struct xte_intermed));
	xte_recs[xte_count].intermed->wpt = wpt;
	xte_recs[xte_count].intermed->xte_rec = xte_recs+xte_count;
	xte_recs[xte_count].intermed->next = NULL;
	xte_recs[xte_count].intermed->prev = tmpprev;
	if ( tmpprev ) {
		tmpprev->next = xte_recs[xte_count].intermed;
	}	
	tmpprev = xte_recs[xte_count].intermed;
	xte_count++;
}

void
compute_xte( struct xte *xte_rec ) {
	const waypoint *wpt3 = xte_rec->intermed->wpt;
	const waypoint *wpt1 = NULL;
	const waypoint *wpt2 = NULL;
	/* if no previous, this is an endpoint and must be preserved. */
	if ( !xte_rec->intermed->prev ) {
		xte_rec->distance = HUGEVAL;
		return;
	}
	wpt1 = xte_rec->intermed->prev->wpt;
	
	/* if no next, this is an endpoint and must be preserved. */
	if ( !xte_rec->intermed->next ) {
		xte_rec->distance = HUGEVAL;
		return;
	}
	wpt2 = xte_rec->intermed->next->wpt;
	
	if ( xteopt || !lenopt ) {
		xte_rec->distance = radtomiles(linedist( 
			wpt1->latitude, wpt1->longitude, 
			wpt2->latitude, wpt2->longitude,
			wpt3->latitude, wpt3->longitude ));
	} 
	else {
		xte_rec->distance = radtomiles( 
		       gcdist( wpt1->latitude, wpt1->longitude, 
			       wpt3->latitude, wpt3->longitude ) +
		       gcdist( wpt3->latitude, wpt3->longitude,
			       wpt2->latitude, wpt2->longitude ) -
		       gcdist( wpt1->latitude, wpt1->longitude,
			       wpt2->latitude, wpt2->longitude ));
	}
}


int
compare_xte( const void *a, const void *b )
{
	double distdiff = ((struct xte *)a)->distance - 
		          ((struct xte *)b)->distance;
	int priodiff = ((struct xte *)a)->intermed->wpt->route_priority -
		       ((struct xte *)b)->intermed->wpt->route_priority;
	
	if (HUGEVAL == ((struct xte *)a)->distance)
		return -1;
	
	if (HUGEVAL == ((struct xte *)b)->distance)
		return 1;

	if ( priodiff < 0 ) return 1;
	if ( priodiff > 0 ) return -1;
	if ( distdiff < 0 ) return 1;
	if ( distdiff > 0 ) return -1;
	return 0;
}

void 
routesimple_head( const route_head *rte ) 
{
	cur_rte = NULL;
   	/* build array of XTE/wpt xref records */
        xte_count = 0;
	tmpprev = NULL;
	totalerror = 0;
	
	/* short-circuit if we already have fewer than the max points */
	if ( countopt && count >= rte->rte_waypt_ct) return;
	
	/* short-circuit if the route is impossible to simplify, too. */
	if ( 2 >= rte->rte_waypt_ct ) return;
	
	xte_recs = (struct xte *) xcalloc(  rte->rte_waypt_ct, sizeof (struct xte));
	cur_rte = rte;
	
}

void
shuffle_xte( struct xte *xte_rec )
{
	struct xte tmp_xte;
	while ( xte_rec > xte_recs && compare_xte(xte_rec, xte_rec-1) < 0 ) {
		tmp_xte.distance = xte_rec->distance;
		tmp_xte.ordinal = xte_rec->ordinal;
		tmp_xte.intermed = xte_rec->intermed;
		xte_rec->distance = xte_rec[-1].distance;
		xte_rec->ordinal = xte_rec[-1].ordinal;
		xte_rec->intermed = xte_rec[-1].intermed;
		xte_rec->intermed->xte_rec = xte_rec;
		xte_rec--;
		xte_rec->distance = tmp_xte.distance;
		xte_rec->ordinal = tmp_xte.ordinal;
		xte_rec->intermed = tmp_xte.intermed;
		xte_rec->intermed->xte_rec = xte_rec;
	}
	while ( xte_rec - xte_recs < xte_count-2 && 
			compare_xte( xte_rec, xte_rec+1) > 0 ) {
		tmp_xte.distance = xte_rec->distance;
		tmp_xte.ordinal = xte_rec->ordinal;
		tmp_xte.intermed = xte_rec->intermed;
		xte_rec->distance = xte_rec[1].distance;
		xte_rec->ordinal = xte_rec[1].ordinal;
		xte_rec->intermed = xte_rec[1].intermed;
		xte_rec->intermed->xte_rec = xte_rec;
		xte_rec++;
		xte_rec->distance = tmp_xte.distance;
		xte_rec->ordinal = tmp_xte.ordinal;
		xte_rec->intermed = tmp_xte.intermed;
		xte_rec->intermed->xte_rec = xte_rec;
	}	
}

void 
routesimple_tail( const route_head *rte )
{
	int i;
	if ( !cur_rte ) return;
	
	/* compute all distances */
	for (i = 0; i < xte_count ; i++ ) {
		compute_xte( xte_recs+i );
	}

	
	/* sort XTE array, lowest XTE last */
	qsort( xte_recs, xte_count, sizeof( struct xte ), compare_xte );
	
	for (i = 0; i < xte_count; i++ ) {
		xte_recs[i].intermed->xte_rec = xte_recs+i;
	}
	/* while we still have too many records... */
	while ( (countopt && count < xte_count) || (erroropt && totalerror < error) ) {
		i = xte_count - 1;
		/* remove the record with the lowest XTE */
		if ( erroropt ) {
			if ( xteopt ) {
				if ( i > 1 ) {
					totalerror = xte_recs[i-1].distance;
				}
				else {
					totalerror = xte_recs[i].distance;
				}
			}
			if ( lenopt ) {
				totalerror += xte_recs[i].distance;
			}
		}
		(*waypt_del_fnp)( (route_head *)(void *)rte,
				(waypoint *)(void *)(xte_recs[i].intermed->wpt));
		waypt_free((waypoint *)(void *)(xte_recs[i].intermed->wpt));
              	
		if ( xte_recs[i].intermed->prev ) {
			xte_recs[i].intermed->prev->next = xte_recs[i].intermed->next;
			compute_xte(xte_recs[i].intermed->prev->xte_rec);
			shuffle_xte(xte_recs[i].intermed->prev->xte_rec);
		}
		if ( xte_recs[i].intermed->next ) {
			xte_recs[i].intermed->next->prev = xte_recs[i].intermed->prev;
			compute_xte(xte_recs[i].intermed->next->xte_rec);
			shuffle_xte(xte_recs[i].intermed->next->xte_rec);
		}
		xte_count--;
		free_xte( xte_recs+xte_count);
	/* end of loop */
	}
	if ( xte_count ) {
		do {
			xte_count--;
			free_xte( xte_recs+xte_count );
		} while(xte_count);
	}
	xfree( xte_recs );
}

void 
routesimple_process( void ) 
{
	waypt_del_fnp = route_del_wpt;
	route_disp_all( routesimple_head, routesimple_tail, routesimple_waypt_pr );

	waypt_del_fnp = track_del_wpt;
	track_disp_all( routesimple_head, routesimple_tail, routesimple_waypt_pr );
}

void
routesimple_init(const char *args) {
	char *fm = NULL;
	count = 0;

	if ( !!countopt == !!erroropt ) {
		fatal( MYNAME ": You must specify either count or error, but not both.\n");
	}
	if ( xteopt && lenopt ) {
		fatal( MYNAME ": crosstrack and length may not be used together.\n");
	}
	if ( !xteopt && !lenopt ) {
		xteopt = (char *)xmalloc( 1 );
	}
		
	if (countopt) {
		count = atol(countopt);
	}
	if (erroropt) {
               error = strtod(erroropt, &fm);

               if ((*fm == 'k') || (*fm == 'K')) {
	            /* distance is kilometers, convert to miles */
	            error *= .6214;
	       }
	}
}

void
routesimple_deinit(void) {
	/* do nothing */
}

filter_vecs_t routesimple_vecs = {
	routesimple_init,
	routesimple_process,
	routesimple_deinit,
	NULL,
	routesimple_args
};
