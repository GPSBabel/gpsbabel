/* 
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

static queue my_route_head;
static queue my_track_head;
static int rte_head_ct;
static int rte_waypts;
static int trk_head_ct;

void
route_init(void)
{
	QUEUE_INIT(&my_route_head);
	QUEUE_INIT(&my_track_head);
}

unsigned int
route_waypt_count(void)
{
	/* total wapoint count -- all routes */
	return rte_waypts;
}

unsigned int
route_count(void)
{
	return rte_head_ct;	/* total # of routes */
}

unsigned int
track_count(void)
{
	return trk_head_ct;	/* total # of tracks */
}

route_head *
route_head_alloc(void)
{
	route_head *rte_head;
	rte_head = (route_head *) xcalloc(sizeof (*rte_head), 1);
	QUEUE_INIT(&rte_head->Q);
	return rte_head;
}


void
route_add_head(route_head *rte)
{
	ENQUEUE_TAIL(&my_route_head, &rte->Q);
	QUEUE_INIT(&rte->waypoint_list);
	rte_head_ct++;
}

void
route_del_head(route_head *rte)
{
	dequeue( &rte->Q );
	route_free( rte );
	rte_head_ct--;
}

void
track_add_head(route_head *rte)
{
	ENQUEUE_TAIL(&my_track_head, &rte->Q);
	QUEUE_INIT(&rte->waypoint_list);
	trk_head_ct++;
}

void
track_del_head(route_head *rte)
{
	dequeue( &rte->Q );
	route_free( rte );
	trk_head_ct--;
}

void
route_add_wpt(route_head *rte, waypoint *wpt)
{
	ENQUEUE_TAIL(&rte->waypoint_list, &wpt->Q);
	rte->rte_waypt_ct++;	/* waypoints in this route */
	rte_waypts++;		/* total waypoints in all routes */
	if (wpt->shortname == NULL) {
		char tmpnam[10];
		snprintf(tmpnam, sizeof(tmpnam), "RPT%03d",rte_waypts);
		wpt->shortname = xstrdup(tmpnam);
		wpt->shortname_is_synthetic = 1;
	}
}

void 
route_del_wpt( route_head *rte, waypoint *wpt)
{
	dequeue( &wpt->Q );
	waypt_free( wpt );
	rte->rte_waypt_ct--;
	rte_waypts--;
}

void
route_free(route_head *rte)
{
	if ( rte->rte_name ) {
		xfree(rte->rte_name);
	}
	if ( rte->rte_desc ) {
		xfree(rte->rte_desc);
	}
	rte_waypts -= rte->rte_waypt_ct;
	waypt_flush(&rte->waypoint_list);
	xfree(rte);
}

void
route_disp (const route_head *rh, waypt_cb cb )
{
	queue *elem, *tmp;
	if (!cb)  {
		return;
	}
	QUEUE_FOR_EACH(&rh->waypoint_list, elem, tmp) {
		waypoint *waypointp;
		waypointp = (waypoint *) elem;
			(*cb)(waypointp);
	}
		
}

void 
route_reverse(const route_head *rte_hd)
{
	/* Cast away const-ness */
	route_head *rh = (route_head *) rte_hd;
	queue *elem, *tmp;
	QUEUE_FOR_EACH(&rh->waypoint_list, elem, tmp) {
		ENQUEUE_HEAD(&rh->waypoint_list, dequeue(elem));
	}
}

void
common_disp_all(queue *qh, route_hdr rh, route_trl rt, waypt_cb wc)
{
	queue *elem, *tmp;
	QUEUE_FOR_EACH(qh, elem, tmp) {
		const route_head *rhp;
		rhp = (route_head *) elem;
		if (rh) (*rh)(rhp);
		route_disp(rhp, wc);
		if (rt) (*rt)(rhp);
	}
}

void 
route_disp_all(route_hdr rh, route_trl rt, waypt_cb wc)
{
	common_disp_all(&my_route_head, rh, rt, wc);
}

void 
track_disp_all(route_hdr rh, route_trl rt, waypt_cb wc)
{
	common_disp_all(&my_track_head, rh, rt, wc);
}

void
route_flush(queue *head)
{
	queue *elem, *tmp;
	queue *q;

	QUEUE_FOR_EACH(head, elem, tmp) {
		q = dequeue(elem);
		route_free((route_head *) q);
	}
}

void
route_flush_all()
{
	route_flush(&my_route_head);
	route_flush(&my_track_head);
}

