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
static int rte_head_ct;
static int rte_waypts;

void
route_init(void)
{
	QUEUE_INIT(&my_route_head);
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
route_add_wpt(route_head *rte, waypoint *wpt)
{
	ENQUEUE_TAIL(&rte->waypoint_list, &wpt->Q);
	rte->rte_waypt_ct++;	/* waypoints in this route */
	rte_waypts++;		/* total waypoints in all routes */
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
	waypt_flush(&rte->waypoint_list);
	xfree(rte);
}

void
route_disp (const route_head *rh, waypt_cb cb )
{
	queue *elem, *tmp;
	QUEUE_FOR_EACH(&rh->waypoint_list, elem, tmp) {
		waypoint *waypointp;
		waypointp = (waypoint *) elem;
			(*cb)(waypointp);
	}
		
}	
void 
route_disp_all(route_hdr rh, route_trl rt, waypt_cb wc)
{
	queue *elem, *tmp;
	QUEUE_FOR_EACH(&my_route_head, elem, tmp) {
		const route_head *rhp;
		rhp = (route_head *) elem;
		(*rh)(rhp);
		route_disp(rhp, wc);
		(*rt)(rhp);
	}
}
void
route_flush(queue *head)
{
	queue *elem, *tmp;
	route_head *last = NULL;
	
	QUEUE_FOR_EACH(head, elem, tmp) {
		if ( last ) {
			route_free(last);
		}
		last = (route_head *)elem;
	}
	if ( last ) {
		route_free(last);
	}
	QUEUE_INIT(head);
}
void
route_flush_all()
{
	route_flush(&my_route_head);
}

