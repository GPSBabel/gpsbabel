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

void
route_init(void)
{
	QUEUE_INIT(&my_route_head);
}


route_head *
route_head_alloc(void)
{
	route_head *rte_head;
	rte_head = xmalloc(sizeof (*rte_head));
	QUEUE_INIT(&rte_head->Q);

}


void
route_add_head(route_head *rte)
{
	ENQUEUE_TAIL(&my_route_head, &rte->Q);
	QUEUE_INIT(&rte->waypoint_list);
}

void
route_add_wpt(route_head *rte, waypoint *wpt)
{
	ENQUEUE_TAIL(&rte->waypoint_list, &wpt->Q);
}

void
route_disp (route_head *rh)
{
	queue *elem, *tmp;
	printf("NEW ROUTE\n");
	QUEUE_FOR_EACH(&rh->waypoint_list, elem, tmp) {
		waypoint *waypointp;
		waypointp = (waypoint *) elem;
			waypt_disp(waypointp);
	}
		
}	
void 
route_disp_all()
{
	queue *elem, *tmp;
	QUEUE_FOR_EACH(&my_route_head, elem, tmp) {
		route_head *rh;
		rh = (route_head *) elem;
		route_disp(rh);
	}
}
