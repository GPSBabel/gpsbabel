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

static queue route_head;

void
route_init(void)
{
	QUEUE_INIT(&route_head);
}

void
route_add(waypoint *wpt)
{
	ENQUEUE_TAIL(&route_head, &wpt->Q);
}

void
route_disp_all(waypt_cb cb)
{
	queue *elem, *tmp;
	waypoint *waypointp;

	QUEUE_FOR_EACH(&route_head, elem, tmp) {
		waypointp = (waypoint *) elem;
		(*cb)(waypointp);
	}
}
