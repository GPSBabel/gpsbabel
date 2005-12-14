/*

    Convert data between waypoints, tracks, and routes.   
    
    Copyright (C) 2005 Robert Lipe   <robertlipe@usa.net>

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

#define MYNAME "cvttype"

static char *action;
static route_head *my_trk_head;

static
arglist_t cvttype_skeleton_args[] = {
	{"action", &action, "Type of conversion to perform", "wpttotrk", 
		ARGTYPE_STRING} , 
	{0, 0, 0, 0, 0}
};

static void
cvttype_skeleton_init(const char *args) 	
{
	my_trk_head = route_head_alloc();
// route or track
	track_add_head(my_trk_head);
}

static void 
cvttype_skeleton_process(void)
{
	queue *elem, *tmp;
        waypoint *wpt;

	QUEUE_FOR_EACH(&waypt_head, elem, tmp) {
		waypoint *wpt = (waypoint *)elem;

// if action == to trk
		waypt_del(wpt);
		route_add_wpt(my_trk_head, wpt);
// if action == to rte
//		waypt_del(wpt);
//		route_add_wpt(my_rte_head, wpt);
			
	}
}

static void
cvttype_skeleton_deinit(void)
{
}

filter_vecs_t cvttype_vecs = {
	cvttype_skeleton_init,
	cvttype_skeleton_process,
	cvttype_skeleton_deinit,
	NULL,
	cvttype_skeleton_args
};
