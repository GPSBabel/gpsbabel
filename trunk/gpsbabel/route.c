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

static 
route_head *
common_route_by_name(queue *routes, const char *name)
{
	queue *elem, *tmp;
	route_head *rte;

	QUEUE_FOR_EACH(routes, elem, tmp) {
		rte = (route_head *) elem;
		if (0 == strcmp(rte->rte_name, name)) {
			return rte;
		}
	}

	return NULL;
}

route_head *
route_find_route_by_name(const char *name)
{
	return common_route_by_name(&my_route_head, name);
}

route_head *
route_find_track_by_name(const char *name)
{
	return common_route_by_name(&my_track_head, name);
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
		wpt->wpt_flags.shortname_is_synthetic = 1;
	}
}

waypoint *
route_find_waypt_by_name( route_head *rh, const char *name )
{
	queue *elem, *tmp;

	QUEUE_FOR_EACH(&rh->waypoint_list, elem, tmp) {
		waypoint *waypointp = (waypoint *) elem;
		if (0 == strcmp(waypointp->shortname, name)) {
			return waypointp;
		}
	}
	return NULL;
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
	if ( rte->fs ) {
		fs_chain_destroy( rte->fs );
	}
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
	rte_head_ct = 0;
	trk_head_ct = 0;
	rte_waypts = 0;
}

void
route_backup(unsigned int *count, queue **head_bak)
{
	queue *elem, *tmp, *elem2, *tmp2;
	queue *qbackup;
	route_head *rte_new;
	unsigned int no;

	no = route_count();
	
	qbackup = xcalloc(1, sizeof(*qbackup));
	QUEUE_INIT(qbackup);
	
	QUEUE_MOVE(qbackup, &my_route_head);
	QUEUE_INIT(&my_route_head);
	
	rte_head_ct = 0;
	rte_waypts = 0;
	
	QUEUE_FOR_EACH(qbackup, elem, tmp)
	{
	    route_head *rte_old = (route_head *)elem;
	    
	    rte_new = route_head_alloc();
	    if (rte_old->rte_name != NULL)
		rte_new->rte_name = xstrdup(rte_old->rte_name);
	    if (rte_old->rte_desc != NULL)
		rte_new->rte_desc = xstrdup(rte_old->rte_desc);
	    if (rte_old->fs)
		rte_new->fs = fs_chain_copy(rte_old->fs);

	    route_add_head(rte_new);
	    rte_new->rte_num = rte_old->rte_num;
	    
	    QUEUE_FOR_EACH(&rte_old->waypoint_list, elem2, tmp2)
	    {
		waypoint *wpt = waypt_dupe((waypoint *)elem2);
		route_add_wpt(rte_new, wpt);
	    }
	}
	*head_bak = qbackup;
	*count = no;
}

static void
route_restore_hdr(const route_head *rte)
{
	rte_head_ct++;
}

static void
track_restore_hdr(const route_head *trk)
{
	trk_head_ct++;
}

static void
route_restore_tlr(const route_head *rte)
{
}

static void
route_restore_wpt(const waypoint *wpt)
{
	rte_waypts++;
}

void
common_restore_finish(void)
{
	rte_head_ct = 0;
	trk_head_ct = 0;
	rte_waypts = 0;
	route_disp_all(route_restore_hdr, route_restore_tlr, route_restore_wpt);
	track_disp_all(track_restore_hdr, route_restore_tlr, route_restore_wpt);
}

void
route_restore(unsigned int count, queue *head_bak)
{
	if (head_bak == NULL) return;
	
	route_flush(&my_route_head);
	QUEUE_INIT(&my_route_head);
	QUEUE_MOVE(&my_route_head, head_bak);
	xfree(head_bak);
	
	common_restore_finish();
}

void
track_backup(unsigned int *count, queue **head_bak)
{
	queue *elem, *tmp, *elem2, *tmp2;
	queue *qbackup;
	route_head *trk_new;
	unsigned int no;
	
	no = track_count();
	
	qbackup = xcalloc(1, sizeof(*qbackup));
	QUEUE_INIT(qbackup);
	
	QUEUE_MOVE(qbackup, &my_track_head);
	QUEUE_INIT(&my_track_head);
	
	trk_head_ct = 0;
	
	QUEUE_FOR_EACH(qbackup, elem, tmp)
	{
	    route_head *trk_old = (route_head *)elem;
	    
	    trk_new = route_head_alloc();
	    if (trk_old->rte_name != NULL)
		trk_new->rte_name = xstrdup(trk_old->rte_name);
	    if (trk_old->rte_desc != NULL)
		trk_new->rte_desc = xstrdup(trk_old->rte_desc);
	    if (trk_old->fs)
		trk_new->fs = fs_chain_copy(trk_old->fs);

	    track_add_head(trk_new);
	    trk_new->rte_num = trk_old->rte_num;
	    
	    QUEUE_FOR_EACH(&trk_old->waypoint_list, elem2, tmp2)
	    {
		waypoint *wpt = waypt_dupe((waypoint *)elem2);
		route_add_wpt(trk_new, wpt);
	    }
	}
	*head_bak = qbackup;
	*count = no;
}

void
track_restore(unsigned int count, queue *head_bak)
{
	if (head_bak == NULL) return;
	
	route_flush(&my_track_head);
	QUEUE_INIT(&my_track_head);
	QUEUE_MOVE(&my_track_head, head_bak);
	xfree(head_bak);
	
	common_restore_finish();
}
