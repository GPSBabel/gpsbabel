/*
    Perform various operations on waypoints.

    Copyright (C) 2002-2007 Robert Lipe, robertlipe@usa.net

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
#include "cet_util.h"

queue waypt_head;
static unsigned int waypt_ct;
static short_handle mkshort_handle;

void
waypt_init(void)
{
	mkshort_handle = mkshort_new_handle();
	QUEUE_INIT(&waypt_head);
}

waypoint *
waypt_dupe(const waypoint *wpt) 
{
	/*
	 * This and waypt_free should be closely synced.
	 */
	waypoint * tmp;
	url_link *url_next;

	tmp = waypt_new();
	memcpy(tmp, wpt, sizeof(waypoint));

	if (wpt->shortname)
		tmp->shortname = xstrdup(wpt->shortname);
	if (wpt->description)
		tmp->description = xstrdup(wpt->description);
	if (wpt->notes)
		tmp->notes = xstrdup(wpt->notes);
	if (wpt->url)
		tmp->url = xstrdup(wpt->url);
	if (wpt->url_link_text)
		tmp->url_link_text = xstrdup(wpt->url_link_text);
	for (url_next = wpt->url_next; url_next; url_next = url_next->url_next) {
		waypt_add_url(tmp,
			(url_next->url) ? xstrdup(url_next->url) : NULL,
			(url_next->url_link_text) ? xstrdup(url_next->url_link_text) : NULL);
	}
	if (wpt->icon_descr && wpt->wpt_flags.icon_descr_is_dynamic)
		tmp->icon_descr = xstrdup(wpt->icon_descr);
	if (wpt->gc_data.desc_short.utfstring) {
		tmp->gc_data.desc_short.utfstring = 
			xstrdup(wpt->gc_data.desc_short.utfstring);
	}
	if (wpt->gc_data.desc_long.utfstring) {
		tmp->gc_data.desc_long.utfstring = 
			xstrdup(wpt->gc_data.desc_long.utfstring);
	}
	if (wpt->gc_data.placer) {
                tmp->gc_data.placer = xstrdup(wpt->gc_data.placer);
        }
	if (wpt->gc_data.hint) {
                tmp->gc_data.hint = xstrdup(wpt->gc_data.hint);
        }

	/*
	 * It's important that this duplicated waypoint not appear
	 * on the master Q.
	 */
	QUEUE_INIT(&tmp->Q);
	tmp->fs = fs_chain_copy( wpt->fs );

	return tmp;
}

void
waypt_add(waypoint *wpt)
{
	ENQUEUE_TAIL(&waypt_head, &wpt->Q);
	waypt_ct++;

	if ((wpt->latitude < -90) || (wpt->latitude > 90.0))
		fatal ("Invalid latitude %f in waypoint.\n", wpt->latitude);
	if ((wpt->longitude < -180) || (wpt->longitude > 180.0))
		fatal ("Invalid longitude %f in waypoint.\n", wpt->latitude);

	/*
	 * Some input may not have one or more of these types so we
	 * try to be sure that we have these fields even if just by
	 * copying them from elsewhere.
	 */
	if (wpt->shortname == NULL) {
		if (wpt->description) {
			wpt->shortname = xstrdup(wpt->description);
		} else if (wpt->notes) {
			wpt->shortname = xstrdup(wpt->notes);
		} else {
		/* Last ditch:  make up a name */
			char cbuf[10];
			snprintf(cbuf, sizeof(cbuf), "WPT%03d", waypt_ct);
			wpt->shortname = xstrdup(cbuf);
		}
	}

	if (wpt->description == NULL || strlen(wpt->description) == 0) {
		if (wpt->description)
			xfree(wpt->description);
		if (wpt->notes != NULL) {
			wpt->description = xstrdup(wpt->notes);
		} else  {
			if (wpt->shortname != NULL) {
				wpt->description = xstrdup(wpt->shortname);
			}
		}
	}
}

void
waypt_del(waypoint *wpt)
{
	dequeue(&wpt->Q);
	waypt_ct--;
}

/*
 * A constructor for a single waypoint.
 */
waypoint *
waypt_new(void)
{
	waypoint *wpt;

	wpt = (waypoint *) xcalloc(sizeof (*wpt), 1);
	wpt->altitude = unknown_alt;
	wpt->course = unknown_course;
	wpt->speed = unknown_speed;
	wpt->fix = fix_unknown;
	wpt->sat = -1;

	QUEUE_INIT(&wpt->Q);
	return wpt;
}

unsigned int
waypt_count(void)
{
	return waypt_ct;
}

void
set_waypt_count(unsigned int nc)
{
	waypt_ct = nc;
}

void
waypt_disp(const waypoint *wpt)
{
	char *tmpdesc = NULL;
	if (wpt->creation_time) {
		printf("%s ", ctime(&wpt->creation_time));
	}
	printposn(wpt->latitude,1);
	printposn(wpt->longitude,0);

	if ( wpt->description ) {	
		tmpdesc = xstrdup( wpt->description);
		printf("%s/%s", 
			global_opts.synthesize_shortnames ? 
			mkshort(mkshort_handle, tmpdesc) : 
				wpt->shortname, 
				tmpdesc);
		if ( tmpdesc ) 
			xfree(tmpdesc);
	}

	if (wpt->altitude != unknown_alt)
		printf(" %f", wpt->altitude);
	printf("\n");
}

void
waypt_status_disp(int total_ct, int myct)
{
	fprintf(stdout, "%d/%d/%d\r", myct*100/total_ct, myct, total_ct);
	fflush(stdout);
}

void
waypt_disp_all(waypt_cb cb)
{
	queue *elem, *tmp;
	waypoint *waypointp;
	int i = 0;

	QUEUE_FOR_EACH(&waypt_head, elem, tmp) {
		waypointp = (waypoint *) elem;
		if (global_opts.verbose_status) {
			i++;
			waypt_status_disp(waypt_ct, i);
		}
		(*cb) (waypointp);
	}
	if (global_opts.verbose_status) {
		fprintf(stdout, "\r\n");
	}
}

void
waypt_init_bounds(bounds *bounds)
{
	/* Set data out of bounds so that even one waypoint will reset */
	bounds->max_lat = -9999;
	bounds->max_lon = -9999;
	bounds->min_lat = 9999;
	bounds->min_lon = 9999;
	bounds->max_alt = -unknown_alt;
	bounds->min_alt = unknown_alt;
}

int
waypt_bounds_valid(bounds *bounds)
{
	/* Returns true if bb has any 'real' data in it */
	return bounds->max_lat > -9999;
}

/*
 * Recompund bounding box based on new position point.
 */
void 
waypt_add_to_bounds(bounds *bounds, const waypoint *waypointp)
{
	if (waypointp->latitude > bounds->max_lat)
		bounds->max_lat = waypointp->latitude;
	if (waypointp->longitude > bounds->max_lon)
		bounds->max_lon = waypointp->longitude;
	if (waypointp->latitude < bounds->min_lat)
		bounds->min_lat = waypointp->latitude;
	if (waypointp->longitude < bounds->min_lon)
		bounds->min_lon = waypointp->longitude;
	if (waypointp->altitude != unknown_alt) {
		if (waypointp->altitude < bounds->min_alt)
			bounds->min_alt = waypointp->altitude;
		if (waypointp->altitude > bounds->max_alt)
			bounds->max_alt = waypointp->altitude;
	}
}


/*
 *  Makes another pass over the data to compute bounding
 *  box data and populates bounding box information.
 */

void
waypt_compute_bounds(bounds *bounds)
{
	queue *elem, *tmp;
	waypoint *waypointp;

	waypt_init_bounds(bounds);

	QUEUE_FOR_EACH(&waypt_head, elem, tmp) {
		waypointp = (waypoint *) elem;
		waypt_add_to_bounds(bounds, waypointp);
	}
}

waypoint *
find_waypt_by_name(const char *name)
{
	queue *elem, *tmp;
	waypoint *waypointp;

	QUEUE_FOR_EACH(&waypt_head, elem, tmp) {
		waypointp = (waypoint *) elem;
		if (0 == strcmp(waypointp->shortname, name)) {
			return waypointp;
		}
	}

	return NULL;
}

void 
waypt_free( waypoint *wpt )
{
	/*
	 * This and waypt_dupe should be closely synced.
	 */
	if (wpt->shortname) {
		xfree(wpt->shortname);
	}
	if (wpt->description) {
		xfree(wpt->description);
	}
	if (wpt->notes) {
		xfree(wpt->notes);
	}
	if (wpt->url) {
		xfree(wpt->url);
	}
	if (wpt->url_link_text) {
		xfree(wpt->url_link_text);
	}
	if (wpt->url_next) {
		url_link *url_next;
		
		for (url_next = wpt->url_next; url_next; ) {
	
			url_link *tonuke = url_next;
			if (tonuke->url) {
				xfree(tonuke->url);
			}
			if (tonuke->url_link_text) {
				xfree(tonuke->url_link_text);
			}
			url_next = tonuke->url_next;
			xfree(tonuke);
		}
	}
	if (wpt->icon_descr && wpt->wpt_flags.icon_descr_is_dynamic) {
		xfree((char *)(void *)wpt->icon_descr);
	}
	if (wpt->gc_data.desc_short.utfstring) {
		xfree(wpt->gc_data.desc_short.utfstring);
	}
	if (wpt->gc_data.desc_long.utfstring) {
		xfree(wpt->gc_data.desc_long.utfstring);
	}
	if (wpt->gc_data.placer) {
		xfree(wpt->gc_data.placer);
	}
	if (wpt->gc_data.hint) {
		xfree (wpt->gc_data.hint);
	} 
	fs_chain_destroy( wpt->fs );
	xfree(wpt);	
}

void 
waypt_flush( queue *head )
{
	queue *elem, *tmp;

	QUEUE_FOR_EACH(head, elem, tmp) {
		waypoint *q = (waypoint *) dequeue(elem);
		waypt_free(q);
		if (head == &waypt_head) {
			waypt_ct--;
		}
	}
}

void
waypt_flush_all()
{
	if ( mkshort_handle ) {
		mkshort_del_handle( &mkshort_handle );
	}
	waypt_flush(&waypt_head);
}

void
waypt_backup(signed int *count, queue **head_bak)
{
	queue *elem, *tmp, *qbackup;
	waypoint *wpt;
	int no = 0;

	qbackup = (queue *) xcalloc(1, sizeof(*qbackup));
	QUEUE_INIT(qbackup);
	
	QUEUE_MOVE(qbackup, &waypt_head);
	QUEUE_INIT(&waypt_head);

	waypt_ct = 0;

	QUEUE_FOR_EACH(qbackup, elem, tmp)
	{
	    wpt = (waypoint *)elem;
	    waypt_add(waypt_dupe(wpt));
	    no++;
	}

	*head_bak = qbackup;
	*count = no;
}

void
waypt_restore(signed int count, queue *head_bak)
{
	if (head_bak == NULL) return;
	
	waypt_flush(&waypt_head);
	QUEUE_INIT(&waypt_head);
	QUEUE_MOVE(&waypt_head, head_bak);
	waypt_ct = count;
	xfree(head_bak);
}

void
waypt_add_url(waypoint *wpt, char *link, char *url_link_text)
{
	if ((link == NULL) && (url_link_text == NULL)) return;
	
	/* Special case first one; it goes right into the waypoint. */
	if ((wpt->url == NULL)  && (wpt->url_link_text == NULL)) {
		wpt->url = link;
		wpt->url_link_text = url_link_text;
	} else {
		url_link *tail;
		url_link *new_link = xcalloc(sizeof(url_link), 1);
		new_link->url = link;
		new_link->url_link_text = url_link_text;

		/* Find current end of chain and tack this onto the end.. */
		for (tail = wpt->url_next;;tail = tail->url_next) {
			if (tail == NULL) {
				wpt->url_next = new_link;
				break;
			}
			if (tail->url_next == NULL) {
				tail->url_next = new_link;
				break;
			}
		}
	}
}
