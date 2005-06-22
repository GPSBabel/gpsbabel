/*
    Track manipulation filter

    Copyright (C) 2005 Olaf Klein, o.b.klein@t-online.de

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
#include <time.h>
#include "defs.h"

#define MYNAME "tracks"

#undef TRACKF_DBG

static char *opt_pack = NULL;
static char *opt_split = NULL;
static char *opt_title = NULL;

static
arglist_t trackfilter_args[] = {
	{"pack", &opt_pack, "Pack all tracks into one", NULL, ARGTYPE_BOOL},
	{"split", &opt_split, "Split track by day", NULL, ARGTYPE_BOOL},
	{"title", &opt_title, "Basic title for new track(s)", NULL, ARGTYPE_STRING},
	{0, 0, 0, 0, 0}
};

typedef struct trkflt
{
	route_head *track;
	time_t first_time;
	time_t last_time;
} trkflt_t;

static trkflt_t *track_list = NULL;
static int track_ct = 0;

/*- dummy callbacks for track_disp_all ---------------------------------------------------*/

static void 
trackfilter_noop_w(const waypoint *w)
{
}

static void 
trackfilter_noop_t(const route_head *h)
{
}

/*----------------------------------------------------------------------------------------*/

static int
trackfilter_qsort_cb(const void *a, const void *b)
{
	const trkflt_t *ra = a;
	const trkflt_t *rb = b;

	return ra->first_time - rb->first_time;
}

/*----------------------------------------------------------------------------------------*/

static void
trackfilter_fill_track_list_cb(const route_head *trk) 	/* callback for track_disp_all */
{
	int i;
	waypoint *wpt, *prev;
	queue *elem, *tmp;
	
	track_list[track_ct].track = (route_head *)trk;

	i = 0;
	prev = NULL;
	
	QUEUE_FOR_EACH((queue *)&trk->waypoint_list, elem, tmp)
	{
	    wpt = (waypoint *)elem;
	    if (wpt->creation_time == 0) fatal(MYNAME ": Found track point without time!\n");
	    i++;
	    if (i == 1) track_list[track_ct].first_time = wpt->creation_time;
	    else if (i == trk->rte_waypt_ct) track_list[track_ct].last_time = wpt->creation_time;
	    if ((prev != NULL) && (prev->creation_time > wpt->creation_time))
		fatal(MYNAME ": Track points bad ordered (timestamp)!\n");
	    prev = wpt;
	}
	track_ct++;
}

/*- global callbacks ---------------------------------------------------------------------*/

static void
trackfilter_init(const char *args) 
{
	int i, j;
	int count = track_count();
	trkflt_t prev;
	
#ifdef TRACKF_DBG
	printf(MYNAME ": init...\n");
#endif
	if (count > 0)
	{
	    track_list = (trkflt_t *) xcalloc(count, sizeof(*track_list));

	    /* check all tracks for time and order */
	
	    track_ct = 0;
	    track_disp_all(trackfilter_fill_track_list_cb, trackfilter_noop_t, trackfilter_noop_w);
	    qsort(track_list, track_ct, sizeof(*track_list), trackfilter_qsort_cb);

	    for (i=1, j=0; i<track_ct; i++, j++)
	    {
		prev = track_list[j];
		if (prev.last_time >= track_list[i].first_time) fatal(MYNAME " Tracks overlaps in time!\n");
	    }
	}
}

static void
trackfilter_deinit(void) 
{
#ifdef TRACKF_DBG
	printf(MYNAME ": deinit...\n");
#endif
	if (track_list != NULL)
	{
	    xfree(track_list);
	    track_list = NULL;
	}
}

#if 0
static int 
compare_wpt_time(const void *a, const void *b)
{
	const waypoint *pa = a;
	const waypoint *pb = b;

	if (pa->creation_time < pb->creation_time ) return -1;
	else if (pa->creation_time > pb->creation_time ) return +1;
	else return 0;
}
#endif

static void
trackfilter_init_rte_name(route_head *track, const time_t time)
{
	char buff[128], tbuff[128];
	struct tm tm;
	
	tm = *localtime(&time);
	
	strftime(tbuff, sizeof(tbuff), "%Y%m%d", &tm);
	
	if (opt_title != NULL) 
	{
	    if (strchr(opt_title, '%') != NULL)
		strftime(buff, sizeof(buff), opt_title, &tm);
	    else
		snprintf(buff, sizeof(buff), "%s-%s", opt_title, tbuff);
	}
	else
	{
	    snprintf(buff, sizeof(buff), "%s-%s", track->rte_name, tbuff);
	}
	
	if (track->rte_name != NULL) xfree(track->rte_name);
	track->rte_name = xstrdup(buff);
	
}

/*******************************************************************************************
*
* option "pack" (default)
* 
*******************************************************************************************/

static void
trackfilter_pack(void)
{
	int i, j, ct;
	route_head *master, *curr;
	queue *elem, *tmp;
	waypoint *wpt;
	waypoint **buff;
	
#ifdef TRACKF_DBG
	printf(MYNAME ": packing tracks...\n");
#endif
	/* we fill up the first track by all others */
	
	master = track_list[0].track;
	
	if (opt_title != NULL)
	{
	    if (master->rte_name != NULL)
		xfree(master->rte_name);
	    master->rte_name = xstrdup(opt_title);
	}
	
	for (i=1; i<track_ct; i++)
	{
	    curr = track_list[i].track;
	    
	    ct = curr->rte_waypt_ct;
	    buff = (waypoint **)xcalloc(ct, sizeof(*buff));
	    
	    j = 0;
	    QUEUE_FOR_EACH((queue *)&curr->waypoint_list, elem, tmp)
	    {
		wpt = (waypoint *)elem;
		buff[j] = wpt;
		j++;
	    }

	    for (j=0; j<ct; j++)
	    {
		wpt = waypt_dupe(buff[j]);
		route_del_wpt(curr, buff[j]);
		route_add_wpt(master, wpt);
	    }
	    
	    xfree(buff);
	    
	    track_del_head(curr);
	}
}

/*******************************************************************************************
*
* option "split"
* 
*******************************************************************************************/

static void
trackfilter_split(void)
{
	route_head *curr;
	route_head *master = track_list[0].track;
	int count = master->rte_waypt_ct;
	
	waypoint **buff;
	waypoint *wpt;
	queue *elem, *tmp;
	int i, j;
	struct tm t1, t2;
	
	if (count <= 1) return;
	
#ifdef TRACKF_DBG
	printf(MYNAME ": splitting track...\n");
#endif
	
	trackfilter_init_rte_name(master, track_list[0].first_time);
	
	buff = (waypoint **) xcalloc(count, sizeof(*buff));
	
	i = 0;
	QUEUE_FOR_EACH((queue *)&master->waypoint_list, elem, tmp)
	{
	    wpt = (waypoint *)elem;
	    buff[i++] = wpt;
	}
	
	curr = NULL;
	
	for (i=0, j=1; j<count; i++, j++)
	{
	    t1 = *localtime(&buff[i]->creation_time);
	    t2 = *localtime(&buff[j]->creation_time);
	    if ((t1.tm_year != t2.tm_year) || 
		(t1.tm_mon != t2.tm_mon) || (t1.tm_mday != t2.tm_mday))
	    {
#ifdef TRACKF_DBG
		printf(MYNAME ": new day %02d.%02d.%04d\n", t2.tm_mday, t2.tm_mon+1, t2.tm_year+1900);
#endif
		curr = (route_head *) route_head_alloc();
		trackfilter_init_rte_name(curr, buff[j]->creation_time);
		
		track_add_head(curr);
	    }
	    if (curr != NULL)
	    {
		wpt = waypt_dupe(buff[j]);
		route_del_wpt(master, buff[j]);
		route_add_wpt(curr, wpt);
		buff[j] = wpt;
	    }
	}
	
	xfree(buff);
}

/******************************************************************************************/

static void 
trackfilter_process(void)
{
	if (track_ct == 0) return;

	if (opt_pack == NULL && opt_split == NULL)
	{
	    trackfilter_pack();
	    return;
	}

	
	if (opt_pack != 0 && track_ct > 0)
	{
	    trackfilter_pack();
	    trackfilter_deinit();
	    trackfilter_init(NULL);
	}
	if (opt_split != 0 && track_ct > 0)
	{
	    if (track_ct > 1) fatal(MYNAME ": Cannot split more than one track, please pack before!\n");
	    trackfilter_split();
	    trackfilter_deinit();
	    trackfilter_init(NULL);
	}
}

/******************************************************************************************/

filter_vecs_t trackfilter_vecs = {
	trackfilter_init,
	trackfilter_process,
	trackfilter_deinit,
	NULL,
	trackfilter_args
};
