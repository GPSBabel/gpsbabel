/*
    Arbitrary Sorting Filter(s)

    Copyright (C) 2004 Robert Lipe, robertlipe@usa.net

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

typedef enum {
	sm_unknown = 0,
	sm_gcid,
	sm_shortname,
	sm_description,
	sm_time
} sort_mode_;

sort_mode_ sort_mode = sm_shortname;	/* How are we sorting these? */

static char *opt_sm_gcid, *opt_sm_shortname, *opt_sm_description, *opt_sm_time;

static
arglist_t sort_args[] = {
	{"gcid", &opt_sm_gcid, "Sort by numeric geocache ID", 
		NULL, ARGTYPE_BOOL },
	{"shortname", &opt_sm_shortname, "Sort by waypoint short name", 
		NULL, ARGTYPE_BOOL },
	{"description", &opt_sm_description, "Sort by waypoint description", 
		NULL, ARGTYPE_BOOL },
	{"time", &opt_sm_time, "Sort by time", 
		NULL, ARGTYPE_BOOL },
	{0, 0, 0, 0, 0}
};

static int
sort_comp(const void * a, const void * b)
{
	const waypoint *x1 = *(waypoint **)a;
	const waypoint *x2 = *(waypoint **)b;

	switch (sort_mode)  {
	   case sm_gcid: return x1->gc_data.id > x2->gc_data.id;
	   case sm_shortname: return strcmp (x1->shortname, x2->shortname);
	   case sm_description: return strcmp (x1->description, x2->description);
	   case sm_time: return x1->creation_time > x2->creation_time;
	   default: abort(); /* Internal caller error. */
	}
}

void 
sort_process(void)
{
	queue * elem, * tmp;
	waypoint ** comp;
	int i = 0, wc;

	wc = waypt_count();

	comp = (waypoint **) xcalloc(wc, sizeof(*comp));

	QUEUE_FOR_EACH(&waypt_head, elem, tmp) {
		comp[i] = (waypoint *)elem;
		waypt_del(comp[i]); /* Pop this waypoint off the master Q */
		i++;
	}

	qsort(comp, wc, sizeof(waypoint *), sort_comp);

	/*
	 * Now re-add the list back.
	 */
	for (i = 0; i < wc ; i++) {
		waypt_add(comp[i]);
	}

	if (comp)
		xfree(comp);
}

void
sort_init(const char *args) 
{
	if (opt_sm_gcid)
		sort_mode = sm_gcid;
	if (opt_sm_shortname)
		sort_mode = sm_shortname;
	if (opt_sm_description)
		sort_mode = sm_description;
	if (opt_sm_time)
		sort_mode = sm_time;
}

filter_vecs_t sort_vecs = {
	sort_init,
	sort_process,
	NULL,
	NULL,
	sort_args
};
