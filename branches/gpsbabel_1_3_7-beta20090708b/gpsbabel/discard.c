/*
    Discard points based on high Degree of Precision (DOP) values.

    Copyright (C) 2005 Robert Lipe, robertlipe@usa.net

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

#if FILTERS_ENABLED
static char *hdopopt = NULL;
static char *vdopopt = NULL;
static char *andopt = NULL;
static char *satopt = NULL;
static char *fixnoneopt = NULL;
static char *fixunknownopt = NULL;
static double hdopf;
static double vdopf;
static int satpf;
static gpsdata_type what;
static route_head *head;

static
arglist_t fix_args[] = {
	{"hdop", &hdopopt, "Suppress waypoints with higher hdop",
		"-1.0", ARGTYPE_BEGIN_REQ | ARGTYPE_FLOAT, ARG_NOMINMAX},
	{"vdop", &vdopopt, "Suppress waypoints with higher vdop",
		"-1.0", ARGTYPE_END_REQ | ARGTYPE_FLOAT, ARG_NOMINMAX},
	{"hdopandvdop", &andopt, "Link hdop and vdop supression with AND",
		NULL, ARGTYPE_BOOL, ARG_NOMINMAX},
	{"sat", &satopt, "Minimium sats to keep waypoints",
		"-1.0", ARGTYPE_BEGIN_REQ | ARGTYPE_INT, ARG_NOMINMAX},
	{"fixnone", &fixnoneopt, "Suppress waypoints without fix",
		NULL, ARGTYPE_BOOL, ARG_NOMINMAX},
	{"fixunknown", &fixunknownopt, "Suppress waypoints with unknown fix",
		NULL, ARGTYPE_BOOL, ARG_NOMINMAX},
	ARG_TERMINATOR
};

/*
 * Decide whether to keep or toss this point.
 */
static void
fix_process_wpt(const waypoint *wpt)
{
	int del = 0;
	int delh = 0;
	int delv = 0;
	
	waypoint *waypointp = (waypoint *) wpt;

	if ((hdopf >= 0.0) && (waypointp->hdop > hdopf))
		delh = 1;
	if ((vdopf >= 0.0) && (waypointp->vdop > vdopf))
		delv = 1;
	
	if (andopt)
		del = delh && delv;
	else
		del = delh || delv;
		
        if ((satpf >= 0) && (waypointp->sat < satpf))
		del = 1;

	if ((fixnoneopt) && (waypointp->fix == fix_none))
		del = 1;

	if ((fixunknownopt) && (waypointp->fix == fix_unknown))
		del = 1;

	if (del) {
		switch(what) {
			case wptdata:
				waypt_del(waypointp);
				break;
			case trkdata:
				track_del_wpt(head, waypointp);
				break;
			case rtedata:
				route_del_wpt(head, waypointp);
				break;
			default:
				return;
		}
		waypt_free(waypointp);
	}
}

static void
fix_process_head(const route_head *trk)
{
	head = (route_head *)trk;
}

static void
fix_process(void)
{
	// Filter waypoints.
	what = wptdata;
	waypt_disp_all(fix_process_wpt);

	// Filter tracks
	what = trkdata;
	track_disp_all(fix_process_head, NULL, fix_process_wpt);
	
	// And routes
	what = rtedata;
	route_disp_all(fix_process_head, NULL, fix_process_wpt);
	
}

static void
fix_init(const char *args) 
{
	if (hdopopt)
		hdopf = atof(hdopopt);
	else
		hdopf = -1.0;

	if (vdopopt)
		vdopf = atof(vdopopt);
	else
		vdopf = -1.0;
	
	if (satopt)
		satpf = atoi(satopt);
	else
		satpf = -1;
		
}

filter_vecs_t discard_vecs = {
	fix_init,
	fix_process,
	NULL,
	NULL,
	fix_args
};
#endif
