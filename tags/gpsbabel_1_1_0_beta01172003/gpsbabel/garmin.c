/*
    Jeeps wrapper for Garmin serial protocol.
  
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

#include <ctype.h>
#include <limits.h>
#include "defs.h"
#include "jeeps/gps.h"

#define MYNAME "GARMIN" 
static const char *portname;

static void
rw_init(const char *fname, const char *opts)
{
	if (global_opts.debug_level > 0)  {
		GPS_Enable_Warning();
		GPS_Enable_User();
	}
	if (global_opts.debug_level > 1)  {
		GPS_Enable_Diagnose();
	}
	GPS_Enable_Error();

        if (GPS_Init(fname) < 0) {
		fatal(MYNAME ":Can't init %s\n", fname);
	}
	portname = fname;

}

static void
rw_deinit(void)
{
}

static void
waypt_read(void)
{
	int i,n;
	GPS_PWay *way;

	if ((n = GPS_Command_Get_Waypoint(portname, &way)) < 0) {
		fatal(MYNAME  ":Can't get waypoint from %s\n", portname);
	}

	for (i = 0; i < n; i++) {
		waypoint *wpt_tmp = xcalloc(sizeof(*wpt_tmp),1);

		wpt_tmp->shortname = way[i]->ident;
		wpt_tmp->description = way[i]->cmnt;
		wpt_tmp->position.longitude.degrees = way[i]->lon;
		wpt_tmp->position.latitude.degrees = way[i]->lat;
		/*
		 * If a unit doesn't store altitude info (i.e. a D103)
		 * gpsmem will default the alt to INT_MAX.   Other units 
		 * (I can't recall if it was the V (D109) hor the Vista (D108) 
		 * return INT_MAX+1, contrary to the Garmin protocol doc which
		 * says they should report 1.0e25.   So we'll try to trap 
		 * all the cases here.     Yes, libjeeps should probably 
		 * do this and not us...
		 */
		if ((way[i]->alt == (float) (1<<31)) || 
		     (way[i]->alt == INT_MAX) ||
		     (way[i]->alt == 1.0e25)
		     ) {
			wpt_tmp->position.altitude.altitude_meters = unknown_alt;
		} else {
			wpt_tmp->position.altitude.altitude_meters = way[i]->alt;
		}
		
		waypt_add(wpt_tmp);
	}
}

static
void
track_read(void)
{
	int32 ntracks;
	GPS_PTrack *array;
	route_head *trk_head = NULL;
	waypoint *waypts;
	int trk_num = 0;
	char rtedescbuf[100];
	int i;

	ntracks = GPS_Command_Get_Track(portname, &array);
	waypts = xcalloc(sizeof (waypoint), ntracks);

	for(i = 0; i < ntracks; i++) {
		if ((trk_head == NULL) || array[i]->tnew) {
			trk_head = route_head_alloc();
			trk_head->rte_num = trk_num;
			sprintf(rtedescbuf, "Track %d", trk_num);
			trk_head->rte_name = xstrdup(rtedescbuf);
			trk_head->rte_num = trk_num;
			trk_num++;
			route_add_head(trk_head);
		}

		waypts[i].position.longitude.degrees = array[i]->lon;
		waypts[i].position.latitude.degrees = array[i]->lat;
		waypts[i].position.altitude.altitude_meters = array[i]->alt;
		waypts[i].shortname = xstrdup(array[i]->trk_ident);
		waypts[i].creation_time = array[i]->Time;
		
		route_add_wpt(trk_head, &waypts[i]);
	}

	while(--ntracks) {
		GPS_Track_Del(&array[ntracks]);
	}
	free(array);
}

static void
data_read(void)
{
	switch(global_opts.objective) {
		case trkdata: 
			track_read();
			break;
		case wptdata:
			waypt_read();
			break;
		default:
			fatal(MYNAME ": Routes are not yet supported\n");
	}
}

static void
data_write(void)
{
	int i;
	int32 ret;
	int n = waypt_count();
	queue *elem, *tmp;
	extern queue waypt_head;
	GPS_PWay *way;
	extern int32 gps_save_id;
	int short_length;

	way = xcalloc(n,sizeof(*way));

	for (i = 0; i < n; i++) {
		if(!((way)[i]=GPS_Way_New()))
			fatal(MYNAME ":not enough memory\n");
	}

	i = 0;
	/*
	 * Grope the unit we're talking to to set setshort_length to 
	 * 	20 for  the V, 
	 * 	10 for Street Pilot, Rhino, 76
	 * 	6 for the III, 12, emap, and etrex
	 * Fortunately, getting this "wrong" only results in ugly names
	 * when we're using the synthesize_shortname path.
	 */
	short_length = 10;

	switch ( gps_waypt_type )	/* waypoint type as defined by jeeps */
	{
		case 100:	/* The GARMIN GPS Interface Specification, */
		case 101:	/* says these waypoint types use an ident */
		case 102:	/* length of 6.  Waypoint types 106, 108 */
		case 103:	/* and 109 are all variable  length    */
		case 104:
		case 105:
		case 107:
		case 150:
		case 151:
		case 152:
		case 154:
		case 155:
			short_length = 6;
			break;
		case 106:	/* Waypoint types with variable ident length */
		case 108: 	/* Need GPSr id to know the actual length */
		case 109:                   
			switch ( gps_save_id )
			{
				case 130:	/* Garmin Etrex (yellow) */
					short_length = 6;
					break;
				case 155:	/* Garmin V */
					short_length = 20;
					break;
				default:
					break;
			}
			break;
		default:
			break;
			
	}
	setshort_length(short_length);
	setshort_mustupper(1);
	QUEUE_FOR_EACH(&waypt_head, elem, tmp) {
		waypoint *wpt;
		char *ident;
		char *src = NULL;
		char *wptname;

		wpt = (waypoint *) elem;

		/*
		 *  Undo less than helpful defaults from Way_New.
		 */
		way[i]->rte_ident[0] = 0;
		way[i]->rte_cmnt[0] = 0;
		way[i]->rte_link_subclass[0] = 0;
		way[i]->rte_link_ident[0] = 0;
		way[i]->city[0] = 0;
		way[i]->state[0] = 0;
		way[i]->facility[0] = 0;
		way[i]->addr[0] = 0;
		way[i]->cross_road[0] = 0;

		if(wpt->description) src = wpt->description;
		if(wpt->notes) src = wpt->notes;

		ident = global_opts.synthesize_shortnames ? 
				mkshort(src) : 
				wpt->shortname;
		strncpy(way[i]->ident,  ident, sizeof(way[i]->ident));
		way[i]->ident[sizeof(way[i]->ident)-1] = 0;
		if (src && strlen(src)) {
			strncpy(way[i]->cmnt, src, sizeof(way[i]->cmnt));
		}
		way[i]->lon = wpt->position.longitude.degrees;
		way[i]->lat = wpt->position.latitude.degrees;
		if (wpt->position.altitude.altitude_meters != unknown_alt) {
			way[i]->alt = wpt->position.altitude.altitude_meters;
		}
		i++;
	}
	if ((ret = GPS_Command_Send_Waypoint(portname, way, n)) < 0) {
		fatal(MYNAME ":communication error sending wayoints..\n");
	}

	for (i = 0; i < n; ++i) {
		GPS_Way_Del(&way[i]);
	}
	free(way);
}

ff_vecs_t garmin_vecs = {
	rw_init,
	rw_init,
	rw_deinit,
	rw_deinit,
	data_read,
	data_write,

};
