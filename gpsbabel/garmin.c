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
#include "defs.h"
#include "jeeps/gps.h"

#define MYNAME "GARMIN" 
static FILE *file_in;
static const char *portname;

static void
rw_init(const char *fname)
{
        if (GPS_Init(fname) < 0) {
		fatal(MYNAME ":Can't init %s\n", fname);
	}
	portname = fname;

}

static void
rw_deinit(void)
{
	fclose(file_in);
}

static void
data_read(void)
{
	int i,n;
	GPS_PWay *way;

	if ((n = GPS_Command_Get_Waypoint(portname, &way)) < 0) {
		fatal(MYNAME  ":Can't get waypoint from %s\n", portname);
	}

	for (i = 0; i < n; i++) {
		waypoint *wpt_tmp = calloc(sizeof(*wpt_tmp),1);

		wpt_tmp->shortname = way[i]->ident;
		wpt_tmp->description = way[i]->cmnt;
		wpt_tmp->position.longitude.degrees = way[i]->lon;
		wpt_tmp->position.latitude.degrees = way[i]->lat;
		wpt_tmp->position.altitude.altitude_meters = way[i]->alt;
		
		waypt_add(wpt_tmp);
	}

}

static void
gpsutil_disp(waypoint *wpt)
{
#if 0
	double lon,lat;
	signed int ilon, ilat;
	const char *icon_token = "0";
	char tbuf[1024];
	char *tp = tbuf;
	time_t tm = wpt->creation_time;

	lon = wpt->position.longitude.degrees;
	lat = wpt->position.latitude.degrees;

	fprintf(file_out, "%08.5f, %08.5f, %s\n",
		lat,
		lon,
		wpt->description);

#else
//	garmin_write_waypoint(wpt);
#endif
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


	if(!(way=(GPS_PWay *)malloc(n*sizeof(GPS_PWay *)))) 
		fatal(MYNAME ":not enough memory\n");
	for (i = 0; i < n; i++) {
		if(!((way)[i]=GPS_Way_New()))
			return MEMORY_ERROR;
	}

	i = 0;
	setshort_length(10);
	setshort_mustupper(1);
	QUEUE_FOR_EACH(&waypt_head, elem, tmp) {
		waypoint *wpt;
		char *ident;

		wpt = (waypoint *) elem;
		ident = global_opts.synthesize_shortnames ? 
				mkshort(wpt->description) : 
				wpt->shortname;

		strncpy(way[i]->ident,  ident, sizeof(way[i]->ident));
		strncpy(way[i]->cmnt, wpt->description, sizeof(way[i]->cmnt));
		way[i]->lon = wpt->position.longitude.degrees;
		way[i]->lat = wpt->position.latitude.degrees;
		way[i]->alt = wpt->position.altitude.altitude_meters;
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
