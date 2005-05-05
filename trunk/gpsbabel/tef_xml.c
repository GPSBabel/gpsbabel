/* 
	Support for XML based "TourExchangeFormat", 
	found in Map & Guide Motorrad-Tourenplaner 2005/06
	
	Copyright (C) 2005 Olaf Klein, o.b.klein@t-online.de

	Based on kml.c, Keyhole "kml" format.
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
#include "xmlgeneric.h"

static char *deficon = NULL;

static waypoint *wpt_tmp = NULL;
static int wpt_tmp_valid = 0;
static route_head *track = NULL;
static int waypoints = 0;

FILE *fd;
FILE *ofd;

#define MYNAME "TourExchangeFormat"

// void DBG(const char *func)
// {
//     printf("DBG(%s)=in %s\n", MYNAME, func);
// }


#if NO_EXPAT
void
tef_xml_rd_init(const char *fname)
{
	fatal(MYNAME ": This build excluded TEF support because expat was not installed.\n");
}

void
tef_xml_read(void)
{
}

#else

static xg_callback	tef_start, tef_item_end, tef_list_start, tef_header, tef_list_end;
static xg_callback	tef_item_start, tef_point;

static 
xg_tag_mapping tef_xml_map[] = {
	{ tef_start,		cb_start,	"/TEF" },
	{ tef_header,   	cb_start,	"/TEF/Header" },
	{ tef_list_start,	cb_start, 	"/TEF/WaypointList" },
	{ tef_item_start, 	cb_start, 	"/TEF/WaypointList/Item" },
	{ tef_point, 		cb_start, 	"/TEF/WaypointList/Item/Point" },
	{ tef_item_end, 	cb_end, 	"/TEF/WaypointList/Item" },
	{ tef_list_end, 	cb_end, 	"/TEF/WaypointList" },
	{ NULL, 		0, 		NULL }
};


/*
 * tef_start: check for comment "TourExchangeFormat"
 */

void 
tef_start(const char *args, const char **attrv)
{
	int valid = 0;
	const char **avp = &attrv[0];
        while (*avp) 
	{
    	    if (strcmp(avp[0], "Comment") == 0) 
	    {
		if (0 == strcmp(avp[1], "TourExchangeFormat")) valid = 1;
	    }
    	    avp+=2;
        }
	if (!valid)
	{
	    fatal(MYNAME ": error in source file.\n");
	}
}

/*
 * tef_header: "Name" > Route name, "Software" > Route descr.
 */

void 
tef_header(const char *args, const char **attrv)
{
	const char **avp = &attrv[0];
	char buff[1024];

	track = route_head_alloc();

        while (*avp) {
                if (strcmp(avp[0], "Name") == 0) 
		{
			track->rte_name = xstrdup(str_utf8_to_cp1252(avp[1]));
                }
                else if (strcmp(avp[0], "Software") == 0) 
		{
			track->rte_desc = xstrdup(str_utf8_to_cp1252(avp[1]));
                }
                avp+=2;
        }
	
	if (!track->rte_name) track->rte_name = xstrdup("No name");
	if (!track->rte_desc) track->rte_desc = xstrdup("No description");

	route_add_head(track);
}


/*
 * tef_list_start: 
 */

void 
tef_list_start(const char *args, const char **unused)
{
	if (!track)
	{
	    track = route_head_alloc();
	    track->rte_name = xstrdup("Unknown");
	    route_add_head(track);
	}
}

/*
 * local procedure for waypoint handling
 */
 
void waypoint_final(int force)
{
	if (!wpt_tmp) return;
	
	if (force || wpt_tmp_valid > 0)
	{
	    waypt_add(wpt_tmp);
	    if (track)
	    {
		waypoint *wpt = waypt_new();
		wpt->shortname = xstrdup(wpt_tmp->shortname);
		wpt->description = xstrdup(wpt_tmp->description);
		wpt->creation_time = wpt_tmp->creation_time;
		wpt->latitude = wpt_tmp->latitude;
		wpt->longitude = wpt_tmp->longitude;
		route_add_wpt(track, wpt);
		wpt_tmp = NULL;
	    }
	}
}


/*
 * 
 */
 
void 
tef_item_end(const char *args, const char **unused)
{
	waypoint_final(waypoints == 0);
	waypoints++;
}

/*
 * 
 */
 
void 
tef_list_end(const char *args, const char **unused)
{
	waypoint_final(1);
	waypoints++;
}

/*
 * 
 */
 
void 
tef_item_start(const char *args, const char **attrv)
{
	const char **avp = &attrv[0];
	const char buf[1024];

	wpt_tmp = waypt_new();
	wpt_tmp_valid = 0;
	wpt_tmp->creation_time = current_time();
	
	while (*avp) 
	{
	    if (0 == strcmp(avp[0], "PointDescription")) 
	    {
//		sprintf(buf, "%04i-%s", waypoints, str_utf8_to_cp1252(avp[1]));
		sprintf(buf, "%s", str_utf8_to_cp1252(avp[1]));
		wpt_tmp->shortname = xstrdup(buf);
	    }
	    if (0 == strcmp(avp[0], "SegDescription")) 
	    {
		wpt_tmp->description = xstrdup(str_utf8_to_cp1252(avp[1]));
	    }
	    if ((0 == strcmp(avp[0], "ViaStation")) && (0 == strcmp(avp[1], "true")))
	    {
		wpt_tmp_valid = 1;
	    }
	    avp+=2;
	}
}

/*
 * 
 */
 
void 
tef_point(const char *args, const char **attrv)
{
	if (!wpt_tmp) return;
	
        const char **avp = &attrv[0];
	char *comma;
        while (*avp) {
                if (strcmp(avp[0], "y") == 0) 
		{
			comma = strstr(avp[1], ",");
			if (comma) *comma='.';
                        sscanf(avp[1], "%lf", &wpt_tmp->latitude);
                }
                else if (strcmp(avp[0], "x") == 0) 
		{
			comma = strstr(avp[1], ",");
			if (comma) *comma='.';
                        sscanf(avp[1], "%lf", &wpt_tmp->longitude);
                }
                avp+=2;
        }
}

/*
 * 
 */
 
static void 
tef_xml_rd_init(const char *fname)
{
	xml_init(fname, tef_xml_map);
}

/*
 * 
 */
 
static void 
tef_xml_read(void)
{
	xml_read();
}

#endif

/*
 * 
 */
 
void 
tef_xml_rd_deinit(void)
{
	xml_deinit();
}

ff_vecs_t tef_xml_vecs = {
	ff_type_file,
	{ ff_cap_read, ff_cap_none, ff_cap_none },
	tef_xml_rd_init,	
	NULL,	
	tef_xml_rd_deinit,
	NULL,
	tef_xml_read,
	NULL,
	NULL, 
	NULL
};
