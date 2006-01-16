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

static waypoint *wpt_tmp = NULL;
static int item_count = -1;
static int waypoints = 0;

static route_head *route = NULL;

static char *routevia = NULL;

static arglist_t tef_xml_args[] = 
{
	{"routevia", &routevia, "Include only via stations in route", NULL, ARGTYPE_BOOL},
	{0, 0, 0, 0 }
};

#define MYNAME "TourExchangeFormat"

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

static xg_callback	tef_start, tef_header, tef_list_start, tef_list_end;
static xg_callback	tef_item_start, tef_point, tef_item_end;

static 
xg_tag_mapping tef_xml_map[] = {
	{ tef_start,		cb_start,	"/TEF" },
	{ tef_header,   	cb_start,	"/TEF/Header" },
	{ tef_list_start,   	cb_start,	"/TEF/WaypointList" },
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

	if (global_opts.objective == rtedata)
	{
	    route = route_head_alloc();
	    while (*avp) 
	    {
		if (strcmp(avp[0], "Name") == 0) 
		{
		    route->rte_name = xstrdup(avp[1]);
		}
		else if (strcmp(avp[0], "Software") == 0) 
		{
		    route->rte_desc = xstrdup(avp[1]);
		}
        	avp+=2;
	    }
	    route_add_head(route);
	}
}

/*
 *
 */
 
void
tef_list_start(const char *args, const char **attrv)
{
	const char **avp = &attrv[0];

        while (*avp) 
	{
    	    if (strcmp(avp[0], "ItemCount") == 0) 
	    {
		sscanf(avp[1], "%d", &item_count);
	    }
            avp+=2;
        }
}

/*
 * local procedure for waypoint handling
 */
 
void waypoint_final()
{
	int via;
	if (wpt_tmp == NULL) return;

	via = wpt_tmp->centiseconds;
	wpt_tmp->centiseconds = 0;
	
	if (via != 0)
	    waypt_add(wpt_tmp);
	    
	if (route != NULL)
	{
	    if ((via != 0) || (routevia == NULL))
	    {
		waypoint *wpt = waypt_dupe(wpt_tmp);
		route_add_wpt(route, wpt);
	    }
	}
	
	if (via == 0)
	    waypt_free(wpt_tmp);
	    
	wpt_tmp = NULL;
}

void 
tef_item_end(const char *args, const char **unused)
{
	waypoint_final();
}

/*
 * 
 */
 
void 
tef_list_end(const char *args, const char **unused)
{
	waypoint_final();
	if (waypoints != item_count)
	{
	    fatal(MYNAME ": waypoint count differs to internal \"ItemCount\"! (%d to %d)\n", waypoints, item_count);
	}
}

/*
 * 
 */
 
void 
tef_item_start(const char *args, const char **attrv)
{
	const char **avp = &attrv[0];

	wpt_tmp = waypt_new();
	wpt_tmp->centiseconds = 0;
	
	if ((waypoints == 1) || (waypoints == item_count)) 
	    wpt_tmp->centiseconds++;
	    
	waypoints++;
	
	while (*avp) 
	{
	    if (0 == strcmp(avp[0], "PointDescription")) 
	    {
		wpt_tmp->shortname = xstrdup(avp[1]);
	    }
	    if (0 == strcmp(avp[0], "SegDescription")) 
	    {
		wpt_tmp->description = xstrdup(avp[1]);
	    }
	    if ((0 == strcmp(avp[0], "ViaStation")) && (0 == strcmp(avp[1], "true")))
	    {
		wpt_tmp->centiseconds = 1;
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
        const char **avp = &attrv[0];
	char *comma;

	if (!wpt_tmp) return;
	
        while (*avp) 
	{
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
	xml_init(fname, tef_xml_map, NULL);
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
	{ ff_cap_none, ff_cap_none, ff_cap_read },
	tef_xml_rd_init,	
	NULL,	
	tef_xml_rd_deinit,
	NULL,
	tef_xml_read,
	NULL,
	NULL, 
	tef_xml_args,
	CET_CHARSET_UTF8, 1
};
