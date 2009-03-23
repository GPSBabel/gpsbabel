/*
	Read PocketFMS flightplan files.

	Copyright (C) 2009 Tobias Kretschmar, tobias.kretschmar@gmx.de

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

static int isFirst = 1;
static route_head *route = NULL;
static waypoint *wpt_to, *wpt_from;

#define MYNAME "PocketFMS FlightPlan"

static xg_callback	wpt_s, wpt_from_lat, wpt_from_lon, wpt_from_name;
static xg_callback	wpt_e, wpt_to_lat, wpt_to_lon, wpt_to_name, wpt_altitude;

static xg_tag_mapping gl_map[] = {
 { wpt_s,			cb_start, "/PocketFMSFlightplan/LIB" },
 { wpt_from_lat,	cb_cdata, "/PocketFMSFlightplan/LIB/FromPoint/Latitude" },
 { wpt_from_lon,	cb_cdata, "/PocketFMSFlightplan/LIB/FromPoint/Longitude" },
 { wpt_from_name,	cb_cdata, "/PocketFMSFlightplan/LIB/FromPoint/FriendlyShortname" },
 { wpt_to_lat,		cb_cdata, "/PocketFMSFlightplan/LIB/ToPoint/Latitude" },
 { wpt_to_lon,		cb_cdata, "/PocketFMSFlightplan/LIB/ToPoint/Longitude" },
 { wpt_to_name,		cb_cdata, "/PocketFMSFlightplan/LIB/ToPoint/FriendlyShortname" },
 { wpt_altitude,	cb_cdata, "/PocketFMSFlightplan/LIB/PlannedAltitude/@Value" },
 { wpt_e,			cb_end,   "/PocketFMSFlightplan/LIB" },
 { NULL, 			0,		NULL}
};

static void
rd_init(const char *fname)
{
	xml_init(fname, gl_map, NULL);
}

static void
data_read(void)
{
	xml_read();
}

static void
rd_deinit(void)
{
	if (route != NULL)
	{
		waypoint *head = (waypoint *) QUEUE_FIRST(&route->waypoint_list);
		waypoint *tail = (waypoint *) QUEUE_LAST(&route->waypoint_list);
		if (head != NULL)
			route->rte_name = xstrdup (head->shortname);
		route->rte_name = xstrappend(route->rte_name, " - ");
		if (tail != NULL)
			route->rte_name = xstrappend(route->rte_name, tail->shortname);
	}
	xml_deinit();
}

static void
wr_init(const char *fname)
{
	fatal("Writing file of type %s is not supported\n", MYNAME);
}

void	wpt_s(const char *args, const char **unused)
{
	if (isFirst == 1) {
		wpt_from = waypt_new();
		route = route_head_alloc();
		route->rte_desc=xstrdup("PocketFMS flightplan");
		route_add_head(route); 
	}
	wpt_to = waypt_new();
}

void	wpt_e(const char *args, const char **unused)
{
	if (isFirst == 1) {
		route_add_wpt(route, wpt_from);
		if (doing_wpts)		
			waypt_add(waypt_dupe(wpt_from));
		wpt_from = NULL;
		isFirst = 0;
	}
	route_add_wpt(route, wpt_to);
	if (doing_wpts)		
		waypt_add(waypt_dupe(wpt_to));
	wpt_to = NULL;
}

void	wpt_from_lat(const char *args, const char **unused)
{
	if (wpt_from != NULL)
		wpt_from->latitude = atof(args);
}

void	wpt_from_lon(const char *args, const char **unused)
{
	if (wpt_from != NULL)
		wpt_from->longitude = atof(args);
}

void	wpt_from_name(const char *args, const char **unused)
{
	if (wpt_from != NULL)
		wpt_from->shortname = xstrappend(wpt_from->shortname, args);
}

void	wpt_to_lat(const char *args, const char **unused)
{
	wpt_to->latitude = atof(args);
}

void	wpt_to_lon(const char *args, const char **unused)
{
	wpt_to->longitude = atof(args);
}

void	wpt_to_name(const char *args, const char **unused)
{
	wpt_to->shortname = xstrappend(wpt_to->shortname, args);
}

void	wpt_altitude(const char *args, const char **unused)
{
	wpt_to->altitude = atof(args);
}

ff_vecs_t pocketfms_fp_vecs = {
		ff_type_file,
	{ 
		ff_cap_read  	/* waypoints */, 
		ff_cap_none 	/* tracks */, 
		ff_cap_read  	/* routes */
	},
		rd_init,
		wr_init,
		rd_deinit,
		NULL,
		data_read,
		NULL,
		NULL,
		NULL,
	CET_CHARSET_ASCII, 0	/* CET-REVIEW */
};
