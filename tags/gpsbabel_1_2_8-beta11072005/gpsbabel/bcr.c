/*

    Support for Motorrad Routenplaner (Map&Guide) .bcr files.

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


#include "defs.h"
#include "garmin_tables.h"
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "cet_util.h"

#define MYNAME "bcr"

#undef BCR_DEBUG
    
#define SEC_UNKNOWN	0
#define SEC_CLIENT 	1
#define SEC_ROUTE	2
#define SEC_DESCR	3
#define SEC_COORD	4

#define R_EARTH		6371000		/* radius of our big blue ball */

/*  
    6371014 would be a better value when converting to f.e. to mapsoure,
    but this seems to be used by Map&Guide when exporting to XML. 
*/

static FILE *fin, *fout;
static char *filename;
static int curr_rte_num, target_rte_num;
static double radius;

/* placeholders for options */

static char *rtenum_opt;
static char *rtename_opt;
static char *radius_opt;

static
arglist_t bcr_args[] = {
	{"index", &rtenum_opt, "Index of route to write (if more the one in source)", NULL, ARGTYPE_INT, "1", NULL },
	{"name", &rtename_opt, "New name for the route", NULL, ARGTYPE_STRING },
	{"radius", &radius_opt, "Radius of our big earth (default 6371000 meters)", NULL, ARGTYPE_FLOAT },
	{0, 0, 0, 0, 0}
};

void
bcr_init_radius(void)
{
	if (radius_opt != NULL)				/* preinitialize the earth radius */
	{
	    radius = atof(radius_opt);
	    if (radius < 0)
		fatal(MYNAME ": Sorry, the radius should be greater than zero!\n");
	}
	else
	    radius = (double)R_EARTH;

	if (global_opts.verbose_status > 0)
	    printf(MYNAME ": We calculate with radius %f meters.\n", radius);
}

static void
bcr_rd_init(const char *fname)
{
	filename = xstrdup(fname);
	fin = xfopen(fname, "r", MYNAME);
	bcr_init_radius();
}

static void
bcr_rd_deinit(void)
{
	fclose(fin);
	xfree(filename);
}

/* ------------------------------------------------------------*/

char *
bcr_next_char(const char *buff)
{
	char *result = (char *)buff;
	while (*result > '\0' && *result <= ' ') result++;	/* trim leading spaces */
	return result;
}

waypoint *
bcr_find_waypt(const char *name, route_head *route)		/* find a waypt by name, create new */
{								/* if not found */
	waypoint *wpt;
	queue *elem, *tmp;
	
	QUEUE_FOR_EACH(&route->waypoint_list, elem, tmp) 
	{
	    wpt = (waypoint *) elem;
	    if (0 == strcmp(wpt->shortname, name)) 
		return wpt;
	}
	wpt = waypt_new();
	wpt->shortname = xstrdup(name);

	return wpt;
}

void
bcr_create_waypts_from_route(route_head *route)
{
	waypoint *wpt;
	queue *elem, *tmp;
	
	QUEUE_FOR_EACH(&route->waypoint_list, elem, tmp) 
	{
	    wpt = waypt_dupe((waypoint *) elem);
	    waypt_add(wpt);
	}
}

void
bcr_wgs84_to_mercator(const double lat, const double lon, int *north, int *east)
{
	double N, E;
	
	N = log(tan(lat * M_PI / 360 + M_PI / 4)) * radius;
	E = lon * radius * M_PI / (double)180;
	
	if (lat > 0) N += 0.500000000001;	/* we go from double to integer */
	else N -= 0.500000000001;		/* it's time to round a little bit */
	if (lon > 0) E += 0.500000000001;
	else E -= 0.500000000001;
	
	*north = N;
	*east = E;
}

void
bcr_mercator_to_wgs84(const int north, const int east, double *lat, double *lon)		
{
	*lat = 2 * (atan(exp(north / radius)) - M_PI / 4) / M_PI * (double)180;
	*lon = (double)east * (double)180 / (radius * M_PI);
}

static int
bcr_sort_route_by_index_cb(const void *a, const void *b)
{
	const waypoint *wa = *(waypoint **)a;
	const waypoint *wb = *(waypoint **)b;
	return wa->centiseconds - wb->centiseconds;
}

route_head *
bcr_sort_route_by_index(route_head *route)
{
	route_head *result;
	queue *elem, *tmp;
	waypoint **list;
	waypoint *wpt;
	int i;
	int count = route->rte_waypt_ct;
	
	if (count == 0) return (route);					/* nothing to do */
	
	result = route_head_alloc();
	result->rte_name = xstrdup(route->rte_name);
	route_add_head(result);
	
	list = (waypoint **) xcalloc(route->rte_waypt_ct, sizeof(*list));
	i = 0;
	QUEUE_FOR_EACH((queue *)&route->waypoint_list, elem, tmp)
	{
	    wpt = (waypoint *)elem;
	    list[i++] = wpt;
	}
	qsort(list, route->rte_waypt_ct, sizeof(*list), bcr_sort_route_by_index_cb);
	for (i=0; i<count; i++)
	{
	    wpt = list[i];
	    wpt->centiseconds = 0;					/* reset our index container */
	    route_add_wpt(result, waypt_dupe(wpt));
	    route_del_wpt(route, wpt);
	}
	
	xfree(list);
	route_del_head(route);
	
	return result;
}

/* ------------------------------------------------------------- */

static void
bcr_data_read(void)
{
	char buff[1024];
	char *src;
	int section = SEC_UNKNOWN;
	char *c, *cx, *ctemp;
	int index;
	int mlat, mlon;		/* mercator data */
	double xalt;
	int line, skip;
	
	route_head *route;
	waypoint *wpt;
	
	route = route_head_alloc();
	route_add_head(route);
	
	line = skip = 0;
	src = NULL;
	
	while (NULL != fgets(buff, sizeof(buff), fin))
	{
	    line++;
	    
	    c = buff;				/* trim the end of the buffer */
	    cx = c + strlen(c) - 1;
	    while ((cx > c) && (*cx <= ' '))
	    {
		*cx = '\0';
		cx--;
	    }
	    if (src != NULL) xfree(src);
	    
	    src = xstrdup(buff);
	    /* !! buff is now free and can be used */
	    
	    c = bcr_next_char(src);		/* skip spaces */
	    if (*c == '\0') continue;		/* skip empty lines */
	    
	    if (*c == '[') 			/* new section */
	    {
		skip = 0;
		
		c = bcr_next_char(++c);
		cx = strchr(c, ']');
		if (cx == NULL) fatal(MYNAME ": error in file structure (\"]\" expected)!\n");
		
		*cx = '\0';
		if (strcmp(c, "CLIENT") == 0) section = SEC_CLIENT;
		else if (strcmp(c, "ROUTE") == 0) section = SEC_ROUTE;
		else if (strcmp(c, "DESCRIPTION") == 0) section = SEC_DESCR;
		else if (strcmp(c, "COORDINATES") == 0) section = SEC_COORD;
		else 
		{
		    printf(MYNAME ": unknown section \"%s\".\n", c);
		    skip = 1;
		}
		continue;
	    }
	    
	    if (skip != 0) continue;
	    	
	    cx = strchr(c, '=');
	    if (cx == NULL) continue;
	    
	    *cx++ = '\0';			/* delimit in key and data */

	    if ((section == SEC_CLIENT) && (strcmp(c, "ROUTENAME") == 0))
	    {
		route->rte_name = xstrdup(cx);
	    }
	    else
	    {
		if (strncmp(c, "STATION", 7) != 0) continue;
		index = atoi(c+7);

		/* bcr_find_waypt(... creates new waypoint, if not in queue */
		
		switch(section)
		{
		    case SEC_CLIENT:
			wpt = bcr_find_waypt(c, route);
			wpt->centiseconds = index;
			ctemp = strchr(cx, ',');
			if (ctemp != NULL) *ctemp = ' ';
			if (2 != sscanf(cx, "%s %lf", buff, &xalt))
			    fatal(MYNAME ": structure error on line %d!\n(data: %s=%s)\n", line, c, cx);
#if 0
			if (xalt != 999999999)
			    wpt->altitude = xalt / 3.2808;	/* convert feet to meters */
#endif
			route_add_wpt(route, wpt);
			
			if (case_ignore_strcmp(buff, "standort") == 0)
			    wpt->icon_descr = mps_find_desc_from_icon_number(18, MAPSOURCE);
			else if (case_ignore_strcmp(buff, "Town") == 0)
			    wpt->icon_descr = mps_find_desc_from_icon_number(69, MAPSOURCE);
			else
			    printf(MYNAME ": Unknown icon \"%s\" found. Please report.\n", buff);
			break;

		    case SEC_DESCR:
			wpt = bcr_find_waypt(c, route);	
			wpt->centiseconds = index;
			
			ctemp = strchr(cx, '@');
			if (ctemp != NULL)
			{
			    *ctemp-- = '\0';
			    if (*ctemp == ',') *ctemp = '\0';
			}
			wpt->description = xstrdup(cx);
			break;

		    case SEC_COORD:
			wpt = bcr_find_waypt(c, route);	
			wpt->centiseconds = index;
			if (2 != sscanf(cx, "%d,%d", &mlon, &mlat))
			    fatal(MYNAME ": structure error on line %d!\n", line);
			    
			bcr_mercator_to_wgs84(mlat, mlon, &wpt->latitude, &wpt->longitude);
		    case SEC_ROUTE:
			break;
		}
	    }
	}
	if (src != NULL) xfree(src);
	src = NULL;
	
	route = bcr_sort_route_by_index(route);
	bcr_create_waypts_from_route(route);
}

/* %%% bcr write support %%% ----------------------------------- */

static void
bcr_wr_init(const char *fname)
{
	filename = xstrdup(fname);
	fout = xfopen(fname, "w", MYNAME);
	bcr_init_radius();
}

static void
bcr_wr_deinit(void)
{
	fclose(fout);
	xfree(filename);
}

static void 
bcr_route_trailer(const route_head *rte)
{
}

static void
bcr_write_wpt(const waypoint *wpt)
{
}

void bcr_write_line(FILE *fout, const char *key, int *index, const char *value)
{
	if (value == NULL)				/* this is mostly used in the world of windows */
	{						/* so we respectfully add a CR/LF on each line */
	    fprintf(fout, "%s\x0d\n", key);
	}
	else
	{
	    char *tmp;
	    
	    tmp = (value != NULL) ? xstrdup(value) : xstrdup("");
	    if (index != NULL)
		fprintf(fout, "%s%d=%s\x0d\n", key, *index, tmp);
	    else
		fprintf(fout, "%s=%s\x0d\n", key, tmp);
	    xfree(tmp);
	}
}

static void 
bcr_route_header(const route_head *route)
{
	queue *elem, *tmp;
	waypoint *wpt;
	char *c;
	int i, icon, north, east, nmin, nmax, emin, emax;
	char buff[128], symbol[32];
	
	curr_rte_num++;
	if (curr_rte_num != target_rte_num) return;	
	
	bcr_write_line(fout, "[CLIENT]", NULL, NULL);			/* client section */

	bcr_write_line(fout, "REQUEST", NULL, "TRUE");
	
	c = route->rte_name;
	if (rtename_opt != 0) c = rtename_opt;
	if (c != NULL)
	    bcr_write_line(fout, "ROUTENAME", NULL, c);
	else
	    bcr_write_line(fout, "ROUTENAME", NULL, "Route");

	bcr_write_line(fout, "DESCRIPTIONLINES", NULL, "1");
	bcr_write_line(fout, "DESCRIPTION1", NULL, "");
	
	i = 0;
	QUEUE_FOR_EACH(&route->waypoint_list, elem, tmp) 
	{
	    i++;
	    wpt = (waypoint *) elem;
	    
	    strncpy(symbol, "Standort", sizeof(symbol));
	    if (wpt->icon_descr != 0)
	    {
		icon = mps_find_icon_number_from_desc(wpt->icon_descr, MAPSOURCE);
		if ((icon >= 69) && (icon <= 72))
		    strncpy(symbol, "Town", sizeof(symbol));
	    }
	    snprintf(buff, sizeof(buff), "%s,%s", symbol, "999999999");
	    bcr_write_line(fout, "STATION", &i, buff);
	}
	    
	bcr_write_line(fout, "[COORDINATES]", NULL, NULL);		/* coords section */
	
	nmin = emin = (1<<30);
	emax = nmax = -nmin;
	
	i = 0;
	QUEUE_FOR_EACH(&route->waypoint_list, elem, tmp) 
	{
	    i++;
	    wpt = (waypoint *) elem;
	    
	    bcr_wgs84_to_mercator(wpt->latitude, wpt->longitude, &north, &east);
	    
	    if (north > nmax) nmax = north;
	    if (east > emax) emax = east;
	    if (north < nmin) nmin = north;
	    if (east < emin) emin = east;
	    
	    snprintf(buff, sizeof(buff), "%d,%d", east, north);
	    bcr_write_line(fout, "STATION", &i, buff);
	}
	
	bcr_write_line(fout, "[DESCRIPTION]", NULL, NULL);		/* descr. section */
	
	i = 0;
	QUEUE_FOR_EACH(&route->waypoint_list, elem, tmp) 
	{
	    i++;
	    wpt = (waypoint *) elem;
	    c = wpt->description;
	    if (c == NULL) c = wpt->shortname;
	    bcr_write_line(fout, "STATION", &i, c);
	}
	
	bcr_write_line(fout, "[ROUTE]", NULL, NULL);			/* route section */

	snprintf(buff, sizeof(buff), "%d,%d,%d,%d", emin, nmax, emax, nmin);
	bcr_write_line(fout, "ROUTERECT", NULL, buff);
	
}

static void
bcr_data_write(void)
{
	
	if (global_opts.objective == rtedata)
	{
	    target_rte_num = 1;
	    
	    if (rtenum_opt != NULL)
	    {
		target_rte_num = atoi(rtenum_opt);
		if (((unsigned)target_rte_num > route_count()) || (target_rte_num < 1))
		    fatal(MYNAME ": invalid route number %d (1..%d))!\n", 
			target_rte_num, route_count());
	    }
	    curr_rte_num = 0;
	    route_disp_all(bcr_route_header, bcr_route_trailer, bcr_write_wpt);
	}
}

ff_vecs_t bcr_vecs = {
	ff_type_file,
	{ ff_cap_none, ff_cap_none, ff_cap_read | ff_cap_write},
	bcr_rd_init,
	bcr_wr_init,
	bcr_rd_deinit,
	bcr_wr_deinit,
	bcr_data_read,
	bcr_data_write,
	NULL,
	bcr_args,
	CET_CHARSET_MS_ANSI, 0	/* CET-REVIEW */
};
