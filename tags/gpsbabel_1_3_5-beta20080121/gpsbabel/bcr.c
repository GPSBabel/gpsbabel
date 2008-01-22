/*

    Support for Motorrad Routenplaner (Map&Guide) .bcr files.

    Copyright (C) 2005-2007 Olaf Klein, o.b.klein@gpsbabel.org

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

/*
    2006/01/22: reader simplified with inifile library
    2007/01/30: new option prefer_shortnames
    		don't check global_opts.objective
    2007/04&14: new handling of DESCRIPTION lines
*/

#include "defs.h"
#include "csv_util.h"
#include "garmin_tables.h"
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "cet_util.h"
#include "inifile.h"

#define MYNAME "bcr"

#undef BCR_DEBUG
    
#define R_EARTH		6371000		/* radius of our big blue ball */
#define BCR_DEF_ICON		"Standort"
#define BCR_DEF_MPS_ICON	"Waypoint"
#define BCR_UNKNOWN		(double) 999999999

/*  
    6371014 would be a better value when converting to f.e. to mapsoure,
    but this seems to be used by Map&Guide when exporting to XML. 
*/

static gbfile *fout;
static char *filename;
static int curr_rte_num, target_rte_num;
static double radius;
static inifile_t *ini;

/* placeholders for options */

static char *rtenum_opt;
static char *rtename_opt;
static char *radius_opt;
static char *prefer_shortnames_opt;

static
arglist_t bcr_args[] = {
	{"index", &rtenum_opt, "Index of route to write (if more the one in source)", 
		NULL, ARGTYPE_INT, "1", NULL },
	{"name", &rtename_opt, "New name for the route", 
		NULL, ARGTYPE_STRING, ARG_NOMINMAX },
	{"radius", &radius_opt, "Radius of our big earth (default 6371000 meters)", "6371000",
		ARGTYPE_FLOAT, ARG_NOMINMAX },
	{"prefer_shortnames", &prefer_shortnames_opt, "Use shortname instead of description",
		NULL, ARGTYPE_BOOL, ARG_NOMINMAX },
	ARG_TERMINATOR
};

typedef struct {
	char *bcr_name;
	char *mps_name;
	char *symbol_DE;
	int  warned;
} bcr_icon_mapping_t;

static
bcr_icon_mapping_t bcr_icon_mapping[] = {
	{ BCR_DEF_ICON,		BCR_DEF_MPS_ICON, 	BCR_DEF_ICON },
	{ "",			BCR_DEF_MPS_ICON, 	"Eigene Adressen" },
	{ "AdrMon alpen",	"Summit",		"Pass-Strassen" },
	{ "AdrMon bauern",	NULL,			"Bauern- und Biohoefe" },
	{ "AdrMon cmpngs",	"Campground",		"Campingplaetzte" },
	{ "AdrMon p_aeu",	"Scenic Area",		"Sehenswertes" },
	{ "AdrMon p_beu",	"Gas Station",		"Tanken" },
	{ "AdrMon p_deu",	"Parking Area",		"Parken" },
	{ "AdrMon p_feu",	"Restaurant",		"Gastro" },
	{ "AdrMon p_geu",	"Museum",		"Freizeit" },
	{ "AdrMon p_heu",	"Gas Station",		"Tankstellen" },
	{ "AdrMon p_keu",	NULL,			"Faehrverbindungen" },
	{ "AdrMon p_leu",	NULL,			"Grenzuebergaenge" },
	{ "AdrMon p_teu",	NULL,			"Wein- und Sektgueter" },
	{ "AdrMon RUINEN",	"Ghost Town",		"Burgen und Schloesser" },
	{ "AdrMon NFHAUS",	"Residence",		"Naturfreundehaeuser" },
	{ "AdrMon racing",	"Bike Trail",		"Rennstrecken" },
	{ "AdrMon TNKRST",	"Bar",			"Tankraststaetten" },
	{ "AdrMon tpclub",	"Contact, Biker",	"Motorrad-Clubs" },
	{ "AdrMon tpequ",	NULL,			"Motorrad-Equipment" },
	{ "AdrMon tphot",	"Hotel",		"Motorrad-Hotels" },
	{ "AdrMon tpmh",	NULL,			"Motorradhaendler" },
	{ "AdrMon tpss",	"Restricted Area",	"Sperrungen" },
	{ "AdrMon tpsw",	"Scenic Area",		"Sehenswertes" },
	{ "AdrMon tptref",	NULL,			"Treffpunkte" },
	{ "AdrMon VORTE",	"Information",		"Ortsinformationen" },
	{ "AdrMon WEBCAM",	NULL,			"WebCam-Standorte" },
	{ "AdrMon youthh",	NULL,			"Jugendherbergen" },
	{ "Town",		"City (Small)",		"Orte" },
	{ NULL,			NULL,			NULL, 0 }
};

static void
bcr_handle_icon_str(const char *str, waypoint *wpt)
{
	bcr_icon_mapping_t *m;
	
	wpt->icon_descr = BCR_DEF_MPS_ICON;
	
	for (m = bcr_icon_mapping; (m->bcr_name); m++) {
		if (case_ignore_strcmp(str, m->bcr_name) == 0) {
			int nr;
			
			if (m->symbol_DE == NULL) {
				if (! m->warned) {
					m->warned = 1;
					warning(MYNAME ": Unknown icon \"%s\" found. Please report.\n", str);
				}
				return;
			}
			wpt->description = xstrdup(m->symbol_DE);
			if (m->mps_name != NULL) {
				nr = gt_find_icon_number_from_desc(m->mps_name, MAPSOURCE);
				wpt->icon_descr = gt_find_desc_from_icon_number(nr, MAPSOURCE, NULL);
			}
			return;
		}
	}
}

static char *
get_bcr_icon_from_icon_descr(const char *icon_descr)
{
	char *result = BCR_DEF_ICON;
	
	if (icon_descr) {
		bcr_icon_mapping_t *m;
		
		for (m = bcr_icon_mapping; (m->bcr_name); m++) {
			if (! m->mps_name) continue;
			if (case_ignore_strcmp(icon_descr, m->mps_name) == 0) {
				result = m->bcr_name;
				break;
			}
		}
	}
	return result;
}

static void
bcr_init_radius(void)
{
	if (radius_opt != NULL)				/* preinitialize the earth radius */
	{
		radius = atof(radius_opt);
		if (radius <= 0)
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
	ini = inifile_init(fname, MYNAME);
	bcr_init_radius();
}

static void
bcr_rd_deinit(void)
{
	inifile_done(ini);
	xfree(filename);
}

/* ------------------------------------------------------------*/

static void
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

static void
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

/* ------------------------------------------------------------- */

static void
bcr_data_read(void)
{
	int index;
	char *str;
	route_head *route;
	
	route = route_head_alloc();
	
	if ((str = inifile_readstr(ini, "client", "routename")))
		route->rte_name = xstrdup(str);

	route_add_head(route);

	for (index = 1; index > 0; index ++) {
	
		char station[32];
		char *str;
		int mlat, mlon;		/* mercator data */
		waypoint *wpt;
		
		snprintf(station, sizeof(station), "STATION%d", index);
		if (NULL == (str = inifile_readstr(ini, "coordinates", station))) break;
		
		if (2 != sscanf(str, "%d,%d", &mlon, &mlat))
			fatal(MYNAME ": structure error at %s (Coordinates)!\n", station);
			
		wpt = waypt_new();
		
		wpt->shortname = xstrdup(station);
		bcr_mercator_to_wgs84(mlat, mlon, &wpt->latitude, &wpt->longitude);
		
		if (NULL != (str = inifile_readstr(ini, "client", station)))
		{
			char *cx;
			
			cx = strchr(str, ',');
			if (cx == NULL)
				fatal(MYNAME ": structure error at %s (Client)!\n", station);
			*cx++ = '\0';
			bcr_handle_icon_str(str, wpt);
		}
		
		if (NULL != (str = inifile_readstr(ini, "description", station))) {
			char *c;
			
			c = strchr(str, ',');
			if (c != NULL) *c = '\0';
			if (*str) wpt->notes = xstrdup(str);
			if ((str = c)) {
				str++;
				c = strchr(str, ',');
				if (c != NULL) *c = '\0';
				if (*str) {
					xfree(wpt->shortname);
					wpt->shortname = xstrdup(str);
				}
			}
		}
		
		route_add_wpt(route, wpt);
	}
	
	/* remove empty route */
	if (route->rte_waypt_ct == 0)
		route_del_head(route);
	else
		bcr_create_waypts_from_route(route);
}

/* %%% bcr write support %%% ----------------------------------- */

static void
bcr_wr_init(const char *fname)
{
	filename = xstrdup(fname);
	fout = gbfopen(fname, "wb", MYNAME);
	bcr_init_radius();
}

static void
bcr_wr_deinit(void)
{
	gbfclose(fout);
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

void bcr_write_line(gbfile *fout, const char *key, int *index, const char *value)
{
	if (value == NULL)				/* this is mostly used in the world of windows */
	{						/* so we respectfully add a CR/LF on each line */
		gbfprintf(fout, "%s\r\n", key);
	}
	else
	{
		char *tmp;
		
		tmp = (value != NULL) ? xstrdup(value) : xstrdup("");
		if (index != NULL)
			gbfprintf(fout, "%s%d=%s\r\n", key, *index, tmp);
		else
			gbfprintf(fout, "%s=%s\r\n", key, tmp);
		xfree(tmp);
	}
}

static void 
bcr_route_header(const route_head *route)
{
	queue *elem, *tmp;
	waypoint *wpt;
	char *sout;
	int i, north, east, nmin, nmax, emin, emax;
	
	curr_rte_num++;
	if (curr_rte_num != target_rte_num) return;	
	
	bcr_write_line(fout, "[CLIENT]", NULL, NULL);			/* client section */
	bcr_write_line(fout, "REQUEST", NULL, "TRUE");
	
	sout = route->rte_name;
	if (rtename_opt != 0) sout = rtename_opt;
	if (sout != NULL)
		bcr_write_line(fout, "ROUTENAME", NULL, sout);
	else
		bcr_write_line(fout, "ROUTENAME", NULL, "Route");

	bcr_write_line(fout, "DESCRIPTIONLINES", NULL, "0");
	
	i = 0;
	QUEUE_FOR_EACH(&route->waypoint_list, elem, tmp) 
	{
		char *icon;
		waypoint *wpt = (waypoint *) elem;

		i++;
		
		icon = get_bcr_icon_from_icon_descr(wpt->icon_descr);

		xasprintf(&sout, "%s,%.f", icon, BCR_UNKNOWN);
		bcr_write_line(fout, "STATION", &i, sout);
		xfree(sout);
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
		
		xasprintf(&sout, "%d,%d", east, north);
		bcr_write_line(fout, "STATION", &i, sout);
		xfree(sout);
	}
	
	bcr_write_line(fout, "[DESCRIPTION]", NULL, NULL);		/* descr. section */
	
	i = 0;
	QUEUE_FOR_EACH(&route->waypoint_list, elem, tmp) 
	{
		char *s1, *s2, *sout;
		
		i++;
		wpt = (waypoint *) elem;
		s1 = wpt->notes;
		if (s1 == NULL) s1 = wpt->description;
		
		if (prefer_shortnames_opt || (s1 == NULL) || (*s1 == '\0')) {
			s2 = s1;
			s1 = wpt->shortname;
		}
		else s2 = wpt->shortname;
		
		if (s1 == NULL) s1 = xstrdup("");
		else s1 = csv_stringclean(s1, ",\t\r\n");
		if (s2 == NULL) s2 = xstrdup("");
		else s2 = csv_stringclean(s2, ",\t\r\n");
		
		if (*s2)
			xasprintf(&sout, "%s,%s,@,0", s1, s2);
		else
			xasprintf(&sout, "%s,%s,@,0", s1, s1);
		bcr_write_line(fout, "STATION", &i, sout);
		
		xfree(s1);
		xfree(s2);
		xfree(sout);
	}
	
	bcr_write_line(fout, "[ROUTE]", NULL, NULL);			/* route section */

	xasprintf(&sout, "%d,%d,%d,%d", emin, nmax, emax, nmin);
	bcr_write_line(fout, "ROUTERECT", NULL, sout);
	xfree(sout);
	
}

static void
bcr_data_write(void)
{
	target_rte_num = 1;
		
	if (rtenum_opt != NULL) {
		target_rte_num = atoi(rtenum_opt);
		if (((unsigned)target_rte_num > route_count()) || (target_rte_num < 1))
			fatal(MYNAME ": invalid route number %d (1..%d))!\n", 
				target_rte_num, route_count());
	}
	curr_rte_num = 0;
	route_disp_all(bcr_route_header, bcr_route_trailer, bcr_write_wpt);
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
