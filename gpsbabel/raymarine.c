 /*

    Support for Raymarine Waypoint File (.rwf).

    Copyright (C) 2006 Olaf Klein, o.b.klein@gpsbabel.org

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
    Known format limits:

    	Waypoint name: max. 16 characters
	Routes:        max. 50 waypoints per route

    History:
    	
    	2006/10/30: Initial release (not yet in GPSBabel source tree)
    	2006/11/08: 
*/

#include "defs.h"
#include "inifile.h"
#include "csv_util.h"

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>

typedef unsigned long long guid_t;

static inifile_t *fin;
static gbfile *fout;
static waypoint **depot;
static short_handle hshort;
static int size_of_depot, items_in_depot;
static int rte_index, rte_wpt_index;
static char *sguid;
static char *opt_location;

#define MYNAME "raymarine"

static
arglist_t raymarine_args[] = 
{
	{ "location", &opt_location, "Default location", "New location", ARGTYPE_STRING, ARG_NOMINMAX },
	ARG_TERMINATOR
};

/* from csv_util.c: convert excel time (days since 1900) to time_t and back again */

#define EXCEL_TO_TIMET(a) ((a - 25569.0) * 86400.0)
#define TIMET_TO_EXCEL(a) ((a / 86400.0) + 25569.0)

/* Bitmaps */

typedef struct {
	int id;
	char *name;
} raymarine_symbol_mapping_t;

static raymarine_symbol_mapping_t raymarine_symbols[] = {
	{ 0, "Unknown Symbol 0" },
	{ 1, "Unknown Symbol 1" },
	{ 2, "Unknown Symbol 2" },
	{ 3, "Red Square" },
	{ 4, "Big Fish" },
	{ 5, "Anchor" },
	{ 6, "Smiley" },
	{ 7, "Sad" },
	{ 8, "Red Button" },
	{ 9, "Sailfish" },
	{ 10, "Danger" },
	{ 11, "Attention" },
	{ 12, "Black Square" },
	{ 13, "Intl. Dive Flag" },
	{ 14, "Vessel" },
	{ 15, "Lobster" },
	{ 16, "Buoy" },
	{ 17, "Exclamation" },
	{ 18, "Red X" },
	{ 19, "Check Mark" },
	{ 20, "Black Plus" },
	{ 21, "Black Cross" },
	{ 22, "MOB" },
	{ 23, "Billfish" },
	{ 24, "Bottom Mark" },
	{ 25, "Circle" },
	{ 26, "Diamond" },
	{ 27, "Diamond Quarters" },
	{ 28, "U.S.Dive Flag" },
	{ 29, "Dolphin" },
	{ 30, "Few Fish" },
	{ 31, "Multiple Fish" },
	{ 32, "Many Fish" },
	{ 33, "Single Fish" },
	{ 34, "Small Fish" },
	{ 35, "Marker" },
	{ 36, "Cocktails" },
	{ 37, "Red Box Marker" },
	{ 38, "Reef" },
	{ 39, "Rocks" },
	{ 40, "FishSchool" },
	{ 41, "Seaweed" },
	{ 42, "Shark" },
	{ 43, "Sportfisher" },
	{ 44, "Swimmer" },
	{ 45, "Top Mark" },
	{ 46, "Trawler" },
	{ 47, "Tree" },
	{ 48, "Triangle" },
	{ 49, "Wreck" }
};

#define RAYMARINE_SYMBOL_CT  sizeof(raymarine_symbols) / sizeof(raymarine_symbol_mapping_t)
#define RAYMARINE_STD_SYMBOL 3

static int
find_symbol_num(const char *descr)
{
	if ((descr != NULL) && (*descr)) {
		
		int i;
		raymarine_symbol_mapping_t *a;
		
		a = &raymarine_symbols[0];
		
		for (i = 0; i < RAYMARINE_SYMBOL_CT; i++, a++) {
			if (case_ignore_strcmp(descr, a->name) == 0) return i;
		}
	}
	
	return RAYMARINE_STD_SYMBOL;
}

/* ============================================= */
/* %%%    R A Y M A R I N E   R E A D E R    %%% */
/* ============================================= */

static void
raymarine_rd_init(const char *fname)
{
	fin = inifile_init(fname, MYNAME);
}

static void
raymarine_rd_done(void)
{
	inifile_done(fin);
}

static void
raymarine_read(void)
{
	waypoint *wpt;
	int ix, rx;
	
	/* Read all waypoints */
	
	for (ix = 0; ix < 0x3FFF; ix++) {
		char sect[10];
		char *str, *name, *lat, *lon;
		
		/* built section identifier */
		snprintf(sect, sizeof(sect), "Wp%d", ix);
		
		/* try to read our most expected values */
		if (NULL == (name = inifile_readstr(fin, sect, "Name"))) break;
		if (NULL == (lat = inifile_readstr(fin, sect, "Lat"))) break;
		if (NULL == (lon = inifile_readstr(fin, sect, "Long"))) break;
		
		wpt = waypt_new();
		wpt->shortname = xstrdup(name);
		wpt->latitude = atof(lat);
		wpt->longitude = atof(lon);
		waypt_add(wpt);
		
		/* try to read optional values */
		if (((str = inifile_readstr(fin, sect, "Notes"))) && *str) wpt->notes = xstrdup(str);
		if (((str = inifile_readstr(fin, sect, "Time"))) && *str) wpt->creation_time = EXCEL_TO_TIMET(atof(str));
		if (((str = inifile_readstr(fin, sect, "Bmp"))) && *str) {
			int symbol = atoi(str);
			
			if ((symbol < 3) && (symbol >= RAYMARINE_SYMBOL_CT))
				symbol = RAYMARINE_STD_SYMBOL;
			wpt->icon_descr = raymarine_symbols[symbol].name;
		}
	}
	
	/* Read all routes */
	
	for (rx = 0; rx < 0x3FFF; rx++) {
		char sect[10];
		char *name;
		route_head *rte;
		int wx;

		snprintf(sect, sizeof(sect), "Rt%d", rx);
		if (NULL == (name = inifile_readstr(fin, sect, "Name"))) break;
		
		rte = route_head_alloc();
		rte->rte_name = xstrdup(name);
		route_add_head(rte);
		
		for (wx = 0; wx < 0x3FFF; wx++) {
			char buff[32];
			char *str;
			waypoint * wpt;
			
			snprintf(buff, sizeof(buff), "Mk%d", wx);
			str = inifile_readstr(fin, sect, buff);
			if ((str == NULL) || (*str == '\0')) break;
			
			wpt = find_waypt_by_name(str);
			if (wpt == NULL)
				fatal(MYNAME ": No associated waypoint for route point %s (Route %s)!\n",
					str, rte->rte_name);
					
			route_add_wpt(rte, waypt_dupe(wpt));
		}
	}
}

/* ============================================= */
/* %%%    R A Y M A R I N E   W R I T E R    %%% */
/* ============================================= */


static guid_t
mkGUID(void)
{
	guid_t res;
	
	if (gpsbabel_now != 0) {
		srand(gpsbabel_now + rand());
		res = ((guid_t) (gpsbabel_now) << 48) | 
			((guid_t)(rand() & 0xFFFF) << 32) | 
			((guid_t)(rand() & 0xFFFF) << 16) | 
			(rand() & 0xFFFF);
	}
	else res = 0;
	
	return res;
}

static char *
GUID2str(guid_t GUID)
{
	gbuint16 w0, w1, w2, w3;
	char *res;
	
	w0 = GUID & 0xFFFF; GUID = GUID >> 16;
	w1 = GUID & 0xFFFF; GUID = GUID >> 16;
	w2 = GUID & 0xFFFF; GUID = GUID >> 16;
	w3 = GUID & 0xFFFF;
	
	xasprintf(&res, "%d-%d-%d-%d", w0, w1, w2, w3);
	return res;
}


static void
register_waypoint(const waypoint *wpt)
{
	int i;
	
	for (i = 0; i < items_in_depot; i++) {
		waypoint *cmp = depot[i];
		if ((strcmp(wpt->shortname, cmp->shortname) == 0) &&
		    (wpt->latitude == cmp->latitude) &&
		    (wpt->longitude == cmp->longitude))
			return;
	}
	
	if (items_in_depot >= size_of_depot) {
		size_of_depot+=16;
		depot = (void *) xrealloc(depot, size_of_depot * sizeof(wpt));
	}
	
	depot[items_in_depot] = (waypoint *)wpt;
	items_in_depot++;
}

static void
write_waypoint(gbfile *fout, const waypoint *wpt, const int waypt_no, const char *location, const char *GUID)
{
	char *notes;
	char *temp;
	double time;
		
	/* ToDo: remove possible line-breaks from notes */
	
	notes = (wpt->notes != NULL) ? wpt->notes : "";
	time = (wpt->creation_time != 0) ? TIMET_TO_EXCEL(wpt->creation_time) : TIMET_TO_EXCEL(gpsbabel_time);
	
	temp = mkshort(hshort, wpt->shortname);
	gbfprintf(fout, "[Wp%d]\n"
			"Loc=%s\n"
			"Name=%s\n"
			"Lat=%.15f\n"
			"Long=%.15f\n",
		waypt_no, location, temp, wpt->latitude, wpt->longitude
	);
	xfree(temp);
	gbfprintf(fout, "Rng=%.15f\n"
			"Bear=%.15f\n"
			"Bmp=%d\n"
			"Fixed=1\n"
			"Locked=0\n"
			"Notes=%s\n",
		0.0, 0.0, 
		find_symbol_num(wpt->icon_descr), 
		notes
	);
	gbfprintf(fout, "Rel=\n"
			"RelSet=1\n"
			"RcCount=1\n"
			"RcRadius=%.15f\n"
			"Show=1\n"
			"RcShow=0\n"
			"SeaTemp=%.15f\n"
			"Depth=%.15f\n"
			"Time=%.15f\n"
			"GUID=%s\n",
		0.0, -32678.0, 65535.0, time, GUID
	);
}

static void
write_route_head_cb(const route_head *rte)
{
	gbfprintf(fout, "[Rt%d]\n"
			"Name=%s\n"
			"Visible=1\n"
			"Guid=%s\n",
		rte_index,
		rte->rte_name,
		sguid
	);
	rte_index++;
	rte_wpt_index = 0;
}

static void
write_route_wpt_cb(const waypoint *wpt)
{
	int i;
	char *s;
	static char *items[] = {
		"Cog",
		"Eta",
		"Length"
		"PredictedDrift",
		"PredictedSet",
		"PredictedSog",
		"PredictedTime",
		"PredictedTwa2",
		"PredictedTwd",
		"PredictedTws" };
		
	s = mkshort(hshort, wpt->shortname);
	gbfprintf(fout, "Mk%d=%s\n", rte_wpt_index, wpt->shortname);
	xfree(s);

	for (i = 0; i < sizeof(items) / sizeof(char *); i++) {
		gbfprintf(fout, "%s%d=%.15f\n", items[i], rte_wpt_index, 0.0);
	}
	rte_wpt_index++;
}

static void
enum_route_hdr_cb(const route_head *rte)
{
	is_fatal(rte->rte_waypt_ct > 50,
		MYNAME ": Routes with more than 50 points are not supported by Waymarine!");
}

static void
enum_route_wpt_cb(const waypoint *wpt)
{
	register_waypoint(wpt);
}

static void
raymarine_wr_init(const char *fname)
{
	fout = gbfopen(fname, "w", MYNAME);

	hshort = mkshort_new_handle();
	setshort_length(hshort, 16);

	setshort_badchars(hshort, ",");
	setshort_mustupper(hshort, 0);
	setshort_mustuniq(hshort, 1);
	setshort_whitespace_ok(hshort, 1);
	setshort_repeating_whitespace_ok(hshort, 1);
}

static void
raymarine_wr_done(void)
{
	mkshort_del_handle(&hshort);
	gbfclose(fout);
}

static void
raymarine_write(void)
{
	int i;
	queue *elem, *tmp;
	extern queue waypt_head;
	guid_t guid;
	
	size_of_depot = 0;
	items_in_depot = 0;
	depot = NULL;
	guid = mkGUID();
	sguid = GUID2str(guid);

	/* enumerate all possible waypoints */
	QUEUE_FOR_EACH(&waypt_head, elem, tmp) {
		waypoint *wpt = (waypoint *) elem;
		register_waypoint(wpt);
	}
	route_disp_all(enum_route_hdr_cb, NULL, enum_route_wpt_cb);
	
	/* write out waypoint summary */
	for (i = 0; i < items_in_depot; i++) {
		waypoint *wpt = depot[i];
		write_waypoint(fout, wpt, i, opt_location, sguid);
	}
	
	/* write out all routes with their waypoints */
	rte_index = 0;
	route_disp_all(write_route_head_cb, NULL, write_route_wpt_cb);
	
	if (depot != NULL) xfree(depot);
	xfree(sguid);
}

/* ================================================== */
/* %%%    M O D U L E   R E G I S T R A T I O N   %%% */
/* ================================================== */

ff_vecs_t raymarine_vecs = {
	ff_type_file,
	{ 
		ff_cap_read | ff_cap_write	/* waypoints */, 
		ff_cap_none 			/* tracks */, 
	  	ff_cap_read | ff_cap_write 	/* routes */, 
	},
	raymarine_rd_init,
	raymarine_wr_init,
	raymarine_rd_done,
	raymarine_wr_done,
	raymarine_read,
	raymarine_write,
	NULL,
	raymarine_args,
	CET_CHARSET_ASCII, 0
};
