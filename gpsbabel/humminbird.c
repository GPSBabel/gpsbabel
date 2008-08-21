/*
    Copyright (C) 2008  Björn Augustsson, oggust@gmail.com
    Copyright (C) 2008  Olaf Klein, o.b.klein@gpsbabel.org
    Copyright (C) 2005  Robert Lipe, robertlipe@usa.net

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
#include <math.h>
#include <string.h>
#include "defs.h"
#include "avltree.h"

#define MYNAME "humminbird"

#define WPT_NAME_LEN		12
#define RTE_NAME_LEN		20
#define MAX_RTE_POINTS		50

#define WPT_MAGIC		0x02020024L
#define RTE_MAGIC		0x03030088L

#define EAST_SCALE		20038297.0 /* this is i1924_equ_axis*M_PI */
#define i1924_equ_axis		6378388.0
#define i1924_polar_axis	6356911.946

#define BAD_CHARS		"\r\n\t"

/* The hwr data format is records-based, and the records are 36 bytes long. */

typedef struct humminbird_waypt_s {
	/* O.K.: the file can also contain routes with a different magic. */
	/* gbuint32 signature; */   /* Just for error checking(?) */
	gbuint16 num;          /* Always ascending in the file. */
	gbuint16 zero;         /* Always seems to be zero. */
	gbuint8  status;       /* Always seems to be 1 */
	gbuint8  icon;         /* See below */
	gbuint16 depth;        /* Water depth. These are fishfinders. In centimeters */
	gbuint32 time;         /* This is a time_t. In UTC */
	gbint32  east;
	gbint32  north;
	char     name[WPT_NAME_LEN];
} humminbird_waypt_t;

typedef struct humminbird_rte_s {
	/* O.K.: the file can contain also routes with a different magic. */
	/* gbuint32 signature; */   /* Just for error checking(?) */
	gbuint16 num;
	gbuint16 zero;
	gbuint8  status;
	gbuint8  U0;
	gbuint8  U1;
	gbint8   count;
	gbuint32 time;
	char     name[RTE_NAME_LEN];
	gbuint16 points[MAX_RTE_POINTS];
} humminbird_rte_t;

static const char* humminbird_icons[] = {
	"Normal",       /*  0 */
	"House",        /*  1 */
	"Red cross",    /*  2 */
	"Fish",         /*  3 */
	"Duck",         /*  4 */
	"Anchor",       /*  5 */
	"Buoy",         /*  6 */
	"Airport",      /*  7 */
	"Camping",      /*  8 */
	"Danger",       /*  9 */
	"Petrol",       /* 10 */
	"Rock",         /* 11 */
	"Weed",         /* 12 */
	"Wreck",        /* 13 */
	"Phone",        /* 14 */
	"Coffee",       /* 15 */
	"Beer",         /* 16 */
	"Mooring",      /* 17 */
	"Pier",         /* 18 */
	"Slip",         /* 19 */
	"Ramp",         /* 20 */
	"Circle",       /* 21 */
	"Diamond",      /* 22 */
	"Flag",         /* 23 */
	"Pattern",      /* 24 */
	"Shower",       /* 25 */
	"Water tap",    /* 26 */
	"Tree",         /* 27 */
	"Recording",    /* 28 */
	"Snapshot"      /* 29 */
};

static gbfile* fin;
static gbfile* fout;
static int waypoint_num;
static short_handle wptname_sh, rtename_sh;
static avltree_t *waypoints;
static humminbird_rte_t *humrte;
static int rte_num;

static
arglist_t humminbird_args[] = {
	ARG_TERMINATOR
};

/* Takes a latitude in degrees,
 * returns a latitude in degrees. */
static double
geodetic_to_geocentric_hwr(const double gd_lat) {
	const double cos_ae = 0.9966349016452;
        const double cos2_ae = cos_ae * cos_ae;
        const double gdr = gd_lat *M_PI / 180.0;

        return atan(cos2_ae * tan(gdr)) * 180.0/M_PI;
}

/* Takes a latitude in degrees,
 * returns a latitude in degrees. */
static double
geocentric_to_geodetic_hwr(const double gc_lat) {
	const double cos_ae = 0.9966349016452;
	const double cos2_ae = cos_ae * cos_ae;
	const double gcr = gc_lat *M_PI / 180.0;

	return atan( tan(gcr)/cos2_ae ) * 180.0/M_PI;
}

/* Takes a projected "north" value, returns latitude in degrees. */
static double
gudermannian_i1924(const double x) {
	const double norm_x = x/i1924_equ_axis;

        return atan(sinh(norm_x)) * 180.0/M_PI;
}

/* Takes latitude in degrees, returns projected "north" value. */
static double
inverse_gudermannian_i1924(const double x) {
        const double x_r = x/180.0 * M_PI;
        const double guder = log(tan(M_PI/4.0 + x_r/2.0));

        return guder * i1924_equ_axis;
}

/*******************************************************************************
* %%%        global callbacks called by gpsbabel main process              %%% *
*******************************************************************************/

static void
humminbird_rd_init(const char *fname)
{
	fin = gbfopen_be(fname, "rb", MYNAME);
	waypoints = avltree_init(0, MYNAME);
}

static void 
humminbird_rd_deinit(void)
{
	avltree_done(waypoints);
	gbfclose(fin);
}

static void
humminbird_read(void)
{
	while(! gbfeof(fin)) {
		gbuint32 signature;

		signature = gbfgetuint32(fin);
		
		if (signature == WPT_MAGIC) { /* a waypoint */
			humminbird_waypt_t w;
			double guder;
			int num_icons;
			waypoint *wpt;
			char buff[10];

			if (! gbfread(&w, 1, sizeof(w), fin))
				fatal(MYNAME ": Unexpected end of file!\n");

			/* Fix endianness - these are now BE */
			w.num = be_read16(&w.num);
			w.zero = be_read16(&w.zero);
			w.depth = be_read16(&w.depth);
			w.time = be_read32(&w.time);
			w.north = be_read32(&w.north);
			w.east = be_read32(&w.east);

			/* All right! Copy the data to the gpsbabel struct... */

			wpt = waypt_new();

			wpt->shortname = xstrndup(w.name, sizeof(w.name));
			wpt->creation_time = w.time;

			guder = gudermannian_i1924(w.north);
			wpt->latitude = geocentric_to_geodetic_hwr(guder);
			wpt->longitude = (double)w.east / EAST_SCALE * 180.0;

			wpt->altitude  = 0.0; /* It's from a fishfinder... */
		
			if (w.depth != 0)
				WAYPT_SET(wpt,depth,(double)w.depth / 100.0);

			num_icons = sizeof(humminbird_icons) / sizeof(humminbird_icons[0]);
			if (w.icon < num_icons)
				wpt->icon_descr = humminbird_icons[w.icon];

			waypt_add(wpt);
			
			/* register the point over his internal Humminbird "Number" */
			snprintf(buff, sizeof(buff), "%d", w.num);
			avltree_insert(waypoints, buff, wpt);

		}
		else if (signature == RTE_MAGIC) { /* here comes a route */
			humminbird_rte_t hrte;

			if (! gbfread(&hrte, 1, sizeof(hrte), fin))
				fatal(MYNAME ": Unexpected end of file!\n");

			hrte.time = be_read32(&hrte.time);
			hrte.num = be_read16(&hrte.num);

			if (hrte.count > 0) {
				int i;
				route_head *rte = NULL;

				for (i = 0; i < hrte.count; i++) {
					waypoint *wpt;
					char buff[10];
					hrte.points[i] = be_read16(&hrte.points[i]);
					
					/* locate the point over his internal Humminbird "Number" */
					snprintf(buff, sizeof(buff), "%d", hrte.points[i]);
					if (avltree_find(waypoints, buff, (void *) &wpt)) {
						if (rte == NULL) {
							rte = route_head_alloc();
							route_add_head(rte);
							rte->rte_name = xstrndup(hrte.name, sizeof(hrte.name));
							/* rte->rte_num = hrte.num + 1; only internal number */
						}
						route_add_wpt(rte, waypt_dupe(wpt));
					}
				}
			}
		}
		else
			fatal(MYNAME ": Invalid record header (no or unknown humminbird file)!\n");

	}
}


/************************************************************************************************/

static void
humminbird_wr_init(const char *fname)
{
	fout = gbfopen_be(fname, "wb", MYNAME);

	wptname_sh = mkshort_new_handle();

	setshort_length(wptname_sh, WPT_NAME_LEN - 1);
	setshort_badchars(wptname_sh, BAD_CHARS);
	setshort_mustupper(wptname_sh, 0);
	setshort_mustuniq(wptname_sh, 0);
	setshort_whitespace_ok(wptname_sh, 1);
	setshort_repeating_whitespace_ok(wptname_sh, 1);
	setshort_defname(wptname_sh, "WPT");

	rtename_sh = mkshort_new_handle();
	setshort_length(rtename_sh, RTE_NAME_LEN - 1);
	setshort_badchars(rtename_sh, BAD_CHARS);
	setshort_mustupper(rtename_sh, 0);
	setshort_mustuniq(rtename_sh, 0);
	setshort_whitespace_ok(rtename_sh, 1);
	setshort_repeating_whitespace_ok(rtename_sh, 1);
	setshort_defname(rtename_sh, "Route");

	waypoints = avltree_init(0, MYNAME);

	waypoint_num = 0;
	rte_num = 0;
}

static void
humminbird_wr_deinit(void)
{
	avltree_done(waypoints);
	mkshort_del_handle(&wptname_sh);
	mkshort_del_handle(&rtename_sh);
	gbfclose(fout);
}

static void
humminbird_write_waypoint(const waypoint *wpt) {
	humminbird_waypt_t hum;
	double lon, north, east;
	int i;
	int num_icons = sizeof(humminbird_icons) / sizeof(humminbird_icons[0]);
	char *name;

	be_write16(&hum.num, waypoint_num++);
	hum.zero   = 0;
	hum.status = 1;
	hum.icon   = 255;

	// Icon....
	if (wpt->icon_descr) {
		for (i = 0; i < num_icons; i++) {
			if (!case_ignore_strcmp(wpt->icon_descr, humminbird_icons[i])) {
				hum.icon = i;
				break;
			}
		}
		if (hum.icon == 255) {	/* no success, no try to find the item in a more comlex name */
			hum.icon = 0;	/* i.e. "Diamond" as part of "Diamond, Green" or "Green Diamond" */
			for (i = 0; i < num_icons; i++) {
				char *match;
				int j;
				xasprintf(&match, "*%s*", humminbird_icons[i]);
				j = case_ignore_str_match(wpt->icon_descr, match);
				xfree(match);
				if (j != 0) {
					hum.icon = i;
					break;
				}
			}
		}
	}

	hum.depth = si_round(WAYPT_GET(wpt, depth, 0)*100.0);
	be_write16(&hum.depth, hum.depth);

	be_write32(&hum.time, wpt->creation_time);

	east = wpt->longitude / 180.0 * EAST_SCALE;
	be_write32(&hum.east, si_round((east)));

	lon = geodetic_to_geocentric_hwr(wpt->latitude);
	north = inverse_gudermannian_i1924(lon);
	be_write32(&hum.north, si_round(north));

	name = (global_opts.synthesize_shortnames) 
		? mkshort_from_wpt(wptname_sh, wpt) 
		: mkshort(wptname_sh, wpt->shortname);
	memset(&hum.name, 0, sizeof(hum.name));
	memcpy(&hum.name, name, strlen(name));
	xfree(name);

	gbfputuint32(WPT_MAGIC, fout);
	gbfwrite(&hum, sizeof(hum), 1, fout);
}

static void
humminbird_rte_head(const route_head *rte)
{
	humrte = NULL;
	if (rte->rte_waypt_ct > 0)
		humrte = xcalloc(1, sizeof(*humrte));
}

static void
humminbird_rte_tail(const route_head *rte)
{
	if (humrte == NULL) return;
	
	if (humrte->count > 0) {
		int i;
		char *name;
		
		humrte->num = rte_num++;
		humrte->time = gpsbabel_time;
		for (i = 0; i < humrte->count; i++)
			be_write16(&humrte->points[i], humrte->points[i]);

		be_write16(&humrte->num, humrte->num);
		be_write32(&humrte->time, humrte->time);

		name = mkshort(rtename_sh, rte->rte_name);
		strncpy(humrte->name, name, sizeof(humrte->name));
		xfree(name);

		gbfputuint32(RTE_MAGIC, fout);
		gbfwrite(humrte, sizeof(*humrte), 1, fout);
	}

	xfree(humrte);
	humrte = NULL;
}

static void
humminbird_write_rtept(const waypoint *wpt)
{
	int i;

	if (humrte == NULL) return;
	i = gb_ptr2int(wpt->extra_data);
	if (i <= 0) return;
	
	if (humrte->count < MAX_RTE_POINTS) {
		humrte->points[humrte->count] = i - 1;
		humrte->count++;
	}
	else {
		warning(MYNAME ": Sorry, routes are limited to %d points!\n", MAX_RTE_POINTS);
		fatal(MYNAME ": You can use our simplify filter to reduce the number of route points.\n");
	}
}

static void
humminbird_write_waypoint_wrapper(const waypoint *wpt)
{
	char *key;
	waypoint *tmpwpt;

	xasprintf(&key, "%s\01%.9f\01\%.9f", wpt->shortname, wpt->latitude, wpt->longitude);

	if (! avltree_find(waypoints, key, (void *)&tmpwpt)) {
		tmpwpt = (waypoint *)wpt;

		avltree_insert(waypoints, key, wpt);

		tmpwpt->extra_data = gb_int2ptr(waypoint_num + 1);	/* NOT NULL */
		humminbird_write_waypoint(wpt);
	}
	else {
		void *p = tmpwpt->extra_data;
		tmpwpt = (waypoint *)wpt;
		tmpwpt->extra_data = p;
	}

	xfree(key);
}

static void
humminbird_write(void)
{
	waypt_disp_all(humminbird_write_waypoint_wrapper);
	route_disp_all(NULL, NULL, humminbird_write_waypoint_wrapper);
	route_disp_all(humminbird_rte_head, humminbird_rte_tail, humminbird_write_rtept);
}

/**************************************************************************/

// capabilities below means: we can only read and write waypoints

ff_vecs_t humminbird_vecs = {
	ff_type_file,
	{ 
		ff_cap_read | ff_cap_write 	/* waypoints */,
		ff_cap_none 			/* tracks */,
		ff_cap_read | ff_cap_write	/* routes */
	},
	humminbird_rd_init,
	humminbird_wr_init,
	humminbird_rd_deinit,
	humminbird_wr_deinit,
	humminbird_read,
	humminbird_write,
	NULL, // humminbird_exit,
	humminbird_args,
	CET_CHARSET_ASCII, 1			/* ascii is the expected character set */
						/* currently fixed !!! */
};

/**************************************************************************/
