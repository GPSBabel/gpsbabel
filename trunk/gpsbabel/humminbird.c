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

#include "defs.h"
#include <ctype.h>

#define MYNAME "humminbird"

#define EAST_SCALE       20038297.0 /* this is i1924_equ_axis*M_PI */
#define i1924_equ_axis   6378388.0
#define i1924_polar_axis 6356911.946

static
arglist_t humminbird_args[] = {
//	{"foo", &fooopt, "The text of the foo option in help",
//	 "default", ARGYTPE_STRING, ARG_NOMINMAX},
	ARG_TERMINATOR
};

/* The hwr data format is records-based, and the records are 36 bytes long. */

typedef struct humminbird_waypt_s {
        gbuint32 signature;    /* Just for error checking(?) */
        gbuint16 num;          /* Always ascending in the file. */
        gbuint16 zero;         /* Always seems to be zero. */
        gbuint8  status;       /* Always seems to be 1 */
        gbuint8  icon;         /* See below */
        gbuint16 depth;        /* Water depth. These are fishfinders. In centimeters */
        gbuint32 time;         /* This is a time_t. In UTC */
        gbint32  east;
        gbint32  north;
        char     name[12];
} humminbird_waypt_t;

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
static int waypoint_num = 0;

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
	fin = gbfopen(fname, "r", MYNAME);
}

static void 
humminbird_rd_deinit(void)
{
	gbfclose(fin);
}

static void
humminbird_read(void)
{
	while(! gbfeof(fin)) {
		humminbird_waypt_t w;
		double guder;
		int bytes, num_icons;
		waypoint *wpt;

		bytes = gbfread(&w, 1, sizeof(humminbird_waypt_t), fin);
		if(bytes == 0) break;

		is_fatal((bytes != sizeof(humminbird_waypt_t)),
		         MYNAME ": Unexpected end of file (%d)!",
		         (int) sizeof(humminbird_waypt_t) - bytes);

		/* Fix endianness - these are now BE */
		w.signature = be_read32(&w.signature);
		w.num       = be_read16(&w.num);
		w.zero      = be_read16(&w.zero);
		w.depth     = be_read16(&w.depth);
		w.time      = be_read32(&w.time);
		w.north     = be_read32(&w.north);
		w.east      = be_read32(&w.east);

		is_fatal(( w.signature != 0x02020024L ),
		         MYNAME ": Invalid record header (no or unknown humminbird file)!");
		
		/* All right! Copy the data to the gpsbabel struct... */
		
		wpt = waypt_new();

		wpt->shortname = xstrndup(w.name, sizeof(w.name));
		wpt->creation_time = w.time;

		guder = gudermannian_i1924(w.north);
		wpt->latitude = geocentric_to_geodetic_hwr(guder);
		wpt->longitude = (double)w.east / EAST_SCALE * 180.0;

		wpt->altitude  = 0.0; /* It's from a fishfinder... */
		
		if(w.depth != 0)
			WAYPT_SET(wpt,depth,(double)w.depth / 100.0);

		num_icons = sizeof(humminbird_icons) / sizeof(humminbird_icons[0]);
		if(w.icon < num_icons)
			wpt->icon_descr = humminbird_icons[w.icon];

		waypt_add(wpt);
	}
}

static void
humminbird_wr_init(const char *fname)
{
	fout = gbfopen(fname, "w", MYNAME);
}

static void
humminbird_wr_deinit(void)
{
	gbfclose(fout);
}

static void
humminbird_write_waypoint(const waypoint *wpt) {
	humminbird_waypt_t hum;
	double lon, north, east;
	int i;
	int num_icons = sizeof(humminbird_icons) / sizeof(humminbird_icons[0]);

	be_write32(&hum.signature, 0x02020024L);
	be_write16(&hum.num, waypoint_num++);
	hum.zero   = 0;
	hum.status = 1;
	hum.icon   = 0;
	
	// Icon....
	if(wpt->icon_descr) {
		for(i=0 ; i<num_icons ; i++) {
			if(!strcmp(wpt->icon_descr, humminbird_icons[i])) {
				hum.icon = i;
				break;
			}
		}
	}
	hum.depth = round(WAYPT_GET(wpt, depth, 0)*100.0);
	be_write16(&hum.depth,  hum.depth);
	
	be_write32(&hum.time, wpt->creation_time);
	
	east = wpt->longitude / 180.0 * EAST_SCALE;
	be_write32(&hum.east, (gbint32)round((east)));

	lon = geodetic_to_geocentric_hwr(wpt->latitude);
	north = inverse_gudermannian_i1924(lon);
	be_write32(&hum.north, (gbint32)round(north));

	strncpy((char *)&hum.name, wpt->shortname, 12);
	gbfwrite(&hum, sizeof(hum), 1, fout);
}

static void
humminbird_write(void)
{
	waypoint_num = 0;
	waypt_disp_all(humminbird_write_waypoint);
}

/**************************************************************************/

// capabilities below means: we can only read and write waypoints
ff_vecs_t humminbird_vecs = {
	ff_type_file,
	{ 
		ff_cap_read | ff_cap_write 	/* waypoints */, 
	  	ff_cap_none 			/* tracks */, 
	  	ff_cap_none 			/* routes */
	},
	humminbird_rd_init,	
	humminbird_wr_init,	
	humminbird_rd_deinit,	
	humminbird_wr_deinit,	
	humminbird_read,
	humminbird_write,
	NULL, // humminbird_exit,
	humminbird_args,
	CET_CHARSET_ASCII, 0			/* ascii is the expected character set */
						/* not fixed, can be changed through command line parameter */
};
/**************************************************************************/
