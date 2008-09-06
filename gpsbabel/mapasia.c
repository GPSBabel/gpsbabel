/*

    Support for MapAsia (.tr7) track file format.

    Copyright (C) 2008 Olaf Klein, o.b.klein@gpsbabel.org

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
#include <time.h>
#include "defs.h"
#include "jeeps/gpsmath.h"

#define MYNAME "tr7"

#define TR7_TRACK_MAGIC	0x223eadb

#define TR7_S_YEAR	0
#define TR7_S_MONTH	2
#define TR7_S_DAY	6
#define TR7_S_HOUR	8
#define TR7_S_MIN	10
#define TR7_S_SEC	12
#define TR7_S_LON	16
#define TR7_S_LAT	20
#define TR7_S_SPEED	24
#define TR7_S_COURSE	26
#define TR7_S_VALID	28
#define TR7_S_FIX	29

static
arglist_t tr7_args[] = {
	ARG_TERMINATOR
};

static gbfile *fin;

/*******************************************************************************
* %%%        global callbacks called by gpsbabel main process              %%% *
*******************************************************************************/

static void
tr7_rd_init(const char *fname)
{
	fin = gbfopen_le(fname, "rb", MYNAME);
}

static void 
tr7_rd_deinit(void)
{
	gbfclose(fin);
}

static void
tr7_read(void)
{
	route_head *trk = NULL;
	unsigned int magic;
	waypoint *prev = NULL;
	
	magic = gbfgetint32(fin);
	if (magic != TR7_TRACK_MAGIC) {
		fatal(MYNAME ": Invalid magic number in header (%X, but %X expected)!\n", magic, TR7_TRACK_MAGIC);
	}

	while (! gbfeof(fin)) {
		unsigned char buff[32];
		gbfread(buff, 1, sizeof(buff), fin);
		if (buff[0] == 0xD8) {
			double lat, lon;
			struct tm tm;
			waypoint *wpt;

			memset(&tm, 0, sizeof(tm));

			lat = (double)le_read32(&buff[TR7_S_LAT]) / 1000000.0;
			lon = (double)le_read32(&buff[TR7_S_LON]) / 1000000.0;
			if ((fabs(lat) > 90) || (fabs(lon) > 180)) {
				trk = NULL;	
				continue;
			}

			tm.tm_year = le_read16(&buff[TR7_S_YEAR]) - 1900;
			tm.tm_mon = buff[TR7_S_MONTH] - 1;
			tm.tm_mday = buff[TR7_S_DAY];
			
			tm.tm_hour = buff[TR7_S_HOUR];
			tm.tm_min = buff[TR7_S_MIN];
			tm.tm_sec = buff[TR7_S_SEC];

			wpt = waypt_new();

			wpt->latitude = lat;
			wpt->longitude = lon;
			wpt->creation_time = mkgmtime(&tm);
			WAYPT_SET(wpt, course, 360 - le_read16(&buff[TR7_S_COURSE]));
			WAYPT_SET(wpt, speed, (double)le_read16(&buff[TR7_S_SPEED]) / 3.6);
#if 0
			/* unsure items */
			wpt->fix = buff[TR7_S_FIX];
			if (buff[TR7_S_VALID] != 'A') {
				waypt_free(wpt);
				continue;
			}
#endif
			if (waypt_speed(prev, wpt) > 9999.9) {
				waypt_free(wpt);
				continue;
			}
			
			if (! trk) {
				trk = route_head_alloc();
				track_add_head(trk);
			}
			track_add_wpt(trk, wpt);
			prev = wpt;
		}
	}
}

/**************************************************************************/

ff_vecs_t mapasia_tr7_vecs = {		/* currently we can only read tracks */
	ff_type_file,
	{ 
		ff_cap_none 	/* waypoints */, 
	  	ff_cap_read	/* tracks */, 
	  	ff_cap_none	/* routes */
	},
	tr7_rd_init,	
	NULL,
	tr7_rd_deinit,	
	NULL,
	tr7_read,
	NULL,
	NULL,
	tr7_args,
	CET_CHARSET_UTF8, 1	/* FIXED - CET-REVIEW - */

};

/**************************************************************************/
