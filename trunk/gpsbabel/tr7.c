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
	unsigned char buff[32];
	waypoint *wpt;
	route_head *trk = NULL;
	unsigned int magic;
	
	magic = gbfgetint32(fin);
	if (magic != TR7_TRACK_MAGIC) {
		fatal(MYNAME ": Invalid magic number in header (%X, but %X expected)!\n", magic, TR7_TRACK_MAGIC);
	}

	while (! gbfeof(fin)) {
		gbfread(buff, 1, sizeof(buff), fin);
		if (buff[0] == 0xD8) {
			struct tm tm;
			double lat, lon;

			lat = (double)le_read32(&buff[20]) / 1000000.0;
			lon = (double)le_read32(&buff[16]) / 1000000.0;
			if ((fabs(lat) > 90) || (fabs(lon) > 180)) continue;

			memset(&tm, 0, sizeof(tm));
			tm.tm_hour = buff[8];
			tm.tm_min = buff[10];
			tm.tm_sec = buff[12];

			wpt = waypt_new();

			wpt->latitude = lat;
			wpt->longitude = lon;
			wpt->creation_time = mkgmtime(&tm);

			if (! trk) {
				trk = route_head_alloc();
				track_add_head(trk);
			}
			track_add_wpt(trk, wpt);
		}
	}
}

/**************************************************************************/

ff_vecs_t tr7_vecs = {		/* currently we can only read tracks */
	ff_type_internal,
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
	CET_CHARSET_ASCII, 0

};

/**************************************************************************/
