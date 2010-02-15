/* 

	Support for XML files from jogmap.de

	Copyright (C) 2009 Robert Lipe  robertlipe@gpsbabel.org

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
#include "jeeps/gpsmath.h"
#include "garmin_tables.h"

static route_head *trk;

static arglist_t jogmap_args[] = 
{
	ARG_TERMINATOR
};

#define MYNAME "xol"

#if ! HAVE_LIBEXPAT
void
jogmap_rd_init(const char *fname)
{
	fatal(MYNAME ": This build excluded \"" MYNAME "\" support because expat was not installed.\n");
}

void
jogmap_read(void)
{
}

#else

// static xg_callback	jogmap_shape, xol_shape_end;
// static xg_callback	jogmap_waypt, xol_overlay;

#define XOL "/overlays/overlay"



static void
jogmap_markers(const char *args, const char **attrv)
{
	trk = route_head_alloc();
	track_add_head(trk);
}

static void
jogmap_marker(const char *args, const char **attrv)
{
	const char **avp = &attrv[0];
	waypoint *wpt = waypt_new();

        while (*avp) {
		if (strcmp(avp[0], "lat") == 0) {
			sscanf(avp[1], "%lf", 
				&wpt->latitude);
		} else
		if (strcmp(avp[0], "lng") == 0) {
			sscanf(avp[1], "%lf", 
				&wpt->longitude);
		}

		avp+=2;
	}

	if (trk) track_add_wpt(trk, wpt);
}

static
xg_tag_mapping jogmap_map[] = {
	{ jogmap_markers,	cb_start,	"/markers" },
	{ jogmap_marker,	cb_start,	"/markers/marker" },
	{ NULL,			0,		NULL }
};

static void 
jogmap_rd_init(const char *fname)
{
	trk = NULL;
	xml_init(fname, jogmap_map, NULL);
}

static void 
jogmap_read(void)
{
	xml_read();
}

#endif

static void 
jogmap_rd_deinit(void)
{
	xml_deinit();
}

ff_vecs_t jogmap_vecs = {
	ff_type_file,
	{ ff_cap_none,		/* waypoints */
	  ff_cap_read,		/* tracks */
	  ff_cap_none },	/* routes */
	jogmap_rd_init,	
	NULL,	
	jogmap_rd_deinit,
	NULL,
	jogmap_read,
	NULL,
	NULL, 
	jogmap_args,
	CET_CHARSET_UTF8, 0
};
