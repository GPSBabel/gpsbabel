/*
    DeLorme GPL Track Format.

    Copyright (C) 2003 Robert Lipe, robertlipe@usa.net

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
#include <time.h>
#include <errno.h>
#include <unistd.h>

#include "defs.h"
// #include "magellan.h"

#define MYNAME "GPL"

extern gpsdata_type objective;

typedef struct gpl_point {
	unsigned int status;
	unsigned int dummy1;
	double lat;
	double lon;
	double alt; /* in feet */
	double heading;
	double speed; /* mps */
	unsigned int tm;
	unsigned int dummy3;
} gpl_point_t;

static FILE *gplfile_in;
static FILE *gplfile_out;

static void
gpl_rd_init(const char *fname)
{
	gplfile_in = fopen(fname, "rb");
	if (sizeof(struct gpl_point) != 56) {
		fatal(MYNAME, ": gpl_point is %d instead of 56.\n", 
				sizeof(struct gpl_point));
	}
	if (gplfile_in == NULL) {
		fatal(MYNAME, ": '%s' for reading\n", fname);
	}
}

static void
gpl_read(void)
{
	waypoint *wpt_tmp;
	route_head *track_head;
	int br;
	gpl_point_t gp;

	track_head = route_head_alloc();
	route_add_head(track_head);

	while (!feof(gplfile_in)) {
		if (0 > fread(&gp, sizeof(gp), 1, gplfile_in)) {
			warning(MYNAME, "short data read.\n");
		}
		wpt_tmp = xcalloc(sizeof(*wpt_tmp),1);
		le_read64(&wpt_tmp->latitude, &gp.lat);
		le_read64(&wpt_tmp->longitude, &gp.lon);
		le_read64(&wpt_tmp->altitude, &gp.alt);
		wpt_tmp->creation_time = le_read32(&gp.tm);
		route_add_wpt(track_head, wpt_tmp);
	}
	
}


static void
gpl_rd_deinit(void)
{
	fclose(gplfile_in);
}

static void
gpl_wr_init(const char *fname)
{
	gplfile_out = fopen(fname, "wb");
	if (gplfile_out == NULL) {
		fatal(MYNAME ": Cannot open '%s' for writing\n", fname);
	}
}

static void
gpl_wr_deinit(void)
{
	fclose(gplfile_out);
}

static void
gpl_trackpt(const waypoint *wpt)
{
	gpl_point_t gp = {0};

	le_read64(&gp.lat, &wpt->latitude);
	le_read64(&gp.lon, &wpt->longitude);
	le_read64(&gp.alt, &wpt->altitude);
	le_write32(&gp.tm, wpt->creation_time);

	fwrite(&gp, sizeof(gp), 1, gplfile_out);
}

static void
gpl_write(void)
{
	route_disp_all(NULL, NULL, gpl_trackpt);
#if 0
	/* 
	 * Whitespace is actually legal, but since waypoint name length is
	 * only 8 bytes, we'll conserve them.
	 */
	setshort_whitespace_ok(mkshort_handle, 0);
	switch (global_opts.objective)
	{
		case trkdata:
			mag_track_pr();
			break;
		case wptdata:
			waypt_disp_all(mag_waypt_pr);
			break;
		case rtedata:
			mag_route_pr();
			break;
		default:
			fatal(MYNAME ": Unknown objective.\n");
	}
#endif
}

ff_vecs_t gpl_vecs = {
	gpl_rd_init,	
	gpl_wr_init,	
	gpl_rd_deinit,	
	gpl_wr_deinit,	
	gpl_read,
	gpl_write,
	NULL
};
