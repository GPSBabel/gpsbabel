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

#include "defs.h"

#define MYNAME "GPL"

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
	gplfile_in = xfopen(fname, "rb", MYNAME);
	if (sizeof(struct gpl_point) != 56) {
		fatal(MYNAME, ": gpl_point is %d instead of 56.\n", 
				sizeof(struct gpl_point));
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
	track_add_head(track_head);

	while (fread(&gp, sizeof(gp), 1, gplfile_in) > 0) {
		wpt_tmp = waypt_new();
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
	gplfile_out = xfopen(fname, "wb", MYNAME);
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
	track_disp_all(NULL, NULL, gpl_trackpt);
}

ff_vecs_t gpl_vecs = {
	ff_type_file,
	gpl_rd_init,	
	gpl_wr_init,	
	gpl_rd_deinit,	
	gpl_wr_deinit,	
	gpl_read,
	gpl_write,
	NULL
};
