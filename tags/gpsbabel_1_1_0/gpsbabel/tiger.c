/*
    Access to  U.S. Census Bureau "tiger" format.

    Copyright (C) 2002 Robert Lipe, robertlipe@usa.net

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
#include "magellan.h"

static FILE *file_in;
static FILE *file_out;

#define MYNAME "GPSUTIL"

static void
rd_init(const char *fname, const char *args)
{
	file_in = fopen(fname, "r");
	if (file_in == NULL) {
		fatal(MYNAME ": Cannot open %s for reading\n", fname);
	}
}

static void
rd_deinit(void)
{
	fclose(file_in);
}

static void
wr_init(const char *fname, const char *args)
{
	file_out = fopen(fname, "w");
	if (file_out == NULL) {
		fatal(MYNAME ": Cannot open %s for writing\n", fname);
	}
}

static void
wr_deinit(void)
{
	fclose(file_out);
}

static void
data_read(void)
{
	double lat,lon;
	char desc[100];
	char icon[100];
	waypoint *wpt_tmp;

	while( fscanf(file_in, "%lf,%lf:%100[^:]:%100[^\n]", 
			&lon, &lat, icon, desc) > 0) {
		wpt_tmp = xcalloc(sizeof (*wpt_tmp), 1);

		wpt_tmp->position.longitude.degrees = lon;
		wpt_tmp->position.latitude.degrees = lat;
		wpt_tmp->description = xstrdup(desc);
		wpt_tmp->shortname = mkshort(desc);

		waypt_add(wpt_tmp);
	}
}

static void
gpsutil_disp(const waypoint *wpt)
{
	fprintf(file_out, "%f,%f:redpin:%s\n", 
		wpt->position.longitude.degrees,
		 wpt->position.latitude.degrees,
		 wpt->description);
}

static void
data_write(void)
{
	waypt_disp_all(gpsutil_disp);
}


ff_vecs_t tiger_vecs = {
	rd_init,
	wr_init,
	rd_deinit,
	wr_deinit,
	data_read,
	data_write,
};
