/*
  Comma separated value files.
  NAME,LONG, LAT.
  
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
#include <ctype.h>

static FILE *file_in;
static FILE *file_out;

#define MYNAME "CSV"

static void
rd_init(const char *fname)
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
wr_init(const char *fname)
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
	char desc[80];
	char *odesc = desc;
	double lat,lon;
	waypoint *wpt_tmp;

	while( fscanf(file_in, "%lf,%lf,%80[^\n]",
			&lat, &lon, desc) > 0) {
		wpt_tmp = calloc(sizeof(*wpt_tmp),1);
		if (wpt_tmp == NULL) {
			fatal(MYNAME ": cannot allocate memory\n");
		}
		while (*odesc == ' ' || *odesc == '\t') {
			odesc++;
		}
		wpt_tmp->shortname = strdup(odesc);
		wpt_tmp->creation_time = time(NULL);

		wpt_tmp->position.longitude.degrees = lon;
		wpt_tmp->position.latitude.degrees = lat;

		waypt_add(wpt_tmp);
	}
}

static void
gpsutil_disp(waypoint *wpt)
{
	double lon,lat;

	lon = wpt->position.longitude.degrees;
	lat = wpt->position.latitude.degrees;

	fprintf(file_out, "%08.5f, %08.5f, %s\n",
		lat,
		lon,
		wpt->description);

}

static void
data_write(void)
{
	waypt_disp_all(gpsutil_disp);
}


ff_vecs_t csv_vecs = {
	rd_init,
	wr_init,
	rd_deinit,
	wr_deinit,
	data_read,
	data_write,
};
