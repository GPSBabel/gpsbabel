/*
    Acess to Garmin MapSource files.

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

#include <stdio.h>
#include <string.h>

#include "defs.h"
#include <ctype.h>

static FILE *mapsource_file_in;
static FILE *mapsource_file_out;

static void
mapsource_rd_init(const char *fname)
{
	mapsource_file_in = fopen(fname, "r");
	if (mapsource_file_in == NULL) {
		fatal("Cannot open '%s' for reading\n", fname);
	}
}

static void
mapsource_rd_deinit(void)
{
	fclose(mapsource_file_in);
}

static void
mapsource_wr_init(const char *fname)
{
	mapsource_file_out = fopen(fname, "w");
	if (mapsource_file_out == NULL) {
		fatal("Cannot open '%s' for writing\n", fname);
		exit(1);
	}
}

static void
mapsource_wr_deinit(void)
{
	fclose(mapsource_file_out);
}

static void
mapsource_read(void)
{
	char name[9], date[20], timeb[20];
	char ibuf[200];
	double lat,lon;
	char latdir, londir;
	int latd, lond;
	float latf, lonf;
	int alt; 
	char altunits[10];
	char icon[20];
	waypoint *wpt_tmp;

	while (fgets(ibuf, sizeof(ibuf), mapsource_file_in)) {
		sscanf(ibuf,
		"Waypoint %s %s %s %c%d %f %c%d %f %d %s Symbol & Name %s",
		 name, date, timeb, &latdir, &latd, &latf, &londir, &lond, &lonf, &alt, altunits, icon);
		wpt_tmp = calloc(sizeof(*wpt_tmp),1);
		if (wpt_tmp == NULL) {
			fatal("MAPSOURCE: cannot allocate memory\n");
		}
/* FIXME: Implement actual appropriate conversion */
		wpt_tmp->position.altitude.altitude_meters = alt * 3.0;
		wpt_tmp->shortname = strdup(name);
		wpt_tmp->description = strdup(name);

		lat = latd + latf/100.0;
		lon = lond + lonf/100.0;
		if (latdir == 'S') lat = -lat;
		if (londir == 'W') lon = -lon;
		wpt_tmp->position.latitude.degrees = lat;
		wpt_tmp->position.longitude.degrees = lon;

		waypt_add(wpt_tmp);
	}
}

static void
mapsource_waypt_pr(waypoint *waypointp)
{
	char tbuf[1024];
	char *tp = tbuf;
	int d;
	strftime(tbuf, sizeof(tbuf), "%d-%b-%y %I:%M:%S%p", localtime(&waypointp->creation_time));
	while (*tp) {
		*tp = toupper(*tp);
		tp++;
	}
/* FIXME: cure the sign dependencies here. */
	fprintf(mapsource_file_out, "Waypoint %s %s ", waypointp->shortname, tbuf);
	fprintf(mapsource_file_out, "N%02.0f", waypointp->position.latitude.degrees);
	d = waypointp->position.latitude.degrees;
	fprintf(mapsource_file_out, " %06.3f ", (waypointp->position.latitude.degrees - d) * 100);
	fprintf(mapsource_file_out, "W%02.0f", fabs(waypointp->position.longitude.degrees));
	d = fabs(waypointp->position.longitude.degrees);
	fprintf(mapsource_file_out, " %06.3f ", (fabs(waypointp->position.longitude.degrees) - d) * 100);
	fprintf(mapsource_file_out, "0 ft Symbol & Name Unknown\n");
}

void
mapsource_write(void)
{
	waypt_disp_all(mapsource_waypt_pr);

}

ff_vecs_t mapsource_vecs = {
	mapsource_rd_init,
	mapsource_wr_init,
	mapsource_rd_deinit,
	mapsource_wr_deinit,
	mapsource_read,
	mapsource_write,
};
