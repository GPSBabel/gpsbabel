/*
    Access Magellan Mapsend files.

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
#include "mapsend.h"

static FILE *mapsend_file_in;
static FILE *mapsend_file_out;

static void
mapsend_rd_init(const char *fname)
{
	mapsend_file_in = fopen(fname, "r");
	if (mapsend_file_in == NULL) {
		fprintf(stderr, "Cannot open '%s' for reading\n", fname);
		exit(1);
	}
}

static void
mapsend_rd_deinit(void)
{
	fclose(mapsend_file_in);
}

static void
mapsend_wr_init(const char *fname)
{
	mapsend_file_out = fopen(fname, "w");
	if (mapsend_file_out == NULL) {
		fprintf(stderr, "Cannot open '%s' for writing\n", fname);
		exit(1);
	}
}

static void
mapsend_wr_deinit(void)
{
	fclose(mapsend_file_out);
}

static void
mapsend_read(void)
{
	mapsend_hdr hdr;
	char tbuf[256];
	char name[257];
	char comment[257];
	char *p;
	int wpt_count;
	unsigned char scount;
	int wpt_number;
	char wpt_icon;
	char wpt_status;
	double wpt_alt;
	double wpt_long;
	double wpt_lat;
	waypoint *wpt_tmp;

	/*
	 * Becuase of the silly struct packing and the goofy variable-length
	 * strings, each member has to be read in one at a time.  Grrr.
	 */

	fread(&hdr, sizeof(hdr), 1, mapsend_file_in);

	fread(&wpt_count, sizeof(wpt_count), 1, mapsend_file_in);

	while (wpt_count--) {
		wpt_tmp = calloc(sizeof(*wpt_tmp), 1);

		fread(&scount, sizeof(scount), 1, mapsend_file_in);
		fread(&tbuf, scount, 1, mapsend_file_in);
		p = strncpy(name, tbuf, scount);
		p[scount] = '\0';

		fread(&scount, sizeof(scount), 1, mapsend_file_in);
		fread(&tbuf, scount, 1, mapsend_file_in);
		p = strncpy(comment, tbuf, scount);
		p[scount] = '\0';

		fread(&wpt_number, sizeof(wpt_number), 1, mapsend_file_in);
		fread(&wpt_icon, sizeof(wpt_icon), 1, mapsend_file_in);
		fread(&wpt_status, sizeof(wpt_status), 1, mapsend_file_in);
		fread(&wpt_alt, sizeof(wpt_alt), 1, mapsend_file_in);
		fread(&wpt_long, sizeof(wpt_long), 1, mapsend_file_in);
		fread(&wpt_lat, sizeof(wpt_lat), 1, mapsend_file_in);

		wpt_tmp->shortname = strdup(name);
		wpt_tmp->description = strdup(comment);
		wpt_tmp->position.altitude.altitude_meters = wpt_alt;
		wpt_tmp->position.latitude.degrees = -wpt_lat;
		wpt_tmp->position.longitude.degrees = wpt_long;

		waypt_add(wpt_tmp);
	}
}

static void
mapsend_waypt_pr(waypoint *waypointp)
{
	int n;
	double falt;
	double flong;
	double flat;
static int cnt = 0;

	n = strlen(waypointp->shortname);
	fwrite(&n, 1, 1, mapsend_file_out);
	fwrite(waypointp->shortname, n, 1, mapsend_file_out);

	n = strlen(waypointp->description);
	fwrite(&n, 1, 1, mapsend_file_out);
	fwrite(waypointp->description, n, 1, mapsend_file_out);

	/* #, icon, status */
n = ++cnt;
	fwrite(&n, 4, 1, mapsend_file_out);
n = 0;
	fwrite(&n, 1, 1, mapsend_file_out);
n = 1;
	fwrite(&n, 1, 1, mapsend_file_out);

	falt = waypointp->position.altitude.altitude_meters;
	fwrite(&falt, sizeof(double), 1, mapsend_file_out);

	flong = waypointp->position.longitude.degrees;
	fwrite(&flong, sizeof(double), 1, mapsend_file_out);
	flat = -waypointp->position.latitude.degrees;
	fwrite(&flat, sizeof(double), 1, mapsend_file_out);
}

void
mapsend_write(void)
{
	mapsend_hdr hdr = {13, "4D533330 MS", "30", 1};
	int wpt_count = waypt_count();
int n = 0;

	fwrite(&hdr, sizeof(hdr), 1, mapsend_file_out);
	fwrite(&wpt_count, sizeof(wpt_count), 1, mapsend_file_out);

	waypt_disp_all(mapsend_waypt_pr);

	fwrite(&n, 4, 1, mapsend_file_out);
/* TODO: Impelment routes here */
}

ff_vecs_t mapsend_vecs = {
	mapsend_rd_init,
	mapsend_wr_init,
	mapsend_rd_deinit,
	mapsend_wr_deinit,
	mapsend_read,
	mapsend_write,
};
