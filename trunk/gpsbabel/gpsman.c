/*
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

static FILE *in_file;
static FILE *out_file;

#define MYNAME "GPSMAN"

static void
gpsman_rd_init(const char *fname, const char *args)
{
	in_file = fopen(fname, "r");
	if (in_file == NULL) {
		fatal(MYNAME ": Cannot open %s for reading\n", fname);
	}

}
static void
gpsman_rd_deinit(void)
{
	fclose(in_file);
}

static void
gpsman_wr_init(const char *fname, const char *args)
{
	out_file = fopen(fname, "w");
	if (out_file == NULL) {
		fatal(MYNAME ": Cannot open %s for writing\n", fname);
	}

	fprintf(out_file, "!Format: DDD 1 WGS 84\n");
}

static void
gpsman_wr_deinit(void)
{
	fclose(out_file);
}

static void
gpsman_disp(const waypoint *waypointp)
{
 	fprintf(out_file, "%-8.8s\t%s\t", 
		waypointp->shortname, waypointp->description);
	fprintdms(out_file, &waypointp->position.latitude, 1);
	fprintdms(out_file, &waypointp->position.longitude, 0);
	fprintf(out_file,"\n");
}

static void
gpsman_parse_cmd(const char *cmd)
{
	return;
}

static void
gpsman_read(void)
{
	char latdir, londir;
	double latf, lonf;
	int lonm, latm;
	double lat, lon;
	char sname[20];
	char descr[100];
	char ibuf[100];
	waypoint *wpt_tmp;

	while( fgets(ibuf, sizeof(ibuf), in_file)) {
		if (ibuf[0] == '%' || ibuf[0] == '\n' ) {
			continue;
		}
		if (ibuf[0] == '!') {
			gpsman_parse_cmd(&ibuf[1]);
			continue;
		}

		sscanf(ibuf, "%[^\t] %[^\t] %c%d %lf %c%d %lf",
			sname, descr, &latdir, &latm, &latf, &londir, &lonm, &lonf);

		wpt_tmp = xcalloc(sizeof(*wpt_tmp),1);

		lat = latm + latf;
		lon = lonm + lonf;

		if (latdir == 'S') lat = -lat;
		if (londir == 'W') lon = -lon;

		wpt_tmp->position.longitude.degrees = lon;
		wpt_tmp->position.latitude.degrees = lat;
		wpt_tmp->shortname = xstrdup(sname);
		wpt_tmp->description = xstrdup(descr);

		waypt_add(wpt_tmp);
	}
}

static void
gpsman_write(void)
{
	fprintf(out_file, "!W:\n");
	waypt_disp_all(gpsman_disp);
}

ff_vecs_t gpsman_vecs = {
	gpsman_rd_init,	
	gpsman_wr_init,	
	gpsman_rd_deinit,
	gpsman_wr_deinit,
	gpsman_read,
	gpsman_write,
};
