/*
    Access gpsutil files.

    Copyright (C) 2002, 2003, 2004 Robert Lipe, robertlipe@usa.net

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
static void *mkshort_handle;

#define MYNAME "GPSUTIL"

static void
rd_init(const char *fname)
{
	file_in = xfopen(fname, "r", MYNAME);
}

static void
rd_deinit(void)
{
	fclose(file_in);
}

static void
wr_init(const char *fname)
{
	file_out = xfopen(fname, "w", MYNAME);
	mkshort_handle = mkshort_new_handle();
}

static void
wr_deinit(void)
{
	fclose(file_out);
	mkshort_del_handle(mkshort_handle);
}

static void
data_read(void)
{
	char ibuf[100];
	char name[9], desc[30];
	double lat,lon;
	char latdir, londir;
	int ilat, ilon;
	long alt; 
	char alttype;
	char icon[3] = {0};
	waypoint *wpt_tmp;
	/*
	 * Make sure that all waypoints in single read have same 
	 * timestamp.
	 */
	time_t now = current_time();


	for(;fgets(ibuf, sizeof(ibuf), file_in);) {
	/*  A sharp in column zero or an blank line is a comment */
	if (ibuf[0] == '#' || ibuf[0] == '\n') continue;
	sscanf(ibuf, "%s %le%c %le%c %ld%c %30[^,] %c",
			name, &lat, &latdir, &lon, &londir,
			&alt, &alttype, desc, icon);
		desc[0] = '\0';
		icon[0] = '\0';
		sscanf(&ibuf[39], "%30c", desc);
		sscanf(&ibuf[70], "%2c", icon);
		rtrim(desc);
		rtrim(icon);
		wpt_tmp = waypt_new();
		wpt_tmp->altitude = alt;
		wpt_tmp->shortname = xstrdup(name);
		wpt_tmp->description = xstrdup(desc);
		wpt_tmp->creation_time = now;

		if (latdir == 'S') lat = -lat;
		if (londir == 'W') lon = -lon;

		lat /= 100.0;
		lon /= 100.0;
		ilon = (int)(lon);
		wpt_tmp->longitude = ilon + (lon - ilon)*(100.0/60.0);
		ilat = (int)(lat);
		wpt_tmp->latitude = ilat + (lat - ilat) * (100.0/60.0);
		wpt_tmp->icon_descr = mag_find_descr_from_token(icon);
		waypt_add(wpt_tmp);
	}
}

static void
gpsutil_disp(const waypoint *wpt)
{
	double lon,lat;
	const char *icon_token;
	char *tdesc = str_utf8_to_ascii(wpt->description);

	icon_token = mag_find_token_from_descr(wpt->icon_descr);

	lon = degrees2ddmm(wpt->longitude);
	lat = degrees2ddmm(wpt->latitude);

	fprintf(file_out, "%-8s %08.3f%c %09.3f%c %07.0f%c %-30.30s %s\n",
                global_opts.synthesize_shortnames ?
                        mkshort_from_wpt(mkshort_handle, wpt) : 
			wpt->shortname,
		fabs(lat),
		lat < 0.0 ? 'S' : 'N',
		fabs(lon),
		lon < 0.0 ? 'W' : 'E',
		wpt->altitude == unknown_alt ? 0 : wpt->altitude,
		'm', 
		wpt->description ? tdesc : "",
		icon_token);

	xfree(tdesc);
}

static void
data_write(void)
{
	waypt_disp_all(gpsutil_disp);
}


ff_vecs_t gpsutil_vecs = {
	ff_type_file,
	FF_CAP_RW_WPT,
	rd_init,
	wr_init,
	rd_deinit,
	wr_deinit,
	data_read,
	data_write,
	NULL
};
