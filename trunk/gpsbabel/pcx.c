/*
    Access to Garmin PCX5 files.

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
#include "garmin_tables.h"
#include <ctype.h>

static FILE *file_in;
static FILE *file_out;
static void *mkshort_handle;
static char *deficon = "Waypoint";

#define MYNAME "PCX"

static
arglist_t pcx_args[] = {
	{"deficon", &deficon, "Default icon name", ARGTYPE_STRING },
	{0, 0, 0, 0}
};

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
	char name[7], desc[40];
	double lat,lon;
	char latdir, londir;
	long alt; 
	int symnum;
	char date[10];
	char time[9];
	char month[4];
	waypoint *wpt_tmp;
	char ibuf[122];
	struct tm tm;
	route_head *track_head = NULL;

	for(;fgets(ibuf, sizeof(ibuf), file_in);) {
		switch (ibuf[0]) {
		case 'W': 
			sscanf(ibuf, "W  %6c %c%lf %c%lf %s %s %ld", 
				name, &latdir, &lat, &londir, &lon, 
				date, time, &alt);
			sscanf(&ibuf[60], "%40c", 
				desc);
			sscanf(&ibuf[116], "%d", 
				&symnum);
			desc[sizeof(desc)-1] = '\0';
			name[sizeof(name)-1] = '\0';
			wpt_tmp = xcalloc(sizeof(*wpt_tmp), 1);
			wpt_tmp->altitude = alt;
			wpt_tmp->shortname = xstrdup(name);
			wpt_tmp->description = xstrdup(desc);
			wpt_tmp->icon_descr = mps_find_desc_from_icon_number(symnum, PCX);

			if (latdir == 'S') lat = -lat;
			if (londir == 'W') lon = -lon;
			wpt_tmp->longitude = ddmm2degrees(lon);
			wpt_tmp->latitude = ddmm2degrees(lat);
			waypt_add(wpt_tmp);
			break;
		case 'H':
			if (ibuf[3] == 'T' && ibuf[4] == 'N') {
				track_head = route_head_alloc();
				track_head->rte_name = strdup(&ibuf[6]);
				track_add_head(track_head);
			}
			break;
		case 'T':
			sscanf(ibuf, "T %lf %lf %s %s %ld", 
				&lat, &lon, date, time, &alt);
			memset(&tm, 0, sizeof(tm));
			tm.tm_hour = atoi(time);
			tm.tm_min = atoi(time+3);
			tm.tm_sec = atoi(time+6);
			tm.tm_mday = atoi(date);
			strncpy(month, date+3, 3);
			month[3] = 0;
			tm.tm_mon = month_lookup(month);
			tm.tm_year = atoi(date + 7) + 100;
			wpt_tmp = waypt_new();
			wpt_tmp->creation_time = mktime(&tm) + get_tz_offset();
			wpt_tmp->latitude = lat;
			wpt_tmp->longitude = lon;
			wpt_tmp->altitude = alt;
			route_add_wpt(track_head, wpt_tmp);
		default:
			;
		}
	}
}

static void
gpsutil_disp(const waypoint *wpt)
{
	double lon,lat;
	int icon_token = 0;
	char tbuf[1024];
	char *tp = tbuf;
	time_t tm = wpt->creation_time;

	lon = degrees2ddmm(wpt->longitude);
	lat = degrees2ddmm(wpt->latitude);

	if (tm == 0) 
		tm = time(NULL);
	strftime(tbuf, sizeof(tbuf), "%d-%b-%y %I:%M:%S", localtime(&tm));
	while (*tp) {
		*tp = toupper(*tp);
		tp++;
	}

	icon_token = mps_find_icon_number_from_desc(wpt->icon_descr, PCX);
	if (get_cache_icon(wpt)) {
		icon_token = mps_find_icon_number_from_desc(get_cache_icon(wpt), PCX);
	}


	fprintf(file_out, "W  %-6.6s %c%08.5f %c%011.5f %s %5d %-40.40s %5e  %d\n",
                global_opts.synthesize_shortnames ?
                        mkshort(mkshort_handle, wpt->description) : 
			wpt->shortname,
		lat < 0.0 ? 'S' : 'N',
		fabs(lat),
		lon < 0.0 ? 'W' : 'E',
		fabs(lon),
		tbuf, 
		-9999,
		wpt->description,
		0.0,
		icon_token);
}

static void
pcx_track_hdr(const route_head *trk)
{
	fprintf(file_out, "\n\nH  TN %s\n", trk->rte_name);
	fprintf(file_out, "H  LATITUDE    LONGITUDE    DATE      TIME     ALT\n");

}

void
pcx_track_disp(const waypoint *wpt)
{
	char tbuf[100];
	struct tm *tm;
	char *tp;

	tm = localtime(&wpt->creation_time);

	strftime(tbuf, sizeof(tbuf), "%d-%b-%y %T", tm);
	for (tp = tbuf; *tp; tp++) {
		*tp = toupper(*tp);
	}
	fprintf(file_out, "T  %+011.7f %+012.7f %s %.f\n",
			wpt->latitude, wpt->longitude, tbuf, wpt->altitude);
}


static void
data_write(void)
{
fprintf(file_out,
"H  SOFTWARE NAME & VERSION\n"
"I  PCX5 2.09\n"
"\n"
"H  R DATUM                IDX DA            DF            DX            DY            DZ\n"
"M  G WGS 84               121 +0.000000e+00 +0.000000e+00 +0.000000e+00 +0.000000e+00 +0.000000e+00\n"
"\n"
"H  COORDINATE SYSTEM\n"
"U  LAT LON DM\n"
"\n"
"H  IDNT   LATITUDE    LONGITUDE    DATE      TIME     ALT   DESCRIPTION                              PROXIMITY     SYMBOL ;waypts\n");
	setshort_length(mkshort_handle, 6);
	waypt_disp_all(gpsutil_disp);

	if (global_opts.objective == trkdata) {
		track_disp_all(pcx_track_hdr, NULL, pcx_track_disp);
	}
}


ff_vecs_t pcx_vecs = {
	ff_type_file,
	rd_init,
	wr_init,
	rd_deinit,
	wr_deinit,
	data_read,
	data_write,
	pcx_args
};
