/*
 * written by robertlipe@usa.net
 */

#include "defs.h"
#include "magellan.h"

static FILE *file_in;
static FILE *file_out;
static void *mkshort_handle;

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
	char name[9], desc[30];
	double lat,lon;
	char latdir, londir;
	int ilat, ilon;
	long alt; 
	char alttype;
	char icon[3] = {0};
	waypoint *wpt_tmp;

	while( fscanf(file_in, "%s %le%c %le%c %ld%c %30[^,] %c",
			name, &lat, &latdir, &lon, &londir,
			&alt, &alttype, desc, icon) > 0) {
		rtrim(desc);
		wpt_tmp = xcalloc(sizeof(*wpt_tmp),1);
		wpt_tmp->position.altitude.altitude_meters = alt;
		wpt_tmp->shortname = xstrdup(name);
		wpt_tmp->description = xstrdup(desc);
		wpt_tmp->creation_time = time(NULL);

		if (latdir == 'S') lat = -lat;
		if (londir == 'W') lon = -lon;

		lat /= 100.0;
		lon /= 100.0;
		ilon = (int)(lon);
		wpt_tmp->position.longitude.degrees = ilon + (lon - ilon)*(100.0/60.0);
		ilat = (int)(lat);
		wpt_tmp->position.latitude.degrees = ilat + (lat - ilat) * (100.0/60.0);
		wpt_tmp->icon_descr = mag_find_descr_from_token(icon);
		waypt_add(wpt_tmp);
	}
}

static void
gpsutil_disp(const waypoint *wpt)
{
	double lon,lat;
	const char *icon_token;

	icon_token = mag_find_token_from_descr(wpt->icon_descr);

	lon = degrees2ddmm(wpt->position.longitude.degrees);
	lat = degrees2ddmm(wpt->position.latitude.degrees);

	fprintf(file_out, "%-8s %08.3f%c %09.3f%c %07.0f%c %-30.30s %s\n",
                global_opts.synthesize_shortnames ?
                        mkshort(mkshort_handle, wpt->description) : 
			wpt->shortname,
		fabs(lat),
		lat < 0.0 ? 'S' : 'N',
		fabs(lon),
		lon < 0.0 ? 'W' : 'E',
		wpt->position.altitude.altitude_meters,
		'm', 
		wpt->description ? wpt->description : "",
		icon_token);
}

static void
data_write(void)
{
	waypt_disp_all(gpsutil_disp);
}


ff_vecs_t gpsutil_vecs = {
	rd_init,
	wr_init,
	rd_deinit,
	wr_deinit,
	data_read,
	data_write,
};
