/*
 * written by robertlipe@usa.net
 */

#include "defs.h"
#include "magellan.h"

static FILE *file_in;
static FILE *file_out;

static void
rd_init(const char *fname)
{
	file_in = fopen(fname, "r");
	if (file_in == NULL) {
		fatal("GPSUTIL: Cannot open %s for reading\n", fname);
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
		fatal("GPSUTIL: Cannot open %s for writing\n", fname);
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
	char name[9], desc[30];
	double lat,lon;
	char latdir, londir;
	long alt; 
	char alttype;
	char icon[3] = {0};
	waypoint *wpt_tmp;

	while( fscanf(file_in, "%s %le%c %le%c %ld%c %30[^,] %c",
			name, &lat, &latdir, &lon, &londir,
			&alt, &alttype, desc, icon) > 0) {
		wpt_tmp = calloc(sizeof(*wpt_tmp),1);
		if (wpt_tmp == NULL) {
			fatal("GPSMAN: cannot allocate memory\n");
		}
		wpt_tmp->position.altitude.altitude_meters = alt;
		wpt_tmp->shortname = strdup(name);
		wpt_tmp->description = strdup(desc);
		wpt_tmp->creation_time = time(NULL);

		if (latdir == 'S') lat = -lat;
		if (londir == 'W') lon = -lon;
		wpt_tmp->position.longitude.degrees = lon/100.0;
		wpt_tmp->position.latitude.degrees = lat/100.0;
		wpt_tmp->icon_descr = strdup(icon);

		waypt_add(wpt_tmp);
	}
}

static void
gpsutil_disp(waypoint *wpt)
{
	double lon,lat;
	const char *icon_token;

	icon_token = mag_find_token_from_descr(wpt->icon_descr);

	lon = wpt->position.longitude.degrees * 100.0;
	lat = wpt->position.latitude.degrees * 100.0;

	fprintf(file_out, "%-8s %08.3f%c %09.3f%c %07.0f%c %-30.30s %s\n",
		wpt->shortname,
		fabs(lat),
		lat < 0.0 ? 'S' : 'N',
		fabs(lon),
		lon < 0.0 ? 'W' : 'E',
		wpt->position.altitude.altitude_meters,
		'm', 
		wpt->description,
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
