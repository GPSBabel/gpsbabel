/*
    Universal CSV - support for csv files, divining field order from the header.

    Copyright (C) 2006 Robert Lipe,  robertlipe@usa.net

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
#include "csv_util.h"
#include "jeeps/gpsmath.h"
#include "strptime.h"
#include <string.h>

#define MYNAME "unicsv"

typedef enum {
	fld_shortname = 0,
	fld_latitude,
	fld_longitude,
	fld_description,
	fld_notes,
	fld_url,
	fld_altitude,
	fld_utm_zone,
	fld_utm_char,
	fld_utm_northing,
	fld_utm_easting,
	fld_utm,
	fld_bng,
	fld_bng_zone,
	fld_bng_northing,
	fld_bng_easting,
	fld_hdop,
	fld_pdop,
	fld_vdop,
	fld_sat,
	fld_fix,
	fld_utc_date,
	fld_utc_time,
	fld_course,
	fld_speed,
	fld_temperature,
	fld_temperature_f,
	fld_heartrate,
	fld_cadence,
	fld_proximity,
	fld_depth,
	fld_terminator
} field_e;

#define STR_LEFT	1
#define STR_RIGHT	2
#define STR_ANY		4
#define STR_EQUAL	8
#define STR_CASE	16

typedef struct {
	char *name;
	field_e type;
	gbuint32 options;
	
} field_t;

/*
 * ! Use always underscores in field names !
 * we check a second time after replacing underscores with spaces
 */
static field_t fields_def[] = {
	{ "name",	fld_shortname, STR_ANY },
	{ "desc",	fld_description, STR_ANY },
	{ "notes",	fld_notes, STR_ANY },
	{ "comment",	fld_notes, STR_ANY },
	{ "text",	fld_notes, STR_ANY },
	{ "url",	fld_url, STR_ANY },
	{ "lat",	fld_latitude, STR_ANY },
	{ "lon",	fld_longitude, STR_ANY },
	{ "x",		fld_longitude, STR_EQUAL },
	{ "y",		fld_latitude, STR_EQUAL },
	{ "alt",	fld_altitude, STR_ANY },
	{ "ele",	fld_altitude, STR_ANY },
	{ "utm_z",	fld_utm_zone, STR_ANY },
	{ "utm_c",	fld_utm_char, STR_ANY },
	{ "utm_n",	fld_utm_northing, STR_ANY },
	{ "utm_e",	fld_utm_easting, STR_ANY },
	{ "utm",	fld_utm, STR_EQUAL },
	{ "bng_z",	fld_bng_zone, STR_ANY },
	{ "bng_n",	fld_bng_northing, STR_ANY },
	{ "bng_e",	fld_bng_easting, STR_ANY },
	{ "bng",	fld_bng, STR_EQUAL },
	{ "hdop",	fld_hdop, STR_ANY },
	{ "pdop",	fld_pdop, STR_ANY },
	{ "vdop",	fld_vdop, STR_ANY },
	{ "sat",	fld_sat, STR_ANY },
	{ "fix",	fld_fix, STR_ANY },
	{ "utc_d",	fld_utc_date, STR_ANY },
	{ "utc_t",	fld_utc_time, STR_ANY },
	{ "head",	fld_course, STR_ANY },
	{ "course",	fld_course, STR_ANY },
	{ "speed",	fld_speed, STR_ANY },
	{ "tempf",	fld_temperature_f, STR_EQUAL },	/* degrees fahrenheit */
	{ "temp",	fld_temperature, STR_ANY },	/* degrees celsius by default */
	{ "heart",	fld_heartrate, STR_ANY },
	{ "cadence",	fld_cadence, STR_ANY },
	{ "prox",	fld_proximity, STR_ANY },
	{ "depth",	fld_depth, STR_ANY },
	{ NULL,		fld_terminator, 0 }
};

static field_e *unicsv_fields_tab;
static int unicsv_fields_tab_ct;
static double unicsv_altscale;
static char *unicsv_fieldsep;
static gbfile *file_in;
static gpsdata_type unicsv_data_type;
static route_head *unicsv_track;

static arglist_t unicsv_args[] = { ARG_TERMINATOR };


/* helpers */

// #define UNICSV_IS(f) (0 == strcmp(s, f))
#define UNICSV_CONTAINS(f) (0 != strstr(s, f))

static char
unicsv_compare_fields(char *s, const field_t *f)
{
	char *name = (char *)f->name;
	char *test = s;
	char result;

	if (! (f->options & STR_CASE)) {
		test = strupper(xstrdup(s));
		name = strupper(xstrdup(f->name));
	}

	if (f->options & STR_EQUAL) {
		result = (strcmp(test, name) == 0);
	}
	else if (f->options & STR_ANY) {
		result = (strstr(test, name) != NULL);
	}
	else {
		int len = strlen(name);
		
		if (f->options & STR_LEFT) {
			result = (strncmp(test, name, len) == 0);
		}
/*
		else if (f->options & STR_RIGHT) {
			result = (strrncmp(test, name, len) == 0);
		}
*/
		else {
			result = 0;	/* what should we do here ? */
		}
	}

	if ((! result) && (strchr(test, ' ') != NULL)) {
		char *tmp = gstrsub(test, " ", "_");
		result = unicsv_compare_fields(tmp, f);
		xfree(tmp);
	}
	
	if (name != f->name) {
		xfree(name);
		xfree(test);
	}

	return result;
}


static void
unicsv_fondle_header(char *ibuf)
{
	char *s;
	int i, column;

	/* Convert the entire header to lower case for convenience. 
	 * If we see a tab in that header, we decree it to be tabsep.
	 */
	unicsv_fieldsep = ",";	
	for (s = ibuf; *s; s++) {
		if (*s == '\t') {
			unicsv_fieldsep = "\t";
		}
		else if (*s == ';') {
			unicsv_fieldsep = ";";
		}
		else {
			*s = tolower(*s);
		}
	}

	column = -1;
	while ((s = csv_lineparse(ibuf, unicsv_fieldsep, "", 0))) {
		
		field_t *f = &fields_def[0];
		
		ibuf = NULL;
		column++;
		unicsv_fields_tab_ct++;
		s = lrtrim(s);
		
		if (column % 4 == 0) {
			int sz = (column + 4) * sizeof(*unicsv_fields_tab);
			if (column == 0) unicsv_fields_tab = xmalloc(sz);
			else unicsv_fields_tab = xrealloc(unicsv_fields_tab, sz);
			for (i = 0; i < 4; i++) unicsv_fields_tab[column + i] = fld_terminator;
		}

		while (f->name) {
			if (unicsv_compare_fields(s, f)) {
				unicsv_fields_tab[column] = f->type;
				break;
			}
			f++;
		}
		
		/* handle some special items */
		if (f->type == fld_altitude) {
			if (UNICSV_CONTAINS("ft") || UNICSV_CONTAINS("feet")) {
				unicsv_altscale = FEET_TO_METERS(1);
			}
		}
/* todo: date, time, maybe a few others */
	}
}

static void
unicsv_rd_init(const char *fname)
{
	char *c;
	unicsv_altscale = 1.0;
	
	unicsv_fields_tab = NULL;
	unicsv_fields_tab_ct = 0;
	unicsv_data_type = wptdata;
	unicsv_track = NULL;

	file_in = gbfopen(fname, "rb", MYNAME);

	if ((c = gbfgetstr(file_in)))
		unicsv_fondle_header(c);
	else
		unicsv_fieldsep = NULL;
}

static void
unicsv_rd_deinit(void)
{
	gbfclose(file_in);
	if (unicsv_fields_tab) xfree(unicsv_fields_tab);
}

static void
unicsv_parse_one_line(char *ibuf)
{
	char *s;
	waypoint *wpt = NULL;
	int column;
	int  utmz = -9999;
	double utme = 0;
	double utmn = 0;
	char utmc = 'N';
	char bng_zone[3] = "";
	double bng_easting = 0;
	double bng_northing = 0;
	struct tm tm;
	char *ok;
	int checked = 0;

	wpt = waypt_new();
	column = -1;
	while ((s = csv_lineparse(ibuf, unicsv_fieldsep, "\"", 0))) {
	
		if (column > unicsv_fields_tab_ct) break;	/* extra fields on line */

		ibuf = NULL;
		column++;
		checked++;
		s = lrtrim(s);

		switch(unicsv_fields_tab[column]) {
		
		case fld_latitude:
			human_to_dec( s, &wpt->latitude, &wpt->longitude, 1 );
			break;
			
		case fld_longitude:
			human_to_dec( s, &wpt->latitude, &wpt->longitude, 2 );
			break;
			
		case fld_shortname:
			if (*s) wpt->shortname = xstrdup(s);
			break;
			
		case fld_description:
			if (*s) wpt->description = xstrdup(s);
			break;

		case fld_notes:
			if (*s) wpt->notes = xstrdup(s);
			break;

		case fld_url:
			if (*s) wpt->url = xstrdup(s);
			break;

		case fld_altitude:
			wpt->altitude = atof(s) * unicsv_altscale;
			break;

		case fld_utm_zone:
			utmz = atoi(s);
			break;
			
		case fld_utm_easting:
			utme = atof(s);
			break;
			
		case fld_utm_northing:
			utmn = atof(s);
			break;
			
		case fld_utm_char:
			utmc = toupper(s[0]);
			break;
			
		case fld_utm:
			parse_coordinates(s, DATUM_WGS84, grid_utm,
				&wpt->latitude, &wpt->longitude, MYNAME);
			break;

		case fld_bng:
			parse_coordinates(s, DATUM_OSGB36, grid_bng,
				&wpt->latitude, &wpt->longitude, MYNAME);
			break;
			
		case fld_bng_zone:
			strncpy(bng_zone, s, sizeof(bng_zone));
			strupper(bng_zone);
			break;

		case fld_bng_northing:
			bng_northing = atof(s);
			break;

		case fld_bng_easting:
			bng_easting = atof(s);
			break;
			
		case fld_hdop:
			wpt->hdop = atof(s);
			unicsv_data_type = trkdata;
			break;

		case fld_pdop:
			wpt->pdop = atof(s);
			unicsv_data_type = trkdata;
			break;

		case fld_vdop:
			wpt->vdop = atof(s);
			unicsv_data_type = trkdata;
			break;

		case fld_sat:
			wpt->sat = atoi(s);
			unicsv_data_type = trkdata;
			break;

		case fld_fix:
			unicsv_data_type = trkdata;
			if (case_ignore_strcmp(s, "none") == 0)
				wpt->fix = fix_none;
			else if (case_ignore_strcmp(s, "2d") == 0)
				wpt->fix = fix_2d;
			else if (case_ignore_strcmp(s, "3d") == 0)
				wpt->fix = fix_3d;
			else if (case_ignore_strcmp(s, "dgps") == 0)
				wpt->fix = fix_dgps;
			else if (case_ignore_strcmp(s, "pps") == 0)
				wpt->fix = fix_pps;
			else wpt->fix = fix_unknown;
			break;

		case fld_utc_date:
			memset(&tm, 0, sizeof(tm));
			if (strchr(s, '.'))
				ok = strptime(s, "%d.%m.%Y", &tm);
			else
				ok = strptime(s, "%d/%m/%Y", &tm);
			if (ok) {
				tm.tm_isdst = -1;
				wpt->creation_time += mkgmtime(&tm);
			}
			else
				fatal(MYNAME ": Could not convert date string (%s).\n", s);
			break;
			
		case fld_utc_time:
			memset(&tm, 0, sizeof(tm));
			if (strptime( s, "%H:%M:%S", &tm)) {
				wpt->creation_time += (
					(SECONDS_PER_HOUR * tm.tm_hour) +
					(60L * tm.tm_min) + tm.tm_sec
				);
			}
			break;

		case fld_speed:
			WAYPT_SET(wpt, speed, atof(s));
			unicsv_data_type = trkdata;
			break;

		case fld_course:
			WAYPT_SET(wpt, course, atof(s));
			unicsv_data_type = trkdata;
			break;
			
		case fld_temperature:
			WAYPT_SET(wpt, temperature, atof(s));
			break;

		case fld_temperature_f:
			WAYPT_SET(wpt, temperature, FAHRENHEIT_TO_CELSIUS(atof(s)));
			break;

		case fld_heartrate:
			wpt->heartrate = atoi(s);
			unicsv_data_type = trkdata;
			break;

		case fld_cadence:
			wpt->cadence = atoi(s);
			unicsv_data_type = trkdata;
			break;

		case fld_proximity:
			WAYPT_SET(wpt, proximity, atof(s));
			break;

		case fld_depth:
			WAYPT_SET(wpt, depth, atof(s));
			break;
			
		case fld_terminator: /* dummy */
			checked--;
			break;
		}
	}

	if (checked == 0) {
		waypt_free(wpt);
		return;
	}
	
	if (utmz != -9999) {
		GPS_Math_UTM_EN_To_Known_Datum(&wpt->latitude, &wpt->longitude,
			utme, utmn, utmz, utmc, DATUM_WGS84);
	}

	if (bng_zone[0]) {
		if (! GPS_Math_UKOSMap_To_WGS84_M(
				bng_zone, bng_easting, bng_northing,
				&wpt->latitude, &wpt->longitude))
			fatal(MYNAME ": Unable to convert BNG coordinates (%s %.f %.f)!\n",
				bng_zone, bng_easting, bng_northing);
	}

	switch(unicsv_data_type) {
		case trkdata:
			if (! unicsv_track) {
				unicsv_track = route_head_alloc();
				track_add_head(unicsv_track);
			}
			track_add_wpt(unicsv_track, wpt);
			break;
		default:
			waypt_add(wpt);
	}
}

static void 
unicsv_rd(void)
{
	char *buff;

	if (unicsv_fieldsep == NULL) return;
	
	while ((buff = gbfgetstr(file_in))) {
	    	buff = lrtrim(buff);
		if (*buff)
			unicsv_parse_one_line(buff);
	}
}

/* --------------------------------------------------------------------------- */

ff_vecs_t unicsv_vecs = {
	ff_type_file,
	{ ff_cap_read, 0, 0},
	unicsv_rd_init,
	NULL,
	unicsv_rd_deinit, 
	NULL,
	unicsv_rd,
	NULL,
	NULL,
	unicsv_args,
	CET_CHARSET_ASCII, 0	/* can be changed with -c ... */
};
