/*
    Universal CSV - support for csv files, divining field order from the header.

    Copyright (C) 2006 Robert Lipe, robertlipe@usa.net,
    copyright (C) 2007,2008 Olaf Klein, o.b.klein@gpsbabel.org

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
#include "cet.h"
#include "cet_util.h"
#include "csv_util.h"
#include "garmin_fs.h"
#include "garmin_tables.h"
#include "jeeps/gpsmath.h"
#include "strptime.h"
#include <string.h>
#include <time.h>

#define MYNAME "unicsv"

/* "UNICSV_FIELD_SEP" and "UNICSV_LINE_SEP" are only used by the writer */

#define UNICSV_FIELD_SEP	","
#define UNICSV_LINE_SEP		"\r\n"
#define UNICSV_QUOT_CHAR	'"'

/* GPSBabel internal and calculated fields */

typedef enum {
	fld_shortname = 0,
	fld_latitude,
	fld_longitude,
	fld_description,
	fld_notes,
	fld_url,
	fld_altitude,
	fld_utm_zone,
	fld_utm_zone_char,
	fld_utm_northing,
	fld_utm_easting,
	fld_utm,
	fld_bng,
	fld_bng_zone,
	fld_bng_northing,
	fld_bng_easting,
	fld_swiss,
	fld_swiss_northing,
	fld_swiss_easting,
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
	fld_symbol,
	fld_date,
	fld_time,
	fld_datetime,
	fld_iso_time,
	fld_year,
	fld_month,
	fld_day,
	fld_hour,
	fld_min,
	fld_sec,
	fld_garmin_city,
	fld_garmin_postal_code,
	fld_garmin_state,
	fld_garmin_country,
	fld_garmin_addr,
	fld_garmin_phone_nr,
	fld_garmin_facility,
	fld_terminator
} field_e;

#define STR_LEFT	1
#define STR_RIGHT	2
#define STR_ANY		4
#define STR_EQUAL	8
#define STR_CASE	16

#define unicsv_unknown	1e25

typedef struct {
	char *name;
	field_e type;
	gbuint32 options;
} field_t;

/*
 * ! Please use always underscores in field names !
 * we check a second time after replacing underscores with spaces
 */
static field_t fields_def[] = {
	/* unhandled columns */
	{ "index",	fld_terminator, STR_ANY },
	{ "no",		fld_terminator, STR_EQUAL },
	{ "mini",	fld_terminator, STR_ANY },	/* maybe minimum anything, so
							   avoid detection as 'min' for minute */
	/* handled columns */
	{ "name",	fld_shortname, STR_ANY },
	{ "title",	fld_shortname, STR_ANY },
	{ "desc",	fld_description, STR_ANY },
	{ "notes",	fld_notes, STR_ANY },
	{ "omment",	fld_notes, STR_ANY },		/* works also for German "Kommentar" */
	{ "text",	fld_notes, STR_ANY },
	{ "url",	fld_url, STR_ANY },
	{ "icon",	fld_symbol, STR_ANY },
	{ "symb",	fld_symbol, STR_ANY },
	{ "lat",	fld_latitude, STR_ANY },
	{ "lon",	fld_longitude, STR_ANY },
	{ "lng",	fld_longitude, STR_ANY },
	{ "x",		fld_longitude, STR_EQUAL },
	{ "y",		fld_latitude, STR_EQUAL },
	{ "z",		fld_altitude, STR_EQUAL },
	{ "x_pos",	fld_longitude, STR_ANY },
	{ "y_pos",	fld_latitude, STR_ANY },
	{ "alt",	fld_altitude, STR_ANY },
	{ "ele",	fld_altitude, STR_ANY },
	{ "utm_z",	fld_utm_zone, STR_ANY },
	{ "utm_c",	fld_utm_zone_char, STR_ANY },
	{ "utm_zc",	fld_utm_zone_char, STR_ANY },
	{ "utm_n",	fld_utm_northing, STR_ANY },
	{ "utm_e",	fld_utm_easting, STR_ANY },
	{ "utm",	fld_utm, STR_EQUAL },
	{ "utm_coo",	fld_utm, STR_ANY },
	{ "utm_pos",	fld_utm, STR_ANY },
	{ "bng_z",	fld_bng_zone, STR_ANY },
	{ "bng_n",	fld_bng_northing, STR_ANY },
	{ "bng_e",	fld_bng_easting, STR_ANY },
	{ "bng",	fld_bng, STR_EQUAL },
	{ "bng_coo",	fld_bng, STR_ANY },
	{ "bng_pos",	fld_bng, STR_ANY },
	{ "swiss_e",	fld_swiss_easting, STR_ANY },
	{ "swiss_n",	fld_swiss_northing, STR_ANY },
	{ "swiss",	fld_swiss, STR_EQUAL },
	{ "swiss_coo",	fld_swiss, STR_ANY },
	{ "swiss_pos",	fld_swiss, STR_ANY },
	{ "hdop",	fld_hdop, STR_ANY },
	{ "pdop",	fld_pdop, STR_ANY },
	{ "vdop",	fld_vdop, STR_ANY },
	{ "sat",	fld_sat, STR_ANY },
	{ "fix",	fld_fix, STR_ANY },
	{ "utc_d",	fld_utc_date, STR_ANY },
	{ "utc_t",	fld_utc_time, STR_ANY },
	{ "head",	fld_course, STR_ANY },
	{ "cour",	fld_course, STR_ANY },
	{ "speed",	fld_speed, STR_ANY },
	{ "velo",	fld_speed, STR_ANY },
	{ "geschw",	fld_speed, STR_ANY },		/* speed in german */
	{ "tempf",	fld_temperature_f, STR_EQUAL },	/* degrees fahrenheit */
	{ "temp",	fld_temperature, STR_ANY },	/* degrees celsius by default */
	{ "heart",	fld_heartrate, STR_ANY },
	{ "caden",	fld_cadence, STR_ANY },
	{ "prox",	fld_proximity, STR_ANY },
	{ "depth",	fld_depth, STR_ANY },
	{ "date",	fld_date, STR_ANY },
	{ "datum",	fld_date, STR_ANY },
	{ "time",	fld_time, STR_ANY },
	{ "zeit",	fld_time, STR_ANY },
	{ "hour",	fld_hour, STR_LEFT },
	{ "min",	fld_min, STR_LEFT },
	{ "sec",	fld_sec, STR_LEFT },
	{ "year",	fld_year, STR_LEFT },
	{ "month",	fld_month, STR_LEFT },
	{ "day",	fld_day, STR_LEFT },

	/* garmin specials */
	{ "addr",	fld_garmin_addr, STR_ANY },
	{ "street",	fld_garmin_addr, STR_ANY },
	{ "city",	fld_garmin_city, STR_ANY },
	{ "country",	fld_garmin_country, STR_ANY },
	{ "post",	fld_garmin_postal_code, STR_ANY },
	{ "zip",	fld_garmin_postal_code, STR_ANY },
	{ "phone",	fld_garmin_phone_nr, STR_ANY },
	{ "state",	fld_garmin_state, STR_ANY },
	{ "faci",	fld_garmin_facility, STR_ANY },
	{ NULL,		fld_terminator, 0 }
};

static field_e *unicsv_fields_tab;
static int unicsv_fields_tab_ct;
static double unicsv_altscale, unicsv_depthscale, unicsv_proximityscale
;
static char *unicsv_fieldsep;
static gbfile *fin, *fout;
static gpsdata_type unicsv_data_type;
static route_head *unicsv_track, *unicsv_route;
static unsigned long long unicsv_outp_flags;
static grid_type unicsv_grid_idx;
static int unicsv_datum_idx;
static char *opt_datum, *opt_grid, *opt_utc;
static int unicsv_waypt_ct;
static char unicsv_detect;

static arglist_t unicsv_args[] = { 
	{"datum", &opt_datum, "GPS datum (def. WGS 84)",
		"WGS 84", ARGTYPE_STRING, ARG_NOMINMAX}, 
	{"grid",  &opt_grid,  "Write position using this grid.",
		NULL, ARGTYPE_STRING, ARG_NOMINMAX},
	{"utc",   &opt_utc,   "Write timestamps with offset x to UTC time", 
		NULL, ARGTYPE_INT, "-23", "+23"},
	ARG_TERMINATOR };


/* helpers */

// #define UNICSV_IS(f) (0 == strcmp(s, f))
#define UNICSV_CONTAINS(f) (0 != strstr(s, f))

/* here we only need a simple yes(0) or no(1) */
static int
unicsv_strrcmp(const char *s1, const char *s2)
{
	int l1, l2;
	
	l1 = strlen(s1);
	l2 = strlen(s2);
	if ((l1 - l2) >= 0)
		return strcmp(s1 + (l1 - l2), s2);
	else
		return 1;	/* false */
}

static int
unicsv_parse_date(const char *str, int *consumed)
{
	int p1, p2, p3, ct;
	char sep[2];
	struct tm tm;
	int lconsumed = 0;
	
	memset(&tm, 0, sizeof(tm));
	ct = sscanf(str, "%d%1[-.//]%d%1[-.//]%d%n", &p1, sep, &p2, sep, &p3, &lconsumed);
	if (consumed && lconsumed) {
		*consumed = lconsumed;
	}
	if (ct != 5) {
		if (consumed) {		/* don't stop here; it's only sniffing */
			*consumed = 0;	/* for a possible date */
			return 0;
		}
		fatal(MYNAME ": Could not parse date string (%s).\n", str);
	}
	
	if ((p1 > 99) || (sep[0] == '-')) { /* Y-M-D (iso like) */
		tm.tm_year = p1;
		tm.tm_mon = p2;
		tm.tm_mday = p3;
	}
	else if (sep[0] == '.') {	/* Germany and any other countries */
		tm.tm_mday = p1;	/* have a fixed D.M.Y format */
		tm.tm_mon = p2;
		tm.tm_year = p3;
	}
	else {
		tm.tm_mday = p2;
		tm.tm_mon = p1;
		tm.tm_year = p3;
	}
	if ((p1 < 100) && (p2 < 100) && (p3 < 100)) {
		if (tm.tm_year < 70) tm.tm_year += 2000;
		else tm.tm_year += 1900;
	}
	/* some low-level checks */
	if ((tm.tm_mon > 12) || (tm.tm_mon < 1) || (tm.tm_mday > 31) || (tm.tm_mday < 1)) {
		if (consumed) {
			*consumed = 0;
			return 0;	/* don't stop here */
		}
		fatal(MYNAME ": Could not parse date string (%s).\n", str);
	}
	
	tm.tm_year -= 1900;
	tm.tm_mon -= 1;
	
	return mkgmtime(&tm);
}

static int
unicsv_parse_time(const char *str, int *msec, int *date)
{
	int hour, min, ct, sec;
	int consumed = 0;
	double ms;
	char sep[1];
	int ldate;

	/* If we have somethine we're pretty sure is a date, parse that
	 * first, skip over it, and pass that back to the caller)
	 */
	ldate = unicsv_parse_date(str, &consumed);
	if (consumed && ldate) {
		str += consumed;
		if (date) {
			*date = ldate;
		}
	}
	
	ct = sscanf(str, "%d%1[.://]%d%1[.://]%d%lf", &hour, sep, &min, sep, &sec, &ms);
	is_fatal(ct < 5, MYNAME ": Could not parse time string (%s).\n", str);
	if (ct == 6) {
		*msec = (ms * 1000000) + 0.5;
		if (*msec > 999999) {
			*msec = 0;
			sec++;
		}
	}
	else *msec = 0;

	return ((hour * SECONDS_PER_HOUR) + (min * 60) + (int)sec);
}

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
		if (f->options & STR_LEFT) {
			result = (strncmp(test, name, strlen(name)) == 0);
		}
		else if (f->options & STR_RIGHT) {
			result = (unicsv_strrcmp(test, name) == 0);
		}
		else {
			result = 0;	/* fallback to "FALSE" */
		}
	}

	if ((! result) && (strchr(test, ' ') != NULL)) {
		/* replace  ' ' with '_' and try again */
		char *tmp = gstrsub(test, " ", "_");
		result = unicsv_compare_fields(tmp, f);
		xfree(tmp);
	}
	if ((! result) && (strchr(test, '-') != NULL)) {
		/* replace  '-' with '_' and try again */
		char *tmp = gstrsub(test, "-", "_");
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
	char *buf = NULL;
	int i, column;
	const cet_cs_vec_t *ascii = &cet_cs_vec_ansi_x3_4_1968;	/* us-ascii */

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
		else if (*s == '|') {
			unicsv_fieldsep = "|";
		}
		else {
			continue;
		}
		break;
	}
	for (s = ibuf; *s; s++) {
		*s = tolower(*s);
	}
	
	/* convert the header line into native ascii */
	if (global_opts.charset != ascii) {
		buf = cet_str_any_to_any(ibuf, global_opts.charset, ascii);
		ibuf = buf;
	}

	column = -1;
	while ((s = csv_lineparse(ibuf, unicsv_fieldsep, "\"", 0))) {
		
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
		if ((! f->name) && global_opts.debug_level)
				warning(MYNAME ": Unhandled column \"%s\".\n", s);
		
		/* handle some special items */
		if (f->type == fld_altitude) {
			if (UNICSV_CONTAINS("ft") || UNICSV_CONTAINS("feet")) {
				unicsv_altscale = FEET_TO_METERS(1);
			}
		}
		if (f->type == fld_depth) {
			if (UNICSV_CONTAINS("ft") || UNICSV_CONTAINS("feet")) {
				unicsv_depthscale = FEET_TO_METERS(1);
			}
		}
		if (f->type == fld_proximity) {
			if (UNICSV_CONTAINS("ft") || UNICSV_CONTAINS("feet")) {
				unicsv_proximityscale = FEET_TO_METERS(1);
			}
		}
		if ((f->type == fld_time) || (f->type == fld_date)) {
			if (UNICSV_CONTAINS("iso"))
				f->type = fld_iso_time;
		}
	}
	if (buf) xfree(buf);
}

static void
unicsv_rd_init(const char *fname)
{
	char *c;
	unicsv_altscale = 1.0;
	unicsv_depthscale = 1.0;
	unicsv_proximityscale = 1.0;
	
	unicsv_fields_tab = NULL;
	unicsv_fields_tab_ct = 0;
	unicsv_data_type = global_opts.objective;
	unicsv_detect = (! (global_opts.masked_objective & (WPTDATAMASK | TRKDATAMASK | RTEDATAMASK | POSNDATAMASK)));

	unicsv_track = unicsv_route = NULL;
	unicsv_datum_idx = gt_lookup_datum_index(opt_datum, MYNAME);

	fin = gbfopen(fname, "rb", MYNAME);

	if ((c = gbfgetstr(fin)))
		unicsv_fondle_header(c);
	else
		unicsv_fieldsep = NULL;
}

static void
unicsv_rd_deinit(void)
{
	gbfclose(fin);
	if (unicsv_fields_tab) xfree(unicsv_fields_tab);
}

static void
unicsv_parse_one_line(char *ibuf)
{
	char *s;
	waypoint *wpt = NULL;
	int column;
	int  utm_zone = -9999;
	double utm_easting = 0;
	double utm_northing = 0;
	char utm_zc = 'N';
	char bng_zone[3] = "";
	double bng_easting = 0;
	double bng_northing = 0;
	double swiss_easting = unicsv_unknown;
	double swiss_northing = unicsv_unknown;
	int checked = 0;
	int date = -1, time = -1, msec = -1;
	char is_localtime = 0;
	garmin_fs_t *gmsd;
	double d;
	struct tm ymd;
	int src_datum = unicsv_datum_idx;

	wpt = waypt_new();
	wpt->latitude = unicsv_unknown;
	wpt->longitude = unicsv_unknown;
	memset(&ymd, 0, sizeof(ymd));

	column = -1;
	while ((s = csv_lineparse(ibuf, unicsv_fieldsep, "\"", 0))) {
	
		if (column > unicsv_fields_tab_ct) break;	/* ignore extra fields on line */

		ibuf = NULL;

		column++;
		checked++;

		s = lrtrim(s);
		if (! *s) continue;	/* skip empty columns */
		
		switch(unicsv_fields_tab[column]) {

		case fld_time:
		case fld_date:
			/* switch column type if it looks like an iso time string */
			if (strchr(s, 'T'))
				unicsv_fields_tab[column] = fld_iso_time;
			break;
		default: ;
		}


		switch(unicsv_fields_tab[column]) {
		
		case fld_latitude:
			human_to_dec( s, &wpt->latitude, &wpt->longitude, 1 );
			break;
			
		case fld_longitude:
			human_to_dec( s, &wpt->latitude, &wpt->longitude, 2 );
			break;
			
		case fld_shortname:
			wpt->shortname = xstrdup(s);
			break;
			
		case fld_description:
			wpt->description = xstrdup(s);
			break;

		case fld_notes:
			wpt->notes = xstrdup(s);
			break;

		case fld_url:
			wpt->url = xstrdup(s);
			break;

		case fld_altitude:
			if (parse_distance(s, &d, unicsv_altscale, MYNAME)) {
				if (fabs(d) < fabs(unknown_alt))
					wpt->altitude = d;
			}
			break;

		case fld_utm_zone:
			utm_zone = atoi(s);
			break;
			
		case fld_utm_easting:
			utm_easting = atof(s);
			break;
			
		case fld_utm_northing:
			utm_northing = atof(s);
			break;
			
		case fld_utm_zone_char:
			utm_zc = toupper(s[0]);
			break;
			
		case fld_utm:
			parse_coordinates(s, unicsv_datum_idx, grid_utm,
				&wpt->latitude, &wpt->longitude, MYNAME);
			/* coordinates from parse_coordinates are in WGS84
			   don't convert a second time */
			src_datum = DATUM_WGS84;
			break;

		case fld_bng:
			parse_coordinates(s, DATUM_OSGB36, grid_bng,
				&wpt->latitude, &wpt->longitude, MYNAME);
			/* coordinates from parse_coordinates are in WGS84
			   don't convert a second time */
			src_datum = DATUM_WGS84;
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
			
		case fld_swiss:
			parse_coordinates(s, DATUM_WGS84, grid_swiss,
				&wpt->latitude, &wpt->longitude, MYNAME);
			/* coordinates from parse_coordinates are in WGS84
			   don't convert a second time */
			src_datum = DATUM_WGS84;
			break;

		case fld_swiss_easting:
			swiss_easting = atof(s);
			break;

		case fld_swiss_northing:
			swiss_northing = atof(s);
			break;

		case fld_hdop:
			wpt->hdop = atof(s);
			if (unicsv_detect) unicsv_data_type = trkdata;
			break;

		case fld_pdop:
			wpt->pdop = atof(s);
			if (unicsv_detect) unicsv_data_type = trkdata;
			break;

		case fld_vdop:
			wpt->vdop = atof(s);
			if (unicsv_detect) unicsv_data_type = trkdata;
			break;

		case fld_sat:
			wpt->sat = atoi(s);
			if (unicsv_detect) unicsv_data_type = trkdata;
			break;

		case fld_fix:
			if (unicsv_detect) unicsv_data_type = trkdata;
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
			if ((is_localtime < 2) && (date < 0)) {
				date = unicsv_parse_date(s, NULL);
				is_localtime = 0;
			}
			break;

		case fld_utc_time:
			if ((is_localtime < 2) && (time < 0)) {
				time = unicsv_parse_time(s, &msec, &date);
				is_localtime = 0;
			}
			break;

		case fld_speed:
			if (parse_speed(s, &d, 1.0, MYNAME)) {
				WAYPT_SET(wpt, speed, d);
				if (unicsv_detect)
					unicsv_data_type = trkdata;
			}
			break;

		case fld_course:
			WAYPT_SET(wpt, course, atof(s));
			if (unicsv_detect) unicsv_data_type = trkdata;
			break;
			
		case fld_temperature:
			d = atof(s);
			if (fabs(d) < 999999) WAYPT_SET(wpt, temperature, d);
			break;

		case fld_temperature_f:
			d = atof(s);
			if (fabs(d) < 999999) WAYPT_SET(wpt, temperature, FAHRENHEIT_TO_CELSIUS(d));
			break;

		case fld_heartrate:
			wpt->heartrate = atoi(s);
			if (unicsv_detect) unicsv_data_type = trkdata;
			break;

		case fld_cadence:
			wpt->cadence = atoi(s);
			if (unicsv_detect) unicsv_data_type = trkdata;
			break;

		case fld_proximity:
			if (parse_distance(s, &d, unicsv_proximityscale, MYNAME))
				WAYPT_SET(wpt, proximity, d);
			break;

		case fld_depth:
			if (parse_distance(s, &d, unicsv_depthscale, MYNAME))
				WAYPT_SET(wpt, depth, d);
			break;
			
		case fld_symbol:
			wpt->icon_descr = xstrdup(s);
			wpt->wpt_flags.icon_descr_is_dynamic = 1;
			break;
		
		case fld_iso_time:
			is_localtime = 2;	/* fix result */
			wpt->creation_time = xml_parse_time(s, &wpt->microseconds);
			break;

		case fld_time:
			if ((is_localtime < 2) && (time < 0)) {
				time = unicsv_parse_time(s, &msec, &date);
				is_localtime = 1;
			}
			break;

		case fld_date: 
			if ((is_localtime < 2) && (date < 0)) {
				date = unicsv_parse_date(s, NULL);
				is_localtime = 1;
			}
			break;
			
		case fld_year:
			ymd.tm_year = atoi(s);
			break;

		case fld_month:
			ymd.tm_mon = atoi(s);
			break;

		case fld_day:
			ymd.tm_mday = atoi(s);
			break;

		case fld_hour:
			ymd.tm_hour = atoi(s);
			break;

		case fld_min:
			ymd.tm_min = atoi(s);
			break;

		case fld_sec:
			ymd.tm_sec = atoi(s);
			break;

		case fld_datetime:
			/* not implemented */
			break;

		case fld_garmin_city:
		case fld_garmin_postal_code:
		case fld_garmin_state:
		case fld_garmin_country:
		case fld_garmin_addr:
		case fld_garmin_phone_nr:
		case fld_garmin_facility:
			gmsd = GMSD_FIND(wpt);
			if (! gmsd) {
				gmsd = garmin_fs_alloc(-1);
				fs_chain_add(&wpt->fs, (format_specific_data *) gmsd);
			}
			switch(unicsv_fields_tab[column]) {
			case fld_garmin_city: GMSD_SETSTR(city, s); break;
			case fld_garmin_postal_code: GMSD_SETSTR(postal_code, s); break;
			case fld_garmin_state: GMSD_SETSTR(state, s); break;
			case fld_garmin_country: GMSD_SETSTR(country, s); break;
			case fld_garmin_addr: GMSD_SETSTR(addr, s); break;
			case fld_garmin_phone_nr: GMSD_SETSTR(phone_nr, s); break;
			case fld_garmin_facility: GMSD_SETSTR(facility, s); break;
			default: break;
			}
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
	
	if (is_localtime < 2) {	/* not fixed */
		if ((time >= 0) && (date >= 0)) {
			time_t t = date + time;
			
			if (is_localtime) {
				struct tm tm;
				tm = *gmtime(&t);
				if (opt_utc)
					wpt->creation_time = mkgmtime(&tm);
				else
					wpt->creation_time = mklocaltime(&tm);
			}
			else
				wpt->creation_time = t;
		}
		else if (time >= 0)
			wpt->creation_time = time;
		else if (date >= 0)
			wpt->creation_time = date;
		else if (ymd.tm_year || ymd.tm_mon || ymd.tm_mday) {
			if (ymd.tm_year < 100) {
				if (ymd.tm_year <= 70) ymd.tm_year += 2000;
				else ymd.tm_year += 1900;
			}
			ymd.tm_year -= 1900;
			
			if (ymd.tm_mon == 0) ymd.tm_mon = 1;
			if (ymd.tm_mday == 0) ymd.tm_mday = 1;
			
			ymd.tm_mon--;
			if (opt_utc)
				wpt->creation_time = mkgmtime(&ymd);
			else
				wpt->creation_time = mklocaltime(&ymd);
		}
		else if (ymd.tm_hour || ymd.tm_min || ymd.tm_sec) {
			if (opt_utc)
				wpt->creation_time = mkgmtime(&ymd);
			else
				wpt->creation_time = mklocaltime(&ymd);
		}

		if (msec >= 0)
			wpt->microseconds = msec;
		
		if (opt_utc)
			wpt->creation_time += atoi(opt_utc) * SECONDS_PER_HOUR;
	}
	
	/* utm/bng/swiss can be optional */

	if ((wpt->latitude == unicsv_unknown) && (wpt->longitude == unicsv_unknown)) {
		if (utm_zone != -9999) {
			GPS_Math_UTM_EN_To_Known_Datum(&wpt->latitude, &wpt->longitude,
				utm_easting, utm_northing, utm_zone, utm_zc, unicsv_datum_idx);
		}
		else if (bng_zone[0]) {
			if (! GPS_Math_UKOSMap_To_WGS84_M(
				bng_zone, bng_easting, bng_northing,
				&wpt->latitude, &wpt->longitude))
			fatal(MYNAME ": Unable to convert BNG coordinates (%s %.f %.f)!\n",
				bng_zone, bng_easting, bng_northing);
			src_datum = DATUM_WGS84;	/* don't convert afterwards */
		}
		else if ((swiss_easting != unicsv_unknown) && (swiss_northing != unicsv_unknown)) {
			GPS_Math_CH1903_NGEN_To_WGS84(swiss_easting, swiss_northing,
				&wpt->latitude, &wpt->longitude);
			src_datum = DATUM_WGS84;	/* don't convert afterwards */
		}
	}
	
	if ((src_datum != DATUM_WGS84) && 
	    (wpt->latitude == unicsv_unknown) && (wpt->longitude == unicsv_unknown)) {
		double alt;
		GPS_Math_Known_Datum_To_WGS84_M(wpt->latitude, wpt->longitude, (double) 0.0, 
			&wpt->latitude, &wpt->longitude, &alt, src_datum);
	}

	switch(unicsv_data_type) {
	case rtedata:
		if (! unicsv_route) {
			unicsv_route = route_head_alloc();
			route_add_head(unicsv_route);
		}
		route_add_wpt(unicsv_route, wpt);
		break;
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
	
	while ((buff = gbfgetstr(fin))) {
	    	buff = lrtrim(buff);
		if ((*buff == '\0') || (*buff == '#')) continue;
		unicsv_parse_one_line(buff);
	}
}

/* =========================================================================== */

static char *
strassign(char **old, char *new)
{
	if (*old) xfree(*old);
	*old = new;
	return new;
}

static void
unicsv_fatal_outside(const waypoint *wpt)
{
	gbfprintf(fout, "#####\n");
	fatal(MYNAME ": %s (%s) is outside of convertable area \"%s\"!\n",
		wpt->shortname ? wpt->shortname : "Waypoint",
		pretty_deg_format(wpt->latitude, wpt->longitude, 'd', NULL, 0),
		gt_get_mps_grid_longname(unicsv_grid_idx, MYNAME));
}

static void
unicsv_print_str(const char *str)
{
	if (str && *str) {
		char *cout, *cx;
		
		cout = strenquote(str, UNICSV_QUOT_CHAR);

		while ((cx = strstr(cout, "\r\n"))) {
			memmove(cx, cx + 1, strlen(cx));
			*cx++ = ',';
			lrtrim(cx);
		}
		while ((cx = strchr(cout, '\r'))) {
			*cx++ = ',';
			lrtrim(cx);
		}
		while ((cx = strchr(cout, '\n'))) {
			*cx++ = ',';
			lrtrim(cx);
		}
		
		gbfprintf(fout, "%s%s", unicsv_fieldsep, cout);
		xfree(cout);
	}
	else gbfputs(unicsv_fieldsep, fout);
}

#define BIT_OF(a) (1ULL << a)
#define FIELD_USED(a) (unicsv_outp_flags & (1ULL << a))

static void 
unicsv_waypt_enum_cb(const waypoint *wpt)
{
	char *shortname;
	garmin_fs_t *gmsd;

	shortname = (wpt->shortname) ? wpt->shortname : "";
	gmsd = GMSD_FIND(wpt);
	
	if (*shortname) unicsv_outp_flags |= BIT_OF(fld_shortname);
	if (wpt->altitude != unknown_alt) unicsv_outp_flags |= BIT_OF(fld_altitude);
	if (wpt->icon_descr && *wpt->icon_descr) unicsv_outp_flags |= BIT_OF(fld_symbol);
	if (wpt->description && *wpt->description && (strcmp(shortname, wpt->description) != 0))
		unicsv_outp_flags |= BIT_OF(fld_description);
	if (wpt->notes && *wpt->notes && (strcmp(shortname, wpt->notes) != 0)) {
		if ((! wpt->description) || (strcmp(wpt->description, wpt->notes) != 0))
		    	unicsv_outp_flags |= BIT_OF(fld_notes);
	}
	if (wpt->url && *wpt->url) unicsv_outp_flags |= BIT_OF(fld_url);
	if (wpt->creation_time != 0) {
		unicsv_outp_flags |= BIT_OF(fld_time);
		if (wpt->creation_time >= SECONDS_PER_DAY)
			unicsv_outp_flags |= BIT_OF(fld_date);
	}

	if (wpt->fix != fix_unknown) unicsv_outp_flags |= BIT_OF(fld_fix);
	if (wpt->vdop > 0) unicsv_outp_flags |= BIT_OF(fld_vdop);
	if (wpt->hdop > 0) unicsv_outp_flags |= BIT_OF(fld_hdop);
	if (wpt->pdop > 0) unicsv_outp_flags |= BIT_OF(fld_pdop);
	if (wpt->sat > 0) unicsv_outp_flags |= BIT_OF(fld_sat);
	if (wpt->heartrate != 0) unicsv_outp_flags |= BIT_OF(fld_heartrate);
	if (wpt->cadence != 0) unicsv_outp_flags |= BIT_OF(fld_cadence);

	/* "flagged" waypoint members */
	if WAYPT_HAS(wpt, course) unicsv_outp_flags |= BIT_OF(fld_course);
	if WAYPT_HAS(wpt, depth) unicsv_outp_flags |= BIT_OF(fld_depth);
	if WAYPT_HAS(wpt, speed) unicsv_outp_flags |= BIT_OF(fld_speed);
	if WAYPT_HAS(wpt, proximity) unicsv_outp_flags |= BIT_OF(fld_proximity);
	if WAYPT_HAS(wpt, temperature) unicsv_outp_flags |= BIT_OF(fld_temperature);
	
	if (gmsd) {
		if GMSD_HAS(addr) unicsv_outp_flags |= BIT_OF(fld_garmin_addr);
		if GMSD_HAS(city) unicsv_outp_flags |= BIT_OF(fld_garmin_city);
		if GMSD_HAS(country) unicsv_outp_flags |= BIT_OF(fld_garmin_country);
		if GMSD_HAS(phone_nr) unicsv_outp_flags |= BIT_OF(fld_garmin_phone_nr);
		if GMSD_HAS(postal_code) unicsv_outp_flags |= BIT_OF(fld_garmin_postal_code);
		if GMSD_HAS(state) unicsv_outp_flags |= BIT_OF(fld_garmin_state);
		if GMSD_HAS(facility) unicsv_outp_flags |= BIT_OF(fld_garmin_facility);
	}
}

static void 
unicsv_waypt_disp_cb(const waypoint *wpt)
{
	double lat, lon, alt;
	char *cout = NULL;
	char *shortname;
	garmin_fs_t *gmsd;

	unicsv_waypt_ct++;

	shortname = (wpt->shortname) ? wpt->shortname : "";
	gmsd = GMSD_FIND(wpt);
	
	if (unicsv_datum_idx == DATUM_WGS84) {
		lat = wpt->latitude;
		lon = wpt->longitude;
		alt = wpt->altitude;
	}
	else {
		GPS_Math_WGS84_To_Known_Datum_M(wpt->latitude, wpt->longitude, 0.0,
			&lat, &lon, &alt, unicsv_datum_idx);
	}

	gbfprintf(fout, "%d%s", unicsv_waypt_ct, unicsv_fieldsep);
	
	switch(unicsv_grid_idx) {

	case grid_lat_lon_ddd:
		cout = pretty_deg_format(lat, lon, 'd', unicsv_fieldsep, 0);
		gbfputs(cout, fout);
		break;

	case grid_lat_lon_dmm:
		cout = pretty_deg_format(lat, lon, 'm', unicsv_fieldsep, 0);
		gbfputs(cout, fout);
		break;
		
	case grid_lat_lon_dms:
		cout = pretty_deg_format(lat, lon, 's', unicsv_fieldsep, 0);
		gbfputs(strassign(&cout, strenquote(cout, UNICSV_QUOT_CHAR)), fout);
		break;

	case grid_bng: {
		char map[3];
		double north, east;

		if (! GPS_Math_WGS84_To_UKOSMap_M(wpt->latitude, wpt->longitude, &east, &north, map))
			unicsv_fatal_outside(wpt);
		gbfprintf(fout, "%s%s%5.0f%s%5.0f",
			map, unicsv_fieldsep, 
			east, unicsv_fieldsep, 
			north);
		break;
	}
	case grid_utm: {
		int zone;
		char zonec;
		double north, east;

		if (! GPS_Math_Known_Datum_To_UTM_EN(lat, lon,
			&east, &north, &zone, &zonec, unicsv_datum_idx))
			unicsv_fatal_outside(wpt);
		gbfprintf(fout, "%02d%s%c%s%.0f%s%.0f",
			zone, unicsv_fieldsep, 
			zonec, unicsv_fieldsep,
			east, unicsv_fieldsep, 
			north);
		break;
	}
	case grid_swiss: {
		double north, east;
		
		if (! GPS_Math_WGS84_To_CH1903_NGEN(wpt->latitude, wpt->longitude, &east, &north))
			unicsv_fatal_outside(wpt);
		gbfprintf(fout, "%.f%s%.f%s",
			east, unicsv_fieldsep, north, unicsv_fieldsep);

	}
	default:
		gbfprintf(fout, "%.6f%s%.6f", lat, unicsv_fieldsep, lon);
		break;
	}
	
	if (cout) xfree(cout);

	if FIELD_USED(fld_shortname) unicsv_print_str(shortname);
	if FIELD_USED(fld_altitude) {
		if (wpt->altitude != unknown_alt)
			gbfprintf(fout, "%s%.1f", unicsv_fieldsep, wpt->altitude);
		else
			gbfputs(unicsv_fieldsep, fout);
	}
	if FIELD_USED(fld_description) unicsv_print_str(wpt->description);
	if FIELD_USED(fld_notes) unicsv_print_str(wpt->notes);
	if FIELD_USED(fld_symbol)
		unicsv_print_str((wpt->icon_descr != NULL) ? wpt->icon_descr : "Waypoint");
	if FIELD_USED(fld_depth) {
		if WAYPT_HAS(wpt, depth)
			gbfprintf(fout, "%s%.1f", unicsv_fieldsep, wpt->depth);
		else
			gbfputs(unicsv_fieldsep, fout);
	}
	if FIELD_USED(fld_proximity) {
		if WAYPT_HAS(wpt, proximity)
			gbfprintf(fout, "%s%.f", unicsv_fieldsep, wpt->proximity);
		else
			gbfputs(unicsv_fieldsep, fout);
	}
	if FIELD_USED(fld_temperature) {
		if WAYPT_HAS(wpt, temperature)
			gbfprintf(fout, "%s%.1f", unicsv_fieldsep, wpt->temperature);
		else
			gbfputs(unicsv_fieldsep, fout);
	}
	if FIELD_USED(fld_speed) {
		if WAYPT_HAS(wpt, speed)
			gbfprintf(fout, "%s%.1f", unicsv_fieldsep, wpt->speed);
		else
			gbfputs(unicsv_fieldsep, fout);
	}
	if FIELD_USED(fld_course) {
		if WAYPT_HAS(wpt, course)
			gbfprintf(fout, "%s%.1f", unicsv_fieldsep, wpt->course);
		else
			gbfputs(unicsv_fieldsep, fout);
	}
	if FIELD_USED(fld_fix) {
		char *fix;
		switch(wpt->fix) {
			case fix_none: fix = "none"; break;
			case fix_2d: fix = "2d"; break;
			case fix_3d: fix = "3d"; break;
			case fix_dgps: fix = "dgps"; break;
			case fix_pps: fix = "pps"; break;
			default: fix = NULL;
		}
		if (fix) unicsv_print_str(fix);
		else gbfputs(unicsv_fieldsep, fout);
	}
	if FIELD_USED(fld_hdop) {
		if (wpt->hdop > 0)
			gbfprintf(fout, "%s%.1f", unicsv_fieldsep, wpt->hdop);
		else
			gbfputs(unicsv_fieldsep, fout);
	}
	if FIELD_USED(fld_vdop) {
		if (wpt->vdop > 0)
			gbfprintf(fout, "%s%.1f", unicsv_fieldsep, wpt->vdop);
		else
			gbfputs(unicsv_fieldsep, fout);
	}
	if FIELD_USED(fld_pdop) {
		if (wpt->pdop > 0)
			gbfprintf(fout, "%s%.1f", unicsv_fieldsep, wpt->pdop);
		else
			gbfputs(unicsv_fieldsep, fout);
	}
	if FIELD_USED(fld_sat) {
		if (wpt->sat > 0)
			gbfprintf(fout, "%s%d", unicsv_fieldsep, wpt->sat);
		else
			gbfputs(unicsv_fieldsep, fout);
	}
	if FIELD_USED(fld_heartrate) {
		if (wpt->heartrate != 0)
			gbfprintf(fout, "%s%u", unicsv_fieldsep, wpt->heartrate);
		else
			gbfputs(unicsv_fieldsep, fout);
	}
	if FIELD_USED(fld_cadence) {
		if (wpt->cadence != 0)
			gbfprintf(fout, "%s%u", unicsv_fieldsep, wpt->cadence);
		else
			gbfputs(unicsv_fieldsep, fout);
	}
	if FIELD_USED(fld_date) {
		if (wpt->creation_time >= SECONDS_PER_DAY) {
			struct tm tm;
			char buf[32];
			time_t time = wpt->creation_time;
			
			if (opt_utc) {
				time += atoi(opt_utc) * SECONDS_PER_HOUR;
				tm = *gmtime(&time);
			}
			else tm = *localtime(&wpt->creation_time);
			tm.tm_year += 1900;
			tm.tm_mon += 1;
			snprintf(buf, sizeof(buf), "%04d/%02d/%02d", tm.tm_year, tm.tm_mon, tm.tm_mday);
			gbfprintf(fout, "%s%s", unicsv_fieldsep, buf);
		}
		else
			gbfputs(unicsv_fieldsep, fout);
	}
	if FIELD_USED(fld_time) {
		if (wpt->creation_time != 0) {
			struct tm tm;
			char buf[32], msec[12];
			time_t time = wpt->creation_time;
			
			if (opt_utc) {
				time += atoi(opt_utc) * SECONDS_PER_HOUR;
				tm = *gmtime(&time);
			}
			else tm = *localtime(&wpt->creation_time);
			snprintf(buf, sizeof(buf), "%02d:%02d:%02d", tm.tm_hour, tm.tm_min, tm.tm_sec);

			if (wpt->microseconds > 0) {
				int len = 6;
				int ms = wpt->microseconds;
				
				while (len && (ms % 10 == 0)) {
					ms /= 10;
					len--;
				}
				snprintf(msec, sizeof(msec), ".%0*d", len, ms);
				strcat(buf, msec);
			}
			gbfprintf(fout, "%s%s", unicsv_fieldsep, buf);
		}
		else
			gbfputs(unicsv_fieldsep, fout);
	}
	if FIELD_USED(fld_url) unicsv_print_str(wpt->url);

	if FIELD_USED(fld_garmin_facility) unicsv_print_str(GMSD_GET(facility, NULL));
	if FIELD_USED(fld_garmin_addr) unicsv_print_str(GMSD_GET(addr, NULL));
	if FIELD_USED(fld_garmin_city) unicsv_print_str(GMSD_GET(city, NULL));
	if FIELD_USED(fld_garmin_postal_code) unicsv_print_str(GMSD_GET(postal_code, NULL));
	if FIELD_USED(fld_garmin_state) unicsv_print_str(GMSD_GET(state, NULL));
	if FIELD_USED(fld_garmin_country) unicsv_print_str(GMSD_GET(country, NULL));
	if FIELD_USED(fld_garmin_phone_nr) unicsv_print_str(GMSD_GET(phone_nr, NULL));

	gbfputs(UNICSV_LINE_SEP, fout);
}

/* --------------------------------------------------------------------------- */


static void
unicsv_wr_init(const char *filename)
{
	fout = gbfopen(filename, "wb", MYNAME);

	unicsv_outp_flags = 0;
	unicsv_grid_idx = grid_unknown;
	unicsv_datum_idx = DATUM_WGS84;
	unicsv_fieldsep = UNICSV_FIELD_SEP;
	unicsv_waypt_ct = 0;

	if (opt_grid != NULL) {
		int i;
		
		if (sscanf(opt_grid, "%d", &i)) {
			unicsv_grid_idx = (grid_type) i;
			if ((unicsv_grid_idx < GRID_INDEX_MIN) || (unicsv_grid_idx > GRID_INDEX_MAX))
				fatal(MYNAME ": Grid index out of range (%d..%d)!\n",
					(int)GRID_INDEX_MIN, (int)GRID_INDEX_MAX);
		}
		else unicsv_grid_idx = gt_lookup_grid_type(opt_grid, MYNAME);
	}
	
	if (unicsv_grid_idx == grid_bng)
		/* force datum to "Ord Srvy Grt Britn" / OSGB36 */
		/* ! ignore parameter "Datum" ! */
		unicsv_datum_idx = DATUM_OSGB36;
	else if (unicsv_grid_idx == grid_swiss)
		/* ! ignore parameter "Datum" ! */
		unicsv_datum_idx = DATUM_WGS84;	/* internal, becomes CH1903 */
	else
		unicsv_datum_idx = gt_lookup_datum_index(opt_datum, MYNAME);
}

static void
unicsv_wr_deinit(void)
{
	gbfclose(fout);
}

static void
unicsv_wr(void)
{
	switch(global_opts.objective) {
	case wptdata:
		waypt_disp_all(unicsv_waypt_enum_cb);
		break;
	case trkdata:
		track_disp_all(NULL, NULL, unicsv_waypt_enum_cb);
		break;
	case rtedata:
		route_disp_all(NULL, NULL, unicsv_waypt_enum_cb);
		break;
	case posndata:
		fatal(MYNAME ": Realtime positioning not supported.\n"); 
	}

	gbfprintf(fout, "No%s", unicsv_fieldsep);

	switch(unicsv_grid_idx) {
	case grid_bng: 
/*		indexed parameters doesn't work under __win32__ (mingw)
		gbfprintf(fout, "BNG-Zone%1$sBNG-East%1$sBNG-North", unicsv_fieldsep);
*/
		gbfprintf(fout, "BNG-Zone%sBNG-East%sBNG-North",
			unicsv_fieldsep, unicsv_fieldsep);
		break;
	case grid_utm: 
/*		indexed parameters doesn't work under __win32__ (mingw)
		gbfprintf(fout, "BNG-Zone%1$sBNG-East%1$sBNG-North", unicsv_fieldsep);
*/
		gbfprintf(fout, "UTM-Zone%sUTM-Ch%sUTM-East%sUTM-North",
			unicsv_fieldsep, unicsv_fieldsep, unicsv_fieldsep);
		break;
	case grid_swiss: 
		gbfprintf(fout, "Swiss-East%sSwiss-North",
			unicsv_fieldsep);
		break;
	default: 
		gbfprintf(fout, "Latitude%sLongitude", unicsv_fieldsep);
	}

	if FIELD_USED(fld_shortname) gbfprintf(fout, "%sName", unicsv_fieldsep);
	if FIELD_USED(fld_altitude) gbfprintf(fout, "%sAltitude", unicsv_fieldsep);
	if FIELD_USED(fld_description) gbfprintf(fout, "%sDescription", unicsv_fieldsep);
	if FIELD_USED(fld_notes) gbfprintf(fout, "%sNotes", unicsv_fieldsep);
	if FIELD_USED(fld_symbol) gbfprintf(fout, "%sSymbol", unicsv_fieldsep);
	if FIELD_USED(fld_depth) gbfprintf(fout, "%sDepth", unicsv_fieldsep);
	if FIELD_USED(fld_proximity) gbfprintf(fout, "%sProximity", unicsv_fieldsep);
	if FIELD_USED(fld_temperature) gbfprintf(fout, "%sTemperature", unicsv_fieldsep);
	if FIELD_USED(fld_speed) gbfprintf(fout, "%sSpeed", unicsv_fieldsep);
	if FIELD_USED(fld_course) gbfprintf(fout, "%sCourse", unicsv_fieldsep);
	if FIELD_USED(fld_fix) gbfprintf(fout, "%sFIX", unicsv_fieldsep);
	if FIELD_USED(fld_hdop) gbfprintf(fout, "%sHDOP", unicsv_fieldsep);
	if FIELD_USED(fld_vdop) gbfprintf(fout, "%sVDOP", unicsv_fieldsep);
	if FIELD_USED(fld_pdop) gbfprintf(fout, "%sPDOP", unicsv_fieldsep);
	if FIELD_USED(fld_sat) gbfprintf(fout, "%sSatellites", unicsv_fieldsep);
	if FIELD_USED(fld_heartrate) gbfprintf(fout, "%sHeartrate", unicsv_fieldsep);
	if FIELD_USED(fld_cadence) gbfprintf(fout, "%sCadence", unicsv_fieldsep);
	if FIELD_USED(fld_date) gbfprintf(fout, "%sDate", unicsv_fieldsep);
	if FIELD_USED(fld_time) gbfprintf(fout, "%sTime", unicsv_fieldsep);
	if FIELD_USED(fld_url) gbfprintf(fout, "%sURL", unicsv_fieldsep);
	if FIELD_USED(fld_garmin_facility) gbfprintf(fout, "%sFacility", unicsv_fieldsep);
	if FIELD_USED(fld_garmin_addr) gbfprintf(fout, "%sAddress", unicsv_fieldsep);
	if FIELD_USED(fld_garmin_city) gbfprintf(fout, "%sCity", unicsv_fieldsep);
	if FIELD_USED(fld_garmin_postal_code) gbfprintf(fout, "%sPostalCode", unicsv_fieldsep);
	if FIELD_USED(fld_garmin_state) gbfprintf(fout, "%sState", unicsv_fieldsep);
	if FIELD_USED(fld_garmin_country) gbfprintf(fout, "%sCountry", unicsv_fieldsep);
	if FIELD_USED(fld_garmin_phone_nr) gbfprintf(fout, "%sPhone", unicsv_fieldsep);
	
	gbfputs(UNICSV_LINE_SEP, fout);

	switch(global_opts.objective) {
	case wptdata:
		waypt_disp_all(unicsv_waypt_disp_cb);
		break;
	case trkdata:
		track_disp_all(NULL, NULL, unicsv_waypt_disp_cb);
		break;
	case rtedata:
		route_disp_all(NULL, NULL, unicsv_waypt_disp_cb);
		break;
	default:
		break;
	}
}

/* --------------------------------------------------------------------------- */

ff_vecs_t unicsv_vecs = {
	ff_type_file,
	FF_CAP_RW_ALL,
	unicsv_rd_init,
	unicsv_wr_init,
	unicsv_rd_deinit, 
	unicsv_wr_deinit,
	unicsv_rd,
	unicsv_wr,
	NULL,
	unicsv_args,
	CET_CHARSET_ASCII, 0	/* can be changed with -c ... */
};
