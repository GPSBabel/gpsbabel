/*

    Support for MapSource Text Export (Tab delimited) files.
    
    Copyright (C) 2006 Olaf Klein, o.b.klein@t-online.de

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

#include <math.h>
#include <time.h>
#include <ctype.h>
#include <errno.h>
#include <stdarg.h>
#include "cet_util.h"
#include "csv_util.h"
#include "garmin_fs.h"
#include "garmin_tables.h"
#include "grtcirc.h"
#include "inifile.h"
#include "jeeps/gpsmath.h"
#include "strptime.h"

#define MYNAME "garmin_txt"

typedef struct gtxt_flags_s {
	unsigned int metric:1;
	unsigned int celsius:1;
	unsigned int utc:1;
	unsigned int enum_waypoints:1;
	unsigned int route_header_written:1;
	unsigned int track_header_written:1;
} gtxt_flags_t;

static FILE *fin, *fout;
static route_head *current_trk, *current_rte;
static int waypoints;
static int routepoints;
static waypoint **wpt_a;
static int wpt_a_ct;
static int grid_index;
static int datum_index;
static char *datum_str;
static int current_line;
static char *date_time_format = NULL;
static int precision = 3;
static time_t utc_offs = 0;

static gtxt_flags_t gtxt_flags;

typedef enum {
	waypt_header = 0,
	rtept_header,
	trkpt_header,
	route_header,
	track_header,
	unknown_header
} header_type;

#define MAX_HEADER_FIELDS 24

static char *header_lines[unknown_header + 1][MAX_HEADER_FIELDS];
static int header_fields[unknown_header + 1][MAX_HEADER_FIELDS];
static int header_ct[unknown_header + 1];

#define GARMIN_UNKNOWN_ALT 1.0e25f
#define DEFAULT_DISPLAY garmin_display_symbol_and_name
#define DEFAULT_DATE_FORMAT "dd/mm/yyyy"
#define DEFAULT_TIME_FORMAT "HH:mm:ss"

/* macros */

#define IS_VALID_ALT(a) (((a) != unknown_alt) && ((a) < GARMIN_UNKNOWN_ALT))
#define DUPSTR(a) (((a) != NULL) && ((a)[0] != 0)) ? xstrdup((a)) : NULL

static char *opt_datum = NULL;
static char *opt_dist = NULL;
static char *opt_temp = NULL;
static char *opt_date_format = NULL;
static char *opt_time_format = NULL;
static char *opt_precision = NULL;
static char *opt_utc = NULL;

static
arglist_t garmin_txt_args[] = {
	{"date",  &opt_date_format, "Read/Write date format (i.e. yyyy/mm/dd)", NULL, ARGTYPE_STRING, ARG_NOMINMAX}, 
	{"datum", &opt_datum, 	    "GPS datum (def. WGS 84)", "WGS 84", ARGTYPE_STRING, ARG_NOMINMAX}, 
	{"dist",  &opt_dist,        "Distance unit [m=metric, s=statue]", "m", ARGTYPE_STRING, ARG_NOMINMAX},
	{"prec",  &opt_precision,   "Precision of coordinates", "3", ARGTYPE_INT, ARG_NOMINMAX},
	{"temp",  &opt_temp,        "Temperature unit [c=celsius, f=fahrenheit]", "c", ARGTYPE_STRING, ARG_NOMINMAX}, 
	{"time",  &opt_time_format, "Read/Write time format (i.e. HH:mm:ss xx)", NULL, ARGTYPE_STRING, ARG_NOMINMAX},
	{"utc",   &opt_utc,         "Write timestamps with offset x to UTC time", NULL, ARGTYPE_INT, "-23", "+23"},
	ARG_TERMINATOR
};

typedef struct info_s
{
	double length;
	time_t start;
	time_t time;
	double speed;
	double total;
	int count;
	waypoint *prev_wpt;
	waypoint *first_wpt;
	waypoint *last_wpt;
} info_t;

static info_t *route_info;
static int route_idx;
static info_t *cur_info;

static char *headers[] = {
	"Name\tDescription\tType\tPosition\tAltitude\tDepth\tProximity\tTemperature\t"
		"Display Mode\tColor\tSymbol\tFacility\tCity\tState\tCountry\t"
		"Date Modified\tLink\tCategories",
	"Waypoint Name\tDistance\tLeg Length\tCourse",
	"Position\tTime\tAltitude\tDepth\tLeg Length\tLeg Time\tLeg Speed\tLeg Course",
	"Name\tLength\tCourse\tWaypoints\tLink",
	"Name\tStart Time\tElapsed Time\tLength\tAverage Speed\tLink",
	NULL
};

/* helpers */

static char *
get_option_val(char *option, char *def)
{
	char *c = (option != NULL) ? option : def;
	return c;
}

static void
init_date_and_time_format(void)
{
	char *c, *origin, *src, *dest, prev;
	char buff[64], timef[32], datef[32];
	static char format[128];
	int offs, Y, H;
	
	memset(&datef, 0, sizeof(datef));
	memset(&timef, 0, sizeof(timef));
	
	origin = get_option_val(opt_date_format, DEFAULT_DATE_FORMAT);
	strncpy(buff, origin, sizeof(buff));
	
	src = lrtrim(buff);
	for (c = src; *c; c++) {
		*c = toupper(*c);
	}
	
	Y = 0;
	prev = '\0';
	offs = (src - buff);
	dest = datef;
	
	for (c = src; *c; c++, offs++) {
		if (isalpha(*c)) {
			switch(*c) {
			case 'J':
			case 'Y':
				if (prev != 'Y') { 
					strcat(dest, "%y"); 
					dest += 2;
					prev = 'Y'; 
				}
				Y++;
				if (Y > 2) *(dest-1) = 'Y';
				break;
			case 'M':
				if (prev != 'M') {
					strcat(dest, "%m");
					dest += 2;
					prev = 'M';
				}
				break;
			case 'D':
			case 'T': 
				if (prev != 'D') {
					strcat(dest, "%d");
					dest += 2;
					prev = 'D';
				}
				break;
			default:
				fatal(MYNAME ": Invalid character \"%c\" in date format!\n", origin[offs]);
			}
		}
		else if (ispunct(*c)) {
			*dest++ = *c;
		}
	}
	
	origin = get_option_val(opt_time_format, DEFAULT_TIME_FORMAT);
	strncpy(buff, origin, sizeof(buff));
	
	src = lrtrim(buff);
	H = 0;
	prev = '\0';
	offs = (src - buff);
	dest = timef;
	
	for (c = src; *c; c++, offs++) {
		if (isalpha(*c)) {
			switch(*c) {
			case 'S':
			case 's':
				if (prev != 'S') { 
					strcat(dest, "%S"); 
					dest += 2;
					prev = 'S'; 
				}
				break;
			case 'M':
			case 'm':
				if (prev != 'M') { 
					strcat(dest, "%M"); 
					dest += 2;
					prev = 'M'; 
				}
				break;
			case 'h':	/* 12-hour-clock */
				if (prev != 'H') {
					strcat(dest, "%l");
					dest += 2;
					prev = 'H';
				}
				else *(dest-1) = 'I';
				break;
			case 'H':	/* 24-hour-clock */
				if (prev != 'H') {
					strcat(dest, "%l");
					dest += 2;
					prev = 'H';
				}
				else *(dest-1) = 'H';
				break;
			case 'x':
				if (prev != 'X') {
					strcat(dest, "%P");
					dest += 2;
					prev = 'X';
				}
				else *(dest-1) = 'P';
				break;
			case 'X':
				if (prev != 'X') {
					strcat(dest, "%p");
					dest += 2;
					prev = 'X';
				}
				else *(dest-1) = 'p';
				break;
			default:
				fatal(MYNAME ": Invalid character \"%c\" in time format!\n", origin[offs]);
			}
		}
		else *dest++ = *c;
	}
	
	strcpy(format, datef);
	strcat(format, " ");
	strcat(format, timef);
	date_time_format = format;
}

static double
distance(double lat1, double lon1, double lat2, double lon2)
{
	double res = radtometers(gcdist(RAD(lat1), RAD(lon1), RAD(lat2), RAD(lon2)));
	if (res < 0.1) res = 0;	/* calc. diffs on 32- and 64-bit hosts */
	return res;
}

static double
course_deg(double lat1, double lon1, double lat2, double lon2)
{
	return DEG(heading(RAD(lat1), RAD(lon1), RAD(lat2), RAD(lon2)));
}

static double
waypt_distance(const waypoint *A, const waypoint *B)		/* !!! from A to B !!! */
{
	double dist = 0;
	garmin_fs_p gmsd;
	
	if ((A == NULL) || (B == NULL)) return 0;
	
	gmsd = GMSD_FIND(A);
	if ((gmsd != NULL) && (gmsd->ilinks != NULL))
	{
		garmin_ilink_t *link = gmsd->ilinks;
		
		dist = distance(A->latitude, A->longitude, link->lat, link->lon);
		while (link->next != NULL)
		{
			garmin_ilink_t *prev = link;
			link = link->next;
			dist += distance(prev->lat, prev->lon, link->lat, link->lon);
		}
		dist += distance(link->lat, link->lon, B->latitude, B->longitude);
	} else 
	{
		dist = distance(A->latitude, A->longitude, B->latitude, B->longitude);
	}
	return dist;
}

static void
convert_datum(waypoint *wpt, const int to_internal_wgs84, double *dest_lat, double *dest_lon)
{
	double alt;

	if (datum_index == 118 /* WGS 84 */) {
		if (to_internal_wgs84 == 0) {
			*dest_lat = wpt->latitude;
			*dest_lon = wpt->longitude;
		}
		return;
	}
		
	if (to_internal_wgs84) { /* convert the waypoint himself */
		GPS_Math_Known_Datum_To_WGS84_M(wpt->latitude, wpt->longitude, 0.0,
			&wpt->latitude, &wpt->longitude, &alt, datum_index);
	} else
		GPS_Math_WGS84_To_Known_Datum_M(wpt->latitude, wpt->longitude, 0.0,
			dest_lat, dest_lon, &alt, datum_index);
}

/* WRITER *****************************************************************/

/* Waypoint preparation */

static void
enum_waypt_cb(const waypoint *wpt)
{
	garmin_fs_p gmsd;
	int wpt_class;
	
	gmsd = GMSD_FIND(wpt); 
	wpt_class = GMSD_GET(wpt_class, 0);
	if (wpt_class < 0x80)
	{
		int i;
		
		if (gtxt_flags.enum_waypoints)			/* enumerate only */
		{
			waypoints++;
			return;
		}
		for (i = 0; i < wpt_a_ct; i++) {		/* check for duplicates */
			waypoint *tmp = wpt_a[i];
			if (case_ignore_strcmp(tmp->shortname, wpt->shortname) == 0)
			{
				wpt_a[i] = (waypoint *)wpt;
				waypoints--;
				return;
				
			}
		}
		wpt_a[wpt_a_ct++] = (waypoint *)wpt;
	}

}

static int
sort_waypt_cb(const void *a, const void *b)
{
	const waypoint *wa = *(waypoint **)a;
	const waypoint *wb = *(waypoint **)b;

	return case_ignore_strcmp(wa->shortname, wb->shortname);
}


/* common route and track pre-work */

static void
prework_hdr_cb(const route_head *rte)
{
	cur_info = &route_info[route_idx];
	cur_info->prev_wpt = NULL;
	cur_info->length = 0;
	cur_info->time = 0;
}

static void
prework_tlr_cb(const route_head *rte)
{
	cur_info->last_wpt = cur_info->prev_wpt;
	route_idx++;
}

static void
prework_wpt_cb(const waypoint *wpt)
{
	waypoint *prev = cur_info->prev_wpt;

	if (prev != NULL) {
		cur_info->time += (wpt->creation_time - prev->creation_time);
		cur_info->length += waypt_distance(prev, wpt);
	}
	else {
		cur_info->first_wpt = (waypoint *)wpt;
		cur_info->start = wpt->creation_time;
	}
	cur_info->prev_wpt = (waypoint *)wpt;
	cur_info->count++;
	routepoints++;
}


/* output helpers */

static void
print_position(const waypoint *wpt)
{
	int deg;
	double min;
	char num[64];
	double lat, lon;
	
	convert_datum((waypoint *)wpt, 0, &lat, &lon);

	deg = fabs(lat);
	min = (double)60.0 * (fabs(lat) - deg);
	snprintf(num, sizeof(num), "%0*.*f", precision + 3, precision, min);
	if (atoi(num) == 60) {
		deg++;
		min = 0;
	}
	fprintf(fout, "%c%d %0*.*f ", lat < 0.0 ? 'S' : 'N', deg, precision + 3, precision, min);
	
	deg = fabs(lon);
	min = (double)60.0 * (fabs(lon) - deg);
	snprintf(num, sizeof(num), "%0*.*f", precision + 3, precision, min);
	if (atoi(num) == 60) {
		deg++;
		min = 0;
	}
	fprintf(fout, "%c%d %0*.*f\t", lon < 0.0 ? 'W' : 'E', deg, precision + 3,  precision, min);
}

static void
print_date_and_time(const time_t time, const int time_only)
{
	struct tm tm;
	char tbuf[32];
	
	if (time_only) {
		tm = *gmtime(&time);
		snprintf(tbuf, sizeof(tbuf), "%d:%02d:%02d", tm.tm_hour, tm.tm_min, tm.tm_sec);
		fprintf(fout, "%s", tbuf);
	}
	else if (time != 0) {
		if (gtxt_flags.utc) {
			time_t t = time + utc_offs;
			tm = *gmtime(&t);
		}
		else
			tm = *localtime(&time);
		strftime(tbuf, sizeof(tbuf), date_time_format, &tm);
		fprintf(fout, "%s ", tbuf);
	}
	fprintf(fout, "\t");
}

static void
print_categories(gbuint16 categories)
{
	int i, count;
	char *c;
	
	if (categories == 0) return;
	
	count = 0;
	for (i = 0; i < 16; i++) {
		if ((categories & 1) != 0) {
			if (global_opts.inifile != NULL) {
				char key[3];
				snprintf(key, sizeof(key), "%d", i + 1);
				c = inifile_readstr(global_opts.inifile, GMSD_SECTION_CATEGORIES, key);
			}
			else c = NULL;
				
			fprintf(fout, "%s", (count++ > 0) ? "," : "");
			if (c == NULL)
				fprintf(fout, "Category %d", i+1);
			else
				fprintf(fout, "%s", c);
			
		}
		categories = categories >> 1;
	}
}

static void
print_course(const waypoint *A, const waypoint *B)		/* seems to be okay */
{
	if ((A != NULL) && (B != NULL) && (A != B)) {
		int course;
		course = si_round((double)360 - course_deg(A->latitude, A->longitude, B->latitude, B->longitude));
		if (course >= 360) {
			course -= 360;
		}
		cet_fprintf(fout, &cet_cs_vec_cp1252, "%d%c true", course, 0xB0);
	}
}

static void
print_distance(const double distance, const int no_scale, const int with_tab)
{
	double dist = distance;
	
	if (gtxt_flags.metric == 0) {
		dist = METERS_TO_FEET(dist);
	
		if ((dist < 5280) || no_scale)
			fprintf(fout, "%.f ft", dist);
		else {
			dist = METERS_TO_MILES(distance);
			if (dist < (double)100)
				fprintf(fout, "%.1f mi", dist);
			else
				fprintf(fout, "%d mi", si_round(dist));
		}
	}
	else
	{
		if ((dist < 1000) || no_scale)
			fprintf(fout, "%.f m", dist);
		else {
			dist = dist / (double)1000.0;
			if (dist < (double)100)
				fprintf(fout, "%.1f km", dist);
			else
				fprintf(fout, "%d km", si_round(dist));
		}
	}
	if (with_tab) fprintf(fout, "\t");
}

static void
print_speed(double *distance, time_t *time)
{
	int idist;
	double dist = *distance;
	char *unit;

	if (!gtxt_flags.metric) {
		dist = METERS_TO_MILES(dist) * 1000.0;
		unit = "mph";
	}
	else unit = "kph";
	idist = si_round(dist);
	
	if ((time != 0) && (idist > 0)) {
		double speed = dist / (double)*time * SECONDS_PER_HOUR / 1000;
		int ispeed = si_round(speed);
		
		if (speed < (double)0.01)
			fprintf(fout, "0 %s", unit);
		else if (ispeed < 2)
			fprintf(fout, "%.1f %s", speed, unit);
		else
			fprintf(fout, "%d %s", ispeed, unit);
	}
	else
		fprintf(fout, "0 %s", unit);
	fprintf(fout, "\t");
}

static void
print_string(const char *fmt, const char *string)
{
	char *c;
	char *buff;
	
	buff = xstrdup(string);
	/* remove unwanted characters from source string */
	for (c = buff; *c; c++) {
		if (iscntrl(*c)) {
			*c = ' ';
		}
	}
	fprintf(fout, fmt, buff);
	xfree(buff);
}


/* main cb's */

static void
write_waypt(const waypoint *wpt)
{
	unsigned char wpt_class;
	garmin_fs_p gmsd;
	char *wpt_type;
	char *dspl_mode;
	char *country;
	double x;
	int i, icon, dynamic;
	char *icon_descr;
	
	gmsd = GMSD_FIND(wpt);

	i = GMSD_GET(display, 0);
	if (i > GT_DISPLAY_MODE_MAX) i = 0;
	dspl_mode = gt_display_mode_names[i];

	wpt_class = GMSD_GET(wpt_class, 0);
	if (wpt_class <= gt_waypt_class_map_line)
		wpt_type = gt_waypt_class_names[wpt_class];
	else
		wpt_type = gt_waypt_class_names[0];
	
	fprintf(fout, "Waypoint\t%s\t", (wpt->shortname) ? wpt->shortname : "");
	if (wpt_class <= gt_waypt_class_airport_ndb) {
		char *temp = wpt->notes;
		if (temp == NULL) {
			if (wpt->description && (strcmp(wpt->description, wpt->shortname) != 0))
				temp = wpt->description;
			else
				temp = "";
		}
		print_string("%s\t", temp);
	}
	else
		fprintf(fout, "\t");
	fprintf(fout, "%s\t", wpt_type);
		
	print_position(wpt);
	
	if IS_VALID_ALT(wpt->altitude)
		print_distance(wpt->altitude, 1, 0);
	fprintf(fout, "\t");
	
	x = GMSD_GET(depth, unknown_alt);
	if (x != unknown_alt)
		print_distance(x, 1, 0);
	fprintf(fout, "\t");

	x = GMSD_GET(proximity, unknown_alt);
	if (x != unknown_alt)
		print_distance(x, 0, 0);
	fprintf(fout, "\t");
	
	x = GMSD_GET(temperature, unknown_alt);
	if (x != unknown_alt) {
		if (gtxt_flags.celsius)
			fprintf(fout, "%.f C", x);
		else
			fprintf(fout, "%.f F", (x * 1.8) + 32);
	}
	fprintf(fout, "\t%s\t", dspl_mode);
	
	fprintf(fout, "Unknown\t"); 				/* Color is fixed: Unknown */

	icon = GMSD_GET(icon, -1);
	if (icon == -1) {
		icon = gt_find_icon_number_from_desc(wpt->icon_descr, GDB);
	}
	icon_descr = gt_find_desc_from_icon_number(icon, GDB, &dynamic);
	print_string("%s\t", icon_descr);
	if (dynamic) xfree(icon_descr);
	
	print_string("%s\t", GMSD_GET(facility, ""));
	print_string("%s\t", GMSD_GET(city, ""));
	print_string("%s\t", GMSD_GET(state, ""));
	country = gt_get_icao_country(GMSD_GET(cc, ""));
	print_string("%s\t", (country != NULL) ? country : "");
	print_date_and_time(wpt->creation_time, 0);
	print_string("%s\t", wpt->url ? wpt->url : ""); 
	print_categories(GMSD_GET(category, 0));
	
	fprintf(fout, "\r\n");
}

static void
route_disp_hdr_cb(const route_head *rte)
{
	current_trk = (route_head *)rte;
	cur_info = &route_info[route_idx];
	cur_info->prev_wpt = NULL;
	cur_info->total = 0;
	if (rte->rte_waypt_ct <= 0) return;

	if (!gtxt_flags.route_header_written) {
		gtxt_flags.route_header_written = 1;
		fprintf(fout, "\r\n\r\nHeader\t%s\r\n", headers[route_header]);
	}

	print_string("\r\nRoute\t%s\t", current_trk->rte_name ? current_trk->rte_name : "");
	print_distance(cur_info->length, 0, 1);
	print_course(cur_info->first_wpt, cur_info->last_wpt);
	fprintf(fout, "\t%d waypoints\t", cur_info->count);
	print_string("%s\r\n", rte->rte_url ? rte->rte_url : "");
	fprintf(fout, "\r\nHeader\t%s\r\n\r\n", headers[rtept_header]);
}

static void
route_disp_tlr_cb(const route_head *rte)
{
	route_idx++;
}

static void
route_disp_wpt_cb(const waypoint *wpt)
{
	waypoint *prev = cur_info->prev_wpt;
	
	fprintf(fout, "Route Waypoint\t");
	fprintf(fout, "%s\t", wpt->shortname);

	if (prev != NULL)
	{
		double dist = waypt_distance(prev, wpt);
		cur_info->total += dist;
		print_distance(cur_info->total, 0, 1);
		print_distance(dist, 0, 1);
		print_course(prev, wpt);
	}
	else 
		print_distance(0, 1, 0);	

	fprintf(fout, "\r\n");
	
	cur_info->prev_wpt = (waypoint *)wpt;
}

static void
track_disp_hdr_cb(const route_head *track)
{
	cur_info = &route_info[route_idx];
	cur_info->prev_wpt = NULL;
	cur_info->total = 0;
	current_trk = (route_head *)track;
	if (track->rte_waypt_ct <= 0) return;
	
	if (!gtxt_flags.track_header_written) {
		gtxt_flags.track_header_written = 1;
		fprintf(fout, "\r\n\r\nHeader\t%s\r\n", headers[track_header]);
	}
		
	print_string("\r\nTrack\t%s\t", current_trk->rte_name ? current_trk->rte_name : "");
	print_date_and_time(cur_info->start, 0);
	print_date_and_time(cur_info->time, 1);
	print_distance(cur_info->length, 0, 1);
	print_speed(&cur_info->length, &cur_info->time);
	print_string("%s", (track->rte_url != NULL) ? track->rte_url : "");
	fprintf(fout, "\r\n\r\nHeader\t%s\r\n\r\n", headers[trkpt_header]);
}

static void
track_disp_tlr_cb(const route_head *track)
{
	route_idx++;
}

static void
track_disp_wpt_cb(const waypoint *wpt)
{
	waypoint *prev = cur_info->prev_wpt;
	time_t delta;
	double dist;
	
	fprintf(fout, "Trackpoint\t");

	print_position(wpt);
	print_date_and_time(wpt->creation_time, 0);
	if IS_VALID_ALT(wpt->altitude)
		print_distance(wpt->altitude, 1, 0);
	fprintf(fout, "\t0.0 %s", (gtxt_flags.metric) ? "m" : "ft");
	if (prev != NULL) {
		fprintf(fout, "\t");
		delta = wpt->creation_time - prev->creation_time;
		dist = distance(prev->latitude, prev->longitude, wpt->latitude, wpt->longitude);
		print_distance(dist, 0, 1);
		print_date_and_time(delta, 1);
		print_speed(&dist, &delta);
		print_course(prev, wpt);
	}
	fprintf(fout, "\r\n");
	
	cur_info->prev_wpt = (waypoint *)wpt;
}

/*******************************************************************************
* %%%        global callbacks called by gpsbabel main process              %%% *
*******************************************************************************/

static void
garmin_txt_wr_init(const char *fname)
{
	memset(&gtxt_flags, 0, sizeof(gtxt_flags));
	
	fout = xfopen(fname, "wb", MYNAME);
	grid_index = 1;
	
	gtxt_flags.metric = (toupper(*get_option_val(opt_dist, "m")) == 'M');
	gtxt_flags.celsius = (toupper(*get_option_val(opt_temp, "c")) == 'C');
	init_date_and_time_format();
	if (opt_precision) {
		precision = atoi(opt_precision);
		is_fatal(precision < 0, MYNAME ": Invalid precision (%s)!", opt_precision);
	}
	datum_str = get_option_val(opt_datum, NULL);
	datum_index = GPS_Lookup_Datum_Index(datum_str);
	is_fatal(datum_index < 0, MYNAME ": Invalid or unknown gps datum (%s)!", datum_str);
	
	if (opt_utc != NULL) {
		if (case_ignore_strcmp(opt_utc, "utc") == 0)
			utc_offs = 0;
		else
			utc_offs = atoi(opt_utc);
		utc_offs *= (60 * 60);
		gtxt_flags.utc = 1;
	}
}

static void
garmin_txt_wr_deinit(void)
{
	fclose(fout);
}

static void
garmin_txt_write(void)
{
	cet_fprintf(fout, &cet_cs_vec_cp1252, "Grid\tLat/Lon hddd%cmm.mmm'\r\n", 0xB0);
	fprintf(fout, "Datum\t%s\r\n\r\n", datum_str);

	waypoints = 0;
	gtxt_flags.enum_waypoints = 1;			/* enum all waypoints */
	waypt_disp_all(enum_waypt_cb);
	route_disp_all(NULL, NULL, enum_waypt_cb);
	gtxt_flags.enum_waypoints = 0;
	
	if (waypoints > 0) {
		int i;
		
		wpt_a_ct = 0;
		wpt_a = (waypoint **)xcalloc(waypoints, sizeof(*wpt_a));
		waypt_disp_all(enum_waypt_cb);
		route_disp_all(NULL, NULL, enum_waypt_cb);
		qsort(wpt_a, waypoints, sizeof(*wpt_a), sort_waypt_cb);
		
		fprintf(fout, "Header\t%s\r\n\r\n", headers[waypt_header]);
		for (i = 0; i < waypoints; i++)
		{
			waypoint *wpt = wpt_a[i];
			write_waypt(wpt);
		}
		xfree(wpt_a);
		
		route_idx = 0;
		route_info = xcalloc(route_count(), sizeof(struct info_s));
		routepoints = 0;
		route_disp_all(prework_hdr_cb, prework_tlr_cb, prework_wpt_cb);
		if (routepoints > 0)
		{
			route_idx = 0;
			route_disp_all(route_disp_hdr_cb, route_disp_tlr_cb, route_disp_wpt_cb);
		}
		xfree(route_info);
	}

	route_idx = 0;
	route_info = xcalloc(track_count(), sizeof(struct info_s));
	routepoints = 0;
	track_disp_all(prework_hdr_cb, prework_tlr_cb, prework_wpt_cb);
		
	if (routepoints > 0) {
		route_idx = 0;
		track_disp_all(track_disp_hdr_cb, track_disp_tlr_cb, track_disp_wpt_cb);
	}
	xfree(route_info);
}

/* READER *****************************************************************/

/* helpers */

static void
free_header(const header_type ht)
{
	int i;
	
	for (i = 0; i < MAX_HEADER_FIELDS; i++) {
		char *c = header_lines[ht][i];
		if (c != NULL) {
			xfree(c);
			header_lines[ht][i] = NULL;
		}
	}
	header_ct[ht] = 0;
	memset(header_fields[ht], 0, sizeof(header_fields[ht]));
}

/* data parsers */

static void
parse_position(const char *str, waypoint *wpt)
{
	double lat, lon;
	unsigned char lathemi, hemilon;
	int deg_lat, deg_lon, min_lat, min_lon;
	
	switch(grid_index) {
		case 0:
			sscanf(str, "%c%lf %c%lf", &lathemi, &lat, &hemilon, &lon);
			break;
		case 1:
			sscanf(str, "%c%d %lf %c%d %lf", &lathemi, &deg_lat, &lat, &hemilon, &deg_lon, &lon);
			lat = (double)deg_lat + (lat / (double)60);
			lon = (double)deg_lon + (lon / (double)60);
			break;
		case 2:
			sscanf(str, "%c%d %d %lf %c%d %d %lf", &lathemi, &deg_lat, &min_lat, &lat, &hemilon, &deg_lon, &min_lon, &lon);
			lat = (double)deg_lat + ((double)min_lat / (double)60) + (lat / (double)3600.0);
			lon = (double)deg_lon + ((double)min_lon / (double)60) + (lon / (double)3600.0);
			break;
	}
	
	if (lathemi == 'S') 
		wpt->latitude = -lat;
	else
		wpt->latitude = lat;

	if (hemilon == 'W')
		wpt->longitude = -lon;
	else
		wpt->longitude = lon;
}

static int
parse_distance(const char *str, double *value)
{
	double x;
	char *buff;
	
	if ((str == NULL) || (*str == '\0')) return 0;
	
	buff = xmalloc(strlen(str) + 1);
	sscanf(str, "%lf %s", &x, buff);
	
	if (case_ignore_strcmp(buff, "km") == 0) {
		*value = x * (double)1000;
	}
	else if (case_ignore_strcmp(buff, "m") == 0) {		/* meters */
		*value = x;
	} 
	else if (case_ignore_strcmp(buff, "ft") == 0) {		/* feet */
		*value = FEET_TO_METERS(x);
	}
	else if (case_ignore_strcmp(buff, "nm") == 0) {		/* mile (nautical / geographical) */
		*value = NMILES_TO_METERS(x);
	}
	else if (case_ignore_strcmp(buff, "mi") == 0) {		/* mile (statute) */
		*value = MILES_TO_METERS(x);
	}
	else if (case_ignore_strcmp(buff, "fa") == 0) {		/* fathom */
		*value = FATHOMS_TO_METERS(x);
	}
	else
		fatal(MYNAME ": Unknown distance unit \"%s\" at line %d!\n", str, current_line);
		
	xfree(buff);
	return 1;
}

static int
parse_date_and_time(char *str, time_t *value)
{
	struct tm tm;
	char *cerr, *cin;
	
	memset(&tm, 0, sizeof(tm));
	cin = lrtrim(str);
	if (*cin == '\0') return 0;
	
	cerr = strptime(cin, date_time_format, &tm);
	if (cerr == NULL) {
		cerr = strptime(cin, "%m/%d/%Y %I:%M:%S %p", &tm);
		is_fatal(cerr == NULL, MYNAME ": Invalid date or/and time \"%s\" at line %d!", cin, current_line);
	}
	
//	printf(MYNAME "_parse_date_and_time: %02d.%02d.%04d, %02d:%02d:%02d\n",
//		tm.tm_mday, tm.tm_mon+1, tm.tm_year+1900, tm.tm_hour, tm.tm_min, tm.tm_sec);

	*value = mktime(&tm);
	return 1;
}

static gbuint16
parse_categories(const char *str)
{
	char buff[256];
	gbuint16 val;
	gbuint16 res = 0;
	char *cin, *cx;
	
	if (*str == '\0') return 0;
	
	strncpy(buff, str, sizeof(buff));
	cin = lrtrim(buff);
	if (*cin == '\0') return 0;
	
	strcat(cin, ",");
	
	while ((cx = strchr(cin, ','))) {
		*cx++ = '\0';
		cin = lrtrim(cin);
		if (*cin != '\0') {
			if (!garmin_fs_convert_category(cin, &val)) 
				warning(MYNAME ": Unable to convert category \"%s\" at line %d!", cin, current_line);
			else
				res = res | val;
		}
		cin = cx;
	}
	return res;
}

static int
parse_temperature(const char *str, double *temperature)
{
	double value;
	unsigned char unit;
	
	if ((str == NULL) || (*str == '\0')) return 0;
	
	if (sscanf(str, "%lf %c", &value, &unit) == 2) {
		unit = toupper(unit);
		switch(unit) {
			case 'C': *temperature = value; break;
			case 'F': *temperature = FAHRENHEIT_TO_CELSIUS(value); break;
			default:
				fatal(MYNAME ": Unknown temperature unit \"%c\" at line %d!\n", unit, current_line);
		}
		return 1;
	}
	else
		fatal(MYNAME ": Invalid temperature \"%s\" at line %d!\n", str, current_line);
}

static void
parse_header(void)
{
	char *str;
	int column = -1;
	
	free_header(unknown_header);
	
	while ((str = csv_lineparse(NULL, "\t", "", column++))) {
		header_lines[unknown_header][column] = xstrdup(str);
		for (str = header_lines[unknown_header][column]; *str; str++) {
			*str = toupper(*str);
		}
		header_ct[unknown_header]++;
		if (header_ct[unknown_header] >= MAX_HEADER_FIELDS) break;
	}
}

static int
parse_display(const char *str, int *val)
{
	gt_display_modes_e i;
	
	if ((str == NULL) || (*str == '\0')) return 0;
	
	for (i = GT_DISPLAY_MODE_MIN; i <= GT_DISPLAY_MODE_MAX; i++) {
		if (case_ignore_strcmp(str, gt_display_mode_names[i]) == 0) {
			*val = i;
			return 1;
		}
	}
	warning(MYNAME ": Unknown display mode \"%s\" at line %d.\n", str, current_line);
}

static void
bind_fields(const header_type ht)
{
	int i;
	char *fields, *c;

	is_fatal((grid_index < 0) || (datum_index < 0), MYNAME ": Incomplete or invalid file header!");
		
	if (header_ct[unknown_header] <= 0) return;
	free_header(ht);
	
	/* make a copy of headers[ht], uppercase, replace "\t" with "\0" */
	
	i = strlen(headers[ht]);
	fields = xmalloc(i + 2);
	strcpy(fields, headers[ht]);
	strcat(fields, "\t");
	for (c = fields; *c; c++) {
		*c = toupper(*c);
	}
	c = fields;
	while ((c = strchr(c, '\t'))) *c++ = '\0';
	
	for (i = 0; i < header_ct[unknown_header]; i++) {
		char *name;
		int field_no;
		name = header_lines[ht][i] = header_lines[unknown_header][i];
		header_lines[unknown_header][i] = NULL;
		
		c = fields;
		field_no = 1;
		while (c != NULL) {
			if (strcmp(c, name) == 0) {
				header_fields[ht][i] = field_no;
#if 0
 				printf("Binding field \"%s\" to internal number %d (%d,%d)\n", name, field_no, ht, i);
#endif
				break;
			}
			field_no++;
			c = c + strlen(c) + 1;
		}
	}
	header_ct[unknown_header] = 0;
	xfree(fields);
}

static void
parse_grid(void)
{
	char *str = csv_lineparse(NULL, "\t", "", 1);
	if (str != NULL) {
		if (strstr(str, "dd.ddddd") != 0) grid_index = 0;
		else if (strstr(str, "mm.mmm") != 0) grid_index = 1;
		else if (strstr(str, "mm'ss.s") != 0) grid_index = 2;
		else fatal(MYNAME ": Unsupported grid (%s)!\n", str);
	}
	else
		fatal(MYNAME ": Missing grid headline!\n");
}

static void
parse_datum(void)
{
	char *str = csv_lineparse(NULL, "\t", "", 1);
	
	if (str != NULL) {
		datum_index = GPS_Lookup_Datum_Index(str);
	 	is_fatal(datum_index < 0, MYNAME ": Unsupported GPS datum \"%s\"!", str);
	}
	else
		fatal(MYNAME ": Missing GPS datum headline!\n");
}

static void
parse_waypoint(void)
{
	char *str;
	int column = -1;
	waypoint *wpt;
	garmin_fs_p gmsd = NULL;
	
	bind_fields(waypt_header);

	wpt = waypt_new();
	gmsd = garmin_fs_alloc(-1);
	fs_chain_add(&wpt->fs, (format_specific_data *) gmsd);

	while ((str = csv_lineparse(NULL, "\t", "", column++))) 
	{
		int i, dynamic;
		double d;
		int field_no = header_fields[waypt_header][column];
		
		switch(field_no) {
			case  1: wpt->shortname = DUPSTR(str); break;
			case  2: wpt->notes = DUPSTR(str); break;
			case  3:
				for (i = 0; i <= gt_waypt_class_map_line; i++) {
					if (case_ignore_strcmp(str, gt_waypt_class_names[i]) == 0) {
						GMSD_SET(wpt_class, i);
						break;
					}
				}
				break;
			case  4: parse_position(str, wpt); break;
			case  5: if (parse_distance(str, &d)) wpt->altitude = d; break;
			case  6: if (parse_distance(str, &d)) GMSD_SET(depth, d); break;
			case  7: if (parse_distance(str, &d)) GMSD_SET(proximity, d); break;
			case  8: if (parse_temperature(str, &d)) GMSD_SET(temperature, d); break;
			case  9: if (parse_display(str, &i)) GMSD_SET(display, i); break;
			case 10: break;	/* skip color */
			case 11: 
				i = gt_find_icon_number_from_desc(str, GDB);
				GMSD_SET(icon, i);
				wpt->icon_descr = gt_find_desc_from_icon_number(i, GDB, &dynamic);
				wpt->wpt_flags.icon_descr_is_dynamic = dynamic;
				break;
			case 12: GMSD_SETSTR(facility, str); break;
			case 13: GMSD_SETSTR(city, str); break;
			case 14: GMSD_SETSTR(state, str); break;
			case 15: GMSD_SETSTR(cc, gt_get_icao_cc(str, wpt->shortname)); break;
			case 16: parse_date_and_time(str, &wpt->creation_time); break;
			case 17: wpt->url = DUPSTR(str); break;
			case 18: GMSD_SET(category, parse_categories(str)); break;
			default: break;
		}
	}
	convert_datum(wpt, 1, NULL, NULL);
	waypt_add(wpt);
}

static void
parse_route_header(void)
{
	char *str;
	int column = -1;
	route_head *rte;
	
	rte = route_head_alloc();
	
	bind_fields(route_header);
	while ((str = csv_lineparse(NULL, "\t", "", column++))) {
		int field_no = header_fields[route_header][column];
		switch(field_no) {
			case 1: rte->rte_name = DUPSTR(str); break;
			case 5: rte->rte_url = DUPSTR(str); break;
		}
	}
	route_add_head(rte);
	current_rte = rte;
}

static void
parse_track_header(void)
{
	char *str;
	int column = -1;
	route_head *trk;
	
	bind_fields(track_header);
	trk = route_head_alloc();
	while ((str = csv_lineparse(NULL, "\t", "", column++))) {
		int field_no = header_fields[track_header][column];
		switch(field_no) {
			case 1: trk->rte_name = DUPSTR(str); break;
			case 6: trk->rte_url = DUPSTR(str); break;
		}
	}
	track_add_head(trk);
	current_trk = trk;
}

static void
parse_route_waypoint(void)
{
	char *str;
	int column = -1;
	waypoint *wpt = NULL;
	
	bind_fields(rtept_header);
	
	while ((str = csv_lineparse(NULL, "\t", "", column++))) {
		int field_no = header_fields[rtept_header][column];
		switch(field_no) {
			case 1: 
				is_fatal((*str == '\0'), MYNAME ": Route waypoint without name at line %d!\n", current_line);
				wpt = find_waypt_by_name(str);
				is_fatal((wpt == NULL), MYNAME ": Route waypoint \"%s\" not in waypoint list (line %d)!\n", str, current_line);
				wpt = waypt_dupe(wpt);
				break;
		}
	}
	if (wpt != NULL)
		route_add_wpt(current_rte, wpt);
}

static void
parse_track_waypoint(void)
{
	char *str;
	int column = -1;
	waypoint *wpt;
	
	bind_fields(trkpt_header);
	wpt = waypt_new();
	
	while ((str = csv_lineparse(NULL, "\t", "", column++))) {
		int field_no = header_fields[trkpt_header][column];
		switch(field_no) {
			case 1: parse_position(str, wpt); break;
			case 2: parse_date_and_time(str, &wpt->creation_time); break;
			case 3: parse_distance(str, &wpt->altitude); break;
		}
	}
	convert_datum(wpt, 1, NULL, NULL);
	route_add_wpt(current_trk, wpt);
}

/***************************************************************/

static void
garmin_rd_init(const char *fname)
{
	memset(&gtxt_flags, 0, sizeof(gtxt_flags));
	
	fin = xfopen(fname, "rb", MYNAME);
	memset(&header_ct, 0, sizeof(header_ct));

	datum_index = -1;
	grid_index = -1;
	
	init_date_and_time_format();
}

static void 
garmin_rd_deinit(void)
{
	header_type h;

	for (h = waypt_header; h <= unknown_header; h++) {
		free_header(h);
	}
	fclose(fin);
}

static void
garmin_txt_read(void)
{
	char buff[1024];

	current_line = 0;

	while ((fgets(buff, sizeof(buff), fin))) {
		char *cin;
		
		current_line++;
		cin = lrtrim(buff);
		if (*cin == '\0') continue;
		
		cin = csv_lineparse(cin, "\t", "", 0);
		
		if (cin == NULL) continue;
		else if (case_ignore_strcmp(cin, "Header") == 0) parse_header();
		else if (case_ignore_strcmp(cin, "Grid") == 0) parse_grid();
		else if (case_ignore_strcmp(cin, "Datum") == 0) parse_datum();
		else if (case_ignore_strcmp(cin, "Waypoint") == 0) parse_waypoint();
		else if (case_ignore_strcmp(cin, "Route Waypoint") == 0) parse_route_waypoint();
		else if (case_ignore_strcmp(cin, "Trackpoint") == 0) parse_track_waypoint();
		else if (case_ignore_strcmp(cin, "Route") == 0) parse_route_header();
		else if (case_ignore_strcmp(cin, "Track") == 0) parse_track_header();
		else if (case_ignore_strcmp(cin, "Map") == 0) /* do nothing */ ;
		else
			fatal(MYNAME ": Unknwon identifier (%s) at line %d!\n", cin, current_line);
			
		/* flush pending data */
		while (csv_lineparse(NULL, "\t", "", 0));
	}
}

ff_vecs_t garmin_txt_vecs = {
	ff_type_file,
	FF_CAP_RW_ALL,
	garmin_rd_init,	
	garmin_txt_wr_init,	
	garmin_rd_deinit,	
	garmin_txt_wr_deinit,	
	garmin_txt_read,
	garmin_txt_write,
	NULL,
	garmin_txt_args,
	CET_CHARSET_MS_ANSI, 0
};
