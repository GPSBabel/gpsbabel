/*

    Support for Suunto Trackmanager SDF format.

    Copyright (C) 2005,2007 Olaf Klein, o.b.klein@gpsbabel.org

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

/*
    2006/04/05: initial release (not published in GPSBbabel)
    2006/07/19: finished reader and writer for type 4,5,28 of ver. 1
    2006/10/31: remove wptdata from case statement (data_write)
    
    ToDo: Ascending/Descending
*/

#include "defs.h"

#if CSVFMTS_ENABLED

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "csv_util.h"
#include "strptime.h"
#include "jeeps/gpsmath.h"
#include "grtcirc.h"

#include <time.h>

#define MYNAME "stmsdf"

#define ALT(a) (a->altitude != unknown_alt) ? a->altitude : 0

typedef enum {
	sdf_unknown,
	sdf_header,
	sdf_points,
	sdf_custom
} sdf_section_e;

static gbfile *fin, *fout;

static int lineno;
static int datum;
static int filetype;
static route_head *route;
static queue trackpts;
static char *rte_name;
static char *rte_desc;

static waypoint *trkpt_out;
static route_head *trk_out;

static double trkpt_dist;
static double minalt, maxalt, maxspeed;
static double this_distance, all_dist;
static time_t this_time, all_time;
static double all_asc, all_desc;
static int this_index;			/* from 1 to ... */
static int all_points;
static int this_points;
static int saved_points;
static time_t start_time;
static unsigned char this_valid;
static short_handle short_h;

#define route_index this_index
#define track_index this_index
#define all_route_points all_points
#define all_track_points all_points
#define route_points this_points
#define track_points this_points
#define saved_track_points saved_points
#define this_route_valid this_valid

/* placeholders for options */

static char *opt_route_index;
static int opt_route_index_value;

static
arglist_t stmsdf_args[] = {
	{ "index", &opt_route_index,
		"Index of route (if more than one in source)", "1", ARGTYPE_INT, "1", NULL },
	ARG_TERMINATOR
};


/* ----------------------------------------------------------- */

static void
parse_header(char *line)
{
	char *str;
	char *key = NULL;
	char *prod = NULL;
	int column = -1;
	
	while ((str = csv_lineparse(line, "=", "", lineno)))
	{
		line = NULL;
		column++;
		
		switch(column) {
			case 0: 
				key = xstrdup(str);
				break;
			case 1: 
				if (case_ignore_strcmp(key, "DATUM") == 0) datum = GPS_Lookup_Datum_Index(str);
				else if (case_ignore_strcmp(key, "FILEVERSION") == 0) {
					int ver = atoi(str);
					is_fatal( (ver != 1),
						MYNAME ": This version '%d' is not yet supported. Please report!", ver);
				}
				else if (case_ignore_strcmp(key, "NAME") == 0) rte_name = xstrdup(str);
				else if (case_ignore_strcmp(key, "NOTES") == 0) /* ToDo */;
				else if (case_ignore_strcmp(key, "SOURCE") == 0) rte_desc = xstrdup(str);
				else if (case_ignore_strcmp(key, "TYPE") == 0) {
					filetype = atoi(str);
					switch(filetype) {
						case 4:	/* M9 TrackLog (Suunto Sail Manager) */
						case 5: /* route */
						case 28: /* X9 TrackLog (Suunto Trek Manager */
							break;
							
						case 78: prod = "S6 SkiChrono";
						case 79: prod = "S6 Skilog";
						
						default:
							if (prod == NULL) prod = "unknown";
							fatal(MYNAME ": Unsupported file type (%s, type %d)!\n", prod, filetype);
					}
				}
				break;
		}
	}
	if (key) xfree(key);
	
}

static int
track_qsort_cb(const void *a, const void *b)
{
	const waypoint *wa = *(waypoint **)a;
	const waypoint *wb = *(waypoint **)b;
	
	return wa->creation_time - wb->creation_time;
}

static void
finalize_tracks(void)
{
	waypoint **list;
	int count = 0;
	queue *elem, *tmp;
	int index;
	route_head *track = NULL;
	int trackno = 0;
	
	count = 0;
	QUEUE_FOR_EACH(&trackpts, elem, tmp) { count++; };
	if (count == 0) return;
	
	list = (void *)xmalloc(count * sizeof(*list));
	
	index = 0;
	QUEUE_FOR_EACH(&trackpts, elem, tmp) {
		list[index] = (waypoint *)elem;
		dequeue(elem);
		index++;
	}

	qsort(list, count, sizeof(*list), track_qsort_cb);

	for (index = 0; index < count; index++) {
		waypoint *wpt = list[index];
		if (wpt->microseconds == 2) {	/* log continued */
			track = NULL;
		}
		if (track == NULL) {
			track = route_head_alloc();
			track_add_head(track);
			trackno++;
			if (rte_name != NULL) {
				if (trackno > 1)
					xasprintf(&track->rte_name, "%s (%d)", rte_name, trackno);
				else
					track->rte_name	= xstrdup(rte_name);
			}
			if (rte_desc != NULL)
				track->rte_desc = xstrdup(rte_desc);
		}
		track_add_wpt(track, wpt);
		if (wpt->microseconds == 1) { /* log pause */
			track = NULL;
		}
		wpt->microseconds = 0;
	}

	xfree(list);
}

static void
parse_point(char *line)
{
	char *str;
	int column = -1;
	int what = -1;		/* -1 = unknown, 0 = tp, 1 = mp, 2 = wp, 3 = ap  */
	waypoint *wpt = NULL;
	char *cx;
	int hour, min, sec, day, month, year;

	year = hour = -1;
	
	while ((str = csv_lineparse(line, ",", "", lineno)))
	{

		line = NULL;
		column++;
		
		switch(column) {
			
			case 0: 
				if (strcmp(str, "\"TP\"") == 0) {
					what = 0;
					column++;	/* skip name */
				}
				else if (strcmp(str, "\"MP\"") == 0) what = 1;
				else if (strcmp(str, "\"WP\"") == 0) what = 2;
				else if (strcmp(str, "\"AP\"") == 0) what = 3;
				else {
					warning(MYNAME ": Unknown point type %s at line %d!\n", str, lineno);
					return;
				}
				wpt = waypt_new();
				break;
				
			case 1:
				wpt->shortname = csv_stringclean(str, "\"");
				if ((what == 2) || (what == 3)) column += 2;	/* doesn't have date and time */
				break;
			case 2:
				sscanf(str, "%d.%d.%d", &day, &month, &year);
				break;
			case 3:
				while ((cx = strchr(str, '.'))) *cx = ':';
				sscanf(str, "%d:%d:%d", &hour, &min, &sec);
				break;
			case 4:
				wpt->latitude = atof(str);
				break;
			case 5:
				wpt->longitude = atof(str);
				break;
			case 6:
				wpt->altitude = atof(str);
				break;
			case 7:
				switch(what) {
					case 0: 
						WAYPT_SET(wpt, speed, atof(str) * 3.6); break;
					case 3: 
						WAYPT_SET(wpt, proximity, atof(str));
						xasprintf(&wpt->notes, "Alarm point: radius=%s", str);
						break;
				}
				break;
			case 8:
				if (what == 0) WAYPT_SET(wpt, course, atof(str));
				break;
			case 9:
			case 10:
				break;
			case 11:
				if (what == 1) wpt->microseconds = atoi(str); /* memory point type */
				break;
		}
	}
	
	if ((year > -1) && (hour > -1)) {
		struct tm tm;
		
		memset(&tm, 0, sizeof(tm));
		
		tm.tm_year = year - 1900;
		tm.tm_mon = month - 1;
		tm.tm_mday = day;
		tm.tm_hour = hour;
		tm.tm_min = min;
		tm.tm_sec = sec;
		
		wpt->creation_time = mklocaltime(&tm);
	}
			
	if (datum != DATUM_WGS84) {
		double ht;
		GPS_Math_WGS84_To_Known_Datum_M(wpt->latitude, wpt->longitude, 0,
			&wpt->latitude, &wpt->longitude, &ht, datum);
	}
	
	switch(what) {
		case 0:
		case 1:
			ENQUEUE_TAIL(&trackpts, &wpt->Q);
			break;
		case 2:
		case 3:
			if (route == NULL) {
				route = route_head_alloc();
				route_add_head(route);
			}
			route_add_wpt(route, wpt);
			break;
	}
}

/* ----------------------------------------------------------- */

static void
rd_init(const char *fname)
{
	fin = gbfopen(fname, "r", MYNAME);
	
	lineno = 0;
	route = NULL;
	datum = DATUM_WGS84;
	filetype = 28;
	rte_name = rte_desc = NULL;

	QUEUE_INIT(&trackpts);
}

static void
rd_deinit(void)
{
	gbfclose(fin);
	if (rte_name) xfree(rte_name);
	if (rte_desc) xfree(rte_desc);
}

static void
data_read(void)
{
	char *buf;
	sdf_section_e section = sdf_unknown;
	
	while ((buf = gbfgetstr(fin)))
	{
		char *cin = lrtrim(buf);
		if ((lineno++ == 0) && fin->unicode) cet_convert_init(CET_CHARSET_UTF8, 1);
		
		if (*cin == '\0') continue;
		
		if (*cin == '[')
		{
			char *cend = strchr(++cin, ']');

			if (cend != NULL)
			{
				*cend = '\0';
				cin = lrtrim(cin);
			}
			if ((*cin == '\0') || (cend == NULL))
				fatal(MYNAME ": Invalid section header!\n");
				
			if (case_ignore_strcmp(cin, "HEADER") == 0) section = sdf_header;
			else if (case_ignore_strcmp(cin, "POINTS") == 0) section = sdf_points;
			else if (case_ignore_strncmp(cin, "CUSTOM", 6) == 0) section = sdf_custom;
			else {
				warning(MYNAME ": Unknown section \"%s\". Please report.\n", cin);
				section = sdf_unknown;
			}
		}
		else switch(section) 
		{
			case sdf_header: 
				parse_header(cin);
				break;
			case sdf_points: 
			    	parse_point(cin);
				break;
			case sdf_custom: 
			case sdf_unknown: break;
		}
	}
	finalize_tracks();	/* memory points can be at the end of all trackpoints */
}


static void
calculate(const waypoint *wpt, double *dist, double *speed, double *course, 
	  double *asc, double *desc)
{
	if (trkpt_out != NULL) {
		
		time_t time;
		
		*course = heading_true_degrees(
			RAD(trkpt_out->latitude), RAD(trkpt_out->longitude),
			RAD(wpt->latitude), RAD(wpt->longitude));

		*dist = radtometers(gcdist(
			RAD(trkpt_out->latitude), RAD(trkpt_out->longitude), 
			RAD(wpt->latitude), RAD(wpt->longitude)));
		if (*dist < 0.1) *dist = 0;	/* calc. diffs on 32- and 64-bit hosts */
		
		time = wpt->creation_time - trkpt_out->creation_time;
		if (time == 0) 
			*speed = 0;
		else 
			*speed = *dist / (double)time;
			
		if (asc && desc && (trkpt_out->altitude != unknown_alt) && (wpt->altitude != unknown_alt)) {
			double dh = wpt->altitude - trkpt_out->altitude;
			if (dh > 0)
				*asc += dh;
			else
				*desc -= dh;
		}
	}
	else {
		*speed = 0;
		*dist = 0;
		*course = 0;
		if (asc) *asc = 0;
		if (desc) *desc = 0;
	}
	if WAYPT_HAS(wpt, speed) *speed = wpt->speed / 3.6;	/* -> meters per second */
	if WAYPT_HAS(wpt, course) *course = wpt->course;
	
}

/* pre-calculation callbacks */

static void 
any_hdr_calc_cb(const route_head *trk)
{
	
	trkpt_out = NULL;
	this_distance = 0;
	this_time = 0;
	this_points = 0;
	
	this_index++;
	this_valid = ((opt_route_index_value < 1) || (opt_route_index_value == this_index));
	if (! this_valid) return;
	
	if (!rte_name && trk->rte_name) {
		rte_name = trk->rte_name;
		rte_desc = trk->rte_desc;
	}
	
	trk_out = (route_head *)trk;
}

static void 
any_waypt_calc_cb(const waypoint *wpt)
{
	double speed, course, dist;
	
	/* we can only write ONE route */
	if (! this_valid) return;
	
	if ((all_points == 0) && (this_points == 0)) start_time = wpt->creation_time;

	this_points++;
		
	if ((wpt->altitude != unknown_alt) && (wpt->altitude < minalt)) minalt = wpt->altitude;
	if ((wpt->altitude != unknown_alt) && (wpt->altitude > maxalt)) maxalt = wpt->altitude;
	calculate(wpt, &dist, &speed, &course, &all_asc, &all_desc);
	if (speed > maxspeed) maxspeed = speed;

	this_distance = this_distance + dist;
	if (trkpt_out != NULL) 
		this_time += (wpt->creation_time - trkpt_out->creation_time);
			
	trkpt_out = (waypoint *)wpt;
}

static void 
any_tlr_calc_cb(const route_head *trk)
{
	if (! this_valid) return;
	
	all_dist += this_distance;
	all_time += this_time;
	all_points += this_points;
}

/* write callbacks */

static void 
track_disp_hdr_cb(const route_head *trk)
{
	track_index++;
	track_points = 0;
	trk_out = (route_head *)trk;
	trkpt_out = NULL;
}


static void 
track_disp_wpt_cb(const waypoint *wpt)
{
	struct tm tm;
	char tbuf[32];
	double course, speed, dist;
	int flag = 0;

	track_points++;
	all_track_points++;
	
	tm = *localtime(&wpt->creation_time);
	strftime(tbuf, sizeof(tbuf), "%d.%m.%Y,%H:%M.%S", &tm);
	
	calculate(wpt, &dist, &speed, &course, NULL, NULL);
	trkpt_dist = trkpt_dist + dist;
		
	if (track_points == trk_out->rte_waypt_ct) {	/* I'm the last in that list */
		if (all_track_points != saved_track_points) {	/* but not the overall latest */
			flag = 1;
		}
	}
	else if (track_points == 1) {			/* I'm the first in that list */
		if (all_track_points > 1) {			/* but not the first ever seen */
			flag = 2;
		}
	}

	if (flag == 1) {
		char *name = wpt->shortname;
		if (name == NULL) name = "Log paused";
		gbfprintf(fout, "\"MP\",\"%s\"", name);
	}
	else if (flag == 2) {
		char *name = wpt->shortname;
		if (name == NULL) name = "Log continued";
		gbfprintf(fout, "\"MP\",\"%s\"", name);
	}
	else
		gbfprintf(fout, "\"TP\"");

	gbfprintf(fout, ",%s,%.6lf,%.6lf,%.f,%.2f", 
		tbuf,
		wpt->latitude, wpt->longitude, ALT(wpt), speed);
	if (flag) 
		gbfprintf(fout, ",0,0,%d", flag);	/* press, temperature, memory point type */
	else
		gbfprintf(fout, ",%.1f", course);
		
	if (trkpt_dist != 0)
		gbfprintf(fout, ",%.6f\n", trkpt_dist);
	else
		gbfprintf(fout, ",0\n");

	trkpt_out = (waypoint *)wpt;
}

static void 
track_disp_tlr_cb(const route_head *rte)
{
	trkpt_out = NULL;
}

static void
route_disp_hdr_cb(const route_head *rte)
{
	route_index++;
	this_route_valid = ((opt_route_index_value < 1) || (opt_route_index_value == track_index));
}

static void
route_disp_wpt_cb(const waypoint *wpt)
{
	if (this_route_valid) {
		char *sn;
		
		if (global_opts.synthesize_shortnames)
			sn = mkshort_from_wpt(short_h, wpt);
		else
			sn = mkshort(short_h, wpt->shortname);
		gbfprintf(fout, "\"WP\",\"%s\",%.8lf,%.8lf,%.f\n",
			sn, wpt->latitude, wpt->longitude, ALT(wpt));
		xfree(sn);
	}
}

static void
track_disp_custom_cb(const waypoint *wpt)
{
	if (wpt->creation_time && (wpt->altitude != unknown_alt)) {
		gbfprintf(fout, "%d,%.f\n", (int)(wpt->creation_time - start_time), wpt->altitude);
	}
}

static void
wr_init(const char *fname)
{
	fout = gbfopen(fname, "w", MYNAME);
	short_h = mkshort_new_handle();
}

static void
wr_deinit(void)
{
	mkshort_del_handle(&short_h);
	gbfclose(fout);
}

static void
data_write(void)
{
	gbfprintf(fout, "[HEADER]\n");
	gbfprintf(fout, "FILEVERSION=1\n");
	gbfprintf(fout, "SOURCE=FILE\n");
	gbfprintf(fout, "DATUM=WGS84\n");
	
	rte_name = NULL;
	rte_desc = NULL;
	trkpt_out = NULL;
	opt_route_index_value = -1;	/* take all tracks from data pool */
	track_index = 0;
	minalt = -unknown_alt;
	maxalt = unknown_alt;
	maxspeed = 0;
	all_dist = 0;
	all_time = 0;
	all_asc = 0;
	all_desc = 0;
	all_points = 0;
	start_time = 0;
	
	setshort_length(short_h, 100);
	setshort_badchars(short_h, "\r\n");
	setshort_mustupper(short_h, 0);
	setshort_mustuniq(short_h, 0);
	setshort_whitespace_ok(short_h, 1);
	setshort_repeating_whitespace_ok(short_h, 1);
	
	switch(global_opts.objective)
	{
		case wptdata: 
			break;

		case rtedata:
			gbfprintf(fout, "TYPE=5\n");

			opt_route_index_value = atoi(opt_route_index);
			route_disp_all(any_hdr_calc_cb, any_tlr_calc_cb, any_waypt_calc_cb);
			gbfprintf(fout, "DISTANCE=%.f\n", all_dist);
			if (rte_name) gbfprintf(fout, "NAME=%s\n", rte_name);
			gbfprintf(fout, "[POINTS]\n");
			if (route_points > 0) {
				track_index = 0;
				route_disp_all(route_disp_hdr_cb, NULL, route_disp_wpt_cb);
			}
			break;

		case trkdata:
			gbfprintf(fout, "TYPE=28\n");
			
			track_disp_all(any_hdr_calc_cb, any_tlr_calc_cb, any_waypt_calc_cb);
			if (all_track_points > 0) {
				if (rte_name) gbfprintf(fout, "NAME=%s\n", rte_name);
				if (minalt != -unknown_alt) gbfprintf(fout, "MINALT=%.f\n", minalt);
				if (maxalt != unknown_alt) gbfprintf(fout, "MAXALT=%.f\n", maxalt);
				gbfprintf(fout, "MAXSPEED=%.2f\n", maxspeed);
				gbfprintf(fout, "DISTANCE=%.f\n", all_dist);
				gbfprintf(fout, "DURATION=%lu\n", all_time);
//				gbfprintf(fout, "TOTASC=%.f\n", all_asc);
//				gbfprintf(fout, "TOTDSC=%.f\n", all_desc);
				if (start_time) {
					struct tm tm;
					char tbuf[32];
					
					tm = *localtime(&start_time);
					strftime(tbuf, sizeof(tbuf), "%d.%m.%Y %H:%M.%S", &tm);
					gbfprintf(fout, "DATE=%s\n", tbuf);
				}
				if (all_time) gbfprintf(fout, "AVGSPEED=%.2f\n", all_dist / (double)all_time);
			}
			gbfprintf(fout, "[POINTS]\n");
			if (all_track_points > 0) {
			
				trkpt_dist = 0;
				saved_track_points = all_track_points;
				all_track_points = 0;
				track_disp_all(track_disp_hdr_cb, track_disp_tlr_cb, track_disp_wpt_cb);
				
				if (start_time) {
					gbfprintf(fout, "[CUSTOM1]\n");
					track_index = 0;
					track_disp_all(NULL, NULL, track_disp_custom_cb);
				}
			}
			break;
		case posndata:
			fatal(MYNAME ": Realtime positioning not supported.\n");
			break;
	}
}

/* ------------------------------------------------------------------ */

ff_vecs_t stmsdf_vecs = {
	ff_type_file,
	{ ff_cap_none, 
	  ff_cap_read | ff_cap_write, 
	  ff_cap_read | ff_cap_write },
	rd_init,
	wr_init,
	rd_deinit,
	wr_deinit,
	data_read,
	data_write,
	NULL,
	stmsdf_args,
	CET_CHARSET_MS_ANSI, 0	/* CET-REVIEW */
};

/* ================================================================== */

#endif /* CSVFMTS_ENABLED */

