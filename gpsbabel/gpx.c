/*
    Access GPX data files.

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
#include "xmlgeneric.h"
#ifndef NO_EXPAT
	#include <expat.h>
	static XML_Parser psr;
#endif

static xml_tag *cur_tag;
static vmem_t cdatastr;
static char *opt_logpoint = NULL;
static int logpoint_ct = 0;

static const char *gpx_version;
static char *gpx_wversion;
static int gpx_wversion_num;
static const char *gpx_creator;
static char *xsi_schema_loc;

static char *gpx_email = NULL;
static char *gpx_author = NULL;
vmem_t current_tag;

static waypoint *wpt_tmp;
static int cache_descr_is_html;
static FILE *fd;
static FILE *ofd;
static void *mkshort_handle;

static const char *input_string = NULL;
static int input_string_len = 0;

static time_t file_time;

static char *gsshortnames = NULL;
static char *snlen = NULL;
static char *suppresswhite = NULL;
static char *urlbase = NULL;
static route_head *trk_head;
static route_head *rte_head;

#define MYNAME "GPX"
#define MY_CBUF 4096
#define DEFAULT_XSI_SCHEMA_LOC "http://www.topografix.com/GPX/1/0 http://www.topografix.com/GPX/1/0/gpx.xsd"
#define DEFAULT_XSI_SCHEMA_LOC_FMT "\"http://www.topografix.com/GPX/%c/%c http://www.topografix.com/GPX/%c/%c/gpx.xsd\""

/* 
 * Format used for floating point formats.  Put in one place to make it
 * easier to tweak when comparing output with other GPX programs that 
 * have more or less digits of output...
 */
/* #define FLT_FMT "%.9lf" */  /* ExpertGPS */
#define FLT_FMT "%0.9lf" 
#define FLT_FMT_T "%lf" 
#define FLT_FMT_R "%lf" 

typedef enum {
	tt_unknown = 0,
	tt_gpx,
	tt_author,
	tt_desc,
	tt_email,
	tt_time,
	tt_wpt,
	tt_wpt_cmt,
	tt_wpt_desc,
	tt_wpt_name,
	tt_wpt_sym,
	tt_wpt_url,
	tt_wpt_ele,
	tt_wpt_time,
	tt_wpt_type,
	tt_wpt_urlname,
	tt_wpt_link, 		/* New in GPX 1.1 */
	tt_wpt_link_text, 	/* New in GPX 1.1 */
	tt_cache,
	tt_cache_name,
	tt_cache_container,
	tt_cache_type,
	tt_cache_difficulty,
	tt_cache_terrain,
	tt_cache_hint,
	tt_cache_desc_short,
	tt_cache_desc_long,
	tt_cache_log_wpt,
	tt_rte,
	tt_rte_name,
	tt_rte_desc,
	tt_rte_cmt,
	tt_rte_number,
	tt_rte_rtept,
	tt_rte_rtept_ele,
	tt_rte_rtept_name,
	tt_rte_rtept_desc,
	tt_rte_rtept_sym,
	tt_rte_rtept_time,
	tt_rte_rtept_cmt,
	tt_rte_rtept_url,
	tt_rte_rtept_urlname,
	tt_trk,
	tt_trk_desc,
	tt_trk_name,
	tt_trk_trkseg,
	tt_trk_number,
	tt_trk_trkseg_trkpt,
	tt_trk_trkseg_trkpt_cmt,
	tt_trk_trkseg_trkpt_name,
	tt_trk_trkseg_trkpt_sym,
	tt_trk_trkseg_trkpt_url,
	tt_trk_trkseg_trkpt_urlname,
	tt_trk_trkseg_trkpt_desc,
	tt_trk_trkseg_trkpt_ele,
	tt_trk_trkseg_trkpt_time,
} tag_type;

typedef struct tag_mapping {
	tag_type tag_type;		/* enum from above for this tag */
	int tag_passthrough;		/* true if we don't generate this */
	const char *tag_name;		/* xpath-ish tag name */
} tag_mapping;

/*
 * xpath(ish) mappings between full tag paths and internal identifers.
 * These appear in the order they appear in the GPX specification.
 * If it's not a tag we explictly handle, it doesn't go here.
 */

tag_mapping tag_path_map[] = {
	{ tt_gpx, 0, "/gpx" },
	{ tt_time, 0, "/gpx/time" },
	{ tt_author, 0, "/gpx/author" },
	{ tt_email, 0, "/gpx/email" },
	{ tt_time, 0, "/gpx/time" },
	{ tt_desc, 0, "/gpx/desc" },

	{ tt_wpt, 0, "/gpx/wpt" },
	{ tt_wpt_ele, 0, "/gpx/wpt/ele" },
	{ tt_wpt_time, 0, "/gpx/wpt/time" },
	{ tt_wpt_name, 0, "/gpx/wpt/name" },
	{ tt_wpt_cmt, 0, "/gpx/wpt/cmt" },
	{ tt_wpt_desc, 0, "/gpx/wpt/desc" },
	{ tt_wpt_url, 0, "/gpx/wpt/url" },
	{ tt_wpt_urlname, 0, "/gpx/wpt/urlname" },
	{ tt_wpt_link, 0, "/gpx/wpt/link" },			/* GPX 1.1 */
	{ tt_wpt_link_text, 0, "/gpx/wpt/link/text" },		/* GPX 1.1 */
	{ tt_wpt_sym, 0, "/gpx/wpt/sym" },
	{ tt_wpt_type, 1, "/gpx/wpt/type" },
	{ tt_cache, 1, "/gpx/wpt/groundspeak:cache" },
	{ tt_cache_name, 1, "/gpx/wpt/groundspeak:cache/groundspeak:name" },
	{ tt_cache_container, 1, "/gpx/wpt/groundspeak:cache/groundspeak:container" },
	{ tt_cache_type, 1, "/gpx/wpt/groundspeak:cache/groundspeak:type" },
	{ tt_cache_difficulty, 1, "/gpx/wpt/groundspeak:cache/groundspeak:difficulty" },
	{ tt_cache_terrain, 1, "/gpx/wpt/groundspeak:cache/groundspeak:terrain" },
	{ tt_cache_hint, 1, "/gpx/wpt/groundspeak:cache/groundspeak:encoded_hints" },
	{ tt_cache_desc_short, 1, "/gpx/wpt/groundspeak:cache/groundspeak:short_description" },
	{ tt_cache_desc_long, 1, "/gpx/wpt/groundspeak:cache/groundspeak:long_description" },
	{ tt_cache_log_wpt, 1, "/gpx/wpt/groundspeak:cache/groundspeak:logs/groundspeak:log/groundspeak:log_wpt" },

	{ tt_rte, 0, "/gpx/rte" },
	{ tt_rte_name, 0, "/gpx/rte/name" },
	{ tt_rte_desc, 0, "/gpx/rte/desc" },
	{ tt_rte_number, 0, "/gpx/rte/number" },
	{ tt_rte_rtept, 0, "/gpx/rte/rtept" },
	{ tt_rte_rtept_ele, 0, "/gpx/rte/rtept/ele" },
	{ tt_rte_rtept_time, 0, "/gpx/rte/rtept/time" },
	{ tt_rte_rtept_name, 0, "/gpx/rte/rtept/name" },
	{ tt_rte_rtept_cmt, 0, "/gpx/rte/rtept/cmt" },
	{ tt_rte_rtept_desc, 0, "/gpx/rte/rtept/desc" },
	{ tt_rte_rtept_url, 0, "/gpx/rte/rtept/url" },
	{ tt_rte_rtept_urlname, 0, "/gpx/rte/rtept/urlname" },
	{ tt_rte_rtept_sym, 0, "/gpx/rte/rtept/sym" },

	{ tt_trk, 0, "/gpx/trk" },
	{ tt_trk_name, 0, "/gpx/trk/name" },
	{ tt_trk_desc, 0, "/gpx/trk/desc" },
	{ tt_trk_trkseg, 0, "/gpx/trk/trkseg" },
	{ tt_trk_number, 0, "/gpx/trk/number" },
	{ tt_trk_trkseg_trkpt, 0, "/gpx/trk/trkseg/trkpt" },
	{ tt_trk_trkseg_trkpt_ele, 0, "/gpx/trk/trkseg/trkpt/ele" },
	{ tt_trk_trkseg_trkpt_time, 0, "/gpx/trk/trkseg/trkpt/time" },
	{ tt_trk_trkseg_trkpt_name, 0, "/gpx/trk/trkseg/trkpt/name" },
	{ tt_trk_trkseg_trkpt_cmt, 0, "/gpx/trk/trkseg/trkpt/cmt" },
	{ tt_trk_trkseg_trkpt_desc, 0, "/gpx/trk/trkseg/trkpt/desc" },
	{ tt_trk_trkseg_trkpt_url, 0, "/gpx/trk/trkseg/trkpt/url" },
	{ tt_trk_trkseg_trkpt_urlname, 0, "/gpx/trk/trkseg/trkpt/urlname" },
	{ tt_trk_trkseg_trkpt_sym, 0, "/gpx/trk/trkseg/trkpt/sym" },
	{0}
};

static tag_type
get_tag(const char *t, int *passthrough)
{
	tag_mapping *tm;
	for (tm = tag_path_map; tm->tag_type != 0; tm++) {
		if (0 == strcmp(tm->tag_name, t)) {
			*passthrough = tm->tag_passthrough;
			return tm->tag_type;
		}
	}
	*passthrough = 1;
	return tt_unknown;
}

static void
tag_gpx(const char **attrv)
{
	const char **avp = &attrv[0];
	while (*avp) {
		if (strcmp(avp[0], "version") == 0) {
			gpx_version = avp[1];
		}
		else if (strcmp(avp[0], "src") == 0) {
			gpx_creator = avp[1];
		}
		else if (strcmp(avp[0], "xsi:schemaLocation") == 0) {
			if (0 == strstr(xsi_schema_loc, avp[1])) {
			    xsi_schema_loc = xstrappend(xsi_schema_loc, " ");
			    xsi_schema_loc = xstrappend(xsi_schema_loc, avp[1]);
			}
		}
		avp+=2;
	}
}

static void
tag_wpt(const char **attrv)
{
	const char **avp = &attrv[0];

	wpt_tmp = waypt_new();

	cur_tag = NULL;
	while (*avp) { 
		if (strcmp(avp[0], "lat") == 0) {
			sscanf(avp[1], "%lf", 
				&wpt_tmp->latitude);
		}
		else if (strcmp(avp[0], "lon") == 0) {
			sscanf(avp[1], "%lf", 
				&wpt_tmp->longitude);
		}
		avp+=2;
	}
}

static void
tag_cache_desc(const char ** attrv)
{
	const char **avp;

	cache_descr_is_html = 0;
	for (avp = &attrv[0]; *avp; avp+=2) {
		if (strcmp(avp[0], "html") == 0) {
			if (strcmp(avp[1], "True") == 0) {
				cache_descr_is_html = 1;
			}
		}
	}
}

static void
tag_gs_cache(const char **attrv)
{
	const char **avp;

	cache_descr_is_html = 0;
	for (avp = &attrv[0]; *avp; avp+=2) {
		if (strcmp(avp[0], "id") == 0) {
				wpt_tmp->gc_data.id = atoi(avp[1]);
		}
	}
}

static void
start_something_else(const char *el, const char **attrv)
{
	const char **avp = attrv;
	char **avcp = NULL;
	int attr_count = 0;
	xml_tag *new_tag;
       
	if ( !wpt_tmp ) {
		return;
	}
	
        new_tag = (xml_tag *)xcalloc(sizeof(xml_tag),1);
        new_tag->tagname = xstrdup(el);
	
	/* count attributes */
	while (*avp) {
		attr_count++;
		avp++;
	}
	
	/* copy attributes */
	avp = attrv;
	new_tag->attributes = (char **)xcalloc(sizeof(char *),attr_count+1);
	avcp = new_tag->attributes;
	while (*avp) {
		*avcp = xstrdup(*avp);
		avcp++;
		avp++;
	}
	*avcp = NULL;
	
	if ( cur_tag ) {
		if ( cur_tag->child ) {
			cur_tag = cur_tag->child;
			while ( cur_tag->sibling ) {
				cur_tag = cur_tag->sibling;
			}
			cur_tag->sibling = new_tag;
			new_tag->parent = cur_tag->parent;
		}
		else {
			cur_tag->child = new_tag;
			new_tag->parent = cur_tag;
		}
	}
	else {
		if ( wpt_tmp->gpx_extras ) {
			cur_tag = wpt_tmp->gpx_extras;
			while ( cur_tag->sibling ) {
				cur_tag = cur_tag->sibling;
			}
			cur_tag->sibling = new_tag;
			new_tag->parent = NULL;
		}
		else {
			wpt_tmp->gpx_extras = new_tag;
			new_tag->parent = NULL;
		}
	}
	cur_tag = new_tag;
}

static void
end_something_else()
{
	if ( cur_tag ) {
		cur_tag = cur_tag->parent;
	}
}

static void
tag_log_wpt(const char **attrv)
{
	waypoint * lwp_tmp;
	const char **avp = &attrv[0];

	/* create a new waypoint */
	lwp_tmp = waypt_new();

	/* extract the lat/lon attributes */
	while (*avp) { 
		if (strcmp(avp[0], "lat") == 0) {
			sscanf(avp[1], "%lf", 
				&lwp_tmp->latitude);
		}
		else if (strcmp(avp[0], "lon") == 0) {
			sscanf(avp[1], "%lf", 
			&lwp_tmp->longitude);
		}
		avp+=2;
	}
	/* Make a new shortname.  Since this is a groundspeak extension, 
	  we assume that GCBLAH is the current shortname format and that
	  wpt_tmp refers to the currently parsed waypoint. Unfortunatley,
	  we need to keep track of log_wpt counts so we don't collide with
	  dupe shortnames.
	*/

	if ((wpt_tmp->shortname) && (strlen(wpt_tmp->shortname) > 2)) {
		/* copy of the shortname */
		lwp_tmp->shortname = xcalloc(7, 1);
		sprintf(lwp_tmp->shortname, "%-4.4s%02d", 
			&wpt_tmp->shortname[2], logpoint_ct++);

		waypt_add(lwp_tmp);
	} 
}

static void
gpx_start(void *data, const char *el, const char **attr)
{
	char *e;
	char *ep;
	int passthrough;

	vmem_realloc(&current_tag, strlen(current_tag.mem) + 2 + strlen(el));
	e = current_tag.mem;
	ep = e + strlen(e);
	*ep++ = '/';
	strcpy(ep, el);

	
	/*
	 * FIXME: Find out why a cdatastr[0] doesn't adequately reset the 	
	 * cdata handler.
	 */
	memset(cdatastr.mem, 0, cdatastr.size);

	switch (get_tag(current_tag.mem, &passthrough)) {
	case tt_gpx:
		tag_gpx(attr);
		break;
	case tt_wpt:
		tag_wpt(attr);
		break;
	case tt_wpt_link:
		if (0 == strcmp(attr[0], "href")) {
			wpt_tmp->url = xstrdup(attr[1]);
		}
		break;
	case tt_rte:
		rte_head = route_head_alloc();
		route_add_head(rte_head);
		break;
	case tt_rte_rtept:
		tag_wpt(attr);
		break; 
	case tt_trk:
		trk_head = route_head_alloc();
		track_add_head(trk_head);
		break;
	case tt_trk_trkseg_trkpt:
		tag_wpt(attr);
		break;
	case tt_unknown:
		start_something_else(el, attr);
		return;
	case tt_cache:
		tag_gs_cache(attr);
		break;
	case tt_cache_log_wpt:
		if (opt_logpoint)
			tag_log_wpt(attr);
		break;
	case tt_cache_desc_long:
	case tt_cache_desc_short:
		tag_cache_desc(attr);
		break;
	default:
		break;
	}
	if (passthrough) {
		start_something_else(el, attr);
	}
}

struct
gs_type_mapping{
	geocache_type type;
	const char *name;
} gs_type_map[] = {
	{ gt_traditional, "Traditional cache" },
	{ gt_multi, "Multi-Cache" },
	{ gt_virtual, "Virtual cache" },
	{ gt_event, "Event cache" },
	{ gt_webcam, "Webcam Cache" },
	{ gt_suprise, "Unknown cache" },
};

struct
gs_container_mapping{
	geocache_container type;
	const char *name;
} gs_container_map[] = {
	{ gc_other, "Unknown" },
	{ gc_micro, "Micro" },
	{ gc_regular, "Regular" },
	{ gc_large, "Large" },
	{ gc_small, "Small" },
	{ gc_virtual, "Virtual" }
};

geocache_type
gs_mktype(const char *t)
{
	int i;
	int sz = sizeof(gs_type_map) / sizeof(gs_type_map[0]);

	for (i = 0; i < sz; i++) {
		if (0 == case_ignore_strcmp(t, gs_type_map[i].name)) {
			return gs_type_map[i].type;
		}
	}
	return gt_unknown;
}

const char *
gs_get_cachetype(geocache_type t)
{
	int i;
	int sz = sizeof(gs_type_map) / sizeof(gs_type_map[0]);

	for (i = 0; i < sz; i++) {
		if (t == gs_type_map[i].type) {
			return gs_type_map[i].name;
		}
	}
	return "Unknown";
}

geocache_container
gs_mkcont(const char *t)
{
	int i;
	int sz = sizeof(gs_container_map) / sizeof(gs_container_map[0]);

	for (i = 0; i < sz; i++) {
		if (0 == case_ignore_strcmp(t, gs_container_map[i].name)) {
			return gs_container_map[i].type;
		}
	}
	return gc_unknown;
}

const char *
gs_get_container(geocache_container t)
{
	int i;
	int sz = sizeof(gs_container_map) / sizeof(gs_container_map[0]);

	for (i = 0; i < sz; i++) {
		if (t == gs_container_map[i].type) {
			return gs_container_map[i].name;
		}
	}
	return "Unknown";
}

time_t 
xml_parse_time( const char *cdatastr ) 
{
	int off_hr = 0;
	int off_min = 0;
	int off_sign = 1;
	char *offsetstr = NULL;
	char *pointstr = NULL;
	struct tm tm;
	time_t rv = 0;
	char *timestr = xstrdup( cdatastr );
	
	offsetstr = strchr( timestr, 'Z' );
	if ( offsetstr ) {
		/* zulu time; offsets stay at defaults */
		*offsetstr = '\0';
	} else {
		offsetstr = strchr( timestr, '+' );
		if ( offsetstr ) {
			/* positive offset; parse it */
			*offsetstr = '\0';
			sscanf( offsetstr+1, "%d:%d", &off_hr, &off_min );
		} else {
			offsetstr = strchr( timestr, 'T' );
			if ( offsetstr ) {
				offsetstr = strchr( offsetstr, '-' );
				if ( offsetstr ) {
					/* negative offset; parse it */
					*offsetstr = '\0';
					sscanf( offsetstr+1, "%d:%d", 
							&off_hr, &off_min );
					off_sign = -1;
				}
			}
		}
	}
	
	pointstr = strchr( timestr, '.' );
	if ( pointstr ) {
		*pointstr = '\0';
	}
	
	sscanf(timestr, "%d-%d-%dT%d:%d:%d", 
		&tm.tm_year,
		&tm.tm_mon,
		&tm.tm_mday,
		&tm.tm_hour,
		&tm.tm_min,
		&tm.tm_sec);
	tm.tm_mon -= 1;
	tm.tm_year -= 1900;
	tm.tm_isdst = 0;
	
	rv = mktime(&tm) + get_tz_offset() - off_sign*off_hr*3600 - 
		off_sign*off_min*60;
	
        xfree(timestr);
	
	return rv;
}

static void
gpx_end(void *data, const char *el)
{
	char *s = strrchr(current_tag.mem, '/');
	float x;
	char *cdatastrp = cdatastr.mem;
	int passthrough;

	if (strcmp(s + 1, el)) {
		fprintf(stderr, "Mismatched tag %s\n", el);
	}

	switch (get_tag(current_tag.mem, &passthrough)) {
	/*
	 * First, the tags that are file-global.
	 */
	case tt_time:
		file_time = xml_parse_time(cdatastrp);
		break;
	case tt_email:
		if (gpx_email) xfree(gpx_email);
		gpx_email = xstrdup(cdatastrp);
		break;
	case tt_author:
		if (gpx_author) xfree(gpx_author);
		gpx_author = xstrdup(cdatastrp);
		break;
	case tt_gpx:
		/* Could invoke release code here */
		break;
	/*
	 * Waypoint-specific tags.
	 */
	case tt_wpt_url:
		wpt_tmp->url = xstrdup(cdatastrp);
		break;
	case tt_wpt_urlname:
	case tt_wpt_link_text:
		wpt_tmp->url_link_text = xstrdup(cdatastrp);
		break;
	case tt_wpt:
		waypt_add(wpt_tmp);
		logpoint_ct = 0;
		cur_tag = NULL;
		wpt_tmp = NULL;
		break;
	case tt_cache_name:
		if (gsshortnames) {
			if (wpt_tmp->notes)
				xfree(wpt_tmp->notes);
			wpt_tmp->notes = xstrdup(cdatastrp);
		}
		break;
	case tt_cache_container:
		wpt_tmp->gc_data.container = gs_mkcont(cdatastrp);
		break;
	case tt_cache_type:
		wpt_tmp->gc_data.type = gs_mktype(cdatastrp);
		break;
	case tt_cache_difficulty:
		sscanf(cdatastrp, "%f", &x);
		wpt_tmp->gc_data.diff = x * 10;
		break;
	case tt_cache_hint:
		rtrim(cdatastrp);
		if (cdatastrp[0]) {
			wpt_tmp->gc_data.hint = xstrdup(cdatastrp);
		}
		break;
	case tt_cache_desc_long:
		rtrim(cdatastrp);
		if (cdatastrp[0]) {
			wpt_tmp->gc_data.desc_long.is_html = cache_descr_is_html;
			wpt_tmp->gc_data.desc_long.utfstring = xstrdup(cdatastrp);
		}
		break;
	case tt_cache_desc_short:
		rtrim(cdatastrp);
		if (cdatastrp[0]) {
			wpt_tmp->gc_data.desc_short.is_html = cache_descr_is_html;
			wpt_tmp->gc_data.desc_short.utfstring = xstrdup(cdatastrp);
		}
		break;
	case tt_cache_terrain:
		sscanf(cdatastrp, "%f", &x);
		wpt_tmp->gc_data.terr = x * 10;
		break;
	/*
	 * Route-specific tags.
 	 */
	case tt_rte_name:
		rte_head->rte_name = xstrdup(cdatastrp);
		break;
	case tt_rte:
		break;
	case tt_rte_rtept:
		route_add_wpt(rte_head, wpt_tmp);
		break;
	case tt_rte_desc:
		rte_head->rte_desc = xstrdup(cdatastrp);
		break;
	case tt_rte_number:
		rte_head->rte_num = atoi(cdatastrp);
		break;
	/*
	 * Track-specific tags.
	 */
	case tt_trk_name:
		trk_head->rte_name = xstrdup(cdatastrp);
		break;
	case tt_trk:
		break;
	case tt_trk_trkseg_trkpt:
		route_add_wpt(trk_head, wpt_tmp);
		break;
	case tt_trk_desc:
		trk_head->rte_desc = xstrdup(cdatastrp);
		break;
	case tt_trk_number:
		trk_head->rte_num = atoi(cdatastrp);
		break;

	/*
	 * Items that are actually in multiple categories.
	 */
	case tt_wpt_ele:
	case tt_rte_rtept_ele:
	case tt_trk_trkseg_trkpt_ele:
		sscanf(cdatastrp, "%lf", &wpt_tmp->altitude);
		break;
	case tt_wpt_name:
	case tt_rte_rtept_name:
	case tt_trk_trkseg_trkpt_name:
		wpt_tmp->shortname = xstrdup(cdatastrp);
		break;
	case tt_wpt_sym:
	case tt_rte_rtept_sym:
	case tt_trk_trkseg_trkpt_sym:
		wpt_tmp->icon_descr = xstrdup(cdatastrp);
		wpt_tmp->wpt_flags.icon_descr_is_dynamic = 1;
		break;
	case tt_wpt_time:
	case tt_trk_trkseg_trkpt_time:
	case tt_rte_rtept_time:
		wpt_tmp->creation_time = xml_parse_time( cdatastrp );
		break;
	case tt_wpt_cmt:
	case tt_rte_rtept_cmt:
	case tt_trk_trkseg_trkpt_cmt:
		wpt_tmp->description = xstrdup(cdatastrp);
		break;
	case tt_wpt_desc:
	case tt_trk_trkseg_trkpt_desc:
	case tt_rte_rtept_desc:
		wpt_tmp->notes = xstrdup(cdatastrp);
		break;
	case tt_unknown:
		end_something_else();
		*s = 0;
		return;
	default:
		break;
	}

	if (passthrough)
		end_something_else();

	*s = 0;
}

#if NO_EXPAT
void
gpx_rd_init(const char *fname)
{
	fatal(MYNAME ": This build excluded GPX support because expat was not installed.\n");
}

#else /* NO_EXPAT */

static void
gpx_cdata(void *dta, const XML_Char *s, int len)
{
	char *estr;
	int *cdatalen;
	char **cdata;
	xml_tag *tmp_tag;

	vmem_realloc(&cdatastr,  1 + len + strlen(cdatastr.mem));
	estr = (char *) cdatastr.mem + strlen(cdatastr.mem);
	memcpy(estr, s, len);
	estr[len]  = 0;

	if (!cur_tag) 
		return;

		if ( cur_tag->child ) {
			tmp_tag = cur_tag->child;
			while ( tmp_tag->sibling ) {
				tmp_tag = tmp_tag->sibling;
			}
			cdata = &(tmp_tag->parentcdata);
			cdatalen = &(tmp_tag->parentcdatalen);
		}
		else {
			cdata = &(cur_tag->cdata);
			cdatalen = &(cur_tag->cdatalen);
		}
		estr = *cdata;
		*cdata = xcalloc( *cdatalen + len + 1, 1);
		if ( estr ) {
			memcpy( *cdata, estr, *cdatalen);
			xfree( estr );
		}
		estr = *cdata + *cdatalen;
		memcpy( estr, s, len );
		*(estr+len) = '\0';
		*cdatalen += len;
}

void
gpx_rd_init(const char *fname)
{
	if ( fname[0] ) {
		fd = xfopen(fname, "r", MYNAME);
	}
	else {
		fd = NULL;
		input_string = fname+1;
		input_string_len = strlen(input_string);
	}


	file_time = 0;
	current_tag = vmem_alloc(1, 0);
	*((char *)current_tag.mem) = '\0';
	
	psr = XML_ParserCreate(NULL);
	if (!psr) {
		fatal(MYNAME ": Cannot create XML Parser\n");
	}
	cdatastr = vmem_alloc(1, 0);
	*((char *)cdatastr.mem) = '\0';

	/* We don't use xstrdup here because we' know we don't free
	 * this across reads and we unlock the safety belt from the 
	 * leak tester.
	 */
	if (!xsi_schema_loc) {
		xsi_schema_loc = strdup(DEFAULT_XSI_SCHEMA_LOC);
	}
	if (!xsi_schema_loc) {
		fatal("gpx: Unable to allocate %d bytes of memory.\n", strlen(DEFAULT_XSI_SCHEMA_LOC) + 1);
	}

	XML_SetElementHandler(psr, gpx_start, gpx_end);
	XML_SetCharacterDataHandler(psr, gpx_cdata);
}
#endif

static 
void 
gpx_rd_deinit(void) 
{
	vmem_free(&current_tag);
	vmem_free(&cdatastr);
	/* 
	 * Don't free schema_loc.  It really is important that we preserve
	 * this across reads or else merges/copies of files with different 
	 * schemas won't retain the headers.
	 *
	if ( xsi_schema_loc ) {
		xfree(xsi_schema_loc);
		xsi_schema_loc = NULL;
	}
	*/ 
	if ( gpx_email ) {
		xfree(gpx_email);
		gpx_email = NULL;
	}
	if ( gpx_author ) {
		xfree(gpx_author);
		gpx_author = NULL;
	}
	if (fd) {
	        fclose(fd);
	}
	XML_ParserFree(psr);
	psr = NULL;
}

void
gpx_wr_init(const char *fname)
{
	mkshort_handle = mkshort_new_handle();

	ofd = xfopen(fname, "w", MYNAME);
}

static void
gpx_wr_deinit(void)
{
	fclose(ofd);
	mkshort_del_handle(mkshort_handle);
}

void
gpx_read(void)
{
#ifndef NO_EXPAT
	int len;
	int done = 0;
	char buf[MY_CBUF];
	int result = 0;

	while (!done) {
		if ( fd ) {
			/* 
			 * The majority of this block (in fact, all but the 
			 * call to XML_Parse) are a disgusting hack to 
			 * correct defective GPX files that Geocaching.com
			 * issues as pocket queries.   They contain escape
			 * characters as entities (&#x00-&#x1f) which makes
			 * them not validate which croaks expat and torments
			 * users.
			 *
			 * Look for '&' in the last 6 chars.   If we find
			 * it, strip it, then read byte-at-a-time until
			 * we find a non-entity.
			 */
			char *badchar;
			char *semi;
			len = fread(buf, 1, sizeof(buf)-7, fd);
			done = feof(fd) || !len;
			buf[len] = '\0';
			badchar = buf+len-7;
			badchar = strchr( badchar, '&' );
			while ( badchar ) {
				int extra = 7;
				semi = strchr( badchar, ';');
				while ( extra && !semi ) {
					len += fread( buf+len, 1, 1, fd);
					buf[len]='\0';
					extra--;
					if ( buf[len-1] == ';') 
						semi= buf+len-1;
				}
				badchar = strchr( badchar+1, '&' );
			} 
			{
				char *hex="0123456789abcdef";
				badchar = strstr( buf, "&#x" );
				while ( badchar ) {
					int val = 0;
					char *hexit = badchar+3;
					semi = strchr( badchar, ';' );
					if ( semi ) {
						while (*hexit && *hexit != ';') {
							val *= 16;
							val += strchr( hex, *hexit )-hex;
							hexit++;
						}
						
						if ( val < 32 ) {
							warning( MYNAME ": Ignoring illegal character %s;\n\tConsider emailing %s at <%s>\n\tabout illegal characters in their GPX files.\n", badchar, gpx_author?gpx_author:"(unknown author)", gpx_email?gpx_email:"(unknown email address)" );
							memmove( badchar, semi+1, strlen(semi+1)+1 );
							len -= (semi-badchar)+1;
							badchar--;
						}
					}
					badchar = strstr( badchar+1, "&#x" );
				} 
			}
			result = XML_Parse(psr, buf, len, done);
		}
		else if (input_string) {
			done = 0;
			result = XML_Parse(psr, input_string, 
					input_string_len, done );
			done = 1;
		}
		else {
			done = 1;
			result = -1;
		}
		if (!result) {
			fatal(MYNAME ": XML parse error at %d: %s\n", 
				XML_GetCurrentLineNumber(psr),
				XML_ErrorString(XML_GetErrorCode(psr)));
		}
	}
#endif /* NO_EXPAT */
}

static void
fprint_tag_and_attrs( char *prefix, char *suffix, xml_tag *tag )
{
	char **pa;
	fprintf( ofd, "%s%s", prefix, tag->tagname );
	pa = tag->attributes;
	if ( pa ) {
		while ( *pa ) {
			fprintf( ofd, " %s=\"%s\"", pa[0], pa[1] );
			pa += 2;
		}
	}
	fprintf( ofd, "%s", suffix );
}

static void
fprint_xml_chain( xml_tag *tag, const waypoint *wpt ) 
{
	char *tmp_ent;
	while ( tag ) {
		if ( !tag->cdata && !tag->child ) {
			fprint_tag_and_attrs( "<", " />", tag );
		}
		else {
			fprint_tag_and_attrs( "<", ">", tag );
		
			if ( tag->cdata ) {
				tmp_ent = xml_entitize( tag->cdata );
				fprintf( ofd, "%s", tmp_ent );
				xfree(tmp_ent);
			}
			if ( tag->child ) {
				fprint_xml_chain(tag->child, wpt);
			}
			if ( strcmp(tag->tagname, "groundspeak:cache" ) == 0 
					&& wpt->gc_data.exported) {
				xml_write_time( ofd, wpt->gc_data.exported, 
						"groundspeak:exported" );
			}
			fprintf( ofd, "</%s>", tag->tagname);
		}
		if ( tag->parentcdata ) {
			tmp_ent = xml_entitize(tag->parentcdata);
			fprintf(ofd, "%s", tmp_ent );
			xfree(tmp_ent);
		}
		tag = tag->sibling;	
	}
}	

void free_gpx_extras( xml_tag *tag )
{
	xml_tag *next = NULL;
	char **ap;
	
	while ( tag ) {
		if (tag->cdata) {
			xfree(tag->cdata);
		}
		if (tag->child) {
			free_gpx_extras(tag->child);
		}
		if (tag->parentcdata) {
			xfree(tag->parentcdata);
		}
		if (tag->tagname) {
        		xfree(tag->tagname);
		}
		if (tag->attributes) {
			ap = tag->attributes;

			while (*ap)
				xfree(*ap++);

			xfree(tag->attributes);
		}
		
		next = tag->sibling;
		xfree(tag);
		tag = next;
	}
}

/*
 * Handle the grossness of GPX 1.0 vs. 1.1 handling of linky links.
 */
static void
write_gpx_url(const waypoint *waypointp)
{
	char *tmp_ent;

	if (waypointp->url) {
		tmp_ent = xml_entitize(waypointp->url);
		if (gpx_wversion_num > 10) {
			
			fprintf(ofd, "  <link href=\"%s%s\">\n", 
				urlbase ? urlbase : "", tmp_ent);
			write_optional_xml_entity(ofd, "  ", "text", 
				waypointp->url_link_text);
			fprintf(ofd, "  </link>\n");
		} else {
			fprintf(ofd, "  <url>%s%s</url>\n", 
				urlbase ? urlbase : "", tmp_ent);
			write_optional_xml_entity(ofd, "  ", "urlname", 
				waypointp->url_link_text);
		}
		xfree(tmp_ent);
	}
}

static void
gpx_waypt_pr(const waypoint *waypointp)
{
	const char *oname;
	char *odesc;

	/*
	 * Desparation time, try very hard to get a good shortname
	 */
	odesc = waypointp->notes;
	if (!odesc) {
		odesc = waypointp->description;
	}
	if (!odesc) {
		odesc = waypointp->shortname;
	}

	oname = global_opts.synthesize_shortnames ?
				  mkshort(mkshort_handle, odesc) : 
				  waypointp->shortname;

	fprintf(ofd, "<wpt lat=\"" FLT_FMT "\" lon=\"" FLT_FMT "\">\n",
		waypointp->latitude,
		waypointp->longitude);
	if (waypointp->altitude != unknown_alt) {
		fprintf(ofd, "  <ele>%f</ele>\n",
			 waypointp->altitude);
	}
	if (waypointp->creation_time) {
		xml_write_time(ofd, waypointp->creation_time, "time");
	}
	write_optional_xml_entity(ofd, "  ", "name", oname);
	write_optional_xml_entity(ofd, "  ", "cmt", waypointp->description);
	if (waypointp->notes && waypointp->notes[0])
		write_xml_entity(ofd, "  ", "desc", waypointp->notes);
	else
		write_optional_xml_entity(ofd, "  ", "desc", waypointp->description);
	write_gpx_url(waypointp);

	write_optional_xml_entity(ofd, "  ", "sym", waypointp->icon_descr);

	fprint_xml_chain( waypointp->gpx_extras, waypointp );
	fprintf(ofd, "</wpt>\n");
}

static void
gpx_track_hdr(const route_head *rte)
{
	fprintf(ofd, "<trk>\n");
	write_optional_xml_entity(ofd, "  ", "name", rte->rte_name);
	write_optional_xml_entity(ofd, "  ", "desc", rte->rte_desc);
	if (rte->rte_num) {
		fprintf(ofd, "<number>%d</number>\n", rte->rte_num);
	}
	fprintf(ofd, "<trkseg>\n");
}

static void
gpx_track_disp(const waypoint *waypointp)
{
	fprintf(ofd, "<trkpt lat=\"" FLT_FMT_T "\" lon=\"" FLT_FMT_T "\">\n",
		waypointp->latitude,
		waypointp->longitude);
	if (waypointp->altitude != unknown_alt) {
		fprintf(ofd, "  <ele>%f</ele>\n",
			 waypointp->altitude);
	}
	if (waypointp->creation_time) {
		xml_write_time(ofd, waypointp->creation_time,"time");
	}

	/* GPX doesn't require a name on output, so if we made one up
	 * on input, we might as well say nothing.
	 */
	if (!waypointp->wpt_flags.shortname_is_synthetic) {
		write_optional_xml_entity(ofd, "  ", "name", 
			waypointp->shortname);
	}
	write_optional_xml_entity(ofd, "  ", "desc", waypointp->notes);
	write_gpx_url(waypointp);
	write_optional_xml_entity(ofd, "  ", "sym", waypointp->icon_descr);
	fprintf(ofd, "</trkpt>\n");
}

static void
gpx_track_tlr(const route_head *rte)
{
	fprintf(ofd, "</trkseg>\n");
	fprintf(ofd, "</trk>\n");
}

static
void gpx_track_pr()
{
	track_disp_all(gpx_track_hdr, gpx_track_tlr, gpx_track_disp);
}

static void
gpx_route_hdr(const route_head *rte)
{
	fprintf(ofd, "<rte>\n");
	write_optional_xml_entity(ofd, "  ", "name", rte->rte_name);
	write_optional_xml_entity(ofd, "  ", "desc", rte->rte_desc);
	if (rte->rte_num) {
		fprintf(ofd, "  <number>%d</number>\n", rte->rte_num);
	}
}

static void
gpx_route_disp(const waypoint *waypointp)
{
	fprintf(ofd, "  <rtept lat=\"" FLT_FMT_R "\" lon=\"" FLT_FMT_R "\">\n",
		waypointp->latitude,
		waypointp->longitude);

	if (waypointp->altitude != unknown_alt) {
		fprintf(ofd, "    <ele>%f</ele>\n",
			 waypointp->altitude);
	}
	if (waypointp->creation_time) {
		xml_write_time(ofd, waypointp->creation_time,"time");
	}
	write_optional_xml_entity(ofd, "    ", "name", waypointp->shortname);
	write_optional_xml_entity(ofd, "    ", "cmt", waypointp->description);
	write_optional_xml_entity(ofd, "    ", "desc", waypointp->notes);
	write_optional_xml_entity(ofd, "    ", "sym", waypointp->icon_descr);
	fprintf(ofd, "  </rtept>\n");
}

static void
gpx_route_tlr(const route_head *rte)
{
	fprintf(ofd, "</rte>\n");
}

static
void gpx_route_pr()
{
	/* output routes */
	route_disp_all(gpx_route_hdr, gpx_route_tlr, gpx_route_disp);
}

void
gpx_write(void)
{
	time_t now = 0;
	int short_length;
	bounds bounds;

	gpx_wversion_num = strtod(gpx_wversion, NULL) * 10;

	if (gpx_wversion_num <= 0) {
		fatal(MYNAME ": gpx version number of '%s' not valid.\n", gpx_wversion);
	}
	
        now = current_time();

	if (snlen)
		short_length = atoi(snlen);
	else
		short_length = 32;

	if (suppresswhite) {
		setshort_whitespace_ok(mkshort_handle, 0);
	}

	setshort_length(mkshort_handle, short_length);

	fprintf(ofd, "<?xml version=\"1.0\"?>\n");
	fprintf(ofd, "<gpx\n version=\"%s\"\n", gpx_wversion);
	fprintf(ofd, "creator=\"GPSBabel - http://www.gpsbabel.org\"\n");
	fprintf(ofd, "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\n");
	fprintf(ofd, "xmlns=\"http://www.topografix.com/GPX/%c/%c\"\n", gpx_wversion[0], gpx_wversion[2]);
	if (xsi_schema_loc) {
		fprintf(ofd, "xsi:schemaLocation=\"%s\">\n", xsi_schema_loc);
	} else {
		fprintf(ofd,
			"xsi:schemaLocation=" DEFAULT_XSI_SCHEMA_LOC_FMT">\n",
			gpx_wversion[0], gpx_wversion[2],
			gpx_wversion[0], gpx_wversion[2]);
	}

	if (gpx_wversion_num > 10) {	
		fprintf(ofd, "<metadata>\n");
	}
	xml_write_time( ofd, now, "time" );
	waypt_compute_bounds(&bounds);
	if (bounds.max_lat  > -360) {
		fprintf(ofd, "<bounds minlat=\"%0.9f\" minlon =\"%0.9f\" "
			       "maxlat=\"%0.9f\" maxlon=\"%0.9f\" />\n",
			       bounds.min_lat, bounds.min_lon, 
			       bounds.max_lat, bounds.max_lon);
	}

	if (gpx_wversion_num > 10) {	
		fprintf(ofd, "</metadata>\n");
	}

	waypt_disp_all(gpx_waypt_pr);
	gpx_route_pr();
	gpx_track_pr();

	fprintf(ofd, "</gpx>\n");
}

static
arglist_t gpx_args[] = {
	{ "gsshortnames", &gsshortnames, 
		"Prefer shorter descriptions from Groundspeak files",
		NULL, ARGTYPE_BOOL },
	{ "snlen", &snlen, "Length of generated shortnames", 
		NULL, ARGTYPE_INT },
	{ "suppresswhite", &suppresswhite, 
		"Suppress whitespace in generated shortnames", 
		NULL, ARGTYPE_BOOL },
	{ "logpoint", &opt_logpoint, 
		"Create waypoints from geocache log entries", 
		NULL, ARGTYPE_BOOL },
	{ "urlbase", &urlbase, "Base URL for link tag in output", 
		NULL, ARGTYPE_STRING},
	{ "gpxver", &gpx_wversion, "Target GPX version for output", 
		"1.0", ARGTYPE_STRING},
	{ 0, 0, 0, 0, 0 }
};

ff_vecs_t gpx_vecs = {
	ff_type_file,
	FF_CAP_RW_ALL,
	gpx_rd_init,	
	gpx_wr_init,	
	gpx_rd_deinit,	
	gpx_wr_deinit,	
	gpx_read,
	gpx_write,
	NULL, 
	gpx_args,
};
