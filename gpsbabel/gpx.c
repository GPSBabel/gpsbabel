/*
    Access GPX data files.

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
#ifndef NO_EXPAT
	#include <expat.h>
	static XML_Parser psr;
#endif

static int in_wpt;
static int in_rte;
static int in_rtept;
static int in_trk;
static int in_trkpt;
static int in_ele;
static int in_name;
static int in_time;
static int in_desc;
static int in_cdata;
static int in_cmt;
static int in_url;
static int in_icon;
static int in_urlname;
static int in_gs_type;
static int in_gs_name;
static int in_gs_container;
static int in_gs_diff;
static int in_gs_terr;
static int in_gs_log;
static int in_gs_log_wpt;
static int in_gs_exported;
static int in_gs_tbugs;
static int in_something_else;
static int in_number;
static int in_email = 0;
static int in_author = 0;

static xml_tag *cur_tag;
static char *cdatastr;
static char *opt_logpoint = NULL;
static int logpoint_ct = 0;

static const char *gpx_version;
static const char *gpx_creator;

static char *gpx_email = NULL;
static char *gpx_author = NULL;
vmem_t current_tag;

static waypoint *wpt_tmp;
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

typedef enum {
	tt_unknown = 0,
	tt_ele,
	tt_name,
	tt_gpx,
	tt_email,
	tt_author,
	tt_wpt,
	tt_desc,
	tt_cmt,
	tt_rte,
	tt_rtept,
	tt_trk,
	tt_trkpt,
	tt_number,
	tt_time,
	tt_url,
	tt_urlname,
	tt_sym,
	tt_cache_type,
	tt_cache_name,
	tt_cache_container,
	tt_cache_difficulty,
	tt_cache_terrain,
	tt_cache_log,
	tt_cache_log_wpt,
	tt_cache_exported,
	tt_cache_travelbugs
} tag_type;

typedef struct tag_mapping {
	tag_type tag_type;
	const char *tag_name;
} tag_mapping;

tag_mapping tag_map[] = {
	{ tt_ele, "ele" },
	{ tt_name, "name" },
	{ tt_gpx, "gpx" },
	{ tt_email, "email" },
	{ tt_author, "author" },
	{ tt_wpt, "wpt" },
	{ tt_desc, "desc" },
	{ tt_cmt, "cmt" },
	{ tt_rte, "rte" },
	{ tt_rtept, "rtept" },
	{ tt_trk, "trk" },
	{ tt_trkpt, "trkpt" },
	{ tt_number, "number" },
	{ tt_time, "time" },
	{ tt_url, "url" },
	{ tt_urlname, "urlname" },
	{ tt_sym, "sym" },
	{ tt_cache_type, "groundspeak:type" },
	{ tt_cache_name, "groundspeak:name" },
	{ tt_cache_container, "groundspeak:container" },
	{ tt_cache_difficulty, "groundspeak:difficulty" },
	{ tt_cache_terrain, "groundspeak:terrain" },
	{ tt_cache_log, "groundspeak:log" },
	{ tt_cache_log_wpt, "groundspeak:log_wpt" },
	{ tt_cache_exported, "groundspeak:exported" },
	{ tt_cache_travelbugs, "groundspeak:travelbugs" },
	{0}
};

static tag_type
get_tag(const char *t)
{
	tag_mapping *tm;

        for (tm = tag_map; tm->tag_type != 0; tm++) {
		if (0 == strcmp(tm->tag_name, t)) {
			return tm->tag_type;
		}
	}
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
		avp+=2;
	}
}

static void
tag_wpt(const char **attrv)
{
	const char **avp = &attrv[0];

	wpt_tmp = xcalloc(sizeof(*wpt_tmp), 1);

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
	lwp_tmp = xcalloc(sizeof(*lwp_tmp), 1);

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
	vmem_realloc(&current_tag, strlen(current_tag.mem) + 1 + strlen(el));
	e = current_tag.mem;
	ep = e + strlen(e);
	*ep++ = '/';
	strcpy(ep, el);
	switch (get_tag(el)) {
	case tt_ele:
		in_ele++;
		break;
	case tt_name:
		in_name ++;
		break;
	case tt_gpx:
		tag_gpx(attr);
		break;
	case tt_email:
		in_email++;
		break;
	case tt_author:
		in_author++;
		break;
	case tt_wpt:
		in_wpt++;
		tag_wpt(attr);
		break;
	case tt_desc:
		in_desc++;
		break;
	case tt_cmt:
		in_cmt++;
		break;
	case tt_rte:
		rte_head = route_head_alloc();
		route_add_head(rte_head);
		in_rte++;
		break;
	case tt_rtept:
		in_rtept++;
		tag_wpt(attr);
		break; 
	case tt_trk:
		trk_head = route_head_alloc();
		route_add_head(trk_head);
		in_trk++;
		break;
	case tt_trkpt:
		in_trkpt++;
		tag_wpt(attr);
		break;
	case tt_number:
		in_number++;
		break;
	case tt_time:
		in_time++;
		break;
	case tt_url:
		in_url++;
		break;
	case tt_urlname:
		in_urlname++;
		break;
	case tt_sym:
		in_icon++;
		break;
	case tt_cache_type:
		in_gs_type++;
		in_something_else++;
		start_something_else( el, attr );
		break;
	case tt_cache_name:
		in_gs_name++;
		in_something_else++;
		start_something_else( el, attr );
		break;
	case tt_cache_container:
		in_gs_container++;
		in_something_else++;
		start_something_else( el, attr );
		break;
	case tt_cache_difficulty:
		in_gs_diff++;
		in_something_else++;
		start_something_else( el, attr );
		break;
	case tt_cache_terrain:
		in_gs_terr++;
		in_something_else++;
		start_something_else( el, attr );
		break;
	case tt_cache_log:
		in_gs_log++;
		in_something_else++;
		start_something_else( el, attr );
		break;
	case tt_cache_log_wpt:
		in_gs_log_wpt++;
                if (opt_logpoint)
		    tag_log_wpt(attr);
		in_something_else++;
		start_something_else( el, attr );
		break;
	case tt_cache_exported:
		in_gs_exported++;
		/* no start_something_else because the old date is eaten */
		break;
	case tt_cache_travelbugs:
		in_gs_tbugs++;
		in_something_else++;
		start_something_else( el, attr );
		break;
	default:
		if (in_wpt) {
			in_something_else++;
			start_something_else( el, attr );
		}
		break;
	}
}

struct
gs_type_mapping{
	geocache_type type;
	const char *name;
} gs_type_map[] = {
	{ gt_traditional, "Traditional cache" },
	{ gt_multi, "Multi-Cache" },
	{ gt_virtual, "Virtual cache" }
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
	{ gc_virtual, "Virtual" }
};

static
geocache_type
gs_mktype(char *t)
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

static
geocache_container
gs_mkcont(char *t)
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

static
time_t 
xml_parse_time( char *cdatastr ) 
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

	if (strcmp(s + 1, el)) {
		fprintf(stderr, "Mismatched tag %s\n", el);
	}
	*s = 0;


	if (in_cdata) {
		if (in_name && in_wpt && !in_gs_tbugs) {
			wpt_tmp->shortname = xstrdup(cdatastr);
		}
		if (in_name && in_trk && !in_trkpt) {
			trk_head->rte_name = xstrdup(cdatastr);
		}
		if (in_desc && in_trk && !in_trkpt) {
			trk_head->rte_desc = xstrdup(cdatastr);
		}
		if (in_number && in_trk) {
			trk_head->rte_num = atoi(cdatastr);
		}
		if (in_name && in_rte && ! in_rtept) {
			rte_head->rte_name = xstrdup(cdatastr);
		}
		if (in_desc && in_rte && ! in_rtept ) {
			rte_head->rte_desc = xstrdup(cdatastr);
		}
		if (in_number && in_rte) {
			rte_head->rte_num = atoi(cdatastr);
		}
		if (in_name && in_rtept) {
			wpt_tmp->shortname = xstrdup(cdatastr);
		}
		if (in_desc && in_rtept) {
			wpt_tmp->notes = xstrdup(cdatastr);
		}
		if (in_cmt && in_rtept) {
			wpt_tmp->description = xstrdup(cdatastr);
		}
		if (in_email) {
			if ( gpx_email ) xfree(gpx_email);
			gpx_email = xstrdup(cdatastr);
		}
		if (in_author) {
			if ( gpx_author ) xfree(gpx_author);
			gpx_author = xstrdup(cdatastr);
		}
		if (gsshortnames) {
			if (in_gs_name && in_wpt && !in_gs_tbugs) {
				wpt_tmp->notes = xstrdup(cdatastr);
			}
		} else {
			if (in_desc && in_wpt) {
				wpt_tmp->notes = xstrdup(cdatastr);
			}
		}
		if ((in_cmt && in_wpt) || (in_cmt && in_rtept)) {
			wpt_tmp->description = xstrdup(cdatastr);
		}
		if (in_url && in_wpt) {
			wpt_tmp->url = xstrdup(cdatastr);
		}
		if (in_urlname && in_wpt) {
			wpt_tmp->url_link_text = xstrdup(cdatastr);
		}
		if ((in_icon && in_wpt) || (in_icon && in_rtept)) {
			wpt_tmp->icon_descr = xstrdup(cdatastr);
			wpt_tmp->icon_descr_is_dynamic = 1;
		}
		if (in_ele) {
			sscanf(cdatastr, "%lf", 
				&wpt_tmp->altitude);
		}
		if (in_time) {
			if ( in_wpt || in_rte || in_trkpt || in_rtept) {
                        	wpt_tmp->creation_time = 
					xml_parse_time( cdatastr );
			}
			else {
				file_time = xml_parse_time( cdatastr );
			}
		}
		if (in_wpt && in_gs_type && !in_gs_log) {
			wpt_tmp->gc_data.type = gs_mktype(cdatastr);
		}
		if (in_wpt && in_gs_container) {
			wpt_tmp->gc_data.container = gs_mkcont(cdatastr);
		}
		if (in_wpt && in_gs_diff) {
			sscanf(cdatastr, "%f", &x);
			wpt_tmp->gc_data.diff = x * 10;
		}
		if (in_wpt && in_gs_terr) {
			sscanf(cdatastr, "%f", &x);
			wpt_tmp->gc_data.terr = x * 10;
		}
		if (in_gs_exported && in_wpt ) {
                        wpt_tmp->gc_data.exported = xml_parse_time( cdatastr );
		}
		in_cdata--;
		memset(cdatastr, 0, MY_CBUF);
	}
	switch (get_tag(el)) {
	case tt_wpt:
		if ( !wpt_tmp->gc_data.exported ) {
			wpt_tmp->gc_data.exported = file_time;
		}
		waypt_add(wpt_tmp);
		in_wpt--;
		logpoint_ct = 0;
		break;
	case tt_rte:
		in_rte--;
		break;
	case tt_rtept:
		route_add_wpt(rte_head, wpt_tmp);
		in_rtept--;
		break;
	case tt_trk:
		in_trk--;
		break;
	case tt_trkpt:
		route_add_wpt(trk_head, wpt_tmp);
		in_trkpt--;
		break;
	case tt_number:
		in_number--;
		break;
	case tt_name:
		in_name--;
		break;
	case tt_desc:
		in_desc--;
		break;
	case tt_email:
		in_email--;
		break;
	case tt_author:
		in_author--;
		break;
	case tt_cmt:
		in_cmt--;
		break;
	case tt_ele:
		in_ele--;
		break;
	case tt_time:
		in_time--;
		break;
	case tt_url:
		in_url--;
		break;
	case tt_urlname:
		in_urlname--;
		break;
	case tt_sym:
		in_icon--;
		break;
	case tt_cache_type:
		in_gs_type--;
		in_something_else--;
		end_something_else();
		break;
	case tt_cache_name:
		in_gs_name--;
		in_something_else--;
		end_something_else();
		break;
	case tt_cache_container:
		in_gs_container--;
		in_something_else--;
		end_something_else();
		break;
	case tt_cache_difficulty:
		in_gs_diff--;
		in_something_else--;
		end_something_else();
		break;
	case tt_cache_terrain:
		in_gs_terr--;
		in_something_else--;
		end_something_else();
		break;
	case tt_cache_log:
		in_gs_log--;
		in_something_else--;
		end_something_else();
		break;
	case tt_cache_log_wpt:
		in_gs_log_wpt--;
		in_something_else--;
		end_something_else();
		break;
	case tt_cache_exported:
		in_gs_exported--;
		/* no end_something_else because the old date is eaten */
		break;
	case tt_cache_travelbugs:
		in_gs_tbugs--;
		in_something_else--;
		end_something_else();
		break;
	default:
		if (in_wpt) {
		in_something_else--;
		end_something_else();
		}
	}
}

#if NO_EXPAT
void
gpx_rd_init(const char *fname)
{
	fatal(MYNAME ": This build excluded GPX support becuase expat was not installed.\n");
}

#else /* NO_EXPAT */

static void
gpx_cdata(void *dta, const XML_Char *s, int len)
{
	char *estr;
	int *cdatalen;
	char **cdata;
	xml_tag *tmp_tag;

	/*
	 * I'm exceedingly unamused that libexpat makes me keep all this
	 * horrible state just I can concatenate buffers that it hands
	 * me as a cdata that are fragmented becuae they span a read.  Grrr.
	 */
	if ((in_name && in_wpt) || (in_desc && in_wpt) || (in_ele) ||
		        (in_email) || (in_author) ||	
			(in_wpt && in_cmt) ||
			(in_wpt && in_url) ||
			(in_wpt && in_urlname) ||
			(in_wpt && in_gs_type) || 
			(in_wpt && in_gs_name) || 
			(in_wpt && in_gs_container) || 
			(in_wpt && in_gs_diff) || 
			(in_wpt && in_gs_terr) || 
			(in_wpt && in_icon) || 
			(in_trk && in_name) || 
			(in_trk && in_desc) || 
			(in_trk && in_number) || 
			(in_rte && in_cmt) || 
			(in_rte && in_name) || 
			(in_rte && in_desc) || 
			(in_rte && in_icon) || 
			(in_rte && in_number) || 
			(in_time))  {
		estr = cdatastr + strlen(cdatastr);
		memcpy(estr, s, len);
		in_cdata++;
	}
	if ( in_wpt && in_something_else && cur_tag && !in_gs_exported) {
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
}

void
gpx_rd_init(const char *fname)
{
	if ( fname[0] ) {
	        fd = fopen(fname, "r");
	        if (fd == NULL) {
		        fatal(MYNAME ": Cannot open %s for reading\n", fname );
	        }
	}
	else {
		fd = NULL;
		input_string = fname+1;
		input_string_len = strlen(input_string);
	}


	file_time = 0;
	current_tag = vmem_alloc(1);
	
	psr = XML_ParserCreate(NULL);
	if (!psr) {
		fatal(MYNAME ": Cannot create XML Parser\n");
	}
	cdatastr = xcalloc(MY_CBUF, 1);
	XML_SetElementHandler(psr, gpx_start, gpx_end);
	XML_SetCharacterDataHandler(psr, gpx_cdata);
}
#endif

static void
gpx_rd_deinit(void)
{
	vmem_free(&current_tag);
	if ( cdatastr ) {
		xfree(cdatastr);
		cdatastr = NULL;
	}
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
}

void
gpx_wr_init(const char *fname)
{
	mkshort_handle = mkshort_new_handle();

	ofd = fopen(fname, "w");
	if (ofd == NULL) {
		fatal(MYNAME ": open %s for writing\n", fname );
	}
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
						*semi = '\0';
						while (*hexit) {
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

/*
 *
 */
static
void
gpx_write_time(const time_t timep, char *elname)
{
	struct tm *tm = gmtime(&timep);
	
	if (!tm)
		return;
	
	fprintf(ofd, "<%s>%02d-%02d-%02dT%02d:%02d:%02dZ</%s>\n",
		elname,
		tm->tm_year+1900, 
		tm->tm_mon+1, 
		tm->tm_mday, 
		tm->tm_hour, 
		tm->tm_min, 
		tm->tm_sec,
		elname
	);

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
				gpx_write_time( wpt->gc_data.exported, 
						"groundspeak:exported" );
			}
			fprintf( ofd, "</%s>", tag->tagname);
			if ( tag->parentcdata ) {
				tmp_ent = xml_entitize(tag->parentcdata);
				fprintf(ofd, "%s", tmp_ent );
				xfree(tmp_ent);
			}
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

static void
gpx_waypt_pr(const waypoint *waypointp)
{
	char *tmp_ent;
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

	fprintf(ofd, "<wpt lat=\"%lf\" lon=\"%lf\">\n",
		waypointp->latitude,
		waypointp->longitude);
	if (waypointp->creation_time) {
		gpx_write_time(waypointp->creation_time, "time");
	}
	if (oname) {
		tmp_ent = xml_entitize(oname);
		fprintf(ofd, "<name>%s</name>\n", tmp_ent);
		xfree(tmp_ent);
	}
	if (waypointp->description) {
		fprintf(ofd, "<cmt>");
		fprintf(ofd, "<![CDATA[%s]]>", waypointp->description);
		fprintf(ofd, "</cmt>\n");
	}
	if (waypointp->notes) {
		fprintf(ofd, "<desc>");
		fprintf(ofd, "<![CDATA[%s]]>", waypointp->notes);
		fprintf(ofd, "</desc>\n");
	} else {
		if (waypointp->description) {
			fprintf(ofd, "<desc>");
			fprintf(ofd, "<![CDATA[%s]]>", waypointp->description);
			fprintf(ofd, "</desc>\n");
		}
	}
	if (waypointp->altitude) {
		fprintf(ofd, "<ele>\n%f\n</ele>\n",
			 waypointp->altitude);
	}
	if (waypointp->url) {
		tmp_ent = xml_entitize(waypointp->url);
		fprintf(ofd, "<url>%s%s</url>\n", urlbase ? urlbase : "", tmp_ent);
		xfree(tmp_ent);
	}
	if (waypointp->url_link_text) {
		tmp_ent = xml_entitize(waypointp->url_link_text);
		fprintf(ofd, "<urlname>%s</urlname>\n", tmp_ent );
		xfree(tmp_ent);
	}
	if (waypointp->icon_descr) {
		tmp_ent = xml_entitize(waypointp->icon_descr);
		fprintf(ofd, "<sym>%s</sym>\n", tmp_ent );
		xfree(tmp_ent);
	}

	fprint_xml_chain( waypointp->gpx_extras, waypointp );
	fprintf(ofd, "</wpt>\n");
}

static void
gpx_track_hdr(const route_head *rte)
{
	char * tmp_ent;
	
	fprintf(ofd, "<trk>\n");
	if (rte->rte_name) {
		tmp_ent = xml_entitize(rte->rte_name);
		fprintf(ofd, "<name>%s</name>\n", tmp_ent);
		xfree(tmp_ent);
	}
	if (rte->rte_desc) {
		tmp_ent = xml_entitize(rte->rte_desc);
		fprintf(ofd, "<desc>%s</desc>\n", tmp_ent);
		xfree(tmp_ent);
	}
	if (rte->rte_num) {
		fprintf(ofd, "<number>%d</number>\n", rte->rte_num);
	}
	fprintf(ofd, "<trkseg>\n");
}

static void
gpx_track_disp(const waypoint *waypointp)
{
	fprintf(ofd, "<trkpt lat=\"%lf\" lon=\"%lf\">\n",
		waypointp->latitude,
		waypointp->longitude);
	if (waypointp->altitude != unknown_alt) {
		fprintf(ofd, "<ele>%f</ele>\n",
			 waypointp->altitude);
	}
	if (waypointp->creation_time) {
		gpx_write_time(waypointp->creation_time,"time");
	}
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
	route_disp_all(gpx_track_hdr, gpx_track_tlr, gpx_track_disp);
}

static void
gpx_route_hdr(const route_head *rte)
{
	char * tmp_ent;
	
	fprintf(ofd, "<rte>\n");
	if (rte->rte_name) {
		tmp_ent = xml_entitize(rte->rte_name);
		fprintf(ofd, "<name>%s</name>\n", tmp_ent);
		xfree(tmp_ent);
	}
	if (rte->rte_desc) {
		tmp_ent = xml_entitize(rte->rte_desc);
		fprintf(ofd, "<desc>%s</desc>\n", tmp_ent);
		xfree(tmp_ent);
	}
	if (rte->rte_num) {
		fprintf(ofd, "<number>%d</number>\n", rte->rte_num);
	}
}

static void
gpx_route_disp(const waypoint *waypointp)
{
	fprintf(ofd, "<rtept lat=\"%f\" lon=\"%f\">\n",
		waypointp->latitude,
		waypointp->longitude);

	if (waypointp->altitude != unknown_alt) {
		fprintf(ofd, "<ele>%f</ele>\n",
			 waypointp->altitude);
	}
	if (waypointp->creation_time) {
		gpx_write_time(waypointp->creation_time,"time");
	}
	if (waypointp->shortname) {
		fprintf(ofd, "<name>");
		fprintf(ofd, "<![CDATA[%s]]>", waypointp->shortname);
		fprintf(ofd, "</name>\n");
	}
	if (waypointp->description) {
		fprintf(ofd, "<cmt>");
		fprintf(ofd, "<![CDATA[%s]]>", waypointp->description);
		fprintf(ofd, "</cmt>\n");
	}
	if (waypointp->notes) {
		fprintf(ofd, "<desc>");
		fprintf(ofd, "<![CDATA[%s]]>", waypointp->notes);
		fprintf(ofd, "</desc>\n");
	} 
	if (waypointp->icon_descr) {
		fprintf(ofd, "<sym>");
		fprintf(ofd, "<![CDATA[%s]]>", waypointp->icon_descr);
		fprintf(ofd, "</sym>");
	}

	fprintf(ofd, "</rtept>\n");
}

static void
gpx_route_tlr(const route_head *rte)
{
	fprintf(ofd, "</rte>\n");
}

static
void gpx_noop()
{
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
	
        time( &now );

	if (snlen)
		short_length = atoi(snlen);
	else
		short_length = 32;

	if (suppresswhite) {
		setshort_whitespace_ok(mkshort_handle, 0);
	}

	setshort_length(mkshort_handle, short_length);

	fprintf(ofd, "<?xml version=\"1.0\"?>\n");
	fprintf(ofd, "<gpx\n version=\"1.0\"\n");
	fprintf(ofd, "creator=\"GPSBabel - http://gpsbabel.sourceforge.net\"\n");
	fprintf(ofd, "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\n");
	fprintf(ofd, "xmlns=\"http://www.topografix.com/GPX/1/0\"\n");
	fprintf(ofd, "xsi:schemaLocation=\"http://www.topografix.com/GPX/1/0 http://www.topografix.com/GPX/1/0/gpx.xsd\">\n");

        gpx_write_time( now, "time" );
	switch(global_opts.objective) {
		case trkdata: 
				gpx_track_pr();
				break;
		case rtedata: 
				gpx_route_pr();
				break;
		case wptdata: 
				waypt_disp_all(gpx_waypt_pr);
			      	break;
		default:
			      break;
	}

	fprintf(ofd, "</gpx>\n");
}

static
arglist_t gpx_args[] = {
	{ "gsshortnames", &gsshortnames, 
		"Prefer shorter descriptions from Groundspeak files",
		ARGTYPE_BOOL },
	{ "snlen", &snlen, "Length of generated shortnames", ARGTYPE_INT },
	{ "suppresswhite", &suppresswhite, 
		"Suppress whitespace in generated shortnames", ARGTYPE_BOOL },
	{ "logpoint", &opt_logpoint, "Create waypoints from geocache log entries", ARGTYPE_BOOL },
	{ "urlbase", &urlbase, "Base URL for link tag in output", ARGTYPE_STRING},
	{ 0, 0, 0, 0 }
};

ff_vecs_t gpx_vecs = {
	gpx_rd_init,	
	gpx_wr_init,	
	gpx_rd_deinit,	
	gpx_wr_deinit,	
	gpx_read,
	gpx_write,
	gpx_args,
};
