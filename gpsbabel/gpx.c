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
#include <expat.h>

static int in_wpt;
static int in_rte;
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
static xml_tag *cur_tag;
static char *cdatastr;
static int opt_logpoint = 0;
static int logpoint_ct = 0;

static XML_Parser psr;

static const char *gpx_version;
static const char *gpx_creator;

static waypoint *wpt_tmp;
static FILE *fd;
static FILE *ofd;
static void *mkshort_handle;

static time_t file_time;

static char *gsshortnames = NULL;

#define MYNAME "GPX"
#define MY_CBUF 4096

static
char * gpx_entitize(const char * str) 
{
	int elen, ecount;
	const char ** ep;
	const char * cp;
	char * p, * tmp, * xstr;
	const char * stdentities[] = {
	"&",	"&amp;",
	"<",	"&lt;",
	">",	"&gt;",
	"'", 	"&apos;",
	"\"",	"&quot;",
	NULL,	NULL 
	};
	ep = stdentities;
	elen = ecount = 0;

	/* figure # of entity replacements and additional size. */
	while (*ep) {
		cp = str;
		while ((cp = strstr(cp, *ep)) != NULL) {
			elen += strlen(*(ep + 1)) - strlen(*ep);
			ecount++;
			cp += strlen(*ep);
		}
		ep += 2;
	}

	/* enough space for the whole string plus entity replacements, if any */
	tmp = xcalloc((strlen(str) + elen + 1), 1);
	strcpy(tmp, str);

	/* no entity replacements */
	if (ecount == 0)
		return (tmp);

	ep = stdentities;

	while (*ep) {
		p = tmp;
		while ((p = strstr(p, *ep)) != NULL) {
			elen = strlen(*(ep + 1));

			xstr = xstrdup(p + strlen(*ep));

			strcpy(p, *(ep + 1));
			strcpy(p + elen, xstr);

			xfree(xstr);

			p += elen;
		}  
		ep += 2;
	}    
	return (tmp);
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
				&wpt_tmp->position.latitude.degrees);
		}
		else if (strcmp(avp[0], "lon") == 0) {
			sscanf(avp[1], "%lf", 
				&wpt_tmp->position.longitude.degrees);
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
				&lwp_tmp->position.latitude.degrees);
		}
		else if (strcmp(avp[0], "lon") == 0) {
			sscanf(avp[1], "%lf", 
			&lwp_tmp->position.longitude.degrees);
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

	if (strcmp(el, "ele") == 0) {
		in_ele++;
	} 
	else if (strcmp(el, "name") == 0) {
		in_name ++;
	} 
	else if (strcmp(el, "gpx") == 0) {
		tag_gpx(attr);
	} 
	else if (strcmp(el, "wpt") == 0) {
		in_wpt++;
		tag_wpt(attr);
	} 
	else if (strcmp(el, "desc") == 0) {
		in_desc++;
	} 
	else if (strcmp(el, "cmt") == 0) {
		in_cmt++;
	} 
	else if (strcmp(el, "rtept") == 0) {
		in_rte++;
		tag_wpt(attr);
	} 
	else if (strcmp(el, "time") == 0) {
		in_time++;
	} 
	else if (strcmp(el, "url") == 0) {
		in_url++;
	} 
	else if (strcmp(el, "urlname") == 0) {
		in_urlname++;
	} 
	else if (strcmp(el, "sym") == 0) {
		in_icon++;
	} 
	else if (strcmp(el, "groundspeak:type") == 0) {
		in_gs_type++;
		in_something_else++;
		start_something_else( el, attr );
	} 
	else if (strcmp(el, "groundspeak:name") == 0) {
		in_gs_name++;
		in_something_else++;
		start_something_else( el, attr );
	} 
	else if (strcmp(el, "groundspeak:container") == 0) {
		in_gs_container++;
		in_something_else++;
		start_something_else( el, attr );
	} 
	else if (strcmp(el, "groundspeak:difficulty") == 0) {
		in_gs_diff++;
		in_something_else++;
		start_something_else( el, attr );
	} 
	else if (strcmp(el, "groundspeak:terrain") == 0) {
		in_gs_terr++;
		in_something_else++;
		start_something_else( el, attr );
	} 
	else if (strcmp(el, "groundspeak:log") == 0) {
		in_gs_log++;
		in_something_else++;
		start_something_else( el, attr );
	} 
	else if (strcmp(el, "groundspeak:log_wpt") == 0) {
		in_gs_log_wpt++;
                if (opt_logpoint)
		    tag_log_wpt(attr);
		in_something_else++;
		start_something_else( el, attr );
	}
	else if (strcmp(el, "groundspeak:exported") == 0) {
		in_gs_exported++;
		/* no start_something_else because the old date is eaten */
	}
	else if (strcmp(el, "groundspeak:travelbugs") == 0) {
		in_gs_tbugs++;
		in_something_else++;
		start_something_else( el, attr );
	} 
	else if (in_wpt) {
		in_something_else++;
		start_something_else( el, attr );
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
	float x;
	if (in_cdata) {
		if (in_name && in_wpt && !in_gs_tbugs) {
			wpt_tmp->shortname = xstrdup(cdatastr);
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
		if (in_cmt && in_wpt) {
			wpt_tmp->description = xstrdup(cdatastr);
		}
		if (in_url && in_wpt) {
			wpt_tmp->url = xstrdup(cdatastr);
		}
		if (in_urlname && in_wpt) {
			wpt_tmp->url_link_text = xstrdup(cdatastr);
		}
		if (in_icon && in_wpt) {
			wpt_tmp->icon_descr = xstrdup(cdatastr);
			wpt_tmp->icon_descr_is_dynamic = 1;
		}
		if (in_ele) {
			sscanf(cdatastr, "%lf", 
				&wpt_tmp->position.altitude.altitude_meters);
		}
		if (in_time) {
			if ( in_wpt || in_rte) {
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
	if (strcmp(el, "wpt") == 0) {
		if ( !wpt_tmp->gc_data.exported ) {
			wpt_tmp->gc_data.exported = file_time;
		}
		waypt_add(wpt_tmp);
		in_wpt--;
		logpoint_ct = 0;
	}
	else if (strcmp(el, "rtept") == 0) {
/*		route_add(wpt_tmp); */
		in_rte--;
	} else if (strcmp(el, "name") == 0) {
		in_name--;
	} else if (strcmp(el, "desc") == 0) {
		in_desc--;
	} else if (strcmp(el, "cmt") == 0) {
		in_cmt--;
	} else if (strcmp(el, "ele") == 0) {
		in_ele--;
	} else if (strcmp(el, "time") == 0) {
		in_time--;
	} else if (strcmp(el, "url") == 0) {
		in_url--;
	} else if (strcmp(el, "urlname") == 0) {
		in_urlname--;
	} else if (strcmp(el, "sym") == 0) {
		in_icon--;
	} else if (strcmp(el, "groundspeak:type") == 0) {
		in_gs_type--;
		in_something_else--;
		end_something_else();
	} else if (strcmp(el, "groundspeak:name") == 0) {
		in_gs_name--;
		in_something_else--;
		end_something_else();
	} else if (strcmp(el, "groundspeak:container") == 0) {
		in_gs_container--;
		in_something_else--;
		end_something_else();
	} else if (strcmp(el, "groundspeak:difficulty") == 0) {
		in_gs_diff--;
		in_something_else--;
		end_something_else();
	} else if (strcmp(el, "groundspeak:terrain") == 0) {
		in_gs_terr--;
		in_something_else--;
		end_something_else();
	} else if (strcmp(el, "groundspeak:log") == 0) {
		in_gs_log--;
		in_something_else--;
		end_something_else();
	} else if (strcmp(el, "groundspeak:log_wpt") == 0) {
		in_gs_log_wpt--;
		in_something_else--;
		end_something_else();
	} else if (strcmp(el, "groundspeak:exported") == 0) {
		in_gs_exported--;
		/* no end_something_else because the old date is eaten */
	} else if (strcmp(el, "groundspeak:travelbugs") == 0) {
		in_gs_tbugs--;
		in_something_else--;
		end_something_else();
	} else if (in_wpt) {
		in_something_else--;
		end_something_else();
	}
}

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
			(in_wpt && in_cmt) ||
			(in_wpt && in_url) ||
			(in_wpt && in_urlname) ||
			(in_wpt && in_gs_type) || 
			(in_wpt && in_gs_name) || 
			(in_wpt && in_gs_container) || 
			(in_wpt && in_gs_diff) || 
			(in_wpt && in_gs_terr) || 
			(in_wpt && in_icon) || 
			(in_time))  {
		estr = cdatastr + strlen(cdatastr);
		memcpy(estr, s, len);
		in_cdata++;
	}
	if ( in_wpt && in_something_else && cur_tag ) {
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
gpx_rd_init(const char *fname, const char *args)
{
	fd = fopen(fname, "r");
	if (fd == NULL) {
		fatal(MYNAME ": Cannot open %s for reading\n", fname );
	}

        if (get_option(args, "logpoint") != NULL)
            opt_logpoint = 1;

	file_time = 0;
	
	psr = XML_ParserCreate(NULL);
	if (!psr) {
		fatal(MYNAME ": Cannot create XML Parser\n");
	}
	cdatastr = xcalloc(MY_CBUF, 1);
	XML_SetElementHandler(psr, gpx_start, gpx_end);
	XML_SetCharacterDataHandler(psr, gpx_cdata);
}

static void
gpx_rd_deinit(void)
{
	if ( cdatastr ) {
		xfree(cdatastr);
	}
	fclose(fd);
}

void
gpx_wr_init(const char *fname, const char *args)
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
	int len;
	int done = 0;
	char buf[MY_CBUF];

	while (!done) {
		len = fread(buf, 1, sizeof(buf), fd);
		done = feof(fd) || !len; 
		if (!XML_Parse(psr, buf, len, done)) {
			fatal(MYNAME ": XML parse error at %d: %s\n", 
				XML_GetCurrentLineNumber(psr),
				XML_ErrorString(XML_GetErrorCode(psr)));
		}
	}
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
				tmp_ent = gpx_entitize( tag->cdata );
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
				tmp_ent = gpx_entitize(tag->parentcdata);
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
		next = tag->sibling;
		xfree(tag);
		tag = next;
	}
}

static void
gpx_waypt_pr(const waypoint *waypointp)
{
	char *tmp_ent;
	const char *oname = global_opts.synthesize_shortnames ?
				  mkshort(mkshort_handle, waypointp->description) : 
				  waypointp->shortname;

	fprintf(ofd, "<wpt lat=\"%lf\" lon=\"%lf\">\n",
		waypointp->position.latitude.degrees,
		waypointp->position.longitude.degrees);
	if (oname) {
		tmp_ent = gpx_entitize(oname);
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
	if (waypointp->position.altitude.altitude_meters) {
		fprintf(ofd, "<ele>\n%f\n</ele>\n",
			 waypointp->position.altitude.altitude_meters);
	}
	if (waypointp->creation_time) {
		gpx_write_time(waypointp->creation_time, "time");
	}
	if (waypointp->url) {
		tmp_ent = gpx_entitize(waypointp->url);
		fprintf(ofd, "<url>%s</url>\n", tmp_ent);
		xfree(tmp_ent);
	}
	if (waypointp->url_link_text) {
		tmp_ent = gpx_entitize(waypointp->url_link_text);
		fprintf(ofd, "<urlname>%s</urlname>\n", tmp_ent );
		xfree(tmp_ent);
	}
	if (waypointp->icon_descr) {
		tmp_ent = gpx_entitize(waypointp->icon_descr);
		fprintf(ofd, "<sym>%s</sym>\n", tmp_ent );
		xfree(tmp_ent);
	}

	fprint_xml_chain( waypointp->gpx_extras, waypointp );
	fprintf(ofd, "</wpt>\n");
}

static void
gpx_track_hdr(const route_head *rte)
{
	fprintf(ofd, "<trk>\n");
	if (rte->rte_name) {
		fprintf(ofd, "  <name>\n");
		fprintf(ofd, "  <![CDATA[%s]]>\n",rte->rte_name);
		fprintf(ofd, "  </name>\n");
	}
	fprintf(ofd, "<trkseg>\n");
}

static void
gpx_track_disp(const waypoint *waypointp)
{
	fprintf(ofd, "<trkpt lat=\"%lf\" lon=\"%lf\">\n",
		waypointp->position.latitude.degrees,
		waypointp->position.longitude.degrees);
	if (waypointp->creation_time) {
		gpx_write_time(waypointp->creation_time,"time");
	}
	if (waypointp->position.altitude.altitude_meters != unknown_alt) {
		fprintf(ofd, "<ele>\n%f\n</ele>\n",
			 waypointp->position.altitude.altitude_meters);
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

void
gpx_write(void)
{
	time_t now = 0;
	
        time( &now );
	setshort_length(mkshort_handle, 32);

	fprintf(ofd, "<?xml version=\"1.0\"?>\n");
	fprintf(ofd, "<gpx\n version=\"1.0\"\n");
	fprintf(ofd, "creator=\"GPSBabel - http://gpsbabel.sourceforge.net\"\n");
	fprintf(ofd, "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\n");
	fprintf(ofd, "xmlns=\"http://www.topografix.com/GPX/1/0\"\n");
	fprintf(ofd, "xsi:schemaLocation=\"http://www.topografix.com/GPX/1/0 http://www.topografix.com/GPX/1/0/gpx.xsd\">\n");

        gpx_write_time( now, "time" );
	switch(global_opts.objective) {
		case trkdata: gpx_track_pr(); 
		case wptdata: waypt_disp_all(gpx_waypt_pr);
			      break;
		default:
			      break;
	}

	fprintf(ofd, "</gpx>\n");
}

static
arglist_t gpx_args[] = {
	{ "gsshortnames", &gsshortnames, "Prefer shorter descriptions from Groundspeak files"},
	{ 0, 0, 0}
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
