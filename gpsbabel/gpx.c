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
static int in_gs_type;
static int in_gs_diff;
static int in_gs_terr;
static char *cdatastr;

static XML_Parser psr;

static const char *gpx_version;
static const char *gpx_creator;

static waypoint *wpt_tmp;
static FILE *fd;
static FILE *ofd;

#define MYNAME "GPX"
#define MY_CBUF 4096

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
gpx_start(void *data, const char *el, const char **attr)
{

	if (strcmp(el, "ele") == 0) {
		in_ele++;
	} if (strcmp(el, "name") == 0) {
		in_name ++;
	} if (strcmp(el, "gpx") == 0) {
		tag_gpx(attr);
	} if (strcmp(el, "wpt") == 0) {
		in_wpt++;
		tag_wpt(attr);
	} if (strcmp(el, "desc") == 0) {
		in_desc++;
	} if (strcmp(el, "cmt") == 0) {
		in_cmt++;
	} if (strcmp(el, "rtept") == 0) {
		in_rte++;
		tag_wpt(attr);
	} if (strcmp(el, "time") == 0) {
		in_time++;
	} if (strcmp(el, "groundspeak:type") == 0) {
		in_gs_type++;
	} if (strcmp(el, "groundspeak:difficulty") == 0) {
		in_gs_diff++;
	} if (strcmp(el, "groundspeak:terrain") == 0) {
		in_gs_terr++;
	}
}

struct
gs_type_mapping{
	geocache_type type;
	const char *name;
} gs_type_map[] = {
	{ gt_traditional, "Traditional cache" },
	{ gt_virtual, "Virtual cache" }
};
static
geocache_type
gs_mktype(char *t)
{
	int i;
	int sz = sizeof(gs_type_map) / sizeof(gs_type_map[0]);

	for (i = 0; i < sz; i++) {
		if (0 == strcmp(t, gs_type_map[i].name)) {
			return gs_type_map[i].type;
		}
	}
	return gt_unknown;
}

static void
gpx_end(void *data, const char *el)
{
	float x;
	if (in_cdata) {
		if (in_name && in_wpt) {
			wpt_tmp->shortname = xstrdup(cdatastr);
		}
		if (in_desc && in_wpt) {
			wpt_tmp->notes = xstrdup(cdatastr);
		}
		if (in_cmt && in_wpt) {
			wpt_tmp->description = xstrdup(cdatastr);
		}
		if (in_ele) {
			sscanf(cdatastr, "%lf", 
				&wpt_tmp->position.altitude.altitude_meters);
		}
		if (in_time && (in_wpt || in_rte)) {
			struct tm tm;
			sscanf(cdatastr, "%d-%d-%dT%d:%d:%dZ\n", 
				&tm.tm_year,
				&tm.tm_mon,
				&tm.tm_mday,
				&tm.tm_hour,
				&tm.tm_min,
				&tm.tm_sec);
			tm.tm_mon -= 1;
			tm.tm_year -= 1900;
			tm.tm_isdst = 1;
			wpt_tmp->creation_time = mktime(&tm);
		}
		if (in_wpt && in_gs_type) {
			wpt_tmp->gc_data.type = gs_mktype(cdatastr);
		}
		if (in_wpt && in_gs_diff) {
			sscanf(cdatastr, "%f", &x);
			wpt_tmp->gc_data.diff = x * 10;
		}
		if (in_wpt && in_gs_terr) {
			sscanf(cdatastr, "%f", &x);
			wpt_tmp->gc_data.terr = x * 10;
		}
		in_cdata--;
		memset(cdatastr, 0, MY_CBUF);
	}
	if (strcmp(el, "wpt") == 0) {
		waypt_add(wpt_tmp);
		in_wpt--;
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
	} if (strcmp(el, "groundspeak:type") == 0) {
		in_gs_type--;
	} if (strcmp(el, "groundspeak:difficulty") == 0) {
		in_gs_diff--;
	} if (strcmp(el, "groundspeak:terrain") == 0) {
		in_gs_terr--;
	}
}

static void
gpx_cdata(void *dta, const XML_Char *s, int len)
{
	char *estr;

	/*
	 * I'm exceedingly unamused that libexpat makes me keep all this
	 * horrible state just I can concatenate buffers that it hands
	 * me as a cdata that are fragmented becuae they span a read.  Grrr.
	 */
	if ((in_name && in_wpt) || (in_desc && in_wpt) || (in_ele) || 
			(in_wpt && in_cmt) ||
			(in_wpt && in_gs_type) || 
			(in_wpt && in_gs_diff) || 
			(in_wpt && in_gs_terr) || 
			(in_time && (in_wpt || in_rte)))  {
		estr = cdatastr + strlen(cdatastr);
		memcpy(estr, s, len);
		in_cdata++;
	}
}

void
gpx_rd_init(const char *fname, const char *args)
{
	fd = fopen(fname, "r");
	if (fd == NULL) {
		fatal(MYNAME ": Cannot open %s for reading\n", fname );
	}
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
	fclose(fd);
}

void
gpx_wr_init(const char *fname, const char *args)
{
	ofd = fopen(fname, "w");
	if (ofd == NULL) {
		fatal(MYNAME ": open %s for writing\n", fname );
	}
}

static void
gpx_wr_deinit(void)
{
	fclose(ofd);
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
gpx_write_time(const time_t timep)
{
	struct tm *tm = gmtime(&timep);
	
	fprintf(ofd, "<time>%02d-%02d-%02dT%02d:%02d:%02dZ</time>\n",
		tm->tm_year+1900, 
		tm->tm_mon+1, 
		tm->tm_mday, 
		tm->tm_hour, 
		tm->tm_min, 
		tm->tm_sec
	);

}

static void
gpx_waypt_pr(const waypoint *waypointp)
{
	const char *oname = global_opts.synthesize_shortnames ?
				  mkshort(waypointp->description) : 
				  waypointp->shortname;

	fprintf(ofd, "<wpt lat=\"%lf\" lon=\"%lf\">\n",
		waypointp->position.latitude.degrees,
		waypointp->position.longitude.degrees);
	if (oname) {
		fprintf(ofd, "<name>%s</name>\n", oname);
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
		gpx_write_time(waypointp->creation_time);
	}
	if (waypointp->url) {
		fprintf(ofd, "<url>%s</url>\n", waypointp->url);
	}
	if (waypointp->url_link_text) {
		fprintf(ofd, "<urlname>%s</urlname>\n",
			 waypointp->url_link_text);
	}
	fprintf(ofd, "</wpt>\n");
}

static void
gpx_track_hdr(route_head *rte)
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
		gpx_write_time(waypointp->creation_time);
	}
	if (waypointp->position.altitude.altitude_meters != unknown_alt) {
		fprintf(ofd, "<ele>\n%f\n</ele>\n",
			 waypointp->position.altitude.altitude_meters);
	}
	fprintf(ofd, "</trkpt>\n");
}

static void
gpx_track_tlr(route_hdr *rte)
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
	setshort_length(32);

	fprintf(ofd, "<?xml version=\"1.0\"?>\n");
	fprintf(ofd, "<gpx\n version=\"1.0\"\n");
	fprintf(ofd, "creator=\"GPSBabel - http://gpsbabel.sourceforge.net\"\n");
	fprintf(ofd, "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\n");
	fprintf(ofd, "xmlns=\"http://www.topografix.com/GPX/1/0\"\n");
	fprintf(ofd, "xsi:schemaLocation=\"http://www.topografix.com/GPX/1/0 http://www.topografix.com/GPX/1/0/gpx.xsd\">\n");

	switch(global_opts.objective) {
		case trkdata: gpx_track_pr(); 
		case wptdata: waypt_disp_all(gpx_waypt_pr);
			      break;
		default:
			      break;
	}

	fprintf(ofd, "</gpx>\n");
}

ff_vecs_t gpx_vecs = {
	gpx_rd_init,	
	gpx_wr_init,	
	gpx_rd_deinit,	
	gpx_wr_deinit,	
	gpx_read,
	gpx_write,
};
