/* 
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
static int in_name;
static int in_link;
static int in_cdata;
static char *cdatastr;

static XML_Parser psr;
static waypoint *wpt_tmp;

FILE *fd;
FILE *ofd;

#define MYNAME "geo"
#define MY_CBUF 4096

static void
tag_coord(const char **attrv)
{
	const char **avp = &attrv[0];


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
tag_name(const char **attrv)
{
	const char **avp = &attrv[0];
	while (*avp) { 
		if (strcmp(avp[0], "id") == 0) {
			wpt_tmp->shortname = xstrdup(avp[1]);
		}
		avp+=2;
	}
}

static void
tag_link(const char **attrv)
{
	const char **avp = &attrv[0];
	while (*avp) { 
		if (strcmp(avp[0], "text") == 0) {
			wpt_tmp->url_link_text = xstrdup(avp[1]);
		}
		avp+=2;
	}
}

static void
geo_start(void *data, const char *el, const char **attr)
{

	if (in_wpt) {
		if (strcmp(el, "ele") == 0) {
			wpt_tmp->position.altitude.altitude_meters = atoi(attr[1]);
		}
		else if (strcmp(el, "name") == 0) {
			tag_name(attr);
		}
		else if (strcmp(el, "coord") == 0) {
			tag_coord(attr);
		}
	}

	if (strcmp(el, "waypoint") == 0) {
		wpt_tmp = xcalloc(sizeof(*wpt_tmp), 1);
		in_wpt++;
	} else if (strcmp(el, "name") == 0) {
		in_name++;
	} else if (strcmp(el, "link") == 0) {
		tag_link(attr);
		in_link++;
	}
}

static void
geo_end(void *data, const char *el)
{
	if (in_cdata) {
		if (in_name) {
			wpt_tmp->description = xstrdup(cdatastr);
		}
		if (in_link) {
			wpt_tmp->url = xstrdup(cdatastr);
		}
		in_cdata--;
		memset(cdatastr,0, MY_CBUF);
	}
	if (strcmp(el, "waypoint") == 0) {
		waypt_add(wpt_tmp);
		in_wpt--;
	}
	else if (strcmp(el, "name") == 0) {
		in_name--;
	}
	else if (strcmp(el, "link") == 0) {
		in_link--;
	}
}

static void
geo_cdata(void *dta, const XML_Char *s, int len)
{
	char *estr;
	if (in_name || in_link) {
		estr = cdatastr + strlen(cdatastr);
		memcpy(estr, s, len); 
		in_cdata++;
	}
}

void
geo_rd_init(const char *fname, const char *args)
{
	fd = fopen(fname, "r");
	if (fd == NULL) {
		fatal(MYNAME ":Cannot open %s for reading\n", fname);
	}

	psr = XML_ParserCreate(NULL);
	if (!psr) {
		fatal(MYNAME ":Cannot create XML parser\n");
	}

	XML_SetElementHandler(psr, geo_start, geo_end);
	cdatastr = xcalloc(MY_CBUF,1);
	XML_SetCharacterDataHandler(psr, geo_cdata);
}

void
geo_rd_deinit(void)
{
	if ( cdatastr ) {
		xfree(cdatastr);
	}
	fclose(fd);
}

void
geo_wr_init(const char *fname, const char *args)
{
	ofd = fopen(fname, "w");
	if (ofd == NULL) {
		fatal(MYNAME ":Cannot open '%s' for writing\n", fname);
	}
}

void
geo_wr_deinit(void)
{
	fclose(ofd);
}

void
geo_read(void)
{
	int len;
	char buf[MY_CBUF];
	
	while ((len = fread(buf, 1, sizeof(buf), fd))) {
		if (!XML_Parse(psr, buf, len, feof(fd))) {
			fatal(MYNAME ":Parse error at %d: %s\n", 
				XML_GetCurrentLineNumber(psr),
				XML_ErrorString(XML_GetErrorCode(psr)));
		}
	}

	XML_ParserFree(psr);
}

static void
geo_waypt_pr(const waypoint *waypointp)
{
	fprintf(ofd, "<waypoint>\n");
	fprintf(ofd, "<name id=\"%s\">", waypointp->shortname);
	fprintf(ofd, "<![CDATA[%s]]>", waypointp->description);
	fprintf(ofd, "</name>\n");

	fprintf(ofd, "<coord lat=\"%lf\" lon=\"%lf\"/>",
		waypointp->position.latitude.degrees,
		waypointp->position.longitude.degrees);
	fprintf(ofd, "\n");

	if (waypointp->url) {
		fprintf(ofd, "<link text =\"Cache Details\">%s</link>\n", 
			waypointp->url);
	}
	fprintf(ofd, "</waypoint>\n");
}

void
geo_write(void)
{
	fprintf(ofd, "<?xml version=\"1.0\"?><loc version=\"1.0\" src=\"EasyGPS\">\n");
	waypt_disp_all(geo_waypt_pr);
	fprintf(ofd, "</loc>\n");
}

ff_vecs_t geo_vecs = {
	geo_rd_init,	
	geo_wr_init,	
	geo_rd_deinit,
	geo_wr_deinit,
	geo_read,
	geo_write,
};
