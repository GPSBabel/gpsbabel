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

#include <expat.h>
#include "defs.h"

static int in_wpt;
static int in_rte;
static int in_ele;
static int in_name;
static int in_time;
static int in_desc;

static XML_Parser psr;

static const char *gpx_version;
static const char *gpx_creator;

static waypoint *wpt_tmp;
static FILE *fd;
static FILE *ofd;

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

	wpt_tmp = calloc(sizeof(*wpt_tmp), 1);
	if (wpt_tmp == NULL) {
		fatal("Can not allocate memory\n");
	}

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
	} if (strcmp(el, "rtept") == 0) {
		in_rte++;
		tag_wpt(attr);
	} if (strcmp(el, "time") == 0) {
		in_time++;
	}
}

static void
gpx_end(void *data, const char *el)
{
	if (strcmp(el, "wpt") == 0) {
		waypt_add(wpt_tmp);
		in_wpt--;
	}
	else if (strcmp(el, "rtept") == 0) {
		route_add(wpt_tmp);
		in_rte--;
	} else if (strcmp(el, "name") == 0) {
		in_name--;
	} else if (strcmp(el, "desc") == 0) {
		in_desc--;
	} else if (strcmp(el, "ele") == 0) {
		in_ele--;
	} else if (strcmp(el, "time") == 0) {
		in_time--;
	}
}

static void
gpx_cdata(void *dta, const XML_Char *s, int len)
{
	char *foo = malloc(len+1);
	foo[len] = 0;
	strncpy(foo, s, len);
	if (in_name && in_wpt) {
		wpt_tmp->shortname = foo;
	}
	if (in_desc && in_wpt) {
		wpt_tmp->description = foo;
	}
	if (in_ele) {
		sscanf(foo, "%lf", 
			&wpt_tmp->position.altitude.altitude_meters);
	}
	if (in_time && (in_wpt || in_rte)) {
		struct tm tm;
		sscanf(foo, "%d-%d-%dT%d:%d:%dZ\n", 
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
}

void
gpx_rd_init(const char *fname)
{
	fd = fopen(fname, "r");
	if (fd == NULL) {
		fatal("GPX: Cannot open %s for reading\n", fname );
	}
	psr = XML_ParserCreate(NULL);
	if (!psr) {
		fatal("GPX: Cannot create XML Parser\n");
	}
	XML_SetElementHandler(psr, gpx_start, gpx_end);
	XML_SetCharacterDataHandler(psr, gpx_cdata);
}

static void
gpx_rd_deinit(void)
{
	fclose(fd);
}

void
gpx_wr_init(const char *fname)
{
	ofd = fopen(fname, "w");
	if (ofd == NULL) {
		fatal("GPX: Cannot open %s for writing\n", fname );
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
	char buf[102400];

	while (!done) {
		len = fread(buf, 1, sizeof(buf), fd);
		done = feof(fd); 
		if (!XML_Parse(psr, buf, len, done)) {
			fatal("GPX: XML parse error at %d: %s\n", 
				XML_GetCurrentLineNumber(psr),
				XML_ErrorString(XML_GetErrorCode(psr)));
		}
	}
}

static void
gpx_waypt_pr(waypoint *waypointp)
{

	fprintf(ofd, "<wpt lat=\"%lf\" lon=\"%lf\">\n",
		waypointp->position.latitude.degrees,
		waypointp->position.longitude.degrees);
	fprintf(ofd, "<name>%s</name>\n", waypointp->shortname);
	fprintf(ofd, "<desc>");
	fprintf(ofd, "<![CDATA[%s]]>", waypointp->description);
	fprintf(ofd, "</desc>\n");
	if (waypointp->position.altitude.altitude_meters) {
		fprintf(ofd, "<ele>\n%f\n</ele>\n",
			 waypointp->position.altitude.altitude_meters);
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

void
gpx_write(void)
{
	fprintf(ofd, "<?xml version=\"1.0\"?>\n");
	fprintf(ofd, "<gpx\n\tversion=\"1.0\">\n");
	waypt_disp_all(gpx_waypt_pr);
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
