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
#include "xmlgeneric.h"

static char *deficon = NULL;

static waypoint *wpt_tmp;

FILE *fd;
FILE *ofd;

static
arglist_t geo_args[] = {
	{"deficon", &deficon, "Default icon name", NULL, ARGTYPE_STRING },
	{0, 0, 0, 0, 0}
};

#define MYNAME "geo"
#define MY_CBUF 4096

#if NO_EXPAT
void
geo_rd_init(const char *fname)
{
	fatal(MYNAME ": This build excluded GEO support because expat was not installed.\n");
}

void
geo_read(void)
{
}
#else

static xg_callback	wpt_s, wpt_e;
static xg_callback	wpt_link_s, wpt_link;
static xg_callback	wpt_name, wpt_name_s, wpt_type, wpt_coord;

static 
xg_tag_mapping loc_map[] = {
	{ wpt_s, 	cb_start, 	"/loc/waypoint" },
	{ wpt_e, 	cb_end, 	"/loc/waypoint" },
	{ wpt_name_s, 	cb_start, 	"/loc/waypoint/name" },
	{ wpt_name, 	cb_cdata, 	"/loc/waypoint/name" },
	{ wpt_type, 	cb_cdata, 	"/loc/waypoint/type" },
	{ wpt_link_s, 	cb_start, 	"/loc/waypoint/link" },
	{ wpt_link, 	cb_cdata, 	"/loc/waypoint/link" },
	{ wpt_coord, 	cb_start, 	"/loc/waypoint/coord" },
	{ NULL, 	0, 		NULL }
};

void wpt_s(const char *args, const char **unused) 
{ 
//	wpt_tmp = waypt_new();
	wpt_tmp = xcalloc(sizeof(*wpt_tmp), 1);
}

void wpt_e(const char *args, const char **unused)
{
	waypt_add(wpt_tmp);
}

void wpt_name_s(const char *args, const char **attrv)
{
        const char **avp = &attrv[0];
        while (*avp) {
                if (0 == strcmp(avp[0], "id")) {
                        wpt_tmp->shortname = xstrdup(avp[1]);
                }
                avp+=2;
        }
}

void wpt_name(const char *args, const char **unused)
{
	if (args) wpt_tmp->description = xstrappend(wpt_tmp->description,args);
}

void wpt_link_s(const char *args, const char **attrv)
{
        const char **avp = &attrv[0];
        while (*avp) {
                if (0 == strcmp(avp[0], "text")) {
                        wpt_tmp->url_link_text = xstrdup(avp[1]);
                }
                avp+=2;
        }
}
void wpt_link(const char *args, const char **attrv)
{
	wpt_tmp->url = xstrdup(args);
}

void wpt_type(const char *args, const char **unused)
{
	wpt_tmp->icon_descr_is_dynamic = 1;
	wpt_tmp->icon_descr = xstrdup(args);
}

void wpt_coord(const char *args, const char **attrv)
{
        const char **avp = &attrv[0];

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

void
geo_rd_init(const char *fname)
{
	xml_init(fname, loc_map);
}

void
geo_read(void)
{
	xml_read();
}
#endif

void
geo_rd_deinit(void)
{
	xml_deinit();
}

void
geo_wr_init(const char *fname)
{
	ofd = xfopen(fname, "w", MYNAME);
}

void
geo_wr_deinit(void)
{
	fclose(ofd);
}

static void
geo_waypt_pr(const waypoint *waypointp)
{
	char *tmp;

	fprintf(ofd, "<waypoint>\n");
	fprintf(ofd, "<name id=\"%s\">", waypointp->shortname);
	fprintf(ofd, "<![CDATA[%s]]>", waypointp->description);
	fprintf(ofd, "</name>\n");

	fprintf(ofd, "<coord lat=\"%lf\" lon=\"%lf\"/>",
		waypointp->latitude,
		waypointp->longitude);
	fprintf(ofd, "\n");

	if (waypointp->icon_descr) {
		fprintf(ofd, "<type>%s</type>\n", deficon ? deficon : waypointp->icon_descr);
	}
	if (waypointp->url) {
		tmp = xml_entitize(waypointp->url);
		fprintf(ofd, "<link text =\"Cache Details\">%s</link>\n", 
			tmp);
		xfree(tmp);
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
	ff_type_file,
	FF_CAP_RW_WPT,
	geo_rd_init,	
	geo_wr_init,	
	geo_rd_deinit,
	geo_wr_deinit,
	geo_read,
	geo_write,
	NULL, 
	geo_args
};
