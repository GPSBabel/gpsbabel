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
static char *nuke_placer;

static waypoint *wpt_tmp;

static gbfile *ofd;

static
arglist_t geo_args[] = {
	{"deficon", &deficon, "Default icon name", NULL, ARGTYPE_STRING, ARG_NOMINMAX },
	{"nuke_placer", &nuke_placer, "Omit Placer name", NULL, ARGTYPE_BOOL, ARG_NOMINMAX },
	ARG_TERMINATOR
};

#define MYNAME "geo"
#define MY_CBUF 4096

#if ! HAVE_LIBEXPAT
static void
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
static xg_callback	wpt_diff, wpt_terr, wpt_container;

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
	{ wpt_diff, 	cb_cdata, 	"/loc/waypoint/difficulty" },
	{ wpt_terr, 	cb_cdata, 	"/loc/waypoint/terrain" },
	{ wpt_container,cb_cdata, 	"/loc/waypoint/container" },
	{ NULL, 	0, 		NULL }
};

void wpt_s(const char *args, const char **unused) 
{ 
	wpt_tmp = waypt_new();
	/*
 	 * 'geo' doesn't really have an 'unknown' and doesn't have any
	 * concept of alt.  Unfortunately, we have many reference files
	 * that have leaked the 'unknown_alt' value into them, so we paper
	 * over that here.
	 */
	wpt_tmp->altitude = 0;
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
	char *s;
	if (!args) return;

	wpt_tmp->description = xstrappend(wpt_tmp->description,args);
	s = xstrrstr(wpt_tmp->description, " by ");
	if (s) {
		waypt_alloc_gc_data(wpt_tmp)->placer = xstrdup(s + 4);

		if (nuke_placer) {
			*s = '\0';
		}
	}
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
	wpt_tmp->wpt_flags.icon_descr_is_dynamic = 1;
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

void wpt_container(const char *args, const char **unused)
{
	int v;

	if (!args) return;
	switch ( atoi(args) )
	{
	case 1: v = gc_unknown; break;
	case 2: v = gc_micro; break;
	case 3: v = gc_regular; break;
	case 4: v = gc_large; break;
	case 5: v = gc_virtual;; break;
	case 6: v = gc_other; break;
	case 8: v = gc_small; break;
	default: v = gc_unknown; break;
	}
	waypt_alloc_gc_data(wpt_tmp)->container = v;
}

void wpt_diff(const char *args, const char **unused)
{
	if (!args) return;
	waypt_alloc_gc_data(wpt_tmp)->diff = atof(args) * 10;
}

void wpt_terr(const char *args, const char **unused)
{
	if (!args) return;
	waypt_alloc_gc_data(wpt_tmp)->terr = atof(args) * 10;
}

static void
geo_rd_init(const char *fname)
{
	xml_init(fname, loc_map, NULL);
}

static void
geo_read(void)
{
	xml_read();
}
#endif

static void
geo_rd_deinit(void)
{
	xml_deinit();
}

static void
geo_wr_init(const char *fname)
{
	ofd = gbfopen(fname, "w", MYNAME);
}

static void
geo_wr_deinit(void)
{
	gbfclose(ofd);
}

static void
geo_waypt_pr(const waypoint *waypointp)
{
	char *tmp;

	gbfprintf(ofd, "<waypoint>\n");
	gbfprintf(ofd, "<name id=\"%s\">", waypointp->shortname);
	gbfprintf(ofd, "<![CDATA[%s]]>", waypointp->description);
	gbfprintf(ofd, "</name>\n");

	gbfprintf(ofd, "<coord lat=\"%lf\" lon=\"%lf\"/>",
		waypointp->latitude,
		waypointp->longitude);
	gbfprintf(ofd, "\n");

	if (waypointp->icon_descr) {
		gbfprintf(ofd, "<type>%s</type>\n", deficon ? deficon : waypointp->icon_descr);
	}
	if (waypointp->url) {
		tmp = xml_entitize(waypointp->url);
		gbfprintf(ofd, "<link text =\"Cache Details\">%s</link>\n", 
			tmp);
		xfree(tmp);
	}
	if (waypointp->gc_data && waypointp->gc_data->diff)
	{
		int v;

		gbfprintf(ofd, "<difficulty>%.1lf</difficulty>\n",
			waypointp->gc_data->diff / 10.0);
		gbfprintf(ofd, "<terrain>%.1lf</terrain>\n",
			waypointp->gc_data->terr / 10.0);
		switch (waypointp->gc_data->container)
		{
		case gc_unknown: v = 1; break;
		case gc_micro: v = 2; break;
		case gc_regular: v = 3; break;
		case gc_large: v = 4; break;
		case gc_virtual: v = 5; break;
		case gc_other: v = 6; break;
		case gc_small: v = 8; break;
		default: v = 1; break;
		}
		gbfprintf(ofd, "<container>%d</container>\n", v);
	}
	gbfprintf(ofd, "</waypoint>\n");
}

static void
geo_write(void)
{
	gbfprintf(ofd, "<?xml version=\"1.0\"?><loc version=\"1.0\" src=\"EasyGPS\">\n");
	waypt_disp_all(geo_waypt_pr);
	gbfprintf(ofd, "</loc>\n");
}

ff_vecs_t geo_vecs = {
	ff_type_file,
	{ ff_cap_read | ff_cap_write, ff_cap_none, ff_cap_none },
	geo_rd_init,	
	geo_wr_init,	
	geo_rd_deinit,
	geo_wr_deinit,
	geo_read,
	geo_write,
	NULL, 
	geo_args,
	CET_CHARSET_UTF8, 0	/* CET-REVIEW */
};
