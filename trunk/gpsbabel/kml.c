/* 
    Support for Keyhole "kml" format.

    STATUS: 03/15/2005 - This file is kind of sketchy and was based
	on examining sample KML data files found on the net.   I do now
	have the full formal  spec from Keyhole, but have not revisited
	this file with that information yet.   I'm checking it in becuase
	there have been a couple of requests for KML support.   RJL


    Copyright (C) 2005 Robert Lipe, robertlipe@usa.net

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
arglist_t kml_args[] = {
	{"deficon", &deficon, "Default icon name", NULL, ARGTYPE_STRING },
	{0, 0, 0, 0, 0}
};

#define MYNAME "kml"

#if NO_EXPAT
void
kml_rd_init(const char *fname)
{
	fatal(MYNAME ": This build excluded KML support because expat was not installed.\n");
}

void
kml_read(void)
{
}
#else

static xg_callback	wpt_s, wpt_e;
static xg_callback	wpt_name, wpt_desc, wpt_coord;

static 
xg_tag_mapping kml_map[] = {
	{ wpt_s, 	cb_start, 	"/Folder/Placemark" },
	{ wpt_e, 	cb_end, 	"/Folder/Placemark" },
//	{ wpt_name_s, 	cb_start, 	"/Folder/Placemark/name" },
	{ wpt_name, 	cb_cdata, 	"/Folder/Placemark/name" },
	{ wpt_desc, 	cb_cdata, 	"/Folder/Placemark/description" },
//	{ wpt_type, 	cb_cdata, 	"/Folder/Placemark/type" },
//	{ wpt_link_s, 	cb_start, 	"/Folder/Placemark/link" },
//	{ wpt_link, 	cb_cdata, 	"/Folder/Placemark/link" },
	{ wpt_coord, 	cb_cdata, 	"/Folder/Placemark/Point/coordinates" },
	{ NULL, 	0, 		NULL }
};

void wpt_s(const char *args, const char **unused) 
{ 
	wpt_tmp = waypt_new();
//	wpt_tmp = xcalloc(sizeof(*wpt_tmp), 1);
}

void wpt_e(const char *args, const char **unused)
{
	waypt_add(wpt_tmp);
}

#if 0
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
#endif

void wpt_name(const char *args, const char **unused)
{
	if (args) wpt_tmp->shortname = xstrdup(args);
}

void wpt_desc(const char *args, const char **unused)
{
	if (args) wpt_tmp->description = xstrappend(wpt_tmp->description, args);
}

void wpt_coord(const char *args, const char **attrv)
{
	sscanf(args, "%lf,%lf,%lf", &wpt_tmp->longitude, &wpt_tmp->latitude, &wpt_tmp->altitude);
}

static 
void
kml_rd_init(const char *fname)
{
	xml_init(fname, kml_map);
}

static
void
kml_read(void)
{
	xml_read();
}
#endif

void
kml_rd_deinit(void)
{
	xml_deinit();
}

void
kml_wr_init(const char *fname)
{
	ofd = xfopen(fname, "w", MYNAME);
}

void
kml_wr_deinit(void)
{
	fclose(ofd);
}

static void
kml_waypt_pr(const waypoint *waypointp)
{
	fputs("    <Placemark>\n", ofd);
//	write_optional_xml_entity(ofd, "\t", "name", waypointp->shortname);
	write_optional_xml_entity(ofd, "\t", "name", waypointp->description);

	fprintf(ofd, "\t<description>");
#if 0
        if (waypointp->description) {
		char *odesc = xml_entitize(waypointp->description);
		fputs(odesc, ofd);
		xfree(odesc);
	}
#endif
	if (waypointp->url) {
		char * odesc = xml_entitize(waypointp->url);
		fputs("\n", ofd);
		fputs(odesc, ofd);
		xfree(odesc);
	}
	fprintf(ofd, "\n\t</description>\n");
//	write_optional_xml_entity(ofd, "\t", "description", waypointp->description);	
	fprintf(ofd, "\t<styleUrl>root://styleMaps#default?iconId=0x400</styleUrl>\n");
	fprintf(ofd, "\t<Point>\n");
	fprintf(ofd, "\t\t<coordinates>%f,%f,%f</coordinates>\n",
		waypointp->longitude, waypointp->latitude, waypointp->altitude == unknown_alt ? 0.0 : waypointp->altitude);
	fprintf(ofd, "\t</Point>\n");
#if 0
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
#endif
	fprintf(ofd, "    </Placemark>\n");
}

void
kml_write(void)
{
	fprintf(ofd, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
	fprintf(ofd, "<Folder>\n");
	waypt_disp_all(kml_waypt_pr);
	fprintf(ofd, "</Folder>\n");
}

ff_vecs_t kml_vecs = {
	ff_type_file,
	FF_CAP_RW_WPT, /* Format can do RW_ALL */
	kml_rd_init,	
	kml_wr_init,	
	kml_rd_deinit,
	kml_wr_deinit,
	kml_read,
	kml_write,
	NULL, 
	kml_args
};
