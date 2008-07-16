/*
    Access Nokia Landmark Exchange files.

    Copyright (C) 2007  Robert Lipe, robertlipe@gpsbabel.org

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
 * Nokia's Landmark Exchange (LMX) format is a straight-forward XML
 * format.  Though they do support a compact binary representation,
 * we don't implement that at this time in GPSBabel.
 */

#include "defs.h"
#include "xmlgeneric.h"

static gbfile *ofd;
static waypoint *wpt_tmp;
char *urllink, *urllinkt;

#define MYNAME "lmx"

static
arglist_t lmx_args[] = {
	ARG_TERMINATOR
};

/*
 * Writer
 */


static void
lmx_wr_init(const char *fname)
{
        ofd = gbfopen(fname, "w", MYNAME);
}

static void
lmx_wr_deinit(void)
{
        gbfclose(ofd);
}

static void
lmx_write_xml(int indent_level, const char *tag, const char *data)
{
	int i;
	char *tmp_ent = xml_entitize(data);

	for (i = 0; i < indent_level; i++) {
		gbfputs("  ", ofd);
	}

	gbfprintf(ofd, "<%s>%s</%s>\n", tag, tmp_ent, tag);

	xfree(tmp_ent);
}

static void
lmx_print(const waypoint *wpt)
{	
	gbfprintf(ofd, "    <lm:landmark>\n");
	if (wpt->shortname) {
		lmx_write_xml(4, "lm:name", wpt->shortname);
	}
	if (wpt->description) {
		lmx_write_xml(4, "lm:description", wpt->description);
	}
	gbfprintf(ofd, "        <lm:coordinates>\n");
	gbfprintf(ofd, "          <lm:latitude>%f</lm:latitude>\n", wpt->latitude);
	gbfprintf(ofd, "          <lm:longitude>%f</lm:longitude>\n",wpt->longitude);
	if (wpt->altitude && (wpt->altitude != unknown_alt)) {
  		gbfprintf(ofd, "          <lm:altitude>%f</lm:altitude>\n",wpt->altitude);
	}
	gbfprintf(ofd, "        </lm:coordinates>\n");

	if (wpt->url && wpt->url[0]) {
		gbfprintf(ofd, "        <lm:mediaLink>\n");
		if (wpt->url_link_text)
			lmx_write_xml(5,"lm:name", wpt->url_link_text);
		gbfprintf(ofd, "          <lm:url>%s</lm:url>\n", wpt->url);
		gbfprintf(ofd, "        </lm:mediaLink>\n");
	}

	gbfprintf(ofd, "    </lm:landmark>\n");
}


static void
lmx_write(void)
{
	gbfprintf(ofd, "<?xml version=\"1.0\" ?>\n");
	gbfprintf(ofd, "<lm:lmx xmlns:lm=\"http://www.nokia.com/schemas/location/landmarks/1/0\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://www.nokia.com/schemas/location/landmarks/1/0/lmx.xsd\">");

	gbfprintf(ofd, "  <lm:landmarkCollection>\n");
	waypt_disp_all(lmx_print);
	gbfprintf(ofd, "  </lm:landmarkCollection>\n");
	gbfprintf(ofd, "</lm:lmx>\n");
}

/*
 * Reader
 */

static xg_callback	lmx_lm_start, lmx_lm_end;
static xg_callback	lmx_lm_name,lmx_lm_desc;
static xg_callback	lmx_lm_lat, lmx_lm_lon, lmx_lm_alt;
static xg_callback	lmx_lm_mlink_s, lmx_lm_mlink_e;
static xg_callback	lmx_lm_link, lmx_lm_linkt;

static xg_tag_mapping gl_map[] = {
#define LM "/lm:lmx/lm:landmarkCollection/lm:landmark"
 { lmx_lm_start, 	cb_start, 	LM },
 { lmx_lm_end,   	cb_end, 	LM },
 { lmx_lm_name,	 	cb_cdata, 	LM "/lm:name" },
 { lmx_lm_desc,  	cb_cdata, 	LM "/lm:description" },
 { lmx_lm_lat,   	cb_cdata, 	LM "/lm:coordinates/lm:latitude" },
 { lmx_lm_lon, 		cb_cdata, 	LM "/lm:coordinates/lm:longitude" },
 { lmx_lm_alt,		cb_cdata, 	LM "/lm:coordinates/lm:altitude" },
 { lmx_lm_mlink_s,  	cb_start, 	LM "/lm:mediaLink" },
 { lmx_lm_link,  	cb_cdata, 	LM "/lm:mediaLink/lm:url" },
 { lmx_lm_linkt, 	cb_cdata, 	LM "/lm:mediaLink/lm:name" },
 { lmx_lm_mlink_e,	cb_end, 	LM "/lm:mediaLink" },
 { NULL, 	0,         NULL}
};

static void
lmx_rd_init(const char *fname)
{
	xml_init(fname, gl_map, NULL);
}

static void
lmx_read(void)
{
	xml_read();
}

static void
lmx_rd_deinit(void)
{
	xml_deinit();
}



static void
lmx_lm_start(const char *args, const char **unused)
{
	wpt_tmp = waypt_new();
}

static void
lmx_lm_end(const char *args, const char **unused)
{
	waypt_add(wpt_tmp);
}

static void
lmx_lm_lat(const char *args, const char **unused)
{
	wpt_tmp->latitude = atof(args);
}

static void
lmx_lm_lon(const char *args, const char **unused)
{
	wpt_tmp->longitude = atof(args);
}

static void
lmx_lm_alt(const char *args, const char **unused)
{
	wpt_tmp->altitude = atof(args);
}

static void
lmx_lm_name(const char *args, const char **unused)
{
	wpt_tmp->shortname = xstrdup(args);
}

static void	
lmx_lm_desc(const char *args, const char **unused)
{
	wpt_tmp->description = xstrdup(args);
}

static void
lmx_lm_mlink_s(const char *args, const char **unused)
{
	urllink = urllinkt = NULL;
}

static void
lmx_lm_link(const char *args, const char **unused)
{
	urllink = xstrdup(args);
}

static void
lmx_lm_linkt(const char *args, const char **unused)
{
	urllinkt = xstrdup(args);
}

static void
lmx_lm_mlink_e(const char *args, const char **unused)
{
	waypt_add_url(wpt_tmp, urllink, urllinkt);
}


ff_vecs_t lmx_vecs = {
        ff_type_file,
	{ 
	  ff_cap_read | ff_cap_write,	/* waypoints */
	  ff_cap_none,			/* tracks */
	  ff_cap_none			/* routes */
	},
        lmx_rd_init,
        lmx_wr_init,
        lmx_rd_deinit,
        lmx_wr_deinit,
        lmx_read,
        lmx_write,
        NULL,
        lmx_args,
	CET_CHARSET_ASCII, 0	/* CET-REVIEW */
};
