/*
    Access Hiketech XML data files.

    Copyright (C) 2004,2005 Robert Lipe, robertlipe@usa.net

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

static FILE *ofd;
static waypoint *wpt_tmp;
static route_head *trk_head;


#define MYNAME "hiketech"

static
arglist_t hiketech_args[] = {
	{0, 0, 0, 0, 0}
};

/* Waypoints */
static xg_callback 	ht_wpt_s;
static xg_callback 	ht_wpt_e;
static xg_callback 	ht_ident;
static xg_callback 	ht_sym;
static xg_callback 	ht_lat;
static xg_callback 	ht_long;
static xg_callback 	ht_alt;

/* Tracks */
static xg_callback	ht_trk_s, ht_trk_e;
static xg_callback	ht_trk_ident;
static xg_callback	ht_trk_pnt_s, ht_trk_pnt_e;
static xg_callback	ht_trk_utc;
static xg_callback	ht_trk_lat;
static xg_callback	ht_trk_long;
static xg_callback	ht_trk_alt;

static xg_tag_mapping ht_map[] = {
	{ ht_wpt_s, 	cb_start, "/hiketech/gpsdata/wpt" },
	{ ht_wpt_e, 	cb_end,   "/hiketech/gpsdata/wpt" },
	{ ht_ident, 	cb_cdata, "/hiketech/gpsdata/wpt/ident" },
	{ ht_sym, 	cb_cdata, "/hiketech/gpsdata/wpt/sym" },
	{ ht_lat, 	cb_cdata, "/hiketech/gpsdata/wpt/lat" },
	{ ht_long, 	cb_cdata, "/hiketech/gpsdata/wpt/long" },
	{ ht_alt, 	cb_cdata, "/hiketech/gpsdata/wpt/alt" },

	{ ht_trk_s, 	cb_start, "/hiketech/gpsdata/trk" },
	{ ht_trk_e, 	cb_end,   "/hiketech/gpsdata/trk" },
	{ ht_trk_ident,	cb_cdata, "/hiketech/gpsdata/trk/ident" },
	{ ht_trk_pnt_s,	cb_start, "/hiketech/gpsdata/trk/pnt" },
	{ ht_trk_pnt_e,	cb_end,   "/hiketech/gpsdata/trk/pnt" },
	{ ht_trk_utc, 	cb_cdata, "/hiketech/gpsdata/trk/pnt/utc" },
	{ ht_trk_lat, 	cb_cdata, "/hiketech/gpsdata/trk/pnt/lat" },
	{ ht_trk_long, 	cb_cdata, "/hiketech/gpsdata/trk/pnt/long" },
	{ ht_trk_alt, 	cb_cdata, "/hiketech/gpsdata/trk/pnt/alt" },
	{ NULL, 	0,         NULL}
};

void
hiketech_rd_init(const char *fname)
{
	xml_init(fname, ht_map);
}

void
hiketech_read(void)
{
	xml_read();
}

void
hiketech_rd_deinit(void)
{
}

void
hiketech_wr_init(const char *fname)
{
        ofd = xfopen(fname, "w", MYNAME);
}

void
hiketech_wr_deinit(void)
{
        fclose(ofd);
}

static void
hiketech_trk_hdr(const route_head *rte)
{
	fprintf(ofd, "<trk>\n");
	write_optional_xml_entity(ofd, " ", "ident", rte->rte_name);
}

static void
hiketech_trk_tlr(const route_head *rte)
{
	fprintf(ofd, "</trk>\n");
}

void
hiketech_print_utc(time_t tm, const char *indent, const char *tag)
{
	char tbuf[80];
        strftime(tbuf, sizeof(tbuf), "%Y-%m-%d %I:%M:%S", gmtime(&tm));
	fprintf(ofd, "%s<%s>%s</%s>\n",indent,tag,tbuf,tag);
}

static void
hiketech_trkpt_pr(const waypoint *waypointp)
{
	fprintf(ofd, " <pnt>\n");
	if (waypointp->creation_time) {
		hiketech_print_utc(waypointp->creation_time, "  ", "utc");
	}
	fprintf(ofd, "  <lat>%f</lat>\n", waypointp->latitude);
	fprintf(ofd, "  <long>%f</long>\n", waypointp->longitude);
	if (waypointp->altitude != unknown_alt) {
		fprintf(ofd, "  <alt>%f</alt>\n",
			 waypointp->altitude);
	}
	fprintf(ofd, " </pnt>\n");
}

static void
hiketech_waypt_pr(const waypoint *wpt)
{	
	fprintf(ofd, "<wpt>\n");
	write_xml_entity(ofd, "\t", "ident", wpt->shortname);
	write_optional_xml_entity(ofd, "\t", "sym", wpt->icon_descr);
	fprintf(ofd, "\t<lat>%f</lat>\n", wpt->latitude);
	fprintf(ofd, "\t<long>%f</long>\n", wpt->longitude);

	/* 
	 * These probably aren't technicallyconstants, but it's all 
	 * we can do for now. 
	 */
	fprintf(ofd, "\t<color>\n\t\t<lbl>FAFFB4</lbl>\n\t\t<obj>FF8000</obj>\n\t</color>\n");
	fprintf(ofd, "</wpt>\n");
}

void
hiketech_write(void)
{
	fprintf(ofd, "<hiketech version=\"1.2\" url=\"http://www.hiketech.com\">\n");
	fprintf(ofd, "<gpsdata>\n");
	track_disp_all(hiketech_trk_hdr, hiketech_trk_tlr, hiketech_trkpt_pr);
	track_disp_all(NULL, NULL, hiketech_trkpt_pr);
	waypt_disp_all(hiketech_waypt_pr);
	fprintf(ofd, "</gpsdata>\n");
	fprintf(ofd, "</hiketech>\n");
}

void	 ht_wpt_s(const char *args, const char **unused)
{
	wpt_tmp = waypt_new();
}

void  	ht_ident(const char *args, const char **unused)
{
	wpt_tmp->shortname = xstrdup(args);
}

void 	ht_sym(const char *args, const char **unused)
{
	wpt_tmp->icon_descr = xstrdup(args);
	wpt_tmp->wpt_flags.icon_descr_is_dynamic = 1;
}

void  	ht_lat(const char *args, const char **unused)
{
	wpt_tmp->latitude = atof(args);
}

void  	ht_long(const char *args, const char **unused)
{
	wpt_tmp->longitude = atof(args);
}

void  	ht_alt(const char *args, const char **unused)
{
	wpt_tmp->altitude = atof(args);
}

void  	ht_wpt_e(const char *args, const char **unused)
{
	waypt_add(wpt_tmp);
	wpt_tmp = NULL;
}

void	ht_trk_s(const char *args, const char **unused)
{
	trk_head = route_head_alloc();
	track_add_head(trk_head);
}

void	ht_trk_e(const char *args, const char **unused)
{

}

void	ht_trk_ident(const char *args, const char **unused)
{
	trk_head->rte_name = xstrdup(args);
}

void	ht_trk_pnt_s(const char *args, const char **unused)
{
	wpt_tmp = waypt_new();
}

void	ht_trk_pnt_e(const char *args, const char **unused)
{
	route_add_wpt(trk_head, wpt_tmp);
}

void	ht_trk_utc(const char *args, const char **unused)
{
	struct tm tm;
	time_t utc;

	sscanf(args, "%d-%d-%d %d:%d:%d",
		&tm.tm_year, &tm.tm_mon,
		&tm.tm_mday, &tm.tm_hour,
		&tm.tm_min, &tm.tm_sec);
        tm.tm_mon -= 1;
        tm.tm_year -= 1900;
        tm.tm_isdst = 0;

        utc = mktime(&tm) + get_tz_offset() ;

	wpt_tmp->creation_time = utc;
}

void	ht_trk_lat(const char *args, const char **unused)
{
	wpt_tmp->latitude = atof(args);
}

void	ht_trk_long(const char *args, const char **unused)
{
	wpt_tmp->longitude = atof(args);
}

void	ht_trk_alt(const char *args, const char **unused)
{
	wpt_tmp->altitude = atof(args);
}



ff_vecs_t hiketech_vecs = {
        ff_type_file,
	{ ff_cap_read | ff_cap_write, ff_cap_read | ff_cap_write },
        hiketech_rd_init,
        hiketech_wr_init,
        hiketech_rd_deinit,
        hiketech_wr_deinit,
        hiketech_read,
        hiketech_write,
        NULL,
        hiketech_args
};

