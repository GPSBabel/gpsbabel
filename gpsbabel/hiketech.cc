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

static gbfile* ofd;
static waypoint* wpt_tmp;
static route_head* trk_head;


#define MYNAME "hiketech"

static
arglist_t hiketech_args[] = {
  ARG_TERMINATOR
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
  { NULL,	(xg_cb_type)0,         NULL}
};

static void
hiketech_rd_init(const char* fname)
{
  xml_init(fname, ht_map, NULL);
}

static void
hiketech_read(void)
{
  xml_read();
}

static void
hiketech_rd_deinit(void)
{
  xml_deinit();
}

static void
hiketech_wr_init(const char* fname)
{
  ofd = gbfopen(fname, "w", MYNAME);
}

static void
hiketech_wr_deinit(void)
{
  gbfclose(ofd);
}

static void
hiketech_trk_hdr(const route_head* rte)
{
  gbfprintf(ofd, "<trk>\n");
  write_optional_xml_entity(ofd, " ", "ident", rte->rte_name);
}

static void
hiketech_trk_tlr(const route_head* rte)
{
  gbfprintf(ofd, "</trk>\n");
}

static void
hiketech_print_utc(time_t tm, const char* indent, const char* tag)
{
  char tbuf[80];
  strftime(tbuf, sizeof(tbuf), "%Y-%m-%d %I:%M:%S", gmtime(&tm));
  gbfprintf(ofd, "%s<%s>%s</%s>\n",indent,tag,tbuf,tag);
}

static void
hiketech_trkpt_pr(const waypoint* waypointp)
{
  gbfprintf(ofd, " <pnt>\n");
  if (waypointp->creation_time) {
    hiketech_print_utc(waypointp->creation_time, "  ", "utc");
  }
  gbfprintf(ofd, "  <lat>%f</lat>\n", waypointp->latitude);
  gbfprintf(ofd, "  <long>%f</long>\n", waypointp->longitude);
  if (waypointp->altitude != unknown_alt) {
    gbfprintf(ofd, "  <alt>%f</alt>\n",
              waypointp->altitude);
  }
  gbfprintf(ofd, " </pnt>\n");
}

static void
hiketech_waypt_pr(const waypoint* wpt)
{
  gbfprintf(ofd, "<wpt>\n");
  write_xml_entity(ofd, "\t", "ident", wpt->shortname);
  write_optional_xml_entity(ofd, "\t", "sym", wpt->icon_descr);
  gbfprintf(ofd, "\t<lat>%f</lat>\n", wpt->latitude);
  gbfprintf(ofd, "\t<long>%f</long>\n", wpt->longitude);

  /*
   * These probably aren't technicallyconstants, but it's all
   * we can do for now.
   */
  gbfprintf(ofd, "\t<color>\n\t\t<lbl>FAFFB4</lbl>\n\t\t<obj>FF8000</obj>\n\t</color>\n");
  gbfprintf(ofd, "</wpt>\n");
}

static void
hiketech_write(void)
{
  gbfprintf(ofd, "<hiketech version=\"1.2\" url=\"http://www.hiketech.com\">\n");
  gbfprintf(ofd, "<gpsdata>\n");
  track_disp_all(hiketech_trk_hdr, hiketech_trk_tlr, hiketech_trkpt_pr);
  track_disp_all(NULL, NULL, hiketech_trkpt_pr);
  waypt_disp_all(hiketech_waypt_pr);
  gbfprintf(ofd, "</gpsdata>\n");
  gbfprintf(ofd, "</hiketech>\n");
}

static
void	 ht_wpt_s(const char* args, const char** unused)
{
  wpt_tmp = waypt_new();
}

static
void  	ht_ident(const char* args, const char** unused)
{
  wpt_tmp->shortname = xstrdup(args);
}

static
void 	ht_sym(const char* args, const char** unused)
{
  wpt_tmp->icon_descr = args;
}

static
void  	ht_lat(const char* args, const char** unused)
{
  wpt_tmp->latitude = atof(args);
}

static
void  	ht_long(const char* args, const char** unused)
{
  wpt_tmp->longitude = atof(args);
}

static
void  	ht_alt(const char* args, const char** unused)
{
  wpt_tmp->altitude = atof(args);
}

static
void  	ht_wpt_e(const char* args, const char** unused)
{
  waypt_add(wpt_tmp);
  wpt_tmp = NULL;
}

static
void	ht_trk_s(const char* args, const char** unused)
{
  trk_head = route_head_alloc();
  track_add_head(trk_head);
}

static
void	ht_trk_e(const char* args, const char** unused)
{

}

static
void	ht_trk_ident(const char* args, const char** unused)
{
  trk_head->rte_name = xstrdup(args);
}

static
void	ht_trk_pnt_s(const char* args, const char** unused)
{
  wpt_tmp = waypt_new();
}

static
void	ht_trk_pnt_e(const char* args, const char** unused)
{
  track_add_wpt(trk_head, wpt_tmp);
}

static
void	ht_trk_utc(const char* args, const char** unused)
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

  utc = mkgmtime(&tm);

  wpt_tmp->creation_time = utc;
}

static
void	ht_trk_lat(const char* args, const char** unused)
{
  wpt_tmp->latitude = atof(args);
}

static
void	ht_trk_long(const char* args, const char** unused)
{
  wpt_tmp->longitude = atof(args);
}

static
void	ht_trk_alt(const char* args, const char** unused)
{
  wpt_tmp->altitude = atof(args);
}



ff_vecs_t hiketech_vecs = {
  ff_type_file,
  { (ff_cap)(ff_cap_read | ff_cap_write), (ff_cap)(ff_cap_read | ff_cap_write) },
  hiketech_rd_init,
  hiketech_wr_init,
  hiketech_rd_deinit,
  hiketech_wr_deinit,
  hiketech_read,
  hiketech_write,
  NULL,
  hiketech_args,
  CET_CHARSET_ASCII, 0	/* CET-REVIEW */
};

