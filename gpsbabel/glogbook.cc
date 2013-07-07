/*
    Access Garmin Logbook (Forerunner/Foretracker) data files.

    Copyright (C) 2004, 2005, 2006, 2007  Robert Lipe, robertlipe@gpsbabel.org

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


#define MYNAME "glogbook"

static
arglist_t glogbook_args[] = {
  ARG_TERMINATOR
};

/* Tracks */
static xg_callback	gl_trk_s;
// static xg_callback	gl_trk_ident;
static xg_callback	gl_trk_pnt_s, gl_trk_pnt_e;
static xg_callback	gl_trk_utc;
static xg_callback	gl_trk_lat;
static xg_callback	gl_trk_long;
static xg_callback	gl_trk_alt;

static xg_tag_mapping gl_map[] = {
  { gl_trk_s,    cb_start, "/History/Run/Track" },
  { gl_trk_pnt_s,cb_start, "/History/Run/Track/Trackpoint/Position" },
  { gl_trk_pnt_e,cb_end,   "/History/Run/Track/Trackpoint/Position" },
  { gl_trk_lat,  cb_cdata, "/History/Run/Track/Trackpoint/Position/Latitude" },
  { gl_trk_long, cb_cdata, "/History/Run/Track/Trackpoint/Position/Longitude" },
  { gl_trk_alt,  cb_cdata, "/History/Run/Track/Trackpoint/Position/Altitude" },
  { gl_trk_utc,  cb_cdata, "/History/Run/Track/Trackpoint/Time" },
  { NULL,	(xg_cb_type)0,         NULL}
};

static void
glogbook_rd_init(const char* fname)
{
  xml_init(fname, gl_map, NULL);
}

static void
glogbook_read(void)
{
  xml_read();
}

static void
glogbook_rd_deinit(void)
{
  xml_deinit();
}

static void
glogbook_wr_init(const char* fname)
{
  ofd = gbfopen(fname, "w", MYNAME);
}

static void
glogbook_wr_deinit(void)
{
  gbfclose(ofd);
}

static void
glogbook_waypt_pr(const waypoint* wpt)
{
  gbfprintf(ofd, "            <Trackpoint>\n");
  gbfprintf(ofd, "                <Position>\n");
  gbfprintf(ofd, "                    <Latitude>%.5f</Latitude>\n", wpt->latitude);
  gbfprintf(ofd, "                    <Longitude>%.5f</Longitude>\n", wpt->longitude);
  if (wpt->altitude != unknown_alt) {
    gbfprintf(ofd, "                    <Altitude>%.3f</Altitude>\n", wpt->altitude);
  }
  gbfprintf(ofd, "                </Position>\n");
  gbfprintf(ofd, "                ");
  QDateTime dt = wpt->GetCreationTime();
  xml_write_time(ofd, dt, "Time");
  gbfprintf(ofd, "            </Trackpoint>\n");
}

static void
glogbook_hdr(const route_head* rte)
{
  gbfprintf(ofd, "        <Track>\n");
}

static void
glogbook_ftr(const route_head* rte)
{
  gbfprintf(ofd, "        </Track>\n");
}

static void
glogbook_write(void)
{
  gbfprintf(ofd, "<?xml version=\"1.0\" ?>\n");
  gbfprintf(ofd, "<History xmlns=\"http://www.garmin.com/xmlschemas/ForerunnerLogbook\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://www.garmin.com/xmlschemas/ForerunnerLogbook http://www.garmin.com/xmlschemas/ForerunnerLogbookv1.xsd\" version=\"1\">\n");
  gbfprintf(ofd, "    <Run>\n");
  track_disp_all(glogbook_hdr, glogbook_ftr, glogbook_waypt_pr);
  gbfprintf(ofd, "    </Run>\n");
  gbfprintf(ofd, "</History>\n");
}

void	gl_trk_s(const char* args, const char** unused)
{
  trk_head = route_head_alloc();
  track_add_head(trk_head);
}
#if 0
void	gl_trk_ident(const char* args, const char** unused)
{
  trk_head->rte_name = xstrdup(args);
}
#endif

void	gl_trk_pnt_s(const char* args, const char** unused)
{
  wpt_tmp = waypt_new();
}

void	gl_trk_pnt_e(const char* args, const char** unused)
{
  track_add_wpt(trk_head, wpt_tmp);
}

void	gl_trk_utc(const char* args, const char** unused)
{
  wpt_tmp->SetCreationTime(xml_parse_time(args));
}

void	gl_trk_lat(const char* args, const char** unused)
{
  wpt_tmp->latitude = atof(args);
}

void	gl_trk_long(const char* args, const char** unused)
{
  wpt_tmp->longitude = atof(args);
}

void	gl_trk_alt(const char* args, const char** unused)
{
  wpt_tmp->altitude = atof(args);
}



ff_vecs_t glogbook_vecs = {
  ff_type_file,
  { ff_cap_none, (ff_cap)(ff_cap_read | ff_cap_write), ff_cap_none},
  glogbook_rd_init,
  glogbook_wr_init,
  glogbook_rd_deinit,
  glogbook_wr_deinit,
  glogbook_read,
  glogbook_write,
  NULL,
  glogbook_args,
  CET_CHARSET_ASCII, 0	/* CET-REVIEW */
};
