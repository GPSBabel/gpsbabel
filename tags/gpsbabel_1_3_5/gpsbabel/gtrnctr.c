/*
    Access Garmin Training Center (Forerunner/Foretracker/Edge) data files.

    Copyright (C) 2006, 2007 Robert Lipe, robertlipe@usa.net

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

static gbfile *ofd;
static waypoint *wpt_tmp;
static route_head *trk_head;

#define MYNAME "gtc"

static
arglist_t gtc_args[] = {
	ARG_TERMINATOR
};
#if 0
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
 { NULL, 	0,         NULL}
};
#endif

static void
gtc_rd_init(const char *fname)
{
	fatal(MYNAME ": this format does not support reading.\n");
}

#if 0
static void
gtc_read(void)
{
	xml_read();
}

static void
gtc_rd_deinit(void)
{
	xml_deinit();
}
#endif

static void
gtc_wr_init(const char *fname)
{
        ofd = gbfopen(fname, "w", MYNAME);
}

static void
gtc_wr_deinit(void)
{
        gbfclose(ofd);
}

static int gtc_indent_level;
static void 
gtc_write_xml(int indent, const char *fmt, ...)
{
	va_list args;

	va_start(args, fmt);

	if (indent < 0) gtc_indent_level--;

	gbfprintf(ofd, "%*s", gtc_indent_level * 2, "");
	gbvfprintf(ofd, fmt, args);

	if (indent > 0) gtc_indent_level++;

	va_end(args);
}

static void
gtc_waypt_pr(const waypoint *wpt)
{	
#if 0
	gbfprintf(ofd, "            <Trackpoint>\n");
	gbfprintf(ofd, "                <Position>\n");
	gbfprintf(ofd, "                    <Latitude>%.5f</Latitude>\n", wpt->latitude);
	gbfprintf(ofd, "                    <Longitude>%.5f</Longitude>\n", wpt->longitude);
	if (wpt->altitude != unknown_alt) {
		gbfprintf(ofd, "                    <Altitude>%.3f</Altitude>\n", wpt->altitude);
	}
	gbfprintf(ofd, "                </Position>\n");
	gbfprintf(ofd, "                ");
	xml_write_time(ofd, wpt->creation_time, "Time");
	gbfprintf(ofd, "            </Trackpoint>\n");
#else
	gtc_write_xml(1, "<Trackpoint>\n");
	if (wpt->creation_time) {
		char time_string[100];
		xml_fill_in_time(time_string, wpt->creation_time, wpt->microseconds,
			XML_LONG_TIME);
		if (time_string[0]) {
			gtc_write_xml(0, "<Time>%s</Time>\n", 
				time_string);
		}
	}
	gtc_write_xml(1, "<Position>\n");
	gtc_write_xml(0, "<LatitudeDegrees>%f</LatitudeDegrees>\n", wpt->latitude);
	gtc_write_xml(0, "<LongitudeDegrees>%f</LongitudeDegrees>\n", wpt->longitude);
	gtc_write_xml(-1, "</Position>\n");
	if (wpt->altitude != unknown_alt) {
		gtc_write_xml(0, "<AltitudeMeters>%f</AltitudeMeters>\n", wpt->altitude);
	}
	if (wpt->heartrate) {
		gtc_write_xml(0, "<HeartRateBpm>%d</HeartRateBpm>\n", wpt->heartrate);
	}
	if (wpt->cadence) {
		gtc_write_xml(0, "<Cadence>%d</Cadence>\n", wpt->cadence);
	}


	gtc_write_xml(-1, "</Trackpoint>\n");
#endif
}

static void
gtc_hdr( const route_head *rte)
{
	gtc_write_xml(1,"<Track>\n");
}

static void
gtc_ftr(const route_head *rte)
{
	gtc_write_xml(-1,"</Track>\n");
}

static time_t gtc_least_time;
static time_t gtc_most_time;

static void
gtc_lap_start(const route_head *rte)
{
	gtc_least_time = 0;
	gtc_most_time = 0;
}

static void
gtc_study_lap(const waypoint *wpt)
{
	if (wpt->creation_time && (gtc_least_time == 0)) 
		gtc_least_time = wpt->creation_time;

	if (wpt->creation_time && (gtc_least_time > wpt->creation_time))
		gtc_least_time =  wpt->creation_time;

	if (wpt->creation_time > gtc_most_time) 
		gtc_most_time = wpt->creation_time;
}

static void
gtc_fake_hdr(void)
{
	long secs = 0;
	if (gtc_least_time && gtc_most_time) {
		secs = gtc_most_time - gtc_least_time;
	}
	gtc_write_xml(0, "<TotalTimeSeconds>%d</TotalTimeSeconds>\n", secs);
	gtc_write_xml(0, "<DistanceMeters>0</DistanceMeters>\n");
	gtc_write_xml(0, "<Calories>0</Calories>\n");
	gtc_write_xml(0, "<AverageHeartRateBpm>0</AverageHeartRateBpm>\n");
	gtc_write_xml(0, "<MaximumHeartRateBpm>0</MaximumHeartRateBpm>\n");
	gtc_write_xml(0, "<Intensity>Active</Intensity>\n");
	gtc_write_xml(0, "<TriggerMethod>Manual</TriggerMethod>\n");
}

void
gtc_write(void)
{
#if 0
	gbfprintf(ofd, "<?xml version=\"1.0\" ?>\n");
	gbfprintf(ofd, "<History xmlns=\"http://www.garmin.com/xmlschemas/ForerunnerLogbook\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://www.garmin.com/xmlschemas/ForerunnerLogbook http://www.garmin.com/xmlschemas/ForerunnerLogbookv1.xsd\" version=\"1\">\n");
	gbfprintf(ofd, "    <Run>\n");
	track_disp_all(gtc_hdr, gtc_ftr, gtc_waypt_pr);
	gbfprintf(ofd, "    </Run>\n");
	gbfprintf(ofd, "</History>\n");
#else
	gtc_write_xml(0, "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\" ?>\n");
	gtc_write_xml(1, "<TrainingCenterDatabase\nxmlns=\"http://www.garmin.com/xmlschemas/TrainingCenterDatabase/v1\"\nxmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\nxsi:schemaLocation=\"http://www.garmin.com/xmlschemas/TrainingCenterDatabase/v1\nhttp://www.garmin.com/xmlschemas/TrainingCenterDatabasev1.xsd\">\n");
	gtc_write_xml(1, "<History>\n");

	gtc_write_xml(1, "<Running>\n");
	gtc_write_xml(1, "<Run>\n");

	gtc_lap_start(NULL);
	track_disp_all(NULL, NULL, gtc_study_lap);

	if (gtc_least_time) {
		char time_string[100];
		xml_fill_in_time(time_string, gtc_least_time, 0, XML_LONG_TIME);
		gtc_write_xml(1, "<Lap StartTime=\"%s\">\n", time_string);
	} else {
		gtc_write_xml(1, "<Lap>\n");
	}
	gtc_fake_hdr();
	track_disp_all(gtc_hdr, gtc_ftr, gtc_waypt_pr);
	gtc_write_xml(-1, "</Lap>\n");
	gtc_write_xml(-1, "</Run>\n");
	gtc_write_xml(-1, "</Running>\n");
	gtc_write_xml(0, "<Biking />\n");
	gtc_write_xml(0, "<Other />\n");
	gtc_write_xml(0, "<MultiSport />\n");

	gtc_write_xml(-1, "</History>\n");
	gtc_write_xml(-1, "</TrainingCenterDatabase>\n");

#endif
}

void	gl_trk_s(const char *args, const char **unused)
{
	trk_head = route_head_alloc();
	track_add_head(trk_head);
}
#if 0
void	gl_trk_ident(const char *args, const char **unused)
{
	trk_head->rte_name = xstrdup(args);
}
#endif

void	gl_trk_pnt_s(const char *args, const char **unused)
{
	wpt_tmp = waypt_new();
}

void	gl_trk_pnt_e(const char *args, const char **unused)
{
	track_add_wpt(trk_head, wpt_tmp);
}

void	gl_trk_utc(const char *args, const char **unused)
{
	wpt_tmp->creation_time = xml_parse_time(args, NULL);
}

void	gl_trk_lat(const char *args, const char **unused)
{
	wpt_tmp->latitude = atof(args);
}

void	gl_trk_long(const char *args, const char **unused)
{
	wpt_tmp->longitude = atof(args);
}

void	gl_trk_alt(const char *args, const char **unused)
{
	wpt_tmp->altitude = atof(args);
}



ff_vecs_t gtc_vecs = {
        ff_type_file,
	{ ff_cap_none, ff_cap_write, ff_cap_none},
        gtc_rd_init,
        gtc_wr_init,
        NULL,
        gtc_wr_deinit,
        NULL,
        gtc_write,
        NULL,
        gtc_args,
	CET_CHARSET_ASCII, 0	/* CET-REVIEW */
};
