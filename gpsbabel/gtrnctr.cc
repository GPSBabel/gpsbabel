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

static gbfile* ofd;
static int lap_ct = 0;
static int lap_s = 0;
static waypoint* wpt_tmp;
static route_head* trk_head;
static computed_trkdata* tdata;

#define MYNAME "gtc"

#define GTC_MAX_NAME_LEN 15

#define MAX_SPORTS 4
static char gtc_sportlist[MAX_SPORTS][16] = { "Biking", "Running", "MultiSport", "Other" };
static int gtc_sport = 0;
static int gtc_course_flag;

static gpsbabel::DateTime gtc_least_time;
static gpsbabel::DateTime gtc_most_time;
static double gtc_start_lat;
static double gtc_start_long;
static double gtc_end_lat;
static double gtc_end_long;

static char* opt_sport, *opt_course;

static
arglist_t gtc_args[] = {
  {
    "course", &opt_course, "Write course rather than history, default yes",
    "1", ARGTYPE_BOOL, ARG_NOMINMAX
  },
  {
    "sport", &opt_sport, "Sport: Biking (deflt), Running, MultiSport, Other",
    "Biking", ARGTYPE_STRING, ARG_NOMINMAX
  },
  ARG_TERMINATOR
};

#if ! HAVE_LIBEXPAT
static void
gtc_rd_init(const char* fname)
{
  fatal(MYNAME ": this format does not support reading.\n");
}

static void
gtc_read(void)
{
}

static void
gtc_rd_deinit(void)
{

}
#else

/* Tracks */
static xg_callback	gtc_trk_s;
static xg_callback	gtc_trk_ident;
static xg_callback	gtc_trk_lap_s, gtc_trk_lap_e;
static xg_callback	gtc_trk_pnt_s, gtc_trk_pnt_e;
static xg_callback	gtc_trk_utc;
static xg_callback	gtc_trk_lat;
static xg_callback	gtc_trk_long;
static xg_callback	gtc_trk_alt;
static xg_callback	gtc_trk_dist;
static xg_callback	gtc_trk_hr;
static xg_callback	gtc_trk_cad;
static xg_callback	gtc_trk_pwr;
static xg_callback	gtc_trk_spd;
static xg_callback	gtc_wpt_crs_s, gtc_wpt_crs_e;
static xg_callback	gtc_wpt_pnt_s, gtc_wpt_pnt_e;
static xg_callback	gtc_wpt_ident;
static xg_callback	gtc_wpt_lat;
static xg_callback	gtc_wpt_long;
static xg_callback	gtc_wpt_icon;
static xg_callback	gtc_wpt_notes;

static xg_tag_mapping gtc_map[] = {
  /* courses tcx v1 & v2 */
  { gtc_trk_s,    cb_start, "/Courses/Course" },
  { gtc_trk_ident,cb_cdata, "/Courses/Course/Name"},
  { gtc_trk_pnt_s,cb_start, "/Courses/Course/Track/Trackpoint" },
  { gtc_trk_pnt_e,cb_end,   "/Courses/Course/Track/Trackpoint" },
  { gtc_trk_utc,  cb_cdata, "/Courses/Course/Track/Trackpoint/Time" },
  { gtc_trk_lat,  cb_cdata, "/Courses/Course/Track/Trackpoint/Position/LatitudeDegrees" },
  { gtc_trk_long, cb_cdata, "/Courses/Course/Track/Trackpoint/Position/LongitudeDegrees" },
  { gtc_trk_alt,  cb_cdata, "/Courses/Course/Track/Trackpoint/AltitudeMeters" },
  { gtc_trk_hr,   cb_cdata, "/Courses/Course/Track/Trackpoint/HeartRateBpm" },
  { gtc_trk_cad,  cb_cdata, "/Courses/Course/Track/Trackpoint/Cadence" },
  { gtc_wpt_crs_s,cb_start, "/Courses/Course/CoursePoint" },
  { gtc_wpt_crs_e,cb_end,   "/Courses/Course/CoursePoint" },
  { gtc_wpt_ident,cb_cdata, "/Courses/Course/CoursePoint/Name"},
  { gtc_trk_utc,  cb_cdata, "/Courses/Course/CoursePoint/Time"},
  { gtc_wpt_lat,  cb_cdata, "/Courses/Course/CoursePoint/Position/LatitudeDegrees"},
  { gtc_wpt_long, cb_cdata, "/Courses/Course/CoursePoint/Position/LongitudeDegrees"},
  { gtc_trk_alt,  cb_cdata, "/Courses/Course/CoursePoint/AltitudeMeters" },
  { gtc_wpt_icon, cb_cdata, "/Courses/Course/CoursePoint/PointType" },
  { gtc_wpt_notes,cb_cdata, "/Courses/Course/CoursePoint/Notes" },

  /* history tcx v2 (activities) */
  { gtc_trk_s,    cb_start, "/Activities/Activity" },
  { gtc_trk_ident,cb_cdata, "/Activities/Activity/Id" },
  { gtc_trk_lap_s,cb_start, "/Activities/Activity/Lap" },
  { gtc_trk_lap_e,cb_end,   "/Activities/Activity/Lap" },
  { gtc_trk_pnt_s,cb_start, "/Activities/Activity/Lap/Track/Trackpoint" },
  { gtc_trk_pnt_e,cb_end,   "/Activities/Activity/Lap/Track/Trackpoint" },
  { gtc_trk_utc,  cb_cdata, "/Activities/Activity/Lap/Track/Trackpoint/Time" },
  { gtc_trk_lat,  cb_cdata, "/Activities/Activity/Lap/Track/Trackpoint/Position/LatitudeDegrees" },
  { gtc_trk_long, cb_cdata, "/Activities/Activity/Lap/Track/Trackpoint/Position/LongitudeDegrees" },
  { gtc_trk_alt,  cb_cdata, "/Activities/Activity/Lap/Track/Trackpoint/AltitudeMeters" },
  { gtc_trk_dist, cb_cdata, "/Activities/Activity/Lap/Track/Trackpoint/DistanceMeters" },
  { gtc_trk_hr,   cb_cdata, "/Activities/Activity/Lap/Track/Trackpoint/HeartRateBpm" },
  { gtc_trk_cad,  cb_cdata, "/Activities/Activity/Lap/Track/Trackpoint/Cadence" },
  { gtc_trk_pwr,  cb_cdata, "/Activities/Activity/Lap/Track/Trackpoint/Extensions/ns3:TPX/ns3:Watts" },
  // It looks like Speed and Watts should be siblings, but Garmin can't get
  // their namespace act very consistent.  This works for a sample provided
  // by Laurent Desmons in 5/2013.
  { gtc_trk_spd,  cb_cdata, "/Activities/Activity/Lap/Track/Trackpoint/Extensions/TPX/Speed" },

  /* history tcx v1 */
  { gtc_trk_s,    cb_start, "/History/Run" },
  { gtc_trk_ident,cb_cdata, "/History/Run/Id" },
  { gtc_trk_lap_s,cb_start, "/History/Run/Lap" },
  { gtc_trk_lap_e,cb_end,   "/History/Run/Lap" },
  { gtc_trk_pnt_s,cb_start, "/History/Run/Lap/Track/Trackpoint" },
  { gtc_trk_pnt_e,cb_end,   "/History/Run/Lap/Track/Trackpoint" },
  { gtc_trk_utc,  cb_cdata, "/History/Run/Lap/Track/Trackpoint/Time" },
  { gtc_trk_lat,  cb_cdata, "/History/Run/Lap/Track/Trackpoint/Position/LatitudeDegrees" },
  { gtc_trk_long, cb_cdata, "/History/Run/Lap/Track/Trackpoint/Position/LongitudeDegrees" },
  { gtc_trk_alt,  cb_cdata, "/History/Run/Lap/Track/Trackpoint/AltitudeMeters" },
  { gtc_trk_hr,   cb_cdata, "/History/Run/Lap/Track/Trackpoint/HeartRateBpm" },
  { gtc_trk_cad,  cb_cdata, "/History/Run/Lap/Track/Trackpoint/Cadence" },

  { gtc_wpt_pnt_s,cb_start, "/Courses/Course/Lap/BeginPosition" },
  { gtc_wpt_pnt_e,cb_end, "/Courses/Course/Lap/BeginPosition" },
  { gtc_wpt_lat,  cb_cdata, "/Courses/Course/Lap/BeginPosition/LatitudeDegrees" },
  { gtc_wpt_long, cb_cdata, "/Courses/Course/Lap/BeginPosition/LongitudeDegrees" },
  { gtc_trk_alt,  cb_cdata, "/Courses/Course/Lap/BeginAltitudeMeters" },

  { NULL,	(xg_cb_type)0,         NULL}
};

static const char*
gtc_tags_to_ignore[] = {
  "TrainingCenterDatabase",
  "CourseFolder",
  "Running",
  "Biking",
  "Other",
  "Multisport",
  NULL,
};

static void
gtc_rd_init(const char* fname)
{
  xml_init(fname, gtc_map, NULL);
  xml_ignore_tags(gtc_tags_to_ignore);
}

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
gtc_wr_init(const char* fname)
{
  int i;

  ofd = gbfopen(fname, "w", MYNAME);

  if (opt_sport) {
    for (i = 0; i < MAX_SPORTS; i++) {
      if (0 == case_ignore_strncmp(opt_sport, gtc_sportlist[i], 2)) {
        gtc_sport = i;
        break;
      }
    }
  }
  gtc_course_flag = atoi(opt_course);
}

static void
gtc_wr_deinit(void)
{
  gbfclose(ofd);
}

static int gtc_indent_level;

static void
gtc_write_xml(int indent, const char* fmt, ...)
{
  va_list args;

  va_start(args, fmt);

  if (indent < 0) {
    gtc_indent_level--;
  }

  gbfprintf(ofd, "%*s", gtc_indent_level * 2, "");
  gbvfprintf(ofd, fmt, args);

  if (indent > 0) {
    gtc_indent_level++;
  }

  va_end(args);
}

static void
gtc_lap_start(const route_head* rte)
{
  gtc_least_time = 0;
  gtc_most_time = 0;
}

static void
gtc_new_study_lap(const route_head* rte)
{
  track_recompute(rte, &tdata);  /* called routine allocates space for tdata */
}

static void
gtc_study_lap(const waypoint* wpt)
{
  if (wpt->creation_time && (gtc_least_time == 0)) {
    gtc_least_time = wpt->GetCreationTime();
    gtc_start_lat = wpt->latitude;
    gtc_start_long = wpt->longitude;
  }

  if (wpt->creation_time && (gtc_least_time > wpt->GetCreationTime())) {
    gtc_least_time = wpt->GetCreationTime();
    gtc_start_lat = wpt->latitude;
    gtc_start_long = wpt->longitude;
  }
  if (wpt->creation_time > gtc_most_time)  {
    gtc_most_time = wpt->GetCreationTime();
    gtc_end_lat = wpt->latitude;
    gtc_end_long = wpt->longitude;
  }
}

static void
gtc_waypt_pr(const waypoint* wpt)
{
  if (wpt->wpt_flags.is_split != 0) {
    gtc_write_xml(1, "<Trackpoint split=\"yes\">\n");
  } else {
    gtc_write_xml(1, "<Trackpoint>\n");
  }

  if (wpt->creation_time) {
    QString time_string = wpt->CreationTimeXML();
    if (!time_string.isEmpty()) {
      gtc_write_xml(0, "<Time>%s</Time>\n",
                    qPrintable(time_string));
    }
  }
  if (wpt->latitude && wpt->longitude) {
    gtc_write_xml(1, "<Position>\n");
    gtc_write_xml(0, "<LatitudeDegrees>%.7f</LatitudeDegrees>\n", wpt->latitude);
    gtc_write_xml(0, "<LongitudeDegrees>%.7f</LongitudeDegrees>\n", wpt->longitude);
    gtc_write_xml(-1, "</Position>\n");
  }
  if (wpt->altitude != unknown_alt) {
    gtc_write_xml(0, "<AltitudeMeters>%.1f</AltitudeMeters>\n", wpt->altitude);
  }
  if (wpt->odometer_distance) {
    gtc_write_xml(0, "<DistanceMeters>%.2f</DistanceMeters>\n", wpt->odometer_distance);
  }
  // TODO: find a schema extension to include wpt->course and wpt->temperature
  // TODO: find a way to include DistanceMeters from odometer information
  if (wpt->heartrate) {
    //gtc_write_xml(0, "<HeartRateBpm>%d</HeartRateBpm>\n", wpt->heartrate);
    gtc_write_xml(1, "<HeartRateBpm xsi:type=\"HeartRateInBeatsPerMinute_t\">\n");
    gtc_write_xml(0,"<Value>%d</Value>\n", wpt->heartrate);
    gtc_write_xml(-1,"</HeartRateBpm>\n");
  }
  if (wpt->cadence) {
    gtc_write_xml(0, "<Cadence>%d</Cadence>\n", wpt->cadence);
  }
  if (wpt->speed || wpt->power) {
    gtc_write_xml(1, "<Extensions>\n");
    gtc_write_xml(1, "<TPX xmlns=\"http://www.garmin.com/xmlschemas/ActivityExtension/v2\">\n");
    /* see http://www8.garmin.com/xmlschemas/ActivityExtensionv2.xsd */
    if (wpt->speed) {
      gtc_write_xml(0, "<Speed>%.3f</Speed>\n", wpt->speed);
    }
    if (wpt->power) {
      gtc_write_xml(0, "<Watts>%.0f</Watts>\n", wpt->power);
    }
    gtc_write_xml(-1, "</TPX>\n");
    gtc_write_xml(-1, "</Extensions>\n");
  }

  gtc_write_xml(-1, "</Trackpoint>\n");
}

static void
gtc_fake_hdr(void)
{
  long secs = 0;
  if (gtc_least_time && gtc_most_time) {
    secs = gtc_most_time - gtc_least_time;
  }

  /* write these in either case, course or activity format */
  gtc_write_xml(0, "<TotalTimeSeconds>%d</TotalTimeSeconds>\n", secs);
  if (tdata->distance_meters) {
    gtc_write_xml(0, "<DistanceMeters>%.2f</DistanceMeters>\n",
                  tdata->distance_meters);
  }
  if (tdata->avg_hrt) {
    gtc_write_xml(1, "<AverageHeartRateBpm xsi:type=\"HeartRateInBeatsPerMinute_t\">\n");
    gtc_write_xml(0, "<Value>%d</Value>\n", (int) (tdata->avg_hrt + 0.5));
    gtc_write_xml(-1,"</AverageHeartRateBpm>\n");
  }
  if (tdata->max_hrt) {
    gtc_write_xml(1, "<MaximumHeartRateBpm xsi:type=\"HeartRateInBeatsPerMinute_t\">\n");
    gtc_write_xml(0, "<Value>%d</Value>\n", (int) (tdata->max_hrt + 0.5));
    gtc_write_xml(-1,"</MaximumHeartRateBpm>\n");
  }
  if (tdata->avg_cad) {
    gtc_write_xml(0, "<Cadence>%d</Cadence>\n", tdata->avg_cad);
  }
  gtc_write_xml(0, "<Intensity>Active</Intensity>\n");

  if (gtc_course_flag) { /* course format */
    gtc_write_xml(1, "<BeginPosition>\n");
    gtc_write_xml(0, "<LatitudeDegrees>%lf</LatitudeDegrees>\n", gtc_start_lat);
    gtc_write_xml(0, "<LongitudeDegrees>%lf</LongitudeDegrees>\n", gtc_start_long);
    gtc_write_xml(-1,"</BeginPosition>\n");
    gtc_write_xml(1, "<EndPosition>\n");
    gtc_write_xml(0, "<LatitudeDegrees>%lf</LatitudeDegrees>\n", gtc_end_lat);
    gtc_write_xml(0, "<LongitudeDegrees>%lf</LongitudeDegrees>\n", gtc_end_long);
    gtc_write_xml(-1,"</EndPosition>\n");

  } else {  /* activity (history) format */
    if (tdata->max_spd) {
      gtc_write_xml(0, "<MaximumSpeed>%.3f</MaximumSpeed>\n", tdata->max_spd);
    }
    //gtc_write_xml(0, "<Calories>0</Calories>\n");
    gtc_write_xml(0, "<TriggerMethod>Manual</TriggerMethod>\n");
  }

}

static void
gtc_act_hdr(const route_head* rte)
{
  gtc_write_xml(1, "<Activity Sport=\"%s\">\n", gtc_sportlist[gtc_sport]);
  gtc_lap_start(NULL);
  gtc_new_study_lap(rte);
  route_disp(rte, gtc_study_lap);
  if (gtc_least_time) {
    gtc_write_xml(0, "<Id>%s</Id>\n", 
                      gtc_least_time.toPrettyString().toUtf8().data());
    gtc_write_xml(1, "<Lap StartTime=\"%s\">\n", 
                      gtc_least_time.toPrettyString().toUtf8().data());
  } else {
    gtc_write_xml(1, "<Lap>\n");
  }
  gtc_fake_hdr();
  xfree(tdata);
  gtc_write_xml(1,"<Track>\n");
}

static void
gtc_act_ftr(const route_head* rte)
{
  gtc_write_xml(-1, "</Track>\n");
  gtc_write_xml(-1, "</Lap>\n");
  gtc_write_xml(-1, "</Activity>\n");
}

static void
gtc_crs_hdr(const route_head* rte)
{

  gtc_write_xml(1, "<Course>\n");
  gtc_lap_start(NULL);
  gtc_new_study_lap(rte);
  route_disp(rte, gtc_study_lap);
  if (rte->rte_name) {
    char* name = xstrndup(rte->rte_name, GTC_MAX_NAME_LEN);
    gtc_write_xml(0, "<Name>%s</Name>\n", name);
    xfree(name);
  } else {
    gtc_write_xml(0, "<Name>New Course</Name>\n");
  }
  /* write_optional_xml_entity(ofd, "      ", "Name", rte->rte_name); */
  gtc_write_xml(1, "<Lap>\n");
  gtc_fake_hdr();
  gtc_write_xml(-1, "</Lap>\n");
  xfree(tdata);
  gtc_write_xml(1,"<Track>\n");
}

static void
gtc_crs_ftr(const route_head* rte)
{
  gtc_write_xml(-1,"</Track>\n");
  gtc_write_xml(-1, "</Course>\n");

}

void
gtc_write(void)
{
  gtc_write_xml(0, "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\" ?>\n");
  gtc_write_xml(1, "<TrainingCenterDatabase xmlns=\"http://www.garmin.com/xmlschemas/TrainingCenterDatabase/v2\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://www.garmin.com/xmlschemas/TrainingCenterDatabase/v2 http://www.garmin.com/xmlschemas/TrainingCenterDatabasev2.xsd\">\n");

  if (gtc_course_flag) {
    gtc_write_xml(1, "<Courses>\n");
    track_disp_all(gtc_crs_hdr, gtc_crs_ftr, gtc_waypt_pr);
    gtc_write_xml(-1, "</Courses>\n");
  } else {
    gtc_write_xml(1, "<Activities>\n");
    track_disp_all(gtc_act_hdr, gtc_act_ftr, gtc_waypt_pr);
    gtc_write_xml(-1, "</Activities>\n");
  }

  gtc_write_xml(-1, "</TrainingCenterDatabase>\n");
}

void
gtc_trk_s(const char* unused, const char** attrv)
{
  trk_head = route_head_alloc();
  track_add_head(trk_head);
}

void
gtc_trk_ident(const char* args, const char** unused)
{
  trk_head->rte_name = xstrdup(args);
}

void
gtc_trk_lap_s(const char* unused, const char** attrv)
{
  lap_ct++;
  lap_s = 1;
}

void
gtc_trk_lap_e(const char* unused, const char** attrv)
{
  lap_s = 0;
}

void
gtc_trk_pnt_s(const char* unused, const char** attrv)
{
  wpt_tmp = waypt_new();
}

void
gtc_trk_pnt_e(const char* args, const char** unused)
{
  if (wpt_tmp->longitude != 0. && wpt_tmp->latitude != 0.) {
    if (lap_s) {
      /* Add the first point of an ActivityLap as
      a waypoint as well as a trackpoint. */
      char cbuf[10];
      waypoint* wpt_lap_s = waypt_dupe(wpt_tmp);
      snprintf(cbuf, sizeof(cbuf), "LAP%03d", lap_ct);
      wpt_lap_s->shortname = xstrdup(cbuf);
      waypt_add(wpt_lap_s);
    }

    track_add_wpt(trk_head, wpt_tmp);
  } else {
    waypt_free(wpt_tmp);
  }

  wpt_tmp = NULL;
  lap_s = 0;
}

void
gtc_trk_utc(const char* args, const char** unused)
{
  wpt_tmp->creation_time = xml_parse_time(args);
}

void
gtc_trk_lat(const char* args, const char** unused)
{
  wpt_tmp->latitude = atof(args);
}

void
gtc_trk_long(const char* args, const char** unused)
{
  wpt_tmp->longitude = atof(args);
}

void
gtc_trk_alt(const char* args, const char** unused)
{
  wpt_tmp->altitude = atof(args);
}

void
gtc_trk_dist(const char* args, const char** unused)
{
  wpt_tmp->odometer_distance = atof(args);
}

void
gtc_trk_hr(const char* args, const char** unused)
{
  wpt_tmp->heartrate = atoi(args);
}

void
gtc_trk_cad(const char* args, const char** unused)
{
  wpt_tmp->cadence = atoi(args);
}

void
gtc_trk_pwr(const char* args, const char** unused)
{
  wpt_tmp->power = atof(args);
}

void
gtc_trk_spd(const char* args, const char** unused)
{
  WAYPT_SET(wpt_tmp, speed, atof(args));
}

void
gtc_wpt_crs_s(const char* unused, const char** attrv)
{
  wpt_tmp = waypt_new();
}

void
gtc_wpt_crs_e(const char* args, const char** unused)
{
  if (wpt_tmp->longitude != 0. && wpt_tmp->latitude != 0.) {
    waypt_add(wpt_tmp);
  } else {
    waypt_free(wpt_tmp);
  }

  wpt_tmp = NULL;
}

void
gtc_wpt_pnt_s(const char* unused, const char** attrv)
{
  wpt_tmp = waypt_new();
  lap_ct++;
}

void
gtc_wpt_pnt_e(const char* args, const char** unused)
{
  if (wpt_tmp->longitude != 0. && wpt_tmp->latitude != 0.) {
    /* Add the begin position of a CourseLap as
    a waypoint. */
    char *cbuf;
    xasprintf(&cbuf, "LAP%03d", lap_ct);
    wpt_tmp->shortname = cbuf;
    waypt_add(wpt_tmp);
  } else {
    waypt_free(wpt_tmp);
  }

  wpt_tmp = NULL;
}

void
gtc_wpt_ident(const char* args, const char** unused)
{
  wpt_tmp->shortname = xstrdup(args);
  /* Set also as notes for compatibility with garmin usb format */
  wpt_tmp->notes = xstrdup(args);
}

void
gtc_wpt_lat(const char* args, const char** unused)
{
  wpt_tmp->latitude = atof(args);
}

void
gtc_wpt_long(const char* args, const char** unused)
{
  wpt_tmp->longitude = atof(args);
}

void
gtc_wpt_icon(const char* args, const char** unused)
{
  wpt_tmp->icon_descr = args;
}

void
gtc_wpt_notes(const char* args, const char** unused)
{
  wpt_tmp->description = xstrdup(args);
}

ff_vecs_t gtc_vecs = {
  ff_type_file,
  {
    ff_cap_read 			/* waypoints */,
    (ff_cap)(ff_cap_read | ff_cap_write) 	/* tracks */,
    ff_cap_none 			/* routes */
  },
  gtc_rd_init,
  gtc_wr_init,
  gtc_rd_deinit,
  gtc_wr_deinit,
  gtc_read,
  gtc_write,
  NULL,
  gtc_args,
  CET_CHARSET_ASCII, 0	/* CET-REVIEW */
};
