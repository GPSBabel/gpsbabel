/*
    Access Garmin Training Center (Forerunner/Foretracker/Edge) data files.

    Copyright (C) 2006, 2007 Robert Lipe, robertlipe+source@gpsbabel.org

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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

 */
/*
 * Relevant schema definitions can be found at
 * http://www8.garmin.com/xmlschemas/TrainingCenterDatabasev2.xsd
 * http://www8.garmin.com/xmlschemas/ActivityExtensionv2.xsd
 */

#include "gtrnctr.h"

#include <QByteArray>            // for QByteArray
#include <QDateTime>             // for QDateTime
#include <QtGlobal>              // for qRound, qPrintable
#include <QXmlStreamAttributes>  // for QXmlStreamAttributes

#include <cstdarg>               // for va_end, va_list, va_start
#include <cstdio>                // for snprintf
#include <iterator>              // for size
#include <optional>              // for optional
#include <type_traits>           // for add_const<>::type

#include "defs.h"                // for Waypoint, route_head, computed_trkdata, waypt_add, route_disp, track_disp_all, case_ignore_strncmp, track_add_head, track_add_wpt, track_recompute, xml_parse_time, CSTR, wp_flags, WAYPT_SET, unknown_alt
#include "xmlgeneric.h"          // for xml_deinit, xml_init, xml_read


#define MYNAME "gtc"

const QStringList GtrnctrFormat::gtc_tags_to_ignore = {
  "TrainingCenterDatabase",
  "CourseFolder",
  "Running",
  "Biking",
  "Other",
  "Multisport"
};

void
GtrnctrFormat::rd_init(const QString& fname)
{
  xml_reader = new XmlGenericReader;
  xml_reader->xml_init(fname, this, gtc_map, nullptr, gtc_tags_to_ignore);
}

void
GtrnctrFormat::read()
{
  xml_reader->xml_read();
}

void
GtrnctrFormat::rd_deinit()
{
  delete xml_reader;
  xml_reader = nullptr;
}

void
GtrnctrFormat::wr_init(const QString& fname)
{
  ofd = gbfopen(fname, "w", MYNAME);

  if (opt_sport) {
    for (unsigned int i = 0; i < std::size(gtc_sportlist); i++) {
      if (0 == case_ignore_strncmp(opt_sport, gtc_sportlist[i], 2)) {
        gtc_sport = i;
        break;
      }
    }
  }
  gtc_course_flag = opt_course;
}

void
GtrnctrFormat::wr_deinit()
{
  gbfclose(ofd);
}

void
GtrnctrFormat::gtc_write_xml(int indent, const char* fmt, ...)
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

void
GtrnctrFormat::gtc_write_xml(int indent, const QString& s)
{
  if (indent < 0) {
    gtc_indent_level--;
  }
  gbfprintf(ofd, "%*s", gtc_indent_level * 2, "");
  gbfputs(s, ofd);
  if (indent > 0) {
    gtc_indent_level++;
  }


}

void
GtrnctrFormat::gtc_lap_start(const route_head* /*unused*/)
{
  gtc_least_time = gpsbabel::DateTime();
  gtc_most_time = gpsbabel::DateTime();
}

computed_trkdata
GtrnctrFormat::gtc_new_study_lap(const route_head* rte)
{
  return track_recompute(rte);
}

void
GtrnctrFormat::gtc_study_lap(const Waypoint* wpt)
{
  if (wpt->creation_time.isValid() && (!gtc_least_time.isValid())) {
    gtc_least_time = wpt->GetCreationTime();
    gtc_start_lat = wpt->latitude;
    gtc_start_long = wpt->longitude;
  }

  if (wpt->creation_time.isValid() && (gtc_least_time > wpt->GetCreationTime())) {
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

void
GtrnctrFormat::gtc_waypt_pr(const Waypoint* wpt)
{
  if (wpt->wpt_flags.is_split != 0) {
    gtc_write_xml(1, "<Trackpoint split=\"yes\">\n");
  } else {
    gtc_write_xml(1, "<Trackpoint>\n");
  }

  if (wpt->creation_time.isValid()) {
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
  if (wpt->speed_has_value() || wpt->power) {
    gtc_write_xml(1, "<Extensions>\n");
    gtc_write_xml(1, "<TPX xmlns=\"http://www.garmin.com/xmlschemas/ActivityExtension/v2\">\n");
    /* see http://www8.garmin.com/xmlschemas/ActivityExtensionv2.xsd */
    if (wpt->speed_has_value()) {
      gtc_write_xml(0, "<Speed>%.3f</Speed>\n", wpt->speed_value());
    }
    if (wpt->power) {
      gtc_write_xml(0, "<Watts>%.0f</Watts>\n", wpt->power);
    }
    gtc_write_xml(-1, "</TPX>\n");
    gtc_write_xml(-1, "</Extensions>\n");
  }

  gtc_write_xml(-1, "</Trackpoint>\n");
}

void
GtrnctrFormat::gtc_fake_hdr(const computed_trkdata& tdata)
{
  /* handle the CourseLap_t or the ActivityLap_t types. */
  /* note that the elements must appear in the order required by the schema. */
  /* also note some of the elements are required. */

  long long secs = 0;
  if (gtc_least_time.isValid() && gtc_most_time.isValid()) {
    secs = gtc_least_time.secsTo(gtc_most_time);
  }

  /* write these in either case, course or activity format */
  gtc_write_xml(0, "<TotalTimeSeconds>%lld</TotalTimeSeconds>\n", secs);
  gtc_write_xml(0, "<DistanceMeters>%.2f</DistanceMeters>\n", tdata.distance_meters);
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
    if (tdata.max_spd) {
      gtc_write_xml(0, "<MaximumSpeed>%.3f</MaximumSpeed>\n", *tdata.max_spd);
    }
    gtc_write_xml(0, "<Calories>0</Calories>\n"); /* element is required */
  }
  if (tdata.avg_hrt) {
    gtc_write_xml(1, "<AverageHeartRateBpm xsi:type=\"HeartRateInBeatsPerMinute_t\">\n");
    gtc_write_xml(0, "<Value>%d</Value>\n", qRound(*tdata.avg_hrt));
    gtc_write_xml(-1,"</AverageHeartRateBpm>\n");
  }
  if (tdata.max_hrt) {
    gtc_write_xml(1, "<MaximumHeartRateBpm xsi:type=\"HeartRateInBeatsPerMinute_t\">\n");
    gtc_write_xml(0, "<Value>%d</Value>\n", *tdata.max_hrt);
    gtc_write_xml(-1,"</MaximumHeartRateBpm>\n");
  }
  gtc_write_xml(0, "<Intensity>Active</Intensity>\n");
  if (tdata.avg_cad) {
    gtc_write_xml(0, "<Cadence>%d</Cadence>\n", qRound(*tdata.avg_cad));
  }

  if (!gtc_course_flag) { /* activity (history) format */
    gtc_write_xml(0, "<TriggerMethod>Manual</TriggerMethod>\n");
  }

}

void
GtrnctrFormat::gtc_act_hdr(const route_head* rte)
{
  gtc_write_xml(1, "<Activity Sport=\"%s\">\n", gtc_sportlist[gtc_sport]);
  gtc_lap_start(nullptr);
  computed_trkdata tdata = gtc_new_study_lap(rte);
  auto gtc_study_lap_lambda = [this](const Waypoint* waypointp)->void {
    gtc_study_lap(waypointp);
  };
  route_disp(rte, gtc_study_lap_lambda);
  if (gtc_least_time.isValid()) {
    gtc_write_xml(0, "<Id>%s</Id>\n",
                  CSTR(gtc_least_time.toPrettyString()));
    gtc_write_xml(1, "<Lap StartTime=\"%s\">\n",
                  CSTR(gtc_least_time.toPrettyString()));
  } else {
    gtc_write_xml(1, "<Lap>\n");
  }
  gtc_fake_hdr(tdata);
  gtc_write_xml(1,"<Track>\n");
}

void
GtrnctrFormat::gtc_act_ftr(const route_head* /*unused*/)
{
  gtc_write_xml(-1, "</Track>\n");
  gtc_write_xml(-1, "</Lap>\n");
  gtc_write_xml(-1, "</Activity>\n");
}

void
GtrnctrFormat::gtc_crs_hdr(const route_head* rte)
{

  gtc_write_xml(1, "<Course>\n");
  gtc_lap_start(nullptr);
  computed_trkdata tdata = gtc_new_study_lap(rte);
  auto gtc_study_lap_lambda = [this](const Waypoint* waypointp)->void {
    gtc_study_lap(waypointp);
  };
  route_disp(rte, gtc_study_lap_lambda);

  if (!rte->rte_name.isEmpty()) {
    QString name = rte->rte_name.left(kGtcMaxNameLen);
    gtc_write_xml(0, QStringLiteral("<Name>%1</Name>\n").arg(name));
  } else {
    gtc_write_xml(0, "<Name>New Course</Name>\n");
  }
  /* write_optional_xml_entity(ofd, "      ", "Name", rte->rte_name); */
  gtc_write_xml(1, "<Lap>\n");
  gtc_fake_hdr(tdata);
  gtc_write_xml(-1, "</Lap>\n");
  gtc_write_xml(1,"<Track>\n");
}

void
GtrnctrFormat::gtc_crs_ftr(const route_head* /*unused*/)
{
  gtc_write_xml(-1,"</Track>\n");
  gtc_write_xml(-1, "</Course>\n");

}

void
GtrnctrFormat::write()
{
  gtc_write_xml(0, "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\" ?>\n");
  gtc_write_xml(1, "<TrainingCenterDatabase xmlns=\"http://www.garmin.com/xmlschemas/TrainingCenterDatabase/v2\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://www.garmin.com/xmlschemas/TrainingCenterDatabase/v2 http://www.garmin.com/xmlschemas/TrainingCenterDatabasev2.xsd\">\n");

  auto gtc_waypt_pr_lambda = [this](const Waypoint* waypointp)->void {
    gtc_waypt_pr(waypointp);
  };
  if (gtc_course_flag) {
    gtc_write_xml(1, "<Courses>\n");
    auto gtc_crs_hdr_lambda = [this](const route_head* rte)->void {
      gtc_crs_hdr(rte);
    };
    auto gtc_crs_ftr_lambda = [this](const route_head* rte)->void {
      gtc_crs_ftr(rte);
    };
    track_disp_all(gtc_crs_hdr_lambda, gtc_crs_ftr_lambda, gtc_waypt_pr_lambda);
    gtc_write_xml(-1, "</Courses>\n");
  } else {
    gtc_write_xml(1, "<Activities>\n");
    auto gtc_act_hdr_lambda = [this](const route_head* rte)->void {
      gtc_act_hdr(rte);
    };
    auto gtc_act_ftr_lambda = [this](const route_head* rte)->void {
      gtc_act_ftr(rte);
    };
    track_disp_all(gtc_act_hdr_lambda, gtc_act_ftr_lambda, gtc_waypt_pr_lambda);
    gtc_write_xml(-1, "</Activities>\n");
  }

  gtc_write_xml(-1, "</TrainingCenterDatabase>\n");
}

void
GtrnctrFormat::gtc_trk_s(const QString& /*unused*/, const QXmlStreamAttributes* /*unused*/)
{
  trk_head = new route_head;
  track_add_head(trk_head);
}

void
GtrnctrFormat::gtc_trk_ident(const QString& args, const QXmlStreamAttributes* /*unused*/)
{
  trk_head->rte_name = args;
}

void
GtrnctrFormat::gtc_trk_lap_s(const QString& /*unused*/, const QXmlStreamAttributes* /*unused*/)
{
  lap_ct++;
  lap_s = 1;
}

void
GtrnctrFormat::gtc_trk_lap_e(const QString& /*unused*/, const QXmlStreamAttributes* /*unused*/)
{
  lap_s = 0;
}

void
GtrnctrFormat::gtc_trk_pnt_s(const QString& /*unused*/, const QXmlStreamAttributes* /*unused*/)
{
  wpt_tmp = new Waypoint;
}

void
GtrnctrFormat::gtc_trk_pnt_e(const QString& /*unused*/, const QXmlStreamAttributes* /*unused*/)
{
  if (wpt_tmp->longitude != 0. && wpt_tmp->latitude != 0.) {
    if (lap_s) {
      /* Add the first point of an ActivityLap as
      a waypoint as well as a trackpoint. */
      char cbuf[10];
      auto* wpt_lap_s = new Waypoint(*wpt_tmp);
      snprintf(cbuf, sizeof(cbuf), "LAP%03d", lap_ct);
      wpt_lap_s->shortname = cbuf;
      waypt_add(wpt_lap_s);
      lap_s = 0;
    }

    track_add_wpt(trk_head, wpt_tmp);
  } else {
    delete wpt_tmp;
  }

  wpt_tmp = nullptr;
}

void
GtrnctrFormat::gtc_trk_utc(const QString& args, const QXmlStreamAttributes* /*unused*/)
{
  wpt_tmp->creation_time = xml_parse_time(args);
}

void
GtrnctrFormat::gtc_trk_lat(const QString& args, const QXmlStreamAttributes* /*unused*/)
{
  wpt_tmp->latitude = args.toDouble();
}

void
GtrnctrFormat::gtc_trk_long(const QString& args, const QXmlStreamAttributes* /*unused*/)
{
  wpt_tmp->longitude = args.toDouble();
}

void
GtrnctrFormat::gtc_trk_alt(const QString& args, const QXmlStreamAttributes* /*unused*/)
{
  wpt_tmp->altitude = args.toDouble();
}

void GtrnctrFormat::gtc_trk_dist(const QString& args, const QXmlStreamAttributes* /*unused*/)
{
  wpt_tmp->odometer_distance = args.toDouble();
}
void GtrnctrFormat::gtc_trk_hr(const QString& args, const QXmlStreamAttributes* /*unused*/)
{
  wpt_tmp->heartrate = args.toDouble();
}
void GtrnctrFormat::gtc_trk_cad(const QString& args, const QXmlStreamAttributes* /*unused*/)
{
  wpt_tmp->cadence = args.toDouble();
}

void
GtrnctrFormat::gtc_trk_pwr(const QString& args, const QXmlStreamAttributes* /*unused*/)
{
  wpt_tmp->power = args.toDouble();
}

void
GtrnctrFormat::gtc_trk_spd(const QString& args, const QXmlStreamAttributes* /*unused*/)
{
  wpt_tmp->set_speed(args.toDouble());
}

void
GtrnctrFormat::gtc_wpt_crs_s(const QString& /*unused*/, const QXmlStreamAttributes* /*unused*/)
{
  wpt_tmp = new Waypoint;
}

void
GtrnctrFormat::gtc_wpt_crs_e(const QString& /*unused*/, const QXmlStreamAttributes* /*unused*/)
{
  if (wpt_tmp->longitude != 0. && wpt_tmp->latitude != 0.) {
    waypt_add(wpt_tmp);
  } else {
    delete wpt_tmp;
  }

  wpt_tmp = nullptr;
}

void
GtrnctrFormat::gtc_wpt_pnt_s(const QString& /*unused*/, const QXmlStreamAttributes* /*unused*/)
{
  wpt_tmp = new Waypoint;
  lap_ct++;
}

void
GtrnctrFormat::gtc_wpt_pnt_e(const QString& /*unused*/, const QXmlStreamAttributes* /*unused*/)
{
  if (wpt_tmp->longitude != 0. && wpt_tmp->latitude != 0.) {
    /* Add the begin position of a CourseLap as
    a waypoint. */
    wpt_tmp->shortname = QString::asprintf("LAP%03d", lap_ct);
    waypt_add(wpt_tmp);
  } else {
    delete wpt_tmp;
  }

  wpt_tmp = nullptr;
}

void GtrnctrFormat::gtc_wpt_ident(const QString& args, const QXmlStreamAttributes* /*unused*/)
{
  wpt_tmp->shortname = (args);
  /* Set also as notes for compatibility with garmin usb format */
  wpt_tmp->notes = (args);
}
void GtrnctrFormat::gtc_wpt_lat(const QString& args, const QXmlStreamAttributes* /*unused*/)
{
  wpt_tmp->latitude = args.toDouble();
}
void GtrnctrFormat::gtc_wpt_long(const QString& args, const QXmlStreamAttributes* /*unused*/)
{
  wpt_tmp->longitude = args.toDouble();
}
void GtrnctrFormat::gtc_wpt_icon(const QString& args, const QXmlStreamAttributes* /*unused*/)
{
  wpt_tmp->icon_descr = args;
}
void GtrnctrFormat::gtc_wpt_notes(const QString& args, const QXmlStreamAttributes* /*unused*/)
{
  wpt_tmp->description = args;
}
