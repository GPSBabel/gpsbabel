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

#include <QChar>                 // for QChar
#include <QDateTime>             // for operator>, QDateTime
#include <QIODevice>             // for QIODevice, operator|
#include <QLatin1Char>           // for QLatin1Char
#include <QStringLiteral>        // for qMakeStringPrivate, QStringLiteral
#include <QXmlStreamAttributes>  // for QXmlStreamAttributes
#include <QtGlobal>              // for qRound

#include <iterator>              // for size
#include <optional>              // for optional

#include "defs.h"                // for Waypoint, route_head, computed_trkdata, waypt_add, route_disp, track_disp_all, case_ignore_strncmp, track_add_head, track_add_wpt, track_recompute, xml_parse_time, unknown_alt, wp_flags
#include "xmlgeneric.h"          // for XmlGenericReader


const QString GtrnctrFormat::activity_extension_uri = "http://www.garmin.com/xmlschemas/ActivityExtension/v2";

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
  ofile = new gpsbabel::File(fname);
  ofile->open(QIODevice::WriteOnly | QIODevice::Text);

  fout = new gpsbabel::XmlStreamWriter(ofile);
  fout->setAutoFormatting(true);
  fout->setAutoFormattingIndent(2);

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
  fout->writeEndDocument();
  delete fout;
  fout = nullptr;
  ofile->close();
  delete ofile;
  ofile = nullptr;
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
    fout->writeStartElement("Trackpoint");
    fout->writeAttribute("split", "yes");
  } else {
    fout->writeStartElement("Trackpoint");
  }

  if (wpt->creation_time.isValid()) {
    QString time_string = wpt->CreationTimeXML();
    fout->writeOptionalTextElement("Time", time_string);
  }
  if (wpt->latitude && wpt->longitude) {
    fout->writeStartElement("Position");
    fout->writeTextElement("LatitudeDegrees", QString::number(wpt->latitude, 'f', 7));
    fout->writeTextElement("LongitudeDegrees", QString::number(wpt->longitude, 'f', 7));
    fout->writeEndElement(); // Position
  }
  if (wpt->altitude != unknown_alt) {
    fout->writeTextElement("AltitudeMeters", QString::number(wpt->altitude, 'f', 1));
  }
  if (wpt->odometer_distance) {
    fout->writeTextElement("DistanceMeters", QString::number(wpt->odometer_distance, 'f', 2));
  }
  // TODO: find a schema extension to include wpt->course and wpt->temperature
  // TODO: find a way to include DistanceMeters from odometer information
  if (wpt->heartrate) {
    //gtc_write_xml(0, "<HeartRateBpm>%d</HeartRateBpm>\n", wpt->heartrate);
    // FIXME need type?
    fout->writeStartElement("HeartRateBpm");
    //gtc_write_xml(1, "<HeartRateBpm xsi:type=\"HeartRateInBeatsPerMinute_t\">\n");
    fout->writeTextElement("Value", QString::number(wpt->heartrate));
    fout->writeEndElement(); // HeartRateBpm
  }
  if (wpt->cadence) {
    fout->writeTextElement("Cadence", QString::number(wpt->cadence));
  }
  if (wpt->speed_has_value() || wpt->power) {
    fout->writeStartElement("Extensions");
    // FIXME namespace
    fout->writeStartElement(activity_extension_uri, "TPX");
    //gtc_write_xml(1, "<TPX xmlns=\"http://www.garmin.com/xmlschemas/ActivityExtension/v2\">\n");
    /* see http://www8.garmin.com/xmlschemas/ActivityExtensionv2.xsd */
    if (wpt->speed_has_value()) {
      fout->writeTextElement(activity_extension_uri, "Speed", QString::number(wpt->speed_value(), 'f', 3));
    }
    if (wpt->power) {
      fout->writeTextElement("Watts", QString::number(wpt->power, 'f', 0));
    }
    fout->writeEndElement(); // TPX
    fout->writeEndElement(); // Extensions
  }

  fout->writeEndElement(); // Trackpoint
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
  fout->writeTextElement("TotalTimeSeconds", QString::number(secs));
  fout->writeTextElement("DistanceMeters", QString::number(tdata.distance_meters, 'f', 2));
  if (gtc_course_flag) { /* course format */
    fout->writeStartElement("BeginPosition");
    fout->writeTextElement("LatitudeDegrees", QString::number(gtc_start_lat, 'f'));
    fout->writeTextElement("LongitudeDegrees", QString::number(gtc_start_long, 'f'));
    fout->writeEndElement(); // BeginPosition
    fout->writeStartElement("EndPosition");
    fout->writeTextElement("LatitudeDegrees", QString::number(gtc_end_lat, 'f'));
    fout->writeTextElement("LongitudeDegrees", QString::number(gtc_end_long, 'f'));
    fout->writeEndElement(); // EndPosition

  } else {  /* activity (history) format */
    if (tdata.max_spd) {
      fout->writeTextElement("MaximumSpeed", QString::number(*tdata.max_spd, 'f', 3));
    }
    fout->writeTextElement("Calories", "0"); /* element is required */
  }
  if (tdata.avg_hrt) {
    // FIXME need type attribute?
    //gtc_write_xml(1, "<AverageHeartRateBpm xsi:type=\"HeartRateInBeatsPerMinute_t\">\n");
    fout->writeStartElement("AverageHeartRateBpm");
    fout->writeTextElement("Value", QString::number(qRound(*tdata.avg_hrt)));
    fout->writeEndElement(); // AverageHeartRateBpm
  }
  if (tdata.max_hrt) {
    // FIXME need type attribute?
    //gtc_write_xml(1, "<MaximumHeartRateBpm xsi:type=\"HeartRateInBeatsPerMinute_t\">\n");
    fout->writeStartElement("MaximumHeartRateBpm");
    fout->writeTextElement("Value", QString::number(*tdata.max_hrt));
    fout->writeEndElement(); // MaximumHeartRateBpm
  }
  fout->writeTextElement("Intensity", "Active");
  if (tdata.avg_cad) {
    fout->writeTextElement("Cadence", QString::number(qRound(*tdata.avg_cad)));
  }

  if (!gtc_course_flag) { /* activity (history) format */
    fout->writeTextElement("TriggerMethod", "Manual");
  }

}

void
GtrnctrFormat::gtc_act_hdr(const route_head* rte)
{
  fout->writeStartElement("Activity");
  fout->writeAttribute("Sport", gtc_sportlist[gtc_sport]);
  gtc_lap_start(nullptr);
  computed_trkdata tdata = gtc_new_study_lap(rte);
  auto gtc_study_lap_lambda = [this](const Waypoint* waypointp)->void {
    gtc_study_lap(waypointp);
  };
  route_disp(rte, gtc_study_lap_lambda);
  if (gtc_least_time.isValid()) {
    fout->writeTextElement("Id", gtc_least_time.toPrettyString());
    fout->writeStartElement("Lap");
    fout->writeAttribute("StartTime", gtc_least_time.toPrettyString());
  } else {
    fout->writeStartElement("Lap");
  }
  gtc_fake_hdr(tdata);
  fout->writeStartElement("Track");
}

void
GtrnctrFormat::gtc_act_ftr(const route_head* /*unused*/)
{
  fout->writeEndElement(); // Track
  fout->writeEndElement(); // Lap
  fout->writeEndElement(); // Activity
}

void
GtrnctrFormat::gtc_crs_hdr(const route_head* rte)
{

  fout->writeStartElement("Course");
  gtc_lap_start(nullptr);
  computed_trkdata tdata = gtc_new_study_lap(rte);
  auto gtc_study_lap_lambda = [this](const Waypoint* waypointp)->void {
    gtc_study_lap(waypointp);
  };
  route_disp(rte, gtc_study_lap_lambda);

  if (!rte->rte_name.isEmpty()) {
    QString name = rte->rte_name.left(kGtcMaxNameLen);
    fout->writeTextElement("Name", name);
  } else {
    fout->writeTextElement("Name", "New Course");
  }
  /* write_optional_xml_entity(ofd, "      ", "Name", rte->rte_name); */
  fout->writeStartElement("Lap");
  gtc_fake_hdr(tdata);
  fout->writeEndElement(); // Lap
  fout->writeStartElement("Track");
}

void
GtrnctrFormat::gtc_crs_ftr(const route_head* /*unused*/)
{
  fout->writeEndElement(); // Track
  fout->writeEndElement(); // Course

}

void
GtrnctrFormat::write()
{
  fout->writeStartDocument("1.0", false);
  fout->writeDefaultNamespace("http://www.garmin.com/xmlschemas/TrainingCenterDatabase/v2");
  fout->writeNamespace("http://www.w3.org/2001/XMLSchema-instance", "xsi");
  fout->writeNamespace(activity_extension_uri);
  fout->writeStartElement("TrainingCenterDatabase");
  fout->writeAttribute("xsi:schemaLocation", "http://www.garmin.com/xmlschemas/TrainingCenterDatabase/v2 http://www.garmin.com/xmlschemas/TrainingCenterDatabasev2.xsd");
  // FIXME change ok?
  //gtc_write_xml(1, "<TrainingCenterDatabase xmlns=\"http://www.garmin.com/xmlschemas/TrainingCenterDatabase/v2\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://www.garmin.com/xmlschemas/TrainingCenterDatabase/v2 http://www.garmin.com/xmlschemas/TrainingCenterDatabasev2.xsd\">\n");

  auto gtc_waypt_pr_lambda = [this](const Waypoint* waypointp)->void {
    gtc_waypt_pr(waypointp);
  };
  if (gtc_course_flag) {
    fout->writeStartElement("Courses");
    auto gtc_crs_hdr_lambda = [this](const route_head* rte)->void {
      gtc_crs_hdr(rte);
    };
    auto gtc_crs_ftr_lambda = [this](const route_head* rte)->void {
      gtc_crs_ftr(rte);
    };
    track_disp_all(gtc_crs_hdr_lambda, gtc_crs_ftr_lambda, gtc_waypt_pr_lambda);
    fout->writeEndElement(); // Courses
  } else {
    fout->writeStartElement("Activities");
    auto gtc_act_hdr_lambda = [this](const route_head* rte)->void {
      gtc_act_hdr(rte);
    };
    auto gtc_act_ftr_lambda = [this](const route_head* rte)->void {
      gtc_act_ftr(rte);
    };
    track_disp_all(gtc_act_hdr_lambda, gtc_act_ftr_lambda, gtc_waypt_pr_lambda);
    fout->writeEndElement(); // Activities
  }

  fout->writeEndElement(); // TrainingCenterDatabase
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
      auto* wpt_lap_s = new Waypoint(*wpt_tmp);
      wpt_lap_s->shortname = QStringLiteral("LAP%1").arg(lap_ct, 3, 10, QLatin1Char('0'));
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
