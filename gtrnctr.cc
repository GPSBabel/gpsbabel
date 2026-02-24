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
#include <QDebug>                // for QDebug
#include <QIODevice>             // for QIODevice, operator|
#include <QLatin1Char>           // for QLatin1Char
#include <QStringLiteral>        // for qMakeStringPrivate, QStringLiteral
#include <QXmlStreamAttributes>  // for QXmlStreamAttributes
#include <QtGlobal>              // for qRound

#include <optional>              // for optional

#include "defs.h"                // for Waypoint, route_head, computed_trkdata, waypt_add, route_disp, track_disp_all, gbFatal, track_add_head, track_add_wpt, track_recompute, xml_parse_time, unknown_alt, wp_flags
#include "src/core/logging.h"    // for FatalMsg
#include "xmlgeneric.h"          // for XmlGenericReader


const QString GtrnctrFormat::activity_extension_ns_uri = QStringLiteral("http://www.garmin.com/xmlschemas/ActivityExtension/v2");
const QString GtrnctrFormat::xsi_ns_uri = QStringLiteral("http://www.w3.org/2001/XMLSchema-instance");

const QStringList GtrnctrFormat::gtc_tags_to_ignore = {
  "TrainingCenterDatabase",
  "CourseFolder",
  "Running",
  "Biking",
  "Other",
  "Multisport"
};

const QStringList GtrnctrFormat::gtc_sportlist = { "Biking", "Running", "Other" };

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
    gtc_sport = -1;
    if (!opt_sport.isEmpty()) {
      for (int i = 0; i < gtc_sportlist.size(); ++i) {
        const auto& item = gtc_sportlist.at(i);
        if (item.startsWith(opt_sport, Qt::CaseInsensitive)) {
          gtc_sport = i;
          break;
        }
      }
    }
    if (gtc_sport < 0) {
      gbFatal(FatalMsg().nospace() << "Invalid sport: " << opt_sport.get() <<
              ".  Valid sports are : " << gtc_sportlist.join(u", "));
    }
  } else {
    gtc_sport = 0;
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
  if (wpt->creation_time.isValid()) {
    if (!gtc_least_time.isValid() || gtc_least_time > wpt->creation_time) {
      gtc_least_time = wpt->creation_time;
      gtc_start_lat = wpt->latitude;
      gtc_start_long = wpt->longitude;
    }
    if (!gtc_most_time.isValid() || wpt->creation_time > gtc_most_time)  {
      gtc_most_time = wpt->creation_time;
      gtc_end_lat = wpt->latitude;
      gtc_end_long = wpt->longitude;
    }
  }
}

void
GtrnctrFormat::gtc_waypt_pr(const Waypoint* wpt)
{
  // Time is a required element of the Trackpoint element.
  if (!wpt->creation_time.isValid()) {
    gbWarning("Skipping track point at %f,%f without time!\n", wpt->latitude, wpt->longitude);
    return;
  }

  fout->writeStartElement(QStringLiteral("Trackpoint"));
  // gpsbabel <= 1.10.0 conditionally wrote the split attribute, but
  // it violates the schema.
  //if (wpt->wpt_flags.is_split != 0) {
  //  fout->writeAttribute(QStringLiteral("split"), QStringLiteral("yes"));
  //}

  if (wpt->creation_time.isValid()) {
    fout->writeOptionalTextElement(QStringLiteral("Time"), wpt->CreationTimeXML());
  }
  if (wpt->latitude && wpt->longitude) {
    fout->writeStartElement(QStringLiteral("Position"));
    fout->writeTextElement(QStringLiteral("LatitudeDegrees"), QString::number(wpt->latitude, 'f', 7));
    fout->writeTextElement(QStringLiteral("LongitudeDegrees"), QString::number(wpt->longitude, 'f', 7));
    fout->writeEndElement(); // Position
  }
  if (wpt->altitude != unknown_alt) {
    fout->writeTextElement(QStringLiteral("AltitudeMeters"), QString::number(wpt->altitude, 'f', 1));
  }
  if (wpt->odometer_distance) {
    fout->writeTextElement(QStringLiteral("DistanceMeters"), QString::number(wpt->odometer_distance, 'f', 2));
  }
  // TODO: find a schema extension to include wpt->course and wpt->temperature
  if (wpt->heartrate) {
    fout->writeStartElement(QStringLiteral("HeartRateBpm"));
    // mimic gpsbabel <= 1.10.0 writing of xsi:type attribute.
    fout->writeAttribute(xsi_ns_uri, QStringLiteral("type"), QStringLiteral("HeartRateInBeatsPerMinute_t"));
    fout->writeTextElement(QStringLiteral("Value"), QString::number(wpt->heartrate));
    fout->writeEndElement(); // HeartRateBpm
  }
  if (wpt->cadence) {
    fout->writeTextElement(QStringLiteral("Cadence"), QString::number(wpt->cadence));
  }
  if (wpt->speed_has_value() || wpt->power) {
    fout->writeStartElement(QStringLiteral("Extensions"));
    // mimic our <= 1.10.0 writer which declared a new default namespace with the TPX element,
    // as opposed to more modern Garmin writers that use a prefix corresponding to an initially
    // declared non-default namespace.
    //fout->writeStartElement(activity_extension_ns_uri, QStringLiteral("TPX"));
    fout->writeStartElement(QStringLiteral("TPX"));
    fout->writeDefaultNamespace(activity_extension_ns_uri);
    /* see http://www8.garmin.com/xmlschemas/ActivityExtensionv2.xsd */
    if (wpt->speed_has_value()) {
      fout->writeTextElement(activity_extension_ns_uri, QStringLiteral("Speed"), QString::number(wpt->speed_value(), 'f', 3));
    }
    if (wpt->power) {
      fout->writeTextElement(activity_extension_ns_uri, QStringLiteral("Watts"), QString::number(wpt->power, 'f', 0));
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
  fout->writeTextElement(QStringLiteral("TotalTimeSeconds"), QString::number(secs));
  fout->writeTextElement(QStringLiteral("DistanceMeters"), QString::number(tdata.distance_meters, 'f', 2));
  if (gtc_course_flag) { /* course format */
    fout->writeStartElement(QStringLiteral("BeginPosition"));
    fout->writeTextElement(QStringLiteral("LatitudeDegrees"), QString::number(gtc_start_lat, 'f'));
    fout->writeTextElement(QStringLiteral("LongitudeDegrees"), QString::number(gtc_start_long, 'f'));
    fout->writeEndElement(); // BeginPosition
    fout->writeStartElement(QStringLiteral("EndPosition"));
    fout->writeTextElement(QStringLiteral("LatitudeDegrees"), QString::number(gtc_end_lat, 'f'));
    fout->writeTextElement(QStringLiteral("LongitudeDegrees"), QString::number(gtc_end_long, 'f'));
    fout->writeEndElement(); // EndPosition

  } else {  /* activity (history) format */
    if (tdata.max_spd) {
      fout->writeTextElement(QStringLiteral("MaximumSpeed"), QString::number(*tdata.max_spd, 'f', 3));
    }
    fout->writeTextElement(QStringLiteral("Calories"), QStringLiteral("0")); /* element is required */
  }
  if (tdata.avg_hrt) {
    fout->writeStartElement(QStringLiteral("AverageHeartRateBpm"));
    // mimic gpsbabel <= 1.10.0 writing of xsi:type attribute.
    fout->writeAttribute(xsi_ns_uri, QStringLiteral("type"), QStringLiteral("HeartRateInBeatsPerMinute_t"));
    fout->writeTextElement(QStringLiteral("Value"), QString::number(qRound(*tdata.avg_hrt)));
    fout->writeEndElement(); // AverageHeartRateBpm
  }
  if (tdata.max_hrt) {
    fout->writeStartElement(QStringLiteral("MaximumHeartRateBpm"));
    // mimic gpsbabel <= 1.10.0 writing of xsi:type attribute.
    fout->writeAttribute(xsi_ns_uri, QStringLiteral("type"), QStringLiteral("HeartRateInBeatsPerMinute_t"));
    fout->writeTextElement(QStringLiteral("Value"), QString::number(*tdata.max_hrt));
    fout->writeEndElement(); // MaximumHeartRateBpm
  }
  fout->writeTextElement(QStringLiteral("Intensity"), QStringLiteral("Active"));
  if (tdata.avg_cad) {
    fout->writeTextElement(QStringLiteral("Cadence"), QString::number(qRound(*tdata.avg_cad)));
  }

  if (!gtc_course_flag) { /* activity (history) format */
    fout->writeTextElement(QStringLiteral("TriggerMethod"), QStringLiteral("Manual"));
  }

}

void
GtrnctrFormat::gtc_act_hdr(const route_head* rte)
{
  fout->writeStartElement(QStringLiteral("Activity"));
  fout->writeAttribute(QStringLiteral("Sport"), gtc_sportlist[gtc_sport]);

  gtc_lap_start(nullptr);
  computed_trkdata tdata = gtc_new_study_lap(rte);
  auto gtc_study_lap_lambda = [this](const Waypoint* waypointp)->void {
    gtc_study_lap(waypointp);
  };
  route_disp(rte, gtc_study_lap_lambda);

  // FIXME: Id is required!
  if (gtc_least_time.isValid()) {
    fout->writeTextElement(QStringLiteral("Id"), gtc_least_time.toPrettyString());
    fout->writeStartElement(QStringLiteral("Lap"));
    fout->writeAttribute(QStringLiteral("StartTime"), gtc_least_time.toPrettyString());
  } else {
    fout->writeStartElement(QStringLiteral("Lap"));
  }
  gtc_fake_hdr(tdata);
  fout->writeStartElement(QStringLiteral("Track"));
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
  fout->writeStartElement(QStringLiteral("Course"));

  gtc_lap_start(nullptr);
  computed_trkdata tdata = gtc_new_study_lap(rte);
  auto gtc_study_lap_lambda = [this](const Waypoint* waypointp)->void {
    gtc_study_lap(waypointp);
  };
  route_disp(rte, gtc_study_lap_lambda);

  if (!rte->rte_name.isEmpty()) {
    QString name = rte->rte_name.left(kGtcMaxNameLen);
    fout->writeTextElement(QStringLiteral("Name"), name);
  } else {
    fout->writeTextElement(QStringLiteral("Name"), QStringLiteral("New Course"));
  }
  /* write_optional_xml_entity(ofd, "      ", "Name", rte->rte_name); */
  fout->writeStartElement(QStringLiteral("Lap"));
  gtc_fake_hdr(tdata);
  fout->writeEndElement(); // Lap
  fout->writeStartElement(QStringLiteral("Track"));
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
  fout->writeStartDocument(QStringLiteral("1.0"), false);
  fout->writeDefaultNamespace(QStringLiteral("http://www.garmin.com/xmlschemas/TrainingCenterDatabase/v2"));
  fout->writeNamespace(xsi_ns_uri, QStringLiteral("xsi"));
  // See other comment "mimic our <= 1.10.0 writer which declared a new default namespace ..."
  //fout->writeNamespace(activity_extension_ns_uri);
  fout->writeStartElement(QStringLiteral("TrainingCenterDatabase"));
  fout->writeAttribute(QStringLiteral("xsi:schemaLocation"),
                       QStringLiteral("http://www.garmin.com/xmlschemas/TrainingCenterDatabase/v2 http://www.garmin.com/xmlschemas/TrainingCenterDatabasev2.xsd"));

  auto gtc_waypt_pr_lambda = [this](const Waypoint* waypointp)->void {
    gtc_waypt_pr(waypointp);
  };
  if (gtc_course_flag) {
    fout->writeStartElement(QStringLiteral("Courses"));
    auto gtc_crs_hdr_lambda = [this](const route_head* rte)->void {
      gtc_crs_hdr(rte);
    };
    auto gtc_crs_ftr_lambda = [this](const route_head* rte)->void {
      gtc_crs_ftr(rte);
    };
    track_disp_all(gtc_crs_hdr_lambda, gtc_crs_ftr_lambda, gtc_waypt_pr_lambda);
    fout->writeEndElement(); // Courses
  } else {
    fout->writeStartElement(QStringLiteral("Activities"));
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
