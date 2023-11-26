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
#ifndef GTRNCTR_H_INCLUDED_
#define GTRNCTR_H_INCLUDED_

#include <QList>                 // for QList
#include <QString>               // for QString
#include <QVector>               // for QVector
#include <QXmlStreamAttributes>  // for QXmlStreamAttributes

#include "defs.h"                // for arglist_t, ff_cap, route_head, Waypoint, computed_trkdata, ARG_NOMINMAX, ff_cap_read, ARGTYPE_BOOL, ARGTYPE_STRING, ff_cap_none, ff_cap_write, ff_type, ff_type_file
#include "format.h"              // for Format
#include "gbfile.h"              // for gbfile
#include "src/core/datetime.h"   // for DateTime
#include "xmlgeneric.h"          // for cb_cdata, xg_functor_map_entry, xg_string, cb_start, cb_end


class GtrnctrFormat : public Format
{
public:
  QVector<arglist_t>* get_args() override
  {
    return &gtc_args;
  }

  ff_type get_type() const override
  {
    return ff_type_file;
  }

  QVector<ff_cap> get_cap() const override
  {
    return {
      ff_cap_read 			/* waypoints */,
      (ff_cap)(ff_cap_read | ff_cap_write) 	/* tracks */,
      ff_cap_none 			/* routes */
    };
  }

  void rd_init(const QString& fname) override;
  void read() override;
  void rd_deinit() override;
  void wr_init(const QString& fname) override;
  void write() override;
  void wr_deinit() override;

private:
  /* Constants */

  static constexpr int kGtcMaxNameLen = 15;
  static constexpr const char* gtc_sportlist[] = { "Biking", "Running", "MultiSport", "Other" };

  static constexpr const char* gtc_tags_to_ignore[] = {
    "TrainingCenterDatabase",
    "CourseFolder",
    "Running",
    "Biking",
    "Other",
    "Multisport",
    nullptr,
  };

  /* Member Functions */

  [[gnu::format(printf, 3, 4)]] void gtc_write_xml(int indent, const char* fmt, ...);
  void gtc_write_xml(int indent, const QString& s);
  void gtc_lap_start(const route_head*  /* unused */);
  static computed_trkdata gtc_new_study_lap(const route_head* rte);
  void gtc_study_lap(const Waypoint* wpt);
  void gtc_waypt_pr(const Waypoint* wpt);
  void gtc_fake_hdr(const computed_trkdata& tdata);
  void gtc_act_hdr(const route_head* rte);
  void gtc_act_ftr(const route_head*  /* unused */);
  void gtc_crs_hdr(const route_head* rte);
  void gtc_crs_ftr(const route_head*  /* unused */);

  void gtc_trk_s(xg_string  /* unused */, const QXmlStreamAttributes*  /* unused */);
  void gtc_trk_ident(xg_string args, const QXmlStreamAttributes*  /* unused */);
  void gtc_trk_lap_s(xg_string  /* unused */, const QXmlStreamAttributes*  /* unused */);
  void gtc_trk_lap_e(xg_string  /* unused */, const QXmlStreamAttributes*  /* unused */);
  void gtc_trk_pnt_s(xg_string  /* unused */, const QXmlStreamAttributes*  /* unused */);
  void gtc_trk_pnt_e(xg_string  /* unused */, const QXmlStreamAttributes*  /* unused */);
  void gtc_trk_utc(xg_string args, const QXmlStreamAttributes*  /* unused */);
  void gtc_trk_lat(xg_string args, const QXmlStreamAttributes*  /* unused */);
  void gtc_trk_long(xg_string args, const QXmlStreamAttributes*  /* unused */);
  void gtc_trk_alt(xg_string args, const QXmlStreamAttributes*  /* unused */);
  void gtc_trk_dist(const QString& args, const QXmlStreamAttributes*  /* unused */);
  void gtc_trk_hr(const QString& args, const QXmlStreamAttributes*  /* unused */);
  void gtc_trk_cad(const QString& args, const QXmlStreamAttributes*  /* unused */);
  void gtc_trk_pwr(xg_string args, const QXmlStreamAttributes*  /* unused */);
  void gtc_trk_spd(xg_string args, const QXmlStreamAttributes*  /* unused */);
  void gtc_wpt_crs_s(const QString&  /* unused */, const QXmlStreamAttributes*  /* unused */);
  void gtc_wpt_crs_e(xg_string  /* unused */, const QXmlStreamAttributes*  /* unused */);
  void gtc_wpt_pnt_s(xg_string  /* unused */, const QXmlStreamAttributes*  /* unused */);
  void gtc_wpt_pnt_e(xg_string  /* unused */, const QXmlStreamAttributes*  /* unused */);
  void gtc_wpt_ident(const QString& args, const QXmlStreamAttributes*  /* unused */);
  void gtc_wpt_lat(const QString& args, const QXmlStreamAttributes*  /* unused */);
  void gtc_wpt_long(const QString& args, const QXmlStreamAttributes*  /* unused */);
  void gtc_wpt_icon(const QString& args, const QXmlStreamAttributes*  /* unused */);
  void gtc_wpt_notes(const QString& args, const QXmlStreamAttributes*  /* unused */);

  /* Data Members */

  gbfile* ofd{};
  int lap_ct = 0;
  int lap_s = 0;
  Waypoint* wpt_tmp{};
  route_head* trk_head{};

  unsigned int gtc_sport = 0;
  int gtc_course_flag{};

  gpsbabel::DateTime gtc_least_time;
  gpsbabel::DateTime gtc_most_time;
  double gtc_start_lat{};
  double gtc_start_long{};
  double gtc_end_lat{};
  double gtc_end_long{};

  char* opt_sport{};
  char* opt_course{};

  QVector<arglist_t> gtc_args = {
    {
      "course", &opt_course, "Write course rather than history, default yes",
      "1", ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    {
      "sport", &opt_sport, "Sport: Biking (deflt), Running, MultiSport, Other",
      "Biking", ARGTYPE_STRING, ARG_NOMINMAX, nullptr
    },
  };

  QList<xg_fmt_map_entry<GtrnctrFormat>> gtc_map = {
    /* courses tcx v1 & v2 */
    { &GtrnctrFormat::gtc_trk_s,    cb_start, "/Courses/Course" },
    { &GtrnctrFormat::gtc_trk_ident,cb_cdata, "/Courses/Course/Name"},
    { &GtrnctrFormat::gtc_trk_pnt_s,cb_start, "/Courses/Course/Track/Trackpoint" },
    { &GtrnctrFormat::gtc_trk_pnt_e,cb_end,   "/Courses/Course/Track/Trackpoint" },
    { &GtrnctrFormat::gtc_trk_utc,  cb_cdata, "/Courses/Course/Track/Trackpoint/Time" },
    { &GtrnctrFormat::gtc_trk_lat,  cb_cdata, "/Courses/Course/Track/Trackpoint/Position/LatitudeDegrees" },
    { &GtrnctrFormat::gtc_trk_long, cb_cdata, "/Courses/Course/Track/Trackpoint/Position/LongitudeDegrees" },
    { &GtrnctrFormat::gtc_trk_alt,  cb_cdata, "/Courses/Course/Track/Trackpoint/AltitudeMeters" },
    { &GtrnctrFormat::gtc_trk_hr,   cb_cdata, "/Courses/Course/Track/Trackpoint/HeartRateBpm" },
    { &GtrnctrFormat::gtc_trk_cad,  cb_cdata, "/Courses/Course/Track/Trackpoint/Cadence" },
    { &GtrnctrFormat::gtc_wpt_crs_s,cb_start, "/Courses/Course/CoursePoint" },
    { &GtrnctrFormat::gtc_wpt_crs_e,cb_end,   "/Courses/Course/CoursePoint" },
    { &GtrnctrFormat::gtc_wpt_ident,cb_cdata, "/Courses/Course/CoursePoint/Name"},
    { &GtrnctrFormat::gtc_trk_utc,  cb_cdata, "/Courses/Course/CoursePoint/Time"},
    { &GtrnctrFormat::gtc_wpt_lat,  cb_cdata, "/Courses/Course/CoursePoint/Position/LatitudeDegrees"},
    { &GtrnctrFormat::gtc_wpt_long, cb_cdata, "/Courses/Course/CoursePoint/Position/LongitudeDegrees"},
    { &GtrnctrFormat::gtc_trk_alt,  cb_cdata, "/Courses/Course/CoursePoint/AltitudeMeters" },
    { &GtrnctrFormat::gtc_wpt_icon, cb_cdata, "/Courses/Course/CoursePoint/PointType" },
    { &GtrnctrFormat::gtc_wpt_notes,cb_cdata, "/Courses/Course/CoursePoint/Notes" },

    /* history tcx v2 (activities) */
    { &GtrnctrFormat::gtc_trk_s,    cb_start, "/Activities/Activity" },
    { &GtrnctrFormat::gtc_trk_ident,cb_cdata, "/Activities/Activity/Id" },
    { &GtrnctrFormat::gtc_trk_lap_s,cb_start, "/Activities/Activity/Lap" },
    { &GtrnctrFormat::gtc_trk_lap_e,cb_end,   "/Activities/Activity/Lap" },
    { &GtrnctrFormat::gtc_trk_pnt_s,cb_start, "/Activities/Activity/Lap/Track/Trackpoint" },
    { &GtrnctrFormat::gtc_trk_pnt_e,cb_end,   "/Activities/Activity/Lap/Track/Trackpoint" },
    { &GtrnctrFormat::gtc_trk_utc,  cb_cdata, "/Activities/Activity/Lap/Track/Trackpoint/Time" },
    { &GtrnctrFormat::gtc_trk_lat,  cb_cdata, "/Activities/Activity/Lap/Track/Trackpoint/Position/LatitudeDegrees" },
    { &GtrnctrFormat::gtc_trk_long, cb_cdata, "/Activities/Activity/Lap/Track/Trackpoint/Position/LongitudeDegrees" },
    { &GtrnctrFormat::gtc_trk_alt,  cb_cdata, "/Activities/Activity/Lap/Track/Trackpoint/AltitudeMeters" },
    { &GtrnctrFormat::gtc_trk_dist, cb_cdata, "/Activities/Activity/Lap/Track/Trackpoint/DistanceMeters" },
    { &GtrnctrFormat::gtc_trk_hr,   cb_cdata, "/Activities/Activity/Lap/Track/Trackpoint/HeartRateBpm" },
    { &GtrnctrFormat::gtc_trk_cad,  cb_cdata, "/Activities/Activity/Lap/Track/Trackpoint/Cadence" },
    { &GtrnctrFormat::gtc_trk_pwr,  cb_cdata, "/Activities/Activity/Lap/Track/Trackpoint/Extensions/ns3:TPX/ns3:Watts" },
    // Sample from Marcelo Kittlein 5/2014 declares a default namespace with the start tag of the TPX element,
    // and thus doesn't use prefixes.
    { &GtrnctrFormat::gtc_trk_pwr,  cb_cdata, "/Activities/Activity/Lap/Track/Trackpoint/Extensions/TPX/Watts" },
    // It looks like Speed and Watts should be siblings, but Garmin can't get
    // their namespace act very consistent.  This works for a sample provided
    // by Laurent Desmons in 5/2013.
    { &GtrnctrFormat::gtc_trk_spd,  cb_cdata, "/Activities/Activity/Lap/Track/Trackpoint/Extensions/TPX/Speed" },

    /* history tcx v1 */
    { &GtrnctrFormat::gtc_trk_s,    cb_start, "/History/Run" },
    { &GtrnctrFormat::gtc_trk_ident,cb_cdata, "/History/Run/Id" },
    { &GtrnctrFormat::gtc_trk_lap_s,cb_start, "/History/Run/Lap" },
    { &GtrnctrFormat::gtc_trk_lap_e,cb_end,   "/History/Run/Lap" },
    { &GtrnctrFormat::gtc_trk_pnt_s,cb_start, "/History/Run/Lap/Track/Trackpoint" },
    { &GtrnctrFormat::gtc_trk_pnt_e,cb_end,   "/History/Run/Lap/Track/Trackpoint" },
    { &GtrnctrFormat::gtc_trk_utc,  cb_cdata, "/History/Run/Lap/Track/Trackpoint/Time" },
    { &GtrnctrFormat::gtc_trk_lat,  cb_cdata, "/History/Run/Lap/Track/Trackpoint/Position/LatitudeDegrees" },
    { &GtrnctrFormat::gtc_trk_long, cb_cdata, "/History/Run/Lap/Track/Trackpoint/Position/LongitudeDegrees" },
    { &GtrnctrFormat::gtc_trk_alt,  cb_cdata, "/History/Run/Lap/Track/Trackpoint/AltitudeMeters" },
    { &GtrnctrFormat::gtc_trk_hr,   cb_cdata, "/History/Run/Lap/Track/Trackpoint/HeartRateBpm" },
    { &GtrnctrFormat::gtc_trk_cad,  cb_cdata, "/History/Run/Lap/Track/Trackpoint/Cadence" },

    { &GtrnctrFormat::gtc_wpt_pnt_s,cb_start, "/Courses/Course/Lap/BeginPosition" },
    { &GtrnctrFormat::gtc_wpt_pnt_e,cb_end, "/Courses/Course/Lap/BeginPosition" },
    { &GtrnctrFormat::gtc_wpt_lat,  cb_cdata, "/Courses/Course/Lap/BeginPosition/LatitudeDegrees" },
    { &GtrnctrFormat::gtc_wpt_long, cb_cdata, "/Courses/Course/Lap/BeginPosition/LongitudeDegrees" },
    { &GtrnctrFormat::gtc_trk_alt,  cb_cdata, "/Courses/Course/Lap/BeginAltitudeMeters" }
  };
  XmlGenericReader* xml_reader{nullptr};

  int gtc_indent_level{};
};
#endif // GTRNCTR_H_INCLUDED_
