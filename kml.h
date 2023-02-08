/*
	Support for Google Earth & Keyhole "kml" format.

	Copyright (C) 2005-2013 Robert Lipe, robertlipe+source@gpsbabel.org
	Updates by Andrew Kirmse, akirmse at google.com

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
#ifndef KML_H_INCLUDED_
#define KML_H_INCLUDED_

#include <tuple>                        // for tuple, make_tuple, tie

#include <QList>                        // for QList
#include <QString>                      // for QString, QStringLiteral, operator+, operator!=
#include <QVector>                      // for QVector
#include <QXmlStreamAttributes>         // for QXmlStreamAttributes

#include "defs.h"
#include "format.h"
#include "src/core/datetime.h"          // for DateTime
#include "src/core/file.h"              // for File
#include "src/core/xmlstreamwriter.h"   // for XmlStreamWriter
#include "units.h"                      // for UnitsFormatter
#include "xmlgeneric.h"                 // for cb_cdata, cb_end, cb_start, xg_callback, xg_string, xg_cb_type, xml_deinit, xml_ignore_tags, xml_init, xml_read, xg_tag_mapping


class KmlFormat : public Format
{
public:
  QVector<arglist_t>* get_args() override
  {
    return &kml_args;
  }

  ff_type get_type() const override
  {
    return ff_type_file;
  }

  QVector<ff_cap> get_cap() const override
  {
    return FF_CAP_RW_ALL;
  }

  void rd_init(const QString& fname) override;
  void read() override;
  void rd_deinit() override;
  void wr_init(const QString& fname) override;
  void write() override;
  void wr_deinit() override;
  void wr_position_init(const QString& fname) override;
  void wr_position(Waypoint* wpt) override;
  void wr_position_deinit() override;

private:
  /* Types */

  enum kml_point_type {
    kmlpt_unknown,
    kmlpt_waypoint,
    kmlpt_track,
    kmlpt_route,
    kmlpt_multitrack,
    kmlpt_other
  };

// Helper to write gx:SimpleList, iterating over a route queue and writing out.

  enum wp_field {
    fld_cadence,
    fld_depth,
    fld_heartrate,
    fld_temperature,
    fld_power
  };

  /* Constants */
  static constexpr const char* default_precision = "6";
  static constexpr int kml_color_limit = 204;	/* allowed range [0,255] */

  static constexpr const char* kml_tags_to_ignore[] = {
    "kml",
    "Document",
    "Folder",
    nullptr
  };

  static constexpr const char* kml_tags_to_skip[] = {
    "Camera",
    "LookAt",
    "styleUrl",
    "snippet",
    nullptr
  };

  // Multitrack ids to correlate Schema to SchemaData
  static constexpr const char* kmt_heartrate = "heartrate";
  static constexpr const char* kmt_cadence = "cadence";
  static constexpr const char* kmt_temperature = "temperature";
  static constexpr const char* kmt_depth = "depth";
  static constexpr const char* kmt_power = "power";

  /* Member Functions */

  void kml_init_color_sequencer(unsigned int steps_per_rev);
  void kml_step_color();
  void wpt_s(const QString& args, const QXmlStreamAttributes* attrs);
  void wpt_e(const QString& args, const QXmlStreamAttributes* attrs);
  void wpt_name(const QString& args, const QXmlStreamAttributes* attrs);
  void wpt_desc(const QString& args, const QXmlStreamAttributes* attrs);
  void wpt_coord(const QString& args, const QXmlStreamAttributes* attrs);
  void wpt_icon(const QString& args, const QXmlStreamAttributes* attrs);
  void trk_coord(const QString& args, const QXmlStreamAttributes* attrs);
  void wpt_time(const QString& args, const QXmlStreamAttributes* attrs);
  void wpt_ts_begin(const QString& args, const QXmlStreamAttributes* attrs);
  void wpt_ts_end(const QString& args, const QXmlStreamAttributes* attrs);
  void gx_trk_s(const QString& args, const QXmlStreamAttributes* attrs);
  void gx_trk_e(const QString& args, const QXmlStreamAttributes* attrs);
  void gx_trk_when(const QString& args, const QXmlStreamAttributes* attrs);
  void gx_trk_coord(const QString& args, const QXmlStreamAttributes* attrs);
  void kml_output_linestyle(char* color, int width) const;
  void kml_write_bitmap_style_(const QString& style, const QString& bitmap, bool highlighted, bool force_heading) const;
  void kml_write_bitmap_style(kml_point_type pt_type, const QString& bitmap, const QString& customstyle) const;
  void kml_output_timestamp(const Waypoint* waypointp) const;
  static void kml_td(gpsbabel::XmlStreamWriter& hwriter, const QString& boldData, const QString& data);
  static void kml_td(gpsbabel::XmlStreamWriter& hwriter, const QString& data);
  void kml_output_trkdescription(const route_head* header, const computed_trkdata* td) const;
  void kml_output_header(const route_head* header, const computed_trkdata* td) const;
  static bool kml_altitude_known(const Waypoint* waypoint);
  void kml_write_coordinates(const Waypoint* waypointp) const;
  void kml_output_lookat(const Waypoint* waypointp) const;
  void kml_output_positioning(bool tessellate) const;
  void kml_output_description(const Waypoint* pt) const;
  void kml_recompute_time_bounds(const Waypoint* waypointp);
  void kml_add_to_bounds(const Waypoint* waypointp);
  void kml_output_point(const Waypoint* waypointp, kml_point_type pt_type) const;
  void kml_output_tailer(const route_head* header);
  static void kml_gc_all_tabs_text(QString& cdataStr);
  void kml_gc_make_balloonstyletext() const;
  void kml_gc_make_balloonstyle() const;
  static QString kml_lookup_gc_icon(const Waypoint* waypointp);
  static const char* kml_lookup_gc_container(const Waypoint* waypointp);
  static QString kml_gc_mkstar(int rating);
  QString kml_geocache_get_logs(const Waypoint* wpt) const;
  void kml_write_data_element(const QString& name, const QString& value) const;
  void kml_write_data_element(const QString& name, int value) const;
  void kml_write_data_element(const QString& name, double value) const;
  void kml_write_cdata_element(const QString& name, const QString& value) const;
  void kml_geocache_pr(const Waypoint* waypointp) const;
  void kml_waypt_pr(const Waypoint* waypointp) const;
  void kml_track_hdr(const route_head* header) const;
  void kml_track_disp(const Waypoint* waypointp) const;
  void kml_track_tlr(const route_head* header);
  void kml_mt_simple_array(const route_head* header, const char* name, wp_field member) const;
  static bool track_has_time(const route_head* header);
  void write_as_linestring(const route_head* header);
  void kml_mt_hdr(const route_head* header);
  void kml_mt_tlr(const route_head* header) const;
  void kml_route_hdr(const route_head* header) const;
  void kml_route_disp(const Waypoint* waypointp) const;
  void kml_route_tlr(const route_head* header);
  void kml_write_AbstractView();
  void kml_mt_array_schema(const char* field_name, const char* display_name, const char* type) const;
  static QString kml_get_posn_icon(int freshness);

  /* Data Members */

  // options
  char* opt_deficon{nullptr};
  char* opt_export_lines{nullptr};
  char* opt_export_points{nullptr};
  char* opt_export_track{nullptr};
  char* opt_line_width{nullptr};
  char* opt_line_color{nullptr};
  char* opt_floating{nullptr};
  char* opt_extrude{nullptr};
  char* opt_trackdata{nullptr};
  char* opt_trackdirection{nullptr};
  char* opt_units{nullptr};
  char* opt_labels{nullptr};
  char* opt_max_position_points{nullptr};
  char* opt_rotate_colors{nullptr};
  char* opt_precision{nullptr};

  bool export_lines{};
  bool export_points{};
  bool export_track{};
  bool floating{};
  bool extrude{};
  bool trackdata{};
  bool trackdirection{};
  int max_position_points{};
  bool rotate_colors{};
  int line_width{};
  int precision{};

  Waypoint* wpt_tmp{nullptr};
  bool wpt_tmp_queued{false};
  QString posnfilename;
  QString posnfilenametmp;

  route_head* gx_trk_head{nullptr};
  QList<gpsbabel::DateTime>* gx_trk_times{nullptr};
  QList<std::tuple<int, double, double, double>>* gx_trk_coords{nullptr};

  UnitsFormatter* unitsformatter{nullptr};
  gpsbabel::File* oqfile{nullptr};
  gpsbabel::XmlStreamWriter* writer{nullptr};

  bool realtime_positioning{};
  bounds kml_bounds{};
  gpsbabel::DateTime kml_time_max;
  gpsbabel::DateTime kml_time_min;

  QVector<arglist_t> kml_args = {
    {"deficon", &opt_deficon, "Default icon name", nullptr, ARGTYPE_STRING, ARG_NOMINMAX, nullptr },
    {
      "lines", &opt_export_lines,
      "Export linestrings for tracks and routes",
      "1", ARGTYPE_BOOL, ARG_NOMINMAX, nullptr,
    },
    {
      "points", &opt_export_points,
      "Export placemarks for tracks and routes",
      "1", ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    {
      "line_width", &opt_line_width,
      "Width of lines, in pixels",
      "6", ARGTYPE_INT, ARG_NOMINMAX, nullptr
    },
    {
      "line_color", &opt_line_color,
      "Line color, specified in hex AABBGGRR",
      "99ffac59", ARGTYPE_STRING, ARG_NOMINMAX, nullptr
    },
    {
      "floating", &opt_floating,
      "Altitudes are absolute and not clamped to ground",
      "0", ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    {
      "extrude", &opt_extrude,
      "Draw extrusion line from trackpoint to ground",
      "0", ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    {
      "track", &opt_export_track,
      "Write KML track (default = 0)",
      "0", ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    {
      "trackdata", &opt_trackdata,
      "Include extended data for trackpoints (default = 1)",
      "1", ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    {
      "trackdirection", &opt_trackdirection,
      "Indicate direction of travel in track icons (default = 0)",
      "0", ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    {
      "units", &opt_units,
      "Units used when writing comments ('s'tatute, 'm'etric,' 'n'autical, 'a'viation)",
      "s", ARGTYPE_STRING, ARG_NOMINMAX, nullptr
    },
    {
      "labels", &opt_labels,
      "Display labels on track and routepoints  (default = 1)",
      "1", ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    {
      "max_position_points", &opt_max_position_points,
      "Retain at most this number of position points  (0 = unlimited)",
      "0", ARGTYPE_INT, ARG_NOMINMAX, nullptr
    },
    {
      "rotate_colors", &opt_rotate_colors,
      "Rotate colors for tracks and routes (default automatic)",
      nullptr, ARGTYPE_FLOAT, "0", "360", nullptr
    },
    {
      "prec", &opt_precision,
      "Precision of coordinates, number of decimals",
      default_precision, ARGTYPE_INT, ARG_NOMINMAX, nullptr
    },
  };

  struct {
    float seq{0.0f};
    float step{0.0f};
    gb_color color;
  } kml_color_sequencer;

  QList<xg_functor_map_entry<KmlFormat>> kml_map = {
    {&KmlFormat::wpt_s, cb_start, "/Placemark"},
    {&KmlFormat::wpt_e, cb_end, "/Placemark"},
    {&KmlFormat::wpt_name, cb_cdata, "/Placemark/name"},
    {&KmlFormat::wpt_desc, cb_cdata, "/Placemark/description"},
    {&KmlFormat::wpt_ts_begin, cb_cdata,"/Placemark/TimeSpan/begin"},
    {&KmlFormat::wpt_ts_end, cb_cdata, "/Placemark/TimeSpan/end"},
    {&KmlFormat::wpt_time, cb_cdata, "/Placemark/TimeStamp/when"},
    // Alias for above used in KML 2.0
    {&KmlFormat::wpt_time, cb_cdata, "/Placemark/TimeInstant/timePosition"},
    {&KmlFormat::wpt_coord, cb_cdata, "/Placemark/Point/coordinates"},
    {&KmlFormat::wpt_coord, cb_cdata, "/Placemark/MultiGeometry/Point/coordinates"},
    {&KmlFormat::wpt_icon, cb_cdata, "/Placemark/Style/Icon/href"},
    {&KmlFormat::trk_coord, cb_cdata, "/Placemark/MultiGeometry/LineString/coordinates"},
    {&KmlFormat::trk_coord, cb_cdata, "/Placemark/GeometryCollection/LineString/coordinates"},
    {&KmlFormat::trk_coord, cb_cdata, "/Placemark/Polygon/outerBoundaryIs/LinearRing/coordinates"},
    {&KmlFormat::trk_coord, cb_cdata, "/Placemark/LineString/coordinates"},
    {&KmlFormat::gx_trk_s,  cb_start, "/Placemark/*gx:Track"},
    {&KmlFormat::gx_trk_e,  cb_end, "/Placemark/*gx:Track"},
    {&KmlFormat::gx_trk_when,  cb_cdata, "/Placemark/*gx:Track/when"},
    {&KmlFormat::gx_trk_coord, cb_cdata, "/Placemark/*gx:Track/gx:coord"},
    {&KmlFormat::gx_trk_s,  cb_start, "/Placemark/Track"}, // KML 2.3
    {&KmlFormat::gx_trk_e,  cb_end, "/Placemark/Track"}, // KML 2.3
    {&KmlFormat::gx_trk_when,  cb_cdata, "/Placemark/Track/when"}, // KML 2.3
    {&KmlFormat::gx_trk_coord, cb_cdata, "/Placemark/Track/coord"}, // KML 2.3
    {&KmlFormat::gx_trk_s,  cb_start, "/Placemark/MultiTrack/Track"}, // KML 2.3
    {&KmlFormat::gx_trk_e,  cb_end, "/Placemark/MultiTrack/Track"}, // KML 2.3
    {&KmlFormat::gx_trk_when,  cb_cdata, "/Placemark/MultiTrack/Track/when"}, // KML 2.3
    {&KmlFormat::gx_trk_coord, cb_cdata, "/Placemark/MultiTrack/Track/coord"} // KML 2.3
  };

  // The TimeSpan/begin and TimeSpan/end DateTimes:
  gpsbabel::DateTime wpt_timespan_begin, wpt_timespan_end;

  static const QString map_templates[];

  route_head* posn_trk_head{nullptr};
};

#endif // KML_H_INCLUDED_
