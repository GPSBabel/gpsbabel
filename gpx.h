/*
    Access GPX data files.

    Copyright (C) 2002-2015 Robert Lipe, gpsbabel.org

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
#ifndef GPX_H_INCLUDED_
#define GPX_H_INCLUDED_

#include <QtCore/QHash>                 // for QHash
#include <QtCore/QString>               // for QString
#include <QtCore/QStringList>           // for QStringList
#include <QtCore/QVector>               // for QVector
#include <QtCore/QVersionNumber>        // for QVersionNumber
#include <QtCore/QXmlStreamAttributes>  // for QXmlStreamAttributes
#include <QtCore/QXmlStreamReader>      // for QXmlStreamReader

#include "defs.h"
#include "format.h"                     // for Format
#include "src/core/file.h"              // for File
#include "src/core/xmlstreamwriter.h"   // for XmlStreamWriter
#include "src/core/xmltag.h"            // for xml_tag


class GpxFormat : public Format
{
public:
  QVector<arglist_t>* get_args() override
  {
    return &gpx_args;
  }

  ff_type get_type() const override
  {
    return ff_type_file;
  }

  QVector<ff_cap> get_cap() const override
  {
    return FF_CAP_RW_ALL;
  }

  QString get_encode() const override
  {
    return CET_CHARSET_UTF8;
  }

  int get_fixed_encode() const override
  {
    return 0;
  }

  void rd_init(const QString& fname) override;
  void read() override;
  void rd_deinit() override;
  void wr_init(const QString& fname) override;
  void write() override;
  void wr_deinit() override;
  void exit() override;

private:
  enum gpx_point_type {
    gpxpt_waypoint,
    gpxpt_track,
    gpxpt_route
  };

  enum tag_type {
    tt_unknown = 0,
    tt_gpx,

    tt_name,		/* Optional file-level info */
    tt_desc,
    tt_author,
    tt_email,
    tt_url,
    tt_urlname,
    tt_keywords,
    tt_link,
    tt_link_text,
    tt_link_type,

    tt_wpt,
    tt_wpttype_ele,
    tt_wpttype_time,
    tt_wpttype_geoidheight,
    tt_wpttype_name,
    tt_wpttype_cmt,
    tt_wpttype_desc,
    tt_wpttype_url,		/* Not in GPX 1.1 */
    tt_wpttype_urlname,	/* Not in GPX 1.1 */
    tt_wpttype_link,		/* New in GPX 1.1 */
    tt_wpttype_link_text,	/* New in GPX 1.1 */
    tt_wpttype_link_type,	/* New in GPX 1.1 */
    tt_wpttype_sym,
    tt_wpttype_type,
    tt_wpttype_fix,
    tt_wpttype_sat,
    tt_wpttype_hdop,		/* HDOPS are common for all three */
    tt_wpttype_vdop,		/* VDOPS are common for all three */
    tt_wpttype_pdop,		/* PDOPS are common for all three */
    tt_cache,
    tt_cache_name,
    tt_cache_container,
    tt_cache_type,
    tt_cache_difficulty,
    tt_cache_terrain,
    tt_cache_hint,
    tt_cache_desc_short,
    tt_cache_desc_long,
    tt_cache_log_wpt,
    tt_cache_log_type,
    tt_cache_log_date,
    tt_cache_placer,
    tt_cache_favorite_points,
    tt_cache_personal_note,

    tt_wpt_extensions,

    tt_garmin_wpt_extensions,	/* don't change this order */
    tt_garmin_wpt_proximity,
    tt_garmin_wpt_temperature,
    tt_garmin_wpt_depth,
    tt_garmin_wpt_display_mode,
    tt_garmin_wpt_categories,
    tt_garmin_wpt_category,
    tt_garmin_wpt_addr,
    tt_garmin_wpt_city,
    tt_garmin_wpt_state,
    tt_garmin_wpt_country,
    tt_garmin_wpt_postal_code,
    tt_garmin_wpt_phone_nr,		/* don't change this order */

    tt_rte,
    tt_rte_name,
    tt_rte_desc,
    tt_rte_cmt,
    tt_rte_url,		/* Not in GPX 1.1 */
    tt_rte_urlname,	/* Not in GPX 1.1 */
    tt_rte_link,		/* New in GPX 1.1 */
    tt_rte_link_text,	/* New in GPX 1.1 */
    tt_rte_link_type,	/* New in GPX 1.1 */
    tt_rte_number,
    tt_garmin_rte_display_color,
    tt_rte_rtept,
    tt_trk,
    tt_trk_desc,
    tt_trk_name,
    tt_trk_trkseg,
    tt_trk_url,		/* Not in GPX 1.1 */
    tt_trk_urlname,	/* Not in GPX 1.1 */
    tt_trk_link,		/* New in GPX 1.1 */
    tt_trk_link_text,	/* New in GPX 1.1 */
    tt_trk_link_type,	/* New in GPX 1.1 */
    tt_trk_number,
    tt_garmin_trk_display_color,
    tt_trk_trkseg_trkpt,
    tt_trk_trkseg_trkpt_course,	/* Not in GPX 1.1 */
    tt_trk_trkseg_trkpt_speed,	/* Not in GPX 1.1 */
    tt_trk_trkseg_trkpt_heartrate,
    tt_trk_trkseg_trkpt_cadence,

    tt_humminbird_wpt_depth,
    tt_humminbird_wpt_status,
    tt_humminbird_trk_trkseg_trkpt_depth,
  };


  void gpx_add_to_global(QStringList& ge, const QString& s);
  inline QString toString(double d);
  inline QString toString(float f);
  void gpx_reset_short_handle();
  void gpx_write_gdata(const QStringList& ge, const QString& tag);
  tag_type get_tag(const QString& t, int* passthrough);
  void prescan_tags();
  void tag_gpx(const QXmlStreamAttributes& attr);
  void tag_wpt(const QXmlStreamAttributes& attr);
  void tag_cache_desc(const QXmlStreamAttributes& attr);
  void tag_gs_cache(const QXmlStreamAttributes& attr);
  void start_something_else(const QString& el, const QXmlStreamAttributes& attr);
  void end_something_else();
  void tag_log_wpt(const QXmlStreamAttributes& attr);
  void gpx_start(const QString& el, const QXmlStreamAttributes& attr);
  void gpx_end(const QString& unused);
  void gpx_cdata(const QString& s);
  void write_attributes(const QXmlStreamAttributes& attributes);
  void fprint_xml_chain(xml_tag* tag, const Waypoint* wpt);
  void write_gpx_url(const UrlList& urls);
  void write_gpx_url(const Waypoint* waypointp);
  void write_gpx_url(const route_head* rh);
  void gpx_write_common_acc(const Waypoint* waypointp);
  void gpx_write_common_position(const Waypoint* waypointp, gpx_point_type point_type);
  void gpx_write_common_extensions(const Waypoint* waypointp, gpx_point_type point_type);
  void gpx_write_common_description(const Waypoint* waypointp, const QString& oname);
  void gpx_waypt_pr(const Waypoint* waypointp);
  void gpx_track_hdr(const route_head* rte);
  void gpx_track_disp(const Waypoint* waypointp);
  void gpx_track_tlr(const route_head* unused);
  void gpx_track_pr();
  void gpx_route_hdr(const route_head* rte);
  void gpx_route_disp(const Waypoint* waypointp);
  void gpx_route_tlr(const route_head* unused);
  void gpx_route_pr();
  void gpx_waypt_bound_calc(const Waypoint* waypointp);
  void gpx_write_bounds();

  QXmlStreamReader* reader;
  xml_tag* cur_tag;
  QString cdatastr;
  char* opt_logpoint = nullptr;
  char* opt_humminbirdext = nullptr;
  char* opt_garminext = nullptr;
  char* opt_elevation_precision = nullptr;
  int logpoint_ct = 0;
  int elevation_precision;

  // to check if two numbers are equivalent use normalized values.
  const QVersionNumber gpx_1_0 = QVersionNumber(1,0).normalized();
  const QVersionNumber gpx_1_1 = QVersionNumber(1,1).normalized();
  QVersionNumber gpx_highest_version_read;
  char* opt_gpxver = nullptr;
  QVersionNumber gpx_write_version;
  QXmlStreamAttributes gpx_namespace_attribute;

  QString current_tag;

  Waypoint* wpt_tmp;
  UrlLink* link_;
  UrlLink* rh_link_;
  bool cache_descr_is_html;
  gpsbabel::File* iqfile;
  gpsbabel::File* oqfile;
  gpsbabel::XmlStreamWriter* writer;
  short_handle mkshort_handle;
  QString link_url;
  QString link_text;
  QString link_type;


  char* snlen = nullptr;
  char* suppresswhite = nullptr;
  char* urlbase = nullptr;
  route_head* trk_head;
  route_head* rte_head;
  const route_head* current_trk_head;		// Output.
  /* used for bounds calculation on output */
  bounds all_bounds;
  int next_trkpt_is_new_seg;

  format_specific_data** fs_ptr;

  /*
   * The file-level information.
   * This works for gpx 1.0, but does not handle all gpx 1.1 metadata.
   * TODO: gpx 1.1 metadata elements author, copyright, extensions,
   * all of which have more complicated content.
   * Note that all gpx 1.0 "global data" has a maxOccurs limit of one,
   * which is the default if maxOccurs is not in the xsd.
   * The only gpx 1.1 metadata that has a maxOccurs limit > one is link.
   * However, multiple gpx files may be read, and their global/metadata
   * combined, by this implementation.
   */
  struct GpxGlobal {
    QStringList name;
    QStringList desc;
    QStringList author;
    QStringList email;
    QStringList url;
    QStringList urlname;
    QStringList keywords;
    UrlList link;
    /* time and bounds aren't here; they're recomputed. */
  };
  GpxGlobal* gpx_global = nullptr;

  struct tag_mapping {
    tag_type tag_type_;		/* enum from above for this tag */
    int tag_passthrough;		/* true if we don't generate this */
    const char* tag_name;		/* xpath-ish tag name */
  };

  /*
   * xpath(ish) mappings between full tag paths and internal identifiers.
   * These appear in the order they appear in the GPX specification.
   * If it's not a tag we explicitly handle, it doesn't go here.
   */

  /* /gpx/<name> for GPX 1.0, /gpx/metadata/<name> for GPX 1.1 */
#define METATAG(type,name) \
  {type, 0, "/gpx/" name}, \
  {type, 0, "/gpx/metadata/" name}

  tag_mapping tag_path_map[158] = {
    { tt_gpx, 0, "/gpx" },
    METATAG(tt_name, "name"),
    METATAG(tt_desc, "desc"),
    { tt_author, 0, "/gpx/author" },
    { tt_email, 0, "/gpx/email" },
    { tt_url, 0, "/gpx/url" },
    { tt_urlname, 0, "/gpx/urlname" },
    METATAG(tt_keywords, "keywords"),
    { tt_link, 0, "/gpx/metadata/link" },
    { tt_link_text, 0, "/gpx/metadata/link/text" },
    { tt_link_type, 0, "/gpx/metadata/link/type" },

    { tt_wpt, 0, "/gpx/wpt" },

    /* Double up the GPX 1.0 and GPX 1.1 styles */
#define GEOTAG(type,name) \
  {type, 1, "/gpx/wpt/groundspeak:cache/groundspeak:" name }, \
  {type, 1, "/gpx/wpt/extensions/cache/" name }, \
  {type, 1, "/gpx/wpt/geocache/" name }  /* opencaching.de */

#define GARMIN_RTE_EXT "/gpx/rte/extensions/gpxx:RouteExtension"
#define GARMIN_TRK_EXT "/gpx/trk/extensions/gpxx:TrackExtension"
#define GARMIN_WPT_EXT "/gpx/wpt/extensions/gpxx:WaypointExtension"
#define GARMIN_TRKPT_EXT "/gpx/trk/trkseg/trkpt/extensions/gpxtpx:TrackPointExtension"
#define GARMIN_RTEPT_EXT "/gpx/rte/rtept/extensions/gpxxx:RoutePointExtension"

//	GEOTAG( tt_cache, 		"cache"),
    { tt_cache, 1, "/gpx/wpt/groundspeak:cache" },

    GEOTAG(tt_cache_name, 		"name"),
    GEOTAG(tt_cache_container, 	"container"),
    GEOTAG(tt_cache_type, 		"type"),
    GEOTAG(tt_cache_difficulty, 	"difficulty"),
    GEOTAG(tt_cache_terrain, 	"terrain"),
    GEOTAG(tt_cache_hint, 		"encoded_hints"),
    GEOTAG(tt_cache_hint, 		"hints"),  /* opencaching.de */
    GEOTAG(tt_cache_desc_short, 	"short_description"),
    GEOTAG(tt_cache_desc_long, 	"long_description"),
    GEOTAG(tt_cache_placer, 	"owner"),
    GEOTAG(tt_cache_favorite_points, 	"favorite_points"),
    GEOTAG(tt_cache_personal_note, 	"personal_note"),
    { tt_cache_log_wpt, 1, "/gpx/wpt/groundspeak:cache/groundspeak:logs/groundspeak:log/groundspeak:log_wpt"},
    { tt_cache_log_wpt, 1, "/gpx/wpt/extensions/cache/logs/log/log_wpt"},
    { tt_cache_log_type, 1, "/gpx/wpt/groundspeak:cache/groundspeak:logs/groundspeak:log/groundspeak:type"},
    { tt_cache_log_type, 1, "/gpx/wpt/extensions/cache/logs/log/type"},
    { tt_cache_log_date, 1, "/gpx/wpt/groundspeak:cache/groundspeak:logs/groundspeak:log/groundspeak:date"},
    { tt_cache_log_date, 1, "/gpx/wpt/extensions/cache/logs/log/date"},

    { tt_wpt_extensions, 0, "/gpx/wpt/extensions" },

    { tt_garmin_wpt_extensions, 0, GARMIN_WPT_EXT },
    { tt_garmin_wpt_proximity, 0, GARMIN_WPT_EXT "/gpxx:Proximity" },
    { tt_garmin_wpt_temperature, 0, GARMIN_WPT_EXT "/gpxx:Temperature" },
    { tt_garmin_wpt_temperature, 1, GARMIN_TRKPT_EXT "/gpxtpx:atemp" },
    { tt_garmin_wpt_depth, 0, GARMIN_WPT_EXT "/gpxx:Depth" },
    { tt_garmin_wpt_display_mode, 0, GARMIN_WPT_EXT "/gpxx:DisplayMode" },
    { tt_garmin_wpt_categories, 0, GARMIN_WPT_EXT "/gpxx:Categories" },
    { tt_garmin_wpt_category, 0, GARMIN_WPT_EXT "/gpxx:Categories/gpxx:Category" },
    { tt_garmin_wpt_addr, 0, GARMIN_WPT_EXT "/gpxx:Address/gpxx:StreetAddress" },
    { tt_garmin_wpt_city, 0, GARMIN_WPT_EXT "/gpxx:Address/gpxx:City" },
    { tt_garmin_wpt_state, 0, GARMIN_WPT_EXT "/gpxx:Address/gpxx:State" },
    { tt_garmin_wpt_country, 0, GARMIN_WPT_EXT "/gpxx:Address/gpxx:Country" },
    { tt_garmin_wpt_postal_code, 0, GARMIN_WPT_EXT "/gpxx:Address/gpxx:PostalCode" },
    { tt_garmin_wpt_phone_nr, 0, GARMIN_WPT_EXT "/gpxx:PhoneNumber"},

    // In Garmin space, but in core of waypoint.
    { tt_trk_trkseg_trkpt_heartrate, 1, GARMIN_TRKPT_EXT "/gpxtpx:hr" },
    { tt_trk_trkseg_trkpt_cadence, 1, GARMIN_TRKPT_EXT "/gpxtpx:cad" },

    { tt_humminbird_wpt_depth, 0, "/gpx/wpt/extensions/h:depth" },  // in centimeters.
    { tt_humminbird_wpt_status, 0, "/gpx/wpt/extensions/h:status" },

    { tt_rte, 0, "/gpx/rte" },
    { tt_rte_name, 0, "/gpx/rte/name" },
    { tt_rte_desc, 0, "/gpx/rte/desc" },
    { tt_rte_url, 0, "/gpx/rte/url"},				/* GPX 1.0 */
    { tt_rte_urlname, 0, "/gpx/rte/urlname"},		/* GPX 1.0 */
    { tt_rte_link, 0, "/gpx/rte/link"},			/* GPX 1.1 */
    { tt_rte_link_text, 0, "/gpx/rte/link/text"},	/* GPX 1.1 */
    { tt_rte_link_type, 0, "/gpx/rte/link/type"},	/* GPX 1.1 */
    { tt_rte_number, 0, "/gpx/rte/number" },
    { tt_garmin_rte_display_color, 1, GARMIN_RTE_EXT "/gpxx:DisplayColor"},

    { tt_rte_rtept, 0, "/gpx/rte/rtept" },

    { tt_trk, 0, "/gpx/trk" },
    { tt_trk_name, 0, "/gpx/trk/name" },
    { tt_trk_desc, 0, "/gpx/trk/desc" },
    { tt_trk_trkseg, 0, "/gpx/trk/trkseg" },
    { tt_trk_url, 0, "/gpx/trk/url"},				/* GPX 1.0 */
    { tt_trk_urlname, 0, "/gpx/trk/urlname"},		/* GPX 1.0 */
    { tt_trk_link, 0, "/gpx/trk/link"},			/* GPX 1.1 */
    { tt_trk_link_text, 0, "/gpx/trk/link/text"},	/* GPX 1.1 */
    { tt_trk_link_type, 0, "/gpx/trk/link/type"},	/* GPX 1.1 */
    { tt_trk_number, 0, "/gpx/trk/number" },
    { tt_garmin_trk_display_color, 1, GARMIN_TRK_EXT "/gpxx:DisplayColor"},

    { tt_trk_trkseg_trkpt, 0, "/gpx/trk/trkseg/trkpt" },
    { tt_trk_trkseg_trkpt_course, 0, "/gpx/trk/trkseg/trkpt/course" },
    { tt_trk_trkseg_trkpt_speed, 0, "/gpx/trk/trkseg/trkpt/speed" },

    { tt_humminbird_trk_trkseg_trkpt_depth, 0, "/gpx/trk/trkseg/trkpt/extensions/h:depth" },  // in centimeters.

    /* Common to tracks, routes, and waypts */
#define GPXWPTTYPETAG(type,passthrough,name) \
  {type, passthrough, "/gpx/wpt/" name }, \
  {type, passthrough, "/gpx/trk/trkseg/trkpt/" name }, \
  {type, passthrough, "/gpx/rte/rtept/" name }

    GPXWPTTYPETAG(tt_wpttype_ele, 0, "ele"),
    GPXWPTTYPETAG(tt_wpttype_time, 0, "time"),
    GPXWPTTYPETAG(tt_wpttype_geoidheight, 0, "geoidheight"),
    GPXWPTTYPETAG(tt_wpttype_name, 0, "name"),
    GPXWPTTYPETAG(tt_wpttype_cmt, 0, "cmt"),
    GPXWPTTYPETAG(tt_wpttype_desc, 0, "desc"),
    GPXWPTTYPETAG(tt_wpttype_url, 0, "url"),				/* GPX 1.0 */
    GPXWPTTYPETAG(tt_wpttype_urlname, 0, "urlname"),		/* GPX 1.0 */
    GPXWPTTYPETAG(tt_wpttype_link, 0, "link"),			/* GPX 1.1 */
    GPXWPTTYPETAG(tt_wpttype_link_text, 0, "link/text"),	/* GPX 1.1 */
    GPXWPTTYPETAG(tt_wpttype_link_type, 0, "link/type"),	/* GPX 1.1 */
    GPXWPTTYPETAG(tt_wpttype_sym, 0, "sym"),
    GPXWPTTYPETAG(tt_wpttype_type, 1, "type"),
    GPXWPTTYPETAG(tt_wpttype_fix, 0, "fix"),
    GPXWPTTYPETAG(tt_wpttype_sat, 0, "sat"),
    GPXWPTTYPETAG(tt_wpttype_hdop, 0, "hdop"),
    GPXWPTTYPETAG(tt_wpttype_vdop, 0, "vdop"),
    GPXWPTTYPETAG(tt_wpttype_pdop, 0, "pdop"),

    {(tag_type)0, 0, nullptr}
  };

// Maintain a fast mapping from full tag names to the struct above.
  QHash<QString, tag_mapping*> hash;

  QVector<arglist_t> gpx_args = {
    {
      "snlen", &snlen, "Length of generated shortnames",
      "32", ARGTYPE_INT, "1", nullptr, nullptr
    },
    {
      "suppresswhite", &suppresswhite,
      "No whitespace in generated shortnames",
      nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    {
      "logpoint", &opt_logpoint,
      "Create waypoints from geocache log entries",
      nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    {
      "urlbase", &urlbase, "Base URL for link tag in output",
      nullptr, ARGTYPE_STRING, ARG_NOMINMAX, nullptr
    },
    {
      "gpxver", &opt_gpxver, "Target GPX version for output",
      nullptr, ARGTYPE_STRING, ARG_NOMINMAX, nullptr
    },
    {
      "humminbirdextensions", &opt_humminbirdext,
      "Add info (depth) as Humminbird extension",
      nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    {
      "garminextensions", &opt_garminext,
      "Add info (depth) as Garmin extension",
      nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    {
      "elevprec", &opt_elevation_precision,
      "Precision of elevations, number of decimals",
      "3", ARGTYPE_INT, ARG_NOMINMAX, nullptr
    },
  };

};
#endif // GPX_H_INCLUDED_
