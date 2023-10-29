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

#include <QHash>                       // for QHash
#include <QList>                       // for QList
#include <QString>                     // for QString
#include <QStringList>                 // for QStringList
#include <QStringView>                 // for QStringView
#include <QVector>                     // for QVector
#include <QVersionNumber>              // for QVersionNumber
#include <QXmlStreamAttributes>        // for QXmlStreamAttributes
#include <QXmlStreamReader>            // for QXmlStreamReader

#include "defs.h"
#include "format.h"                    // for Format
#include "formspec.h"                  // for FormatSpecificData
#include "mkshort.h"                   // for MakeShort
#include "src/core/file.h"             // for File
#include "src/core/xmlstreamwriter.h"  // for XmlStreamWriter
#include "src/core/xmltag.h"           // for xml_tag


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

  void rd_init(const QString& fname) override;
  void read() override;
  void rd_deinit() override;
  void wr_init(const QString& fname) override;
  void write() override;
  void wr_deinit() override;
  void exit() override;

private:
  /*
   * This structure holds the element contents of elements in the
   * the gpx namespace that are only passed from the gpx reader to
   * the gpx writer.
   * We explcitly write these instead of passing them through so they
   * are written in the correct sequence.
   */
  struct gpx_wpt_fsdata : FormatSpecificData {
    gpx_wpt_fsdata() : FormatSpecificData(kFsGpxWpt) {}
  
    gpx_wpt_fsdata* clone() const override
    {
      return new gpx_wpt_fsdata(*this);
    }
  
    QString magvar;
    QString src;
    QString type;
    QString ageofdgpsdata;
    QString dgpsid;
  };

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
    tt_wpttype_magvar,
    tt_wpttype_geoidheight,
    tt_wpttype_name,
    tt_wpttype_cmt,
    tt_wpttype_desc,
    tt_wpttype_src,
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
    tt_wpttype_ageofdgpsdata,
    tt_wpttype_dgpsid,
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

    tt_garmin_wpt_extensions,
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
    tt_garmin_wpt_phone_nr,

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

  struct tag_mapping {
    tag_type type{tt_unknown};		/* enum from above for this tag */
    bool passthrough{true};		/* true if we don't generate this */
  };


  static void gpx_add_to_global(QStringList& ge, const QString& s);
  static inline QString toString(double d);
  static inline QString toString(float f);
  void gpx_reset_short_handle();
  void gpx_write_gdata(const QStringList& ge, const QString& tag) const;
  tag_mapping get_tag(const QString& t) const;
  void tag_gpx(const QXmlStreamAttributes& attr);
  void tag_wpt(const QXmlStreamAttributes& attr);
  void tag_cache_desc(const QXmlStreamAttributes& attr);
  void tag_gs_cache(const QXmlStreamAttributes& attr) const;
  void tag_garmin_fs(tag_type tag, const QString& text, Waypoint* waypt);
  void start_something_else(QStringView el, const QXmlStreamAttributes& attr);
  void end_something_else();
  void tag_log_wpt(const QXmlStreamAttributes& attr) const;
  void gpx_start(QStringView el, const QXmlStreamAttributes& attr);
  void gpx_end(QStringView unused);
  void gpx_cdata(QStringView s);
  QString qualifiedName() const;
  void write_attributes(const QXmlStreamAttributes& attributes) const;
  void fprint_xml_chain(XmlTag* tag) const;
  void write_gpx_url(const UrlList& urls) const;
  void write_gpx_url(const Waypoint* waypointp) const;
  void write_gpx_url(const route_head* rh) const;
  void gpx_write_common_acc(const Waypoint* waypointp, const gpx_wpt_fsdata* fs_gpxwpt) const;
  void gpx_write_common_position(const Waypoint* waypointp, gpx_point_type point_type, const gpx_wpt_fsdata* fs_gpxwpt) const;
  void gpx_write_common_extensions(const Waypoint* waypointp, gpx_point_type point_type) const;
  void gpx_write_common_description(const Waypoint* waypointp, gpx_point_type point_type, const gpx_wpt_fsdata* fs_gpxwpt) const;
  void gpx_waypt_pr(const Waypoint* waypointp) const;
  void gpx_write_common_core(const Waypoint* waypointp, gpx_point_type point_type) const;
  void gpx_track_hdr(const route_head* rte);
  void gpx_track_disp(const Waypoint* waypointp) const;
  void gpx_track_tlr(const route_head* unused);
  void gpx_track_pr();
  void gpx_route_hdr(const route_head* rte) const;
  void gpx_route_disp(const Waypoint* waypointp) const;
  void gpx_route_tlr(const route_head* unused) const;
  void gpx_route_pr();
  void gpx_waypt_bound_calc(const Waypoint* waypointp);
  void gpx_write_bounds();

  QXmlStreamReader* reader{};
  XmlTag* cur_tag{};
  QString cdatastr;
  char* opt_logpoint = nullptr;
  char* opt_humminbirdext = nullptr;
  char* opt_garminext = nullptr;
  char* opt_elevation_precision = nullptr;
  int logpoint_ct = 0;
  int elevation_precision{};

  // to check if two numbers are equivalent use normalized values.
  const QVersionNumber gpx_1_0 = QVersionNumber(1,0).normalized();
  const QVersionNumber gpx_1_1 = QVersionNumber(1,1).normalized();
  QVersionNumber gpx_highest_version_read;
  char* opt_gpxver = nullptr;
  QVersionNumber gpx_write_version;
  QXmlStreamAttributes gpx_namespace_attribute;

  QString current_tag;

  Waypoint* wpt_tmp{};
  UrlLink* link_{};
  UrlLink* rh_link_{};
  bool cache_descr_is_html{};
  gpsbabel::File* iqfile{};
  gpsbabel::File* oqfile{};
  gpsbabel::XmlStreamWriter* writer{};
  MakeShort* mkshort_handle{};
  QString link_url;
  QString link_text;
  QString link_type;


  char* snlen = nullptr;
  char* suppresswhite = nullptr;
  char* urlbase = nullptr;
  route_head* trk_head{};
  route_head* rte_head{};
  const route_head* current_trk_head{};		// Output.
  /* used for bounds calculation on output */
  bounds all_bounds{};
  int next_trkpt_is_new_seg{};

  FormatSpecificDataList* fs_ptr{};
  gpx_wpt_fsdata* wpt_fsdata{nullptr};

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

  /*
   * xpath(ish) mappings between full tag paths and internal identifiers.
   * These appear in the order they appear in the GPX specification.
   * If it's not a tag we explicitly handle, it doesn't go here.
   */

  /* /gpx/<name> for GPX 1.0, /gpx/metadata/<name> for GPX 1.1 */
#define METATAG(type,name) \
  {"/gpx/" name, {type, false}}, \
  {"/gpx/metadata/" name, {type, false}}

#define GEOTAG(type,name) \
  {"/gpx/wpt/groundspeak:cache/groundspeak:" name, {type, true}}

#define GPXWPTTYPETAG(name,type,passthrough) \
  {"/gpx/wpt/" name, {type, passthrough}}, \
  {"/gpx/trk/trkseg/trkpt/" name, {type, passthrough}}, \
  {"/gpx/rte/rtept/" name, {type, passthrough}}

#define GARMIN_RTE_EXT "/gpx/rte/extensions/gpxx:RouteExtension"
#define GARMIN_TRK_EXT "/gpx/trk/extensions/gpxx:TrackExtension"
#define GARMIN_WPT_EXT "/gpx/wpt/extensions/gpxx:WaypointExtension"
#define GARMIN_TRKPT_EXT "/gpx/trk/trkseg/trkpt/extensions/gpxtpx:TrackPointExtension"

// Maintain a fast mapping from full tag names to the struct above.
  const QHash<QString, tag_mapping> hash = {
    {"/gpx", {tt_gpx, false}},
    METATAG(tt_name, "name"),
    METATAG(tt_desc, "desc"),
    {"/gpx/author", {tt_author, false}},
    {"/gpx/email", {tt_email, false}},
    {"/gpx/url", {tt_url, false}},
    {"/gpx/urlname", {tt_urlname, false}},
    METATAG(tt_keywords, "keywords"),
    {"/gpx/metadata/link", {tt_link, false}},
    {"/gpx/metadata/link/text", {tt_link_text, false}},
    {"/gpx/metadata/link/type", {tt_link_type, false}},

    {"/gpx/wpt", {tt_wpt, false}},

    /* Double up the GPX 1.0 and GPX 1.1 styles */
//	GEOTAG(tt_cache, "cache"),
    {"/gpx/wpt/groundspeak:cache", {tt_cache, true}},

    GEOTAG(tt_cache_name, "name"),
    GEOTAG(tt_cache_container, "container"),
    GEOTAG(tt_cache_type, "type"),
    GEOTAG(tt_cache_difficulty, "difficulty"),
    GEOTAG(tt_cache_terrain, "terrain"),
    GEOTAG(tt_cache_hint, "encoded_hints"),
    GEOTAG(tt_cache_desc_short, "short_description"),
    GEOTAG(tt_cache_desc_long, "long_description"),
    GEOTAG(tt_cache_placer, "owner"),
    GEOTAG(tt_cache_favorite_points, "favorite_points"),
    GEOTAG(tt_cache_personal_note, "personal_note"),
    {"/gpx/wpt/groundspeak:cache/groundspeak:logs/groundspeak:log/groundspeak:log_wpt", {tt_cache_log_wpt, true}},
    {"/gpx/wpt/groundspeak:cache/groundspeak:logs/groundspeak:log/groundspeak:type", {tt_cache_log_type, true}},
    {"/gpx/wpt/groundspeak:cache/groundspeak:logs/groundspeak:log/groundspeak:date", {tt_cache_log_date, true}},

    {"/gpx/wpt/extensions", {tt_wpt_extensions, false}},

    {GARMIN_WPT_EXT, {tt_garmin_wpt_extensions, false}},
    {GARMIN_WPT_EXT "/gpxx:Proximity", {tt_garmin_wpt_proximity, false}},
    {GARMIN_WPT_EXT "/gpxx:Temperature", {tt_garmin_wpt_temperature, false}},
    {GARMIN_TRKPT_EXT "/gpxtpx:atemp", {tt_garmin_wpt_temperature, true}},
    {GARMIN_WPT_EXT "/gpxx:Depth", {tt_garmin_wpt_depth, false}},
    {GARMIN_WPT_EXT "/gpxx:DisplayMode", {tt_garmin_wpt_display_mode, false}},
    {GARMIN_WPT_EXT "/gpxx:Categories", {tt_garmin_wpt_categories, false}},
    {GARMIN_WPT_EXT "/gpxx:Categories/gpxx:Category", {tt_garmin_wpt_category, false}},
    {GARMIN_WPT_EXT "/gpxx:Address/gpxx:StreetAddress", {tt_garmin_wpt_addr, false}},
    {GARMIN_WPT_EXT "/gpxx:Address/gpxx:City", {tt_garmin_wpt_city, false}},
    {GARMIN_WPT_EXT "/gpxx:Address/gpxx:State", {tt_garmin_wpt_state, false}},
    {GARMIN_WPT_EXT "/gpxx:Address/gpxx:Country", {tt_garmin_wpt_country, false}},
    {GARMIN_WPT_EXT "/gpxx:Address/gpxx:PostalCode", {tt_garmin_wpt_postal_code, false}},
    {GARMIN_WPT_EXT "/gpxx:PhoneNumber", {tt_garmin_wpt_phone_nr, false}},

    // In Garmin space, but in core of waypoint.
    {GARMIN_TRKPT_EXT "/gpxtpx:hr", {tt_trk_trkseg_trkpt_heartrate, true}},
    {GARMIN_TRKPT_EXT "/gpxtpx:cad", {tt_trk_trkseg_trkpt_cadence, true}},

    {"/gpx/wpt/extensions/h:depth", {tt_humminbird_wpt_depth, false}},	// in centimeters.
    {"/gpx/wpt/extensions/h:status", {tt_humminbird_wpt_status, false}},

    {"/gpx/rte", {tt_rte, false}},
    {"/gpx/rte/name", {tt_rte_name, false}},
    {"/gpx/rte/desc", {tt_rte_desc, false}},
    {"/gpx/rte/url", {tt_rte_url, false}},							/* GPX 1.0 */
    {"/gpx/rte/urlname", {tt_rte_urlname, false}},			/* GPX 1.0 */
    {"/gpx/rte/link", {tt_rte_link, false}},						/* GPX 1.1 */
    {"/gpx/rte/link/text", {tt_rte_link_text, false}},	/* GPX 1.1 */
    {"/gpx/rte/link/type", {tt_rte_link_type, false}},	/* GPX 1.1 */
    {"/gpx/rte/number", {tt_rte_number, false}},
    {GARMIN_RTE_EXT "/gpxx:DisplayColor", {tt_garmin_rte_display_color, true}},

    {"/gpx/rte/rtept", {tt_rte_rtept, false}},

    {"/gpx/trk", {tt_trk, false}},
    {"/gpx/trk/name", {tt_trk_name, false}},
    {"/gpx/trk/desc", {tt_trk_desc, false}},
    {"/gpx/trk/trkseg", {tt_trk_trkseg, false}},
    {"/gpx/trk/url", {tt_trk_url, false}},							/* GPX 1.0 */
    {"/gpx/trk/urlname", {tt_trk_urlname, false}},			/* GPX 1.0 */
    {"/gpx/trk/link", {tt_trk_link, false}},						/* GPX 1.1 */
    {"/gpx/trk/link/text", {tt_trk_link_text, false}},	/* GPX 1.1 */
    {"/gpx/trk/link/type", {tt_trk_link_type, false}},	/* GPX 1.1 */
    {"/gpx/trk/number", {tt_trk_number, false}},
    {GARMIN_TRK_EXT "/gpxx:DisplayColor", {tt_garmin_trk_display_color, true}},

    {"/gpx/trk/trkseg/trkpt", {tt_trk_trkseg_trkpt, false}},
    {"/gpx/trk/trkseg/trkpt/course", {tt_trk_trkseg_trkpt_course, false}},
    {"/gpx/trk/trkseg/trkpt/speed", {tt_trk_trkseg_trkpt_speed, false}},

    {"/gpx/trk/trkseg/trkpt/extensions/h:depth", {tt_humminbird_trk_trkseg_trkpt_depth, false}},	// in centimeters.

    /* Common to tracks, routes, and waypts */
    GPXWPTTYPETAG("ele", tt_wpttype_ele, false),
    GPXWPTTYPETAG("time", tt_wpttype_time, false),
    GPXWPTTYPETAG("magvar", tt_wpttype_magvar, false),
    GPXWPTTYPETAG("geoidheight", tt_wpttype_geoidheight, false),
    GPXWPTTYPETAG("name", tt_wpttype_name, false),
    GPXWPTTYPETAG("cmt", tt_wpttype_cmt, false),
    GPXWPTTYPETAG("desc", tt_wpttype_desc, false),
    GPXWPTTYPETAG("src", tt_wpttype_src, false),
    GPXWPTTYPETAG("url", tt_wpttype_url, false),							/* GPX 1.0 */
    GPXWPTTYPETAG("urlname", tt_wpttype_urlname, false),			/* GPX 1.0 */
    GPXWPTTYPETAG("link", tt_wpttype_link, false),						/* GPX 1.1 */
    GPXWPTTYPETAG("link/text", tt_wpttype_link_text, false),	/* GPX 1.1 */
    GPXWPTTYPETAG("link/type", tt_wpttype_link_type, false),	/* GPX 1.1 */
    GPXWPTTYPETAG("sym", tt_wpttype_sym, false),
    GPXWPTTYPETAG("type", tt_wpttype_type, false),
    GPXWPTTYPETAG("fix", tt_wpttype_fix, false),
    GPXWPTTYPETAG("sat", tt_wpttype_sat, false),
    GPXWPTTYPETAG("hdop", tt_wpttype_hdop, false),
    GPXWPTTYPETAG("vdop", tt_wpttype_vdop, false),
    GPXWPTTYPETAG("pdop", tt_wpttype_pdop, false),
    GPXWPTTYPETAG("ageofdgpsdata", tt_wpttype_ageofdgpsdata, false),
    GPXWPTTYPETAG("dgpsid", tt_wpttype_dgpsid, false),
  };

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
