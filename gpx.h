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

  enum class tag_type {
    unknown = 0,
    gpx,

    name,		/* Optional file-level info */
    desc,
    author,
    email,
    url,
    urlname,
    keywords,
    link,
    link_text,
    link_type,

    wpt,
    wpttype_ele,
    wpttype_time,
    wpttype_magvar,
    wpttype_geoidheight,
    wpttype_name,
    wpttype_cmt,
    wpttype_desc,
    wpttype_src,
    wpttype_url,		/* Not in GPX 1.1 */
    wpttype_urlname,	/* Not in GPX 1.1 */
    wpttype_link,		/* New in GPX 1.1 */
    wpttype_link_text,	/* New in GPX 1.1 */
    wpttype_link_type,	/* New in GPX 1.1 */
    wpttype_sym,
    wpttype_type,
    wpttype_fix,
    wpttype_sat,
    wpttype_hdop,		/* HDOPS are common for all three */
    wpttype_vdop,		/* VDOPS are common for all three */
    wpttype_pdop,		/* PDOPS are common for all three */
    wpttype_ageofdgpsdata,
    wpttype_dgpsid,
    cache,
    cache_name,
    cache_container,
    cache_type,
    cache_difficulty,
    cache_terrain,
    cache_hint,
    cache_desc_short,
    cache_desc_long,
    cache_log_wpt,
    cache_log_type,
    cache_log_date,
    cache_placer,
    cache_favorite_points,
    cache_personal_note,

    wpt_extensions,

    garmin_wpt_extensions,
    garmin_wpt_proximity,
    garmin_wpt_temperature,
    garmin_wpt_depth,
    garmin_wpt_display_mode,
    garmin_wpt_categories,
    garmin_wpt_category,
    garmin_wpt_addr,
    garmin_wpt_city,
    garmin_wpt_state,
    garmin_wpt_country,
    garmin_wpt_postal_code,
    garmin_wpt_phone_nr,

    rte,
    rte_name,
    rte_desc,
    rte_cmt,
    rte_url,		/* Not in GPX 1.1 */
    rte_urlname,	/* Not in GPX 1.1 */
    rte_link,		/* New in GPX 1.1 */
    rte_link_text,	/* New in GPX 1.1 */
    rte_link_type,	/* New in GPX 1.1 */
    rte_number,
    garmin_rte_display_color,
    rte_rtept,
    trk,
    trk_desc,
    trk_name,
    trk_trkseg,
    trk_url,		/* Not in GPX 1.1 */
    trk_urlname,	/* Not in GPX 1.1 */
    trk_link,		/* New in GPX 1.1 */
    trk_link_text,	/* New in GPX 1.1 */
    trk_link_type,	/* New in GPX 1.1 */
    trk_number,
    garmin_trk_display_color,
    trk_trkseg_trkpt,
    trk_trkseg_trkpt_course,	/* Not in GPX 1.1 */
    trk_trkseg_trkpt_speed,	/* Not in GPX 1.1 */
    trk_trkseg_trkpt_heartrate,
    trk_trkseg_trkpt_cadence,

    humminbird_wpt_depth,
    humminbird_wpt_status,
    humminbird_trk_trkseg_trkpt_depth,
  };

  struct tag_mapping {
    tag_type type{tag_type::unknown}; /* enum from above for this tag */
    /*
     * passthrough should be true for
     * 1) The gpx 1.1 extensions element and any descendents.
     * 2) Any element from a foreign (non gpx) namespace and any descendents.
     *    This rule is necessary for gpx 1.0.
     */
    bool passthrough{true};
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
  static void tag_garmin_fs(tag_type tag, const QString& text, Waypoint* waypt);
  void start_something_else(QStringView el, const QXmlStreamAttributes& attr);
  void end_something_else();
  void tag_log_wpt(const QXmlStreamAttributes& attr) const;
  void gpx_start(QStringView el, const QXmlStreamAttributes& attr);
  void gpx_end(QStringView unused);
  void gpx_cdata(QStringView s);
  QString qualifiedName() const;
  void write_attributes(const QXmlStreamAttributes& attributes) const;
  void fprint_xml_chain(const XmlTag* tag) const;
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
  OptionCString opt_logpoint;
  OptionCString opt_humminbirdext;
  OptionCString opt_garminext;
  OptionCString opt_elevation_precision;
  int logpoint_ct = 0;
  int elevation_precision{};

  // to check if two numbers are equivalent use normalized values.
  const QVersionNumber gpx_1_0 = QVersionNumber(1,0).normalized();
  const QVersionNumber gpx_1_1 = QVersionNumber(1,1).normalized();
  QVersionNumber gpx_highest_version_read;
  OptionCString opt_gpxver;
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


  OptionCString snlen;
  OptionCString suppresswhite;
  OptionCString urlbase;
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
    {"/gpx", {tag_type::gpx, false}},
    METATAG(tag_type::name, "name"),
    METATAG(tag_type::desc, "desc"),
    {"/gpx/author", {tag_type::author, false}},
    {"/gpx/email", {tag_type::email, false}},
    {"/gpx/url", {tag_type::url, false}},
    {"/gpx/urlname", {tag_type::urlname, false}},
    METATAG(tag_type::keywords, "keywords"),
    {"/gpx/metadata/link", {tag_type::link, false}},
    {"/gpx/metadata/link/text", {tag_type::link_text, false}},
    {"/gpx/metadata/link/type", {tag_type::link_type, false}},

    {"/gpx/wpt", {tag_type::wpt, false}},

    /* Double up the GPX 1.0 and GPX 1.1 styles */
//	GEOTAG(tt_cache, "cache"),
    {"/gpx/wpt/groundspeak:cache", {tag_type::cache, true}},

    GEOTAG(tag_type::cache_name, "name"),
    GEOTAG(tag_type::cache_container, "container"),
    GEOTAG(tag_type::cache_type, "type"),
    GEOTAG(tag_type::cache_difficulty, "difficulty"),
    GEOTAG(tag_type::cache_terrain, "terrain"),
    GEOTAG(tag_type::cache_hint, "encoded_hints"),
    GEOTAG(tag_type::cache_desc_short, "short_description"),
    GEOTAG(tag_type::cache_desc_long, "long_description"),
    GEOTAG(tag_type::cache_placer, "owner"),
    GEOTAG(tag_type::cache_favorite_points, "favorite_points"),
    GEOTAG(tag_type::cache_personal_note, "personal_note"),
    {"/gpx/wpt/groundspeak:cache/groundspeak:logs/groundspeak:log/groundspeak:log_wpt", {tag_type::cache_log_wpt, true}},
    {"/gpx/wpt/groundspeak:cache/groundspeak:logs/groundspeak:log/groundspeak:type", {tag_type::cache_log_type, true}},
    {"/gpx/wpt/groundspeak:cache/groundspeak:logs/groundspeak:log/groundspeak:date", {tag_type::cache_log_date, true}},

    {"/gpx/wpt/extensions", {tag_type::wpt_extensions, true}},

    {GARMIN_WPT_EXT, {tag_type::garmin_wpt_extensions, true}},
    {GARMIN_WPT_EXT "/gpxx:Proximity", {tag_type::garmin_wpt_proximity, true}},
    {GARMIN_WPT_EXT "/gpxx:Temperature", {tag_type::garmin_wpt_temperature, true}},
    {GARMIN_TRKPT_EXT "/gpxtpx:atemp", {tag_type::garmin_wpt_temperature, true}},
    {GARMIN_WPT_EXT "/gpxx:Depth", {tag_type::garmin_wpt_depth, true}},
    {GARMIN_WPT_EXT "/gpxx:DisplayMode", {tag_type::garmin_wpt_display_mode, true}},
    {GARMIN_WPT_EXT "/gpxx:Categories", {tag_type::garmin_wpt_categories, true}},
    {GARMIN_WPT_EXT "/gpxx:Categories/gpxx:Category", {tag_type::garmin_wpt_category, true}},
    {GARMIN_WPT_EXT "/gpxx:Address/gpxx:StreetAddress", {tag_type::garmin_wpt_addr, true}},
    {GARMIN_WPT_EXT "/gpxx:Address/gpxx:City", {tag_type::garmin_wpt_city, true}},
    {GARMIN_WPT_EXT "/gpxx:Address/gpxx:State", {tag_type::garmin_wpt_state, true}},
    {GARMIN_WPT_EXT "/gpxx:Address/gpxx:Country", {tag_type::garmin_wpt_country, true}},
    {GARMIN_WPT_EXT "/gpxx:Address/gpxx:PostalCode", {tag_type::garmin_wpt_postal_code, true}},
    {GARMIN_WPT_EXT "/gpxx:PhoneNumber", {tag_type::garmin_wpt_phone_nr, true}},

    // In Garmin space, but in core of waypoint.
    {GARMIN_TRKPT_EXT "/gpxtpx:hr", {tag_type::trk_trkseg_trkpt_heartrate, true}},
    {GARMIN_TRKPT_EXT "/gpxtpx:cad", {tag_type::trk_trkseg_trkpt_cadence, true}},

    {"/gpx/wpt/extensions/h:depth", {tag_type::humminbird_wpt_depth, true}},	// in centimeters.
    {"/gpx/wpt/extensions/h:status", {tag_type::humminbird_wpt_status, true}},

    {"/gpx/rte", {tag_type::rte, false}},
    {"/gpx/rte/name", {tag_type::rte_name, false}},
    {"/gpx/rte/desc", {tag_type::rte_desc, false}},
    {"/gpx/rte/url", {tag_type::rte_url, false}},							/* GPX 1.0 */
    {"/gpx/rte/urlname", {tag_type::rte_urlname, false}},			/* GPX 1.0 */
    {"/gpx/rte/link", {tag_type::rte_link, false}},						/* GPX 1.1 */
    {"/gpx/rte/link/text", {tag_type::rte_link_text, false}},	/* GPX 1.1 */
    {"/gpx/rte/link/type", {tag_type::rte_link_type, false}},	/* GPX 1.1 */
    {"/gpx/rte/number", {tag_type::rte_number, false}},
    {GARMIN_RTE_EXT "/gpxx:DisplayColor", {tag_type::garmin_rte_display_color, true}},

    {"/gpx/rte/rtept", {tag_type::rte_rtept, false}},

    {"/gpx/trk", {tag_type::trk, false}},
    {"/gpx/trk/name", {tag_type::trk_name, false}},
    {"/gpx/trk/desc", {tag_type::trk_desc, false}},
    {"/gpx/trk/trkseg", {tag_type::trk_trkseg, false}},
    {"/gpx/trk/url", {tag_type::trk_url, false}},							/* GPX 1.0 */
    {"/gpx/trk/urlname", {tag_type::trk_urlname, false}},			/* GPX 1.0 */
    {"/gpx/trk/link", {tag_type::trk_link, false}},						/* GPX 1.1 */
    {"/gpx/trk/link/text", {tag_type::trk_link_text, false}},	/* GPX 1.1 */
    {"/gpx/trk/link/type", {tag_type::trk_link_type, false}},	/* GPX 1.1 */
    {"/gpx/trk/number", {tag_type::trk_number, false}},
    {GARMIN_TRK_EXT "/gpxx:DisplayColor", {tag_type::garmin_trk_display_color, true}},

    {"/gpx/trk/trkseg/trkpt", {tag_type::trk_trkseg_trkpt, false}},
    {"/gpx/trk/trkseg/trkpt/course", {tag_type::trk_trkseg_trkpt_course, false}},
    {"/gpx/trk/trkseg/trkpt/speed", {tag_type::trk_trkseg_trkpt_speed, false}},

    {"/gpx/trk/trkseg/trkpt/extensions/h:depth", {tag_type::humminbird_trk_trkseg_trkpt_depth, true}},	// in centimeters.

    /* Common to tracks, routes, and waypts */
    GPXWPTTYPETAG("ele", tag_type::wpttype_ele, false),
    GPXWPTTYPETAG("time", tag_type::wpttype_time, false),
    GPXWPTTYPETAG("magvar", tag_type::wpttype_magvar, false),
    GPXWPTTYPETAG("geoidheight", tag_type::wpttype_geoidheight, false),
    GPXWPTTYPETAG("name", tag_type::wpttype_name, false),
    GPXWPTTYPETAG("cmt", tag_type::wpttype_cmt, false),
    GPXWPTTYPETAG("desc", tag_type::wpttype_desc, false),
    GPXWPTTYPETAG("src", tag_type::wpttype_src, false),
    GPXWPTTYPETAG("url", tag_type::wpttype_url, false),							/* GPX 1.0 */
    GPXWPTTYPETAG("urlname", tag_type::wpttype_urlname, false),			/* GPX 1.0 */
    GPXWPTTYPETAG("link", tag_type::wpttype_link, false),						/* GPX 1.1 */
    GPXWPTTYPETAG("link/text", tag_type::wpttype_link_text, false),	/* GPX 1.1 */
    GPXWPTTYPETAG("link/type", tag_type::wpttype_link_type, false),	/* GPX 1.1 */
    GPXWPTTYPETAG("sym", tag_type::wpttype_sym, false),
    GPXWPTTYPETAG("type", tag_type::wpttype_type, false),
    GPXWPTTYPETAG("fix", tag_type::wpttype_fix, false),
    GPXWPTTYPETAG("sat", tag_type::wpttype_sat, false),
    GPXWPTTYPETAG("hdop", tag_type::wpttype_hdop, false),
    GPXWPTTYPETAG("vdop", tag_type::wpttype_vdop, false),
    GPXWPTTYPETAG("pdop", tag_type::wpttype_pdop, false),
    GPXWPTTYPETAG("ageofdgpsdata", tag_type::wpttype_ageofdgpsdata, false),
    GPXWPTTYPETAG("dgpsid", tag_type::wpttype_dgpsid, false),
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
