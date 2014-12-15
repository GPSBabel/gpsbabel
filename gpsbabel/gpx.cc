/*
    Access GPX data files.

    Copyright (C) 2002-2014 Robert Lipe, gpsbabel.org

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
#include "cet_util.h"
#include "garmin_fs.h"
#include "garmin_tables.h"
#include "src/core/logging.h"
#include "src/core/file.h"
#include "src/core/xmlstreamwriter.h"
#include "src/core/xmltag.h"

#include <QtCore/QXmlStreamReader>
#include <QtCore/QRegExp>
#include <QtCore/QDateTime>
#include <QtCore/QDebug>

#include <math.h>


static QXmlStreamReader* reader;
static xml_tag* cur_tag;
static QString cdatastr;
static char* opt_logpoint = NULL;
static char* opt_humminbirdext = NULL;
static char* opt_garminext = NULL;
static int logpoint_ct = 0;

// static char* gpx_version = NULL;
QString gpx_version;
static char* gpx_wversion;
static int gpx_wversion_num;
static QXmlStreamAttributes gpx_namespace_attribute;

static QString current_tag;

static Waypoint* wpt_tmp;
static UrlLink* link_;
static int cache_descr_is_html;
static gpsbabel::File* iqfile;
static gpsbabel::File* oqfile;
static gpsbabel::XmlStreamWriter* writer;
static short_handle mkshort_handle;
static QString link_url;
static QString link_text;
static QString link_type;


static char* snlen = NULL;
static char* suppresswhite = NULL;
static char* urlbase = NULL;
static route_head* trk_head;
static route_head* rte_head;
static const route_head* current_trk_head;		// Output.
/* used for bounds calculation on output */
static bounds all_bounds;
static int next_trkpt_is_new_seg;

static format_specific_data** fs_ptr;


#define MYNAME "GPX"
#ifndef CREATOR_NAME_URL
#  define CREATOR_NAME_URL "GPSBabel - http://www.gpsbabel.org"
#endif

typedef enum  {
  gpxpt_waypoint,
  gpxpt_track,
  gpxpt_route
} gpx_point_type;

typedef enum  {
  tt_unknown = 0,
  tt_gpx,

  tt_name,		/* Optional file-level info */
  tt_desc,
  tt_author,
  tt_email,
  tt_url,
  tt_urlname,
  tt_keywords,

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
  tt_rte_number,
  tt_garmin_rte_display_color,
  tt_rte_rtept,
  tt_trk,
  tt_trk_desc,
  tt_trk_name,
  tt_trk_trkseg,
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
} tag_type;

typedef struct {
  struct queue queue;
  char* tagdata;
} gpx_global_entry;

/*
 * The file-level information.
 */
static
struct gpx_global {
  gpx_global_entry name;
  gpx_global_entry desc;
  gpx_global_entry author;
  gpx_global_entry email;
  gpx_global_entry url;
  gpx_global_entry urlname;
  gpx_global_entry keywords;
  /* time and bounds aren't here; they're recomputed. */
}* gpx_global ;

static void
gpx_add_to_global(gpx_global_entry* ge, const QString& s)
{
  queue* elem, *tmp;
  gpx_global_entry* gep;

  QUEUE_FOR_EACH(&ge->queue, elem, tmp) {
    gep = BASE_STRUCT(elem, gpx_global_entry, queue);
    if (0 == s.compare(gep->tagdata)) {
      return;
    }
  }

  gep = (gpx_global_entry*) xcalloc(sizeof(*gep), 1);
  QUEUE_INIT(&gep->queue);
  gep->tagdata = xstrdup(s);
  ENQUEUE_TAIL(&ge->queue, &gep->queue);
}

static void
gpx_rm_from_global(gpx_global_entry* ge)
{
  queue* elem, *tmp;

  QUEUE_FOR_EACH(&ge->queue, elem, tmp) {
    gpx_global_entry* g = (gpx_global_entry*) dequeue(elem);
    xfree(g->tagdata);
    xfree(g);
  }
}

// Temporarily mock the old GPX writer's hardcoded fixed length for float/double
// types.  This can be removed once we have time/interest in regenerating all our
// zillion reference files.
static inline QString toString(double d)
{
  return QString::number(d, 'f', 9);
};

static inline QString toString(float f)
{
  return QString::number(f, 'f', 6);
};


/*
 * gpx_reset_short_handle: used for waypoint, route and track names
 * this allows gpx:wpt names to overlap gpx:rtept names, etc.
 */
static void
gpx_reset_short_handle(void)
{
  if (mkshort_handle != NULL) {
    mkshort_del_handle(&mkshort_handle);
  }

  mkshort_handle = mkshort_new_handle();

  if (suppresswhite) {
    setshort_whitespace_ok(mkshort_handle, 0);
  }

  setshort_length(mkshort_handle, atoi(snlen));
}

static void
gpx_write_gdata(gpx_global_entry* ge, const char* tag)
{
  queue* elem, *tmp;
  gpx_global_entry* gep;

  if (!gpx_global || QUEUE_EMPTY(&ge->queue)) {
    return;
  }
  writer->writeStartElement(tag);
  QUEUE_FOR_EACH(&ge->queue, elem, tmp) {
    gep = BASE_STRUCT(elem, gpx_global_entry, queue);
    writer->writeCharacters(gep->tagdata);
    /* Some tags we just output once. */
    if ((0 == strcmp(tag, "url")) ||
        (0 == strcmp(tag, "email"))) {
      break;
    }
  }
  writer->writeEndElement();
}


typedef struct tag_mapping {
  tag_type tag_type_;		/* enum from above for this tag */
  int tag_passthrough;		/* true if we don't generate this */
  const char* tag_name;		/* xpath-ish tag name */
} tag_mapping;

/*
 * xpath(ish) mappings between full tag paths and internal identifers.
 * These appear in the order they appear in the GPX specification.
 * If it's not a tag we explictly handle, it doesn't go here.
 */

/* /gpx/<name> for GPX 1.0, /gpx/metadata/<name> for GPX 1.1 */
#define METATAG(type,name) \
  {type, 0, "/gpx/" name}, \
  {type, 0, "/gpx/metadata/" name}

tag_mapping tag_path_map[] = {
  { tt_gpx, 0, "/gpx" },
  METATAG(tt_name, "name"),
  METATAG(tt_desc, "desc"),
  { tt_author, 0, "/gpx/author" },
  { tt_email, 0, "/gpx/email" },
  { tt_url, 0, "/gpx/url" },
  { tt_urlname, 0, "/gpx/urlname" },
  METATAG(tt_keywords, "keywords"),

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
  { tt_rte_number, 0, "/gpx/rte/number" },
  { tt_garmin_rte_display_color, 1, GARMIN_RTE_EXT "/gpxx:DisplayColor"},

  { tt_rte_rtept, 0, "/gpx/rte/rtept" },

  { tt_trk, 0, "/gpx/trk" },
  { tt_trk_name, 0, "/gpx/trk/name" },
  { tt_trk_desc, 0, "/gpx/trk/desc" },
  { tt_trk_trkseg, 0, "/gpx/trk/trkseg" },
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

  {(tag_type)0, 0, NULL}
};

// Maintain a fast mapping from full tag names to the struct above.
QHash<QString, tag_mapping*> hash;

static tag_type
get_tag(const QString& t, int* passthrough)
{
  tag_mapping* tm = hash[t];
  if (tm) {
    *passthrough = tm->tag_passthrough;
    return tm->tag_type_;
  }
  *passthrough = 1;
  return tt_unknown;
}

static void
prescan_tags(void)
{
  tag_mapping* tm;
  for (tm = tag_path_map; tm->tag_type_ != 0; tm++) {
    hash[tm->tag_name] = tm;
  }
}

static void
tag_gpx(const QXmlStreamAttributes& attr)
{
  if (attr.hasAttribute("version")) {
    /* Set the default output version to the highest input
     * version.
     */
    if (gpx_version.isEmpty()) {
      gpx_version = attr.value("version").toString();
    } else if ((gpx_version.toInt() * 10) < (attr.value("version").toString().toDouble() * 10)) {
      gpx_version = attr.value("version").toString();
    }
  }
  /* save namespace declarations in case we pass through elements
   * that use them to the writer.
   */
  const QXmlStreamNamespaceDeclarations ns = reader->namespaceDeclarations();
  for (int i = 0; i < ns.size(); ++i) {
    QString prefix = ns[i].prefix().toString();
    QString namespaceUri = ns[i].namespaceUri().toString();
    if (!prefix.isEmpty() && (0 != prefix.compare("xsi"))) {
      if (! gpx_namespace_attribute.hasAttribute(prefix.prepend("xmlns:"))) {
        gpx_namespace_attribute.append(prefix, namespaceUri);
      }
    }
  }
}

static void
tag_wpt(const QXmlStreamAttributes& attr)
{
  wpt_tmp = new Waypoint;
  link_ = new UrlLink;

  cur_tag = NULL;
  if (attr.hasAttribute("lat")) {
    wpt_tmp->latitude = attr.value("lat").toString().toDouble();
  }
  if (attr.hasAttribute("lon")) {
    wpt_tmp->longitude = attr.value("lon").toString().toDouble();
  }
  fs_ptr = &wpt_tmp->fs;
}

static void
tag_cache_desc(const QXmlStreamAttributes& attr)
{
  cache_descr_is_html = 0;
  if (attr.hasAttribute("html")) {
    if (attr.value("html").toString().compare("True") == 0) {
      cache_descr_is_html = 1;
    }
  }
}

static void
tag_gs_cache(const QXmlStreamAttributes& attr)
{
  geocache_data* gc_data = wpt_tmp->AllocGCData();

  if (attr.hasAttribute("id")) {
    gc_data->id = attr.value("id").toString().toInt();
  }
  if (attr.hasAttribute("available")) {
    if (attr.value("available").toString().compare("True", Qt::CaseInsensitive) == 0) {
      gc_data->is_available = status_true;
    } else if (attr.value("available").toString().compare("False", Qt::CaseInsensitive) == 0) {
      gc_data->is_available = status_false;
    }
  }
  if (attr.hasAttribute("archived")) {
    if (attr.value("archived").toString().compare("True", Qt::CaseInsensitive) == 0) {
      gc_data->is_archived = status_true;
    } else if (attr.value("archived").toString().compare("False", Qt::CaseInsensitive) == 0) {
      gc_data->is_archived = status_false;
    }
  }
}

static void
start_something_else(const QString el, const QXmlStreamAttributes& attr)
{
  char** avcp;
  int attr_count;
  xml_tag* new_tag;
  fs_xml* fs_gpx;

  if (!fs_ptr) {
    return;
  }

  new_tag = new xml_tag;
  new_tag->tagname = el;

  attr_count = attr.size();
  const QXmlStreamNamespaceDeclarations nsdecl = reader->namespaceDeclarations();
  const int ns_count = nsdecl.size();
  new_tag->attributes = (char**)xcalloc(sizeof(char*),2*(attr_count+ns_count)+1);
  avcp = new_tag->attributes;
  for (int i = 0; i < attr_count; i++)  {
    *avcp = xstrdup(attr[i].name().toString());
    avcp++;
    *avcp = xstrdup(attr[i].value().toString());
    avcp++;
  }
  for (int i = 0; i < ns_count; i++)  {
    *avcp = xstrdup(nsdecl[i].prefix().toString().prepend(nsdecl[i].prefix().isEmpty()? "xmlns" : "xmlns:"));
    avcp++;
    *avcp = xstrdup(nsdecl[i].namespaceUri().toString());
    avcp++;
  }

  *avcp = NULL; // this indicates the end of the attribute name value pairs.

  if (cur_tag) {
    if (cur_tag->child) {
      cur_tag = cur_tag->child;
      while (cur_tag->sibling) {
        cur_tag = cur_tag->sibling;
      }
      cur_tag->sibling = new_tag;
      new_tag->parent = cur_tag->parent;
    } else {
      cur_tag->child = new_tag;
      new_tag->parent = cur_tag;
    }
  } else {
    fs_gpx = (fs_xml*)fs_chain_find(*fs_ptr, FS_GPX);

    if (fs_gpx && fs_gpx->tag) {
      cur_tag = fs_gpx->tag;
      while (cur_tag->sibling) {
        cur_tag = cur_tag->sibling;
      }
      cur_tag->sibling = new_tag;
      new_tag->parent = NULL;
    } else {
      fs_gpx = fs_xml_alloc(FS_GPX);
      fs_gpx->tag = new_tag;
      fs_chain_add(fs_ptr, (format_specific_data*)fs_gpx);
      new_tag->parent = NULL;
    }
  }
  cur_tag = new_tag;
}

static void
end_something_else()
{
  if (cur_tag) {
    cur_tag = cur_tag->parent;
  }
}

static void
tag_log_wpt(const QXmlStreamAttributes& attr)
{
  /* create a new waypoint */
  Waypoint* lwp_tmp = new Waypoint;

  /* extract the lat/lon attributes */
  if (attr.hasAttribute("lat")) {
    lwp_tmp->latitude = attr.value("lat").toString().toDouble();
  }
  if (attr.hasAttribute("lon")) {
    lwp_tmp->longitude = attr.value("lon").toString().toDouble();
  }
  /* Make a new shortname.  Since this is a groundspeak extension,
    we assume that GCBLAH is the current shortname format and that
    wpt_tmp refers to the currently parsed waypoint. Unfortunatley,
    we need to keep track of log_wpt counts so we don't collide with
    dupe shortnames.
  */
#if NEW_STRINGS
  if (wpt_tmp->shortname.size() > 2) {
// FIXME: think harder about this later.
    lwp_tmp->shortname = wpt_tmp->shortname.mid(2, 4) + "-FIXME";

#else
  if ((wpt_tmp->shortname) && (strlen(wpt_tmp->shortname) > 2)) {
    /* copy of the shortname */
    lwp_tmp->shortname = (char*) xcalloc(7, 1);
    sprintf(lwp_tmp->shortname, "%-4.4s%02d",
            &wpt_tmp->shortname[2], logpoint_ct++);
#endif
    waypt_add(lwp_tmp);
  }
}

static void
gpx_start(const QString& el, const QXmlStreamAttributes& attr)
{
  int passthrough;
  int tag;

  /*
   * Reset end-of-string without actually emptying/reallocing cdatastr.
   */
  cdatastr = QString();

  tag = get_tag(current_tag, &passthrough);
  switch (tag) {
  case tt_gpx:
    tag_gpx(attr);
    break;
  case tt_wpt:
    tag_wpt(attr);
    break;
  case tt_wpttype_link:
    if (attr.hasAttribute("href")) {
      link_url = attr.value("href").toString();
    }
    break;
  case tt_rte:
    rte_head = route_head_alloc();
    route_add_head(rte_head);
    fs_ptr = &rte_head->fs;
    break;
  case tt_rte_rtept:
    tag_wpt(attr);
    break;
  case tt_trk:
    trk_head = route_head_alloc();
    track_add_head(trk_head);
    fs_ptr = &trk_head->fs;
    break;
  case tt_trk_trkseg_trkpt:
    tag_wpt(attr);
    if (next_trkpt_is_new_seg) {
      wpt_tmp->wpt_flags.new_trkseg = 1;
      next_trkpt_is_new_seg = 0;
    }
    break;
  case tt_unknown:
    start_something_else(el, attr);
    return;
  case tt_cache:
    tag_gs_cache(attr);
    break;
  case tt_cache_log_wpt:
    if (opt_logpoint) {
      tag_log_wpt(attr);
    }
    break;
  case tt_cache_desc_long:
  case tt_cache_desc_short:
    tag_cache_desc(attr);
    break;
  case tt_cache_placer:
    if (attr.hasAttribute("id")) {
      wpt_tmp->AllocGCData()->placer_id = attr.value("id").toString().toInt();
    }
  default:
    break;
  }
  if (passthrough) {
    start_something_else(el, attr);
  }
}

struct
    gs_type_mapping {
  geocache_type type;
  const char* name;
} gs_type_map[] = {
  { gt_traditional, "Traditional Cache" },
  { gt_traditional, "Traditional" }, /* opencaching.de */
  { gt_multi, "Multi-cache" },
  { gt_multi, "Multi" }, /* opencaching.de */
  { gt_virtual, "Virtual Cache" },
  { gt_virtual, "Virtual" }, /* opencaching.de */
  { gt_event, "Event Cache" },
  { gt_event, "Event" }, /* opencaching.de */
  { gt_webcam, "Webcam Cache" },
  { gt_webcam, "Webcam" }, /* opencaching.de */
  { gt_suprise, "Unknown Cache" },
  { gt_earth, "Earthcache" },
  { gt_earth, "Earth" }, /* opencaching.de */
  { gt_cito, "Cache In Trash Out Event" },
  { gt_letterbox, "Letterbox Hybrid" },
  { gt_locationless, "Locationless (Reverse) Cache" },
  { gt_ape, "Project APE Cache" },
  { gt_mega, "Mega-Event Cache" },
  { gt_wherigo, "Wherigo Cache" },

  { gt_benchmark, "Benchmark" }, /* Not Groundspeak; for GSAK  */
};

struct
    gs_container_mapping {
  geocache_container type;
  const char* name;
} gs_container_map[] = {
  { gc_other, "Unknown" },
  { gc_other, "Other" }, /* Synonym on read. */
  { gc_micro, "Micro" },
  { gc_regular, "Regular" },
  { gc_large, "Large" },
  { gc_small, "Small" },
  { gc_virtual, "Virtual" }
};

geocache_type
gs_mktype(const QString& t)
{
  int i;
  int sz = sizeof(gs_type_map) / sizeof(gs_type_map[0]);

  for (i = 0; i < sz; i++) {
    if (!t.compare(gs_type_map[i].name,Qt::CaseInsensitive)) {
      return gs_type_map[i].type;
    }
  }
  return gt_unknown;
}

const char*
gs_get_cachetype(geocache_type t)
{
  int i;
  int sz = sizeof(gs_type_map) / sizeof(gs_type_map[0]);

  for (i = 0; i < sz; i++) {
    if (t == gs_type_map[i].type) {
      return gs_type_map[i].name;
    }
  }
  return "Unknown";
}

geocache_container
gs_mkcont(const QString& t)
{
  int i;
  int sz = sizeof(gs_container_map) / sizeof(gs_container_map[0]);

  for (i = 0; i < sz; i++) {
    if (!t.compare(gs_container_map[i].name,Qt::CaseInsensitive)) {
      return gs_container_map[i].type;
    }
  }
  return gc_unknown;
}

const char*
gs_get_container(geocache_container t)
{
  int i;
  int sz = sizeof(gs_container_map) / sizeof(gs_container_map[0]);

  for (i = 0; i < sz; i++) {
    if (t == gs_container_map[i].type) {
      return gs_container_map[i].name;
    }
  }
  return "Unknown";
}

gpsbabel::DateTime
xml_parse_time(const QString& dateTimeString)
{
  int off_hr = 0;
  int off_min = 0;
  int off_sign = 1;
  char* offsetstr = NULL;
  char* pointstr = NULL;
  char* timestr = xstrdup(dateTimeString);

  offsetstr = strchr(timestr, 'Z');
  if (offsetstr) {
    /* zulu time; offsets stay at defaults */
    *offsetstr = '\0';
  } else {
    offsetstr = strchr(timestr, '+');
    if (offsetstr) {
      /* positive offset; parse it */
      *offsetstr = '\0';
      sscanf(offsetstr + 1, "%d:%d", &off_hr, &off_min);
    } else {
      offsetstr = strchr(timestr, 'T');
      if (offsetstr) {
        offsetstr = strchr(offsetstr, '-');
        if (offsetstr) {
          /* negative offset; parse it */
          *offsetstr = '\0';
          sscanf(offsetstr + 1, "%d:%d", &off_hr, &off_min);
          off_sign = -1;
        }
      }
    }
  }

  double fsec = 0;
  pointstr = strchr(timestr, '.');
  if (pointstr) {
    sscanf(pointstr, "%le", &fsec);
#if 0
    /* Round to avoid FP jitter */
    if (microsecs) {
      *microsecs = .5 + (fsec * 1000000.0) ;
    }
#endif
    *pointstr = '\0';
  }

  int year = 0, mon = 0, mday = 0, hour = 0, min = 0, sec = 0;
  QDateTime dt;
  int res = sscanf(timestr, "%d-%d-%dT%d:%d:%d", &year, &mon, &mday, &hour,
                   &min, &sec);
  if (res > 0) {
    QDate date(year, mon, mday);
    QTime time(hour, min, sec);

    // Fractional part of time.
    if (fsec) {
      time = time.addMSecs(lround(fsec * 1000));
    }

    // Any offsets that were stuck at the end.
    time = time.addSecs(-off_sign * off_hr * 3600 - off_sign * off_min * 60);

    dt = QDateTime(date, time, Qt::UTC);
  } else {
    dt = QDateTime();
  }
  xfree(timestr);
  return dt;
}

static void
gpx_end(const QString& el)
{
  float x;
  int passthrough;
  static QDateTime gc_log_date;
  tag_type tag;

  // Remove leading, trailing whitespace.
  cdatastr = cdatastr.trimmed();

  tag = get_tag(current_tag, &passthrough);

  switch (tag) {
    /*
     * First, the tags that are file-global.
     */
  case tt_name:
    gpx_add_to_global(&gpx_global->name, cdatastr);
    break;
  case tt_desc:
    gpx_add_to_global(&gpx_global->desc, cdatastr);
    break;
  case tt_author:
    gpx_add_to_global(&gpx_global->author, cdatastr);
    break;
  case tt_email:
    gpx_add_to_global(&gpx_global->email, cdatastr);
    break;
  case tt_url:
    gpx_add_to_global(&gpx_global->url, cdatastr);
    break;
  case tt_urlname:
    gpx_add_to_global(&gpx_global->urlname, cdatastr);
    break;
  case tt_keywords:
    gpx_add_to_global(&gpx_global->keywords, cdatastr);
    break;

    /*
     * Waypoint-specific tags.
     */
  case tt_wpt:
    if (link_) {
      if (!link_->url_.isEmpty()) {
        wpt_tmp->AddUrlLink(*link_);
      }
      delete link_;
      link_ = NULL;
    }
    waypt_add(wpt_tmp);
    logpoint_ct = 0;
    cur_tag = NULL;
    wpt_tmp = NULL;
    break;
  case tt_cache_name:
    wpt_tmp->notes = cdatastr;
    break;
  case tt_cache_container:
    wpt_tmp->AllocGCData()->container = gs_mkcont(cdatastr);
    break;
  case tt_cache_type:
    wpt_tmp->AllocGCData()->type = gs_mktype(cdatastr);
    break;
  case tt_cache_difficulty:
    x = cdatastr.toDouble();
    wpt_tmp->AllocGCData()->diff = x * 10;
    break;
  case tt_cache_hint:
    wpt_tmp->AllocGCData()->hint = cdatastr;
    break;
  case tt_cache_desc_long: {
    geocache_data* gc_data = wpt_tmp->AllocGCData();
    gc_data->desc_long.is_html = cache_descr_is_html;
    gc_data->desc_long.utfstring = cdatastr;
  }
  break;
  case tt_cache_desc_short: {
    geocache_data* gc_data = wpt_tmp->AllocGCData();
    gc_data->desc_short.is_html = cache_descr_is_html;
    gc_data->desc_short.utfstring = cdatastr;
  }
  break;
  case tt_cache_terrain:
    x = cdatastr.toDouble();
    wpt_tmp->AllocGCData()->terr = x * 10;
    break;
  case tt_cache_placer:
    wpt_tmp->AllocGCData()->placer = cdatastr;
    break;
  case tt_cache_log_date:
    gc_log_date = xml_parse_time(cdatastr);
    break;
    /*
     * "Found it" logs follow the date according to the schema,
     * if this is the first "found it" for this waypt, just use the
     * last date we saw in this log.
     */
  case tt_cache_log_type:
    if ((cdatastr.compare("Found it") == 0) &&
        (0 == wpt_tmp->gc_data->last_found.toTime_t())) {
      wpt_tmp->AllocGCData()->last_found = gc_log_date;
    }
    gc_log_date = QDateTime();
    break;
  case tt_cache_favorite_points:
    wpt_tmp->AllocGCData()->favorite_points  = cdatastr.toInt();
    break;
  case tt_cache_personal_note:
    wpt_tmp->AllocGCData()->personal_note  = cdatastr;
    break;

    /*
     * Garmin-waypoint-specific tags.
     */
  case tt_garmin_wpt_proximity:
  case tt_garmin_wpt_temperature:
  case tt_garmin_wpt_depth:
  case tt_garmin_wpt_display_mode:
  case tt_garmin_wpt_category:
  case tt_garmin_wpt_addr:
  case tt_garmin_wpt_city:
  case tt_garmin_wpt_state:
  case tt_garmin_wpt_country:
  case tt_garmin_wpt_postal_code:
  case tt_garmin_wpt_phone_nr:
    garmin_fs_xml_convert(tt_garmin_wpt_extensions, tag, cdatastr, wpt_tmp);
    break;

    /*
     * Humminbird-waypoint-specific tags.
     */
  case tt_humminbird_wpt_depth:
  case tt_humminbird_trk_trkseg_trkpt_depth:
    WAYPT_SET(wpt_tmp, depth, cdatastr.toDouble() / 100.0)
    break;
    /*
     * Route-specific tags.
     */
  case tt_rte_name:
    rte_head->rte_name = cdatastr;
    break;
  case tt_rte:
    break;
  case tt_rte_rtept:
    if (link_) {
      if (!link_->url_.isEmpty()) {
        wpt_tmp->AddUrlLink(*link_);
      }
      delete link_;
      link_ = NULL;
    }
    route_add_wpt(rte_head, wpt_tmp);
    wpt_tmp = NULL;
    break;
  case tt_rte_desc:
    rte_head->rte_desc = cdatastr;
    break;
  case tt_garmin_rte_display_color:
    rte_head->line_color.bbggrr = gt_color_value_by_name(cdatastr);
    break;
  case tt_rte_number:
    rte_head->rte_num = cdatastr.toInt();
    break;
    /*
     * Track-specific tags.
     */
  case tt_trk_name:
    trk_head->rte_name = cdatastr;
    break;
  case tt_trk:
    break;
  case tt_trk_trkseg:
    next_trkpt_is_new_seg = 1;
    break;
  case tt_trk_trkseg_trkpt:
    if (link_) {
      if (!link_->url_.isEmpty()) {
        wpt_tmp->AddUrlLink(*link_);
      }
      delete link_;
      link_ = NULL;
    }
    track_add_wpt(trk_head, wpt_tmp);
    wpt_tmp = NULL;
    break;
  case tt_trk_desc:
    trk_head->rte_desc = cdatastr;
    break;
  case tt_garmin_trk_display_color:
    trk_head->line_color.bbggrr = gt_color_value_by_name(cdatastr);
    break;
  case tt_trk_number:
    trk_head->rte_num = cdatastr.toInt();
    break;
  case tt_trk_trkseg_trkpt_course:
    WAYPT_SET(wpt_tmp, course, cdatastr.toDouble());
    break;
  case tt_trk_trkseg_trkpt_speed:
    WAYPT_SET(wpt_tmp, speed, cdatastr.toDouble());
    break;
  case tt_trk_trkseg_trkpt_heartrate:
    wpt_tmp->heartrate = cdatastr.toDouble();
    break;
  case tt_trk_trkseg_trkpt_cadence:
    wpt_tmp->cadence = cdatastr.toDouble();
    break;

    /*
     * Items that are actually in multiple categories.
     */
  case tt_wpttype_ele:
    wpt_tmp->altitude = cdatastr.toDouble();
    break;
  case tt_wpttype_name:
    wpt_tmp->shortname = cdatastr;
    break;
  case tt_wpttype_sym:
    wpt_tmp->icon_descr = cdatastr;
    break;
  case tt_wpttype_time:
    wpt_tmp->SetCreationTime(xml_parse_time(cdatastr));
    break;
  case tt_wpttype_geoidheight:
    WAYPT_SET(wpt_tmp, geoidheight, cdatastr.toDouble());
    break;
  case tt_wpttype_cmt:
    wpt_tmp->description = cdatastr;
    break;
  case tt_wpttype_desc:
    wpt_tmp->notes = cdatastr;
    break;
  case tt_wpttype_pdop:
    wpt_tmp->pdop = cdatastr.toDouble();
    break;
  case tt_wpttype_hdop:
    wpt_tmp->hdop = cdatastr.toDouble();
    break;
  case tt_wpttype_vdop:
    wpt_tmp->vdop = cdatastr.toDouble();
    break;
  case tt_wpttype_sat:
    wpt_tmp->sat = cdatastr.toDouble();
    break;
  case tt_wpttype_fix:
    if (cdatastr == QLatin1String("none")) {
      wpt_tmp->fix = fix_none;
    } else if (cdatastr == QLatin1String("2d")) {
      wpt_tmp->fix = fix_2d;
    } else if (cdatastr == QLatin1String("3d")) {
      wpt_tmp->fix = fix_3d;
    } else if (cdatastr == QLatin1String("dgps")) {
      wpt_tmp->fix = fix_dgps;
    } else if (cdatastr == QLatin1String("pps")) {
      wpt_tmp->fix = fix_pps;
    } else {
      wpt_tmp->fix = fix_unknown;
    }
    break;
  case tt_wpttype_url:
    link_->url_ = cdatastr;
    break;
  case tt_wpttype_urlname:
    link_->url_link_text_ = cdatastr;
    break;
  case tt_wpttype_link:
    waypt_add_url(wpt_tmp, link_url, link_text, link_type);
    link_type = QString();
    link_text = QString();
    link_url = QString();
    break;
  case tt_wpttype_link_text:
    link_text = cdatastr;
    break;
  case tt_wpttype_link_type:
    link_type = cdatastr;
    break;
  case tt_unknown:
    end_something_else();
    return;
  default:
    break;
  }

  if (passthrough) {
    end_something_else();
  }

}


static void
gpx_cdata(const QString& s)
{
  QString* cdata;
  xml_tag* tmp_tag;
  cdatastr += s;

  if (!cur_tag) {
    return;
  }

  if (cur_tag->child) {
    tmp_tag = cur_tag->child;
    while (tmp_tag->sibling) {
      tmp_tag = tmp_tag->sibling;
    }
    cdata = &(tmp_tag->parentcdata);
  } else {
    cdata = &(cur_tag->cdata);
  }
  *cdata = cdatastr.trimmed();
}

static void
gpx_rd_init(const char* fname)
{
  iqfile = new gpsbabel::File(fname);
  iqfile->open(QIODevice::ReadOnly);
  reader = new QXmlStreamReader(iqfile);

  current_tag.clear();

  prescan_tags();

  cdatastr = QString();

  if (NULL == gpx_global) {
    gpx_global = (struct gpx_global*) xcalloc(sizeof(*gpx_global), 1);
    QUEUE_INIT(&gpx_global->name.queue);
    QUEUE_INIT(&gpx_global->desc.queue);
    QUEUE_INIT(&gpx_global->author.queue);
    QUEUE_INIT(&gpx_global->email.queue);
    QUEUE_INIT(&gpx_global->url.queue);
    QUEUE_INIT(&gpx_global->urlname.queue);
    QUEUE_INIT(&gpx_global->keywords.queue);
  }

  fs_ptr = NULL;
}

static
void
gpx_rd_deinit(void)
{
  delete reader;
  reader = NULL;
  iqfile->close();
  delete iqfile;
  iqfile = NULL;
  wpt_tmp = NULL;
  cur_tag = NULL;
}

static void
gpx_wr_init(const char* fname)
{
  mkshort_handle = NULL;
  oqfile = new gpsbabel::File(fname);
  oqfile->open(QIODevice::WriteOnly | QIODevice::Text);

  writer = new gpsbabel::XmlStreamWriter(oqfile);
  writer->setAutoFormattingIndent(2);
  writer->writeStartDocument();
}

static void
gpx_wr_deinit(void)
{
  writer->writeEndDocument();
  delete writer;
  writer = NULL;
  oqfile->close();
  delete oqfile;
  oqfile = NULL;

  mkshort_del_handle(&mkshort_handle);
}

void
gpx_read(void)
{
  for (bool atEnd = false; !reader->atEnd() && !atEnd;)  {
    reader->readNext();
    // do processing
    switch (reader->tokenType()) {
    case QXmlStreamReader::StartElement:
      current_tag.append("/");
      current_tag.append(reader->qualifiedName());

      {
        const QXmlStreamAttributes attrs = reader->attributes();
        gpx_start(reader->qualifiedName().toString(), attrs);
      }
      break;

    case QXmlStreamReader::EndElement:
      gpx_end(reader->qualifiedName().toString());
      current_tag.chop(reader->qualifiedName().length() + 1);
      cdatastr.clear();
      break;

    case QXmlStreamReader::Characters:
//    It is tempting to skip this if reader->isWhitespace().
//    That would lose all whitespace element values if the exist,
//    but it would skip line endings and indentation that doesn't matter.
      gpx_cdata(reader->text().toString());
      break;

    case QXmlStreamReader::Invalid:
      atEnd = true;
      break;

    default:
      break;
    }
  }

  if (reader->hasError())  {
    Fatal() << MYNAME << "Read error:" << reader->errorString()
            << "File:" << iqfile->fileName()
            << "Line:" << reader->lineNumber()
            << "Column:" << reader->columnNumber();
  }
}

static void
write_tag_attributes(xml_tag* tag)
{
  char** pa;
  pa = tag->attributes;
  if (pa) {
    while (*pa) {
      writer->writeAttribute(pa[0], pa[1]);
      pa += 2;
    }
  }
}

static void
fprint_xml_chain(xml_tag* tag, const Waypoint* wpt)
{
  while (tag) {
    writer->writeStartElement(tag->tagname);

    if (tag->cdata.isEmpty() && !tag->child) {
      write_tag_attributes(tag);
      // No children?  Self-closing tag.
      writer->writeEndElement();
    } else {
      write_tag_attributes(tag);

      if (!tag->cdata.isEmpty()) {
        writer->writeCharacters(tag->cdata);
      }
      if (tag->child) {
        fprint_xml_chain(tag->child, wpt);
      }
      if (wpt && wpt->gc_data->exported.isValid() &&
          tag->tagname.compare("groundspeak:cache") == 0) {
        writer->writeTextElement("time",
                                 wpt->gc_data->exported.toPrettyString());
      }
      writer->writeEndElement();
    }
    if (!tag->parentcdata.isEmpty()) {
      // FIXME: The length check is necessary to get line endings correct in our test suite.
      // Writing the zero length string eats a newline, at least with Qt 4.6.2.
      writer->writeCharacters(tag->parentcdata);
    }
    tag = tag->sibling;
  }
}

void free_gpx_extras(xml_tag* tag)
{
  xml_tag* next = NULL;
  char** ap;

  while (tag) {
    if (tag->child) {
      free_gpx_extras(tag->child);
    }
    if (tag->attributes) {
      ap = tag->attributes;

      while (*ap) {
        xfree(*ap++);
      }

      xfree(tag->attributes);
    }

    next = tag->sibling;
    delete tag;
    tag = next;
  }
}

/*
 * Handle the grossness of GPX 1.0 vs. 1.1 handling of linky links.
 */
static void
write_gpx_url(const Waypoint* waypointp)
{
  if (!waypointp->HasUrlLink()) {
    return;
  }

  if (gpx_wversion_num > 10) {
    foreach(UrlLink l, waypointp->GetUrlLinks()) {
      writer->writeStartElement("link");
      writer->writeAttribute("href", l.url_);
      writer->writeOptionalTextElement("text", l.url_link_text_);
      writer->writeOptionalTextElement("type", l.url_link_type_);
      writer->writeEndElement();
    }
    return;
  }
  UrlLink l = waypointp->GetUrlLink();
  writer->writeTextElement("url", QString(urlbase) + QString(l.url_));
  writer->writeOptionalTextElement("urlname", QString(l.url_link_text_));
}

/*
 * Write optional accuracy information for a given (way|track|route)point
 * to the output stream.  Done in one place since it's common for all three.
 * Order counts.
 */
static void
gpx_write_common_acc(const Waypoint* waypointp)
{
  const char* fix = NULL;

  switch (waypointp->fix) {
  case fix_2d:
    fix = "2d";
    break;
  case fix_3d:
    fix = "3d";
    break;
  case fix_dgps:
    fix = "dgps";
    break;
  case fix_pps:
    fix = "pps";
    break;
  case fix_none:
    fix = "none";
    break;
    /* GPX spec says omit if we don't know. */
  case fix_unknown:
  default:
    break;
  }

  if (fix) {
    writer->writeTextElement("fix", fix);
  }
  if (waypointp->sat > 0) {
    writer->writeTextElement("sat", QString::number(waypointp->sat));
  }
  if (waypointp->hdop) {
    writer->writeTextElement("hdop", toString(waypointp->hdop));
  }
  if (waypointp->vdop) {
    writer->writeTextElement("vdop", toString(waypointp->vdop));
  }
  if (waypointp->pdop) {
    writer->writeTextElement("pdop", toString(waypointp->pdop));
  }
  /* TODO: ageofdgpsdata should go here */
  /* TODO: dgpsid should go here */
}


static void
gpx_write_common_position(const Waypoint* waypointp, const gpx_point_type point_type)
{
  if (waypointp->altitude != unknown_alt) {
    writer->writeTextElement("ele", QString::number(waypointp->altitude, 'f', 6));
  }
  QString t = waypointp->CreationTimeXML();
  writer->writeOptionalTextElement("time", t);
  if (gpxpt_track==point_type && 10 == gpx_wversion_num) {
    /* These were accidentally removed from 1.1, and were only a part of trkpts in 1.0 */
    if WAYPT_HAS(waypointp, course) {
      writer->writeTextElement("course", toString(waypointp->course));
    }
    if WAYPT_HAS(waypointp, speed) {
      writer->writeTextElement("speed", toString(waypointp->speed));
    }
  }
  /* TODO:  magvar should go here */
  if (WAYPT_HAS(waypointp, geoidheight)) {
    writer->writeOptionalTextElement("geoidheight",QString::number(waypointp->geoidheight, 'f', 1));
  }
}

static void
gpx_write_common_extensions(const Waypoint* waypointp, const gpx_point_type point_type)
{
  // gpx version we are writing is >= 1.1.
  if ((opt_humminbirdext && (WAYPT_HAS(waypointp, depth) || WAYPT_HAS(waypointp, temperature))) ||
      (opt_garminext && gpxpt_waypoint==point_type && (WAYPT_HAS(waypointp, proximity) || WAYPT_HAS(waypointp, temperature) || WAYPT_HAS(waypointp, depth))) ||
      (opt_garminext && gpxpt_track==point_type && (WAYPT_HAS(waypointp, temperature) || WAYPT_HAS(waypointp, depth) || waypointp->heartrate != 0 || waypointp->cadence != 0))) {
    writer->writeStartElement("extensions");

    if (opt_humminbirdext) {
      if (WAYPT_HAS(waypointp, depth)) {
        writer->writeTextElement("h:depth", toString(waypointp->depth * 100.0));
      }
      if (WAYPT_HAS(waypointp, temperature)) {
        writer->writeTextElement("h:temperature", toString(waypointp->temperature));
      }
    }

    if (opt_garminext) {
      // Although not required by the schema we assume that gpxx:WaypointExtension must be a child of gpx:wpt.
      // Although not required by the schema we assume that gpxx:RoutePointExtension must be a child of gpx:rtept.
      // Although not required by the schema we assume that gpxx:TrackPointExtension  must be a child of gpx:trkpt.
      // Although not required by the schema we assume that gpxtpx:TrackPointExtension must be a child of gpx:trkpt.
      switch (point_type) {
      case gpxpt_waypoint:
        if (WAYPT_HAS(waypointp, proximity) || WAYPT_HAS(waypointp, temperature) || WAYPT_HAS(waypointp, depth)) {
          writer->writeStartElement("gpxx:WaypointExtension");
          if (WAYPT_HAS(waypointp, proximity)) {
            writer->writeTextElement("gpxx:Proximity", toString(waypointp->proximity));
          }
          if (WAYPT_HAS(waypointp, temperature)) {
            writer->writeTextElement("gpxx:Temperature", toString(waypointp->temperature));
          }
          if (WAYPT_HAS(waypointp, depth)) {
            writer->writeTextElement("gpxx:Depth", toString(waypointp->depth));
          }
          writer->writeEndElement(); // "gpxx:WaypointExtension"
        }
        break;
      case gpxpt_route:
        /* we don't have any appropriate data for the children of gpxx:RoutePointExtension */
        break;
      case gpxpt_track:
        if (WAYPT_HAS(waypointp, temperature) || WAYPT_HAS(waypointp, depth) || waypointp->heartrate != 0 || waypointp->cadence != 0) {
          // gpxtpx:TrackPointExtension is a replacement for gpxx:TrackPointExtension.
          writer->writeStartElement("gpxtpx:TrackPointExtension");
          if (WAYPT_HAS(waypointp, temperature)) {
            writer->writeTextElement("gpxtpx:atemp", toString(waypointp->temperature));
          }
          if (WAYPT_HAS(waypointp, depth)) {
            writer->writeTextElement("gpxtpx:depth", toString(waypointp->depth));
          }
          if (waypointp->heartrate != 0) {
            writer->writeTextElement("gpxtpx:hr", QString::number(waypointp->heartrate));
          }
          if (waypointp->cadence != 0) {
            writer->writeTextElement("gpxtpx:cad", QString::number(waypointp->cadence));
          }
          writer->writeEndElement(); // "gpxtpx:TrackPointExtension"
        }
        break;
      }
    }

    writer->writeEndElement(); // "extensions"
  }
}

static void
gpx_write_common_description(const Waypoint* waypointp, QString oname)
{
  writer->writeOptionalTextElement("name", oname);

  writer->writeOptionalTextElement("cmt", waypointp->description);
  if (!waypointp->notes.isEmpty()) {
    writer->writeTextElement("desc", waypointp->notes);
  } else {
    writer->writeOptionalTextElement("desc", waypointp->description);
  }
  /* TODO: src should go here */
  write_gpx_url(waypointp);
  writer->writeOptionalTextElement("sym", waypointp->icon_descr);
  /* TODO: type should go here */
}

static void
gpx_waypt_pr(const Waypoint* waypointp)
{
  QString oname;
  fs_xml* fs_gpx;
  garmin_fs_t* gmsd;	/* gARmIN sPECIAL dATA */

  writer->writeStartElement("wpt");
  writer->writeAttribute("lat", toString(waypointp->latitude));
  writer->writeAttribute("lon", toString(waypointp->longitude));

  oname = global_opts.synthesize_shortnames ?
          mkshort_from_wpt(mkshort_handle, waypointp) :
          waypointp->shortname;
  gpx_write_common_position(waypointp, gpxpt_waypoint);
  gpx_write_common_description(waypointp, oname);
  gpx_write_common_acc(waypointp);

  if (!(opt_humminbirdext || opt_garminext)) {
    fs_gpx = (fs_xml*)fs_chain_find(waypointp->fs, FS_GPX);
    gmsd = GMSD_FIND(waypointp);
    if (fs_gpx) {
      if (! gmsd) {
        fprint_xml_chain(fs_gpx->tag, waypointp);
      }
    }
    if (gmsd && (gpx_wversion_num > 10)) {
      /* MapSource doesn't accepts extensions from 1.0 */
      garmin_fs_xml_fprint(waypointp, writer);
    }
  } else {
    gpx_write_common_extensions(waypointp, gpxpt_waypoint);
  }
  writer->writeEndElement();
}

static void
gpx_track_hdr(const route_head* rte)
{
  fs_xml* fs_gpx;
  current_trk_head = rte;

  writer->writeStartElement("trk");
  writer->writeOptionalTextElement("name", rte->rte_name);
  writer->writeOptionalTextElement("desc", rte->rte_desc);

  if (rte->rte_num) {
    writer->writeTextElement("number", QString::number(rte->rte_num));
  }

  if (gpx_wversion_num > 10) {
    if (!(opt_humminbirdext || opt_garminext)) {
      fs_gpx = (fs_xml*)fs_chain_find(rte->fs, FS_GPX);
      if (fs_gpx) {
        fprint_xml_chain(fs_gpx->tag, NULL);
      }
    } else if (opt_garminext) {
      if (rte->line_color.bbggrr > unknown_color) {
        int ci = gt_color_index_by_rgb(rte->line_color.bbggrr);
        if (ci > 0) {
          writer->writeStartElement("extensions");
          writer->writeStartElement("gpxx:TrackExtension");
          writer->writeTextElement("gpxx:DisplayColor", QString("%1")
                                   .arg(gt_color_name(ci)));
          writer->writeEndElement(); // Close gpxx:TrackExtension tag
          writer->writeEndElement(); // Close extensions tag
        }
      }
    }
  }
}

static void
gpx_track_disp(const Waypoint* waypointp)
{
  fs_xml* fs_gpx;
  int first_in_trk;
  first_in_trk = waypointp->Q.prev == &current_trk_head->waypoint_list;

  if (waypointp->wpt_flags.new_trkseg) {
    if (!first_in_trk) {
      writer->writeEndElement();
    }
    writer->writeStartElement("trkseg");
  }

  writer->writeStartElement("trkpt");
  writer->writeAttribute("lat", toString(waypointp->latitude));
  writer->writeAttribute("lon", toString(waypointp->longitude));

  gpx_write_common_position(waypointp, gpxpt_track);

  /* GPX doesn't require a name on output, so if we made one up
   * on input, we might as well say nothing.
   */
  QString oname;
  oname = global_opts.synthesize_shortnames ?
          mkshort_from_wpt(mkshort_handle, waypointp) :
          waypointp->shortname;
  gpx_write_common_description(waypointp,
                               waypointp->wpt_flags.shortname_is_synthetic ?
                               NULL : oname);
  gpx_write_common_acc(waypointp);

  if (!(opt_humminbirdext || opt_garminext)) {
    fs_gpx = (fs_xml*)fs_chain_find(waypointp->fs, FS_GPX);
    if (fs_gpx) {
      fprint_xml_chain(fs_gpx->tag, waypointp);
    }
  } else {
    gpx_write_common_extensions(waypointp, gpxpt_track);
  }
  writer->writeEndElement();
}

static void
gpx_track_tlr(const route_head*)
{
  if (!QUEUE_EMPTY(&current_trk_head->waypoint_list)) {
    writer->writeEndElement();
  }

  writer->writeEndElement();

  current_trk_head = NULL;
}

static
void gpx_track_pr()
{
  track_disp_all(gpx_track_hdr, gpx_track_tlr, gpx_track_disp);
}

static void
gpx_route_hdr(const route_head* rte)
{
  fs_xml* fs_gpx;

  writer->writeStartElement("rte");
  writer->writeOptionalTextElement("name", rte->rte_name);
  writer->writeOptionalTextElement("desc", rte->rte_desc);

  if (rte->rte_num) {
    writer->writeTextElement("number", QString::number(rte->rte_num));
  }

  if (gpx_wversion_num > 10) {
    if (!(opt_humminbirdext || opt_garminext)) {
      fs_gpx = (fs_xml*)fs_chain_find(rte->fs, FS_GPX);
      if (fs_gpx) {
        fprint_xml_chain(fs_gpx->tag, NULL);
      }
    } else if (opt_garminext) {
      if (rte->line_color.bbggrr > unknown_color) {
        int ci = gt_color_index_by_rgb(rte->line_color.bbggrr);
        if (ci > 0) {
          writer->writeStartElement("extensions");
          writer->writeStartElement("gpxx:RouteExtension");
          // FIXME: the value to use for IsAutoNamed is questionable.
          writer->writeTextElement("gpxx:IsAutoNamed", rte->rte_name.isEmpty()? "true" : "false"); // Required element
          writer->writeTextElement("gpxx:DisplayColor", QString("%1")
                                   .arg(gt_color_name(ci)));
          writer->writeEndElement(); // Close gpxx:RouteExtension tag
          writer->writeEndElement(); // Close extensions tag
        }
      }
    }
  }
}

static void
gpx_route_disp(const Waypoint* waypointp)
{
  QString oname;
  fs_xml* fs_gpx;
  writer->writeStartElement("rtept");
  writer->writeAttribute("lat", toString(waypointp->latitude));
  writer->writeAttribute("lon", toString(waypointp->longitude));

  oname = global_opts.synthesize_shortnames ?
          mkshort_from_wpt(mkshort_handle, waypointp) :
          waypointp->shortname;
  gpx_write_common_position(waypointp, gpxpt_route);
  gpx_write_common_description(waypointp, oname);
  gpx_write_common_acc(waypointp);

  if (!(opt_humminbirdext || opt_garminext)) {
    fs_gpx = (fs_xml*)fs_chain_find(waypointp->fs, FS_GPX);
    if (fs_gpx) {
      fprint_xml_chain(fs_gpx->tag, waypointp);
    }
  } else {
    gpx_write_common_extensions(waypointp, gpxpt_route);
  }
  writer->writeEndElement();
}

static void
gpx_route_tlr(const route_head*)
{
  writer->writeEndElement(); // Close rte tag.
}

static
void gpx_route_pr()
{
  /* output routes */
  route_disp_all(gpx_route_hdr, gpx_route_tlr, gpx_route_disp);
}

static void
gpx_waypt_bound_calc(const Waypoint* waypointp)
{
  waypt_add_to_bounds(&all_bounds, waypointp);
}

static void
gpx_write_bounds(void)
{
  waypt_init_bounds(&all_bounds);

  waypt_disp_all(gpx_waypt_bound_calc);
  route_disp_all(NULL, NULL, gpx_waypt_bound_calc);
  track_disp_all(NULL, NULL, gpx_waypt_bound_calc);

  if (waypt_bounds_valid(&all_bounds)) {
    writer->writeStartElement("bounds");
    writer->writeAttribute("minlat", toString(all_bounds.min_lat));
    writer->writeAttribute("minlon", toString(all_bounds.min_lon));
    writer->writeAttribute("maxlat", toString(all_bounds.max_lat));
    writer->writeAttribute("maxlon", toString(all_bounds.max_lon));
    writer->writeEndElement();
  }
}

static void
gpx_write(void)
{
  /* if an output version is not specified and an input version is
   * available use it, otherwise use the default.
   */

  if (!gpx_wversion) {
    if (gpx_version.isEmpty()) {
      gpx_wversion = (char*)"1.0";
    } else {
      // FIXME: this is gross.  The surrounding code is badly tortured by
      // there being three concepts of "output version", each with a different
      // data type (QString, int, char*).  This section needs a rethink. For
      // now, we stuff over the QString gpx_version into the global char *
      // gpx_wversion without making a malloc'ed copy.
      static char tmp[16];
      strncpy(tmp, CSTR(gpx_version), sizeof(tmp));
      gpx_wversion = tmp;
    }
  }

  if (opt_humminbirdext || opt_garminext) {
    gpx_wversion = (char*)"1.1";
  }

  gpx_wversion_num = strtod(gpx_wversion, NULL) * 10;

  if (gpx_wversion_num <= 0) {
    Fatal() << MYNAME << ": gpx version number of "
            << gpx_wversion << "not valid.";
  }

  // FIXME: This write of a blank line is needed for Qt 4.6 (as on Centos 6.3)
  // to include just enough whitespace between <xml/> and <gpx...> to pass
  // diff -w.  It's here for now to shim compatibility with our zillion
  // reference files, but this blank link can go away some day.
  writer->writeCharacters("\n");

  writer->setAutoFormatting(true);
  writer->writeStartElement("gpx");
  writer->writeAttribute("version", gpx_wversion);
  writer->writeAttribute("creator", CREATOR_NAME_URL);
  writer->writeAttribute("xmlns", QString("http://www.topografix.com/GPX/%1/%2").arg(gpx_wversion[0]).arg(gpx_wversion[2]));
  if (opt_humminbirdext || opt_garminext) {
    if (opt_humminbirdext) {
      writer->writeAttribute("xmlns:h","http://humminbird.com");
    }
    if (opt_garminext) {
      writer->writeAttribute("xmlns:gpxx", "http://www.garmin.com/xmlschemas/GpxExtensions/v3");
      writer->writeAttribute("xmlns:gpxtpx", "http://www.garmin.com/xmlschemas/TrackPointExtension/v1");
    }
  } else {
    writer->writeAttributes(gpx_namespace_attribute);
  }

  if (gpx_wversion_num > 10) {
    writer->writeStartElement("metadata");
  }
  gpx_write_gdata(&gpx_global->name, "name");
  gpx_write_gdata(&gpx_global->desc, "desc");
  /* In GPX 1.1, author changed from a string to a PersonType.
   * since it's optional, we just drop it instead of rewriting it.
   */
  if (gpx_wversion_num < 11) {
    gpx_write_gdata(&gpx_global->author, "author");
  }
  /* In GPX 1.1 email, url, urlname aren't allowed. */
  if (gpx_wversion_num < 11) {
    gpx_write_gdata(&gpx_global->email, "email");
    gpx_write_gdata(&gpx_global->url, "url");
    gpx_write_gdata(&gpx_global->urlname, "urlname");
  }

  gpsbabel::DateTime now = current_time();
  writer->writeTextElement("time", now.toPrettyString());

  gpx_write_gdata(&gpx_global->keywords, "keywords");

  gpx_write_bounds();

  if (gpx_wversion_num > 10) {
    writer->writeEndElement();
  }

  gpx_reset_short_handle();
  waypt_disp_all(gpx_waypt_pr);
  gpx_reset_short_handle();
  gpx_route_pr();
  gpx_reset_short_handle();
  gpx_track_pr();
  writer->writeEndElement(); // Close gpx tag.
}


static void
gpx_free_gpx_global(void)
{
  gpx_rm_from_global(&gpx_global->name);
  gpx_rm_from_global(&gpx_global->desc);
  gpx_rm_from_global(&gpx_global->author);
  gpx_rm_from_global(&gpx_global->email);
  gpx_rm_from_global(&gpx_global->url);
  gpx_rm_from_global(&gpx_global->urlname);
  gpx_rm_from_global(&gpx_global->keywords);
  xfree(gpx_global);
}

static void
gpx_exit(void)
{
  gpx_version.clear();

  gpx_namespace_attribute.clear();

  if (gpx_global) {
    gpx_free_gpx_global();
    gpx_global = NULL;
  }
}

static
arglist_t gpx_args[] = {
  {
    "snlen", &snlen, "Length of generated shortnames",
    "32", ARGTYPE_INT, "1", NULL, NULL
  },
  {
    "suppresswhite", &suppresswhite,
    "No whitespace in generated shortnames",
    NULL, ARGTYPE_BOOL, ARG_NOMINMAX, NULL
  },
  {
    "logpoint", &opt_logpoint,
    "Create waypoints from geocache log entries",
    NULL, ARGTYPE_BOOL, ARG_NOMINMAX, NULL
  },
  {
    "urlbase", &urlbase, "Base URL for link tag in output",
    NULL, ARGTYPE_STRING, ARG_NOMINMAX, NULL
  },
  {
    "gpxver", &gpx_wversion, "Target GPX version for output",
    NULL, ARGTYPE_STRING, ARG_NOMINMAX, NULL
  },
  {
    "humminbirdextensions", &opt_humminbirdext,
    "Add info (depth) as Humminbird extension",
    NULL, ARGTYPE_BOOL, ARG_NOMINMAX, NULL
  },
  {
    "garminextensions", &opt_garminext,
    "Add info (depth) as Garmin extension",
    NULL, ARGTYPE_BOOL, ARG_NOMINMAX, NULL
  },
  ARG_TERMINATOR
};

ff_vecs_t gpx_vecs = {
  ff_type_file,
  FF_CAP_RW_ALL,
  gpx_rd_init,
  gpx_wr_init,
  gpx_rd_deinit,
  gpx_wr_deinit,
  gpx_read,
  gpx_write,
  gpx_exit,
  gpx_args,
  CET_CHARSET_UTF8, 0,	/* non-fixed to create non UTF-8 XML's for testing | CET-REVIEW */
  NULL_POS_OPS,
  NULL,
};
