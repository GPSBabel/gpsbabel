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

#include <cmath>                                   // for lround
#include <cstdio>                                  // for sscanf
#include <cstdlib>                                 // for atoi, strtod
#include <cstring>                                 // for strchr, strncpy

#include <QDate>                                   // for QDate
#include <QDateTime>                               // for QDateTime
#include <QHash>                                   // for QHash
#include <QIODevice>                               // for QIODevice, operator|, QIODevice::ReadOnly, QIODevice::Text, QIODevice::WriteOnly
#include <QLatin1Char>                             // for QLatin1Char
#include <QLatin1String>                           // for QLatin1String
#include <QString>                                 // for QString, QStringLiteral, operator+, operator==
#include <QStringList>                             // for QStringList
#include <QStringView>                             // for QStringView
#include <QTime>                                   // for QTime
#include <QVersionNumber>                          // for QVersionNumber
#include <QXmlStreamAttribute>                     // for QXmlStreamAttribute
#include <QXmlStreamAttributes>                    // for QXmlStreamAttributes
#include <QXmlStreamNamespaceDeclaration>          // for QXmlStreamNamespaceDeclaration
#include <QXmlStreamNamespaceDeclarations>         // for QXmlStreamNamespaceDeclarations
#include <QXmlStreamReader>                        // for QXmlStreamReader, QXmlStreamReader::Characters, QXmlStreamReader::EndDocument, QXmlStreamReader::EndElement, QXmlStreamReader::Invalid, QXmlStreamReader::StartElement
#include <Qt>                                      // for CaseInsensitive, UTC
#include <QtGlobal>                                // for qAsConst, QAddConst<>::Type

#include "defs.h"
#include "gpx.h"
#include "garmin_fs.h"                             // for garmin_fs_xml_convert, garmin_fs_xml_fprint, GMSD_FIND
#include "garmin_tables.h"                         // for gt_color_index_by_rgb, gt_color_name, gt_color_value_by_name
#include "src/core/datetime.h"                     // for DateTime
#include "src/core/file.h"                         // for File
#include "src/core/logging.h"                      // for Warning, Fatal
#include "src/core/xmlstreamwriter.h"              // for XmlStreamWriter
#include "src/core/xmltag.h"                       // for xml_tag, fs_xml, fs_xml_alloc, free_gpx_extras


#define MYNAME "GPX"
#ifndef CREATOR_NAME_URL
#  define CREATOR_NAME_URL "GPSBabel - https://www.gpsbabel.org"
#endif

void
GpxFormat::gpx_add_to_global(QStringList& ge, const QString& s)
{
  if (!ge.contains(s)) {
    ge.append(s);
  }
}

// Temporarily mock the old GPX writer's hardcoded fixed length for float/double
// types.  This can be removed once we have time/interest in regenerating all our
// zillion reference files.
inline QString GpxFormat::toString(double d)
{
  return QString::number(d, 'f', 9);
}

inline QString GpxFormat::toString(float f)
{
  return QString::number(f, 'f', 6);
}


/*
 * gpx_reset_short_handle: used for waypoint, route and track names
 * this allows gpx:wpt names to overlap gpx:rtept names, etc.
 */
void
GpxFormat::gpx_reset_short_handle()
{
  if (mkshort_handle != nullptr) {
    mkshort_del_handle(&mkshort_handle);
  }

  mkshort_handle = mkshort_new_handle();

  if (suppresswhite) {
    setshort_whitespace_ok(mkshort_handle, 0);
  }

  setshort_length(mkshort_handle, atoi(snlen));
}

void
GpxFormat::gpx_write_gdata(const QStringList& ge, const QString& tag) const
{
  if (!ge.isEmpty()) {
    writer->writeStartElement(tag);
    // TODO: This seems questionable.
    // We concatenate element content from multiple elements,
    // possibly from multiple input files, into one element.
    // This is necessary to comply with the schema as
    // these elements have maxOccurs limits of 1.
    for (const auto& str : ge) {
      writer->writeCharacters(str);
      /* Some tags we just output once. */
      if ((tag == QLatin1String("url")) ||
          (tag == QLatin1String("email"))) {
        break;
      }
    }
    writer->writeEndElement();
  }
}

GpxFormat::tag_mapping
GpxFormat::get_tag(const QString& t) const
{
  // returns default constructed value if key not found.
  return hash.value(t);
}

void
GpxFormat::tag_gpx(const QXmlStreamAttributes& attr)
{
  if (attr.hasAttribute(QLatin1String("version"))) {
    /* Set the default output version to the highest input
     * version.
     */
    QVersionNumber thisVersion = QVersionNumber::fromString(attr.value(QLatin1String("version")).toString()).normalized();
    if (gpx_highest_version_read.isNull()) {
      gpx_highest_version_read = thisVersion;
    } else if (!thisVersion.isNull() && (gpx_highest_version_read < thisVersion)) {
      gpx_highest_version_read = thisVersion;
    }
  }
  /* save namespace declarations in case we pass through elements
   * that use them to the writer.
   */
  const QXmlStreamNamespaceDeclarations ns = reader->namespaceDeclarations();
  for (const auto& n : ns) {
    QString prefix = n.prefix().toString();
    QString namespaceUri = n.namespaceUri().toString();
    /* don't toss any xsi declaration, it might used for tt_unknown or passthrough. */
    if (!prefix.isEmpty()) {
      if (! gpx_namespace_attribute.hasAttribute(prefix.prepend("xmlns:"))) {
        gpx_namespace_attribute.append(prefix, namespaceUri);
      }
    }
  }
}

void
GpxFormat::tag_wpt(const QXmlStreamAttributes& attr)
{
  wpt_tmp = new Waypoint;
  link_ = new UrlLink;

  cur_tag = nullptr;
  if (attr.hasAttribute(QLatin1String("lat"))) {
    wpt_tmp->latitude = attr.value(QLatin1String("lat")).toDouble();
  }
  if (attr.hasAttribute(QLatin1String("lon"))) {
    wpt_tmp->longitude = attr.value(QLatin1String("lon")).toDouble();
  }
  fs_ptr = &wpt_tmp->fs;
}

void
GpxFormat::tag_cache_desc(const QXmlStreamAttributes& attr)
{
  cache_descr_is_html = false;
  if (attr.hasAttribute(QLatin1String("html"))) {
    if (attr.value(QLatin1String("html")).compare(QLatin1String("True")) == 0) {
      cache_descr_is_html = true;
    }
  }
}

void
GpxFormat::tag_gs_cache(const QXmlStreamAttributes& attr) const
{
  geocache_data* gc_data = wpt_tmp->AllocGCData();

  if (attr.hasAttribute(QLatin1String("id"))) {
    gc_data->id = attr.value(QLatin1String(QLatin1String("id"))).toLongLong();
  }
  if (attr.hasAttribute(QLatin1String("available"))) {
    if (attr.value(QLatin1String("available")).compare(QLatin1String("True"), Qt::CaseInsensitive) == 0) {
      gc_data->is_available = status_true;
    } else if (attr.value(QLatin1String("available")).compare(QLatin1String("False"), Qt::CaseInsensitive) == 0) {
      gc_data->is_available = status_false;
    }
  }
  if (attr.hasAttribute(QLatin1String("archived"))) {
    if (attr.value(QLatin1String("archived")).compare(QLatin1String("True"), Qt::CaseInsensitive) == 0) {
      gc_data->is_archived = status_true;
    } else if (attr.value(QLatin1String("archived")).compare(QLatin1String("False"), Qt::CaseInsensitive) == 0) {
      gc_data->is_archived = status_false;
    }
  }
}

void
GpxFormat::start_something_else(QStringView el, const QXmlStreamAttributes& attr)
{
  if (!fs_ptr) {
    return;
  }

  auto* new_tag = new xml_tag;
  new_tag->tagname = el.toString();

  const QXmlStreamNamespaceDeclarations ns = reader->namespaceDeclarations();
  new_tag->attributes.reserve(attr.size() + ns.size());
  /*
   * It was found to be faster to append one element at a time compared to
   *   a) assiging new_tag->attributes = attr, or
   *   b) appending in one step, new_tag.attributes.apppend(attr)
   * Tested with on ubuntu bionic with Qt 5.9.5 and a large Geocache file
   * generated by Groundspeak.
   */
  for (const auto& a : attr) {
    new_tag->attributes.append(a);
  }
  for (const auto& n : ns) {
    QString prefix = n.prefix().toString().prepend(n.prefix().isEmpty()? "xmlns" : "xmlns:");
    QString namespaceUri = n.namespaceUri().toString();
    new_tag->attributes.append(prefix, namespaceUri);
  }

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
    const auto* fs_gpx = reinterpret_cast<fs_xml*>(fs_ptr->FsChainFind(kFsGpx));

    if (fs_gpx && fs_gpx->tag) {
      cur_tag = fs_gpx->tag;
      while (cur_tag->sibling) {
        cur_tag = cur_tag->sibling;
      }
      cur_tag->sibling = new_tag;
      new_tag->parent = nullptr;
    } else {
      fs_xml* new_fs_gpx = fs_xml_alloc(kFsGpx);
      new_fs_gpx->tag = new_tag;
      fs_ptr->FsChainAdd(new_fs_gpx);
      new_tag->parent = nullptr;
    }
  }
  cur_tag = new_tag;
}

void
GpxFormat::end_something_else()
{
  if (cur_tag) {
    cur_tag = cur_tag->parent;
  }
}

void
GpxFormat::tag_log_wpt(const QXmlStreamAttributes& attr) const
{
  /* create a new waypoint */
  auto* lwp_tmp = new Waypoint;

  /* extract the lat/lon attributes */
  if (attr.hasAttribute(QLatin1String("lat"))) {
    lwp_tmp->latitude = attr.value(QLatin1String("lat")).toDouble();
  }
  if (attr.hasAttribute(QLatin1String("lon"))) {
    lwp_tmp->longitude = attr.value(QLatin1String("lon")).toDouble();
  }
  /* Make a new shortname.  Since this is a groundspeak extension,
    we assume that GCBLAH is the current shortname format and that
    wpt_tmp refers to the currently parsed waypoint. Unfortunately,
    we need to keep track of log_wpt counts so we don't collide with
    dupe shortnames.
  */
  if (wpt_tmp->shortname.size() > 2) {
// FIXME: think harder about this later.
    lwp_tmp->shortname = wpt_tmp->shortname.mid(2, 4) + "-FIXME";

    waypt_add(lwp_tmp);
  }
}

void
GpxFormat::gpx_start(QStringView el, const QXmlStreamAttributes& attr)
{
  /*
   * Reset end-of-string without actually emptying/reallocing cdatastr.
   */
  cdatastr = QString();

  tag_mapping tag = get_tag(current_tag);
  switch (tag.type) {
  case tt_gpx:
    tag_gpx(attr);
    break;
  case tt_link:
    if (attr.hasAttribute(QLatin1String("href"))) {
      link_url = attr.value(QLatin1String("href")).toString();
    }
    break;
  case tt_wpt:
    tag_wpt(attr);
    break;
  case tt_wpttype_link:
    if (attr.hasAttribute(QLatin1String("href"))) {
      link_url = attr.value(QLatin1String("href")).toString();
    }
    break;
  case tt_rte:
    rte_head = new route_head;
    route_add_head(rte_head);
    rh_link_ = new UrlLink;
    fs_ptr = &rte_head->fs;
    break;
  case tt_rte_rtept:
    tag_wpt(attr);
    break;
  case tt_trk:
    trk_head = new route_head;
    track_add_head(trk_head);
    rh_link_ = new UrlLink;
    fs_ptr = &trk_head->fs;
    break;
  case tt_trk_trkseg_trkpt:
    tag_wpt(attr);
    if (next_trkpt_is_new_seg) {
      wpt_tmp->wpt_flags.new_trkseg = 1;
      next_trkpt_is_new_seg = 0;
    }
    break;
  case tt_rte_link:
  case tt_trk_link:
    if (attr.hasAttribute(QLatin1String("href"))) {
      link_url = attr.value(QLatin1String("href")).toString();
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
    if (attr.hasAttribute(QLatin1String("id"))) {
      wpt_tmp->AllocGCData()->placer_id = attr.value(QLatin1String("id")).toInt();
    }
  default:
    break;
  }
  if (tag.passthrough) {
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
  { gt_surprise, "Unknown Cache" },
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
  int sz = sizeof(gs_type_map) / sizeof(gs_type_map[0]);

  for (int i = 0; i < sz; i++) {
    if (!t.compare(gs_type_map[i].name,Qt::CaseInsensitive)) {
      return gs_type_map[i].type;
    }
  }
  return gt_unknown;
}

const char*
gs_get_cachetype(geocache_type t)
{
  int sz = sizeof(gs_type_map) / sizeof(gs_type_map[0]);

  for (int i = 0; i < sz; i++) {
    if (t == gs_type_map[i].type) {
      return gs_type_map[i].name;
    }
  }
  return "Unknown";
}

geocache_container
gs_mkcont(const QString& t)
{
  int sz = sizeof(gs_container_map) / sizeof(gs_container_map[0]);

  for (int i = 0; i < sz; i++) {
    if (!t.compare(gs_container_map[i].name,Qt::CaseInsensitive)) {
      return gs_container_map[i].type;
    }
  }
  return gc_unknown;
}

const char*
gs_get_container(geocache_container t)
{
  int sz = sizeof(gs_container_map) / sizeof(gs_container_map[0]);

  for (int i = 0; i < sz; i++) {
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

  QByteArray dts = dateTimeString.toUtf8();
  char* timestr = dts.data();

  char* offsetstr = strchr(timestr, 'Z');
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
  char* pointstr = strchr(timestr, '.');
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
    dt = QDateTime(date, time, Qt::UTC);

    // Fractional part of time.
    if (fsec) {
      dt = dt.addMSecs(lround(fsec * 1000));
    }

    // Any offsets that were stuck at the end.
    dt = dt.addSecs(-off_sign * off_hr * 3600 - off_sign * off_min * 60);
  } else {
    dt = QDateTime();
  }
  return dt;
}

void
GpxFormat::gpx_end(QStringView /*unused*/)
{
  static QDateTime gc_log_date;

  // Remove leading, trailing whitespace.
  cdatastr = cdatastr.trimmed();

  tag_mapping tag = get_tag(current_tag);

  switch (tag.type) {
  /*
   * First, the tags that are file-global.
   */
  case tt_name:
    gpx_add_to_global(gpx_global->name, cdatastr);
    break;
  case tt_desc:
    gpx_add_to_global(gpx_global->desc, cdatastr);
    break;
  case tt_author:
    gpx_add_to_global(gpx_global->author, cdatastr);
    break;
  case tt_email:
    gpx_add_to_global(gpx_global->email, cdatastr);
    break;
  case tt_url:
    gpx_add_to_global(gpx_global->url, cdatastr);
    break;
  case tt_urlname:
    gpx_add_to_global(gpx_global->urlname, cdatastr);
    break;
  case tt_keywords:
    gpx_add_to_global(gpx_global->keywords, cdatastr);
    break;
  case tt_link:
    (gpx_global->link).AddUrlLink(UrlLink(link_url, link_text, link_type));
    link_type.clear();
    link_text.clear();
    link_url.clear();
    break;
  case tt_link_text:
    link_text = cdatastr;
    break;
  case tt_link_type:
    link_type = cdatastr;
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
      link_ = nullptr;
    }
    waypt_add(wpt_tmp);
    logpoint_ct = 0;
    cur_tag = nullptr;
    wpt_tmp = nullptr;
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
    wpt_tmp->AllocGCData()->diff = cdatastr.toFloat() * 10;
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
    wpt_tmp->AllocGCData()->terr = cdatastr.toFloat() * 10;
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
    if ((cdatastr.compare(QLatin1String("Found it")) == 0) &&
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
    garmin_fs_xml_convert(tt_garmin_wpt_extensions, tag.type, cdatastr, wpt_tmp);
    break;

  /*
   * Humminbird-waypoint-specific tags.
   */
  case tt_humminbird_wpt_depth:
  case tt_humminbird_trk_trkseg_trkpt_depth:
    WAYPT_SET(wpt_tmp, depth, cdatastr.toDouble() / 100.0);
    break;
  /*
   * Route-specific tags.
   */
  case tt_rte_name:
    rte_head->rte_name = cdatastr;
    break;
  case tt_rte:
    if (rh_link_) {
      if (!rh_link_->url_.isEmpty()) {
        rte_head->rte_urls.AddUrlLink(*rh_link_);
      }
      delete rh_link_;
      rh_link_ = nullptr;
    }
    break;
  case tt_rte_rtept:
    if (link_) {
      if (!link_->url_.isEmpty()) {
        wpt_tmp->AddUrlLink(*link_);
      }
      delete link_;
      link_ = nullptr;
    }
    route_add_wpt(rte_head, wpt_tmp);
    wpt_tmp = nullptr;
    break;
  case tt_rte_desc:
    rte_head->rte_desc = cdatastr;
    break;
  case tt_garmin_rte_display_color:
    rte_head->line_color.bbggrr = gt_color_value_by_name(cdatastr);
    break;
  case tt_rte_link:
    rte_head->rte_urls.AddUrlLink(UrlLink(link_url, link_text, link_type));
    link_type.clear();
    link_text.clear();
    link_url.clear();
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
    if (rh_link_) {
      if (!rh_link_->url_.isEmpty()) {
        trk_head->rte_urls.AddUrlLink(*rh_link_);
      }
      delete rh_link_;
      rh_link_ = nullptr;
    }
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
      link_ = nullptr;
    }
    track_add_wpt(trk_head, wpt_tmp);
    wpt_tmp = nullptr;
    break;
  case tt_trk_desc:
    trk_head->rte_desc = cdatastr;
    break;
  case tt_garmin_trk_display_color:
    trk_head->line_color.bbggrr = gt_color_value_by_name(cdatastr);
    break;
  case tt_trk_link:
    trk_head->rte_urls.AddUrlLink(UrlLink(link_url, link_text, link_type));
    link_type.clear();
    link_text.clear();
    link_url.clear();
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
  case tt_rte_url:
  case tt_trk_url:
    rh_link_->url_ = cdatastr;
    break;
  case tt_rte_urlname:
  case tt_trk_urlname:
    rh_link_->url_link_text_ = cdatastr;
    break;
  case tt_rte_link_text:
  case tt_trk_link_text:
    link_text = cdatastr;
    break;
  case tt_rte_link_type:
  case tt_trk_link_type:
    link_type = cdatastr;
    break;
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
    wpt_tmp->pdop = cdatastr.toFloat();
    break;
  case tt_wpttype_hdop:
    wpt_tmp->hdop = cdatastr.toFloat();
    break;
  case tt_wpttype_vdop:
    wpt_tmp->vdop = cdatastr.toFloat();
    break;
  case tt_wpttype_sat:
    wpt_tmp->sat = cdatastr.toInt();
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
    link_type.clear();
    link_text.clear();
    link_url.clear();
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

  if (tag.passthrough) {
    end_something_else();
  }

}


void
GpxFormat::gpx_cdata(QStringView s)
{
  QString* cdata;
  cdatastr += s.toString();

  if (!cur_tag) {
    return;
  }

  if (cur_tag->child) {
    xml_tag* tmp_tag = cur_tag->child;
    while (tmp_tag->sibling) {
      tmp_tag = tmp_tag->sibling;
    }
    cdata = &(tmp_tag->parentcdata);
  } else {
    cdata = &(cur_tag->cdata);
  }
  *cdata = cdatastr.trimmed();
}

void
GpxFormat::rd_init(const QString& fname)
{
  iqfile = new gpsbabel::File(fname);
  iqfile->open(QIODevice::ReadOnly);
  reader = new QXmlStreamReader(iqfile);

  current_tag.clear();

  cdatastr = QString();

  if (nullptr == gpx_global) {
    gpx_global = new GpxGlobal;
  }

  fs_ptr = nullptr;
}

void
GpxFormat::rd_deinit()
{
  delete reader;
  reader = nullptr;
  iqfile->close();
  delete iqfile;
  iqfile = nullptr;
  wpt_tmp = nullptr;
  cur_tag = nullptr;
}

void
GpxFormat::wr_init(const QString& fname)
{
  mkshort_handle = nullptr;
  oqfile = new gpsbabel::File(fname);
  oqfile->open(QIODevice::WriteOnly | QIODevice::Text);

  writer = new gpsbabel::XmlStreamWriter(oqfile);
  writer->setAutoFormattingIndent(2);
  writer->writeStartDocument();

  /* if an output version is not specified and an input version is
  * available use it, otherwise use the default.
  */

  if (opt_gpxver != nullptr) {
    gpx_write_version = QVersionNumber::fromString(opt_gpxver).normalized();
  } else if (!gpx_highest_version_read.isNull()) {
    gpx_write_version = gpx_highest_version_read;
  } else {
    gpx_write_version = gpx_1_0;
  }

  if (opt_humminbirdext || opt_garminext) {
    gpx_write_version = gpx_1_1;
  }

  // It's a good thing 0, 0.0, 0.0.0 aren't valid gpx versions,
  // normalization makes them null.
  if (gpx_write_version.isNull() || (gpx_write_version < gpx_1_0)) {
    fatal(FatalMsg() << MYNAME ": gpx version number"
            << gpx_write_version << "not valid.");
  }

  writer->setAutoFormatting(true);
  writer->writeStartElement(QStringLiteral("gpx"));
  writer->writeAttribute(QStringLiteral("version"),
                         QStringLiteral("%1.%2")
                         .arg(gpx_write_version.majorVersion())
                         .arg(gpx_write_version.minorVersion()));
  writer->writeAttribute(QStringLiteral("creator"), CREATOR_NAME_URL);
  writer->writeAttribute(QStringLiteral("xmlns"),
                         QStringLiteral("http://www.topografix.com/GPX/%1/%2")
                         .arg(gpx_write_version.majorVersion())
                         .arg(gpx_write_version.minorVersion()));
  if (opt_humminbirdext || opt_garminext) {
    if (opt_humminbirdext) {
      writer->writeAttribute(QStringLiteral("xmlns:h"), QStringLiteral("http://humminbird.com"));
    }
    if (opt_garminext) {
      writer->writeAttribute(QStringLiteral("xmlns:gpxx"),
                             QStringLiteral("http://www.garmin.com/xmlschemas/GpxExtensions/v3"));
      writer->writeAttribute(QStringLiteral("xmlns:gpxtpx"),
                             QStringLiteral("http://www.garmin.com/xmlschemas/TrackPointExtension/v1"));
    }
  } else {
    writer->writeAttributes(gpx_namespace_attribute);
  }

  if (gpx_write_version > gpx_1_0) {
    writer->writeStartElement(QStringLiteral("metadata"));
  }
  if (gpx_global) {
    gpx_write_gdata(gpx_global->name, QStringLiteral("name"));
    gpx_write_gdata(gpx_global->desc, QStringLiteral("desc"));
  }
  /* In GPX 1.1, author changed from a string to a PersonType.
   * since it's optional, we just drop it instead of rewriting it.
   */
  if (gpx_write_version < gpx_1_1) {
    if (gpx_global) {
      gpx_write_gdata(gpx_global->author, QStringLiteral("author"));
    }
  } // else {
  // TODO: gpx 1.1 author goes here.
  //}
  /* In GPX 1.1 email, url, urlname aren't allowed. */
  if (gpx_write_version < gpx_1_1) {
    if (gpx_global) {
      gpx_write_gdata(gpx_global->email, QStringLiteral("email"));
      gpx_write_gdata(gpx_global->url, QStringLiteral("url"));
      gpx_write_gdata(gpx_global->urlname, QStringLiteral("urlname"));
    }
  } else {
    if (gpx_global) {
      // TODO: gpx 1.1 copyright goes here
      for (const auto& l : qAsConst(gpx_global->link)) {
        writer->writeStartElement(QStringLiteral("link"));
        writer->writeAttribute(QStringLiteral("href"), l.url_);
        writer->writeOptionalTextElement(QStringLiteral("text"), l.url_link_text_);
        writer->writeOptionalTextElement(QStringLiteral("type"), l.url_link_type_);
        writer->writeEndElement();
      }
    }
  }

  gpsbabel::DateTime now = current_time();
  writer->writeTextElement(QStringLiteral("time"), now.toPrettyString());

  if (gpx_global) {
    gpx_write_gdata(gpx_global->keywords, QStringLiteral("keywords"));
  }

  gpx_write_bounds();

  // TODO: gpx 1.1 extensions go here.

  if (gpx_write_version > gpx_1_0) {
    writer->writeEndElement();
  }

}

void
GpxFormat::wr_deinit()
{
  writer->writeEndDocument();
  delete writer;
  writer = nullptr;
  oqfile->close();
  delete oqfile;
  oqfile = nullptr;

  mkshort_del_handle(&mkshort_handle);
}

void
GpxFormat::read()
{
  for (bool atEnd = false; !reader->atEnd() && !atEnd;)  {
    reader->readNext();
    // do processing
    switch (reader->tokenType()) {
    case QXmlStreamReader::StartElement:
      current_tag.append(QLatin1Char('/'));
      current_tag.append(reader->qualifiedName());
      gpx_start(reader->qualifiedName(), reader->attributes());
      break;

    case QXmlStreamReader::EndElement:
      gpx_end(reader->qualifiedName());
      current_tag.chop(reader->qualifiedName().length() + 1);
      cdatastr.clear();
      break;

    case QXmlStreamReader::Characters:
//    It is tempting to skip this if reader->isWhitespace().
//    That would lose all whitespace element values if the exist,
//    but it would skip line endings and indentation that doesn't matter.
      gpx_cdata(reader->text());
      break;

//  On windows with input redirection we can read an Invalid token
//  after the EndDocument token.  This also will set an error
//  "Premature end of document." that we will fatal on below.
//  This occurs with Qt 5.9.2 on windows when the file being
//  sent to stdin has dos line endings.
//  This does NOT occur with Qt 5.9.2 on windows when the file being
//  sent to stdin has unix line endings.
//  This does NOT occur with Qt 5.9.2 on windows with either line
//  endings if the file is read directly, i.e. not sent through stdin.
//  An example of a problematic file is reference/basecamp.gpx,
//  which fails on windows with this invocation from a command prompt:
//  .\GPSBabel.exe -i gpx -f - < reference\basecamp.gpx
//  This was demonstrated on 64 bit windows 10.  Other versions of
//  windows and Qt likely fail as well.
//  To avoid this we quit reading when we see the EndDocument.
//  This does not prevent us from correctly detecting the error
//  "Extra content at end of document."
    case QXmlStreamReader::EndDocument:
    case QXmlStreamReader::Invalid:
      atEnd = true;
      break;

    default:
      break;
    }
  }

  if (reader->hasError())  {
    fatal(FatalMsg() << MYNAME << "Read error:" << reader->errorString()
            << "File:" << iqfile->fileName()
            << "Line:" << reader->lineNumber()
            << "Column:" << reader->columnNumber());
  }
}

void
GpxFormat::write_attributes(const QXmlStreamAttributes& attributes) const
{
  for (const auto& attribute : attributes) {
    writer->writeAttribute(attribute.qualifiedName().toString(), attribute.value().toString());
  }
}

void
GpxFormat::fprint_xml_chain(xml_tag* tag, const Waypoint* wpt) const
{
  while (tag) {
    writer->writeStartElement(tag->tagname);
    write_attributes(tag->attributes);
    if (tag->cdata.isEmpty() && !tag->child) {
      // No children?  Self-closing tag.
      writer->writeEndElement();
    } else {
      if (!tag->cdata.isEmpty()) {
        writer->writeCharacters(tag->cdata);
      }
      if (tag->child) {
        fprint_xml_chain(tag->child, wpt);
      }
      if (wpt && wpt->gc_data->exported.isValid() &&
          tag->tagname.compare(QLatin1String("groundspeak:cache")) == 0) {
        writer->writeTextElement(QStringLiteral("time"),
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

/*
 * Handle the grossness of GPX 1.0 vs. 1.1 handling of linky links.
 */
void
GpxFormat::write_gpx_url(const UrlList& urls) const
{
  if (gpx_write_version > gpx_1_0) {
    for (const auto& l : urls) {
      if (!l.url_.isEmpty()) {
        writer->writeStartElement(QStringLiteral("link"));
        writer->writeAttribute(QStringLiteral("href"), l.url_);
        writer->writeOptionalTextElement(QStringLiteral("text"), l.url_link_text_);
        writer->writeOptionalTextElement(QStringLiteral("type"), l.url_link_type_);
        writer->writeEndElement();
      }
    }
  } else {
    UrlLink l = urls.GetUrlLink();
    if (!l.url_.isEmpty()) {
      writer->writeTextElement(QStringLiteral("url"), QString(urlbase) + l.url_);
      writer->writeOptionalTextElement(QStringLiteral("urlname"), l.url_link_text_);
    }
  }
}

void
GpxFormat::write_gpx_url(const Waypoint* waypointp) const
{
  if (waypointp->HasUrlLink()) {
    write_gpx_url(waypointp->urls);
  }
}

void
GpxFormat::write_gpx_url(const route_head* rh) const
{
  if (rh->rte_urls.HasUrlLink()) {
    write_gpx_url(rh->rte_urls);
  }
}

/*
 * Write optional accuracy information for a given (way|track|route)point
 * to the output stream.  Done in one place since it's common for all three.
 * Order counts.
 */
void
GpxFormat::gpx_write_common_acc(const Waypoint* waypointp) const
{
  const char* fix = nullptr;

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
    writer->writeTextElement(QStringLiteral("fix"), fix);
  }
  if (waypointp->sat > 0) {
    writer->writeTextElement(QStringLiteral("sat"), QString::number(waypointp->sat));
  }
  if (waypointp->hdop) {
    writer->writeTextElement(QStringLiteral("hdop"), toString(waypointp->hdop));
  }
  if (waypointp->vdop) {
    writer->writeTextElement(QStringLiteral("vdop"), toString(waypointp->vdop));
  }
  if (waypointp->pdop) {
    writer->writeTextElement(QStringLiteral("pdop"), toString(waypointp->pdop));
  }
  /* TODO: ageofdgpsdata should go here */
  /* TODO: dgpsid should go here */
}


void
GpxFormat::gpx_write_common_position(const Waypoint* waypointp, const gpx_point_type point_type) const
{
  if (waypointp->altitude != unknown_alt) {
    writer->writeTextElement(QStringLiteral("ele"), QString::number(waypointp->altitude, 'f', elevation_precision));
  }
  QString t = waypointp->CreationTimeXML();
  writer->writeOptionalTextElement(QStringLiteral("time"), t);
  if (gpxpt_track==point_type && gpx_1_0 == gpx_write_version) {
    /* These were accidentally removed from 1.1, and were only a part of trkpts in 1.0 */
    if WAYPT_HAS(waypointp, course) {
      writer->writeTextElement(QStringLiteral("course"), toString(waypointp->course));
    }
    if WAYPT_HAS(waypointp, speed) {
      writer->writeTextElement(QStringLiteral("speed"), toString(waypointp->speed));
    }
  }
  /* TODO:  magvar should go here */
  if (WAYPT_HAS(waypointp, geoidheight)) {
    writer->writeOptionalTextElement(QStringLiteral("geoidheight"),QString::number(waypointp->geoidheight, 'f', 1));
  }
}

void
GpxFormat::gpx_write_common_extensions(const Waypoint* waypointp, const gpx_point_type point_type) const
{
  // gpx version we are writing is >= 1.1.
  garmin_fs_t* gmsd = (opt_garminext) ? garmin_fs_t::find(waypointp) : nullptr;  // only needed if garmin extensions selected

  if ((opt_humminbirdext && (WAYPT_HAS(waypointp, depth) || WAYPT_HAS(waypointp, temperature))) ||
      (opt_garminext && gpxpt_route==point_type && gmsd != nullptr && gmsd->ilinks != nullptr)  ||
      (opt_garminext && gpxpt_waypoint==point_type && (WAYPT_HAS(waypointp, proximity) || WAYPT_HAS(waypointp, temperature) || WAYPT_HAS(waypointp, depth))) ||
      (opt_garminext && gpxpt_track==point_type && (WAYPT_HAS(waypointp, temperature) || WAYPT_HAS(waypointp, depth) || waypointp->heartrate != 0 || waypointp->cadence != 0))) {
    writer->writeStartElement(QStringLiteral("extensions"));

    if (opt_humminbirdext) {
      if (WAYPT_HAS(waypointp, depth)) {
        writer->writeTextElement(QStringLiteral("h:depth"), toString(waypointp->depth * 100.0));
      }
      if (WAYPT_HAS(waypointp, temperature)) {
        writer->writeTextElement(QStringLiteral("h:temperature"), toString(waypointp->temperature));
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
          writer->writeStartElement(QStringLiteral("gpxx:WaypointExtension"));
          if (WAYPT_HAS(waypointp, proximity)) {
            writer->writeTextElement(QStringLiteral("gpxx:Proximity"), toString(waypointp->proximity));
          }
          if (WAYPT_HAS(waypointp, temperature)) {
            writer->writeTextElement(QStringLiteral("gpxx:Temperature"), toString(waypointp->temperature));
          }
          if (WAYPT_HAS(waypointp, depth)) {
            writer->writeTextElement(QStringLiteral("gpxx:Depth"), toString(waypointp->depth));
          }
          writer->writeEndElement(); // "gpxx:WaypointExtension"
        }
        break;
      case gpxpt_route:
        if (gmsd != nullptr && gpxpt_route==point_type && gmsd->ilinks != nullptr) {
          writer->writeStartElement(QStringLiteral("gpxx:RoutePointExtension"));
          garmin_ilink_t* link = gmsd->ilinks;
          garmin_ilink_t* prior = nullptr;  // GDB files sometime contain repeated point; omit them
          while (link != nullptr) {
            if (prior == nullptr || prior->lat != link->lat || prior->lon != link->lon) {
              writer->writeStartElement(QStringLiteral("gpxx:rpt"));
              writer->writeAttribute(QStringLiteral("lat"), toString(link->lat));
              writer->writeAttribute(QStringLiteral("lon"), toString(link->lon));
              writer->writeEndElement(); // "gpxx:rpt"
            }
            prior = link;
            link = link->next;
          }
          writer->writeEndElement(); // "gpxx:RoutePointExtension"
        }
        break;
      case gpxpt_track:
        if (WAYPT_HAS(waypointp, temperature) || WAYPT_HAS(waypointp, depth) || waypointp->heartrate != 0 || waypointp->cadence != 0) {
          // gpxtpx:TrackPointExtension is a replacement for gpxx:TrackPointExtension.
          writer->writeStartElement(QStringLiteral("gpxtpx:TrackPointExtension"));
          if (WAYPT_HAS(waypointp, temperature)) {
            writer->writeTextElement(QStringLiteral("gpxtpx:atemp"), toString(waypointp->temperature));
          }
          if (WAYPT_HAS(waypointp, depth)) {
            writer->writeTextElement(QStringLiteral("gpxtpx:depth"), toString(waypointp->depth));
          }
          if (waypointp->heartrate != 0) {
            writer->writeTextElement(QStringLiteral("gpxtpx:hr"), QString::number(waypointp->heartrate));
          }
          if (waypointp->cadence != 0) {
            writer->writeTextElement(QStringLiteral("gpxtpx:cad"), QString::number(waypointp->cadence));
          }
          writer->writeEndElement(); // "gpxtpx:TrackPointExtension"
        }
        break;
      }
    }

    writer->writeEndElement(); // "extensions"
  }
}

void
GpxFormat::gpx_write_common_description(const Waypoint* waypointp, const QString& oname) const
{
  writer->writeOptionalTextElement(QStringLiteral("name"), oname);

  writer->writeOptionalTextElement(QStringLiteral("cmt"), waypointp->description);
  if (!waypointp->notes.isEmpty()) {
    writer->writeTextElement(QStringLiteral("desc"), waypointp->notes);
  } else {
    writer->writeOptionalTextElement(QStringLiteral("desc"), waypointp->description);
  }
  /* TODO: src should go here */
  write_gpx_url(waypointp);
  writer->writeOptionalTextElement(QStringLiteral("sym"), waypointp->icon_descr);
  /* TODO: type should go here */
}

void
GpxFormat::gpx_waypt_pr(const Waypoint* waypointp) const
{
  writer->writeStartElement(QStringLiteral("wpt"));
  writer->writeAttribute(QStringLiteral("lat"), toString(waypointp->latitude));
  writer->writeAttribute(QStringLiteral("lon"), toString(waypointp->longitude));

  QString oname = global_opts.synthesize_shortnames ?
                  mkshort_from_wpt(mkshort_handle, waypointp) :
                  waypointp->shortname;
  gpx_write_common_position(waypointp, gpxpt_waypoint);
  gpx_write_common_description(waypointp, oname);
  gpx_write_common_acc(waypointp);

  if (!(opt_humminbirdext || opt_garminext)) {
    const auto* fs_gpx = reinterpret_cast<fs_xml*>(waypointp->fs.FsChainFind(kFsGpx));
    auto* gmsd = garmin_fs_t::find(waypointp); /* gARmIN sPECIAL dATA */
    if (fs_gpx) {
      if (! gmsd) {
        fprint_xml_chain(fs_gpx->tag, waypointp);
      }
    }
    if (gmsd && (gpx_write_version > gpx_1_0)) {
      /* MapSource doesn't accepts extensions from 1.0 */
      garmin_fs_xml_fprint(waypointp, writer);
    }
  } else {
    gpx_write_common_extensions(waypointp, gpxpt_waypoint);
  }
  writer->writeEndElement();
}

void
GpxFormat::gpx_track_hdr(const route_head* rte)
{
  current_trk_head = rte;

  writer->writeStartElement(QStringLiteral("trk"));
  writer->writeOptionalTextElement(QStringLiteral("name"), rte->rte_name);
  writer->writeOptionalTextElement(QStringLiteral("desc"), rte->rte_desc);
  write_gpx_url(rte);

  if (rte->rte_num) {
    writer->writeTextElement(QStringLiteral("number"), QString::number(rte->rte_num));
  }

  if (gpx_write_version > gpx_1_0) {
    if (!(opt_humminbirdext || opt_garminext)) {
      const auto* fs_gpx = reinterpret_cast<fs_xml*>(rte->fs.FsChainFind(kFsGpx));
      if (fs_gpx) {
        fprint_xml_chain(fs_gpx->tag, nullptr);
      }
    } else if (opt_garminext) {
      if (rte->line_color.bbggrr > unknown_color) {
        int ci = gt_color_index_by_rgb(rte->line_color.bbggrr);
        if (ci > 0) {
          writer->writeStartElement(QStringLiteral("extensions"));
          writer->writeStartElement(QStringLiteral("gpxx:TrackExtension"));
          writer->writeTextElement(QStringLiteral("gpxx:DisplayColor"), QStringLiteral("%1")
                                   .arg(gt_color_name(ci)));
          writer->writeEndElement(); // Close gpxx:TrackExtension tag
          writer->writeEndElement(); // Close extensions tag
        }
      }
    }
  }
}

void
GpxFormat::gpx_track_disp(const Waypoint* waypointp) const
{
  bool first_in_trk = waypointp == current_trk_head->waypoint_list.front();

  if (waypointp->wpt_flags.new_trkseg) {
    if (!first_in_trk) {
      writer->writeEndElement();
    }
    writer->writeStartElement(QStringLiteral("trkseg"));
  }

  writer->writeStartElement(QStringLiteral("trkpt"));
  writer->writeAttribute(QStringLiteral("lat"), toString(waypointp->latitude));
  writer->writeAttribute(QStringLiteral("lon"), toString(waypointp->longitude));

  gpx_write_common_position(waypointp, gpxpt_track);

  QString oname = global_opts.synthesize_shortnames ?
                  mkshort_from_wpt(mkshort_handle, waypointp) :
                  waypointp->shortname;
  gpx_write_common_description(waypointp,
                               waypointp->wpt_flags.shortname_is_synthetic ?
                               nullptr : oname);
  gpx_write_common_acc(waypointp);

  if (!(opt_humminbirdext || opt_garminext)) {
    const auto* fs_gpx = reinterpret_cast<fs_xml*>(waypointp->fs.FsChainFind(kFsGpx));
    if (fs_gpx) {
      fprint_xml_chain(fs_gpx->tag, waypointp);
    }
  } else {
    gpx_write_common_extensions(waypointp, gpxpt_track);
  }
  writer->writeEndElement();
}

void
GpxFormat::gpx_track_tlr(const route_head* /*unused*/)
{
  if (!current_trk_head->waypoint_list.empty()) {
    writer->writeEndElement();
  }

  writer->writeEndElement();

  current_trk_head = nullptr;
}

void
GpxFormat::gpx_track_pr()
{
  auto gpx_track_hdr_lambda = [this](const route_head* rte)->void {
    gpx_track_hdr(rte);
  };
  auto gpx_track_tlr_lambda = [this](const route_head* rte)->void {
    gpx_track_tlr(rte);
  };
  auto gpx_track_disp_lambda = [this](const Waypoint* waypointp)->void {
    gpx_track_disp(waypointp);
  };
  track_disp_all(gpx_track_hdr_lambda, gpx_track_tlr_lambda, gpx_track_disp_lambda);
}

void
GpxFormat::gpx_route_hdr(const route_head* rte) const
{
  writer->writeStartElement(QStringLiteral("rte"));
  writer->writeOptionalTextElement(QStringLiteral("name"), rte->rte_name);
  writer->writeOptionalTextElement(QStringLiteral("desc"), rte->rte_desc);
  write_gpx_url(rte);

  if (rte->rte_num) {
    writer->writeTextElement(QStringLiteral("number"), QString::number(rte->rte_num));
  }

  if (gpx_write_version > gpx_1_0) {
    if (!(opt_humminbirdext || opt_garminext)) {
      const auto* fs_gpx = reinterpret_cast<fs_xml*>(rte->fs.FsChainFind(kFsGpx));
      if (fs_gpx) {
        fprint_xml_chain(fs_gpx->tag, nullptr);
      }
    } else if (opt_garminext) {
      if (rte->line_color.bbggrr > unknown_color) {
        int ci = gt_color_index_by_rgb(rte->line_color.bbggrr);
        if (ci > 0) {
          writer->writeStartElement(QStringLiteral("extensions"));
          writer->writeStartElement(QStringLiteral("gpxx:RouteExtension"));
          // FIXME: the value to use for IsAutoNamed is questionable.
          writer->writeTextElement(QStringLiteral("gpxx:IsAutoNamed"), rte->rte_name.isEmpty()? QStringLiteral("true") : QStringLiteral("false")); // Required element
          writer->writeTextElement(QStringLiteral("gpxx:DisplayColor"), QStringLiteral("%1")
                                   .arg(gt_color_name(ci)));
          writer->writeEndElement(); // Close gpxx:RouteExtension tag
          writer->writeEndElement(); // Close extensions tag
        }
      }
    }
  }
}

void
GpxFormat::gpx_route_disp(const Waypoint* waypointp) const
{
  writer->writeStartElement(QStringLiteral("rtept"));
  writer->writeAttribute(QStringLiteral("lat"), toString(waypointp->latitude));
  writer->writeAttribute(QStringLiteral("lon"), toString(waypointp->longitude));

  QString oname = global_opts.synthesize_shortnames ?
                  mkshort_from_wpt(mkshort_handle, waypointp) :
                  waypointp->shortname;
  gpx_write_common_position(waypointp, gpxpt_route);
  gpx_write_common_description(waypointp, oname);
  gpx_write_common_acc(waypointp);

  if (!(opt_humminbirdext || opt_garminext)) {
    const auto* fs_gpx = reinterpret_cast<fs_xml*>(waypointp->fs.FsChainFind(kFsGpx));
    if (fs_gpx) {
      fprint_xml_chain(fs_gpx->tag, waypointp);
    }
  } else {
    gpx_write_common_extensions(waypointp, gpxpt_route);
  }
  writer->writeEndElement();
}

void
GpxFormat::gpx_route_tlr(const route_head* /*unused*/) const
{
  writer->writeEndElement(); // Close rte tag.
}

void
GpxFormat::gpx_route_pr()
{
  /* output routes */
  auto gpx_route_hdr_lambda = [this](const route_head* rte)->void {
    gpx_route_hdr(rte);
  };
  auto gpx_route_tlr_lambda = [this](const route_head* rte)->void {
    gpx_route_tlr(rte);
  };
  auto gpx_route_disp_lambda = [this](const Waypoint* waypointp)->void {
    gpx_route_disp(waypointp);
  };
  route_disp_all(gpx_route_hdr_lambda, gpx_route_tlr_lambda, gpx_route_disp_lambda);
}

void
GpxFormat::gpx_waypt_bound_calc(const Waypoint* waypointp)
{
  waypt_add_to_bounds(&all_bounds, waypointp);
}

void
GpxFormat::gpx_write_bounds()
{
  waypt_init_bounds(&all_bounds);

  auto gpx_waypt_bound_calc_lambda = [this](const Waypoint* waypointp)->void {
    gpx_waypt_bound_calc(waypointp);
  };
  waypt_disp_all(gpx_waypt_bound_calc_lambda);
  route_disp_all(nullptr, nullptr, gpx_waypt_bound_calc_lambda);
  track_disp_all(nullptr, nullptr, gpx_waypt_bound_calc_lambda);

  if (waypt_bounds_valid(&all_bounds)) {
    writer->writeStartElement(QStringLiteral("bounds"));
    writer->writeAttribute(QStringLiteral("minlat"), toString(all_bounds.min_lat));
    writer->writeAttribute(QStringLiteral("minlon"), toString(all_bounds.min_lon));
    writer->writeAttribute(QStringLiteral("maxlat"), toString(all_bounds.max_lat));
    writer->writeAttribute(QStringLiteral("maxlon"), toString(all_bounds.max_lon));
    writer->writeEndElement();
  }
}

void
GpxFormat::write()
{

  elevation_precision = atoi(opt_elevation_precision);

  gpx_reset_short_handle();
  auto gpx_waypt_pr_lambda = [this](const Waypoint* waypointp)->void {
    gpx_waypt_pr(waypointp);
  };
  waypt_disp_all(gpx_waypt_pr_lambda);
  gpx_reset_short_handle();
  gpx_route_pr();
  gpx_reset_short_handle();
  gpx_track_pr();
  writer->writeEndElement(); // Close gpx tag.
}

void
GpxFormat::exit()
{
  gpx_highest_version_read = QVersionNumber();

  gpx_namespace_attribute.clear();

  delete gpx_global;
  gpx_global = nullptr;
}
