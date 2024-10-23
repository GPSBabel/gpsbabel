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

#include "gpx.h"

#include <cassert>                          // for assert
#include <cmath>                            // for lround
#include <cstdio>                           // for sscanf
#include <cstdint>                          // for uint16_t
#include <cstring>                          // for strchr
#include <optional>                         // for optional
#include <utility>                          // for as_const

#include <QByteArray>                       // for QByteArray
#include <QDate>                            // for QDate
#include <QDateTime>                        // for QDateTime
#include <QHash>                            // for QHash
#include <QIODevice>                        // for QIODevice, operator|, QIODevice::ReadOnly, QIODevice::Text, QIODevice::WriteOnly
#include <QLatin1Char>                      // for QLatin1Char
#include <QLatin1String>                    // for QLatin1String
#include <QString>                          // for QString, QStringLiteral, operator+, operator==
#include <QStringList>                      // for QStringList
#include <QStringView>                      // for QStringView
#include <QTime>                            // for QTime
#include <QVersionNumber>                   // for QVersionNumber
#include <QXmlStreamAttribute>              // for QXmlStreamAttribute
#include <QXmlStreamAttributes>             // for QXmlStreamAttributes
#include <QXmlStreamNamespaceDeclaration>   // for QXmlStreamNamespaceDeclaration
#include <QXmlStreamNamespaceDeclarations>  // for QXmlStreamNamespaceDeclarations
#include <QXmlStreamReader>                 // for QXmlStreamReader, QXmlStreamReader::Characters, QXmlStreamReader::EndDocument, QXmlStreamReader::EndElement, QXmlStreamReader::Invalid, QXmlStreamReader::StartElement
#include <Qt>                               // for CaseInsensitive, UTC

#include "defs.h"
#include "garmin_fs.h"                      // for garmin_fs_t, garmin_ilink_t
#include "garmin_tables.h"                  // for gt_color_index_by_rgb, gt_color_name, gt_color_value_by_name
#include "geocache.h"                       // for Geocache, Geocache::UtfSt...
#include "mkshort.h"                        // for MakeShort
#include "src/core/datetime.h"              // for DateTime
#include "src/core/file.h"                  // for File
#include "src/core/logging.h"               // for Warning, Fatal
#include "src/core/xmlstreamwriter.h"       // for XmlStreamWriter
#include "src/core/xmltag.h"                // for xml_tag, fs_xml, fs_xml_alloc, free_gpx_extras


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
  delete mkshort_handle;

  mkshort_handle = new MakeShort;

  if (suppresswhite) {
    mkshort_handle->set_whitespace_ok(false);
  }

  mkshort_handle->set_length(snlen.toInt());
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
  Geocache* gc_data = wpt_tmp->AllocGCData();

  if (attr.hasAttribute(QLatin1String("id"))) {
    gc_data->id = attr.value(QLatin1String(QLatin1String("id"))).toLongLong();
  }
  if (attr.hasAttribute(QLatin1String("available"))) {
    if (attr.value(QLatin1String("available")).compare(QLatin1String("True"), Qt::CaseInsensitive) == 0) {
      gc_data->is_available = Geocache::status_t::gs_true;
    } else if (attr.value(QLatin1String("available")).compare(QLatin1String("False"), Qt::CaseInsensitive) == 0) {
      gc_data->is_available = Geocache::status_t::gs_false;
    }
  }
  if (attr.hasAttribute(QLatin1String("archived"))) {
    if (attr.value(QLatin1String("archived")).compare(QLatin1String("True"), Qt::CaseInsensitive) == 0) {
      gc_data->is_archived = Geocache::status_t::gs_true;
    } else if (attr.value(QLatin1String("archived")).compare(QLatin1String("False"), Qt::CaseInsensitive) == 0) {
      gc_data->is_archived = Geocache::status_t::gs_false;
    }
  }
}

void
GpxFormat::tag_garmin_fs(tag_type tag, const QString& text, Waypoint* waypt)
{
  garmin_fs_t* gmsd = garmin_fs_t::find(waypt);
  if (gmsd == nullptr) {
    gmsd = new garmin_fs_t(-1);
    waypt->fs.FsChainAdd(gmsd);
  }

  switch (tag) {
  case tag_type::garmin_wpt_proximity:
    waypt->set_proximity(text.toDouble());
    break;
  case tag_type::garmin_wpt_temperature:
    waypt->set_temperature(text.toFloat());
    break;
  case tag_type::garmin_wpt_depth:
    waypt->set_depth(text.toDouble());
    break;
  case tag_type::garmin_wpt_display_mode:
    // element DispalyMode value is case sensitive.
    if (text == u"SymbolOnly") {
      garmin_fs_t::set_display(gmsd, gt_display_mode_symbol);
    } else if (text == u"SymbolAndDescription") {
      garmin_fs_t::set_display(gmsd, gt_display_mode_symbol_and_comment);
    } else {
      garmin_fs_t::set_display(gmsd, gt_display_mode_symbol_and_name);
    }
    break;
  case tag_type::garmin_wpt_category:
    if (auto cat = garmin_fs_t::convert_category(text); cat.has_value()) {
      cat = *cat | (garmin_fs_t::get_category(gmsd, 0));
      garmin_fs_t::set_category(gmsd, *cat);
    } else {
      // There's nothing a user can really do about this (well, they could
      // create a gpsbabel.ini that mapped them to garmin category numbers
      // but that feature is so obscure and used in so few outputs that
      // there's no reason to alarm the user.  Just silently disregard
      // category names that don't map cleanly.
      // warning(MYNAME ": Unable to convert category \"%s\"!\n", CSTR(text));
    }
    break;
  case tag_type::garmin_wpt_addr:
    garmin_fs_t::set_addr(gmsd, text);
    break;
  case tag_type::garmin_wpt_city:
    garmin_fs_t::set_city(gmsd, text);
    break;
  case tag_type::garmin_wpt_state:
    garmin_fs_t::set_state(gmsd, text);
    break;
  case tag_type::garmin_wpt_country:
    garmin_fs_t::set_country(gmsd, text);
    break;
  case tag_type::garmin_wpt_postal_code:
    garmin_fs_t::set_postal_code(gmsd, text);
    break;
  case tag_type::garmin_wpt_phone_nr:
    garmin_fs_t::set_phone_nr(gmsd, text);
    break;
  default:
    // do nothing
    break;
  }
}

void
GpxFormat::start_something_else(QStringView el, const QXmlStreamAttributes& attr)
{
  if (!fs_ptr) {
    return;
  }

  auto* new_tag = new XmlTag;
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
      auto* new_fs_gpx = new fs_xml(kFsGpx);
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
  case tag_type::gpx:
    tag_gpx(attr);
    break;
  case tag_type::link:
    if (attr.hasAttribute(QLatin1String("href"))) {
      link_url = attr.value(QLatin1String("href")).toString();
    }
    break;
  case tag_type::wpt:
    tag_wpt(attr);
    break;
  case tag_type::wpttype_link:
    if (attr.hasAttribute(QLatin1String("href"))) {
      link_url = attr.value(QLatin1String("href")).toString();
    }
    break;
  case tag_type::rte:
    rte_head = new route_head;
    route_add_head(rte_head);
    rh_link_ = new UrlLink;
    fs_ptr = &rte_head->fs;
    break;
  case tag_type::rte_rtept:
    tag_wpt(attr);
    break;
  case tag_type::trk:
    trk_head = new route_head;
    track_add_head(trk_head);
    rh_link_ = new UrlLink;
    fs_ptr = &trk_head->fs;
    break;
  case tag_type::trk_trkseg_trkpt:
    tag_wpt(attr);
    if (next_trkpt_is_new_seg) {
      wpt_tmp->wpt_flags.new_trkseg = 1;
      next_trkpt_is_new_seg = 0;
    }
    break;
  case tag_type::rte_link:
  case tag_type::trk_link:
    if (attr.hasAttribute(QLatin1String("href"))) {
      link_url = attr.value(QLatin1String("href")).toString();
    }
    break;
  case tag_type::unknown:
    start_something_else(el, attr);
    return;
  case tag_type::cache:
    tag_gs_cache(attr);
    break;
  case tag_type::cache_log_wpt:
    if (opt_logpoint) {
      tag_log_wpt(attr);
    }
    break;
  case tag_type::cache_desc_long:
  case tag_type::cache_desc_short:
    tag_cache_desc(attr);
    break;
  case tag_type::cache_placer:
    if (attr.hasAttribute(QLatin1String("id"))) {
      wpt_tmp->AllocGCData()->placer_id = attr.value(QLatin1String("id")).toInt();
    }
    break;
  default:
    break;
  }
  if (tag.passthrough) {
    start_something_else(el, attr);
  }
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

  int year = 0;
  int mon = 1;
  int mday = 1;
  int hour = 0;
  int min = 0;
  int sec = 0;
  gpsbabel::DateTime dt;
  int res = sscanf(timestr, "%d-%d-%dT%d:%d:%d", &year, &mon, &mday, &hour,
                   &min, &sec);
  if (res > 0) {
    QDate date(year, mon, mday);
    QTime time(hour, min, sec);
    dt = QDateTime(date, time, QtUTC);

    // Fractional part of time.
    if (fsec) {
      dt = dt.addMSecs(lround(fsec * 1000));
    }

    // Any offsets that were stuck at the end.
    dt = dt.addSecs(-off_sign * off_hr * 3600 - off_sign * off_min * 60);
  }
  return dt;
}

void
GpxFormat::gpx_end(QStringView /*unused*/)
{
  static gpsbabel::DateTime gc_log_date;

  // Remove leading, trailing whitespace.
  cdatastr = cdatastr.trimmed();

  tag_mapping tag = get_tag(current_tag);

  switch (tag.type) {
  /*
   * First, the tags that are file-global.
   */
  case tag_type::name:
    gpx_add_to_global(gpx_global->name, cdatastr);
    break;
  case tag_type::desc:
    gpx_add_to_global(gpx_global->desc, cdatastr);
    break;
  case tag_type::author:
    gpx_add_to_global(gpx_global->author, cdatastr);
    break;
  case tag_type::email:
    gpx_add_to_global(gpx_global->email, cdatastr);
    break;
  case tag_type::url:
    gpx_add_to_global(gpx_global->url, cdatastr);
    break;
  case tag_type::urlname:
    gpx_add_to_global(gpx_global->urlname, cdatastr);
    break;
  case tag_type::keywords:
    gpx_add_to_global(gpx_global->keywords, cdatastr);
    break;
  case tag_type::link:
    (gpx_global->link).AddUrlLink(UrlLink(link_url, link_text, link_type));
    link_type.clear();
    link_text.clear();
    link_url.clear();
    break;
  case tag_type::link_text:
    link_text = cdatastr;
    break;
  case tag_type::link_type:
    link_type = cdatastr;
    break;

  /*
   * Waypoint-specific tags.
   */
  case tag_type::wpt:
    if (link_) {
      if (!link_->url_.isEmpty()) {
        wpt_tmp->AddUrlLink(*link_);
      }
      delete link_;
      link_ = nullptr;
    }
    if (wpt_fsdata != nullptr) {
      wpt_tmp->fs.FsChainAdd(wpt_fsdata);
      wpt_fsdata = nullptr;
    }
    waypt_add(wpt_tmp);
    logpoint_ct = 0;
    cur_tag = nullptr;
    wpt_tmp = nullptr;
    fs_ptr = nullptr;
    break;
  case tag_type::cache_name:
    wpt_tmp->notes = cdatastr;
    break;
  case tag_type::cache_container:
    wpt_tmp->AllocGCData()->set_container(cdatastr);
    break;
  case tag_type::cache_type:
    wpt_tmp->AllocGCData()->set_type(cdatastr);
    break;
  case tag_type::cache_difficulty:
    wpt_tmp->AllocGCData()->diff = cdatastr.toFloat() * 10;
    break;
  case tag_type::cache_hint:
    wpt_tmp->AllocGCData()->hint = cdatastr;
    break;
  case tag_type::cache_desc_long: {
    Geocache* gc_data = wpt_tmp->AllocGCData();
    gc_data->desc_long.is_html = cache_descr_is_html;
    gc_data->desc_long.utf_string = cdatastr;
  }
  break;
  case tag_type::cache_desc_short: {
    Geocache* gc_data = wpt_tmp->AllocGCData();
    gc_data->desc_short.is_html = cache_descr_is_html;
    gc_data->desc_short.utf_string = cdatastr;
  }
  break;
  case tag_type::cache_terrain:
    wpt_tmp->AllocGCData()->terr = cdatastr.toFloat() * 10;
    break;
  case tag_type::cache_placer:
    wpt_tmp->AllocGCData()->placer = cdatastr;
    break;
  case tag_type::cache_log_date:
    gc_log_date = xml_parse_time(cdatastr);
    break;
  /*
   * "Found it" logs follow the date according to the schema,
   * if this is the first "found it" for this waypt, just use the
   * last date we saw in this log.
   */
  case tag_type::cache_log_type:
    if ((cdatastr.compare(u"Found it") == 0) &&
        (!wpt_tmp->gc_data->last_found.isValid())) {
      wpt_tmp->AllocGCData()->last_found = gc_log_date;
    }
    gc_log_date = gpsbabel::DateTime();
    break;
  case tag_type::cache_favorite_points:
    wpt_tmp->AllocGCData()->favorite_points = cdatastr.toInt();
    break;
  case tag_type::cache_personal_note:
    wpt_tmp->AllocGCData()->personal_note = cdatastr;
    break;

  /*
   * Garmin-waypoint-specific tags.
   */
  case tag_type::garmin_wpt_proximity:
  case tag_type::garmin_wpt_temperature:
  case tag_type::garmin_wpt_depth:
  case tag_type::garmin_wpt_display_mode:
  case tag_type::garmin_wpt_category:
  case tag_type::garmin_wpt_addr:
  case tag_type::garmin_wpt_city:
  case tag_type::garmin_wpt_state:
  case tag_type::garmin_wpt_country:
  case tag_type::garmin_wpt_postal_code:
  case tag_type::garmin_wpt_phone_nr:
    tag_garmin_fs(tag.type, cdatastr, wpt_tmp);
    break;

  /*
   * Humminbird-waypoint-specific tags.
   */
  case tag_type::humminbird_wpt_depth:
  case tag_type::humminbird_trk_trkseg_trkpt_depth:
    wpt_tmp->set_depth(cdatastr.toDouble() / 100.0);
    break;
  /*
   * Route-specific tags.
   */
  case tag_type::rte_name:
    rte_head->rte_name = cdatastr;
    break;
  case tag_type::rte:
    if (rh_link_) {
      if (!rh_link_->url_.isEmpty()) {
        rte_head->rte_urls.AddUrlLink(*rh_link_);
      }
      delete rh_link_;
      rh_link_ = nullptr;
    }
    fs_ptr = nullptr;
    break;
  case tag_type::rte_rtept:
    if (link_) {
      if (!link_->url_.isEmpty()) {
        wpt_tmp->AddUrlLink(*link_);
      }
      delete link_;
      link_ = nullptr;
    }
    if (wpt_fsdata != nullptr) {
      wpt_tmp->fs.FsChainAdd(wpt_fsdata);
      wpt_fsdata = nullptr;
    }
    route_add_wpt(rte_head, wpt_tmp);
    wpt_tmp = nullptr;
    fs_ptr = nullptr;
    break;
  case tag_type::rte_desc:
    rte_head->rte_desc = cdatastr;
    break;
  case tag_type::garmin_rte_display_color:
    rte_head->line_color.bbggrr = gt_color_value_by_name(cdatastr);
    break;
  case tag_type::rte_link:
    rte_head->rte_urls.AddUrlLink(UrlLink(link_url, link_text, link_type));
    link_type.clear();
    link_text.clear();
    link_url.clear();
    break;
  case tag_type::rte_number:
    rte_head->rte_num = cdatastr.toInt();
    break;
  /*
   * Track-specific tags.
   */
  case tag_type::trk_name:
    trk_head->rte_name = cdatastr;
    break;
  case tag_type::trk:
    if (rh_link_) {
      if (!rh_link_->url_.isEmpty()) {
        trk_head->rte_urls.AddUrlLink(*rh_link_);
      }
      delete rh_link_;
      rh_link_ = nullptr;
    }
    fs_ptr = nullptr;
    break;
  case tag_type::trk_trkseg:
    next_trkpt_is_new_seg = 1;
    fs_ptr = nullptr;
    break;
  case tag_type::trk_trkseg_trkpt:
    if (link_) {
      if (!link_->url_.isEmpty()) {
        wpt_tmp->AddUrlLink(*link_);
      }
      delete link_;
      link_ = nullptr;
    }
    if (wpt_fsdata != nullptr) {
      wpt_tmp->fs.FsChainAdd(wpt_fsdata);
      wpt_fsdata = nullptr;
    }
    track_add_wpt(trk_head, wpt_tmp);
    wpt_tmp = nullptr;
    fs_ptr = nullptr;
    break;
  case tag_type::trk_desc:
    trk_head->rte_desc = cdatastr;
    break;
  case tag_type::garmin_trk_display_color:
    trk_head->line_color.bbggrr = gt_color_value_by_name(cdatastr);
    break;
  case tag_type::trk_link:
    trk_head->rte_urls.AddUrlLink(UrlLink(link_url, link_text, link_type));
    link_type.clear();
    link_text.clear();
    link_url.clear();
    break;
  case tag_type::trk_number:
    trk_head->rte_num = cdatastr.toInt();
    break;
  case tag_type::trk_trkseg_trkpt_course:
    wpt_tmp->set_course(cdatastr.toDouble());
    break;
  case tag_type::trk_trkseg_trkpt_speed:
    wpt_tmp->set_speed(cdatastr.toDouble());
    break;
  case tag_type::trk_trkseg_trkpt_heartrate:
    wpt_tmp->heartrate = cdatastr.toDouble();
    break;
  case tag_type::trk_trkseg_trkpt_cadence:
    wpt_tmp->cadence = cdatastr.toDouble();
    break;

  /*
   * Items that are actually in multiple categories.
   */
  case tag_type::rte_url:
  case tag_type::trk_url:
    rh_link_->url_ = cdatastr;
    break;
  case tag_type::rte_urlname:
  case tag_type::trk_urlname:
    rh_link_->url_link_text_ = cdatastr;
    break;
  case tag_type::rte_link_text:
  case tag_type::trk_link_text:
    link_text = cdatastr;
    break;
  case tag_type::rte_link_type:
  case tag_type::trk_link_type:
    link_type = cdatastr;
    break;
  case tag_type::wpttype_ele:
    wpt_tmp->altitude = cdatastr.toDouble();
    break;
  case tag_type::wpttype_name:
    wpt_tmp->shortname = cdatastr;
    break;
  case tag_type::wpttype_sym:
    wpt_tmp->icon_descr = cdatastr;
    break;
  case tag_type::wpttype_time:
    wpt_tmp->SetCreationTime(xml_parse_time(cdatastr));
    break;
  case tag_type::wpttype_magvar:
    if (wpt_fsdata == nullptr) {
      wpt_fsdata = new gpx_wpt_fsdata;
    }
    wpt_fsdata->magvar = cdatastr;
    break;
  case tag_type::wpttype_geoidheight:
    wpt_tmp->set_geoidheight(cdatastr.toDouble());
    break;
  case tag_type::wpttype_cmt:
    wpt_tmp->description = cdatastr;
    break;
  case tag_type::wpttype_desc:
    wpt_tmp->notes = cdatastr;
    break;
  case tag_type::wpttype_src:
    if (wpt_fsdata == nullptr) {
      wpt_fsdata = new gpx_wpt_fsdata;
    }
    wpt_fsdata->src = cdatastr;
    break;
  case tag_type::wpttype_type:
    if (wpt_fsdata == nullptr) {
      wpt_fsdata = new gpx_wpt_fsdata;
    }
    wpt_fsdata->type = cdatastr;
    break;
  case tag_type::wpttype_pdop:
    wpt_tmp->pdop = cdatastr.toFloat();
    break;
  case tag_type::wpttype_hdop:
    wpt_tmp->hdop = cdatastr.toFloat();
    break;
  case tag_type::wpttype_vdop:
    wpt_tmp->vdop = cdatastr.toFloat();
    break;
  case tag_type::wpttype_ageofdgpsdata:
    if (wpt_fsdata == nullptr) {
      wpt_fsdata = new gpx_wpt_fsdata;
    }
    wpt_fsdata->ageofdgpsdata = cdatastr;
    break;
  case tag_type::wpttype_dgpsid:
    if (wpt_fsdata == nullptr) {
      wpt_fsdata = new gpx_wpt_fsdata;
    }
    wpt_fsdata->dgpsid = cdatastr;
    break;
  case tag_type::wpttype_sat:
    wpt_tmp->sat = cdatastr.toInt();
    break;
  case tag_type::wpttype_fix:
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
  case tag_type::wpttype_url:
    link_->url_ = cdatastr;
    break;
  case tag_type::wpttype_urlname:
    link_->url_link_text_ = cdatastr;
    break;
  case tag_type::wpttype_link:
    waypt_add_url(wpt_tmp, link_url, link_text, link_type);
    link_type.clear();
    link_text.clear();
    link_url.clear();
    break;
  case tag_type::wpttype_link_text:
    link_text = cdatastr;
    break;
  case tag_type::wpttype_link_type:
    link_type = cdatastr;
    break;
  case tag_type::unknown:
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
    XmlTag* tmp_tag = cur_tag->child;
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

  if (opt_gpxver) {
    gpx_write_version = QVersionNumber::fromString(opt_gpxver.get()).normalized();
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
      for (const auto& l : std::as_const(gpx_global->link)) {
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

  delete mkshort_handle;
  mkshort_handle = nullptr;
}

QString
GpxFormat::qualifiedName() const
{
  /* The prefixes used in our hash table may not match those used in the input
   * file.  So we map from the namespaceUris to the prefixes used in our
   * hash table.
   */
  static const QHash<QString, QString> tag_ns_prefixes = {
    {"http://www.garmin.com/xmlschemas/GpxExtensions/v3", "gpxx"},
    {"http://www.garmin.com/xmlschemas/TrackPointExtension/v1", "gpxtpx"},
    {"http://www.groundspeak.com/cache/1/0", "groundspeak"},
    {"http://www.groundspeak.com/cache/1/0/1", "groundspeak"},
    {"http://humminbird.com", "h"}
  };

  if (auto uri = reader->namespaceUri().toString(); tag_ns_prefixes.contains(uri)) {
    return QStringLiteral("%1:%2").arg(tag_ns_prefixes.value(uri)).arg(reader->name());
  } else {
    return reader->qualifiedName().toString();
  }
}

void
GpxFormat::read()
{
  for (bool atEnd = false; !reader->atEnd() && !atEnd;) {
    reader->readNext();
    // do processing
    switch (reader->tokenType()) {
    case QXmlStreamReader::StartElement:
      current_tag.append(QLatin1Char('/'));
      current_tag.append(qualifiedName());
      gpx_start(reader->qualifiedName(), reader->attributes());
      break;

    case QXmlStreamReader::EndElement:
      gpx_end(reader->qualifiedName());
      current_tag.chop(qualifiedName().length() + 1);
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

  if (reader->hasError()) {
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
GpxFormat::fprint_xml_chain(const XmlTag* tag) const
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
        fprint_xml_chain(tag->child);
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
    const UrlLink& l = urls.GetUrlLink();
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
GpxFormat::gpx_write_common_acc(const Waypoint* waypointp, const gpx_wpt_fsdata* fs_gpxwpt) const
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
  if (fs_gpxwpt) {
    writer->writeOptionalTextElement(QStringLiteral("ageofdgpsdata"), fs_gpxwpt->ageofdgpsdata);
    writer->writeOptionalTextElement(QStringLiteral("dgpsid"), fs_gpxwpt->dgpsid);
  }
}


void
GpxFormat::gpx_write_common_position(const Waypoint* waypointp, const gpx_point_type point_type, const gpx_wpt_fsdata* fs_gpxwpt) const
{
  if (waypointp->altitude != unknown_alt) {
    writer->writeTextElement(QStringLiteral("ele"), QString::number(waypointp->altitude, 'f', elevation_precision));
  }
  QString t = waypointp->CreationTimeXML();
  writer->writeOptionalTextElement(QStringLiteral("time"), t);
  if (gpxpt_track==point_type && gpx_1_0 == gpx_write_version) {
    /* These were accidentally removed from 1.1, and were only a part of trkpts in 1.0 */
    if (waypointp->course_has_value()) {
      writer->writeTextElement(QStringLiteral("course"), toString(waypointp->course_value()));
    }
    if (waypointp->speed_has_value()) {
      writer->writeTextElement(QStringLiteral("speed"), toString(waypointp->speed_value()));
    }
  }
  if (fs_gpxwpt) {
    writer->writeOptionalTextElement(QStringLiteral("magvar"), fs_gpxwpt->magvar);
  }
  if (waypointp->geoidheight_has_value()) {
    writer->writeOptionalTextElement(QStringLiteral("geoidheight"),QString::number(waypointp->geoidheight_value(), 'f', 1));
  }
}

void
GpxFormat::gpx_write_common_extensions(const Waypoint* waypointp, const gpx_point_type point_type) const
{
  assert(gpx_write_version >= gpx_1_1);

  writer->stackOptionalStartElement(QStringLiteral("extensions"));

  if (opt_humminbirdext) {
    if (waypointp->depth_has_value()) {
      writer->stackTextElement(QStringLiteral("h:depth"), toString(waypointp->depth_value() * 100.0));
    }
    if (waypointp->temperature_has_value()) {
      writer->stackTextElement(QStringLiteral("h:temperature"), toString(waypointp->temperature_value()));
    }
  }

  if (opt_garminext) {
    // Although not required by the schema we assume that gpxx:WaypointExtension must be a child of gpx:wpt.
    // Although not required by the schema we assume that gpxx:RoutePointExtension must be a child of gpx:rtept.
    // Although not required by the schema we assume that gpxx:TrackPointExtension must be a child of gpx:trkpt.
    // Although not required by the schema we assume that gpxtpx:TrackPointExtension must be a child of gpx:trkpt.
    const garmin_fs_t* gmsd = garmin_fs_t::find(waypointp);
    switch (point_type) {
    case gpxpt_waypoint:
      writer->stackOptionalStartElement(QStringLiteral("gpxx:WaypointExtension"));
      if (waypointp->proximity_has_value()) {
        writer->stackTextElement(QStringLiteral("gpxx:Proximity"), toString(waypointp->proximity_value()));
      }
      if (waypointp->temperature_has_value()) {
        writer->stackTextElement(QStringLiteral("gpxx:Temperature"), toString(waypointp->temperature_value()));
      }
      if (waypointp->depth_has_value()) {
        writer->stackTextElement(QStringLiteral("gpxx:Depth"), toString(waypointp->depth_value()));
      }

      if (garmin_fs_t::has_display(gmsd)) {
        const char* cx;
        switch (gmsd->display) {
        case gt_display_mode_symbol:
          cx = "SymbolOnly";
          break;
        case gt_display_mode_symbol_and_comment:
          cx = "SymbolAndDescription";
          break;
        default:
          cx = "SymbolAndName";
          break;
        }
        writer->stackTextElement(QStringLiteral("gpxx:DisplayMode"), cx);
      }

      if (garmin_fs_t::has_category(gmsd)) {
        uint16_t cx = gmsd->category;
        writer->stackStartElement(QStringLiteral("gpxx:Categories"));
        const QStringList categoryList = garmin_fs_t::print_categories(cx);
        for (const auto& text : categoryList) {
            writer->stackTextElement(QStringLiteral("gpxx:Category"), text);
        }
        writer->stackEndElement(); // gpxx:Categories
      }

      writer->stackOptionalStartElement(QStringLiteral("gpxx:Address"));
      writer->stackOptionalTextElement(QStringLiteral("gpxx:StreetAddress"), garmin_fs_t::get_addr(gmsd, nullptr));
      writer->stackOptionalTextElement(QStringLiteral("gpxx:City"), garmin_fs_t::get_city(gmsd, nullptr));
      writer->stackOptionalTextElement(QStringLiteral("gpxx:State"), garmin_fs_t::get_state(gmsd, nullptr));
      writer->stackOptionalTextElement(QStringLiteral("gpxx:Country"), garmin_fs_t::get_country(gmsd, nullptr));
      writer->stackOptionalTextElement(QStringLiteral("gpxx:PostalCode"), garmin_fs_t::get_postal_code(gmsd, nullptr));
      writer->stackEndElement(); // gpxx:Address

      writer->stackOptionalTextElement(QStringLiteral("gpxx:PhoneNumber"), garmin_fs_t::get_phone_nr(gmsd, nullptr));

      writer->stackEndElement(); // gpxx:WaypointExtension
      break;
    case gpxpt_route:
      if (gmsd != nullptr && !gmsd->ilinks.isEmpty()) {
        writer->stackOptionalStartElement(QStringLiteral("gpxx:RoutePointExtension"));
        double prior_lat; // GDB files sometime contain repeated point; omit them
        double prior_lon;
        bool first = true;
        for (const auto& link : gmsd->ilinks) {
          if (first || (prior_lat != link.lat) || (prior_lon != link.lon)) {
            writer->stackStartElement(QStringLiteral("gpxx:rpt"));
            writer->stackAttribute(QStringLiteral("lat"), toString(link.lat));
            writer->stackAttribute(QStringLiteral("lon"), toString(link.lon));
            writer->stackEndElement(); // "gpxx:rpt"
            prior_lat = link.lat;
            prior_lon = link.lon;
            first = false;
          }
        }
        writer->stackEndElement(); // gpxx:RoutePointExtension
      }
      break;
    case gpxpt_track:
      // gpxtpx:TrackPointExtension is a replacement for gpxx:TrackPointExtension.
      writer->stackOptionalStartElement(QStringLiteral("gpxtpx:TrackPointExtension"));
      if (waypointp->temperature_has_value()) {
        writer->stackTextElement(QStringLiteral("gpxtpx:atemp"), toString(waypointp->temperature_value()));
      }
      if (waypointp->depth_has_value()) {
        writer->stackTextElement(QStringLiteral("gpxtpx:depth"), toString(waypointp->depth_value()));
      }
      if (waypointp->heartrate != 0) {
        writer->stackTextElement(QStringLiteral("gpxtpx:hr"), QString::number(waypointp->heartrate));
      }
      if (waypointp->cadence != 0) {
        writer->stackTextElement(QStringLiteral("gpxtpx:cad"), QString::number(waypointp->cadence));
      }
      writer->stackEndElement(); // gpxtpx:TrackPointExtension
      break;
    }
  }

  writer->stackEndElement(); // "extensions"
}

void
GpxFormat::gpx_write_common_description(const Waypoint* waypointp, const gpx_point_type point_type, const gpx_wpt_fsdata* fs_gpxwpt) const
{
  QString oname;
  if (!((point_type == gpxpt_track) && waypointp->wpt_flags.shortname_is_synthetic)) {
    oname = global_opts.synthesize_shortnames ?
            mkshort_handle->mkshort_from_wpt(waypointp) : waypointp->shortname;
  }
  writer->writeOptionalTextElement(QStringLiteral("name"), oname);

  writer->writeOptionalTextElement(QStringLiteral("cmt"), waypointp->description);
  if (!waypointp->notes.isEmpty()) {
    writer->writeTextElement(QStringLiteral("desc"), waypointp->notes);
  } else {
    writer->writeOptionalTextElement(QStringLiteral("desc"), waypointp->description);
  }
  if (fs_gpxwpt) {
    writer->writeOptionalTextElement(QStringLiteral("src"), fs_gpxwpt->src);
  }
  write_gpx_url(waypointp);
  writer->writeOptionalTextElement(QStringLiteral("sym"), waypointp->icon_descr);
  if (fs_gpxwpt) {
    writer->writeOptionalTextElement(QStringLiteral("type"), fs_gpxwpt->type);
  }
}

void GpxFormat::gpx_write_common_core(const Waypoint* waypointp,
                                      const gpx_point_type point_type) const
{
  const auto* fs_gpxwpt = reinterpret_cast<gpx_wpt_fsdata*>(waypointp->fs.FsChainFind(kFsGpxWpt));

  gpx_write_common_position(waypointp, point_type, fs_gpxwpt);
  gpx_write_common_description(waypointp, point_type, fs_gpxwpt);
  gpx_write_common_acc(waypointp, fs_gpxwpt);
}

void
GpxFormat::gpx_waypt_pr(const Waypoint* waypointp) const
{
  writer->writeStartElement(QStringLiteral("wpt"));
  writer->writeAttribute(QStringLiteral("lat"), toString(waypointp->latitude));
  writer->writeAttribute(QStringLiteral("lon"), toString(waypointp->longitude));

  gpx_write_common_core(waypointp, gpxpt_waypoint);

  if (!(opt_humminbirdext || opt_garminext)) {
    const auto* fs_gpx = reinterpret_cast<fs_xml*>(waypointp->fs.FsChainFind(kFsGpx));
    if (fs_gpx && fs_gpx->tag) {
      fprint_xml_chain(fs_gpx->tag);
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

  if (!(opt_humminbirdext || opt_garminext)) {
    const auto* fs_gpx = reinterpret_cast<fs_xml*>(rte->fs.FsChainFind(kFsGpx));
    if (fs_gpx) {
      fprint_xml_chain(fs_gpx->tag);
    }
  } else if (opt_garminext) {
    if (rte->line_color.bbggrr > unknown_color) {
      int ci = gt_color_index_by_rgb(rte->line_color.bbggrr);
      if (ci > 0) {
        writer->writeStartElement(QStringLiteral("extensions"));
        writer->writeStartElement(QStringLiteral("gpxx:TrackExtension"));
        writer->writeTextElement(QStringLiteral("gpxx:DisplayColor"), gt_color_name(ci));
        writer->writeEndElement(); // Close gpxx:TrackExtension tag
        writer->writeEndElement(); // Close extensions tag
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

  gpx_write_common_core(waypointp, gpxpt_track);

  if (!(opt_humminbirdext || opt_garminext)) {
    const auto* fs_gpx = reinterpret_cast<fs_xml*>(waypointp->fs.FsChainFind(kFsGpx));
    if (fs_gpx) {
      fprint_xml_chain(fs_gpx->tag);
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

  if (!(opt_humminbirdext || opt_garminext)) {
    const auto* fs_gpx = reinterpret_cast<fs_xml*>(rte->fs.FsChainFind(kFsGpx));
    if (fs_gpx) {
      fprint_xml_chain(fs_gpx->tag);
    }
  } else if (opt_garminext) {
    if (rte->line_color.bbggrr > unknown_color) {
      int ci = gt_color_index_by_rgb(rte->line_color.bbggrr);
      if (ci > 0) {
        writer->writeStartElement(QStringLiteral("extensions"));
        writer->writeStartElement(QStringLiteral("gpxx:RouteExtension"));
        // FIXME: the value to use for IsAutoNamed is questionable.
        writer->writeTextElement(QStringLiteral("gpxx:IsAutoNamed"), rte->rte_name.isEmpty()? QStringLiteral("true") : QStringLiteral("false")); // Required element
        writer->writeTextElement(QStringLiteral("gpxx:DisplayColor"), gt_color_name(ci));
        writer->writeEndElement(); // Close gpxx:RouteExtension tag
        writer->writeEndElement(); // Close extensions tag
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

  gpx_write_common_core(waypointp, gpxpt_route);

  if (!(opt_humminbirdext || opt_garminext)) {
    const auto* fs_gpx = reinterpret_cast<fs_xml*>(waypointp->fs.FsChainFind(kFsGpx));
    if (fs_gpx) {
      fprint_xml_chain(fs_gpx->tag);
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

  elevation_precision = opt_elevation_precision.toInt();

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
