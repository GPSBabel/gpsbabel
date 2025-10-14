/*
    Copyright (C) 2002-2023 Robert Lipe, robertlipe+source@gpsbabel.org

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

#include "geocache.h"

#include <QString>               // for QString
#include <QVector>               // for QVector
#include <Qt>                    // for CaseInsensitive

#include "defs.h"                // for strip_html, global_options, global_opts


const QVector<Geocache::type_mapping> Geocache::type_map = {
  {type_t::gt_traditional, "Traditional Cache" },
  {type_t::gt_traditional, "Traditional" }, /* opencaching.de */
  {type_t::gt_multi, "Multi-cache" },
  {type_t::gt_multi, "Multi" }, /* opencaching.de */
  {type_t::gt_virtual, "Virtual Cache" },
  {type_t::gt_virtual, "Virtual" }, /* opencaching.de */
  {type_t::gt_event, "Event Cache" },
  {type_t::gt_event, "Event" }, /* opencaching.de */
  {type_t::gt_webcam, "Webcam Cache" },
  {type_t::gt_webcam, "Webcam" }, /* opencaching.de */
  {type_t::gt_surprise, "Unknown Cache" },
  {type_t::gt_earth, "Earthcache" },
  {type_t::gt_earth, "Earth" }, /* opencaching.de */
  {type_t::gt_cito, "Cache In Trash Out Event" },
  {type_t::gt_letterbox, "Letterbox Hybrid" },
  {type_t::gt_locationless, "Locationless (Reverse) Cache" },
  {type_t::gt_ape, "Project APE Cache" },
  {type_t::gt_mega, "Mega-Event Cache" },
  {type_t::gt_wherigo, "Wherigo Cache" },

  {type_t::gt_benchmark, "Benchmark" } /* Not Groundspeak; for GSAK  */
};

const QVector<Geocache::container_mapping> Geocache::container_map = {
  {container_t::gc_other, "Unknown" },
  {container_t::gc_other, "Other" }, /* Synonym on read. */
  {container_t::gc_micro, "Micro" },
  {container_t::gc_regular, "Regular" },
  {container_t::gc_large, "Large" },
  {container_t::gc_small, "Small" },
  {container_t::gc_virtual, "Virtual" }
};

QString Geocache::UtfString::strip_html() const
{
  return is_html? ::strip_html(utf_string) : utf_string;
}

void Geocache::set_type(const QString& type_name)
{
  for (const auto& map_entry : type_map) {
    if (!type_name.compare(map_entry.name,Qt::CaseInsensitive)) {
      type = map_entry.type;
      return;
    }
  }
  type = type_t::gt_unknown;
}

QString Geocache::get_type() const
{
  for (const auto& map_entry : type_map) {
    if (type == map_entry.type) {
      return map_entry.name;
    }
  }
  return "Unknown";
}

void Geocache::set_container(const QString& container_name)
{
  for (const auto& map_entry : container_map) {
    if (!container_name.compare(map_entry.name,Qt::CaseInsensitive)) {
      container = map_entry.container;
      return;
    }
  }
  container = container_t::gc_unknown;
}

QString Geocache::get_container() const
{
  for (const auto& map_entry : container_map) {
    if (container == map_entry.container) {
      return map_entry.name;
    }
  }
  return "Unknown";
}

/*
 * Return a QString that is suitable for icon lookup based on geocache
 * attributes.  The strings used are those present in a GPX file from
 * geocaching.com.  Thus we sort of make all the other formats do lookups
 * based on these strings.
 */
QString Geocache::get_icon() const
{
  if (!global_opts.smart_icons) {
    return nullptr;
  }

  /*
   * For icons, type overwrites container.  So a multi-micro will
   * get the icons for "multi".
   */
  switch (type) {
  case type_t::gt_virtual:
    return "Virtual cache";
  case type_t::gt_multi:
    return "Multi-Cache";
  case type_t::gt_event:
    return "Event Cache";
  case type_t::gt_surprise:
    return "Unknown Cache";
  case type_t::gt_webcam:
    return "Webcam Cache";
  default:
    break;
  }

  switch (container) {
  case container_t::gc_micro:
    return "Micro-Cache";
  default:
    break;
  }

  if (diff > 1) {
    return "Geocache";
  }

  return nullptr;
}
