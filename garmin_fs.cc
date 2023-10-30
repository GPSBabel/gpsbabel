/*

    Implementation of special data used by Garmin products.

    Copyright (C) 2006, 2007, 2008 Olaf Klein, o.b.klein@gpsbabel.org

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

#include <QString>                     // for QString, operator==
#include <Qt>                          // for CaseInsensitive

#include "defs.h"
#include "garmin_fs.h"
#include "garmin_tables.h"             // for gt_switch_display_mode_value, gt_display_mode_symbol, gt_display_mode_symbol_and_comment, gt_display_mode_symbol_and_name
#include "inifile.h"                   // for inifile_readstr
#include "src/core/xmlstreamwriter.h"  // for XmlStreamWriter


#define MYNAME "garmin_fs"

garmin_fs_t*
garmin_fs_alloc(const int protocol)
{
  auto* result = new garmin_fs_t;

  result->protocol = protocol;

  return result;
}

garmin_fs_t* garmin_fs_t::clone() const
{
  auto* copy = new garmin_fs_t(*this);

  /* do not deep copy interlinks, only increment the reference counter */
  if (ilinks != nullptr) {
    ilinks->ref_count++;
  }

#ifdef GMSD_EXPERIMENTAL
  memcopy(subclass, other.subclass, sizeof(subclass));
#endif

  return copy;
}

garmin_fs_t::~garmin_fs_t()
{
  garmin_ilink_t* links;
  if ((links = ilinks) != nullptr) {
    links->ref_count--;
    if (links->ref_count <= 0) {
      while (links != nullptr) {
        garmin_ilink_t* tmp = links;
        links = links->next;
        xfree(tmp);
      }
    }
  }
}

bool
garmin_fs_convert_category(const QString& category_name, uint16_t* category)
{
  // Is the name  "Category" followed by a number? Use that number.
  if (category_name.startsWith(u"Category ", Qt::CaseInsensitive)) {
    bool ok;
    int i = category_name.mid(9).toInt(&ok);
    if (ok && (i >= 1) && (i <= 16)) {
      *category = (1 << --i);
      return true;
    }
  }
  if (global_opts.inifile != nullptr) {
    // Do we have a gpsbabel.ini that maps category names to category #'s?
    for (int i = 0; i < 16; i++) {
      QString key = QString::number(i + 1);
      QString c = inifile_readstr(global_opts.inifile, GMSD_SECTION_CATEGORIES, key);
      if (c.compare(category_name, Qt::CaseInsensitive) == 0) {
        *category = (1 << i);
        return true;
      }
    }
  }
  return false;
}

bool
garmin_fs_merge_category(const QString& category_name, Waypoint* waypt)
{
  uint16_t cat;

  // Attempt to get a textual category name to a category number.
  if (!garmin_fs_convert_category(category_name, &cat)) {
    return false;
  }

  garmin_fs_t* gmsd = garmin_fs_t::find(waypt);
  cat = cat | (garmin_fs_t::get_category(gmsd, 0));

  if (gmsd == nullptr) {
    gmsd = garmin_fs_alloc(-1);
    waypt->fs.FsChainAdd(gmsd);
  }
  garmin_fs_t::set_category(gmsd, cat);
  return true;
}
