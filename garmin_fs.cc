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

#include "garmin_fs.h"

#include <QString>    // for QString
#include <Qt>         // for CaseInsensitive

#include "defs.h"
#include "inifile.h"  // for inifile_readstr


std::optional<uint16_t>
garmin_fs_t::convert_category(const QString& category_name)
{
  std::optional<uint16_t> category;

  // Is the name  "Category" followed by a number? Use that number.
  if (category_name.startsWith(u"Category ", Qt::CaseInsensitive)) {
    bool ok;
    int i = category_name.mid(9).toInt(&ok);
    if (ok && (i >= 1) && (i <= 16)) {
      category = (1 << --i);
      return category;
    }
  }
  if (global_opts.inifile != nullptr) {
    // Do we have a gpsbabel.ini that maps category names to category #'s?
    for (int i = 0; i < 16; i++) {
      QString key = QString::number(i + 1);
      QString c = inifile_readstr(global_opts.inifile, kGmsdSectionCategories, key);
      if (c.compare(category_name, Qt::CaseInsensitive) == 0) {
        category = (1 << i);
        return category;
      }
    }
  }
  return category;
}

QStringList
garmin_fs_t::print_categories(uint16_t categories)
{
  QStringList categoryList;

  if (categories == 0) {
    return categoryList;
  }

  for (int i = 0; i < 16; i++) {
    if ((categories & 1) != 0) {
      QString c;
      if (global_opts.inifile != nullptr) {
        QString key = QString::number(i + 1);
        c = inifile_readstr(global_opts.inifile, kGmsdSectionCategories, key);
      }

      if (c.isNull()) {
        categoryList << QString::asprintf("Category %d", i+1);
      }
//				*fout << QString::asprintf("%s", gps_categories[i]);
      else {
        categoryList << c;
      }

    }
    categories = categories >> 1;
  }
  return categoryList;
}
