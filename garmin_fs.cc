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

#include "defs.h"
#include "cet_util.h"
#include "garmin_fs.h"
#include "garmin_tables.h"
#include "inifile.h"

#include <QtCore/QString>
#include <QtCore/QXmlStreamWriter>
#include <cassert>
#include <cstdio>
#include <cstdlib>

#define MYNAME "garmin_fs"

garmin_fs_t*
garmin_fs_alloc(const int protocol)
{
  auto* result = new garmin_fs_t;
  result->fs.type = FS_GMSD;
  result->fs.copy = (fs_copy) garmin_fs_copy;
  result->fs.destroy = garmin_fs_destroy;
  result->fs.next = nullptr;

  result->protocol = protocol;

  return result;
}

void
garmin_fs_destroy(void* fs)
{
  delete (garmin_fs_t*) fs;
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

void garmin_fs_copy(garmin_fs_t** dest, garmin_fs_t* src)
{
  if (src == nullptr) {
    *dest = nullptr;
    return;
  }
  *dest = new garmin_fs_t(*src);
}

garmin_fs_t::garmin_fs_t(const garmin_fs_t& other) :
  fs(other.fs),
  flags(other.flags),
  protocol(other.protocol),
  icon(other.icon),
  wpt_class(other.wpt_class),
  display(other.display),
  category(other.category),
  city(other.city),
  facility(other.facility),
  state(other.state),
  cc(other.cc),
  cross_road(other.cross_road),
  addr(other.addr),
  country(other.country),
  phone_nr(other.phone_nr),
  phone_nr2(other.phone_nr2),
  fax_nr(other.fax_nr),
  postal_code(other.postal_code),
  email(other.email),
  ilinks(other.ilinks)
{
  /* do not deep copy interlinks, only increment the reference counter */
  if (ilinks != nullptr) {
    ilinks->ref_count++;
  }

#ifdef GMSD_EXPERIMENTAL
  memcopy(subclass, other.subclass, sizeof(subclass));
#endif
}

/* GPX - out */

void
garmin_fs_xml_fprint(const Waypoint* waypt,
                     QXmlStreamWriter* writer)
{
  garmin_fs_t* gmsd = garmin_fs_t::find(waypt);

  if (gmsd == nullptr) {
    return;
  }

  /* Find out if there is at least one field set */
  QString addr = garmin_fs_t::get_addr(gmsd, "");
  if (addr.isEmpty()) {
    addr = garmin_fs_t::get_city(gmsd, "");
  }
  if (addr.isEmpty()) {
    addr = garmin_fs_t::get_country(gmsd, "");
  }
  if (addr.isEmpty()) {
    addr = garmin_fs_t::get_postal_code(gmsd, "");
  }
  if (addr.isEmpty()) {
    addr = garmin_fs_t::get_state(gmsd, "");
  }

  QString phone = garmin_fs_t::get_phone_nr(gmsd, "");

  if (!addr.isEmpty() || !phone.isEmpty() ||
      (gmsd->flags.category && gmsd->category) ||
      WAYPT_HAS(waypt, depth) ||
      WAYPT_HAS(waypt, proximity) ||
      WAYPT_HAS(waypt, temperature) ||
      gmsd->flags.display) {
    writer->writeStartElement(QStringLiteral("extensions"));
    writer->writeStartElement(QStringLiteral("gpxx:WaypointExtension"));
    writer->writeNamespace(QStringLiteral("http://www.garmin.com/xmlschemas/GpxExtensions/v3"),
                           "gpxx");
    if WAYPT_HAS(waypt, proximity) {
      writer->writeTextElement(QStringLiteral("gpxx:Proximity"), QString::number(waypt->proximity, 'f', 6));
    }
    if WAYPT_HAS(waypt, temperature) {
      writer->writeTextElement(QStringLiteral("gpxx:Temperature"),  QString::number(waypt->temperature, 'f', 6));
    }
    if WAYPT_HAS(waypt, depth) {
      writer->writeTextElement(QStringLiteral("gpxx:Depth"), QString::number(waypt->depth, 'f', 6));
    }
    if (gmsd->flags.display) {
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
      writer->writeTextElement(QStringLiteral("gpxx:DisplayMode"), cx);
    }
    if (gmsd->flags.category && gmsd->category) {
      uint16_t cx = gmsd->category;
      writer->writeStartElement(QStringLiteral("gpxx:Categories"));
      for (int i = 0; i < 16; i++) {
        if (cx & 1) {
          writer->writeTextElement(QStringLiteral("gpxx:Category"), QStringLiteral("Category %1").arg(i+1));
        }
        cx = cx >> 1;
      }
      writer->writeEndElement(); // gpxx:Categories
    }
    if (!addr.isEmpty()) {
      QString str;
      writer->writeStartElement(QStringLiteral("gpxx:Address"));

      if (!(str = garmin_fs_t::get_addr(gmsd, nullptr)).isEmpty()) {
        writer->writeTextElement(QStringLiteral("gpxx:StreetAddress"), str);
      }
      if (!(str = garmin_fs_t::get_city(gmsd, nullptr)).isEmpty()) {
        writer->writeTextElement(QStringLiteral("gpxx:City"), str);
      }
      if (!(str = garmin_fs_t::get_state(gmsd, nullptr)).isEmpty()) {
        writer->writeTextElement(QStringLiteral("gpxx:State"), str);
      }
      if (!(str = garmin_fs_t::get_country(gmsd, nullptr)).isEmpty()) {
        writer->writeTextElement(QStringLiteral("gpxx:Country"), str);
      }
      if (!(str = garmin_fs_t::get_postal_code(gmsd, nullptr)).isEmpty()) {
        writer->writeTextElement(QStringLiteral("gpxx:PostalCode"), str);
      }
      writer->writeEndElement(); // /gpxx::Address
    }

    if (!phone.isEmpty()) {
      writer->writeTextElement(QStringLiteral("gpxx:PhoneNumber"), phone);
    }

    writer->writeEndElement(); // /gpxx::WaypointExtension
    writer->writeEndElement(); // /extensions.
  }

}

void
garmin_fs_xml_convert(const int base_tag, int tag, const QString& qstr, Waypoint* waypt)
{
  // FIXME: eliminate C string copy/use here:
  const char *cdatastr = xstrdup(qstr);
  garmin_fs_t* gmsd = garmin_fs_t::find(waypt);
  if (gmsd == nullptr) {
    gmsd = garmin_fs_alloc(-1);
    fs_chain_add(&waypt->fs, (format_specific_data*) gmsd);
  }

  tag -= base_tag;
  /*
  	tt_garmin_waypt_extension, -> 0
  	tt_garmin_proximity, -> 1
  	tt_garmin_temperature,-> 2
  	tt_garmin_depth, -> 3
  	tt_garmin_display_mode, -> 4
  	tt_garmin_categories, -> 5
  	tt_garmin_category, -> 6
  	tt_garmin_addr, -> 7
  	tt_garmin_city, -> 8
  	tt_garmin_state, -> 9
  	tt_garmin_country, -> 10
  	tt_garmin_postal_code, -> 11
  	tt_garmin_phone_nr, -> 12
  */
  switch (tag) {
  case 1:
    if (*cdatastr) {
      WAYPT_SET(waypt, proximity, atof(cdatastr));
    }
    break;
  case 2:
    if (*cdatastr) {
      WAYPT_SET(waypt, temperature, atof(cdatastr));
    }
    break;
  case 3:
    if (*cdatastr) {
      WAYPT_SET(waypt, depth, atof(cdatastr));
    }
    break;
  case 4:
    if (case_ignore_strcmp(cdatastr, "SymbolOnly") == 0) {
      garmin_fs_t::set_display(gmsd, gt_display_mode_symbol);
    } else if (case_ignore_strcmp(cdatastr, "SymbolAndDescription") == 0) {
      garmin_fs_t::set_display(gmsd, gt_display_mode_symbol_and_comment);
    } else {
      garmin_fs_t::set_display(gmsd, gt_display_mode_symbol_and_name);
    }
    break;
  case 6:
    if (! garmin_fs_merge_category(cdatastr, waypt)) {
      // There's nothing a user can really do about this (well, they could
      // create a gpsbabel.ini that mapped them to garmin category numbers
      // but that feature is so obscure and used in so few outputs that
      // there's no reason to alarm the user.  Just silently disregard
      // category names that don't map cleanly.
      // warning(MYNAME ": Unable to convert category \"%s\"!\n", cdatastr);
    }
    break;
  case 7:
    garmin_fs_t::set_addr(gmsd, cdatastr);
    break;
  case 8:
    garmin_fs_t::set_city(gmsd, cdatastr);
    break;
  case 9:
    garmin_fs_t::set_state(gmsd, cdatastr);
    break;
  case 10:
    garmin_fs_t::set_country(gmsd, cdatastr);
    break;
  case 11:
    garmin_fs_t::set_postal_code(gmsd, cdatastr);
    break;
  case 12:
    garmin_fs_t::set_phone_nr(gmsd, cdatastr);
    break;
  }
  xfree(cdatastr);
}

unsigned char
garmin_fs_convert_category(const char* category_name, uint16_t* category)
{
  int i;
  int cat = 0;

  // Is the name  "Category" followed by a number? Use that number.
  if ((case_ignore_strncmp(category_name, "Category ", 9) == 0) &&
      (1 == sscanf(category_name + 9, "%d", &i)) &&
      (i >= 1) && (i <= 16)) {
    cat = (1 << --i);
  } else if (global_opts.inifile != nullptr) {
    // Do we have a gpsbabel.ini that maps category names to category #'s?
    for (i = 0; i < 16; i++) {
      char key[3];

      // use assertion to silence gcc 7.3 warning
      // warning: ‘%d’ directive output may be truncated writing between 1 and 11 bytes into a region of size 3 [-Wformat-truncation=]
      assert((i>=0) && (i<16));
      snprintf(key, sizeof(key), "%d", i + 1);
      QString c = inifile_readstr(global_opts.inifile, GMSD_SECTION_CATEGORIES, key);
      if (c.compare(category_name, Qt::CaseInsensitive) == 0) {
        cat = (1 << i);
        break;
      }
    }
  }
  if (cat == 0) {
    return 0;
  } else {
    *category = cat;
    return 1;
  }
}

unsigned char
garmin_fs_merge_category(const char* category_name, Waypoint* waypt)
{
  uint16_t cat;

  // Attempt to get a textual category name to a category number.
  if (!garmin_fs_convert_category(category_name, &cat)) {
    return 0;
  }

  garmin_fs_t* gmsd = garmin_fs_t::find(waypt);
  cat = cat | (garmin_fs_t::get_category(gmsd, 0));

  if (gmsd == nullptr) {
    gmsd = garmin_fs_alloc(-1);
    fs_chain_add(&waypt->fs, (format_specific_data*) gmsd);
  }
  garmin_fs_t::set_category(gmsd, cat);
  return 1;
}

void
garmin_fs_garmin_after_read(const GPS_PWay way, Waypoint* wpt, const int protoid)
{
  garmin_fs_t* gmsd = garmin_fs_alloc(protoid);
  fs_chain_add(&wpt->fs, (format_specific_data*) gmsd);

  /* nothing happens until gmsd is allocated some lines above */

  /* !!! class needs protocol specific conversion !!! (ToDo)
  garmin_fs_t::set_wpt_class(gmsd, way[i]->wpt_class);
  */
  /* flagged data fields */
  garmin_fs_t::set_display(gmsd, gt_switch_display_mode_value(way->dspl, gps_waypt_type, 1));
  if (way->category != 0) {
    garmin_fs_t::set_category(gmsd, way->category);
  }
  if (way->dst < 1.0e25f) {
    WAYPT_SET(wpt, proximity, way->dst);
  }
  if (way->temperature_populated) {
    WAYPT_SET(wpt, temperature, way->temperature);
  }
  if (way->dpth < 1.0e25f) {
    WAYPT_SET(wpt, depth, way->dpth);
  }
  // These use STRTOUNICODE which uses gobal_opts.codec.
  garmin_fs_t::set_cc(gmsd, STRTOUNICODE(QByteArray(way->cc, sizeof(way->cc)).constData()));
  garmin_fs_t::set_city(gmsd, STRTOUNICODE(QByteArray(way->city, sizeof(way->city)).constData()));
  garmin_fs_t::set_state(gmsd, STRTOUNICODE(QByteArray(way->state, sizeof(way->state)).constData()));
  garmin_fs_t::set_facility(gmsd, STRTOUNICODE(QByteArray(way->facility, sizeof(way->facility)).constData()));
  garmin_fs_t::set_cross_road(gmsd, STRTOUNICODE(QByteArray(way->cross_road, sizeof(way->cross_road)).constData()));
  garmin_fs_t::set_addr(gmsd, STRTOUNICODE(QByteArray(way->addr, sizeof(way->addr)).constData()));
}

void
garmin_fs_garmin_before_write(const Waypoint* wpt, GPS_PWay way, const int protoid)
{
  garmin_fs_t* gmsd = garmin_fs_t::find(wpt);

  (void)protoid; // unused for now.

  if (gmsd == nullptr) {
    return;
  }

  /* ToDo: protocol specific conversion of class
  way[i]->wpt_class = garmin_fs_t::get_wpt_class(gmsd, way[i]->wpt_class);
  	*/
  way->dspl = gt_switch_display_mode_value(
                garmin_fs_t::get_display(gmsd, way->dspl), gps_waypt_type, 0);
  way->category = garmin_fs_t::get_category(gmsd, way->category);
  way->dpth = WAYPT_GET(wpt, depth, way->dpth);
  way->dst = WAYPT_GET(wpt, proximity, way->dpth);
  way->temperature = WAYPT_GET(wpt, temperature, way->temperature);

  // These use STRFROMUNICODE which uses gobal_opts.codec.
  strncpy(way->cc, STRFROMUNICODE(garmin_fs_t::get_cc(gmsd, nullptr)), sizeof(way->cc));
  strncpy(way->city, STRFROMUNICODE(garmin_fs_t::get_city(gmsd, nullptr)), sizeof(way->city));
  strncpy(way->state, STRFROMUNICODE(garmin_fs_t::get_state(gmsd, nullptr)), sizeof(way->state));
  strncpy(way->facility, STRFROMUNICODE(garmin_fs_t::get_facility(gmsd, nullptr)), sizeof(way->facility));
  strncpy(way->cross_road, STRFROMUNICODE(garmin_fs_t::get_cross_road(gmsd, nullptr)), sizeof(way->cross_road));
  strncpy(way->addr, STRFROMUNICODE(garmin_fs_t::get_addr(gmsd, nullptr)), sizeof(way->addr));
}
