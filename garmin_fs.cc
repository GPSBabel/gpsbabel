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
    Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111 USA

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
  garmin_fs_t* result = (garmin_fs_t*)xcalloc(1, sizeof(*result));
  result->fs.type = FS_GMSD;
  result->fs.copy = (fs_copy) garmin_fs_copy;
  result->fs.destroy = garmin_fs_destroy;
  result->fs.convert = garmin_fs_convert;
  result->fs.next = nullptr;

  result->protocol = protocol;

  return result;
}

void
garmin_fs_destroy(void* fs)
{
  garmin_fs_t* data = (garmin_fs_t*) fs;
  if (data != nullptr) {
    garmin_ilink_t* ilinks;

    if (data->addr != nullptr) {
      xfree(data->addr);
    }
    if (data->cc != nullptr) {
      xfree(data->cc);
    }
    if (data->city != nullptr) {
      xfree(data->city);
    }
    if (data->country != nullptr) {
      xfree(data->country);
    }
    if (data->cross_road != nullptr) {
      xfree(data->cross_road);
    }
    if (data->facility != nullptr) {
      xfree(data->facility);
    }
    if (data->phone_nr != nullptr) {
      xfree(data->phone_nr);
    }
    if (data->phone_nr2 != nullptr) {
      xfree(data->phone_nr2);
    }
    if (data->fax_nr != nullptr) {
      xfree(data->fax_nr);
    }
    if (data->email != nullptr) {
      xfree(data->email);
    }
    if (data->postal_code != nullptr) {
      xfree(data->postal_code);
    }
    if (data->state != nullptr) {
      xfree(data->state);
    }

    if ((ilinks = data->ilinks) != nullptr) {
      ilinks->ref_count--;
      if (ilinks->ref_count <= 0) {
        while (ilinks != nullptr) {
          garmin_ilink_t* tmp = ilinks;
          ilinks = ilinks->next;
          xfree(tmp);
        }
      }
    }
    xfree(data);
  }
}

void garmin_fs_copy(garmin_fs_t** dest, garmin_fs_t* src)
{
  if (src == nullptr) {
    *dest = nullptr;
    return;
  }
  *dest = (garmin_fs_t*) xmalloc(sizeof(*src));

  /* do not copy interlinks, only increment the refrence counter */
  if (src->ilinks != nullptr) {
    src->ilinks->ref_count++;
  }

  memcpy(*dest, src, sizeof(*src));

  (*dest)->addr = (src->addr != nullptr) ? xstrdup(src->addr) : nullptr;
  (*dest)->cc = (src->cc != nullptr) ? xstrdup(src->cc) : nullptr;
  (*dest)->city = (src->city != nullptr) ? xstrdup(src->city) : nullptr;
  (*dest)->country = (src->country != nullptr) ? xstrdup(src->country) : nullptr;
  (*dest)->cross_road = (src->cross_road != nullptr) ? xstrdup(src->cross_road) : nullptr;
  (*dest)->facility = (src->facility != nullptr) ? xstrdup(src->facility) : nullptr;
  (*dest)->phone_nr = (src->phone_nr != nullptr) ? xstrdup(src->phone_nr) : nullptr;
  (*dest)->phone_nr2 = (src->phone_nr2 != nullptr) ? xstrdup(src->phone_nr2) : nullptr;
  (*dest)->fax_nr = (src->fax_nr != nullptr) ? xstrdup(src->fax_nr) : nullptr;
  (*dest)->email = (src->email != nullptr) ? xstrdup(src->email) : nullptr;
  (*dest)->postal_code = (src->postal_code != nullptr) ? xstrdup(src->postal_code) : nullptr;
  (*dest)->state = (src->state != nullptr) ? xstrdup(src->state) : nullptr;
}

void garmin_fs_convert(void* fs)
{
  garmin_fs_t* gmsd = (garmin_fs_t*) fs;

  if (gmsd->addr) {
    gmsd->addr = cet_convert_string(gmsd->addr);
  }
  if (gmsd->cc) {
    gmsd->cc = cet_convert_string(gmsd->cc);
  }
  if (gmsd->city) {
    gmsd->city = cet_convert_string(gmsd->city);
  }
  if (gmsd->country) {
    gmsd->country = cet_convert_string(gmsd->country);
  }
  if (gmsd->cross_road) {
    gmsd->cross_road = cet_convert_string(gmsd->cross_road);
  }
  if (gmsd->facility) {
    gmsd->facility = cet_convert_string(gmsd->facility);
  }
  if (gmsd->phone_nr) {
    gmsd->phone_nr = cet_convert_string(gmsd->phone_nr);
  }
  if (gmsd->phone_nr2) {
    gmsd->phone_nr2 = cet_convert_string(gmsd->phone_nr2);
  }
  if (gmsd->fax_nr) {
    gmsd->fax_nr = cet_convert_string(gmsd->fax_nr);
  }
  if (gmsd->email) {
    gmsd->email = cet_convert_string(gmsd->email);
  }
  if (gmsd->postal_code) {
    gmsd->postal_code = cet_convert_string(gmsd->postal_code);
  }
  if (gmsd->state) {
    gmsd->state = cet_convert_string(gmsd->state);
  }
}

/* GPX - out */

void
garmin_fs_xml_fprint(const Waypoint* waypt,
                     QXmlStreamWriter* writer)
{
  garmin_fs_t* gmsd = GMSD_FIND(waypt);

  if (gmsd == nullptr) {
    return;
  }

  /* Find out if there is at least one field set */
  const char* addr = GMSD_GET(addr, "");
  if (! *addr) {
    addr = GMSD_GET(city, "");
  }
  if (! *addr) {
    addr = GMSD_GET(country, "");
  }
  if (! *addr) {
    addr = GMSD_GET(postal_code, "");
  }
  if (! *addr) {
    addr = GMSD_GET(state, "");
  }

  const char* phone = GMSD_GET(phone_nr, "");

  if (*addr || *phone ||
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
    if (*addr) {
      char* str;
      writer->writeStartElement(QStringLiteral("gpxx:Address"));

      if ((str = GMSD_GET(addr, NULL))) {
        writer->writeTextElement(QStringLiteral("gpxx:StreetAddress"), str);
      }
      if ((str = GMSD_GET(city, NULL))) {
        writer->writeTextElement(QStringLiteral("gpxx:City"), str);
      }
      if ((str = GMSD_GET(state, NULL))) {
        writer->writeTextElement(QStringLiteral("gpxx:State"), str);
      }
      if ((str = GMSD_GET(country, NULL))) {
        writer->writeTextElement(QStringLiteral("gpxx:Country"), str);
      }
      if ((str = GMSD_GET(postal_code, NULL))) {
        writer->writeTextElement(QStringLiteral("gpxx:PostalCode"), str);
      }
      writer->writeEndElement(); // /gpxx::Address
    }

    if (*phone) {
      writer->writeTextElement(QStringLiteral("gpxx:PhoneNumber"), phone);
    }

    writer->writeEndElement(); // /gpxx::WaypointExtension
    writer->writeEndElement(); // /extensions.
  }

}

void
garmin_fs_xml_convert(const int base_tag, int tag, const QString& Qcdatastr, Waypoint* waypt)
{
  // FIXME: eliminate C string copy/use here:
  const char *cdatastr = xstrdup(Qcdatastr);
  garmin_fs_t* gmsd = GMSD_FIND(waypt);
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
      GMSD_SET(display, gt_display_mode_symbol);
    } else if (case_ignore_strcmp(cdatastr, "SymbolAndDescription") == 0) {
      GMSD_SET(display, gt_display_mode_symbol_and_comment);
    } else {
      GMSD_SET(display, gt_display_mode_symbol_and_name);
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
    GMSD_SETSTR(addr, cdatastr);
    break;
  case 8:
    GMSD_SETSTR(city, cdatastr);
    break;
  case 9:
    GMSD_SETSTR(state, cdatastr);
    break;
  case 10:
    GMSD_SETSTR(country, cdatastr);
    break;
  case 11:
    GMSD_SETSTR(postal_code, cdatastr);
    break;
  case 12:
    GMSD_SETSTR(phone_nr, cdatastr);
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

  garmin_fs_t* gmsd = GMSD_FIND(waypt);
  cat = cat | (GMSD_GET(category, 0));

  if (gmsd == nullptr) {
    gmsd = garmin_fs_alloc(-1);
    fs_chain_add(&waypt->fs, (format_specific_data*) gmsd);
  }
  GMSD_SET(category, cat);
  return 1;
}

void
garmin_fs_garmin_after_read(const GPS_PWay way, Waypoint* wpt, const int protoid)
{
  garmin_fs_t* gmsd = garmin_fs_alloc(protoid);
  fs_chain_add(&wpt->fs, (format_specific_data*) gmsd);

  /* nothing happens until gmsd is allocated some lines above */

  /* !!! class needs protocol specific conversion !!! (ToDo)
  GMSD_SET(wpt_class, way[i]->wpt_class);
  */
  /* flagged data fields */
  GMSD_SET(display, gt_switch_display_mode_value(way->dspl, gps_waypt_type, 1));
  if (way->category != 0) {
    GMSD_SET(category, way->category);
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
  GMSD_SETNSTR(cc, way->cc, sizeof(way->cc));
  GMSD_SETNSTR(state, way->state, sizeof(way->state));
  GMSD_SETSTR(city, way->city);
  GMSD_SETSTR(facility, way->facility);
  GMSD_SETSTR(cross_road, way->cross_road);
  GMSD_SETSTR(addr, way->addr);
}

void
garmin_fs_garmin_before_write(const Waypoint* wpt, GPS_PWay way, const int protoid)
{
  garmin_fs_t* gmsd = GMSD_FIND(wpt);

  (void)protoid; // unused for now.

  if (gmsd == nullptr) {
    return;
  }

  /* ToDo: protocol specific conversion of class
  way[i]->wpt_class = GMSD_GET(wpt_class, way[i]->wpt_class);
  	*/
  way->dspl = gt_switch_display_mode_value(
                GMSD_GET(display, way->dspl), gps_waypt_type, 0);
  way->category = GMSD_GET(category, way->category);
  way->dpth = WAYPT_GET(wpt, depth, way->dpth);
  way->dst = WAYPT_GET(wpt, proximity, way->dpth);
  way->temperature = WAYPT_GET(wpt, temperature, way->temperature);

  GMSD_GETNSTR(cc, way->cc, sizeof(way->cc));
  GMSD_GETNSTR(city, way->city, sizeof(way->city));
  GMSD_GETNSTR(state, way->state, sizeof(way->state));
  GMSD_GETNSTR(facility, way->facility, sizeof(way->facility));
  GMSD_GETNSTR(cross_road, way->cross_road, sizeof(way->cross_road));
  GMSD_GETNSTR(addr, way->addr, sizeof(way->addr));
}
