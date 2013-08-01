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
#include "garmin_fs.h"
#include "garmin_tables.h"
#include "inifile.h"

#include <QtCore/QXmlStreamWriter>

#define MYNAME "garmin_fs"

garmin_fs_t*
garmin_fs_alloc(const int protocol)
{
  garmin_fs_t* result = NULL;

  result = (garmin_fs_t*)xcalloc(1, sizeof(*result));
  result->fs.type = FS_GMSD;
  result->fs.copy = (fs_copy) garmin_fs_copy;
  result->fs.destroy = garmin_fs_destroy;
  result->fs.convert = garmin_fs_convert;
  result->fs.next = NULL;

  result->protocol = protocol;

  return result;
}

void
garmin_fs_destroy(void* fs)
{
  garmin_fs_t* data = (garmin_fs_t*) fs;
  if (data != NULL) {
    garmin_ilink_t* ilinks;

    if (data->addr != NULL) {
      xfree(data->addr);
    }
    if (data->cc != NULL) {
      xfree(data->cc);
    }
    if (data->city != NULL) {
      xfree(data->city);
    }
    if (data->country != NULL) {
      xfree(data->country);
    }
    if (data->cross_road != NULL) {
      xfree(data->cross_road);
    }
    if (data->facility != NULL) {
      xfree(data->facility);
    }
    if (data->phone_nr != NULL) {
      xfree(data->phone_nr);
    }
    if (data->phone_nr2 != NULL) {
      xfree(data->phone_nr2);
    }
    if (data->fax_nr != NULL) {
      xfree(data->fax_nr);
    }
    if (data->email != NULL) {
      xfree(data->email);
    }
    if (data->postal_code != NULL) {
      xfree(data->postal_code);
    }
    if (data->state != NULL) {
      xfree(data->state);
    }

    if ((ilinks = data->ilinks) != NULL) {
      ilinks->ref_count--;
      if (ilinks->ref_count <= 0) {
        while (ilinks != NULL) {
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
  if (src == NULL) {
    *dest = NULL;
    return;
  }
  *dest = (garmin_fs_t*) xmalloc(sizeof(*src));

  /* do not copy interlinks, only increment the refrence counter */
  if (src->ilinks != NULL) {
    src->ilinks->ref_count++;
  }

  memcpy(*dest, src, sizeof(*src));

  (*dest)->addr = (src->addr != NULL) ? xstrdup(src->addr) : NULL;
  (*dest)->cc = (src->cc != NULL) ? xstrdup(src->cc) : NULL;
  (*dest)->city = (src->city != NULL) ? xstrdup(src->city) : NULL;
  (*dest)->country = (src->country != NULL) ? xstrdup(src->country) : NULL;
  (*dest)->cross_road = (src->cross_road != NULL) ? xstrdup(src->cross_road) : NULL;
  (*dest)->facility = (src->facility != NULL) ? xstrdup(src->facility) : NULL;
  (*dest)->phone_nr = (src->phone_nr != NULL) ? xstrdup(src->phone_nr) : NULL;
  (*dest)->phone_nr2 = (src->phone_nr2 != NULL) ? xstrdup(src->phone_nr2) : NULL;
  (*dest)->fax_nr = (src->fax_nr != NULL) ? xstrdup(src->fax_nr) : NULL;
  (*dest)->email = (src->email != NULL) ? xstrdup(src->email) : NULL;
  (*dest)->postal_code = (src->postal_code != NULL) ? xstrdup(src->postal_code) : NULL;
  (*dest)->state = (src->state != NULL) ? xstrdup(src->state) : NULL;
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
garmin_fs_xml_fprint(const waypoint* waypt,
                     QXmlStreamWriter& writer)
{
  const char* phone, *addr;
  garmin_fs_t* gmsd = GMSD_FIND(waypt);

  if (gmsd == NULL) {
    return;
  }

  /* Find out if there is at least one field set */
  addr = GMSD_GET(addr, "");
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

  phone = GMSD_GET(phone_nr, "");

  if (*addr || *phone ||
      (gmsd->flags.category && gmsd->category) ||
      WAYPT_HAS(waypt, depth) ||
      WAYPT_HAS(waypt, proximity) ||
      WAYPT_HAS(waypt, temperature) ||
      gmsd->flags.display) {
    writer.writeStartElement("extensions");
    writer.writeStartElement("gpxx:WaypointExtension");
    writer.writeNamespace("http://www.garmin.com/xmlschemas/GpxExtensions/v3",
                          "gpxx");
    if WAYPT_HAS(waypt, proximity) {
      writer.writeTextElement("gpxx:Proximity", QString::number(waypt->proximity, 'f', 6));
    }
    if WAYPT_HAS(waypt, temperature) {
      writer.writeTextElement("gpxx:Temperature",  QString::number(waypt->temperature, 'f', 6));
    }
    if WAYPT_HAS(waypt, depth) {
      writer.writeTextElement("gpxx:Depth", QString::number(waypt->depth, 'f', 6));
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
      writer.writeTextElement("gpxx:DisplayMode", cx);
    }
    if (gmsd->flags.category && gmsd->category) {
      int i;
      uint16_t cx = gmsd->category;
      writer.writeStartElement("gpxx:Categories");
      for (i = 0; i < 16; i++) {
        if (cx & 1) {
          writer.writeTextElement("gpxx:Category", QString("Category %1").arg(i+1));
        }
        cx = cx >> 1;
      }
      writer.writeEndElement(); // gpxx:Categories
    }
    if (*addr) {
      char* str;
      writer.writeStartElement("gpxx:Address");

      if ((str = GMSD_GET(addr, NULL))) {
        writer.writeTextElement("gpxx:StreetAddress", str);
      }
      if ((str = GMSD_GET(city, NULL))) {
        writer.writeTextElement("gpxx:City", str);
      }
      if ((str = GMSD_GET(state, NULL))) {
        writer.writeTextElement("gpxx:State", str);
      }
      if ((str = GMSD_GET(country, NULL))) {
        writer.writeTextElement("gpxx:Country", str);
      }
      if ((str = GMSD_GET(postal_code, NULL))) {
        writer.writeTextElement("gpxx:PostalCode", str);
      }
      writer.writeEndElement(); // /gpxx::Address
    }

    if (*phone) {
      writer.writeTextElement("gpxx:PhoneNumber", phone);
    }

    writer.writeEndElement(); // /gpxx::WaypointExtension
    writer.writeEndElement(); // /extensions.
  }

}

void
garmin_fs_xml_convert(const int base_tag, int tag, const char* cdatastr, waypoint* waypt)
{
  garmin_fs_t* gmsd;

  gmsd = GMSD_FIND(waypt);
  if (gmsd == NULL) {
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
  } else if (global_opts.inifile != NULL) {
    // Do we have a gpsbabel.ini that maps category names to category #'s?
    for (i = 0; i < 16; i++) {
      char* c;
      char key[3];

      snprintf(key, sizeof(key), "%d", i + 1);
      c = inifile_readstr(global_opts.inifile, GMSD_SECTION_CATEGORIES, key);
      if ((c != NULL) && (case_ignore_strcmp(c, category_name) == 0)) {
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
garmin_fs_merge_category(const char* category_name, waypoint* waypt)
{
  uint16_t cat;
  garmin_fs_t* gmsd;

  // Attempt to get a textual category name to a category number.
  if (!garmin_fs_convert_category(category_name, &cat)) {
    return 0;
  }

  gmsd = GMSD_FIND(waypt);
  cat = cat | (GMSD_GET(category, 0));

  if (gmsd == NULL) {
    gmsd = garmin_fs_alloc(-1);
    fs_chain_add(&waypt->fs, (format_specific_data*) gmsd);
  }
  GMSD_SET(category, cat);
  return 1;
}

void
garmin_fs_garmin_after_read(const GPS_PWay way, waypoint* wpt, const int protoid)
{
  garmin_fs_t* gmsd = NULL;

  gmsd = garmin_fs_alloc(protoid);
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
garmin_fs_garmin_before_write(const waypoint* wpt, GPS_PWay way, const int protoid)
{
  garmin_fs_t* gmsd = GMSD_FIND(wpt);

  if (gmsd == NULL) {
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
