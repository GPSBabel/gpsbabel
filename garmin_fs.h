/*

    Implementation of special data used by Garmin products.

    Copyright (C) 2006 Olaf Klein, o.b.klein@gpsbabel.org
    Copyright (C) 2006-2014 Robert Lipe, robertlipe@gpsbabel.org

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

#ifndef GARMIN_FS_H
#define GARMIN_FS_H

#include <cstddef>                  // for size_t
#include <cstdint>                  // for int32_t, int16_t, uint16_t

#include <QtCore/QString>           // for QString
#include <QtCore/QXmlStreamWriter>  // for QXmlStreamWriter

#include "defs.h"
#include "formspec.h"               // for FsChainFind, kFsGmsd, FormatSpecificData
#include "jeeps/gps.h"


/* this order is used by most devices */
/* enum garmin_display_t {
	garmin_display_symbol_and_name = 0,
	garmin_display_symbol_only = 1,
	garmin_display_symbol_and_description = 2
};
*/

struct garmin_ilink_t {
  int ref_count;
  double lat, lon, alt;
  garmin_ilink_t* next;
};

struct garmin_fs_flags_t {
public:
  garmin_fs_flags_t() :
  icon(0),
  wpt_class(0),
  display(0),
  category(0),
  city(0),
  state(0),
  facility(0),
  cc(0),
  cross_road(0),
  addr(0),
  country(0),
  phone_nr(0),
  phone_nr2(0),
  fax_nr(0),
  postal_code(0),
  email(0)
#ifdef GMSD_EXPERIMENTAL
  , subclass(0)
#endif
  {}

  unsigned int icon:1;
  unsigned int wpt_class:1;
  unsigned int display:1;
  unsigned int category:1;
  unsigned int city:1;
  unsigned int state:1;
  unsigned int facility:1;
  unsigned int cc:1;
  unsigned int cross_road:1;
  unsigned int addr:1;
  unsigned int country:1;
  unsigned int phone_nr:1;
  unsigned int phone_nr2:1;
  unsigned int fax_nr:1;
  unsigned int postal_code:1;
  unsigned int email:1;
#ifdef GMSD_EXPERIMENTAL
  unsigned int subclass:1;
#endif
};

class garmin_fs_t : public FormatSpecificData {
public:
  garmin_fs_flags_t flags;

  int protocol{0};		/* ... used by device (-1 is MapSource) */

  int32_t icon{0};
  int wpt_class{0};
  int32_t display{0};
  int16_t category{0};
  QString city;					/* city name */
  QString facility;			/* facility name */
  QString state;				/* state */
  QString cc;						/* country code */
  QString cross_road;		/* Intersection road label */
  QString addr;					/* address + number */
  QString country;			/* country */
  QString phone_nr;			/* phone number */
  QString phone_nr2;		/* phone number (2) */
  QString fax_nr;				/* fax number */
  QString postal_code;	/* postal code */
  QString email;				/* email address */
  garmin_ilink_t* ilinks{nullptr};
#ifdef GMSD_EXPERIMENTAL
  char subclass[22]{};
#endif

public:
  garmin_fs_t() = default;
  ~garmin_fs_t() override;
  garmin_fs_t(const garmin_fs_t& other);
  garmin_fs_t& operator=(const garmin_fs_t& rhs) = delete; /* not implemented */
  garmin_fs_t(garmin_fs_t&&) = delete; /* not implemented */
  garmin_fs_t& operator=(garmin_fs_t&&) = delete; /* not implemented */

  static garmin_fs_t* find(const Waypoint* wpt) {
    return reinterpret_cast<garmin_fs_t*>(wpt->fs.FsChainFind(kFsGmsd));
  }

#define GEN_GMSD_METHODS(field) \
  static bool has_##field(const garmin_fs_t* gmsd) \
  { \
    return gmsd && gmsd->flags.field; \
  } \
  static decltype(field) get_##field(const garmin_fs_t* gmsd, decltype(field) p) \
  { \
    return (gmsd && gmsd->flags.field)? gmsd->field : p; \
  } \
  static void set_##field(garmin_fs_t* gmsd, decltype(field) p) \
  { \
    if (gmsd) { \
      gmsd->field = p; \
      gmsd->flags.field = 1; \
    } \
  } \
  static void unset_##field(garmin_fs_t* gmsd) \
  { \
    if (gmsd) { \
      gmsd->flags.field = 0; \
    } \
  }

  GEN_GMSD_METHODS(icon)
  GEN_GMSD_METHODS(wpt_class)
  GEN_GMSD_METHODS(display)
  GEN_GMSD_METHODS(category)

#undef GEN_GMSD_METHODS

#define GEN_GMSD_STR_METHODS(field) \
  static bool has_##field(const garmin_fs_t* gmsd) \
  { \
    return gmsd && gmsd->flags.field; \
  } \
  static QString get_##field(const garmin_fs_t* gmsd, const QString& p) \
  { \
    return (gmsd && gmsd->flags.field)? gmsd->field : p; \
  } \
  static void set_##field(garmin_fs_t* gmsd, const char* p) \
  { \
    if (gmsd && p && *p) { \
      gmsd->field = p; \
      gmsd->flags.field = 1; \
    } \
  } \
  static void set_##field(garmin_fs_t* gmsd, const QString& p) \
  { \
    if (gmsd && !p.isEmpty()) { \
      gmsd->field = p; \
      gmsd->flags.field = 1; \
    } \
  } \
  static void unset_##field(garmin_fs_t* gmsd) \
  { \
    if (gmsd) { \
      gmsd->flags.field = 0; \
    } \
  }

  GEN_GMSD_STR_METHODS(city)
  GEN_GMSD_STR_METHODS(facility)
  GEN_GMSD_STR_METHODS(state)
  GEN_GMSD_STR_METHODS(cc)
  GEN_GMSD_STR_METHODS(cross_road)
  GEN_GMSD_STR_METHODS(addr)
  GEN_GMSD_STR_METHODS(country)
  GEN_GMSD_STR_METHODS(phone_nr)
  GEN_GMSD_STR_METHODS(phone_nr2)
  GEN_GMSD_STR_METHODS(fax_nr)
  GEN_GMSD_STR_METHODS(postal_code)
  GEN_GMSD_STR_METHODS(email)

#undef GEN_GMSD_STR_METHODS
};

garmin_fs_t* garmin_fs_alloc(int protocol);
void garmin_fs_destroy(void* fs);
void garmin_fs_copy(void** dest, const void* src);
char* garmin_fs_xstrdup(const char* src, size_t size);

/* for GPX */
void garmin_fs_xml_convert(int base_tag, int tag, const QString& qstr, Waypoint* waypt);
void garmin_fs_xml_fprint(const Waypoint* waypt, QXmlStreamWriter*);

/* common garmin_fs utilities */

/* ..convert_category: returns 1=OK; 0=Unable to convert category */
unsigned char garmin_fs_convert_category(const char* category_name, uint16_t* category);

/* ..merge_category: returns 1=OK; 0=Unable to convert category */
unsigned char garmin_fs_merge_category(const char* category_name, Waypoint* waypt);

#define GMSD_SECTION_CATEGORIES "Garmin Categories"

void garmin_fs_garmin_after_read(GPS_PWay way, Waypoint* wpt, int protoid);
void garmin_fs_garmin_before_write(const Waypoint* wpt, GPS_PWay way, int protoid);

#endif
