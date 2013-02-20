/*

    Implementation of special data used by Garmin products.

    Copyright (C) 2006 Olaf Klein, o.b.klein@gpsbabel.org

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

#ifndef GARMIN_FS_H
#define GARMIN_FS_H

#include <ctype.h>
#include "defs.h"
#include "jeeps/gps.h"

/* this order is used by most devices */
/* typedef enum {
	garmin_display_symbol_and_name = 0,
	garmin_display_symbol_only = 1,
	garmin_display_symbol_and_description = 2
} garmin_display_t;
*/

/* macros */

#define GMSD_FIND(a) (garmin_fs_t *) fs_chain_find((a)->fs, FS_GMSD)
#define GMSD_HAS(a) (gmsd && gmsd->flags.a)

/* GMSD_GET(a,b): a = any gmsd field, b = default value */
#define GMSD_GET(a,b) ((gmsd) && (gmsd->flags.a)) ? (gmsd->a) : (b)

/* GMSD_SET(a,b): a = numeric gmsd field, b = numeric source */
#define GMSD_SET(a,b) if (gmsd) {gmsd->a = (b); gmsd->flags.a = 1; }

/* GMSD_UNSET(a): a = gmsd field */
#define GMSD_UNSET(a) if (gmsd) { gmsd->flags.a = 0; }

/* GMSD_SETSTR(a,b): a = gmsd field, b = null terminated source */
#define GMSD_SETSTR(a,b) if (gmsd && (b) && (b)[0]) { gmsd->a = xstrdup((b)); gmsd->flags.a = 1; }

/* GMSD_SETNSTR(a,b,c): a = gmsd field, b = source, c = sizeof(source) */
#define GMSD_SETNSTR(a,b,c) if (gmsd && (b) && (b)[0]) { gmsd->a = xstrndup((b),(c)); gmsd->flags.a = 1; }

/* GMSD_GETNSTR(a,b,c): a = gmsd field, b = target, c = sizeof(target) */
#define GMSD_GETNSTR(a,b,c) if (gmsd && gmsd->flags.a) strncpy((b),gmsd->a,(c))

typedef struct garmin_ilink_s {
  int ref_count;
  double lat, lon, alt;
  struct garmin_ilink_s* next;
} garmin_ilink_t;

typedef struct {
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
} garmin_fs_flags_t;

typedef struct garmin_fs_s {
  format_specific_data fs;
  garmin_fs_flags_t flags;

  int protocol;		/* ... used by device (-1 is MapSource) */

  gbint32 icon;
  int wpt_class;
  gbint32 display;
  gbint16 category;
  char* city;			/* city name */
  char* facility;			/* facility name */
  char* state;			/* state */
  char* cc;			/* country code */
  char* cross_road;		/* Intersection road label */
  char* addr;			/* address + number */
  char* country;			/* country */
  char* phone_nr;			/* phone number */
  char* phone_nr2;		/* phone number (2) */
  char* fax_nr;			/* fax number */
  char* postal_code;		/* postal code */
  char* email;			/* email address */
  garmin_ilink_t* ilinks;
#ifdef GMSD_EXPERIMENTAL
  char subclass[22];
#endif
} garmin_fs_t, *garmin_fs_p;

garmin_fs_t* garmin_fs_alloc(const int protocol);
void garmin_fs_destroy(void* fs);
void garmin_fs_copy(garmin_fs_t** dest, garmin_fs_t* src);
void garmin_fs_convert(void* fs);
char* garmin_fs_xstrdup(const char* src, size_t size);

/* for GPX */
void garmin_fs_xml_convert(const int base_tag, int tag, const char* cdatastr, waypoint* waypt);
class QXmlStreamWriter;
void garmin_fs_xml_fprint(gbfile* ofd, const waypoint* waypt, QXmlStreamWriter&);

/* common garmin_fs utilities */

/* ..convert_category: returns 1=OK; 0=Unable to convert category */
unsigned char garmin_fs_convert_category(const char* category_name, gbuint16* category);

/* ..merge_category: returns 1=OK; 0=Unable to convert category */
unsigned char garmin_fs_merge_category(const char* category_name, waypoint* waypt);

#define GMSD_SECTION_CATEGORIES "Garmin Categories"

void garmin_fs_garmin_after_read(const GPS_PWay way, waypoint* wpt, const int protoid);
void garmin_fs_garmin_before_write(const waypoint* wpt, GPS_PWay way, const int protoid);

#endif
