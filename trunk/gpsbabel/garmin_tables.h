/*
    Garmin icon tables
    Based on information provided by Ian Cowley, Sigurd Humerfelt,
	and Garmin MapSource

    Copyright (C) 2003 Robert Lipe, robertlipe@usa.net

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

#ifndef GARMIN_TABLES_H
#define GARMIN_TABLES_H

#include "defs.h"

#define DEFAULT_ICON_DESCR "Waypoint"
#define DEFAULT_ICON_VALUE 18

typedef struct icon_mapping {
  const int mpssymnum;
  const int pcxsymnum;
  const char* icon;
} icon_mapping_t;

typedef enum {MAPSOURCE, PCX, GARMIN_SERIAL, GDB} garmin_formats_e;

const char* gt_find_desc_from_icon_number(const int icon, garmin_formats_e garmin_format, int* dynamic);
int gt_find_icon_number_from_desc(const char* desc, garmin_formats_e garmin_format);

extern icon_mapping_t garmin_icon_table[];

typedef enum {
  gt_waypt_class_user_waypoint = 0,
  gt_waypt_class_airport,
  gt_waypt_class_intersection,
  gt_waypt_class_ndb,
  gt_waypt_class_vor,
  gt_waypt_class_runway_threshold,
  gt_waypt_class_airport_intersection,
  gt_waypt_class_airport_ndb,
  gt_waypt_class_map_point,
  gt_waypt_class_map_area,
  gt_waypt_class_map_intersection,
  gt_waypt_class_map_address,
  gt_waypt_class_map_line
} gt_waypt_classes_e;

extern char* gt_waypt_class_names[];

typedef struct gt_country_code_s {
  const char* cc;
  const char* country;
} gt_country_code_t;

extern gt_country_code_t gt_country_codes[];

const char* gt_get_icao_country(const char* cc);
const char* gt_get_icao_cc(const char* country, const char* shortname);

/* this order is used by most devices */
typedef enum {
  gt_display_mode_symbol_and_name = 0,
  gt_display_mode_symbol,
  gt_display_mode_symbol_and_comment
} gt_display_modes_e;

extern char* gt_display_mode_names[];

#define GT_DISPLAY_MODE_MIN gt_display_mode_symbol_and_name
#define GT_DISPLAY_MODE_MAX gt_display_mode_symbol_and_comment

typedef enum {
  gt_gdb_display_mode_symbol = 0,
  gt_gdb_display_mode_symbol_and_name,
  gt_gdb_display_mode_symbol_and_comment
} gt_gdb_display_modes_e;

unsigned char gt_convert_category(const char* name, int* category);

unsigned char gt_switch_display_mode_value(const unsigned char display_mode, const int protoid, const char device);

grid_type gt_lookup_grid_type(const char* grid_name, const char* module);
const char* gt_get_mps_grid_longname(const grid_type grid, const char* module);
int gt_lookup_datum_index(const char* datum_str, const char* module);
const char* gt_get_mps_datum_name(const int datum_index);
gbuint32 gt_color_value(const int garmin_index);
gbuint32 gt_color_value_by_name(const char* name);
int gt_color_index_by_name(const char* name);
int gt_color_index_by_rgb(const int rgb);
const char* gt_color_name(const int garmin_index);

#endif
