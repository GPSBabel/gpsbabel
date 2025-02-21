/*
    Garmin icon tables
    Based on information provided by Ian Cowley, Sigurd Humerfelt,
	and Garmin MapSource

    Copyright (C) 2003-2014 Robert Lipe, robertlipe+source@gpsbabel.org

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

#ifndef GARMIN_TABLES_H
#define GARMIN_TABLES_H

#include <cstdint>              // for uint32_t
#include <QString>              // for QString
#include "defs.h"               // for grid_type


enum garmin_formats_e {MAPSOURCE, PCX, GARMIN_SERIAL, GDB};

QString gt_find_desc_from_icon_number(int icon, garmin_formats_e garmin_format);
int gt_find_icon_number_from_desc(const QString& desc, garmin_formats_e garmin_format);

enum gt_waypt_classes_e {
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
};

extern const QStringList gt_waypt_class_names;

const char* gt_get_icao_country(const QString& cc);
const char* gt_get_icao_cc(const QString& country, const QString& shortname);

/* this order is used by most devices */
enum gt_display_modes_e {
  gt_display_mode_symbol_and_name = 0,
  gt_display_mode_symbol,
  gt_display_mode_symbol_and_comment
};

extern const QStringList gt_display_mode_names;

enum gt_gdb_display_modes_e {
  gt_gdb_display_mode_symbol = 0,
  gt_gdb_display_mode_symbol_and_name,
  gt_gdb_display_mode_symbol_and_comment
};

unsigned char gt_switch_display_mode_value(unsigned char display_mode, int protoid, char device);

grid_type gt_lookup_grid_type(const QString& grid_name);
QString gt_get_mps_grid_longname(grid_type grid);
int gt_lookup_datum_index(const QString& datum_str);
QString gt_get_mps_datum_name(int datum_index);
uint32_t gt_color_value(unsigned int garmin_index);
uint32_t gt_color_value_by_name(const QString& name);
int gt_color_index_by_name(const QString& name);
int gt_color_index_by_rgb(int rgb);
QString gt_color_name(unsigned int garmin_index);

#endif
