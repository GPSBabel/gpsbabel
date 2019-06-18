/*

    Misc GPS (text to data) parsers.

    Copyright (C) 2007 Olaf Klein, o.b.klein@gpsbabel.org

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
#include "jeeps/gpsmath.h"
#include <cmath>
#include <cstdio>
#include <cstdlib> //strtod

/*
 * parse_distance:
 *
 *  str:     input string, i.e. "128.2 km" or "22mi"
 *  val:     pointer to resulting value
 *  scale:   scaling parameter for unit-less values
 *  module:  calling module, i.e. "garmin_txt"
 */

int
parse_distance(const char* str, double* val, double scale, const char* module)
{
  char* unit;

  if ((str == nullptr) || (*str == '\0')) {
    return 0;
  }
  *val = strtod(str, &unit);
  if (unit == nullptr) {
    fatal("%s: Unconvertible numeric value (%s)!\n", module, str);
  }

  if (fabs(*val) + 1 >= 1.0e25) {
    return 0; /* not only Garmin uses this as 'unknown value' */
  }

  while (isspace(*unit)) {
    unit++;
  }

  if (*unit == '\0') {
    *val *= scale;
    return 1;
  }

  if (case_ignore_strcmp(unit, "m") == 0) /* do nothing, that's our standard */;
  else if (case_ignore_strcmp(unit, "ft") == 0) {
    *val = FEET_TO_METERS(*val);
  } else if (case_ignore_strcmp(unit, "feet") == 0) {
    *val = FEET_TO_METERS(*val);
  } else if (case_ignore_strcmp(unit, "k") == 0) {
    *val *= 1000.0;
  } else if (case_ignore_strcmp(unit, "km") == 0) {
    *val *= 1000.0;
  } else if (case_ignore_strcmp(unit, "nm") == 0) {
    *val = NMILES_TO_METERS(*val);
  } else if (case_ignore_strcmp(unit, "mi") == 0) {
    *val = MILES_TO_METERS(*val);
  } else if (case_ignore_strcmp(unit, "fa") == 0) {
    *val = FATHOMS_TO_METERS(*val);
  } else {
    fatal("%s: Unsupported distance unit in item '%s'!\n", module, str);
  }
  return 2;
}

int
parse_distance(const QString& str, double* val, double scale, const char* module) {
  return parse_distance(CSTR(str), val, scale, module);
}

/*
 * parse_speed:
 *
 *  str:     input string, i.e. "22.3 km/h" or "40mph"
 *  val:     pointer to resulting value
 *  scale:   scaling parameter for unit-less values
 *  module:  calling module, i.e. "garmin_txt"
 */
int
parse_speed(const char* str, double* val, const double scale, const char* module)
{
  char* unit;

  if ((str == nullptr) || (*str == '\0')) {
    return 0;
  }

  *val = strtod(str, &unit);
  if (unit == nullptr) {
    fatal("%s: Unconvertible numeric value (%s)!\n", module, str);
  }

  while (isspace(*unit)) {
    unit++;
  }

  if (*unit == '\0') {
    *val *= scale;
    return 1;
  }

  if (case_ignore_strcmp(unit, "m/s") == 0) ;
  else if (case_ignore_strcmp(unit, "mps") == 0) ;
  else if (case_ignore_strcmp(unit, "kph") == 0) {
    *val = KPH_TO_MPS(*val);
  } else if (case_ignore_strcmp(unit, "km/h") == 0) {
    *val = KPH_TO_MPS(*val);
  } else if (case_ignore_strcmp(unit, "kmh") == 0) {
    *val = KPH_TO_MPS(*val);
  } else if (case_ignore_strcmp(unit, "kt") == 0) {
    *val = KNOTS_TO_MPS(*val);
  } else if (case_ignore_strcmp(unit, "knot") == 0) {
    *val = KNOTS_TO_MPS(*val);
  } else if (case_ignore_strcmp(unit, "mph") == 0) {
    *val = MPH_TO_MPS(*val);
  } else if (case_ignore_strcmp(unit, "mi/h") == 0) {
    *val = MPH_TO_MPS(*val);
  } else if (case_ignore_strcmp(unit, "mih") == 0) {
    *val = MPH_TO_MPS(*val);
  } else {
    warning("%s: Unsupported speed unit '%s' in item '%s'!\n", module, unit, str);
  }

  return 2;
}

int
parse_speed(const QString& str, double* val, const double scale, const char* module)
{
  return parse_speed(CSTR(str), val, scale, module);
}

/*
 * Convert string 'str' into geodetic latitude & longitude values. The format
 * will be interpreted depending on 'grid' parameter.
 *
 * return value: number of characters parsed
 */

int
parse_coordinates(const char* str, int datum, const grid_type grid,
                  double* latitude, double* longitude, const char* module)
{
  double lat, lon;
  unsigned char lathemi=0, lonhemi=0;
  int deg_lat, deg_lon, min_lat, min_lon;
  char map[3];
  int utmz;
  double utme, utmn;
  char utmc;
  int result;
  int ct;
  double lx, ly;
  const char* format;

  int valid = 1;

  switch (grid) {

  case grid_lat_lon_ddd:
    format = "%c%lf %c%lf%n";
    ct = sscanf(str, format,
                &lathemi, &lat, &lonhemi, &lon, &result);
    valid = (ct == 4);
    break;

  case grid_lat_lon_dmm:
    format = "%c%d %lf %c%d %lf%n";
    ct = sscanf(str, format,
                &lathemi, &deg_lat, &lat, &lonhemi, &deg_lon, &lon, &result);
    valid = (ct == 6);
    if (valid) {
      lat = (double)deg_lat + (lat / 60.0);
      lon = (double)deg_lon + (lon / 60.0);
    }
    break;

  case grid_lat_lon_dms:
    format = "%c%d %d %lf %c%d %d %lf%n";
    ct = sscanf(str, format,
                &lathemi, &deg_lat, &min_lat, &lat, &lonhemi, &deg_lon, &min_lon, &lon,
                &result);
    valid = (ct == 8);
    if (valid) {
      lat = (double)deg_lat + ((double)min_lat / 60.0) + (lat / 3600.0);
      lon = (double)deg_lon + ((double)min_lon / 60.0) + (lon / 3600.0);
    }
    break;

  case grid_bng:
    datum = DATUM_WGS84;	/* fix */
    format = "%2s %lf %lf%n";
    ct = sscanf(str, format,
                map, &lx, &ly,
                &result);
    valid = (ct == 3);
    if (valid) {
      if (! GPS_Math_UKOSMap_To_WGS84_M(map, lx, ly, &lat, &lon))
        fatal("%s: Unable to convert BNG coordinates (%s)!\n",
              module, str);
    }
    lathemi = lonhemi = '\0';
    break;

  case grid_utm:
    format = "%d %c %lf %lf%n";
    ct = sscanf(str, format,
                &utmz, &utmc, &utme, &utmn,
                &result);
    valid = (ct == 4);
    if (valid) {
      if (! GPS_Math_UTM_EN_To_Known_Datum(&lat, &lon, utme, utmn, utmz, utmc, datum))
        fatal("%s: Unable to convert UTM coordinates (%s)!\n",
              module, str);
    }
    lathemi = lonhemi = '\0';
    break;

  case grid_swiss: {
    double east, north;

    datum = DATUM_WGS84;	/* fix */
    format = "%lf %lf%n";
    ct = sscanf(str, format,
                &east, &north, &result);
    valid = (ct == 2);
    GPS_Math_Swiss_EN_To_WGS84(east, north, &lat, &lon);
    break;
  }
  default:
    /* this should never happen in a release version */
    fatal("%s/util: Unknown grid in parse_coordinates (%d)!\n",
          module, (int)grid);
  }

  if (! valid) {
    warning("%s: sscanf error using format \"%s\"!\n", module, format);
    warning("%s: parsing has stopped at parameter number %d.\n", module, ct);
    fatal("%s: could not convert coordinates \"%s\"!\n", module, str);
  }

  if (lathemi == 'S') {
    lat = -lat;
  }
  if (lonhemi == 'W') {
    lon = -lon;
  }

  if (datum != DATUM_WGS84) {
    double alt;
    GPS_Math_Known_Datum_To_WGS84_M(lat, lon, 0.0,
                                    &lat, &lon, &alt, datum);
  }

  if (latitude) {
    *latitude = lat;
  }
  if (longitude) {
    *longitude = lon;
  }

  return result;
}

int
parse_coordinates(const QString& str, int datum, const grid_type grid,
                  double* latitude, double* longitude, const char* module)
{
  return parse_coordinates(CSTR(str), datum, grid,
                           latitude, longitude, module);
}
