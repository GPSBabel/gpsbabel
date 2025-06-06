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

#include <cmath>                   // for fabs
#include <cstdio>                  // for sscanf
#include <stdexcept>               // for invalid_argument, out_of_range
#include <string>                  // for stod

#include <QString>                 // for QString
#include <QtGlobal>                // for qPrintable

#include "defs.h"                  // for case_ignore_strcmp, gbFatal, grid_type, KPH_TO_MPS, MPH_TO_MPS, gbWarning, FEET_TO_METERS, KNOTS_TO_MPS, kDatumWGS84, FATHOMS_TO_METERS, MILES_TO_METERS, NMILES_TO_METERS, parse_coordinates, CSTR, parse_distance, parse_double, parse_integer, parse_speed
#include "jeeps/gpsmath.h"         // for GPS_Math_Known_Datum_To_WGS84_M, GPS_Math_Swiss_EN_To_WGS84, GPS_Math_UKOSMap_To_WGS84_H, GPS_Math_UTM_EN_To_Known_Datum


/*
 * parse_integer
 *
 *  str:     input string
 *  id:      identifier for error messages
 *  ok:      conversion status.
 *           if nullptr any conversion errors will fatal.
 *  end:     unconverted trailing portion of string.
 *           if nullptr a non-empty trailing portion will cause a conversion error.
 *  base:    conversion base
 */

int parse_integer(const QString& str, const QString& id, bool* ok, QString* end, int base)
{
  auto ss = str.toStdString();
  size_t pos = 0;
  int result = 0;
  try {
    result = stoi(ss, &pos, base);
  } catch (const std::invalid_argument&) {
    if (ok == nullptr) {
      gbFatal("%s: conversion to integer failed: invalid argument \"%s\".\n",
            gbLogCStr(id), gbLogCStr(str));
    } else {
      *ok = false;
      return 0;
    }
  } catch (const std::out_of_range&) {
    if (ok == nullptr) {
      gbFatal("%s: conversion to integer failed: out of range \"%s\".\n",
            gbLogCStr(id), gbLogCStr(str));
    } else {
      *ok = false;
      return 0;
    }
  } catch (...) {
    if (ok == nullptr) {
      gbFatal("%s: conversion to integer failed: unknown exception \"%s\".\n",
            gbLogCStr(id), gbLogCStr(str));
    } else {
      *ok = false;
      return 0;
    }
  }

  QString remainder = QString::fromStdString(ss.erase(0, pos));
  if ((end == nullptr) && !remainder.trimmed().isEmpty()) {
    if (ok == nullptr) {
      gbFatal("%s: conversion to integer failed: conversion of \"%s\" failed due to unexpected trailing data \"%s\".\n",
            gbLogCStr(id), gbLogCStr(str), gbLogCStr(remainder));
    } else {
      *ok = false;
      return 0;
    }
  }
  if (end != nullptr) { // return possibly empty trailing portion of str
    *end = remainder;
  }

  if (ok != nullptr) {
    *ok = true;
  }

  return result;
}

/*
 * parse_double
 *
 *  str:     input string
 *  id:      identifier for error messages
 *  ok:      conversion status.
 *           if nullptr any conversion errors will fatal.
 *  end:     unconverted trailing portion of string.
 *           if nullptr a non-empty trailing portion will cause a conversion error.
 *  base:    conversion base
 *
 */

double parse_double(const QString& str, const QString& id, bool* ok, QString* end)
{
  auto ss = str.toStdString();
  size_t pos = 0;
  double result = 0.0;
  try {
    result = stod(ss, &pos);
  } catch (const std::invalid_argument&) {
    if (ok == nullptr) {
      gbFatal("%s: conversion to double failed: invalid argument \"%s\".\n",
            gbLogCStr(id), gbLogCStr(str));
    } else {
      *ok = false;
      return 0.0;
    }
  } catch (const std::out_of_range&) {
    if (ok == nullptr) {
      gbFatal("%s: conversion to double failed: out of range \"%s\".\n",
            gbLogCStr(id), gbLogCStr(str));
    } else {
      *ok = false;
      return 0.0;
    }
  } catch (...) {
    if (ok == nullptr) {
      gbFatal("%s: conversion to double failed: unknown exception \"%s\".\n",
            gbLogCStr(id), gbLogCStr(str));
    } else {
      *ok = false;
      return 0.0;
    }
  }

  QString remainder = QString::fromStdString(ss.erase(0, pos));
  if ((end == nullptr) && !remainder.trimmed().isEmpty()) {
    if (ok == nullptr) {
      gbFatal("%s: conversion to double failed: conversion of \"%s\" failed due to unexpected trailing data \"%s\".\n",
            gbLogCStr(id), gbLogCStr(str), gbLogCStr(remainder));
    } else {
      *ok = false;
      return 0.0;
    }
  }
  if (end != nullptr) { // return possibly empty trailing portion of str
    *end = remainder;
  }

  if (ok != nullptr) {
    *ok = true;
  }

  return result;
}

/*
 * parse_distance:
 *
 *  str:     input string, i.e. "128.2 km" or "22mi"
 *  val:     pointer to resulting value
 *  scale:   scaling parameter for unit-less values
 */

int
parse_distance(const QString& str, double* val, double scale)
{
  if (str.isEmpty()) {
    return 0;
  }

  QString unit;
  constexpr bool* dieonfailure = nullptr;
  *val = parse_double(str, "", dieonfailure, &unit);

  if (fabs(*val) + 1 >= 1.0e25) {
    return 0; /* not only Garmin uses this as 'unknown value' */
  }

  unit = unit.trimmed();

  if (unit.isEmpty()) {
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
    gbFatal("Unsupported distance unit in item '%s'!\n", gbLogCStr(str));
  }
  return 2;
}

/*
 * parse_speed:
 *
 *  str:     input string, i.e. "22.3 km/h" or "40mph"
 *  val:     pointer to resulting value
 *  scale:   scaling parameter for unit-less values
 */
int
parse_speed(const QString& str, double* val, const double scale)
{

  if (str.isEmpty()) {
    return 0;
  }

  QString unit;
  constexpr bool* dieonfailure = nullptr;
  *val = parse_double(str, "", dieonfailure, &unit);

  unit = unit.trimmed();

  if (unit.isEmpty()) {
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
    gbWarning("Unsupported speed unit '%s' in item '%s'!\n", gbLogCStr(unit), gbLogCStr(str));
  }

  return 2;
}

/*
 * Convert string 'str' into geodetic latitude & longitude values. The format
 * will be interpreted depending on 'grid' parameter.
 *
 * return value: number of characters parsed
 */

int
parse_coordinates(const char* str, int datum, const grid_type grid,
                  double* latitude, double* longitude)
{
  double lat;
  double lon;
  unsigned char lathemi=0;
  unsigned char lonhemi=0;
  int deg_lat;
  int deg_lon;
  int min_lat;
  int min_lon;
  char map[3];
  int utmz;
  double utme;
  double utmn;
  char utmc;
  int result;
  int ct;
  double lx;
  double ly;
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
    datum = kDatumWGS84;	/* fix */
    format = "%2s %lf %lf%n";
    ct = sscanf(str, format,
                map, &lx, &ly,
                &result);
    valid = (ct == 3);
    if (valid) {
      if (! GPS_Math_UKOSMap_To_WGS84_H(map, lx, ly, &lat, &lon))
        gbFatal("Unable to convert BNG coordinates (%s)!\n",
              str);
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
        gbFatal("Unable to convert UTM coordinates (%s)!\n",
              str);
    }
    lathemi = lonhemi = '\0';
    break;

  case grid_swiss: {
    double east;
    double north;

    datum = kDatumWGS84;	/* fix */
    format = "%lf %lf%n";
    ct = sscanf(str, format,
                &east, &north, &result);
    valid = (ct == 2);
    GPS_Math_Swiss_EN_To_WGS84(east, north, &lat, &lon);
    break;
  }
  default:
    /* this should never happen in a release version */
    gbFatal("Unknown grid in parse_coordinates (%d)!\n",
          (int)grid);
  }

  if (! valid) {
    gbWarning("sscanf error using format \"%s\"!\n", format);
    gbWarning("parsing has stopped at parameter number %d.\n", ct);
    gbFatal("could not convert coordinates \"%s\"!\n", str);
  }

  if (lathemi == 'S') {
    lat = -lat;
  }
  if (lonhemi == 'W') {
    lon = -lon;
  }

  if (datum != kDatumWGS84) {
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
                  double* latitude, double* longitude)
{
  return parse_coordinates(CSTR(str), datum, grid,
                           latitude, longitude);
}
