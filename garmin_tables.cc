/*
    Garmin icon tables
    Based on information provided by Ian Cowley, Sigurd Humerfelt,
	and Garmin MapSource

    Copyright (C) 2003-2007 Robert Lipe, robertlipe+source@gpsbabel.org

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

#include <cstdint>               // for int32_t
#include <cstring>               // for strncpy, strchr, strlen, strncmp
#include <QtCore/QChar>          // for operator==, QChar
#include <QtCore/QDebug>         // for QDebug
#include <QtCore/QStringRef>     // for QStringRef
#include <QtCore/Qt>             // for CaseInsensitive
#include "defs.h"
#include "garmin_tables.h"
#include "jeeps/gpsmath.h"       // for GPS_Lookup_Datum_Index, GPS_Math_Get_Datum_Name
#include "src/core/logging.h"    // for Fatal

#define MYNAME "garmin_tables"

// these tables are in an include file so they can be shared with mkicondoc.
#include "garmin_icon_tables.h"  // for garmin_icon_table, garmin_smart_icon_table

/* ICAO country code table */

/* source: https://en.wikipedia.org/wiki/ICAO_airport_code */

const gt_country_code_t gt_country_codes[] = {
  { "ZM,", "Mongolia" },
  { "ZK,", "North Korea" },
  { "Z*,", "China" },
  { "Y*,", "Australia" },
  { "WS,", "Singapore" },
  { "WP,", "Timor-Leste" },
  { "WM,", "Malaysia (Peninsular Malaysia)" },
  { "WB,", "Brunei, Malaysia (East Malaysia)" },
  { "WA,WI,WQ,WR,", "Indonesia" },
  { "VY,", "Myanmar" },
  { "VV,", "Vietnam" },
  { "VT,", "Thailand" },
  { "VR,", "Maldives" },
  { "VQ,", "Bhutan" },
  { "VN,", "Nepal" },
  { "VM,", "Macau" },
  { "VL,", "Laos" },
  { "VH,", "Hong Kong" },
  { "VG,", "Bangladesh" },
  { "VD,", "Cambodia" },
  { "VC,", "Sri Lanka" },
  { "VA,VE,VI,VO,", "India" },
  { "UT,", "Tajikistan, Turkmenistan, Uzbekistan" },
  { "UM,", "Belarus and Russia (Kaliningrad Oblast)" },
  { "UK,", "Ukraine" },
  { "UG,", "Georgia" },
  { "UD,", "Armenia" },
  { "UC,", "Kyrgyzstan" },
  { "UB,", "Azerbaijan" },
  { "UA,", "Kazakhstan" },
  { "U*,", "Russia" },
  { "TX,", "UK (Bermuda)" },
  { "TV,", "Saint Vincent and the Grenadines" },
  { "TU,", "UK (British Virgin Islands)" },
  { "TT,", "Trinidad and Tobago" },
  { "TR,", "UK (Montserrat)" },
  { "TQ,", "UK (Anguilla)" },
  { "TN,", "Caribbean Netherlands, Aruba, Curaçao, Sint Maarten" },
  { "TL,", "Saint Lucia" },
  { "TK,", "Saint Kitts and Nevis" },
  { "TJ,", "USA (Puerto Rico)" },
  { "TI,", "USA (U.S. Virgin Islands)" },
  { "TG,", "Grenada" },
  { "TF,", "France (Guadeloupe, Martinique, Saint Barthélemy, Saint Martin)" },
  { "TD,", "Dominica" },
  { "TB,", "Barbados" },
  { "TA,", "Antigua and Barbuda" },
  { "SY,", "Guyana" },
  { "SV,", "Venezuela" },
  { "SU,", "Uruguay" },
  { "SP,", "Peru" },
  { "SO,", "France (French Guiana)" },
  { "SM,", "Suriname" },
  { "SL,", "Bolivia" },
  { "SK,", "Colombia" },
  { "SH,", "Chile" },
  { "SG,", "Paraguay" },
  { "SF,", "United Kingdom (Falkland Islands)" },
  { "SE,", "Ecuador" },
  { "SC,", "Chile (including Easter Island)" },
  { "SB,SI,SJ,", "Brazil" },
  { "SA,", "Argentina" },
  { "RP,", "Philippines" },
  { "RO,", "Japan (Okinawa)" },
  { "RK,", "South Korea" },
  { "RJ,", "Japan (Mainland)" },
  { "RC,", "Taiwan" },
  { "PW,", "USA (Wake Island)" },
  { "PT,", "Federated States of Micronesia, Palau" },
  { "PM,", "USA (Midway Island)" },
  { "PL,", "Kiribati (Line Islands)" },
  { "PK,", "Marshall Islands" },
  { "PJ,", "USA (Johnston Atoll)" },
  { "PH,", "USA (Hawaii)" },
  { "PG,", "USA (Guam, Northern Mariana Islands)" },
  { "PC,", "Kiribati (Canton Airfield, Phoenix Islands)" },
  { "PB,", "USA (Baker Island)" },
  { "PA,PF,PO,PP,", "USA (Alaska)" },
  { "OY,", "Yemen" },
  { "OT,", "Qatar" },
  { "OS,", "Syria" },
  { "OR,", "Iraq" },
  { "OP,", "Pakistan" },
  { "OO,", "Oman" },
  { "OM,", "United Arab Emirates" },
  { "OL,", "Lebanon" },
  { "OK,", "Kuwait" },
  { "OJ,", "Jordan and the West Bank" },
  { "OI,", "Iran" },
  { "OE,", "Saudi Arabia" },
  { "OB,", "Bahrain" },
  { "OA,", "Afghanistan" },
  { "NZ,", "New Zealand, Antarctica" },
  { "NW,", "France (New Caledonia)" },
  { "NV,", "Vanuatu" },
  { "NT,", "France (French Polynesia)" },
  { "NS,", "Samoa, United States (American Samoa)" },
  { "NL,", "France (Wallis and Futuna)" },
  { "NI,", "Niue" },
  { "NG,", "Kiribati (Gilbert Islands), Tuvalu" },
  { "NF,", "Fiji, Tonga" },
  { "NC,", "Cook Islands" },
  { "MZ,", "Belize" },
  { "MY,", "Bahamas" },
  { "MW,", "Cayman Islands" },
  { "MU,", "Cuba" },
  { "MT,", "Haiti" },
  { "MS,", "El Salvador" },
  { "MR,", "Costa Rica" },
  { "MP,", "Panama" },
  { "MN,", "Nicaragua" },
  { "MM,", "Mexico" },
  { "MK,", "Jamaica" },
  { "MH,", "Honduras" },
  { "MG,", "Guatemala" },
  { "MD,", "Dominican Republic" },
  { "MB,", "Turks and Caicos Islands" },
  { "LZ,", "Slovakia" },
  { "LY,", "Serbia and Montenegro" },
  { "LX,", "Gibraltar" },
  { "LW,", "Macedonia" },
  { "LV,", "Palestinian Territories" },
  { "LU,", "Moldova" },
  { "LT,", "Turkey" },
  { "LS,", "Switzerland" },
  { "LR,", "Romania" },
  { "LQ,", "Bosnia and Herzegovina" },
  { "LP,", "Portugal (including the Azores and Madeira)" },
  { "LO,", "Austria" },
  { "LN,", "Monaco" },
  { "LM,", "Malta" },
  { "LL,", "Israel" },
  { "LK,", "Czech Republic" },
  { "LJ,", "Slovenia" },
  { "LI,", "Italy" },
  { "LH,", "Hungary" },
  { "LG,", "Greece" },
  { "LF,", "France (Metropolitan France; including Saint-Pierre and Miquelon)" },
  { "LE,", "Spain (mainland section and Balearic Islands)" },
  { "LD,", "Croatia" },
  { "LC,", "Cyprus" },
  { "LB,", "Bulgaria" },
  { "LA,", "Albania" },
  { "K*,", "United States of America" },
  { "HU,", "Uganda" },
  { "HT,", "Tanzania" },
  { "HS,", "Sudan and South Sudan" },
  { "HR,", "Rwanda" },
  { "HL,", "Libya" },
  { "HK,", "Kenya" },
  { "HH,", "Eritrea" },
  { "HE,", "Egypt" },
  { "HD,", "Djibouti" },
  { "HC,", "Somalia (including Somaliland)" },
  { "HB,", "Burundi" },
  { "HA,", "Ethiopia" },
  { "GV,", "Cape Verde" },
  { "GU,", "Guinea" },
  { "GS,", "Western Sahara" },
  { "GQ,", "Mauritania" },
  { "GO,", "Senegal" },
  { "GM,", "Morocco" },
  { "GL,", "Liberia" },
  { "GG,", "Guinea-Bissau" },
  { "GF,", "Sierra Leone" },
  { "GE,", "Spain (Ceuta and Melilla)" },
  { "GC,", "Spain (Canary Islands)" },
  { "GB,", "The Gambia" },
  { "GA,", "Mali" },
  { "FZ,", "Democratic Republic of the Congo" },
  { "FY,", "Namibia" },
  { "FX,", "Lesotho" },
  { "FW,", "Malawi" },
  { "FV,", "Zimbabwe" },
  { "FT,", "Chad" },
  { "FS,", "Seychelles" },
  { "FQ,", "Mozambique" },
  { "FP,", "São Tomé and Príncipe" },
  { "FO,", "Gabon" },
  { "FN,", "Angola" },
  { "FM,", "Comoros, France (Mayotte and Réunion), and Madagascar" },
  { "FL,", "Zambia" },
  { "FK,", "Cameroon" },
  { "FJ,", "British Indian Ocean Territory" },
  { "FI,", "Mauritius" },
  { "FH,", "Saint Helena, Ascension and Tristan da Cunha" },
  { "FG,", "Equatorial Guinea" },
  { "FE,", "Central African Republic" },
  { "FD,", "Swaziland" },
  { "FC,", "Republic of the Congo" },
  { "FB,", "Botswana" },
  { "FA,", "South Africa" },
  { "EY,", "Lithuania" },
  { "EV,", "Latvia" },
  { "ET,", "Germany (military)" },
  { "ES,", "Sweden" },
  { "EP,", "Poland" },
  { "EN,", "Norway" },
  { "EL,", "Luxembourg" },
  { "EK,", "Denmark and the Faroe Islands" },
  { "EI,", "Ireland" },
  { "EH,", "Netherlands" },
  { "EG,", "United Kingdom (and Crown dependencies)" },
  { "EF,", "Finland" },
  { "EE,", "Estonia" },
  { "ED,", "Germany (civil)" },
  { "EB,", "Belgium" },
  { "DX,", "Togo" },
  { "DT,", "Tunisia" },
  { "DR,", "Niger" },
  { "DN,", "Nigeria" },
  { "DI,", "Côte d'Ivoire" },
  { "DG,", "Ghana" },
  { "DF,", "Burkina Faso" },
  { "DB,", "Benin" },
  { "DA,", "Algeria" },
  { "C*,", "Canada" },
  { "BK,", "Kosovo" },
  { "BI,", "Iceland" },
  { "BG,", "Greenland" },
  { "AY,", "Papua New Guinea" },
  { "AN,", "Nauru" },
  { "AG,", "Solomon Islands" },
  { nullptr, nullptr }
};

/* gt_waypt_classes: gdb internal order */
const char* const gt_waypt_class_names[] = {
  "User Waypoint",
  "Airport",
  "Intersection",
  "NDB",
  "VOR",
  "Runway Threshold",
  "Airport Intersection",
  "Airport NDB",
  "Map Point",
  "Map Area",
  "Map Intersection",
  "Map Address",
  "Map Line",
  nullptr
};

/* gt_display_mode_names: this order is used by most devices */
const char* const gt_display_mode_names[] = {
  "Symbol & Name",
  "Symbol",
  "Symbol & Description"
};

struct grid_mapping_t {
  const char* shortname;
  const char* longname;
  grid_type grid;
};

/* gt_mps_grid_names: !!! degree sign substituted with '*' !!! */

static const grid_mapping_t gt_mps_grid_names[] = {
  { "ddd",	"Lat/Lon hddd.ddddd*",		grid_lat_lon_ddd },
  { "dmm",	"Lat/Lon hddd*mm.mmm'",		grid_lat_lon_dmm },
  { "dms",	"Lat/Lon hddd*mm'ss.s\"",	grid_lat_lon_dms },
  { "bng",	"British National Grid",	grid_bng },
  { "utm",	"UTM",				grid_utm },
  { "swiss",	"Swiss grid",			grid_swiss },
  { nullptr,	nullptr,	(grid_type) 0 }
};

/* gt_mps_datum_names: */

struct datum_mapping_t {
  const char* jeeps_name;
  const char* mps_name;
};

/* will be continued (when requested) */
static const datum_mapping_t gt_mps_datum_names[] = {
  { "Alaska-NAD27",	"NAD27 Alaska" },
  { "Bahamas NAD27",	"NAD27 Bahamas" },
  { "Canada_Mean(NAD27)",	"NAD27 Canada" },
  { "Canal_Zone_(NAD27)",	"NAD27 Canal Zone" },
  { "Carribean NAD27",	"NAD27 Caribbean" },
  { "Cent America NAD27",	"NAD27 Central" },
  { "Cuba NAD27",		"NAD27 Cuba" },
  { "Geodetic Datum 49",	"Geodetic Datum '49" },
  { "Greenland NAD27",	"NAD27 Greenland" },
  { "Mexico NAD27",	"NAD27 Mexico" },
  { "North America 83",	"NAD83" },
  { "OSGB36",		"Ord Srvy Grt Britn" },
  { nullptr,	nullptr }
};

struct garmin_color_t {
  const char* name;
  int32_t rgb;
};

static const garmin_color_t gt_colors[] = {
  { "Unknown",		unknown_color },
  { "Black", 		0x000000 },
  { "DarkRed",		0x00008B },
  { "DarkGreen",		0x006400 },
  { "DarkYellow",		0x008B8B },
  { "DarkBlue",		0x8B0000 },
  { "DarkMagenta",	0x8B008B },
  { "DarkCyan",		0x8B8B00 },
  { "LightGray",		0xD3D3D3 },
  { "DarkGray",		0xA9A9A9 },
  { "Red",		0x0000FF },
  { "Green",		0x008000 },
  { "Yellow",		0x00FFFF },
  { "Blue",		0xFF0000 },
  { "Magenta",		0xFF00FF },
  { "Cyan",		0xFFFF00 },
  { "White",		0xFFFFFF },
  { "Transparent",	unknown_color }, /* Currently not handled */
  { nullptr, 0 }
};

#define GT_COLORS_CT ((sizeof(gt_colors) / sizeof(gt_colors[0])) - 1)

unsigned char
gt_switch_display_mode_value(const unsigned char display_mode, const int protoid, const char device)
{
  if (device) {
    switch (protoid) {

    case 103:
    case 107:
    case 108:
    case 109:
    case 110:
      return display_mode & 3;
      break;
    case 104:
      switch (display_mode) {
      case 0:
      case 1:
        return gt_display_mode_symbol;
      case 3:
        return gt_display_mode_symbol_and_name;
      case 5:
        return gt_display_mode_symbol_and_comment;
      }
    case 155:
      switch (display_mode) {
      case 1:
        return gt_display_mode_symbol;
      case 3:
        return gt_display_mode_symbol_and_name;
      case 5:
        return gt_display_mode_symbol_and_comment;
      }
    }
    return gt_display_mode_symbol_and_name;
  } else {
    switch (protoid) {

    case 103:
    case 107:
    case 108:
    case 109:
    case 110:
      return display_mode & 3;
    case 104:
    case 155:
      switch (display_mode) {
      case gt_display_mode_symbol:
        return 1;
      case gt_display_mode_symbol_and_name:
        return 3;
      case gt_display_mode_symbol_and_comment:
        return 5;
      }
    }
    return 0;
  }
}

QString
gt_find_desc_from_icon_number(const int icon, garmin_formats_e garmin_format)
{
  if ((garmin_format == GDB) && (icon >= 500) && (icon <= 563)) {
    return QString("Custom %1").arg(icon - 500);
  }

  if ((garmin_format == PCX) && (icon >= 7680) && (icon <= 8191)) {
    return QString("Custom %1").arg(icon - 7680);
  }

  for (const icon_mapping_t* i = garmin_icon_table; i->icon; i++) {
    switch (garmin_format) {
    case MAPSOURCE:
    case GDB:
      if (icon == i->mpssymnum) {
        return i->icon;
      }
      break;
    case PCX:
    case GARMIN_SERIAL:
      if (icon == i->pcxsymnum) {
        return i->icon;
      }
      break;
    default:
      fatal(MYNAME ": unknown garmin format.\n");
    }
  }
  return DEFAULT_ICON_DESCR;
}

int gt_find_icon_number_from_desc(const QString& desc, garmin_formats_e garmin_format)
{
  static int find_flag = 0;
  const icon_mapping_t* i;
  int def_icon = DEFAULT_ICON_VALUE;

  if (desc.isNull()) {
    return def_icon;
  }

  /*
   * If we were given a numeric icon number as a description
   * (i.e. 8255), just return that.
   */
  int n = desc.toInt();
  if (n)  {
    return n;
  }

  if (desc.startsWith("Custom ", Qt::CaseInsensitive)) {
    int base = 0;
    if (garmin_format == GDB) {
      base = 500;
    }
    if (garmin_format == PCX) {
      base = 7680;
    }
    if (base) {
      n = desc.midRef(7).toInt();
      return n + base;
    }
  }

  for (i = garmin_smart_icon_table; global_opts.smart_icons && i->icon; i++) {
    if (desc.compare(i->icon, Qt::CaseInsensitive) == 0) {
      switch (garmin_format) {
      case MAPSOURCE:
      case GDB:
        return i->mpssymnum;
      case PCX:
      case GARMIN_SERIAL:
        return i->pcxsymnum;
      default:
        fatal(MYNAME ": unknown garmin format.\n");
      }
    }
  }
  for (i = garmin_icon_table; i->icon; i++) {
    if (desc.compare(i->icon, Qt::CaseInsensitive) == 0) {
      switch (garmin_format) {
      case MAPSOURCE:
      case GDB:
        return i->mpssymnum;
      case PCX:
      case GARMIN_SERIAL:
        return i->pcxsymnum;
      default:
        fatal(MYNAME ": unknown garmin format.\n");
      }
    }
  }

  /*
   *  try to handle some complex icon names: i.e. "Blue Diamond" and "Diamond, Blue"
   *  "find_flag" prevents us from a possible endless loop
   */

  if (find_flag == 0) {
    const char* prefixes[] = {
      "White ", "Red ", "Green ", "Blue ", "Black ", nullptr
    };
    // Rewrite "Green Square" to "Square, Green".
    for (const char** prefix = prefixes; *prefix != nullptr; prefix++) {
      if (desc.startsWith(*prefix, Qt::CaseInsensitive)) {
        QString buff = desc;
        buff.replace(*prefix, "");
        buff.append(", ");
        buff.append(*prefix);
        buff = buff.trimmed();

        find_flag = 1;
        int result = gt_find_icon_number_from_desc(buff, garmin_format);
        find_flag = 0;

        return result;
      }
    }
  }
  return def_icon;
}

const char*
gt_get_icao_country(const QString& cc)
{
  const gt_country_code_t* x = &gt_country_codes[0];

  if (cc.isEmpty()) {
    return nullptr;
  }

  do {
    const char* ccx = x->cc;
    const QString qccx = x->cc;
    while (ccx != nullptr) {
      if (qccx.left(2) == cc) {
        return x->country;
      }
      if ((ccx[0] == cc[0]) && (ccx[1] == '*')) {
        return x->country;
      }
      ccx = strchr(ccx, ',');
      if (ccx == nullptr) {
        break;
      }
      ccx++;
    }
    x++;
  } while (x->cc != nullptr);
  return nullptr;
}

const char*
gt_get_icao_cc(const QString& country, const QString& shortname)
{
  static char res[3];
  const gt_country_code_t* x = &gt_country_codes[0];

  if (country.isEmpty()) {
    if (shortname == nullptr) {
      return nullptr;
    }
    switch (shortname.length()) {
    case 3:
      strncpy(res, CSTR(shortname), 1);
      break;
    case 4:
      strncpy(res, CSTR(shortname), 2);
      break;
    default:
      return nullptr;
    }
    const char* test = gt_get_icao_country(res);
    if (test != nullptr) {
      return res;
    } else {
      return nullptr;
    }
  }

  do {
    if (country.compare(x->country, Qt::CaseInsensitive) != 0) {
      x++;
      continue;
    }

    if (strlen(x->cc) <= 3) {
      strncpy(res, x->cc, 3);
      if (res[1] == '*') {
        res[1] = '\0';
      } else {
        res[2] = '\0';
      }
      return res;
    }
    if (shortname.length() == 4) {
      const char* ccx = x->cc;

      strncpy(res, CSTR(shortname), 2);
      res[2] = '\0';
      while (ccx != nullptr) {
        if (strncmp(ccx, res, 2) == 0) {
          return res;
        }
        if ((ccx[0] == res[0]) && (ccx[1] == '*')) {
          return res;
        }
        ccx = strchr(ccx, ',');
        if (ccx == nullptr) {
          break;
        }
        ccx++;
      }
    }
    return nullptr;
  } while (x->country != nullptr);
  return nullptr;
}

grid_type
gt_lookup_grid_type(const char* grid_name, const QString& module)
{
  for (const grid_mapping_t* g = gt_mps_grid_names; (g->shortname); g++) {
    if (QString::compare(grid_name, g->shortname, Qt::CaseInsensitive) == 0 ||
        QString::compare(grid_name, g->longname,Qt::CaseInsensitive) == 0) {
      return g->grid;
    }
  }

  fatal(FatalMsg() << module << ": Unsupported grid (" << grid_name <<
                       ". See GPSBabel help for supported grids.\n");

  return grid_unknown;	/* (warnings) */
}

const char*
gt_get_mps_grid_longname(const grid_type grid, const char* module)
{
  if ((grid < GRID_INDEX_MIN) || (grid > GRID_INDEX_MAX))
    fatal("%s: Grid index out of range %d (%d..%d)!",
          module, (int) grid,
          (int)GRID_INDEX_MIN, (int)GRID_INDEX_MAX);
  return gt_mps_grid_names[grid].longname;
}

const char*
gt_get_mps_datum_name(const int datum_index)
{
  const char* result = GPS_Math_Get_Datum_Name(datum_index);

  for (const datum_mapping_t* d = gt_mps_datum_names; (d->jeeps_name); d++)
    if (QString::compare(result, d->jeeps_name, Qt::CaseInsensitive) == 0) {
      return d->mps_name;
    }

  return result;
}

int
gt_lookup_datum_index(const char* datum_str, const QString& module)
{
  const char* name = datum_str;

  for (const datum_mapping_t* d = gt_mps_datum_names; (d->jeeps_name); d++) {
    if (QString::compare(name, d->mps_name, Qt::CaseInsensitive) == 0) {
      name = d->jeeps_name;
      break;
    }
  }

  int result = GPS_Lookup_Datum_Index(name);

  // Didn't get a hit?  Try again after modifying the lookup.
  if (result < 0) {
    QString tmp = QString(datum_str) + " mean";
    result = GPS_Lookup_Datum_Index(tmp);
  }

  if (result < 0) {
    fatal(FatalMsg() << module << ": Unsupported datum (" << datum_str <<
                         "). See GPSBabel help for supported datums.");
  }
  return result;
}

uint32_t
gt_color_value(const unsigned int garmin_index)
{
  if ((garmin_index < GT_COLORS_CT)) {
    return gt_colors[garmin_index].rgb;
  } else {
    return unknown_color;  /* -1 */
  }
}

uint32_t
gt_color_value_by_name(const QString& name)
{
  for (unsigned int i = 0; i < GT_COLORS_CT; i++)
    if (QString::compare(gt_colors[i].name, name, Qt::CaseInsensitive) == 0) {
      return gt_colors[i].rgb;
    }

  return gt_colors[0].rgb;
}

int
gt_color_index_by_name(const QString& name)
{
  for (unsigned int i = 0; i < GT_COLORS_CT; i++)
    if (QString::compare(gt_colors[i].name, name, Qt::CaseInsensitive) == 0) {
      return i;
    }

  return 0; /* unknown */
}

int
gt_color_index_by_rgb(const int rgb)
{
  for (unsigned int i = 0; i < GT_COLORS_CT; i++)
    if (rgb == gt_colors[i].rgb) {
      return i;
    }

  return 0; /* unknown */
}

const char*
gt_color_name(const unsigned int garmin_index)
{
  if ((garmin_index < GT_COLORS_CT)) {
    return gt_colors[garmin_index].name;
  } else {
    return gt_colors[0].name;
  }
}
