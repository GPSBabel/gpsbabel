/*

    Support for Motorrad Routenplaner (Map&Guide) .bcr files.

    Copyright (C) 2005-2007 Olaf Klein, o.b.klein@gpsbabel.org

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

/*
    2006/01/22: reader simplified with inifile library
    2007/01/30: new option prefer_shortnames
    		don't check global_opts.objective
    2007/04&14: new handling of DESCRIPTION lines
*/

#include <cmath>            // for M_PI, atan, exp, log, tan
#include <cstdio>           // for printf, snprintf, sscanf
#include <cstdlib>          // for atof, atoi

#include <QtCore/QString>   // for QString, operator+
#include <QtCore/Qt>        // for CaseInsensitive
#include <QtCore/QtGlobal>  // for foreach

#include "defs.h"
#include "csv_util.h"       // for csv_stringclean
#include "garmin_tables.h"  // for gt_find_desc_from_icon_number, gt_find_icon_number_from_desc, MAPSOURCE
#include "gbfile.h"         // for gbfprintf, gbfclose, gbfopen, gbfile
#include "inifile.h"        // for inifile_readstr, inifile_done, inifile_init, inifile_t


#define MYNAME "bcr"

#undef BCR_DEBUG

#define R_EARTH		6371000		/* radius of our big blue ball */
#define BCR_DEF_ICON		"Standort"
#define BCR_DEF_MPS_ICON	"Waypoint"
#define BCR_UNKNOWN		/*(double) */ 999999999

/*
    6371014 would be a better value when converting to f.e. to mapsoure,
    but this seems to be used by Map&Guide when exporting to XML.
*/

static gbfile* fout;
static int curr_rte_num, target_rte_num;
static double radius;
static inifile_t* ini;

/* placeholders for options */

static char* rtenum_opt;
static char* rtename_opt;
static char* radius_opt;
static char* prefer_shortnames_opt;

static
arglist_t bcr_args[] = {
  {
    "index", &rtenum_opt, "Index of route to write (if more than one in source)",
    nullptr, ARGTYPE_INT, "1", nullptr, nullptr
  },
  {
    "name", &rtename_opt, "New name for the route",
    nullptr, ARGTYPE_STRING, ARG_NOMINMAX, nullptr
  },
  {
    "radius", &radius_opt, "Radius of our big earth (default 6371000 meters)", "6371000",
    ARGTYPE_FLOAT, ARG_NOMINMAX, nullptr
  },
  {
    "prefer_shortnames", &prefer_shortnames_opt, "Use shortname instead of description",
    nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
  },
  ARG_TERMINATOR
};

typedef struct {
  const char* bcr_name;
  const char* mps_name;
  const char* symbol_DE;
  bool  warned;
} bcr_icon_mapping_t;

static
bcr_icon_mapping_t bcr_icon_mapping[] = {
  { BCR_DEF_ICON,		BCR_DEF_MPS_ICON, 	BCR_DEF_ICON, false },
  { "",			BCR_DEF_MPS_ICON, 	"Eigene Adressen", false },
  { "AdrMon alpen",	"Summit",		"Pass-Strassen", false },
  { "AdrMon bauern",	nullptr,			"Bauern- und Biohoefe", false },
  { "AdrMon cmpngs",	"Campground",		"Campingplaetzte", false },
  { "AdrMon p_aeu",	"Scenic Area",		"Sehenswertes", false },
  { "AdrMon p_beu",	"Gas Station",		"Tanken", false },
  { "AdrMon p_deu",	"Parking Area",		"Parken", false },
  { "AdrMon p_feu",	"Restaurant",		"Gastro", false },
  { "AdrMon p_geu",	"Museum",		"Freizeit", false },
  { "AdrMon p_heu",	"Gas Station",		"Tankstellen", false },
  { "AdrMon p_keu",	nullptr,			"Faehrverbindungen", false },
  { "AdrMon p_leu",	nullptr,			"Grenzuebergaenge", false },
  { "AdrMon p_teu",	nullptr,			"Wein- und Sektgueter", false },
  { "AdrMon RUINEN",	"Ghost Town",		"Burgen und Schloesser", false },
  { "AdrMon NFHAUS",	"Residence",		"Naturfreundehaeuser", false },
  { "AdrMon racing",	"Bike Trail",		"Rennstrecken", false },
  { "AdrMon TNKRST",	"Bar",			"Tankraststaetten", false },
  { "AdrMon tpclub",	"Contact, Biker",	"Motorrad-Clubs", false },
  { "AdrMon tpequ",	nullptr,			"Motorrad-Equipment", false },
  { "AdrMon tphot",	"Hotel",		"Motorrad-Hotels", false },
  { "AdrMon tpmh",	nullptr,			"Motorradhaendler", false },
  { "AdrMon tpss",	"Restricted Area",	"Sperrungen", false },
  { "AdrMon tpsw",	"Scenic Area",		"Sehenswertes", false },
  { "AdrMon tptref",	nullptr,			"Treffpunkte", false },
  { "AdrMon VORTE",	"Information",		"Ortsinformationen", false },
  { "AdrMon WEBCAM",	nullptr,			"WebCam-Standorte", false },
  { "AdrMon youthh",	nullptr,			"Jugendherbergen", false },
  { "Town",		"City (Small)",		"Orte", false },
  { nullptr,			nullptr,			nullptr, false }
};

static void
bcr_handle_icon_str(const char* str, Waypoint* wpt)
{
  wpt->icon_descr = BCR_DEF_MPS_ICON;

  for (bcr_icon_mapping_t* m = bcr_icon_mapping; (m->bcr_name); m++) {
    if (case_ignore_strcmp(str, m->bcr_name) == 0) {
      if (m->symbol_DE == nullptr) {
        if (! m->warned) {
          m->warned = true;
          warning(MYNAME ": Unknown icon \"%s\" found. Please report.\n", str);
        }
        return;
      }
      wpt->description = m->symbol_DE;
      if (m->mps_name != nullptr) {
        int nr = gt_find_icon_number_from_desc(m->mps_name, MAPSOURCE);
        wpt->icon_descr = gt_find_desc_from_icon_number(nr, MAPSOURCE);
      }
      return;
    }
  }
}

static const char*
get_bcr_icon_from_icon_descr(const QString& icon_descr)
{
  const char* result = BCR_DEF_ICON;

  if (!icon_descr.isNull()) {
    for (bcr_icon_mapping_t* m = bcr_icon_mapping; (m->bcr_name); m++) {
      if (! m->mps_name) {
        continue;
      }
      if (icon_descr.compare(m->mps_name, Qt::CaseInsensitive) == 0) {
        result = m->bcr_name;
        break;
      }
    }
  }
  return result;
}

static void
bcr_init_radius()
{
  if (radius_opt != nullptr) {			/* preinitialize the earth radius */
    radius = atof(radius_opt);
    if (radius <= 0) {
      fatal(MYNAME ": Sorry, the radius should be greater than zero!\n");
    }
  } else {
    radius = (double)R_EARTH;
  }

  if (global_opts.verbose_status > 0) {
    printf(MYNAME ": We calculate with radius %f meters.\n", radius);
  }
}

static void
bcr_rd_init(const QString& fname)
{
  ini = inifile_init(fname, MYNAME);
  bcr_init_radius();
}

static void
bcr_rd_deinit()
{
  inifile_done(ini);
}

/* ------------------------------------------------------------*/

static void
bcr_create_waypts_from_route(route_head* route)
{
  foreach (const Waypoint* wpt, route->waypoint_list) {
    waypt_add(new Waypoint(*wpt));
  }
}

static void
bcr_wgs84_to_mercator(const double lat, const double lon, int* north, int* east)
{
  double N = log(tan(lat * M_PI / 360 + M_PI / 4)) * radius;
  double E = lon * radius * M_PI / 180.0;

  if (lat > 0) {
    N += 0.500000000001;  /* we go from double to integer */
  } else {
    N -= 0.500000000001;  /* it's time to round a little bit */
  }
  if (lon > 0) {
    E += 0.500000000001;
  } else {
    E -= 0.500000000001;
  }

  *north = N;
  *east = E;
}

static void
bcr_mercator_to_wgs84(const int north, const int east, double* lat, double* lon)
{
  *lat = 2 * (atan(exp(north / radius)) - M_PI / 4) / M_PI * 180.0;
  *lon = (double)east * 180.0 / (radius * M_PI);
}

/* ------------------------------------------------------------- */

static void
bcr_data_read()
{
  QString str;

  route_head* route = route_head_alloc();

  str = inifile_readstr(ini, "client", "routename");
  if (!str.isNull()) {
    route->rte_name = str;
  }

  route_add_head(route);

  for (int index = 1; index > 0; index ++) {

    char station[32];
    QString str;
    int mlat, mlon;		/* mercator data */

    snprintf(station, sizeof(station), "STATION%d", index);
    str = inifile_readstr(ini, "coordinates", station);
    if (str.isNull()) {
      break;
    }

    if (2 != sscanf(CSTR(str), "%d,%d", &mlon, &mlat)) {
      fatal(MYNAME ": structure error at %s (Coordinates)!\n", station);
    }

    Waypoint* wpt = new Waypoint;

    wpt->shortname = station;
    bcr_mercator_to_wgs84(mlat, mlon, &wpt->latitude, &wpt->longitude);

    str = inifile_readstr(ini, "client", station);
    if (!str.isNull()) {
      int cx = str.indexOf(',');
      if (cx < 0) {
        fatal(MYNAME ": structure error at %s (Client)!\n", station);
      }
      bcr_handle_icon_str(CSTR(str.left(cx)), wpt);
    }

    str = inifile_readstr(ini, "description", station);
    if (!str.isNull()) {
      QString note = str.section(',', 0, 0);
      if (!note.isEmpty()) {
        wpt->notes = note;
      }
      QString shortname = str.section(',', 1, 1);
      if (!shortname.isEmpty()) {
        wpt->shortname = shortname;
      }
    }

    route_add_wpt(route, wpt);
  }

  /* remove empty route */
  if (route->rte_waypt_ct == 0) {
    route_del_head(route);
  } else {
    bcr_create_waypts_from_route(route);
  }
}

/* %%% bcr write support %%% ----------------------------------- */

static void
bcr_wr_init(const QString& fname)
{
  fout = gbfopen(fname, "wb", MYNAME);
  bcr_init_radius();
}

static void
bcr_wr_deinit()
{
  gbfclose(fout);
}

static void
bcr_route_trailer(const route_head*)
{
}

static void
bcr_write_wpt(const Waypoint*)
{
}

static void bcr_write_line(gbfile* fout, const QString& key, const int* index, const QString& value)
{
  if (value.isEmpty()) {			/* this is mostly used in the world of windows */
    /* so we respectfully add a CR/LF on each line */
    gbfprintf(fout, "%s\r\n", CSTR(key));
  } else {
    char* tmp = (value != nullptr) ? xstrdup(value) : xstrdup("");
    if (index != nullptr) {
      gbfprintf(fout, "%s%d=%s\r\n", CSTR(key), *index, tmp);
    } else {
      gbfprintf(fout, "%s=%s\r\n", CSTR(key), tmp);
    }
    xfree(tmp);
  }
}

static void
bcr_route_header(const route_head* route)
{
  int north, east, nmax, emin;

  curr_rte_num++;
  if (curr_rte_num != target_rte_num) {
    return;
  }

  bcr_write_line(fout, "[CLIENT]", nullptr, nullptr);			/* client section */
  bcr_write_line(fout, "REQUEST", nullptr, "TRUE");

  QString sout = route->rte_name;
  if (rtename_opt != nullptr) {
    sout = rtename_opt;
  }
  if (sout != nullptr) {
    bcr_write_line(fout, "ROUTENAME", nullptr, sout);
  } else {
    bcr_write_line(fout, "ROUTENAME", nullptr, "Route");
  }

  bcr_write_line(fout, "DESCRIPTIONLINES", nullptr, "0");

  int i = 0;
  foreach (const Waypoint* wpt, route->waypoint_list) {

    i++;

    const char* icon = get_bcr_icon_from_icon_descr(wpt->icon_descr);

    sout = QString("%1,%2").arg(icon).arg(BCR_UNKNOWN,10);
    bcr_write_line(fout, "STATION", &i, sout);
  }

  bcr_write_line(fout, "[COORDINATES]", nullptr, nullptr);		/* coords section */

  int nmin = emin = (1<<30);
  int emax = nmax = -nmin;

  i = 0;
  foreach (const Waypoint* wpt, route->waypoint_list) {
    i++;

    bcr_wgs84_to_mercator(wpt->latitude, wpt->longitude, &north, &east);

    if (north > nmax) {
      nmax = north;
    }
    if (east > emax) {
      emax = east;
    }
    if (north < nmin) {
      nmin = north;
    }
    if (east < emin) {
      emin = east;
    }

    sout = QString::number(east) + "," + QString::number(north);
    bcr_write_line(fout, "STATION", &i, sout);
  }

  bcr_write_line(fout, "[DESCRIPTION]", nullptr, nullptr);		/* descr. section */

  i = 0;
  foreach (const Waypoint* wpt, route->waypoint_list) {
    QString s2;

    i++;
    QString s1 = wpt->notes;
    if (s1.isEmpty()) {
      s1 = wpt->description;
    }

    if (prefer_shortnames_opt || (s1.isEmpty())) {
      s2 = s1;
      s1 = wpt->shortname;
    } else {
      s2 = wpt->shortname;
    }

    if (s1.isEmpty()) {
      s1 = QString();
    } else {
      s1 = csv_stringclean(s1, ",\t\r\n");
    }
    if (s2.isEmpty()) {
      s2 = QString();
    } else {
      s2 = csv_stringclean(s2, ",\t\r\n");
    }

    if (sout.isEmpty()) {
      sout = QString("%1,%2,@,0").arg(s1, s1);
    } else {
      sout = QString("%1,%2,@,0").arg(s1, s2);
    }

    bcr_write_line(fout, "STATION", &i, sout);
  }

  bcr_write_line(fout, "[ROUTE]", nullptr, nullptr);		/* route section */

  sout = QString::number(emin) + "," +
         QString::number(nmax) + "," +
         QString::number(emax) + "," +
         QString::number(nmin);
  bcr_write_line(fout, "ROUTERECT", nullptr, sout);
}

static void
bcr_data_write()
{
  target_rte_num = 1;

  if (rtenum_opt != nullptr) {
    target_rte_num = atoi(rtenum_opt);
    if (((unsigned)target_rte_num > route_count()) || (target_rte_num < 1))
      fatal(MYNAME ": invalid route number %d (1..%d))!\n",
            target_rte_num, route_count());
  }
  curr_rte_num = 0;
  route_disp_all(bcr_route_header, bcr_route_trailer, bcr_write_wpt);
}

ff_vecs_t bcr_vecs = {
  ff_type_file,
  { ff_cap_none, ff_cap_none, (ff_cap)(ff_cap_read | ff_cap_write)},
  bcr_rd_init,
  bcr_wr_init,
  bcr_rd_deinit,
  bcr_wr_deinit,
  bcr_data_read,
  bcr_data_write,
  nullptr,
  bcr_args,
  CET_CHARSET_MS_ANSI, 0,	/* CET-REVIEW */
  NULL_POS_OPS,
  nullptr
};
