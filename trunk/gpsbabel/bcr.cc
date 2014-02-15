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

#include "defs.h"
#include "csv_util.h"
#include "garmin_tables.h"
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "cet_util.h"
#include "inifile.h"

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
static char* filename;
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
    NULL, ARGTYPE_INT, "1", NULL
  },
  {
    "name", &rtename_opt, "New name for the route",
    NULL, ARGTYPE_STRING, ARG_NOMINMAX
  },
  {
    "radius", &radius_opt, "Radius of our big earth (default 6371000 meters)", "6371000",
    ARGTYPE_FLOAT, ARG_NOMINMAX
  },
  {
    "prefer_shortnames", &prefer_shortnames_opt, "Use shortname instead of description",
    NULL, ARGTYPE_BOOL, ARG_NOMINMAX
  },
  ARG_TERMINATOR
};

typedef struct {
  const char* bcr_name;
  const char* mps_name;
  const char* symbol_DE;
  int  warned;
} bcr_icon_mapping_t;

static
bcr_icon_mapping_t bcr_icon_mapping[] = {
  { BCR_DEF_ICON,		BCR_DEF_MPS_ICON, 	BCR_DEF_ICON },
  { "",			BCR_DEF_MPS_ICON, 	"Eigene Adressen" },
  { "AdrMon alpen",	"Summit",		"Pass-Strassen" },
  { "AdrMon bauern",	NULL,			"Bauern- und Biohoefe" },
  { "AdrMon cmpngs",	"Campground",		"Campingplaetzte" },
  { "AdrMon p_aeu",	"Scenic Area",		"Sehenswertes" },
  { "AdrMon p_beu",	"Gas Station",		"Tanken" },
  { "AdrMon p_deu",	"Parking Area",		"Parken" },
  { "AdrMon p_feu",	"Restaurant",		"Gastro" },
  { "AdrMon p_geu",	"Museum",		"Freizeit" },
  { "AdrMon p_heu",	"Gas Station",		"Tankstellen" },
  { "AdrMon p_keu",	NULL,			"Faehrverbindungen" },
  { "AdrMon p_leu",	NULL,			"Grenzuebergaenge" },
  { "AdrMon p_teu",	NULL,			"Wein- und Sektgueter" },
  { "AdrMon RUINEN",	"Ghost Town",		"Burgen und Schloesser" },
  { "AdrMon NFHAUS",	"Residence",		"Naturfreundehaeuser" },
  { "AdrMon racing",	"Bike Trail",		"Rennstrecken" },
  { "AdrMon TNKRST",	"Bar",			"Tankraststaetten" },
  { "AdrMon tpclub",	"Contact, Biker",	"Motorrad-Clubs" },
  { "AdrMon tpequ",	NULL,			"Motorrad-Equipment" },
  { "AdrMon tphot",	"Hotel",		"Motorrad-Hotels" },
  { "AdrMon tpmh",	NULL,			"Motorradhaendler" },
  { "AdrMon tpss",	"Restricted Area",	"Sperrungen" },
  { "AdrMon tpsw",	"Scenic Area",		"Sehenswertes" },
  { "AdrMon tptref",	NULL,			"Treffpunkte" },
  { "AdrMon VORTE",	"Information",		"Ortsinformationen" },
  { "AdrMon WEBCAM",	NULL,			"WebCam-Standorte" },
  { "AdrMon youthh",	NULL,			"Jugendherbergen" },
  { "Town",		"City (Small)",		"Orte" },
  { NULL,			NULL,			NULL, 0 }
};

static void
bcr_handle_icon_str(const char* str, Waypoint* wpt)
{
  bcr_icon_mapping_t* m;

  wpt->icon_descr = BCR_DEF_MPS_ICON;

  for (m = bcr_icon_mapping; (m->bcr_name); m++) {
    if (case_ignore_strcmp(str, m->bcr_name) == 0) {
      int nr;

      if (m->symbol_DE == NULL) {
        if (! m->warned) {
          m->warned = 1;
          warning(MYNAME ": Unknown icon \"%s\" found. Please report.\n", str);
        }
        return;
      }
      wpt->description = m->symbol_DE;
      if (m->mps_name != NULL) {
        nr = gt_find_icon_number_from_desc(m->mps_name, MAPSOURCE);
        wpt->icon_descr = gt_find_desc_from_icon_number(nr, MAPSOURCE, NULL);
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
    bcr_icon_mapping_t* m;

    for (m = bcr_icon_mapping; (m->bcr_name); m++) {
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
bcr_init_radius(void)
{
  if (radius_opt != NULL) {			/* preinitialize the earth radius */
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
bcr_rd_init(const char* fname)
{
  filename = xstrdup(fname);
  ini = inifile_init(fname, MYNAME);
  if (ini->unicode) {
    cet_convert_init(CET_CHARSET_UTF8, 1);
  }
  bcr_init_radius();
}

static void
bcr_rd_deinit(void)
{
  inifile_done(ini);
  xfree(filename);
}

/* ------------------------------------------------------------*/

static void
bcr_create_waypts_from_route(route_head* route)
{
  Waypoint* wpt;
  queue* elem, *tmp;

  QUEUE_FOR_EACH(&route->waypoint_list, elem, tmp) {
    wpt = new Waypoint(*(Waypoint*) elem);
    waypt_add(wpt);
  }
}

static void
bcr_wgs84_to_mercator(const double lat, const double lon, int* north, int* east)
{
  double N, E;

  N = log(tan(lat * M_PI / 360 + M_PI / 4)) * radius;
  E = lon * radius * M_PI / (double)180;

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

void
bcr_mercator_to_wgs84(const int north, const int east, double* lat, double* lon)
{
  *lat = 2 * (atan(exp(north / radius)) - M_PI / 4) / M_PI * (double)180;
  *lon = (double)east * (double)180 / (radius * M_PI);
}

/* ------------------------------------------------------------- */

static void
bcr_data_read(void)
{
  int index;
  char* str;
  route_head* route;

  route = route_head_alloc();

  if ((str = inifile_readstr(ini, "client", "routename"))) {
    route->rte_name = str;
  }

  route_add_head(route);

  for (index = 1; index > 0; index ++) {

    char station[32];
    char* str;
    int mlat, mlon;		/* mercator data */
    Waypoint* wpt;

    snprintf(station, sizeof(station), "STATION%d", index);
    if (NULL == (str = inifile_readstr(ini, "coordinates", station))) {
      break;
    }

    if (2 != sscanf(str, "%d,%d", &mlon, &mlat)) {
      fatal(MYNAME ": structure error at %s (Coordinates)!\n", station);
    }

    wpt = new Waypoint;

    wpt->shortname = station;
    bcr_mercator_to_wgs84(mlat, mlon, &wpt->latitude, &wpt->longitude);

    if (NULL != (str = inifile_readstr(ini, "client", station))) {
      char* cx;

      cx = strchr(str, ',');
      if (cx == NULL) {
        fatal(MYNAME ": structure error at %s (Client)!\n", station);
      }
      *cx++ = '\0';
      bcr_handle_icon_str(str, wpt);
    }

    if (NULL != (str = inifile_readstr(ini, "description", station))) {
      char* c;

      c = strchr(str, ',');
      if (c != NULL) {
        *c = '\0';
      }
      if (*str) {
        wpt->notes = str;
      }
      if ((str = c)) {
        str++;
        c = strchr(str, ',');
        if (c != NULL) {
          *c = '\0';
        }
        if (*str) {
          wpt->shortname = str;
        }
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
bcr_wr_init(const char* fname)
{
  filename = xstrdup(fname);
  fout = gbfopen(fname, "wb", MYNAME);
  bcr_init_radius();
}

static void
bcr_wr_deinit(void)
{
  gbfclose(fout);
  xfree(filename);
}

static void
bcr_route_trailer(const route_head* rte)
{
}

static void
bcr_write_wpt(const Waypoint* wpt)
{
}

void bcr_write_line(gbfile* fout, const QString& key, int* index, const QString& value)
{
  if (value.isEmpty()) {			/* this is mostly used in the world of windows */
    /* so we respectfully add a CR/LF on each line */
    gbfprintf(fout, "%s\r\n", CSTR(key));
  } else {
    char* tmp;

    tmp = (value != NULL) ? xstrdup(value) : xstrdup("");
    if (index != NULL) {
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
  queue* elem, *tmp;
  Waypoint* wpt;
  QString sout;
  int i, north, east, nmin, nmax, emin, emax;

  curr_rte_num++;
  if (curr_rte_num != target_rte_num) {
    return;
  }

  bcr_write_line(fout, "[CLIENT]", NULL, NULL);			/* client section */
  bcr_write_line(fout, "REQUEST", NULL, "TRUE");

  sout = route->rte_name;
  if (rtename_opt != 0) {
    sout = rtename_opt;
  }
  if (sout != NULL) {
    bcr_write_line(fout, "ROUTENAME", NULL, sout);
  } else {
    bcr_write_line(fout, "ROUTENAME", NULL, "Route");
  }

  bcr_write_line(fout, "DESCRIPTIONLINES", NULL, "0");

  i = 0;
  QUEUE_FOR_EACH(&route->waypoint_list, elem, tmp) {
    const char* icon;
    Waypoint* wpt = (Waypoint*) elem;

    i++;

    icon = get_bcr_icon_from_icon_descr(wpt->icon_descr);

    sout = QString("%1,%2").arg(icon).arg(BCR_UNKNOWN,10);
    bcr_write_line(fout, "STATION", &i, sout);
  }

  bcr_write_line(fout, "[COORDINATES]", NULL, NULL);		/* coords section */

  nmin = emin = (1<<30);
  emax = nmax = -nmin;

  i = 0;
  QUEUE_FOR_EACH(&route->waypoint_list, elem, tmp) {
    i++;
    wpt = (Waypoint*) elem;

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

  bcr_write_line(fout, "[DESCRIPTION]", NULL, NULL);		/* descr. section */

  i = 0;
  QUEUE_FOR_EACH(&route->waypoint_list, elem, tmp) {
    QString s1, s2;

    i++;
    wpt = (Waypoint*) elem;
    s1 = wpt->notes;
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
      sout = QString("%1,%2,@,0").arg(s1).arg(s1);
    } else {
      sout = QString("%1,%2,@,0").arg(s1).arg(s2);
    }

    bcr_write_line(fout, "STATION", &i, sout);
  }

  bcr_write_line(fout, "[ROUTE]", NULL, NULL);		/* route section */

//  xasprintf(&sout, "%d,%d,%d,%d", emin, nmax, emax, nmin);
  sout = QString::number(emin) + "," +
         QString::number(nmax) + "," +
         QString::number(emax) + "," +
         QString::number(nmin);
  bcr_write_line(fout, "ROUTERECT", NULL, sout);
}

static void
bcr_data_write(void)
{
  target_rte_num = 1;

  if (rtenum_opt != NULL) {
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
  NULL,
  bcr_args,
  CET_CHARSET_MS_ANSI, 0	/* CET-REVIEW */
};
