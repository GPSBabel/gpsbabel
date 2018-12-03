/*

   Support for Raymarine Waypoint File (.rwf).

   Copyright (C) 2006,2007 Olaf Klein, o.b.klein@gpsbabel.org

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
    Known format limits:

    	Waypoint name: max. 16 characters
    	Route name:    max. 16 characters
	Routes:        max. 50 waypoints per route
	???:           the character set may be only a subset of std. ASCII

    History:

    	2006/10/30: Initial release (not yet in GPSBabel source tree)
    	2006/11/08:
	2007/03/17: Remove GUIDs from writer (not really valid)
	            Fix "PredictedTwa" output
		    Initialize location with "My Waypoints"
		    Change default value for RcCount and RelSet (now 0)
	2007/04/18: Limit route names also to 16 characters
		    Bug-fix - add missing comma (write_route_wpt_cb/items)
		    Change line feeds to fixed CRLF
		    Sort waypoints by name (not really needed, but nice)
		    Add some MapSource icon names to icon mappings
		    Remove unused id from icon table
*/

#include "defs.h"
#include "csv_util.h"
#include "inifile.h"

#include <QtCore/QString>
#include <cctype>
#include <cstdio>
#include <cstdlib>

typedef unsigned long long guid_t;

static inifile_t* fin;
static gbfile* fout;
static Waypoint** waypt_table;
static short_handle hshort_wpt, hshort_rte;
static int waypt_table_sz, waypt_table_ct;
static int rte_index, rte_wpt_index;
static char* opt_location;

#define MYNAME "raymarine"

static
arglist_t raymarine_args[] = {
  { "location", &opt_location, "Default location", "My Waypoints", ARGTYPE_STRING, ARG_NOMINMAX , nullptr},
  ARG_TERMINATOR
};

/* from csv_util.c: convert excel time (days since 1900) to time_t and back again */

#define EXCEL_TO_TIMET(a) ((a - 25569.0) * 86400.0)
#define TIMET_TO_EXCEL(a) ((a / 86400.0) + 25569.0)

#define LINE_FEED "\r\n"

/* Bitmaps */

typedef struct {
  const char* name;
  const char* mps_name;
} raymarine_symbol_mapping_t;

static raymarine_symbol_mapping_t raymarine_symbols[] = {
  { /* 0 */  "Unknown Symbol 0", nullptr },
  { /* 1 */  "Unknown Symbol 1", nullptr },
  { /* 2 */  "Unknown Symbol 2", nullptr },
  { /* 3 */  "Red Square", nullptr },
  { /* 4 */  "Big Fish", nullptr },
  { /* 5 */  "Anchor", nullptr },
  { /* 6 */  "Smiley", "Contact, Smiley" },
  { /* 7 */  "Sad", nullptr },
  { /* 8 */  "Red Button", "Navaid, Red" },
  { /* 9 */  "Sailfish", nullptr },
  { /* 10 */ "Danger", "Skull and Crossbones" },
  { /* 11 */ "Attention", nullptr },
  { /* 12 */ "Black Square", nullptr },
  { /* 13 */ "Intl. Dive Flag", "Diver Down Flag 2" },
  { /* 14 */ "Vessel", "Marina" },
  { /* 15 */ "Lobster", nullptr },
  { /* 16 */ "Buoy", "Buoy, White" },
  { /* 17 */ "Exclamation", nullptr },
  { /* 18 */ "Red X", nullptr },
  { /* 19 */ "Check Mark", nullptr },
  { /* 20 */ "Black Plus", nullptr },
  { /* 21 */ "Black Cross", nullptr },
  { /* 22 */ "MOB", nullptr },
  { /* 23 */ "Billfish", nullptr },
  { /* 24 */ "Bottom Mark", nullptr },
  { /* 25 */ "Circle", "Circle, Red" },
  { /* 26 */ "Diamond", "Block, Red" },
  { /* 27 */ "Diamond Quarters", "Diamond, Red" },
  { /* 28 */ "U.S. Dive Flag", "Diver Down Flag 1" },
  { /* 29 */ "Dolphin", nullptr },
  { /* 30 */ "Few Fish", nullptr },
  { /* 31 */ "Multiple Fish", nullptr },
  { /* 32 */ "Many Fish", nullptr },
  { /* 33 */ "Single Fish", nullptr },
  { /* 34 */ "Small Fish", nullptr },
  { /* 35 */ "Marker", nullptr },
  { /* 36 */ "Cocktails", "Bar" },
  { /* 37 */ "Red Box Marker", nullptr },
  { /* 38 */ "Reef", nullptr },
  { /* 39 */ "Rocks", nullptr },
  { /* 40 */ "Fish School", nullptr },
  { /* 41 */ "Seaweed", "Weed Bed" },
  { /* 42 */ "Shark", nullptr },
  { /* 43 */ "Sportfisher", nullptr },
  { /* 44 */ "Swimmer", "Swimming Area" },
  { /* 45 */ "Top Mark", nullptr },
  { /* 46 */ "Trawler", nullptr },
  { /* 47 */ "Tree", nullptr },
  { /* 48 */ "Triangle", "Triangle, Red" },
  { /* 49 */ "Wreck", "Shipwreck" }
};

#define RAYMARINE_SYMBOL_CT  sizeof(raymarine_symbols) / sizeof(raymarine_symbol_mapping_t)
#define RAYMARINE_STD_SYMBOL 3

static int
find_symbol_num(const QString& descr)
{
  if (!descr.isNull()) {
    raymarine_symbol_mapping_t* a = &raymarine_symbols[0];

    for (unsigned int i = 0; i < RAYMARINE_SYMBOL_CT; i++, a++) {
      if (descr.compare(a->name, Qt::CaseInsensitive) == 0) {
        return i;
      }
      if (a->mps_name && (descr.compare(a->mps_name, Qt::CaseInsensitive) == 0)) {
        return i;
      }
    }
  }

  return RAYMARINE_STD_SYMBOL;
}

/* ============================================= */
/* %%%    R A Y M A R I N E   R E A D E R    %%% */
/* ============================================= */

static void
raymarine_rd_init(const QString& fname)
{
  fin = inifile_init(fname, MYNAME);
}

static void
raymarine_rd_done()
{
  inifile_done(fin);
}

static void
raymarine_read()
{
  /* Read all waypoints */

  for (unsigned int ix = 0; ix < 0x3FFF; ix++) {
    char sect[10];
    QString str, name, lat, lon;

    /* built section identifier */
    snprintf(sect, sizeof(sect), "Wp%d", ix);

    /* try to read our most expected values */
    name = inifile_readstr(fin, sect, "Name");
    if (name.isNull()) {
      break;
    }
    lat = inifile_readstr(fin, sect, "Lat");
    if (lat.isNull()) {
      break;
    }
    lon = inifile_readstr(fin, sect, "Long");
    if (lon.isNull()) {
      break;
    }

    Waypoint* wpt = new Waypoint;
    wpt->shortname = name;
    wpt->latitude = lat.toDouble();
    wpt->longitude = lon.toDouble();
    waypt_add(wpt);

    /* try to read optional values */
    str = inifile_readstr(fin, sect, "Notes");
    if (!str.isEmpty()) {
      wpt->notes = str;
    }
    str = inifile_readstr(fin, sect, "Time");
    if (!str.isEmpty()) {
      wpt->SetCreationTime(EXCEL_TO_TIMET(str.toDouble()));
    }
    str = inifile_readstr(fin, sect, "Bmp");
    if (!str.isEmpty()) {
      unsigned int symbol = str.toInt();

      if ((symbol < 3) && (symbol >= RAYMARINE_SYMBOL_CT)) {
        symbol = RAYMARINE_STD_SYMBOL;
      }
      wpt->icon_descr = raymarine_symbols[symbol].name;
    }
  }

  /* Read all routes */

  for (unsigned int rx = 0; rx < 0x3FFF; rx++) {
    char sect[10];
    QString name;

    snprintf(sect, sizeof(sect), "Rt%u", rx);
    name = inifile_readstr(fin, sect, "Name");
    if (name.isNull()) {
      break;
    }

    route_head* rte = route_head_alloc();
    rte->rte_name = name;
    route_add_head(rte);

    for (int wx = 0; wx < 0x3FFF; wx++) {
      char buff[32];

      snprintf(buff, sizeof(buff), "Mk%d", wx);
      QString str = inifile_readstr(fin, sect, buff);
      if (str.isEmpty()) {
        break;
      }

      Waypoint* wpt = find_waypt_by_name(str);
      if (wpt == nullptr)
        fatal(MYNAME ": No associated waypoint for route point %s (Route %s)!\n",
              qPrintable(str), qPrintable(rte->rte_name));

      route_add_wpt(rte, new Waypoint(*wpt));
    }
  }
}

/* ============================================= */
/* %%%    R A Y M A R I N E   W R I T E R    %%% */
/* ============================================= */

/* make waypoint shortnames unique */

static char
same_points(const Waypoint* A, const Waypoint* B)
{
  return ( /* !!! We are case-sensitive !!! */
           (A->shortname == B->shortname) &&
           (A->latitude == B->latitude) &&
           (A->longitude == B->longitude));
}

static void
register_waypt(const Waypoint* ref, const char)
{
  Waypoint* wpt = const_cast<Waypoint*>(ref);

  for (int i = 0; i < waypt_table_ct; i++) {
    Waypoint* cmp = waypt_table[i];

    if (same_points(wpt, cmp)) {
      wpt->extra_data = cmp->extra_data;
      return;
    }
  }

  if (waypt_table_ct >= waypt_table_sz) {
    waypt_table_sz += 32;
    if (waypt_table) {
      waypt_table = (Waypoint**) xrealloc(waypt_table, waypt_table_sz * sizeof(wpt));
    } else {
      waypt_table = (Waypoint**) xmalloc(waypt_table_sz * sizeof(wpt));
    }
  }

  wpt->extra_data = (void*)mkshort(hshort_wpt, CSTRc(wpt->shortname));

  waypt_table[waypt_table_ct] = wpt;
  waypt_table_ct++;
}

static void
enum_waypt_cb(const Waypoint* wpt)
{
  register_waypt(wpt, 0);
}

static void
enum_rtept_cb(const Waypoint* wpt)
{
  register_waypt(wpt, 1);
}

static int
qsort_cb(const void* a, const void* b)
{
  const Waypoint* wa = *(Waypoint**)a;
  const Waypoint* wb = *(Waypoint**)b;
  return wa->shortname.compare(wb->shortname);
}

static void
write_waypoint(gbfile* fout, const Waypoint* wpt, const int waypt_no, const char* location)
{
  QString notes = wpt->notes;
  if (notes == nullptr) {
    notes = wpt->description;
    if (notes == nullptr) {
      notes = "";
    }
  }
  notes = csv_stringclean(notes, LINE_FEED);
  double time = wpt->creation_time.isValid() ? TIMET_TO_EXCEL(wpt->GetCreationTime().toTime_t()) : TIMET_TO_EXCEL(gpsbabel_time);
  char* name = (char*)wpt->extra_data;

  gbfprintf(fout, "[Wp%d]" LINE_FEED
            "Loc=%s" LINE_FEED
            "Name=%s" LINE_FEED
            "Lat=%.15f" LINE_FEED
            "Long=%.15f" LINE_FEED,
            waypt_no, location, name, wpt->latitude, wpt->longitude
           );
  gbfprintf(fout, "Rng=%.15f" LINE_FEED
            "Bear=%.15f" LINE_FEED
            "Bmp=%d" LINE_FEED
            "Fixed=1" LINE_FEED
            "Locked=0" LINE_FEED
            "Notes=%s" LINE_FEED,
            0.0, 0.0,
            find_symbol_num(wpt->icon_descr),
            CSTR(notes)
           );
  gbfprintf(fout, "Rel=" LINE_FEED
            "RelSet=0" LINE_FEED
            "RcCount=0" LINE_FEED
            "RcRadius=%.15f" LINE_FEED
            "Show=1" LINE_FEED
            "RcShow=0" LINE_FEED
            "SeaTemp=%.15f" LINE_FEED
            "Depth=%.15f" LINE_FEED
            "Time=%.10f00000" LINE_FEED,
            0.0, -32678.0, 65535.0, time
           );
}

static void
write_route_head_cb(const route_head* rte)
{
  QString name = rte->rte_name;
  if (name.isEmpty()) {
    name=QString("Route%1").arg(rte_index);
  }
  name = mkshort(hshort_rte, name);
  gbfprintf(fout, "[Rt%d]" LINE_FEED
            "Name=%s" LINE_FEED
            "Visible=1" LINE_FEED,
            rte_index,
            CSTR(name)
           );
  rte_index++;
  rte_wpt_index = 0;
}

static void
write_route_wpt_cb(const Waypoint* wpt)
{
  static const char* items[] = {
    "Cog",
    "Eta",
    "Length",
    "PredictedDrift",
    "PredictedSet",
    "PredictedSog",
    "PredictedTime",
    "PredictedTwa",
    "PredictedTwd",
    "PredictedTws"
  };

  gbfprintf(fout, "Mk%d=%s" LINE_FEED, rte_wpt_index, (char*)wpt->extra_data);
  for (auto & item : items) {
    gbfprintf(fout, "%s%d=%.15f" LINE_FEED, item, rte_wpt_index, 0.0);
  }

  rte_wpt_index++;
}

static void
enum_route_hdr_cb(const route_head* rte)
{
  is_fatal(rte->rte_waypt_ct > 50,
           MYNAME ": Routes with more than 50 points are not supported by Raymarine!");
}

static short_handle
raymarine_new_short_handle()
{
  short_handle res = mkshort_new_handle();

  setshort_length(res, 16);
  setshort_badchars(res, ",");
  setshort_mustupper(res, 0);
  setshort_mustuniq(res, 1);
  setshort_whitespace_ok(res, 1);
  setshort_repeating_whitespace_ok(res, 1);

  return res;
}

static void
raymarine_wr_init(const QString& fname)
{
  fout = gbfopen(fname, "wb", MYNAME);

  hshort_wpt = raymarine_new_short_handle();
  hshort_rte = raymarine_new_short_handle();
}

static void
raymarine_wr_done()
{
  mkshort_del_handle(&hshort_wpt);
  mkshort_del_handle(&hshort_rte);

  gbfclose(fout);
}

static void
raymarine_write()
{
  int i;

  waypt_table_sz = 0;
  waypt_table_ct = 0;
  waypt_table = nullptr;

  /* enumerate all possible waypoints */
  waypt_disp_all(enum_waypt_cb);
  route_disp_all(enum_route_hdr_cb, nullptr, enum_rtept_cb);

  if (waypt_table_ct == 0) {
    return;
  }

  qsort(waypt_table, waypt_table_ct, sizeof(*waypt_table), qsort_cb);

  /* write out waypoint summary */
  for (i = 0; i < waypt_table_ct; i++) {
    Waypoint* wpt = waypt_table[i];
    write_waypoint(fout, wpt, i, opt_location);
  }

  /* write out all routes with their waypoints */
  rte_index = 0;
  route_disp_all(write_route_head_cb, nullptr, write_route_wpt_cb);

  /* release local used data */
  for (i = 0; i < waypt_table_ct; i++) {
    Waypoint* wpt = waypt_table[i];
    xfree(wpt->extra_data);
    wpt->extra_data = nullptr;
  }
  xfree(waypt_table);
}

/* ================================================== */
/* %%%    M O D U L E   R E G I S T R A T I O N   %%% */
/* ================================================== */

ff_vecs_t raymarine_vecs = {
  ff_type_file,
  {
    (ff_cap)(ff_cap_read | ff_cap_write)	/* waypoints */,
    ff_cap_none 			/* tracks */,
    (ff_cap)(ff_cap_read | ff_cap_write) 	/* routes */,
  },
  raymarine_rd_init,
  raymarine_wr_init,
  raymarine_rd_done,
  raymarine_wr_done,
  raymarine_read,
  raymarine_write,
  nullptr,
  raymarine_args,
  CET_CHARSET_ASCII, 0	/* should we force this to 1 ? */
  , NULL_POS_OPS,
  nullptr
};
