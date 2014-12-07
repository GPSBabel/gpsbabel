/*

    Support for "GeoGrid Viewer ascii overlay files".

    Copyright (C) 2008 Olaf Klein (o.b.klein@gpsbabel.org).

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

#include "defs.h"
#include "inifile.h"
#include "grtcirc.h"

#include <cmath>

#define MYNAME "ggv_ovl"

static
arglist_t ggv_ovl_args[] = {
  ARG_TERMINATOR
};

typedef enum {
  OVL_SYMBOL_BITMAP = 1,
  OVL_SYMBOL_TEXT,
  OVL_SYMBOL_LINE,
  OVL_SYMBOL_POLYGON,
  OVL_SYMBOL_RECTANGLE,
  OVL_SYMBOL_CIRCLE,
  OVL_SYMBOL_TRIANGLE
} OVL_SYMBOL_TYP;

typedef enum {
  OVL_COLOR_RED = 1,	/* = 1 */
  OVL_COLOR_LIME,		/* = 2 */
  OVL_COLOR_BLUE,		/* = 3 */
  OVL_COLOR_YELLOW,	/* = 4 */
  OVL_COLOR_BLACK,	/* = 5 */
  OVL_COLOR_WHITE,	/* = 6 */
  OVL_COLOR_7,		/* = 7 (draws only a simple line) */
  OVL_COLOR_FUCHSIA,	/* = 8 */
  OVL_COLOR_AQUA,		/* = 9 */
} OVL_COLOR_TYP;

/* some hints:
		# "col":   color
		# "group": 1 means NO GROUP
		# "size":  size in pixels PLUS 100
		# "with":
		# "zoom":
		# "art":   line-style
 */
static inifile_t* inifile;
static gbfile* fout;

static int symbol_ct;	/* Number of symbols written */
static int group_ct;	/* Group number during write */
static int track_ct, route_ct;
static bounds all_bounds;
static OVL_COLOR_TYP color;

/*******************************************************************************
* %%%        global callbacks called by gpsbabel main process              %%% *
*******************************************************************************/

static void
ggv_ovl_rd_init(const char* fname)
{
  inifile = inifile_init(fname, MYNAME);
  if (inifile->unicode) {
    cet_convert_init(CET_CHARSET_UTF8, 1);
  }

  route_ct = 0;
  track_ct = 0;
}

static void
ggv_ovl_rd_deinit(void)
{
  inifile_done(inifile);
}

static void
ggv_ovl_read(void)
{
  int symbols;
  int i;

  symbols = inifile_readint_def(inifile, "Overlay", "Symbols", -1);

  for (i = 1; i <= symbols; i++) {
    int points;
    OVL_SYMBOL_TYP type;
    char symbol[32];

    snprintf(symbol, sizeof(symbol), "Symbol %d", i);

    type = (OVL_SYMBOL_TYP) inifile_readint_def(inifile, symbol, "Typ", 0);
    points = inifile_readint_def(inifile, symbol, "Punkte", -1);

    switch (type) {

      char coord[32];
      Waypoint* wpt;
      char* cx;
      int group;

    case OVL_SYMBOL_LINE:
    case OVL_SYMBOL_POLYGON:

      if (!inifile_readint(inifile, symbol, "Group", &group)) {
        group = -1;
      }

      if (points > 0) {
        int j;
        route_head* rte, *trk;

        rte = trk = route_head_alloc();
        if (group > 1) {
          route_add_head(rte);
          route_ct++;
            trk->rte_name = QString("Route %1").arg(route_ct);
        } else {
          track_add_head(trk);
          track_ct++;
            trk->rte_name = QString("Track %1").arg(track_ct);
        }

        for (j = 0; j < points; j++) {

          wpt = new Waypoint;

          snprintf(coord, sizeof(coord), "YKoord%d", j);
          if ((cx = inifile_readstr(inifile, symbol, coord))) {
            wpt->latitude = atof(cx);
          } else {
            continue;
          }

          snprintf(coord, sizeof(coord), "XKoord%d", j);
          if ((cx = inifile_readstr(inifile, symbol, coord))) {
            wpt->longitude = atof(cx);
          } else {
            continue;
          }

          if (group > 1) {
            route_add_wpt(rte, wpt);
          } else {
            track_add_wpt(trk, wpt);
          }
        }
      }
      break;

    case OVL_SYMBOL_CIRCLE:
    case OVL_SYMBOL_TRIANGLE:

      wpt = new Waypoint;
      wpt->shortname = symbol;

      if ((cx = inifile_readstr(inifile, symbol, "YKoord"))) {
        wpt->latitude = atof(cx);
      } else {
        continue;
      }
      if ((cx = inifile_readstr(inifile, symbol, "XKoord"))) {
        wpt->longitude = atof(cx);
      } else {
        continue;
      }

      waypt_add(wpt);
      break;

    case OVL_SYMBOL_BITMAP:
    case OVL_SYMBOL_TEXT:
    case OVL_SYMBOL_RECTANGLE:
      break;
    }
  }
}

/**************************************************************************/

/* prototypes used in main functions */

static void waypt_disp_cb(const Waypoint* wpt);
static void track_disp_cb(const route_head* trk);
static void route_disp_cb(const route_head* rte);
static void write_bounds(void);
static void draw_symbol_basics(const OVL_SYMBOL_TYP typ, const int art, const OVL_COLOR_TYP color, const Waypoint* wpt);
static int get_direction(const Waypoint* A, const Waypoint* B);
// static void draw_symbol_text(const char *text, const waypoint *reference);

/* -----------------------------------------------------------------------*/

static void
ggv_ovl_wr_init(const char* fname)
{
  fout = gbfopen(fname, "w", MYNAME);

  symbol_ct = 0;
}

static void
ggv_ovl_wr_deinit(void)
{
  gbfclose(fout);
}

static void
ggv_ovl_write(void)
{
  group_ct = 1;	/* tracks are not grouped */
  color = OVL_COLOR_FUCHSIA;
  track_disp_all(track_disp_cb, NULL, NULL);

  group_ct++;
  color = OVL_COLOR_AQUA;
  route_disp_all(route_disp_cb, NULL, NULL);

  group_ct++;
  color = OVL_COLOR_LIME;
  waypt_disp_all(waypt_disp_cb);

  gbfprintf(fout, "[Overlay]\n");
  gbfprintf(fout, "Symbols=%d\n", symbol_ct);
  gbfprintf(fout, "[MapLage]\n");
  gbfprintf(fout, "MapName=Bundesrepublik 1:1 Mio\n");
  gbfprintf(fout, "DimmFc=100\n");
  gbfprintf(fout, "ZoomFc=100\n");
  write_bounds();
  gbfprintf(fout, "RefOn=0\n");	/* no reference point */
}

/**************************************************************************/

static void
waypt_disp_cb(const Waypoint* wpt)
{
  draw_symbol_basics(OVL_SYMBOL_CIRCLE, 1, color, wpt);
  gbfprintf(fout, "Width=20\n");
  gbfprintf(fout, "Height=20\n");
  gbfprintf(fout, "Dir=100\n");
  gbfprintf(fout, "Zoom=1\n");
  gbfprintf(fout, "Size=102\n");
  gbfprintf(fout, "Area=2\n");
//	draw_symbol_text(wpt->shortname, wpt);
}

/* -----------------------------------------------------------------------*/

static void
track_disp_cb(const route_head* trk)
{
  int i;
  queue* elem, *tmp;
  int waypt_ct = trk->rte_waypt_ct;

  if (waypt_ct <= 0) {
    return;
  }

  draw_symbol_basics(OVL_SYMBOL_LINE, 1, color, NULL);

  gbfprintf(fout, "Zoom=1\n");
  gbfprintf(fout, "Size=105\n");
  gbfprintf(fout, "Punkte=%d\n", waypt_ct);

  i = 0;

  QUEUE_FOR_EACH(&(trk->waypoint_list), elem, tmp) {

    Waypoint* wpt = (Waypoint*) elem;

    gbfprintf(fout, "XKoord%d=%0.8f\n", i, wpt->longitude);
    gbfprintf(fout, "YKoord%d=%0.8f\n", i, wpt->latitude);

    i++;
  }
}

/* -----------------------------------------------------------------------*/

static void
route_disp_cb(const route_head* rte)
{
  int i;
  queue* elem, *tmp;
  Waypoint* prev;
  int waypt_ct = rte->rte_waypt_ct;

  if (waypt_ct <= 0) {
    return;
  }

  track_disp_cb(rte);	/* draw a line as tracks */

  color = OVL_COLOR_RED;

  i = 0;
  prev = NULL;

  QUEUE_FOR_EACH(&(rte->waypoint_list), elem, tmp) {

    Waypoint* wpt = (Waypoint*) elem;

    if (prev != NULL) {
      draw_symbol_basics(OVL_SYMBOL_TRIANGLE, 1, (OVL_COLOR_TYP)9 /* color */, prev);

      gbfprintf(fout, "Width=12\n");
      gbfprintf(fout, "Height=8\n");
      gbfprintf(fout, "Dir=%d\n", 100 + get_direction(prev, wpt));
      gbfprintf(fout, "Zoom=1\n");
      gbfprintf(fout, "Size=101\n");
      gbfprintf(fout, "Area=2\n");
    }

    i++;
    prev = wpt;
  }
}

/* -----------------------------------------------------------------------*/

static void
waypt_bound_calc(const Waypoint* waypointp)
{
  waypt_add_to_bounds(&all_bounds, waypointp);
}

static void
write_bounds(void)
{
  waypt_init_bounds(&all_bounds);

  waypt_disp_all(waypt_bound_calc);
  route_disp_all(NULL, NULL, waypt_bound_calc);
  track_disp_all(NULL, NULL, waypt_bound_calc);

  if (waypt_bounds_valid(&all_bounds)) {

    double cx = all_bounds.min_lat + ((all_bounds.max_lat - all_bounds.min_lat) / 2);
    double cy = all_bounds.min_lon + ((all_bounds.max_lon - all_bounds.min_lon) / 2);

    gbfprintf(fout, "CenterLat=%0.8f\n", cx);
    gbfprintf(fout, "CenterLong=%0.8f\n", cy);
  } else {
    gbfprintf(fout, "CenterLong=10.52374295\n");
    gbfprintf(fout, "CenterLat=52.26474445\n");
  }
}

static void
draw_symbol_basics(const OVL_SYMBOL_TYP typ, const int art, const OVL_COLOR_TYP color, const Waypoint* wpt)
{
  symbol_ct++;

  gbfprintf(fout, "[Symbol %d]\n", symbol_ct);
  gbfprintf(fout, "Typ=%d\n", typ);
  gbfprintf(fout, "Group=%d\n", group_ct);
  gbfprintf(fout, "Col=%d\n", color);
  if (art >= 0) {
    gbfprintf(fout, "Art=%d\n", art);
  }
  if (wpt) {
    gbfprintf(fout, "XKoord=%.8f\n", wpt->longitude);
    gbfprintf(fout, "YKoord=%.8f\n", wpt->latitude);
  }
}

/* the following code comes from first overlay module */

static int
get_direction(const Waypoint* A, const Waypoint* B)
{
  double lata, lona, latb, lonb;
  double dist, dir;
  int res;

  lata = RAD(A->latitude);
  lona = RAD(A->longitude);
  latb = RAD(B->latitude);
  lonb = RAD(B->longitude);

  dist = gcdist(lata, lona, latb, lonb);
  dir = acos((sin(latb) - sin(lata) * cos(dist)) / (cos(lata) * sin(dist)));
  if (lonb < lona) {
    dir = -dir;
  }
  res = (int) DEG(dir);
  res = 360 - (res + 270);
  if (res < 0) {
    res += 360;
  } else if (res > 360) {
    res -= 360.0;
  }

  return res;
}

#if 0
static void
draw_symbol_text(const char* text, const Waypoint* reference)
{
  Waypoint wpt;

  if ((reference == NULL) || (text == NULL)) {
    return;
  }
  if (*text == '\0') {
    return;
  }

  wpt = *reference;

  wpt.latitude = wpt.latitude + 0.015;
  wpt.longitude = wpt.longitude + 0.015;

  draw_symbol_basics(OVL_SYMBOL_TEXT, -1, OVL_COLOR_BLACK, &wpt);

  gbfprintf(fout, "Area=1\n");
  gbfprintf(fout, "Zoom=1\n");
  gbfprintf(fout, "Size=120\n");
  gbfprintf(fout, "Font=3\n");
  gbfprintf(fout, "Dir=100\n");
  gbfprintf(fout, "Text=%s\n", text);
}
#endif

/**************************************************************************/

ff_vecs_t ggv_ovl_vecs = {
  ff_type_file,
  FF_CAP_RW_ALL,
  ggv_ovl_rd_init,
  ggv_ovl_wr_init,
  ggv_ovl_rd_deinit,
  ggv_ovl_wr_deinit,
  ggv_ovl_read,
  ggv_ovl_write,
  NULL,
  ggv_ovl_args,
  CET_CHARSET_MS_ANSI, 0
};

/**************************************************************************/
