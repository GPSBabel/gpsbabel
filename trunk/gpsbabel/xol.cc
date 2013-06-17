/*

	Support for Swiss Map # (.xol) format

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
	Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111 USA

*/

#include "defs.h"
#include "xmlgeneric.h"
#include "jeeps/gpsmath.h"
#include "garmin_tables.h"

static waypoint *wpt;
static route_head *trk;
static gbfile *fout;
static int space;
static bounds all_bounds;
static short_handle short_h;

static arglist_t xol_args[] = {
  ARG_TERMINATOR
};

#define MYNAME "xol"

#if ! HAVE_LIBEXPAT
void
xol_rd_init(const char *fname)
{
  fatal(MYNAME ": This build excluded \"" MYNAME "\" support because expat was not installed.\n");
}

void
xol_read(void)
{
}

#else

static xg_callback	xol_shape, xol_shape_end;
static xg_callback	xol_waypt, xol_overlay;

#define XOL "/overlays/overlay"

static
xg_tag_mapping xol_map[] = {
  { xol_overlay,		cb_start,	XOL },
  { xol_shape,		cb_start,	XOL "/shapes/*shape" },
  { xol_shape_end,	cb_end,		XOL "/shapes/*shape" },
  { xol_waypt,		cb_start,	XOL "/shapes/shape/*points/point" },
  { NULL,	(xg_cb_type)0,		NULL }
};


static void
xol_overlay(const char *args, const char **attrv)
{
  const char **avp = &attrv[0];

  while (*avp) {
    if (strcmp(avp[0], "version") == 0) {
      if (strcmp(avp[1], "1.0") != 0) {
        fatal(MYNAME ": Unsupported version %s.\n", avp[1]);
      }
    }

    avp+=2;
  }
}

static void
xol_shape(const char *args, const char **attrv)
{
  const char **avp = &attrv[0];

  while (*avp) {
    if (strcmp(avp[0], "type") == 0) {
      if (strcmp(avp[1], "waypoint") == 0) {
        wpt = waypt_new();
      } else if (strcmp(avp[1], "polyline") == 0) {
        trk = route_head_alloc();
        track_add_head(trk);
      }
    } else if (strcmp(avp[0], "name") == 0) {
      if (wpt) {
        wpt->shortname = xstrdup(avp[1]);
      } else if (trk) {
        trk->rte_name = xstrdup(avp[1]);
      }
    } else if (strcmp(avp[0], "comment") == 0) {
      if (wpt) {
        wpt->notes = xstrdup(avp[1]);
      }
    } else if (strcmp(avp[0], "alt") == 0) {
      if (wpt) {
        wpt->altitude = atof(avp[1]);
      }
    } else if (strcmp(avp[0], "timestamp") == 0) {
      if (wpt) {
        wpt->creation_time = xml_parse_time(avp[1], &wpt->microseconds);
      }
    } else if (strcmp(avp[0], "icon") == 0) {
      if (wpt) {
        wpt->icon_descr = avp[1];
      }
    }

    avp+=2;
  }
}

static void
xol_shape_end(const char *args, const char **unused)
{
  if (wpt) {
    if (trk) {
      track_add_wpt(trk, wpt);
    } else {
      waypt_add(wpt);
    }
    wpt = NULL;
  } else if (trk) {
    if (trk->rte_waypt_ct == 0) {
      track_del_head(trk);
    }
    trk = NULL;
  }
}

static void
xol_waypt(const char *args, const char **attrv)
{
  const char **avp = &attrv[0];
  int x=0, y=0;

  while (*avp) {
    if (strcmp(avp[0], "y") == 0) {
      y = atoi(avp[1]);
    } else if (strcmp(avp[0], "x") == 0) {
      x = atoi(avp[1]);
    }
    avp+=2;
  }

  GPS_Math_Swiss_EN_To_WGS84((double)x, (double)y, &wpt->latitude, &wpt->longitude);
}

static void
xol_rd_init(const char *fname)
{
  trk = NULL;
  wpt = NULL;

  xml_init(fname, xol_map, NULL);
}

static void
xol_read(void)
{
  xml_read();
}

#endif

static void
xol_rd_deinit(void)
{
  xml_deinit();
}

/* writer */

static void
xol_fatal_outside(const waypoint *wpt)
{
  gbfprintf(fout, "#####\n");
  fatal(MYNAME ": %s (%s) is outside of convertable area \"%s\"!\n",
        wpt->shortname ? wpt->shortname : "Waypoint",
        pretty_deg_format(wpt->latitude, wpt->longitude, 'd', NULL, 0),
        gt_get_mps_grid_longname(grid_swiss, MYNAME));
}


static void
xol_write_time(const waypoint *wpt)
{
  QString time_string = wpt->CreationTimeXML();
  if (!time_string.isEmpty()) {
    gbfprintf(fout, " timestamp=\"%s\"", qPrintable(time_string));
  }
}

static void
xol_write_string(const char *name, const char *str)
{
  if (str && *str) {
    char *temp = strenquote(str, '"');
    gbfprintf(fout, " %s=%s", name, temp);
    xfree(temp);
  }
}

static void
xol_waypt_bound_calc(const waypoint *wpt)
{
  waypt_add_to_bounds(&all_bounds, wpt);
}

static void
xol_wr_init(const char *fname)
{
  fout = gbfopen(fname, "w", MYNAME);

  space = 1;
  waypt_init_bounds(&all_bounds);
  short_h = mkshort_new_handle();

  setshort_length(short_h, 1024); 	/* ??? */
  setshort_badchars(short_h, "\r\n\t");
  setshort_mustupper(short_h, 0);
  setshort_mustuniq(short_h, 1);
  setshort_whitespace_ok(short_h, 1);
  setshort_repeating_whitespace_ok(short_h, 1);
  setshort_defname(short_h, "Waypoint");
}

static void
xol_wr_deinit(void)
{
  mkshort_del_handle(&short_h);
  gbfclose(fout);
}

static void
xol_waypt_disp_cb(const waypoint *wpt)
{
  double x, y;
  char *name;

  name = wpt->shortname;
  if ((name == NULL) || (*name == '\0') || global_opts.synthesize_shortnames) {
    name = mkshort_from_wpt(short_h, wpt);
  } else {
    name = mkshort(short_h, name);
  }

  if (! GPS_Math_WGS84_To_Swiss_EN(wpt->latitude, wpt->longitude, &x, &y)) {
    xol_fatal_outside(wpt);
  }

  gbfprintf(fout, "%*s<shape type=\"waypoint\"", space++*2, "");
  xol_write_string("name", name);
  xol_write_string("comment", wpt->notes);
  xol_write_string("icon", wpt->icon_descr.toUtf8().data());
  if (wpt->creation_time) {
    xol_write_time(wpt);
  }
  if (wpt->altitude != unknown_alt) {
    gbfprintf(fout, " alt=\"%.f\"", wpt->altitude);
  }
  gbfprintf(fout, ">\n");

  gbfprintf(fout, "%*s<points>\n", space++*2, "");
  gbfprintf(fout, "%*s<point x=\"%.f\" y=\"%.f\"/>\n", space*2, "", x, y);
  gbfprintf(fout, "%*s</points>\n", --space*2, "");
  gbfprintf(fout, "%*s</shape>\n", --space*2, "");

  xfree(name);
}

static void
xol_track_hdr_disp_cb(const route_head *trk)
{
  gbfprintf(fout, "%*s<shape type=\"polyline\"", space++*2, "");
  xol_write_string("name", trk->rte_name);
  gbfprintf(fout, " lineSize=\"3\" lineColor=\"#e60000\" lineStyle=\"solid\">\n");
  gbfprintf(fout, "%*s<waypoints>\n", space++*2, "");
}

static void
xol_track_tlr_disp_cb(const route_head *trk)
{
  gbfprintf(fout, "%*s</waypoints>\n", --space*2, "");
  gbfprintf(fout, "%*s</shape>\n", --space*2, "");
}

static void
xol_trkpt_disp_cb(const waypoint *wpt)
{
  double x, y;

  if (! GPS_Math_WGS84_To_Swiss_EN(wpt->latitude, wpt->longitude, &x, &y)) {
    xol_fatal_outside(wpt);
  }

  gbfprintf(fout, "%*s<shape type=\"waypoint\"", space++*2, "");
  if (wpt->creation_time) {
    xol_write_time(wpt);
  }
  if (wpt->altitude != unknown_alt) {
    gbfprintf(fout, " alt=\"%.f\"", wpt->altitude);
  }
  gbfprintf(fout, ">\n");
  gbfprintf(fout, "%*s<points>\n", space++*2, "");
  gbfprintf(fout, "%*s<point x=\"%.f\" y=\"%.f\"/>\n", space*2, "", x, y);
  gbfprintf(fout, "%*s</points>\n", --space*2, "");
  gbfprintf(fout, "%*s</shape>\n", --space*2, "");
}

static void
xol_write(void)
{
  double x, y;

  waypt_disp_all(xol_waypt_bound_calc);
  track_disp_all(NULL, NULL, xol_waypt_bound_calc);

  if (! waypt_bounds_valid(&all_bounds)) {
    fatal(MYNAME ": No data available!\n");
  }

  if (! GPS_Math_WGS84_To_Swiss_EN(
        (all_bounds.min_lat + all_bounds.max_lat) / 2,
        (all_bounds.min_lon + all_bounds.max_lon) / 2, &x, &y)) {
    gbfprintf(fout, "#####\n");
    fatal(MYNAME ": At least one point is outside of convertable area \"%s\"!\n",
          gt_get_mps_grid_longname(grid_swiss, MYNAME));
  }

  gbfprintf(fout, "<?xml version=\"1.0\" encoding=\"%s\"?>\n", global_opts.charset_name);
  gbfprintf(fout, "<overlays>\n");
  gbfprintf(fout, "%*s<overlay version=\"1.0\">\n", space++*2, "");
  gbfprintf(fout, "%*s<center x=\"%.f\" y=\"%.f\"/>\n", space*2, "", x, y);
  gbfprintf(fout, "%*s<shapes>\n", space++*2, "");
  waypt_disp_all(xol_waypt_disp_cb);
  track_disp_all(xol_track_hdr_disp_cb, xol_track_tlr_disp_cb, xol_trkpt_disp_cb);
  gbfprintf(fout, "%*s</shapes>\n", --space*2, "");
  gbfprintf(fout, "%*s</overlay>\n", --space*2, "");
  gbfprintf(fout, "</overlays>\n");
}

ff_vecs_t xol_vecs = {
  ff_type_file,
  {
    (ff_cap)(ff_cap_read | ff_cap_write),	/* waypoints */
    (ff_cap)(ff_cap_read | ff_cap_write),	/* tracks */
    ff_cap_none
  },		/* routes */
  xol_rd_init,
  xol_wr_init,
  xol_rd_deinit,
  xol_wr_deinit,
  xol_read,
  xol_write,
  NULL,
  xol_args,
  CET_CHARSET_UTF8, 0
};
