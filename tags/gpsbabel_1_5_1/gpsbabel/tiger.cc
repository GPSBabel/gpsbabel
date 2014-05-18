/*
    Access to  U.S. Census Bureau "tiger" format.

    Copyright (C) 2002-2014 Robert Lipe, robertlipe+source@gpsbabel.org

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

#include <math.h>

#include "defs.h"
#include "csv_util.h"

static gbfile* file_in, *file_out;
static short_handle mkshort_handle;
static short_handle mkshort_whandle;

#define MYNAME "GPSUTIL"

static double maxlat, maxlon, minlat, minlon;
static int rec_cnt;
static char* nolabels = NULL;
static char* genurl = NULL;
static char* suppresswhite = NULL;
static char* iconismarker = NULL;
static char* snlen = NULL;

static char* margin  = NULL;
static char* xpixels = NULL;
static char* ypixels = NULL;
static char* oldthresh = NULL;
static char* oldmarker  = NULL;
static char* newmarker  = NULL;
static char* unfoundmarker  = NULL;

static int short_length;
static double thresh_days;

/*
 *   The code bracketed by CLICKMAP is to generate clickable image maps
 *   for a web browser.   It's functional, but is missing the math to do
 *   the projection transformations.   Some trig geek can finish that.
 */
#if CLICKMAP
static gbfile* linkf;
static char* clickmap = NULL;
#endif


static
arglist_t tiger_args[] = {
  {
    "nolabels", &nolabels, "Suppress labels on generated pins",
    NULL, ARGTYPE_BOOL, ARG_NOMINMAX
  },
  {
    "genurl", &genurl, "Generate file with lat/lon for centering map",
    NULL, ARGTYPE_OUTFILE, ARG_NOMINMAX
  },
  {
    "margin", &margin, "Margin for map.  Degrees or percentage",
    "15%", ARGTYPE_FLOAT, ARG_NOMINMAX
  },
  {
    "snlen", &snlen, "Max shortname length when used with -s",
    "10", ARGTYPE_INT, "1", NULL
  },
  {
    "oldthresh", &oldthresh,
    "Days after which points are considered old",
    "14", ARGTYPE_INT, ARG_NOMINMAX
  },
  {
    "oldmarker", &oldmarker, "Marker type for old points",
    "redpin", ARGTYPE_STRING, ARG_NOMINMAX
  },
  {
    "newmarker", &newmarker, "Marker type for new points",
    "greenpin", ARGTYPE_STRING, ARG_NOMINMAX
  },
  {
    "suppresswhite", &suppresswhite,
    "Suppress whitespace in generated shortnames",
    NULL, ARGTYPE_BOOL, ARG_NOMINMAX
  },
  {
    "unfoundmarker", &unfoundmarker, "Marker type for unfound points",
    "bluepin", ARGTYPE_STRING, ARG_NOMINMAX
  },
  {
    "xpixels", &xpixels, "Width in pixels of map",
    "768", ARGTYPE_INT, ARG_NOMINMAX
  },
  {
    "ypixels", &ypixels, "Height in pixels of map",
    "768", ARGTYPE_INT, ARG_NOMINMAX
  },
  {
    "iconismarker", &iconismarker,
    "The icon description is already the marker", NULL,
    ARGTYPE_BOOL, ARG_NOMINMAX
  },
#if CLICKMAP
  {
    "clickmap", &clickmap, "Generate Clickable map web page",
    NULL, ARGTYPE_BOOL, ARG_NOMINMAX
  },
#endif
  ARG_TERMINATOR
};


static void
rd_init(const char* fname)
{
  file_in = gbfopen(fname, "rb", MYNAME);
  mkshort_handle = mkshort_new_handle();
}

static void
rd_deinit(void)
{
  gbfclose(file_in);
  mkshort_del_handle(&mkshort_handle);
}

static void
wr_init(const char* fname)
{
  file_out = gbfopen(fname, "w", MYNAME);
  thresh_days = strtod(oldthresh, NULL);
}

static void
wr_deinit(void)
{
  gbfclose(file_out);
}

static void
data_read(void)
{
  double lat,lon;
  char desc[101];
  char icon[101];
  char* ibuf;
  Waypoint* wpt_tmp;
  int line = 0;

  while ((ibuf = gbfgetstr(file_in))) {
    if ((line++ == 0) && file_in->unicode) {
      cet_convert_init(CET_CHARSET_UTF8, 1);
    }
    if (sscanf(ibuf, "%lf,%lf:%100[^:]:%100[^\n]",
               &lon, &lat, icon, desc)) {
      wpt_tmp = new Waypoint;

      wpt_tmp->longitude = lon;
      wpt_tmp->latitude = lat;
      wpt_tmp->description = desc;
      wpt_tmp->shortname = mkshort(mkshort_handle, QString(desc));

      waypt_add(wpt_tmp);
    }
  }
}

static void
tiger_disp(const Waypoint* wpt)
{
  QString pin;
  double lat = wpt->latitude;
  double lon = wpt->longitude;

  if (iconismarker) {
    pin = wpt->icon_descr;
  } else if (wpt->icon_descr.contains("-unfound")) {
    pin = unfoundmarker;
  } else if (wpt->GetCreationTime() > current_time().addSecs(-3600 * 24 * thresh_days)) {
    pin = newmarker;
  } else {
    pin = oldmarker;
  }

  if (genurl) {
    if (lat > maxlat) {
      maxlat = lat;
    }
    if (lon > maxlon) {
      maxlon = lon;
    }
    if (lat < minlat) {
      minlat = lat;
    }
    if (lon < minlon) {
      minlon = lon;
    }
  }

  gbfprintf(file_out, "%f,%f:%s", lon, lat, pin.toUtf8().data());
  if (!nolabels) {
    QString temp;
    QString desc = csv_stringclean(wpt->description, ":");
    if (global_opts.synthesize_shortnames) {
      temp = desc;
      desc = mkshort(mkshort_whandle, desc);
    }
    gbfprintf(file_out, ":%s", CSTR(desc));
    if (temp != NULL) {
      desc = temp;
    }
  }
  gbfprintf(file_out, "\n");
}

#if CLICKMAP
static void
map_plot(const Waypoint* wpt)
{
  static int x,y;

  /* Replace with real math. */
  x+=10;
  y+=10;

  gbfprintf(linkf, "<area shape=\"circle\" coords=\"%d,%d,7\" href=\"%s\" alt=\"%s\"\n", x, y, wpt->url, wpt->description);
}
#endif /* CLICKMAP */

static double
dscale(double distance)
{
  /*
   * If we have any specified margin options  factor those in now.
   * A additional little boundary is helpful because Tiger always
   * puts the pin above the actual coord and if we don't pad the
   * top will be clipped.   It also makes the maps more useful to
   * have a little bit of context around the pins on the border.
   */

  if (strchr(margin, '%')) {
    return distance + strtod(margin, NULL) / 100.0 * distance;
  } else {
    return strtod(margin, NULL) + distance;
  }
}

static void
data_write(void)
{
  double latsz,lonsz;
  maxlat = -9999.0;
  maxlon = -9999.0;
  minlat = 9999.0;
  minlon = 9999.0;
  rec_cnt = 0;

  short_length = atoi(snlen);
  mkshort_whandle = mkshort_new_handle();

  if (suppresswhite) {
    setshort_whitespace_ok(mkshort_whandle, 0);
  }

  setshort_length(mkshort_whandle, short_length);

  gbfprintf(file_out, "#tms-marker\n");
  waypt_disp_all(tiger_disp);

  if (genurl) {
    gbfile* urlf;

    urlf = gbfopen(genurl, "w", MYNAME);
    latsz = fabs(maxlat - minlat);
    lonsz = fabs(maxlon - minlon);

    /*
     * Center the map along X and Y axis the midpoint of
     * our min and max coords each way.
     */
    gbfprintf(urlf, "lat=%f&lon=%f&ht=%f&wid=%f",
              minlat + (latsz/2.0),
              minlon + (lonsz/2.0),
              dscale(latsz),
              dscale(lonsz));

    gbfprintf(urlf, "&iwd=%s&iht=%s", xpixels, ypixels);
    gbfclose(urlf);
#if CLICKMAP
    if (clickmap) {
      linkf = gbfopen(clickmap, "w", MYNAME);
      gbfprintf(linkf, "<map name=\"map\">\n");
      waypt_disp_all(map_plot);
      gbfprintf(linkf, "</map>\n");
      gbfclose(linkf);
      linkf = NULL;
    }
#endif
  }

  mkshort_del_handle(&mkshort_whandle);
}


ff_vecs_t tiger_vecs = {
  ff_type_file,
  FF_CAP_RW_WPT,
  rd_init,
  wr_init,
  rd_deinit,
  wr_deinit,
  data_read,
  data_write,
  NULL,
  tiger_args,
  CET_CHARSET_ASCII, 0	/* CET-REVIEW */
};
