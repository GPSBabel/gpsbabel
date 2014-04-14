/*
    Access gpsutil files.

    Copyright (C) 2002, 2003, 2004 Robert Lipe, robertlipe+source@gpsbabel.org

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
#include "magellan.h"

static gbfile* file_in, *file_out;
static short_handle mkshort_handle;

#define MYNAME "GPSUTIL"

static void
rd_init(const char* fname)
{
  file_in = gbfopen(fname, "rb", MYNAME);
}

static void
rd_deinit(void)
{
  gbfclose(file_in);
}

static void
wr_init(const char* fname)
{
  file_out = gbfopen(fname, "w", MYNAME);
  mkshort_handle = mkshort_new_handle();
}

static void
wr_deinit(void)
{
  gbfclose(file_out);
  mkshort_del_handle(&mkshort_handle);
}

static void
data_read(void)
{
  char* ibuf;
  char desc[31];
  double lat,lon;
  char latdir, londir;
  int ilat, ilon;
  long alt;
  char alttype;
  char icon[3];
  Waypoint* wpt_tmp;
  int line = 0;
  /*
   * Make sure that all waypoints in single read have same
   * timestamp.
   */
  time_t now = current_time().toTime_t();
  icon[0] = 0;

  while ((ibuf = gbfgetstr(file_in))) {
    int n, len;
    char* sn;

    if ((line++ == 0) && file_in->unicode) {
      cet_convert_init(CET_CHARSET_UTF8, 1);
    }

    /*  A sharp in column zero or an blank line is a comment */
    ibuf = lrtrim(ibuf);
    len = strlen(ibuf);
    if ((len == 0) || (*ibuf == '#')) {
      continue;
    }

    if (len > 71) {
      int offs = len - 71;
      sn = xstrndup(ibuf, offs + 8);
      ibuf += (offs + 9);
    } else {
      sn = xstrndup(ibuf, 8);
      ibuf += 9;
    }

    n = sscanf(ibuf, "%lf%c %lf%c %ld%c %30[^,] %2s",
               &lat, &latdir, &lon, &londir,
               &alt, &alttype, desc, icon);
    /* Require at least first threee fields, otherwise ignore */
    if (n < 2) {
      xfree(sn);
      continue;
    }
    rtrim(sn);
    rtrim(desc);
    rtrim(icon);
    wpt_tmp = new Waypoint;
    wpt_tmp->altitude = alt;
    wpt_tmp->shortname = sn;
    xfree(sn);
    wpt_tmp->description = desc;
    wpt_tmp->SetCreationTime(now);

    if (latdir == 'S') {
      lat = -lat;
    }
    if (londir == 'W') {
      lon = -lon;
    }

    lat /= 100.0;
    lon /= 100.0;
    ilon = (int)(lon);
    wpt_tmp->longitude = ilon + (lon - ilon)*(100.0/60.0);
    ilat = (int)(lat);
    wpt_tmp->latitude = ilat + (lat - ilat) * (100.0/60.0);
    wpt_tmp->icon_descr = mag_find_descr_from_token(icon);
    waypt_add(wpt_tmp);
  }
}

static void
gpsutil_disp(const Waypoint* wpt)
{
  double lon,lat;
  QString icon_token;
  char* tdesc = xstrdup(wpt->description);

  icon_token = mag_find_token_from_descr(wpt->icon_descr);

  lon = degrees2ddmm(wpt->longitude);
  lat = degrees2ddmm(wpt->latitude);

  gbfprintf(file_out, "%-8.8s %08.3f%c %09.3f%c %07.0f%c %-30.30s %s\n",
            global_opts.synthesize_shortnames ?
            CSTRc(mkshort_from_wpt(mkshort_handle, wpt)) :
            CSTRc(wpt->shortname),
            fabs(lat),
            lat < 0.0 ? 'S' : 'N',
            fabs(lon),
            lon < 0.0 ? 'W' : 'E',
            ((wpt->altitude == unknown_alt) ||
             (wpt->altitude < 0.0)) ? 0 : wpt->altitude,
            'm',
            CSTRc(wpt->description) ? tdesc : "",
            icon_token.toUtf8().data());

  xfree(tdesc);
}

static void
data_write(void)
{
  waypt_disp_all(gpsutil_disp);
}


ff_vecs_t gpsutil_vecs = {
  ff_type_file,
  FF_CAP_RW_WPT,
  rd_init,
  wr_init,
  rd_deinit,
  wr_deinit,
  data_read,
  data_write,
  NULL,
  NULL,
  CET_CHARSET_ASCII, 0	/* CET-REVIEW */
};
