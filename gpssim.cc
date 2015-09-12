/*
    Write points to Franson Technology GpsGate simulator

    Copyright (C) 2006  Robert Lipe, robertlipe+source@gpsbabel.org

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
#include <math.h>
#include <stdlib.h>
#include <stdio.h>

#define MYNAME "gpssim"

static gbfile* fout;
static char* wayptspd;
static char* splitfiles_opt;
static int splitfiles;
static char* fnamestr;
static int trk_count;
static int doing_tracks;

static
arglist_t gpssim_args[] = {
  {
    "wayptspd", &wayptspd, "Default speed for waypoints (knots/hr)",
    NULL, ARGTYPE_FLOAT, ARG_NOMINMAX
  },
  {
    "split", &splitfiles_opt, "Split input into separate files",
    "0", ARGTYPE_BOOL, ARG_NOMINMAX
  },
  ARG_TERMINATOR
};

/*
 * The only thing kind of odd about this format is the "split"
 * option.  There's some trashing about with the 'splitfiles' toggle
 * to ensure that waypoints land in one file and each track and each
 * route land in files of their own.
 */

static void
gpssim_wr_init(const char* fname)
{
  fnamestr =  xstrdup(fname);
  trk_count = 0;
  splitfiles = splitfiles_opt ? atoi(splitfiles_opt) : 0;

  /* If writing to stdout, never split files */
  if (0 == strcmp("-",splitfiles_opt)) {
    splitfiles = 0;
  }

  if (!splitfiles) {
    fout = gbfopen(fname, "wb", MYNAME);
  }
}

static void
gpssim_wr_deinit(void)
{
  if (fout) {
    gbfclose(fout);
    fout = NULL;
  }

  xfree(fnamestr);
}


/*
 * All these files are written in binary mode, so put CR/NL pairs
 * in them explictly in case we're writing from a UNIX-like host.
 */

static void
gpssim_write_sentence(const char* const s)
{
  gbfprintf(fout, "$%s*%02X\r\n", s, nmea_cksum(s));
}

static void
gpssim_write_spd(double knotsperhour)
{
  char obuf[1024];

  snprintf(obuf, sizeof(obuf), "FRSPD,%.2f", knotsperhour);
  gpssim_write_sentence(obuf);
}

static void
gpssim_write_pt(const Waypoint* wpt)
{
  char obuf[1024];
  double lat, lon;

  if WAYPT_HAS(wpt, speed) {
    gpssim_write_spd(MPS_TO_KNOTS(wpt->speed));
  }

  lat = degrees2ddmm(wpt->latitude);
  lon = degrees2ddmm(wpt->longitude);

  snprintf(obuf, sizeof(obuf), "FRWPT,%10.5f,%c,%011.5f,%c,%.1f",
           fabs(lat), lat < 0 ? 'S' : 'N',
           fabs(lon), lon < 0 ? 'W' : 'E',
           wpt->altitude == unknown_alt ? 0 : wpt->altitude
          );

  if (wpt->creation_time.isValid()) {
    char tbuf[20];
    int hms, ymd;
    struct tm* tm;

    const time_t tt = wpt->GetCreationTime().toTime_t();
    tm = gmtime(&tt);
    hms = tm->tm_hour * 10000 + tm->tm_min * 100 + tm->tm_sec;
    ymd = tm->tm_mday * 10000 + tm->tm_mon * 100 + tm->tm_year;

    snprintf(tbuf, sizeof(tbuf), ",%d,%d",ymd, hms);
    strcat(obuf, tbuf);
  }

  gpssim_write_sentence(obuf);
}

static void
gpssim_trk_hdr(const route_head* rh)
{
  if (splitfiles) {
    char c[1024];
    char* ofname = xstrdup(fnamestr);

    if (fout) {
      fatal(MYNAME ": output file already open.\n");
    }

    snprintf(c, sizeof(c), "%s%04d.gpssim",
             doing_tracks ? "-track" : "-route",
             trk_count++);
    ofname = xstrappend(ofname, c);
    fout = gbfopen(ofname, "wb", MYNAME);
    xfree(ofname);
  }
  track_recompute(rh, NULL);
}

static void
gpssim_trk_ftr(const route_head* rh)
{
  if (splitfiles) {
    gbfclose(fout);
    fout = NULL;
  }
}

static void
gpssim_write(void)
{
  if (waypt_count()) {
    if (splitfiles) {
      char* ofname = xstrdup(fnamestr);
      ofname = xstrappend(ofname, "-waypoints.gpssim");
      fout = gbfopen(ofname, "wb", MYNAME);
      xfree(ofname);
    }
    if (wayptspd && wayptspd[0]) {
      gpssim_write_spd(atof(wayptspd));
    }
    waypt_disp_all(gpssim_write_pt);
    if (splitfiles) {
      gbfclose(fout);
      fout = NULL;
    }
  }

  doing_tracks = 1;
  track_disp_all(gpssim_trk_hdr, gpssim_trk_ftr, gpssim_write_pt);

  trk_count = 0;
  doing_tracks = 0;
  route_disp_all(gpssim_trk_hdr, gpssim_trk_ftr, gpssim_write_pt);
}


ff_vecs_t gpssim_vecs = {
  ff_type_file,
  { ff_cap_write, ff_cap_write, ff_cap_write },
  NULL,
  gpssim_wr_init,
  NULL,
  gpssim_wr_deinit,
  NULL,
  gpssim_write,
  NULL,
  gpssim_args,
  CET_CHARSET_ASCII, 0
};
