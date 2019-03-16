/*

    Support for "GeoGrid Viewer" binary tracklogs (*.log)

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

#include <cmath>                   // for fabs, floor, lround
#include <cstdio>                  // for sscanf
#include <cstring>                 // for memset, strncmp
#include <cstdint>                 // for int16_t
#include <ctime>                   // for gmtime

#include <QtCore/QString>          // for QString
#include <QtCore/QTime>            // for QTime
#include <QtCore/QtGlobal>         // for foreach

#include "defs.h"
#include "gbfile.h"                // for gbfputint16, gbfclose, gbfopen, gbfputflt, gbfgetc, gbfputcstr, gbfputdbl, gbfread, gbfile
#include "grtcirc.h"               // for heading_true_degrees
#include "src/core/datetime.h"     // for DateTime



#define MYNAME "ggv_log"

static gbfile* fin, *fout;
static int ggv_log_ver;

static
arglist_t ggv_log_args[] = {
  ARG_TERMINATOR
};

/*******************************************************************************
* %%%        global callbacks called by gpsbabel main process              %%% *
*******************************************************************************/

static void
ggv_log_rd_init(const QString& fname)
{
  static char magic[32];
  int len = 0;

  fin = gbfopen(fname, "rb", MYNAME);

  for (;;) {
    int cin = gbfgetc(fin);
    if (cin < 0) {
      break;
    }

    magic[len++] = cin;

    if (cin == '\0') {
      double ver = 0;
      if (strncmp(magic, "DOMGVGPS Logfile V", 18) != 0) {
        break;
      }

      char* sver = &magic[18];
      sscanf(sver, "%lf:", &ver);
      ggv_log_ver = ver * 10;
      if ((ggv_log_ver == 10) || (ggv_log_ver == 25)) {
        return;  /* header accepted */
      }

      fatal(MYNAME ": Sorry, unsupported version (%s)!\n", sver);
    } else if (len == sizeof(magic)) {
      break;
    }
  }
  fatal(MYNAME ": Invalid header. Probably no " MYNAME " file!\n");
}

static void
ggv_log_rd_deinit()
{
  gbfclose(fin);
}

static void
ggv_log_read()
{
  int bufsz = 0, len;
  route_head* trk = nullptr;

  switch (ggv_log_ver) {
  case 10:
    bufsz = 0x2A;
    break;
  case 25:
    bufsz = 0x6F;
    break;
  }

  signed char* buf = (signed char*) xmalloc(bufsz);

  while ((len = gbfread(buf, 1, bufsz, fin))) {
    struct tm tm;

    if (len != bufsz) {
      break;
    }

    if (trk == nullptr) {
      trk = route_head_alloc();
      track_add_head(trk);
    }

    memset(&tm, 0, sizeof(tm));

    Waypoint* wpt = new Waypoint;

    int deg = (int16_t) le_read16(&buf[0]);
    int min = le_read16(&buf[2]);
    float sec = le_read_float(&buf[4]);
    double xlat = (double)deg + ((double)min / 60.0) + (sec / 3600.0);
    wpt->latitude = xlat;

    deg = (int16_t) le_read16(&buf[8]);
    min = le_read16(&buf[10]);
    sec = le_read_float(&buf[12]);
    double xlon = (double)deg + ((double)min / 60.0) + (sec / 3600.0);
    wpt->longitude = xlon;

    WAYPT_SET(wpt, course, le_read16(&buf[16 + 0]));
    int milliseconds = 0;
    if (ggv_log_ver == 10) {
      wpt->altitude = le_read16(&buf[16 +  2]);
      WAYPT_SET(wpt, speed, le_read16(&buf[16 +  4]));
      tm.tm_year =    le_read16(&buf[16 +  8]);
      tm.tm_mon =     le_read16(&buf[16 + 10]);
      tm.tm_mday =    le_read16(&buf[16 + 12]);
      tm.tm_hour =    le_read16(&buf[16 + 14]);
      tm.tm_min =     le_read16(&buf[16 + 16]);
      double secs = le_read_double(&buf[16 + 18]);
      tm.tm_sec = (int)secs;
      milliseconds = lround((secs - tm.tm_sec) * 1000.0);
    } else {
      wpt->altitude = le_read16(&buf[16 + 4]);
      wpt->sat = (unsigned char)buf[16 + 14];

      /* other probably valid double values at offset:

      22: 0.0 - 20.0
      43: 0.0 - 59.0
      51: -1.0
      61: -1.0
      79: .. - 20.0 ? speed over ground ? (++)
      87: ? course ?
      95: 0.0 - 3.1 (++)
      103: -1

      */
    }

    if (wpt->altitude == 0) {
      wpt->altitude = unknown_alt;
    }

    if (tm.tm_year >= 1900) {
      tm.tm_year -= 1900;
      if (tm.tm_mon > 0) {
        tm.tm_mon--;
        wpt->SetCreationTime(mkgmtime(&tm), milliseconds);
      }
    }

    track_add_wpt(trk, wpt);
  }
  xfree(buf);
}

static void
ggv_log_wr_init(const QString& fname)
{
  fout = gbfopen(fname, "wb", MYNAME);

  gbfputcstr("DOMGVGPS Logfile V1.0:", fout);
}

static void
ggv_log_wr_deinit()
{
  gbfclose(fout);
}

static void
ggv_log_track_head_cb(const route_head* trk)
{
  const Waypoint* prev = nullptr;

  foreach (const Waypoint* wpt, trk->waypoint_list) {
    double  course = 0, speed = 0;
    struct tm tm;
    double secs = 0;

    int latint = wpt->latitude;
    int lonint = wpt->longitude;
    double latmin = 60.0 * (fabs(wpt->latitude) - latint);
    double lonmin = 60.0 * (fabs(wpt->longitude) - lonint);
    double latsec = 60.0 * (latmin - floor(latmin));
    double lonsec = 60.0 * (lonmin - floor(lonmin));

    if (wpt->creation_time.isValid()) {
      time_t t = wpt->GetCreationTime().toTime_t();
      tm = *gmtime(&t);
      tm.tm_mon += 1;
      tm.tm_year += 1900;
    } else {
      memset(&tm, 0, sizeof(tm));
    }

    if (prev != nullptr) {
      course = heading_true_degrees(
                 prev->latitude, prev->longitude,
                 wpt->latitude, wpt->longitude);
      speed = waypt_speed(prev, wpt);
    }
    if (wpt->creation_time.isValid()) {
      secs = (double)tm.tm_sec + wpt->GetCreationTime().time().msec() / 1000.0;
    }

    gbfputint16((int16_t) latint, fout);
    gbfputint16((int16_t) latmin, fout);
    gbfputflt(latsec, fout);
    gbfputint16((int16_t) lonint, fout);
    gbfputint16((int16_t) lonmin, fout);
    gbfputflt(lonsec, fout);
    gbfputint16((int16_t) course, fout);
    gbfputint16((int16_t)(wpt->altitude != unknown_alt) ? wpt->altitude : 0, fout);
    gbfputint16((int16_t) speed, fout);
    gbfputint16(0, fout);
    gbfputint16(tm.tm_year, fout);
    gbfputint16(tm.tm_mon, fout);
    gbfputint16(tm.tm_mday, fout);
    gbfputint16(tm.tm_hour, fout);
    gbfputint16(tm.tm_min, fout);
    gbfputdbl(secs, fout);

    prev = wpt;
  }
}

static void
ggv_log_write()
{
  track_disp_all(ggv_log_track_head_cb, nullptr, nullptr);
}

/**************************************************************************/

ff_vecs_t ggv_log_vecs = {
  ff_type_file,
  {
    ff_cap_none, 			/* waypoints */
    (ff_cap)(ff_cap_read | ff_cap_write),	/* tracks */
    ff_cap_none			/* routes */
  },
  ggv_log_rd_init,
  ggv_log_wr_init,
  ggv_log_rd_deinit,
  ggv_log_wr_deinit,
  ggv_log_read,
  ggv_log_write,
  nullptr,
  ggv_log_args,
  CET_CHARSET_ASCII, 1
  , NULL_POS_OPS,
  nullptr
};
/**************************************************************************/
