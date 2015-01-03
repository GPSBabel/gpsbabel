/*

    Support for the Jelbert GeoTagger JTR data file format.

    Copyright (C) 2008 Olaf Klein, o.b.klein@gpsbabel.org

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
#include "csv_util.h"
#include <QtCore/QHash>
#include <cmath>
#include <stdlib.h>
#include <stdio.h>

#define MYNAME "jtr"

static
arglist_t jtr_args[] = {
  ARG_TERMINATOR
};

static gbfile* fin, *fout;
static QHash<QString, const Waypoint*> trkpts;

static time_t
jtr_parse_time(const char* str, struct tm* tm, int* milli)
{
  long int hms;
  char* dot;

  hms = strtol(str, &dot, 10);
  if (hms > 0) {
    tm->tm_sec = hms % 100;
    hms = hms / 100;
    tm->tm_min = hms % 100;
    hms = hms / 100;
    tm->tm_hour = hms % 100;

    if ((*dot == '.') && (milli != NULL)) {
      *milli = atoi(dot + 1) * 10;
    }

    return mkgmtime(tm);
  } else {
    return 0;
  }
}

static time_t
jtr_parse_date(const char* str, struct tm* tm)
{
  int dmy = atoi(str);

  if (dmy > 0) {
    tm->tm_year = dmy % 100 + 100;
    dmy = dmy / 100;
    tm->tm_mon  = dmy % 100 - 1;
    dmy = dmy / 100;
    tm->tm_mday = dmy;
    return mkgmtime(tm);
  } else {
    return 0;
  }
}

/*******************************************************************************
* %%%        global callbacks called by gpsbabel main process              %%% *
*******************************************************************************/

static void
jtr_rd_init(const char* fname)
{
  fin = gbfopen(fname, "r", MYNAME);
}

static void
jtr_rd_deinit(void)
{
  trkpts.clear();
  gbfclose(fin);
}

static void
jtr_read(void)
{
  char* str;
  int line = 0;
  route_head* trk = NULL;

  while ((str = gbfgetstr(fin))) {
    Waypoint* wpt;
    struct tm tm;
    char* tmp;
    int column = -1;
    char valid = 'V';
    double lat, lon;
    float speed, course, mcourse, mvar, mdev;
    time_t time = 0;
    int mills = 0;
    char buf[32];
    char mvardir, mdevdir;

    line++;

    str = lrtrim(str);
    if (*str == '\0') {
      continue;
    }

    if (strncmp(str, "GEOTAG2,", 8) != 0) {
      fatal(MYNAME ": Unknown or unsupported file (missing \"GEOTAG2\")!\n");
    }

    memset(&tm, 0, sizeof(tm));
    lat = lon = 999;
    speed = course = mcourse = mvar = mdev = -1;
    mvardir = mdevdir = 0;

    column = -1;
    tmp = str;
    while ((str = csv_lineparse(tmp, ",", "", column++))) {
      tmp = NULL;

      if (*str == '\0') {
        continue;
      }

      switch (column) {
      case 0:
        break;		/* GEOTAG2 */
      case 1:
        jtr_parse_time(str, &tm, &mills);
        break;
      case 2:
        valid = *str;
        break;
      case 3:
        lat = ddmm2degrees(atof(str));
        break;
      case 4:
        if (*str == 'S') {
          lat *= -1;
        }
        break;
      case 5:
        lon = ddmm2degrees(atof(str));
        break;
      case 6:
        if (*str == 'W') {
          lon *= -1;
        }
        break;
      case 7:
        speed = KNOTS_TO_MPS(atof(str));
        break;
      case 8:
        course = atof(str);
        break;
      case 9:
        jtr_parse_date(str, &tm);
        break;
      case 13:
        mcourse = atof(str);
        break;
      case 14:
        mdev = atof(str);
        break;
      case 15:
        mdevdir = *str;
        break;
      case 16:
        mvar = atof(str);
        break;
      case 17:
        mvardir = *str;
        break;
      default:
        break;
      }
    }

    if ((lat == 999) || (lon == 999) || (valid != 'A')) {
      continue;
    }

    if (tm.tm_year > 0) {
      time = mkgmtime(&tm);
    } else {
      time = 0;
    }

    /* check for duplicates as suggested in format description */

    snprintf(buf, sizeof(buf), "%.6f\01%.6f\01%ld", lat, lon, (long)time);
    if (trkpts.contains(QString::fromUtf8(buf))) {
      continue;
    }

    wpt = new Waypoint;

    wpt->latitude = lat;
    wpt->longitude = lon;
    wpt->SetCreationTime(time, mills);
    if (speed >= 0) {
      WAYPT_SET(wpt, speed, speed);
    }
    if (mcourse >= 0) {
      course = mcourse;
      if (mvar >= 0) {
        if (mvardir == 'W') {
          course += mvar;
        } else if (mvardir == 'E') {
          course -= mvar;
        }
      }
      if (mdev >= 0) {
        if (mdevdir == 'W') {
          course += mdev;
        } else if (mdevdir == 'E') {
          course -= mdev;
        }
      }
    }
    if (course >= 0) {
      WAYPT_SET(wpt, course, course);
    }

    if (trk == NULL) {
      trk = route_head_alloc();
      track_add_head(trk);
    }

    trkpts.insert(QString::fromUtf8(buf), wpt);
    track_add_wpt(trk, wpt);
  }
}

static void
jtr_wr_init(const char* fname)
{
  fout = gbfopen(fname, "wb", MYNAME);
}

static void
jtr_wr_deinit(void)
{
  gbfclose(fout);
}

static void
jtr_trkpt_disp_cb(const Waypoint* wpt)
{
  char* str, *tmp;
  char stime[10], sdate[7], scourse[6], sspeed[8];
  struct tm tm;

  if (wpt->creation_time.isValid()) {
    const time_t tt = wpt->GetCreationTime().toTime_t();
    tm = *gmtime(&tt);

    tm.tm_year += 1900;
    tm.tm_mon += 1;
    snprintf(sdate, sizeof(sdate), "%02d%02d%02d", tm.tm_mday, tm.tm_mon, tm.tm_year % 100);
    snprintf(stime, sizeof(stime), "%02d%02d%02d.%02d", tm.tm_hour, tm.tm_min, tm.tm_sec, wpt->creation_time.time().msec());
    if (wpt->creation_time.time().msec() == 0) {
      stime[6] = 0;
    }
  } else {
    stime[0] = 0;
    sdate[0] = 0;
  }
  if (WAYPT_HAS(wpt, speed) && (wpt->speed >= 0)) {
    snprintf(sspeed, sizeof(sspeed), "%.1f", MPS_TO_KNOTS(wpt->speed));
  } else {
    sspeed[0] = 0;
  }
  if (WAYPT_HAS(wpt, course) && (wpt->course >= 0)) {
    snprintf(scourse, sizeof(scourse), "%.1f", wpt->course);
  } else {
    scourse[0] = 0;
  }

  xasprintf(&str, "GEOTAG2,%s,%c,%09.4f,%c,%010.4f,%c,%s,%s,%s,,E,A",
            stime,
            wpt->creation_time.isValid() ? 'A' : 'V',
            fabs(degrees2ddmm(wpt->latitude)),
            wpt->latitude < 0 ? 'S' : 'N',
            fabs(degrees2ddmm(wpt->longitude)),
            wpt->longitude < 0 ? 'W' : 'E',
            sspeed,
            scourse,
            sdate);

  xasprintf(&tmp, "%s*%02X", str, nmea_cksum(str));
  xfree(str);
  str = tmp;

  xasprintf(&tmp, "%s,,,E,,E*%02X\r", str, nmea_cksum(str));
  xfree(str);
  str = tmp;

  gbfputs(str, fout);
  xfree(str);
}

static void
jtr_write(void)
{
  track_disp_all(NULL, NULL, jtr_trkpt_disp_cb);
}

/**************************************************************************/

ff_vecs_t jtr_vecs = {
  ff_type_file,
  {
    ff_cap_none, 			/* waypoints */
    (ff_cap)(ff_cap_read | ff_cap_write), 	/* tracks */
    ff_cap_none 			/* routes */
  },
  jtr_rd_init,
  jtr_wr_init,
  jtr_rd_deinit,
  jtr_wr_deinit,
  jtr_read,
  jtr_write,
  NULL,
  jtr_args,
  CET_CHARSET_ASCII, 0			/* ascii is the expected character set */
  /* not fixed, can be changed through command line parameter */
};

/**************************************************************************/
