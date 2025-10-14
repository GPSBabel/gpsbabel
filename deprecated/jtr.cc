/*

    Support for the Jelbert GeoTagger JTR data file format.

    Copyright (C) 2008 Olaf Klein

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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

 */
#include "defs.h"
#include "csv_util.h"
#include <QHash>
//#include <cassert>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include "nmea.h"

#define MYNAME "jtr"

static
QVector<arglist_t> jtr_args = {
};

static gbfile* fin, *fout;
static QHash<QString, const Waypoint*> trkpts;

void
jtr_parse_date(const char* str, QDate& date)
{
  int dmy = atoi(str);
  if (dmy > 0) {
    int year= dmy % 100 + 2000;
    dmy = dmy / 100;
    int month = dmy % 100;
    dmy = dmy / 100;
    int day = dmy;
    date = QDate(year, month, day);
  }
}

void
jtr_parse_time(const char* str, QTime& time)
{
  char* dot;
  long int hms = strtol(str, &dot, 10);
  int sec = hms % 100;
  hms = hms / 100;
  int min  = hms % 100;
  hms = hms / 100;
  int hour = hms % 100;

  if (*dot == '.') {
    int milli = atoi(dot + 1) * 10;
    sec += milli / 1000;
  }

  time = QTime(hour, min, sec);
}

/*******************************************************************************
* %%%        global callbacks called by gpsbabel main process              %%% *
*******************************************************************************/

static void
jtr_rd_init(const QString& fname)
{
  fin = gbfopen(fname, "r", MYNAME);
}

static void
jtr_rd_deinit()
{
  trkpts.clear();
  gbfclose(fin);
}

static void
jtr_read()
{
  char* str;
  int line = 0;
  route_head* trk = nullptr;

  while ((str = gbfgetstr(fin))) {
    QDate date;
    QTime time;
    char valid = 'V';
    double lon;
    float course, mcourse, mvar, mdev;
    char mdevdir;

    line++;

    str = lrtrim(str);
    if (*str == '\0') {
      continue;
    }

    if (strncmp(str, "GEOTAG2,", 8) != 0) {
      fatal(MYNAME ": Unknown or unsupported file (missing \"GEOTAG2\")!\n");
    }

    double lat = lon = 999;
    float speed = course = mcourse = mvar = mdev = -1;
    char mvardir = mdevdir = 0;

    int column = -1;
    char* tmp = str;
    while ((str = csv_lineparse(tmp, ",", "", column++))) {
      tmp = nullptr;

      if (*str == '\0') {
        continue;
      }

      switch (column) {
      case 0:
        break;		/* GEOTAG2 */
      case 1:
        jtr_parse_time(str, time);
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
        jtr_parse_date(str, date);
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

    QDateTime dt = QDateTime(date, time, Qt::UTC);

    /* check for duplicates as suggested in format description */
    QString buf = QString("%1\01%2\01%3")
                  .arg(QString::number(lat, 'f', 6),
                       QString::number(lon, 'f', 6),
                       QString::number(dt.toSecsSinceEpoch()));
    if (trkpts.contains(buf)) {
      continue;
    }

    auto* wpt = new Waypoint;

    wpt->latitude = lat;
    wpt->longitude = lon;
    wpt->SetCreationTime(dt);
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

    if (trk == nullptr) {
      trk = new route_head;
      track_add_head(trk);
    }

    trkpts.insert(buf, wpt);
    track_add_wpt(trk, wpt);
  }
}

static void
jtr_wr_init(const QString& fname)
{
  fout = gbfopen(fname, "wb", MYNAME);
}

static void
jtr_wr_deinit()
{
  gbfclose(fout);
}

static void
jtr_trkpt_disp_cb(const Waypoint* wpt)
{
  char* str, *tmp;
  char scourse[6], sspeed[8];
  QString sdate;
  QString stime;

  if (wpt->creation_time.isValid()) {
    gpsbabel::DateTime dt = wpt->GetCreationTime().toUTC();

    // round time to centiseconds.
    int msec = dt.time().msec();
    int csec = lround(msec/10.0);
    dt = dt.addMSecs(csec*10-msec);
    sdate = dt.toString(QStringLiteral("ddMMyy"));
    stime = dt.toString(QStringLiteral("HHmmss.zzz"));
    // toss milliseconds position which should now be zero.
    //assert(stime.endsWith('0'));
    stime.chop(1);
    // suppress fractional seconds if they are zero.
    stime = stime.replace(QLatin1String(".00"), QLatin1String(""));
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
            CSTR(stime),
            wpt->creation_time.isValid() ? 'A' : 'V',
            fabs(degrees2ddmm(wpt->latitude)),
            wpt->latitude < 0 ? 'S' : 'N',
            fabs(degrees2ddmm(wpt->longitude)),
            wpt->longitude < 0 ? 'W' : 'E',
            sspeed,
            scourse,
            CSTR(sdate));

  xasprintf(&tmp, "%s*%02X", str, NmeaFormat::nmea_cksum(str));
  xfree(str);
  str = tmp;

  xasprintf(&tmp, "%s,,,E,,E*%02X\r", str, NmeaFormat::nmea_cksum(str));
  xfree(str);
  str = tmp;

  gbfputs(str, fout);
  xfree(str);
}

static void
jtr_write()
{
  track_disp_all(nullptr, nullptr, jtr_trkpt_disp_cb);
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
  nullptr,
  &jtr_args,
  CET_CHARSET_ASCII, 0			/* ascii is the expected character set */
  /* not fixed, can be changed through command line parameter */
  , NULL_POS_OPS,
  nullptr
};

/**************************************************************************/
