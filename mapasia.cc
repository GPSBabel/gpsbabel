/*

    Support for MapAsia (.tr7) track file format.

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

#include <cmath>                // for fabs
#include <cstring>              // for memset

#include <QtCore/QDate>         // for QDate
#include <QtCore/QDateTime>     // for QDateTime
#include <QtCore/QString>       // for QString
#include <QtCore/QTime>         // for QTime
#include <QtCore/Qt>            // for UTC
#include <QtCore/QtGlobal>      // for foreach

#include "defs.h"
#include "gbfile.h"             // for gbfclose, gbfeof, gbfgetint32, gbfputint32, gbfread, gbfwrite, gbfile, gbfopen_le
#include "session.h"            // for curr_session
#include "src/core/datetime.h"  // for DateTime


#define MYNAME "mapasia"

#define TR7_TRACK_MAGIC	0x223EADB

#define TR7_S_SIZE	32

#define TR7_S_YEAR	0
#define TR7_S_MONTH	2
#define TR7_S_DAY	6
#define TR7_S_HOUR	8
#define TR7_S_MIN	10
#define TR7_S_SEC	12
#define TR7_S_LON	16
#define TR7_S_LAT	20
#define TR7_S_SPEED	24
#define TR7_S_COURSE	26
#define TR7_S_VALID	28
#define TR7_S_FIX	29

static gbfile* fin, *fout;
static const Waypoint* wpt_tmp;
static const route_head* trk_tmp;
static int course_tmp, speed_tmp;

static
arglist_t tr7_args[] = {
  ARG_TERMINATOR
};

/*******************************************************************************
* %%%                             R E A D E R                              %%% *
*******************************************************************************/

static void
tr7_rd_init(const QString& fname)
{
  fin = gbfopen_le(fname, "rb", MYNAME);
}

static void
tr7_read()
{
  route_head* trk = nullptr;
  Waypoint* prev = nullptr;

  unsigned int magic = gbfgetint32(fin);
  if (magic != TR7_TRACK_MAGIC) {
    fatal(MYNAME ": Invalid magic number in header (%X, but %X expected)!\n", magic, TR7_TRACK_MAGIC);
  }

  while (! gbfeof(fin)) {
    unsigned char buff[TR7_S_SIZE];

    gbfread(buff, 1, sizeof(buff), fin);

    double lat = (double)le_read32(&buff[TR7_S_LAT]) / 1000000.0;
    double lon = (double)le_read32(&buff[TR7_S_LON]) / 1000000.0;

    if ((fabs(lat) > 90) || (fabs(lon) > 180)) {	/* that really happens */
      trk = nullptr;
      continue;
    }

    QDate date(le_read16(&buff[TR7_S_YEAR]),
               buff[TR7_S_MONTH],
               buff[TR7_S_DAY]);
    QTime time(buff[TR7_S_HOUR],
               buff[TR7_S_MIN],
               buff[TR7_S_SEC]);
    if (!date.isValid() || !time.isValid()) {
      continue;
    }

    float speed = KPH_TO_MPS(le_read16(&buff[TR7_S_SPEED]));
    float course = 360 - le_read16(&buff[TR7_S_COURSE]);
    if ((speed < 0) || (course > 360) || (course < 0)) {
      continue;
    }

    Waypoint* wpt = new Waypoint;

    wpt->latitude = lat;
    wpt->longitude = lon;

    wpt->SetCreationTime(QDateTime(date, time, Qt::UTC));

    WAYPT_SET(wpt, course, course);
    WAYPT_SET(wpt, speed, speed);

#if 0		/* unsure, not validated items */
    wpt->fix = buff[TR7_S_FIX];
    if (buff[TR7_S_VALID] != 'A') {
      delete wpt;
      continue;
    }
#endif
    if (waypt_speed(prev, wpt) > 9999.9) {	/* filter out some bad trackpoints */
      delete wpt;
      continue;
    }

    if (prev) {	/* other track or bad timestamp */
      if (wpt->creation_time.isValid() && (prev->creation_time.toTime_t() > wpt->creation_time.toTime_t())) {
        trk = nullptr;
      } else if (waypt_distance(prev, wpt) > 9999.9) {
        trk = nullptr;
      }
    }

    if (! trk) {
      trk = route_head_alloc();
      track_add_head(trk);
    }
    track_add_wpt(trk, wpt);
    prev = wpt;
  }
}

static void
tr7_check_after_read_head_cb(const route_head* trk)
{
  trk_tmp = trk;
  course_tmp = 0;
  speed_tmp = 0;
}

static void
tr7_check_after_read_wpt_cb(const Waypoint* wpt)
{
  if (wpt->speed != 0) {
    speed_tmp = 1;
  }
  if (wpt->course != 360.0) {
    course_tmp = 1;
  }
}

static void
tr7_check_after_read_trailer_cb(const route_head* trk)
{
  foreach (Waypoint* wpt, trk->waypoint_list) {
    if (speed_tmp == 0) {
      WAYPT_UNSET(wpt, speed);
    }
    if (course_tmp == 0) {
      WAYPT_UNSET(wpt, course);
      wpt->course = 0;
    }
  }
}

static void
tr7_rd_deinit()
{
  track_disp_session(curr_session(),
                     tr7_check_after_read_head_cb,
                     tr7_check_after_read_trailer_cb,
                     tr7_check_after_read_wpt_cb);
  gbfclose(fin);
}

/*******************************************************************************
* %%%                             W R I T E R                              %%% *
*******************************************************************************/

static void
tr7_disp_track_head_cb(const route_head*)
{
  wpt_tmp = nullptr;
}

static void
tr7_disp_waypt_cb(const Waypoint* wpt)
{
  unsigned char buff[TR7_S_SIZE];
  double speed, course;

  memset(buff, 0, sizeof(buff));

  le_write32(&buff[TR7_S_LON], (int)(wpt->longitude * 1000000.0));
  le_write32(&buff[TR7_S_LAT], (int)(wpt->latitude * 1000000.0));

  if WAYPT_HAS(wpt, course) {
    course = wpt->course;
  } else if (wpt_tmp != nullptr) {
    course =  waypt_course(wpt_tmp, wpt);
  } else {
    course = -1;
  }
  if (course >= 0) {
    le_write16(&buff[TR7_S_COURSE], (int)(360 - course));
  }

  QDateTime dt = wpt->GetCreationTime().toUTC();
  if (dt.isValid()) {
    QDate d = dt.date();

    le_write16(&buff[TR7_S_YEAR], d.year());
    buff[TR7_S_MONTH] = d.month();
    buff[TR7_S_DAY] = d.day();

    QTime t = dt.time();
    buff[TR7_S_HOUR] = t.hour();
    buff[TR7_S_MIN] = t.minute();
    buff[TR7_S_SEC] = t.second();

    if WAYPT_HAS(wpt, speed) {
      speed = wpt->speed;
    } else if (wpt_tmp != nullptr) {
      speed = waypt_speed(wpt_tmp, wpt);
    } else {
      speed = -1;
    }
    if (speed >= 0) {
      le_write16(&buff[TR7_S_SPEED], (int)MPS_TO_KPH(speed));
    }
  }
  buff[TR7_S_VALID] = 'A';	/* meaning unknown */

#if 0	/* not validated */
  if (wpt->fix != fix_unknown) {
    buff[TR7_S_FIX] = wpt->fix;
  }
#endif
  gbfwrite(buff, 1, sizeof(buff), fout);

  wpt_tmp = wpt;
}

static void
tr7_wr_init(const QString& fname)
{
  fout = gbfopen_le(fname, "wb", MYNAME);
  gbfputint32(TR7_TRACK_MAGIC, fout);
}

static void
tr7_wr_deinit()
{
  gbfclose(fout);
}

static void
tr7_write()
{
  track_disp_all(tr7_disp_track_head_cb, nullptr, tr7_disp_waypt_cb);
}

/**************************************************************************/

ff_vecs_t mapasia_tr7_vecs = {		/* we can read and write tracks */
  ff_type_file,
  {
    ff_cap_none 			/* waypoints */,
    (ff_cap)(ff_cap_read | ff_cap_write)	/* tracks */,
    ff_cap_none			/* routes */
  },
  tr7_rd_init,
  tr7_wr_init,
  tr7_rd_deinit,
  tr7_wr_deinit,
  tr7_read,
  tr7_write,
  nullptr,
  tr7_args,
  CET_CHARSET_UTF8, 1	/* FIXED - CET-REVIEW - */
  , NULL_POS_OPS,
  nullptr

};

/**************************************************************************/
