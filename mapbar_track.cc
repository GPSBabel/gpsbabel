/*
    China mapbar navigation track reader for sonim xp3300
       it's maybe can used in other demo devices of mapbar navigation

    Copyright (C) 2013 xiao jian cheng, azuresky.xjc@gmail.com
    Copyright (C) 2001-2013 Robert Lipe, robertlipe+source@gpsbabel.org

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
#include <QtCore/QDebug>

#define MYNAME "mapbar_track"

static gbfile* fin;

static
arglist_t mapbar_track_args[] = {
  ARG_TERMINATOR
};

/*******************************************************************************
* %%%        global callbacks called by gpsbabel main process              %%% *
*******************************************************************************/

static void
mapbar_track_rd_init(const QString& fname)
{
  fin = gbfopen(fname, "r", MYNAME);
}

static void
mapbar_track_rd_deinit()
{
  gbfclose(fin);
}

static gpsbabel::DateTime
read_datetime()
{
  int hour = gbfgetint16(fin);
  int min = gbfgetint16(fin);
  int sec = gbfgetint16(fin);
  int year = gbfgetint16(fin);
  int mon = gbfgetint16(fin);
  int mday = gbfgetint16(fin);
  gpsbabel::DateTime t(QDate(year, mon, mday), QTime(hour, min, sec));
// qDebug() << t;
  return t;
}

static const double DIV_RATE  = 100000.0f;
static Waypoint*
read_waypoint()
{
  int longitude = gbfgetint32(fin);
  int latitude = gbfgetint32(fin);

  Waypoint* ret = new Waypoint;

  ret->latitude = double(latitude)/DIV_RATE;
  ret->longitude = double(longitude)/DIV_RATE;

  return ret;
}

static void
mapbar_track_read()
{
  route_head* track = route_head_alloc();
  is_fatal((track == nullptr), MYNAME ": memory non-enough");
  track_add_head(track);

  (void) read_datetime(); // start_time currently unused
  (void) read_datetime(); // end_time currently unused

  ushort name[101];
  // read 100 UCS-2 characters that are each stored little endian.
  // note gbfread wouldn't get this right on big endian machines.
  for (int idx=0; idx<100; idx++) {
    name[idx] = gbfgetint16(fin);
  }
  name[100] = 0;
  // At this point, name is a UCS-2 encoded, zero terminated string.
  // All our internals use Qt encoding, so convert now.
  track->rte_name = QString::fromUtf16(name);

  // skip two pair waypoint
  gbfseek(fin, 8*4, SEEK_CUR);
  // skip way length
  gbfseek(fin, 8, SEEK_CUR);
  // skip fixed value
  gbfseek(fin, 4, SEEK_CUR);

  int end_flag = gbfgetint32(fin);
  for (;;) {
    if (end_flag) {
      break;
    }

    int length = gbfgetint32(fin);
    is_fatal((length < 1) || (length > 1600), MYNAME ": get bad buffer length");

    is_fatal((length % 8 != 0), MYNAME ": bad buffer size");
    gbfseek(fin, 16, SEEK_CUR);

    const int amount = length/8;
    for (int i = 0; i < amount; ++i) {
      Waypoint* tmp = read_waypoint();
      track_add_wpt(track, tmp);
    }

    end_flag = gbfgetint32(fin);
  }
}

// capabilities below means: we can only read trackpoints.

ff_vecs_t mapbar_track_vecs = {
  ff_type_file,
  { ff_cap_none, (ff_cap)(ff_cap_read), ff_cap_none },
  mapbar_track_rd_init,
  nullptr,
  mapbar_track_rd_deinit,
  nullptr,
  mapbar_track_read,
  nullptr,
  nullptr,
  mapbar_track_args,
  CET_CHARSET_UTF8, 0
  /* not fixed, can be changed through command line parameter */
  , NULL_POS_OPS,
  nullptr
};
