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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

 */

#include "mapbar_track.h"

#include <QChar>                // for QChar
#include <QDate>                // for QDate
#include <QString>              // for QString
#include <QTime>                // for QTime

#include <cstdio>               // for SEEK_CUR

#include "defs.h"               // for fatal, Waypoint, track_add_head, track_add_wpt, route_head
#include "gbfile.h"             // for gbfgetint16, gbfgetint32, gbfseek, gbfclose, gbfopen
#include "src/core/datetime.h"  // for DateTime


#define MYNAME "mapbar_track"

/*******************************************************************************
* %%%        global callbacks called by gpsbabel main process              %%% *
*******************************************************************************/

void
MapbarTrackFormat::rd_init(const QString& fname)
{
  fin = gbfopen(fname, "r", MYNAME);
}

void
MapbarTrackFormat::rd_deinit()
{
  gbfclose(fin);
}

gpsbabel::DateTime
MapbarTrackFormat::read_datetime()
{
  int hour = gbfgetint16(fin);
  int min = gbfgetint16(fin);
  int sec = gbfgetint16(fin);
  int year = gbfgetint16(fin);
  int mon = gbfgetint16(fin);
  int mday = gbfgetint16(fin);
  gpsbabel::DateTime t(QDate(year, mon, mday), QTime(hour, min, sec));
  return t;
}

Waypoint*
MapbarTrackFormat::read_waypoint()
{
  int longitude = gbfgetint32(fin);
  int latitude = gbfgetint32(fin);

  auto* ret = new Waypoint;

  ret->latitude = double(latitude)/DIV_RATE;
  ret->longitude = double(longitude)/DIV_RATE;

  return ret;
}

void
MapbarTrackFormat::read()
{
  auto* track = new route_head;
  track_add_head(track);

  (void) read_datetime(); // start_time currently unused
  (void) read_datetime(); // end_time currently unused

  QChar name[101];
  // read 100 UCS-2 characters that are each stored little endian.
  // note gbfread wouldn't get this right on big endian machines.
  for (int idx=0; idx<100; idx++) {
    name[idx] = gbfgetint16(fin);
  }
  name[100] = u'\0';
  // At this point, name is a UCS-2 encoded, zero terminated string.
  // All our internals use Qt encoding, so convert now.
  track->rte_name = QString(name);

  // skip two pair waypoint
  gbfseek(fin, 8*4, SEEK_CUR);
  // skip way length
  gbfseek(fin, 8, SEEK_CUR);
  // skip fixed value
  gbfseek(fin, 4, SEEK_CUR);

  int end_flag = gbfgetint32(fin);
  while (!end_flag) {
    int length = gbfgetint32(fin);
    if ((length < 1) || (length > 1600)) {
      fatal(MYNAME ": get bad buffer length");
    }

    if (length % 8 != 0) {
      fatal(MYNAME ": bad buffer size");
    }
    gbfseek(fin, 16, SEEK_CUR);

    const int amount = length/8;
    for (int i = 0; i < amount; ++i) {
      Waypoint* tmp = read_waypoint();
      track_add_wpt(track, tmp);
    }

    end_flag = gbfgetint32(fin);
  }
}
