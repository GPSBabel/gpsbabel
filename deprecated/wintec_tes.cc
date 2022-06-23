/*

    Wintec tes support.

    Copyright (C) 2010  Robert Lipe, robertlipe+source@gpsbabel.org

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

#include "wintec_tes.h"

#include <ctime>                   // for time_t, tm
#include <cstring>                 // for memset

#include "defs.h"                  // for Waypoint, mkgmtime, track_add_head, track_add_wpt, waypt_add, route_head


#define MYNAME "wintec_tes"

void
WintecTesFormat::rd_init(const QString& fname)
{
  fin = gbfopen(fname, "r", MYNAME);
}

void
WintecTesFormat::rd_deinit()
{
  gbfclose(fin);
}

time_t
WintecTesFormat::wintec_date_to_time(uint32_t w)
{
  struct tm tm;
  memset(&tm, 0, sizeof(tm));
  tm.tm_sec  = ((w & 0x0000003f));
  tm.tm_min  = ((w & 0x00000fc0) >> 6);
  tm.tm_hour = ((w & 0x0001f000) >> 12);
  tm.tm_mday = ((w & 0x003f0000) >> 17);
  tm.tm_mon  = ((w & 0x03c00000) >> 22) - 1;
  tm.tm_year = ((w & 0xfc000000) >> 26) + 100;

  return mkgmtime(&tm);
}

void
WintecTesFormat::read()
{
  auto* trk = new route_head;
  track_add_head(trk);

  while (!gbfeof(fin)) {
    uint16_t flags = gbfgetuint16(fin);
    uint32_t date = gbfgetuint32(fin);
    int32_t latitude = gbfgetint32(fin);
    int32_t longitude = gbfgetint32(fin);
    int16_t alt = gbfgetint16(fin);  // Signed.  Meters.

    (void) flags; // Silence 'unused' warning until we use flags.
    auto* wpt = new Waypoint;
    wpt->latitude = latitude / 1.0e7;
    wpt->longitude = longitude / 1.0e7;
    wpt->SetCreationTime(wintec_date_to_time(date));
    // The unit of altitude isn't clear and we have a lot of
    // samples with wildly negative values, so ignore those for now.
    wpt->altitude = alt;

    // The description given to us says this is a bitmask with
    //  0x01 "split mark" (not at all clear what that is)
    //  0x02 interest point
    //  0x04 track point
    //  But of the files we've seen, none have had > 1 bit set
    //  and none have had 0x04 set.
    //  Wintec's software puts a waypoint in the track, so we
    //  mock that.
    if (flags &  0x02) {
      auto* temp = new Waypoint(*wpt);
      waypt_add(temp);
    }

    track_add_wpt(trk, wpt);
  }
}
