/*

    teletype .way module

    Copyright (C) 2002-2014 Robert Lipe

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

#include "teletype.h"

#include <cstdio>   // for SEEK_CUR
#include "defs.h"   // for Waypoint, waypt_add


#define MYNAME "teletype"


/*******************************************************************************
* %%%        global callbacks called by gpsbabel main process              %%% *
*******************************************************************************/

void
TeletypeFormat::rd_init(const QString& fname)
{
  char header[64];

  fin = gbfopen(fname, "r", MYNAME);

  gbfread(header, sizeof(header), 1, fin);
  tty_wpt_count = gbfgetint32(fin);
}

void
TeletypeFormat::rd_deinit()
{
  gbfclose(fin);
}

void
TeletypeFormat::read()
{
  for (uint32_t i = 0; i < tty_wpt_count; i++) {
    auto* wpt = new Waypoint;
    wpt->shortname = (gbfgetcstr(fin));
    wpt->description = (gbfgetcstr(fin));

    if (true) { // needs bit values of NEWFORMAT2
      uint32_t direction = gbfgetuint32(fin);
      uint32_t mins = gbfgetuint32(fin);
      (void) direction ;
      (void) mins ;
    }

    if (true) {  // need bit value of NEWFORMAT
      int len = gbfgetuint16(fin);
      gbfseek(fin, len, SEEK_CUR);
    }
    wpt->latitude = gbfgetint32(fin) / 1000000.0 ;
    wpt->longitude = gbfgetint32(fin) / 1000000.0 ;
    gbfseek(fin, 21, SEEK_CUR);

    waypt_add(wpt);
  }
}
