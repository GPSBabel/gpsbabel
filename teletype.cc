/*

    teletype .way module

    Copyright (C) 2002-2014 Robert Lipe, robertlipe+source@gpsbabel.org

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

#define MYNAME "teletype"


static
QVector<arglist_t> teletype_args = {
};

/*******************************************************************************
* %%%        global callbacks called by gpsbabel main process              %%% *
*******************************************************************************/

static uint32_t tty_wpt_count;
static gbfile* fin;

static void
teletype_rd_init(const QString& fname)
{
  char header[64];

  fin = gbfopen(fname, "r", MYNAME);

  gbfread(header, sizeof(header), 1, fin);
  tty_wpt_count = gbfgetint32(fin);
}

static void
teletype_rd_deinit()
{
  gbfclose(fin);
}

static void
teletype_read()
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

/**************************************************************************/

// capabilities below means: we can only read and write waypoints
// please change this depending on your new module

ff_vecs_t teletype_vecs = {
  ff_type_file,
  {
    (ff_cap)(ff_cap_read) 	/* waypoints */,
    ff_cap_none 			/* tracks */,
    ff_cap_none 			/* routes */
  },
  teletype_rd_init,
  nullptr,
  teletype_rd_deinit,
  nullptr,
  teletype_read,
  nullptr,
  nullptr,
  &teletype_args,
  CET_CHARSET_ASCII, 0			/* ascii is the expected character set */
  /* not fixed, can be changed through command line parameter */
  , NULL_POS_OPS,
  nullptr
};
/**************************************************************************/
