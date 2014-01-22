/*

    teletype .way module

    Copyright (C) 2009  Robert Lipe, robertlipe@gpsbabel.org

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

#define MYNAME "teletype"


static
arglist_t teletype_args[] = {
  ARG_TERMINATOR
};

/*******************************************************************************
* %%%        global callbacks called by gpsbabel main process              %%% *
*******************************************************************************/

static uint32_t tty_wpt_count;
static gbfile* fin;

static void
teletype_rd_init(const char* fname)
{
  char header[64];

  fin = gbfopen(fname, "r", MYNAME);

  gbfread(header, sizeof(header), 1, fin);
  tty_wpt_count = gbfgetint32(fin);
}

static void
teletype_rd_deinit(void)
{
  gbfclose(fin);
}

static void
teletype_read(void)
{
  uint32_t i;
  for (i = 0; i < tty_wpt_count; i++) {
    waypoint* wpt = new waypoint;
    wpt->shortname = (gbfgetcstr(fin));
    wpt->description = (gbfgetcstr(fin));

    if (1) { // needs bit values of NEWFORMAT2
      uint32_t direction = gbfgetuint32(fin);
      uint32_t mins = gbfgetuint32(fin);
      (void) direction ;
      (void) mins ;
    }

    if (1) {  // need bit value of NEWFORMAT
      int len = gbfgetuint16(fin);
      // probably could treat as a pascal string
      char* junk = (char*) xmalloc(len);
      gbfread(junk, len, 1, fin);
      xfree(junk);
    }
    wpt->latitude = gbfgetint32(fin) / 1000000.0 ;
    wpt->longitude = gbfgetint32(fin) / 1000000.0 ;

    {
      char jibberish[21];
      gbfread(jibberish, sizeof(jibberish), 1, fin);
    }


    waypt_add(wpt);
  }
}

static void
teletype_exit(void)		/* optional */
{
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
  NULL,
  teletype_rd_deinit,
  NULL,
  teletype_read,
  NULL,
  teletype_exit,
  teletype_args,
  CET_CHARSET_ASCII, 0			/* ascii is the expected character set */
  /* not fixed, can be changed through command line parameter */
};
/**************************************************************************/
