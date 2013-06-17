/*

    Track reader for "Dell Axim Navigation System" GPB files,

    Copyright (C) 2006 Olaf Klein, o.b.klein@gpsbabel.org

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

#include <ctype.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#include "defs.h"

#define MYNAME "axim_gpb"

#define RECORD_LEN	344

static gbfile* fin;

static
arglist_t axim_gpb_args[] = {
  ARG_TERMINATOR
};

static float
le_read32_float(const void* src)
{
  float f;
  gbint32 i;

  i = le_read32(src);
  memcpy(&f, &i, 4);

  return f;
}

static void
decode_buff(const char* buff, route_head* track)
{
  struct tm tm;
  double lat, lon, alt, dir;
  float vdop, hdop, pdop, spd, Uf1;
  int sats;
  waypoint* wpt;

  wpt = waypt_new();

  memset(&tm, '\0', sizeof(tm));

  tm.tm_year = le_read16((void*)(buff + 16));
  tm.tm_mon = le_read16((void*)(buff + 18));
  tm.tm_mday = le_read16((void*)(buff + 22));
  tm.tm_hour = le_read16((void*)(buff + 24));
  tm.tm_min = le_read16((void*)(buff + 26));
  tm.tm_sec = le_read16((void*)(buff + 28));
  lat = le_read_double((void*)(buff + 32));
  lon = le_read_double((void*)(buff + 40));
  spd = le_read32_float((void*)(buff + 48));
  dir = le_read32_float((void*)(buff + 52));

  alt = le_read32_float((void*)(buff + 64));
  Uf1 = le_read32_float((void*)(buff + 68));

  hdop = le_read32_float((void*)(buff + 84));
  vdop = le_read32_float((void*)(buff + 88));
  pdop = le_read32_float((void*)(buff + 92));
  sats = le_read16((void*)(buff + 96));

  wpt->latitude = lat;
  wpt->longitude = lon;
  wpt->altitude = alt;
#if 0
  /* These values can be, but must not be right. */
  /* Further checks are needed to verify that. */
  /*         (!!! reference data !!!)          */
  WAYPT_SET(wpt, course, dir);
  wpt->hdop = hdop;
  wpt->vdop = vdop;
  wpt->pdop = pdop;
  wpt->sat = sats;
  WAYPT_SET(wpt, speed, spd * 10);
#endif
  /* We don't have a header with some magic fixed numbers or strings. */
  /* So let us check the range for some basic values */

  is_fatal(
    (tm.tm_year < 2005) ||
    (tm.tm_mon < 1) || (tm.tm_mon > 12) ||
    (tm.tm_mday < 1) || (tm.tm_mday > 31) ||
    (tm.tm_hour > 23) || (tm.tm_min > 60) || (tm.tm_sec > 60),
    MYNAME ": Invalid or unsupported file (invalid time-stamp).");
  is_fatal(
    (fabs(wpt->latitude) > 90) ||
    (fabs(wpt->longitude) > 180),
    MYNAME ": Invalid or unsupported file (lat or/and lon out of range).");

  /* post work */

  tm.tm_year-=1900;
  tm.tm_mon--;
  wpt->SetCreationTime(mkgmtime(&tm));

  track_add_wpt(track, wpt);
}

/*******************************************************************************
* %%%        global callbacks called by gpsbabel main process              %%% *
*******************************************************************************/

static void
axim_gpb_rd_init(const char* fname)
{
  fin = gbfopen(fname, "rb", MYNAME);
}

static void
axim_gpb_rd_deinit(void)
{
  gbfclose(fin);
}

static void
axim_gpb_read(void)
{
  char buff[RECORD_LEN];
  route_head* track = NULL;
  size_t bytes;

  while ((bytes = gbfread(buff, 1, RECORD_LEN, fin))) {
    is_fatal((bytes != RECORD_LEN), MYNAME ": Invalid or unsupported file (filesize).");
    if (track == NULL) {
      track = route_head_alloc();
      track_add_head(track);
    }
    decode_buff(buff, track);
  }
}

/**************************************************************************/

ff_vecs_t axim_gpb_vecs = {
  ff_type_file,
  {
    ff_cap_none 	/* waypoints */,
    ff_cap_read 	/* tracks */,
    ff_cap_none 	/* routes */,
  },
  axim_gpb_rd_init,
  NULL,
  axim_gpb_rd_deinit,
  NULL,
  axim_gpb_read,
  NULL,
  NULL,
  axim_gpb_args,
  CET_CHARSET_ASCII, 0
};
/**************************************************************************/
