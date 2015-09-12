/*
	PocketFMS waypoint text files (wpt).

	Copyright (C) 2009 Tobias Kretschmar, tobias.kretschmar@gmx.de

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
#include <stdlib.h>
#include <stdio.h>

#define MYNAME "PocketFMS waypoint text file format"

static gbfile* file_in, *file_out;

static void
rd_init(const char* fname)
{
  file_in = gbfopen_le(fname, "r", MYNAME);
}

double wppos_to_dec(char* value)
{
  if (strstr(value, "\xB0") == NULL) {
    return atof(value);
  } else {
    int degrees, minutes;
    float seconds;
    int sign = 1;

    if (toupper(value[0]) == 'N' || toupper(value[0]) == 'E' || value[0] == '+') {
      value = &value[1];
    } else if (toupper(value[0]) == 'S' || toupper(value[0]) == 'W' || value[0] == '-') {
      value = &value[1];
      sign = -1;
    }

    sscanf(value, "%d\xB0%d'%f\"", &degrees, &minutes, &seconds);
    return sign * (degrees + ((float)minutes / 60) + (seconds / 3600));
  }
}

static void
data_read(void)
{
  char* buff;
  int linecount = 0;
  while ((buff = gbfgetstr(file_in))) {
    char* s;
    Waypoint* wpt;
    rtrim(buff);
    if (strlen(buff) == 0) {
      break;
    }
    linecount++;
    wpt = new Waypoint;
    s = buff;
    s = csv_lineparse(s, "\\w", "", linecount);
    if (!s) {
      fatal(MYNAME "Invalid name");
    }
    wpt->shortname = s;
    s = csv_lineparse(NULL, "\\w", "", linecount);
    if (!s) {
      fatal(MYNAME "Invalid latitude %s", qPrintable(wpt->shortname));
    }
    wpt->latitude = wppos_to_dec(s);

    s = csv_lineparse(NULL, "\\w", "", linecount);
    if (!s) {
      fatal(MYNAME "Invalid longitude %s", qPrintable(wpt->shortname));
    }
    wpt->longitude = wppos_to_dec(s);
    waypt_add(wpt);
  }
}

static void
rd_deinit(void)
{
  gbfclose(file_in);
}

static void
wr_init(const char* fname)
{
  file_out = gbfopen_le(fname, "w", MYNAME);
}

static void
enigma_waypt_disp(const Waypoint* wpt)
{
  if (!wpt->shortname.isEmpty()) {
    // The output might have a space or control character.
    int i, l = wpt->shortname.length();
    char *t = (char*) xmalloc(l + 1);
    char* d = t;
    for (i = 0; i < l; i++) {
      char s = wpt->shortname[i].cell();
      if (isgraph(s)) {
        *d++ = s;
      }
    }
    *d = 0;
    gbfprintf(file_out, "%s %f %f\n", t, wpt->latitude, wpt->longitude);
    xfree(t);
  } else {
    gbfprintf(file_out, "%s %f %f\n", "NONAME", wpt->latitude, wpt->longitude);
  }
}

static void
data_write(void)
{
  waypt_disp_all(enigma_waypt_disp);
}

static void
wr_deinit(void)
{
  gbfclose(file_out);
}

ff_vecs_t pocketfms_wp_vecs = {
  ff_type_file,
  {
    (ff_cap)(ff_cap_read | ff_cap_write),  	/* waypoints */
    ff_cap_none,                    /* tracks */
    ff_cap_none,			/* routes */
  },
  rd_init,
  wr_init,
  rd_deinit,
  wr_deinit,
  data_read,
  data_write,
  NULL,
  NULL,
  CET_CHARSET_ASCII, 0	/* CET-REVIEW */
};
