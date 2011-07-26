/*

	Support of Palm/OS files from Map&Guide based products like
	"PowerRoute" 5+6, "Motorrad Routenplaner"

	Copyright (C) 2005 Olaf Klein, o.b.klein@gpsbabel.org

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

#include "defs.h"
#if PDBFMTS_ENABLED
#include "pdbfile.h"
#include "jeeps/gpsmath.h"

#define MYNAME "mag_pdb"

#define PROUTE_MAGIC	0x766d6170 		/* vmap */
#define PROUTE_ROUTE	0x49444154		/* IDAT */

static pdbfile* file_in;

static arglist_t magpdb_args[] = {
  ARG_TERMINATOR
};

static double
magpdb_to_degree(const int degx)
{
  int m, d, x;
  double s, res;

  d = degx / 100000;
  x = degx % 100000;
  m = x / 1000;
  x = x % 1000;
  s = (double)(x) / 10;

  GPS_Math_DegMinSec_To_Deg(d, m, s, &res);

  return res;
}

static void
magpdb_read_data(const char* data, const size_t data_len)
{
  route_head* route;
  char* cin = (char*)data;
  char* cend = cin + data_len;

  route = route_head_alloc();
  route_add_head(route);

  while (cin < cend) {
    char* lend;
    int len;

    lend = strchr(cin, '\x0A');
    if (lend == NULL) {
      break;
    }

    len = (lend - cin);
    if (len > 0) {
      double distance;
      int hour, min;
      *lend = '\0';

      if (case_ignore_strncmp(cin, "Wegname=", 8) == 0) {	/* This only works with the german release */
        /* test-data created with other releases are welcome */
        cin += 8;
        if (*cin != '\0') {
          route->rte_name = xstrdup(cin);
        }
      } else if (case_ignore_strncmp(cin, "Fahrzeit=", 9) == 0) {
      } else if (case_ignore_strncmp(cin, "Kosten=", 7) == 0) {
      } else if (case_ignore_strncmp(cin, "Entfernung=", 11) == 0) {
      }
      /* check, if line starts with time and distance */
      else if (3 == sscanf(cin, "%d:%d %lf", &hour, &min, &distance)) {
        char* buff, *comma;

        /* detect time-format settings, 12,0 or 12.0 */

        comma = strchr(cin, '.');
        buff = strchr(cin, ',');
        if (comma == NULL) {
          comma = buff;
        } else if ((buff != NULL) && (buff < comma)) {
          comma = buff;
        }
        if (comma != NULL) {
          char separator = *comma;

          /* now we are looking for a sequence like 0,1 NE (123456,654321) */

          buff = xmalloc(strlen(cin) + 1);		/* safe target space for sscanf( ... */

          comma = cin;
          while ((comma = strchr(comma, separator))) {
            int i, xlat, xlon;
            waypoint* wpt;
            char* cx;

            comma++;

            if (isdigit(*comma) == 0) {
              continue;
            }
            if (isdigit(*(comma - 2)) == 0) {
              continue;
            }

            if (4 != sscanf(comma, "%d %s (%d,%d)", &i, buff, &xlon, &xlat)) {
              continue;
            }
            if (strchr("NESW", *buff) == NULL) {
              continue;  /* north, east, ... */
            }

            cx = comma - 2;				/* go left over delta distance */
            while (isdigit(*cx) != 0) {
              *cx-- = '\0';
            }
            cin = lrtrim(cin);

            for (i = 0; i < 2; i++) {		/* skip time and distance at start of line */
              cin = strchr(cin, ' ');
              cin = lrtrim(cin);
            }

            wpt = waypt_new();

            wpt->latitude = magpdb_to_degree(xlat);
            wpt->longitude = magpdb_to_degree(xlon);
            wpt->description = xstrdup(cin);

            cx = strchr(comma, ')');		/* find tailing notes after the coordinates */
            if (cx != NULL) {
              char* tail = lrtrim(++cx);
              if (*tail != '\0') {
                wpt->notes = xstrdup(tail);
              }
            }
            /* generate some waypoints from our route-only format */
            if ((*cin != '-') && (case_ignore_strncmp(cin, "bei ", 4) != 0)) {
              waypt_add(waypt_dupe(wpt));
            }

            route_add_wpt(route, wpt);
            break;
          }
          xfree(buff);
        }
      }

    }
    cin = lend + 1;
  }
}

/* ============================================================================================
 * &&& gobal callbacks &&&
 * ----------------------------------------------------------------------------------------- */

static void magpdb_rd_init(const char* fname)
{
  file_in = pdb_open(fname, MYNAME);
}

static void magpdb_rd_deinit(void)
{
  pdb_close(file_in);
}

static void magpdb_read(void)
{
  pdbrec_t* pdb_rec;

  is_fatal((file_in->creator != PROUTE_MAGIC),	/* identify the database */
           MYNAME ": Not a Map&Guide pdb file (0x%08x).", file_in->creator);

  is_fatal((file_in->version != 0), 		/* only version "0" currently seen and tested */
           MYNAME ": This file is from an unsupported version (%d) of Map&Guide and is unsupported.", file_in->version + 5);

  is_fatal((file_in->type != PROUTE_ROUTE),
           MYNAME ": Unknown pdb data type (0x%08x).", file_in->type);

  for (pdb_rec = file_in->rec_list; pdb_rec; pdb_rec = pdb_rec->next) {
    char* data = (char*)pdb_rec->data;

    if (be_read16(data) == 0) {
      int len = be_read16(data + 2);
      magpdb_read_data(data + 4, len);
    }
  }
}

/* ======================================================================================= */

ff_vecs_t magpdb_vecs = {
  ff_type_file,
  { ff_cap_read, ff_cap_none, ff_cap_read },	/* real route + emulated waypoints */
  magpdb_rd_init,
  NULL,
  magpdb_rd_deinit,
  NULL,
  magpdb_read,
  NULL,
  NULL,
  magpdb_args,
  CET_CHARSET_MS_ANSI, 1	/* CET-REVIEW */
};
#endif
