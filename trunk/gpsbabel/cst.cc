/*

    Support for CarteSurTable data file,

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


#include "defs.h"
#include "strptime.h"
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MYNAME "cst"

#undef CST_DEBUG

#define CST_UNKNOWN	0
#define CST_HEADER	1
#define CST_ROUTE	2
#define CST_NOTES	3
#define CST_REFERENCE	4
#define CST_VERSION	5

static gbfile* fin;

static route_head* temp_route;

/* placeholders for options */

static
arglist_t cst_args[] = {
  ARG_TERMINATOR
};

/* helpers */

static void
cst_add_wpt(const route_head* track, waypoint* wpt)
{
  if ((wpt == NULL) || (track == NULL)) {
    return;
  }

  if (wpt->shortname != NULL) {
    waypt_add(waypt_dupe(wpt));
    // Rather than creating a new waypt on each read, tis format bizarrely
    // recycles the same one, relying on waypt_dupe() above and then manually
    // resetting fields.  Weird.
    wpt->url_link_list_.clear();

    if (temp_route == NULL) {
      temp_route = route_head_alloc();
      route_add_head(temp_route);
    }
    route_add_wpt(temp_route, waypt_dupe(wpt));
  }
  track_add_wpt((route_head*)track, (waypoint*)wpt);
}

static char*
cst_make_url(char* str)
{
  int len = strlen(str);
  char* res;

  if (len < 3) {
    return NULL;
  }

  if (strstr(str, "://") > str) {
    return xstrdup(str);
  } else if (strstr(str, ":\\") == str+1) {	/* DOS 0.01++ file format */
    res = xstrdup("file://*:");
    res[7] = *str++;
    res[8] = *str++;
    res = xstrappend(res, str);
    {
      char* c;
      int i;

      c = res;			/* replace all backslashes with a slash */
      while ((c = strchr(c, '\\'))) {
        *c++ = '/';
      }

      c = res;			/* enumerate number of spaces within filename */
      i = 0;
      while ((c = strchr(c, ' '))) {
        c++;
        i++;
      }

      if (i > 0) {		/* .. and replace them with "%20" */
        char* src, *dest, *last;

        last = src = res;
        res = dest = (char*) xcalloc(strlen(src) + (2*i) + 1, 1);
        while ((c = strchr(src, ' '))) {
          if (c != src) {
            strncpy(dest, src, c - src);
          }
          strcat(dest, "%20");
          c++;
          src = c;
          dest = res + strlen(res);
        }
        while (*src != '\0') {
          *dest++ = *src++;
        }
        xfree(last);
      }
    }
    return res;

  } else {
    return NULL;
  }

}

/* --------------------------------------------------------------------------- */

static void
cst_rd_init(const char* fname)
{
  fin = gbfopen(fname, "rb", MYNAME);
  temp_route = NULL;
}

static void
cst_rd_deinit(void)
{
  gbfclose(fin);
}

/* --------------------------------------------------------------------------- */

static void
cst_data_read(void)
{
  char* buff;
  int line = 0;
  int data_lines = -1;
  int line_of_count = -1;
  int valid = 0;
  int section = CST_UNKNOWN;
  int cst_version;
  int cst_points = -1;
  route_head* track = NULL;
  waypoint* wpt = NULL;

  while ((buff = gbfgetstr(fin))) {
    char* cin = buff;

    if ((line++ == 0) && fin->unicode) {
      cet_convert_init(CET_CHARSET_UTF8, 1);
    }
    cin = lrtrim(buff);
    if (strlen(cin) == 0) {
      continue;
    }

    if (strncmp(cin, "; ", 2) == 0) {
      continue;
    }
    if (*cin == '#') {
      section = CST_UNKNOWN;
      if (strcmp(cin+1, "ROUTE") == 0) {
        section = CST_ROUTE;
      } else if (strcmp(cin+1, "VERSION") == 0) {
        section = CST_VERSION;
      } else if (strcmp(cin+1, "NOTES") == 0) {
        section = CST_NOTES;
      } else if (strcmp(cin+1, "REFERENCE") == 0) {
        section = CST_REFERENCE;
      } else if (strcmp(cin+1, "CARTE SUR TABLE DATA FILE") == 0) {
        section = CST_HEADER;
        valid = 1;
      } else {
        warning(MYNAME ": Unknown section \"%s\".\n", cin+1);
      }

      continue;
    }

    if (valid == 0) {
      continue;
    }

    switch (section) {
    case CST_ROUTE:
      if (*cin == ';') {
        int data = 0;

        if (*(cin+1) != '\xA4') {
          continue;
        }

        if (strncmp(cin + 2, "bitmap", 6) == 0) {
          cin = lrtrim(cin + 8);
          if (*cin != '\0') {
            char* url = cst_make_url(cin);
            wpt->AddUrlLink(url);
            if (url) {
              xfree(url);
            }
          }
        }

        while ((buff = gbfgetstr(fin))) {
          line++;
          cin = lrtrim(buff);

          if (strcmp(cin + 2, "note") == 0) {
            buff = gbfgetstr(fin);
            if (buff == NULL) {
              buff = (char*) "";
            }
            line++;
            cin = lrtrim(buff);
            if (*cin != '\0') {
              wpt->notes = QString::fromLatin1(cin);
            }
          } else if (strcmp(cin + 2, "end") == 0) {
            data = 1;
            break;
          }
        }
        if (data == 0) {
          fatal(MYNAME ": Unexpected end of file!\n");
        }
      } else {
        int interp, i;
        char name[256];
        char* pow;

        if (data_lines < 0) {
          if ((2 != sscanf(cin, "%d %128s", &i, name)) ||
              (case_ignore_strcmp(name, "Points") != 0)) {
            fatal(MYNAME "-line %d: Number of points expected!\n", line);
          }
          line_of_count = line;
          data_lines = 0;
          cst_points = i;
          continue;
        }

        cst_add_wpt(track, wpt);
        wpt = NULL;


        wpt = waypt_new();

        if (5 != sscanf(cin, "%lf %lf %lf %d %s",
                        &wpt->longitude,
                        &wpt->latitude,
                        &wpt->altitude,
                        &interp, name)) {
          fatal(MYNAME ": Could not interprete line %d!\n", line);
        }

        data_lines++;

        if (strcmp(name, "1") == 0) {
          track = route_head_alloc();
          track_add_head(track);
        } else if (strncmp(name, "NAME:", 5) == 0) {
          wpt->shortname = QString::fromLatin1(((char*)&name) + 5);
        }

        pow = strrchr(cin, '^');
        if (pow != NULL) {
          struct tm tm;

          pow = lrtrim(++pow);
          strptime(pow, "%Y %m %d %H:%M:%S", &tm);

          wpt->SetCreationTime(mkgmtime(&tm));
        }
        wpt->latitude /= 100000.0;
        wpt->longitude /= 100000.0;
      }
      break;


    case CST_VERSION:
      cst_version = atoi(cin);
      if (cst_version != 40) {
        warning(MYNAME ": Not tested with file version %d.\n", cst_version);
      }
      break;

    case CST_REFERENCE:
      if ((strncmp(cin, "DATUM ", 6) == 0) && (strstr(cin, "WGS 84") == NULL)) {
        fatal(MYNAME ": Unsupported datum (%s)!\n", cin);
      }
      break;

    case CST_HEADER:
    case CST_NOTES:
      break;
    }
  }
  cst_add_wpt(track, wpt);
  wpt = NULL;

  if ((cst_points >= 0) && (data_lines != cst_points)) {
    warning(MYNAME ": Loaded %d point(s), but line %d says %d!\n", data_lines, line_of_count, cst_points);
  }
}

ff_vecs_t cst_vecs = {
  ff_type_file,
  { ff_cap_read, ff_cap_read, ff_cap_read },
  cst_rd_init,
  NULL, 		/* cst_wr_init, */
  cst_rd_deinit,
  NULL,		/* cst_wr_deinit, */
  cst_data_read,
  NULL,		/* cst_data_write, */
  NULL,
  cst_args,
  CET_CHARSET_MS_ANSI, 0	/* CET-REVIEW */
};
