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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
*/


#include "defs.h"
#include "cet_util.h"
#include <cstdio>
#include <cstdlib>

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
QVector<arglist_t> cst_args = {
};

/* helpers */

static void
cst_add_wpt(route_head* track, Waypoint* wpt)
{
  if ((wpt == nullptr) || (track == nullptr)) {
    return;
  }

  if (wpt->shortname != nullptr) {
    waypt_add(new Waypoint(*wpt));
    // Rather than creating a new waypt on each read, tis format bizarrely
    // recycles the same one, relying on new waypoint(*) above and then manually
    // resetting fields.  Weird.
    wpt->urls.clear();

    if (temp_route == nullptr) {
      temp_route = new route_head;
      route_add_head(temp_route);
    }
    route_add_wpt(temp_route, new Waypoint(*wpt));
  }
  track_add_wpt(track, wpt);
}

static char*
cst_make_url(char* str)
{
  int len = strlen(str);

  if (len < 3) {
    return nullptr;
  }

  if (strstr(str, "://") > str) {
    return xstrdup(str);
  } else if (strstr(str, ":\\") == str+1) {	/* DOS 0.01++ file format */
    char* res = xstrdup("file://*:");
    res[7] = *str++;
    res[8] = *str++;
    res = xstrappend(res, str);
    {
      char* c = res;			/* replace all backslashes with a slash */
      while ((c = strchr(c, '\\'))) {
        *c++ = '/';
      }

      c = res;			/* enumerate number of spaces within filename */
      int i = 0;
      while ((c = strchr(c, ' '))) {
        c++;
        i++;
      }

      if (i > 0) {		/* .. and replace them with "%20" */
        char* src, *dest;

        char* last = src = res;
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
    return nullptr;
  }

}

/* --------------------------------------------------------------------------- */

static void
cst_rd_init(const QString& fname)
{
  fin = gbfopen(fname, "rb", MYNAME);
  temp_route = nullptr;
}

static void
cst_rd_deinit()
{
  gbfclose(fin);
}

/* --------------------------------------------------------------------------- */

static void
cst_data_read()
{
  char* buff;
  int line = 0;
  int data_lines = -1;
  int line_of_count = -1;
  int valid = 0;
  int section = CST_UNKNOWN;
  int cst_version;
  int cst_points = -1;
  route_head* track = nullptr;
  Waypoint* wpt = nullptr;

  while ((buff = gbfgetstr(fin))) {
    if ((line++ == 0) && fin->unicode) {
      cet_convert_init(CET_CHARSET_UTF8, 1);
    }
    char* cin = lrtrim(buff);
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
            if (buff == nullptr) {
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

        wpt = new Waypoint;

        if (5 != sscanf(cin, "%lf %lf %lf %d %s",
                        &wpt->longitude,
                        &wpt->latitude,
                        &wpt->altitude,
                        &interp, name)) {
          fatal(MYNAME ": Could not interpret line %d!\n", line);
        }

        data_lines++;

        if (strcmp(name, "1") == 0) {
          track = new route_head;
          track_add_head(track);
        } else if (strncmp(name, "NAME:", 5) == 0) {
          wpt->shortname = QString::fromLatin1(name + 5);
        }

        QString time_string(cin);
        int caret = time_string.indexOf('^');
        if (caret > -1) {
          QString dts = time_string.mid(caret + 1).trimmed();
          QDateTime dt = QDateTime::fromString(dts, "yyyy MM dd hh:mm:ss");
          dt.setTimeSpec(Qt::UTC);
          wpt->SetCreationTime(dt);
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
      if ((strncmp(cin, "DATUM ", 6) == 0) && (strstr(cin, "WGS 84") == nullptr)) {
        fatal(MYNAME ": Unsupported datum (%s)!\n", cin);
      }
      break;

    case CST_HEADER:
    case CST_NOTES:
      break;
    }
  }
  cst_add_wpt(track, wpt);
  wpt = nullptr;

  if ((cst_points >= 0) && (data_lines != cst_points)) {
    warning(MYNAME ": Loaded %d point(s), but line %d says %d!\n", data_lines, line_of_count, cst_points);
  }
}

ff_vecs_t cst_vecs = {
  ff_type_file,
  { ff_cap_read, ff_cap_read, ff_cap_read },
  cst_rd_init,
  nullptr, 		/* cst_wr_init, */
  cst_rd_deinit,
  nullptr,		/* cst_wr_deinit, */
  cst_data_read,
  nullptr,		/* cst_data_write, */
  nullptr,
  &cst_args,
  CET_CHARSET_MS_ANSI, 0	/* CET-REVIEW */
  , NULL_POS_OPS,
  nullptr
};
