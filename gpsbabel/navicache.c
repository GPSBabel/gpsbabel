/*
    Copyright (C) 2003 Robert Lipe, robertlipe@usa.net

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
#include "cet_util.h"
#if HAVE_LIBEXPAT
#include <expat.h>
static XML_Parser psr;
#endif

static waypoint* wpt_tmp;

static gbfile* fin, *fout;

static char* noretired = NULL;

static
arglist_t nav_args[] = {
  {
    "noretired", &noretired, "Suppress retired geocaches",
    NULL, ARGTYPE_BOOL, ARG_NOMINMAX
  },
  ARG_TERMINATOR
};

#define MYNAME "navicache"
#define MY_CBUF 4096
#define NC_URL	"http://www.navicache.com/cgi-bin/db/displaycache2.pl?CacheID="

#if ! HAVE_LIBEXPAT
static void
nav_rd_init(const char* fname)
{
  fatal(MYNAME ": This build excluded GPX support because expat was not installed.\n");
}

static void
nav_read(void)
{
}
#else

static struct
    nc_type_mapping {
  geocache_type type;
  const char* name;
} nc_type_map[] = {
  { gt_unknown, "unknown" },
  { gt_traditional, "normal" },
  { gt_multi, "Multi-part" },
  { gt_virtual, "Virtual" },
  { gt_event, "event" }
};

static struct
    nc_container_mapping {
  geocache_container type;
  const char* name;
} nc_container_map[] = {
  { gc_other, "Unknown" },
  { gc_micro, "Micro" },
  { gc_regular, "Normal" },
  { gc_large, "Large" },
  { gc_virtual, "Virtual" }
};

static
geocache_type
nc_mktype(const char* t)
{
  int i;
  int sz = sizeof(nc_type_map) / sizeof(nc_type_map[0]);

  for (i = 0; i < sz; i++) {
    if (0 == case_ignore_strcmp(t, nc_type_map[i].name)) {
      return nc_type_map[i].type;
    }
  }
  return gt_unknown;
}

static
geocache_container
nc_mkcont(const char* t)
{
  int i;
  int sz = sizeof(nc_container_map) / sizeof(nc_container_map[0]);

  for (i = 0; i < sz; i++) {
    if (0 == case_ignore_strcmp(t, nc_container_map[i].name)) {
      return nc_container_map[i].type;
    }
  }
  return gc_unknown;
}

static void
nav_start(void* data, const XML_Char* xml_el, const XML_Char** xml_attr)
{
  const char* el;
  const char** attr;

  el = xml_convert_to_char_string(xml_el);
  attr = xml_convert_attrs_to_char_string(xml_attr);
  if (0 == strcmp(el, "CacheDetails")) {
    const char** ap;
    geocache_data* gc_data;
    wpt_tmp = waypt_new();
    gc_data = waypt_alloc_gc_data(wpt_tmp);

    for (ap = attr; *ap; ap+=2) {
      if (0 == strcmp(ap[0], "cache_id")) {
        int id;

        id = atoi(ap[1]);
        xasprintf(&wpt_tmp->shortname, "N%05X", id);
        xasprintf(&wpt_tmp->url, "%s%d", NC_URL, id);
      } else if (0 == strcmp(ap[0], "name")) {
        wpt_tmp->description = xstrdup(ap[1]);
      } else if (0 == strcmp(ap[0], "user_name")) {
        gc_data->placer = xstrdup(ap[1]);
      } else if (0 == strcmp(ap[0], "latitude")) {
        sscanf(ap[1], "%lf",
               &wpt_tmp->latitude);
      } else if (0 == strcmp(ap[0], "longitude")) {
        sscanf(ap[1], "%lf",
               &wpt_tmp->longitude);
      } else if (0 == strcmp(ap[0], "longitude")) {
        sscanf(ap[1], "%lf",
               &wpt_tmp->longitude);
      } else if (0 == strcmp(ap[0], "difficulty")) {
        float x;
        sscanf(ap[1], "%f", &x);
        gc_data->diff = x * 10;
      } else if (0 == strcmp(ap[0], "terrain")) {
        float x;
        sscanf(ap[1], "%f", &x);
        gc_data->terr = x * 10;
      } else if (0 == strcmp(ap[0], "cache_type")) {
        gc_data->type = nc_mktype(ap[1]);
        if (!strcmp(ap[1], "normal")) {
          wpt_tmp->icon_descr = "Geocache-regular";
        } else if (!strcmp(ap[1], "multi-part")) {
          wpt_tmp->icon_descr = "Geocache-multi";
        } else if (!strcmp(ap[1], "moving_travelling")) {
          wpt_tmp->icon_descr = "Geocache-moving";
        } else {
          xasprintf(&wpt_tmp->icon_descr,
                    "Geocache-%-.20s", ap[1]);
        }
      } else if (0 == strcmp(ap[0], "hidden_date")) {
        struct tm tm;

        sscanf(ap[1], "%d-%d-%d",
               &tm.tm_year,
               &tm.tm_mon,
               &tm.tm_mday);
        tm.tm_mon -= 1;
        tm.tm_year -= 1900;
        tm.tm_isdst = 0;
        tm.tm_hour = 0;
        tm.tm_min = 0;
        tm.tm_sec = 0;
        wpt_tmp->creation_time = mktime(&tm);
      } else if (0 == strcmp(ap[0], "retired")) {
        if (!strcmp(ap[1], "yes") && noretired) {
          xfree(wpt_tmp);
          return;
        }
      } else if (0 == strcmp(ap[0], "cache_size")) {
        gc_data->container = nc_mkcont(ap[1]);
      }  else if (0 == strcmp(ap[0], "description")) {
        gc_data->desc_long.is_html = 1;
        gc_data->desc_long.utfstring = xstrdup(ap[1]);
      } else if (0 == strcmp(ap[0], "comments")) {
        gc_data->desc_short.is_html = 1;
        gc_data->desc_short.utfstring = xstrdup(ap[1]);
      }
    }
    waypt_add(wpt_tmp);
  }

  xml_free_converted_attrs(attr);
  xml_free_converted_string(el);
}

static void
nav_end(void* data, const XML_Char* el)
{
}

static void
nav_rd_init(const char* fname)
{
  fin = gbfopen(fname, "r", MYNAME);

  psr = XML_ParserCreate(NULL);
  if (!psr) {
    fatal(MYNAME ":Cannot create XML parser\n");
  }

  XML_SetUnknownEncodingHandler(psr, cet_lib_expat_UnknownEncodingHandler, NULL);
  XML_SetElementHandler(psr, nav_start, nav_end);
}

static void
nav_read(void)
{
  int len;
  char buf[MY_CBUF];

  while ((len = gbfread(buf, 1, sizeof(buf), fin))) {
    if (!XML_Parse(psr, buf, len, gbfeof(fin))) {
      fatal(MYNAME ":Parse error at %d: %s\n",
            (int) XML_GetCurrentLineNumber(psr),
            XML_ErrorString(XML_GetErrorCode(psr)));
    }
  }

  XML_ParserFree(psr);
}

#endif

static void
nav_rd_deinit(void)
{
  gbfclose(fin);
}

static void
nav_wr_init(const char* fname)
{
  fatal(MYNAME ": Does not support writing Navicache files.\n");
  fout = gbfopen(fname, "w", MYNAME);
}

static void
nav_wr_deinit(void)
{
  gbfclose(fout);
}

static void
nav_write(void)
{
}

ff_vecs_t navicache_vecs = {
  ff_type_file,
  { ff_cap_read, ff_cap_none, ff_cap_none },
  nav_rd_init,
  nav_wr_init,
  nav_rd_deinit,
  nav_wr_deinit,
  nav_read,
  nav_write,
  NULL,
  nav_args,
  CET_CHARSET_ASCII, 0	/* CET-REVIEW */
};
