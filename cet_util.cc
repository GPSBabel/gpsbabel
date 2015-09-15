/*

    Character encoding transformation - utilities

    Copyright (C) 2005-2008 Olaf Klein, o.b.klein@gpsbabel.org

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
#include "cet.h"
#include "cet_util.h"
#include <stdlib.h> // qsort
#include "src/core/logging.h"
#include <QtCore/QDebug>
#include <QtCore/QTextCodec>

#define MYNAME "cet_util"

static cet_cs_vec_t* cet_cs_vec_root = NULL;

typedef struct cet_cs_alias_s {
  char* name;
  cet_cs_vec_t* vec;
} cet_cs_alias_t;

static cet_cs_alias_t* cet_cs_alias;
static int cet_cs_alias_ct = 0;
static int cet_cs_vec_ct = 0;
static int cet_output = 0;

/* %%% fixed inbuild character sets %%% */

#include "cet/ansi_x3_4_1968.h"
#include "cet/cp1252.h"
#include "cet/iso_8859_8.h"


/* %%% short hand strings transmission for main character sets %%% */

short*
cet_str_utf8_to_uni(const char* src, int* length)
{
  return cet_str_any_to_uni(src, &cet_cs_vec_utf8, length);
}

/* %%% cet_str_any_to_any %%%
 *
 * -->> for use in mkshort */

char*
cet_str_any_to_any(const char* src, const cet_cs_vec_t* src_vec, const cet_cs_vec_t* dest_vec)
{
  char* c0, *c1;
  const cet_cs_vec_t* v_in = (src_vec != NULL)  ? src_vec :  &cet_cs_vec_ansi_x3_4_1968;
  const cet_cs_vec_t* v_out = (dest_vec != NULL) ? dest_vec : &cet_cs_vec_ansi_x3_4_1968;

  if (src == NULL) {
    return NULL;
  } else if ((*src == '\0') || (v_in == v_out)) {
    return xstrdup(src);
  }

  c0 = (v_in == &cet_cs_vec_utf8) ? xstrdup(src) : cet_str_any_to_utf8(src, v_in);
  c1 = (v_out == &cet_cs_vec_utf8) ? xstrdup(c0) : cet_str_utf8_to_any(c0, v_out);
  xfree(c0);

  return c1;
}

static signed int
cet_cs_alias_qsort_cb(const void* a, const void* b)
{
  const cet_cs_alias_t* va = (const cet_cs_alias_t*) a;
  const cet_cs_alias_t* vb = (const cet_cs_alias_t*) b;
  return case_ignore_strcmp(va->name, vb->name);
}

void
cet_register_cs(cet_cs_vec_t* vec)
{
  if (vec->next == NULL) {
    vec->next = cet_cs_vec_root;
    cet_cs_vec_root = vec;
    cet_cs_vec_ct++;
#ifdef DEBUG_MEM
    cet_check_cs(vec);
#endif
  }
}

/* Dummy vector for our native character set */

const char* cet_cs_utf8_alias[] = {
  "utf8", NULL
};

cet_cs_vec_t cet_cs_vec_utf8 = {
  CET_CHARSET_UTF8,
  cet_cs_utf8_alias,
  NULL,
  NULL,
  NULL,
  0,
  0,
  NULL,
  0,
  NULL,
  0,
  NULL,
};

void
cet_register(void)
{
  int i, c;

  if (cet_cs_vec_root != NULL) {
    return;
  }

  cet_cs_vec_ct = 0;
  cet_register_cs(&cet_cs_vec_utf8);			/* internal place holder */
 // Alias for "US-ASCII".
#ifdef cet_cs_name_ansi_x3_4_1968
  cet_register_cs(&cet_cs_vec_ansi_x3_4_1968);
#endif
#ifdef cet_cs_name_cp1252
  cet_register_cs(&cet_cs_vec_cp1252);
#endif
#ifdef cet_cs_name_iso_8859_8
  cet_register_cs(&cet_cs_vec_iso_8859_8);
#endif


  if (cet_cs_vec_ct > 0) {
    cet_cs_vec_t* p;
    cet_cs_alias_t* list;
    c = 0;

    /* enumerate count of all names and aliases */

    for (p = cet_cs_vec_root; p != NULL; p = p->next) {
      c++;
      if (p->alias != NULL) {
        char** a = (char**)p->alias;
        while ((*a) != NULL) {
          a++;
          c++;
        }
      }
    }
    /* create name to vec table */

    list = (cet_cs_alias_t*) xcalloc(c, sizeof(*list));
    i = 0;
    for (p = cet_cs_vec_root; p != NULL; p = p->next) {
      if (p->alias != NULL) {
        char** a = (char**)p->alias;

        list[i].name = xstrdup(p->name);
        list[i].vec = p;
        i++;
        while (*a != NULL) {
          list[i].name = xstrdup(*a);
          list[i].vec = p;
          i++;
          a++;
        }
      }
    }
    qsort(list, c, sizeof(*list), cet_cs_alias_qsort_cb);
    cet_cs_alias = list;
    cet_cs_alias_ct = c;

    /* install fallback for ascii-like (first 128 ch.) character sets */
    for (i = 1250; i <= 1258; i++) {
      char name[16];
      cet_cs_vec_t* vec;

      snprintf(name, sizeof(name), "WIN-CP%d", i);
      if ((vec = cet_find_cs_by_name(name))) {
        vec->fallback = &cet_cs_vec_ansi_x3_4_1968;
      }
    }
    for (i = 1; i <= 15; i++) {
      char name[16];
      cet_cs_vec_t* vec;

      snprintf(name, sizeof(name), "ISO-8859-%d", i);
      if ((vec = cet_find_cs_by_name(name))) {
        vec->fallback = &cet_cs_vec_ansi_x3_4_1968;
      }
    }
  }
#ifdef CET_DEBUG
  printf("We have registered %d character sets with %d aliases\n", cet_cs_vec_ct, cet_cs_alias_ct);
#endif
}

cet_cs_vec_t*
cet_find_cs_by_name(const QString& name)
{
  int i, j;

  cet_register();

  if (cet_cs_alias == NULL) {
    return NULL;
  }

  i = 0;
  j = cet_cs_alias_ct - 1;

  while (i <= j) {
    int a, x;
    cet_cs_alias_t* n;

    a = (i + j) >> 1;
    n = &cet_cs_alias[a];
    x = case_ignore_strcmp(name, n->name);
    if (x == 0) {
      return n->vec;
    } else if (x < 0) {
      j = a - 1;
    } else {
      i = a + 1;
    }
  }
  return NULL;
}

void
cet_deregister(void)
{
  int i;
  int j = cet_cs_alias_ct;
  cet_cs_alias_t* p = cet_cs_alias;

  if (p == NULL) {
    return;
  }

  cet_cs_alias_ct = 0;
  cet_cs_alias = NULL;

  for (i = 0; i < j; i++) {
    xfree(p[i].name);
  }
  xfree(p);
}

/* gpsbabel additions */

int
cet_validate_cs(const QString& cs, cet_cs_vec_t** vec, QString* cs_name)
{
  cet_cs_vec_t* v;

  if (cs.isEmpty()) { /* set default us-ascii */
    *vec = &cet_cs_vec_ansi_x3_4_1968;
    *cs_name = CET_CHARSET_ASCII;
    return 1;
  }

  v = cet_find_cs_by_name(cs);
  if (v != NULL) {
    // TODO: make v->name into q QString and replace this...
    char* tmp = xstrdup(v->name);
    *cs_name = strupper(tmp);
    xfree(tmp);
    *vec = v;
    return 1;
  } else {
    cs_name->clear();
    *vec = NULL;
    return 0;
  }
}

void
cet_convert_deinit(void)
{
  global_opts.charset = NULL;
  global_opts.codec = NULL;
}

void
cet_convert_init(const QString& cs_name, const int force)
{
  if ((force != 0) || (global_opts.charset == NULL)) {
    cet_convert_deinit();
    if (0 == cet_validate_cs(cs_name, &global_opts.charset, &global_opts.charset_name)) {
      Fatal() << "Unsupported character set \"" << cs_name << ".";
    }
    if (cs_name.isEmpty()) {	/* set default us-ascii */
      global_opts.codec = QTextCodec::codecForName(CET_CHARSET_ASCII);
    } else {
      QByteArray ba = CSTR(cs_name);
      global_opts.codec = QTextCodec::codecForName(ba);
    }
    if (!global_opts.codec) {
      Fatal() << "Unsupported character set \"" << cs_name << ".";
    }
  }
}

/* -------------------------------------------------------------------- */

static void
cet_flag_waypt(const Waypoint* wpt)
{
  ((Waypoint*)(wpt))->wpt_flags.cet_converted = 1;
}

static void
cet_flag_route(const route_head* rte)
{
  ((route_head*)(rte))->cet_converted = 1;
}

static void
cet_flag_all(void)
{
  waypt_disp_all(cet_flag_waypt);
  route_disp_all(cet_flag_route, NULL, cet_flag_waypt);
  track_disp_all(cet_flag_route, NULL, cet_flag_waypt);
}

/* -------------------------------------------------------------------- */
/* %%%         complete data strings transformation                 %%% */
/* -------------------------------------------------------------------- */

static char* (*converter)(const char*) = NULL;

/* two converters */

static char*
cet_convert_to_utf8(const char* str)
{
  return cet_str_any_to_utf8(str, global_opts.charset);
}

static char*
cet_convert_from_utf8(const char* str)
{
  return cet_str_utf8_to_any(str, global_opts.charset);
}

/* cet_convert_string: internal used within cet_convert_strings process */

char*
cet_convert_string(char* str)
{
  char* res;

  if (str == NULL) {
    return NULL;  /* return origin if empty or NULL */
  } else if (*str == '\0') {
    return str;
  }

  res = converter(str);
  xfree(str);
  return res;
}

const char*
cet_convert_string(const QString& str)
{
  // FIXME: this is really weird.  Since cet_convert_string wants to free
  // its argument (!) we make a duplicate just to satisfy that kind of goofy
  // requirement.
  return cet_convert_string(xstrdup(str));
}

/* cet_convert_waypt: internal used within cet_convert_strings process */

static void
cet_convert_waypt(const Waypoint* wpt)
{
  Waypoint* w = (Waypoint*)wpt;
  format_specific_data* fs;

  if ((cet_output == 0) && (w->wpt_flags.cet_converted != 0)) {
    return;
  }

  w->wpt_flags.cet_converted = 1;

  fs = wpt->fs;
  while (fs != NULL) {
    if (fs->convert != NULL) {
      fs->convert(fs);
    }
    fs = fs->next;
  }
}

/* cet_convert_route_hdr: internal used within cet_convert_strings process */

static void
cet_convert_route_hdr(const route_head* route)
{
  route_head* rte = (route_head*)route;

  if ((cet_output == 0) && (rte->cet_converted != 0)) {
    return;
  }

  rte->cet_converted = 1;
}

/* cet_convert_route_tlr: internal used within cet_convert_strings process */

static void
cet_convert_route_tlr(const route_head* route)
{
  (void)route;
}

/* %%% cet_convert_strings (public) %%%
 *
 * - Convert all well known strings of GPS data from or to UTF-8 -
 *
 * !!! One of "source" or "target" must be internal cet_cs_vec_utf8 or NULL !!! */

void
cet_convert_strings(const cet_cs_vec_t* source, const cet_cs_vec_t* target, const char* format)
{
  char* cs_name_from, *cs_name_to;
  (void)format;

  converter = NULL;

  if ((source == NULL) || (source == &cet_cs_vec_utf8)) {
    if ((target == NULL) || (target == &cet_cs_vec_utf8)) {
      cet_flag_all();
      return;
    }

    cet_output = 1;

    converter = cet_convert_from_utf8;
    cs_name_from = (char*)cet_cs_vec_utf8.name;
    cs_name_to = (char*)target->name;
  } else {
    if ((target != NULL) && (target != &cet_cs_vec_utf8)) {
      fatal(MYNAME ": Internal error!\n");
    }

    cet_output = 0;

    converter = cet_convert_to_utf8;
    cs_name_to = (char*)cet_cs_vec_utf8.name;
    cs_name_from = (char*)source->name;
  }

  if (global_opts.debug_level > 0) {
    printf(MYNAME ": Converting from \"%s\" to \"%s\"", cs_name_from, cs_name_to);
  }

  waypt_disp_all(cet_convert_waypt);
  route_disp_all(cet_convert_route_hdr, cet_convert_route_tlr, cet_convert_waypt);
  track_disp_all(cet_convert_route_hdr, cet_convert_route_tlr, cet_convert_waypt);

  cet_output = 0;

  if (global_opts.debug_level > 0) {
    printf(", done.\n");
  }
}
