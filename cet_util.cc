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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
*/

#include <cstdio>                // for printf, snprintf
#include <cstdlib>               // for qsort

#include <QtCore/QByteArray>     // for QByteArray
#include <QtCore/QDebug>         // for QDebug
#include <QtCore/QTextCodec>     // for QTextCodec

#include "defs.h"
#include "cet.h"                 // for cet_cs_vec_t, cet_str_any_to_utf8, cet_str_utf8_to_any, cet_str_any_to_uni
#include "cet_util.h"
#include "src/core/logging.h"    // for Fatal


#define MYNAME "cet_util"

static cet_cs_vec_t* cet_cs_vec_root = nullptr;

struct cet_cs_alias_t {
  char* name;
  cet_cs_vec_t* vec;
};

static cet_cs_alias_t* cet_cs_alias;
static int cet_cs_alias_ct = 0;
static int cet_cs_vec_ct = 0;

/* %%% fixed inbuild character sets %%% */

#include "cet/ansi_x3_4_1968.h"  // for cet_cs_vec_ansi_x3_4_1968, cet_cs_name_ansi_x3_4_1968
#include "cet/cp1252.h"          // for cet_cs_name_cp1252, cet_cs_vec_cp1252
#include "cet/iso_8859_8.h"      // for cet_cs_name_iso_8859_8, cet_cs_vec_iso_8859_8


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
  const cet_cs_vec_t* v_in = (src_vec != nullptr)  ? src_vec :  &cet_cs_vec_ansi_x3_4_1968;
  const cet_cs_vec_t* v_out = (dest_vec != nullptr) ? dest_vec : &cet_cs_vec_ansi_x3_4_1968;

  if (src == nullptr) {
    return nullptr;
  } else if ((*src == '\0') || (v_in == v_out)) {
    return xstrdup(src);
  }

  char* c0 = (v_in == &cet_cs_vec_utf8) ? xstrdup(src) : cet_str_any_to_utf8(src, v_in);
  char* c1 = (v_out == &cet_cs_vec_utf8) ? xstrdup(c0) : cet_str_utf8_to_any(c0, v_out);
  xfree(c0);

  return c1;
}

static signed int
cet_cs_alias_qsort_cb(const void* a, const void* b)
{
  const auto* va = (const cet_cs_alias_t*) a;
  const auto* vb = (const cet_cs_alias_t*) b;
  return case_ignore_strcmp(va->name, vb->name);
}

void
cet_register_cs(cet_cs_vec_t* vec)
{
  if (vec->next == nullptr) {
    vec->next = cet_cs_vec_root;
    cet_cs_vec_root = vec;
    cet_cs_vec_ct++;
  }
}

/* Dummy vector for our native character set */

const char* cet_cs_utf8_alias[] = {
  "utf8", nullptr
};

cet_cs_vec_t cet_cs_vec_utf8 = {
  CET_CHARSET_UTF8,
  cet_cs_utf8_alias,
  nullptr,
  nullptr,
  nullptr,
  0,
  0,
  nullptr,
  0,
  nullptr,
  0,
  nullptr,
};

void
cet_register()
{
  int i, c;

  if (cet_cs_vec_root != nullptr) {
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
    c = 0;

    /* enumerate count of all names and aliases */

    for (p = cet_cs_vec_root; p != nullptr; p = p->next) {
      c++;
      if (p->alias != nullptr) {
        const char** a = p->alias;
        while ((*a) != nullptr) {
          a++;
          c++;
        }
      }
    }
    /* create name to vec table */

    cet_cs_alias_t* list = (cet_cs_alias_t*) xcalloc(c, sizeof(*list));
    i = 0;
    for (p = cet_cs_vec_root; p != nullptr; p = p->next) {
      if (p->alias != nullptr) {
        const char** a = p->alias;

        list[i].name = xstrdup(p->name);
        list[i].vec = p;
        i++;
        while (*a != nullptr) {
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
  cet_register();

  if (cet_cs_alias == nullptr) {
    return nullptr;
  }

  int i = 0;
  int j = cet_cs_alias_ct - 1;

  while (i <= j) {
    int a = (i + j) >> 1;
    auto n = &cet_cs_alias[a];
    int x = case_ignore_strcmp(name, n->name);
    if (x == 0) {
      return n->vec;
    } else if (x < 0) {
      j = a - 1;
    } else {
      i = a + 1;
    }
  }
  return nullptr;
}

void
cet_deregister()
{
  int j = cet_cs_alias_ct;
  cet_cs_alias_t* p = cet_cs_alias;

  if (p == nullptr) {
    return;
  }

  cet_cs_alias_ct = 0;
  cet_cs_alias = nullptr;

  for (int i = 0; i < j; i++) {
    xfree(p[i].name);
  }
  xfree(p);
}

/* gpsbabel additions */

int
cet_validate_cs(const QString& cs, cet_cs_vec_t** vec, QString* cs_name)
{
  if (cs.isEmpty()) { /* set default us-ascii */
    *vec = &cet_cs_vec_ansi_x3_4_1968;
    *cs_name = CET_CHARSET_ASCII;
    return 1;
  }

  cet_cs_vec_t* v = cet_find_cs_by_name(cs);
  if (v != nullptr) {
    // TODO: make v->name into q QString and replace this...
    char* tmp = xstrdup(v->name);
    *cs_name = strupper(tmp);
    xfree(tmp);
    *vec = v;
    return 1;
  } else {
    cs_name->clear();
    *vec = nullptr;
    return 0;
  }
}

void
cet_convert_deinit()
{
  global_opts.charset = nullptr;
  global_opts.codec = nullptr;
}

void
cet_convert_init(const QString& cs_name, const int force)
{
  if ((force != 0) || (global_opts.charset == nullptr)) {
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
