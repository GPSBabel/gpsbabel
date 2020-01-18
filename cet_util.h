/*

    Character encoding transformation - utilities header

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

#ifndef CET_UTIL_H_INCLUDED_
#define CET_UTIL_H_INCLUDED_

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <QtCore/QString>
#include "cet.h"
#include "defs.h"

cet_cs_vec_t* cet_find_cs_by_name(const QString& name);
void cet_register();
void cet_deregister();

extern cet_cs_vec_t cet_cs_vec_cp1252;

extern cet_cs_vec_t cet_cs_vec_ansi_x3_4_1968;

short* cet_str_utf8_to_uni(const char* src, int* length);

extern cet_cs_vec_t cet_cs_vec_utf8;


/* helpers */

[[deprecated]] char* cet_str_any_to_any(const char* src, const cet_cs_vec_t* src_vec, const cet_cs_vec_t* dest_vec);

/* gpsbabel extensions */

void cet_convert_init(const QString& cs_name, int force);
void cet_convert_deinit();

#endif  // CET_UTIL_H_INCLUDED_
