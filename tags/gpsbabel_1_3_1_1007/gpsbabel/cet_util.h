/*

    Character encoding transformation - utilities header

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

#ifndef CET_UTIL_H
#define CET_UTIL_H

#include <ctype.h>
#include <stdio.h>
#include "config.h"

#if HAVE_LIBEXPAT
# include <expat.h>
#else
typedef char XML_Char;
#endif

#include "cet.h"

cet_cs_vec_t *cet_find_cs_by_name(const char *name);
void cet_register(void);
void cet_deregister(void);

/* short hand transmissions */

char *cet_str_utf8_to_cp1252(const char *src);
char *cet_str_cp1252_to_utf8(const char *src);
extern cet_cs_vec_t cet_cs_vec_cp1252;

char *cet_str_iso8859_1_to_utf8(const char *src);
char *cet_str_utf8_to_iso8859_1(const char *src);
extern cet_cs_vec_t cet_cs_vec_iso8859_1;

char *cet_str_iso8859_15_to_utf8(const char *src);
char *cet_str_utf8_to_iso8859_15(const char *src);
extern const cet_cs_vec_t cet_cs_vec_iso8859_15;

char *cet_str_utf8_to_us_ascii(const char *src);
char *cet_str_us_ascii_to_utf8(const char *src);
extern cet_cs_vec_t cet_cs_vec_ansi_x3_4_1968;


extern cet_cs_vec_t cet_cs_vec_utf8;


/* Missing defines in older expat libraries | CET-REVIEW  */

/* Taken from expat_external.h (Expat 1.95.7) */

#ifndef XML_STATUS_OK			
# define XML_STATUS_OK 1
#endif
#ifndef XML_STATUS_ERROR
# define XML_STATUS_ERROR 0
#endif


#ifndef XMLCALL
#if defined(XML_USE_MSC_EXTENSIONS)
#define XMLCALL __cdecl
#elif defined(__GNUC__) && defined(__i386)
#define XMLCALL __attribute__((cdecl))
#else
/* For any platform which uses this definition and supports more than
   one calling convention, we need to extend this definition to
   declare the convention used on that platform, if it's possible to
   do so.

   If this is the case for your platform, please file a bug report
   with information on how to identify your platform via the C
   pre-processor and how to specify the same calling convention as the
   platform's malloc() implementation.
*/
#define XMLCALL
#endif
#endif  /* not defined XMLCALL */

#if HAVE_LIBEXPAT
int XMLCALL cet_lib_expat_UnknownEncodingHandler(void *data, const XML_Char *encoding, XML_Encoding *info);
#endif

/* helpers */

char *cet_str_uni_to_any(const short *src, int length, const cet_cs_vec_t *dest_vec);
char *cet_str_any_to_any(const char *src, const cet_cs_vec_t *src_vec, const cet_cs_vec_t *dest_vec);
int cet_valid_char(const char *src, const cet_cs_vec_t *vec);

int cet_gbfprintf(gbfile *stream, const cet_cs_vec_t *src_vec, const char *fmt, ...);

/* cet_convert_string: !!! ONLY VALID WITHIN 'cet_convert_strings' process !!! */
char *cet_convert_string(char *str);

/* gpsbabel extensions */

void cet_convert_init(const char *cs_name, const int force);
void cet_convert_strings(const cet_cs_vec_t *source, const cet_cs_vec_t *target, const char *format);
void cet_convert_deinit(void);

void cet_disp_character_set_names(FILE *fout);

/*
 * Conversion from XML_Char string to char string.  If XML_Char is the
 * same as char, these routines do nothing.  If XML_Char is a wide
 * character, xml_convert_to_char_string converts the string to a
 * newly allocated char string, and xml_free_converted_string frees
 * it.
 */

const char *xml_convert_to_char_string_n(const XML_Char *str, int *nbytes);
const char *xml_convert_to_char_string(const XML_Char *str);
void xml_free_converted_string(const char *str);

const char **xml_convert_attrs_to_char_string(const XML_Char **xml_attr);
void xml_free_converted_attrs(const char **attr);

#endif
