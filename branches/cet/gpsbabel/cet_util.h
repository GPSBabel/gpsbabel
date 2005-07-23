#ifndef CET_UTIL_H
#define CET_UTIL_H

#include <ctype.h>
#include <stdio.h>
#include "cet.h"

#ifndef NO_EXPAT
# include <expat.h>
#endif

cet_cs_vec_t *cet_find_cs_by_name(const char *name);
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

/* helpers */
int XMLCALL cet_UnknownEncodingHandler(void *data,const XML_Char *encoding,XML_Encoding *info);

char *cet_str_uni_to_any(const short *src, int length, const cet_cs_vec_t *dest_vec);
#if 0
char *cet_str_any_to_any(const char *src, const cet_cs_vec_t *src_vec, const cet_cs_vec_t *dest_vec);
#endif

int cet_valid_char(const char *src, const cet_cs_vec_t *vec);

void cet_convert_init(const char *cs_name, const int force);
void cet_convert_strings(const cet_cs_vec_t *source, const cet_cs_vec_t *target, const char *format);
void cet_convert_deinit(void);

void cet_disp_character_set_names(FILE *fout);

#endif
