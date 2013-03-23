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


#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

#include "defs.h"
#include "cet.h"
#include "cet_util.h"

#if HAVE_LIBEXPAT
# include <expat.h>
#endif

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
#include "cet/iso_8859_1.h"
#include "cet/iso_8859_8.h"
#include "cet/iso_8859_15.h"
#include "cet/cp1252.h"
#include "cet/cp1255.h"

/* %%% short hand strings transmission for main character sets %%% */

char*
cet_str_utf8_to_iso8859_1(const char* src)
{
  return cet_str_utf8_to_any(src, &cet_cs_vec_iso_8859_1);
}

char*
cet_str_iso8859_1_to_utf8(const char* src)
{
  return cet_str_any_to_utf8(src, &cet_cs_vec_iso_8859_1);
}

char*
cet_str_utf8_to_iso8859_8(const char* src)
{
  return cet_str_utf8_to_any(src, &cet_cs_vec_iso_8859_8);
}

char*
cet_str_iso8859_8_to_utf8(const char* src)
{
  return cet_str_any_to_utf8(src, &cet_cs_vec_iso_8859_8);
}

char*
cet_str_utf8_to_iso8859_15(const char* src)
{
  return cet_str_utf8_to_any(src, &cet_cs_vec_iso_8859_15);
}

char*
cet_str_iso8859_15_to_utf8(const char* src)
{
  return cet_str_any_to_utf8(src, &cet_cs_vec_iso_8859_15);
}

char*
cet_str_utf8_to_us_ascii(const char* src)
{
  return cet_str_utf8_to_any(src, &cet_cs_vec_ansi_x3_4_1968);
}

char*
cet_str_us_ascii_to_utf8(const char* src)
{
  return cet_str_any_to_utf8(src, &cet_cs_vec_ansi_x3_4_1968);
}

char*
cet_str_utf8_to_cp1252(const char* src)
{
  return cet_str_utf8_to_any(src, &cet_cs_vec_cp1252);
}

char*
cet_str_cp1252_to_utf8(const char* src)
{
  return cet_str_any_to_utf8(src, &cet_cs_vec_cp1252);
}

char*
cet_str_utf8_to_cp1255(const char* src)
{
  return cet_str_utf8_to_any(src, &cet_cs_vec_cp1255);
}

char*
cet_str_cp1255_to_utf8(const char* src)
{
  return cet_str_any_to_utf8(src, &cet_cs_vec_cp1255);
}
short*
cet_str_utf8_to_uni(const char* src, int* length)
{
  return cet_str_any_to_uni(src, &cet_cs_vec_utf8, length);
}



/* helpers */

/* %%% %%%
 *
 *
 */
#if HAVE_LIBEXPAT
int XMLCALL cet_lib_expat_UnknownEncodingHandler(void* data, const XML_Char* xml_encoding, XML_Encoding* info)
{
  cet_cs_vec_t* cs;
  int i, c, ucs4_def;
  const char* encoding;

  encoding = xml_convert_to_char_string(xml_encoding);
  cs = cet_find_cs_by_name(encoding);
  if (cs == NULL) {
    return XML_STATUS_ERROR;  /* fatal(MYNAME ": Unknown character set \"%s\"!\n", encoding); */
  }

  cet_char_to_ucs4(CET_NOT_CONVERTABLE_DEFAULT, cs, &ucs4_def);

  for (i = 0; i < cs->ucs4_offset; i++) {
    info->map[i] = i;  /* equal to ascii part */
  }
  for (i = 0; i < cs->ucs4_count; i++) {						/* mixed table */
    c = cs->ucs4_map[i];
    if (c == -1) {
      c = ucs4_def;
    }
    info->map[i + cs->ucs4_offset] = c;
  }
  for (i = cs->ucs4_offset + cs->ucs4_count; i < 256; i++) {
    info->map[i] = ucs4_def;  /* non convertable trailer */
  }

  info->data = NULL;
  info->convert = NULL;
  info->release = NULL;

  cet_convert_init(CET_CHARSET_UTF8, 1);		/* We do not need to transform any string */

  if (global_opts.verbose_status > 0) {
    printf(MYNAME ": XML parser - encoding handler for character set \"%s\" established.\n", encoding);
  }

  xml_free_converted_string(encoding);
  return XML_STATUS_OK;
}
#endif


char*
cet_str_uni_to_any(const short* src, int length, const cet_cs_vec_t* dest_vec)
{
  char* res, *c;
  const cet_cs_vec_t* cs = (dest_vec != NULL) ? dest_vec : &cet_cs_vec_ansi_x3_4_1968;

  res = cet_str_uni_to_utf8(src, length);
  if (cs != &cet_cs_vec_utf8) {
    c = cet_str_utf8_to_any(res, cs);
    xfree(res);
    res = c;
  }
  return res;
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

/* %%% cet_valid_char %%%
 *
 * returnes 1 if convertable otherwise 0
 *
 */

int
cet_valid_char(const char* src, const cet_cs_vec_t* vec)
{
  int value;

  const cet_cs_vec_t* v = (vec != NULL) ? vec : &cet_cs_vec_ansi_x3_4_1968;
  return cet_char_to_ucs4(*src, v, &value);
}

/* %%% include character set headers %%% */
#if defined (CET_WANTED)
#include "cet/iso_8859_2.h"
#include "cet/cp1250.h"
#include "cet/latin_greek_1.h"
#include "cet/macintosh.h"
#include "cet/cp1250.h"
#include "cet/cp1251.h"
#include "cet/cp1252.h"
#include "cet/cp1253.h"
#include "cet/cp1254.h"
#include "cet/cp1255.h"
#include "cet/cp1256.h"
#include "cet/cp1257.h"
#include "cet/ibm437.h"
#include "cet/ibm850.h"
#include "cet/ibm851.h"
#include "cet/ibm852.h"
#include "cet/ibm855.h"
#include "cet/ibm857.h"
#include "cet/ibm860.h"
#include "cet/ibm861.h"
#include "cet/ibm862.h"
#include "cet/ibm863.h"
#include "cet/ibm864.h"
#include "cet/ibm865.h"
#include "cet/ibm868.h"
#include "cet/ibm869.h"
#include "cet/iso_8859_1.h"
#include "cet/iso_8859_10.h"
#include "cet/iso_8859_13.h"
#include "cet/iso_8859_14.h"
#include "cet/iso_8859_15.h"
#include "cet/iso_8859_2.h"
#include "cet/iso_8859_3.h"
#include "cet/iso_8859_4.h"
#include "cet/iso_8859_5.h"
#include "cet/iso_8859_6.h"
#include "cet/iso_8859_7.h"
#include "cet/iso_8859_8.h"
#include "cet/iso_8859_9.h"
#include "cet/koi8_r.h"
#include "cet/koi8_ru.h"
#include "cet/koi_8.h"
#endif

#if CET_WANTED
#include "cet/ansi_x3_4_1968.h"
#include "cet/atarist.h"
#include "cet/baltic.h"
#include "cet/bs_4730.h"
#include "cet/bs_viewdata.h"
#include "cet/csa_z243_4_1985_1.h"
#include "cet/csa_z243_4_1985_2.h"
#include "cet/csa_z243_4_1985_gr.h"
#include "cet/csn_369103.h"
#include "cet/cwi.h"
#include "cet/dec_mcs.h"
#include "cet/din_66003.h"
#include "cet/ds_2089.h"
#include "cet/ecma_cyrillic.h"
#include "cet/es.h"
#include "cet/es2.h"
#include "cet/gb_1988_80.h"
#include "cet/gost_19768_87.h"
#include "cet/hp_roman8.h"
#include "cet/ibm037.h"
#include "cet/ibm1004.h"
#include "cet/ibm1026.h"
#include "cet/ibm1047.h"
#include "cet/ibm256.h"
#include "cet/ibm273.h"
#include "cet/ibm277.h"
#include "cet/ibm278.h"
#include "cet/ibm280.h"
#include "cet/ibm284.h"
#include "cet/ibm285.h"
#include "cet/ibm297.h"
#include "cet/ibm500.h"
#include "cet/ibm871.h"
#include "cet/ibm891.h"
#include "cet/ibm903.h"
#include "cet/ibm904.h"
#include "cet/iec_p27_1.h"
#include "cet/iso_10367_box.h"
#include "cet/iso_5427.h"
#include "cet/iso_646_irv.h"
#include "cet/iso_6937_2_25.h"
#include "cet/iso_8859_supp.h"
#include "cet/it.h"
#include "cet/jis_c6220_1969_ro.h"
#include "cet/jis_x0201.h"
#include "cet/jus_i_b1_002.h"
#include "cet/jus_i_b1_003_mac.h"
#include "cet/jus_i_b1_003_serb.h"
#include "cet/keybcs2.h"
#include "cet/koi8_u.h"
#include "cet/koi_7.h"
#include "cet/koi_8_cs2.h"
#include "cet/ksc5636.h"
#include "cet/latin_greek_1.h"
#include "cet/mac_is.h"
#include "cet/macintosh.h"
#include "cet/macintosh_ce.h"
#include "cet/msz_7795_3.h"
#include "cet/nats_dano.h"
#include "cet/nats_sefi.h"
#include "cet/nc_nc00_10.h"
#include "cet/nextstep.h"
#include "cet/nf_z_62_010.h"
#include "cet/nf_z_62_010__1973_.h"
#include "cet/ns_4551_1.h"
#include "cet/ns_4551_2.h"
#include "cet/pt.h"
#include "cet/pt2.h"
#include "cet/sami.h"
#include "cet/sen_850200_b.h"
#include "cet/sen_850200_c.h"
#include "cet/tcvn.h"
#include "cet/viscii.h"
#include "cet/vps.h"
#endif

#ifdef DEBUG_MEM

void
cet_check_cs(cet_cs_vec_t* vec)	/* test well sorted link & extra tables */
{
  cet_ucs4_link_t* link;

  if ((link = (cet_ucs4_link_t*)vec->ucs4_link)) {
    int i, j;

    for (i = 0, j = 1; j < vec->ucs4_links; i++, j++) {
      if (link[i].value >= link[j].value) {
        printf(MYNAME ": checked 0x%04x with 0x%04x\n", link[i].value, link[j].value);
        fatal(MYNAME ": \"%s\"-link-table unsorted !!!\n", vec->name);
      }

    }
  }
  if ((link = (cet_ucs4_link_t*)vec->ucs4_extra)) {
    int i, j;

    for (i = 0, j = 1; j < vec->ucs4_extras; i++, j++) {
      if (link[i].value >= link[j].value) {
        printf(MYNAME ": check 0x%04x with 0x%04x\n", link[i].value, link[j].value);
        fatal(MYNAME ": \"%s\"-extra-table unsorted !!!\n", vec->name);
      }

    }
  }
}

#endif

static signed int
cet_cs_alias_qsort_cb(const void* a, const void* b)
{
  const cet_cs_alias_t* va = (const cet_cs_alias_t*) a;
  const cet_cs_alias_t* vb = (const cet_cs_alias_t*) b;
  return case_ignore_strcmp(va->name, vb->name);
}

static signed int
cet_cs_vec_qsort_cb(const void* a, const void* b)
{
  const cet_cs_vec_t* va = *(cet_cs_vec_t**)a;
  const cet_cs_vec_t* vb = *(cet_cs_vec_t**)b;
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
  NULL,	/* dec */
  NULL,	/* enc */
  NULL,	/* link */
  0,
  0,
  NULL,	/* extra */
  0,	/* extras */
  NULL,
  0
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

#ifdef cet_cs_name_ansi_x3_4_1968
  cet_register_cs(&cet_cs_vec_ansi_x3_4_1968);
#endif
#ifdef cet_cs_name_atarist
  cet_register_cs(&cet_cs_vec_atarist);
#endif
#ifdef cet_cs_name_baltic
  cet_register_cs(&cet_cs_vec_baltic);
#endif
#ifdef cet_cs_name_bs_4730
  cet_register_cs(&cet_cs_vec_bs_4730);
#endif
#ifdef cet_cs_name_bs_viewdata
  cet_register_cs(&cet_cs_vec_bs_viewdata);
#endif
#ifdef cet_cs_name_cp1250
  cet_register_cs(&cet_cs_vec_cp1250);
#endif
#ifdef cet_cs_name_cp1251
  cet_register_cs(&cet_cs_vec_cp1251);
#endif
#ifdef cet_cs_name_cp1252
  cet_register_cs(&cet_cs_vec_cp1252);
#endif
#ifdef cet_cs_name_cp1253
  cet_register_cs(&cet_cs_vec_cp1253);
#endif
#ifdef cet_cs_name_cp1254
  cet_register_cs(&cet_cs_vec_cp1254);
#endif
#ifdef cet_cs_name_cp1255
  cet_register_cs(&cet_cs_vec_cp1255);
#endif
#ifdef cet_cs_name_cp1256
  cet_register_cs(&cet_cs_vec_cp1256);
#endif
#ifdef cet_cs_name_cp1257
  cet_register_cs(&cet_cs_vec_cp1257);
#endif
#ifdef cet_cs_name_csa_z243_4_1985_1
  cet_register_cs(&cet_cs_vec_csa_z243_4_1985_1);
#endif
#ifdef cet_cs_name_csa_z243_4_1985_2
  cet_register_cs(&cet_cs_vec_csa_z243_4_1985_2);
#endif
#ifdef cet_cs_name_csa_z243_4_1985_gr
  cet_register_cs(&cet_cs_vec_csa_z243_4_1985_gr);
#endif
#ifdef cet_cs_name_csn_369103
  cet_register_cs(&cet_cs_vec_csn_369103);
#endif
#ifdef cet_cs_name_cwi
  cet_register_cs(&cet_cs_vec_cwi);
#endif
#ifdef cet_cs_name_dec_mcs
  cet_register_cs(&cet_cs_vec_dec_mcs);
#endif
#ifdef cet_cs_name_din_66003
  cet_register_cs(&cet_cs_vec_din_66003);
#endif
#ifdef cet_cs_name_ds_2089
  cet_register_cs(&cet_cs_vec_ds_2089);
#endif
#ifdef cet_cs_name_ecma_cyrillic
  cet_register_cs(&cet_cs_vec_ecma_cyrillic);
#endif
#ifdef cet_cs_name_es
  cet_register_cs(&cet_cs_vec_es);
#endif
#ifdef cet_cs_name_es2
  cet_register_cs(&cet_cs_vec_es2);
#endif
#ifdef cet_cs_name_gb_1988_80
  cet_register_cs(&cet_cs_vec_gb_1988_80);
#endif
#ifdef cet_cs_name_gost_19768_87
  cet_register_cs(&cet_cs_vec_gost_19768_87);
#endif
#ifdef cet_cs_name_hp_roman8
  cet_register_cs(&cet_cs_vec_hp_roman8);
#endif
#ifdef cet_cs_name_ibm037
  cet_register_cs(&cet_cs_vec_ibm037);
#endif
#ifdef cet_cs_name_ibm1004
  cet_register_cs(&cet_cs_vec_ibm1004);
#endif
#ifdef cet_cs_name_ibm1026
  cet_register_cs(&cet_cs_vec_ibm1026);
#endif
#ifdef cet_cs_name_ibm1047
  cet_register_cs(&cet_cs_vec_ibm1047);
#endif
#ifdef cet_cs_name_ibm256
  cet_register_cs(&cet_cs_vec_ibm256);
#endif
#ifdef cet_cs_name_ibm273
  cet_register_cs(&cet_cs_vec_ibm273);
#endif
#ifdef cet_cs_name_ibm277
  cet_register_cs(&cet_cs_vec_ibm277);
#endif
#ifdef cet_cs_name_ibm278
  cet_register_cs(&cet_cs_vec_ibm278);
#endif
#ifdef cet_cs_name_ibm280
  cet_register_cs(&cet_cs_vec_ibm280);
#endif
#ifdef cet_cs_name_ibm284
  cet_register_cs(&cet_cs_vec_ibm284);
#endif
#ifdef cet_cs_name_ibm285
  cet_register_cs(&cet_cs_vec_ibm285);
#endif
#ifdef cet_cs_name_ibm297
  cet_register_cs(&cet_cs_vec_ibm297);
#endif
#ifdef cet_cs_name_ibm437
  cet_register_cs(&cet_cs_vec_ibm437);
#endif
#ifdef cet_cs_name_ibm500
  cet_register_cs(&cet_cs_vec_ibm500);
#endif
#ifdef cet_cs_name_ibm850
  cet_register_cs(&cet_cs_vec_ibm850);
#endif
#ifdef cet_cs_name_ibm851
  cet_register_cs(&cet_cs_vec_ibm851);
#endif
#ifdef cet_cs_name_ibm852
  cet_register_cs(&cet_cs_vec_ibm852);
#endif
#ifdef cet_cs_name_ibm855
  cet_register_cs(&cet_cs_vec_ibm855);
#endif
#ifdef cet_cs_name_ibm857
  cet_register_cs(&cet_cs_vec_ibm857);
#endif
#ifdef cet_cs_name_ibm860
  cet_register_cs(&cet_cs_vec_ibm860);
#endif
#ifdef cet_cs_name_ibm861
  cet_register_cs(&cet_cs_vec_ibm861);
#endif
#ifdef cet_cs_name_ibm862
  cet_register_cs(&cet_cs_vec_ibm862);
#endif
#ifdef cet_cs_name_ibm863
  cet_register_cs(&cet_cs_vec_ibm863);
#endif
#ifdef cet_cs_name_ibm864
  cet_register_cs(&cet_cs_vec_ibm864);
#endif
#ifdef cet_cs_name_ibm865
  cet_register_cs(&cet_cs_vec_ibm865);
#endif
#ifdef cet_cs_name_ibm868
  cet_register_cs(&cet_cs_vec_ibm868);
#endif
#ifdef cet_cs_name_ibm869
  cet_register_cs(&cet_cs_vec_ibm869);
#endif
#ifdef cet_cs_name_ibm871
  cet_register_cs(&cet_cs_vec_ibm871);
#endif
#ifdef cet_cs_name_ibm891
  cet_register_cs(&cet_cs_vec_ibm891);
#endif
#ifdef cet_cs_name_ibm903
  cet_register_cs(&cet_cs_vec_ibm903);
#endif
#ifdef cet_cs_name_ibm904
  cet_register_cs(&cet_cs_vec_ibm904);
#endif
#ifdef cet_cs_name_iec_p27_1
  cet_register_cs(&cet_cs_vec_iec_p27_1);
#endif
#ifdef cet_cs_name_iso_10367_box
  cet_register_cs(&cet_cs_vec_iso_10367_box);
#endif
#ifdef cet_cs_name_iso_5427
  cet_register_cs(&cet_cs_vec_iso_5427);
#endif
#ifdef cet_cs_name_iso_646_irv
  cet_register_cs(&cet_cs_vec_iso_646_irv);
#endif
#ifdef cet_cs_name_iso_6937_2_25
  cet_register_cs(&cet_cs_vec_iso_6937_2_25);
#endif
#ifdef cet_cs_name_iso_8859_1
  cet_register_cs(&cet_cs_vec_iso_8859_1);
#endif
#ifdef cet_cs_name_iso_8859_10
  cet_register_cs(&cet_cs_vec_iso_8859_10);
#endif
#ifdef cet_cs_name_iso_8859_13
  cet_register_cs(&cet_cs_vec_iso_8859_13);
#endif
#ifdef cet_cs_name_iso_8859_14
  cet_register_cs(&cet_cs_vec_iso_8859_14);
#endif
#ifdef cet_cs_name_iso_8859_15
  cet_register_cs(&cet_cs_vec_iso_8859_15);
#endif
#ifdef cet_cs_name_iso_8859_2
  cet_register_cs(&cet_cs_vec_iso_8859_2);
#endif
#ifdef cet_cs_name_iso_8859_3
  cet_register_cs(&cet_cs_vec_iso_8859_3);
#endif
#ifdef cet_cs_name_iso_8859_4
  cet_register_cs(&cet_cs_vec_iso_8859_4);
#endif
#ifdef cet_cs_name_iso_8859_5
  cet_register_cs(&cet_cs_vec_iso_8859_5);
#endif
#ifdef cet_cs_name_iso_8859_6
  cet_register_cs(&cet_cs_vec_iso_8859_6);
#endif
#ifdef cet_cs_name_iso_8859_7
  cet_register_cs(&cet_cs_vec_iso_8859_7);
#endif
#ifdef cet_cs_name_iso_8859_8
  cet_register_cs(&cet_cs_vec_iso_8859_8);
#endif
#ifdef cet_cs_name_iso_8859_9
  cet_register_cs(&cet_cs_vec_iso_8859_9);
#endif
#ifdef cet_cs_name_iso_8859_supp
  cet_register_cs(&cet_cs_vec_iso_8859_supp);
#endif
#ifdef cet_cs_name_it
  cet_register_cs(&cet_cs_vec_it);
#endif
#ifdef cet_cs_name_jis_c6220_1969_ro
  cet_register_cs(&cet_cs_vec_jis_c6220_1969_ro);
#endif
#ifdef cet_cs_name_jis_x0201
  cet_register_cs(&cet_cs_vec_jis_x0201);
#endif
#ifdef cet_cs_name_jus_i_b1_002
  cet_register_cs(&cet_cs_vec_jus_i_b1_002);
#endif
#ifdef cet_cs_name_jus_i_b1_003_mac
  cet_register_cs(&cet_cs_vec_jus_i_b1_003_mac);
#endif
#ifdef cet_cs_name_jus_i_b1_003_serb
  cet_register_cs(&cet_cs_vec_jus_i_b1_003_serb);
#endif
#ifdef cet_cs_name_keybcs2
  cet_register_cs(&cet_cs_vec_keybcs2);
#endif
#ifdef cet_cs_name_koi8_r
  cet_register_cs(&cet_cs_vec_koi8_r);
#endif
#ifdef cet_cs_name_koi8_ru
  cet_register_cs(&cet_cs_vec_koi8_ru);
#endif
#ifdef cet_cs_name_koi8_u
  cet_register_cs(&cet_cs_vec_koi8_u);
#endif
#ifdef cet_cs_name_koi_7
  cet_register_cs(&cet_cs_vec_koi_7);
#endif
#ifdef cet_cs_name_koi_8
  cet_register_cs(&cet_cs_vec_koi_8);
#endif
#ifdef cet_cs_name_koi_8_cs2
  cet_register_cs(&cet_cs_vec_koi_8_cs2);
#endif
#ifdef cet_cs_name_ksc5636
  cet_register_cs(&cet_cs_vec_ksc5636);
#endif
#ifdef cet_cs_name_latin_greek_1
  cet_register_cs(&cet_cs_vec_latin_greek_1);
#endif
#ifdef cet_cs_name_mac_is
  cet_register_cs(&cet_cs_vec_mac_is);
#endif
#ifdef cet_cs_name_macintosh
  cet_register_cs(&cet_cs_vec_macintosh);
#endif
#ifdef cet_cs_name_macintosh_ce
  cet_register_cs(&cet_cs_vec_macintosh_ce);
#endif
#ifdef cet_cs_name_msz_7795_3
  cet_register_cs(&cet_cs_vec_msz_7795_3);
#endif
#ifdef cet_cs_name_nats_dano
  cet_register_cs(&cet_cs_vec_nats_dano);
#endif
#ifdef cet_cs_name_nats_sefi
  cet_register_cs(&cet_cs_vec_nats_sefi);
#endif
#ifdef cet_cs_name_nc_nc00_10
  cet_register_cs(&cet_cs_vec_nc_nc00_10);
#endif
#ifdef cet_cs_name_nextstep
  cet_register_cs(&cet_cs_vec_nextstep);
#endif
#ifdef cet_cs_name_nf_z_62_010
  cet_register_cs(&cet_cs_vec_nf_z_62_010);
#endif
#ifdef cet_cs_name_nf_z_62_010__1973_
  cet_register_cs(&cet_cs_vec_nf_z_62_010__1973_);
#endif
#ifdef cet_cs_name_ns_4551_1
  cet_register_cs(&cet_cs_vec_ns_4551_1);
#endif
#ifdef cet_cs_name_ns_4551_2
  cet_register_cs(&cet_cs_vec_ns_4551_2);
#endif
#ifdef cet_cs_name_pt
  cet_register_cs(&cet_cs_vec_pt);
#endif
#ifdef cet_cs_name_pt2
  cet_register_cs(&cet_cs_vec_pt2);
#endif
#ifdef cet_cs_name_sami
  cet_register_cs(&cet_cs_vec_sami);
#endif
#ifdef cet_cs_name_sen_850200_b
  cet_register_cs(&cet_cs_vec_sen_850200_b);
#endif
#ifdef cet_cs_name_sen_850200_c
  cet_register_cs(&cet_cs_vec_sen_850200_c);
#endif
#ifdef cet_cs_name_tcvn
  cet_register_cs(&cet_cs_vec_tcvn);
#endif
#ifdef cet_cs_name_viscii
  cet_register_cs(&cet_cs_vec_viscii);
#endif
#ifdef cet_cs_name_vps
  cet_register_cs(&cet_cs_vec_vps);
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
cet_find_cs_by_name(const char* name)
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
cet_validate_cs(const char* cs, cet_cs_vec_t** vec, char** cs_name)
{
  cet_cs_vec_t* v;

  if ((cs == NULL) || (strlen(cs) == 0)) {	/* set default us-ascii */
    *vec = &cet_cs_vec_ansi_x3_4_1968;
    *cs_name = xstrdup(CET_CHARSET_ASCII);
    return 1;
  }

  v = cet_find_cs_by_name(cs);
  if (v != NULL) {
    *cs_name = strupper(xstrdup(v->name));
    *vec = v;
    return 1;
  } else {
    *cs_name = NULL;
    *vec = NULL;
    return 0;
  }
}

void
cet_convert_deinit(void)
{
  if (global_opts.charset_name != NULL) {
    xfree(global_opts.charset_name);
  }
  global_opts.charset = NULL;
  global_opts.charset_name = NULL;
}

void
cet_convert_init(const char* cs_name, const int force)
{
  if ((force != 0) || (global_opts.charset == NULL)) {
    cet_convert_deinit();
    if (0 == cet_validate_cs(cs_name, &global_opts.charset, &global_opts.charset_name)) {
      fatal("Unsupported character set \"%s\"!\n", cs_name);
    }
  }
}

/* -------------------------------------------------------------------- */

static void
cet_flag_waypt(const waypoint* wpt)
{
  ((waypoint*)(wpt))->wpt_flags.cet_converted = 1;
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

const char *
cet_convert_string(const QString& str) {
  // FIXME: this is really weird.  Since cet_convert_string wants to free
  // its argument (!) we make a duplicate just to satisfy that kind of goofy
  // requirement.
  return cet_convert_string(xstrdup(str.toUtf8().data()));
}

/* cet_convert_waypt: internal used within cet_convert_strings process */

static void
cet_convert_waypt(const waypoint* wpt)
{
  waypoint* w = (waypoint*)wpt;
  format_specific_data* fs;
  url_link* url_next;
  geocache_data* gc_data = (geocache_data*)wpt->gc_data;

  if ((cet_output == 0) && (w->wpt_flags.cet_converted != 0)) {
    return;
  }

  w->wpt_flags.cet_converted = 1;

  w->shortname = cet_convert_string(wpt->shortname);
  w->description = cet_convert_string(wpt->description);
  w->notes = cet_convert_string(wpt->notes);
  w->url = cet_convert_string(wpt->url);
  w->url_link_text = cet_convert_string(wpt->url_link_text);
  for (url_next = w->url_next; url_next; url_next = url_next->url_next) {
    url_next->url = cet_convert_string(url_next->url);
    url_next->url_link_text = cet_convert_string(url_next->url_link_text);
  }
  if (gc_data) {
    gc_data->placer = cet_convert_string(gc_data->placer);
    gc_data->hint = cet_convert_string(gc_data->hint);
  }

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

  rte->rte_name = cet_convert_string(route->rte_name);
  rte->rte_desc = cet_convert_string(route->rte_desc);
  rte->rte_url = cet_convert_string(route->rte_url);
}

/* cet_convert_route_tlr: internal used within cet_convert_strings process */

static void
cet_convert_route_tlr(const route_head* route)
{
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

/* %%% cet_disp_character_set_names %%%
 *
 * - Put all character set names and aliases to "FILE" - */

void
cet_disp_character_set_names(FILE* fout)
{
  int i, c, ac;
  cet_cs_vec_t* vec;
  cet_cs_vec_t** list;

  if (cet_cs_alias_ct == 0) {
    return;
  }

  c = 0;
  for (vec = cet_cs_vec_root; vec != NULL; vec = vec->next) {
    c++;
  }

  if (cet_cs_vec_ct != c) {
    fatal(MYNAME ": internal error \"%s\"!\n", "cet_disp_character_set_names");
  }

  list = (cet_cs_vec_t**)xcalloc(c, sizeof(*list));

  i = 0;								/* fill the list */
  for (vec = cet_cs_vec_root; vec != NULL; vec = vec->next) {
    list[i++] = vec;
  }
  qsort(list, c, sizeof(*list), cet_cs_vec_qsort_cb);		/* sort list by name */

  ac = 0;

  fprintf(fout, "GPSBabel builtin character sets: (-c option)\n");
  for (i = 0; i < c; i++) {
    char** a;

    vec = list[i];
    fprintf(fout, "\n* %s", vec->name);

    a = (char**)vec->alias;
    if (a != NULL) {
      int column = 0;
      int alias = 0;

      while (*a != NULL) {
        if (case_ignore_strcmp(*a, vec->name) != 0) {
          ac++;
          fprintf(fout, "%s%s%s",
                  (alias++ > 0) ? ", " : "",
                  (column++ % 6 == 0) ? "\n\t" : "",
                  *a);
        }
        a++;
      }
    }
  }
  fprintf(fout, "\n\n");
  fprintf(fout, "We have %d builtin character sets with %d aliases!\n", c, ac);
  xfree(list);
}

/* %%% cet_fprintf / cet_vfprintf %%%
 *
 * - print any special hard-coded characters from inside a module - */

int cet_gbfprintf(gbfile* stream, const cet_cs_vec_t* src_vec, const char* fmt, ...)
{
  int res;
  char* cout;
  va_list args;

  va_start(args, fmt);
  xvasprintf(&cout, fmt, args);
  va_end(args);

  if (global_opts.charset != src_vec) {
    if (src_vec != &cet_cs_vec_utf8) {
      char* ctemp = cet_str_any_to_utf8(cout, src_vec);
      xfree(cout);
      cout = ctemp;
    }
    if (global_opts.charset != &cet_cs_vec_utf8) {
      char* ctemp = cet_str_utf8_to_any(cout, global_opts.charset);
      xfree(cout);
      cout = ctemp;
    }
  }

  res = gbfprintf(stream, "%s", cout);
  xfree(cout);

  return res;
}

/*
 * 'str'  points to an array of XML_Chars which may be UNICODE16
 * words in native endianness.
 */

const char* xml_convert_to_char_string_n(const XML_Char* src, int* n)
{
#ifdef XML_UNICODE
  char* utf8;
  char* utf8b;
  int i, j;

  /*
   * '*n' is the number of source bytes.
   * Walk over that, converting each character and
   * discarding it, but tallying 'i' as the number of
   * bytes in the destination string.
   */
  i = 0;
  for (j = 0; j < *n; j++) {
    i += cet_ucs4_to_utf8(NULL, 6, src[j]);
  }

  /* Update output byte count in caller. */
  *n = i;

  /* Appropriately size (not zero terminated) buffer */
  utf8 = utf8b = xmalloc(i);

  for (j = 0; utf8 < utf8b + i; j++) {
    utf8 += cet_ucs4_to_utf8(utf8, 6, src[j]);
  }

  return utf8b;
#else
  return src;
#endif
}

/*
 * 'str'  points to NULL terminated string of XML_Chars which
 * may be UNICODE16 words in native endianness.
 */

const char* xml_convert_to_char_string(const XML_Char* src)
{
#ifdef XML_UNICODE
  char* utf8;
  char* utf8b;
  int i, j;
  const XML_Char* in = src;

  /* Walk source array until we find source terminator */
  i = 0;
  for (j = 0; src[j]; j++) {
    i += cet_ucs4_to_utf8(NULL, 6, src[j]);
  }

  /* We return a NUL terminated string. */
  utf8 = utf8b = xmalloc(i + 1);
  in = src;

  for (j = 0; utf8 < utf8b + i; j++) {
    utf8 += cet_ucs4_to_utf8(utf8, 6, src[j]);
  }
  *utf8 = '\0';

  return utf8b;

#else
  return src;
#endif
}


void xml_free_converted_string(const char* str)
{
#ifdef XML_UNICODE
  xfree((void*) str);
#endif
}

const char** xml_convert_attrs_to_char_string(const XML_Char** xml_attr)
{
#ifdef XML_UNICODE
  // First count size of array
  int size = 0;
  int i;
  const XML_Char** ptr;
  const char** char_attrs;

  if (xml_attr == NULL) {
    return NULL;
  }

  for (ptr = xml_attr; *ptr != NULL; ++ptr) {
    ++size;
  }

  // Allocate space
  char_attrs = xmalloc((size + 1) * sizeof(char*));

  // Duplicate strings
  for (i = 0; i < size; ++i) {
    char_attrs[i] = xml_convert_to_char_string(xml_attr[i]);
  }
  char_attrs[size] = NULL;

  return char_attrs;
#else
  return xml_attr;
#endif
}

void xml_free_converted_attrs(const char** attr)
{
#ifdef XML_UNICODE
  while (attr != NULL && *attr != NULL) {
    xfree((void*)*attr);
    ++attr;
  }
#endif
}
