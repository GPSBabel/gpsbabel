/*

    Character encoding transformation - basics

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

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* ! ALL vec PARAMETERS HAVE TO BE A VALID POINTER TO A cet_cs_vec_t RECORD  ! */

/* =========================================================================== */
/* %%%            single character or value transmission                   %%% */
/* --------------------------------------------------------------------------- */

/* %%% cet_char_to_ucs4 %%%
 *
 * single character to UCS-4 code %%%
 * return values: 0 if convertable character, otherwise 1
 */

int
cet_char_to_ucs4(const char src, const cet_cs_vec_t* vec, int* value)
{
  int trash, c;
  int* dest;

  c = ((unsigned char)src & 0xFF);
  dest = (value != NULL) ? value : &trash;

  *dest = c;
  c -= vec->ucs4_offset;

  if (c < 0) {
    return CET_SUCCESS;
  } else if ((c >= vec->ucs4_count) || (vec->ucs4_map[c] == -1)) {
    return CET_ERROR;
  } else {
    *dest = vec->ucs4_map[c];
    return CET_SUCCESS;
  }
}

/* %%% cet_ucs4_to_utf8 %%%
 *
 * convert single UCS-4 value into UTF-8 sequence
 *
 * return values: >= 0: length of produced UTF-8 sequence
 *                 < 0: -bytes more needed in target space
 */

int
cet_ucs4_to_utf8(char* dest, size_t dest_size, int value)
{
  int result;
  unsigned char trash[16];
  unsigned char* c;

  c = (dest != NULL) ? (unsigned char*) dest : trash;

  if ((value & 0xffffff80) == 0) {		/* <= 7 bits */
    if (dest_size < 1) {
      return (dest_size - 1);
    }
    *c++ = value;
    result = 1;
  } else if ((value & 0xfffff800) == 0) {	/* <= 11 bits */
    if (dest_size < 2) {
      return (dest_size - 2);
    }
    *c++ = (0xc0 | (value >> 6));
    *c++ = (0x80 | (value & 0x3f));
    result = 2;

  } else if ((value & 0xffff0000) == 0) {	/* <= 16 bits */
    if (dest_size < 3) {
      return (dest_size - 3);
    }
    *c++ = (0xe0 | (value >> 12));
    *c++ = (0x80 | ((value >> 6) & 0x3f));
    *c++ = (0x80 | (value & 0x3f));
    result = 3;
  } else if ((value & 0xffe00000) == 0) {	/* <= 21 bits */
    if (dest_size < 4) {
      return (dest_size - 4);
    }
    *c++ = (0xf0 | (value >> 18));
    *c++ = (0x80 | ((value >> 12) & 0x3f));
    *c++ = (0x80 | ((value >> 6) & 0x3f));
    *c++ = (0x80 | (value & 0x3f));
    result = 4;
  } else if ((value & 0xfc000000) == 0) {	/* <= 26 bits */
    if (dest_size < 5) {
      return (dest_size - 5);
    }
    *c++ = (0xf8 | (value >> 24));
    *c++ = (0x80 | ((value >> 18) & 0x3f));
    *c++ = (0x80 | ((value >> 12) & 0x3f));
    *c++ = (0x80 | ((value >> 6) & 0x3f));
    *c++ = (0x80 | (value & 0x3f));
    result = 5;
  } else if ((value & 0x80000000) == 0) {	/* <= 31 bits */
    if (dest_size < 6) {
      return (dest_size - 6);
    }
    *c++ = (0xfc | (value >> 30));
    *c++ = (0x80 | ((value >> 24) & 0x3f));
    *c++ = (0x80 | ((value >> 18) & 0x3f));
    *c++ = (0x80 | ((value >> 12) & 0x3f));
    *c++ = (0x80 | ((value >> 6) & 0x3f));
    *c++ = (0x80 | (value & 0x3f));
    result = 6;
  } else {
    return 0;				/* Value = -1 */
  }
  return result;
}

/* %%% cet_utf8_to_ucs4 %%%
 *
 * decode single UTF-8 sequence into UCS-4 value
 *
 * return values: 0 if success, otherwise 1
 */
int
cet_utf8_to_ucs4(const char* str, int* bytes, int* value)
{
  unsigned char* cp = (unsigned char*)str;

  if (*cp < 0x80) {
    if (bytes != NULL) {
      *bytes = 1;
    }
    if (value != NULL) {
      *value = *cp;
    }
    return CET_SUCCESS;
  } else {
    unsigned char bits = 0xc0;
    unsigned char mask = 0xe0;
    int len = 0;

    for (len = 1; len <= 6; len++) {		/* outer loop, test UTF-8 frame */
      if ((*cp & mask) == bits) {
        int i = len;
        while (i-- > 0) {
          cp++;
          if ((*cp & 0xc0) != 0x80) {
            break;  /* invalid 	*/
          } else if (i == 0) {		/* all valid 	*/
            char* c = (char*)str;		/* found valid sequence, now storing value */
            int res = *c++ & (mask ^ 0xFF);
            i = len;
            while (i-- > 0) {
              res = (res << 6) | (*c++ & 0x3f);
            }

            if (bytes != NULL) {
              *bytes = len + 1;
            }
            if (value != NULL) {
              *value = res;
            }
            return CET_SUCCESS;
          }
        }
      }
      bits = (bits >> 1) | 0x80;
      mask = (mask >> 1) | 0x80;
    }
  }
  if (bytes != NULL) {
    *bytes = 1;
  }
  if (value != NULL) {
    *value = *cp;
  }
  return CET_ERROR;						/* not valid */
}

/* %%% cet_ucs4_to_char %%%
 *
 * convert single UCS-4 value to original character from CS
 *
 * return values: coverted character or "CET_NOT_CONVERTABLE_DEFAULT"
 *                if not possible
 */
short
cet_ucs4_to_char(const int value, const cet_cs_vec_t* vec)
{
  cet_ucs4_link_t* link;

  if ((link = (cet_ucs4_link_t*)vec->ucs4_link)) {
    int i = 0;
    int j = vec->ucs4_links - 1;			/* validate ucs value against vec */
    while (i <= j) {
      int a = (i + j) >> 1;
      int x = link[a].value;

      if (x < value) {
        i = a + 1;
      } else if (x > value) {
        j = a - 1;
      } else {
        return link[a].origin;
      }
    }
  }

  if ((link = (cet_ucs4_link_t*)vec->ucs4_extra)) {	/* can be NULL */
    int i = 0;
    int j = vec->ucs4_extras - 1;
    while (i <= j) {
      int a = (i + j) >> 1;
      int x = link[a].value;

      if (x < value) {
        i = a + 1;
      } else if (x > value) {
        j = a - 1;
      } else {
        return link[a].origin;
      }
    }
  }

  if (value < vec->ucs4_offset + vec->ucs4_count) {
    return (char)value & 0xFF;
  } else {
    if (vec->fallback && (vec->fallback != vec)) {
      return cet_ucs4_to_char(value, vec->fallback);
    } else {
      return CET_NOT_CONVERTABLE_DEFAULT;
    }
  }
}

/* %%% cet_utf8_to_char %%%
 *
 * Convert single UTF-8 sequence directly into associated characted
 * by given character set.
 */

short
cet_utf8_to_char(const char* str, const cet_cs_vec_t* vec, /* out */ int* bytes, int* value)
{
  int b, v;

  cet_utf8_to_ucs4(str, &b, &v);			/* decode UTF-8 sequence */

  if (bytes != NULL) {
    *bytes = b;
  }
  if (value != NULL) {
    *value = v;
  }

  return cet_ucs4_to_char(v, vec);
}

/* =========================================================================== */
/* %%%              UTF-8 string manipulation functions                    %%% */
/* =========================================================================== */

/* %%% cet_utf8_strlen %%%
 *
 * Returns the number of valid (visible) characters.
 */
unsigned int
cet_utf8_strlen(const char* str)
{
  if (str) {
    const char* cin = str;
    int len = 0;

    while (*cin) {
      int bytes, value;
      if (CET_SUCCESS == cet_utf8_to_ucs4(cin, &bytes, &value)) {
        len++;
      }
      cin += bytes;
    }
    return len;
  } else {
    return 0;
  }
}

/* %%% cet_utf8_strdup %%%
 *
 * Checks and duplicates an UTF-8 string
 */
char*
cet_utf8_strdup(const char* str)
{
  if (str) {
    return cet_utf8_strndup(str, strlen(str));
  } else {
    return NULL;
  }
}

/* %%% cet_utf8_strndup %%%
 *
 * Checks and duplicates an UTF-8 string
 */
char*
cet_utf8_strndup(const char* str, const int maxlen)
{
  if (str) {
    const char* cin = str;
    char* res, *cout;
    int len = 0;

    res = cout = xstrdup(cin);

    while (*cin && (len < maxlen)) {
      int bytes, value;
      if (CET_SUCCESS == cet_utf8_to_ucs4(cin, &bytes, &value)) {
        cout += cet_ucs4_to_utf8(cout, 6, value);
        len += 1;
      }
      cin += bytes;
    }
    *cout = '\0';

    if ((cin - str) != (cout - res)) {
      cout = xstrdup(res);
      xfree(res);
      res = cout;
    }

    return res;
  } else {
    return NULL;
  }
}

/* =========================================================================== */
/* %%%                   full string transformation                        %%% */
/* =========================================================================== */

/* %%% cet_str_utf8_to_any %%%
 *
 * Converts a UTF-8 string to given character set
 */
char*
cet_str_utf8_to_any(const char* src, const cet_cs_vec_t* vec)
{
  char* c = (char*)src;
  int len;
  char* res, *dest, *cend;

  if (c == NULL) {
    return NULL;
  }
  if (vec->ucs4_count == 0) {
    return xstrdup(src);  /* UTF-8 -> UTF-8 */
  }

  len = strlen(c);
  res = dest = (char*) xmalloc(len + 1);	/* target will become smaller or equal length */

  cend = c + len;

  while (c < cend) {
    int bytes;
    *dest++ = cet_utf8_to_char(c, vec, &bytes, NULL);
    c += bytes;
  }
  *dest = '\0';

  return res;
}


/* %%% cet_str_any_to_utf8 %%%
 *
 * Converts a string from given character set to UTF-8
 */
char*
cet_str_any_to_utf8(const char* src, const cet_cs_vec_t* vec)
{
  int len, value;
  char* result, *cin, *cout;
  char temp = CET_NOT_CONVERTABLE_DEFAULT;

  cin = (char*)src;
  if (cin == NULL) {
    return NULL;
  }
  if (vec->ucs4_count == 0) {
    return xstrdup(src);  /* UTF-8 -> UTF-8 */
  }

  len = 0;
  while (*cin != '\0') {	/* determine length of resulting UTF-8 string */
    if (CET_ERROR == cet_char_to_ucs4(*cin++, vec, &value)) {
      cet_char_to_ucs4(temp, vec, &value);
    }
    len += cet_ucs4_to_utf8(NULL, 6, value);
  }

  result = cout = (char*) xmalloc(len + 1);
  cin = (char*)src;

  while (*cin != '\0') {
    if (CET_ERROR == cet_char_to_ucs4(*cin++, vec, &value)) {
      cet_char_to_ucs4(temp, vec, &value);
    }
    cout += cet_ucs4_to_utf8(cout, 6, value);
  }
  *cout = '\0';
  return result;
}

/* %%% cet_str_uni_to_utf8 %%%
 *
 * Converts an unicode string to UTF-8
 */
char*
cet_str_uni_to_utf8(const short* src, const int length)
{
  int i, len;
  unsigned short* cin;
  char* res, *cout;

  if (src == NULL) {
    return NULL;
  }

  len = 0;
  i = length;
  cin = (unsigned short*)src;

  while (i-- > 0) {
    len += cet_ucs4_to_utf8(NULL, 6, le_read16(cin++));
  }

  res = cout = (char*) xmalloc(len + 1);
  cin = (unsigned short*)src;
  i = length;

  while (i-- > 0) {
    cout += cet_ucs4_to_utf8(cout, 6, le_read16(cin++));
  }

  *cout = '\0';

  return res;
}

/* %%% cet_str_any_to_uni %%%
 *
 * Converts a string in given character set to a 'wide string' (unicode)
 */
short*
cet_str_any_to_uni(const char* src, const cet_cs_vec_t* vec, int* length)
{
  char* utf8;
  int len;
  short* res, *sout;

  if (! src) {
    utf8 = xstrdup("");
  } else if (vec->ucs4_count == 0) {
    utf8 = cet_utf8_strdup(src);  /* UTF-8 -> clean UTF-8 */
  } else {
    utf8 = cet_str_any_to_utf8(src, vec);
  }

  len = cet_utf8_strlen(utf8);
  res = sout = (short int*) xcalloc(2, len + 1);

  if (len) {
    char* cin = utf8;

    while (*cin) {
      int bytes, value;
      if (CET_SUCCESS == cet_utf8_to_ucs4(cin, &bytes, &value)) {
        le_write16(sout, value);
        sout++;
      }
      cin += bytes;
    }
  }

  *sout = 0;
  if (length) {
    *length = len;
  }
  xfree(utf8);

  return res;
}
