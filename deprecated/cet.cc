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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
*/

#include <cstring>   // for strlen

#include "defs.h"
#include "cet.h"

/* ! ALL vec PARAMETERS HAVE TO BE A VALID POINTER TO A cet_cs_vec_t RECORD  ! */

/* =========================================================================== */
/* %%%            single character or value transmission                   %%% */
/* --------------------------------------------------------------------------- */

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

  unsigned char* c = (dest != nullptr) ? (unsigned char*) dest : trash;

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
  auto* cp = (unsigned char*)str;

  if (*cp < 0x80) {
    if (bytes != nullptr) {
      *bytes = 1;
    }
    if (value != nullptr) {
      *value = *cp;
    }
    return CET_SUCCESS;
  } else {
    unsigned char bits = 0xc0;
    unsigned char mask = 0xe0;

    for (int len = 1; len <= 6; len++) {		/* outer loop, test UTF-8 frame */
      if ((*cp & mask) == bits) {
        int i = len;
        while (i-- > 0) {
          cp++;
          if ((*cp & 0xc0) != 0x80) {
            break;  /* invalid 	*/
          } else if (i == 0) {		/* all valid 	*/
            const char* c = str;		/* found valid sequence, now storing value */
            int res = *c++ & (mask ^ 0xFF);
            i = len;
            while (i-- > 0) {
              res = (res << 6) | (*c++ & 0x3f);
            }

            if (bytes != nullptr) {
              *bytes = len + 1;
            }
            if (value != nullptr) {
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
  if (bytes != nullptr) {
    *bytes = 1;
  }
  if (value != nullptr) {
    *value = *cp;
  }
  return CET_ERROR;						/* not valid */
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
    return nullptr;
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
    char* cout;
    int len = 0;

    char* res = cout = xstrdup(cin);

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
    return nullptr;
  }
}
