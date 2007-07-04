/*

    Character encoding transformation - basics header

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

#ifndef CET_H
#define CET_H

#include <ctype.h>
#include <stdio.h>

#define CET_ERROR	1
#define CET_SUCCESS	0

typedef struct cet_ucs4_link_s
{
	int value;			/* UCS-4 value 			*/
	short origin;			/* associeted character 	*/
} cet_ucs4_link_t;

typedef struct cet_cs_vec_s
{
	const char *name;			/* name of character set 	*/
	const char **alias;			/* alias table  		*/
	struct cet_cs_vec_s *fallback;		/* fallback character set       */
	void *unused;
	const int *ucs4_map;			/* char to UCS-4 value table 	*/
	const int ucs4_offset;			/* first non standard character */
	const int ucs4_count;			/* values in table 		*/
	const cet_ucs4_link_t *ucs4_link;	/* UCS-4 to char backward links */
	const int ucs4_links;			/* number of links 		*/
	const cet_ucs4_link_t *ucs4_extra;	/* Non standard UCS-4 to ...    */
	const int ucs4_extras;			/* number of extra links 	*/
	struct cet_cs_vec_s *next;
} cet_cs_vec_t;

/* single char/value transmission */

int cet_utf8_to_ucs4(const char *str, int *bytes, int *value);
int cet_ucs4_to_utf8(char *dest, size_t dest_size, int value);

/* single char/value transmission - vec based */

int cet_char_to_ucs4(const char src, const cet_cs_vec_t *vec, int *value);
short cet_utf8_to_char(const char *str, const cet_cs_vec_t *vecint, int *bytes, int *value);
short cet_ucs4_to_char(const int value, const cet_cs_vec_t *vec);

/* string to string - vector based */

char *cet_str_utf8_to_any(const char *src, const cet_cs_vec_t *vec);
char *cet_str_any_to_utf8(const char *src, const cet_cs_vec_t *vec);

char *cet_str_uni_to_utf8(const short *src, const int length);

#endif
