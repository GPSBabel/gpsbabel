#ifndef CET_H
#define CET_H

#include <ctype.h>
#include <stdio.h>

typedef struct cet_ucs4_link_s
{
	int value;			/* UCS-4 value 			*/
	short origin;			/* associeted character 	*/
} cet_ucs4_link_t;

typedef struct cet_cs_vec_s
{
	const char *name;			/* name of character set 	*/
	const char **alias;			/* alias table  		*/
	int (*decode)(const char *, int *);	/* ... to UCS-4 decoder 	*/
	short (*encode)(const int);		/* UCS-4 to ... encoder		*/
	const int *ucs4_map;			/* char to UCS-4 value table 	*/
	const int ucs4_offset;			/* first non standard character */
	const int ucs4_count;			/* values in table 		*/
	const cet_ucs4_link_t *ucs4_link;	/* UCS-4 to char backward links */
	const int ucs4_links;			/* number of links 		*/
	const cet_ucs4_link_t *ucs4_extra;	/* extra backward links		*/
	const int ucs4_extras;			/* number of extra links 	*/
	struct cet_cs_vec_s *next;
} cet_cs_vec_t;

/* single char/value transmission */

int cet_utf8_to_ucs4(const char *str, int *bytes, int *value);
int cet_ucs4_to_utf8(char *dest, size_t dest_size, int value);

/* single char/value transmission - vec based */

int cet_char_to_ucs4(const char *src, const cet_cs_vec_t *vec, int *value);
short cet_utf8_to_char(const char *str, const cet_cs_vec_t *vecint, int *bytes, int *value);
short cet_ucs4_to_char(const int value, const cet_cs_vec_t *vec);

/* string to string - vector based */

char *cet_str_utf8_to_any(const char *src, const cet_cs_vec_t *vec);
char *cet_str_any_to_utf8(const char *src, const cet_cs_vec_t *vec);

char *cet_str_uni_to_utf8(const short *src, const int length);

#endif
