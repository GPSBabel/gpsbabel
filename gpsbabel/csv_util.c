/*
    Utilities for parsing Comma Seperated Value files (CSV)

    Copyright (C) 2002 Alex Mottram (geo_alexm at cox-internet.com)

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
#include <unistd.h>
#include "defs.h"
#include "csv_util.h"

#define MYNAME "CSV_UTIL"

/*********************************************************************/
/* csv_stringclean() - remove any unwanted characters from string.   */
/*                     returns copy of string.                       */
/*     usage: p = csv_stringclean(stringtoclean, "&,\"")             */
/*            (strip out ampersands, commas, and quotes.             */
/*********************************************************************/
char *
csv_stringclean(const char *string, const char *chararray) {
    char * p1;
    char * p2;
    const char * cp;
    char * tmp = xstrdup(string);

    if ((! string) || (! chararray)) {
        return (tmp);
    }

    /* p2 - end of the original string */
    p2 = tmp + strlen(tmp);
    
    cp = chararray;

    while (*cp) {
        p1 = tmp;
        while (*p1) {
            if (*cp == *p1) {
                /* we don't want this character! */
                strncpy(p1, p1 + 1, (p2 - p1));
                p1[p2 - p1] = '\0';
            }
            p1++;
        }
        cp++;
    }
    
    return (tmp);
}

/***********************************************************************************/
/* csv_stringtrim() - trim whitespace and leading and trailing enclosures (quotes) */
/*                    returns a copy of the modified string                        */
/*    usage: p = csv_stringtrim(string, "\"")                                      */
/***********************************************************************************/
char *
csv_stringtrim(const char *string, const char *enclosure)
{
    static const char *p1 = NULL;
    char *p2 = NULL;
    char * tmp = xstrdup(string);
    size_t elen;

    if (!strlen(string)) {
	return (tmp);
    }

    if (!enclosure) {
	elen = 0;
    } else {
	elen = strlen(enclosure);
    }

    p2 = tmp + strlen(tmp) - 1;
    p1 = tmp;

    /* trim off trailing whitespace */
    while ((p2 > p1) && isspace(*p2)) {
	p2--;
    }

    /* advance p1 past any leading whitespace */
    while ((p1 < p2) && (isspace(*p1))) {
	p1++;
    }

    /* if we have enclosures, skip past them in pairs */
    if (elen) {
	while (((size_t) (p2 - p1) > elen) &&
	       (strncmp(p1, enclosure, elen) == 0) &&
	       (strncmp((p2 - elen + 1), enclosure, elen) == 0)) {
	    p2 -= elen;
            p1 += elen;
	}
    }

    /* copy what's left over back into tmp. */
    strncpy(tmp, p1, (p2 - p1) + 1);
    tmp[(p2 - p1) + 1] = '\0';

    return (tmp);
}

/*****************************************************************************/
/* csv_lineparse() - extract data fields from a delimited string. designed   */
/*                   to handle quoted and delimited data within quotes.      */
/*                   returns temporary COPY of delimited data field (use it  */
/*                   or lose it on the next call).                           */
/*    usage: p = csv_lineparse(string, ",", "\"", line)  [initial call]      */
/*           p = csv_lineparse(NULL, ",", "\"", line)    [subsequent calls]  */
/*****************************************************************************/
char *
csv_lineparse(const char *stringstart, const char *delimited_by, 
		const char *enclosed_in, const int line_no)
{
    const char *sp;
    static const char *p = NULL;
    static char *tmp = NULL;
    size_t dlen, elen;
    int enclosedepth = 0;
    short int dfound;

    if (!p) {
	/* first pass thru */
	p =  stringstart;

	if (!p) {
	    /* last pass out */
	    return (NULL);
	}
    }

    if (tmp) {
	free(tmp);
	tmp = NULL;
    }

    /* the beginning of the string we start with (this pass) */
    sp = p;

    /* length of delimiters and enclosures */
    dlen = strlen(delimited_by);
    elen = strlen(enclosed_in);
    
    dfound = 0;

    while ((*p) && (!dfound)) {
        if ((elen) && (strncmp(p, enclosed_in, elen) == 0)) {
	    if (enclosedepth)
		enclosedepth--;
	    else
		enclosedepth++;
	}

	if ((!enclosedepth) && (strncmp(p, delimited_by, dlen) == 0)) {
	    dfound = 1;

	} else {
	    p++;
	}
    }

    /* allocate enough space for this data field */
    tmp = xcalloc((p - sp) + 1, sizeof(char));

    strncpy(tmp, sp, (p - sp));
    tmp[p - sp] = '\0'; 

    if (dfound) {
	/* skip over the delimited_by */
	p += dlen;
    } else {
	/* end of the line */
	p = NULL;
    }

    if (enclosedepth != 0) {
	fprintf(stderr, "%s: Warning- Unbalanced Field Enclosures (%s) on line %d\n",
		MYNAME, enclosed_in, line_no);
    }

    return (tmp);
}

