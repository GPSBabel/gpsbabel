/*
    Misc utilities.

    Copyright (C) 2002 Robert Lipe, robertlipe@usa.net

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
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <errno.h>

static int i_am_little_endian = -1;
static int doswap(void);

#ifdef DEBUG_MEM
#define DEBUG_FILENAME "/tmp/gpsbabel.debug"

static FILE *debug_mem_file = NULL;
void 
debug_mem_open() 
{
	debug_mem_file = xfopen( DEBUG_FILENAME, "a", "debug" );
}

void
debug_mem_output(char *format, ...) 
{
	va_list args;
	va_start( args, format );
	if ( debug_mem_file ) {
		vfprintf( debug_mem_file, format, args );
		fflush( debug_mem_file );
	}
	va_end( args );
}

void
debug_mem_close()
{
	if ( debug_mem_file ) {
		fclose(debug_mem_file);
	}
	debug_mem_file = NULL;
}
#endif

void *
#ifdef DEBUG_MEM
XMALLOC(size_t size, DEBUG_PARAMS)
#else
xmalloc(size_t size)
#endif
{
	void *obj = malloc(size);

#ifdef DEBUG_MEM
	debug_mem_output( "malloc, %x, %d, %s, %d\n", 
			obj, size, file, line );
#endif
	if (!obj) {
		fatal("gpsbabel: Unable to allocate %d bytes of memory.\n", size);
	}

	return obj;
}

void *
#ifdef DEBUG_MEM
XCALLOC(size_t nmemb, size_t size, DEBUG_PARAMS)
#else
xcalloc(size_t nmemb, size_t size)
#endif
{
	void *obj = calloc(nmemb, size);
#ifdef DEBUG_MEM
	debug_mem_output( "calloc, %x, %d, %d, %s, %d\n", 
			obj, nmemb, size, file, line );
#endif

	if (!obj) {
		fatal("gpsbabel: Unable to allocate %d bytes of memory.\n", size);
	}

	return obj;
}

void
#ifdef DEBUG_MEM
XFREE( void *mem, DEBUG_PARAMS )
#else
xfree( void *mem )
#endif
{
	free(mem);
#ifdef DEBUG_MEM
	debug_mem_output( "free, %x, %s, %d\n", 
			mem, file, line );
#endif
}

char *
#ifdef DEBUG_MEM
XSTRDUP(const char *s, DEBUG_PARAMS )
#else
xstrdup(const char *s)
#endif
{
	char *o = strdup(s);
#ifdef DEBUG_MEM
	debug_mem_output( "strdup, %x, %x, %s, %d\n", 
			o, s, file, line );
#endif

	if (!o) {
		fatal("gpsbabel: Unable to allocate %d bytes of memory.\n", strlen(s));
	}

	return o;
}

/*
 * Duplicate at most sz bytes in str.
 */
char *
xstrndup(const char *str, size_t sz)
{
	size_t newlen;
	char *newstr;

	newlen = strlen(str);
	if (newlen > sz) {
		newlen = sz;
	}

	newstr = xmalloc(newlen + 1);
	memcpy(newstr, str, newlen);    
	newstr[newlen] = 0;

	return newstr;
}

/*
 * Lazily trim whitespace (though not from allocated version) 
 * while copying.
 */
char *
xstrndupt(const char *str, size_t sz)
{
	size_t newlen;
	char *newstr;

	newlen = strlen(str);
	if (newlen > sz) {
		newlen = sz;
	}

	newstr = xmalloc(newlen + 1);
	memcpy(newstr, str, newlen);
	newstr[newlen] = '\0';
	rtrim(newstr);

	return newstr;
}

void *
#ifdef DEBUG_MEM
XREALLOC(void *p, size_t s, DEBUG_PARAMS )
#else
xrealloc(void *p, size_t s)
#endif
{
	char *o = realloc(p,s);
#ifdef DEBUG_MEM
	debug_mem_output( "realloc, %x, %x, %x, %s, %d\n", 
			o, p, s, file, line );
#endif

	if (!o) {
		fatal("gpsbabel: Unable to realloc %d bytes of memory.\n", s);
	}

	return o;
}

/*
* For an allocated string, realloc it and append 's'
*/
char *
#ifdef DEBUG_MEM
XSTRAPPEND(char *src, const char *new, DEBUG_PARAMS)
#else
xstrappend(char *src, const char *new)
#endif
{
	size_t newsz;

	if (!src) {
		return xxstrdup(new, file, line);
	}

	newsz = strlen(src) + strlen(new) + 1;
	src = xxrealloc(src, newsz, file, line);
	strcat(src, new);

	return src;
}

/*
 * Wrapper for open that honours - for stdin, stdout, unifies error text.
 */
FILE *
xfopen(const char *fname, const char *type, const char *errtxt)
{
	FILE *f;
	int am_writing = strchr(type, 'w') != NULL;

	if (fname == NULL) {
		fatal("%s must have a filename specified for %s.\n",
				errtxt, am_writing ? "write" : "read");
	}

	if (0 == strcmp(fname, "-"))
		return am_writing ? stdout : stdin;
	f = fopen(fname, type);
	if (NULL == f) {
		fatal("%s cannot open '%s' for %s.  Error was '%s'.\n",
				errtxt, fname, 
				am_writing ? "write" : "read", 
				strerror(errno));
	}
	return f;
}


/* 
 * Duplicate a pascal string into a normal C string.
 */
char *
pstrdup(char *src)
{
	int len = src[0];
	char *obuf = xmalloc(len + 1);

	memcpy(obuf, src + 1, len);
	obuf[len] = 0;

	return obuf;
}

void 
rtrim(char *s)
{
	char *t = s;

	if (!s || !*s) {
		return;
	}

	while (*s) {
		s++;
	}

	s--;
	while ((s >= t) && isspace (*s)) {
		*s = 0;
		s--;
	}
}

/*
 *   Like strcmp, but case insensitive.  Like Berkeley's strcasecmp.
 */

int 
case_ignore_strcmp(const char *s1, const char *s2)
{
	for(;toupper(*s1) == toupper(*s2); ++ s1, ++s2) {
		if (*s1 == 0)
			return 0;
	}
	return (toupper(*s1) < toupper(*s2)) ? -1 : +1;

}

void
printposn(const double c, int is_lat)
{
	char d;
	if (is_lat) {
		if (c < 0) d = 'S'; else d = 'N';
	} else {
		if (c < 0) d = 'W'; else d = 'E';
	}
	printf("%f%c ", fabs(c), d);
}

void
fatal(const char *fmt, ...)
{
	va_list ap;
	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	va_end(ap);
	exit(1);
}

void
warning(const char *fmt, ...)
{
	va_list ap;
	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	va_end(ap);
}

/*
 * Read 4 bytes in big-endian.   Return as "int" in native endianness.
 */
signed int
be_read32(void *p)
{
	unsigned char *i = (unsigned char *) p;
	return i[0] << 24 | i[1] << 16  | i[2] << 8 | i[3];
}

signed int
be_read16(void *p)
{
	unsigned char *i = (unsigned char *) p;
	return i[0] << 8 | i[1];
}

void
be_write16(void *addr, unsigned value)
{
	unsigned char *p = addr;
	p[0] = value >> 8;
	p[1] = value;
	
}

void
be_write32(void *pp, unsigned i)
{
	char *p = (char *)pp;

	p[0] = (i >> 24) & 0xff;
	p[1] = (i >> 16) & 0xff;
	p[2] = (i >> 8) & 0xff;
	p[3] = i & 0xff;
}

signed int
le_read16(void *addr)
{
	unsigned char *p = addr;
	return p[0] | (p[1] << 8);
}

signed int
le_read32(void *addr)
{
	unsigned char *p = addr;
	return p[0] | (p[1] << 8) | (p[2] << 16) | (p[3] << 24);
}

/*
 *  Read a little-endian 64-bit value from 'src' and return it in 'dest' 
 *  in host endianness.
 */
void
le_read64(void *dest, const void *src)
{
	char *cdest = dest;
	const char *csrc = src;

	doswap(); /* make sure i_am_little_endian is initialized */

	if (i_am_little_endian) {
		memcpy(dest, src, 8);
	} else {
		int i;
		for (i = 0; i < 8; i++) {
			cdest[i] = csrc[7-i];
		}
	}
}

void
le_write16(void *addr, unsigned value)
{
	unsigned char *p = addr;
	p[0] = value;
	p[1] = value >> 8;
	
}

void 
le_write32(void *addr, unsigned value)
{
	unsigned char *p = addr;
	p[0] = value;
	p[1] = value >> 8;
	p[2] = value >> 16;
	p[3] = value >> 24;
}

signed int 
si_round( double d )
{
	if ( d < 0 ) {
		return (signed int)(d-0.5);
	}
	else {
		return (signed int)(d+0.5);
	}
}

/*
 *  Return a time_t suitable for adding to a time_t that is in GMT to
 *  make it a local time.
 */
signed int 
get_tz_offset(void)
{
	time_t now = time(0);
	time_t later = mktime(gmtime(&now));

	if (later == -1) {
		return 0;
	} else {
		return (signed int) difftime(now, later);
	}
}

/*
 * Return a pointer to a constant string that is suitable for icon lookup
 * based on geocache attributes.   The strings used are those present in 
 * a GPX file from geocaching.com.  Thus we sort of make all the other 
 * formats do lookups based on these strings.
 */
const char *
get_cache_icon(const waypoint *waypointp)
{
	if (global_opts.no_smart_icons)
		return NULL;

	/*
	 * For icons, type overwrites container.  So a multi-micro will 
	 * get the icons for "multi".
 	 */
	switch (waypointp->gc_data.container) {
		case gc_micro: 
			return "Micro-Cache";
			break;
		default:
			break;
	}
	switch (waypointp->gc_data.type) {
		case gt_virtual:
			return "Virtual cache";
		case gt_multi:
			return "Multi-Cache";
		case gt_event:
			return "Event Cache";
		case gt_suprise:
			return "Unknown Cache";
		default:
			break;
	}
	return NULL;
}

static int doswap()
{
  if (i_am_little_endian < 0)
  {
	/*	On Intel, Vax and MIPs little endian, -1.0 maps to the bytes
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf0, 0x3f and on Motorola,
		SPARC, ARM, and PowerPC, it maps to
		0x3f, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00.
	*/
	double d = 1.0;
	char c[8];
	memcpy(c, &d, 8);
	i_am_little_endian = (c[0] == 0);
  }
  return i_am_little_endian;
}

double
pdb_read_double(void* ptr)
{
  double ret;
  char r[8];
  int i;
  doswap(); /* make sure i_am_little_endian is initialized */
  for (i = 0; i < 8; i++)
  {
	int j = (i_am_little_endian)?(7-i):i;
	r[i] = ((char*)ptr)[j];
  }
  memcpy(&ret, r, 8);
  return ret;
}

void
pdb_write_double(void* ptr, double d)
{
  char r[8];
  int i;
  char *optr = ptr;

  memcpy(r, &d, 8);
  doswap(); /* make sure i_am_little_endian is initialized */
  for (i = 0; i < 8; i++)
  {
	int j = (i_am_little_endian)?(7-i):i;
	*optr++ = r[j];
  }
  return;
}

/* Magellan and PCX formats use this DDMM.mm format */
double ddmm2degrees(double pcx_val) {
	double minutes;
	signed int deg;
	deg = (signed int) (pcx_val / 100.0);
	minutes = (((pcx_val / 100.0) - deg) * 100.0) / 60.0;
	return (double) deg + minutes;
}

double degrees2ddmm(double deg_val) {
	signed int deg;
	deg = (signed int) deg_val;
	return (double) (deg * 100.0) + ((deg_val - deg) * 60.0);
}

/*
 * replace a single occurrence of "search" in  "s" with "replace".
 * Returns an allocated copy if substitution was made, otherwise returns NULL.
 * Doesn't try to make an optimally sized dest buffer.
 */
char *
strsub(char *s, char *search, char *replace)
{
       char *p;
       int len = strlen(s);
       int slen = strlen(search);
       int rlen = strlen(replace);
       char *d;

       p = strstr(s, search);
       if (!slen || !p) {
               return NULL;
       }
       
       d = xmalloc(len + rlen);

       /* Copy first part */
       len = p - s;
       memcpy(d, s, len);
       d[len] = 0;

       /* Copy replacement */
       strcat(d, replace);

       /* Copy last part */
       strcat(d, p + slen);
       return d;
}

			
void utf8_to_int( const char *cp, int *bytes, int *value ) 
{
	if ( (*cp & 0xe0) == 0xc0 ) {
		if ( (*(cp+1) & 0xc0) != 0x80 ) goto dodefault;
		*bytes = 2;
		*value = ((*cp & 0x1f) << 6) | 
			(*(cp+1) & 0x3f); 
	}
	else if ( (*cp & 0xf0) == 0xe0 ) {
		if ( (*(cp+1) & 0xc0) != 0x80 ) goto dodefault;
		if ( (*(cp+2) & 0xc0) != 0x80 ) goto dodefault;
		*bytes = 3;
		*value = ((*cp & 0x0f) << 12) | 
			((*(cp+1) & 0x3f) << 6) | 
			(*(cp+2) & 0x3f); 
	}
	else if ( (*cp & 0xf8) == 0xf0 ) {
		if ( (*(cp+1) & 0xc0) != 0x80 ) goto dodefault;
		if ( (*(cp+2) & 0xc0) != 0x80 ) goto dodefault;
		if ( (*(cp+3) & 0xc0) != 0x80 ) goto dodefault;
		*bytes = 4;
		*value = ((*cp & 0x07) << 18) | 
			((*(cp+1) & 0x3f) << 12) | 
			((*(cp+2) & 0x3f) << 6) | 
			(*(cp+3) & 0x3f); 
	}
	else if ( (*cp & 0xfc) == 0xf8 ) {
		if ( (*(cp+1) & 0xc0) != 0x80 ) goto dodefault;
		if ( (*(cp+2) & 0xc0) != 0x80 ) goto dodefault;
		if ( (*(cp+3) & 0xc0) != 0x80 ) goto dodefault;
		if ( (*(cp+4) & 0xc0) != 0x80 ) goto dodefault;
		*bytes = 5;
		*value = ((*cp & 0x03) << 24) | 
			((*(cp+1) & 0x3f) << 18) | 
			((*(cp+2) & 0x3f) << 12) | 
			((*(cp+3) & 0x3f) << 6) |
			(*(cp+4) & 0x3f); 
	}
	else if ( (*cp & 0xfe) == 0xfc ) {
		if ( (*(cp+1) & 0xc0) != 0x80 ) goto dodefault;
		if ( (*(cp+2) & 0xc0) != 0x80 ) goto dodefault;
		if ( (*(cp+3) & 0xc0) != 0x80 ) goto dodefault;
		if ( (*(cp+4) & 0xc0) != 0x80 ) goto dodefault;
		if ( (*(cp+5) & 0xc0) != 0x80 ) goto dodefault;
		*bytes = 6;
		*value = ((*cp & 0x01) << 30) | 
			((*(cp+1) & 0x3f) << 24) | 
			((*(cp+2) & 0x3f) << 18) | 
			((*(cp+3) & 0x3f) << 12) |
			((*(cp+4) & 0x3f) << 6) |
			(*(cp+5) & 0x3f); 
	}
	else {
dodefault:
		*bytes = 1;
		*value = (unsigned char)*cp;
	}
}

char * str_utf8_to_cp1252( const char * str )
{
	char *result = xstrdup( str );
	char *cur = result;
	
	while ( cur && *cur ) {
		if ( *cur & 0x80 ) {
			int bytes;
			int value;
			utf8_to_int( cur, &bytes, &value );
			if ( value > 0xff ) {
				switch (value) {
					case 0x20AC: value = 0x80; break;
					case 0x201A: value = 0x82; break;
					case 0x0192: value = 0x83; break;
					case 0x201E: value = 0x84; break;
					case 0x2026: value = 0x85; break;
					case 0x2020: value = 0x86; break;
					case 0x2021: value = 0x87; break;
					case 0x02C6: value = 0x88; break;
					case 0x2030: value = 0x89; break;
					case 0x0160: value = 0x8A; break;
					case 0x2039: value = 0x8B; break;
					case 0x0152: value = 0x8C; break;
					case 0x017D: value = 0x8E; break;
					case 0x2018: value = 0x91; break;
					case 0x2019: value = 0x92; break;
					case 0x201C: value = 0x93; break;
					case 0x201D: value = 0x94; break;
					case 0x2022: value = 0x95; break;
					case 0x2013: value = 0x96; break;
					case 0x2014: value = 0x97; break;
					case 0x02DC: value = 0x98; break;
					case 0x2122: value = 0x99; break;
					case 0x0161: value = 0x9A; break;
					case 0x203A: value = 0x9B; break;
					case 0x0153: value = 0x9C; break;
					case 0x017E: value = 0x9E; break;
					case 0x0178: value = 0x9F; break;
					/* default is the generic "currency  */
					/* sign" because question marks  */
					/* just look stupid. */
					default: value = 0xA4; break;
				}
			}
			*cur = (char)value;
			strcpy( cur+1, cur+bytes );
		}
		cur++;
	}
	return result;
}

char * str_utf8_to_ascii( const char * str )
{
	char *result = xstrdup( str );
	char *cur = result;
	
	while ( cur && *cur ) {
		if ( *cur & 0x80 ) {
			int bytes;
			int value;
			utf8_to_int( cur, &bytes, &value );

			switch (value) {
				case 0x201c: value = '\"'; break;
				case 0x201d: value = '\"'; break;
				case 0xb4:
				case 0x2018: value = '`'; break;
				case 0x2019: value = '\''; break;
					    
				case 0xf2: 
				case 0xf3: 
				case 0xf4: 
				case 0xf5: 
				case 0xf6: value = 'o'; break;

				case 0xe0:
				case 0xe1:
				case 0xe2:
				case 0xe3:
				case 0xe4:
				case 0xe5: value = 'a'; break;

				case 0xe8:
				case 0xe9:
				case 0xea:
				case 0xeb: value = 'e'; break;

				case 0xc0:
				case 0xc1:
				case 0xc2:
				case 0xc3:
				case 0xc4:
				case 0xc5: value = 'A'; break;

				case 0xf8: value = '0'; break;
				default: value='?'; break;;
			}
			*cur = (char)value;
			strcpy( cur+1, cur+bytes );
		}
		cur++;
	}
	return result;
}

/*
 *  Without getting into all the complexity of technically legal HTML,
 *  this function tries to strip "ugly" parts of it to make it more 
 *  pleasant for a human reader.   Yes, this falls down in all kinds of
 *  ways such as spaces within the tags, etc.
 */
char * 
strip_html(const utf_string *in)
{
	char *outstring, *out;
	int ctr;
	char *instr = in->utfstring;

	if (!in->is_html)
		return in->utfstring;
	/*
	 * We only shorten, so just dupe the input buf for space.
	 */
	out = outstring = xstrdup(in->utfstring);
	outstring[0] = 0;

	for(ctr=0; ; instr++) {
		switch(*instr) {
			case 0: 
				return (out);

			case '<':
				if (instr[1] == 'p')
					*outstring++ = '\n';
				ctr++;
				break;
			case '>':
				ctr--;
				break;
			case '\n':
				continue;
			default:
				if (ctr == 0) {
					*outstring++ = *instr;
				}
		}
	}
}

char * xml_entitize(const char * str) 
{
	int elen, ecount, nsecount;
	const char ** ep;
	const char * cp;
	char * p, * tmp, * xstr;
	const char * stdentities[] = {
	"&",	"&amp;",
	"<",	"&lt;",
	">",	"&gt;",
	"'", 	"&apos;",
	"\"",	"&quot;",
	NULL,	NULL 
	};
	char tmpsub[20];
	int bytes = 0;
	int value = 0;
	ep = stdentities;
	elen = ecount = nsecount = 0;

	/* figure # of entity replacements and additional size. */
	while (*ep) {
		cp = str;
		while ((cp = strstr(cp, *ep)) != NULL) {
			elen += strlen(*(ep + 1)) - strlen(*ep);
			ecount++;
			cp += strlen(*ep);
		}
		ep += 2;
	}
	
	/* figure the same for other than standard entities (i.e. anything
	 * that isn't in the range U+0000 to U+007F */
	for ( cp = str; *cp; cp++ ) {
		if ( *cp & 0x80 ) {
			
			utf8_to_int( cp, &bytes, &value );
			cp += bytes-1;
			elen += sprintf( tmpsub, "&#x%x;", value ) - bytes;
		        nsecount++;	
		}
	}

	/* enough space for the whole string plus entity replacements, if any */
	tmp = xcalloc((strlen(str) + elen + 1), 1);
	strcpy(tmp, str);

	/* no entity replacements */
	if (ecount == 0 && nsecount == 0)
		return (tmp);

        if ( ecount != 0 ) {	
		ep = stdentities;

		while (*ep) {
			p = tmp;
			while ((p = strstr(p, *ep)) != NULL) {
				elen = strlen(*(ep + 1));

				xstr = xstrdup(p + strlen(*ep));

				strcpy(p, *(ep + 1));
				strcpy(p + elen, xstr);

				xfree(xstr);

				p += elen;
			}  
			ep += 2;
		}
	}

	if ( nsecount != 0 ) {
		p = tmp;
		while (*p) {
			if ( *p & 0x80 ) {
				utf8_to_int( p, &bytes, &value );
				if ( p[bytes] ) {
					xstr = xstrdup( p + bytes );
				}
				else {
					xstr = NULL;
				}
				sprintf( p, "&#x%x;", value );
				p = p+strlen(p);
				if ( xstr ) {
					strcpy( p, xstr );
					xfree(xstr);
				}
			}
			else {
				p++;
			}
		}
	}	
	return (tmp);
}
