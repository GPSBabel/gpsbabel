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
#ifdef DEBUG_MEM
XSTRNDUP(const char *str, size_t sz, DEBUG_PARAMS )
#else
xstrndup(const char *str, size_t sz)
#endif
{
	size_t newlen;
	char *newstr;

	newlen = strlen(str);
	if (newlen > sz) {
		newlen = sz;
	}

	newstr = (char *) xmalloc(newlen + 1);
	memcpy(newstr, str, newlen);    
	newstr[newlen] = 0;

	return newstr;
}

/*
 * Lazily trim whitespace (though not from allocated version) 
 * while copying.
 */
char *
#ifdef DEBUG_MEM
XSTRNDUPT(const char *str, size_t sz, DEBUG_PARAMS )
#else
xstrndupt(const char *str, size_t sz)
#endif
{
	size_t newlen;
	char *newstr;

	newlen = strlen(str);
	if (newlen > sz) {
		newlen = sz;
	}

	newstr = (char *) xmalloc(newlen + 1);
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
	char *o = (char *) realloc(p,s);
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
XSTRAPPEND(char *src, const char *newd, DEBUG_PARAMS)
#else
xstrappend(char *src, const char *newd)
#endif
{
	size_t newsz;

	if (!src) {
		return xxstrdup(newd, file, line);
	}

	newsz = strlen(src) + strlen(newd) + 1;
	src = xxrealloc(src, newsz, file, line);
	strcat(src, newd);

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

void
xfprintf(const char *errtxt, FILE *stream, const char *format, ...)
{
	va_list ap;
	va_start(ap, format);
	if (vfprintf(stream, format, ap) < 0) {
		fatal("%s writing output file.  Error was '%s'.\n",
				errtxt, strerror(errno));
	}
	va_end(ap);
}

void
xfputs(const char *errtxt, const char *s, FILE *stream)
{
	if (fputs(s, stream) < 0) {
		fatal("%s Writing output file.  Error was '%s'.\n",
				errtxt, strerror(errno));
	}
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
	time_t now = current_time();
	time_t later = mktime(gmtime(&now));

	if (later == -1) {
		return 0;
	} else {
		return (signed int) difftime(now, later);
	}
}

/*
 * A wrapper for time(2) that allows us to "freeze" time for testing.
 */
time_t
current_time(void)
{
	static char *frozen;
	
	if (getenv("GPSBABEL_FREEZE_TIME")) {
		return 0;
	}

	return time(NULL);
}

/*
 * Return the (zero based) month number of the year or -1 for failure.
 */
signed int
month_lookup(const char *m)
{
	static const char *months[] = {
		"JAN", "FEB", "MAR", "APR", "MAY", "JUN", 
		"JUL", "AUG", "SEP", "OCT", "NOV", "DEC", NULL };
	const char **mp;

	for (mp = months; *mp; mp++) {
		if (0 == case_ignore_strcmp(*mp, m))
			return mp - months;
	}
	return -1;
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
	switch (waypointp->gc_data.type) {
		case gt_virtual:
			return "Virtual cache";
		case gt_multi:
			return "Multi-Cache";
		case gt_event:
			return "Event Cache";
		case gt_suprise:
			return "Unknown Cache";
		case gt_webcam:
			return "Webcam Cache";
		default:
			break;
	}
	switch (waypointp->gc_data.container) {
		case gc_micro: 
			return "Micro-Cache";
			break;
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

char *
rot13( const char *s )
{
	char *result = xstrdup( s );
	char *cur = result;
	int flip = 1;
	while (cur && *cur ) {
		if ( flip ) {
			if (*cur == '[') flip = 0;
			else if ( *cur >= 'A' && *cur <= 'Z' ) {
				*cur = 'A' + ((*cur-'A')+13)%26;
			}
			else if ( *cur >= 'a' && *cur <= 'z' ) {
				*cur = 'a' + ((*cur-'a')+13)%26;
			}
		}
		else if ( *cur == ']' ) flip = 1;
		cur++;
	}
	return result;
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
			char *strvalue = NULL;
			utf8_to_int( cur, &bytes, &value );
			switch (value) {
				case 0x2026: strvalue = "..."; break;
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
			if (strvalue) {
				memcpy(cur, strvalue, bytes);
				cur += bytes - 1;
			} else {
				*cur = (char)value;
				strcpy( cur+1, cur+bytes );
			}
		}
		cur++;
	}
	return result;
}

/* 
 * Get rid of potentially nasty HTML that would influence another record
 * that includes;
 * <body> - to stop backgrounds from being loaded
 * </body> and </html>- stop processing altogether
 * <style> </style> - stop overriding styles for everything
 */
char *
strip_nastyhtml(const char * in)
{
	char *returnstr, *sp;
	char *lcstr, *lcp;
	int i;
	
	sp = returnstr = xstrdup(in);
	lcp = lcstr = xstrdup(in);
	
	while (*lcp) {
		*lcp = tolower(*lcp);
		lcp++;
	}
	while (lcp = strstr(lcstr, "<body")) {   /* becomes <---- */
		sp = returnstr + (lcp - lcstr) ;
		sp++; *sp++ = '-'; *sp++ = '-'; *sp++ = '-'; *sp++ = '-'; 
		*lcp = '*';         /* so we wont find it again */
	}
	while (lcp = strstr(lcstr, "</body")) {
		sp = returnstr + (lcp - lcstr) ; /* becomes </---- */
		sp++; sp++; *sp++ = '-'; *sp++ = '-'; *sp++ = '-'; *sp++ = '-'; 
		*lcp = '*';         /* so we wont find it again */
	}
	while (lcp = strstr(lcstr, "</html")) {
		sp = returnstr + (lcp - lcstr) ; /* becomes </---- */
		sp++; sp++; *sp++ = '-'; *sp++ = '-'; *sp++ = '-'; *sp++ = '-'; 
		*lcp = '*';         /* so we wont find it again */
	}
	while (lcp = strstr(lcstr, "<style")) {
		sp = returnstr + (lcp - lcstr) ; /* becomes <!--   */
		sp++; *sp++ = '!'; *sp++ = '-'; *sp++ = '-';  *sp++ = ' '; *sp++ = ' '; *sp = ' ';
		*lcp = '*';         /* so we wont find it again */
	}
	while (lcp = strstr(lcstr, "</style>")) {
		sp = returnstr + (lcp - lcstr) ; /* becomes    --> */
		*sp++ = ' '; *sp++ = ' '; *sp++ = ' '; *sp++ = ' '; *sp++ = ' '; *sp++ = '-'; *sp++ = '-'; 
		*lcp = '*';         /* so we wont find it again */
	}
	while (lcp = strstr(lcstr, "<image")) {
		sp = returnstr + (lcp - lcstr) ; /* becomes <img */
		sp+=3; *sp++ = 'g'; *sp++ = ' '; *sp++ = ' ';
		*lcp = '*';
	}
	xfree (lcstr);
	return (returnstr);
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
	char *instr = in->utfstring;
	char tag[8];
	short int taglen;
	
	if (!in->is_html)
		return in->utfstring;
	/*
	 * We only shorten, so just dupe the input buf for space.
	 */

	outstring = out = xstrdup(in->utfstring);

	tag[0] = 0;
	while (*instr) {
		if ((*instr == '<') || (*instr == '&')) {
			tag[0] = *instr;
			taglen = 0;
		}
		
		if (! tag[0]) {
			if (*instr != '\n')
				*out++ = *instr;
		}
		else {
			if (taglen < (sizeof(tag)-1)) {
				tag[taglen++] = tolower(*instr);
				tag[taglen] = 0;
			}
		}
		
		if ( ((tag[0] == '<') && (*instr == '>')) ||
		     ((tag[0] == '&') && (*instr == ';')) ) {
			if (! strcmp(tag,"&amp;"))
				*out++ = '&';
			else if (! strcmp (tag, "&lt;"))
				*out++ = '<';
			else if (! strcmp (tag, "&gt;"))
				*out++ = '>';
			else if (! strcmp (tag, "&quot;"))
				*out++ = '"';
			else if (! strcmp (tag, "&nbsp;"))
				*out++ = ' ';
			else if (! strcmp (tag, "&deg;")) {
				*out++ = 'd'; *out++ = 'e'; *out++ = 'g';
			}
			else if ((tag[0]=='<') && (tag[1]=='p'))
				*out++ = '\n';
			else if ((tag[0]=='<') && (tag[1]=='b') && (tag[2]=='r'))
				*out++ = '\n';
			else if ((tag[0]=='<') && (tag[1]=='/') && (tag[2]=='t') && (tag[3]=='r'))
				*out++ = '\n';
			else if ((tag[0]=='<') && (tag[1]=='/') && (tag[2]=='t') && (tag[3]=='d'))
				*out++ = ' ';
			else if ((tag[0]=='<') && (tag[1]=='i') && (tag[2]=='m') && (tag[3]=='g')) {
				*out++ = '['; *out++ = 'I'; *out++ = 'M'; *out++ = 'G'; *out++ = ']';
			}
			
		      tag[0] = 0;
		}
		*instr++;
	}
	*out++ = 0;
	return (outstring);
}

typedef struct {
	const char * text;
	const char * entity;
	int  not_html;
} entity_types;

static 
entity_types stdentities[] =  {
	{ "&",	"&amp;", 0 },
	{ "'", 	"&apos;", 1 },
	{ "<",	"&lt;", 0 },
	{ ">",	"&gt;", 0 },
	{ "\"",	"&quot;", 0 },
	{ NULL,	NULL, 0 }
};

static 
char * 
entitize(const char * str, int is_html) 
{
	int elen, ecount, nsecount;
	entity_types *ep;
	const char * cp;
	char * p, * tmp, * xstr;

	char tmpsub[20];
	int bytes = 0;
	int value = 0;
	ep = stdentities;
	elen = ecount = nsecount = 0;

	/* figure # of entity replacements and additional size. */
	while (ep->text) {
		cp = str;
		while ((cp = strstr(cp, ep->text)) != NULL) {
			elen += strlen(ep->entity) - strlen(ep->text);
			ecount++;
			cp += strlen(ep->text);
		}
		ep++;
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
		for (ep = stdentities; ep->text; ep++) {
			p = tmp;
			if (is_html && ep->not_html)  {
				continue;
			}
			while ((p = strstr(p, ep->text)) != NULL) {
				elen = strlen(ep->entity);

				xstr = xstrdup(p + strlen(ep->text));

				strcpy(p, ep->entity);
				strcpy(p + elen, xstr);

				xfree(xstr);

				p += elen;
			}  
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

/*
 * Public callers for the above to hide the absence of &apos from HTML
 */

char * xml_entitize(const char * str) 
{
	return entitize(str, 0);
}

char * html_entitize(const char * str) 
{
	return entitize(str, 1);
}

/*
 * xml_tag utilities
 */

xml_tag *xml_next( xml_tag *root, xml_tag *cur )
{
	if ( cur->child ) {
		cur = cur->child;
	}
	else if ( cur->sibling ) {
		cur = cur->sibling;
	}
	else {
		cur = cur->parent;
		if ( cur == root ) {
			cur = NULL;
		}
		if ( cur ) {
			cur = cur->sibling;
		}
	}
	return cur;
}

xml_tag *xml_findnext( xml_tag *root, xml_tag *cur, char *tagname ) 
{
	xml_tag *result = cur;
	do {
		result = xml_next( root, result );
	} while ( result && case_ignore_strcmp( result->tagname, tagname ));
	return result;
}

xml_tag *xml_findfirst( xml_tag *root, char *tagname )
{
	return xml_findnext( root, root, tagname );
}

char *xml_attribute( xml_tag *tag, char *attrname ) 
{
	char *result = NULL;
	if ( tag->attributes ) {
		char **attr = tag->attributes;
		while ( attr && *attr ) {
			if ( 0 == case_ignore_strcmp( *attr, attrname )) {
				result = attr[1];
				break;
			}
			attr+=2;
		}
	}
	return result;
}
