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

static int i_am_little_endian = -1;
static int doswap(void);

#ifdef DEBUG_MEM
#define DEBUG_FILENAME "/tmp/gpsbabel.debug"

static FILE *debug_mem_file = NULL;
void 
debug_mem_open() 
{
	debug_mem_file = fopen( DEBUG_FILENAME, "a" );
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

coord
mkposn(const char *string)
{
	coord coord = {0};
	sscanf(string, "%lf", &coord.degrees);
	return coord;
}

void
printposn(const coord *c, int is_lat)
{
	char d;
	if (is_lat) {
		if (c->degrees < 0) d = 'S'; else d = 'N';
	} else {
		if (c->degrees < 0) d = 'W'; else d = 'E';
	}
	printf("%f%c ", fabs(c->degrees), d);
}

void
fprintdms(FILE *file, const coord *c, int is_lat)
{
	char d;
	if (is_lat) {
		if (c->degrees < 0) d = 'S'; else d = 'N';
	} else {
		if (c->degrees < 0) d = 'W'; else d = 'E';
	}
	fprintf(file, "%c%f\t", d, fabs(c->degrees));
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
