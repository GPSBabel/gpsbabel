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

void *
xmalloc(size_t size)
{
	void *obj = malloc(size);

	if (!obj) {
		fatal("gpsbabel: Unable to allocate %d bytes of memory.\n", size);
	}

	return obj;
}

void *
xcalloc(size_t nmemb, size_t size)
{
	void *obj = calloc(nmemb, size);

	if (!obj) {
		fatal("gpsbabel: Unable to allocate %d bytes of memory.\n", size);
	}

	return obj;
}

char *
xstrdup(const char *s)
{
	char *o = strdup(s);

	if (!o) {
		fatal("gpsbabel: Unable to allocate %d bytes of memory.\n", strlen(s));
	}

	return o;
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
	printf("%lf%c ", fabs(c->degrees), d);
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
	fprintf(file, "%c%lf\t", d, fabs(c->degrees));
}
void
fatal(const char *fmt, ...)
{
	va_list ap;
	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	exit(1);
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
	char *i = (char *) p;
	return i[0] << 8 | i[0];
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
