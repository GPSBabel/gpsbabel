/*
    Access Magellan Mapsend files.

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

#include <stdio.h>
#include <string.h>


#include "defs.h"
#include "mapsend.h"

static FILE *mapsend_file_in;
static FILE *mapsend_file_out;

static int endianness_tested;
static int i_am_little_endian;

static void
test_endianness(void)
{
	union {
                long l;
                unsigned char uc[sizeof (long)];
        } u;
        unsigned int i;

        u.l = 1;
        i_am_little_endian = u.uc[0];

	endianness_tested = 1;

}

static void
mapsend_rd_init(const char *fname)
{
	mapsend_file_in = fopen(fname, "rb");
	if (mapsend_file_in == NULL) {
		fprintf(stderr, "Cannot open '%s' for reading\n", fname);
		exit(1);
	}
}

static void
mapsend_rd_deinit(void)
{
	fclose(mapsend_file_in);
}

static 
size_t 
my_fread8(void *ptr, FILE *stream)
{
	char cbuf[8];
	char *cptr = ptr;
	size_t rv;

	if (!endianness_tested) {
		test_endianness();
	}

	if (i_am_little_endian) {	
		rv = fread(ptr, 8, 1, stream);
	} else { 
		rv = fread(cbuf, 8, 1, stream);
		cptr[0] = cbuf[7];
		cptr[1] = cbuf[6];
		cptr[2] = cbuf[5];
		cptr[3] = cbuf[4];
		cptr[4] = cbuf[3];
		cptr[5] = cbuf[2];
		cptr[6] = cbuf[1];
		cptr[7] = cbuf[0];
	}
	return rv;
}

static 
size_t
my_fwrite8(void *ptr, FILE *stream)
{
	char cbuf[8];
	char *cptr = ptr;

	if (!endianness_tested) {
		test_endianness();
	}

	if (i_am_little_endian) {	
		return fwrite(ptr, 8, 1, stream);
	} else {
		cbuf[0] = cptr[7];
		cbuf[1] = cptr[6];
		cbuf[2] = cptr[5];
		cbuf[3] = cptr[4];
		cbuf[4] = cptr[3];
		cbuf[5] = cptr[2];
		cbuf[6] = cptr[1];
		cbuf[7] = cptr[0];
		return fwrite(cbuf, 8, 1, stream);
	}
}

static 
size_t 
my_fread4(void *ptr, FILE *stream)
{
	char cbuf[4];
	char *cptr = ptr;
	size_t rv;
	
	if (!endianness_tested) {
		test_endianness();
	}

	if (i_am_little_endian) {	
		rv = fread(ptr, 4, 1, stream);
	} else {
		rv = fread(cbuf, 4, 1, stream);
		cptr[0] = cbuf[3];
		cptr[1] = cbuf[2];
		cptr[2] = cbuf[1];
		cptr[3] = cbuf[0];
	}
	return rv;
}

static 
size_t
my_fwrite4(void *ptr, FILE *stream)
{
	char cbuf[4];
	char *cptr = ptr;

	if (!endianness_tested) {
		test_endianness();
	}

	if (i_am_little_endian) {	
		return fwrite(ptr, 4, 1, stream);
	} else {
		cbuf[0] = cptr[3];
		cbuf[1] = cptr[2];
		cbuf[2] = cptr[1];
		cbuf[3] = cptr[0];
		return fwrite(cbuf, 4, 1, stream);
	}
}

static void
mapsend_wr_init(const char *fname)
{
	mapsend_file_out = fopen(fname, "wb");
	if (mapsend_file_out == NULL) {
		fprintf(stderr, "Cannot open '%s' for writing\n", fname);
		exit(1);
	}
}

static void
mapsend_wr_deinit(void)
{
	fclose(mapsend_file_out);
}

static void
mapsend_read(void)
{
	mapsend_hdr hdr;
	char tbuf[256];
	char name[257];
	char comment[257];
	char *p;
	int wpt_count;
	unsigned char scount;
	int wpt_number;
	char wpt_icon;
	char wpt_status;
	double wpt_alt;
	double wpt_long;
	double wpt_lat;
	waypoint *wpt_tmp;

	/*
	 * Becuase of the silly struct packing and the goofy variable-length
	 * strings, each member has to be read in one at a time.  Grrr.
	 */

	fread(&hdr, sizeof(hdr), 1, mapsend_file_in);

	my_fread4(&wpt_count, mapsend_file_in);

	while (wpt_count--) {
		wpt_tmp = calloc(sizeof(*wpt_tmp), 1);

		fread(&scount, sizeof(scount), 1, mapsend_file_in);
		fread(tbuf, scount, 1, mapsend_file_in);
		p = strncpy(name, tbuf, scount);
		p[scount] = '\0';

		fread(&scount, sizeof(scount), 1, mapsend_file_in);
		fread(tbuf, scount, 1, mapsend_file_in);
		p = strncpy(comment, tbuf, scount);
		p[scount] = '\0';

		my_fread4(&wpt_number, mapsend_file_in);
		fread(&wpt_icon, sizeof(wpt_icon), 1, mapsend_file_in);
		fread(&wpt_status, sizeof(wpt_status), 1, mapsend_file_in);
		my_fread8(&wpt_alt, mapsend_file_in);
		my_fread8(&wpt_long, mapsend_file_in);
		my_fread8(&wpt_lat, mapsend_file_in);

		wpt_tmp->shortname = strdup(name);
		wpt_tmp->description = strdup(comment);
		wpt_tmp->position.altitude.altitude_meters = wpt_alt;
		wpt_tmp->position.latitude.degrees = -wpt_lat;
		wpt_tmp->position.longitude.degrees = wpt_long;

		waypt_add(wpt_tmp);
	}
}

static void
mapsend_waypt_pr(waypoint *waypointp)
{
	int n;
	unsigned char c;
	double falt;
	double flong;
	double flat;
static int cnt = 0;

	c = strlen(waypointp->shortname);
	fwrite(&c, 1, 1, mapsend_file_out);
	fwrite(waypointp->shortname, c, 1, mapsend_file_out);

	c = strlen(waypointp->description);
	fwrite(&c, 1, 1, mapsend_file_out);
	fwrite(waypointp->description, c, 1, mapsend_file_out);

	/* #, icon, status */
n = ++cnt;
	my_fwrite4(&n, mapsend_file_out);
n = 0;
	fwrite(&n, 1, 1, mapsend_file_out);
n = 1;
	fwrite(&n, 1, 1, mapsend_file_out);

	falt = waypointp->position.altitude.altitude_meters;
	my_fwrite8(&falt, mapsend_file_out);

	flong = waypointp->position.longitude.degrees;
	my_fwrite8(&flong, mapsend_file_out);
	flat = -waypointp->position.latitude.degrees;
	my_fwrite8(&flat, mapsend_file_out);
}

void
mapsend_write(void)
{
	mapsend_hdr hdr = {13, "4D533330 MS", "30", 1};
	int wpt_count = waypt_count();
int n = 0;

	fwrite(&hdr, sizeof(hdr), 1, mapsend_file_out);
	my_fwrite4(&wpt_count, mapsend_file_out);

	waypt_disp_all(mapsend_waypt_pr);

	my_fwrite4(&n, mapsend_file_out);
/* TODO: Impelment routes here */
}

ff_vecs_t mapsend_vecs = {
	mapsend_rd_init,
	mapsend_wr_init,
	mapsend_rd_deinit,
	mapsend_wr_deinit,
	mapsend_read,
	mapsend_write,
};
