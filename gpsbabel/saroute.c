/*
    Read various Delorme routes including anr, rte, and rtd.
 
    Copyright (C) 2003 Ron Parker and Robert Lipe.

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

#include <stddef.h>
#include <stdio.h>
#include <string.h>

#define MYNAME "saroute"
#include "defs.h"

FILE *infile;

unsigned short
ReadShort(FILE * f)
{
	unsigned short result = 0;

	fread(&result, sizeof (result), 1, f);
	return le_read16(&result);
}

unsigned long
ReadLong(FILE * f)
{
	unsigned long result = 0;

	fread(&result, sizeof (result), 1, f);
	return le_read32(&result);
}

unsigned char *
ReadRecord(FILE * f,
	   unsigned long size)
{
	unsigned char *result = xmalloc(size);

	fread(result, size, 1, f);
	return result;
}

void
Skip(FILE * f,
     unsigned long distance)
{
	fseek(f, distance, SEEK_CUR);
}

static void
rd_init(const char *fname)
{
	infile = fopen(fname, "rb");
	if (infile == NULL) {
		fatal(MYNAME ": Cannot open %s for reading\n", fname);
	}
}

static void
rd_deinit(void)
{
	fclose(infile);
}

static void
my_read(void)
{

	unsigned short version;
	unsigned long count;
	unsigned long recsize;
	unsigned char *record;
	unsigned short stringlen;
	struct ll {
		long lat;
		long lon;
	} *latlon;
	unsigned short coordcount;
	route_head *track_head;
	waypoint *wpt_tmp;

	ReadShort(infile);		/* magic */
	version = ReadShort(infile);
	
	ReadLong(infile);
	if (version >= 6) {
		ReadLong(infile);
		ReadLong(infile);
	}

	/*
	 * end of header 
	 */

	ReadShort(infile);
	recsize = ReadLong(infile);
	/*
	 * the first recsize, oddly, doesn't include the filename string
	 * but it does include the header. 
	 */
	record = ReadRecord(infile, recsize);

	stringlen = le_read16((unsigned short *)(record + 0x1a));
	Skip(infile, stringlen - 4);
	free(record);

	/*
	 * end of filename record 
	 */

	/*
	 * here lie the route description records 
	 */
	if ( version < 6 ) {
		track_head = route_head_alloc();
		route_add_head(track_head);
	}
	count = ReadLong(infile);
	while (count) {
		ReadShort(infile);
		recsize = ReadLong(infile);
		if (version < 6) {
			double lat;
			double lon;

			record = ReadRecord(infile, recsize);
			latlon = (struct ll *)(record);

			lat = (0x80000000UL -
			       le_read32(&latlon->lat)) / (double)(0x800000);
			lon = (0x80000000UL -
			       le_read32(&latlon->lon)) / (double)(0x800000);
	
			wpt_tmp = xcalloc(sizeof (*wpt_tmp), 1);
			wpt_tmp->latitude = lat;
			wpt_tmp->longitude = -lon;
			route_add_wpt(track_head, wpt_tmp);
		} else {
			Skip(infile, recsize);
			/*
			 * two longs of scrap after each record, don't know why 
			 */
			ReadLong(infile);
			ReadLong(infile);
		}
		count--;
	}
	/*
	 * end of route desc records 
	 */

	/*
	 * unknown record (route params?) lives here 
	 */
	count = ReadLong(infile);
	while (count) {
		ReadShort(infile);
		recsize = ReadLong(infile);
		Skip(infile, recsize);
		count--;
	}
	/*
	 * end of unknown record 
	 */

	/*
	 * routing begins here 
	 */
	count = ReadLong(infile);
	if ( count ) {
		track_head = route_head_alloc();
		route_add_head(track_head);
	}
	while (count) {
		ReadShort(infile);
		recsize = ReadLong(infile);
		record = ReadRecord(infile, recsize);
		stringlen = le_read16((unsigned short *)record);
		coordcount =
			le_read16((unsigned short *)(record + 2 + stringlen + 0x3c));
		latlon = (struct ll *)(record + 2 + stringlen + 0x3c + 2);
		count--;
		if (count) {
			coordcount--;
		}
		while (coordcount) {
			double lat;
			double lon;

			wpt_tmp = xcalloc(sizeof (*wpt_tmp), 1);

			lat = (0x80000000UL -
			       le_read32(&latlon->lat)) / (double)(0x800000);
			lon = (0x80000000UL -
			       le_read32(&latlon->lon)) / (double)(0x800000);

			wpt_tmp->latitude = lat;
			wpt_tmp->longitude = -lon;
			route_add_wpt(track_head, wpt_tmp);

			latlon++;
			coordcount--;
		}
		free(record);
	}
	/*
	 * end of routing 
	 */

}

static void
wr_init(const char *fname)
{
	fatal(MYNAME ":Not enough information is known about this format to write it.\n");
}

ff_vecs_t saroute_vecs = {
	rd_init,
	wr_init,
	rd_deinit,
	NULL,
	my_read,
	NULL,
	NULL
};
