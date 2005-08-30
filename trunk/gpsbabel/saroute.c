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

char *turns_important = NULL;
char *turns_only = NULL;
char *split = NULL;

static
arglist_t saroute_args[] = {
	{"turns_important", &turns_important, 
		"Keep turns if simplify filter is used", 
		NULL, ARGTYPE_BOOL },
	{"turns_only", &turns_only, "Only read turns; skip all other points",
		NULL, ARGTYPE_BOOL },
	{"split", &split, "Split into multiple routes at turns",
       		NULL, ARGTYPE_BOOL },
	{0, 0, 0, 0 }
};

unsigned short
ReadShort(FILE * f)
{
	gbuint16 result = 0;

	fread(&result, sizeof (result), 1, f);
	return le_read16(&result);
}

unsigned long
ReadLong(FILE * f)
{
	gbuint32 result = 0;

	fread(&result, sizeof (result), 1, f);
	return le_read32(&result);
}

unsigned char *
ReadRecord(FILE * f,
	   unsigned long size)
{
	unsigned char *result = (unsigned char *) xmalloc(size);

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
	infile = xfopen(fname, "rb", MYNAME);
	if ( split && (turns_important || turns_only )) {
		fatal( MYNAME 
		      ": turns options are not compatible with split\n" );
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
	unsigned long outercount;
	unsigned long recsize;
	unsigned short stringlen;
	unsigned char *record;
	static int serial = 0;
	struct ll {
		gbint32 lat;
		gbint32 lon;
	} *latlon;
	unsigned short coordcount;
	route_head *track_head = NULL;
	route_head *old_track_head = NULL;
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
	xfree(record);

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
	
			wpt_tmp = waypt_new();
			wpt_tmp->latitude = lat;
			wpt_tmp->longitude = -lon;
			wpt_tmp->shortname = (char *) xmalloc(7);
			sprintf( wpt_tmp->shortname, "\\%5.5x", serial++ );
			route_add_wpt(track_head, wpt_tmp);
			xfree(record);
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
	 * outercount is the number of route segments (start+end+stops+vias-1)
	 */

	outercount = ReadLong(infile);
	while (outercount) {

		/*
		 * unknown record (route params?) lives here 
		 */
		ReadShort(infile);
		recsize = ReadLong(infile);
		Skip(infile, recsize);

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
			old_track_head = NULL;
			ReadShort(infile);
			recsize = ReadLong(infile);
			record = ReadRecord(infile, recsize);
			stringlen = le_read16((unsigned short *)record);
			if ( split && stringlen ) {
			    if ( track_head->rte_waypt_ct ) {
				old_track_head = track_head;
				track_head = route_head_alloc();
				route_add_head( track_head );
			    } // end if
			    if ( !track_head->rte_name ) {
		   		track_head->rte_name = 
					(char *)xmalloc(stringlen+1);
				strncpy( track_head->rte_name, 
					record+2, stringlen );
				track_head->rte_name[stringlen] = '\0';
			    } 
			}
				
			coordcount = le_read16((unsigned short *)
					(record + 2 + stringlen + 0x3c));
			latlon = (struct ll *)(record + 2 + stringlen + 0x3c + 2);
			count--;
			if (count) {
				coordcount--;
			}
			while (coordcount) {
				double lat;
				double lon;

				wpt_tmp = waypt_new();

				lat = (0x80000000UL -
				       le_read32(&latlon->lat)) / 
					(double)(0x800000);
				lon = (0x80000000UL -
				       le_read32(&latlon->lon)) / 
					(double)(0x800000);

				wpt_tmp->latitude = lat;
				wpt_tmp->longitude = -lon;
				wpt_tmp->shortname = (char *) xmalloc(7);
				if ( turns_important && stringlen ) 
					wpt_tmp->route_priority=1;
				sprintf( wpt_tmp->shortname, "\\%5.5x", 
						serial++ );
				if ( !turns_only || stringlen ) {
					route_add_wpt(track_head, wpt_tmp);
					if ( old_track_head ) {
						route_add_wpt(old_track_head,
					           waypt_dupe(wpt_tmp));
						old_track_head = NULL;
					}
				}
				
	
				latlon++;
				coordcount--;
				stringlen = 0;
				/* the stop point is a "turn" */
				if ( coordcount == 1 && count == 0 ) {
					stringlen = 1;
				}
			}
			if ( version > 10 ) {
				Skip(infile,2*sizeof(gbuint32));
			}
			xfree(record);
		}
		/*
		 * end of routing 
		 */
		outercount--;
	}

}

static void
wr_init(const char *fname)
{
	fatal(MYNAME ":Not enough information is known about this format to write it.\n");
}

ff_vecs_t saroute_vecs = {
	ff_type_file,
	{ ff_cap_none, ff_cap_read, ff_cap_none},
	rd_init,
	wr_init,
	rd_deinit,
	NULL,
	my_read,
	NULL,
	NULL, 
	saroute_args,
	CET_CHARSET_UTF8, 1	/* do nothing | CET-REVIEW */
};
