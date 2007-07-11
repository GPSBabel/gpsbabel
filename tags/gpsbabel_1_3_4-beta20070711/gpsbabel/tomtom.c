/*
    Read and write TomTom .ov2 files.

    Copyright (C) 2005  Ronald Parker (babeltomtom@parkrrrr.com) and
                        Robert Lipe (robertlipe@usa.net)

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


/*
    This module is based on my reverse-engineering of the .ov2 format, so
    it might not be aware of all record types.  In particular, I've seen 
    a type-3 record that may contain additional strings, but since I haven't
    seen any of those from a legitimate source, I don't know what they are
    supposed to contain.  Thus, they are not currently supported.  (The one
    I saw was due to an errant pair of double-quotes in the input to
    makeov2.exe.)   -- Ron Parker, 28 April 2005
    
    Because they've been seen in the wild, I have updated the reader to 
    deal with type 3 as if they were type 2.  I still haven't seen any 
    records that fill in the other two strings, so until I know for sure
    that they are indeed strings, I'm just putting them on the end of the 
    description string beyond the NUL terminator.  -- Ron Parker, 17 July 2006
*/
   

#include "defs.h"

#define MYNAME "TomTom"

static FILE *file_in;
static FILE *file_out;

static
arglist_t tomtom_args[] = {
	ARG_TERMINATOR
};

static void
rd_init(const char *fname)
{
	file_in = xfopen(fname, "rb", MYNAME);
}

static void
rd_deinit(void)
{
	fclose(file_in);
}

static void
wr_init(const char *fname)
{
	file_out = xfopen(fname, "wb", MYNAME);
}

static void
wr_deinit(void)
{
	fclose(file_out);
}

static unsigned long
read_long(FILE * f)
{
        gbuint32 result = 0;
	
        fread(&result, sizeof (result), 1, f);
        return le_read32(&result);
}

static unsigned char
read_char( FILE *f)
{
	unsigned char result = 0;
	fread( &result, 1, 1, f );
	return result;
}

static void
data_read(void)
{
	unsigned char rectype;
	long recsize;
	long x;
	long y;
	char *desc;
	waypoint *wpt_tmp;
	while (!feof( file_in ) ) {
		rectype = read_char( file_in );
		if ( rectype == 1 ) {
			/* a block header; ignored on read */
			read_long( file_in );
			read_long( file_in );
			read_long( file_in );
			read_long( file_in );
			read_long( file_in );
		}
		else if ( rectype == 2 || rectype == 3 ) {
			recsize = read_long( file_in );
			x = read_long( file_in );
			y = read_long( file_in );
			desc = (char *)xmalloc( recsize - 13 );
			fread( desc, recsize-13, 1, file_in );
			
			wpt_tmp = waypt_new();

			wpt_tmp->longitude = x/100000.0;
			wpt_tmp->latitude = y/100000.0;
			wpt_tmp->description = desc;

			waypt_add(wpt_tmp);
		}
	}
}


struct hdr{
	waypoint *wpt;
};

static int compare_lon(const void *a, const void *b);

static
int 
compare_lat(const void *a, const void *b)
{
	const struct hdr *wa = a;
	const struct hdr *wb = b;

        double difference = wa->wpt->latitude - wb->wpt->latitude;
	if ( difference < 0 ) {
		return -1;
	}
	if ( difference ) {
		return 1;
	}
	if ( wa->wpt->longitude - wb->wpt->longitude == 0 ) {
		return strcmp(wa->wpt->shortname, wb->wpt->shortname);
	}
	return compare_lon(a,b);
}

static
int 
compare_lon(const void *a, const void *b)
{
	const struct hdr *wa = a;
	const struct hdr *wb = b;

        double difference = wa->wpt->longitude - wb->wpt->longitude;
	if ( difference < 0 ) {
		return -1;
	}
	if ( difference ) {
		return 1;
	}
	if ( wa->wpt->latitude - wb->wpt->latitude == 0 ) {
		return strcmp(wa->wpt->shortname, wb->wpt->shortname);
	}
	return compare_lat(a,b);
}

static void 
write_long( FILE *file, long value ) {
	 gbuint32 tmp = 0;
	 le_write32( &tmp, value );
		 
	 fwrite( &tmp, sizeof(tmp), 1, file );
} 

static void
write_float_as_long( FILE *file, double value ) 
{
	long tmp = (value + 0.500000000001);
	write_long( file, tmp);
}

static void
write_char( FILE *file, unsigned char value ) {
	fwrite( &value, 1, 1, file );
}

static void
write_string( FILE *file, char *str ) {
	fwrite( str, strlen(str), 1, file );
	write_char( file, '\0' );
}

struct blockheader {
	struct hdr *start;
	long count;
	long size;
	double minlat;
	double maxlat;
	double minlon; 
	double maxlon;
	struct blockheader *ch1;
	struct blockheader *ch2;
};

static void
write_blocks( FILE *f, struct blockheader *blocks ) {
	int i;
	write_char( f, 1 );
	write_long( f, blocks->size );
	write_float_as_long( f, blocks->maxlon*100000 );
	write_float_as_long( f, blocks->maxlat*100000 );
	write_float_as_long( f, blocks->minlon*100000 );
	write_float_as_long( f, blocks->minlat*100000 );
	if ( blocks->ch1 ) {
		write_blocks( f, blocks->ch1 );
	}
	if ( blocks->ch2 ) {
		write_blocks( f, blocks->ch2 );
	}
	if ( !blocks->ch1 && !blocks->ch2 ) {
		for ( i = 0; i < blocks->count; i++ ) {
			write_char( f, 2 );
			write_long( f, strlen( blocks->start[i].wpt->description ) + 14 );
			write_float_as_long( f, blocks->start[i].wpt->longitude*100000);
			write_float_as_long( f, blocks->start[i].wpt->latitude*100000);
			write_string( f, blocks->start[i].wpt->description);
		}
	}
}	

static struct blockheader *
compute_blocks( struct hdr *start, int count,
	     double minlon, double maxlon, double minlat, double maxlat ) {
	struct blockheader *newblock;
	
	newblock = (struct blockheader *)xcalloc( sizeof( *newblock ), 1);
	newblock->start = start;
	newblock->count = count;
	newblock->minlon = minlon;
	newblock->maxlon = maxlon;
	newblock->minlat = minlat;
	newblock->maxlat = maxlat;
	newblock->size = 4 * 5 + 1;   /* hdr is 5 longs, 1 char */
	if ( count < 20 ) {
		int i;
		waypoint *wpt = NULL;

	        for ( i = 0; i < count; i++ ) {
			newblock->size += 4 * 3 + 1;  
				/* wpt const part 3 longs, 1 char */
			wpt = start[i].wpt;
			newblock->size += strlen( wpt->description ) + 1;
		}	
	}
	else {
		if ( (maxlat-minlat)>(maxlon-minlon)) {
			/* split along lats */
			qsort( start, count, sizeof(*start), compare_lat);
			newblock->ch1 = compute_blocks( start, count/2,
				minlon, maxlon, minlat, 
				start[count/2].wpt->latitude );
			newblock->ch2 = compute_blocks( start+count/2, 
				count-count/2, minlon, maxlon,
				start[count/2].wpt->latitude, maxlat );
		}	
		else {
			/* split along lons */
			qsort( start, count, sizeof(*start), compare_lon);
			newblock->ch1 = compute_blocks( start, count/2,
				minlon, start[count/2].wpt->longitude, 
				minlat, maxlat );
			newblock->ch2 = compute_blocks( start+count/2, 
				count-count/2, start[count/2].wpt->longitude, 
				maxlon, minlat, maxlat );
		}
		if ( newblock->ch1 ) {
			newblock->size += newblock->ch1->size;
		}
		if ( newblock->ch2 ) {
			newblock->size += newblock->ch2->size;
		}
	}
	return newblock;
}

static void
free_blocks( struct blockheader *block ) {
	if ( block->ch1 ) free_blocks( block->ch1 );
	if ( block->ch2 ) free_blocks( block->ch2 );
	xfree( block );
}

static void
data_write(void)
{
	int ct = waypt_count();
	struct hdr *htable, *bh;
        queue *elem, *tmp;
	extern queue waypt_head;
        waypoint *waypointp;
	double minlon = 200;
	double maxlon = -200;
	double minlat = 200;
	double maxlat = -200;
	struct blockheader *blocks = NULL;

	htable = xmalloc(ct * sizeof(*htable));
	bh = htable;

        QUEUE_FOR_EACH(&waypt_head, elem, tmp) {
	    waypointp = (waypoint *) elem;
	    bh->wpt = waypointp;
	    if ( waypointp->longitude > maxlon ) {
		    maxlon = waypointp->longitude;
	    }
	    if ( waypointp->longitude < minlon ) {
		    minlon = waypointp->longitude;
	    }
	    if ( waypointp->latitude > maxlat ) {
		    maxlat = waypointp->latitude;
	    }
	    if ( waypointp->latitude < minlat ) {
		    minlat = waypointp->latitude;
	    }
	    bh ++;
	}

	blocks = compute_blocks( htable, ct, minlon, maxlon, minlat, maxlat );
	write_blocks( file_out, blocks );
	free_blocks( blocks );
	
	xfree(htable);
}

ff_vecs_t tomtom_vecs = {
	ff_type_file,
	FF_CAP_RW_WPT,
	rd_init,
	wr_init,
	rd_deinit,
	wr_deinit,
	data_read,
	data_write,
	NULL,
	tomtom_args,
	CET_CHARSET_ASCII, 0	/* CET-REVIEW */
};
