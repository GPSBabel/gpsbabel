/* 
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
#include "xmlgeneric.h"

static char *encoded_points = NULL;
static char *encoded_levels = NULL;

FILE *fd;

static long serial = 0;

#define MYNAME "google"
#define MY_CBUF 4096

#if NO_EXPAT
void
google_rd_init(const char *fname)
{
	fatal(MYNAME ": This build excluded Google Maps support because expat was not installed.\n");
}

void
google_read(void)
{
}
#else

static xg_callback      goog_points, goog_levels, goog_poly_e;

static 
xg_tag_mapping google_map[] = {
	{ goog_points,  cb_cdata,       "/page/directions/polyline/points" },
	{ goog_levels,  cb_cdata,       "/page/directions/polyline/levels" },
	{ goog_poly_e,  cb_end,         "/page/directions/polyline" },
	{ NULL,         0,              NULL }
};

void goog_points( const char *args, const char **unused )
{
	if (args)  
	{
		if ( encoded_points )
		{
			encoded_points = xstrappend( encoded_points, args );
		}
		else 
		{
			encoded_points = xstrdup(args);
		}
	}
}

void goog_levels( const char *args, const char **unused )
{
	if (args)  
	{
		if ( encoded_levels )
		{
			encoded_levels = xstrappend( encoded_levels, args );
		}
		else 
		{
			encoded_levels = xstrdup(args);
		}
	}
}

static long decode_goog64( char **str )
{
	long result = 0;
	unsigned char c = 0;
	unsigned char shift = 0;
	
	do 
	{
		c = (unsigned char)(*(*str)++)-'?';
		result |= (c & 31)<<shift;
		shift += 5;
	} while ( c & ~31 );
	
	if ( result & 1 ) 
	{
		result = ~result;
	}
	return result/2;
}

void goog_poly_e( const char *args, const char **unused )
{
	long lat = 0;
	long lon = 0;
	long level = 0;
        char *str = encoded_points;
	char *lstr = encoded_levels;
	
	route_head *track_head = route_head_alloc();
	route_add_head(track_head);

	while ( str && *str ) 
	{
		lat += decode_goog64( &str );
		lon += decode_goog64( &str );
		
		level = 0;
		if ( lstr && *lstr ) 
		{
			level = decode_goog64( &lstr );
		} 

		{
			waypoint *wpt_tmp = waypt_new();
			wpt_tmp->latitude = lat / 100000.0;
			wpt_tmp->longitude = lon / 100000.0;
			wpt_tmp->route_priority=level;
			wpt_tmp->shortname = (char *) xmalloc(7);
			sprintf( wpt_tmp->shortname, "\\%5.5x", serial++ );
			route_add_wpt(track_head, wpt_tmp);
		}
	}
	
	if ( encoded_points ) 
	{
		xfree( encoded_points );
		encoded_points = NULL;
	}
	if ( encoded_levels )
	{
		xfree( encoded_levels );
		encoded_levels = NULL;
	}
}

void
google_rd_init(const char *fname)
{
	xml_init(fname, google_map);
}

void
google_read(void)
{
	xml_read();
}
#endif

void
google_rd_deinit(void)
{
	xml_deinit();
}

ff_vecs_t google_vecs = {
	ff_type_file,
        { ff_cap_none, ff_cap_read, ff_cap_none},
	google_rd_init,	
	NULL,
	google_rd_deinit,
	NULL,
	google_read,
	NULL,
	NULL, 
	NULL
};
