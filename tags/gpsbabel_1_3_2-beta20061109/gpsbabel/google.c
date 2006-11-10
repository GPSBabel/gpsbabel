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
static char *script = NULL;
static route_head *routehead;
static short_handle desc_handle;

static int serial = 0;

#define MYNAME "google"
#define MY_CBUF 4096

#if ! HAVE_LIBEXPAT
static void
google_rd_init(const char *fname)
{
	fatal(MYNAME ": This build excluded Google Maps support because expat was not installed.\n");
}

static void
google_read(void)
{
}
#else

static xg_callback      goog_points, goog_levels, goog_poly_e, goog_script;
static xg_callback	goog_segment_s, goog_segment, goog_td_s, goog_td_b;
static xg_callback	goog_td_e;

static 
xg_tag_mapping google_map[] = {
	{ goog_points,  cb_cdata,       "/page/directions/polyline/points" },
	{ goog_levels,  cb_cdata,       "/page/directions/polyline/levels" },
	{ goog_poly_e,  cb_end,         "/page/directions/polyline" },
	{ goog_script,  cb_cdata,       "/html/head/script" },
	{ goog_segment_s, cb_start,      "/page/directions/segments/segment" },
	{ goog_segment, cb_cdata,      "/page/directions/segments/segment" },
	{ goog_td_s,    cb_start,      "/div/table/tr/td" },
	{ goog_td_b,      cb_cdata,      "/div/table/tr/td/b" },
	{ goog_td_e,    cb_end,        "/div/table/tr/td" },
	{ NULL,         0,              NULL }
};

void goog_script( const char *args, const char **unused ) 
{
	if (args)
	{
		if ( script ) 
		{
			script = xstrappend( script, args );
		}
		else
		{
			script = xstrdup( args );
		}
	}
}			

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

static char goog_segname[7];
static char *goog_realname = NULL;

/*
 * The segments contain an index into the points array.  We use that
 * index to find the waypoint and insert a better name for it.
 */
void goog_segment_s( const char *args, const char **attrv )
{
        const char **avp = &attrv[0];
        while (*avp) {
                if (0 == strcmp(avp[0], "pointIndex")) {
			snprintf(goog_segname, sizeof(goog_segname), "\\%5.5x", atoi(avp[1]));
		}
		avp += 2;
	}

}

void goog_segment( const char *args, const char **unused )
{
	waypoint *wpt_tmp;

	wpt_tmp = route_find_waypt_by_name( routehead, goog_segname);
	if (wpt_tmp) {
		xfree(wpt_tmp->shortname);
		wpt_tmp->shortname = mkshort(desc_handle,args);
		wpt_tmp->description = xstrdup(args);
	}
}

void goog_td_s( const char *args, const char **attrv )
{
	const char **avp = &attrv[0];
	int isdesc = 0;
	while (*avp) {
		if ( 0 == strcmp(avp[0], "class" )) {
			isdesc = !strcmp(avp[1], "desc" );
		}
		else if ( isdesc && (0 == strcmp( avp[0], "id" ))) {
			snprintf( goog_segname, sizeof(goog_segname),
				"\\%5.5x",
				atoi(avp[1] + 6 ));
		}
		avp += 2;
	}
}
	
void goog_td_b( const char *args, const char **attrv ) {
	if ( goog_segname[0] == '\\' && !strchr( args, '\xa0')) {
		if ( goog_realname ) {
			xfree( goog_realname );
			goog_realname = NULL;
		}
		goog_realname = xmalloc( strlen(args)+1);
		strcpy( goog_realname, args );
	}
}
void goog_td_e( const char *args, const char **attrv ) 
{
	if ( goog_segname[0] == '\\' && goog_realname ) {
		goog_segment( goog_realname, attrv );
	}
	goog_segname[0] = '\0';
	if ( goog_realname ) {
		xfree( goog_realname );
		goog_realname = NULL;
	}
}

static long decode_goog64( char **str )
{
	long result = 0;
	unsigned char c = 0;
	unsigned char shift = 0;

        if ( !(**str)) {
		return 0;
	}
	
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
	long level1 = -9999;
	long level2 = -9999;
        char *str = encoded_points;
	char *lstr = encoded_levels;

	routehead = route_head_alloc();
	route_add_head(routehead);

	while ( str && *str ) 
	{
		lat += decode_goog64( &str );
		lon += decode_goog64( &str );
		
		level = -1;
		level2 = level1;
		if ( lstr && *lstr ) 
		{
			level1 = -decode_goog64( &lstr );
		}
		else 
		{
			level1 = -9999;
		}
		level = (level1<level2)?level1:level2;
		
		/* level of -9999 happens for endpoints */
	        if ( level == -9999 ) 
			level = 99999;	
		
		{
			waypoint *wpt_tmp = waypt_new();
			wpt_tmp->latitude = lat / 100000.0;
			wpt_tmp->longitude = lon / 100000.0;
			wpt_tmp->route_priority=level;
			wpt_tmp->shortname = (char *) xmalloc(7);
			sprintf( wpt_tmp->shortname, "\\%5.5x", serial++ );
			route_add_wpt(routehead, wpt_tmp);
		}
	}
	
}

static void
google_rd_init(const char *fname)
{
	desc_handle = mkshort_new_handle();
	setshort_length(desc_handle, 12);

	xml_init(fname, google_map, "ISO-8859-1" );
}

static void
google_read(void)
{
	xml_read();
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
	if ( script ) 
	{
		char *xml = strchr( script, '\'' );
		char *dict = strstr( script, "({" );
		
		char *end = NULL;
		
		if ( xml && (!dict || (xml < dict ))) {
			xml++;
			end = strchr( xml+1, '\'' );
			if ( end ) {
				*end = '\0';
				xml_deinit();
				xml_init( NULL, google_map, NULL );
				xml_readstring( xml );
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
		}
		else if ( dict ) {
		  char *panel = strstr( dict, "panel: '" );
		  encoded_points = strstr( dict, "points: '" );
		  encoded_levels = strstr( dict, "levels: '" );
		  
		  if ( encoded_points && encoded_levels ) {
	            encoded_points += 9;
		    encoded_levels += 9;
		    end = strchr( encoded_points, '\'' );
		    if ( end ) {
	              *end = '\0';
		      end = encoded_points;
		      while ( (end = strstr(end, "\\\\" ))) {
			memmove( end, end+1, strlen(end)+1 );
			end++;
		      }
		      end = strchr( encoded_levels, '\'' );
		      if ( end ) {
			*end = '\0';
		        end = encoded_levels;
  		        while ( (end = strstr(end, "\\\\" ))) {
			  memmove( end, end+1, strlen(end)+1 );
			  end++;
		        }
			goog_poly_e( NULL, NULL );
		      }
		    }		      
		  }
	          if ( panel ) {
		    panel += 8;
		    end = strstr( panel, "/table><div class=\\\"legal" );
		    if ( end ) {
	              strcpy(end,"/table></div>");
		      end = panel;
		      while ( (end = strstr( end, "\\\"" ))) {
		        memmove( end, end+1, strlen(end)+1 );
		      }
		      end = panel;
		      while ( (end = strstr( end, "\\'" ))) {
			memmove( end, end+1, strlen(end)+1 );
		      }
		      xml_deinit();
		      xml_init( NULL, google_map, NULL );
		      xml_readstring( panel );
		    }
		  }
		}
		xfree( script );
	}
}
#endif

static void
google_rd_deinit(void)
{
	xml_deinit();
	mkshort_del_handle(&desc_handle);
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
	NULL,
	CET_CHARSET_UTF8, 1	/* CET-REVIEW */
};
