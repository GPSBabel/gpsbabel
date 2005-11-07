/*
    Output only format for Human Readable formats.

    Copyright (C) 2004 Scott Brynen, scott (at) brynen.com
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
#include "jeeps/gpsmath.h"
#include <ctype.h>

static FILE *file_out;
static short_handle mkshort_handle;

static char *suppresssep = NULL;
static char *encrypt = NULL;
static char *includelogs = NULL;

#define MYNAME "TEXT"

static
arglist_t text_args[] = {
	{ "nosep", &suppresssep, 
		"Suppress separator lines between waypoints", 
		NULL, ARGTYPE_BOOL },
	{ "encrypt", &encrypt,
		"Encrypt hints using ROT13", NULL, ARGTYPE_BOOL },
	{ "logs", &includelogs,
		 "Include groundspeak logs if present", NULL, ARGTYPE_BOOL },
	{0, 0, 0, 0, 0}
};



static void
wr_init(const char *fname)
{
	file_out = xfopen(fname, "w", MYNAME);
	mkshort_handle = mkshort_new_handle();
}

static void
wr_deinit(void)
{
	fclose(file_out);
	mkshort_del_handle(&mkshort_handle);
}

static void
text_disp(const waypoint *wpt)
{
	int latint, lonint;
	char tbuf[1024];
	time_t tm = wpt->creation_time;
	int32 utmz;
	double utme, utmn;
	char utmzc;
	fs_xml *fs_gpx;
	
	lonint = abs((int) wpt->longitude);
	latint = abs((int) wpt->latitude);

	GPS_Math_WGS84_To_UTM_EN(wpt->latitude, wpt->longitude, 
		&utme, &utmn, &utmz, &utmzc);

	if (tm == 0) 
		tm = time(NULL);
	strftime(tbuf, sizeof(tbuf), "%d-%b-%Y", localtime(&tm));

	fprintf(file_out, "%-16s  %c%d %06.3f  %c%d %06.3f  (%ld%c %6.0f %7.0f)",
		(global_opts.synthesize_shortnames) ? mkshort_from_wpt(mkshort_handle, wpt) : wpt->shortname,
		wpt->latitude < 0 ? 'S' : 'N',  abs(latint), 60.0 * (fabs(wpt->latitude) - latint), 
		wpt->longitude < 0 ? 'W' : 'E', abs(lonint), 60.0 * (fabs(wpt->longitude) - lonint),
		utmz, utmzc, utme, utmn);
	if (wpt->altitude != unknown_alt) 
		fprintf (file_out, "  alt: %1.1f", wpt->altitude);
	fprintf (file_out, "\n");
	if (strcmp(wpt->description, wpt->shortname)) {
		fprintf(file_out, "%s\n", wpt->description);
	}
	if (wpt->gc_data.terr) {
	        if (wpt->gc_data.desc_short.utfstring) {
	                char *stripped_html = strip_html(&wpt->gc_data.desc_short);
			fprintf (file_out, "\n%s\n", stripped_html);
                	xfree(stripped_html);
       		}
	        if (wpt->gc_data.desc_long.utfstring) {
	                char *stripped_html = strip_html(&wpt->gc_data.desc_long);
			fprintf (file_out, "\n%s\n", stripped_html);
                	xfree(stripped_html);
       		}
		if (wpt->gc_data.hint) {
			char *hint = NULL;
			if ( encrypt ) 
				hint = rot13( wpt->gc_data.hint );
			else
				hint = xstrdup( wpt->gc_data.hint );
			fprintf (file_out, "\nHint: %s\n", hint);
			xfree( hint );
		}
	}
	else if (wpt->notes && (!wpt->description || strcmp(wpt->notes,wpt->description))) {
		fprintf (file_out, "%s\n", wpt->notes);
	}

	fs_gpx = NULL;
	if ( includelogs ) {
		fs_gpx = (fs_xml *)fs_chain_find( wpt->fs, FS_GPX);
	}
	
	if ( fs_gpx && fs_gpx->tag ) {
		xml_tag *root = fs_gpx->tag;
		xml_tag *curlog = NULL;
		xml_tag *logpart = NULL;
		curlog = xml_findfirst( root, "groundspeak:log" );
		while ( curlog ) {
			time_t logtime = 0;
			struct tm *logtm = NULL;
			fprintf( file_out, "\n" );
			
			logpart = xml_findfirst( curlog, "groundspeak:type" );
			if ( logpart ) {
				fprintf( file_out, "%s by ", logpart->cdata );
			}
			
			logpart = xml_findfirst( curlog, "groundspeak:finder" );
			if ( logpart ) {
				fprintf( file_out, "%s on ", logpart->cdata );
			}
			
			logpart = xml_findfirst( curlog, "groundspeak:date" );
			if ( logpart ) {
				logtime = xml_parse_time( logpart->cdata );
				logtm = localtime( &logtime );
				if ( logtm ) {
					fprintf( file_out, 
						"%2.2d/%2.2d/%4.4d\n",
						logtm->tm_mon+1,
						logtm->tm_mday,
						logtm->tm_year+1900
						);
				}
			}
			
			logpart = xml_findfirst( curlog, "groundspeak:log_wpt" );
			if ( logpart ) {
				char *coordstr = NULL;
				float lat = 0;
				int latdeg = 0;
				float lon = 0;
				int londeg = 0;
				coordstr = xml_attribute( logpart, "lat" );
				if ( coordstr ) {
					lat = atof( coordstr );
				}
			        coordstr = xml_attribute( logpart, "lon" );
				if ( coordstr ) {
					lon = atof( coordstr );
				}
				latdeg = abs(lat);
				londeg = abs(lon);
				
				fprintf( file_out,
					"%c %d %.3f' %c %d %.3f'\n",
				
					lat < 0 ? 'S' : 'N', latdeg, 60.0 * (fabs(lat) - latdeg), 
					lon < 0 ? 'W' : 'E', londeg, 60.0 * (fabs(lon) - londeg)
				);
			}
			
			logpart = xml_findfirst( curlog, "groundspeak:text" );
			if ( logpart ) {
				char *encstr = NULL;
				char *s = NULL;
				int encoded = 0;
				encstr = xml_attribute( logpart, "encoded" );
				encoded = (encstr[0] != 'F');
				
				if ( encrypt && encoded ) {
					s = rot13( logpart->cdata );
				}
				else {
					s = xstrdup( logpart->cdata );
				}
					
				fprintf( file_out, "%s", s ); 
				xfree( s );
			}

			fprintf( file_out, "\n" );
			curlog = xml_findnext( root, curlog, "groundspeak:log" );
		}
	}
	if (! suppresssep) 
		fprintf(file_out, "-----------------------------------------------------------------------------\n");
	else
		fprintf(file_out, "\n");
		
	
}

static void
data_write(void)
{
	if (! suppresssep) 
		fprintf(file_out, "-----------------------------------------------------------------------------\n");
	setshort_length(mkshort_handle, 6);
	waypt_disp_all(text_disp);
}


ff_vecs_t text_vecs = {
	ff_type_file,
	{ ff_cap_write, ff_cap_none, ff_cap_none},
	NULL,
	wr_init,
	NULL,
	wr_deinit,
	NULL,
	data_write,
	NULL, 
	text_args,
	CET_CHARSET_ASCII, 1	/* CET-REVIEW */

};
