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
static void *mkshort_handle;

static char *stylesheet = NULL;
static char *encrypt = NULL;
static char *includelogs = NULL;

#define MYNAME "HTML"

static
arglist_t html_args[] = {
	{ "stylesheet", &stylesheet, 
		"Path to HTML style sheet", ARGTYPE_STRING },
	{ "encrypt", &encrypt,
		"Encrypt hints using ROT13", ARGTYPE_BOOL },
	{ "logs", &includelogs, 
		"Include groundspeak logs if present", ARGTYPE_BOOL },
	{0, 0, 0, 0}
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
	mkshort_del_handle(mkshort_handle);
}

static void
html_disp(const waypoint *wpt)
{
	int latint, lonint;
	char tbuf[1024];
	time_t tm = wpt->creation_time;
	long utmz;
	double utme, utmn;
	char utmzc;
	
	lonint = abs(wpt->longitude);
	latint = abs(wpt->latitude);
	GPS_Math_WGS84_To_UTM_EN(wpt->latitude, wpt->longitude, 
		&utme, &utmn, &utmz, &utmzc);

	if (tm == 0) 
		tm = time(NULL);
	strftime(tbuf, sizeof(tbuf), "%d-%b-%Y", localtime(&tm));


	fprintf(file_out, "<hr>\n");
	fprintf(file_out, "<a name=\"%s\"></a><table width=\"100%%\"><tr><td>\n", wpt->shortname);
	fprintf(file_out, "<h3 class=\"waypoint\">%s - %c%d&deg;%06.3f %c%d&deg;%06.3f (%ld%c %6.0f %7.0f)",
		(global_opts.synthesize_shortnames) ? mkshort(mkshort_handle, wpt->description) : wpt->shortname,
		wpt->latitude < 0 ? 'S' : 'N',  latint, 60.0 * (fabs(wpt->latitude) - latint), 
		wpt->longitude < 0 ? 'W' : 'E', lonint, 60.0 * (fabs(wpt->longitude) - lonint),
		utmz, utmzc, utme, utmn);
	if (wpt->altitude != unknown_alt) 
		fprintf (file_out, " alt: %1.1f", wpt->altitude);
	fprintf (file_out, "<br>\n");

	if (strcmp(wpt->description, wpt->shortname)) {
		if (wpt->url) {
			char *d = html_entitize(wpt->description);
			fprintf(file_out, "<a href=\"%s\">%s</a></h3>\n", wpt->url, d);
			xfree(d);
		}
		else {
			fprintf(file_out, "%s</h3>\n", wpt->description);
		}		
		
	}
	if (wpt->gc_data.terr) {
	        if (wpt->gc_data.desc_short.utfstring) {
			fprintf (file_out, "<p class=\"descshort\">%s</p>\n", strip_nastyhtml(wpt->gc_data.desc_short.utfstring));
       		}
	        if (wpt->gc_data.desc_long.utfstring) {
			fprintf (file_out, "<p class=\"desclong\">%s</p>\n", strip_nastyhtml(wpt->gc_data.desc_long.utfstring));
       		}
		if (wpt->gc_data.hint) {
			char *hint = NULL;
			if ( encrypt )
				hint = rot13( wpt->gc_data.hint );
			else 
				hint = xstrdup( wpt->gc_data.hint );
			fprintf (file_out, "<p class=\"hint\"><strong>Hint:</strong> %s</p>\n", hint);
			xfree( hint );
		}
	}
	else if (wpt->notes && (!wpt->description || strcmp(wpt->notes,wpt->description))) {
		fprintf (file_out, "<p class=\"notes\">%s</p>\n", wpt->notes);
	}
	if ( includelogs && wpt->gpx_extras ) {
		xml_tag *root = wpt->gpx_extras;
		xml_tag *curlog = NULL;
		xml_tag *logpart = NULL;
		curlog = xml_findfirst( root, "groundspeak:log" );
		while ( curlog ) {
			time_t logtime = 0;
			struct tm *logtm = NULL;
			fprintf( file_out, "<p class=\"log\">\n" );
			
			logpart = xml_findfirst( curlog, "groundspeak:type" );
			if ( logpart ) {
				fprintf( file_out, "<span class=\"logtype\">%s</span> by ", logpart->cdata );
			}
			
			logpart = xml_findfirst( curlog, "groundspeak:finder" );
			if ( logpart ) {
				char *f = html_entitize( logpart->cdata );
				fprintf( file_out, "<span class=\"logfinder\">%s</span> on ", f );
				xfree( f );
			}
			
			logpart = xml_findfirst( curlog, "groundspeak:date" );
			if ( logpart ) {
				logtime = xml_parse_time( logpart->cdata );
				logtm = localtime( &logtime );
				if ( logtm ) {
					fprintf( file_out, 
						"<span class=\"logdate\">%2.2d/%2.2d/%4.4d</span><br>\n",
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
					"<span class=\"logcoords\">%c %d&deg; %.3f' %c %d&deg; %.3f'</span><br>\n",
				
					lat < 0 ? 'S' : 'N', latdeg, 60.0 * (fabs(lat) - latdeg), 
					lon < 0 ? 'W' : 'E', londeg, 60.0 * (fabs(lon) - londeg)
				);
			}
			
			logpart = xml_findfirst( curlog, "groundspeak:text" );
			if ( logpart ) {
				char *encstr = NULL;
				char *s = NULL;
				char *t = NULL;
				int encoded = 0;
				encstr = xml_attribute( logpart, "encoded" );
				encoded = (encstr[0] != 'F');
				
				if ( encrypt && encoded ) {
					s = rot13( logpart->cdata );
				}
				else {
					s = xstrdup( logpart->cdata );
				}
					
				t = html_entitize( s );
				fprintf( file_out, "%s", t ); 
				xfree( t );
				xfree( s );
			}

			fprintf( file_out, "</p>\n" );
			curlog = xml_findnext( root, curlog, "groundspeak:log" );
		}
	}
	fprintf(file_out, "</td></tr></table>\n");
}

static void
html_index(const waypoint *wpt)
{
	char *sn = html_entitize(wpt->shortname);
	char *d = html_entitize(wpt->description);

	fprintf(file_out, "<a href=\"#%s\">%s - %s</a><br>\n", sn, sn, d);

	xfree(sn);
	xfree(d);
}

static void
data_write(void)
{
	setshort_length(mkshort_handle, 6);

	fprintf(file_out, "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0 Transitional//EN\">\n");
	fprintf(file_out, "<html>\n");
	fprintf(file_out, "<head>\n");
	fprintf(file_out, " <title>GPSBabel HTML Output</title>\n");
	fprintf(file_out, " <meta name=\"Generator\" content=\"GPSBabel\">\n");
	if (stylesheet) 
		fprintf(file_out, " <link rel=\"stylesheet\" type=\"text/css\" href=\"%s\">\n", stylesheet);
	fprintf(file_out, "</head>\n");
	fprintf(file_out, "<body>\n");

	fprintf(file_out, "<p class=\"index\">\n");
	waypt_disp_all(html_index);
	fprintf(file_out, "</p>\n");
	
	waypt_disp_all(html_disp);

	fprintf(file_out, "</body>");
	fprintf(file_out, "</html>");

}


ff_vecs_t html_vecs = {
	ff_type_file,
	NULL,
	wr_init,
	NULL,
	wr_deinit,
	NULL,
	data_write,
	html_args
};
