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

#define MYNAME "HTML"

static
arglist_t html_args[] = {
	{ "stylesheet", &stylesheet, 
		"Path to HTML style sheet", ARGTYPE_STRING },
	{ "encrypt", &encrypt,
		"Encrypt hints using ROT13", ARGTYPE_BOOL },
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
		wpt->latitude < 0 ? 'S' : 'N',  abs(latint), 60.0 * (fabs(wpt->latitude) - latint), 
		wpt->longitude < 0 ? 'W' : 'E', abs(lonint), 60.0 * (fabs(wpt->longitude) - lonint),
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
	else if (!wpt->notes && (!wpt->description || strcmp(wpt->notes,wpt->description))) {
		fprintf (file_out, "<p class=\"notes\">%s</p>\n", wpt->notes);
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
