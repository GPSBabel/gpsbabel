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

static char *suppresssep = NULL;
static char *encrypt = NULL;

#define MYNAME "TEXT"

static
arglist_t text_args[] = {
	{ "nosep", &suppresssep, 
		"Suppress separator lines between waypoints", ARGTYPE_BOOL },
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
text_disp(const waypoint *wpt)
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

	fprintf(file_out, "%-16s  %c%d %06.3f  %c%d %06.3f  (%ld%c %6.0f %7.0f)",
		(global_opts.synthesize_shortnames) ? mkshort(mkshort_handle, wpt->description) : wpt->shortname,
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
	else if (strcmp(wpt->notes,wpt->description)) {
		fprintf (file_out, "%s\n", wpt->notes);
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
	NULL,
	wr_init,
	NULL,
	wr_deinit,
	NULL,
	data_write,
	text_args
};
