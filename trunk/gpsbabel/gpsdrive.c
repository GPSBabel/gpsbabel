/*
  Space separated value files for GpsDrive waypoints.

  GpsDrive can be found @ http://www.kraftvoll.at/software
	
  Format: 

  waypoint latitude longitude 
   
  12/01/02 - hacked cvs.c by alan curry
  
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
#include "csv_util.h"
#include <ctype.h>

static FILE *file_in;
static FILE *file_out;
static void *mkshort_wr_handle;
static void *mkshort_rd_handle;

#define MYNAME "GPSDRIVE"

static void
rd_init(const char *fname, const char *args)
{
	file_in = fopen(fname, "r");
	if (file_in == NULL) {
		fatal(MYNAME ": Cannot open %s for reading\n", fname);
	}
}

static void
rd_deinit(void)
{
	fclose(file_in);
}

static void
wr_init(const char *fname, const char *args)
{
	file_out = fopen(fname, "w");
	if (file_out == NULL) {
		fatal(MYNAME ": Cannot open %s for writing\n", fname);
	}
}


static void
wr_deinit(void)
{
	fclose(file_out);
}

static void
data_read(void)
{
	char buff[1024];
	char *s;
	int i;
	waypoint *wpt_tmp;
	int linecount = 0;
	mkshort_rd_handle = mkshort_new_handle();

	do {
		linecount++;
		memset(buff, '\0', sizeof(buff));
		fgets(buff, sizeof(buff), file_in);
                 
		if (strlen(buff)) {

		    wpt_tmp = xcalloc(sizeof(*wpt_tmp), 1);
		    s = buff;

		    /* data delimited by spaces, not enclosed */
		    s = csv_lineparse(s, " ", "", linecount);

	        i = 0;

		while (s) {

			switch (i) {
			case 0:
				wpt_tmp->description = csv_stringtrim(s, " ");
				break;
			case 1:
				wpt_tmp->position.latitude.degrees = atof(s);
				break;
			case 2:
				wpt_tmp->position.longitude.degrees = atof(s);
				break;
			case 3:
				rtrim(s);
				wpt_tmp->icon_descr = xstrdup(s);
				wpt_tmp->icon_descr_is_dynamic = 1;
				break;
			default:
			    warning(MYNAME 
				": Warning: unmapped data fields on line %d.\n", 
			       	linecount);
			    break;
			}
			i++;

			s = csv_lineparse(NULL, " ", "", linecount);
		}
	    
		wpt_tmp->creation_time = time(NULL);
		
		/* We'll make up our own shortname. */
		if (wpt_tmp->description) {
			wpt_tmp->shortname = mkshort(mkshort_rd_handle, wpt_tmp->description);
			waypt_add(wpt_tmp);
		}

	} else {
		/* empty line */
	}

    } while (!feof(file_in));
    mkshort_del_handle(mkshort_rd_handle);
}

static void
gpsdrive_waypt_pr(const waypoint *wpt)
{
	double lon,lat;
	char * shortname = NULL;
	char *isrc, *owpt;
	char *tmpstr;

	lon = wpt->position.longitude.degrees;
	lat = wpt->position.latitude.degrees;

	isrc = wpt->notes ? wpt->notes : wpt->description;
	if (global_opts.synthesize_shortnames) {
		shortname = mkshort(mkshort_wr_handle, isrc);
	} else {
		if ( wpt->shortname )
			shortname = csv_stringclean(wpt->shortname, ",\"");
			
		if (( shortname == NULL ) && wpt->description )
			shortname = csv_stringclean(wpt->description, ",\"");	

		if (( shortname == NULL ) && wpt->notes )
		    shortname = csv_stringclean(wpt->notes, ",\"");

		if ( shortname ) {
			tmpstr = shortname;
			shortname = mkshort(mkshort_wr_handle, shortname);
			xfree(tmpstr);
		}
	}

	fprintf(file_out, "%s %08.5f %08.5f",
		shortname,
		lat,
		lon);
	if (wpt->icon_descr) {
		char *s = csv_stringclean(wpt->icon_descr, " ");
		fprintf(file_out, " %s", s);
		xfree(s);
	}
	fprintf(file_out, "\n");
		
	if (shortname)
		xfree(shortname);

}

static void
data_write(void)
{
	mkshort_wr_handle = mkshort_new_handle();
	setshort_length(mkshort_wr_handle, 20);
	setshort_whitespace_ok(mkshort_wr_handle, 0);

	waypt_disp_all(gpsdrive_waypt_pr);
	mkshort_del_handle(mkshort_wr_handle);
}

ff_vecs_t gpsdrive_vecs = {
	rd_init,
	wr_init,
	rd_deinit,
	wr_deinit,
	data_read,
	data_write,
};

