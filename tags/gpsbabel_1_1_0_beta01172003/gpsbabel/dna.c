/*
  Comma separated value files for Navitrak DNA waypoints.
  WPT#,Northing,Easting,Name
  
  10/1/02 - t. e. zickus, shamelessly hacked from csv.c, below.

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

#define MYNAME "DNA"

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

	do {
		linecount++;
		memset(buff, '\0', sizeof(buff));
		fgets(buff, sizeof(buff), file_in);
                 
		if (strlen(buff)) {

		    wpt_tmp = xcalloc(sizeof(*wpt_tmp), 1);
		    s = buff;

		    /* data delimited by commas, not enclosed */
		    s = csv_lineparse(s, ",", "", linecount);

	    	    i = 0;

		while (s) {
			switch (i) {
			case 0: /* WPT #, skip. */
			  break;
			case 1:
				wpt_tmp->position.latitude.degrees = atof(s);
				break;
			case 2:
				wpt_tmp->position.longitude.degrees = atof(s);
			    	break;
			case 3:
		    		wpt_tmp->description = csv_stringtrim(s, " ");
		    		break;
			default:
			    	fprintf (stderr, "%s: Warning: unmapped data fields on line %d.\n", 
			        	MYNAME, linecount);
			    	break;
			}
			i++;

			s = csv_lineparse(NULL, ",", "", linecount);
		}
	    
		wpt_tmp->creation_time = time(NULL);
		
		/* We'll make up our own shortname. */
		wpt_tmp->shortname = mkshort(wpt_tmp->description);
		
		waypt_add(wpt_tmp);

	} else {
		/* empty line */
	}

    } while (!feof(file_in));
}

static void
dna_waypt_pr(const waypoint *wpt)
{
	double lon,lat;
	char * description = NULL;
	static int wpt_num = 0;

	lon = wpt->position.longitude.degrees;
	lat = wpt->position.latitude.degrees;

        if (wpt->description) 
	    description = csv_stringclean(wpt->description, ",\"");

	fprintf(file_out, "%d,%08.5f,%08.5f,%s\n",
		wpt_num++,
		lat,
		lon,
		description);
		
	if (description)
		free (description);

}

static void
data_write(void)
{
	waypt_disp_all(dna_waypt_pr);
}

ff_vecs_t dna_vecs = {
	rd_init,
	wr_init,
	rd_deinit,
	wr_deinit,
	data_read,
	data_write,
};
