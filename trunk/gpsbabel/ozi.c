/*
    OziExplorer Waypoint File Version 1.1 Format (.wpt)
    Fixed-Length Comma Delimited 

    Contributed to gpsbabel by Alex Mottram (geo_alexm at cox-internet.com)

    As described in Maptech Terrain Navigator Help File.
    Tested against Terrain Navigator and ExpertGPS import/export .MXF files.

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
#include <math.h> /* for floor */

#define MYNAME	"OZI"

static FILE *file_in;
static FILE *file_out;

static void 
rd_init(const char *fname)
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
wr_init(const char *fname)
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
    waypoint *wpt_tmp;
    int i;
    int linecount = 0;

    do {
        linecount++;
	memset(&buff, '\0', sizeof(buff));
	fgets(buff, sizeof(buff), file_in);

	if ((strlen(buff)) && (strstr(buff, ",") != NULL)) {

	    wpt_tmp = xcalloc(sizeof(*wpt_tmp), 1);

	    /* data delimited by commas, possibly enclosed in quotes.  */
	    s = buff;
	    s = csv_lineparse(s, ",", "", linecount);

	    i = 0;
	    while (s) {
		switch (i) {
		case 0:
		    /* sequence # */
		    break;
		case 1:
		    /* waypoint name */
		    wpt_tmp->shortname = csv_stringtrim(s, "");
		    break;
		case 2:
		    /* degrees latitude */
		    wpt_tmp->position.latitude.degrees = atof(s);
		    break;
		case 3:
		    /* degrees longitude */
		    wpt_tmp->position.longitude.degrees = atof(s);
		    break;
		case 4:
                    /* DAYS since 1900 00:00:00 in days.days (5.5) */
		    wpt_tmp->creation_time = (atof(s) - 25569.0) * 86400.0;                    
		    break;
		case 5:
		    /* icons 0-xx */
		    break;
		case 6:
		    /* unknown - always 1 */
		    break;
		case 7:
		    /* display format options 0-8 */
		    break;
		case 8:
		    /* foreground color (0=black) */
		    break;
		case 9:
		    /* background color (65535=yellow)*/
		    break;
		case 10:
		    /* Description */
		    wpt_tmp->description = csv_stringtrim(s, "");

		    break;
		case 11:
		    /* pointer direction 0,1,2,3 bottom,top,left,right */
		    break;
		case 12:
		    /* garmin gps display flags (0-name w/sym, 1-sym only, 2-comment w/symbol */
		    break;
		case 13:
		    /* proximity distance - meters */
		    break;
		case 14:
		    /* altitude in feet */
		    wpt_tmp->position.altitude.altitude_meters = (atof(s) * .3048);
		    break;
		case 15:
		    /* waypoint text name size */
		    break;
		case 16:
		    /* bold checkbox (1=bold, default 0) */
		    break;
		case 17:
		    /* symbol size - 17 default */
		    break;
		default:
		    /* whoa! nelly */
		    fprintf (stderr, "%s: Warning: data fields on line %d exceed specification.\n", 
		        MYNAME, linecount);
		    break;
		}
		i++;

		s = csv_lineparse(NULL, ",", "", linecount);
	    }
	    
   	    waypt_add(wpt_tmp);

	} else {
            /* empty line */
	}

    } while (!feof(file_in));
}

static void 
ozi_waypt_pr(const waypoint * wpt)
{
    static int index = 0;
    double alt_feet;
    double ozi_time;
    char * description;
    char * shortname;

    ozi_time = (wpt->creation_time / 86400.0) + 25569.0;
    alt_feet = (wpt->position.altitude.altitude_meters * 3.2808); 
    
    if ((! wpt->shortname) || (global_opts.synthesize_shortnames)) {
        if (wpt->description) {
            if (global_opts.synthesize_shortnames)
                shortname = mkshort(wpt->description);
            else
                shortname = csv_stringclean(wpt->description, ",");
        } else {
            /* no description available */
            shortname = xstrdup("");
        }
    } else{
        shortname = csv_stringclean(wpt->shortname, ",");
    }

    if (! wpt->description) {
        if (shortname) {
            description = csv_stringclean(shortname, ",");
        } else {
            description = xstrdup("");
        }
    } else{
        description = csv_stringclean(wpt->description, ",");
    }

    index++;

    fprintf(file_out, "%4d,%-14.14s,%11.6f,%11.6f,%011.5f,%3d,%2d,%2d,%10d,%10d,%-40.40s,%2d,%2d,%5d,%7.0f,%2d,%2d,%2d\n", 
	index, shortname, wpt->position.latitude.degrees, 
	wpt->position.longitude.degrees, ozi_time, 0, 1, 3, 0, 65535,
	description, 0, 0, 0, alt_feet, 6, 0, 17);

    free(description);
    free(shortname);

}

static void 
data_write(void)
{

    fprintf(file_out, "OziExplorer Waypoint File Version 1.1\n");
    fprintf(file_out, "WGS 84\n");
    fprintf(file_out, "Reserved 2\n");
    fprintf(file_out, "Reserved 3\n");

    if (global_opts.synthesize_shortnames) {
        setshort_length(32);
        setshort_whitespace_ok(0);
        setshort_badchars("\",");
    }

    waypt_disp_all(ozi_waypt_pr);
}

ff_vecs_t ozi_vecs = {
    rd_init,
    wr_init,
    rd_deinit,
    wr_deinit,
    data_read,
    data_write,
};

