/*
    ----------------------------------------------------------------------------------------
    TopoMapPro (.txt)
    New Zealand Mapping Software
    www.topomappro.com
    (Tab Delimited text file)

    Based on gpsbabel .MXF format by Alex Mottram (geo_alexm at cox-internet.com)
    Tweaked for TopoMapPro by Nick Heaphy (nick at automata dot co dot nz)

    Group sID sDescription fLat fLong fEasting fNorthing fAlt iColour iSymbol sHyperLink
    25    6   80           8    8     8        8         8    4       4       128  (lengths)
    
    Based on the specifications found in the TopoMapPro documentation available from website
    ----------------------------------------------------------------------------------------

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

#define MYNAME	"TMP"

static FILE *file_in;
static FILE *file_out;

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
    char *holder;
    waypoint *wpt_tmp;
    int i;
    int linecount = 0;

    do {
        linecount++;
	memset(&buff, '\0', sizeof(buff));
	fgets(buff, sizeof(buff), file_in);

	/* skip the line if it contains "sHyperLink" as it is a header (I hope :) */
	if ((strlen(buff)) && (strstr(buff, "sHyperLink") == NULL)) {

	    wpt_tmp = xcalloc(sizeof(*wpt_tmp), 1);

	    /* data delimited by tabs, not enclosed in quotes.  */
	    s = buff;
	    s = csv_lineparse(s, "\t", "", linecount);
	    
	    i = 0;
	    while (s) {
		switch (i) {
		
		/* Group sID sDescription fLat fLong fEasting fNorthing fAlt iColour iSymbol sHyperLink */
		/*   0    1       2         3    4      5         6       7    8       9       10       */
		
		case 0:
			 /* ignore: group  */
			 break;
		case 1:
		    wpt_tmp->shortname = csv_stringtrim(s, "");
		    break;
		case 2:
			/* Description is not a TopoMapPro format requirement.
			   If we assign "" then .loc/.gpx will generate empty XML tags :(
			*/
			holder = csv_stringtrim(s, "");
			if (strlen(holder))
		    	wpt_tmp->description = holder;
		    break;
		case 3:
		    wpt_tmp->position.latitude.degrees = atof(s);
		    break;
		case 4:
		    wpt_tmp->position.longitude.degrees = atof(s);
		    break;
		case 5:
			/* ignore: NZMapGrid Easting  */
			 break;
		case 6:
			/* ignore: NZMapGrid Northing  */
			 break;
		case 7:
			wpt_tmp->position.altitude.altitude_meters = atof(s);
		    break;
		case 8:
		    /* ignore: color  */
		    break;
		case 9:
		    /* ignore: symbol (non standard) */
		    break;
		case 10:
			/* URL is not a TopoMapPro format requirement.
			   You can store file links etc, we will discard anything that is not http
			   (as URLs in TMP must start "http:") as other GPS formats probably can't 
			   use the TopoMapLinks links.
			   (plus discards length 0 strings (so no empty XML tags))
			*/
			holder = csv_stringtrim(s, "");
			if (strstr(holder, "http:") != NULL)
				wpt_tmp->url = holder;
			break;
		default:
		    /* whoa! nelly */
		    fprintf (stderr, "%s: Warning: data fields on line %d exceed specification.\n", 
		        MYNAME, linecount);
		    break;
		}
		i++;

		s = csv_lineparse(NULL, "\t", "\"", linecount);
	    }
	    
	    if (i != 11) {
   	        free(wpt_tmp);
	        fprintf (stderr, "%s: WARNING - extracted %d fields from line %d. \nData on line ignored.\n", 
	            MYNAME, i, linecount);
	    } else {
   	        waypt_add(wpt_tmp);
   	    }

	} else {
            /* empty line */
	}

    } while (!feof(file_in));
}

static void 
tmp_waypt_pr(const waypoint * wpt)
{
    int icon = 1; /* default to "flag" */
    int colour = 255; /*default to red */
    char *shortname = NULL;
    char *description = NULL;

 	if ((! wpt->shortname) || (global_opts.synthesize_shortnames)) {
        if (wpt->description) {
            if (global_opts.synthesize_shortnames)
                shortname = mkshort(wpt->description);
            else
                shortname = csv_stringclean(wpt->description, ",\"");
        } else {
            /* no description available */
            shortname = xstrdup("");
        }
    } else{
        shortname = csv_stringclean(wpt->shortname, ",\"");
    }

    if (! wpt->description) {
        if (shortname) {
            description = csv_stringclean(shortname, ",\"");
        } else {
            description = xstrdup("");
        }
    } else{
        description = csv_stringclean(wpt->description, ",\"");
    }
    
    /* Group sID sDescription fLat fLong fEasting fNorthing fAlt iColour iSymbol sHyperLink */
	/*   0    1       2         3    4      5         6       7    8       9       10       */
	/* Number of characters */
    /*  25    6      80         8    8      8         8       8    4       4       128      */
    
    fprintf(file_out, "new\t%.6s\t%.80s\t%08.6f\t%08.6f\t0\t0\t%.2f\t%d\t%d\t%.128s\n",
    	shortname,
    	description,
	    wpt->position.latitude.degrees,
	    wpt->position.longitude.degrees,
	    wpt->position.altitude.altitude_meters,
	    colour,
	    icon,
	    wpt->url
	);
	    

    if (description)
	free(description);
    if (shortname)
	free(shortname);
}

static void 
data_write(void)
{
	/* Short names */
	if (global_opts.synthesize_shortnames) {
        setshort_length(6);
        setshort_whitespace_ok(0);
        setshort_badchars("\",");
    }
    
	/* Write file header */
	fprintf(file_out, "Group\tsID\tsDescription\tfLat\tfLong\tfEasting\tfNorthing\tfAlt\tiColour\tiSymbol\tsHyperLink\n");

    waypt_disp_all(tmp_waypt_pr);
}

ff_vecs_t tmp_vecs = {
    rd_init,
    wr_init,
    rd_deinit,
    wr_deinit,
    data_read,
    data_write,
};

