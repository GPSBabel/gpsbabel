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

#define MYNAME	"TMPro"

static gbfile *file_in, *file_out;
static short_handle mkshort_handle;

static void 
rd_init(const char *fname)
{
    file_in = gbfopen(fname, "rb", MYNAME);
    if (gbfunicode(file_in)) cet_convert_init(CET_CHARSET_UTF8, 1);
}

static void 
rd_deinit(void)
{
    gbfclose(file_in);
}

static void 
wr_init(const char *fname)
{
    file_out = gbfopen(fname, "w", MYNAME);
}

static void 
wr_deinit(void)
{
    gbfclose(file_out);
}

static void 
data_read(void)
{
    char *buff;
    char *s;
    char *holder;
    waypoint *wpt_tmp;
    int i;
    int linecount = 0;
    
    while ((buff = gbfgetstr(file_in))) {
        linecount++;

	/* skip the line if it contains "sHyperLink" as it is a header (I hope :) */
	if ((strlen(buff)) && (strstr(buff, "sHyperLink") == NULL)) {

	    wpt_tmp = waypt_new();

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
		    wpt_tmp->shortname = csv_stringtrim(s, "", 0);
		    break;
		case 2:
			/* Description is not a TopoMapPro format requirement.
			   If we assign "" then .loc/.gpx will generate empty XML tags :(
			*/
			holder = csv_stringtrim(s, "", 0);
			if (strlen(holder))
				wpt_tmp->description = holder;
			else 
				xfree(holder);
		    break;
		case 3:
		    wpt_tmp->latitude = atof(s);
		    break;
		case 4:
		    wpt_tmp->longitude = atof(s);
		    break;
		case 5:
			/* ignore: NZMapGrid Easting  */
			 break;
		case 6:
			/* ignore: NZMapGrid Northing  */
			 break;
		case 7:
			wpt_tmp->altitude = atof(s);
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
			   (as URLs in TMPro must start "http:") as other GPS formats probably can't 
			   use the TopoMapLinks links.
			   (plus discards length 0 strings (so no empty XML tags))
			*/
			holder = csv_stringtrim(s, "", 0);
			if (strstr(holder, "http:") != NULL)
				wpt_tmp->url = holder;
			else 
				xfree(holder);
			break;
		default:
		    /* whoa! nelly */
		    warning(MYNAME ": Warning: data fields on line %d exceed specification.\n", 
		        linecount);
		    break;
		}
		i++;

		s = csv_lineparse(NULL, "\t", "\"", linecount);
	    }
	    
	    if (i != 11) {
   	        xfree(wpt_tmp);
	        warning(MYNAME ": WARNING - extracted %d fields from line %d. \nData on line ignored.\n", 
	            i, linecount);
	    } else {
   	        waypt_add(wpt_tmp);
   	    }

	} else {
            /* empty line */
	}

    }
}

static void 
tmpro_waypt_pr(const waypoint * wpt)
{
    int icon = 1; /* default to "flag" */
    int colour = 255; /*default to red */
    char *shortname = NULL;
    char *description = NULL;

 	if ((! wpt->shortname) || (global_opts.synthesize_shortnames)) {
        if (wpt->description) {
            if (global_opts.synthesize_shortnames)
                shortname = mkshort_from_wpt(mkshort_handle, wpt);
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
    
    gbfprintf(file_out, "new\t%.6s\t%.80s\t%08.6f\t%08.6f\t\t\t%.2f\t%d\t%d\t%.128s\n",
    	shortname,
    	description,
	    wpt->latitude,
	    wpt->longitude,
	    wpt->altitude,
	    colour,
	    icon,
	    wpt->url ? wpt->url : ""
	);
	    

    if (description)
	xfree(description);
    if (shortname)
	xfree(shortname);
}

static void 
data_write(void)
{
	/* Short names */
	if (global_opts.synthesize_shortnames) {
	mkshort_handle = mkshort_new_handle();
        setshort_length(mkshort_handle, 6);
        setshort_whitespace_ok(mkshort_handle, 0);
        setshort_badchars(mkshort_handle, "\",");
    }
    
	/* Write file header */
	gbfprintf(file_out, "Group\tsID\tsDescription\tfLat\tfLong\tfEasting\tfNorthing\tfAlt\tiColour\tiSymbol\tsHyperLink\n");

    waypt_disp_all(tmpro_waypt_pr);
    mkshort_del_handle(&mkshort_handle);
}

ff_vecs_t tmpro_vecs = {
    ff_type_file,
    FF_CAP_RW_WPT,
    rd_init,
    wr_init,
    rd_deinit,
    wr_deinit,
    data_read,
    data_write,
    NULL,
    NULL,
    CET_CHARSET_ASCII, 0	/* CET-REVIEW */
};

