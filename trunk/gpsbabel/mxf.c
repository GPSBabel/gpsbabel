/*
    Mapsend Exchange Format (.mxf)
    (Maptech Terrain Navigator, Terrain Professional, Take a Hike)
    (AKA Yet Another CSV Format) 

    Contributed to gpsbabel by Alex Mottram (geo_alexm at cox-internet.com)

    LAT, LON, "Waypoint Name", "Big Name", "Small Name", COLOR, ICON

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
#include <ctype.h>

#define MYNAME	"MXF"

static FILE *file_in;
static FILE *file_out;

static char * 
csvstringclean(char * string)
{
    static char * p1 = NULL;
    char * p2 = NULL;
    
    if (! string) { 
        return (string); /* :) */
    }

    p2 = string;
    
    while ((*p2) && (p2++)) { }
    p2--;
    
    while (isspace(*p2)) {
        *p2 = '\0';
        p2--;
    }
    
    p1 = string;
    
    while (isspace(*p1)) {
        p1++;
    }

    /* yank quotes in pairs only if they are bounding us */
    while ((*p1 == '"') && (*p2 == '"')) {
        *p2 = '\0';
        p2--;
        p1++;
    }

    return (p1);    
}

/* string parser.  sorta like strtok with quotes & pointers..  */
/* designed to handle quoted and delimited data within quotes. */
static char * 
csvparse(char *stringstart, char *delimiter)
{
    char *sp;
    static char *p = NULL;
    static char *tmp = NULL;
    size_t dlen;
    int quotedepth = 0;
    short int dfound;

    if (!p) {
	p = stringstart;

	if (!p) {
	    return (NULL);
	}
    }

    if (tmp) {
	free(tmp);
	tmp = NULL;
    }

    sp = p;

    dlen = strlen(delimiter);
    dfound = 0;

    while ((*p) && (! dfound)) {
	if (*p == '"') {
	    if (quotedepth) 
		quotedepth--;
	    else 
		quotedepth++;
	}

	if ((!quotedepth) && (strncmp(p, delimiter, dlen) == 0)) {
        	dfound = 1;
        	
	} else {
	    	p++;
	}

    }

    tmp = (char *) calloc((p - sp) + 1, sizeof(char));

    if (! tmp) {
	fatal(MYNAME ": cannot allocate memory\n");
    }

    strncpy(tmp, sp, (p - sp));

    if (dfound) {
        /* skip over the delimiter */
        p += dlen;
    } else {
        /* end of the line */
        p = NULL;
    }

    return (tmp);
}

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
    char buff[256];
    char *s;
    waypoint *wpt_tmp;
    int i;
    int linecount = 0;

    do {
        linecount++;
	memset(&buff, '\0', sizeof(buff));
	fgets(buff, sizeof(buff), file_in);

	if (strlen(buff)) {

	    wpt_tmp = calloc(sizeof(*wpt_tmp), 1);

	    if (wpt_tmp == NULL) {
		fatal(MYNAME ": cannot allocate memory\n");
	    }

	    s = buff;
	    s = csvparse(s, ", ");

	    i = 0;
	    while (s) {
		switch (i) {
		case 0:
		    wpt_tmp->position.latitude.degrees = atof(s);
		    break;
		case 1:
		    wpt_tmp->position.longitude.degrees = atof(s);
		    break;
		case 2:
		    wpt_tmp->description = strdup(csvstringclean(s));
		    break;
		case 3:
		    wpt_tmp->shortname = strdup(csvstringclean(s));
		    break;
		case 4:
                    /* ignore.  another name-type  */
		    break;
		case 5:
		    /* ignore: color  */
		    break;
		case 6:
		    /* ignore: icon */
		    break;
		default:
		    /* whoa! nelly */
		    fprintf (stderr, "%s: Warning: data fields on line %d exceed specification.\n", 
		        MYNAME, linecount);
		    break;
		}
		i++;

		s = csvparse(NULL, ", ");
	    }
	    
	    if (i != 7) {
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
mxf_disp(waypoint * wpt)
{
    int icon = 47; /* default to "dot" */
    const char *color_hex = "ff0000";

    fprintf(file_out, "%08.5f, %08.5f, \"%s\", \"%s\", \"%s\", %s, %d\n",
	    wpt->position.latitude.degrees, wpt->position.longitude.degrees,
	    wpt->description, wpt->shortname, wpt->description, 
	    color_hex, icon);
}

static void 
data_write(void)
{
    waypt_disp_all(mxf_disp);
}

ff_vecs_t mxf_vecs = {
    rd_init,
    wr_init,
    rd_deinit,
    wr_deinit,
    data_read,
    data_write,
};

