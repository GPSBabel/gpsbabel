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

#define MYNAME	"OZI"

static void *mkshort_handle = NULL;


static void
ozi_set_style()
{
    /* set up the ozi xcsv_file struct */
    xcsv_file_init();

    /* this is an internal style, don't mess with it */
    xcsv_file.is_internal = 1;

    /* how the file gets split up */
    xcsv_file.field_delimiter = ",";
    xcsv_file.record_delimiter =	"\n";
    xcsv_file.badchars = ",";

    /* prologue */
    xcsv_prologue_add("OziExplorer Waypoint File Version 1.1");
    xcsv_prologue_add("WGS 84");
    xcsv_prologue_add("Reserved 2");
    xcsv_prologue_add("Reserved 3");

    /* individual field mappings */
    xcsv_ifield_add("INDEX", "1", "%4d");
    xcsv_ifield_add("SHORTNAME", "", "%-14.14s");
    xcsv_ifield_add("LAT_DECIMAL", "", "%11.6f");
    xcsv_ifield_add("LON_DECIMAL", "", "%11.6f");
    xcsv_ifield_add("EXCEL_TIME", "", "%011.5f");
    xcsv_ifield_add("CONSTANT", "0", "%3s");		/* icon */
    xcsv_ifield_add("CONSTANT", "1", "%2s");		/* 1 */
    xcsv_ifield_add("CONSTANT", "3", "%2s");		/* display format opts */
    xcsv_ifield_add("CONSTANT", "0", "%10s");		/* foreground color */
    xcsv_ifield_add("CONSTANT", "65535", "%10s");	/* background color */
    xcsv_ifield_add("DESCRIPTION", "", "%-40.40s");	
    xcsv_ifield_add("CONSTANT", "0", "%2s");		/* pointer direction */
    xcsv_ifield_add("CONSTANT", "0", "%2s");		/* garmin display flags */
    xcsv_ifield_add("CONSTANT", "0", "%5s");		/* proximity distance */
    xcsv_ifield_add("ALT_FEET", "", "%7.0f");
    xcsv_ifield_add("CONSTANT", "6", "%2s");		/* waypt name text size */
    xcsv_ifield_add("CONSTANT", "0", "%2s");		/* bold checkbox */
    xcsv_ifield_add("CONSTANT", "17", "%2s");		/* symbol size */

    /* outfields are infields */
    if (xcsv_file.ofield)
        xfree(xcsv_file.ofield);
    xcsv_file.ofield = &xcsv_file.ifield;
    xcsv_file.ofield_ct = xcsv_file.ifield_ct;

    /* set up mkshort */
    if (global_opts.synthesize_shortnames) {
        setshort_length(mkshort_handle, 32);
        setshort_whitespace_ok(mkshort_handle, 0);
        setshort_badchars(mkshort_handle, xcsv_file.badchars);
    }
}

static void
ozi_rd_init(const char *fname, const char *args)
{
    ozi_set_style();
    
    xcsv_file.xcsvfp = fopen(fname, "r");
    
    if (xcsv_file.xcsvfp == NULL) {
        fatal(MYNAME ": Cannot open %s for reading\n", fname);
    }
}

static void
ozi_wr_init(const char *fname, const char *args)
{
    ozi_set_style();

    mkshort_handle = mkshort_new_handle();

    xcsv_file.xcsvfp = fopen(fname, "w");
    
    if (xcsv_file.xcsvfp == NULL) {
        fatal(MYNAME ": Cannot open %s for reading\n", fname);
    }
}

static void
ozi_deinit(void)
{

    if (xcsv_file.xcsvfp) 
        fclose(xcsv_file.xcsvfp);
        
    xcsv_destroy_style();
    if ( mkshort_handle) 
    	mkshort_del_handle(mkshort_handle);
    mkshort_handle = NULL;    
}

ff_vecs_t ozi_vecs = {
    ozi_rd_init,
    ozi_wr_init,
    ozi_deinit,
    ozi_deinit,
    xcsv_data_read,
    xcsv_data_write,
};
