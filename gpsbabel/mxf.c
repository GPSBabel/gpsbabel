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
#include "csv_util.h"

#define MYNAME	"MXF"

static void *mkshort_handle;

static void
mxf_set_style()
{
    /* set up the csv xcsv_file struct */
    xcsv_file_init();

    /* this is an internal style, don't mess with it */
    xcsv_file.is_internal = 1;

    /* how the file gets split up */
    xcsv_file.field_delimiter = ", ";
    xcsv_file.record_delimiter = "\n";
    xcsv_file.badchars = "\",";

    xcsv_ifield_add("LAT_DECIMAL", "", "%08.5f");
    xcsv_ifield_add("LON_DECIMAL", "", "%08.5f");
    xcsv_ifield_add("DESCRIPTION", "", "%s");
    xcsv_ifield_add("SHORTNAME", "", "%s");
    xcsv_ifield_add("IGNORE", "", "%s");
    xcsv_ifield_add("CONSTANT", "ff0000", "%s");
    xcsv_ifield_add("CONSTANT", "47", "%s");

    xcsv_ofield_add("LAT_DECIMAL", "", "%08.5f");
    xcsv_ofield_add("LON_DECIMAL", "", "%08.5f");
    xcsv_ofield_add("DESCRIPTION", "", "\"%s\"");
    xcsv_ofield_add("SHORTNAME", "", "\"%s\"");
    xcsv_ofield_add("DESCRIPTION", "", "\"%s\"");
    xcsv_ofield_add("CONSTANT", "ff0000", "%s");
    xcsv_ofield_add("CONSTANT", "47", "%s");

    /* set up mkshort */
    if (global_opts.synthesize_shortnames) {
        setshort_length(mkshort_handle, 32);
        setshort_whitespace_ok(mkshort_handle, 0);
        setshort_badchars(mkshort_handle, xcsv_file.badchars);
    }
}

static void
mxf_rd_init(const char *fname, const char *args)
{
    mxf_set_style();

    xcsv_file.xcsvfp = fopen(fname, "r");
    
    if (xcsv_file.xcsvfp == NULL) {
        fatal(MYNAME ": Cannot open %s for reading\n", fname );
    }
}

static void
mxf_wr_init(const char *fname, const char *args)
{
    mkshort_handle = mkshort_new_handle();

    mxf_set_style();

    xcsv_file.xcsvfp = fopen(fname, "w");
    
    if (xcsv_file.xcsvfp == NULL) {
        fatal(MYNAME ": Cannot open %s for reading\n", fname );
    }
}

static void
mxf_deinit(void)
{
    if (xcsv_file.xcsvfp) 
        fclose(xcsv_file.xcsvfp);
        
    xcsv_destroy_style();
}

ff_vecs_t mxf_vecs = {
    mxf_rd_init,
    mxf_wr_init,
    mxf_deinit,
    mxf_deinit,
    xcsv_data_read,
    xcsv_data_write,
};
