/*
    OziExplorer Waypoints/Tracks/Routes
    Comma Delimited 

    As described in OziExplorer Help File

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
#include <math.h>                /* for floor */

#define MYNAME        "OZI"


static FILE *file_in;
static FILE *file_out;
static void *mkshort_handle;
static route_head *trk_head;
static route_head *rte_head;

static int track_out_count;
static int route_out_count;
static int route_wpt_count;

static char *snlenopt;
static char *snwhiteopt;
static char *snupperopt;
static char *snuniqueopt;

static
arglist_t ozi_args[] = {
	{"snlen", &snlenopt, "Max synthesized shortname length",
		ARGTYPE_INT},
	{"snwhite", &snwhiteopt, "(0/1) Allow whitespace synth. shortnames",
		ARGTYPE_BOOL},
	{"snupper", &snupperopt, "(0/1) UPPERCASE synth. shortnames",
	        ARGTYPE_BOOL},
	{"snunique", &snuniqueopt, "(0/1) Make synth. shortnames unique",
		ARGTYPE_BOOL},
	{0, 0, 0, 0}
};

gpsdata_type ozi_objective;

static char *ozi_ofname = NULL;

static void
ozi_openfile(char *fname) {
    char *c, *tmpname;
    char *ozi_extensions[] = {0, "plt", "wpt", "rte"};
    char buff[32];
    
    /* if we're doing multi-track output, sequence the filenames like:
     * mytrack.plt, mytrack-1.plt...
     */

    if ((track_out_count) && (ozi_objective == trkdata)) {
        sprintf(buff, "-%d", track_out_count);
    } else {
        buff[0] = '\0';
    }

    /* allocate more than enough room for new filename */
    tmpname = (char *) xcalloc(1, strlen(fname) +
                         strlen(buff) +
                         strlen(ozi_extensions[ozi_objective]) +
                         2); /* . (dot) plus null term */

    strcpy(tmpname, fname);

    /* locate and remove file extension */
    c = strrchr(tmpname, '.');

    if (c)
        *c = '\0';

    /* append the -xx sequence number for tracks if needed */
    strcat(tmpname + strlen(tmpname), buff);

    strcat(tmpname, ".");

    /* append the extension after the "." */
    strcat(tmpname, ozi_extensions[ozi_objective]);

    /* re-open file_out with the new filename */
    if (file_out) {
        fclose(file_out);
        file_out = NULL;
    }

    file_out = xfopen(tmpname, "wb", MYNAME);

    xfree(tmpname);

    return;
}

static void 
ozi_track_hdr(const route_head * rte)
{
    static char *ozi_trk_header = 
        "OziExplorer Track Point File Version 2.1\r\n"
        "WGS 84\r\n"
        "Altitude is in Feet\r\n"
        "Reserved 3\r\n" 
        "0,2,255,ComplimentsOfGPSBabel,0,0,2,8421376\r\n"
        "0\r\n";

    ozi_openfile(ozi_ofname);
    fprintf(file_out, ozi_trk_header);

    track_out_count++;
}

static void 
ozi_track_disp(const waypoint * waypointp)
{
    double alt_feet;
    double ozi_time;

    ozi_time = (waypointp->creation_time / 86400.0) + 25569.0;

    if (waypointp->altitude == unknown_alt) {
        alt_feet = -777;
    } else {
        alt_feet = (waypointp->altitude * 3.2808);
    }

    fprintf(file_out, "%.6f,%.6f,0,%.0f,%.5f,,\r\n",
            waypointp->latitude, waypointp->longitude, alt_feet, ozi_time);
}

static void
ozi_track_tlr(const route_head * rte)
{
}

static void 
ozi_track_pr()
{
    track_disp_all(ozi_track_hdr, ozi_track_tlr, ozi_track_disp);
}

static void
ozi_route_hdr(const route_head * rte)
{
    static char *ozi_route_header = 
        "OziExplorer Route File Version 1.0\r\n"
        "WGS 84\r\n" 
        "Reserved 1\r\n" 
        "Reserved 2\r\n";

    /* prologue on 1st pass only */
    if (route_out_count == 0) {
        fprintf(file_out, ozi_route_header);
    }

    route_out_count++;
    route_wpt_count = 0;

    /*
     * Route Record 
     * Field 1 : R - indicating route details
     * Field 2 : Number - this is the location in the array, must be unique, usually start at 0 for Garmins 1 for other and increment.
     * Field 3 : Name - the waypoint name, use the correct length name to suit the GPS type.
     * Field 4 : Description.
     * Field 5 : Route Color as displayed on map (RGB).
     *
     * R, 0,R0 ,,255 
     * R, 1, ICP GALHETA,, 16711680 
     */
     
     fprintf(file_out, "R,%d,%s,%s,\r\n", 
         route_out_count, 
         rte->rte_name ? rte->rte_name : "", 
         rte->rte_desc ? rte->rte_desc : "");

}

static void
ozi_route_disp(const waypoint * waypointp)
{
    double alt_feet;
    double ozi_time;

    route_wpt_count++;

    ozi_time = (waypointp->creation_time / 86400.0) + 25569.0;

    if (waypointp->altitude == unknown_alt) {
        alt_feet = -777;
    } else {
        alt_feet = (waypointp->altitude * 3.2808);
    }

/*
 *   Field 1 : W - indicating route waypoint details.
 *   Field 2 : Route Number - location in array of routes
 *   Field 3 : Number - this is the location in the array of route waypoints, this field is now ignored.
 *   Field 4 : Wp Number - this is the number of the waypoint (the Wp number within the GPS for lowrances)
 *   Field 5 : Name - the waypoint name, use the correct length name to suit the GPS type.
 *   Field 6 : Latitude - decimal degrees.
 *   Field 7 : Longitude - decimal degrees.
 *   Field 8 : Date - see Date Format below, if blank a preset date will be used
 *   Field 9 : Symbol - 0 to number of symbols in GPS
 *   Field 10 : Status - always set to 1
 *   Field 11 : Map Display Format
 *   Field 12 : Foreground Color (RGB value)
 *   Field 13 : Background Color (RGB value)
 *   Field 14 : Description (max 40), no commas
 *   Field 15 : Pointer Direction
 *   Field 16 : Garmin Display Format
 *
 * W,1,7,7,007,-25.581670,-48.316660,36564.54196,10,1,4,0,65535,TR ILHA GALHETA,0,0 
 */

    fprintf(file_out, "W,%d,%d,,%s,%.6f,%.6f,%.5f,0,1,3,0,65535,%s,0,0\r\n", 
            route_out_count,
            route_wpt_count,
            waypointp->shortname ? waypointp->shortname : "",
            waypointp->latitude,
            waypointp->longitude,
            ozi_time,
            waypointp->description ? waypointp->description : "");

}

static void
ozi_route_tlr(const route_head * rte)
{
}

static void 
ozi_route_pr()
{
    route_disp_all(ozi_route_hdr, ozi_route_tlr, ozi_route_disp);
}

static void
rd_init(const char *fname)
{
    file_in = xfopen(fname, "r", MYNAME);

    mkshort_handle = mkshort_new_handle();
}

static void
rd_deinit(void)
{
    fclose(file_in);
    file_in = NULL;
    mkshort_del_handle(mkshort_handle);
}

static void
wr_init(const char *fname)
{
    
    /* At this point, we have no idea whether we'll be writing waypoint,
     * route, or tracks.  So we'll hold off opening any files until
     * we're actually ready to write.
     */

    ozi_ofname = (char *)fname;

    mkshort_handle = mkshort_new_handle();

    /* set mkshort options from the command line if applicable */
    if (global_opts.synthesize_shortnames) {

        if (snlenopt)
            setshort_length(mkshort_handle, atoi(snlenopt));
        else 
            setshort_length(mkshort_handle, 32);

        if (snwhiteopt)
            setshort_whitespace_ok(mkshort_handle, atoi(snwhiteopt));

        if (snupperopt)
            setshort_mustupper(mkshort_handle, atoi(snupperopt));

        if (snuniqueopt)
            setshort_mustuniq(mkshort_handle, atoi(snuniqueopt));

        setshort_badchars(mkshort_handle, "\",");
    }

}

static void
wr_deinit(void)
{
    fclose(file_out);
    file_out = NULL;
    ozi_ofname = NULL;

    mkshort_del_handle(mkshort_handle);
}

static void
ozi_parse_waypt(int field, char *str, waypoint * wpt_tmp)
{
    double alt;

    switch (field) {
    case 0:
        /* sequence # */
        break;
    case 1:
        /* waypoint name */
        wpt_tmp->shortname = csv_stringtrim(str, "", 0);
        break;
    case 2:
        /* degrees latitude */
        wpt_tmp->latitude = atof(str);
        break;
    case 3:
        /* degrees longitude */
        wpt_tmp->longitude = atof(str);
        break;
    case 4:
        /* DAYS since 1900 00:00:00 in days.days (5.5) */
        wpt_tmp->creation_time = (atof(str) - 25569.0) * 86400.0;
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
        /* background color (65535=yellow) */
        break;
    case 10:
        /* Description */
        wpt_tmp->description = csv_stringtrim(str, "", 0);
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
        alt = atof(str);
        if (alt == -777) {
            wpt_tmp->altitude = unknown_alt;
        } else {
            wpt_tmp->altitude = alt * .3048;
        }
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
        /* 
         * Fields 18-23 were added around version 3.90.4g of
         * Ozi, but aren't documented.   We silently ignore 
         * these or any additional fields we don't need.
         */
    default:
        break;
    }
}

static void
ozi_parse_track(int field, char *str, waypoint * wpt_tmp)
{
    double alt;

    switch (field) {
    case 0:
        /* latitude */
        wpt_tmp->latitude = atof(str);
        break;
    case 1:
        /* longitude */
        wpt_tmp->longitude = atof(str);
        break;
    case 2:
        /* ignore */
        break;
    case 3:
        /* altitude in feet */
        alt = atof(str);
        if (alt == -777) {
            wpt_tmp->altitude = unknown_alt;
        } else {
            wpt_tmp->altitude = alt * .3048;
        }
        break;
    case 4:
        /* DAYS since 1900 00:00:00 in days.days (5.5) */
        wpt_tmp->creation_time = (atof(str) - 25569.0) * 86400.0;
        break;
    default:
        break;
    }
}

static void
ozi_parse_routepoint(int field, char *str, waypoint * wpt_tmp)
{

    switch (field) {
    case 0:
        /* W */
        break;
    case 1:
        /* route # */
        break;
    case 2:
        /* waypoint # -- ignored by ozi */
        break;
    case 3:
        /* waypoint # */
        break;
    case 4:
        /* waypoint name */
        wpt_tmp->shortname = csv_stringclean(str, ",");
        break;
    case 5:
        /* latitude */
        wpt_tmp->latitude = atof(str);
        break;
    case 6:
        /* longitude */
        wpt_tmp->longitude = atof(str);
        break;
    case 7:
        /* DAYS since 1900 00:00:00 in days.days (5.5) */
        wpt_tmp->creation_time = (atof(str) - 25569.0) * 86400.0;
        break;
    case 8:
        /* symbol */
        break;
    case 9:
        /* status */
        break;
    case 10:
        /* map display format */
        break;
    case 11:
        /* foreground color (RGB) */
        break;
    case 12:
        /* background color (RGB) */
        break;
    case 13:
        /* description */
        wpt_tmp->description = csv_stringclean(str, ",");
        break;
    default:
        break;
    }
}

static void
ozi_parse_routeheader(int field, char *str, waypoint * wpt_tmp)
{

    switch (field) {
    case 0:
        /* R */
        rte_head = route_head_alloc();
        route_add_head(rte_head);
        break;
    case 1:
        /* route # */
        rte_head->rte_num = atoi(str);
        break;
    case 2:
        /* route name */
        rte_head->rte_name = csv_stringclean(str, ",");
        break;
    case 3:
        /* route description */
        rte_head->rte_desc = csv_stringclean(str, ",");
        break;
    case 4:
        /* route color */
        break;
    default:
        break;
    }
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
        memset(buff, '\0', sizeof(buff));
        fgets(buff, sizeof(buff), file_in);

        /* 
         * this is particularly nasty.  use the first line of the file
         * to attempt to divine the data type we are parsing
         */
        if (linecount == 1) {
            if (strstr(buff, "Track Point") != NULL) {
                trk_head = route_head_alloc();
                track_add_head(trk_head);
                ozi_objective = trkdata;
            } else
            if (strstr(buff, "Route File") != NULL) {
                ozi_objective = rtedata;
            } else {
                ozi_objective = wptdata;
            }
        }
        if ((strlen(buff)) && (strstr(buff, ",") != NULL)) {

            wpt_tmp = waypt_new();

            /* data delimited by commas, possibly enclosed in quotes.  */
            s = buff;
            s = csv_lineparse(s, ",", "", linecount);

            i = 0;
            while (s) {
                switch (ozi_objective) {
                case trkdata:
                    ozi_parse_track(i, s, wpt_tmp);
                    break;
                case rtedata:
                    if (buff[0] == 'R') {
                        ozi_parse_routeheader(i, s, wpt_tmp);
                    } else {
                        ozi_parse_routepoint(i, s, wpt_tmp);
                    }

                    break;
                case wptdata:
                    ozi_parse_waypt(i, s, wpt_tmp);
                    break;
                }
                i++;
                s = csv_lineparse(NULL, ",", "", linecount);
            }

            switch (ozi_objective) {
            case trkdata:
                if (linecount > 6) /* skipping over file header */
                    route_add_wpt(trk_head, wpt_tmp);
                else
                    waypt_free(wpt_tmp);
                break;
            case rtedata:
                if (linecount > 5) /* skipping over file header */
                    route_add_wpt(rte_head, wpt_tmp);
                else
                    waypt_free(wpt_tmp);
                break;
            case wptdata:
                if (linecount > 4)  /* skipping over file header */
                    waypt_add(wpt_tmp);
                else
                    waypt_free(wpt_tmp);
                break;
            }

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
    char *description;
    char *shortname;

    ozi_time = (wpt->creation_time / 86400.0) + 25569.0;

    if (wpt->altitude == unknown_alt) {
        alt_feet = -777;
    } else {
        alt_feet = (wpt->altitude * 3.2808);
    }

    if ((!wpt->shortname) || (global_opts.synthesize_shortnames)) {
        if (wpt->description) {
            if (global_opts.synthesize_shortnames)
                shortname = mkshort(mkshort_handle, wpt->description);
            else
                shortname = csv_stringclean(wpt->description, ",");
        } else {
            /* no description available */
            shortname = xstrdup("");
        }
    } else {
        shortname = csv_stringclean(wpt->shortname, ",");
    }

    if (!wpt->description) {
        if (shortname) {
            description = csv_stringclean(shortname, ",");
        } else {
            description = xstrdup("");
        }
    } else {
        description = csv_stringclean(wpt->description, ",");
    }

    index++;

    fprintf(file_out,
            "%d,%s,%.6f,%.6f,%.5f,%d,%d,%d,%d,%d,%s,%d,%d,%d,%.0f,%d,%d,%d\r\n",
            index, shortname, wpt->latitude, wpt->longitude, ozi_time, 0,
            1, 3, 0, 65535, description, 0, 0, 0, alt_feet, 6, 0, 17);

    xfree(description);
    xfree(shortname);

}

static void
data_write(void)
{
    static char *ozi_wpt_header = 
        "OziExplorer Waypoint File Version 1.1\r\n"
        "WGS 84\r\n" 
        "Reserved 2\r\n" 
        "Reserved 3\r\n";

    track_out_count = route_out_count = 0;
    
    if (waypt_count()) {
        ozi_objective = wptdata;
        ozi_openfile(ozi_ofname);
        fprintf(file_out, ozi_wpt_header);
        waypt_disp_all(ozi_waypt_pr);
    }

    if (track_count()) {
        ozi_objective = trkdata;
        ozi_track_pr(); /* ozi_track_hdr handles filenames / file_out */
    }

    if (route_count()) {
        ozi_objective = rtedata;
        ozi_openfile(ozi_ofname); /* ozi routes go in one big file */
        ozi_route_pr();
    }

}

ff_vecs_t ozi_vecs = {
    ff_type_file,
    rd_init,
    wr_init,
    rd_deinit,
    wr_deinit,
    data_read,
    data_write,
    ozi_args
};
