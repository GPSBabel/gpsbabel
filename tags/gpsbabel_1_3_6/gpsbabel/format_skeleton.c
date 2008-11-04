/*

    Format converter module skeleton.

    Steps to create a new format.

    1) Copy this file to <your_format_name>.c 
    2) Rename all format_skeleton tokens to <your_format_name>. 
    3) Replace the fictional name and address in the Copyright section below.
       As your work is likely built on the work of others, please retain
       the original line.
    4) Create a new section in vecs.c.
    5) Add compilation instructions to Makefile.
    6) Add sample files (it's better when they're created by the "real" 
       application and not our own output) to reference/ along with 
       files in a well supported (preferably non-binary) format and 
       entries in our 'testo' program.   This allows users of different
       OSes and hardware to exercise your module.

    Copyright (C) YYYY John Doe, anybody@wherever.com
    Copyright (C) 2005  Robert Lipe, robertlipe@usa.net

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

#define MYNAME "format_skeleton"


// Any arg in this list will appear in command line help and will be 
// populated for you.
// Values for ARGTYPE_xxx can be found in defs.h and are used to 
// select the type of option.
static
arglist_t format_skeleton_args[] = {
// {"foo", &fooopt, "The text of the foo option in help", 
//   "default", ARGYTPE_STRING, ARG_NOMINMAX} , 
	ARG_TERMINATOR
};

/*******************************************************************************
* %%%        global callbacks called by gpsbabel main process              %%% *
*******************************************************************************/

static void
format_skeleton_rd_init(const char *fname)
{
//	fin = gbfopen(fname, "r", MYNAME);
}

static void 
format_skeleton_rd_deinit(void)
{
//	gbfclose(fin);
}

static void
format_skeleton_read(void)
{
//	your special code to extract waypoint, route and track
//	information from gbfile "fin"
//
// Sample text-file read code:
//	char *s;
//	while ((s = gbfgetstr(fin))) {
//		do_anything(s);
//	}
//
//
// For waypoints:
//         while (have waypoints) {
//                 waypoint = waypt_new()
//                 populate waypoint
//                 waypt_add(waypoint);
//         }
// 
// For routes:
// 
//         route = route_head_alloc();
//         populate struct route_hdr
//	   route_add_head(route);      
//         while (have more routepoints) {
//                 waypoint = waypt_new()
//                 populate waypoint
//                 route_add_wpt(route, waypoint)
//         }
// 
// Tracks are just like routes, except the word "track" replaces "routes".
//
}

static void
format_skeleton_wr_init(const char *fname)
{
//	fout = gbfopen(fname, "w", MYNAME);
}

static void
format_skeleton_wr_deinit(void)
{
//	gbfclose(fout);
}

static void
format_skeleton_write(void)
{
// Here is how you register callbacks for all waypoints, routes, tracks.
// waypt_disp_all(waypt)
// route_disp_all(head, tail, rtept);
// track_disp_all(head, tail, trkpt);
}

static void
format_skeleton_exit(void)		/* optional */
{
}

/**************************************************************************/

// capabilities below means: we can only read and write waypoints
// please change this depending on your new module 

ff_vecs_t format_skeleton_vecs = {
	ff_type_file,
	{ 
		ff_cap_read | ff_cap_write 	/* waypoints */, 
	  	ff_cap_none 			/* tracks */, 
	  	ff_cap_none 			/* routes */
	},
	format_skeleton_rd_init,	
	format_skeleton_wr_init,	
	format_skeleton_rd_deinit,	
	format_skeleton_wr_deinit,	
	format_skeleton_read,
	format_skeleton_write,
	format_skeleton_exit,
	format_skeleton_args,
	CET_CHARSET_ASCII, 0			/* ascii is the expected character set */
						/* not fixed, can be changed through command line parameter */
};
/**************************************************************************/
