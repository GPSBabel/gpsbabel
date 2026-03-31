/*

    Format converter module skeleton.

    Steps to create a new format.

    1) Copy format_skeleton.h, format_skeleton.cc to <your_format_name>.h,
       <your_format_name>.cc
    2) Rename all format_skeleton tokens to <your_format_name>.
    3) Replace the fictional name and address in the Copyright section below.
       ** As your work is likely built on the work of others, please retain
       the original line. **
    4) Create a new entry in vec_list in vecs.cc. You are strongly encouraged to use
       a dynamic format, i.e. one that uses &fmtfactory<FormatSkeletonFormat>.
       Add the include for your .h file.
     An example entry in vec_list would be:
    {
      nullptr,
      "skel",
      ".skel",
      "Format Skeleton",
      nullptr,
      &fmtfactory<FormatSkeletonFormat>
    },
    5) Add entries to SOURCES, HEADERS and TESTS in CMakeLists.txt
    6) Add sample files (it's better when they're created by the "real"
       application and not our own output) to reference/ along with
       files in a well supported (preferably non-binary) format and
       entries in our 'testo' program.   This allows users of different
       OSes and hardware to exercise your module.

    Copyright (C) YYYY John Doe, anybody@wherever.com
    Copyright (C) 2001-YYYY Robert Lipe, robertlipe+source@gpsbabel.org

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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

 */

#include "format_skeleton.h"

#include <QString>

#include "defs.h"


/*******************************************************************************
* %%%        global callbacks called by gpsbabel main process              %%% *
*******************************************************************************/

void FormatSkeletonFormat::rd_init(const QString& fname)
{
//	fin = gbfopen(fname, "r");
}

void FormatSkeletonFormat::rd_deinit()
{
//	gbfclose(fin);
}

void FormatSkeletonFormat::read()
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
//                 waypoint = new waypoint
//                 populate waypoint
//                 waypt_add(waypoint);
//         }
//
// For routes:
//
//         route = new route_head;
//         populate struct route_hdr
//	   route_add_head(route);
//         while (have more routepoints) {
//                 waypoint = new waypoint
//                 populate waypoint
//                 route_add_wpt(route, waypoint)
//         }
//
// Tracks are just like routes, except the word "track" replaces "routes".
//
}

void FormatSkeletonFormat::wr_init(const QString& fname)
{
//	fout = gbfopen(fname, "w");
}

void FormatSkeletonFormat::wr_deinit()
{
//	gbfclose(fout);
}

void FormatSkeletonFormat::write()
{
// Here is how you register callbacks for all waypoints, routes, tracks.
// waypt_disp_all(waypt)
// route_disp_all(head, tail, rtept);
// track_disp_all(head, tail, trkpt);
}

void
FormatSkeletonFormat::exit(void)		/* optional */
{
}
