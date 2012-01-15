/*
    Access to MapSend files.

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

 *
 * Information from:
 *  Mapsend File Format Description Revision 1.1, March 6, 2002 from Thales.
 *
 * Note this file format was clearly NOT designed for cross-architecture
 * portability.  In fact, because of the pascal nature of the 'string'
 * data type described in that document, it's impractical to describe
 * a 'struct waypoint' in C.
 *
 */

typedef struct {
  char ms_length;
  char ms_signature[11];
  char ms_version[2];
  char ms_type;
  char _ms_type[3];
} mapsend_hdr;

typedef enum {
  ms_type_rgn = 0,
  ms_type_wpt = 1,
  ms_type_track = 2,
  ms_type_log = 3
} ms_type;
