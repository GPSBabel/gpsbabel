/*

    nukedata: remove all (waypoint|tracks|routes) from the stream.

    Copyright (C) 2005 Robert Lipe   robertlipe+source@gpsbabel.org

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

#include "defs.h"
#include "nukedata.h"

#if FILTERS_ENABLED
#define MYNAME "nukedata"

void NukeDataFilter::process()
{
  if (*nukewpts != '0') {
    waypt_flush_all();
  }
  if (*nuketrks != '0') {
    route_flush_all_tracks();
  }
  if (*nukertes != '0') {
    route_flush_all_routes();
  }
}

#endif
