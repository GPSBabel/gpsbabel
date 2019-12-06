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

#ifndef NUKEDATA_H_INCLUDED_
#define NUKEDATA_H_INCLUDED_

#include "defs.h"    // for ARGTYPE_BOOL, ARG_NOMINMAX, arglist_t, ARG_TERMI...
#include "filter.h"  // for Filter

#if FILTERS_ENABLED

class NukeDataFilter:public Filter
{
public:
  QVector<arglist_t>* get_args() override
  {
    return &args;
  }
  void process() override;

private:
  char* nukewpts;
  char* nuketrks;
  char* nukertes;

  QVector<arglist_t> args = {
    {
      "waypoints", &nukewpts, "Remove all waypoints from data stream",
      "0", ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    {
      "tracks", &nuketrks, "Remove all tracks from data stream",
      "0", ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    {
      "routes", &nukertes, "Remove all routes from data stream",
      "0", ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
  };

};

#endif
#endif // NUKEDATA_H_INCLUDED_
