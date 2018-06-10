/*
    Interpolate filter

    Copyright (C) 2002 Robert Lipe, robertlipe+source@gpsbabel.org

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

#ifndef INTERPOLATE_H_INCLUDED_
#define INTERPOLATE_H_INCLUDED_

#include "defs.h"    // for ARG_NOMINMAX, arglist_t, ARGTYPE_BEGIN_EXCL, ARG...
#include "filter.h"  // for Filter

#if FILTERS_ENABLED

class InterpolateFilter:public Filter
{
public:
  arglist_t* get_args() override
  {
    return args;
  }
  void init() override;
  void process() override;

private:
  char* opt_interval = nullptr;
  unsigned int interval = 0;
  char* opt_dist = nullptr;
  double dist = 0;
  char* opt_route = nullptr;

  arglist_t args[4] = {
    {
      "time", &opt_interval, "Time interval in seconds", nullptr,
      ARGTYPE_BEGIN_EXCL | ARGTYPE_BEGIN_REQ | ARGTYPE_INT,
      "0", nullptr, nullptr
    },
    {
      "distance", &opt_dist, "Distance interval in miles or kilometers",
      nullptr, ARGTYPE_END_EXCL | ARGTYPE_END_REQ | ARGTYPE_STRING,
      ARG_NOMINMAX, nullptr
    },
    {
      "route", &opt_route, "Interpolate routes instead", nullptr,
      ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    ARG_TERMINATOR
  };

};
#endif // FILTERS_ENABLED
#endif // INTERPOLATE_H_INCLUDED_
