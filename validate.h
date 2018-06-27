/*

    validate internal data structures.

    Copyright (C) 2013 Robert Lipe   robertlipe+source@gpsbabel.org

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

#ifndef VALIDATE_H_INCLUDED_
#define VALIDATE_H_INCLUDED_

#include "defs.h"    // for route_head (ptr only), ARGTYPE_BOOL, ARG_NOMINMAX
#include "filter.h"  // for Filter

#if FILTERS_ENABLED

class ValidateFilter:public Filter
{
public:
  arglist_t* get_args() override
  {
    return args;
  }
  void process() override;

private:
  char* opt_debug;
  bool debug;
  char* opt_checkempty;
  bool checkempty;
  unsigned int point_ct;
  unsigned int head_ct;
  unsigned int segment_ct_start;
  const char* segment_type;
  arglist_t args[3] = {
    {
      "checkempty", &opt_checkempty, "Check for empty input",
      "0", ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    {
      "debug", &opt_debug, "Output debug messages instead of possibly issuing a fatal error",
      "0", ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    ARG_TERMINATOR
  };

  void validate_head(const route_head*);
  void validate_head_trl(const route_head* header);
  void validate_point(const Waypoint*);

};

#endif
#endif // VALIDATE_H_INCLUDED_
