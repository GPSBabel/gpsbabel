/*
    Stack filter

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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

 */

#ifndef STACKFILTER_H_INCLUDED_
#define STACKFILTER_H_INCLUDED_

#include <QVector>         // for QVector

#include "defs.h"    // for ARGTYPE_BOOL, ARG_NOMINMAX, ARGTYPE_BEGIN_EXCL
#include "filter.h"  // for Filter

#if FILTERS_ENABLED

class StackFilter:public Filter
{
public:
  QVector<arglist_t>* get_args() override
  {
    return &args;
  }
  void init() override;
  void process() override;
  void deinit() override;
  void exit() override;

private:
  OptionCString opt_push;
  OptionCString opt_copy;
  OptionCString opt_pop;
  OptionCString opt_append;
  OptionCString opt_discard;
  OptionCString opt_replace;
  OptionCString opt_swap;
  OptionCString opt_depth;
  OptionCString nowarn;
  int  warnings_enabled = 1;
  int  swapdepth = 0;

  QVector<arglist_t> args = {
    {
      "push", &opt_push, "Push waypoint list onto stack", nullptr,
      ARGTYPE_BEGIN_EXCL | ARGTYPE_BEGIN_REQ | ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    {
      "pop", &opt_pop, "Pop waypoint list from stack", nullptr,
      ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    {
      "swap", &opt_swap, "Swap waypoint list with <depth> item on stack",
      nullptr, ARGTYPE_END_EXCL | ARGTYPE_END_REQ | ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    {
      "copy", &opt_copy, "(push) Copy waypoint list", nullptr,
      ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    {
      "append", &opt_append, "(pop) Append list", nullptr,
      ARGTYPE_BEGIN_EXCL | ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    {
      "discard", &opt_discard, "(pop) Discard top of stack",
      nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    {
      "replace", &opt_replace, "(pop) Replace list (default)",
      nullptr, ARGTYPE_END_EXCL | ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    {
      "depth", &opt_depth, "(swap) Item to use (default=1)",
      nullptr, ARGTYPE_INT, "0", nullptr, nullptr
    },
    {
      "nowarn", &nowarn, "Suppress cleanup warning", nullptr,
      ARGTYPE_BOOL | ARGTYPE_HIDDEN, ARG_NOMINMAX, nullptr
    },
  };

  struct stack_elt
  {
    WaypointList waypts;
    RouteList routes;
    RouteList tracks;
    stack_elt* next{nullptr};
  };
  stack_elt* stack = nullptr;

};
#endif // FILTERS_ENABLED
#endif // STACKFILTER_H_INCLUDED_
