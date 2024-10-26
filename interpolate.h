/*
    Interpolate filter

    Copyright (C) 2002,2023 Robert Lipe, robertlipe+source@gpsbabel.org

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

#ifndef INTERPOLATE_H_INCLUDED_
#define INTERPOLATE_H_INCLUDED_

#include <QList>     // for QList
#include <QString>   // for QString
#include <QVector>   // for QVector

#include "defs.h"    // for ARG_NOMINMAX, arglist_t, ARGTYPE_BEGIN_EXCL, ARG...
#include "filter.h"  // for Filter
#include "option.h"  // for OptionString, OptionBool

#if FILTERS_ENABLED

class InterpolateFilter:public Filter
{
public:
  QVector<arglist_t>* get_args() override
  {
    return &args;
  }
  void init() override;
  void process() override;

private:
  /* Member Functions */

  void process_rte(route_head* rte);

  /* Data Members */

  OptionDouble opt_time;
  double max_time_step{0};
  OptionDouble opt_dist;
  double max_dist_step{0};
  OptionBool opt_route;

  QVector<arglist_t> args = {
    {
      "time", &opt_time, "Time interval in seconds", nullptr,
      ARGTYPE_BEGIN_EXCL | ARGTYPE_BEGIN_REQ | ARGTYPE_FLOAT,
      "0", nullptr, nullptr
    },
    {
      "distance", &opt_dist, "Distance interval",
      nullptr, ARGTYPE_END_EXCL | ARGTYPE_END_REQ | ARGTYPE_ALLOW_TRAILING_DATA | ARGTYPE_FLOAT,
      ARG_NOMINMAX, nullptr
    },
    {
      "route", &opt_route, "Interpolate routes instead", nullptr,
      ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
  };

};
#endif // FILTERS_ENABLED
#endif // INTERPOLATE_H_INCLUDED_
