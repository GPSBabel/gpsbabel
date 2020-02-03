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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

 */

#ifndef INTERPOLATE_H_INCLUDED_
#define INTERPOLATE_H_INCLUDED_

#include <QtCore/QVector>       // for QVector
#include <QtCore/QtGlobal>      // for qint64

#include "defs.h"               // for ARG_NOMINMAX, arglist_t, ARGTYPE_BEGIN_EXCL, ARG...
#include "filter.h"             // for Filter
#include "src/core/optional.h"  // for optional

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
  char* opt_time{nullptr};
  double max_time_step{0};
  char* opt_dist{nullptr};
  double max_dist_step{0};
  char* opt_route{nullptr};

  QVector<arglist_t> args = {
    {
      "time", &opt_time, "Time interval in seconds", nullptr,
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
  };

};
#endif // FILTERS_ENABLED
#endif // INTERPOLATE_H_INCLUDED_
