/*
    Track resampling filter

    Copyright (C) 2021,2023 Robert Lipe, robertlipe+source@gpsbabel.org

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

#ifndef RESAMPLE_H_INCLUDED_
#define RESAMPLE_H_INCLUDED_

#include <tuple>               // for tuple

#include <QList>               // for QList
#include <QString>             // for QString
#include <QVector>             // for QVector

#include "defs.h"              // for arglist_t, ARGTYPE_INT, Waypoint, route_head
#include "filter.h"            // for Filter
#include "option.h"            // for OptionString
#include "src/core/nvector.h"  // for NVector


#if FILTERS_ENABLED

class ResampleFilter:public Filter
{
public:
  QVector<arglist_t>* get_args() override
  {
    return &args;
  }
  void init() override;
  void deinit() override;
  void process() override;

private:

  /* Member Functions */

  void average_waypoint(Waypoint* wpt, bool zero_stuffed);
  void interpolate_rte(route_head* rte);
  void decimate_rte(const route_head* rte);

  /* Data Members */

  QVector<std::tuple<gpsbabel::NVector, int, double>> history;
  gpsbabel::NVector accumulated_position;
  int accumulated_altitude_valid_count{0};
  double accumulated_altitude{0.0};
  double filter_gain{0.0};
  int wpt_zero_stuffed{};

  int counter{0};
  int average_count{0};
  int decimate_count{0};
  int interpolate_count{0};

  OptionString decimateopt;
  OptionString interpolateopt;
  OptionString averageopt;

  QVector<arglist_t> args = {
    {
      "decimate", &decimateopt, "Decimate, decrease sample rate by a factor of n", nullptr,
      ARGTYPE_INT, "2", nullptr, nullptr
    },
    {
      "interpolate", &interpolateopt, "Interpolate, increase sample rate by a factor of n", nullptr,
      ARGTYPE_INT, "2", nullptr, nullptr
    },
    {
      "average", &averageopt, "Running average of n points", nullptr,
      ARGTYPE_INT, "2", nullptr, nullptr
    }
  };

};

#endif // FILTERS_ENABLED
#endif // RESAMPLE_H_INCLUDED_
