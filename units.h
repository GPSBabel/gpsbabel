/*
    Display scaled distances in 'local' units.

    Copyright (C) 2006 Robert Lipe, robertlipe+source@gpsbabel.org

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


#ifndef GPSBABEL_UNITS_H
#define GPSBABEL_UNITS_H

#include <utility>  // for pair

#include <QString>  // for QString


class UnitsFormatter
{
public:

  /* Types */

  enum class units_t {
    statute,
    metric,
    nautical,
    aviation
  };

  /* Member Functions */

  void setunits(units_t u);
  std::pair<double, QString> fmt_distance(double distance_meters) const;
  std::pair<double, QString> fmt_altitude(double distance_meters) const;
  std::pair<double, QString> fmt_speed(double speed_meters_per_sec) const;

  /* Data Members */

  units_t units{units_t::statute};

};
#endif //GPSBABEL_UNITS_H
