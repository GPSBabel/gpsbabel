/*
    Great Circle utility functions

    Copyright (C) 2002-2014 Robert Lipe, robertlipe+source@gpsbabel.org

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

#ifndef GRTCIRC_H
#define GRTCIRC_H

#include <tuple>   // for tuple
#include "defs.h"  // for PositionRad, PositionDeg

/* Note PositionDeg and PositionRad can be implicity converted to
 * each other, so you may use either to interface to these functions.
 */

double gcdist(double lat1, double lon1, double lat2, double lon2);
double gcdist(PositionRad pos1, PositionRad pos2);
double heading_true_degrees(PositionRad pos1, PositionRad pos2);

std::tuple<double, PositionDeg, double> linedistprj(PositionRad pos1,
                                                    PositionRad pos2,
                                                    PositionRad pos3);

double linedist(PositionRad pos1, PositionRad pos2, PositionRad pos3);

double radtometers(double rads);
double radtomiles(double rads);

PositionDeg linepart(PositionRad pos1, PositionRad pos2, double frac);
#endif
