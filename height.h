/*
	Copyright (C) 2008 Alexander Stapff, a.stapff@gmx.de

	Geoid separation code by Oleg Gusev, from data by Peter Dana.
	This code was published by the gpsd project (http://gpsd.berlios.de/)
	under the BSD license.

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

#ifndef HEIGHT_H_INCLUDED_
#define HEIGHT_H_INCLUDED_

#include "defs.h"    // for ARG_NOMINMAX, Waypoint (ptr only), arglist_t
#include "filter.h"  // for Filter

#if FILTERS_ENABLED

class HeightFilter:public Filter
{
public:
  arglist_t* get_args() override
  {
    return args;
  }
  void init() override;
  void process() override;

private:
  char* addopt        = nullptr;
  char* wgs84tomslopt = nullptr;
  double addf;

  arglist_t args[3] = {
    {
      "add", &addopt, "Adds a constant value to every altitude (meter, append \"f\" (x.xxf) for feet)",
      nullptr, ARGTYPE_BEGIN_REQ | ARGTYPE_FLOAT, ARG_NOMINMAX, nullptr
    },
    {
      "wgs84tomsl", &wgs84tomslopt, "Converts WGS84 ellipsoidal height to orthometric height (MSL)",
      nullptr, ARGTYPE_END_REQ | ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    ARG_TERMINATOR
  };

  double bilinear(double x1, double y1, double x2, double y2, double x, double y, double z11, double z12, double z21, double z22);
  double wgs84_separation(double lat, double lon);
  void correct_height(const Waypoint* wpt);

};

#endif // FILTERS_ENABLED
#endif // HEIGHT_H_INCLUDED_
