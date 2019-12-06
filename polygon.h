/*
    Inside/Outside polygon filter

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

#ifndef POLYGON_H_INCLUDED_
#define POLYGON_H_INCLUDED_

#include "defs.h"    // for ARG_NOMINMAX, arglist_t, ARGTYPE_BOOL, ARGTYPE_FILE
#include "filter.h"  // for Filter

#if FILTERS_ENABLED

class PolygonFilter:public Filter
{
public:
  QVector<arglist_t>* get_args() override
  {
    return &args;
  }
  void process() override;

private:
  char* polyfileopt = nullptr;
  char* exclopt = nullptr;

  struct extra_data {
    unsigned short state;
    unsigned short override;
  };

  QVector<arglist_t> args = {
    {
      "file", &polyfileopt,  "File containing vertices of polygon",
      nullptr, ARGTYPE_FILE | ARGTYPE_REQUIRED, ARG_NOMINMAX, nullptr
    },
    {
      "exclude", &exclopt, "Exclude points inside the polygon",
      nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
  };

  void polytest(double lat1, double lon1,
                double lat2, double lon2,
                double wlat, double wlon,
                unsigned short* state, int first, int last);

};
#endif // FILTERS_ENABLED
#endif // POLYGON_H_INCLUDED_
