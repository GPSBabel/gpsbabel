/*
    Route reversal filter.

    Copyright (C) 2003 Robert Lipe, robertlipe+source@gpsbabel.org

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
#ifndef REVERSE_ROUTE_H_INCLUDED_
#define REVERSE_ROUTE_H_INCLUDED_

#include <QtCore/QVector>  // for QVector

#include "defs.h"    // for Waypoint (ptr only), arglist_t, route_head (ptr ...
#include "filter.h"  // for Filter

#if FILTERS_ENABLED

class ReverseRouteFilter:public Filter
{
public:
  QVector<arglist_t>* get_args() override
  {
    return &args;
  }
  void init() override;
  void process() override;

private:
  int prev_new_trkseg{};
  QVector<arglist_t> args = {
  };

  void reverse_route_wpt(const Waypoint* waypointp);
  void reverse_route_head(const route_head* rte);

};
#endif
#endif // REVERSE_ROUTE_H_INCLUDED_
