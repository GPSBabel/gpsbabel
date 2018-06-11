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
    Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111 USA

 */
#include "defs.h"    // for Waypoint (ptr only), arglist_t, route_head (ptr ...
#include "filter.h"  // for Filter

#if FILTERS_ENABLED

class ReverseRouteFilter:public Filter
{
public:
  arglist_t* get_args() override
  {
    return args;
  }
  void init() override;
  void process() override;

private:
  int prev_new_trkseg;
  arglist_t args[1] = {
    ARG_TERMINATOR
  };

private:
  void reverse_route_wpt(const Waypoint* waypointp);
  void reverse_route_head(const route_head* rte);

  typedef void (ReverseRouteFilter::*RteHdCb)(const route_head*);
  class RteHdFunctor
  {
  public:
    RteHdFunctor(ReverseRouteFilter& obj, RteHdCb cb) : that(&obj), _cb(cb) {}
    void operator()(const route_head* rh)
    {
      ((that)->*(_cb))(rh);
    }

  private:
    ReverseRouteFilter* that;
    RteHdCb _cb;
  };

  typedef void (ReverseRouteFilter::*WayptCb)(const Waypoint*);
  class WayptFunctor
  {
  public:
    WayptFunctor(ReverseRouteFilter& obj, WayptCb cb) : that(&obj), _cb(cb) {}
    void operator()(const Waypoint* wpt)
    {
      ((that)->*(_cb))(wpt);
    }

  private:
    ReverseRouteFilter* that;
    WayptCb _cb;
  };

};
#endif
