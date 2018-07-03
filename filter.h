/*
    Filter Base Class

    Copyright (C) 2018 Robert Lipe, robertlipe+source@gpsbabel.org

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
#ifndef FILTER_H_INCLUDED_
#define FILTER_H_INCLUDED_

#include "defs.h"

class Filter
{
public:
  Filter() = default;
  // Provide virtual public destructor to avoid undefined behavior when
  // an object of derived class type is deleted through a pointer to
  // its base class type.
  // https://wiki.sei.cmu.edu/confluence/display/cplusplus/OOP52-CPP.+Do+not+delete+a+polymorphic+object+without+a+virtual+destructor
  virtual ~Filter() = default;
  // And that requires us to explictly default the move and copy operations.
  // https://github.com/isocpp/CppCoreGuidelines/blob/master/CppCoreGuidelines.md#c21-if-you-define-or-delete-any-default-operation-define-or-delete-them-all.
  Filter(const Filter&) = default;
  Filter& operator=(const Filter&) = default;
  Filter(Filter&&) = default;
  Filter& operator=(Filter&&) = default;

  virtual arglist_t* get_args() = 0;

  virtual void init()
  {
    /* Called before filter processing */

    /* optional.  If not needed, delete and replace entry in vecs with NULL  */

    /* This may be used to parse filter options, allocate memory, and do other
     * housekeeping that should be done before filtering */
  }

  virtual void process() = 0;	/* this procedure must be present in vecs */
//{
// Here is how you register callbacks for all waypoints, routes, tracks.
// waypt_disp_all(waypt)
// route_disp_all(head, tail, rtept);
// track_disp_all(head, tail, trkpt);
//}

  virtual void deinit()
  {
    /* called after filter processing */

    /* optional.   If not needed, delete and replace entry in vecs with NULL */

    /* This should be used to clean up any memory allocations that are no longer
     * needed after the filter terminates. */
  }

  virtual void exit()
  {
    /* called on program exit */

    /* optional.   If not needed, delete and replace entry in vecs with NULL */

    /* You should not need this for simple filters, but it may be used to
     * clean up memory allocations that must persist from one invocation of
     * your filter to the next (for example, the stack in the stack filter.)
     * Note that this member will be called even if your filter has not been
     * used, so it *cannot* assume that _init or _process has been called
     * previously. */
  }

protected:
  template <class MyFilter>
  class RteHdFunctor
  {
  public:
    typedef void (MyFilter::*RteHdCb)(const route_head*);
    RteHdFunctor(MyFilter* obj, RteHdCb cb) : that(obj), _cb(cb) {}
    void operator()(const route_head* rh)
    {
      ((that)->*(_cb))(rh);
    }

  private:
    MyFilter* that;
    RteHdCb _cb;
  };

  template <class MyFilter>
  class WayptFunctor
  {
  public:
    typedef void (MyFilter::*WayptCb)(const Waypoint*);
    WayptFunctor(MyFilter* obj, WayptCb cb) : that(obj), _cb(cb) {}
    void operator()(const Waypoint* wpt)
    {
      ((that)->*(_cb))(wpt);
    }

  private:
    MyFilter* that;
    WayptCb _cb;
  };

};
#endif // FILTER_H_INCLUDED_
