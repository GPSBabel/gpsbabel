/*

    Format converter module skeleton.

    Steps to create a new format.

    1) Copy this file to <your_format_name>.c
    2) Rename all format_skeleton tokens to <your_format_name>.
    3) Replace the fictional name and address in the Copyright section below.
       ** As your work is likely built on the work of others, please retain
       the original line. **
    4) Create a new section in vecs.c.
    5) Add compilation instructions to Makefile.
    6) Add sample files (it's better when they're created by the "real"
       application and not our own output) to reference/ along with
       files in a well supported (preferably non-binary) format and
       entries in our 'testo' program.   This allows users of different
       OSes and hardware to exercise your module.

    Copyright (C) YYYY John Doe, anybody@wherever.com
    Copyright (C) 2001-YYYY Robert Lipe, robertlipe+source@gpsbabel.org

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
#ifndef FORMAT_H_INCLUDED_
#define FORMAT_H_INCLUDED_

#include "defs.h"


class Format
{
public:
  Format() = default;
  // Provide virtual public destructor to avoid undefined behavior when
  // an object of derived class type is deleted through a pointer to
  // its base class type.
  // https://wiki.sei.cmu.edu/confluence/display/cplusplus/OOP52-CPP.+Do+not+delete+a+polymorphic+object+without+a+virtual+destructor
  virtual ~Format() = default;
  // And that requires us to explicitly default or delete the move and copy operations.
  // To prevent slicing we delete them.
  // https://github.com/isocpp/CppCoreGuidelines/blob/master/CppCoreGuidelines.md#c21-if-you-define-or-delete-any-default-operation-define-or-delete-them-all.
  Format(const Format&) = delete;
  Format& operator=(const Format&) = delete;
  Format(Format&&) = delete;
  Format& operator=(Format&&) = delete;

  /*******************************************************************************
  * %%%        global callbacks called by gpsbabel main process              %%% *
  *******************************************************************************/

  virtual void rd_init(const QString& /* fname */)
  {
    fatal("Format does not support reading.\n");
//	fin = gbfopen(fname, "r", MYNAME);
  }

  virtual void rd_deinit()
  {
//	gbfclose(fin);
  }

  virtual void read()
  {
//	your special code to extract waypoint, route and track
//	information from gbfile "fin"
//
// Sample text-file read code:
//	char *s;
//	while ((s = gbfgetstr(fin))) {
//		do_anything(s);
//	}
//
//
// For waypoints:
//         while (have waypoints) {
//                 waypoint = new waypoint
//                 populate waypoint
//                 waypt_add(waypoint);
//         }
//
// For routes:
//
//         route = route_head_alloc();
//         populate struct route_hdr
//	   route_add_head(route);
//         while (have more routepoints) {
//                 waypoint = new waypoint
//                 populate waypoint
//                 route_add_wpt(route, waypoint)
//         }
//
// Tracks are just like routes, except the word "track" replaces "routes".
//
  }

  virtual void wr_init(const QString& /* fname */)
  {
    fatal("Format does not support writing.\n");
//	fout = gbfopen(fname, "w", MYNAME);
  }

  virtual void wr_deinit()
  {
//	gbfclose(fout);
  }

  virtual void write()
  {
// Here is how you register callbacks for all waypoints, routes, tracks.
// waypt_disp_all(waypt)
// route_disp_all(head, tail, rtept);
// track_disp_all(head, tail, trkpt);
  }

  virtual void exit()		/* optional */
  {
  }

  virtual void rd_position_init(const QString& /* fname */)
  {
    fatal("Realtime tracking (-T) is not supported by this input type.\n");
  }

  virtual Waypoint* rd_position(posn_status* /* status */)
  {
    return nullptr;
  }

  virtual void rd_position_deinit()
  {
  }

  virtual void wr_position_init(const QString& /* fname */)
  {
  }

  virtual void wr_position(Waypoint* /* wpt */)
  {
    fatal("This output format does not support output of realtime positioning.\n");
  }

  virtual void wr_position_deinit()
  {
  }

  /*******************************************************************************
  * %%%                          Accessors                                   %%% *
  *******************************************************************************/

  virtual QVector<arglist_t>* get_args()
  {
    return nullptr;
  }

  virtual ff_type get_type() const = 0;
  virtual QVector<ff_cap> get_cap() const = 0;
  virtual QString get_encode() const = 0;
  virtual int get_fixed_encode() const = 0;

  QString get_name() const
  {
    return name;
  }

  void set_name(const QString& nm)
  {
    name = nm;
  }

private:
  QString name;

protected:
  template <class MyFormat>
  class RteHdFunctor
  {
  public:
    using RteHdCb = void (MyFormat::*)(const route_head*);
    RteHdFunctor(MyFormat* obj, RteHdCb cb) : that(obj), _cb(cb) {}
    void operator()(const route_head* rh)
    {
      ((that)->*(_cb))(rh);
    }

  private:
    MyFormat* that;
    RteHdCb _cb;
  };

  template <class MyFormat>
  class WayptFunctor
  {
  public:
    using WayptCb = void (MyFormat::*)(const Waypoint*);
    WayptFunctor(MyFormat* obj, WayptCb cb) : that(obj), _cb(cb) {}
    void operator()(const Waypoint* wpt)
    {
      ((that)->*(_cb))(wpt);
    }

  private:
    MyFormat* that;
    WayptCb _cb;
  };

};
#endif // FORMAT_H_INCLUDED_
