/*

    Swap data filter

    Copyright (C) 2008 Olaf Klein, o.b.klein@gpsbabel.org

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

#ifndef SWAPDATA_H_INCLUDED_
#define SWAPDATA_H_INCLUDED_

#include <QVector>         // for QVector

#include "defs.h"    // for arglist_t, Waypoint
#include "filter.h"  // for Filter

#if FILTERS_ENABLED

class SwapDataFilter:public Filter
{
public:
  QVector<arglist_t>* get_args() override
  {
    return &args;
  }
  void process() override;

private:
  QVector<arglist_t> args = {
  };

  void swapdata_cb(const Waypoint* ref);

};

#endif // FILTERS_ENABLED
#endif // SWAPDATA_H_INCLUDED_
