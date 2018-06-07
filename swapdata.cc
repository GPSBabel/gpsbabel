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
    Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111 USA

 */

#include "defs.h"
#include "filterdefs.h"
#include "swapdata.h"

#define MYNAME "swapdata"

#if FILTERS_ENABLED

SwapDataFilter* SwapDataFilter::fObj = nullptr; // definition required for odr-use.

void SwapDataFilter::swapdata_cb(const Waypoint* ref)
{
  Waypoint* wpt = (Waypoint*)ref;
  double x;

  x = wpt->latitude;
  wpt->latitude = wpt->longitude;
  wpt->longitude = x;

  return;
}

/*******************************************************************************
* %%%        global callbacks called by gpsbabel main process              %%% *
*******************************************************************************/

void SwapDataFilter::process(void)	/* this procedure must be present in vecs */
{
  setObj(*this);

  waypt_disp_all(&swapdata_cb_glue);
  route_disp_all(nullptr, nullptr, &swapdata_cb_glue);
  track_disp_all(nullptr, nullptr, &swapdata_cb_glue);
}

#endif // FILTERS_ENABLED
