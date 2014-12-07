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

#define MYNAME "swapdata"

#if FILTERS_ENABLED

static
arglist_t swapdata_args[] = {
  ARG_TERMINATOR
};

static void
swapdata_cb(const Waypoint* ref)
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

static void
swapdata_process(void)	/* this procedure must be present in vecs */
{
  waypt_disp_all(swapdata_cb);
  route_disp_all(NULL, NULL, swapdata_cb);
  track_disp_all(NULL, NULL, swapdata_cb);
}

/*******************************************************************************/

filter_vecs_t swapdata_vecs = {
  NULL,
  swapdata_process,
  NULL,
  NULL,
  swapdata_args
};

/*******************************************************************************/
#endif // FILTERS_ENABLED
