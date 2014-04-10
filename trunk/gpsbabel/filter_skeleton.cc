/*

    Filter skeleton:

    Simply copy this file to <your_filter_name>.c and
    rename all filter_skeleton tokens to <your_filter_name>. Replace
    the stupid name and address in the Copyright few lines below.
    To active your new filter you have to create a new section in
    filter_vecs and finally add complying statements to Makefile.

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
    Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111 USA

 */

#include "defs.h"
#include "filterdefs.h"
#include <ctype.h>

#define MYNAME "filter_skeleton"

#if FILTERS_ENABLED

// Any arg in this list will appear in command line help and will be
// populated for you.
static
arglist_t filter_skeleton_args[] = {
// {"foo", &fooopt, "The text of the foo option in help",
//   "default", ARGYTPE_STRING, ARG_NOMINMAX} ,
  ARG_TERMINATOR
};

/*******************************************************************************
* %%%        global callbacks called by gpsbabel main process              %%% *
*******************************************************************************/

static void
filter_skeleton_init(const char* args)
{
  /* Called before filter processing */

  /* optional.  If not needed, delete and replace entry in vecs with NULL  */

  /* This may be used to parse filter options, allocate memory, and do other
   * housekeeping that should be done before filtering */
}

static void
filter_skeleton_process(void)	/* this procedure must be present in vecs */
{
// Here is how you register callbacks for all waypoints, routes, tracks.
// waypt_disp_all(waypt)
// route_disp_all(head, tail, rtept);
// track_disp_all(head, tail, trkpt);
}

static void
filter_skeleton_deinit(void)
{
  /* called after filter processing */

  /* optional.   If not needed, delete and replace entry in vecs with NULL */

  /* This should be used to clean up any memory allocations that are no longer
   * needed after the filter terminates. */
}

static void
filter_skeleton_exit(void)
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

/*******************************************************************************/

filter_vecs_t filter_skeleton_vecs = {
  filter_skeleton_init,
  filter_skeleton_process,
  filter_skeleton_deinit,
  filter_skeleton_exit,
  filter_skeleton_args
};

/*******************************************************************************/
#endif // FILTERS_ENABLED
