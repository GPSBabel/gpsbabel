/*
    Global data for GPSBabel.
   
    Copyright (C) 2005 Robert Lipe, robertlipe@usa.net

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
#include "gbversion.h"

global_options global_opts;
const char gpsbabel_version[] = VERSION;
time_t gpsbabel_now;	/* gpsbabel startup-time; initialized in main.c with time() */
time_t gpsbabel_time;	/* gpsbabel startup-time; initialized in main.c with current_time(), ! ZERO within testo ! */

posn_status tracking_status;
