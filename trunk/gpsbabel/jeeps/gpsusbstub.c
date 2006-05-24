/*
    Stubs to keep build happy when USB just isn't available to us.

    Copyright (C) 2004, 2006 Robert Lipe, robertlipe@usa.net

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


#include "config.h"
#if !HAVE_LIBUSB 

const char no_usb[] = "USB suport is not available in this build.\n";

int
gusb_init(const char *portname)
{
	fatal(no_usb);
}

#endif /* defined(NO_USB) */
