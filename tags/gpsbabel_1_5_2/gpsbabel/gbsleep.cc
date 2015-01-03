/*
    OS abstraction to sleep a given number of milliseconds.

    Copyright (C) 2006 Robert Lipe, robertlipe+source@gpsbabel.org

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
#if HAVE_CONFIG_H
#include "config.h"
#endif

#if __WIN32__

#include <windows.h>
void
gb_sleep(unsigned long microseconds)
{
  Sleep(microseconds/1000 + 1);
}

#elif defined HAVE_NANOSLEEP

#include <time.h>
void
gb_sleep(unsigned long microseconds)
{
  struct timespec req;
  req.tv_sec  = microseconds / 1000000;
  req.tv_nsec = (microseconds * 1000) % 1000000000;
  nanosleep(&req, NULL);
}
#elif defined HAVE_SLEEP
/* Amazingly underachieving, but probably "good enough" */
#include <unistd.h>
void
gb_sleep(unsigned long microseconds)
{
  sleep(microseconds / 1000000);
}
#endif
