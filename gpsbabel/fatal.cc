/*
    Functions to indicate inconsistent or fatal conditions.

    Copyright (C) 2002-2014 Robert Lipe, robertlipe+source@gpsbabel.org

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
void
fatal(const char* fmt, ...)
{
  va_list ap;
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  va_end(ap);
  exit(1);
}

void
warning(const char* fmt, ...)
{
  va_list ap;
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  va_end(ap);
}

void
debug_print(int level, const char* fmt, ...)
{
  va_list ap;
  int i;
  for (i=1; i<level; i++) {
    fprintf(stderr, ".");
  }
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  va_end(ap);
}

