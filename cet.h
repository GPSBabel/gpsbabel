/*

    Character encoding transformation - basics header

    Copyright (C) 2005-2008 Olaf Klein, o.b.klein@gpsbabel.org

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

#ifndef CET_H
#define CET_H

#include <cstddef> // for size_t

#define CET_ERROR	1
#define CET_SUCCESS	0

/* single char/value transmission */

int cet_utf8_to_ucs4(const char* str, int* bytes, int* value);
int cet_ucs4_to_utf8(char* dest, size_t dest_size, int value);

#endif
