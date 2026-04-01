/*
    Copyright (C) 2007 Olaf Klein, o.b.klein@gpsbabel.org
    Copyright (C) 2024 Robert Lipe, robertlipe+source@gpsbabel.org

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

#ifndef PARSE_H_INCLUDED_
#define PARSE_H_INCLUDED_

#include <QString>

/*
 * Text parsing functions for converting strings to numeric values.
 * These provide consistent error handling and validation across GPSBabel.
 */

/*
 * parse_integer
 *
 *  str:     input string
 *  id:      identifier for error messages (typically option name)
 *  ok:      conversion status.
 *           if nullptr any conversion errors will be fatal.
 *  end:     unconverted trailing portion of string.
 *           if nullptr a non-empty trailing portion will cause a conversion error.
 *  base:    conversion base (10 for decimal, 16 for hex, etc.)
 *
 * Returns the parsed integer value, or 0 if conversion fails and ok != nullptr.
 */
int parse_integer(const QString& str, const QString& id, bool* ok = nullptr, QString* end = nullptr, int base = 10);

/*
 * parse_double
 *
 *  str:     input string
 *  id:      identifier for error messages (typically option name)
 *  ok:      conversion status.
 *           if nullptr any conversion errors will be fatal.
 *  end:     unconverted trailing portion of string.
 *           if nullptr a non-empty trailing portion will cause a conversion error.
 *
 * Returns the parsed double value, or 0.0 if conversion fails and ok != nullptr.
 */
double parse_double(const QString& str, const QString& id, bool* ok = nullptr, QString* end = nullptr);

#endif // PARSE_H_INCLUDED_