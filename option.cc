/*
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
#include "option.h"

#include <QString>             // for QString

#include "defs.h"              // for strToDouble, strToInt


int OptionString::toInt()
{
  return parse_integer(value_, id_);
}

int OptionString::toInt(bool* ok)
{
  return parse_integer(value_, id_, ok);
}

int OptionString::toInt(bool* ok, QString* end, int base)
{
  return parse_integer(value_, id_, ok, end, base);
}

double OptionString::toDouble()
{
  return parse_double(value_, id_);
}

double OptionString::toDouble(bool* ok)
{
  return parse_double(value_, id_, ok);
}

double OptionString::toDouble(bool* ok, QString* end)
{
  return parse_double(value_, id_, ok, end);
}
