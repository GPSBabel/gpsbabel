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

#include "defs.h"              // for parse_double, parse_integer


int OptionString::toInt() const
{
  return parse_integer(value_, id_);
}

int OptionString::toInt(bool* ok) const
{
  return parse_integer(value_, id_, ok);
}

int OptionString::toInt(bool* ok, QString* end, int base) const
{
  return parse_integer(value_, id_, ok, end, base);
}

double OptionString::toDouble() const
{
  return parse_double(value_, id_);
}

double OptionString::toDouble(bool* ok) const
{
  return parse_double(value_, id_, ok);
}

double OptionString::toDouble(bool* ok, QString* end) const
{
  return parse_double(value_, id_, ok, end);
}

void OptionInt::init(const QString& id)
{
  id_ = id;
}

void OptionInt::reset()
{
  value_ = QString();
  result_ = 0;
  end_ = QString();
}

void OptionInt::set(const QString& s)
{
  value_ = s;

  // Fatal on conversion error.
  QString* endp = allow_trailing_data_? &end_: nullptr;
  constexpr bool* dieonerror = nullptr;
  result_ = parse_integer(value_, id_, dieonerror, endp, base_);
}

bool OptionInt::isValid(const QString& s) const
{
  bool ok;
  QString end;
  QString* endp = allow_trailing_data_? &end : nullptr;
  (void) parse_integer(s, id_, &ok, endp, base_);
  return ok;
}

int OptionInt::get_result(QString* end) const
{
  if (end != nullptr) {
    *end = end_;
  }
  return result_;
}

bool OptionInt::trailing_data_allowed() const
{
  return allow_trailing_data_;
}

void OptionDouble::init(const QString& id)
{
  id_ = id;
}

void OptionDouble::reset()
{
  value_ = QString();
  result_ = 0.0;
  end_ = QString();
}

void OptionDouble::set(const QString& s)
{
  value_ = s;

  // Fatal on conversion error.
  QString* endp = allow_trailing_data_? &end_: nullptr;
  constexpr bool* dieonerror = nullptr;
  result_ = parse_double(value_, id_, dieonerror, endp);
}

bool OptionDouble::isValid(const QString& s) const
{
  bool ok;
  QString end;
  QString* endp = allow_trailing_data_? &end : nullptr;
  (void) parse_double(s, id_, &ok, endp);
  return ok;
}

double OptionDouble::get_result(QString* end) const
{
  if (end != nullptr) {
    *end = end_;
  }
  return result_;
}

bool OptionDouble::trailing_data_allowed() const
{
  return allow_trailing_data_;
}

OptionInt::OptionInt(bool allow_trailing_data, int base) :
  allow_trailing_data_(allow_trailing_data),
  base_(base)
{}

bool OptionInt::has_value() const
{
  return !value_.isNull();
}

bool OptionInt::isEmpty() const
{
  return value_.isEmpty();
}

const QString& OptionInt::get() const
{
  return value_;
}

OptionInt::operator const QString& () const
{
    return value_;
}
