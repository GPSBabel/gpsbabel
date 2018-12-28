/*
    Copyright (C) 2018 Robert Lipe, robertlipe@gpsbabel.org

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

// someday, when we require c++17,
// the intent is that this can all go away by
// changing:
// 1. 'gpsbabel_optional::optional' to 'std::optional',
// 2. 'include "optional.h"' becomes 'include <optional>'.

#ifndef GPSBABEL_OPTIONAL_H_INCLUDED_
#define GPSBABEL_OPTIONAL_H_INCLUDED_

#if __cplusplus >= 201703L
#warning "using std::optional"
#include <optional>
#define gpsbabel_optional std
#else
#include <cassert>
namespace gpsbabel_optional
{
template<typename T>
class optional
{
private:
  bool null{true};
  T v{};

public:
  constexpr optional() noexcept = default;
  explicit optional(T n) : null{false}, v{n} {}

  // There is a known mismatch with std::optional.
  // o = {}; should act like reset() so o.has_value() returns false.
  //         instead o.has_value() returns true and o.value() returns 0.
  optional& operator=(const T& rhs)
  {
    null = false;
    v = rhs;
    return *this;
  }

  // if constexpr will conflict with constexpr non-const version in c++11, ok in c++14.
  // but MSVC2015 with the c++14 standard can't handle it.  MSVC2017 is ok.
  // intentionally does not check to see if a value is contained, use value() for that.
  const T& operator*() const&
  {
    return v;
  }

  // if constexpr will conflict with constexpr const version in c++11, ok in c++14.
  // but MSVC2015 with the c++14 standard can't handle it.  MSVC2017 is ok.
  // intentionally does not check to see if a value is contained, use value() for that.
  T& operator*()&
  {
    return v;
  }

  constexpr explicit operator bool() const noexcept
  {
    return !null;
  }

  constexpr bool has_value() const noexcept
  {
    return !null;
  }

  // if constexpr will conflict with constexpr const version in c++11, ok in c++14.
  // but MSVC2015 with the c++14 standard can't handle it.  MSVC2017 is ok.
  // if constexpr cannot use assert.
  T& value() &
  {
    assert(!null);
    return v;
  }

  // if constexpr will conflict with constexpr non-const version in c++11, ok in c++14.
  // but MSVC2015 with the c++14 standard can't handle it.  MSVC2017 is ok.
  // if constexpr cannot use assert.
  const T& value() const&
  {
    assert(!null);
    return v;
  }

  void reset() noexcept
  {
    null = true;
  }
};

template< class T, class U >
constexpr bool operator==(const optional<T>& lhs, const optional<U>& rhs)
{
  if (bool(lhs) != bool(rhs)) {
    return false;
  }
  if (bool(lhs) == false) {
    return true;
  }
  return *lhs == *rhs;
}

template< class T, class U >
constexpr bool operator!=(const optional<T>& lhs, const optional<U>& rhs)
{
  if (bool(lhs) != bool(rhs)) {
    return true;
  }
  if (bool(lhs) == false) {
    return false;
  }
  return *lhs != *rhs;
}

template< class T, class U >
constexpr bool operator<(const optional<T>& lhs, const optional<U>& rhs)
{
  if (!rhs) {
    return false;
  }
  if (!lhs) {
    return true;
  }
  return *lhs < *rhs;
}

template< class T, class U >
constexpr bool operator>(const optional<T>& lhs, const optional<U>& rhs)
{
  if (!lhs) {
    return false;
  }
  if (!rhs) {
    return true;
  }
  return *lhs > *rhs;
}

template< class T, class U >
constexpr bool operator<=(const optional<T>& lhs, const optional<U>& rhs)
{
  if (!lhs) {
    return true;
  }
  if (!rhs) {
    return false;
  }
  return *lhs <= *rhs;
}

template< class T, class U >
constexpr bool operator>=(const optional<T>& lhs, const optional<U>& rhs)
{
  if (!rhs) {
    return true;
  }
  if (!lhs) {
    return false;
  }
  return *lhs >= *rhs;
}


template< class T, class U >
constexpr bool operator==(const optional<T>& opt, const U& value)
{
  return bool(opt) ? (*opt == value) : false;
}

template< class T, class U >
constexpr bool operator==(const T& value, const optional<U>& opt)
{
  return bool(opt) ? (value == *opt) : false;
}

template< class T, class U >
constexpr bool operator!=(const optional<T>& opt, const U& value)
{
  return bool(opt) ? (*opt != value) : true;
}

template< class T, class U >
constexpr bool operator!=(const T& value, const optional<U>& opt)
{
  return bool(opt) ? (value != *opt) : true;
}

template< class T, class U >
constexpr bool operator<(const optional<T>& opt, const U& value)
{
  return bool(opt) ? (*opt < value) : true;
}

template< class T, class U >
constexpr bool operator<(const T& value, const optional<U>& opt)
{
  return bool(opt) ? (value < *opt) : false;
}

template< class T, class U >
constexpr bool operator<=(const optional<T>& opt, const U& value)
{
  return bool(opt) ? *opt <= value : true;
}

template< class T, class U >
constexpr bool operator<=(const T& value, const optional<U>& opt)
{
  return bool(opt) ? value <= *opt : false;
}

template< class T, class U >
constexpr bool operator>(const optional<T>& opt, const U& value)
{
  return bool(opt) ? (*opt > value) : false;
}

template< class T, class U >
constexpr bool operator>(const T& value, const optional<U>& opt)
{
  return bool(opt) ? (value > *opt) : true;
}

template< class T, class U >
constexpr bool operator>=(const optional<T>& opt, const U& value)
{
  return bool(opt) ? (*opt >= value) : false;
}

template< class T, class U >
constexpr bool operator>=(const T& value, const optional<U>& opt)
{
  return bool(opt) ? (value >= *opt) : true;
}

} // namespace
#endif

#endif // GPSBABEL_OPTIONAL_H_INCLUDED_
