/*
STATUS: experimental
    For data types we might not know (altitude, heartrate) provide a simple
    set of getters/setters to them that still exposes whether a member is
    valid without leaking through 'unknown' values.

    Copyright (C) 2013 Robert Lipe, robertlipe@gpsbabel.org

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

#include <limits>

// As this code began in C, we have several hundred places that set and
// read creation_time as a time_t.  Provide some operator overloads to make
// that less painful.
// Probably all of these should eventually go away and be replaced by 
// "Better" code in the callers.

// Consider putting in a namespace instead of prefixing 'gb'.
namespace gpsbabel {

class OptionalDouble {
public:
  OptionalDouble() :
    valid_(false) { }

  inline bool isValid() const {
    return valid_;
  }

  double Get() const {
    if (valid_) {
      return v_;
    }
    // This might be heavy-handed.  Perhaps ctor could declare what "you read
    // my thing when I told you not to read my thing" value should return.
    return std::numeric_limits<double>::quiet_NaN();
  }

  void Set(double v) {
    valid_ = true;
    v_ = v;
  }

 private:
  double v_;
  bool valid_;
};

} // namespace gpsbabel
