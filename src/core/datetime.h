/*
    Shim to provide overloads between QDateTime and the zillions of places
    that know about time_t.

    Copyright (C) 2012, 2013 Robert Lipe, robertlipe@gpsbabel.org

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

#ifndef DATETIME_H_INCLUDED_
#define DATETIME_H_INCLUDED_

#include <ctime>

#include <QtCore/QtGlobal>
#include <QtCore/QDateTime>
#include <QtCore/QString>

// As this code began in C, we have several hundred places that set and
// read creation_time as a time_t.  Provide some operator overloads to make
// that less painful.
// Probably all of these should eventually go away and be replaced by 
// "Better" code in the callers.

// Consider putting in a namespace instead of prefixing 'gb'.
namespace gpsbabel {

class DateTime : public QDateTime {
public:
  // As a crutch, mimic the old behaviour of an uninitialized creation time
  // being 1/1/1970.
  // The choice of Qt::TimeSpec here can be critical to performance.
  // With a Qt::LocalTime conversions between Qt::UTC and Qt::LocalTime
  // can be required when using a method such as *Epoch, add*.
  // Using Qt::Utc avoids these conversions.
  // The lowranceusr regression test was measured taking 2.7x longer,
  // and the entire regression suite took 1.7x longer, with
  // Qt::LocalTime compared to Qt::UTC on ubuntu bionic.
  // Note that these conversions can be required if the Qt::TimeSpec is
  // set to Qt:LocalTime after construction.
  DateTime() : QDateTime(QDateTime::fromMSecsSinceEpoch(0, Qt::UTC)) {
  }

  DateTime(const QDate& date, const QTime& time) : QDateTime(date, time) {}
  DateTime(const QDateTime& dt) : QDateTime(dt) {}

  // Temporary: Override the standard, also handle time_t 0 as invalid.
  bool isValid() const {
    return date().isValid() && time().isValid() && toTime_t() > 0;
  }

  // Like toString, but with subsecond time that's included only when
  // the trailing digits aren't .000.  Always UTC.
  QString toPrettyString() const {
    if (time().msec()) {
      return toUTC().toString(QStringLiteral("yyyy-MM-ddTHH:mm:ss.zzzZ"));
    } else {
      return toUTC().toString(QStringLiteral("yyyy-MM-ddTHH:mm:ssZ"));
    }
  }
};

} // namespace gpsbabel

#endif // DATETIME_H_INCLUDED_
