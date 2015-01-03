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
    Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111 USA

 */


#include <time.h>

#include <QtCore/QtGlobal>
#include <QtCore/QDateTime>

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
  DateTime() {
    setTime_t(0);
  }

  DateTime(QDate date, QTime time) : QDateTime(date, time) {}
  DateTime(QDateTime dt) : QDateTime(dt) {}

  // TODO: this should go away in favor of .addSecs().
  // add time_t without losing any existing milliseconds.
  DateTime& operator+=(const time_t& t) {
    QDateTime dt = addSecs(t);
    setDate(dt.date());
    setTime(dt.time());
    return *this;
  }

  // Integer form: YYMMDD
  int ymd() const {
    QDate d(date());
    return d.year() * 10000 + d.month() * 100 + d.day();
  }

  int ddmmyy() const {
    QDate d(date());
    return d.day() * 10000 + d.month() * 100 + d.year();
  }

  int hms() const {
    QTime t(time());
    return t.hour() * 10000 + t.minute() * 100 + t.second();
  }

  // Temporary: Override the standard, also handle time_t 0 as invalid.
  bool isValid() const {
    return date().isValid() && time().isValid() && toTime_t() > 0;
  }


  // Qt 4.6 and under doesn't have msecsTo.  Fortunately, it's easy to
  // provide.  It's a 64-bit because if the times aren't on the same day,
  // the returned value can be quite large.
  qint64 msecsTo(const QDateTime &dt) const {
    QDateTime dtutc = dt.toUTC();
    QDateTime thisutc = toUTC();
    qint64 days = thisutc.daysTo(dtutc);
    qint64 msecs = thisutc.time().msecsTo(dtutc.time());
    return days * (1000 * 3600 * 24) + msecs;
  }

  // Qt 4.6 and under doesn't have toMSecsSinceEpoch.
  qint64 toMSecsSinceEpoch() const {
    QDateTime epoch = QDateTime(QDate(1970, 1, 1), QTime(0, 0, 0, 0), Qt::UTC);
    return -msecsTo(epoch);
  }

  // This was added in Qt 4.7, but it's easy enough to knock out here.
  void setMSecsSinceEpoch(qint64 msecs) {
    int ddays = msecs / 86400000;
    msecs %= 86400000;
    setDate(QDate(1970, 1, 1).addDays(ddays));
    setTime(QTime(0,0).addMSecs(msecs));
  }

  // Like toString, but with subsecond time that's included only when
  // the trailing digits aren't .000.  Always UTC.
  QString toPrettyString() const {
    const char* format;
    if (time().msec()) {
      format = "yyyy-MM-ddTHH:mm:ss.zzzZ";
    } else {
      format = "yyyy-MM-ddTHH:mm:ssZ";
    }
    return toUTC().toString(format);
  }
};

} // namespace gpsbabel
