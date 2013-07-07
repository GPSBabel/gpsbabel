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

#include <QtCore/qdatetime.h>

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
    this->setTime_t(0);
    t_ = -1;
  }

  DateTime(QDate date, QTime time) : QDateTime(date, time) { }
  DateTime(QDateTime dt) : QDateTime(dt) { }


  // Handle time_tm tm = wpt->creation_time;
  operator const time_t() const  {
    return this->toTime_t();
  }

  const time_t& operator=(const time_t& t) {
    this->setTime_t(t);
    return t;
  }

  time_t operator-- (int)  {
    setTime_t(toTime_t() - 1);
    return this->toTime_t();
  }

  time_t operator++ (int)  {
    setTime_t(toTime_t() + 1);
    return this->toTime_t();
  }

  time_t operator+=(const time_t&t) {
   setTime_t(toTime_t() + t);
    return this->toTime_t();
  }

  // Handle       tm = *gmtime(&wpt->creation_time)  ...poorly.
  inline const time_t* operator&() {
    t_ = this->toTime_t();
//fprintf(stderr, "inline set time_t %d\n", t_);
    return &t_;
  }

  // Before Qt, GPSBabel had a 'microseconds' which is excessive and
  // not really supported in QDateTime.  Milliseconds is fine, but we
  // provide these shims for code that used usecs.
  void addUSecs(qint64 usecs) const {
    this->addMSecs(usecs / 1000);
  }
  int usec() const {
    return this->time().msec() * 1000;
  }

  // Integer form: YYMMDD
  int ymd() const {
    QDate date(this->date());
    return date.year() * 10000 + date.month() * 100 + date.day();
  }

  int ddmmyy() const {
    QDate date(this->date());
    return date.day() * 10000 + date.month() * 100 + date.year();
  }

  int hms() const {
    QTime time(this->time());
    return time.hour() * 10000 + time.minute() * 100 + time.second();
  }

  // Like toString, but with subsecond time that's included only when
  // the trailing digits aren't .000.  Always UTC.
  QString toPrettyString() const {
    const char* format;
    if (this->time().msec()) {
      format = "yyyy-MM-ddTHH:mm:ss.zzzZ";
    } else {
      format = "yyyy-MM-ddTHH:mm:ssZ";
    }
    return this->toUTC().toString(format);
  }

 private:
  time_t t_;
};

} // namespace gpsbabel
