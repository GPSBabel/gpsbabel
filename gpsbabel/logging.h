/*
    Copyright (C) 2014 Robert Lipe, robertlipe+source@gpsbabel.org

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
#ifndef gpsbabel_logging_h_included
#define gpsbabel_logging_h_included

// A wrapper for QTextStream that provides a sensible Warning() and Fatal()
// with convenient stream operators.

#include <QtCore/QTextStream.h>
#include <QtCore/QFile.h>

class Warning {
 public:
  Warning(bool fatal = false) :
   fatal_(fatal) {
    file_.open(stderr, QIODevice::WriteOnly);
    fileStream_.setDevice(&file_);
  }
  ~Warning() {
    fileStream_ << '\n';
    if (fatal_) {
      fileStream_.flush();
      exit(1);
    }
  }
  inline Warning& operator << (char d) { fileStream_ << d; return optionalSpace(); }
  inline Warning& operator << (signed short d) { fileStream_ << d; return optionalSpace(); }
  inline Warning& operator << (unsigned short d) { fileStream_ << d; return optionalSpace(); }
  inline Warning& operator << (signed int d) { fileStream_ << d; return optionalSpace(); }
  inline Warning& operator << (unsigned int d) { fileStream_ << d; return optionalSpace(); }
  inline Warning& operator << (signed long d) { fileStream_ << d; return optionalSpace(); }
  inline Warning& operator << (unsigned long d) { fileStream_ << d; return optionalSpace(); }
  inline Warning& operator << (float d) { fileStream_ << d; return optionalSpace(); }
  inline Warning& operator << (double d) { fileStream_ << d; return optionalSpace(); }
  inline Warning& operator << (const char* d) { fileStream_ << QString::fromUtf8(d); return optionalSpace(); }
  inline Warning& operator << (QString d) { fileStream_ << '\"' << d << '\"'; return optionalSpace(); }
  inline Warning& operator << (const void* d) { fileStream_ << '\"' << d << '\"'; return optionalSpace(); }

  inline Warning& optionalSpace() {
    fileStream_ << ' ';
    return *this;
  }
private:
  QFile file_;
  QTextStream fileStream_;
  bool fatal_;
};

class Fatal : public Warning {
 public:
  Fatal() : Warning(true) {}
};

#endif //  gpsbabel_logging_h_included
