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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

 */
#ifndef gpsbabel_logging_h_included
#define gpsbabel_logging_h_included

// A wrapper for QDebug that provides a sensible Warning() and Fatal()
// with convenient functions, stream operators and manipulators.

#include <QtCore/QDebug>       // for QDebug
#include <QtCore/QFile>        // for QFile
#include <QtCore/QIODevice>    // for QIODevice, QIODevice::WriteOnly
#include <QtCore/QString>      // for QString
#include <QtCore/QTextStream>  // for QTextStream
#include <cstdio>              // for stderr
#include <cstdlib>             // for exit


class Warning : public QDebug {
public:
  explicit Warning(bool fatal = false) : QDebug(&msg_), fatal_(fatal) {
  }
  ~Warning() {
    QFile file;
    file.open(stderr, QIODevice::WriteOnly);
    QTextStream fileStream(&file);
    fileStream << msg_ << '\n';
    file.close();
    if (fatal_) {
      exit(1);
    }
  }
private:
  QString msg_;
  bool fatal_;
};

class Fatal : public Warning {
 public:
  Fatal() : Warning(true) {}
};

#endif //  gpsbabel_logging_h_included
