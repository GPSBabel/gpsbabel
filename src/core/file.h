/*
    Copyright (C) 2013 Robert Lipe, gpsbabel.org

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

#include <QtCore/QFile>
#include <QtCore/QIODevice>
#include <stdio.h>
#include "defs.h"

// Mimic gbfile open services

namespace gpsbabel
{

class File : public QFile
{
public:
  File(const QString& s) : QFile(s) {}

  /* in the tradition of gbfile we assume WriteOnly or ReadOnly, not ReadWrite */
  bool open(OpenMode mode) {
    bool status;

    if (QFile::fileName() == "-") {
      if (mode & QIODevice::WriteOnly) {
        status = QFile::open(stdout, mode);
      } else {
        status = QFile::open(stdin, mode);
      }
    } else {
      status =  QFile::open(mode);
    }

    if (!status) {
      fatal("Cannot open '%s' for %s.  Error was '%s'.\n",
            qPrintable(QFile::fileName()),
            mode & QIODevice::WriteOnly? "write" : "read",
            qPrintable(QFile::errorString()));
    }
    return status;
  }

};

}; // namespace gpsbabel
