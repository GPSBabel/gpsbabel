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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

 */

#ifndef SRC_CORE_FILE_INCLUDED_H_
#define SRC_CORE_FILE_INCLUDED_H_

#include <QFile>
#include <QFileInfo>
#include <QStringBuilder>
#include <QIODevice>
#include <cstdio>
#include "src/core/logging.h"
#include "defs.h"

// Mimic gbfile open services

namespace gpsbabel
{

class File : public QFile
{
public:
    explicit File(const QString& s) : QFile(s) {}

  /* in the tradition of gbfile we assume WriteOnly or ReadOnly, not ReadWrite */
  bool open(OpenMode mode) override {
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
      fatal(FatalMsg().noquote() << "Cannot open '" %
        (gpsbabel_testmode() ?
          QFileInfo(*this).fileName() :
          QFileInfo(*this).absoluteFilePath()) %
        "' for " %
        (mode & QIODevice::WriteOnly ? "write" : "read") %
        ".  Error was '" %
        QFile::errorString()  %
        "'.");
    }
    return status;
  }

};

} // namespace gpsbabel

#endif // SRC_CORE_FILE_INCLUDED_H_
