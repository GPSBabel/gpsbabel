/*
    Copyright (C) 2019 Robert Lipe, gpsbabel.org

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

#include <QtCore/QByteArray>   // for QByteArray
#include <QtCore/QIODevice>    // for QIODevice, QIODevice::OpenModeFlag
#include <QtCore/QString>      // for QString
#include <QtCore/QTextCodec>   // for QTextCodec
#include <QtCore/QTextStream>  // for QTextStream

#include "src/core/file.h"     // for File


namespace gpsbabel
{

class TextStream : public QTextStream
{
public:
  void open(const QString& fname, QIODevice::OpenModeFlag mode, const char* module, const char* codec = "UTF-8");
  void close();

private:
  gpsbabel::File* file_{nullptr};
  QTextCodec* codec_{nullptr};
};

}; // namespace
