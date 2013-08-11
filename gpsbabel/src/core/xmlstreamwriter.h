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
#include <QtCore/QXmlStreamWriter>

// As this code began in C, we have several hundred places that write
// c strings.  Add a test that the string contains anything useful
// before serializing an empty tag.

namespace gpsbabel {

class XmlStreamWriter : public QXmlStreamWriter {
public:
  XmlStreamWriter(QString* s) : QXmlStreamWriter(s) {}
  XmlStreamWriter(QFile* f) : QXmlStreamWriter(f) {}

  // Dont emit the attribute if there's nothing interesting in it.
  void writeOptionalAttribute(QString tag, QString value) {
    if (!value.isEmpty()) {
      writeAttribute(tag, value);
    }
  }

  // Dont emit the tag if there's nothing interesting in it.
  void writeOptionalTextElement(QString tag, QString value) {
    if (!value.isEmpty()) {
      writeTextElement(tag, value);
    }
  }

};

}; // namespace gpsbabel
