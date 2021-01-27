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

#include "src/core/xmlstreamwriter.h"

#include <QtCore/QString>           // for QString
#include <QtCore/QXmlStreamWriter>  // for QXmlStreamWriter
#include <QtCore/QtGlobal>          // for QT_VERSION, QT_VERSION_CHECK

// As this code began in C, we have several hundred places that write
// c strings.  Add a test that the string contains anything useful
// before serializing an empty tag.
// We rely on Qt to strip out characters that are illegal in xml.  These can
// creep into our structures from other formats where they are legal.

// Verify Qt is new enough to strip out illegal characters.
// This fix went into Qt 5.11.0. See
// https://github.com/GPSBabel/gpsbabel/issues/637
// https://bugreports.qt.io/browse/QTBUG-63150
// https://github.com/qt/qtbase/commit/3b5b8f1d4ab8092e5dd337b7b4e32d85fda2e0b7
#if (QT_VERSION < QT_VERSION_CHECK(5, 11, 0))
#error We rely on the fix for QTBUG-63150 introduced in Qt 5.11.0.
#endif


namespace gpsbabel
{
// Dont emit the element if there's nothing interesting in it.
void XmlStreamWriter::writeOptionalTextElement(const QString& qualifiedName, const QString& text)
{
  if (!text.isEmpty()) {
    QXmlStreamWriter::writeTextElement(qualifiedName, text);
  }
}

} // namespace gpsbabel
