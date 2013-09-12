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

#include <src/core/xmlstreamwriter.h>
#include <QtCore/QtGlobal>

#include <QtCore/QFile>
#if (QT_VERSION < QT_VERSION_CHECK(5, 0, 0))
#include <QtCore/QRegExp>
#else
#include <QtCore/QRegularExpression>
#endif
#include <QtCore/QXmlStreamWriter>

// As this code began in C, we have several hundred places that write
// c strings.  Add a test that the string contains anything useful
// before serializing an empty tag.
// We also strip out characters that are illegal in xml.  These can creep
// into our structures from other formats where they are legal.

namespace gpsbabel
{

XmlStreamWriter:: XmlStreamWriter(QString* s) : QXmlStreamWriter(s) {}

XmlStreamWriter::XmlStreamWriter(QFile* f) : QXmlStreamWriter(f) {}

#if (QT_VERSION < QT_VERSION_CHECK(5, 0, 0))
QRegExp XmlStreamWriter::badXml10 = QRegExp("[\\x0000-\\x0008]|[\\x000b-\\x000c]|[\\x000e-\\x001f]");
#else
QRegularExpression XmlStreamWriter::badXml10 = QRegularExpression("[\\x00-\\x08]|[\\x0b-\\x0c]|[\\x0e-\\x1f]");
#endif

// Dont emit the attribute if there's nothing interesting in it.
void XmlStreamWriter::writeOptionalAttribute(const QString& qualifiedName, QString value)
{
  if (!value.isEmpty()) {
    QXmlStreamWriter::writeAttribute(qualifiedName, value.replace(badXml10, " "));
  }
}

// Dont emit the element if there's nothing interesting in it.
void XmlStreamWriter::writeOptionalTextElement(const QString& qualifiedName, QString text)
{
  if (!text.isEmpty()) {
    QXmlStreamWriter::writeTextElement(qualifiedName, text.replace(badXml10, " "));
  }
}

void XmlStreamWriter::writeAttribute(const QString& qualifiedName, QString value)
{
  QXmlStreamWriter::writeAttribute(qualifiedName, value.replace(badXml10, " "));
}

void XmlStreamWriter::writeCDATA(QString text)
{
  QXmlStreamWriter::writeCDATA(text.replace(badXml10, " "));
}

void XmlStreamWriter::writeCharacters(QString text)
{
  QXmlStreamWriter::writeCharacters(text.replace(badXml10, " "));
}

void XmlStreamWriter::writeTextElement(const QString& qualifiedName, QString value)
{
  QXmlStreamWriter::writeTextElement(qualifiedName, value.replace(badXml10, " "));
}

} // namespace gpsbabel
