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

#ifndef XMLSTREAMWRITER_H
#define XMLSTREAMWRITER_H

#include <QtCore/QtGlobal>
#include <QtCore/QXmlStreamWriter>

class QFile;
#if (QT_VERSION < QT_VERSION_CHECK(5, 0, 0))
class QRegExp;
#else
class QRegularExpression;
#endif

namespace gpsbabel
{

class XmlStreamWriter : public QXmlStreamWriter
{
private:
#if (QT_VERSION < QT_VERSION_CHECK(5, 0, 0))
  static QRegExp badXml10;
#else
  static QRegularExpression badXml10;
#endif

public:
  XmlStreamWriter(QString* s);
  XmlStreamWriter(QFile* f);

  void writeOptionalAttribute(const QString& qualifiedName, QString value);
  void writeOptionalTextElement(const QString& qualifiedName, QString text);
  void writeAttribute(const QString& qualifiedName, QString value);
  void writeCDATA(QString text);
  void writeCharacters(QString text);
  void writeTextElement(const QString& qualifiedName, QString value);

};

} // namespace gpsbabel

#endif // XMLSTREAMWRITER_H

