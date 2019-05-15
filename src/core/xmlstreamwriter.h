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

#ifndef XMLSTREAMWRITER_H
#define XMLSTREAMWRITER_H

#include <QtCore/QTextCodec>
#include <QtCore/QXmlStreamWriter>

class QFile;

namespace gpsbabel
{

// From the "vendor" range, see:
// https://www.iana.org/assignments/character-sets/character-sets.xhtml
const int UTF8_FOR_XML_MIB = 2000;

class XmlTextCodec : public QTextCodec
{
private:
  QTextCodec* utf8Codec;
public:
  XmlTextCodec();
  static XmlTextCodec *instance;
  QByteArray name() const override;
  int mibEnum() const override;
protected:
  QByteArray convertFromUnicode(const QChar* chars, int len, QTextCodec::ConverterState* state) const override;
  QString convertToUnicode(const char* chars, int len, QTextCodec::ConverterState* state) const override;
};

class XmlStreamWriter : public QXmlStreamWriter
{
public:
  explicit XmlStreamWriter(QString* string);
  explicit XmlStreamWriter(QFile* f);

  void writeStartDocument();
  void writeOptionalTextElement(const QString& qualifiedName, const QString& text);
};

} // namespace gpsbabel

#endif // XMLSTREAMWRITER_H

