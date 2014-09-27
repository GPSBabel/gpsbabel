/*
    Copyright (C) 2014 Robert Lipe, gpsbabel.org

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

#ifndef QUSASCIICODEC_H
#define QUSASCIICODEC_H

#include <QtCore/QTextCodec>

namespace gpsbabel
{

class UsAsciiCodec : public QTextCodec
{
public:
  QByteArray name() const;
  QList<QByteArray> aliases() const;
  int mibEnum() const;
  QString convertToUnicode(const char*, int, ConverterState*) const;
  QByteArray convertFromUnicode(const QChar*, int, ConverterState*) const;
};

} // namespace gpsbabel
#endif // QUSASCIICODEC_H
