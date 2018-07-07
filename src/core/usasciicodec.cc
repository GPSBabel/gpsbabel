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

#include "usasciicodec.h"
#include <QtCore/QByteArray>
#include <QtCore/QChar>
#include <QtCore/QLatin1Char>
#include <QtCore/QList>
#include <QtCore/QString>

// The availability of a US-ASCII codec is not promised by QTextCodec.
// On Mac OS X is is likely one will not be available.
// We provide our own to guarantee we have one.

namespace gpsbabel
{

QByteArray UsAsciiCodec::name() const
{
  return "US-ASCII";
}

QList<QByteArray> UsAsciiCodec::aliases() const
{
  QList<QByteArray> list;
  list += "iso-ir-6";
  list += "ANSI_X3.4-1968";
  list += "ANSI_X3.4-1986";
  list += "ISO_646.irv:1991";
  list += "ISO646-US";
  list += "us";
  list += "IBM367";
  list += "cp367";
  list += "csASCII";
  return list;
}

int UsAsciiCodec::mibEnum() const
{
  return 3;
}

QString UsAsciiCodec::convertToUnicode(const char* chars, int len, ConverterState* state) const
{
  QString result(len, Qt::Uninitialized);
  QChar* uc = result.data();
  const unsigned char* c = (const unsigned char*)chars;
  int invalid = 0;

  for (int i = 0; i < len; i++) {
    if (c[i] < 128) {
      uc[i] = QLatin1Char(c[i]);
    } else {
      uc[i] = QChar::ReplacementCharacter;
      ++invalid;
    }
  }

  if (state) {
    state->invalidChars += invalid;
    state->remainingChars = 0;
  }
  return result;
}

QByteArray UsAsciiCodec::convertFromUnicode(const QChar* uc, int len, ConverterState* state) const
{
  QByteArray result(len, Qt::Uninitialized);
  unsigned char* c = (unsigned char*)result.data();
  const char replacement = (state && state->flags & ConvertInvalidToNull) ? 0 : '?';
  int invalid = 0;

  for (int i = 0; i < len; i++) {
    int u = uc[i].unicode();
    if (u < 128) {
      c[i] = (char) u;
    } else {
      c[i] = replacement;
      ++invalid;
    }
  }

  if (state) {
    state->invalidChars += invalid;
    state->remainingChars = 0;
  }
  return result;
}

} // namespace gpsbabel
