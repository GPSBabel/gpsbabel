/*
    Copyright (C) 2014 Robert Lipe, robertlipe+source@gpsbabel.org

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
#ifndef gpsbabel_logging_h_included
#define gpsbabel_logging_h_included

// A wrapper for QDebug that provides a sensible Warning() and FatalMsg()
// with convenient functions, stream operators and manipulators.

#include <QDebug>            // for QDebug
#include <QtGlobal>          // for QtCriticalMsg, QtWarningMsg


class Warning : public QDebug
{
public:
  explicit Warning() : QDebug(QtWarningMsg) {}
};

/*
 * To use a FatalMsg pass it to fatal(), e.g.
 * fatal(FatalMsg() << "bye bye");
 *
 * This
 * 1) allows the noreturn attribute on fatal to be use by analysis
 *    tools such as cppcheck.
 * 2) allows fatal to throw an exception instead of calling exit.
 *    This could be caught by main for a cleaner exit from a fatal error.
 */
class FatalMsg : public QDebug
{
public:
  // We don't use QtFatalMsg here because we don't want the destructor to call abort.
  explicit FatalMsg() : QDebug(QtCriticalMsg) {}
};

class DebugIndent
{
public:
  explicit DebugIndent(int l) : level(l) {}

  int level;
};

QDebug operator<< (QDebug debug, const DebugIndent& indent);

class Debug : public QDebug
{
public:
  Debug() : QDebug(QtDebugMsg) {nospace().noquote();}
  explicit Debug(int l) : QDebug(QtDebugMsg) {nospace().noquote() << DebugIndent(l);}
};

/*
 * Kludge any used QTextStream modifiers into Qt namespace as they are in newer
 * versions of Qt.  This makes source compatiblity easier.
 */
#if (QT_VERSION < QT_VERSION_CHECK(5, 14, 0))
namespace Qt
{
  inline QTextStream& dec(QTextStream &s) { return ::dec(s); }
  inline QTextStream& hex(QTextStream &s) { return ::hex(s); }
  inline QTextStream& endl(QTextStream &s) { return ::endl(s); }
  inline QTextStream& uppercasedigits(QTextStream &s) { return ::uppercasedigits(s); }
}
#endif
#endif //  gpsbabel_logging_h_included
