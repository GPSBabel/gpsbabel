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
#ifndef SRC_CORE_LOGGING_H_
#define SRC_CORE_LOGGING_H_

// A wrapper for QDebug that provides a sensible Warning() and FatalMsg()
// with convenient functions, stream operators and manipulators.

#include "defs.h"

#include <QDebug>            // for QDebug
#include <QtGlobal>          // for QtCriticalMsg, QtWarningMsg


class Warning : public QDebug
{
public:
  explicit Warning() : QDebug(QtWarningMsg) {}
};

/*
 * To use a FatalMsg pass it to gbFatal(), e.g.
 * gbFatal(FatalMsg() << "bye bye");
 *
 * This
 * 1) allows the noreturn attribute on gbFatal to be use by analysis
 *    tools such as cppcheck.
 * 2) allows gbFatal to throw an exception instead of calling exit.
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
  explicit DebugIndent(int level) : level_(level) {}
  friend QDebug& operator<<(QDebug& debug, const DebugIndent& indent);

private:
  int level_;
};

QDebug& operator<< (QDebug& debug, const DebugIndent& indent);

class Debug : public QDebug
{
public:
  Debug() : QDebug(QtDebugMsg) {nospace().noquote();}
  explicit Debug(int level) : QDebug(QtDebugMsg) {nospace().noquote() << DebugIndent(level);}
};

class ConditionalDebug {
public:
  explicit ConditionalDebug(int level) : enabled_(level <= global_opts.debug_level) {
    if (enabled_) {
      debug_ = new QDebug(QtDebugMsg);
      debug_->nospace().noquote();
    } else {
      debug_ = nullptr;
    }
  }

  ~ConditionalDebug() {
    delete debug_;
  }

  template<typename T>
  ConditionalDebug& operator<<(const T& value) {
    if (debug_) {
      *debug_ << value;
    }
    return *this;
  }

private:
  bool enabled_;
  QDebug* debug_;
};

inline ConditionalDebug gbDebug(int level) {
  return ConditionalDebug(level);
}

// gdDebug(foo) <<  blah; only blogs if global_opts.debug_level >= foo.
// ConditionalDebug gbDebug(int level);

#endif //  SRC_CORE_LOGGING_H_
