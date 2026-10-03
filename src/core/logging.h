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

#include <cstdlib>               // for exit
#ifndef MOCK_FORMAT
#include <format>                // for format, format_string
#else
#include <fmt/format.h>
#endif
#include <string>                // for basic_string, string
#include <type_traits>           // for type_identity
#include <utility>               // for forward

#include <QBasicUtf8StringView>  // for QBasicUtf8StringView
#include <QDebug>                // for QDebug
#include <QMessageLogger>        // for QtMsgType, qCritical, qInfo, qWarning
#include <QUtf8StringView>       // for QUtf8StringView
#include <QtGlobal>              // for qDebug

#include "defs.h"                // for format


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

/* std::format_string interface.
 * A newline is added by the MessageHandler.
 */

[[noreturn]] inline void gbLogFatal(const char* message)
{
    qCritical().noquote() << message;
    exit(1);
}

[[noreturn]] inline void gbLogFatal(const std::string& message)
{
    qCritical().noquote() << QUtf8StringView(message);
    exit(1);
}

template <typename... Args>
[[noreturn]] void gbLogFatal(gpsbabel::format_string<Args...> fmt, Args&&... args)
{
    qCritical().noquote() << QUtf8StringView(gpsbabel::format(fmt, std::forward<Args>(args)...));
    exit(1);
}

inline void gbLogDebug(const char* message)
{
    qDebug().noquote() << message;
}

inline void gbLogDebug(const std::string& message)
{
    qDebug().noquote() << QUtf8StringView(message);
}

template <typename... Args>
void gbLogDebug(gpsbabel::format_string<Args...> fmt, Args&&... args)
{
    qDebug().noquote() << QUtf8StringView(gpsbabel::format(fmt, std::forward<Args>(args)...));
}

inline void gbLogWarning(const char* message)
{
    qWarning().noquote() << message;
}

inline void gbLogWarning(const std::string& message)
{
    qWarning().noquote() << QUtf8StringView(message);
}

template <typename... Args>
void gbLogWarning(gpsbabel::format_string<Args...> fmt, Args&&... args)
{
    qWarning().noquote() << QUtf8StringView(gpsbabel::format(fmt, std::forward<Args>(args)...));
}

inline void gbLogInfo(const char* message)
{
    qInfo().noquote() << message;
}

inline void gbLogInfo(const std::string& message)
{
    qInfo().noquote() << QUtf8StringView(message);
}

template <typename... Args>
void gbLogInfo(gpsbabel::format_string<Args...> fmt, Args&&... args)
{
    qInfo().noquote() << QUtf8StringView(gpsbabel::format(fmt, std::forward<Args>(args)...));
}

#endif //  SRC_CORE_LOGGING_H_
