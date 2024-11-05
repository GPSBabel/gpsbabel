/*
    Functions to indicate inconsistent or fatal conditions.

    Copyright (C) 2002-2014,2024 Robert Lipe, robertlipe+source@gpsbabel.org

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

#include <cstdarg>             // for va_copy, va_end, va_list, va_start
#include <cstdio>              // for vsnprintf
#include <cstdlib>             // for exit

#include <QDebug>              // for QDebug
#include <QString>             // for QString
#include <QtGlobal>            // for qCritical, qDebug, qInfo, qWarning

#include "defs.h"              // for DebugLog, gbFatal, gbDebug, gbInfo, gbWarning
#include "src/core/logging.h"  // for FatalMsg

#ifdef PIGS_FLY
static QByteArray xvasprintf(const char* fmt, va_list args)
{
  va_list args2;
  va_copy(args2, args);
  auto cbufsz = 1 + vsnprintf(nullptr, 0, fmt, args);
  char* cbuf = new char[cbufsz];
  vsnprintf(cbuf, cbufsz, fmt, args2);
  va_end(args2);
  QByteArray rval(cbuf);
  delete[] cbuf;
  return rval;
}
#endif

[[noreturn]] void gbFatal(QDebug& msginstance)
{
  gbFlush(QtDebugMsg);
  gbFlush(QtInfoMsg);
  gbFlush(QtWarningMsg);

  auto* myinstance = new FatalMsg;
  myinstance->swap(msginstance);
  delete myinstance;
  exit(1);
}

[[noreturn]] void
gbFatal(const char* fmt, ...)
{
  gbFlush(QtDebugMsg);
  gbFlush(QtInfoMsg);
  gbFlush(QtWarningMsg);

  va_list args;
  va_start(args, fmt);
  gbVLog(QtCriticalMsg, fmt, args);
  gbFlush(QtCriticalMsg);
  va_end(args);
  exit(1);
}

void
gbWarning(const char* fmt, ...)
{
  va_list args;
  va_start(args, fmt);
  gbVLog(QtWarningMsg, fmt, args);
  va_end(args);
}

void
gbInfo(const char* fmt, ...)
{
  va_list args;
  va_start(args, fmt);
  gbVLog(QtInfoMsg, fmt, args);
  va_end(args);
}

void
gbDebug(const char* fmt, ...)
{
  va_list args;
  va_start(args, fmt);
  gbVLog(QtDebugMsg, fmt, args);
  va_end(args);
}

static QString gbDebugLogString_;
static QString gbInfoLogString_;
static QString gbWarningLogString_;
static QString gbCriticalLogString_;

static QString& getLogString(QtMsgType type)
{
  switch (type)
  {
  case QtDebugMsg:
    return gbDebugLogString_;
    break;
  case QtInfoMsg:
    return gbInfoLogString_;
    break;
  case QtWarningMsg:
    return gbWarningLogString_;
    break;
  case QtCriticalMsg:
  case QtFatalMsg:
  default:
    return gbCriticalLogString_;
    break;
  }
}

static void sendLogMsg(QtMsgType type, const QString& msg)
{
  switch (type)
  {
  case QtDebugMsg:
    qDebug().noquote() << msg;
    break;
  case QtInfoMsg:
    qInfo().noquote() << msg;
    break;
  case QtWarningMsg:
    qWarning().noquote() << msg;
    break;
  case QtCriticalMsg:
  case QtFatalMsg:
  default:
    qCritical().noquote() << msg;
    break;
  }
}

void gbVLog(QtMsgType type, const char* fmt, va_list args)
{
  QString& logString = getLogString(type);

  logString.append(QString::vasprintf(fmt, args));

  for (auto idx = logString.indexOf('\n'); idx >= 0; idx = logString.indexOf('\n')) {
    QString msg = logString.sliced(0, idx);
    sendLogMsg(type, msg);
    logString.remove(0, idx + 1);
  }
}

void gbFlush(QtMsgType type)
{
  QString& logString = getLogString(type);

  if (!logString.isEmpty()) {
    if (logString.endsWith('\n')) {
      logString.chop(1);
    }
    sendLogMsg(type, logString);
    logString = QString();
  }
}
