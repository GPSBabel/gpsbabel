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

#include <cstdarg>             // for va_end, va_list, va_start
#include <cstdio>              // for fprintf, stderr, fflush
#include <cstdlib>             // for exit

#include <QDebug>              // for QDebug
#include <QMessageLogContext>  // for QtMsgType, QMessageLogContext, qFormatLogMessage
#include <QString>             // for QString
#include <QtGlobal>            // for qPrintable

#include "defs.h"              // for gbFatal, gbDebug, gbInfo, gbVLegacyLog, gbWarning
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
  auto* myinstance = new FatalMsg;
  myinstance->swap(msginstance);
  delete myinstance;
  exit(1);
}

[[noreturn]] void
gbFatal(const char* fmt, ...)
{
  va_list args;
  va_start(args, fmt);
  gbVLegacyLog(QtCriticalMsg, fmt, args);
  va_end(args);
  exit(1);
}

void
gbWarning(const char* fmt, ...)
{
  va_list args;
  va_start(args, fmt);
  gbVLegacyLog(QtWarningMsg, fmt, args);
  va_end(args);
}

void
gbInfo(const char* fmt, ...)
{
  va_list args;
  va_start(args, fmt);
  gbVLegacyLog(QtInfoMsg, fmt, args);
  va_end(args);
}

void
gbDebug(const char* fmt, ...)
{
  va_list args;
  va_start(args, fmt);
  gbVLegacyLog(QtDebugMsg, fmt, args);
  va_end(args);
}

static void LegacyLogMessageHandler(QtMsgType type, const QString& msg)
{
  static bool lineInProgress = false;

  if (lineInProgress) {
    fprintf(stderr, "%s", qPrintable(msg));
  } else {
    QString message = qFormatLogMessage(type, QMessageLogContext(), msg);
    fprintf(stderr, "%s", qPrintable(message));
  }
  fflush(stderr);
  
  lineInProgress = !msg.endsWith('\n');
}

void gbVLegacyLog(QtMsgType type, const char* fmt, va_list args)
{
  QString logString(QString::vasprintf(fmt, args));

  for (auto idx = logString.indexOf('\n'); idx >= 0; idx = logString.indexOf('\n')) {
    QString msg = logString.sliced(0, idx + 1);
    LegacyLogMessageHandler(type, msg);
    logString.remove(0, idx + 1);
  }
  if (!logString.isEmpty()) {
    LegacyLogMessageHandler(type, logString);
  }
}
