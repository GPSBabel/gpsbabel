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

#include <QByteArray>          // for QByteArray
#include <QDebug>              // for QDebug
#include <QtGlobal>            // for qCritical, qDebug, qInfo, qWarning

#include "defs.h"              // for DebugLog, fatal, debug, info, warning
#include "src/core/logging.h"  // for FatalMsg

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

[[noreturn]] void fatal(QDebug& msginstance)
{
  auto* myinstance = new FatalMsg;
  myinstance->swap(msginstance);
  delete myinstance;
  exit(1);
}

[[noreturn]] void
fatal(const char* fmt, ...)
{
  va_list args;
  va_start(args, fmt);
  QByteArray msg = xvasprintf(fmt, args);
  va_end(args);
  if (msg.endsWith('\n')) {
    msg.chop(1);
  }
  qCritical().noquote() << msg;
  exit(1);
}

void
warning(const char* fmt, ...)
{
  va_list args;
  va_start(args, fmt);
  QByteArray msg = xvasprintf(fmt, args);
  va_end(args);
  if (msg.endsWith('\n')) {
    msg.chop(1);
  }
  qWarning().noquote() << msg;
}

void
info(const char* fmt, ...)
{
  va_list args;
  va_start(args, fmt);
  QByteArray msg = xvasprintf(fmt, args);
  va_end(args);
  if (msg.endsWith('\n')) {
    msg.chop(1);
  }
  qInfo().noquote() << msg;
}

void
debug(const char* fmt, ...)
{
  va_list args;
  va_start(args, fmt);
  QByteArray msg = xvasprintf(fmt, args);
  va_end(args);
  if (msg.endsWith('\n')) {
    msg.chop(1);
  }
  qDebug().noquote() << msg;
}

int DebugLog::vlog(const char* fmt, va_list args)
{
  int rc = 0;

  buf_.append(xvasprintf(fmt, args));

  for (auto idx = buf_.indexOf('\n'); idx >= 0; idx = buf_.indexOf('\n')) {
    QByteArray msg = buf_.sliced(0, idx + 1);
    if (msg.endsWith('\n')) {
      msg.chop(1);
    }
    qDebug().noquote() << msg;
    rc += msg.size();
    buf_.remove(0, idx + 1);
  }

  return rc;
}

int DebugLog::log(const char* fmt, ...)
{
  va_list args;
  va_start(args, fmt);
  int rc = vlog(fmt, args);
  va_end(args);
  return rc;
}

int DebugLog::flush()
{
  int rc = 0;

  if (!buf_.isEmpty()) {
    if (buf_.endsWith('\n')) {
      buf_.chop(1);
    }
    qDebug().noquote() << buf_;
    rc += buf_.size();
    buf_ = QByteArray();
  }

  return rc;
}
