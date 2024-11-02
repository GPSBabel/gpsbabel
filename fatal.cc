/*
    Functions to indicate inconsistent or fatal conditions.

    Copyright (C) 2002-2014 Robert Lipe, robertlipe+source@gpsbabel.org

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
#include <cstdio>              // for fflush, stdout
#include <cstdlib>             // for exit

#include <QDebug>              // for QDebug
#include <QString>             // for QString
#include <QtGlobal>            // for qDebug

#include "defs.h"              // for Fatal, debug_print, fatal, warning
#include "src/core/logging.h"  // for FatalMsg


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
  /* flush any buffered standard output */
  fflush(stdout);

  va_list ap;
  va_start(ap, fmt);
  QString msg = QString::vasprintf(fmt, ap);
  va_end(ap);
  qCritical().noquote() << msg;
  exit(1);
}

void
warning(const char* fmt, ...)
{
  va_list ap;
  va_start(ap, fmt);
  QString msg = QString::vasprintf(fmt, ap);
  va_end(ap);
  qWarning().noquote() << msg;
}

void
info(const char* fmt, ...)
{
  va_list ap;
  va_start(ap, fmt);
  QString msg = QString::vasprintf(fmt, ap);
  va_end(ap);
  qInfo().noquote() << msg;
}

void
debug(const char* fmt, ...)
{
  va_list ap;
  va_start(ap, fmt);
  QString msg = QString::vasprintf(fmt, ap);
  va_end(ap);
  qDebug().noquote() << msg;
}

int DebugLog::log(const char* fmt, ...)
{
  va_list args1;
  va_start(args1, fmt);
  va_list args2;
  va_copy(args2, args1);
  char cbuf[1 + vsnprintf(nullptr, 0, fmt, args1)];
  va_end(args1);
  vsnprintf(cbuf, sizeof cbuf, fmt, args2);
  va_end(args2);

  buf_.append(QString::asprintf("%s", cbuf));

  int rc = 0;

  for (auto idx = buf_.indexOf('\n'); idx >= 0; idx = buf_.indexOf('\n')) {
    auto msg = buf_.sliced(0, idx + 1).toLocal8Bit();
    debug("%s", msg.constData());
    rc += msg.size();
    buf_.remove(0, idx + 1);
  }

  return rc;
}

int DebugLog::flush()
{
  int rc = 0;

  if (!buf_.isEmpty()) {
    auto msg = buf_.toLocal8Bit();
    debug("%s", msg.constData());
    rc += msg.size();
    buf_ = QString();
  }

  return rc;
}
