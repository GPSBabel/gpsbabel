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
  qDebug().noquote() << msg;
  exit(1);
}

void
warning(const char* fmt, ...)
{
  va_list ap;
  va_start(ap, fmt);
  QString msg = QString::vasprintf(fmt, ap);
  va_end(ap);
  qDebug().noquote() << msg;
}
