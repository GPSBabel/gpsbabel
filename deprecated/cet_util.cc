/*

    Character encoding transformation - utilities

    Copyright (C) 2005-2008 Olaf Klein

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

#include <QByteArray>            // for QByteArray
#include <QDebug>                // for QDebug
#include <QTextCodec>            // for QTextCodec

#include "defs.h"
#include "cet_util.h"
#include "src/core/logging.h"    // for Fatal


#define MYNAME "cet_util"

/* gpsbabel additions */

void
cet_convert_deinit()
{
  global_opts.codec = nullptr;
}

void
cet_convert_init(const QString& cs_name, const int force)
{
  if ((force != 0) || (global_opts.codec == nullptr)) {
    cet_convert_deinit();
    if (cs_name.isEmpty()) {	/* set default us-ascii */
      global_opts.codec = QTextCodec::codecForName(CET_CHARSET_ASCII);
    } else {
      global_opts.codec = QTextCodec::codecForName(CSTR(cs_name));
    }
    if (!global_opts.codec) {
      fatal(FatalMsg() << "Unsupported character set \"" << cs_name << ".");
    }
  }
}
