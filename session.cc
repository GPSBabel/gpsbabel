/*

    GPSBabel session (format session) management
    Copyright (C) 2008 Olaf Klein, o.b.klein@gpsbabel.org

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

#include "defs.h"
#include "session.h"

#include <QtCore/QList>  // for QList

static QList<session_t> session_list;

void
session_init()
{
  session_list.clear();
}

void
session_exit()
{
  session_list.clear();
}

void
start_session(const QString& name, const QString& filename)
{
  session_list.append(session_t(name, filename));
}

const session_t*
curr_session()
{
  if (!session_list.isEmpty()) {
    return &session_list.last();
  } else {
    fatal("Attempt to fetch session outside of session range.");
  }
}

/* non public functions */

