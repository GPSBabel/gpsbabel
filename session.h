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

#ifndef SESSION_H_INCLUDED_
#define SESSION_H_INCLUDED_

#include <QtCore/QString>  // for QString
#include <utility>

struct session_t {
public:
  QString name;					/* in normal case the name of a format */
  QString filename;			/* used file within format */

public:
  session_t(QString name_p, QString filename_p) : name{std::move(name_p)},filename{std::move(filename_p)} {}
};

void session_init();
void session_exit();

void start_session(const QString& name, const QString& filename);
const session_t* curr_session();

#endif  // SESSION_H_INCLUDED_
