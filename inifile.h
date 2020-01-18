/*
    Library for inifile like data files.

    Copyright (C) 2006 Olaf Klein, o.b.klein@gpsbabel.org

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

#ifndef HAVE_INIFILE_H
#define HAVE_INIFILE_H

#include <QtCore/QHash>    // for QHash
#include <QtCore/QList>    // for QList
#include <QtCore/QString>  // for QString

class InifileSection;
struct inifile_t {
  QHash<QString, InifileSection> sections;
  QString source;
};

/*
	inifile_init:
	  reads inifile filename into memory
	  myname represents the calling module
 */
inifile_t* inifile_init(const QString& filename, const char* myname);
void inifile_done(inifile_t* inifile);

bool inifile_has_section(const inifile_t* inifile, const QString& section);

/*
     inifile_readstr:
       returns a null QString if not found, otherwise a non-null but possibly
       empty Qstring with the value of key ...
 */
QString inifile_readstr(const inifile_t* inifile, const QString& section, const QString& key);

/*
     inifile_readint:
       on success the value is stored into "*value" and "inifile_readint" returns 1,
       otherwise inifile_readint returns 0
 */
int inifile_readint(const inifile_t* inifile, const QString& section, const QString& key, int* value);

/*
     inifile_readint_def:
       if found inifile_readint_def returns value of key, otherwise a default value "def"
 */
int inifile_readint_def(const inifile_t* inifile, const QString& section, const QString& key, int def);

#endif
