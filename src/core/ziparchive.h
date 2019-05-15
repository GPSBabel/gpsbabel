/*
    Create .zip archives.

    Copyright (C) 2015 Robert Lipe, gpsbabel.org

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

#include <QtCore/QString>
#include <QtCore/QStringList>
#include "defs.h"
#ifdef HAVE_LIBMINIZIP
#include <minizip/zip.h>
#else
#include "zlib/contrib/minizip/zip.h"
#endif

class  ZipArchive
{
 public:
  ZipArchive(QString zipfile);
 ~ZipArchive();

 void Close();
 zipFile Open(QString zipfilename);
 bool Add(QString item_to_add);
 bool Add(QStringList items_to_add);

private:
  QString filename_;
  bool valid_;
  zipFile zipfile_;
};
