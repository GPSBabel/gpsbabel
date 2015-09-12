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
    Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111 USA

 */

#include "src/core/ziparchive.h"

#include "defs.h"
#include "src/core/logging.h"
#include "zlib/contrib/minizip/zip.h"

#include <QtCore/QFile>
#include <QtCore/QDebug>

ZipArchive::ZipArchive(QString filename)
: filename_(filename), valid_(false) {
  zipfile_ = Open(filename);
  valid_ = zipfile_ != NULL;
}

ZipArchive::~ZipArchive() {
  Close();
}

zipFile ZipArchive::Open(QString filename) {
  zipFile zipfile = NULL;
  int creat = APPEND_STATUS_CREATE;
  zipfile = zipOpen64( CSTR(filename), creat );
  return zipfile;
}

void ZipArchive::Close() {
  zipClose(zipfile_, NULL );
  zipfile_ = NULL;
  valid_ = false;
}

bool ZipArchive::Add(QString item_to_add) {
  if(!zipfile_) {
    return true;
  }

  zip_fileinfo zi = {{ 0 }};

  int err = zipOpenNewFileInZip64(zipfile_, CSTR(item_to_add), &zi,
   NULL,0,NULL,0,NULL,
   Z_DEFLATED,
   Z_DEFAULT_STRATEGY,
   Z_DEFAULT_COMPRESSION);
  if (err) {
    Fatal() << "Error adding" << item_to_add <<  "to zip file";
  }

  QFile src(item_to_add);
  if (!src.open(QIODevice::ReadOnly)) {
    Fatal() << "Error reading" << item_to_add <<  "to zip file";
  }

  // Be lazy and read the whole file back into memory (again).
  QByteArray b = src.readAll();
  if (zipWriteInFileInZip(zipfile_, b.constData(), b.size())) {
    Fatal() << "Error writing" << item_to_add << "to zip";
  }
  if (zipCloseFileInZip(zipfile_)) {
    Fatal() << "Error closing" << item_to_add << "to zip";
  }
  return false;
}

bool ZipArchive::Add(QStringList items_to_add) {
  for (int i = 0; i < items_to_add.size(); ++i) {
    if (Add(items_to_add[i]))
      return true;
  }
  return false;
}