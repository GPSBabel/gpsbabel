/*
    Copyright (C) 2019 Robert Lipe, gpsbabel.org

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

#include <QtCore/QFile>        // for QFile

#include "src/core/textstream.h"
#include "defs.h"              // for fatal
#include "src/core/file.h"     // for File


namespace gpsbabel
{

void TextStream::open(const QString& fname, QIODevice::OpenModeFlag mode, const char* module, const char* codec_name)
{
  codec_ = QTextCodec::codecForName(codec_name);
  if (codec_ == nullptr) {
    list_codecs();
    fatal("%s: Unsupported codec '%s'.\n", module, codec_name);
  }

  file_ = new gpsbabel::File(fname);
  file_->open(mode);
  setDevice(file_);
  setCodec(codec_);

  if (mode & QFile::ReadOnly) {
    if (codec_->mibEnum() == 106) { // UTF-8
      setAutoDetectUnicode(true);
    }
  }

  if (mode & QFile::WriteOnly) {
    // enable bom for all UTF codecs except UTF-8
    if (codec_->mibEnum() != 106) {
      setGenerateByteOrderMark(true);
    }
  }
}

void TextStream::close()
{
  flush();
  if (file_ != nullptr) {
    file_->close();
    delete file_;
    file_ = nullptr;
  }
  codec_ = nullptr;
}

}; // namespace
