/*
    Copyright (C) 2019-2021 Robert Lipe, gpsbabel.org

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


#include <QtGlobal>          // for qint64

#include <QByteArrayView>    // for QByteArrayView
#include <QFile>             // for QFile
#include <QFlags>            // for QFlags
#include <QIODevice>         // for QIODevice
#include <QIODeviceBase>           // for QIODeviceBase::OpenMode
#include <QStringConverter>  // for QStringConverter, QStringConverter::Utf8, QStringConverter::Encoding, QStringConverter::Utf16

#include <optional>          // for optional

#include "defs.h"            // for gbFatal, list_codecs
#include "src/core/textstream.h"
#include "src/core/file.h"   // for File


namespace gpsbabel
{

void TextStream::open(const QString& fname, QIODevice::OpenMode mode, const char* codec_name)
{
  std::optional<QStringConverter::Encoding> encoding = QStringConverter::encodingForName(codec_name);
  bool use_stringconverter = encoding.has_value();

  /* When reading autodetect unicode.
   * The requested codec may not be supported by QStringConverter,
   * but autodetection may switch to a converter that is.
   */
  if (!use_stringconverter && (mode & QFile::ReadOnly)) {
    auto scanfile = gpsbabel::File(fname);
    scanfile.open(mode);
    char data[4];
    qint64 bytesread = scanfile.read(data, 4);
    scanfile.close();
    encoding = QStringConverter::encodingForData(QByteArrayView(data, bytesread));
    if (encoding.has_value()) {
      use_stringconverter = true;
    }
  }

  if (use_stringconverter) {
    file_ = new gpsbabel::File(fname);
    file_->open(mode);
    setDevice(file_);
    setEncoding(encoding.value());

    if (mode & QFile::ReadOnly) {
      if (encoding.value() == QStringConverter::Utf8) {
        setAutoDetectUnicode(true);
      }
    }

    if (mode & QFile::WriteOnly) {
      // enable bom for all UTF codecs except UTF-8
      if (encoding.value() != QStringConverter::Utf8) {
        setGenerateByteOrderMark(true);
      }
    }
  } else {
    device_ = new gpsbabel::CodecDevice(fname, codec_name);
    bool status = device_->open(mode);
    if (!status) {
      gbFatal("device not open %d\n", status);
    }
    setDevice(device_);
    setEncoding(QStringConverter::Utf16);
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
  if (device_ != nullptr) {
    device_->close();
    delete device_;
    device_ = nullptr;
  }
}

} // namespace gpsbabel
