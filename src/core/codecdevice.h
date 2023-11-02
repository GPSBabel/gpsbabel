/*
    Copyright (C) 2021 Robert Lipe, robertlipe+source@gpsbabel.org

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
#ifndef SRC_CORE_CODECDEVICE_H_
#define SRC_CORE_CODECDEVICE_H_

#include <QIODevice>        // for QIODevice
#include <QIODeviceBase>    // for QIODeviceBase::OpenMode
#include <QString>          // for QString
#include <QTextCodec>       // for QTextCodec
#include <QTextDecoder>     // for QTextDecoder
#include <QTextEncoder>     // for QTextEncoder
#include <QtGlobal>         // for qint64

#include "src/core/file.h"  // for File

namespace gpsbabel
{

class CodecDevice : public QIODevice
{
public:
  CodecDevice(const QString& fname, const char* module, const char* codec_name);
  ~CodecDevice();
  bool open(QIODevice::OpenMode mode) override;
  bool isSequential() const override;
  void close() override;

private:
  qint64 readData(char* data, qint64 maxlen) override;
  qint64 writeData(const char* data, qint64 len) override;

private:
  QString fname_;
  const char* module_;
  const char* codec_name_;
  gpsbabel::File* file_{nullptr};
  QTextCodec* codec_{nullptr};
  QTextDecoder* decoder_{nullptr};
  QTextEncoder* encoder_{nullptr};
  QString unicodebuffer_;
  qint64 unicodebuffer_bytes_{0};
  const char* unicodebuffer_data_{nullptr};
  static constexpr qint64 charbuffer_size_ = 1024;
  char charbuffer_[charbuffer_size_];
  char* charbuffer_data_{charbuffer_};
  qint64 charbuffer_bytes_free_{charbuffer_size_};
};

} // namespace gpsbabel
#endif // SRC_CORE_CODECDEVICE_H_
