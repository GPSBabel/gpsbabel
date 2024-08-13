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

#include <cassert>     // for assert
#include <cstring>     // for memcpy
#include <algorithm>   // for min

#include <QByteArray>  // for QByteArray
#include <QChar>       // for QChar
#include <QFlags>      // for QFlags

#include "defs.h"      // for list_codecs, warning
#include "codecdevice.h"

namespace gpsbabel
{

CodecDevice::CodecDevice(const QString& fname, const char* module, const char* codec_name) :
  fname_(fname), module_(module), codec_name_(codec_name)
{
}

CodecDevice::~CodecDevice()
{
//  close();
}

bool CodecDevice::open(QIODevice::OpenMode mode)
{
  codec_ = QTextCodec::codecForName(codec_name_);
  if (codec_ == nullptr) {
    list_codecs();
    fatal("%s: Unsupported codec '%s'.\n", module_, codec_name_);
    // return false;
  }

  if (mode & QIODevice::ReadOnly) {
    decoder_ = codec_->makeDecoder();
  }

  if (mode & QIODevice::WriteOnly) {
    encoder_ = codec_->makeEncoder();
  }

  file_ = new gpsbabel::File(fname_);
  bool status = file_->open(mode);

  QIODevice::open(mode);

  return status;

}

qint64 CodecDevice::readData(char* data, qint64 maxlen)
{
  qint64 bytesdelivered = 0;

  while (bytesdelivered < maxlen) {
    if (unicodebuffer_bytes_ > 0) {
      qint64 bytes = std::min(maxlen, unicodebuffer_bytes_);
      memcpy(data, unicodebuffer_data_, bytes);
      unicodebuffer_bytes_ -= bytes;
      unicodebuffer_data_ += bytes;
      data += bytes;
      bytesdelivered += bytes;
      if (bytesdelivered == maxlen) {
        break;
      }
    }

    qint64 bytesread = file_->read(charbuffer_, charbuffer_size_);
    if (bytesread <= 0) { // no more data is available or error.
      if (bytesdelivered > 0) {
        break;
      }
      return -1;
    }

    unicodebuffer_ = decoder_->toUnicode(charbuffer_, bytesread);
    unicodebuffer_bytes_ = unicodebuffer_.size() * sizeof(QChar);
    unicodebuffer_data_ = reinterpret_cast<const char*>(unicodebuffer_.constData());
  }
  return bytesdelivered;
}

qint64 CodecDevice::writeData(const char* data, qint64 len)
{
  qint64 bytes_consumed = 0;

  while (bytes_consumed < len) {
    qint64 bytes= std::min(len - bytes_consumed, charbuffer_bytes_free_);
    memcpy(charbuffer_data_, data, bytes);
    bytes_consumed += bytes;
    charbuffer_data_ += bytes;
    charbuffer_bytes_free_ -= bytes;
    data += bytes;

    if (charbuffer_bytes_free_ == 0) {
      static_assert(charbuffer_size_%sizeof(QChar) == 0);
      QByteArray ba = encoder_->fromUnicode(reinterpret_cast<const QChar*>(charbuffer_), charbuffer_size_/sizeof(QChar));
      file_->write(ba);
      charbuffer_data_ = charbuffer_;
      charbuffer_bytes_free_ = charbuffer_size_;
    }
  }
  return len;
}

void CodecDevice::close()
{
  if (charbuffer_bytes_free_ < charbuffer_size_) {
    qint64 bytes = charbuffer_size_ - charbuffer_bytes_free_;
    assert(bytes%sizeof(QChar) == 0);
    QByteArray ba = encoder_->fromUnicode(reinterpret_cast<const QChar*>(charbuffer_), bytes/sizeof(QChar));
    file_->write(ba);
    charbuffer_data_ = charbuffer_;
    charbuffer_bytes_free_ = charbuffer_size_;
  }
  file_->close();
  QIODevice::close();

  if (file_ != nullptr) {
    delete file_;
    file_ = nullptr;
  }
  if (encoder_ != nullptr) {
    delete encoder_;
    encoder_ = nullptr;
  }
  if (decoder_ != nullptr) {
    delete decoder_;
    decoder_ = nullptr;
  }
}

bool CodecDevice::isSequential() const
{
  return true;
}

} // namespace gpsbabel
