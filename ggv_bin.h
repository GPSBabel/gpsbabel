/*

    Handle Geogrid-Viewer binary overlay file format (.ovl)

    Copyright (C) 2016-2020 Ralf Horstmann <ralf@ackstorm.de>
    Copyright (C) 2016-2020 Robert Lipe, robertlipe+source@gpsbabel.org

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
#ifndef GGV_BIN_H_INCLUDED_
#define GGV_BIN_H_INCLUDED_

#include "defs.h"
#include "format.h"


class GgvBinFormat : public Format
{
public:
  ff_type get_type() const override
  {
    return ff_type_file;
  }

  QVector<ff_cap> get_cap() const override
  {
    return {
      ff_cap_none,  // waypoints
      ff_cap_read,  // tracks
      ff_cap_none   // routes
    };
  }

  QString get_encode() const override
  {
    return CET_CHARSET_ASCII;
  }

  int get_fixed_encode() const override
  {
    return 0;
  }

  void rd_init(const QString& fname) override;
  void read() override;
  void rd_deinit() override;

private:
  static void ggv_bin_read_bytes(QDataStream& stream, QByteArray& buf, int len, const char* descr = nullptr);
  static quint16 ggv_bin_read16(QDataStream& stream, const char* descr = nullptr);
  static quint32 ggv_bin_read32(QDataStream& stream, const char* descr = nullptr);
  static void ggv_bin_read_text16(QDataStream& stream, QByteArray& buf, const char* descr = nullptr);
  static void ggv_bin_read_text32(QDataStream& stream, QByteArray& buf, const char* descr = nullptr);
  static double ggv_bin_read_double(QDataStream& stream, const char* descr = nullptr);
  static void ggv_bin_read_v2(QDataStream& stream);
  static void ggv_bin_read_v34_header(QDataStream& stream, quint32& number_labels, quint32 &number_records);
  static void ggv_bin_read_v34_label(QDataStream& stream);
  static QString ggv_bin_read_v34_common(QDataStream& stream);
  static void ggv_bin_read_v34_record(QDataStream& stream);
  static void ggv_bin_read_v34(QDataStream& stream);

  QString read_fname;

};

#endif
