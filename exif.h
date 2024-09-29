/*

    Support for embedded (JPEG) Exif-GPS information.

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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

 */

/*
 * Exif specifications can be found at
 * 2016, version 2.31: http://www.cipa.jp/std/documents/e/DC-008-Translation-2016-E.pdf
 * 2012, version 2.3: http://www.cipa.jp/std/documents/e/DC-008-2012_E.pdf
 * 2010, version 2.3: http://www.cipa.jp/std/documents/e/DC-008-2010_E.pdf
 * 2002, version 2.2: http://www.exif.org/Exif2-2.PDF
 * 1998, version 2.1: http://www.exif.org/Exif2-1.PDF
 *
 * TIFF specifications can be found at
 * version 6.0: https://www.itu.int/itudoc/itu-t/com16/tiff-fx/docs/tiff6.pdf
 * version 6.0: http://www.npes.org/pdf/TIFF-v6.pdf
 * version 6.0: http://www.alternatiff.com/resources/TIFF6.pdf
 */
#ifndef EXIF_H_INCLUDED_
#define EXIF_H_INCLUDED_

#include <QDate>      // for QDate
#include <QDateTime>  // for QDateTime
#include <QList>      // for QList
#include <QString>    // for QString
#include <QTime>      // for QTime
#include <QVariant>   // for QVariant
#include <QVector>    // for QVector

#include <cstdint>    // for uint32_t, uint16_t, uint8_t, int16_t, int32_t

#include "defs.h"     // for arglist_t, ff_cap, Waypoint, ARG_NOMINMAX, ARGTYPE_BOOL, ff_cap_none, ARGTYPE_INT, ARGTYPE_STRING, ff_cap_read, ff_cap_write, ff_type, ff_type_file
#include "format.h"   // for Format
#include "gbfile.h"   // for gbfile, gbsize_t


class ExifFormat : public Format
{
public:
  QVector<arglist_t>* get_args() override
  {
    return &exif_args;
  }

  ff_type get_type() const override
  {
    return ff_type_file;
  }

  QVector<ff_cap> get_cap() const override
  {
    return {
      (ff_cap)(ff_cap_read | ff_cap_write)  /* waypoints */,
      ff_cap_none       /* tracks */,
      ff_cap_none       /* routes */
    };
  }

  void rd_init(const QString& fname) override;
  void read() override;
  void rd_deinit() override;
  void wr_init(const QString& fname) override;
  void write() override;
  void wr_deinit() override;

private:
  /* Types */

  struct ExifTag {
    uint16_t id{0};           // tag that identifies the field.
    uint16_t type{0};         // field type.
    uint32_t count{0};        // number of values. Note that Count is not the total number of bytes.
    uint32_t offset{0};       // byte offset relative to beginning of TIFF file to value (only for values longer than 4 bytes).
    QVector<QVariant> data;   // value
    uint32_t size{0};         // derived size in bytes of value.

    uint32_t tag_offset {0};  // byte offset relative to beginning of TIFF file of this tag, for debug only.
    uint8_t raw[4] {0,0,0,0}; // raw value/offset data, for debug only.

    bool operator==(const ExifTag& other) const
    {
      return id == other.id;
    }

    // grow data vector, initializing any new elements to type T with value 0.
    template <typename T>
    void grow(const int new_size)
    {
      int old_size = data.size();
      if (new_size > old_size) {
        data.resize(new_size);
        for (int idx = old_size; idx < new_size; ++idx) {
          data[idx] = 0;
        }
      }
    }

    // Return data value interpreted as EXIF_TYPE_LONG.
    // This is most useful when the type is EXIF_TYPE_LONG and the count is one,
    // which occurs for multiple specific tags where we need the value.
    uint32_t toLong() const
    {
      return data.at(0).value<uint32_t>();
    }
  };

  struct ExifIfd {
    uint32_t next_ifd{0};
    uint16_t nr{0};
    uint16_t count{0};
    QList<ExifTag> tags;
  };

  struct ExifApp {
    uint16_t marker{0};
    gbsize_t len{0};
    gbfile* fcache{nullptr};
    gbfile* fexif{nullptr};
    QList<ExifIfd> ifds;
  };

  template <class T>
  class Rational
  {
  public:
    T num;
    T den;

    Rational() = default;
    Rational(T n, T d) : num{n}, den{d} {}
  };

  /* Constants */

  static constexpr uint8_t writer_gps_tag_version[4] = {2, 0, 0, 0};

  /* Member Functions */

  static void print_buff(const char* buf, int sz, const char* cmt);
  static uint16_t exif_type_size(uint16_t type);
  static QString exif_time_str(const QDateTime& time);
  static QByteArray exif_read_str(ExifTag* tag);
  static double exif_read_double(const ExifTag* tag, int index);
  static double exif_read_coord(const ExifTag* tag);
  static QTime exif_read_timestamp(const ExifTag* tag);
  static QDate exif_read_datestamp(const ExifTag* tag);
  void exif_release_apps();
  static uint32_t exif_ifd_size(ExifIfd* ifd);
  ExifApp* exif_load_apps();
#ifndef NDEBUG
  static void exif_validate_tag_structure(const ExifTag* tag);
#endif
  static ExifIfd* exif_read_ifd(ExifApp* app, uint16_t ifd_nr, gbsize_t offs, uint32_t* exif_ifd_ofs, uint32_t* gps_ifd_ofs, uint32_t* inter_ifd_ofs);
  static void exif_read_app(ExifApp* app);
  static void exif_examine_app(ExifApp* app);
  static ExifIfd* exif_find_ifd(ExifApp* app, uint16_t ifd_nr);
  static ExifTag* exif_find_tag(ExifApp* app, uint16_t ifd_nr, uint16_t tag_id);
  QDateTime exif_get_exif_time(ExifApp* app) const;
  Waypoint* exif_waypt_from_exif_app(ExifApp* app) const;
  static Rational<int> exif_dec2frac(double val, double tolerance);
  ExifTag* exif_put_value(int ifd_nr, uint16_t tag_id, uint16_t type, int count, int index, const void* data) const;
  void exif_put_double(int ifd_nr, int tag_id, int index, double val) const;
  void exif_put_str(int ifd_nr, int tag_id, const char* val) const;
  void exif_put_coord(int ifd_nr, int tag_id, double val) const;
  void exif_put_long(int ifd_nr, int tag_id, int index, int32_t val) const;
  void exif_put_short(int ifd_nr, int tag_id, int index, int16_t val) const;
  void exif_remove_tag(int ifd_nr, int tag_id) const;
  void exif_find_wpt_by_time(const Waypoint* wpt);
  void exif_find_wpt_by_name(const Waypoint* wpt);
  static bool exif_sort_tags_cb(const ExifTag& A, const ExifTag& B);
  static bool exif_sort_ifds_cb(const ExifIfd& A, const ExifIfd& B);
  static void exif_write_value(ExifTag* tag, gbfile* fout);
  static void exif_write_ifd(ExifIfd* ifd, char next, gbfile* fout);
  void exif_write_apps() const;

  /* Data Members */

  gbfile* fin_{};
  gbfile* fout_{};
  QList<ExifApp*>* exif_apps{};
  ExifApp* exif_app_{};
  const Waypoint* exif_wpt_ref{};
  QDateTime exif_time_ref;
  char exif_success{};
  QString exif_fout_name;

  char* opt_filename{};
  char* opt_overwrite{};
  char* opt_frame{};
  char* opt_name{};
  char* opt_offsettime{};

  QVector<arglist_t> exif_args = {
    { "filename", &opt_filename, "Set waypoint name to source filename", "Y", ARGTYPE_BOOL, ARG_NOMINMAX, nullptr },
    { "frame", &opt_frame, "Time-frame (in seconds)", "10", ARGTYPE_INT, "0", nullptr, nullptr },
    { "name", &opt_name, "Locate waypoint for tagging by this name", nullptr, ARGTYPE_STRING, ARG_NOMINMAX, nullptr },
    { "overwrite", &opt_overwrite, "!OVERWRITE! the original file. Default=N", "N", ARGTYPE_BOOL, ARG_NOMINMAX, nullptr },
    { "offset", &opt_offsettime, "Image Offset Time (+HH:MM or -HH:MM)", nullptr, ARGTYPE_STRING, ARG_NOMINMAX, nullptr },
  };
};
#endif // EXIF_H_INCLUDED_
