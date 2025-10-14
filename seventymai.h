/*
    Copyright (C) 2025 Robert Lipe, robertlipe+source@gpsbabel.org

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

#ifndef SEVENTYMAI_H_INCLUDED_
#define SEVENTYMAI_H_INCLUDED_

#include <QList>                  // for QList
#include <QString>                // for QString
#include <QVector>                // for QVector

#include "defs.h"                 // for ff_cap, ff_type
#include "format.h"               // for Format
#include "src/core/textstream.h"  // for TextStream


class SeventymaiFormat : public Format
{
public:
  using Format::Format;
  /* Member Functions */
  ff_type get_type() const override
  {
    return ff_type_file;
  }

  QVector<ff_cap> get_cap() const override
  {
    return {ff_cap_none, ff_cap_read, ff_cap_none};
  }

  void rd_init(const QString& fname) override;
  void read() override;
  void rd_deinit() override;

private:
  /* Types */

  /* Constants */
  static constexpr int kColTime = 1; // time_t
  static constexpr int kColStatus = 2; // A => active
  static constexpr int kColLatitude = 3; // degrees
  static constexpr int kColLongitude = 4; // degrees
  static constexpr int kColCourse = 5; // centidegrees
  static constexpr int kColSpeed = 6; // cm/s
  static constexpr int kColUnknown1 = 7; // Accelerometer component?
  static constexpr int kColUnknown2 = 8; // Accelerometer component?
  static constexpr int kColUnknown3 = 9; // Accelerometer component?
  static constexpr int kColVideo = 10; // Video filename
  static constexpr int kColUnknown4 = 11;
  static constexpr int kColUnknown5 = 12;
  static constexpr int kColUnknown6 = 13;

  static constexpr int kNumCols = 13;  // number of columns

  /* Member Functions */

  static void add_track_head(route_head* trk, int& trk_num);

  /* Data Members */

  gpsbabel::TextStream* stream;
};
#endif // SEVENTYMAI_H_INCLUDED_
