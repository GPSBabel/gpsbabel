/*
    Write points to SubRip subtitle file (for video geotagging)

    Copyright (C) 2010 Michael von Glasow, michael @t vonglasow d.t com
    Copyright (C) 2014 Gleb Smirnoff, glebius @t FreeBSD d.t org

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
#ifndef SUBRIP_H_INCLUDED_
#define SUBRIP_H_INCLUDED_

#include <QtCore/QDateTime>  // for QDateTime, operator<<
#include <QtCore/QString>    // for QString
#include <QtCore/QTime>      // for QTime
#include <QtCore/QVector>    // for QVector

#include "defs.h"
#include "format.h"          // for Format
#include "gbfile.h"          // for gbfprintf, gbfclose, gbfopen, gbfwrite, gbfile


class SubripFormat : public Format
{
public:
  QVector<arglist_t>* get_args() override
  {
    return &subrip_args;
  }

  ff_type get_type() const override
  {
    return ff_type_file;
  }

  QVector<ff_cap> get_cap() const override
  {
    return { ff_cap_none, ff_cap_write, ff_cap_none }; // waypoints, track, route; for now, we just do tracks
  }

  QString get_encode() const override
  {
    return CET_CHARSET_ASCII;
  }

  int get_fixed_encode() const override
  {
    return 0;
  }

  void wr_init(const QString& fname) override;
  void write() override;
  void wr_deinit() override;

private:
  /* Member Functions */

  QTime video_time(const QDateTime& dt) const;
  void subrip_prevwp_pr(const Waypoint* waypointp);
  void subrip_trkpt_pr(const Waypoint* waypointp);

  /* Data Members */

  char* opt_videotime{nullptr};
  char* opt_gpstime{nullptr};
  char* opt_gpsdate{nullptr};
  char* opt_format{nullptr};
  QDateTime gps_datetime;    // Date time corresponding to video video_offset_ms
  QDateTime video_datetime;  // Date time corresponding to video time 00:00:00,000.
  int video_offset_ms{0};
  int stnum{1};
  gbfile* fout{nullptr};
  const Waypoint* prevwpp{nullptr};
  double vspeed{0.0};
  double gradient{0.0};

  QVector<arglist_t> subrip_args = {
    {"video_time", &opt_videotime, "Video position for which exact GPS time is known (hhmmss[.sss], default is 00:00:00,000)", nullptr, ARGTYPE_STRING, ARG_NOMINMAX, nullptr },
    {"gps_time", &opt_gpstime, "GPS time at position video_time (hhmmss[.sss], default is first timestamp of track)", nullptr, ARGTYPE_STRING, ARG_NOMINMAX, nullptr },
    {"gps_date", &opt_gpsdate, "GPS date at position video_time (yyyymmdd, default is first timestamp of track)", nullptr, ARGTYPE_STRING, ARG_NOMINMAX, nullptr },
    {"format", &opt_format, "Format for subtitles", "%s km/h %e m\\n%t %l", ARGTYPE_STRING, ARG_NOMINMAX, nullptr },
  };
};
#endif // SUBRIP_H_INCLUDED_
