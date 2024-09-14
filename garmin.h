/*
    Jeeps wrapper for Garmin serial protocol.

    Copyright (C) 2002, 2003, 2004, 2005, 2006  Robert Lipe, robertlipe+source@gpsbabel.org

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
#ifndef GARMIN_H_INCLUDED_
#define GARMIN_H_INCLUDED_

#include <cstdio>              // for size_t

#include <QByteArray>          // for QByteArray
#include <QString>             // for QString
#include <QTextCodec>          // for QTextCodec
#include <QVector>             // for QVector

#include "defs.h"
#include "format.h"            // for Format
#include "jeeps/gpsdevice.h"   // for gpsdevh
#include "jeeps/gpssend.h"     // for GPS_PWay, GPS_SWay, GPS_PTrack, GPS_PPvt_Data, GPS_SLap
#include "mkshort.h"           // for MakeShort


class GarminFormat : public Format
{
public:
  QVector<arglist_t>* get_args() override
  {
    return &garmin_args;
  }

  ff_type get_type() const override
  {
    return ff_type_serial;
  }

  QVector<ff_cap> get_cap() const override
  {
    return FF_CAP_RW_ALL;
  }

  void rd_init(const QString& fname) override;
  void read() override;
  void rd_deinit() override
  {
    rw_deinit();
  }
  void wr_init(const QString& fname) override
  {
    rw_init(fname);
  }
  void write() override;
  void wr_deinit() override
  {
    rw_deinit();
  }
  void rd_position_init(const QString& fname) override;
  Waypoint* rd_position(posn_status* status) override;
  void rd_position_deinit() override
  {
    rw_deinit();
  }

private:
  /* Member Functions */

  QByteArray str_from_unicode(const QString& qstr);
  QString str_to_unicode(const QByteArray& cstr);
  static void write_char_string(char* dest, const char* source, size_t destsize);
  void rw_init(const QString& fname);
  void rw_deinit();
  static int waypt_read_cb(int total_ct, GPS_SWay** /* unused */);
  void waypt_read();
  static int lap_read_nop_cb(int /* unused */, GPS_SWay** /* unused */);
  static unsigned int checkWayPointIsAtSplit(Waypoint* wpt, GPS_SLap** laps, int nlaps);
  void track_read();
  void route_read();
  static void pvt2wpt(GPS_PPvt_Data pvt, Waypoint* wpt);
  static GPS_SWay* sane_GPS_Way_New();
  static int waypt_write_cb(GPS_SWay** /* unused */);
  static const char* get_gc_info(const Waypoint* wpt);
  int waypoint_prepare();
  void waypoint_write();
  void route_hdr_pr(const route_head* rte);
  void route_waypt_pr(const Waypoint* wpt);
  void route_write();
  void track_hdr_pr(const route_head* trk_head);
  void track_waypt_pr(const Waypoint* wpt);
  int track_prepare();
  void track_write();
  void course_write();
  static const char* d103_symbol_from_icon_number(unsigned int n);
  static int d103_icon_number_from_symbol(const QString& s);
  void garmin_fs_garmin_after_read(GPS_PWay way, Waypoint* wpt, int protoid);
  void garmin_fs_garmin_before_write(const Waypoint* wpt, GPS_PWay way, int protoid);

  /* Data Members */

  const char* portname{};
  MakeShort* mkshort_handle{};
  GPS_PWay* tx_waylist{};
  GPS_PWay* tx_routelist{};
  GPS_PWay* cur_tx_routelist_entry{};
  GPS_PTrack* tx_tracklist{};
  GPS_PTrack* cur_tx_tracklist_entry{};
  int my_track_count = 0;
  char* getposn = nullptr;
  char* poweroff = nullptr;
  char* eraset = nullptr;
  char* resettime = nullptr;
  char* snlen = nullptr;
  char* snwhiteopt = nullptr;
  char* deficon = nullptr;
  char* category = nullptr;
  char* categorybitsopt = nullptr;
  char* baudopt = nullptr;
  char* opt_codec = nullptr;
  int baud = 0;
  int categorybits{};
  bool receiver_must_upper = true;
  QTextCodec* codec{nullptr};

  QString valid_chars;

  QVector<arglist_t> garmin_args = {
    {
      "snlen", &snlen, "Length of generated shortnames", nullptr,
      ARGTYPE_INT, "1", nullptr, nullptr
    },
    {
      "snwhite", &snwhiteopt, "Allow whitespace synth. shortnames",
      nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    { "deficon", &deficon, "Default icon name", nullptr, ARGTYPE_STRING, ARG_NOMINMAX, nullptr },
    {
      "get_posn", &getposn, "Return current position as a waypoint",
      nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    {
      "power_off", &poweroff, "Command unit to power itself down",
      nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    {
      "erase_t", &eraset, "Erase existing courses when writing new ones",
      nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    {
      "resettime", &resettime, "Sync GPS time to computer time",
      nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    {
      "category", &category, "Category number to use for written waypoints",
      nullptr, ARGTYPE_INT, "1", "16", nullptr
    },
    {
      "bitscategory", &categorybitsopt, "Bitmap of categories",
      nullptr, ARGTYPE_INT, "1", "65535", nullptr
    },
    {
      "baud", &baudopt, "Speed in bits per second of serial port (baud=9600)",
      nullptr, ARGTYPE_INT, ARG_NOMINMAX, nullptr
    },
    {
      "codec", &opt_codec, "override codec to use for device",
      nullptr, ARGTYPE_STRING, ARG_NOMINMAX, nullptr
    },

  };

  gpsdevh* pvt_fd{};

  static constexpr const char* d103_icons[16] = {
    "dot",
    "house",
    "gas",
    "car",
    "fish",
    "boat",
    "anchor",
    "wreck",
    "exit",
    "skull",
    "flag",
    "camp",
    "circle_x",
    "deer",
    "1st_aid",
    "back-track"
  };
};
#endif // GARMIN_H_INCLUDED_
