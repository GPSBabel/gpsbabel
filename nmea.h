/*
	Read files containing selected NMEA 0183 sentences.
	Based on information by Eino Uikkanenj

	Copyright (C) 2004-2015 Robert Lipe, robertlipe+source@gpsbabel.org

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
#ifndef NMEA_H_INCLUDED_
#define NMEA_H_INCLUDED_

#include <ctime>                   // for gmtime

#include <QtCore/QList>            // for QList
#include <QtCore/QString>          // for QString, QString::KeepEmptyParts
#include <QtCore/QVector>          // for QVector

#include "defs.h"
#include "format.h"
#include "gbfile.h"                // for gbfprintf, gbfflush, gbfclose, gbfopen, gbfgetstr, gbfile


class NmeaFormat : public Format
{
public:
  /* Member Functions */

  QVector<arglist_t>* get_args() override
  {
    return &nmea_args;
  }

  ff_type get_type() const override
  {
    return ff_type_file;
  }

  QVector<ff_cap> get_cap() const override
  {
    return {
      (ff_cap)(ff_cap_read | ff_cap_write),
      (ff_cap)(ff_cap_read | ff_cap_write),
      ff_cap_none
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
  void wr_init(const QString& fname) override;
  void write() override;
  void wr_deinit() override;
  void rd_position_init(const QString& fname) override;
  Waypoint* rd_position(posn_status* status) override;
  void rd_position_deinit() override;
  void wr_position_init(const QString& fname) override;
  void wr_position(Waypoint* wpt) override;
  void wr_position_deinit() override;

  static int nmea_cksum(const char* const buf);

private:
  /* Types */

  enum preferred_posn_type {
    gp_unknown = 0,
    gpgga,
    gplgll,
    gprmc
  };

  enum {
    rm_unknown = 0,
    rm_serial,
    rm_file
  } read_mode;

  /* Member Functions */

  Waypoint* nmea_new_wpt();
  void nmea_add_wpt(Waypoint* wpt, route_head* trk) const;
  static void nmea_release_wpt(Waypoint* wpt);
  void nmea_set_waypoint_time(Waypoint* wpt, tm* time, double fsec);
  void gpgll_parse(char* ibuf);
  void gpgga_parse(char* ibuf);
  void gprmc_parse(char* ibuf);
  void gpwpl_parse(char* ibuf);
  void gpzda_parse(char* ibuf);
  void gpgsa_parse(char* ibuf) const;
  void gpvtg_parse(char* ibuf) const;
  static double pcmpt_deg(int d);
  void pcmpt_parse(char* ibuf);
  void nmea_fix_timestamps(route_head* track);
  static int notalkerid_strmatch(const char* s1, const char* sentenceFormatterMnemonicCode);
  void nmea_parse_one_line(char* ibuf);
  static void safe_print(int cnt, const char* b);
  int hunt_sirf();
  void nmea_wayptpr(const Waypoint* wpt) const;
  void nmea_track_init(const route_head* unused);
  void nmea_trackpt_pr(const Waypoint* wpt);
  void sirf_write(unsigned char* buf) const;
  void reset_sirf_to_nmea(int br);

  /* Data Members */

  gbfile* file_in, *file_out;
  route_head* trk_head;
  short_handle mkshort_handle;
  preferred_posn_type posn_type;
  struct tm tm;
  Waypoint* curr_waypt;
  Waypoint* last_waypt;
  void* gbser_handle;
  QString posn_fname;
  QList<Waypoint*> pcmpt_head;

  int without_date;	/* number of created trackpoints without a valid date */
  struct tm opt_tm;	/* converted "date" parameter */

  char* opt_gprmc;
  char* opt_gpgga;
  char* opt_gpvtg;
  char* opt_gpgsa;
  char* snlenopt;
  char* optdate;
  char* getposnarg;
  char* opt_sleep;
  char* opt_baud;
  char* opt_append;
  char* opt_gisteq;
  char* opt_ignorefix;

  long sleepus;
  int getposn;
  int append_output;
  bool amod_waypoint;

  time_t last_time;
  double last_read_time;   /* Last timestamp of GGA or PRMC */
  int datum;
  int had_checksum;

  Waypoint* nmea_rd_posn(posn_status*);
  void nmea_rd_posn_init(const QString& fname);

  int wpt_not_added_yet;

  QVector<arglist_t> nmea_args = {
    {"snlen", &snlenopt, "Max length of waypoint name to write", "6", ARGTYPE_INT, "1", "64", nullptr },
    {"gprmc", &opt_gprmc, "Read/write GPRMC sentences", "1", ARGTYPE_BOOL, ARG_NOMINMAX, nullptr },
    {"gpgga", &opt_gpgga, "Read/write GPGGA sentences", "1", ARGTYPE_BOOL, ARG_NOMINMAX, nullptr },
    {"gpvtg", &opt_gpvtg, "Read/write GPVTG sentences", "1", ARGTYPE_BOOL, ARG_NOMINMAX, nullptr },
    {"gpgsa", &opt_gpgsa, "Read/write GPGSA sentences", "1", ARGTYPE_BOOL, ARG_NOMINMAX, nullptr },
    {"date", &optdate, "Complete date-free tracks with given date (YYYYMMDD).", nullptr, ARGTYPE_INT, ARG_NOMINMAX, nullptr },
    {
      "get_posn", &getposnarg, "Return current position as a waypoint",
      nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    {"pause", &opt_sleep, "Decimal seconds to pause between groups of strings", nullptr, ARGTYPE_INT, ARG_NOMINMAX, nullptr},
    {"append_positioning", &opt_append, "Append realtime positioning data to the output file instead of truncating", "0", ARGTYPE_BOOL, ARG_NOMINMAX, nullptr},
    {"baud", &opt_baud, "Speed in bits per second of serial port (baud=4800)", nullptr, ARGTYPE_INT, ARG_NOMINMAX, nullptr },
    {"gisteq", &opt_gisteq, "Write tracks for Gisteq Phototracker", "0", ARGTYPE_BOOL, ARG_NOMINMAX, nullptr},
    {"ignore_fix", &opt_ignorefix, "Accept position fixes in gpgga marked invalid", "0", ARGTYPE_BOOL, ARG_NOMINMAX, nullptr},
  };

};
#endif // NMEA_H_INCLUDED_
