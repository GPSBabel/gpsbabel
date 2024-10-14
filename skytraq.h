/*

    Serial download of track data from GPS loggers with Skytraq chipset.

    Copyright (C) 2008-2019  Mathias Adam, m.adam (at) adamis.de

    2008         J.C Haessig, jean-christophe.haessig (at) dianosis.org
    2009-09-06 | Josef Reisinger | Added "set target location", i.e. -i skytrag,targetlocation=<lat>:<lng>
    2010-10-23 | Josef Reisinger | Added read/write for miniHomer POI

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
#ifndef SKYTRAQ_H_INCLUDED_
#define SKYTRAQ_H_INCLUDED_

#include <QDateTime>  // for QDateTime
#include <QString>    // for QString
#include <QVector>    // for QVector

#include <cstdint>    // for uint8_t, int32_t, uint32_t, uint16_t, int16_t

#include "defs.h"
#include "format.h"   // for Format
#include "gbfile.h"   // for gbfile


class SkytraqBase
{
protected:
  /* Types */

  struct read_state {
    route_head*          route_head_;
    unsigned            wpn, tpn;

    unsigned gps_week;
    unsigned gps_sec;
    long x, y, z;
  };

  struct full_item {
    uint32_t gps_week;
    uint32_t gps_sec;
    int32_t  x;
    int32_t  y;
    int32_t  z;
  };

  struct compact_item {
    uint16_t dt;
    int16_t dx;
    int16_t dy;
    int16_t dz;
  };

  struct multi_hz_item {
    uint32_t gps_week;
    uint32_t gps_sec;
    int32_t  lat;
    int32_t  lon;
    int32_t  alt;
  };

  struct full_item_frame {
    unsigned char ts[4];
    unsigned char x[4];
    unsigned char y[4];
    unsigned char z[4];
  };

  struct compact_item_frame {
    unsigned char dt[2]; /* big endian unsigned short */
    unsigned char dpos[4];
  };

  struct multi_hz_item_frame {
    unsigned char v_kmh[2];
    unsigned char ts[4];
    unsigned char lat[4];
    unsigned char lon[4];
    unsigned char alt[4];
  };

  struct item_frame {
    unsigned char type_and_speed[2];
    union {
      multi_hz_item_frame multi_hz;
      full_item_frame full;
      compact_item_frame comp;
    };
  };

  /* Constants */

  static constexpr uint8_t NL[2] = { 0x0D, 0x0A };
  static constexpr uint8_t MSG_START[2] = { 0xA0, 0xA1 };
  static constexpr uint8_t SECTOR_READ_END[13] = { 'E','N','D', 0, 'C','H','E','C','K','S','U','M','=' };

  /* Member Functions */

  [[gnu::format(printf, 2, 3)]] static void db(int l, const char* msg, ...);
  void rd_drain() const;
  int rd_char(int* errors) const;
  int rd_buf(uint8_t* buf, int len) const;
  int rd_word() const;
  void wr_char(int c) const;
  void wr_buf(const unsigned char* str, int len) const;
  static int skytraq_calc_checksum(const unsigned char* buf, int len);
  int skytraq_rd_msg(void* payload, unsigned int len) const;
  void skytraq_wr_msg(const uint8_t* payload, int len) const;
  int skytraq_expect_ack(uint8_t id) const;
  int skytraq_expect_msg(uint8_t id, uint8_t* payload, int len) const;
  int skytraq_wr_msg_verify(const uint8_t* payload, int len) const;
  int skytraq_system_restart() const;
  int skytraq_set_baud(int baud) const;
  int skytraq_configure_logging() const;
  int skytraq_get_log_buffer_status(uint32_t* log_wr_ptr, uint16_t* sectors_free, uint16_t* sectors_total) const;
  static unsigned int me_read32(const unsigned char* p);
  QDateTime gpstime_to_qdatetime(int week, int sec) const;
  static void ECEF_to_LLA(double x, double y, long int z, double* lat, double* lon, double* alt);
  static void state_init(read_state* pst);
  Waypoint* make_trackpoint(read_state* st, double lat, double lon, double alt) const;
  int process_data_item(read_state* pst, const item_frame* pitem, int len) const;
  int process_data_sector(read_state* pst, const uint8_t* buf, int len) const;
  int skytraq_read_single_sector(unsigned int sector, uint8_t* buf) const;
  int skytraq_read_multiple_sectors(int first_sector, unsigned int sector_count, uint8_t* buf) const;
  void skytraq_read_tracks() const;
  int skytraq_probe() const;
  int skytraq_erase() const;
  void skytraq_set_location() const;
  void skytraq_rd_init(const QString& fname);
  void skytraq_read() const;
  void skytraq_rd_deinit();

  /* Data Members */

  void* serial_handle = nullptr;		/* IO file descriptor */
  int skytraq_baud = 0;		/* detected baud rate */

  OptionCString opt_erase;		/* erase after read? (0/1) */
  OptionCString opt_initbaud;		/* baud rate used to init device */
  OptionCString opt_dlbaud;		/* baud rate used for downloading tracks */
  OptionCString opt_read_at_once;	/* number of sectors to read at once (Venus6 only) */
  OptionCString opt_first_sector;	/* first sector to be read from the device (default: 0) */
  OptionCString opt_last_sector;	/* last sector to be read from the device (default: smart read everything) */
  OptionCString opt_dump_file;		/* dump raw data to this file (optional) */
  OptionCString opt_no_output;		/* disable output? (0/1) */
  OptionCString opt_set_location;	/* set if the "targetlocation" options was used */
  OptionCString opt_configure_logging;
  OptionCString opt_gps_utc_offset;
  OptionCString opt_gps_week_rollover;
};

class SkytraqFormat : public Format, private SkytraqBase
{
public:
  QVector<arglist_t>* get_args() override
  {
    return &skytraq_args;
  }

  ff_type get_type() const override
  {
    return ff_type_serial;
  }

  QVector<ff_cap> get_cap() const override
  {
    /*         waypoints,      tracks,      routes */
    return { ff_cap_read, ff_cap_read, ff_cap_none };
  }

  void rd_init(const QString& fname) override {skytraq_rd_init(fname);}
  void read() override {skytraq_read();}
  void rd_deinit() override {skytraq_rd_deinit();}

private:
  /* Data Members */

  QVector<arglist_t> skytraq_args = {
    {
      "erase", &opt_erase, "Erase device data after download",
      "0", ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    {
      "targetlocation", &opt_set_location, "Set location finder target location as lat,lng",
      nullptr, ARGTYPE_STRING, "", "", nullptr
    },
    {
      "configlog", &opt_configure_logging, "Configure logging parameter as tmin:tmax:dmin:dmax",
      nullptr, ARGTYPE_STRING, "", "", nullptr
    },
    {
      "baud", &opt_dlbaud, "Baud rate used for download",
      "230400", ARGTYPE_INT, "0", "230400", nullptr
    },
    {
      "initbaud", &opt_initbaud, "Baud rate used to init device (0=autodetect)",
      "0", ARGTYPE_INT, "4800", "230400", nullptr
    },
    {
      "read-at-once", &opt_read_at_once, "Number of sectors to read at once (0=use single sector mode)",
      "255", ARGTYPE_INT, "0", "255", nullptr
    },
    {
      "first-sector", &opt_first_sector, "First sector to be read from the device",
      "0", ARGTYPE_INT, "0", "65535", nullptr
    },
    {
      "last-sector", &opt_last_sector, "Last sector to be read from the device (-1: smart read everything)",
      "-1", ARGTYPE_INT, "-1", "65535", nullptr
    },
    {
      "dump-file", &opt_dump_file, "Dump raw data to this file",
      nullptr, ARGTYPE_OUTFILE, ARG_NOMINMAX, nullptr
    },
    {
      "no-output", &opt_no_output, "Disable output (useful with erase)",
      "0", ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    {
      "gps-utc-offset", &opt_gps_utc_offset, "Seconds that GPS time tracks UTC (0: best guess)",
      "0", ARGTYPE_INT, ARG_NOMINMAX, nullptr
    },
    {
      "gps-week-rollover", &opt_gps_week_rollover, "GPS week rollover period we're in (-1: best guess)",
      "-1", ARGTYPE_INT, ARG_NOMINMAX, nullptr
    },
  };
};

class SkytraqfileFormat : public Format, private SkytraqBase
{
public:
  QVector<arglist_t>* get_args() override
  {
    return &skytraq_fargs;
  }

  ff_type get_type() const override
  {
    return ff_type_file;
  }

  QVector<ff_cap> get_cap() const override
  {
    /*         waypoints,      tracks,      routes */
    return { ff_cap_read, ff_cap_read, ff_cap_none };
  }

  void rd_init(const QString& fname) override;
  void read() override;
  void rd_deinit() override;

private:

  /* Data Members */

  gbfile* file_handle = nullptr;		/* file descriptor (used by skytraq-bin format) */

  QVector<arglist_t> skytraq_fargs = {
    {
      "first-sector", &opt_first_sector, "First sector to be read from the file",
      "0", ARGTYPE_INT, "0", "65535", nullptr
    },
    {
      "last-sector", &opt_last_sector, "Last sector to be read from the file (-1: read till empty sector)",
      "-1", ARGTYPE_INT, "-1", "65535", nullptr
    },
    {
      "gps-utc-offset", &opt_gps_utc_offset, "Seconds that GPS time tracks UTC (0: best guess)",
      "0", ARGTYPE_INT, ARG_NOMINMAX, nullptr
    },
    {
      "gps-week-rollover", &opt_gps_week_rollover, "GPS week rollover period we're in (-1: best guess)",
      "-1", ARGTYPE_INT, ARG_NOMINMAX, nullptr
    },
  };
};


/**************************************************************************/
/*
 * support POI of skytraq based miniHomer device
 * http://navin.com.tw/miniHomer.htm
 * 2010-10-23	Josef Reisinger
 */
class MinihomerFormat : public Format, private SkytraqBase
{
public:
  QVector<arglist_t>* get_args() override
  {
    return &miniHomer_args;
  }

  ff_type get_type() const override
  {
    return ff_type_serial;
  }

  QVector<ff_cap> get_cap() const override
  {
    /*         waypoints,      tracks,      routes */
    return { ff_cap_read, ff_cap_read, ff_cap_none };
  }

  void rd_init(const QString& fname) override;
  void read() override;
  void rd_deinit() override;

private:

  /* Constants */

  /*
   * Names of the POIs on miniHomer
   */
  static constexpr const char* poinames[] = {
    "Home", "Car", "Boat", "Heart", "Bar"
  };

  /* Member Functions */

  static void lla2ecef(double lat, double lng, double alt, double* ecef_x, double* ecef_y, double* ecef_z);
  void miniHomer_get_poi() const;
  int miniHomer_set_poi(uint16_t poinum, const char* opt_poi) const;

  /* Data Members */

  OptionCString opt_set_poi_home;	/* set if a "poi" option was used */
  OptionCString opt_set_poi_car;	/* set if a "poi" option was used */
  OptionCString opt_set_poi_boat;	/* set if a "poi" option was used */
  OptionCString opt_set_poi_heart;	/* set if a "poi" option was used */
  OptionCString opt_set_poi_bar;	/* set if a "poi" option was used */

  QVector<arglist_t> miniHomer_args = {
    { "baud",         &opt_dlbaud,        "Baud rate used for download", "115200", ARGTYPE_INT, "0", "115200", nullptr },
    { "dump-file",    &opt_dump_file,     "Dump raw data to this file", nullptr, ARGTYPE_OUTFILE, ARG_NOMINMAX, nullptr },
    { "erase",        &opt_erase,         "Erase device data after download", "0", ARGTYPE_BOOL, ARG_NOMINMAX, nullptr },
    { "first-sector", &opt_first_sector,  "First sector to be read from the device", "0", ARGTYPE_INT, "0", "65535", nullptr },
    { "initbaud",     &opt_initbaud,      "Baud rate used to init device (0=autodetect)", "38400", ARGTYPE_INT, "38400", "38400", nullptr },
    { "last-sector",  &opt_last_sector,   "Last sector to be read from the device (-1: smart read everything)", "-1", ARGTYPE_INT, "-1", "65535", nullptr },
    { "no-output",    &opt_no_output,     "Disable output (useful with erase)", "0", ARGTYPE_BOOL, ARG_NOMINMAX, nullptr },
    { "read-at-once", &opt_read_at_once,  "Number of sectors to read at once (0=use single sector mode)", "255", ARGTYPE_INT, "0", "255", nullptr },
    { "Home",         &opt_set_poi_home,  "POI for Home Symbol as lat:lng[:alt]", nullptr, ARGTYPE_STRING, "", "", nullptr },
    { "Car",          &opt_set_poi_car,   "POI for Car Symbol as lat:lng[:alt]", nullptr, ARGTYPE_STRING, "", "", nullptr },
    { "Boat",         &opt_set_poi_boat,  "POI for Boat Symbol as lat:lng[:alt]", nullptr, ARGTYPE_STRING, "", "", nullptr },
    { "Heart",        &opt_set_poi_heart, "POI for Heart Symbol as lat:lng[:alt]", nullptr, ARGTYPE_STRING, "", "", nullptr },
    { "Bar",          &opt_set_poi_bar,   "POI for Bar Symbol as lat:lng[:alt]", nullptr, ARGTYPE_STRING, "", "", nullptr },
    {
      "gps-utc-offset", &opt_gps_utc_offset, "Seconds that GPS time tracks UTC (0: best guess)",
      "0", ARGTYPE_INT, ARG_NOMINMAX, nullptr
    },
    {
      "gps-week-rollover", &opt_gps_week_rollover, "GPS week rollover period we're in (-1: best guess)",
      "-1", ARGTYPE_INT, ARG_NOMINMAX, nullptr
    },
  };

  QString mhport;

};

#endif // SKYTRAQ_H_INCLUDED_
