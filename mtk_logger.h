/*
  Download track data from GPS loggers based in MTK chipset.

    Copyright (C) 2007 Per Borgentun, e4borgen(at)yahoo.com
    With lot of inspiration from wbt-200.c
       Copyright (C) 2006 Andy Armstrong

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
   --------------------------------------------------------------
  This module will download track data from a MTK chipset based GPS logger.
  It will also convert the raw data.bin file to MTK compatible CSV and
  gpsbabel output of your choice.
  It has been tested with Transystem i-Blue 747 but other devices should
  work as well (Qstarz BT-Q1000, iTrek Z1, ...)

  For more info and tweaks on MTK based loggers
   <http://www.gpspassion.com/forumsen/topic.asp?TOPIC_ID=81990>
   <http://www.gpspassion.com/forumsen/topic.asp?TOPIC_ID=81315>
 For info about the used log format:
  <http://spreadsheets.google.com/pub?key=pyCLH-0TdNe-5N-5tBokuOA&gid=5>

  Module updated 2008-2009. Now also handles Holux M-241 and
  Holux GR-245 aka. GPSport-245 devices. These devices have small differences
  in the log format and use a lower baudrate to transfer the data.

 Example usage::
   # Read from USB port, output trackpoints & waypoints in GPX format
  ./gpsbabel -D 2 -t -w -i mtk -f /dev/ttyUSB0 -o gpx -F out.gpx

   # Parse an existing .bin file (data_2007_09_04.bin), output trackpoints as
   #  both CSV file and GPX, discard points without fix
  ./gpsbabel -D 2 -t -i mtk-bin,csv=data__2007_09_04.csv -f data_2007_09_04.bin -x discard,fixnone -o gpx -F out.gpx

  Tip: Check out the -x height,wgs84tomsl filter to correct the altitude.
  Todo:
    o ....

 */
#ifndef MTK_LOGGER_H_INCLUDED_
#define MTK_LOGGER_H_INCLUDED_

#include <cstdio>    // for FILE
#include <ctime>     // for time_t

#include <QString>   // for QString
#include <QVector>   // for QVector

#include "defs.h"
#include "format.h"  // for Format
#include "gbfile.h"  // for gbfile
#include "option.h"  // for OptionBool, OptionString


class MtkLoggerBase
{
protected:
  /* MTK packet id's -- currently unused... */
  enum MTK_NMEA_PACKET {
    PMTK_TEST = 0,
    PMTK_ACK  = 1,
    PMTK_SYS_MSG = 10,
    PMTK_CMD_HOT_START  = 101,
    PMTK_CMD_WARM_START = 102,
    PMTK_CMD_COLD_START = 103,
    PMTK_CMD_FULL_COLD_START  = 104,
    PMTK_CMD_LOG                = 182, /* Data log commands */
    PMTK_SET_NMEA_BAUDRATE      = 251,
    PMTK_API_SET_DGPS_MODE    = 301,
    PMTK_API_SET_SBAS_ENABLED = 313,
    PMTK_API_SET_NMEA_OUTPUT  = 314,
    PMTK_API_SET_PWR_SAV_MODE = 320,
    PMTK_API_SET_DATUM          = 330,
    PMTK_API_SET_DATUM_ADVANCE  = 331,
    PMTK_API_SET_USER_OPTION    = 390,
    PMTK_API_Q_FIX_CTL          = 400,
    PMTK_API_Q_DGPS_MODE    = 401,
    PMTK_API_Q_SBAS_ENABLED = 413,
    PMTK_API_Q_NMEA_OUTPUT  = 414,
    PMTK_API_Q_PWR_SAV_MODE = 420,
    PMTK_API_Q_DATUM            = 430,
    PMTK_API_Q_DATUM_ADVANCE    = 431,
    PMTK_API_GET_USER_OPTION    = 490,
    PMTK_DT_FIX_CTL             = 500,
    PMTK_DT_DGPS_MODE    = 501,
    PMTK_DT_SBAS_ENABLED = 513,
    PMTK_DT_NMEA_OUTPUT  = 514,
    PMTK_DT_PWR_SAV_MODE = 520,
    PMTK_DT_DATUM               = 530,
    PMTK_DT_FLASH_USER_OPTION   = 590,
    PMTK_Q_VERSION       = 604,
    PMTK_Q_RELEASE              = 605,
    PMTK_DT_VERSION      = 704,
    PMTK_DT_RELEASE             = 705
  };

  static constexpr unsigned char LOG_RST[16] = {
    0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, /* start marker */
    0x00, 0x00, 0x00, 0x00, 0x00,       /* data */
    0xbb, 0xbb, 0xbb, 0xbb
  };          /* end marker */

  static constexpr const char* MTK_ACK[] = { /* Flags returned from PMTK001 ack packet */
    "Invalid packet", "Unsupported packet type",
    "Valid packet but action failed", "Valid packet, action success"
  };

  /* *************************************** */

  /* Data id, type, sizes used by MTK chipset - don't touch.... */
  enum {
    UTC = 0,
    VALID,
    LATITUDE,
    LONGITUDE,
    HEIGHT,
    SPEED,
    HEADING,
    DSTA,
    DAGE,
    PDOP,
    HDOP,
    VDOP,
    NSAT,
    SID,
    ELEVATION,
    AZIMUTH,
    SNR,
    RCR,
    MILLISECOND,
    DISTANCE,
  } /* DATA_TYPES */;

  struct log_type_t {
    int id;
    int size;
    const char* name;
  };
  log_type_t log_type[32] =  {
    { 0, 4, "UTC" },
    { 1, 2, "VALID" },
    { 2, 8, "LATITUDE,N/S"},
    { 3, 8, "LONGITUDE,E/W"},
    { 4, 4, "HEIGHT" },
    { 5, 4, "SPEED" },
    { 6, 4, "HEADING" },
    { 7, 2, "DSTA" },
    { 8, 4, "DAGE" },
    { 9, 2, "PDOP" },
    { 10, 2, "HDOP"},
    { 11, 2, "VDOP"},
    { 12, 2, "NSAT (USED/VIEW)"},
    { 13, 4, "SID",},
    { 14, 2, "ELEVATION" },
    { 15, 2, "AZIMUTH" },
    { 16, 2, "SNR"},
    { 17, 2, "RCR"},
    { 18, 2, "MILLISECOND"},
    { 19, 8, "DISTANCE" },
    { 20, 0, nullptr},
  };

  struct sat_info {
    char id, used;
    short elevation, azimut, snr;
  };

  struct data_item {
    time_t timestamp;
    short valid;
    double lat;
    double lon;
    float height;
    float speed;
    float heading;
    short dsta; // differential station id
    float dage;  // differential data age
    float pdop, hdop, vdop;
    char sat_used, sat_view, sat_count;
    short rcr;
    unsigned short timestamp_ms;
    double distance;
    sat_info sat_data[32];
  };

  struct mtk_loginfo {
    unsigned int bitmask;
    int logLen;
    int period, distance, speed; /* in 10:ths of sec, m, km/h */
    int track_event;
  };

  /* *************************************** */

  /* MTK chip based devices with different baudrate, tweaks, ... */
  enum MTK_DEVICE_TYPE {
    MTK_LOGGER,
    HOLUX_M241,
    HOLUX_GR245
  };


  void* fd{};  /* serial fd */
  FILE* fl{};  /* bin.file fd */
  char* port{}; /* serial port name */
  OptionBool OPT_erase;  /* erase ? command option */
  OptionBool OPT_erase_only;  /* erase_only ? command option */
  OptionBool OPT_log_enable;  /* enable ? command option */
  OptionString csv_file; /* csv ? command option */
  OptionString OPT_block_size_kb; /* block_size_kb ? command option */
  MTK_DEVICE_TYPE mtk_device = MTK_LOGGER;

  mtk_loginfo mtk_info{};

  static constexpr char LIVE_CHAR[4] = {'-', '\\','|','/'};

  static constexpr char CMD_LOG_DISABLE[]= "$PMTK182,5*20\r\n";
  static constexpr char CMD_LOG_ENABLE[] = "$PMTK182,4*21\r\n";
  static constexpr char CMD_LOG_FORMAT[] = "$PMTK182,2,2*39\r\n";
  static constexpr char CMD_LOG_ERASE[]  = "$PMTK182,6,1*3E\r\n";
  static constexpr char CMD_LOG_STATUS[] = "$PMTK182,2,7*3C\r\n";

// Arguments for log fetch 'mtk' command..

  QVector<arglist_t> mtk_sargs = {
    {
      "erase", &OPT_erase, "Erase device data after download",
      "0", ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    {
      "erase_only", &OPT_erase_only, "Only erase device data, do not download anything",
      "0", ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    {
      "log_enable", &OPT_log_enable, "Enable logging after download",
      "0", ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    {
      "csv",   &csv_file, "MTK compatible CSV output file",
      nullptr, ARGTYPE_STRING, ARG_NOMINMAX, nullptr
    },
    {
      "block_size_kb", &OPT_block_size_kb, "Size of blocks in KB to request from device",
      "1", ARGTYPE_INT, "1", "64", nullptr
    },
  };

  QVector<arglist_t> mtk_fargs = {
    {
      "csv",   &csv_file, "MTK compatible CSV output file",
      nullptr, ARGTYPE_STRING, ARG_NOMINMAX, nullptr
    },
  };

  route_head* trk_head = nullptr;

  gbfile* cd{};

  [[gnu::format(printf, 2, 3)]] static void dbg(int l, const char* msg, ...);
  static QString GetTempName(bool backup);
  int do_send_cmd(const char* cmd, int cmdLen);
  int do_cmd(const char* cmd, const char* expect, char** rslt, time_t timeout_sec);
  void mtk_rd_init_m241(const QString& fname);
  void mtk_rd_init(const QString& fname);
  void mtk_rd_deinit();
  int mtk_erase();
  void mtk_read();
  int add_trackpoint(int idx, long unsigned int bmask, data_item* itm);
  void mtk_csv_init(const QString& csv_fname, long unsigned int bitmask);
  void mtk_csv_deinit();
  static int csv_line(gbfile* csvFile, int idx, long unsigned int bmask, data_item* itm);
  int mtk_parse(unsigned char* data, int dataLen, unsigned int bmask);
  int mtk_parse_info(const unsigned char* data, int dataLen);
  int mtk_log_len(unsigned int bitmask);
  void file_init_m241(const QString& fname);
  void file_init(const QString& fname);
  void file_deinit();
  void holux245_init();
  int is_holux_string(const unsigned char* data, int dataLen);
  void file_read();
};

class MtkFormat : public Format, private MtkLoggerBase
{
public:
  QVector<arglist_t>* get_args() override
  {
    return &mtk_sargs;
  }

  ff_type get_type() const override
  {
    return ff_type_serial;
  }

  QVector<ff_cap> get_cap() const override
  {
    return { ff_cap_read, ff_cap_read, ff_cap_none };
  }

  void rd_init(const QString& fname) override
  {
    mtk_rd_init(fname);
  }

  void read() override
  {
    mtk_read();
  }

  void rd_deinit() override
  {
    mtk_rd_deinit();
  }
};

class MtkM241Format : public Format, private MtkLoggerBase
{
public:
  QVector<arglist_t>* get_args() override
  {
    return &mtk_sargs;
  }

  ff_type get_type() const override
  {
    return ff_type_serial;
  }

  QVector<ff_cap> get_cap() const override
  {
    return { ff_cap_none, ff_cap_read, ff_cap_none };
  }

  void rd_init(const QString& fname) override
  {
    mtk_rd_init_m241(fname);
  }

  void read() override
  {
    mtk_read();
  }

  void rd_deinit() override
  {
    mtk_rd_deinit();
  }
};

class MtkFileFormat : public Format, private MtkLoggerBase
{
public:
  QVector<arglist_t>* get_args() override
  {
    return &mtk_fargs;
  }

  ff_type get_type() const override
  {
    return ff_type_file;
  }

  QVector<ff_cap> get_cap() const override
  {
    return { ff_cap_read, ff_cap_read, ff_cap_none };
  }

  void rd_init(const QString& fname) override
  {
    file_init(fname);
  }

  void read() override
  {
    file_read();
  }

  void rd_deinit() override
  {
    file_deinit();
  }
};

class MtkM241FileFormat : public Format, private MtkLoggerBase
{
public:
  QVector<arglist_t>* get_args() override
  {
    return &mtk_fargs;
  }

  ff_type get_type() const override
  {
    return ff_type_file;
  }

  QVector<ff_cap> get_cap() const override
  {
    return { ff_cap_read, ff_cap_read, ff_cap_none };
  }

  void rd_init(const QString& fname) override
  {
    file_init_m241(fname);
  }

  void read() override
  {
    file_read();
  }

  void rd_deinit() override
  {
    file_deinit();
  }
};
#endif // MTK_LOGGER_H_INCLUDED_
