/*
    Global data for GPSBabel.

    Copyright (C) 2012-2016, Zingo Andersen zingo@zingo.org
    Copyright (C) 2016 Robert Lipe, robertlipe+source@gpsbabel.org

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
 * This is the bridge between the GPSBabel and globalsat sport devices
 * gh625XT. Globalsat has a few devices under the sport brand and they
 * are using a similar USB serial protocol.
 * Currently only gh625XT is supported by this driver but the code could
 * extended (maybe autodetect) support more devices in the future.
 *
 * The code is based on GH625XT-COMMUNICATION_SPECS_20111205-1.pdf from Globasat
 * that they nicely supplied on request.
 *
 * Usage examples:
 * gpsbabel -i globalsat -f /dev/ttyUSB0 -o gpx,garminextensions -F xxx.gpx
 * gpsbabel -i globalsat -f /dev/ttyUSB0 -o gtrnctr,course=0,sport=Running -F xxx.fit
 *
 */
#ifndef GLOBALSATSPORT_H_INCLUDED_
#define GLOBALSATSPORT_H_INCLUDED_

#include <cstdint>           // for uint32_t, uint8_t, uint16_t, int16_t

#include <QtCore/QString>    // for QString
#include <QtCore/QTimeZone>  // for QTimeZone
#include <QtCore/QVector>    // for QVector

#include "defs.h"
#include "format.h"          // for Format
#include "gbfile.h"          // for gbfclose, gbfopen, gbfread, gbfwrite, gbfile


class GlobalsatSportFormat : public Format
{
public:
  QVector<arglist_t>* get_args() override
  {
    return &globalsat_args;
  }

  ff_type get_type() const override
  {
    return ff_type_serial;
  }

  QVector<ff_cap> get_cap() const override
  {
    return {
      ff_cap_none,			// waypoints
      ff_cap_read,			// tracks
      ff_cap_none,			// routes
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
  /* Types */
  enum globalsat_commands_e {
    CommandWhoAmI = 0xBF,
    CommandGetSystemInformation = 0x85,
    CommandGetSystemConfiguration = 0x86,

    CommandSetSystemConfiguration = 0x96,
    CommandSetSystemInformation = 0x98,
    CommandGetScreenshot = 0x83,

    CommandGetWaypoints = 0x77,
    CommandSendWaypoint = 0x76,
    CommandDeleteWaypoints = 0x75,

    CommandSendRoute = 0x93,
    CommandDeleteAllRoutes = 0x97,

    CommandGetTrackFileHeaders = 0x78,
    CommandGetTrackFileSections = 0x80,
    CommandGetNextTrackSection = 0x81,
    //CommandReGetLastSection = 0x82,
    CommandId_FINISH = 0x8A,
    CommandSendTrackStart = 0x90,
    CommandSendTrackSection = 0x91,

    HeaderTypeLaps = 0xAA,
    HeaderTypeTrackPoints = 0x55,

    ResponseInsufficientMemory = 0x95,
    ResponseResendTrackSection = 0x92,
    ResponseSendTrackFinish = 0x9A
  };

  struct gh_date {
    uint8_t Year;
    uint8_t Month;
    uint8_t Day;
  };

  struct gh_time {
    uint8_t Hour;
    uint8_t Minute;
    uint8_t Second;
  };

  struct gh_trainheader {
    gh_date dateStart;
    gh_time timeStart;
    uint32_t TotalPoint;		//6-9
    uint32_t TotalTime;		//10-13
    uint32_t TotalDistance;	//14-17
    uint16_t LapCnts;		//18-19
    union {
      uint32_t Index;
      uint32_t StartPt;
    } gh_ptrec;			//20-23
    union {
      uint32_t LapIndex;
      uint32_t EndPt;
    } gh_laprec;			//24-27
    uint8_t DataType;		//28
  };

  struct gh_db_train {
    gh_date dateStart;
    gh_time timeStart;
    uint32_t TotalPoint;		//6-9
    uint32_t TotalTime;		//10-13
    uint32_t TotalDistance;	//14-17
    uint16_t LapCnts;		//18-19
    union {
      uint32_t Index;
      uint32_t StartPt;
    } gh_ptrec;			//20-23
    union {
      uint32_t LapIndex;
      uint32_t EndPt;
    } gh_laprec;			//24-27
    uint8_t MultiSport;		//28 on/off
    uint16_t Calory;		//29-30
    uint32_t MaxSpeed;
    uint8_t MaxHeart;
    uint8_t AvgHeart;

    uint16_t Ascent;
    uint16_t Descent;

    int16_t MinAlti;
    int16_t MaxAlti;
    uint16_t AvgCadns;
    uint16_t BestCadns;
    uint16_t AvgPower;
    uint16_t MaxPower;
    uint8_t Sport1;
    uint8_t Sport2;
    uint8_t Sport3;
    uint8_t Sport4;
    uint8_t Sport5;
  };

  struct gh_db_lap {
    uint32_t AccruedTime;
    uint32_t TotalTime;
    uint32_t TotalDistance;
    uint16_t Calory;
    uint32_t MaxSpeed;
    uint8_t MaxHeart;
    uint8_t AvgHeart;
    int16_t MinAlti;
    int16_t MaxAlti;
    uint16_t AvgCadns;
    uint16_t BestCadns;
    uint16_t AvgPower;
    uint16_t MaxPower;
    uint8_t MultiSportIndex;
    uint32_t StartPt;
    uint32_t EndPt;
  };

  struct gh_recpoint {
    uint32_t Latitude;
    uint32_t Longitude;
    int16_t Altitude;
    uint32_t Speed;
    uint8_t HeartRate;
    uint32_t IntervalTime;
    uint16_t Cadence;
    uint16_t PwrCadence;
    uint16_t Power;
  };

  /* Member Functions */

  void serial_init(const char* fname);
  void serial_deinit();
  int serial_recv_byte() const;
  void serial_write_byte(uint8_t byte) const;
  int recv_byte();
  void write_byte(uint8_t byte);
  void globalsat_write_package(uint8_t* payload, uint32_t size);
  uint8_t* globalsat_read_package(int* out_length, uint8_t* out_DeviceCommand);
  void globalsat_send_simple(uint8_t command);
  void globalsat_probe_device();
  void waypoint_read();
  void track_read();
  static void route_read();

  /* Data Members */

  void* serial_handle{nullptr};
  bool isSizeSwapped{false};

  char* showlist{nullptr};               // if true show a list instead of download tracks
  char* track{nullptr};                  // if not 0 only download this track, if 0 download all

  char* opt_dump_file{nullptr};          // dump raw data to this file (optional)
  char* opt_input_dump_file{nullptr};    // if true input is from a dump-file instead of serial console
  char* opt_timezone{nullptr};
  gbfile* dumpfile{nullptr};             // used for creating bin/RAW datadump files, useful for testing
  gbfile* in_file{nullptr};              // used for reading from bin/RAW datadump files, useful for testing
  QTimeZone* timezn{nullptr};

  QVector<arglist_t> globalsat_args = {
    {"showlist", &showlist, "list tracks", nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr},
    {"track", &track, "get track", "0", ARGTYPE_INT, ARG_NOMINMAX, nullptr},
    {"dump-file", &opt_dump_file, "Dump raw data to this file", nullptr, ARGTYPE_OUTFILE, ARG_NOMINMAX, nullptr},
    {"input-is-dump-file", &opt_input_dump_file, "Dump raw data to this file", nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr},
    {"timezone", &opt_timezone, "Time zone ID", nullptr, ARGTYPE_STRING, ARG_NOMINMAX, nullptr},
  };
};
#endif // GPX_H_INCLUDED_
