/*
    Handle energympro (GPS training watch) .cpo files

    Copyright (c) 2014 Zingo Andersen zingo@vectrace.com
    Copyright (C) 2014 Robert Lipe, robertlipe+source@gpsbabel.org

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
#ifndef ENERGYMPRO_H_INCLUDED_
#define ENERGYMPRO_H_INCLUDED_

#include <cstdint>              // for uint8_t, uint16_t, uint32_t, int16_t

#include <QString>              // for QString
#include <QTimeZone>            // for QTimeZone
#include <QVector>              // for QVector

#include "defs.h"
#include "format.h"             // for Format
#include "gbfile.h"             // for gbfgetc, gbfseek, gbfclose, gbfopen, gbfread, gbfgetuint32, gbfcopyfrom, gbfgetuint16, gbfile, gbsize_t
#include "src/core/datetime.h"  // for DateTime


class EnergymproFormat : public Format
{
public:
  QVector<arglist_t>* get_args() override
  {
    return &energympro_args;
  }

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
  /* Types */

  struct tw_date {
    uint8_t Year;
    uint8_t Month;
    uint8_t Day;
  };

  struct tw_time {
    uint8_t Hour;
    uint8_t Minute;
    uint8_t Second;
  };

  struct tw_workout {
    tw_date       dateStart;            // start date
    tw_time       timeStart;            // start time
    uint16_t      TotalRecPt;           // Total record Point
    uint32_t      TotalTime;            // Total Time
    uint32_t      TotalDist;            // Total Distance
    uint16_t      LapNumber;            // Lap Number
    uint16_t      Calory;               // Calory
    uint32_t      MaxSpeed;             // Max Speed
    uint32_t      AvgSpeed;             // average Speed
    uint8_t       MaxHeart;             // Max Heartrate
    uint8_t       AvgHeart;             // average Heart
    uint16_t      Ascent;               // Ascent
    uint16_t      Descent;              // Descent
    int16_t       MinAlti;              // Min Altitude
    int16_t       MaxAlti;              // Max Altitude
    uint8_t       AvgCad;               // average Cadence
    uint8_t       MaxCad;               // Best Cadence
    uint16_t      AvgPower;             // average Power
    uint16_t      MaxPower;             // Max Power
    char          VersionProduct[15];
    uint8_t       reserved1;
    uint8_t       VersionVerNum;
    uint8_t       reserved2[17];
  };

  struct tw_point {
    uint32_t  Latitude;
    uint32_t  Longitude;
    int16_t   Altitude;
    uint16_t  reserved1;
    uint32_t  Speed;
    uint16_t  IntervalDist;          // Interval Distance
    uint16_t  reserved2;
    uint32_t  IntervalTime;          // Interval time
    uint8_t   Status;                //Status (0 = ok, 1 = miss, 2 = no good, 3 = bad)
    uint8_t   HR_Heartrate;
    uint8_t   HR_Status;
    uint8_t   reserved3;
    uint32_t  Speed_Speed;
    uint8_t   Speed_Status;
    uint8_t   reserved4;
    uint8_t   reserved5;
    uint8_t   reserved6;
    uint8_t   Cadence_Cadence;
    uint8_t   Cadence_Status;
    uint16_t  Power_Cadence;
    uint16_t  Power_Power;
    uint8_t   Power_Status;
    uint8_t   reserved7;
    uint8_t   Temp;
    uint8_t   reserved8;
    uint8_t   reserved9;
    uint8_t   reserved10;
  };

  struct tw_lap {
    uint32_t       splitTime;        // split time
    uint32_t       TotalTime;        // Total Time
    uint16_t       Number;           // Number
    uint16_t       reserved1;
    uint32_t       lDistance;        // Distance
    uint16_t       Calorie;          // Calorie
    uint16_t       reserved2;
    uint32_t       MaxSpeed;         // Max Speed
    uint32_t       AvgSpeed;         // average Speed
    uint8_t        MaxHeartrate;     // Max Heartrate
    uint8_t        AvgHeartrate;     // average Heartrate
    int16_t        MinAlti;          // Min Altitude
    int16_t        MaxAlti;          // Max Altitude
    uint8_t        AvgCad;           // average Cadence
    uint8_t        MaxCad;           // Max Cadence
    uint16_t       AvgPower;         // average Power
    uint16_t       MaxPower;         // Max Power
    uint16_t       StartRecPt;       // start record point
    uint16_t       FinishRecPt;      // Finish record point
  };

  /* Member Functions */

  void read_point(route_head* gpsbabel_route, gpsbabel::DateTime& gpsDateTime) const;
  void read_lap() const;
  void track_read();

  /* Data Members */

  gbfile* file_in{nullptr};
  char* opt_timezone{nullptr};
  QTimeZone* timezn{nullptr};

  QVector<arglist_t> energympro_args = {
    {"timezone", &opt_timezone, "Time zone ID", nullptr, ARGTYPE_STRING, ARG_NOMINMAX, nullptr},
  };
};
#endif // ENERGYMPRO_H_INCLUDED_
