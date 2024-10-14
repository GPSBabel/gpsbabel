/*

    Support for FIT track files.

    Copyright (C) 2011 Paul Brook, paul@nowt.org
    Copyright (C) 2003-2011  Robert Lipe, robertlipe+source@gpsbabel.org
    Copyright (C) 2019 Martin Buck, mb-tmp-tvguho.pbz@gromit.dyndns.org

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
#ifndef GARMIN_FIT_H_INCLUDED_
#define GARMIN_FIT_H_INCLUDED_

#include <cstdint>              // for uint8_t, uint16_t, uint32_t
#include <deque>                // for deque
#include <stdexcept>            // for runtime_error
#include <utility>              // for pair
#include <vector>               // for vector

#include <QHash>                // for QHash
#include <QList>                // for QList
#include <QString>              // for QString
#include <QVariant>             // for QVariant
#include <QVector>              // for QVector

#include "defs.h"
#include "format.h"             // for Format
#include "gbfile.h"             // for gbfile
#include "src/core/datetime.h"  // for DateTime


class GarminFitFormat : public Format
{
public:
  QVector<arglist_t>* get_args() override
  {
    return &fit_args;
  }

  ff_type get_type() const override
  {
    return ff_type_file;
  }

  QVector<ff_cap> get_cap() const override
  {
    return {
      ff_cap_write,				/* waypoints */
      (ff_cap)(ff_cap_read | ff_cap_write),	/* tracks */
      ff_cap_none 				/* routes */
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

  struct fit_field_t {
    int id {};
    int size{};
    int type{};
  };

  struct fit_message_def {
    int endian{};
    int global_id{};
    QList<fit_field_t> fields;
  };

  struct fit_data_t {
    int len{};
    int endian{};
    route_head* track{nullptr};
    uint32_t last_timestamp{};
    uint32_t global_utc_offset{};
    QHash<int, fit_message_def> message_def;
  };

  struct FitCourseRecordPoint {
    FitCourseRecordPoint(const Waypoint& wpt, bool is_course_point, unsigned int course_point_type = kCoursePointTypeGeneric)
      : lat(wpt.latitude),
        lon(wpt.longitude),
        altitude(wpt.altitude),
        speed(wpt.speed_value_or(-1)),
        odometer_distance(wpt.odometer_distance),
        creation_time(wpt.creation_time),
        shortname(wpt.shortname),
        is_course_point(is_course_point),
        course_point_type(course_point_type) {}
    double lat, lon, altitude;
    double speed, odometer_distance;
    gpsbabel::DateTime creation_time;
    QString shortname;
    bool is_course_point;
    unsigned int course_point_type;
  };

  class ReaderException : public std::runtime_error
  {
    using std::runtime_error::runtime_error;
  };

  /* Constants */

// constants for global IDs
  static constexpr int kIdFileId = 0;
  static constexpr int kIdDeviceSettings = 0;
  static constexpr int kIdLap = 19;
  static constexpr int kIdRecord = 20;
  static constexpr int kIdEvent = 21;
  static constexpr int kIdLocations = 29;
  static constexpr int kIdCourse = 31;
  static constexpr int kIdCoursePoint = 32;

// constants for local IDs (for writing)
  static constexpr int kWriteLocalIdFileId = 0;
  static constexpr int kWriteLocalIdCourse = 1;
  static constexpr int kWriteLocalIdLap = 2;
  static constexpr int kWriteLocalIdEvent = 3;
  static constexpr int kWriteLocalIdCoursePoint = 4;
  static constexpr int kWriteLocalIdRecord = 5;

// constants for message fields
// for all global IDs
  static constexpr int kFieldTimestamp = 253;
  static constexpr int kFieldMessageIndex = 254;
// for global ID: file id
  static constexpr int kFieldType = 0;
  static constexpr int kFieldManufacturer = 1;
  static constexpr int kFieldProduct = 2;
  static constexpr int kFieldTimeCreated = 4;
// for global ID: device settings
  static constexpr int kFieldGlobalUtcOffset = 4;
// for global ID: lap
  static constexpr int kFieldStartTime = 2;
  static constexpr int kFieldStartLatitude = 3;
  static constexpr int kFieldStartLongitude = 4;
  static constexpr int kFieldEndLatitude = 5;
  static constexpr int kFieldEndLongitude = 6;
  static constexpr int kFieldElapsedTime = 7;
  static constexpr int kFieldTotalTimerTime = 8;
  static constexpr int kFieldTotalDistance = 9;
  static constexpr int kFieldAvgSpeed = 13;
  static constexpr int kFieldMaxSpeed = 14;
// for global ID: record
  static constexpr int kFieldLatitude = 0;
  static constexpr int kFieldLongitude = 1;
  static constexpr int kFieldAltitude = 2;
  static constexpr int kFieldHeartRate = 3;
  static constexpr int kFieldCadence = 4;
  static constexpr int kFieldDistance = 5;
  static constexpr int kFieldSpeed = 6;
  static constexpr int kFieldPower = 7;
  static constexpr int kFieldTemperature = 13;
  static constexpr int kFieldEnhancedSpeed = 73;
  static constexpr int kFieldEnhancedAltitude = 78;
// for global ID: event
  static constexpr int kFieldEvent = 0;
  static constexpr int kEnumEventTimer = 0;
  static constexpr int kFieldEventType = 1;
  static constexpr int kEnumEventTypeStart = 0;
  static constexpr int kFieldEventGroup = 4;
// for global ID: locations
  static constexpr int kFieldLocationName = 0;
  static constexpr int kFieldLocLatitude = 1;
  static constexpr int kFieldLocLongitude = 2;
  static constexpr int kEnumLocationIcon = 3;
  static constexpr int kFieldLocAltitude = 4;
  static constexpr int kFieldLocationDescription = 6;
// for global ID: course
  static constexpr int kFieldSport = 4;
  static constexpr int kFieldName = 5;
// for global ID: course point
  static constexpr int kFieldCPTimeStamp = 1;
  static constexpr int kFieldCPPositionLat = 2;
  static constexpr int kFieldCPPositionLong = 3;
  static constexpr int kFieldCPDistance = 4;
  static constexpr int kFieldCPName = 6;
  static constexpr int kFieldCPType = 5;

// For developer fields as a non conflicting id
  static constexpr int kFieldInvalid = 255;

// types for message definitions
  static constexpr int kTypeEnum = 0x00;
  static constexpr int kTypeUint8 = 0x02;
  static constexpr int kTypeString = 0x07;
  static constexpr int kTypeUint16 = 0x84;
  static constexpr int kTypeSint32 = 0x85;
  static constexpr int kTypeUint32 = 0x86;

// misc. constants for message fields
  static constexpr int kFileCourse = 0x06;
  static constexpr int kEventTimer = 0x00;
  static constexpr int kEventTypeStart = 0x00;
  static constexpr int kEventTypeStopDisableAll = 0x09;
  static constexpr int kCoursePointTypeGeneric = 0x00;
  static constexpr int kCoursePointTypeLeft = 0x06;
  static constexpr int kCoursePointTypeRight = 0x07;

  static constexpr int kWriteHeaderLen = 12;
  static constexpr int kWriteHeaderCrcLen = 14;
  static constexpr int kReadHeaderCrcLen = 14;

  static constexpr double kSynthSpeed = 10.0 * 1000 / 3600; /* speed in m/s */

  /* Member Functions */

  void fit_parse_header();
  uint8_t fit_getuint8();
  uint16_t fit_getuint16();
  uint32_t fit_getuint32();
  QString fit_getstring(int size);
  void fit_parse_definition_message(uint8_t header);
  QVariant fit_read_field(const fit_field_t& f);
  void fit_parse_data(const fit_message_def& def, int time_offset);
  void fit_parse_data_message(uint8_t header);
  void fit_parse_compressed_message(uint8_t header);
  void fit_parse_record();
  void fit_check_file_crc() const;
  void fit_write_message_def(uint8_t local_id, uint16_t global_id, const std::vector<fit_field_t>& fields) const;
  static uint16_t fit_crc16(uint8_t data, uint16_t crc);
  void fit_write_timestamp(const gpsbabel::DateTime& t) const;
  void fit_write_fixed_string(const QString& s, unsigned int len) const;
  void fit_write_position(double pos) const;
  void fit_write_msg_file_id(uint8_t type, uint16_t manufacturer, uint16_t product, const gpsbabel::DateTime& time_created) const;
  void fit_write_msg_course(const QString& name, uint8_t sport) const;
  void fit_write_msg_lap(const gpsbabel::DateTime& timestamp, const gpsbabel::DateTime& start_time, double start_position_lat, double start_position_long, double end_position_lat, double end_position_long, uint32_t total_elapsed_time_s, double total_distance_m, double avg_speed_ms, double max_speed_ms) const;
  void fit_write_msg_event(const gpsbabel::DateTime& timestamp, uint8_t event, uint8_t event_type, uint8_t event_group) const;
  void fit_write_msg_course_point(const gpsbabel::DateTime& timestamp, double position_lat, double position_long, double distance_m, const QString& name, uint8_t type) const;
  void fit_write_msg_record(const gpsbabel::DateTime& timestamp, double position_lat, double position_long, double distance_m, double altitude, double speed_ms) const;
  void fit_write_file_header(uint32_t file_size, uint16_t crc) const;
  void fit_write_header_msgs(const gpsbabel::DateTime& ctime, const QString& name) const;
  void fit_write_file_finish() const;
  void fit_collect_track_hdr(const route_head* rte);
  void fit_collect_trackpt(const Waypoint* waypointp);
  void fit_collect_track_tlr(const route_head* rte);
  void fit_collect_waypt(const Waypoint* waypointp);

  /* Data Members */

  OptionCString opt_allpoints;
  OptionCString opt_recoverymode;
  int lap_ct = 0;
  bool new_trkseg = false;
  bool write_header_msgs = false;

  QVector<arglist_t> fit_args = {
    {
      "allpoints", &opt_allpoints,
      "Read all points even if latitude or longitude is missing",
      nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    {
      "recoverymode", &opt_recoverymode,
      "Attempt to recovery data from corrupt file",
      nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
  };

  const std::vector<std::pair<QString, int> > kCoursePointTypeMapping = {
    {"left", kCoursePointTypeLeft},
    {"links", kCoursePointTypeLeft},
    {"gauche", kCoursePointTypeLeft},
    {"izquierda", kCoursePointTypeLeft},
    {"sinistra", kCoursePointTypeLeft},

    {"right", kCoursePointTypeRight},
    {"rechts", kCoursePointTypeRight},
    {"droit", kCoursePointTypeRight},
    {"derecha", kCoursePointTypeRight},
    {"destro", kCoursePointTypeRight},
  };

  fit_data_t fit_data;

  std::deque<FitCourseRecordPoint> course, waypoints;

  gbfile* fin{nullptr};
  gbfile* fout{nullptr};

  /*******************************************************************************
  * FIT writing
  *******************************************************************************/

  const std::vector<fit_field_t> fit_msg_fields_file_id = {
    // field id,            size, type
    { kFieldType,           0x01, kTypeEnum   },
    { kFieldManufacturer,   0x02, kTypeUint16 },
    { kFieldProduct,        0x02, kTypeUint16 },
    { kFieldTimeCreated,    0x04, kTypeUint32 },
  };
  const std::vector<fit_field_t> fit_msg_fields_course = {
    { kFieldName,           0x10, kTypeString },
    { kFieldSport,          0x01, kTypeEnum   },
  };
  const std::vector<fit_field_t> fit_msg_fields_lap = {
    { kFieldTimestamp,      0x04, kTypeUint32 },
    { kFieldStartTime,      0x04, kTypeUint32 },
    { kFieldStartLatitude,  0x04, kTypeSint32 },
    { kFieldStartLongitude, 0x04, kTypeSint32 },
    { kFieldEndLatitude,    0x04, kTypeSint32 },
    { kFieldEndLongitude,   0x04, kTypeSint32 },
    { kFieldElapsedTime,    0x04, kTypeUint32 },
    { kFieldTotalTimerTime, 0x04, kTypeUint32 },
    { kFieldTotalDistance,  0x04, kTypeUint32 },
    { kFieldAvgSpeed,       0x02, kTypeUint16 },
    { kFieldMaxSpeed,       0x02, kTypeUint16 },
  };
  const std::vector<fit_field_t> fit_msg_fields_event = {
    { kFieldTimestamp,      0x04, kTypeUint32 },
    { kFieldEvent,          0x01, kTypeEnum   },
    { kFieldEventType,      0x01, kTypeEnum   },
    { kFieldEventGroup,     0x01, kTypeUint8  },
  };
  const std::vector<fit_field_t> fit_msg_fields_course_point = {
    { kFieldCPTimeStamp,    0x04, kTypeUint32 },
    { kFieldCPPositionLat,  0x04, kTypeSint32 },
    { kFieldCPPositionLong, 0x04, kTypeSint32 },
    { kFieldCPDistance,     0x04, kTypeUint32 },
    { kFieldCPName,         0x10, kTypeString },
    { kFieldCPType,         0x01, kTypeEnum   },
  };
  const std::vector<fit_field_t> fit_msg_fields_record = {
    { kFieldTimestamp,      0x04, kTypeUint32 },
    { kFieldLatitude,       0x04, kTypeSint32 },
    { kFieldLongitude,      0x04, kTypeSint32 },
    { kFieldDistance,       0x04, kTypeUint32 },
    { kFieldAltitude,       0x02, kTypeUint16 },
    { kFieldSpeed,          0x02, kTypeUint16 },
  };
};
#endif // GARMIN_FIT_H_INCLUDED_
