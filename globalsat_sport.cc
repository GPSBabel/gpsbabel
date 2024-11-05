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

#include <cstdint>
#include <cstdlib>              // for free, malloc

#include <QByteArray>           // for QByteArray
#include <QDate>                // for QDate
#include <QDateTime>            // for QDateTime
#include <QString>              // for QString
#include <QTime>                // for QTime
#include <QTimeZone>            // for QTimeZone
#include <QtGlobal>             // for qPrintable

#include "defs.h"
#include "globalsat_sport.h"
#include "gbfile.h"             // for gbfclose, gbfopen, gbfread, gbfwrite, gbfile
#include "gbser.h"              // for gbser_deinit, gbser_flush, gbser_init, gbser_readc_wait, gbser_set_speed, gbser_writec, gbser_ERROR, gbser_NOTHING, gbser_OK
#include "src/core/datetime.h"  // for DateTime


void
GlobalsatSportFormat::serial_init(const char* fname)
{
  if (serial_handle = gbser_init(fname), nullptr == serial_handle) {
    gbFatal("Can't open port '%s'\n", fname);
  }
  if (gbser_set_speed(serial_handle, 115200) != gbser_OK) {
    gbFatal("Can't configure port '%s'\n", fname);
  }
  // Toss anything that came in before our speed was set
  gbser_flush(serial_handle);
}

void
GlobalsatSportFormat::serial_deinit()
{
  if (global_opts.debug_level > 1) {
    gbLog("serial_deinit()\n");
  }
  gbser_deinit(serial_handle);
  serial_handle = nullptr;
  if (global_opts.debug_level > 1) {
    gbLog("serial_deinit() Done\n");
  }
}

int
GlobalsatSportFormat::serial_recv_byte() const
{
  /* allow for a delay of 4s */
  int result = gbser_readc_wait(serial_handle, 4000);
  switch (result) {
  case gbser_ERROR:
    gbFatal("serial_recv_byte(): error reading one byte\n");
    break;
  case gbser_NOTHING:
    gbFatal("serial_recv_byte(): read timeout\n");
    break;
  }
  return result;
}

void
GlobalsatSportFormat::serial_write_byte(uint8_t byte)
{
  if (global_opts.debug_level > 1) {
    gbLog("0x%02x (%d), ", byte, byte);
  }

  int n = gbser_writec(serial_handle, byte);
  if (n == gbser_ERROR) {
    gbFatal("globalsat_probe_device(): write failed\n");
  }
}

int
GlobalsatSportFormat::recv_byte()
{
  int result=0;
  // Read from serial or dumpfile
  if (!opt_input_dump_file) {
    result=serial_recv_byte();
  } else {
    result = gbfgetc(in_file);
    if (result < 0) {
      gbFatal("read error");
    }
  }
  // Check if byte should be dumped also into a file
  if (dumpfile) {
    gbfputc(result, dumpfile);
  }
  return result;
}

void
GlobalsatSportFormat::write_byte(uint8_t byte)
{
  // Write serial or not at all if input is a dumpfile
  if (!opt_input_dump_file) {
    serial_write_byte(byte);
  }
  //else {
  // Do nothing as commands are not used when dumpfile is used instead
  // of serial device
  //}
}


void
GlobalsatSportFormat::globalsat_write_package(uint8_t* payload, uint32_t size)
{
  //All globalsat devices but gh561
  //2 <len_h> <len_l> <payload...> <crc>
  // gh561 (isSizeSwapped)
  //2 <len_l> <len_h> <payload...> <crc>

  uint8_t crc = 0;
  write_byte(2);

  if (!isSizeSwapped) {
    write_byte((0xff00 & size) >> 8);
    crc ^= (0xff00 & size) >> 8;
    write_byte(0xff & size);
    crc ^= (0xff & size);
  } else {
    write_byte(0xff & size);
    crc ^= (0xff & size);
    write_byte((0xff00 & size) >> 8);
    crc ^= (0xff00 & size) >> 8;
  }

  if (payload != nullptr) {
    for (uint32_t i = 0; i < size; i++) {
      write_byte(payload[i]);
      crc ^= payload[i];
    }
  }
  write_byte(crc);
  if (global_opts.debug_level > 1) {
    gbLog("\n");
  }
}

uint8_t*
GlobalsatSportFormat::globalsat_read_package(int* out_length, uint8_t* out_DeviceCommand)
{
  uint8_t crc;
  uint8_t calc_crc = 0;

  uint8_t DeviceCommand = recv_byte();
  if (global_opts.debug_level > 1) {
    gbLog("DeviceCommand: 0x%02x ", DeviceCommand);
  }
  uint8_t len_h = recv_byte();
  calc_crc ^= len_h;
  uint8_t len_l = recv_byte();
  calc_crc ^= len_l;

  int length = (len_h << 8) + len_l;
  if (global_opts.debug_level > 1) {
    gbLog("len=%d Payload:", length);
  }

  auto* payload = (uint8_t*) malloc(length);
  if (payload == nullptr) {
    goto error_out;
  }

  for (int i = 0; i < length; i++) {
    payload[i] = recv_byte();
    calc_crc ^= payload[i];
  }

  crc = recv_byte();
  if (global_opts.debug_level > 1) {
    gbLog("crc=0x%x should be=0x%x\n", crc, calc_crc);
  }
  if (crc == calc_crc) {
    *out_DeviceCommand = DeviceCommand;
    *out_length = length;
    return payload;
  }
  //crc error
  free(payload);
error_out:
  *out_length = 0;
  return nullptr;
}

/*
 * Send one byte package
 */
void
GlobalsatSportFormat::globalsat_send_simple(uint8_t command)
{
  uint8_t payload[1];
  payload[0] = command;
  globalsat_write_package(payload, 1);
}

void
GlobalsatSportFormat::globalsat_probe_device()
{
  //TODO try this first if fails try with false, to support 561
  isSizeSwapped = false;		//all devices but gh561 since gh561 has swapped size.

  globalsat_send_simple(CommandWhoAmI);

  int len;
  uint8_t DeviceCommand;
  uint8_t* payload = globalsat_read_package(&len, &DeviceCommand);
  if ((len > 0) && (payload != nullptr)) {
    if (global_opts.debug_level > 1) {
      gbLog("Got package!!!\n");
    }
    //TODO figure out what device it is if we start to support more devices then gh625XT
  }

  if (payload) {
    free(payload);
  }
}

void
GlobalsatSportFormat::rd_init(const QString& fname)
{
  if (global_opts.debug_level > 1) {
    gbLog("rd_init()\n");
  }
  if (opt_dump_file) {
    dumpfile = gbfopen(opt_dump_file, "wb");
    if (!dumpfile) {
      gbWarning("rd_init() creating dumpfile %s FAILED continue anyway\n", gbLogCStr(opt_dump_file));
    } else {
      if (global_opts.debug_level > 1) {
        gbLog("rd_init() creating dumpfile %s for writing binary copy of serial stream\n", gbLogCStr(opt_dump_file));
      }
    }
  }
  if (!opt_input_dump_file) {
    serial_init(qPrintable(fname));
  } else {
    // read from dump-file instead of serial
    in_file = gbfopen(fname, "rb");
    if (!in_file) {
      gbFatal("Could not open dumpfile for input: %s", gbLogCStr(fname));
    }

  }
  if (opt_timezone) {
    if (QTimeZone::isTimeZoneIdAvailable(opt_timezone.get().toUtf8())) {
      timezn = new QTimeZone(opt_timezone.get().toUtf8());
    } else {
      list_timezones();
      gbFatal("Requested time zone \"%s\" not available.\n", gbLogCStr(opt_timezone));
    }
  } else {
    timezn = nullptr;
  }
  globalsat_probe_device();
}

void
GlobalsatSportFormat::rd_deinit()
{
  if (global_opts.debug_level > 1) {
    gbLog("rd_deinit()\n");
  }
  if (!opt_input_dump_file) {
    serial_deinit();
  } else {
    if (in_file) {
      gbfclose(in_file);
    }
  }
  if (dumpfile) {
    gbfclose(dumpfile);
    dumpfile = nullptr;
  }
  if (timezn != nullptr) {
    delete timezn;
    timezn = nullptr;
  }
  if (global_opts.debug_level > 1) {
    gbLog("rd_deinit() Done\n");
  }
}

void
GlobalsatSportFormat::waypoint_read()
{
  if (global_opts.debug_level > 1) {
    gbLog("  waypoint_read()\n");
  }
  //CommandGetTrackFileHeaders
  globalsat_send_simple(CommandGetWaypoints);

  int len;
  uint8_t DeviceCommand;
  uint8_t* in_payload = globalsat_read_package(&len, &DeviceCommand);
  if ((len > 0) && (in_payload != nullptr)) {
    if (global_opts.debug_level > 1) {
      gbLog("Got package!!!\n");
    }
  }
  if (in_payload) {
    free(in_payload);
  }
  track_read();
}

void
GlobalsatSportFormat::track_read()
{
  if (global_opts.debug_level > 1) {
    gbLog("  track_read()\n");
  }
  //CommandGetTrackFileHeaders
  globalsat_send_simple(CommandGetTrackFileHeaders);
  if (global_opts.debug_level > 1) {
    gbLog("Sent...\n");
  }

  int length;
  uint8_t DeviceCommand;
  uint8_t* payload = globalsat_read_package(&length, &DeviceCommand);
  if ((length > 0) && (payload != nullptr)) {
    if (global_opts.debug_level > 1) {
      gbLog("Got package!!! headers\n");
    }
    //payload is packed with a number of trainingheaders with the size of 29bytes each
    int number_headers = length / 29;	//29=packed sizeof(gh_trainheader)
    if (global_opts.debug_level > 1) {
      gbLog("length=%d sizeof(gh_trainheader)=%d number_headers=%d\n", length, 29, number_headers);
    }

    for (int i = 0; i < number_headers; i++) {
      int pos = i * 29; //29=packed sizeof(gh_trainheader)
      uint8_t* th_hdr = & (payload[pos]);
      gh_trainheader th_header;
      th_header.dateStart.Year = th_hdr[0];
      th_header.dateStart.Month = th_hdr[1];
      th_header.dateStart.Day = th_hdr[2];
      th_header.timeStart.Hour = th_hdr[3];
      th_header.timeStart.Minute = th_hdr[4];
      th_header.timeStart.Second = th_hdr[5];
      th_header.TotalPoint = be_read32(th_hdr+6);
      th_header.TotalTime = be_read32(th_hdr+10);
      th_header.TotalDistance = be_read32(th_hdr+14);
      th_header.LapCnts = be_read16(th_hdr+18);
      th_header.gh_ptrec.Index = be_read32(th_hdr+20);
      th_header.gh_laprec.LapIndex = be_read32(th_hdr+24);
      th_header.DataType = th_hdr[28];

      if (showlist || global_opts.debug_level > 1) {
        gbLog("Track[%02i]: %02d-%02d-%02d ", i, th_header.dateStart.Year, th_header.dateStart.Month, th_header.dateStart.Day);
        gbLog("%02d:%02d:%02d ", th_header.timeStart.Hour, th_header.timeStart.Minute, th_header.timeStart.Second);
        int time_s=th_header.TotalTime / 10;
        int time_h=time_s/(60*60);
        time_s-=time_h*(60*60);
        int time_m=time_s/60;
        time_s-=time_m*60;
        gbLog("Points:%6u Time:%02d:%02d:%02d Dist:%9um LapCnts:%5d ", th_header.TotalPoint, time_h, time_m, time_s, th_header.TotalDistance, th_header.LapCnts);
        gbLog("Index/StartPt:%u ", th_header.gh_ptrec.Index);
        gbLog("LapIndex/EndPt:%u ", th_header.gh_laprec.LapIndex);
        gbLog("DataType:0x%x\n", th_header.DataType);
      }

      if (!showlist) {
        auto* trk = new route_head;

        trk->rte_name = QString::asprintf("%02d-%02d-%02d_%02d:%02d:%02d",
                                          th_header.dateStart.Year,
                                          th_header.dateStart.Month,
                                          th_header.dateStart.Day,
                                          th_header.timeStart.Hour,
                                          th_header.timeStart.Minute,
                                          th_header.timeStart.Second);
        trk->rte_desc = QString("GH625XT GPS tracklog data");

        track_add_head(trk);

        uint8_t GetTrack[5];
        GetTrack[0] = CommandGetTrackFileSections;
        GetTrack[1] = 0x0;
        GetTrack[2] = 0x1;
        GetTrack[3] = (0xFF00 & th_header.gh_ptrec.Index) >> 8;
        GetTrack[4] = 0xFF & th_header.gh_ptrec.Index;
        globalsat_write_package(GetTrack, 5);

        uint8_t trackDeviceCommand;
        int track_length;
        uint8_t* track_payload = globalsat_read_package(&track_length, &trackDeviceCommand);
        if ((track_length == 0) || (track_payload == nullptr)) {
          gbFatal("track length is 0 bytes or payload nonexistent.\n");
        }
        //      printf("Got track package!!! Train data\n");

        uint8_t* dbtrain = track_payload;
        gh_db_train db_train;
        db_train.dateStart.Year = dbtrain[0];
        db_train.dateStart.Month = dbtrain[1];
        db_train.dateStart.Day = dbtrain[2];
        db_train.timeStart.Hour = dbtrain[3];
        db_train.timeStart.Minute = dbtrain[4];
        db_train.timeStart.Second = dbtrain[5];
        db_train.TotalPoint = be_read32(dbtrain+6);
        db_train.TotalTime = be_read32(dbtrain+10);
        db_train.TotalDistance = be_read32(dbtrain+14);
        db_train.LapCnts = be_read16(dbtrain+18);
        db_train.gh_ptrec.Index = be_read32(dbtrain+20);
        db_train.gh_laprec.LapIndex = be_read32(dbtrain+24);
        db_train.MultiSport = dbtrain[28];
        db_train.Calory = be_readu16(dbtrain+29);
        db_train.MaxSpeed = be_read32(dbtrain+31);
        db_train.MaxHeart = dbtrain[35];
        db_train.AvgHeart = dbtrain[36];
        db_train.Ascent = be_readu16(dbtrain+37);
        db_train.Descent = be_readu16(dbtrain+39);
        db_train.MinAlti = be_read16(dbtrain+41);
        db_train.MaxAlti = be_read16(dbtrain+43);
        db_train.AvgCadns = be_readu16(dbtrain+45);
        db_train.BestCadns = be_readu16(dbtrain+47);
        db_train.AvgPower = be_readu16(dbtrain+49);
        db_train.MaxPower = be_readu16(dbtrain+51);
        db_train.Sport1 = dbtrain[53];
        db_train.Sport2 = dbtrain[54];
        db_train.Sport3 = dbtrain[55];
        db_train.Sport4 = dbtrain[56];
        db_train.Sport5 = dbtrain[57];

        if (global_opts.debug_level > 1) {
          gbLog("\nTrainData:%02d-%02d-%02d ", db_train.dateStart.Year, db_train.dateStart.Month, db_train.dateStart.Day);
          gbLog("%02d:%02d:%02d ", db_train.timeStart.Hour, db_train.timeStart.Minute, db_train.timeStart.Second);
          gbLog("Total(points:%6u time:%6us dist:%9um) LapCnts:%5d ", db_train.TotalPoint, db_train.TotalTime / 10, db_train.TotalDistance, db_train.LapCnts);
          gbLog("Index/StartPt:%u ", db_train.gh_ptrec.Index);
          gbLog("LapIndex/EndPt:%u ", db_train.gh_laprec.LapIndex);
          gbLog("MultiSport:0x%x ", db_train.MultiSport);
        }
        int total_laps = db_train.LapCnts;
        int total_laps_left = total_laps;
        free(track_payload);
        track_payload = nullptr;

        gpsbabel::DateTime gpsDateTime;

        // Get laps
        while (total_laps_left > 0) {
          globalsat_send_simple(CommandGetNextTrackSection);
          track_payload = globalsat_read_package(&track_length, &trackDeviceCommand);
          if ((track_length == 0) || (track_payload == nullptr)) {
            gbFatal("track length is 0 bytes or payload nonexistent.\n");
          }
          //	printf("Got track package!!! Laps data\n");

          uint8_t* hdr = track_payload;
          gh_trainheader header;
          header.dateStart.Year = hdr[0];
          header.dateStart.Month = hdr[1];
          header.dateStart.Day = hdr[2];
          header.timeStart.Hour = hdr[3];
          header.timeStart.Minute = hdr[4];
          header.timeStart.Second = hdr[5];
          header.TotalPoint = be_read32(hdr+6);
          header.TotalTime = be_read32(hdr+10);
          header.TotalDistance = be_read32(hdr+14);
          header.LapCnts = be_read16(hdr+18);
          header.gh_ptrec.Index = be_read32(hdr+20);
          header.gh_laprec.LapIndex = be_read32(hdr+24);
          header.DataType = hdr[28];


          if (global_opts.debug_level > 1) {
            gbLog("Lap Trainheader: %02d-%02d-%02d ", header.dateStart.Year, header.dateStart.Month, header.dateStart.Day);
            gbLog("%02d:%02d:%02d ", header.timeStart.Hour, header.timeStart.Minute, header.timeStart.Second);
            gbLog("Total(points:%6u time:%6us dist:%9um) LapCnts:%5d ", header.TotalPoint, header.TotalTime / 10, header.TotalDistance, header.LapCnts);
            gbLog("Index/StartPt:%u ", header.gh_ptrec.Index);
            gbLog("LapIndex/EndPt:%u ", header.gh_laprec.LapIndex);
            gbLog("DataType:0x%x\n", header.DataType);
          }

          /*
           * GPS year: 2000+; std::tm year: 1900+
           * GPS month: 1-12, std::tm month: 0-11
           */

          QDate gpsDate = QDate(header.dateStart.Year+2000, header.dateStart.Month, header.dateStart.Day);
          QTime gpsTime = QTime(header.timeStart.Hour, header.timeStart.Minute, header.timeStart.Second);
          if (timezn != nullptr) {
            gpsDateTime = gpsbabel::DateTime(QDateTime(gpsDate, gpsTime, *timezn).toUTC());
          } else {
            gpsDateTime = gpsbabel::DateTime(QDateTime(gpsDate, gpsTime, QtLocalTime).toUTC());
          }

          int laps_in_package = header.gh_laprec.LapIndex - header.gh_ptrec.Index + 1;
//					printf("Lap Data:\n");
          uint8_t* lap_start_pos = track_payload + 29;	//29=packed sizeof(gh_trainheader)
          for (int lap = 0; lap < laps_in_package; lap++) {
            uint8_t* dblap = (lap_start_pos) + lap * 41;	//  packed sizeof(gh_db_lap)=41
            gh_db_lap db_lap;

            db_lap.AccruedTime = be_read32(dblap+0);
            db_lap.TotalTime = be_read32(dblap+4);
            db_lap.TotalDistance = be_read32(dblap+8);
            db_lap.Calory = be_readu16(dblap+12);
            db_lap.MaxSpeed = be_read32(dblap+14);
            db_lap.MaxHeart = dblap[18];
            db_lap.AvgHeart = dblap[19];
            db_lap.MinAlti = be_read16(dblap+20);
            db_lap.MaxAlti = be_read16(dblap+22);
            db_lap.AvgCadns = be_readu16(dblap+24);
            db_lap.BestCadns = be_readu16(dblap+26);
            db_lap.AvgPower = be_readu16(dblap+28);
            db_lap.MaxPower = be_readu16(dblap+30);
            db_lap.MultiSportIndex = dblap[32];
            db_lap.StartPt = be_read32(dblap+33);
            db_lap.EndPt = be_read32(dblap+37);

            if (global_opts.debug_level > 1) {
              gbLog("     lap[%d] AccruedTime:%us TotalTime:%us TotalDist:%um", lap, db_lap.AccruedTime, db_lap.TotalTime / 10, db_lap.TotalDistance);
              gbLog(" Calory:%d MaxSpeed:%u Hearth max:%d avg:%d ", db_lap.Calory, db_lap.MaxSpeed, db_lap.MaxHeart, db_lap.AvgHeart);
              gbLog(" Alt min:%d max:%d", db_lap.MinAlti, db_lap.MaxAlti);
              gbLog(" Cadns avg:%d best:%d", db_lap.AvgCadns, db_lap.BestCadns);
              gbLog(" Power avg:%d Max:%d", db_lap.AvgPower, db_lap.MaxPower);
              gbLog(" MultisportIndex:%d", db_lap.MultiSportIndex);
              gbLog(" StartPt:%u EndPt:%u\n", db_lap.StartPt, db_lap.EndPt);
            }
          }
          free(track_payload);
          track_payload = nullptr;

          total_laps_left -= laps_in_package;
        }

        globalsat_send_simple(CommandGetNextTrackSection);
        do {
          if (track_payload) {
            // rest of the time in the loop
            free(track_payload);
            globalsat_send_simple(CommandGetNextTrackSection);
          }

          track_payload = globalsat_read_package(&track_length, &trackDeviceCommand);
          if ((track_length > 0) && (track_payload != nullptr)) {
            //	  printf("Got track package!!! Train data\n");
            uint8_t* laptrain_hdr = track_payload;
            gh_trainheader laptrain_header;
            laptrain_header.dateStart.Year = laptrain_hdr[0];
            laptrain_header.dateStart.Month = laptrain_hdr[1];
            laptrain_header.dateStart.Day = laptrain_hdr[2];
            laptrain_header.timeStart.Hour = laptrain_hdr[3];
            laptrain_header.timeStart.Minute = laptrain_hdr[4];
            laptrain_header.timeStart.Second = laptrain_hdr[5];
            laptrain_header.TotalPoint = be_read32(laptrain_hdr+6);
            laptrain_header.TotalTime = be_read32(laptrain_hdr+10);
            laptrain_header.TotalDistance = be_read32(laptrain_hdr+14);
            laptrain_header.LapCnts = be_read16(laptrain_hdr+18);
            laptrain_header.gh_ptrec.StartPt = be_read32(laptrain_hdr+20);
            laptrain_header.gh_laprec.EndPt = be_read32(laptrain_hdr+24);
            laptrain_header.DataType = laptrain_hdr[28];


            if (global_opts.debug_level > 1) {
              gbLog("Lap Trainheader: %02d-%02d-%02d ", laptrain_header.dateStart.Year, laptrain_header.dateStart.Month, laptrain_header.dateStart.Day);
              gbLog("%02d:%02d:%02d ", laptrain_header.timeStart.Hour, laptrain_header.timeStart.Minute, laptrain_header.timeStart.Second);
              gbLog("Total(points:%6u time:%6us dist:%9um) LapCnts:%5d ", laptrain_header.TotalPoint, laptrain_header.TotalTime / 10, laptrain_header.TotalDistance, laptrain_header.LapCnts);
              gbLog("StartPt:%u ", laptrain_header.gh_ptrec.StartPt);
              gbLog("EndPt:%u ", laptrain_header.gh_laprec.EndPt);
              gbLog("DataType:0x%x\n", laptrain_header.DataType);
            }

            int recpoints_in_package = laptrain_header.gh_laprec.EndPt - laptrain_header.gh_ptrec.StartPt + 1;
            //	  printf("Recpoints Data:\n");
            uint8_t* recpoints_start_pos = track_payload + 29;	//29=packed sizeof(gh_trainheader)
            for (int recpoint = 0; recpoint < recpoints_in_package; recpoint++) {
              uint8_t* ghpoint = (recpoints_start_pos + recpoint * 25);	//  packed sizeof(gh_recpoint)=25
              gh_recpoint point;
              point.Latitude = be_read32(ghpoint);
              point.Longitude = be_read32(ghpoint+4);
              point.Altitude = be_read16(ghpoint+8);
              point.Speed = be_read32(ghpoint+10);
              point.HeartRate = ghpoint[14];
              point.IntervalTime = be_read32(ghpoint+15);
              point.Cadence = be_readu16(ghpoint+19);
              point.PwrCadence = be_readu16(ghpoint+21);
              point.Power = be_readu16(ghpoint+23);

              //Time from last point in sec's * 10 (e.g. point.IntervalTime is sec multiplied with 10)
              // convert to millisecs
              gpsDateTime = gpsDateTime.addMSecs(point.IntervalTime*100);

              // if (global_opts.debug_level > 1) {
              //   qDebug() << "DateTime2:" << gpsDateTime.toString();
              // }
              if (global_opts.debug_level > 1) {
                gbLog("     recpoint[%2d] Lat:%f Long:%f Alt:%dm", recpoint, (double)((int32_t) point.Latitude) / 1000000.0, (double)((int32_t) point.Longitude) / 1000000.0, point.Altitude);
                gbLog(" Speed:%f HR:%d", (double) point.Speed / 100, point.HeartRate);
                gbLog(" Time:%u Cadence:%d", point.IntervalTime, point.Cadence);
                gbLog(" PwrCadense:%d Power:%d\n", point.PwrCadence, point.Power);
              }

              auto* wpt = new Waypoint(); // waypt_new();
              wpt->SetCreationTime(gpsDateTime);
              wpt->longitude = ((int32_t) point.Longitude) / 1000000.0;
              wpt->latitude = ((int32_t) point.Latitude) / 1000000.0;
              wpt->altitude = point.Altitude;
              wpt->set_speed(((double) point.Speed / 100.0) * 1000.0 / 3600.0);
              wpt->heartrate = point.HeartRate;
              wpt->cadence = point.Cadence;	//TODO convert in any way??
              wpt->power = point.Power;	//TODO convert in any way??

              track_add_wpt(trk, wpt);
            }
          }
        } while (trackDeviceCommand == CommandGetTrackFileSections);
        if (track_payload) {
          free(track_payload);
        }
      }
    }
  }
  if (payload) {
    free(payload);
  }
}

void
GlobalsatSportFormat::route_read()
{
  if (global_opts.debug_level > 1) {
    gbLog("route_read() TODO\n");
  }
}

void
GlobalsatSportFormat::read()
{
  if (global_opts.debug_level > 1) {
    gbLog("read()\n");
  }

  if (global_opts.masked_objective & WPTDATAMASK) {
    waypoint_read();
  }
  if (global_opts.masked_objective & TRKDATAMASK) {
    track_read();
  }
  if (global_opts.masked_objective & RTEDATAMASK) {
    route_read();
  }
  if (!(global_opts.masked_objective &
        (WPTDATAMASK | TRKDATAMASK | RTEDATAMASK | POSNDATAMASK))) {
    gbFatal("Nothing to do.\n");
  }
}
