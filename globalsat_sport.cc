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
    Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111 USA

 */
/*
 * This is the bridge between the GPSBabel and globalsat sport devices
 * gh625XT. Globalsat has a few devices under the sport brand and they
 * are using a similar USB serial protocal.
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


#include "defs.h"
#include "gbser.h"
#include <cctype>
#include <climits>
#include <cstdio>

#define MYNAME "GlobalsatSport"

static void* serial_handle;
static bool isSizeSwapped;

static char* showlist = nullptr;               // if true show a list instead of download tracks
static char* track = nullptr;                     // if not 0 only download this track, if 0 download all

static char* opt_dump_file = nullptr;	            // dump raw data to this file (optional)
static char* opt_input_dump_file = nullptr;    // if true input is from a dump-file instead of serial console
static gbfile* dumpfile = nullptr;             // used for creating bin/RAW datadump files, usefull for testing
static gbfile* in_file = nullptr;              // used for reading from bin/RAW datadump files, usefull for testing

static
arglist_t globalsat_args[] = {
  {"showlist", &showlist, "list tracks", nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr},
  {"track", &track, "get track", "0", ARGTYPE_INT, ARG_NOMINMAX, nullptr},
  {"dump-file", &opt_dump_file, "Dump raw data to this file", nullptr, ARGTYPE_OUTFILE, ARG_NOMINMAX, nullptr},
  {"input-is-dump-file", &opt_input_dump_file, "Dump raw data to this file", nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr},
  ARG_TERMINATOR
};

typedef enum {
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

  ResponseInsuficientMemory = 0x95,
  ResponseResendTrackSection = 0x92,
  ResponseSendTrackFinish = 0x9A
} globalsat_commands_e;

typedef struct tagDATE {
  uint8_t Year;
  uint8_t Month;
  uint8_t Day;
} gh_date;

typedef struct tagTIME {
  uint8_t Hour;
  uint8_t Minute;
  uint8_t Second;
} gh_time;

typedef struct tagTRAINHEADER {
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
} gh_trainheader;

typedef struct tagDB_TRAIN {
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
} gh_db_train;

typedef struct tagDB_LAP {
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
} gh_db_lap;

typedef struct tagRECPOINT {
  uint32_t Latitude;
  uint32_t Longitude;
  int16_t Altitude;
  uint32_t Speed;
  uint8_t HeartRate;
  uint32_t IntervalTime;
  uint16_t Cadence;
  uint16_t PwrCadence;
  uint16_t Power;
} gh_recpoint;

static void
serial_init(const char* fname)
{
  if (serial_handle = gbser_init(fname), nullptr == serial_handle) {
    fatal(MYNAME ": Can't open port '%s'\n", fname);
  }
  if (gbser_set_speed(serial_handle, 115200) != gbser_OK) {
    fatal(MYNAME ": Can't configure port '%s'\n", fname);
  }
  // Toss anything that came in before our speed was set
  gbser_flush(serial_handle);
}

static void
serial_deinit()
{
  if (global_opts.debug_level > 1) {
    printf(MYNAME " serial_deinit()\n");
  }
  gbser_deinit(serial_handle);
  serial_handle = nullptr;
  if (global_opts.debug_level > 1) {
    printf(MYNAME " serial_deinit() Done\n");
  }
}

static int
serial_recv_byte()
{
  /* allow for a delay of 4s */
  int result = gbser_readc_wait(serial_handle, 4000);
  switch (result) {
  case gbser_ERROR:
    fatal("serial_recv_byte(): error reading one byte\n");
    break;
  case gbser_NOTHING:
    fatal("serial_recv_byte(): read timeout\n");
    break;
  }
  return result;
}

static void
serial_write_byte(uint8_t byte)
{
  if (global_opts.debug_level > 1) {
    printf("0x%02x (%d), ", byte, byte);
  }

  int n = gbser_writec(serial_handle, byte);
  if (n == gbser_ERROR) {
    fatal("globalsat_probe_device(): write failed\n");
  }
}

static int
recv_byte()
{
  int result=0;
  // Read from serial or dumpfile
  if (!opt_input_dump_file) {
    result=serial_recv_byte();
  } else {
    int bytes = gbfread(&result, 1, 1, in_file);
    is_fatal((bytes != 1), MYNAME ": read error");
  }
  // Check if byte sould be dumped also into a file
  if (dumpfile) {
    gbfwrite(&result, 1, 1, dumpfile);
  }
  return result;
}

static void
write_byte(uint8_t byte)
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


static void
globalsat_write_package(uint8_t* payload, uint32_t size)
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
    printf("\n");
  }
}

static uint8_t*
globalsat_read_package(int* out_length, uint8_t* out_DeviceCommand)
{
  uint8_t crc;
  uint8_t calc_crc = 0;

  uint8_t DeviceCommand = recv_byte();
  if (global_opts.debug_level > 1) {
    printf("DeviceCommand: 0x%02x ", DeviceCommand);
  }
  uint8_t len_h = recv_byte();
  calc_crc ^= len_h;
  uint8_t len_l = recv_byte();
  calc_crc ^= len_l;

  int length = (len_h << 8) + len_l;
  if (global_opts.debug_level > 1) {
    printf("len=%d Payload:", length);
  }

  uint8_t* payload = (uint8_t*) malloc(length);
  if (payload == nullptr) {
    goto error_out;
  }

  for (int i = 0; i < length; i++) {
    payload[i] = recv_byte();
    calc_crc ^= payload[i];
  }

  crc = recv_byte();
  if (global_opts.debug_level > 1) {
    printf("crc=0x%x should be=0x%x\n", crc, calc_crc);
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
static void
globalsat_send_simple(uint8_t command)
{
  uint8_t payload[1];
  payload[0] = command;
  globalsat_write_package(payload, 1);
}

static void
globalsat_probe_device()
{
  //TODO try this first if fails try with false, to support 561
  isSizeSwapped = false;		//all devices but gh561 since gh561 has swaped size.

  globalsat_send_simple(CommandWhoAmI);

  int len;
  uint8_t DeviceCommand;
  uint8_t* payload = globalsat_read_package(&len, &DeviceCommand);
  if ((len > 0) && (payload != nullptr)) {
    if (global_opts.debug_level > 1) {
      printf("Got package!!!\n");
    }
    //TODO figure out what device it is if we start to support more devices then gh625XT
  }

  if (payload) {
    free(payload);
  }
}

static void
rd_init(const QString& fname)
{
  if (global_opts.debug_level > 1) {
    printf(MYNAME " rd_init()\n");
  }
  if (opt_dump_file) {
    dumpfile = gbfopen(opt_dump_file, "wb", MYNAME);
    if (!dumpfile) {
      printf(MYNAME " rd_init() creating dumpfile %s FAILED continue anyway\n",opt_dump_file);
    } else {
      if (global_opts.debug_level > 1) {
        printf(MYNAME " rd_init() creating dumpfile %s for writing binnary copy of serial stream\n",opt_dump_file);
      }
    }
  }
  if (!opt_input_dump_file) {
    serial_init(qPrintable(fname));
  } else {
    // read from dump-file instead of serial
    in_file = gbfopen(fname, "rb", MYNAME);
    is_fatal(!in_file, "Could not open dumpfile for input: %s", qPrintable(fname));

  }
  globalsat_probe_device();
}

static void
wr_init(const QString& fname)
{
  if (global_opts.debug_level > 1) {
    printf(MYNAME " wr_init()\n");
  }
  serial_init(qPrintable(fname));
}


static void
rd_deinit()
{
  if (global_opts.debug_level > 1) {
    printf(MYNAME " rd_deinit()\n");
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
  if (global_opts.debug_level > 1) {
    printf(MYNAME " rd_deinit() Done\n");
  }
}

static void
wr_deinit()
{
  if (global_opts.debug_level > 1) {
    printf(MYNAME " wr_deinit()\n");
  }
  serial_deinit();
}

static void track_read();


static void
waypoint_read()
{
  if (global_opts.debug_level > 1) {
    printf(MYNAME "   waypoint_read()\n");
  }
  //CommandGetTrackFileHeaders
  globalsat_send_simple(CommandGetWaypoints);

  int len;
  uint8_t DeviceCommand;
  uint8_t* in_payload = globalsat_read_package(&len, &DeviceCommand);
  if ((len > 0) && (in_payload != nullptr)) {
    if (global_opts.debug_level > 1) {
      printf("Got package!!!\n");
    }
  }
  if (in_payload) {
    free(in_payload);
  }
  track_read();
}

static void
track_read()
{
  if (global_opts.debug_level > 1) {
    printf(MYNAME "   track_read()\n");
  }
  //CommandGetTrackFileHeaders
  globalsat_send_simple(CommandGetTrackFileHeaders);
  if (global_opts.debug_level > 1) {
    printf("Sent...\n");
  }

  int length;
  uint8_t DeviceCommand;
  uint8_t* payload = globalsat_read_package(&length, &DeviceCommand);
  if ((length > 0) && (payload != nullptr)) {
    if (global_opts.debug_level > 1) {
      printf("Got package!!! headers\n");
    }
    //payload is packed with a number of trainingheaders with the size of 29bytes each
    int number_headers = length / 29;	//29=packed sizeof(gh_trainheader)
    if (global_opts.debug_level > 1) {
      printf("length=%d sizeof(gh_trainheader)=%d number_headers=%d\n", length, 29, number_headers);
    }

    for (int i = 0; i < number_headers; i++) {
      int pos = i * 29; //29=packed sizeof(gh_trainheader)
      uint8_t* hdr = & (payload[pos]);
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

      if (showlist || global_opts.debug_level > 1) {
        printf("Track[%02i]: %02d-%02d-%02d ", i,header.dateStart.Year,header.dateStart.Month, header.dateStart.Day);
        printf("%02d:%02d:%02d ", header.timeStart.Hour,header.timeStart.Minute, header.timeStart.Second);
        int time_s=header.TotalTime / 10;
        int time_h=time_s/(60*60);
        time_s-=time_h*(60*60);
        int time_m=time_s/60;
        time_s-=time_m*60;
        printf("Points:%6d Time:%02d:%02d:%02d Dist:%9dm LapCnts:%5d ",	header.TotalPoint, time_h,time_m,time_s, header.TotalDistance, header.LapCnts);
        printf("Index/StartPt:%d ", header.gh_ptrec.Index);
        printf("LapIndex/EndPt:%d ", header.gh_laprec.LapIndex);
        printf("DataType:0x%x\n", header.DataType);
      }

      if (!showlist) {
        route_head* trk = route_head_alloc();

        QString str;
        str.sprintf("%02d-%02d-%02d_%02d:%02d:%02d", header.dateStart.Year, header.dateStart.Month, header.dateStart.Day, header.timeStart.Hour, header.timeStart.Minute, header.timeStart.Second);
        trk->rte_name = str;
        trk->rte_desc = QString("GH625XT GPS tracklog data");

        track_add_head(trk);

        uint8_t GetTrack[5];
        GetTrack[0] = CommandGetTrackFileSections;
        GetTrack[1] = 0x0;
        GetTrack[2] = 0x1;
        GetTrack[3] = (0xFF00 & header.gh_ptrec.Index) >> 8;
        GetTrack[4] = 0xFF & header.gh_ptrec.Index;
        globalsat_write_package(GetTrack, 5);

        uint8_t trackDeviceCommand;
        int track_length;
        uint8_t* track_payload = globalsat_read_package(&track_length, &trackDeviceCommand);
        is_fatal(((track_length == 0) || (track_payload == nullptr)) , "tracklength in 0 bytes or payload nonexistant");
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
          printf("\nTrainData:%02d-%02d-%02d ", db_train.dateStart.Year,db_train.dateStart.Month, db_train.dateStart.Day);
          printf("%02d:%02d:%02d ", db_train.timeStart.Hour, db_train.timeStart.Minute, db_train.timeStart.Second);
          printf("Total(points:%6d time:%6ds dist:%9dm) LapCnts:%5d ", db_train.TotalPoint,db_train.TotalTime / 10,db_train.TotalDistance, db_train.LapCnts);
          printf("Index/StartPt:%d ", db_train.gh_ptrec.Index);
          printf("LapIndex/EndPt:%d ", db_train.gh_laprec.LapIndex);
          printf("MultiSport:0x%x ", db_train.MultiSport);
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
          is_fatal(((track_length == 0) || (track_payload == nullptr)), "tracklength in 0 bytes or payload nonexistant");
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
            printf("Lap Trainheader: %02d-%02d-%02d ", header.dateStart.Year, header.dateStart.Month, header.dateStart.Day);
            printf("%02d:%02d:%02d ", header.timeStart.Hour, header.timeStart.Minute, header.timeStart.Second);
            printf("Total(points:%6d time:%6ds dist:%9dm) LapCnts:%5d ", header.TotalPoint,header.TotalTime / 10, header.TotalDistance, header.LapCnts);
            printf("Index/StartPt:%d ", header.gh_ptrec.Index);
            printf("LapIndex/EndPt:%d ", header.gh_laprec.LapIndex);
            printf("DataType:0x%x\n", header.DataType);
          }

          /*
           * GPS year: 2000+; struct tm year: 1900+
           * GPS month: 1-12, struct tm month: 0-11
           */

          QDate gpsDate = QDate(header.dateStart.Year+2000,header.dateStart.Month,header.dateStart.Day);
          QTime gpsTime = QTime(header.timeStart.Hour-2,header.timeStart.Minute,header.timeStart.Second);
          gpsDateTime = gpsbabel::DateTime(gpsDate,gpsTime);
          gpsDateTime.setTimeSpec(Qt::UTC);

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
              printf("     lap[%d] AccruedTime:%ds TotalTime:%ds TotalDist:%dm", lap, db_lap.AccruedTime, db_lap.TotalTime / 10, db_lap.TotalDistance);
              printf(" Calory:%d MaxSpeed:%d Hearth max:%d avg:%d ", db_lap.Calory, db_lap.MaxSpeed, db_lap.MaxHeart, db_lap.AvgHeart);
              printf(" Alt min:%d max:%d", db_lap.MinAlti, db_lap.MaxAlti);
              printf(" Cadns avg:%d best:%d", db_lap.AvgCadns, db_lap.BestCadns);
              printf(" Power avg:%d Max:%d", db_lap.AvgPower, db_lap.MaxPower);
              printf(" MultisportIndex:%d", db_lap.MultiSportIndex);
              printf(" StartPt:%d EndPt:%d\n", db_lap.StartPt, db_lap.EndPt);
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
            header.gh_ptrec.StartPt = be_read32(hdr+20);
            header.gh_laprec.EndPt = be_read32(hdr+24);
            header.DataType = hdr[28];


            if (global_opts.debug_level > 1) {
              printf("Lap Trainheader: %02d-%02d-%02d ", header.dateStart.Year, header.dateStart.Month, header.dateStart.Day);
              printf("%02d:%02d:%02d ", header.timeStart.Hour, header.timeStart.Minute, header.timeStart.Second);
              printf("Total(points:%6d time:%6ds dist:%9dm) LapCnts:%5d ", header.TotalPoint, header.TotalTime / 10, header.TotalDistance, header.LapCnts);
              printf("StartPt:%d ", header.gh_ptrec.StartPt);
              printf("EndPt:%d ", header.gh_laprec.EndPt);
              printf("DataType:0x%x\n", header.DataType);
            }

            int recpoints_in_package = header.gh_laprec.EndPt - header.gh_ptrec.StartPt + 1;
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

              //Time from last point in sec's * 10 (e.g. point.lntervalTime is sec multiplied witn 10)
              // convert to milisecs
              gpsbabel::DateTime gpsbabeltime = gpsDateTime.addMSecs(point.IntervalTime*100);
              gpsbabeltime.setTimeSpec(Qt::UTC);
              gpsDateTime.setDate(gpsbabeltime.date());
              gpsDateTime.setTime(gpsbabeltime.time());

              // if (global_opts.debug_level > 1) {
              //   qDebug() << "DateTime2:" << gpsDateTime.toString();
              // }
              if (global_opts.debug_level > 1) {
                printf("     recpoint[%2d] Lat:%f Long:%f Alt:%dm", recpoint,(double)((int32_t) point.Latitude) / 1000000.0,(double)((int32_t) point.Longitude) / 1000000.0, point.Altitude);
                printf(" Speed:%f HR:%d",(double) point.Speed / 100, point.HeartRate);
                printf(" Time:%d Cadence:%d", point.IntervalTime, point.Cadence);
                printf(" PwrCadense:%d Power:%d\n", point.PwrCadence,point.Power);
              }

              Waypoint* wpt = new Waypoint(); // waypt_new();
              //wpt->creation_time = mkgmtime(&gpstime);
              wpt->SetCreationTime(gpsbabeltime);
              wpt->longitude = ((int32_t) point.Longitude) / 1000000.0;
              wpt->latitude = ((int32_t) point.Latitude) / 1000000.0;
              wpt->altitude = point.Altitude;
              wpt->speed = ((double) point.Speed / 100.0) * 1000.0 / 3600.0;
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

static void
route_read()
{
  if (global_opts.debug_level > 1) {
    printf(MYNAME "   route_read() TODO\n");
  }
}

static void
data_read()
{
  if (global_opts.debug_level > 1) {
    printf(MYNAME " data_read()\n");
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
    fatal(MYNAME ": Nothing to do.\n");
  }
}

// This used the serial comunication to the watch
ff_vecs_t globalsat_sport_vecs = {
  ff_type_serial,		//type
  FF_CAP_RW_ALL,		//cap[3]
  rd_init,			//rd_init
  wr_init,			//wr_init
  rd_deinit,			//rd_deinit
  wr_deinit,			//wr_deinit
  data_read,			//read
  nullptr,			//write
  nullptr,				//exit
  globalsat_args,		//args
  CET_CHARSET_ASCII, 0		//encode,fixed_encode
  //NULL                   //name dynamic/internal?
  , NULL_POS_OPS,
  nullptr
};

// This reads from a RAW dump bile from a watch
// Usefull for testing generata a dumpfile with
// gpsbabel -i globalsat,dump-file=<dumpfilename> -f /dev/ttyUSB0 -o gpx,garminextensions -F <1:st gpx file name>
// gpsbabel -i globalsat-bin -f <dumpfilename> -o gpx,garminextensions -F <2:nd gpx file name>
ff_vecs_t globalsat_sport_fvecs = {
  ff_type_serial,		//type
  FF_CAP_RW_ALL,		//cap[3]
  rd_init,			//rd_init
  wr_init,			//wr_init
  rd_deinit,			//rd_deinit
  wr_deinit,			//wr_deinit
  data_read,			//read
  nullptr,			//write
  nullptr,				//exit
  globalsat_args,		//args
  CET_CHARSET_ASCII, 0		//encode,fixed_encode
  //NULL                   //name dynamic/internal?
  , NULL_POS_OPS,
  nullptr
};
