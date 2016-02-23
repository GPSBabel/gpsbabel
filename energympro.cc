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
    Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111 USA

 */

#include "defs.h"
#include <QtCore/QDebug>
#include <limits.h>

#define MYNAME "energympro"

static gbfile* file_in;

typedef struct tagDATE
{
  uint8_t Year;
  uint8_t Month;
  uint8_t Day;
} tw_date;

typedef struct tagTIME
{
  uint8_t Hour;
  uint8_t Minute;
  uint8_t Second;
} tw_time;

typedef struct Workout
    {
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
    } tw_workout;


typedef struct Point
    {
    uint32_t  Latitude;
    uint32_t  Longitude;
    int16_t   Altitude;
    uint16_t  reserved1;
    uint32_t  Speed;
    uint16_t  IntervalDist;          // Interval Distance
    uint16_t  reserved2;
    uint32_t  lntervalTime;          // Interval time
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
    } tw_point;

typedef struct Lap
    {
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
    } tw_lap;


//*******************************************************************************
//           local helper functions
//*******************************************************************************
static void 
read_point(route_head* gpsbabel_route,gpsbabel::DateTime& gpsDateTime)
{
  tw_point point;
  gbfread(&point,sizeof(tw_point),1,file_in);
  if (global_opts.debug_level > 1) {
    printf ("Point: lat:%8d long:%8d alt:%8d ",point.Latitude,point.Longitude,point.Altitude);
    printf ("speed:%6d dist:%5d time:%5d Status:%1d", point.Speed,point.IntervalDist,point.lntervalTime,point.Status); 
    printf ("HR:(%3d,%1d)" , point.HR_Heartrate,point.HR_Status);
    printf ("Speed:(%8d,%1d)" , point.Speed_Speed,point.Speed_Status);
    printf ("Cad:(%3d,%1d)" , point.Cadence_Cadence, point.Cadence_Status);
    printf ("Power (Cad:%6d Pow:%6d,%2d)Temp:%3d\n" , point.Power_Cadence, point.Power_Power, point.Power_Status, point.Temp);

    qDebug() << "DateTime1:" << gpsDateTime.toString();
    qDebug() << "point.lntervalTime:" << point.lntervalTime;
  }

  //Time from last point in sec's * 10 (e.g. point.lntervalTime is sec multiplied witn 10)
  // convert to milisecs
  gpsbabel::DateTime gpsbabeltime = gpsDateTime.addMSecs(point.lntervalTime*100);
  gpsDateTime.setDate(gpsbabeltime.date());
  gpsDateTime.setTime(gpsbabeltime.time());

  //remove parts of sec (on purpose) on reported time, we keep track of parts of sec in 
  // global structure so we don't drift
  qint64 mSecsSinceEpoc = gpsbabeltime.toMSecsSinceEpoch();
  gpsbabeltime.setMSecsSinceEpoch(mSecsSinceEpoc-mSecsSinceEpoc%1000);

  Waypoint* waypt;
  waypt = new Waypoint;
  waypt->latitude = (point.Latitude / (double)1000000);
  waypt->longitude = (point.Longitude / (double)1000000);
  waypt->altitude = point.Altitude;

  if (global_opts.debug_level > 1) {
    qDebug() << "DateTime2:" << gpsDateTime.toString();
  }

  waypt->SetCreationTime(gpsbabeltime);

  if (point.Speed_Status == 0) {
    WAYPT_SET(waypt, speed, point.Speed_Speed / 100.0f);
  }
  if (point.HR_Status == 0) {
    waypt->heartrate = point.HR_Heartrate;
  }
  if (point.Cadence_Status == 0) {
    waypt->cadence = point.Cadence_Cadence;
  }
  if (point.Power_Status == 0) {
    waypt->power = point.Power_Power;
  }
  WAYPT_SET(waypt, temperature, point.Temp);
  track_add_wpt(gpsbabel_route, waypt);
}


static void 
read_lap(void)
{
  tw_lap lap;
  gbfread(&lap,sizeof(tw_lap),1,file_in);
  if (global_opts.debug_level > 1) {
    printf ("LAP: splitTime:%6ds TotalTime:%6ds LapNumber:%5d ",lap.splitTime/10,lap.TotalTime/10,lap.Number);
    printf ("dist:%08dm Cal:%5d Speed:(%6d,%6d) ", lap.lDistance,lap.Calorie,lap.MaxSpeed,lap.AvgSpeed);
    printf ("HR:(%3d,%3d)" , lap.MaxHeartrate,lap.AvgHeartrate);
    printf ("Alt:(%6d,%6d) ", lap.MinAlti,lap.MaxAlti);
    printf ("Cad:(%3d,%3d) ", lap.AvgCad,lap.MaxCad);
    printf ("Power:(%3d,%3d)w ", lap.AvgPower,lap.MaxPower);
    printf ("Pt:(%6d,%6d)\n", lap.StartRecPt,lap.FinishRecPt);
  }
}

//*******************************************************************************
//           global callbacks called by gpsbabel main process
//*******************************************************************************

static void
rd_init(const QString& fname)
{
  if (global_opts.debug_level > 1) {
    printf (MYNAME " rd_deinit()\n");
  }
  gbfile* fileorg_in = gbfopen(fname, "rb", MYNAME);

  /* copy file to memory stream (needed for seek-ops and piped commands) */
  file_in = gbfopen(NULL, "wb", MYNAME);
  gbsize_t size;
  size = gbfcopyfrom(file_in, fileorg_in, 0x7FFFFFFF);
  if(global_opts.debug_level > 1) {
    printf (MYNAME "  filesize=%d\n",size);
  }
  gbfclose(fileorg_in);
}

static void
rd_deinit(void)
{
  if (global_opts.debug_level > 1) {
    printf (MYNAME " rd_deinit()\n");
  }
  gbfclose(file_in);
}

static void 
track_read(void)
{
  if(global_opts.debug_level > 1) {
    printf (MYNAME "  waypoint_read()\n");
  }

  gbfseek(file_in, 0L, SEEK_END);
  gbfseek(file_in, (int32_t) -(sizeof(tw_workout)), SEEK_CUR);
  tw_workout workout;
  workout.dateStart.Year = gbfgetc(file_in);
  workout.dateStart.Month = gbfgetc(file_in);
  workout.dateStart.Day = gbfgetc(file_in);
  workout.timeStart.Hour = gbfgetc(file_in);
  workout.timeStart.Minute = gbfgetc(file_in);
  workout.timeStart.Second = gbfgetc(file_in);
  workout.TotalRecPt = gbfgetint16(file_in); 
  workout.TotalTime = gbfgetint32(file_in);
  workout.TotalDist = gbfgetint32(file_in);
  workout.LapNumber = gbfgetint16(file_in);
  workout.Calory = gbfgetint16(file_in);
  workout.MaxSpeed = gbfgetint32(file_in);
  workout.AvgSpeed = gbfgetint32(file_in);
  workout.MaxHeart = gbfgetc(file_in);
  workout.AvgHeart = gbfgetc(file_in);

  if (global_opts.debug_level > 1) {
    printf ("%04d-%02d-%02d ", workout.dateStart.Year+2000,workout.dateStart.Month, workout.dateStart.Day);
    printf ("%02d:%02d:%02d ", workout.timeStart.Hour,workout.timeStart.Minute, workout.timeStart.Second);
    printf ("Total(RecPt:%6d Time:%6ds Dist:%9dm) LapNumber:%5d \n",workout.TotalRecPt,workout.TotalTime/10, workout.TotalDist, workout.LapNumber);
  }

  /*
   * GPS year: 2000+; struct tm year: 1900+
   */
  QDate gpsDate = QDate(workout.dateStart.Year+2000,workout.dateStart.Month,workout.dateStart.Day);
  QTime gpsTime = QTime(workout.timeStart.Hour,workout.timeStart.Minute,workout.timeStart.Second);
  gpsbabel::DateTime gpsDateTime = gpsbabel::DateTime(gpsDate,gpsTime);
  gpsDateTime.setTimeSpec(Qt::UTC);
  route_head* gpsbabel_route = route_head_alloc();
  
  track_add_head(gpsbabel_route);
  gbfseek(file_in, 0L, SEEK_SET);

  for(int point=0;point<workout.TotalRecPt;point++) {
    read_point(gpsbabel_route,gpsDateTime);
  }

  gbfseek(file_in, sizeof(tw_point)*(workout.TotalRecPt), SEEK_SET);
  for(int lap=0;lap<workout.LapNumber;lap++) {
    read_lap();
  }
}

static void
data_read(void)
{
  if (global_opts.debug_level > 1) {
    printf (MYNAME " data_read()\n");
  }

  track_read();
}


ff_vecs_t energympro_vecs = {
  ff_type_file,
  {
    ff_cap_none,  // waypoints
    ff_cap_read,  // tracks
    ff_cap_none   // routes
  },
		rd_init,      // rd_init
		NULL,         // wr_init
		rd_deinit,    // rd_deinit
		NULL,         // wr_deinit
		data_read,    // read
		NULL,         // write
		NULL,         // exit
		NULL,         //args
		CET_CHARSET_ASCII, 0  //encode,fixed_encode
		//NULL                //name dynamic/internal?
};
