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

#include <cstdint>              // for int32_t
#include <cstdio>               // for printf, SEEK_SET, SEEK_CUR, SEEK_END

#include <QtCore/QDate>         // for QDate
#include <QtCore/QDateTime>     // for QDateTime
#include <QtCore/QDebug>        // for QDebug
#include <QtCore/QString>       // for QString
#include <QtCore/QTime>         // for QTime
#include <QtCore/QTimeZone>     // for QTimeZone
#include <QtCore/Qt>            // for UTC

#include "defs.h"
#include "energympro.h"
#include "gbfile.h"             // for gbfgetc, gbfseek, gbfclose, gbfopen, gbfread, gbfgetuint32, gbfcopyfrom, gbfgetuint16, gbfile, gbsize_t
#include "src/core/datetime.h"  // for DateTime


#define MYNAME "energympro"

//*******************************************************************************
//           local helper functions
//*******************************************************************************
void
EnergymproFormat::read_point(route_head* gpsbabel_route, gpsbabel::DateTime& gpsDateTime) const
{
  tw_point point{};
  gbfread(&point, sizeof(tw_point), 1, file_in);
  if (global_opts.debug_level > 1) {
    printf("Point: lat:%8u long:%8u alt:%8d ", point.Latitude, point.Longitude, point.Altitude);
    printf("speed:%6u dist:%5u time:%5u Status:%1u", point.Speed, point.IntervalDist, point.lntervalTime, point.Status);
    printf("HR:(%3d,%1d)", point.HR_Heartrate, point.HR_Status);
    printf("Speed:(%8u,%1d)", point.Speed_Speed, point.Speed_Status);
    printf("Cad:(%3d,%1d)", point.Cadence_Cadence, point.Cadence_Status);
    printf("Power (Cad:%6d Pow:%6d,%2d)Temp:%3d\n", point.Power_Cadence, point.Power_Power, point.Power_Status, point.Temp);

    qDebug() << "DateTime1:" << gpsDateTime.toString();
    qDebug() << "point.lntervalTime:" << point.lntervalTime;
  }

  //Time from last point in sec's * 10 (e.g. point.lntervalTime is sec multiplied with 10)
  // convert to millisecs
  gpsDateTime = gpsDateTime.addMSecs(point.lntervalTime*100);

  auto waypt = new Waypoint;
  waypt->latitude = (point.Latitude / 1000000.0);
  waypt->longitude = (point.Longitude / 1000000.0);
  waypt->altitude = point.Altitude;

  if (global_opts.debug_level > 1) {
    qDebug() << "DateTime2:" << gpsDateTime.toString();
  }

  waypt->SetCreationTime(gpsDateTime);

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


void
EnergymproFormat::read_lap() const
{
  tw_lap lap{};
  gbfread(&lap, sizeof(tw_lap), 1, file_in);
  if (global_opts.debug_level > 1) {
    printf("LAP: splitTime:%6us TotalTime:%6us LapNumber:%5d ", lap.splitTime/10, lap.TotalTime/10, lap.Number);
    printf("dist:%08um Cal:%5u Speed:(%6u,%6u) ", lap.lDistance, lap.Calorie, lap.MaxSpeed, lap.AvgSpeed);
    printf("HR:(%3d,%3d)", lap.MaxHeartrate, lap.AvgHeartrate);
    printf("Alt:(%6d,%6d) ", lap.MinAlti, lap.MaxAlti);
    printf("Cad:(%3d,%3d) ", lap.AvgCad, lap.MaxCad);
    printf("Power:(%3d,%3d)w ", lap.AvgPower, lap.MaxPower);
    printf("Pt:(%6d,%6d)\n", lap.StartRecPt, lap.FinishRecPt);
  }
}

//*******************************************************************************
//           global callbacks called by gpsbabel main process
//*******************************************************************************

void
EnergymproFormat::rd_init(const QString& fname)
{
  if (global_opts.debug_level > 1) {
    printf(MYNAME " rd_deinit()\n");
  }
  gbfile* fileorg_in = gbfopen(fname, "rb", MYNAME);

  /* copy file to memory stream (needed for seek-ops and piped commands) */
  file_in = gbfopen(nullptr, "wb", MYNAME);
  gbsize_t size = gbfcopyfrom(file_in, fileorg_in, 0x7FFFFFFF);
  if (global_opts.debug_level > 1) {
    printf(MYNAME "  filesize=%u\n", size);
  }
  gbfclose(fileorg_in);
  if (opt_timezone) {
    if (QTimeZone::isTimeZoneIdAvailable(opt_timezone)) {
      timezn = new QTimeZone(opt_timezone);
    } else {
      list_timezones();
      fatal(MYNAME ": Requested time zone \"%s\" is not available.\n", opt_timezone);
    }
  } else {
    timezn = nullptr;
  }
}

void
EnergymproFormat::rd_deinit()
{
  if (timezn != nullptr) {
    delete timezn;
    timezn = nullptr;
  }
  if (global_opts.debug_level > 1) {
    printf(MYNAME " rd_deinit()\n");
  }
  gbfclose(file_in);
}

void
EnergymproFormat::track_read()
{
  if (global_opts.debug_level > 1) {
    printf(MYNAME "  waypoint_read()\n");
  }

  gbfseek(file_in, 0L, SEEK_END);
  gbfseek(file_in, -(int32_t)(sizeof(tw_workout)), SEEK_CUR);
  tw_workout workout{};
  workout.dateStart.Year = gbfgetc(file_in);
  workout.dateStart.Month = gbfgetc(file_in);
  workout.dateStart.Day = gbfgetc(file_in);
  workout.timeStart.Hour = gbfgetc(file_in);
  workout.timeStart.Minute = gbfgetc(file_in);
  workout.timeStart.Second = gbfgetc(file_in);
  workout.TotalRecPt = gbfgetuint16(file_in);
  workout.TotalTime = gbfgetuint32(file_in);
  workout.TotalDist = gbfgetuint32(file_in);
  workout.LapNumber = gbfgetuint16(file_in);
  workout.Calory = gbfgetuint16(file_in);
  workout.MaxSpeed = gbfgetuint32(file_in);
  workout.AvgSpeed = gbfgetuint32(file_in);
  workout.MaxHeart = gbfgetc(file_in);
  workout.AvgHeart = gbfgetc(file_in);

  if (global_opts.debug_level > 1) {
    printf("%04d-%02d-%02d ", workout.dateStart.Year+2000, workout.dateStart.Month, workout.dateStart.Day);
    printf("%02d:%02d:%02d ", workout.timeStart.Hour, workout.timeStart.Minute, workout.timeStart.Second);
    printf("Total(RecPt:%6d Time:%6us Dist:%9um) LapNumber:%5d \n", workout.TotalRecPt, workout.TotalTime/10, workout.TotalDist, workout.LapNumber);
  }

  /*
   * GPS year: 2000+; struct tm year: 1900+
   */
  QDate gpsDate = QDate(workout.dateStart.Year+2000, workout.dateStart.Month, workout.dateStart.Day);
  QTime gpsTime = QTime(workout.timeStart.Hour, workout.timeStart.Minute, workout.timeStart.Second);
  gpsbabel::DateTime gpsDateTime;
  if (timezn != nullptr) {
    gpsDateTime = gpsbabel::DateTime(QDateTime(gpsDate, gpsTime, *timezn).toUTC());
  } else {
    gpsDateTime = gpsbabel::DateTime(QDateTime(gpsDate, gpsTime, Qt::LocalTime).toUTC());
  }

  auto* gpsbabel_route = new route_head;

  track_add_head(gpsbabel_route);
  gbfseek(file_in, 0L, SEEK_SET);

  for (int point=0; point<workout.TotalRecPt; point++) {
    read_point(gpsbabel_route, gpsDateTime);
  }

  gbfseek(file_in, sizeof(tw_point)*(workout.TotalRecPt), SEEK_SET);
  for (int lap=0; lap<workout.LapNumber; lap++) {
    read_lap();
  }
}

void
EnergymproFormat::read()
{
  if (global_opts.debug_level > 1) {
    printf(MYNAME " data_read()\n");
  }

  track_read();
}
