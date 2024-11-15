// -*- C++ -*-
// $Id: filterdata.cpp,v 1.4 2009-11-02 20:38:02 robertl Exp $
//------------------------------------------------------------------------
//
//  Copyright (C) 2009  S. Khai Mong <khai@mangrai.com>.
//
//  This program is free software; you can redistribute it and/or
//  modify it under the terms of the GNU General Public License as
//  published by the Free Software Foundation; either version 2 of the
//  License, or (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//  General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301,
//  USA.
//
//------------------------------------------------------------------------

#include "filterdata.h"
#include <QChar>    // for QChar
#include <QDate>    // for QDate
#include <QVector>  // for QVector

QStringList WayPtsFilterData::makeOptionString()
{
  QStringList args;
  if (!inUse_) {
    return args;
  }

  static const QVector<QString> radius_units = {"mi", "km"};
  if (radius) {
    args << QString("-x");
    args << QString("radius,distance=%1%2,lat=%3,lon=%4")
         .arg(radiusVal).arg(radius_units.at(radiusUnit)).arg(latVal, 0, 'f', 8).arg(longVal, 0, 'f', 8);
  }
  if (duplicates && (shortNames || locations)) {
    args << QString("-x");
    QString s = "duplicate";
    if (shortNames) {
      s += ",shortname";
    }
    if (locations) {
      s += ",location";
    }
    args << s;
  }

  static const QVector<QString> position_units = {"ft", "m"};
  if (position) {
    args << QString("-x");
    args << QString("position,distance=%1%2").arg(positionVal).arg(position_units.at(positionUnit));
  }
  return args;
}

//------------------------------------------------------------------------
static QString optionDate(const QDateTime& dt)
{
  QDateTime d = dt.toUTC();

  QDate date = d.date();
  QTime time = d.time();
  QString s = QString("%1%2%3%4%5%6")
              .arg(date.year(),   4, 10, QChar('0'))
              .arg(date.month(),  2, 10, QChar('0'))
              .arg(date.day(),    2, 10, QChar('0'))
              .arg(time.hour(),   2, 10, QChar('0'))
              .arg(time.minute(), 2, 10, QChar('0'))
              .arg(time.second(), 2, 10, QChar('0'));
  return s;
}

//------------------------------------------------------------------------
QStringList TrackFilterData::makeOptionString()
{
  QStringList args;
  if (!inUse_) {
    return args;
  }

  QString s;
  if (GPSFixes) {
    static const char* fixStrings[] = {"none", "pps", "dgpss", "3d", "2d"}; // match with designer!
    s += QString(",fix=%1").arg(fixStrings[GPSFixesVal]);
  }
  if (course) {
    s += ",course";
  }
  if (speed) {
    s += ",speed";
  }
  if (pack) {
    s += ",pack";
  }
  if (merge) {
    s += ",merge";
  }
  if (pack || merge) {
    if (splitByDate)  {
      s += ",split";
    }
    if (splitByTime)  {
      s += ",split";
      if (splitTime > 0) {
        s += QString("=%1%2").arg(splitTime).arg("mhd"[splitTimeUnit]);
      }
    }
    if (splitByDistance && splitDist > 0) {
      double d = splitDist;
      char u = ' ';
      if (splitDistUnit == 0) { // ft.
        d /= 5280.0;
        u = 'm';
      } else if (splitDistUnit == 1) { //m
        d /= 1000.0;
        u = 'k';
      } else if (splitDistUnit == 2) { //km
        u = 'k';
      } else if (splitDistUnit == 3) { //m
        u = 'm';
      }
      s += QString(",sdistance=%1%2").arg(d).arg(u);
    }
  }

  if (start) {
    s += QString(",start=%1").arg(optionDate(startTime));
  }
  if (stop) {
    s += QString(",stop=%1").arg(optionDate(stopTime));
  }
  if (move) {
    s += QString(",move=%1w%2d%3h%4m%5s").arg(weeks).arg(days).arg(hours).arg(mins).arg(secs);
  }
  if (title) {
    s += QString(",title=%1").arg(titleString);
  }

  if (s.length() != 0) {
    args << "-x" << "track" + s;
  }

  return args;
}

//------------------------------------------------------------------------
QStringList RtTrkFilterData::makeOptionString()
{
  QStringList args;
  if (!inUse_) {
    return args;
  }

  if (reverse_) {
    args << QString("-x") << QString("reverse");
  }
  if (simplify_) {
    args << QString("-x") << QString("simplify,count=%1").arg(limitTo_);
  }

  return args;
}

//------------------------------------------------------------------------
QStringList MiscFltFilterData::makeOptionString()
{
  QStringList args;
  if (!inUse_) {
    return args;
  }

  if (nukeRoutes_ || nukeTracks_ || nukeWaypoints_) {
    args << QString("-x");
    QString s = "nuketypes";
    if (nukeRoutes_) {
      s += ",routes";
    }
    if (nukeTracks_) {
      s += ",tracks";
    }
    if (nukeWaypoints_) {
      s += ",waypoints";
    }
    args << s;
  }

  if (swap_) {
    args << "-x" << "swap";
  }

  if (transform_) {
    static const char* xformStr[] = {
      "wpt=trk",
      "trk=rte",
      "rte=wpt",
      "wpt=rte",
      "rte=trk",
      "trk=wpt"
    };
    args << QString("-x");
    QString s= QString("transform,%1").arg(xformStr[transformVal_]);
    if (del_) {
      s += ",del";
    }
    args << s;
  }

  if (sortWpt_ || sortRte_ || sortTrk_) {
    args << "-x";
    QString s = "sort";
    if (sortWpt_) {
      const QStringList wptopts= {"description", "gcid", "shortname", "time"};
      s += QString(",%1").arg(wptopts.at(sortWptBy_));
    }
    if (sortRte_) {
      const QStringList rteopts= {"rtedesc", "rtename", "rtenum"};
      s += QString(",%1").arg(rteopts.at(sortRteBy_));
    }
    if (sortTrk_) {
      const QStringList trkopts= {"trkdesc", "trkname", "trknum"};
      s += QString(",%1").arg(trkopts.at(sortTrkBy_));
    }
    args << s;

  }
  return args;
}
