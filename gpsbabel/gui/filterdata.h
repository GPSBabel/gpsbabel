// -*- C++ -*-
// $Id: filterdata.h,v 1.2 2009-07-31 18:32:32 robertl Exp $
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
//  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111
//  USA
//
//------------------------------------------------------------------------
#ifndef FILTERDATA_H
#define FILTERDATA_H

#include <QDateTime>
#include <QSettings>
#include <QStringList>
#include "setting.h"

//------------------------------------------------------------------------

class FilterData {
public:
  FilterData(): inUse(true) {};

  void saveSettings(QSettings &st) {
    SettingGroup sg;
    makeSettingGroup(sg);
    sg.saveSettings(st);
  }
  void restoreSettings(QSettings &st) {
    SettingGroup sg;
    makeSettingGroup(sg);
    sg.restoreSettings(st);
  }
  virtual void makeSettingGroup(SettingGroup &sg) = 0;
  virtual QStringList makeOptionString() = 0;

public:
  bool inUse;
};
//------------------------------------------------------------------------

class TrackFilterData: public FilterData  {
 public:
  TrackFilterData(): FilterData(), title(false), titleString(QString()),
		     move(false),  days(0), hours(0), mins(0), secs(0),
		     TZ(false),
		     start(false),
		     stop(false),
		     pack(false), merge(false), split(false), 
		     GPSFixes(false), GPSFixesVal(0),
		     course(false), speed(false),
		     splitTime(0), splitTimeUnit(0),
		     splitDist(0), splitDistUnit(0)
  {
    titleString = "ACTIVE LOG #%Y%m%d";
    stopTime = QDateTime::currentDateTime();
    stopTime.setTime(QTime(23, 59, 59));
    startTime = stopTime.addMonths(-6);
    startTime.setTime(QTime(0, 0, 1));
  }
  virtual void makeSettingGroup(SettingGroup &sg)
  {
    sg.addVarSetting(new BoolSetting("trks.inUse", inUse));
    sg.addVarSetting(new BoolSetting("trks.GPSFixes", GPSFixes));
    sg.addVarSetting(new IntSetting("trks.GPSFixesVal", GPSFixesVal));
    sg.addVarSetting(new BoolSetting("trks.course", course));
    sg.addVarSetting(new BoolSetting("trks.speed", speed));
    sg.addVarSetting(new BoolSetting("trks.pack", pack));
    sg.addVarSetting(new BoolSetting("trks.merge", merge));
    sg.addVarSetting(new BoolSetting("trks.split", split));
    sg.addVarSetting(new BoolSetting("trks.start", start));
    sg.addVarSetting(new DateTimeSetting("trks.startTime", startTime));
    sg.addVarSetting(new BoolSetting("trks.stop", stop));
    sg.addVarSetting(new DateTimeSetting("trks.stopTime", stopTime));
    sg.addVarSetting(new BoolSetting("trks.TZ", TZ));
    sg.addVarSetting(new BoolSetting("trks.move", move));
    sg.addVarSetting(new IntSetting("trks.days", days));
    sg.addVarSetting(new IntSetting("trks.mins", mins));
    sg.addVarSetting(new IntSetting("trks.hours", hours));
    sg.addVarSetting(new IntSetting("trks.secs", secs));
    sg.addVarSetting(new BoolSetting("trks.title", title));
    sg.addVarSetting(new StringSetting("trks.titleString", titleString));
    sg.addVarSetting(new IntSetting("trks.splitTime", splitTime));
    sg.addVarSetting(new IntSetting("trks.splitTimeUnit", splitTimeUnit));
    sg.addVarSetting(new IntSetting("trks.splitDist", splitDist));
    sg.addVarSetting(new IntSetting("trks.splitDistUnit", splitDistUnit));
  }
  virtual QStringList makeOptionString();
  
 public:
  bool title;
  QString titleString;
  bool move;
  int  days, hours, mins, secs;
  bool TZ;

  bool start;
  QDateTime startTime;
  bool stop;
  QDateTime stopTime;
  bool pack, merge, split, GPSFixes;
  int  GPSFixesVal;
  bool course, speed;
  int  splitTime, splitTimeUnit;
  int  splitDist, splitDistUnit;
};

//------------------------------------------------------------------------

class WayPtsFilterData: public FilterData  {
 public:
  WayPtsFilterData(): FilterData(), 
		      duplicates(false), shortNames(true), locations(false), 
		      position(false), radius(false), sort(false),
		      positionVal(0.0), radiusVal(0.0),
		      longVal(0.0), latVal(0.0),
		      positionUnit(0), radiusUnit(0)
    {
    }
  
  virtual QStringList makeOptionString();
  virtual void makeSettingGroup(SettingGroup &sg)
  {
    sg.addVarSetting(new BoolSetting("wpts.inUse", inUse));
    sg.addVarSetting(new BoolSetting("wpts.radius", radius));
    sg.addVarSetting(new DoubleSetting("wpts.radiusVal", radiusVal));
    sg.addVarSetting(new IntSetting("wpts.radiusUnit", radiusUnit));
    sg.addVarSetting(new DoubleSetting("wpts.latVal", latVal));
    sg.addVarSetting(new DoubleSetting("wpts.longVal", longVal));
    sg.addVarSetting(new BoolSetting("wpts.duplicates", duplicates));
    sg.addVarSetting(new BoolSetting("wpts.shortNames", shortNames));
    sg.addVarSetting(new BoolSetting("wpts.locations", locations));
    sg.addVarSetting(new BoolSetting("wpts.position", position));
    sg.addVarSetting(new DoubleSetting("wpts.positionVal", positionVal));
    sg.addVarSetting(new IntSetting("wpts.positionUnit", positionUnit));
    sg.addVarSetting(new BoolSetting("wpts.sort", sort));
  }


 public:
  bool duplicates, shortNames, locations, position, radius, sort;
  double positionVal;
  double radiusVal;
  double longVal, latVal;
  int positionUnit, radiusUnit;
};

//------------------------------------------------------------------------
class RtTrkFilterData: public FilterData  {
 public:
  RtTrkFilterData(): FilterData(), 
		     simplify(false),
		     reverse(false),
		     limitTo(100)
    {
    }
  
  virtual QStringList makeOptionString();
  virtual void makeSettingGroup(SettingGroup &sg) {
    sg.addVarSetting(new BoolSetting("rttrk.inUse", inUse));
    sg.addVarSetting(new BoolSetting("rttrk.reverse", reverse));
    sg.addVarSetting(new BoolSetting("rttrk.simplify", simplify));
    sg.addVarSetting(new IntSetting("rttrk.limitTo", limitTo));
  }

 public:
  bool simplify, reverse;
  int limitTo;
};

//------------------------------------------------------------------------
class MiscFltFilterData: public FilterData  {
 public:
  MiscFltFilterData(): FilterData(), 
		       transform(false),
		       del(false),
		       swap(false),
		       transformVal(0)
    {
    }
  
  virtual QStringList makeOptionString();
  virtual void makeSettingGroup(SettingGroup &sg)
  {
    sg.addVarSetting(new BoolSetting("mscflt.inUse", inUse));
    sg.addVarSetting(new BoolSetting("mscflt.transform", transform));
    sg.addVarSetting(new IntSetting("mscflt.transformVal", transformVal));
    sg.addVarSetting(new BoolSetting("mscflt.delete", del));
    sg.addVarSetting(new BoolSetting("mscflt.swap", swap));
  }

 public:
  bool transform, del, swap;
  int transformVal;
};


//------------------------------------------------------------------------
//
class AllFiltersData
{
 public:
  AllFiltersData()
  {
    defaultAll();
    filters << &miscFltFilterData;
    filters << &trackFilterData;
    filters << &wayPtsFilterData;
    filters << &rtTrkFilterData;
  }
  
  void defaultAll() {
    miscFltFilterData = MiscFltFilterData();
    trackFilterData = TrackFilterData();
    wayPtsFilterData = WayPtsFilterData();
    rtTrkFilterData = RtTrkFilterData();
  }

  QStringList getAllFilterStrings() {
    QStringList args;
    for (int i=0; i<filters.size(); i++) 
      args << filters[i]->makeOptionString();
    return args;
  }

  TrackFilterData trackFilterData;
  WayPtsFilterData wayPtsFilterData;
  RtTrkFilterData rtTrkFilterData;
  MiscFltFilterData miscFltFilterData;
  QList<FilterData *>filters;
};


#endif
