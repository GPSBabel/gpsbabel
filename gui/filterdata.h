// -*- C++ -*-
// $Id: filterdata.h,v 1.4 2009-11-02 20:38:02 robertl Exp $
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
#ifndef FILTERDATA_H
#define FILTERDATA_H

#include <QDateTime>
#include <QSettings>
#include <QStringList>
#include "setting.h"

//------------------------------------------------------------------------
class FilterData
{
public:

  /* Special Member Functions */

  FilterData() = default;
  FilterData(const FilterData&) = default;
  FilterData& operator=(const FilterData&) = default;
  FilterData(FilterData&&) = default;
  FilterData& operator=(FilterData&&) = default;
  virtual ~FilterData() = default;

  /* Member Functions */

  void saveSettings(QSettings& st)
  {
    makeSettingGroup().saveSettings(st);
  }
  void restoreSettings(QSettings& st)
  {
    makeSettingGroup().restoreSettings(st);
  }
  virtual QStringList makeOptionString() = 0;
  virtual SettingGroup makeSettingGroup() = 0;

  /* Data Members */

  bool inUse_{true};
};

//------------------------------------------------------------------------
class TrackFilterData: public FilterData
{
public:

  /* Special Member Functions */

  TrackFilterData()
  {
    titleString = "ACTIVE LOG #%Y%m%d";
    stopTime = QDateTime::currentDateTime();
    stopTime.setTime(QTime(23, 59, 59));
    startTime = stopTime.addMonths(-6);
    startTime.setTime(QTime(0, 0, 1));
  }

  /* Member Functions */

  QStringList makeOptionString() override;
  SettingGroup makeSettingGroup() override
  {
    SettingGroup sg;
    sg.addVarSetting(new BoolSetting("trks.inUse", inUse_));
    sg.addVarSetting(new BoolSetting("trks.GPSFixes", GPSFixes));
    sg.addVarSetting(new IntSetting("trks.GPSFixesVal", GPSFixesVal));
    sg.addVarSetting(new BoolSetting("trks.course", course));
    sg.addVarSetting(new BoolSetting("trks.speed", speed));
    sg.addVarSetting(new BoolSetting("trks.pack", pack));
    sg.addVarSetting(new BoolSetting("trks.merge", merge));
    sg.addVarSetting(new BoolSetting("trks.split", split));
    sg.addVarSetting(new BoolSetting("trks.splitByDate", splitByDate));
    sg.addVarSetting(new BoolSetting("trks.splitByTime", splitByTime));
    sg.addVarSetting(new BoolSetting("trks.splitByDistance", splitByDistance));
    sg.addVarSetting(new BoolSetting("trks.start", start));
    sg.addVarSetting(new DateTimeSetting("trks.startTime", startTime));
    sg.addVarSetting(new BoolSetting("trks.stop", stop));
    sg.addVarSetting(new DateTimeSetting("trks.stopTime", stopTime));
    sg.addVarSetting(new BoolSetting("trks.TZ", TZ));
    sg.addVarSetting(new BoolSetting("trks.move", move));
    sg.addVarSetting(new IntSetting("trks.weeks", weeks));
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
    return sg;
  }

  /* Data Members */

  bool title{false};
  QString titleString;
  bool move{false};
  int weeks{0};
  int days{0};
  int hours{0};
  int mins{0};
  int secs{0};
  bool TZ{false};
  bool start{false};
  QDateTime startTime;
  bool stop{false};
  QDateTime stopTime;
  bool pack{false};
  bool merge{false};
  bool split{false};
  bool GPSFixes{false};
  int GPSFixesVal{0};
  bool splitByDate{false};
  bool splitByTime{false};
  bool splitByDistance{false};
  bool course{false};
  bool speed{false};
  int splitTime{0};
  int splitTimeUnit{0};
  int splitDist{0};
  int splitDistUnit{0};
};

//------------------------------------------------------------------------
class WayPtsFilterData: public FilterData
{
public:

  /* Member Functions */

  QStringList makeOptionString() override;
  SettingGroup makeSettingGroup() override
  {
    SettingGroup sg;
    sg.addVarSetting(new BoolSetting("wpts.inUse", inUse_));
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
    return sg;
  }

  /* Data Members */

  bool duplicates{false};
  bool shortNames{true};
  bool locations{false};
  bool position{false};
  bool radius{false};
  double positionVal{0.0};
  double radiusVal{0.0};
  double longVal{0.0};
  double latVal{0.0};
  int positionUnit{0};
  int radiusUnit{0};
};

//------------------------------------------------------------------------
class RtTrkFilterData: public FilterData
{
public:

  /* Member Functions */

  QStringList makeOptionString() override;
  SettingGroup makeSettingGroup() override
  {
    SettingGroup sg;
    sg.addVarSetting(new BoolSetting("rttrk.inUse", inUse_));
    sg.addVarSetting(new BoolSetting("rttrk.reverse", reverse_));
    sg.addVarSetting(new BoolSetting("rttrk.simplify", simplify_));
    sg.addVarSetting(new IntSetting("rttrk.limitTo", limitTo_));
    return sg;
  }

  /* Data Members */

  bool simplify_{false};
  bool reverse_{false};
  int limitTo_{100};
};

//------------------------------------------------------------------------
class MiscFltFilterData: public FilterData
{
public:

  /* Member Functions */

  QStringList makeOptionString() override;
  SettingGroup makeSettingGroup() override
  {
    SettingGroup sg;
    sg.addVarSetting(new BoolSetting("mscflt.nukeRoutes", nukeRoutes_));
    sg.addVarSetting(new BoolSetting("mscflt.nukeTracks", nukeTracks_));
    sg.addVarSetting(new BoolSetting("mscflt.nukeWaypoints", nukeWaypoints_));
    sg.addVarSetting(new BoolSetting("mscflt.inUse", inUse_));
    sg.addVarSetting(new BoolSetting("mscflt.transform", transform_));
    sg.addVarSetting(new IntSetting("mscflt.transformVal", transformVal_));
    sg.addVarSetting(new BoolSetting("mscflt.delete", del_));
    sg.addVarSetting(new BoolSetting("mscflt.swap", swap_));
    sg.addVarSetting(new BoolSetting("mscflt.sortWpt", sortWpt_));
    sg.addVarSetting(new IntSetting("mscflt.sortWptBy", sortWptBy_));
    sg.addVarSetting(new BoolSetting("mscflt.sortRte", sortRte_));
    sg.addVarSetting(new IntSetting("mscflt.sortRteBy", sortRteBy_));
    sg.addVarSetting(new BoolSetting("mscflt.sortTrk", sortTrk_));
    sg.addVarSetting(new IntSetting("mscflt.sortTrkBy", sortTrkBy_));
    return sg;
  }

  /* Data Members */

  bool nukeRoutes_{false};
  bool nukeTracks_{false};
  bool nukeWaypoints_{false};
  bool transform_{false};
  bool del_{false};
  bool swap_{false};
  bool sortWpt_{false};
  bool sortRte_{false};
  bool sortTrk_{false};
  int transformVal_{0};
  int sortWptBy_{0};
  int sortRteBy_{0};
  int sortTrkBy_{0};
};

//------------------------------------------------------------------------
class AllFiltersData
{
public:

  /* Special Member Functions */

  AllFiltersData()
  {
    filters << &miscFltFilterData;
    filters << &trackFilterData;
    filters << &wayPtsFilterData;
    filters << &rtTrkFilterData;
  }

  /* Member Functions */

  void defaultAll()
  {
    miscFltFilterData = MiscFltFilterData();
    trackFilterData = TrackFilterData();
    wayPtsFilterData = WayPtsFilterData();
    rtTrkFilterData = RtTrkFilterData();
  }
  QStringList getAllFilterStrings()
  {
    QStringList args;
    for (auto& filter : filters) {
      args << filter->makeOptionString();
    }
    return args;
  }

  /* Data Members */

  MiscFltFilterData miscFltFilterData;
  TrackFilterData trackFilterData;
  WayPtsFilterData wayPtsFilterData;
  RtTrkFilterData rtTrkFilterData;
  QList<FilterData*>filters;
};

#endif
