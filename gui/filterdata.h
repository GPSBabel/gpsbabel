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

#include <QDate>        // for QDate
#include <QDateTime>    // for QDateTime
#include <QList>        // for QList
#include <QSettings>    // for QSettings
#include <QString>      // for QString
#include <QStringList>  // for QStringList
#include <memory>       // for make_unique, unique_ptr
#include <utility>      // for as_const
#include "setting.h"    // for BoolSetting, IntSetting, SettingGroup, VarSetting, DoubleSetting, DateTimeSetting, StringSetting

//------------------------------------------------------------------------

class FilterData
{
public:
  /* Special Member Functions */

  FilterData() = default;
  FilterData(const FilterData &) = default;
  FilterData &operator=(const FilterData &) = default;
  FilterData(FilterData &&) = default;
  FilterData &operator=(FilterData &&) = default;
  virtual ~FilterData() = default;

  /* Member Functions */

  void saveSettings(QSettings& st)
  {
    SettingGroup sg;
    makeSettingGroup(sg);
    sg.saveSettings(st);
  }
  void restoreSettings(QSettings& st)
  {
    SettingGroup sg;
    makeSettingGroup(sg);
    sg.restoreSettings(st);
  }
  virtual void makeSettingGroup(SettingGroup& sg) = 0;
  virtual QStringList makeOptionString() const = 0;

  /* Member Data */

  bool inUse_{true};
};
//------------------------------------------------------------------------

class TrackFilterData: public FilterData
{
public:
  /* Special Member Functions */

  TrackFilterData() : titleString("ACTIVE LOG #%Y%m%d")
  {
    QDate today = QDate::currentDate();
    stopTime = today.endOfDay();
    startTime = today.addMonths(-6).startOfDay();
  }

  /* Member Functions */

  QStringList makeOptionString() const override;
  void makeSettingGroup(SettingGroup& sg) override
  {
    sg.addVarSetting(std::make_unique<BoolSetting>("trks.inUse", inUse_));
    sg.addVarSetting(std::make_unique<BoolSetting>("trks.GPSFixes", GPSFixes));
    sg.addVarSetting(std::make_unique<IntSetting>("trks.GPSFixesVal", GPSFixesVal));
    sg.addVarSetting(std::make_unique<BoolSetting>("trks.course", course));
    sg.addVarSetting(std::make_unique<BoolSetting>("trks.speed", speed));
    sg.addVarSetting(std::make_unique<BoolSetting>("trks.pack", pack));
    sg.addVarSetting(std::make_unique<BoolSetting>("trks.merge", merge));
    sg.addVarSetting(std::make_unique<BoolSetting>("trks.split", split));
    sg.addVarSetting(std::make_unique<BoolSetting>("trks.splitByDate", splitByDate));
    sg.addVarSetting(std::make_unique<BoolSetting>("trks.splitByTime", splitByTime));
    sg.addVarSetting(std::make_unique<BoolSetting>("trks.splitByDistance", splitByDistance));
    sg.addVarSetting(std::make_unique<BoolSetting>("trks.start", start));
    sg.addVarSetting(std::make_unique<DateTimeSetting>("trks.startTime", startTime));
    sg.addVarSetting(std::make_unique<BoolSetting>("trks.stop", stop));
    sg.addVarSetting(std::make_unique<DateTimeSetting>("trks.stopTime", stopTime));
    sg.addVarSetting(std::make_unique<BoolSetting>("trks.localTime", localTime));
    sg.addVarSetting(std::make_unique<BoolSetting>("trks.utc", utc));
    sg.addVarSetting(std::make_unique<BoolSetting>("trks.move", move));
    sg.addVarSetting(std::make_unique<IntSetting>("trks.weeks", weeks));
    sg.addVarSetting(std::make_unique<IntSetting>("trks.days", days));
    sg.addVarSetting(std::make_unique<IntSetting>("trks.mins", mins));
    sg.addVarSetting(std::make_unique<IntSetting>("trks.hours", hours));
    sg.addVarSetting(std::make_unique<IntSetting>("trks.secs", secs));
    sg.addVarSetting(std::make_unique<BoolSetting>("trks.title", title));
    sg.addVarSetting(std::make_unique<StringSetting>("trks.titleString", titleString));
    sg.addVarSetting(std::make_unique<IntSetting>("trks.splitTime", splitTime));
    sg.addVarSetting(std::make_unique<IntSetting>("trks.splitTimeUnit", splitTimeUnit));
    sg.addVarSetting(std::make_unique<IntSetting>("trks.splitDist", splitDist));
    sg.addVarSetting(std::make_unique<IntSetting>("trks.splitDistUnit", splitDistUnit));
  }

  /* Data Members */

  bool title{false};
  QString titleString;
  bool move{false};
  int  weeks{0}, days{0}, hours{0}, mins{0}, secs{0};
  bool localTime{true}, utc{false};

  bool start{false};
  QDateTime startTime;
  bool stop{false};
  QDateTime stopTime;
  bool pack{false}, merge{false}, split{false}, GPSFixes{false};
  int  GPSFixesVal{0};
  bool splitByDate{false}, splitByTime{false}, splitByDistance{false};
  bool course{false}, speed{false};
  int  splitTime{0}, splitTimeUnit{0};
  int  splitDist{0}, splitDistUnit{0};
};

//------------------------------------------------------------------------

class WayPtsFilterData: public FilterData
{
public:
  /* Member Functions */

  QStringList makeOptionString() const override;
  void makeSettingGroup(SettingGroup& sg) override
  {
    sg.addVarSetting(std::make_unique<BoolSetting>("wpts.inUse", inUse_));
    sg.addVarSetting(std::make_unique<BoolSetting>("wpts.radius", radius));
    sg.addVarSetting(std::make_unique<DoubleSetting>("wpts.radiusVal", radiusVal));
    sg.addVarSetting(std::make_unique<IntSetting>("wpts.radiusUnit", radiusUnit));
    sg.addVarSetting(std::make_unique<DoubleSetting>("wpts.latVal", latVal));
    sg.addVarSetting(std::make_unique<DoubleSetting>("wpts.longVal", longVal));
    sg.addVarSetting(std::make_unique<BoolSetting>("wpts.duplicates", duplicates));
    sg.addVarSetting(std::make_unique<BoolSetting>("wpts.shortNames", shortNames));
    sg.addVarSetting(std::make_unique<BoolSetting>("wpts.locations", locations));
    sg.addVarSetting(std::make_unique<BoolSetting>("wpts.position", position));
    sg.addVarSetting(std::make_unique<DoubleSetting>("wpts.positionVal", positionVal));
    sg.addVarSetting(std::make_unique<IntSetting>("wpts.positionUnit", positionUnit));
  }

  /* Data Members */

  bool duplicates{false}, shortNames{true}, locations{false}, position{false}, radius{false};
  double positionVal{0.0};
  double radiusVal{0.0};
  double longVal{0.0}, latVal{0.0};
  int positionUnit{0}, radiusUnit{0};
};

//------------------------------------------------------------------------
class RtTrkFilterData: public FilterData
{
public:
  /* Member Functions */

  QStringList makeOptionString() const override;
  void makeSettingGroup(SettingGroup& sg) override
  {
    sg.addVarSetting(std::make_unique<BoolSetting>("rttrk.inUse", inUse_));
    sg.addVarSetting(std::make_unique<BoolSetting>("rttrk.reverse", reverse_));
    sg.addVarSetting(std::make_unique<BoolSetting>("rttrk.simplify", simplify_));
    sg.addVarSetting(std::make_unique<IntSetting>("rttrk.limitTo", limitTo_));
  }

  /* Data Members */

  bool simplify_{false}, reverse_{false};
  int limitTo_{100};
};

//------------------------------------------------------------------------
class MiscFltFilterData: public FilterData
{
public:
  /* Member Functions */

  QStringList makeOptionString() const override;
  void makeSettingGroup(SettingGroup& sg) override
  {
    sg.addVarSetting(std::make_unique<BoolSetting>("mscflt.nukeRoutes", nukeRoutes_));
    sg.addVarSetting(std::make_unique<BoolSetting>("mscflt.nukeTracks", nukeTracks_));
    sg.addVarSetting(std::make_unique<BoolSetting>("mscflt.nukeWaypoints", nukeWaypoints_));
    sg.addVarSetting(std::make_unique<BoolSetting>("mscflt.inUse", inUse_));
    sg.addVarSetting(std::make_unique<BoolSetting>("mscflt.transform", transform_));
    sg.addVarSetting(std::make_unique<IntSetting>("mscflt.transformVal", transformVal_));
    sg.addVarSetting(std::make_unique<BoolSetting>("mscflt.delete", del_));
    sg.addVarSetting(std::make_unique<BoolSetting>("mscflt.swap", swap_));
    sg.addVarSetting(std::make_unique<BoolSetting>("mscflt.sortWpt", sortWpt_));
    sg.addVarSetting(std::make_unique<IntSetting>("mscflt.sortWptBy", sortWptBy_));
    sg.addVarSetting(std::make_unique<BoolSetting>("mscflt.sortRte", sortRte_));
    sg.addVarSetting(std::make_unique<IntSetting>("mscflt.sortRteBy", sortRteBy_));
    sg.addVarSetting(std::make_unique<BoolSetting>("mscflt.sortTrk", sortTrk_));
    sg.addVarSetting(std::make_unique<IntSetting>("mscflt.sortTrkBy", sortTrkBy_));
  }

  /* Member Data */

  bool nukeRoutes_{false}, nukeTracks_{false}, nukeWaypoints_{false};
  bool transform_{false}, del_{false}, swap_{false};
  bool sortWpt_{false}, sortRte_{false}, sortTrk_{false};
  int transformVal_{0};
  int sortWptBy_{0}, sortRteBy_{0}, sortTrkBy_{0};
};


//------------------------------------------------------------------------
//
class AllFiltersData
{
public:
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
    for (const auto* filter : std::as_const(filters)) {
      args << filter->makeOptionString();
    }
    return args;
  }

  /* Member Data */

  TrackFilterData trackFilterData;
  WayPtsFilterData wayPtsFilterData;
  RtTrkFilterData rtTrkFilterData;
  MiscFltFilterData miscFltFilterData;
  QList<FilterData*>filters{&miscFltFilterData, &trackFilterData, &wayPtsFilterData, &rtTrkFilterData};
};
#endif
