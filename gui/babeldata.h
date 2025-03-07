// -*- C++ -*-
// $Id: babeldata.h,v 1.8 2010-06-19 23:59:06 robertl Exp $
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
#ifndef BABELDATA_H
#define BABELDATA_H

#include <QDate>        // for QDate
#include <QDateTime>    // for QDateTime
#include <QList>        // for QList
#include <QSettings>    // for QSettings
#include <QString>      // for QString
#include <QStringList>  // for QStringList
#include <QUuid>        // for QUuid
#include <memory>       // for make_unique, unique_ptr
#include "setting.h"    // for SettingGroup, BoolSetting, StringSetting, IntSetting, DateTimeSetting

class BabelData
{
public:
  /* Constants */

  static constexpr int noType_ = -1;
  static constexpr int fileType_ = 0;
  static constexpr int deviceType_ = 1;

  /* Special Member Functions */

  BabelData():
    inputType_(fileType_),
    outputType_(fileType_),
    upgradeCheckTime_(QDate(2001, 1, 1).startOfDay()),
    installationUuid_(QUuid::createUuid().toString()),
    donateSplashed_(QDate(2010, 1, 1).startOfDay())
  {
  }

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

  void makeSettingGroup(SettingGroup& sg)
  {
    sg.addVarSetting(std::make_unique<IntSetting>("app.inputType", inputType_));
    sg.addVarSetting(std::make_unique<StringSetting>("app.inputFileFormat", inputFileFormat_));
    sg.addVarSetting(std::make_unique<StringSetting>("app.inputDeviceFormat", inputDeviceFormat_));
    sg.addVarSetting(std::make_unique<StringSetting>("app.inputCharSet", inputCharSet_));
    sg.addVarSetting(std::make_unique<StringSetting>("app.inputDeviceName", inputDeviceName_));

    sg.addVarSetting(std::make_unique<BoolSetting>("app.xlateWayPts", xlateWayPts_));
    sg.addVarSetting(std::make_unique<BoolSetting>("app.xlateRoutes", xlateRoutes_));
    sg.addVarSetting(std::make_unique<BoolSetting>("app.xlateTracks", xlateTracks_));

    sg.addVarSetting(std::make_unique<IntSetting>("app.outputType", outputType_));
    sg.addVarSetting(std::make_unique<StringSetting>("app.outputFileFormat", outputFileFormat_));
    sg.addVarSetting(std::make_unique<StringSetting>("app.outputDeviceFormat", outputDeviceFormat_));
    sg.addVarSetting(std::make_unique<StringSetting>("app.outputCharSet", outputCharSet_));
    sg.addVarSetting(std::make_unique<StringSetting>("app.outputDeviceName", outputDeviceName_));

    sg.addVarSetting(std::make_unique<BoolSetting>("app.synthShortNames", synthShortNames_));
    sg.addVarSetting(std::make_unique<BoolSetting>("app.forceGPSTypes", forceGPSTypes_));

    sg.addVarSetting(std::make_unique<StringSetting>("app.inputBrowse", inputBrowse_));
    sg.addVarSetting(std::make_unique<StringSetting>("app.outputBrowse", outputBrowse_));

    sg.addVarSetting(std::make_unique<BoolSetting>("app.previewGmap", previewGmap_));
    sg.addVarSetting(std::make_unique<IntSetting>("app.upgradeCheckMethod", upgradeCheckMethod_));
    sg.addVarSetting(std::make_unique<DateTimeSetting>("app.upgradeCheckTime", upgradeCheckTime_));
    sg.addVarSetting(std::make_unique<DateTimeSetting>("app.donateSplashed", donateSplashed_));
    sg.addVarSetting(std::make_unique<StringSetting>("app.installationUuid", installationUuid_));
    sg.addVarSetting(std::make_unique<IntSetting>("app.upgradeCallbacks", upgradeCallbacks_));
    sg.addVarSetting(std::make_unique<IntSetting>("app.upgradeAccept", upgradeAccept_));
    sg.addVarSetting(std::make_unique<IntSetting>("app.upgradeDeclines", upgradeDeclines_));
    sg.addVarSetting(std::make_unique<IntSetting>("app.upgradeErrors", upgradeErrors_));
    sg.addVarSetting(std::make_unique<IntSetting>("app.upgradeOffers", upgradeOffers_));
    sg.addVarSetting(std::make_unique<IntSetting>("app.runCount", runCount_));

    // Global preferences.
    sg.addVarSetting(std::make_unique<BoolSetting>("app.startupVersionCheck", startupVersionCheck_));
    sg.addVarSetting(std::make_unique<BoolSetting>("app.reportStatistics", reportStatistics_));
    sg.addVarSetting(std::make_unique<BoolSetting>("app.upgradeMenuEnabled", upgradeMenuEnabled_));
    sg.addVarSetting(std::make_unique<BoolSetting>("app.mapPreviewEnabled", mapPreviewEnabled_));
    sg.addVarSetting(std::make_unique<BoolSetting>("app.allowBetaUpgrades", allowBetaUpgrades_));
    sg.addVarSetting(std::make_unique<BoolSetting>("app.ignoreVersionMismatch", ignoreVersionMismatch_));
    sg.addVarSetting(std::make_unique<BoolSetting>("app.disableDonateDialog", disableDonateDialog_));
  }

  /* Data Members */

  int inputType_;
  QString inputFileFormat_;
  QString inputDeviceFormat_;
  QStringList inputFileNames_;
  QString inputDeviceName_;
  QString inputCharSet_;

  bool xlateWayPts_{true};
  bool xlateRoutes_{true};
  bool xlateTracks_{true};

  int outputType_;
  QString outputFileFormat_;
  QString outputDeviceFormat_;
  QString outputFileName_;
  QString outputDeviceName_;
  QString outputCharSet_;

  bool synthShortNames_{false};
  bool forceGPSTypes_{false};
  int  debugLevel_{-1};

  QString inputBrowse_, outputBrowse_;

  bool  previewGmap_{false};
  int   upgradeCheckMethod_{0};
  QDateTime upgradeCheckTime_;
  QString installationUuid_;
  int upgradeCallbacks_{0};
  int upgradeAccept_{0};
  int upgradeDeclines_{0};
  int upgradeErrors_{0};
  int upgradeOffers_{0};
  int runCount_{0};

  // Global preferences.
  bool startupVersionCheck_{true};
  bool reportStatistics_{true};
  bool upgradeMenuEnabled_{true};
  bool mapPreviewEnabled_{true};
  bool allowBetaUpgrades_{false};
  bool ignoreVersionMismatch_{false};
  bool disableDonateDialog_{false};
  QDateTime donateSplashed_;
};
#endif
