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
//  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111
//  USA
//
//------------------------------------------------------------------------
#ifndef BABELDATA_H
#define BABELDATA_H

#include <QSettings>
#include <QStringList>
#include <QUuid>
#include "setting.h"


class BabelData {
public:
  BabelData(): 
    inputType_(fileType_),
    inputFileFormat_(QString()),
    inputDeviceFormat_(QString()),
    inputFileNames_(QStringList()),
    inputDeviceName_(QString()),
    inputCharSet_(QString()),
    xlateWayPts_(true),
    xlateRoutes_(true),
    xlateTracks_(true),
    outputType_(fileType_),
    outputFileFormat_(QString()),
    outputDeviceFormat_(QString()),
    outputFileName_(QString()),
    outputDeviceName_(QString()),
    outputCharSet_(QString()),
    synthShortNames_(false),
    forceGPSTypes_(false),
    enableCharSetXform_(false),
    debugLevel_(-1),
    inputBrowse_(QString()),
    outputBrowse_(QString()),
    previewGmap_(false),
    upgradeCheckMethod_(0),
    upgradeCheckTime_(QDateTime(QDate(2001, 1, 1), QTime(0, 0))),
    installationUuid_(QUuid::createUuid().toString()),
    upgradeCallbacks_(0),
    upgradeAccept_(0),
    upgradeDeclines_(0),
    upgradeErrors_(0),
    upgradeOffers_(0),
    runCount_(0),
    startupVersionCheck_(true),
    reportStatistics_(true),
    allowBetaUpgrades_(false),
    ignoreVersionMismatch_(false),
    disableDonateDialog_(false),
    donateSplashed_(QDateTime(QDate(2010, 1, 1), QTime(0, 0, 0)))
  {
  };
  
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

  void makeSettingGroup(SettingGroup &sg) {
    sg.addVarSetting(new IntSetting("app.inputType", inputType_));
    sg.addVarSetting(new StringSetting("app.inputFileFormat", inputFileFormat_));
    sg.addVarSetting(new StringSetting("app.inputDeviceFormat", inputDeviceFormat_));
    sg.addVarSetting(new StringSetting("app.inputCharSet", inputCharSet_));
    sg.addVarSetting(new StringSetting("app.inputDeviceName", inputDeviceName_));

    sg.addVarSetting(new BoolSetting("app.xlateWayPts", xlateWayPts_));
    sg.addVarSetting(new BoolSetting("app.xlateRoutes", xlateRoutes_));
    sg.addVarSetting(new BoolSetting("app.xlateTracks", xlateTracks_));

    sg.addVarSetting(new IntSetting("app.outputType", outputType_));
    sg.addVarSetting(new StringSetting("app.outputFileFormat", outputFileFormat_));
    sg.addVarSetting(new StringSetting("app.outputDeviceFormat", outputDeviceFormat_));
    sg.addVarSetting(new StringSetting("app.outputCharSet", outputCharSet_));
    sg.addVarSetting(new StringSetting("app.outputDeviceName", outputDeviceName_));

    sg.addVarSetting(new BoolSetting("app.synthShortNames", synthShortNames_));
    sg.addVarSetting(new BoolSetting("app.forceGPSTypes", forceGPSTypes_));
    sg.addVarSetting(new BoolSetting("app.enableCharSetXform", enableCharSetXform_));

    sg.addVarSetting(new StringSetting("app.inputBrowse", inputBrowse_));
    sg.addVarSetting(new StringSetting("app.outputBrowse", outputBrowse_));

    sg.addVarSetting(new BoolSetting("app.previewGmap", previewGmap_));
    sg.addVarSetting(new IntSetting("app.upgradeCheckMethod", upgradeCheckMethod_));
    sg.addVarSetting(new DateTimeSetting("app.upgradeCheckTime", upgradeCheckTime_));
    sg.addVarSetting(new DateTimeSetting("app.donateSplashed", donateSplashed_));
    sg.addVarSetting(new StringSetting("app.installationUuid", installationUuid_));
    sg.addVarSetting(new IntSetting("app.upgradeCallbacks", upgradeCallbacks_));
    sg.addVarSetting(new IntSetting("app.upgradeAccept", upgradeAccept_));
    sg.addVarSetting(new IntSetting("app.upgradeDeclines", upgradeDeclines_));
    sg.addVarSetting(new IntSetting("app.upgradeErrors", upgradeErrors_));
    sg.addVarSetting(new IntSetting("app.upgradeOffers", upgradeOffers_));
    sg.addVarSetting(new IntSetting("app.runCount", runCount_));

    // Global preferences.
    sg.addVarSetting(new BoolSetting("app.startupVersionCheck", startupVersionCheck_));
    sg.addVarSetting(new BoolSetting("app.reportStatistics", reportStatistics_));
    sg.addVarSetting(new BoolSetting("app.allowBetaUpgrades", allowBetaUpgrades_));
    sg.addVarSetting(new BoolSetting("app.ignoreVersionMismatch", ignoreVersionMismatch_));
    sg.addVarSetting(new BoolSetting("app.disableDonateDialog", disableDonateDialog_));

  }

  static const int noType_;
  static const int fileType_;
  static const int deviceType_;

  int inputType_;
  QString inputFileFormat_;
  QString inputDeviceFormat_;
  QStringList inputFileNames_;
  QString inputDeviceName_;
  QString inputCharSet_;

  bool xlateWayPts_;
  bool xlateRoutes_;
  bool xlateTracks_;
  
  int outputType_;
  QString outputFileFormat_;
  QString outputDeviceFormat_;
  QString outputFileName_;
  QString outputDeviceName_;
  QString outputCharSet_;

  bool synthShortNames_;
  bool forceGPSTypes_;
  bool enableCharSetXform_;
  int  debugLevel_;

  QString inputBrowse_, outputBrowse_;

  bool  previewGmap_;
  int   upgradeCheckMethod_;
  QDateTime upgradeCheckTime_;
  QString installationUuid_;
  int upgradeCallbacks_;
  int upgradeAccept_;
  int upgradeDeclines_;
  int upgradeErrors_;
  int upgradeOffers_;
  int runCount_;

  // Global preferences.
  bool startupVersionCheck_;
  bool reportStatistics_;
  bool allowBetaUpgrades_;
  bool ignoreVersionMismatch_;
  bool disableDonateDialog_;
  QDateTime donateSplashed_;

};

#endif
