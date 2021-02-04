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

#include <QSettings>
#include <QStringList>
#include <QUuid>
#include "setting.h"


class BabelData
{
public:

  /* Constants */

  static constexpr int noType_ = -1;
  static constexpr int fileType_ = 0;
  static constexpr int deviceType_ = 1;

  /* Member Functions */

  void saveSettings(QSettings& st)
  {
    makeSettingGroup().saveSettings(st);
  }
  void restoreSettings(QSettings& st)
  {
    makeSettingGroup().restoreSettings(st);
  }
  SettingGroup makeSettingGroup()
  {
    SettingGroup sg;
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

    return sg;
  }

  /* Data Members */

  int inputType_{fileType_};
  QString inputFileFormat_;
  QString inputDeviceFormat_;
  QStringList inputFileNames_;
  QString inputDeviceName_;
  QString inputCharSet_;

  bool xlateWayPts_{true};
  bool xlateRoutes_{true};
  bool xlateTracks_{true};

  int outputType_{fileType_};
  QString outputFileFormat_;
  QString outputDeviceFormat_;
  QString outputFileName_;
  QString outputDeviceName_;
  QString outputCharSet_;

  bool synthShortNames_{false};
  bool forceGPSTypes_{false};
  bool enableCharSetXform_{false};
  int debugLevel_{-1};

  QString inputBrowse_;
  QString outputBrowse_;

  bool previewGmap_{false};
  int upgradeCheckMethod_{0};
  QDateTime upgradeCheckTime_{QDateTime(QDate(2001, 1, 1), QTime(0, 0))};
  QString installationUuid_{QUuid::createUuid().toString()};
  int upgradeCallbacks_{0};
  int upgradeAccept_{0};
  int upgradeDeclines_{0};
  int upgradeErrors_{0};
  int upgradeOffers_{0};
  int runCount_{0};

  // Global preferences.
  bool startupVersionCheck_{true};
  bool reportStatistics_{true};
  bool allowBetaUpgrades_{false};
  bool ignoreVersionMismatch_{false};
  bool disableDonateDialog_{false};
  QDateTime donateSplashed_{QDateTime(QDate(2010, 1, 1), QTime(0, 0, 0))};

};

#endif
