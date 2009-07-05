// -*- C++ -*-
// $Id: babeldata.h,v 1.1 2009-07-05 21:14:56 robertl Exp $
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
#include "setting.h"


class BabelData {
public:
  BabelData(): 
    inputType(fileType), 
    inputFileFormat(QString()),
    inputDeviceFormat(QString()),
    inputFileNames(QStringList()),
    inputDeviceName(QString()),
    inputCharSet(QString()),
    xlateWayPts(true),
    xlateRoutes(true),
    xlateTracks(true),
    outputType(fileType),
    outputFileFormat(QString()),
    outputDeviceFormat(QString()),
    outputFileName(QString()),
    outputDeviceName(QString()),
    outputCharSet(QString()),
    synthShortNames(false),
    forceGPSTypes(false),
    enableCharSetXform(false),
    debugLevel(-1),
    inputBrowse(QString()),
    outputBrowse(QString()),
    previewGmap(false)
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
    sg.addVarSetting(new IntSetting("app.inputType", inputType));
    sg.addVarSetting(new StringSetting("app.inputFileFormat", inputFileFormat));
    sg.addVarSetting(new StringSetting("app.inputDeviceFormat", inputDeviceFormat));
    sg.addVarSetting(new StringSetting("app.inputCharSet", inputCharSet));
    sg.addVarSetting(new StringSetting("app.inputDeviceName", inputDeviceName));

    sg.addVarSetting(new BoolSetting("app.xlateWayPts", xlateWayPts));
    sg.addVarSetting(new BoolSetting("app.xlateRoutes", xlateRoutes));
    sg.addVarSetting(new BoolSetting("app.xlateTracks", xlateTracks));

    sg.addVarSetting(new IntSetting("app.outputType", outputType));
    sg.addVarSetting(new StringSetting("app.outputFileFormat", outputFileFormat));
    sg.addVarSetting(new StringSetting("app.outputDeviceFormat", outputDeviceFormat));
    sg.addVarSetting(new StringSetting("app.outputCharSet", outputCharSet));
    sg.addVarSetting(new StringSetting("app.outputDeviceName", outputDeviceName));

    sg.addVarSetting(new BoolSetting("app.synthShortNames", synthShortNames));
    sg.addVarSetting(new BoolSetting("app.forceGPSTypes", forceGPSTypes));
    sg.addVarSetting(new BoolSetting("app.enableCharSetXform", enableCharSetXform));

    sg.addVarSetting(new StringSetting("app.inputBrowse", inputBrowse));
    sg.addVarSetting(new StringSetting("app.outputBrowse", outputBrowse));

    sg.addVarSetting(new BoolSetting("app.previewGmap", previewGmap));
  }

  static const int noType;
  static const int fileType;
  static const int deviceType;

  int inputType;
  QString inputFileFormat;
  QString inputDeviceFormat;
  QStringList inputFileNames;
  QString inputDeviceName;
  QString inputCharSet;

  bool xlateWayPts;
  bool xlateRoutes;
  bool xlateTracks;
  
  int outputType;
  QString outputFileFormat;
  QString outputDeviceFormat;
  QString outputFileName;
  QString outputDeviceName;
  QString outputCharSet;

  bool synthShortNames;
  bool forceGPSTypes;
  bool enableCharSetXform;
  int  debugLevel;

  QString inputBrowse, outputBrowse;

  bool  previewGmap;

};

#endif
