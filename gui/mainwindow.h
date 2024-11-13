// -*- C++ -*-
// $Id: mainwindow.h,v 1.13 2010-11-01 03:30:42 robertl Exp $
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
#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QCloseEvent>      // for QCloseEvent
#include <QComboBox>        // for QComboBox
#include <QDragEnterEvent>  // for QDragEnterEvent
#include <QDropEvent>       // for QDropEvent
#include <QEvent>           // for QEvent
#include <QLabel>           // for QLabel
#include <QLineEdit>        // for QLineEdit
#include <QList>            // for QList
#include <QMainWindow>      // for QMainWindow
#include <QObject>          // for Q_OBJECT, slots
#include <QPixmap>          // for QPixmap
#include <QString>          // for QString
#include <QStringList>      // for QStringList
#include <QTranslator>      // for QTranslator
#include <QWidget>          // for QWidget

#include "babeldata.h"      // for BabelData
#include "filterdata.h"     // for AllFiltersData
#include "format.h"         // for Format
#include "ui_mainwinui.h"   // for Ui_MainWindow
#include "upgrade.h"        // for UpgradeCheck


class MainWindow: public QMainWindow
{
  Q_OBJECT


public:
  MainWindow(QWidget* parent);


private:
  Ui_MainWindow     ui_;
  QList<Format>  formatList_;
  QString        babelVersion_;
  bool		 isBeta_;
  QPixmap        lights_[4];
  AllFiltersData filterData_;
  BabelData      babelData_;
  bool           fmtChgInterlock_;
  QTranslator     translator_;     // translation for the GUI.
  QTranslator     translatorCore_; // translation for the core application.
  QTranslator     translatorQt_;   // translations for Qt.
  QString         currLang_;       // currently loaded language.

private:
  void loadFormats();
  void loadLanguage(const QString& rLanguage);
  void switchTranslator(QTranslator&, const QString&);
  QString filterForFormat(int idx);
  QString ensureExtensionPresent(const QString& nanme, int idx);
  QString findBabelVersion();
  bool    filterForFormatIncludes(int idx, const QString& s);
  int  formatIndexFromName(bool isFile, const QString&);
  QList<int>inputFileFormatIndices();
  QList<int>inputDeviceFormatIndices();
  QList<int>outputFileFormatIndices();
  QList<int>outputDeviceFormatIndices();
  int  currentComboFormatIndex(QComboBox* comboBox);
  bool isOkToGo();
  bool runGpsbabel(const QStringList& args, QString& errorString, QString& outputString);
  void crossCheckInOutFormats();
  void setIndicatorLights(QLabel* label, const QString& type, int code) ;
  void displayOptionsText(QLineEdit*, QComboBox*, bool);

  void saveSettings();
  void restoreSettings();
  void setComboToFormat(QComboBox* comboBox, const QString&, bool isFile);
  void setComboToDevice(QComboBox* comboBox, const QString&);

  void loadDeviceNameCombos();
  void loadInputDeviceNameCombo(const QString& format);
  void loadOutputDeviceNameCombo(const QString& format);
  bool formatSupportsUSB(const QString& format);
  bool formatSupportsSerial(const QString& format);
  void updateFilterStatus();
  void setWidgetValues();
  void getWidgetValues();
  UpgradeCheck* upgrade;
  bool allowBetaUpgrades();
  void osLoadDeviceNameCombos(QComboBox*);
  QString getFormatNameForExtension(const QString& ext);

protected:
  void closeEvent(QCloseEvent*) override;
  void changeEvent(QEvent*) override;

private slots:
  void aboutActionX();
  void applyActionX();
  void browseInputFile();
  void browseOutputFile();
  void closeActionX();
  void donateActionX();
  void dragEnterEvent(QDragEnterEvent*) override;
  void dropEvent(QDropEvent* event) override;
  void filtersClicked();
  void helpActionX();
  void inputDeviceOptBtnClicked();
  void inputFileOptBtnClicked();
  void inputFormatChanged(int);
  void inputOptionButtonClicked();
  void inputFileNameEdited();
  void moreOptionButtonClicked();
  void outputDeviceOptBtnClicked();
  void outputFileOptBtnClicked();
  void outputFileNameEdited();
  void outputFormatChanged(int);
  void outputOptionButtonClicked();
  void preferencesActionX();
  void visitWebsiteActionX();
  void resetFormatDefaults();
  void upgradeCheckActionX();


};


#endif
