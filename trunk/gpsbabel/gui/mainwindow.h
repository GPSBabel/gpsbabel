// -*- C++ -*-
// $Id: mainwindow.h,v 1.5 2009-08-28 17:21:37 robertl Exp $
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
#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include "ui_mainwinui.h"
#include "format.h"
#include "filterdata.h"
#include "babeldata.h"
#include "upgrade.h"

class MainWindow: public QMainWindow {
  Q_OBJECT


  public:
  MainWindow(QWidget* parent);
  ~MainWindow();

private:
  Ui_MainWindow     ui;
  QList<Format>  formatList;
  QString        babelVersion;
  QPixmap        lights[4];
  QStringList    charSets;
  AllFiltersData filterData;
  BabelData      bd;
  bool           fmtChgInterlock;

private:
  void loadFormats();
  QString filterForFormat(int idx);
  bool    filterForFormatIncludes(int idx, const QString &s);
  int  formatIndexFromName(bool isFile, const QString &);
  QList<int>inputFileFormatIndices();
  QList<int>inputDeviceFormatIndices();
  QList<int>outputFileFormatIndices();
  QList<int>outputDeviceFormatIndices();
  int  currentComboFormatIndex(QComboBox *comboBox);
  bool isOkToGo();
  bool runGpsbabel(const QStringList &args, QString &errorString, QString &outputString);
  void crossCheckInOutFormats();
  void setIndicatorLights(QLabel *label, const QString type, int code) ;
  void displayOptionsText(QLineEdit *, QComboBox *, bool);

  void saveSettings();
  void restoreSettings();
  void setComboToFormat(QComboBox *comboBox, const QString &, bool isFile);
  void setComboToDevice(QComboBox *comboBox, const QString &);

  void loadDeviceNameCombos();
  void loadInputDeviceNameCombo(QString format);
  void loadOutputDeviceNameCombo(QString format);
  bool MainWindow::formatSupportsUSB(QString format);
  bool MainWindow::formatSupportsSerial(QString format);
  void loadCharSetCombos();
  void checkCharSetCombos();
  QString charSetFromCombo(QComboBox *);
  void setComboToCharSet(QComboBox *, const QString &);
  void updateFilterStatus();
  void setWidgetValues();
  void getWidgetValues();
  UpgradeCheck *upgrade;
  void osLoadDeviceNameCombos(QComboBox*);

protected:
  void closeEvent(QCloseEvent*);

 private slots:
  void inputFileOptBtnClicked();
  void inputDeviceOptBtnClicked();
  void inputOptionButtonClicked();
  void inputFormatChanged(int);
  void browseInputFile();
  void outputFileOptBtnClicked();
  void outputDeviceOptBtnClicked();
  void outputOptionButtonClicked();
  void outputFormatChanged(int);
  void browseOutputFile();
  void moreOptionButtonClicked();
  void applyActionX();
  void aboutActionX();
  void helpActionX();
  void closeActionX();
  void filtersClicked();
  void resetFormatDefaults();

};


#endif
