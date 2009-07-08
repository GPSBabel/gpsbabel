// -*- C++ -*-
// $Id: maindlg.h,v 1.1 2009-07-05 21:14:56 robertl Exp $
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
#ifndef MAINDLG_H
#define MAINDLG_H

#include "ui_mainui.h"
#include "persistdlg.h"
#include "format.h"
#include "filterdata.h"
#include "babeldata.h"

class MainDlg: public PersistentDialog {
  Q_OBJECT

  
  public:
  MainDlg(QWidget* parent);

private:
  Ui_MainDlg     ui;
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
  void loadCharSetCombos();
  void checkCharSetCombos();
  QString charSetFromCombo(QComboBox *);
  void setComboToCharSet(QComboBox *, const QString &);
  void updateFilterStatus();
  void setWidgetValues();
  void getWidgetValues();

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
  void applyClicked();
  void rejectedClicked();
  void aboutClicked();
  void helpClicked();
  void filtersClicked();
  void resetFormatDefaults();

};


#endif
