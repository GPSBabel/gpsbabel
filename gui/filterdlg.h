// -*- C++ -*-
// $Id: filterdlg.h,v 1.1 2009-07-05 21:14:56 robertl Exp $
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
#ifndef FILTER_H
#define FILTER_H

#include <QDialog>          // for QDialog
#include <QList>            // for QList
#include <QListWidgetItem>  // for QListWidgetItem
#include <QObject>          // for Q_OBJECT, slots
#include <QStackedWidget>   // for QStackedWidget
#include <QString>          // for QString
#include <QWidget>          // for QWidget
#include "filterdata.h"     // for AllFiltersData
#include "filterwidgets.h"  // for FilterWidget
#include "ui_filterui.h"    // for Ui_FilterDlg

class FilterDialog: public QDialog
{
  Q_OBJECT
public:
  FilterDialog(QWidget* parent, AllFiltersData& fd_);

  void runDialog();


private:
  static int lastPage_;
  QList <FilterWidget*>pages_;
  QList <bool*>usePages_;
  QStackedWidget* widgetStack_;
  Ui_FilterDlg ui_;
  AllFiltersData& fd_;

  void addFilterPage(const QString& name, FilterWidget* w, bool*);

private slots:
  void pageSelectionChanged(int);
  void itemClickedX(QListWidgetItem*);
  void resetX();
  void helpX();
};

#endif
