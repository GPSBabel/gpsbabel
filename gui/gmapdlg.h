// -*- C++ -*-
// $Id: gmapdlg.h,v 1.2 2009-11-02 20:38:02 robertl Exp $
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
#ifndef GMAPDLG_H
#define GMAPDLG_H

#include <QDialog>             // for QDialog
#include <QItemSelection>      // for QItemSelection
#include <QModelIndex>         // for QModelIndex
#include <QObject>             // for Q_OBJECT, slots
#include <QPlainTextEdit>      // for QPlainTextEdit
#include <QPoint>              // for QPoint
#include <QStandardItem>       // for QStandardItem
#include <QStandardItemModel>  // for QStandardItemModel
#include <QString>             // for QString
#include <QWidget>             // for QWidget
#include "gpx.h"               // for Gpx, GpxRoute, GpxTrack, GpxWaypoint
#include "map.h"               // for Map
#include "ui_gmapui.h"         // for Ui_GMapDlg

class GMapDialog: public QDialog
{
  Q_OBJECT
public:
  GMapDialog(QWidget* parent, const Gpx& mapData, QPlainTextEdit* te);

private:
  static constexpr bool debug_ = true;

  Ui_GMapDlg ui_;
  Map* mapWidget_;
  QStandardItemModel* model_;
  QStandardItem* wptItem_;
  QStandardItem* trkItem_;
  QStandardItem* rteItem_;
  const Gpx& gpx_;

  static void appendWaypointInfo(QStandardItem* it, const GpxWaypoint& wpt);
  static void appendTrackInfo(QStandardItem* it, const GpxTrack& trk);
  static void appendRouteInfo(QStandardItem* it, const GpxRoute& rte);

  static QString formatLength(double l);

  void expandCollapseAll(QStandardItem* top, bool exp);
  static void checkUncheckAll(QStandardItem* top, bool ck);
  void showHideChild(const QStandardItem* child);
  void showHideChildren(const QStandardItem* top);
  static void showOnlyThis(QStandardItem* top, int menuRow);
  void showTopContextMenu(const QStringList& text, QStandardItem* top, const QPoint& pt);
  void showChildContextMenu(const QString& text, const QStandardItem* child, const QPoint& pt);

  //
private slots:
  void itemChangedX(QStandardItem* it);
  void waypointClickedX(int i);
  void trackClickedX(int i);
  void routeClickedX(int i);
  void treeDoubleClicked(const QModelIndex& idx);
  void selectionChangedX(const QItemSelection& sel,  const QItemSelection& desel);
  void showContextMenu(const QPoint& pt);
};

#endif
