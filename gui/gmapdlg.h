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

#include <QStackedWidget>
#include <QStandardItem>
#include <QModelIndex>
#include "ui_gmapui.h"
#include "gpx.h"
#include "map.h"

class GMapDialog: public QDialog
{
  Q_OBJECT
public:
  GMapDialog(QWidget* parent, const QString& gpxFileName, QPlainTextEdit* te);

private:
  Ui_GMapDlg ui_;
  Map* mapWidget_;
  QStandardItemModel* model_;
  QStandardItem* wptItem_, *trkItem_, *rteItem_;
  QList<QStandardItem*> wptList_, trkList_, rteList_;
  Gpx gpx_;
  int menuIndex_;

  void appendWaypointInfo(QStandardItem* it, const GpxWaypoint& wpt);
  void appendTrackInfo(QStandardItem* it, const GpxTrack& trk);
  void appendRouteInfo(QStandardItem* it, const GpxRoute& rte);

  int waypointIndex(QStandardItem* it);
  int trackIndex(QStandardItem* it);
  int routeIndex(QStandardItem* it);
  QString formatLength(double l);

  //
private slots:
  void itemChangedX(QStandardItem*);
  void waypointClickedX(int i);
  void trackClickedX(int i);
  void routeClickedX(int i);
  void treeDoubleClicked(const QModelIndex& idx);
  void selectionChangedX(const QItemSelection&,  const QItemSelection&);
  void copyButtonClickedX();
  void showContextMenu(const QPoint&);


  void expandCollapseAll(const QList<QStandardItem*>& li,
                         QStandardItem* it, bool exp);
  void checkUncheckAll(const QList<QStandardItem*>& li,
                       QStandardItem* it, bool exp);
  void expandAllWaypoints();
  void expandAllTracks();
  void expandAllRoutes();

  void collapseAllWaypoints();
  void collapseAllTracks();
  void collapseAllRoutes();

  void hideAllWaypoints();
  void hideAllTracks();
  void hideAllRoutes();

  void showAllWaypoints();
  void showAllTracks();
  void showAllRoutes();

  void showOnlyThisWaypoint();
  void showOnlyThisTrack();
  void showOnlyThisRoute();
};

#endif
