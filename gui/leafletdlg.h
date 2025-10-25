//
//  Copyright (C) 2025  Robert Lipe
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 2 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//  General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
// -*- C++ -*-
#ifndef LEAFLETDLG_H
#define LEAFLETDLG_H

#include <QDialog>
#include <QItemSelection>
#include <QModelIndex>
#include <QObject>
#include <QPlainTextEdit>
#include <QPoint>
#include <QStandardItem>
#include <QStandardItemModel>
#include <QString>
#include <QWidget>
#include "gpx.h"
#include "leafletmap.h"
#include "ui_leafletdlg.h"         // for Ui_LeafletMapDlg

enum ItemDataRole {
    OriginalIndexRole = Qt::UserRole + 1
};

class LeafletMapDialog: public QDialog
{
  Q_OBJECT

public:
  LeafletMapDialog(QWidget* parent, const Gpx& mapData, const QString& geojsonData, QPlainTextEdit* te);


private:
  static constexpr bool debug_ = false;
  Ui::LeafletMapDlg ui_;
  LeafletMap* mapWidget_;
  QStandardItemModel* model_;
  QStandardItem* wptItem_;
  QStandardItem* trkItem_;
  QStandardItem* rteItem_;
  const Gpx& gpx_;
  bool itemChangedActive_{false};

private slots:
  void itemChangedX(QStandardItem* it);
  void treeDoubleClicked(const QModelIndex& idx);
  void itemClickedX(const QStandardItem* it);
  void selectionChangedX(const QItemSelection& sel, const QItemSelection& desel);
  void showContextMenu(const QPoint& pt);
  void routePointClicked(int i);
  void trackClicked(int i);

protected:
  QString formatLength(double l);
  void appendWaypointInfo(QStandardItem* it, const GpxWaypoint& wpt);
  void appendTrackInfo(QStandardItem* it, const GpxTrack& trk);
  void appendRouteInfo(QStandardItem* it, const GpxRoute& rte);
  void showHideChild(const QStandardItem* child);
  void showHideChildren(const QStandardItem* top);
  void expandCollapseAll(QStandardItem* top, bool exp);
  void showHideAll(QStandardItem* top, bool ck);
  void showOnlyThis(QStandardItem* top, int menuRow);
  void showTopContextMenu(const QStringList& text, QStandardItem* top, const QPoint& pt);
  void showChildContextMenu(const QString& text, const QStandardItem* child, const QPoint& pt);
  void trace(const QString& label, const QStandardItem* it);
  void onMapLoadedAndRendered();
};
#endif
