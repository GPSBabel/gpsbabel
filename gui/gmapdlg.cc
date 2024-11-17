// -*- C++ -*-
// $Id: gmapdlg.cpp,v 1.3 2009-11-02 20:38:02 robertl Exp $
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

#include "gmapdlg.h"
#include <QAbstractItemView>    // for QAbstractItemView
#include <QDateTime>            // for QDateTime, operator<, operator>
#include <QFrame>               // for QFrame
#include <QHBoxLayout>          // for QHBoxLayout
#include <QHeaderView>          // for QHeaderView
#include <QItemSelectionModel>  // for QItemSelectionModel
#include <QMenu>                // for QMenu
#include <QModelIndexList>      // for QModelIndexList
#include <QStandardItemModel>   // for QStandardItemModel
#include <QTreeView>            // for QTreeView
#include <Qt>                   // for CheckState, ContextMenuPolicy
#include <utility>              // for as_const
#include "appname.h"            // for appName
#include "gpx.h"                // for GpxWaypoint, GpxTrack, GpxRoute, Gpx, GpxItem, GpxTrackPoint, GpxTrackSegment
#include "latlng.h"             // for LatLn

//------------------------------------------------------------------------
class StandardItem: public QStandardItem
{
public:
  StandardItem(const QString& text): QStandardItem(text)
  {
    this->setEditable(false);
  }
};

//------------------------------------------------------------------------
QString GMapDialog::formatLength(double l)
{
  double metricLength = l;
  QString metricUnit = tr("meters");
  int metricPrecision = 2;
  if (l > 1000.0) {
    metricLength = l/1000.0;
    metricUnit = "km";
    metricPrecision = 3;
  }

  double fpsLength = l*1000.0/25.4/12.0;
  QString fpsUnit = tr("feet");
  int fpsPrecision = 1;
  if (fpsLength >5280.0) {
    fpsLength /= 5280.0;
    fpsUnit = tr("miles");
    fpsPrecision = 3;
  }
  return QString(tr("Length: %1 %2\n  %3 %4")
                 .arg(metricLength, 0, 'f', metricPrecision)
                 .arg(metricUnit)
                 .arg(fpsLength, 0, 'f', fpsPrecision)
                 .arg(fpsUnit));

}
//------------------------------------------------------------------------
void GMapDialog::appendWaypointInfo(QStandardItem* it, const GpxWaypoint& wpt)
{
  it->appendRow(new StandardItem(tr("Lat: %1").arg(wpt.getLocation().lat(), 0, 'f', 7)));
  it->appendRow(new StandardItem(tr("Lng: %1").arg(wpt.getLocation().lng(), 0, 'f', 7)));
  if (wpt.getDescription() != QString()) {
    it->appendRow(new StandardItem(tr("Desc: %1").arg(wpt.getDescription())));
  }
  if (wpt.getComment() != QString() && wpt.getComment() != wpt.getDescription()) {
    it->appendRow(new StandardItem(tr("Cmt: %1").arg(wpt.getComment())));
  }
  if (wpt.getElevation() > -50000) {
    it->appendRow(new StandardItem(tr("Ele: %1").arg(wpt.getElevation())));
  }

}

//------------------------------------------------------------------------
void GMapDialog::appendTrackInfo(QStandardItem* it, const GpxTrack& trk)
{
  QDateTime startTime;
  QDateTime stopTime;
  bool first = true;
  int count = 0;
  for (const GpxTrackSegment& seg : trk.getTrackSegments()) {
    for (const GpxTrackPoint& pt : seg.getTrackPoints()) {
      count++;
      QDateTime t = pt.getDateTime();
      if (!t.isValid()) {
        continue;
      }
      if (first) {
        startTime = t;
        stopTime = t;
        first = false;
      } else {
        if (t < startTime) {
          startTime = t;
        }
        if (t > stopTime) {
          stopTime = t;
        }
      }
    }
  }
  if (startTime.isValid()) {
    it->appendRow(new StandardItem(tr("Start: %1")
                                   .arg(startTime.toString(u"yyyy-MMM-dd HH:mm:ss"))));
    it->appendRow(new StandardItem(tr("Stop: %1")
                                   .arg(stopTime.toString(u"yyyy-MMM-dd HH:mm:ss"))));
  }
  it->appendRow(new StandardItem(tr("Points: %1").arg(count)));

  it->appendRow(new StandardItem(formatLength(trk.length())));

}

//------------------------------------------------------------------------
void GMapDialog::appendRouteInfo(QStandardItem* it, const GpxRoute& rte)
{
  it->appendRow(new StandardItem(formatLength(rte.length())));
}

//------------------------------------------------------------------------
GMapDialog::GMapDialog(QWidget* parent, const QString& gpxFileName, QPlainTextEdit* te): QDialog(parent)
{
  ui_.setupUi(this);
  this->setWindowTitle(QString(appName) + " " + QString("Google Maps"));
  gpx_.read(gpxFileName);

  mapWidget_ = new Map(this, gpx_, te);
  auto* lay = new QHBoxLayout(ui_.frame);
  lay->setContentsMargins(0, 0, 0, 0);
  lay->addWidget(mapWidget_);

  model_ = new QStandardItemModel(this);
  menuIndex_ = -1; // Actually set for real in showContextMenu().

  wptItem_ = new StandardItem(tr("Waypoints"));
  wptItem_->setCheckable(true);
  wptItem_->setCheckState(Qt::Checked);
  model_->appendRow(wptItem_);
  for (const auto& wpt : std::as_const(gpx_.getWaypoints())) {
    QStandardItem* it = new StandardItem(wpt.getName());
    wptItem_->appendRow(it);
    it->setCheckable(true);
    it->setCheckState(Qt::Checked);
    appendWaypointInfo(it, wpt);
  }

  trkItem_ = new StandardItem(tr("Tracks"));
  trkItem_->setCheckable(true);
  trkItem_->setCheckState(Qt::Checked);
  model_->appendRow(trkItem_);
  for (const auto& trk : std::as_const(gpx_.getTracks())) {
    QStandardItem* it = new StandardItem(trk.getName());
    trkItem_->appendRow(it);
    it->setCheckable(true);
    it->setCheckState(Qt::Checked);
    appendTrackInfo(it, trk);
  }

  rteItem_ = new StandardItem(tr("Routes"));
  rteItem_->setCheckable(true);
  rteItem_->setCheckState(Qt::Checked);
  model_->appendRow(rteItem_);
  for (const auto& rte : std::as_const(gpx_.getRoutes())) {
    QStandardItem* it = new StandardItem(rte.getName());
    rteItem_->appendRow(it);
    it->setCheckable(true);
    it->setCheckState(Qt::Checked);
    appendRouteInfo(it, rte);
  }

  ui_.treeView->header()->hide();
  ui_.treeView->setModel(model_);
  ui_.treeView->setExpandsOnDoubleClick(false);
  connect(model_, &QStandardItemModel::itemChanged,
          this,  &GMapDialog::itemChangedX);
  connect(mapWidget_, &Map::waypointClicked, this, &GMapDialog::waypointClickedX);
  connect(mapWidget_, &Map::routeClicked, this, &GMapDialog::routeClickedX);
  connect(mapWidget_, &Map::trackClicked, this, &GMapDialog::trackClickedX);
  connect(ui_.treeView, &QAbstractItemView::doubleClicked,
          this, &GMapDialog::treeDoubleClicked);
  connect(ui_.treeView->selectionModel(), &QItemSelectionModel::selectionChanged,
          this, &GMapDialog::selectionChangedX);

  ui_.treeView->setContextMenuPolicy(Qt::CustomContextMenu);
  connect(ui_.treeView, &QWidget::customContextMenuRequested,
          this, &GMapDialog::showContextMenu);
}

//-------------------------------------------------------------------------
void GMapDialog::itemChangedX(QStandardItem* it)
{
  bool show = (it->checkState() == Qt::Checked);
  if (it == trkItem_) {
    if (show) {
      mapWidget_->showTracks(gpx_.getTracks());
    } else {
      mapWidget_->hideAllTracks();
    }
  }

  else if (it == wptItem_) {
    if (show) {
      mapWidget_->showWaypoints(gpx_.getWaypoints());
    } else {
      mapWidget_->hideAllWaypoints();
    }
  }

  else if (it == rteItem_) {
    if (show) {
      mapWidget_->showRoutes(gpx_.getRoutes());
    } else {
      mapWidget_->hideAllRoutes();
    }
  }

  else {
    // Individual items, find the right one.
    const QStandardItem* parent = it->parent();
    int row = it->row();
    qDebug() << "item name" << it->text() << "parent" << ((parent == nullptr)? "none" : parent->text()) << "row" << row;

    if (parent == wptItem_) {
      GpxItem& git = gpx_.getWaypoints()[row];
      git.setVisible(show);
      mapWidget_->setWaypointVisibility(row, show);
    } else if (parent == trkItem_) {
      GpxItem& git = gpx_.getTracks()[row];
      git.setVisible(show);
      mapWidget_->setTrackVisibility(row, show);
    } else if (parent == rteItem_) {
      GpxItem& git = gpx_.getRoutes()[row];
      git.setVisible(show);
      mapWidget_->setRouteVisibility(row, show);
    }
  }
}

//-------------------------------------------------------------------------
void GMapDialog::treeDoubleClicked(const QModelIndex& idx)
{
qDebug() << "tree dbl click";
  QStandardItem* it = model_->itemFromIndex(idx);
  const QStandardItem* parent = it->parent();
  int row = it->row();
  if (parent == wptItem_) {
    it->setCheckState(Qt::Checked);
    gpx_.getWaypoints()[row].setVisible(true);
    mapWidget_->panTo(gpx_.getWaypoints()[row].getLocation());
    mapWidget_->setWaypointVisibility(row, true);
  } else if (parent == trkItem_) {
    mapWidget_->frameTrack(row);
    it->setCheckState(Qt::Checked);
    gpx_.getTracks()[row].setVisible(true);
    mapWidget_->setTrackVisibility(row, true);
  } else if (parent == rteItem_) {
    mapWidget_->frameRoute(row);
    it->setCheckState(Qt::Checked);
    gpx_.getRoutes()[row].setVisible(true);
    mapWidget_->setRouteVisibility(row, true);
  }
}

//-------------------------------------------------------------------------
void GMapDialog::waypointClickedX(int i)
{
  const QStandardItem* it = wptItem_->child(i);
  if (it != nullptr) {
    QModelIndex idx = model_->indexFromItem(it);
    ui_.treeView->scrollTo(idx, QAbstractItemView::PositionAtCenter);
    ui_.treeView->selectionModel()->select(idx, QItemSelectionModel::ClearAndSelect);
  }
}
//-------------------------------------------------------------------------
void GMapDialog::trackClickedX(int i)
{
  const QStandardItem* it = trkItem_->child(i);
  if (it != nullptr) {
    QModelIndex idx = model_->indexFromItem(it);
    ui_.treeView->scrollTo(idx, QAbstractItemView::PositionAtCenter);
    ui_.treeView->selectionModel()->select(idx, QItemSelectionModel::ClearAndSelect);
  }
}

//-------------------------------------------------------------------------
void GMapDialog::routeClickedX(int i)
{
  const QStandardItem* it = rteItem_->child(i);
  if (it != nullptr) {
    QModelIndex idx = model_->indexFromItem(it);
    ui_.treeView->scrollTo(idx, QAbstractItemView::PositionAtCenter);
    ui_.treeView->selectionModel()->select(idx, QItemSelectionModel::ClearAndSelect);
  }
}

//-------------------------------------------------------------------------
void GMapDialog::selectionChangedX(const QItemSelection& sel,  const QItemSelection& desel)
{
qDebug() << "selectionChangedX";
  for (const QModelIndexList idxs = desel.indexes(); const auto& idx : idxs) {
    const QStandardItem* it = model_->itemFromIndex(idx);
    const QStandardItem* parent = it->parent();
    if (parent == wptItem_) {
      int row = it->row();
      mapWidget_->setWaypointColorBlue(row);
    }
  }
  for (const QModelIndexList idxs = sel.indexes(); const auto& idx : idxs) {
    const QStandardItem* it = model_->itemFromIndex(idx);
    const QStandardItem* parent = it->parent();
    if (parent == wptItem_) {
      int row = it->row();
      mapWidget_->setWaypointColorRed(row);
    }
  }
}

//------------------------------------------------------------------------
void GMapDialog::expandCollapseAll(QStandardItem* top, bool exp)
{
  ui_.treeView->setExpanded(model_->indexFromItem(top), exp);
  for (int row = 0; row < top->rowCount(); ++row) {
    const QStandardItem* child = top->child(row);
    ui_.treeView->setExpanded(model_->indexFromItem(child), exp);
  }
}

//------------------------------------------------------------------------
void GMapDialog::expandAllWaypoints()
{
  expandCollapseAll(wptItem_, true);
}
//------------------------------------------------------------------------
void GMapDialog::expandAllTracks()
{
  expandCollapseAll(trkItem_, true);
}
//------------------------------------------------------------------------
void GMapDialog::expandAllRoutes()
{
  expandCollapseAll(rteItem_, true);
}

//------------------------------------------------------------------------
void GMapDialog::collapseAllWaypoints()
{
  expandCollapseAll(wptItem_, false);
}
//------------------------------------------------------------------------
void GMapDialog::collapseAllTracks()
{
  expandCollapseAll(trkItem_, false);
}
//------------------------------------------------------------------------
void GMapDialog::collapseAllRoutes()
{
  expandCollapseAll(rteItem_, false);
}

//------------------------------------------------------------------------
void GMapDialog::checkUncheckAll(QStandardItem* top, bool ck)
{
  top->setCheckState(ck ? Qt::Checked: Qt::Unchecked);
  for (int row = 0; row < top->rowCount(); ++row) {
    QStandardItem* child = top->child(row);
    child->setCheckState(ck ? Qt::Checked: Qt::Unchecked);
  }
}
//------------------------------------------------------------------------
void GMapDialog::showAllWaypoints()
{
  checkUncheckAll(wptItem_, true);
  mapWidget_->showWaypoints(gpx_.getWaypoints());
}
//------------------------------------------------------------------------
void GMapDialog::showAllTracks()
{
  checkUncheckAll(trkItem_, true);
  mapWidget_->showTracks(gpx_.getTracks());
}

//------------------------------------------------------------------------
void GMapDialog::showAllRoutes()
{
  checkUncheckAll(rteItem_, true);
  mapWidget_->showRoutes(gpx_.getRoutes());
}

//------------------------------------------------------------------------
void GMapDialog::hideAllWaypoints()
{
  checkUncheckAll(wptItem_, false);
  mapWidget_->showWaypoints(gpx_.getWaypoints());
}
//------------------------------------------------------------------------
void GMapDialog::hideAllTracks()
{
  checkUncheckAll(trkItem_, false);
  mapWidget_->showTracks(gpx_.getTracks());
}
//------------------------------------------------------------------------
void GMapDialog::hideAllRoutes()
{
  checkUncheckAll(rteItem_, false);
  mapWidget_->showRoutes(gpx_.getRoutes());
}

//------------------------------------------------------------------------
template<typename GpxItemType> requires std::derived_from<GpxItemType, GpxItem>
void GMapDialog::showOnlyThis(QList<GpxItemType>& list, QStandardItem* top)
{
  for (int row = 0; row < top->rowCount(); ++row) {
    list[row].setVisible(row == menuIndex_);
    QStandardItem* child = top->child(row);
    child->setCheckState(row == menuIndex_? Qt::Checked: Qt::Unchecked);
  }
  top->setCheckState(Qt::Checked);
  mapWidget_->showWaypoints(gpx_.getWaypoints());
}
//------------------------------------------------------------------------
void GMapDialog::showOnlyThisWaypoint()
{
  showOnlyThis(gpx_.getWaypoints(), wptItem_);
}
//------------------------------------------------------------------------
void GMapDialog::showOnlyThisTrack()
{
  showOnlyThis(gpx_.getTracks(), trkItem_);
}
//------------------------------------------------------------------------
void GMapDialog::showOnlyThisRoute()
{
  showOnlyThis(gpx_.getRoutes(), rteItem_);
}

//------------------------------------------------------------------------
void GMapDialog::showContextMenu(const QPoint& pt)
{
qDebug() << "show context menu";
  QModelIndex idx = ui_.treeView->indexAt(pt);
  if (idx.isValid()) {
    const QStandardItem* it = model_->itemFromIndex(idx);
    if (it == wptItem_) {
      QMenu menu(this);
      menu.addAction(tr("Show All Waypoints"), this, &GMapDialog::showAllWaypoints);
      menu.addAction(tr("Hide All Waypoints"), this, &GMapDialog::hideAllWaypoints);
      menu.addAction(tr("Expand All"), this, &GMapDialog::expandAllWaypoints);
      menu.addAction(tr("Collapse All"), this, &GMapDialog::collapseAllWaypoints);
      menu.exec(ui_.treeView->mapToGlobal(pt));
    } else if (it == rteItem_) {
      QMenu menu(this);
      menu.addAction(tr("Show All Routes"), this, &GMapDialog::showAllRoutes);
      menu.addAction(tr("Hide All Routes"), this, &GMapDialog::hideAllRoutes);
      menu.addAction(tr("Expand All"), this, &GMapDialog::expandAllRoutes);
      menu.addAction(tr("Collapse All"), this, &GMapDialog::collapseAllRoutes);
      menu.exec(ui_.treeView->mapToGlobal(pt));
    } else if (it == trkItem_) {
      QMenu menu(this);
      menu.addAction(tr("Show All Tracks"), this, &GMapDialog::showAllTracks);
      menu.addAction(tr("Hide All Tracks"), this, &GMapDialog::hideAllTracks);
      menu.addAction(tr("Expand All"), this, &GMapDialog::expandAllTracks);
      menu.addAction(tr("Collapse All"), this, &GMapDialog::collapseAllTracks);
      menu.exec(ui_.treeView->mapToGlobal(pt));
    } else if (it !=  nullptr) {
      const QStandardItem* parent = it->parent();
      int row = it->row();
      if (parent == wptItem_) {
        QMenu menu(this);
        menu.addAction(tr("Show Only This Waypoint"), this, &GMapDialog::showOnlyThisWaypoint);
        menuIndex_ = row;
        menu.exec(ui_.treeView->mapToGlobal(pt));
      } else if (parent == trkItem_) {
        QMenu menu(this);
        menu.addAction(tr("Show Only This Track"), this, &GMapDialog::showOnlyThisTrack);
        menuIndex_ = row;
        menu.exec(ui_.treeView->mapToGlobal(pt));
      } else if (parent == rteItem_) {
        QMenu menu(this);
        menu.addAction(tr("Show Only This Route"), this, &GMapDialog::showOnlyThisRoute);
        menuIndex_ = row;
        menu.exec(ui_.treeView->mapToGlobal(pt));
      }
    }
  }
}
