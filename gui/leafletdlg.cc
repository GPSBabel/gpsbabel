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
// -*- C++ -*

#include <QAbstractItemView>
#include <QDateTime>
#include <QDebug>
#include <QFrame>
#include <QHBoxLayout>
#include <QHeaderView>
#include <QItemSelectionModel>
#include <QList>
#include <QMenu>
#include <QMetaObject>
#include <QModelIndexList>
#include <QStandardItemModel>
#include <QTreeView>
#include <Qt>
#include <QtGlobal>
#include "appname.h"
#include "gpx.h"
#include "leafletdlg.h"
#include "leafletmap.h"
#include "latlng.h"
#include "ui_leafletdlg.h"

//------------------------------------------------------------------------
class StandardItem: public QStandardItem
{
public:
  StandardItem(const QString& text) :
    QStandardItem(text)
  {
    this->setEditable(false);
  }
};

//------------------------------------------------------------------------
QString
LeafletMapDialog::formatLength(double l)
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
void
LeafletMapDialog::appendWaypointInfo(QStandardItem* it, const GpxWaypoint& wpt)
{
  it->appendRow(new StandardItem(tr("Lat: %1").arg(wpt.getLocation().lat(), 0, 'f', 7)));
  it->appendRow(new StandardItem(tr("Lng: %1").arg(wpt.getLocation().lng(), 0, 'f', 7)));
  if (!wpt.getDescription().isEmpty()) {
    it->appendRow(new StandardItem(tr("Desc: %1").arg(wpt.getDescription())));
  }
  if (!wpt.getComment().isEmpty() && wpt.getComment() != wpt.getDescription()) {
    it->appendRow(new StandardItem(tr("Cmt: %1").arg(wpt.getComment())));
  }
  if (wpt.getElevation() > -50000) {
    it->appendRow(new StandardItem(tr("Ele: %1").arg(wpt.getElevation())));
  }

}

//------------------------------------------------------------------------
void
LeafletMapDialog::appendTrackInfo(QStandardItem* it, const GpxTrack& trk)
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
void
LeafletMapDialog::appendRouteInfo(QStandardItem* it, const GpxRoute& rte)
{
  it->appendRow(new StandardItem(formatLength(rte.length())));
}

//------------------------------------------------------------------------
LeafletMapDialog::LeafletMapDialog(QWidget* parent,
                                   const Gpx& mapData, const QString& geojsonData, QPlainTextEdit* te) :
  QDialog(parent),
  gpx_(mapData)
{
  ui_.setupUi(this);
  this->setWindowTitle(QString(appName) + " " + QString("Leaflet Maps"));

  mapWidget_ = new LeafletMap(this, gpx_, geojsonData, te);
  auto* lay = new QHBoxLayout(ui_.frame);
  lay->setContentsMargins(0, 0, 0, 0);
  lay->addWidget(mapWidget_);


  model_ = new QStandardItemModel(this);
  model_->blockSignals(true);

  wptItem_ = new StandardItem(tr("Waypoints"));
  wptItem_->setCheckable(true);
  model_->appendRow(wptItem_);
  int waypointIdx = 0;
  for (const auto& wpt : gpx_.getWaypoints()) {
    QStandardItem* it = new StandardItem(wpt.getName());
    it->setData(waypointIdx, OriginalIndexRole);
    wptItem_->appendRow(it);
    it->setCheckable(true);
    it->setCheckState(Qt::Checked);
    appendWaypointInfo(it, wpt);
    waypointIdx++;
  }

  trkItem_ = new StandardItem(tr("Tracks"));
  trkItem_->setCheckable(true);
  model_->appendRow(trkItem_);
  int trackIdx = 0;
  for (const auto& trk : gpx_.getTracks()) {
    QStandardItem* it = new StandardItem(trk.getName());
    it->setData(trackIdx, OriginalIndexRole);
    trkItem_->appendRow(it);
    it->setCheckable(true);
    it->setCheckState(Qt::Checked);
    appendTrackInfo(it, trk);
    trackIdx++;
  }

  rteItem_ = new StandardItem(tr("Routes"));
  rteItem_->setCheckable(true);
  rteItem_->setCheckState(Qt::Checked);
  model_->appendRow(rteItem_);
  int routeIdx = 0;
  for (const auto& rte : gpx_.getRoutes()) {
    QStandardItem* it = new StandardItem(rte.getName());
    it->setData(routeIdx, OriginalIndexRole);
    rteItem_->appendRow(it);
    it->setCheckable(true);
    it->setCheckState(Qt::Checked);
    appendRouteInfo(it, rte);
    routeIdx++;
  }
  model_->blockSignals(false);


  ui_.treeView->header()->hide();
  ui_.treeView->setModel(model_);
  ui_.treeView->setExpandsOnDoubleClick(false);
  connect(model_, &QStandardItemModel::itemChanged,
          this, &LeafletMapDialog::itemChangedX);
  connect(mapWidget_, &LeafletMap::waypointClicked, this, [this](int i)->void {
    QStandardItem* item = wptItem_->child(i);
    if (item) itemClickedX(item);
  });
  connect(mapWidget_, &LeafletMap::routeClicked, this, [this](int i)->void {
    QStandardItem* item = rteItem_->child(i);
    if (item) itemClickedX(item);
  });
  connect(mapWidget_, &LeafletMap::routePointClicked, this, &LeafletMapDialog::routePointClicked);
  connect(mapWidget_, &LeafletMap::trackClicked, this, [this](int i)->void {
    QStandardItem* item = trkItem_->child(i);
    if (item) itemClickedX(item);
  });
  connect(ui_.treeView, &QAbstractItemView::doubleClicked,
          this, &LeafletMapDialog::treeDoubleClicked);
  connect(ui_.treeView->selectionModel(), &QItemSelectionModel::selectionChanged,
          this, &LeafletMapDialog::selectionChangedX);

  ui_.treeView->setContextMenuPolicy(Qt::CustomContextMenu);
  connect(ui_.treeView, &QWidget::customContextMenuRequested,
          this, &LeafletMapDialog::showContextMenu);
  connect(mapWidget_, &LeafletMap::mapRendered, this, &LeafletMapDialog::onMapLoadedAndRendered);
}

//------------------------------------------------------------------------
void
LeafletMapDialog::onMapLoadedAndRendered()
{

  model_->blockSignals(true);
  mapWidget_->setAllWaypointsVisibility(true);
  mapWidget_->setAllTracksVisibility(true);
  mapWidget_->setAllRoutesVisibility(true);
  model_->blockSignals(false);
  // Explicitly set check states after map visibility is handled
  wptItem_->setCheckState(Qt::Checked);
  trkItem_->setCheckState(Qt::Checked);
  rteItem_->setCheckState(Qt::Checked);
  ui_.treeView->update(wptItem_->index());
  ui_.treeView->update(trkItem_->index());
  ui_.treeView->update(rteItem_->index());
}

void LeafletMapDialog::routePointClicked(int i)
{
  QStandardItem* item = rteItem_->child(i);
  if (item) itemClickedX(item);
}

void LeafletMapDialog::trackClicked(int i)
{
  QStandardItem* item = trkItem_->child(i);
  if (item) itemClickedX(item);
}

//------------------------------------------------------------------------
void
LeafletMapDialog::trace(const QString& label, const QStandardItem* it)
{
  if constexpr(debug_) {
    QDebug qdb(QtDebugMsg);
    qdb.nospace().noquote() << label + ": ";
    qdb.quote();
    if (it == nullptr) {
      qdb << "null item";
    } else {
      QStandardItem* parent = it->parent();
      if (parent == nullptr) {
        qdb << "parent: none";
      } else {
        qdb << "parent: " << parent->text();
      }
      qdb << " item: " << it->text() << " (row: " << it->row() << ")";
    }
  }
}

//------------------------------------------------------------------------
void
LeafletMapDialog::showHideChild(const QStandardItem* child)
{

  const QStandardItem* top = child->parent();
  bool showTop = (top->checkState() == Qt::Checked || top->checkState() == Qt::PartiallyChecked);
  bool show = showTop && (child->checkState() == Qt::Checked);
  int originalIndex = child->data(OriginalIndexRole).toInt();
  if (top == wptItem_) {
    mapWidget_->setWaypointVisibility(originalIndex, show);
  } else if (top == trkItem_) {
    mapWidget_->setTrackVisibility(originalIndex, show);
  } else if (top == rteItem_) {
    mapWidget_->setRouteVisibility(originalIndex, show);
  }
}

//------------------------------------------------------------------------
void
LeafletMapDialog::showHideChildren(const QStandardItem* top)
{
  for (int row = 0; row < top->rowCount(); ++row) {
    const QStandardItem* child = top->child(row);
    showHideChild(child);
  }
}

void
LeafletMapDialog::itemChangedX(QStandardItem* it)
{
  if (itemChangedActive_) {
    return; // Prevent re-entrancy
  }
  itemChangedActive_ = true;

  if (it->isCheckable()) {
    Qt::CheckState newState = it->checkState();

    if (it == wptItem_ || it == trkItem_ || it == rteItem_) {
      // A parent item was clicked.
      if (newState == Qt::PartiallyChecked) {
        newState = Qt::Checked;
        it->setCheckState(newState); // This will re-enter, but be guarded.
      }
      // Propagate state to all children.
      for (int row = 0; row < it->rowCount(); ++row) {
        it->child(row)->setCheckState(newState);
      }
      // Update the map.
      if (it == wptItem_) mapWidget_->setAllWaypointsVisibility(newState != Qt::Unchecked);
      else if (it == trkItem_) mapWidget_->setAllTracksVisibility(newState != Qt::Unchecked);
      else if (it == rteItem_) mapWidget_->setAllRoutesVisibility(newState != Qt::Unchecked);

    } else {
      // A child item was clicked.
      QStandardItem* parent = it->parent();
      if (parent) {
        // Recalculate the parent's state based on its children.
        int checkedCount = 0;
        int childCount = parent->rowCount();
        for (int row = 0; row < childCount; ++row) {
          if (parent->child(row)->checkState() == Qt::Checked) {
            checkedCount++;
          }
        }

        if (checkedCount == 0) {
          parent->setCheckState(Qt::Unchecked);
        } else if (checkedCount == childCount) {
          parent->setCheckState(Qt::Checked);
        } else {
          parent->setCheckState(Qt::PartiallyChecked);
        }
        showHideChild(it); // Update this item's visibility on the map.
      }
    }
  }

  itemChangedActive_ = false; // Reset the guard.
}

//------------------------------------------------------------------------
void
LeafletMapDialog::treeDoubleClicked(const QModelIndex& idx)
{
  QStandardItem* it = model_->itemFromIndex(idx);
  trace("treeDoubleClicked", it);
  QStandardItem* parent = it->parent();
  int originalIndex = it->data(OriginalIndexRole).toInt();
  if (parent == wptItem_) {
    parent->setCheckState(Qt::Checked);
    it->setCheckState(Qt::Checked);
    mapWidget_->panTo(gpx_.getWaypoints().at(originalIndex).getLocation());
  } else if (parent == trkItem_) {
    parent->setCheckState(Qt::Checked);
    it->setCheckState(Qt::Checked);
    mapWidget_->frameTrack(originalIndex);
  } else if (parent == rteItem_) {
    parent->setCheckState(Qt::Checked);
    it->setCheckState(Qt::Checked);
    mapWidget_->frameRoute(originalIndex);
  }
}

//------------------------------------------------------------------------
void
LeafletMapDialog::itemClickedX(const QStandardItem* it)
{
  trace("itemXClicked", it);
  if (it != nullptr) {
    QModelIndex idx = model_->indexFromItem(it);
    ui_.treeView->scrollTo(idx, QAbstractItemView::PositionAtCenter);
    ui_.treeView->selectionModel()->select(idx, QItemSelectionModel::ClearAndSelect);
  }
}

//------------------------------------------------------------------------
void
LeafletMapDialog::selectionChangedX(const QItemSelection& sel, const QItemSelection& desel)
{

  for (const QModelIndexList idxs = desel.indexes(); const auto& idx : idxs) {
    const QStandardItem* it = model_->itemFromIndex(idx);
    const QStandardItem* parent = it->parent();
    if (parent == wptItem_) {
      int originalIndex = it->data(OriginalIndexRole).toInt();
      // mapWidget_->setWaypointColorBlue(originalIndex);
    }
  }
  for (const QModelIndexList idxs = sel.indexes(); const auto& idx : idxs) {
    const QStandardItem* it = model_->itemFromIndex(idx);
    const QStandardItem* parent = it->parent();
    if (parent == wptItem_) {
      int originalIndex = it->data(OriginalIndexRole).toInt();
      // mapWidget_->setWaypointColorRed(originalIndex);
    }
  }
}

//------------------------------------------------------------------------
void
LeafletMapDialog::expandCollapseAll(QStandardItem* top, bool exp)
{
  ui_.treeView->setExpanded(model_->indexFromItem(top), exp);
  for (int row = 0; row < top->rowCount(); ++row) {
    const QStandardItem* child = top->child(row);
    ui_.treeView->setExpanded(model_->indexFromItem(child), exp);
  }
}

//------------------------------------------------------------------------
void
LeafletMapDialog::showHideAll(QStandardItem* top, bool ck)
{
  trace("showHideAll", top);

  // Disconnect the signal to prevent re-entrancy during programmatic changes
  disconnect(model_, &QStandardItemModel::itemChanged, this, &LeafletMapDialog::itemChangedX);

  if (ck) {
    mapWidget_->resetBounds();
  }
  top->setCheckState(ck ? Qt::Checked: Qt::Unchecked);
  for (int row = 0; row < top->rowCount(); ++row) {
    QStandardItem* child = top->child(row);
    child->setCheckState(ck ? Qt::Checked: Qt::Unchecked);
  }

  // Reconnect the signal
  connect(model_, &QStandardItemModel::itemChanged, this, &LeafletMapDialog::itemChangedX);
}

//------------------------------------------------------------------------
void
LeafletMapDialog::showOnlyThis(QStandardItem* top, int menuRow)
{
  trace("showOnlyThis", top->child(menuRow));

  // Disconnect the signal to prevent re-entrancy during programmatic changes
  disconnect(model_, &QStandardItemModel::itemChanged, this, &LeafletMapDialog::itemChangedX);

  QStandardItem* child = top->child(menuRow);
  int originalIndex = child->data(OriginalIndexRole).toInt();

  if (top == wptItem_) {
    mapWidget_->panTo(gpx_.getWaypoints().at(originalIndex).getLocation());
  } else if (top == trkItem_) {
    mapWidget_->frameTrack(originalIndex);
  } else if (top == rteItem_) {
    mapWidget_->frameRoute(originalIndex);
  }

  for (int row = 0; row < top->rowCount(); ++row) {
    QStandardItem* child = top->child(row);
    child->setCheckState(row == menuRow ? Qt::Checked : Qt::Unchecked);
  }
  top->setCheckState(Qt::Checked);

  // Reconnect the signal
  connect(model_, &QStandardItemModel::itemChanged, this, &LeafletMapDialog::itemChangedX);
}

void
LeafletMapDialog::showTopContextMenu(const QStringList& text, QStandardItem* top, const QPoint& pt)
{
  QMenu menu(this);
  menu.addAction(text.at(0), this, [this, &top]()->void {
    showHideAll(top, true);
  });
  menu.addAction(text.at(1), this, [this, &top]()->void {
    showHideAll(top, false);
  });
  menu.addAction(text.at(2), this, [this, &top]()->void {
    expandCollapseAll(top, true);
  });
  menu.addAction(text.at(3), this, [this, &top]()->void {
    expandCollapseAll(top, false);
  });
  menu.exec(ui_.treeView->mapToGlobal(pt));
}

void
LeafletMapDialog::showChildContextMenu(const QString& text, const QStandardItem* child, const QPoint& pt)
{
  QStandardItem* parent = child->parent();
  int row = child->row();
  QMenu menu(this);
  menu.addAction(text, this, [this, &row, &parent]()->void {
    showOnlyThis(parent, row);
  });
  menu.exec(ui_.treeView->mapToGlobal(pt));
}

//------------------------------------------------------------------------
void
LeafletMapDialog::showContextMenu(const QPoint& pt)
{

  QModelIndex idx = ui_.treeView->indexAt(pt);
  if (idx.isValid()) {
    QStandardItem* it = model_->itemFromIndex(idx);
    if (it == wptItem_) {
      const QStringList labels = {tr("Show All Waypoints"),
                                  tr("Hide All Waypoints"),
                                  tr("Expand All"),
                                  tr("Collapse All")
                                 };
      showTopContextMenu(labels, it, pt);
    } else if (it == rteItem_) {
      const QStringList labels = {tr("Show All Routes"),
                                  tr("Hide All Routes"),
                                  tr("Expand All"),
                                  tr("Collapse All")
                                 };
      showTopContextMenu(labels, it, pt);
    } else if (it == trkItem_) {
      const QStringList labels = {tr("Show All Tracks"),
                                  tr("Hide All Tracks"),
                                  tr("Expand All"),
                                  tr("Collapse All")
                                 };
      showTopContextMenu(labels, it, pt);
    } else if (it != nullptr) {
      QStandardItem* parent = it->parent();
      if (parent == wptItem_) {
        showChildContextMenu(tr("Show Only This Waypoint"), it, pt);
      } else if (parent == trkItem_) {
        showChildContextMenu(tr("Show Only This Track"), it, pt);
      } else if (parent == rteItem_) {
        showChildContextMenu(tr("Show Only This Route"), it, pt);
      }
    }
  }
}
