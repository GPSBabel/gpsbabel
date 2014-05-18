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
//  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111
//  USA
//
//------------------------------------------------------------------------

#include <QStandardItemModel>
#include <QMenu>
#include <QHeaderView>
#include "gmapdlg.h"
#include "appname.h"
#include "gpx.h"

//------------------------------------------------------------------------
class StandardItem: public QStandardItem
{
 public:
  StandardItem(const QString &text): QStandardItem(text)
    {
      this->setEditable(false);
    }
};

//------------------------------------------------------------------------
class TreeAction: public QAction
{
public:
  TreeAction(const QString &text,
	     QObject *obj, const char *member,  QObject *parent): QAction(text, parent)
  {
    connect(this, SIGNAL(triggered()), obj, member);
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
void GMapDialog::appendWaypointInfo(QStandardItem *it, const GpxWaypoint &wpt)
{
  it->appendRow(new StandardItem(tr("Lat: %1").arg(wpt.getLocation().lat(), 0, 'f', 7)));
  it->appendRow(new StandardItem(tr("Lng: %1").arg(wpt.getLocation().lng(), 0, 'f', 7)));
  if (wpt.getDescription() != QString())
    it->appendRow(new StandardItem(tr("Desc: %1").arg(wpt.getDescription())));
  if (wpt.getComment() != QString() && wpt.getComment() != wpt.getDescription())
    it->appendRow(new StandardItem(tr("Cmt: %1").arg(wpt.getComment())));
  if (wpt.getElevation() > -50000)
    it->appendRow(new StandardItem(tr("Ele: %1").arg(wpt.getElevation())));

}

//------------------------------------------------------------------------
void GMapDialog::appendTrackInfo(QStandardItem *it, const GpxTrack &trk)
{
  QDateTime startTime, stopTime;
  bool first = true;
  int count = 0;
  foreach (const GpxTrackSegment &seg, trk.getTrackSegments()) {
    foreach (const GpxTrackPoint &pt, seg.getTrackPoints()) {
      count++;
      QDateTime t = pt.getDateTime();
      if (!t.isValid())
	continue;
      if (first) {
	startTime = t;
	stopTime = t;
	first = false;
      }
      else {
	if (t < startTime)
	  startTime = t;
	if (t > stopTime)
	  stopTime = t;
      }
    }
  }
  if (startTime.isValid()) {
    it->appendRow(new StandardItem(tr("Start: %1")
				    .arg(startTime.toString("yyyy-MMM-dd HH:mm:ss"))));
    it->appendRow(new StandardItem(tr("Stop: %1")
				    .arg(stopTime.toString("yyyy-MMM-dd HH:mm:ss"))));
  }
  it->appendRow(new StandardItem(tr("Points: %1").arg(count)));

  it->appendRow(new StandardItem(formatLength(trk.length())));

}

//------------------------------------------------------------------------
void GMapDialog::appendRouteInfo(QStandardItem *it, const GpxRoute &rte)
{
  it->appendRow(new StandardItem(formatLength(rte.length())));
}

//------------------------------------------------------------------------
GMapDialog::GMapDialog(QWidget *parent, const QString &gpxFileName, QPlainTextEdit *te): QDialog(parent)
{
  ui_.setupUi(this);
  this->setWindowTitle(QString(appName) + " " + QString("Google Maps"));
  gpx_.read(gpxFileName);

  mapWidget_ = new Map(this, gpx_, te);
  QHBoxLayout *lay = new QHBoxLayout(ui_.frame);
  lay->setContentsMargins(0, 0, 0, 0);
  lay->addWidget(mapWidget_);

  model_ = new QStandardItemModel(this);
  menuIndex_ = -1; // Actually set for real in showContextMenu().

  wptItem_ = new StandardItem(tr("Waypoints"));
  wptItem_->setCheckable(true);
  wptItem_->setCheckState(Qt::Checked);
  model_->appendRow(wptItem_);
  for (int i=0; i<gpx_.getWaypoints().size(); i++) {
    GpxWaypoint &wpt = gpx_.getWaypoints()[i];
    QStandardItem *it = new StandardItem(wpt.getName());
    wptItem_->appendRow(it);
    it->setCheckable(true);
    it->setCheckState(Qt::Checked);
    it->setData(qVariantFromValue((void *)&wpt));
    appendWaypointInfo(it, wpt);
    wptList_ << it;
  }

  trkItem_ = new StandardItem(tr("Tracks"));
  trkItem_->setCheckable(true);
  trkItem_->setCheckState(Qt::Checked);
  model_->appendRow(trkItem_);
  for (int i=0; i<gpx_.getTracks().size(); i++) {
    GpxTrack &trk = gpx_.getTracks()[i];
    QStandardItem *it = new StandardItem(trk.getName());
    trkItem_->appendRow(it);
    it->setCheckable(true);
    it->setCheckState(Qt::Checked);
    it->setData(qVariantFromValue((void *)&trk));
    appendTrackInfo(it, trk);
    trkList_ << it;
  }

  rteItem_ = new StandardItem(tr("Routes"));
  rteItem_->setCheckable(true);
  rteItem_->setCheckState(Qt::Checked);
  model_->appendRow(rteItem_);
  for (int i=0; i<gpx_.getRoutes().size(); i++) {
    GpxRoute &rte = gpx_.getRoutes()[i];
    QStandardItem *it = new StandardItem(rte.getName());
    rteItem_->appendRow(it);
    it->setCheckable(true);
    it->setCheckState(Qt::Checked);
    it->setData(qVariantFromValue((void *)&rte));
    appendRouteInfo(it, rte);
    rteList_ << it;
  }

  ui_.treeView->header()->hide();
  ui_.treeView->setModel(model_);
  ui_.treeView->setExpandsOnDoubleClick(false);
  connect(model_, SIGNAL(itemChanged(QStandardItem *)),
	  this,  SLOT(itemChangedX(QStandardItem *)));
  connect(mapWidget_, SIGNAL(waypointClicked(int)), this, SLOT(waypointClickedX(int)));
  connect(mapWidget_, SIGNAL(routeClicked(int)), this, SLOT(routeClickedX(int)));
  connect(mapWidget_, SIGNAL(trackClicked(int)), this, SLOT(trackClickedX(int)));
  connect(ui_.treeView, SIGNAL(doubleClicked(const QModelIndex &)),
	  this, SLOT(treeDoubleClicked(const QModelIndex&)));
  connect(ui_.treeView->selectionModel(), SIGNAL(selectionChanged (const QItemSelection &,  const QItemSelection &)),
	  this, SLOT(selectionChangedX(const QItemSelection &,  const QItemSelection &)));
						
  ui_.treeView->setContextMenuPolicy(Qt::CustomContextMenu);
  connect(ui_.treeView, SIGNAL(customContextMenuRequested(const QPoint &)),
	  this, SLOT(showContextMenu(const QPoint &)));

  connect(ui_.copyButton, SIGNAL(clicked()), this, SLOT(copyButtonClickedX()));

  ui_.copyButton->hide(); // Hide for now, not working
}

//-------------------------------------------------------------------------
void GMapDialog::itemChangedX(QStandardItem *it)
{
  bool show = (it->checkState() == Qt::Checked);
  if (it == trkItem_) {
    if (show)
      mapWidget_->showTracks(gpx_.getTracks());
    else
      mapWidget_->hideAllTracks();
  }

  else if (it == wptItem_) {
    if (show)
      mapWidget_->showWaypoints(gpx_.getWaypoints());
    else
      mapWidget_->hideAllWaypoints();
  }

  else if (it == rteItem_) {
    if (show)
      mapWidget_->showRoutes(gpx_.getRoutes());
    else
      mapWidget_->hideAllRoutes();
  }

  else {
    // Individual items, find the right one.
    GpxItem *git = static_cast<GpxItem *>(it->data().value<void *>());
    if (git != 0) {
      git->setVisible(show);
      for (int i=0; i<gpx_.getWaypoints().size(); i++) {
	if (&gpx_.getWaypoints()[i] == git) {
	  mapWidget_->setWaypointVisibility(i, show);
	}
      }
      for (int i=0; i<gpx_.getTracks().size(); i++) {
	if (&gpx_.getTracks()[i] == git) {
	  mapWidget_->setTrackVisibility(i, show);
	}
      }
      for (int i=0; i<gpx_.getRoutes().size(); i++) {
	if (&gpx_.getRoutes()[i] == git) {
	  mapWidget_->setRouteVisibility(i, show);
	}
      }
    }
  }
}

//-------------------------------------------------------------------------
int GMapDialog::waypointIndex(QStandardItem *it)
{
  for (int j=0; j<wptList_.size(); j++){
    if (it == wptList_[j])
      return j;
  }
  return -1;
}

//-------------------------------------------------------------------------
int GMapDialog::trackIndex(QStandardItem *it)
{
  for (int j=0; j<trkList_.size(); j++){
    if (it == trkList_[j])
      return j;
  }
  return -1;
}

//-------------------------------------------------------------------------
int GMapDialog::routeIndex(QStandardItem *it)
{
  for (int j=0; j<rteList_.size(); j++){
    if (it == rteList_[j])
      return j;
  }
  return -1;
}

//-------------------------------------------------------------------------
void GMapDialog::treeDoubleClicked(const QModelIndex &idx)
{
  QStandardItem *it = model_->itemFromIndex(idx);
  int i = waypointIndex(it);
  if (i >= 0) {
    it->setCheckState(Qt::Checked);
    gpx_.getWaypoints()[i].setVisible(true);
    mapWidget_->panTo(gpx_.getWaypoints()[i].getLocation());
    mapWidget_->setWaypointVisibility(i, true);
    return;
  }
  i = trackIndex(it);
  if (i >= 0) {
    mapWidget_->frameTrack(i);
    it->setCheckState(Qt::Checked);
    gpx_.getTracks()[i].setVisible(true);
    mapWidget_->setTrackVisibility(i, true);
    return;
  }
  i = routeIndex(it);
  if (i >= 0) {
    mapWidget_->frameRoute(i);
    it->setCheckState(Qt::Checked);
    gpx_.getRoutes()[i].setVisible(true);
    mapWidget_->setRouteVisibility(i, true);
    return;
  }
}

//-------------------------------------------------------------------------
void GMapDialog::waypointClickedX(int i)
{
  if (i>=0 && i < wptList_.size()) {
    QStandardItem *it = wptList_[i];
    QModelIndex idx = model_->indexFromItem(it);
    ui_.treeView->scrollTo(idx, QAbstractItemView::PositionAtCenter);
    ui_.treeView->selectionModel()->select(idx, QItemSelectionModel::ClearAndSelect);
  }
}
//-------------------------------------------------------------------------
void GMapDialog::trackClickedX(int i)
{
  if (i>=0 && i <trkList_.size()) {
    QStandardItem *it = trkList_[i];
    QModelIndex idx = model_->indexFromItem(it);
    ui_.treeView->scrollTo(idx, QAbstractItemView::PositionAtCenter);
    ui_.treeView->selectionModel()->select(idx, QItemSelectionModel::ClearAndSelect);
  }
}

//-------------------------------------------------------------------------
void GMapDialog::routeClickedX(int i)
{
  if (i>=0 && i <rteList_.size()) {
    QStandardItem *it = rteList_[i];
    QModelIndex idx = model_->indexFromItem(it);
    ui_.treeView->scrollTo(idx, QAbstractItemView::PositionAtCenter);
    ui_.treeView->selectionModel()->select(idx, QItemSelectionModel::ClearAndSelect);
  }
}

//-------------------------------------------------------------------------
void GMapDialog::selectionChangedX (const QItemSelection &sel,  const QItemSelection &desel)
{
  int k=0;
  foreach (QStandardItem*w, wptList_) {
    QModelIndex idx = model_->indexFromItem(w);
    if (desel.contains(idx))
      mapWidget_->setWaypointColorBlue(k);
    if (sel.contains(idx))
      mapWidget_->setWaypointColorRed(k);
    k++;
  }
}

//------------------------------------------------------------------------
void GMapDialog::expandCollapseAll(const QList<QStandardItem *> &li,
				   QStandardItem *top, bool exp)
{
  ui_.treeView->setExpanded(model_->indexFromItem(top), exp);
  foreach (QStandardItem*it, li) {
    QModelIndex idx = model_->indexFromItem(it);
    ui_.treeView->setExpanded(idx, exp);
  }
}

//------------------------------------------------------------------------
void GMapDialog::expandAllWaypoints()
{
  expandCollapseAll(wptList_, wptItem_, true);
}
//------------------------------------------------------------------------
void GMapDialog::expandAllTracks()
{
  expandCollapseAll(trkList_, trkItem_, true);
}
//------------------------------------------------------------------------
void GMapDialog::expandAllRoutes()
{
  expandCollapseAll(rteList_, rteItem_, true);
}

//------------------------------------------------------------------------
void GMapDialog::collapseAllWaypoints()
{
  expandCollapseAll(wptList_, wptItem_,false);
}
//------------------------------------------------------------------------
void GMapDialog::collapseAllTracks()
{
  expandCollapseAll(trkList_, trkItem_,false);
}
//------------------------------------------------------------------------
void GMapDialog::collapseAllRoutes()
{
  expandCollapseAll(rteList_, rteItem_,false);
}

//------------------------------------------------------------------------
void GMapDialog::checkUncheckAll(const QList<QStandardItem *> &li,
				   QStandardItem *top, bool ck)
{
  top->setCheckState(ck ? Qt::Checked: Qt::Unchecked);
  foreach (QStandardItem*it, li) {
    it->setCheckState(ck ? Qt::Checked: Qt::Unchecked);
  }
}
//------------------------------------------------------------------------
void GMapDialog::showAllWaypoints()
{
  foreach (GpxWaypoint wpt, gpx_.getWaypoints())
    wpt.setVisible(true);
  checkUncheckAll(wptList_, wptItem_, true);
  mapWidget_->showWaypoints(gpx_.getWaypoints());
}
//------------------------------------------------------------------------
void GMapDialog::showAllTracks()
{
  foreach (GpxTrack trk, gpx_.getTracks())
    trk.setVisible(true);
  checkUncheckAll(trkList_, trkItem_, true);
  mapWidget_->showTracks(gpx_.getTracks());
}

//------------------------------------------------------------------------
void GMapDialog::showAllRoutes()
{
  foreach (GpxRoute rte, gpx_.getRoutes())
    rte.setVisible(true);
  checkUncheckAll(rteList_, rteItem_, true);
  mapWidget_->showRoutes(gpx_.getRoutes());
}

//------------------------------------------------------------------------
void GMapDialog::hideAllWaypoints()
{
  foreach (GpxWaypoint wpt, gpx_.getWaypoints())
    wpt.setVisible(false);
  checkUncheckAll(wptList_, wptItem_, false);
  mapWidget_->showWaypoints(gpx_.getWaypoints());
}
//------------------------------------------------------------------------
void GMapDialog::hideAllTracks()
{
  foreach (GpxTrack trk, gpx_.getTracks())
    trk.setVisible(false);
  checkUncheckAll(trkList_, trkItem_, false);
  mapWidget_->showTracks(gpx_.getTracks());

}
//------------------------------------------------------------------------
void GMapDialog::hideAllRoutes()
{
  foreach (GpxRoute rte, gpx_.getRoutes())
    rte.setVisible(false);
  checkUncheckAll(rteList_, rteItem_, false);
  mapWidget_->showRoutes(gpx_.getRoutes());

}

//------------------------------------------------------------------------
void GMapDialog::showOnlyThisWaypoint()
{
  QList <GpxWaypoint> &tlist = gpx_.getWaypoints();
  for (int i=0; i<tlist.size(); i++) {
    tlist[i].setVisible(i == menuIndex_? true: false);
    trkList_[i]->setCheckState(i==menuIndex_? Qt::Checked: Qt::Unchecked);
  }
  wptItem_->setCheckState(Qt::Checked);
  mapWidget_->showWaypoints(gpx_.getWaypoints());
}
//------------------------------------------------------------------------
void GMapDialog::showOnlyThisTrack()
{
  QList <GpxTrack> &tlist = gpx_.getTracks();
  for (int i=0; i<tlist.size(); i++) {
    tlist[i].setVisible(i == menuIndex_? true: false);
    trkList_[i]->setCheckState(i==menuIndex_? Qt::Checked: Qt::Unchecked);
  }
  trkItem_->setCheckState(Qt::Checked);
  mapWidget_->showTracks(gpx_.getTracks());

}
//------------------------------------------------------------------------
void GMapDialog::showOnlyThisRoute()
{
  QList <GpxRoute> &rlist = gpx_.getRoutes();
  for (int i=0; i<rlist.size(); i++) {
    rlist[i].setVisible(i == menuIndex_? true: false);
    rteList_[i]->setCheckState(i==menuIndex_? Qt::Checked: Qt::Unchecked);
  }
  rteItem_->setCheckState(Qt::Checked);
  mapWidget_->showRoutes(gpx_.getRoutes());

}

//------------------------------------------------------------------------
void GMapDialog::showContextMenu(const QPoint &pt)
{
  QModelIndex idx = ui_.treeView->indexAt(pt);
  QStandardItem *it = model_->itemFromIndex(idx);
  int j;
  if (model_->indexFromItem(wptItem_) == idx) {
    QMenu menu(this);
    menu.addAction(new TreeAction(tr("Show All Waypoints"), this, SLOT(showAllWaypoints()), &menu));
    menu.addAction(new TreeAction(tr("Hide All Waypoints"), this, SLOT(hideAllWaypoints()),&menu));
    menu.addAction(new TreeAction(tr("Expand All"), this, SLOT(expandAllWaypoints()),&menu));
    menu.addAction(new TreeAction(tr("Collapse All"), this, SLOT(collapseAllWaypoints()),&menu));
    menu.exec(ui_.treeView->mapToGlobal(pt));
  }
  else if (model_->indexFromItem(rteItem_) == idx) {
    QMenu menu(this);
    menu.addAction(new TreeAction(tr("Show All Routes"), this, SLOT(showAllRoutes()), &menu));
    menu.addAction(new TreeAction(tr("Hide All Routes"), this, SLOT(hideAllRoutes()),&menu));
    menu.addAction(new TreeAction(tr("Expand All"), this, SLOT(expandAllRoutes()),&menu));
    menu.addAction(new TreeAction(tr("Collapse All"), this, SLOT(collapseAllRoutes()),&menu));
    menu.exec(ui_.treeView->mapToGlobal(pt));
  }
  else if (model_->indexFromItem(trkItem_) == idx) {
    QMenu menu(this);
    menu.addAction(new TreeAction(tr("Show All Tracks"), this, SLOT(showAllTracks()), &menu));
    menu.addAction(new TreeAction(tr("Hide All Tracks"), this, SLOT(hideAllTracks()),&menu));
    menu.addAction(new TreeAction(tr("Expand All"), this, SLOT(expandAllTracks()),&menu));
    menu.addAction(new TreeAction(tr("Collapse All"), this, SLOT(collapseAllTracks()),&menu));
    menu.exec(ui_.treeView->mapToGlobal(pt));
  }
  else if ((j = waypointIndex(it)) >=0) {
    QMenu menu(this);
    menu.addAction(new TreeAction(tr("Show Only This Waypoint"), this, SLOT(showOnlyThisWaypoint()), &menu));
    menuIndex_ = j;
    menu.exec(ui_.treeView->mapToGlobal(pt));
  }
  else if ((j = trackIndex(it)) >=0) {
    QMenu menu(this);
    menu.addAction(new TreeAction(tr("Show Only This Track"), this, SLOT(showOnlyThisTrack()), &menu));
    menuIndex_ = j;
    menu.exec(ui_.treeView->mapToGlobal(pt));
  }
  else if ((j = routeIndex(it)) >=0) {
    QMenu menu(this);
    menu.addAction(new TreeAction(tr("Show Only This Route"), this, SLOT(showOnlyThisRoute()), &menu));
    menuIndex_ = j;
    menu.exec(ui_.treeView->mapToGlobal(pt));
  }
  else {
  }
}
//------------------------------------------------------------------------
void GMapDialog::copyButtonClickedX() {
  
}
