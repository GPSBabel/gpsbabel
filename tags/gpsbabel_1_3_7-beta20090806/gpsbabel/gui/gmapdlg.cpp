// -*- C++ -*-
// $Id: gmapdlg.cpp,v 1.1 2009-07-05 21:14:56 robertl Exp $
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
  ui.setupUi(this);
  this->setWindowTitle(QString(appName) + " " + QString("Google Maps"));
  gpx.read(gpxFileName);

  mapWidget = new Map(this, gpx, te);
  QHBoxLayout *lay = new QHBoxLayout(ui.frame);
  lay->setContentsMargins(0, 0, 0, 0);
  lay->addWidget(mapWidget);

  model = new QStandardItemModel(this);
  
  wptItem = new StandardItem(tr("Waypoints"));
  wptItem->setCheckable(true);
  wptItem->setCheckState(Qt::Checked);
  model->appendRow(wptItem);
  for (int i=0; i<gpx.getWaypoints().size(); i++) {
    GpxWaypoint &wpt = gpx.getWaypoints()[i];
    QStandardItem *it = new StandardItem(wpt.getName());
    wptItem->appendRow(it);
    it->setCheckable(true);
    it->setCheckState(Qt::Checked);
    it->setData(qVariantFromValue((void *)&wpt));
    appendWaypointInfo(it, wpt);
    wptList << it;
  }

  trkItem = new StandardItem(tr("Tracks"));
  trkItem->setCheckable(true);
  trkItem->setCheckState(Qt::Checked);
  model->appendRow(trkItem);
  for (int i=0; i<gpx.getTracks().size(); i++) {
    GpxTrack &trk = gpx.getTracks()[i];
    QStandardItem *it = new StandardItem(trk.getName());
    trkItem->appendRow(it);
    it->setCheckable(true);
    it->setCheckState(Qt::Checked);
    it->setData(qVariantFromValue((void *)&trk));
    appendTrackInfo(it, trk);
    trkList << it;
  }

  rteItem = new StandardItem(tr("Routes"));
  rteItem->setCheckable(true);
  rteItem->setCheckState(Qt::Checked);
  model->appendRow(rteItem);
  for (int i=0; i<gpx.getRoutes().size(); i++) {
    GpxRoute &rte = gpx.getRoutes()[i];
    QStandardItem *it = new StandardItem(rte.getName());
    rteItem->appendRow(it);
    it->setCheckable(true);
    it->setCheckState(Qt::Checked);
    it->setData(qVariantFromValue((void *)&rte));
    appendRouteInfo(it, rte);
    rteList << it;
  }

  ui.treeView->header()->hide();
  ui.treeView->setModel(model);
  ui.treeView->setExpandsOnDoubleClick(false);
  connect(model, SIGNAL(itemChanged(QStandardItem *)),
	  this,  SLOT(itemChangedX(QStandardItem *)));
  connect(mapWidget, SIGNAL(waypointClicked(int)), this, SLOT(waypointClickedX(int)));
  connect(mapWidget, SIGNAL(routeClicked(int)), this, SLOT(routeClickedX(int)));
  connect(mapWidget, SIGNAL(trackClicked(int)), this, SLOT(trackClickedX(int)));
  connect(ui.treeView, SIGNAL(doubleClicked(const QModelIndex &)), 
	  this, SLOT(treeDoubleClicked(const QModelIndex&)));
  connect(ui.treeView->selectionModel(), SIGNAL(selectionChanged (const QItemSelection &,  const QItemSelection &)),
	  this, SLOT(selectionChangedX(const QItemSelection &,  const QItemSelection &)));
						
  ui.treeView->setContextMenuPolicy(Qt::CustomContextMenu);
  connect(ui.treeView, SIGNAL(customContextMenuRequested(const QPoint &)),
	  this, SLOT(showContextMenu(const QPoint &)));


}

//-------------------------------------------------------------------------
void GMapDialog::itemChangedX(QStandardItem *it) 
{
  bool show = (it->checkState() == Qt::Checked);
  if (it == trkItem) {
    if (show) 
      mapWidget->showTracks(gpx.getTracks());
    else 
      mapWidget->hideAllTracks();
  }

  else if (it == wptItem) {
    if (show)
      mapWidget->showWaypoints(gpx.getWaypoints());
    else 
      mapWidget->hideAllWaypoints();
  }

  else if (it == rteItem) {
    if (show)
      mapWidget->showRoutes(gpx.getRoutes());
    else 
      mapWidget->hideAllRoutes();
  }

  else {
    // Individual items, find the right one.
    GpxItem *git = static_cast<GpxItem *>(it->data().value<void *>());
    if (git != 0) {
      git->setVisible(show);
      for (int i=0; i<gpx.getWaypoints().size(); i++) {
	if (&gpx.getWaypoints()[i] == git) {
	  mapWidget->setWaypointVisibility(i, show);
	}
      }
      for (int i=0; i<gpx.getTracks().size(); i++) {
	if (&gpx.getTracks()[i] == git) {
	  mapWidget->setTrackVisibility(i, show);
	}
      }
      for (int i=0; i<gpx.getRoutes().size(); i++) {
	if (&gpx.getRoutes()[i] == git) {
	  mapWidget->setRouteVisibility(i, show);
	}
      }
    }
  }
}  

//-------------------------------------------------------------------------
int GMapDialog::waypointIndex(QStandardItem *it)
{
  for (int j=0; j<wptList.size(); j++){
    if (it == wptList[j]) 
      return j;
  }
  return -1;
}

//-------------------------------------------------------------------------
int GMapDialog::trackIndex(QStandardItem *it)
{
  for (int j=0; j<trkList.size(); j++){
    if (it == trkList[j]) 
      return j;
  }
  return -1;
}

//-------------------------------------------------------------------------
int GMapDialog::routeIndex(QStandardItem *it)
{
  for (int j=0; j<rteList.size(); j++){
    if (it == rteList[j]) 
      return j;
  }
  return -1;
}

//-------------------------------------------------------------------------
void GMapDialog::treeDoubleClicked(const QModelIndex &idx) 
{
  QStandardItem *it = model->itemFromIndex(idx);
  int i = waypointIndex(it);
  if (i >= 0) {
    it->setCheckState(Qt::Checked);
    gpx.getWaypoints()[i].setVisible(true);
    mapWidget->panTo(gpx.getWaypoints()[i].getLocation());
    mapWidget->setWaypointVisibility(i, true);
    return;
  }
  i = trackIndex(it);
  if (i >= 0) {
    mapWidget->frameTrack(i);
    it->setCheckState(Qt::Checked);
    gpx.getTracks()[i].setVisible(true);
    mapWidget->setTrackVisibility(i, true);
    return;
  }
  i = routeIndex(it);
  if (i >= 0) {
    mapWidget->frameRoute(i);
    it->setCheckState(Qt::Checked);
    gpx.getRoutes()[i].setVisible(true);
    mapWidget->setRouteVisibility(i, true);
    return;
  }
}

//-------------------------------------------------------------------------
void GMapDialog::waypointClickedX(int i) 
{
  if (i>=0 && i < wptList.size()) {
    QStandardItem *it = wptList[i];
    QModelIndex idx = model->indexFromItem(it);
    ui.treeView->scrollTo(idx, QAbstractItemView::PositionAtCenter);
    ui.treeView->selectionModel()->select(idx, QItemSelectionModel::ClearAndSelect);
  }
}
//-------------------------------------------------------------------------
void GMapDialog::trackClickedX(int i) 
{
  if (i>=0 && i <trkList.size()) {
    QStandardItem *it = trkList[i];
    QModelIndex idx = model->indexFromItem(it);
    ui.treeView->scrollTo(idx, QAbstractItemView::PositionAtCenter);
    ui.treeView->selectionModel()->select(idx, QItemSelectionModel::ClearAndSelect);
  }
}

//-------------------------------------------------------------------------
void GMapDialog::routeClickedX(int i) 
{
  if (i>=0 && i <rteList.size()) {
    QStandardItem *it = rteList[i];
    QModelIndex idx = model->indexFromItem(it);
    ui.treeView->scrollTo(idx, QAbstractItemView::PositionAtCenter);
    ui.treeView->selectionModel()->select(idx, QItemSelectionModel::ClearAndSelect);
  }
}

//-------------------------------------------------------------------------
void GMapDialog::selectionChangedX (const QItemSelection &sel,  const QItemSelection &desel)
{
  int k=0;
  foreach (QStandardItem*w, wptList) {
    QModelIndex idx = model->indexFromItem(w);
    if (desel.contains(idx)) 
      mapWidget->setWaypointColorBlue(k);
    if (sel.contains(idx)) 
      mapWidget->setWaypointColorRed(k);
    k++;
  }
}

//------------------------------------------------------------------------
void GMapDialog::expandCollapseAll(const QList<QStandardItem *> &li, 
				   QStandardItem *top, bool exp)
{
  ui.treeView->setExpanded(model->indexFromItem(top), exp);
  foreach (QStandardItem*it, li) {
    QModelIndex idx = model->indexFromItem(it);
    ui.treeView->setExpanded(idx, exp);
  }
}

//------------------------------------------------------------------------
void GMapDialog::expandAllWaypoints()
{
  expandCollapseAll(wptList, wptItem, true);
}
//------------------------------------------------------------------------
void GMapDialog::expandAllTracks()
{
  expandCollapseAll(trkList, trkItem, true);
}
//------------------------------------------------------------------------
void GMapDialog::expandAllRoutes()
{
  expandCollapseAll(rteList, rteItem, true);
}

//------------------------------------------------------------------------
void GMapDialog::collapseAllWaypoints()
{
  expandCollapseAll(wptList, wptItem,false);
}
//------------------------------------------------------------------------
void GMapDialog::collapseAllTracks()
{
  expandCollapseAll(trkList, trkItem,false);
}
//------------------------------------------------------------------------
void GMapDialog::collapseAllRoutes()
{
  expandCollapseAll(rteList, rteItem,false);
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
  foreach (GpxWaypoint wpt, gpx.getWaypoints()) 
    wpt.setVisible(true);
  checkUncheckAll(wptList, wptItem, true);
  mapWidget->showWaypoints(gpx.getWaypoints());
}
//------------------------------------------------------------------------
void GMapDialog::showAllTracks()
{
  foreach (GpxTrack trk, gpx.getTracks()) 
    trk.setVisible(true);
  checkUncheckAll(trkList, trkItem, true);
  mapWidget->showTracks(gpx.getTracks());
}

//------------------------------------------------------------------------
void GMapDialog::showAllRoutes()
{
  foreach (GpxRoute rte, gpx.getRoutes()) 
    rte.setVisible(true);
  checkUncheckAll(rteList, rteItem, true);
  mapWidget->showRoutes(gpx.getRoutes());
}

//------------------------------------------------------------------------
void GMapDialog::hideAllWaypoints()
{
  foreach (GpxWaypoint wpt, gpx.getWaypoints()) 
    wpt.setVisible(false);
  checkUncheckAll(wptList, wptItem, false);
  mapWidget->showWaypoints(gpx.getWaypoints());
}
//------------------------------------------------------------------------
void GMapDialog::hideAllTracks()
{
  foreach (GpxTrack trk, gpx.getTracks()) 
    trk.setVisible(false);
  checkUncheckAll(trkList, trkItem, false);
  mapWidget->showTracks(gpx.getTracks());

}
//------------------------------------------------------------------------
void GMapDialog::hideAllRoutes()
{
  foreach (GpxRoute rte, gpx.getRoutes()) 
    rte.setVisible(false);
  checkUncheckAll(rteList, rteItem, false);
  mapWidget->showRoutes(gpx.getRoutes());

}

//------------------------------------------------------------------------
void GMapDialog::showOnlyThisWaypoint()
{
  QList <GpxWaypoint> &tlist = gpx.getWaypoints();
  for (int i=0; i<tlist.size(); i++) {
    tlist[i].setVisible(i == menuIndex? true: false);
    trkList[i]->setCheckState(i==menuIndex? Qt::Checked: Qt::Unchecked);
  }
  wptItem->setCheckState(Qt::Checked);
  mapWidget->showWaypoints(gpx.getWaypoints());
}
//------------------------------------------------------------------------
void GMapDialog::showOnlyThisTrack()
{
  QList <GpxTrack> &tlist = gpx.getTracks();
  for (int i=0; i<tlist.size(); i++) {
    tlist[i].setVisible(i == menuIndex? true: false);
    trkList[i]->setCheckState(i==menuIndex? Qt::Checked: Qt::Unchecked);
  }
  trkItem->setCheckState(Qt::Checked);
  mapWidget->showTracks(gpx.getTracks());

}
//------------------------------------------------------------------------
void GMapDialog::showOnlyThisRoute()
{
  QList <GpxRoute> &rlist = gpx.getRoutes();
  for (int i=0; i<rlist.size(); i++) {
    rlist[i].setVisible(i == menuIndex? true: false);
    rteList[i]->setCheckState(i==menuIndex? Qt::Checked: Qt::Unchecked);
  }
  rteItem->setCheckState(Qt::Checked);
  mapWidget->showRoutes(gpx.getRoutes());

}

//------------------------------------------------------------------------
void GMapDialog::showContextMenu(const QPoint &pt) 
{
  QModelIndex idx = ui.treeView->indexAt(pt);
  QStandardItem *it = model->itemFromIndex(idx);
  int j;
  if (model->indexFromItem(wptItem) == idx) {
    QMenu menu(this);
    menu.addAction(new TreeAction(tr("Show All Waypoints"), this, SLOT(showAllWaypoints()), &menu));
    menu.addAction(new TreeAction(tr("Hide All Waypoints"), this, SLOT(hideAllWaypoints()),&menu));
    menu.addAction(new TreeAction(tr("Expand All"), this, SLOT(expandAllWaypoints()),&menu));
    menu.addAction(new TreeAction(tr("Collapse All"), this, SLOT(collapseAllWaypoints()),&menu));
    menu.exec(ui.treeView->mapToGlobal(pt));
  }
  else if (model->indexFromItem(rteItem) == idx) {
    QMenu menu(this);
    menu.addAction(new TreeAction(tr("Show All Routes"), this, SLOT(showAllRoutes()), &menu));
    menu.addAction(new TreeAction(tr("Hide All Routes"), this, SLOT(hideAllRoutes()),&menu));
    menu.addAction(new TreeAction(tr("Expand All"), this, SLOT(expandAllRoutes()),&menu));
    menu.addAction(new TreeAction(tr("Collapse All"), this, SLOT(collapseAllRoutes()),&menu));
    menu.exec(ui.treeView->mapToGlobal(pt));
  }
  else if (model->indexFromItem(trkItem) == idx) {
    QMenu menu(this);
    menu.addAction(new TreeAction(tr("Show All Tracks"), this, SLOT(showAllTracks()), &menu));
    menu.addAction(new TreeAction(tr("Hide All Tracks"), this, SLOT(hideAllTracks()),&menu));
    menu.addAction(new TreeAction(tr("Expand All"), this, SLOT(expandAllTracks()),&menu));
    menu.addAction(new TreeAction(tr("Collapse All"), this, SLOT(collapseAllTracks()),&menu));
    menu.exec(ui.treeView->mapToGlobal(pt));
  }
  else if ((j = waypointIndex(it)) >=0) {
    QMenu menu(this);
    menu.addAction(new TreeAction(tr("Show Only This Waypoint"), this, SLOT(showOnlyThisWaypoint()), &menu));
    menuIndex = j;
    menu.exec(ui.treeView->mapToGlobal(pt));
  }
  else if ((j = trackIndex(it)) >=0) {
    QMenu menu(this);
    menu.addAction(new TreeAction(tr("Show Only This Track"), this, SLOT(showOnlyThisTrack()), &menu));
    menuIndex = j;
    menu.exec(ui.treeView->mapToGlobal(pt));
  }
  else if ((j = routeIndex(it)) >=0) {
    QMenu menu(this);
    menu.addAction(new TreeAction(tr("Show Only This Route"), this, SLOT(showOnlyThisRoute()), &menu));
    menuIndex = j;
    menu.exec(ui.treeView->mapToGlobal(pt));
  }
  else {
  }
}

