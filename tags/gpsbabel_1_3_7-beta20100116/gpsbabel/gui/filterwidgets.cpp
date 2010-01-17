// -*- C++ -*-
// $Id: filterwidgets.cpp,v 1.5 2009-11-02 20:38:02 robertl Exp $
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

#include "filterwidgets.h"


//------------------------------------------------------------------------
TrackWidget::TrackWidget(QWidget *parent, TrackFilterData &tfd): FilterWidget(parent) , tfd(tfd)
{
  ui.setupUi(this);

  // Checkbox interlocks
  addCheckEnabler(ui.titleCheck, ui.titleText);
  addCheckEnabler(ui.moveCheck,
		  (QList<QWidget *> ()
		   << ui.daysLabel << ui.daysSpin
		   << ui.hoursLabel<< ui.hoursSpin
		   << ui.minsLabel << ui.minsSpin
		   << ui.secsLabel << ui.secsSpin));
  addCheckEnabler(ui.startCheck,    ui.startEdit);
  addCheckEnabler(ui.stopCheck,     ui.stopEdit);
  addCheckEnabler(ui.GPSFixesCheck, ui.GPSFixesCombo);

  addCheckEnabler(ui.splitTimeCheck, 
		  (QList<QWidget *> ()
		   <<ui.splitTimeSpin
		   <<ui.splitTimeCombo));
  addCheckEnabler(ui.splitDistanceCheck, 
		  (QList<QWidget *> ()
		   <<ui.splitDistSpin
		   <<ui.splitDistCombo));

  connect(ui.mergeCheck, SIGNAL(clicked()) , this, SLOT(mergeCheckX()));
  connect(ui.packCheck,  SIGNAL(clicked()),  this, SLOT(packCheckX()));
  connect(ui.startCheck, SIGNAL(clicked()),  this, SLOT(otherCheckX()));
  connect(ui.stopCheck,   SIGNAL(clicked()), this, SLOT(otherCheckX()));

  connect(ui.splitDateCheck,   SIGNAL(clicked()), this, SLOT(splitDateX()));
  connect(ui.splitTimeCheck,   SIGNAL(clicked()), this, SLOT(splitTimeX()));
  connect(ui.splitDistanceCheck,   SIGNAL(clicked()), this, SLOT(splitDistanceX()));

  ui.startEdit->setDisplayFormat("dd MMM yyyy hh:mm:ss AP");
  ui.stopEdit->setDisplayFormat("dd MMM yyyy hh:mm:ss AP");

  // Collect the data fields.
  fopts << new BoolFilterOption(tfd.title,  ui.titleCheck);
  fopts << new BoolFilterOption(tfd.move,   ui.moveCheck);
  fopts << new BoolFilterOption(tfd.TZ,     ui.TZCheck);
  fopts << new BoolFilterOption(tfd.start,  ui.startCheck);
  fopts << new BoolFilterOption(tfd.stop,   ui.stopCheck);
  fopts << new BoolFilterOption(tfd.pack,   ui.packCheck);
  fopts << new BoolFilterOption(tfd.merge,  ui.mergeCheck);
  fopts << new BoolFilterOption(tfd.splitByDate,  ui.splitDateCheck);
  fopts << new BoolFilterOption(tfd.splitByTime,  ui.splitTimeCheck);
  fopts << new BoolFilterOption(tfd.splitByDistance,  ui.splitDistanceCheck);
  fopts << new BoolFilterOption(tfd.GPSFixes,  ui.GPSFixesCheck);
  fopts << new BoolFilterOption(tfd.course, ui.courseCheck);
  fopts << new BoolFilterOption(tfd.speed,  ui.speedCheck);

  fopts << new IntSpinFilterOption(tfd.days,  ui.daysSpin, -2000, 2000);
  fopts << new IntSpinFilterOption(tfd.hours, ui.hoursSpin);
  fopts << new IntSpinFilterOption(tfd.mins,  ui.minsSpin);
  fopts << new IntSpinFilterOption(tfd.secs,  ui.secsSpin);
  fopts << new IntSpinFilterOption(tfd.splitTime,  ui.splitTimeSpin, 0, 1000);
  fopts << new IntSpinFilterOption(tfd.splitDist,  ui.splitDistSpin, 0, 5280);

  fopts << new DateTimeFilterOption(tfd.startTime, ui.startEdit);
  fopts << new DateTimeFilterOption(tfd.stopTime,  ui.stopEdit);

  fopts << new StringFilterOption(tfd.titleString, ui.titleText);
  fopts << new ComboFilterOption(tfd.GPSFixesVal,  ui.GPSFixesCombo);
  fopts << new ComboFilterOption(tfd.splitTimeUnit,  ui.splitTimeCombo);
  fopts << new ComboFilterOption(tfd.splitDistUnit,  ui.splitDistCombo);
  setWidgetValues();
  checkChecks();
}

//------------------------------------------------------------------------
void TrackWidget::otherCheckX()
{
  ui.TZCheck->setEnabled(ui.stopCheck->isChecked() || ui.startCheck->isChecked());

  ui.splitTimeSpin->setEnabled(ui.splitTimeCheck->isChecked());
  ui.splitTimeCombo->setEnabled(ui.splitTimeCheck->isChecked());
  ui.splitDistSpin->setEnabled(ui.splitDistanceCheck->isChecked());
  ui.splitDistCombo->setEnabled(ui.splitDistanceCheck->isChecked());
  
  bool bb = (ui.mergeCheck->isChecked() || ui.packCheck->isChecked());
  ui.splitDateCheck->setEnabled(bb);
  ui.splitTimeCheck->setEnabled(bb);
  ui.splitDistanceCheck->setEnabled(bb);
}

//------------------------------------------------------------------------
void TrackWidget::mergeCheckX()
{
  if (ui.mergeCheck->isChecked())
    ui.packCheck->setChecked(false);
  otherCheckX();
}
//------------------------------------------------------------------------
void TrackWidget::packCheckX()
{
  if (ui.packCheck->isChecked())
    ui.mergeCheck->setChecked(false);
  otherCheckX();
}

//------------------------------------------------------------------------
void TrackWidget::splitDateX()
{
  if (ui.splitDateCheck->isChecked()) {
    ui.splitTimeCheck->setChecked(false);
    ui.splitDistanceCheck->setChecked(false);
  }
  otherCheckX();
}
//------------------------------------------------------------------------
void TrackWidget::splitTimeX()
{
  if (ui.splitTimeCheck->isChecked()) {
    ui.splitDateCheck->setChecked(false);
    ui.splitDistanceCheck->setChecked(false);
  }
  otherCheckX();
}
//------------------------------------------------------------------------
void TrackWidget::splitDistanceX()
{
  if (ui.splitDistanceCheck->isChecked()) {
    ui.splitDateCheck->setChecked(false);
    ui.splitTimeCheck->setChecked(false);
  }
  otherCheckX();
}


//------------------------------------------------------------------------
//------------------------------------------------------------------------
WayPtsWidget::WayPtsWidget(QWidget *parent, WayPtsFilterData &wfd): FilterWidget(parent) , wfd(wfd)
{
  ui.setupUi(this);
  addCheckEnabler(ui.duplicatesCheck,
		  QList<QWidget*>() << ui.shortNamesCheck << ui.locationsCheck);
  addCheckEnabler(ui.positionCheck, 
		  QList<QWidget*>() << ui.positionText << ui.positionUnitCombo);
  addCheckEnabler(ui.radiusCheck,
		  QList<QWidget*>() << ui.latLabel << ui.latText << ui.longLabel <<
		  ui.longText << ui.radiusUnitCombo << ui.radiusText);

  fopts << new BoolFilterOption(wfd.duplicates, ui.duplicatesCheck);
  fopts << new BoolFilterOption(wfd.shortNames, ui.shortNamesCheck);
  fopts << new BoolFilterOption(wfd.locations, ui.locationsCheck);
  fopts << new BoolFilterOption(wfd.position, ui.positionCheck);
  fopts << new BoolFilterOption(wfd.radius, ui.radiusCheck);
  fopts << new BoolFilterOption(wfd.sort, ui.sortCheck);
  fopts << new DoubleFilterOption(wfd.positionVal, ui.positionText, 0.0, 1.0E308);
  fopts << new DoubleFilterOption(wfd.radiusVal, ui.radiusText, 0.0, 1.0E308);
  fopts << new DoubleFilterOption(wfd.longVal, ui.longText, -180, 180, 7, 'f');
  fopts << new DoubleFilterOption(wfd.latVal, ui.latText,  -90, 90, 7, 'f');
  fopts << new ComboFilterOption(wfd.positionUnit, ui.positionUnitCombo);
  fopts << new ComboFilterOption(wfd.radiusUnit, ui.radiusUnitCombo);

  connect(ui.shortNamesCheck, SIGNAL(clicked()), this, SLOT(shortNamesCkX()));
  connect(ui.locationsCheck, SIGNAL(clicked()), this, SLOT(locationsCkX()));
  setWidgetValues();
  checkChecks();
}
//------------------------------------------------------------------------
void WayPtsWidget::shortNamesCkX()
{
  if (!ui.shortNamesCheck->isChecked())
    ui.locationsCheck->setChecked(true);
}
//------------------------------------------------------------------------
void WayPtsWidget::locationsCkX()
{
  if (!ui.locationsCheck->isChecked())
    ui.shortNamesCheck->setChecked(true);
}

//------------------------------------------------------------------------
//------------------------------------------------------------------------
RtTrkWidget::RtTrkWidget(QWidget *parent, RtTrkFilterData &rfd): FilterWidget(parent) , rfd(rfd)
{
  ui.setupUi(this);
  addCheckEnabler(ui.simplifyCheck,
		  QList<QWidget*>() << ui.limitToLabel << ui.limitToSpin << ui.pointLabel);

  fopts << new BoolFilterOption(rfd.simplify, ui.simplifyCheck);
  fopts << new BoolFilterOption(rfd.reverse, ui.reverseCheck);
  fopts << new IntSpinFilterOption(rfd.limitTo, ui.limitToSpin, 1, 5000);
  setWidgetValues();
  checkChecks();
}

//------------------------------------------------------------------------
//------------------------------------------------------------------------
MiscFltWidget::MiscFltWidget(QWidget *parent, MiscFltFilterData &mfd): FilterWidget(parent) , mfd(mfd)
{
  ui.setupUi(this);
  ui.transformCombo->addItem(QString("%1 %2 %3").arg(tr("Tracks")).arg(QChar(8594)).arg(tr("Waypoints")));
  ui.transformCombo->addItem(QString("%1 %2 %3").arg(tr("Routes")).arg(QChar(8594)).arg(tr("Tracks")));
  ui.transformCombo->addItem(QString("%1 %2 %3").arg(tr("Waypoints")).arg(QChar(8594)).arg(tr("Routes")));
  ui.transformCombo->addItem(QString("%1 %2 %3").arg(tr("Routes")).arg(QChar(8594)).arg(tr("Waypoints")));
  ui.transformCombo->addItem(QString("%1 %2 %3").arg(tr("Tracks")).arg(QChar(8594)).arg(tr("Routes")));
  ui.transformCombo->addItem(QString("%1 %2 %3").arg(tr("Waypoints")).arg(QChar(8594)).arg(tr("Tracks")));
  addCheckEnabler(ui.transformCheck,
		  QList<QWidget*>() << ui.transformCombo << ui.deleteCheck);

  fopts << new BoolFilterOption(mfd.transform, ui.transformCheck);
  fopts << new BoolFilterOption(mfd.swap, ui.swapCheck);
  fopts << new BoolFilterOption(mfd.del, ui.deleteCheck);
  fopts << new BoolFilterOption(mfd.nukeTracks, ui.nukeTracks);
  fopts << new BoolFilterOption(mfd.nukeRoutes, ui.nukeRoutes);
  fopts << new BoolFilterOption(mfd.nukeWaypoints, ui.nukeWaypoints);
  fopts << new ComboFilterOption(mfd.transformVal,  ui.transformCombo);

  setWidgetValues();
  checkChecks();
}
