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
//  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301,
//  USA.
//

#include "filterwidgets.h"

#include <QtCore/QChar>            // for QChar
#include <QtWidgets/QCheckBox>     // for QCheckBox
#include <QtWidgets/QLabel>        // for QLabel
#include <QtWidgets/QRadioButton>  // for QRadioButton


//------------------------------------------------------------------------
TrackWidget::TrackWidget(QWidget* parent, TrackFilterData& tfd): FilterWidget(parent), tfd(tfd)
{
  ui.setupUi(this);

  // Checkbox interlocks
  addCheckEnabler(ui.titleCheck, ui.titleText);
  addCheckEnabler(ui.moveCheck,
                  (QList<QWidget*> ()
                   << ui.weeksLabel << ui.weeksSpin
                   << ui.daysLabel << ui.daysSpin
                   << ui.hoursLabel<< ui.hoursSpin
                   << ui.minsLabel << ui.minsSpin
                   << ui.secsLabel << ui.secsSpin));
  addCheckEnabler(ui.startCheck,    ui.startEdit);
  addCheckEnabler(ui.stopCheck,     ui.stopEdit);
  addCheckEnabler(ui.GPSFixesCheck, ui.GPSFixesCombo);

  addCheckEnabler(ui.splitTimeCheck,
                  (QList<QWidget*> ()
                   <<ui.splitTimeSpin
                   <<ui.splitTimeCombo));
  addCheckEnabler(ui.splitDistanceCheck,
                  (QList<QWidget*> ()
                   <<ui.splitDistSpin
                   <<ui.splitDistCombo));

  connect(ui.mergeCheck, &QAbstractButton::clicked, this, &TrackWidget::mergeCheckX);
  connect(ui.packCheck,  &QAbstractButton::clicked,  this, &TrackWidget::packCheckX);
  connect(ui.startCheck, &QAbstractButton::clicked,  this, &TrackWidget::otherCheckX);
  connect(ui.stopCheck,   &QAbstractButton::clicked, this, &TrackWidget::otherCheckX);

  connect(ui.splitDateCheck,   &QAbstractButton::clicked, this, &TrackWidget::splitDateX);
  connect(ui.splitTimeCheck,   &QAbstractButton::clicked, this, &TrackWidget::splitTimeX);
  connect(ui.splitDistanceCheck,   &QAbstractButton::clicked, this, &TrackWidget::splitDistanceX);

  ui.startEdit->setDisplayFormat("dd MMM yyyy hh:mm:ss AP");
  ui.stopEdit->setDisplayFormat("dd MMM yyyy hh:mm:ss AP");

  // Collect the data fields.
  fopts << QSharedPointer<BoolFilterOption>(new BoolFilterOption(tfd.title,  ui.titleCheck));
  fopts << QSharedPointer<BoolFilterOption>(new BoolFilterOption(tfd.move,   ui.moveCheck));
  fopts << QSharedPointer<BoolFilterOption>(new BoolFilterOption(tfd.TZ,     ui.TZCheck));
  fopts << QSharedPointer<BoolFilterOption>(new BoolFilterOption(tfd.start,  ui.startCheck));
  fopts << QSharedPointer<BoolFilterOption>(new BoolFilterOption(tfd.stop,   ui.stopCheck));
  fopts << QSharedPointer<BoolFilterOption>(new BoolFilterOption(tfd.pack,   ui.packCheck));
  fopts << QSharedPointer<BoolFilterOption>(new BoolFilterOption(tfd.merge,  ui.mergeCheck));
  fopts << QSharedPointer<BoolFilterOption>(new BoolFilterOption(tfd.splitByDate,  ui.splitDateCheck));
  fopts << QSharedPointer<BoolFilterOption>(new BoolFilterOption(tfd.splitByTime,  ui.splitTimeCheck));
  fopts << QSharedPointer<BoolFilterOption>(new BoolFilterOption(tfd.splitByDistance,  ui.splitDistanceCheck));
  fopts << QSharedPointer<BoolFilterOption>(new BoolFilterOption(tfd.GPSFixes,  ui.GPSFixesCheck));
  fopts << QSharedPointer<BoolFilterOption>(new BoolFilterOption(tfd.course, ui.courseCheck));
  fopts << QSharedPointer<BoolFilterOption>(new BoolFilterOption(tfd.speed,  ui.speedCheck));

  fopts << QSharedPointer<IntSpinFilterOption>(new IntSpinFilterOption(tfd.weeks,  ui.weeksSpin, ui.weeksSpin->minimum(), ui.weeksSpin->maximum()));
  fopts << QSharedPointer<IntSpinFilterOption>(new IntSpinFilterOption(tfd.days,  ui.daysSpin, ui.daysSpin->minimum(), ui.daysSpin->maximum()));
  fopts << QSharedPointer<IntSpinFilterOption>(new IntSpinFilterOption(tfd.hours, ui.hoursSpin, ui.hoursSpin->minimum(), ui.hoursSpin->maximum()));
  fopts << QSharedPointer<IntSpinFilterOption>(new IntSpinFilterOption(tfd.mins,  ui.minsSpin, ui.minsSpin->minimum(), ui.minsSpin->maximum()));
  fopts << QSharedPointer<IntSpinFilterOption>(new IntSpinFilterOption(tfd.secs,  ui.secsSpin, ui.secsSpin->minimum(), ui.secsSpin->maximum()));
  fopts << QSharedPointer<IntSpinFilterOption>(new IntSpinFilterOption(tfd.splitTime,  ui.splitTimeSpin, 0, 1000));
  fopts << QSharedPointer<IntSpinFilterOption>(new IntSpinFilterOption(tfd.splitDist,  ui.splitDistSpin, 0, 5280));

  fopts << QSharedPointer<DateTimeFilterOption>(new DateTimeFilterOption(tfd.startTime, ui.startEdit));
  fopts << QSharedPointer<DateTimeFilterOption>(new DateTimeFilterOption(tfd.stopTime,  ui.stopEdit));

  fopts << QSharedPointer<StringFilterOption>(new StringFilterOption(tfd.titleString, ui.titleText));
  fopts << QSharedPointer<ComboFilterOption>(new ComboFilterOption(tfd.GPSFixesVal,  ui.GPSFixesCombo));
  fopts << QSharedPointer<ComboFilterOption>(new ComboFilterOption(tfd.splitTimeUnit,  ui.splitTimeCombo));
  fopts << QSharedPointer<ComboFilterOption>(new ComboFilterOption(tfd.splitDistUnit,  ui.splitDistCombo));
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
  if (ui.mergeCheck->isChecked()) {
    ui.packCheck->setChecked(false);
  }
  otherCheckX();
}
//------------------------------------------------------------------------
void TrackWidget::packCheckX()
{
  if (ui.packCheck->isChecked()) {
    ui.mergeCheck->setChecked(false);
  }
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
WayPtsWidget::WayPtsWidget(QWidget* parent, WayPtsFilterData& wfd): FilterWidget(parent), wfd(wfd)
{
  ui.setupUi(this);
  addCheckEnabler(ui.duplicatesCheck,
                  QList<QWidget*>() << ui.shortNamesCheck << ui.locationsCheck);
  addCheckEnabler(ui.positionCheck,
                  QList<QWidget*>() << ui.positionText << ui.positionUnitCombo);
  addCheckEnabler(ui.radiusCheck,
                  QList<QWidget*>() << ui.latLabel << ui.latText << ui.longLabel <<
                  ui.longText << ui.radiusUnitCombo << ui.radiusText);

  fopts << QSharedPointer<BoolFilterOption>(new BoolFilterOption(wfd.duplicates, ui.duplicatesCheck));
  fopts << QSharedPointer<BoolFilterOption>(new BoolFilterOption(wfd.shortNames, ui.shortNamesCheck));
  fopts << QSharedPointer<BoolFilterOption>(new BoolFilterOption(wfd.locations, ui.locationsCheck));
  fopts << QSharedPointer<BoolFilterOption>(new BoolFilterOption(wfd.position, ui.positionCheck));
  fopts << QSharedPointer<BoolFilterOption>(new BoolFilterOption(wfd.radius, ui.radiusCheck));
  fopts << QSharedPointer<DoubleFilterOption>(new DoubleFilterOption(wfd.positionVal, ui.positionText, 0.0, 1.0E308));
  fopts << QSharedPointer<DoubleFilterOption>(new DoubleFilterOption(wfd.radiusVal, ui.radiusText, 0.0, 1.0E308));
  fopts << QSharedPointer<DoubleFilterOption>(new DoubleFilterOption(wfd.longVal, ui.longText, -180, 180, 7, 'f'));
  fopts << QSharedPointer<DoubleFilterOption>(new DoubleFilterOption(wfd.latVal, ui.latText,  -90, 90, 7, 'f'));
  fopts << QSharedPointer<ComboFilterOption>(new ComboFilterOption(wfd.positionUnit, ui.positionUnitCombo));
  fopts << QSharedPointer<ComboFilterOption>(new ComboFilterOption(wfd.radiusUnit, ui.radiusUnitCombo));

  connect(ui.shortNamesCheck, &QAbstractButton::clicked, this, &WayPtsWidget::shortNamesCkX);
  connect(ui.locationsCheck, &QAbstractButton::clicked, this, &WayPtsWidget::locationsCkX);
  setWidgetValues();
  checkChecks();
}
//------------------------------------------------------------------------
void WayPtsWidget::shortNamesCkX()
{
  if (!ui.shortNamesCheck->isChecked()) {
    ui.locationsCheck->setChecked(true);
  }
}
//------------------------------------------------------------------------
void WayPtsWidget::locationsCkX()
{
  if (!ui.locationsCheck->isChecked()) {
    ui.shortNamesCheck->setChecked(true);
  }
}

//------------------------------------------------------------------------
//------------------------------------------------------------------------
RtTrkWidget::RtTrkWidget(QWidget* parent, RtTrkFilterData& rfd): FilterWidget(parent), rfd(rfd)
{
  ui.setupUi(this);
  addCheckEnabler(ui.simplifyCheck,
                  QList<QWidget*>() << ui.limitToLabel << ui.limitToSpin << ui.pointLabel);

  fopts << QSharedPointer<BoolFilterOption>(new BoolFilterOption(rfd.simplify_, ui.simplifyCheck));
  fopts << QSharedPointer<BoolFilterOption>(new BoolFilterOption(rfd.reverse_, ui.reverseCheck));
  fopts << QSharedPointer<IntSpinFilterOption>(new IntSpinFilterOption(rfd.limitTo_, ui.limitToSpin, 1, 5000));
  setWidgetValues();
  checkChecks();
}

//------------------------------------------------------------------------
//------------------------------------------------------------------------
MiscFltWidget::MiscFltWidget(QWidget* parent, MiscFltFilterData& mfd): FilterWidget(parent), mfd(mfd)
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
  addCheckEnabler(ui.sortWptCheck, ui.sortWptBy);
  addCheckEnabler(ui.sortRteCheck, ui.sortRteBy);
  addCheckEnabler(ui.sortTrkCheck, ui.sortTrkBy);

  fopts << QSharedPointer<BoolFilterOption>(new BoolFilterOption(mfd.transform_, ui.transformCheck));
  fopts << QSharedPointer<BoolFilterOption>(new BoolFilterOption(mfd.swap_, ui.swapCheck));
  fopts << QSharedPointer<BoolFilterOption>(new BoolFilterOption(mfd.del_, ui.deleteCheck));
  fopts << QSharedPointer<BoolFilterOption>(new BoolFilterOption(mfd.nukeTracks_, ui.nukeTracks));
  fopts << QSharedPointer<BoolFilterOption>(new BoolFilterOption(mfd.nukeRoutes_, ui.nukeRoutes));
  fopts << QSharedPointer<BoolFilterOption>(new BoolFilterOption(mfd.nukeWaypoints_, ui.nukeWaypoints));
  fopts << QSharedPointer<BoolFilterOption>(new BoolFilterOption(mfd.sortWpt_, ui.sortWptCheck));
  fopts << QSharedPointer<BoolFilterOption>(new BoolFilterOption(mfd.sortRte_, ui.sortRteCheck));
  fopts << QSharedPointer<BoolFilterOption>(new BoolFilterOption(mfd.sortTrk_, ui.sortTrkCheck));
  fopts << QSharedPointer<ComboFilterOption>(new ComboFilterOption(mfd.transformVal_,  ui.transformCombo));
  fopts << QSharedPointer<ComboFilterOption>(new ComboFilterOption(mfd.sortWptBy_, ui.sortWptBy));
  fopts << QSharedPointer<ComboFilterOption>(new ComboFilterOption(mfd.sortRteBy_, ui.sortRteBy));
  fopts << QSharedPointer<ComboFilterOption>(new ComboFilterOption(mfd.sortTrkBy_, ui.sortTrkBy));

  setWidgetValues();
  checkChecks();
}
