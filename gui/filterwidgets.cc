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

#include <cassert>       // for assert
#include <limits>        // for numeric_limits

#include <QChar>         // for QChar
#include <QCheckBox>     // for QCheckBox
#include <QEvent>        // for QEvent
#include <QLabel>        // for QLabel
#include <QRadioButton>  // for QRadioButton
#if (QT_VERSION >= QT_VERSION_CHECK(6, 7, 0))
#include <QTimeZone>     // for QTimeZone
#endif
#include <Qt>            // for TimeSpec


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

  connect(ui.localTime, &QAbstractButton::clicked, this, &TrackWidget::TZX);
  connect(ui.utc, &QAbstractButton::clicked, this, &TrackWidget::TZX);

  ui.startEdit->setDisplayFormat("dd MMM yyyy hh:mm:ss AP");
  ui.stopEdit->setDisplayFormat("dd MMM yyyy hh:mm:ss AP");

  // Qt5 QDateTimeEdit::setDateTime ignored the passed QDateTime::timeSpec.
  // Qt6 QDateTimeEdit::setDateTime will convert the passed QDateTime if the passed
  // QDateTime::timeSpec doesn't match QDateTimeEdit::timeSpec.
  // If the two timeSpecs match Qt5 and Qt6 behave the same.
#if (QT_VERSION >= QT_VERSION_CHECK(6, 7, 0))
  assert(tfd.startTime.timeZone() == tfd.stopTime.timeZone());
  assert((tfd.startTime.timeZone() == QTimeZone::UTC) || (tfd.startTime.timeZone() == QTimeZone::LocalTime));
  ui.startEdit->setTimeZone(tfd.startTime.timeZone());
  ui.stopEdit->setTimeZone(tfd.stopTime.timeZone());
  // Make sure the initial state of the localTime and utc radio buttons
  // is in agreement with the startTime::timeSpec and stopTime::timeSpec.
  tfd.localTime = tfd.startTime.timeZone() == QTimeZone::LocalTime;
#else
  assert(tfd.startTime.timeSpec() == tfd.stopTime.timeSpec());
  assert((tfd.startTime.timeSpec() == Qt::UTC) || (tfd.startTime.timeSpec() == Qt::LocalTime));
  ui.startEdit->setTimeSpec(tfd.startTime.timeSpec());
  ui.stopEdit->setTimeSpec(tfd.stopTime.timeSpec());
  // Make sure the initial state of the localTime and utc radio buttons
  // is in agreement with the startTime::timeSpec and stopTime::timeSpec.
  tfd.localTime = tfd.startTime.timeSpec() == Qt::LocalTime;
#endif
  tfd.utc = !tfd.localTime;

  // Collect the data fields.
  addFilterOption(std::make_unique<BoolFilterOption>(tfd.title,  ui.titleCheck));
  addFilterOption(std::make_unique<BoolFilterOption>(tfd.move,   ui.moveCheck));
  addFilterOption(std::make_unique<BoolFilterOption>(tfd.localTime,     ui.localTime));
  addFilterOption(std::make_unique<BoolFilterOption>(tfd.utc,     ui.utc));
  addFilterOption(std::make_unique<BoolFilterOption>(tfd.start,  ui.startCheck));
  addFilterOption(std::make_unique<BoolFilterOption>(tfd.stop,   ui.stopCheck));
  addFilterOption(std::make_unique<BoolFilterOption>(tfd.pack,   ui.packCheck));
  addFilterOption(std::make_unique<BoolFilterOption>(tfd.merge,  ui.mergeCheck));
  addFilterOption(std::make_unique<BoolFilterOption>(tfd.splitByDate,  ui.splitDateCheck));
  addFilterOption(std::make_unique<BoolFilterOption>(tfd.splitByTime,  ui.splitTimeCheck));
  addFilterOption(std::make_unique<BoolFilterOption>(tfd.splitByDistance,  ui.splitDistanceCheck));
  addFilterOption(std::make_unique<BoolFilterOption>(tfd.GPSFixes,  ui.GPSFixesCheck));
  addFilterOption(std::make_unique<BoolFilterOption>(tfd.course, ui.courseCheck));
  addFilterOption(std::make_unique<BoolFilterOption>(tfd.speed,  ui.speedCheck));

  addFilterOption(std::make_unique<IntSpinFilterOption>(tfd.weeks,  ui.weeksSpin, ui.weeksSpin->minimum(), ui.weeksSpin->maximum()));
  addFilterOption(std::make_unique<IntSpinFilterOption>(tfd.days,  ui.daysSpin, ui.daysSpin->minimum(), ui.daysSpin->maximum()));
  addFilterOption(std::make_unique<IntSpinFilterOption>(tfd.hours, ui.hoursSpin, ui.hoursSpin->minimum(), ui.hoursSpin->maximum()));
  addFilterOption(std::make_unique<IntSpinFilterOption>(tfd.mins,  ui.minsSpin, ui.minsSpin->minimum(), ui.minsSpin->maximum()));
  addFilterOption(std::make_unique<IntSpinFilterOption>(tfd.secs,  ui.secsSpin, ui.secsSpin->minimum(), ui.secsSpin->maximum()));
  addFilterOption(std::make_unique<IntSpinFilterOption>(tfd.splitTime,  ui.splitTimeSpin, 0, 1000));
  addFilterOption(std::make_unique<IntSpinFilterOption>(tfd.splitDist,  ui.splitDistSpin, 0, 5280));

  addFilterOption(std::make_unique<DateTimeFilterOption>(tfd.startTime, ui.startEdit));
  addFilterOption(std::make_unique<DateTimeFilterOption>(tfd.stopTime,  ui.stopEdit));

  addFilterOption(std::make_unique<StringFilterOption>(tfd.titleString, ui.titleText));
  addFilterOption(std::make_unique<ComboFilterOption>(tfd.GPSFixesVal,  ui.GPSFixesCombo));
  addFilterOption(std::make_unique<ComboFilterOption>(tfd.splitTimeUnit,  ui.splitTimeCombo));
  addFilterOption(std::make_unique<ComboFilterOption>(tfd.splitDistUnit,  ui.splitDistCombo));
  setWidgetValues();
  checkChecks();
}

//------------------------------------------------------------------------
void TrackWidget::otherCheckX() const
{
  ui.localTime->setEnabled(ui.stopCheck->isChecked() || ui.startCheck->isChecked());
  ui.utc->setEnabled(ui.stopCheck->isChecked() || ui.startCheck->isChecked());

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
void TrackWidget::TZX() const
{
  if (ui.localTime->isChecked()) {
#if (QT_VERSION >= QT_VERSION_CHECK(6, 7, 0))
    ui.startEdit->setTimeZone(QTimeZone::LocalTime);
    ui.stopEdit->setTimeZone(QTimeZone::LocalTime);
#else
    ui.startEdit->setTimeSpec(Qt::LocalTime);
    ui.stopEdit->setTimeSpec(Qt::LocalTime);
#endif
  } else {
#if (QT_VERSION >= QT_VERSION_CHECK(6, 7, 0))
    ui.startEdit->setTimeZone(QTimeZone::UTC);
    ui.stopEdit->setTimeZone(QTimeZone::UTC);
#else
    ui.startEdit->setTimeSpec(Qt::UTC);
    ui.stopEdit->setTimeSpec(Qt::UTC);
#endif
  }
  // Force update of Edit displays, so the displayed
  // datetimes are in sync with the specified time spec.
  auto ev = QEvent(QEvent::LocaleChange);
  ui.startEdit->event(&ev);
  ui.stopEdit->event(&ev);
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

  addFilterOption(std::make_unique<BoolFilterOption>(wfd.duplicates, ui.duplicatesCheck));
  addFilterOption(std::make_unique<BoolFilterOption>(wfd.shortNames, ui.shortNamesCheck));
  addFilterOption(std::make_unique<BoolFilterOption>(wfd.locations, ui.locationsCheck));
  addFilterOption(std::make_unique<BoolFilterOption>(wfd.position, ui.positionCheck));
  addFilterOption(std::make_unique<BoolFilterOption>(wfd.radius, ui.radiusCheck));
  addFilterOption(std::make_unique<DoubleFilterOption>(wfd.positionVal, ui.positionText, 0.0, 1.0E308));
  addFilterOption(std::make_unique<DoubleFilterOption>(wfd.radiusVal, ui.radiusText, 0.0, 1.0E308));
  addFilterOption(std::make_unique<DoubleFilterOption>(wfd.longVal, ui.longText, -180, 180, 7, 'f'));
  addFilterOption(std::make_unique<DoubleFilterOption>(wfd.latVal, ui.latText,  -90, 90, 7, 'f'));
  addFilterOption(std::make_unique<ComboFilterOption>(wfd.positionUnit, ui.positionUnitCombo));
  addFilterOption(std::make_unique<ComboFilterOption>(wfd.radiusUnit, ui.radiusUnitCombo));

  connect(ui.shortNamesCheck, &QAbstractButton::clicked, this, &WayPtsWidget::shortNamesCkX);
  connect(ui.locationsCheck, &QAbstractButton::clicked, this, &WayPtsWidget::locationsCkX);
  setWidgetValues();
  checkChecks();
}
//------------------------------------------------------------------------
void WayPtsWidget::shortNamesCkX() const
{
  if (!ui.shortNamesCheck->isChecked()) {
    ui.locationsCheck->setChecked(true);
  }
}
//------------------------------------------------------------------------
void WayPtsWidget::locationsCkX() const
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

  addFilterOption(std::make_unique<BoolFilterOption>(rfd.simplify_, ui.simplifyCheck));
  addFilterOption(std::make_unique<BoolFilterOption>(rfd.reverse_, ui.reverseCheck));
  addFilterOption(std::make_unique<IntSpinFilterOption>(rfd.limitTo_, ui.limitToSpin, 1, std::numeric_limits<int>::max()));
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

  addFilterOption(std::make_unique<BoolFilterOption>(mfd.transform_, ui.transformCheck));
  addFilterOption(std::make_unique<BoolFilterOption>(mfd.swap_, ui.swapCheck));
  addFilterOption(std::make_unique<BoolFilterOption>(mfd.del_, ui.deleteCheck));
  addFilterOption(std::make_unique<BoolFilterOption>(mfd.nukeTracks_, ui.nukeTracks));
  addFilterOption(std::make_unique<BoolFilterOption>(mfd.nukeRoutes_, ui.nukeRoutes));
  addFilterOption(std::make_unique<BoolFilterOption>(mfd.nukeWaypoints_, ui.nukeWaypoints));
  addFilterOption(std::make_unique<BoolFilterOption>(mfd.sortWpt_, ui.sortWptCheck));
  addFilterOption(std::make_unique<BoolFilterOption>(mfd.sortRte_, ui.sortRteCheck));
  addFilterOption(std::make_unique<BoolFilterOption>(mfd.sortTrk_, ui.sortTrkCheck));
  addFilterOption(std::make_unique<ComboFilterOption>(mfd.transformVal_,  ui.transformCombo));
  addFilterOption(std::make_unique<ComboFilterOption>(mfd.sortWptBy_, ui.sortWptBy));
  addFilterOption(std::make_unique<ComboFilterOption>(mfd.sortRteBy_, ui.sortRteBy));
  addFilterOption(std::make_unique<ComboFilterOption>(mfd.sortTrkBy_, ui.sortTrkBy));

  setWidgetValues();
  checkChecks();
}
