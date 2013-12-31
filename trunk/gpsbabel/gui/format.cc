// -*- C++ -*-
// $Id: format.cpp,v 1.4 2010-02-14 05:33:37 robertl Exp $
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
#include "format.h"
#include "mainwindow.h"

QString Format::htmlBase_ = QString();

static void saveOptions(QSettings &settings, const QString &prefix, const QList<FormatOption> &options) {
  for (int i=0; i<options.size(); i++) {
    QString kp = prefix + "." + options[i].getName();
    QString k1 = kp + ".selected";
    QString k2 = kp + ".value";
    settings.setValue(k1, options[i].getSelected());
    settings.setValue(k2, options[i].getValue());
  }
}

static void restoreOptions(QSettings &settings, const QString&prefix, QList<FormatOption> &options) {
  for (int i=0; i<options.size(); i++) {
    QString kp = prefix + "." + options[i].getName();
    QString k1 = kp + ".selected";
    QString k2 = kp + ".value";
    if (settings.contains(k1) && settings.contains(k2)) {
      options[i].setSelected(settings.value(k1).toBool());
      options[i].setValue(settings.value(k2));
    }
  }
}

void Format::saveSettings(QSettings &settings)
{
  saveOptions(settings, name_+".input", inputOptions_);
  saveOptions(settings, name_+".output", outputOptions_);
  settings.setValue(name_ + ".readcount", getReadUseCount());
  settings.setValue(name_ + ".writecount", getWriteUseCount());
  settings.setValue(name_+".hidden", isHidden());
}

void Format::restoreSettings(QSettings &settings)
{
  restoreOptions(settings, name_ + ".input", inputOptions_);
  restoreOptions(settings, name_ + ".output", outputOptions_);
  bumpReadUseCount(settings.value(name_ + ".readcount").toInt());
  bumpWriteUseCount(settings.value(name_ + ".writecount").toInt());
  hidden_ = settings.value(name_ + ".hidden", false).toBool();
}

void Format::setToDefault()
{
  for (int i=0; i<inputOptions_.size(); i++) {
    inputOptions_[i].setSelected(false);
    inputOptions_[i].setValue(QVariant());
  }
  for (int i=0; i<outputOptions_.size(); i++) {
    outputOptions_[i].setSelected(false);
    outputOptions_[i].setValue(QVariant());
  }
}
