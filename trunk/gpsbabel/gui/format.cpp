// -*- C++ -*-
// $Id: format.cpp,v 1.2 2009-11-02 20:38:02 robertl Exp $
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

QString Format::htmlBase = QString();

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
  saveOptions(settings, name+".input", inputOptions);
  saveOptions(settings, name+".output", outputOptions);
}

void Format::restoreSettings(QSettings &settings)
{
  restoreOptions(settings, name+".input", inputOptions);
  restoreOptions(settings, name+".output", outputOptions);
}

void Format::setToDefault()
{
  for (int i=0; i<inputOptions.size(); i++) {
    inputOptions[i].setSelected(false);
    inputOptions[i].setValue(QVariant());
  }
  for (int i=0; i<outputOptions.size(); i++) {
    outputOptions[i].setSelected(false);
    outputOptions[i].setValue(QVariant());
  }
}
