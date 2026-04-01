// -*- C++ -*-
//------------------------------------------------------------------------
//
//  Copyright (C) 2010  Robert Lipe <robertlipe@gpsbabel.org>
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

#ifndef DONATE_H
#define DONATE_H

#include <QCheckBox>    // for QCheckBox
#include <QDialog>      // for QDialog
#include <QLabel>       // for QLabel
#include <QObject>      // for Q_OBJECT, slots
#include <QWidget>      // for QWidget
#include "ui_donate.h"  // for Ui_Donate

class Donate: public QDialog
{
  Q_OBJECT

public:
  Donate(QWidget* parent);
  void showNever(bool f)
  {
    ui_.neverAgain->setVisible(f);
    ui_.textLine2->setVisible(f);
  }
  bool neverAgain()
  {
    return ui_.neverAgain->isChecked();
  }

private:
  Ui_Donate  ui_;

private slots:
  void contributeClicked();

};

#endif
