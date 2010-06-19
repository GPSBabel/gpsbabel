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
//  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111
//  USA
//

#ifndef DONATE_H
#define DONATE_H

#include "ui_donate.h"

class Donate: public QDialog {
 Q_OBJECT

 public:
  Donate(QWidget *parent);
  void showNever(bool f)  { 
    ui.neverAgain->setVisible(f); 
    ui.textLine2->setVisible(f); 
  }
  bool neverAgain() { return ui.neverAgain->isChecked(); }

 private:
  Ui_Donate  ui;

 private slots:
  void contributeClicked();

};

#endif
