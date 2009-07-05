// -*- C++ -*-
// $Id: persistdlg.cpp,v 1.1 2009-07-05 21:14:56 robertl Exp $
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

#include "persistdlg.h"
#include <QApplication>
#include <QCloseEvent>


PersistentDialog::PersistentDialog(QWidget *parent): 
  QWidget(parent, Qt::Dialog)
{
  returnValue = 0;
  setWindowModality(Qt::WindowModal);
}

PersistentDialog::~PersistentDialog()
{
}


void PersistentDialog::accept()
{
  returnValue = 1;
  eventLoop->exit(1);
}

void PersistentDialog::reject()
{
  returnValue = 0;
  eventLoop->exit();
}

int PersistentDialog::exec()
{
  returnValue = 0;
  show();

  QEventLoop evl;
  eventLoop = &evl;
  (void) eventLoop->exec();
  hide();
  return returnValue;
}

void PersistentDialog::closeEvent(QCloseEvent*)  
{
  //eventLoop->exit();
  reject();
}
