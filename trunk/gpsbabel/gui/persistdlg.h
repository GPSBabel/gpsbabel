// -*- C++ -*-
// $Id: persistdlg.h,v 1.1 2009-07-05 21:14:56 robertl Exp $
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
//
// A dialog that will prevent the escape key from closing it.  Also no
// default buttons, and "ENTER" key actions.  Needed so that you won't
// lose all the edited data --- The regular Qt QDialog has no way of
// bypassing ESC key.
// 
#ifndef PERSISTDLG_H
#define PERSISTDLG_H

#include <QWidget>
#include <QEventLoop>

class PersistentDialog: public QWidget
{
Q_OBJECT
 public: 
  PersistentDialog(QWidget *parent = 0);
  ~PersistentDialog();
  int exec();

protected:
  void closeEvent(QCloseEvent*ev);

protected slots:
  void accept();
  void reject();

private:
  int returnValue;
  QEventLoop *eventLoop;
};



#endif

