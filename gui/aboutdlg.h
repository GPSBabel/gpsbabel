// -*- C++ -*-
// $Id: aboutdlg.h,v 1.1 2009-07-05 21:14:56 robertl Exp $
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

#ifndef ABOUTDLG_H
#define ABOUTDLG_H

#include <QString>            // for QString
#include <QDialog>            // for QDialog
#include <QWidget>            // for QWidget
#include "ui_aboutui.h"       // for Ui_AboutDlg

class AboutDlg: public QDialog
{
public:
  AboutDlg(QWidget* parent,  const QString& ver1,
           const QString& ver2, const QString& ver3,
           const QString& date,
           const QString& installationId);

private:
  Ui_AboutDlg  ui_;

};

#endif
