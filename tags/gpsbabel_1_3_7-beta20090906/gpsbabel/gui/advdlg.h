// -*- C++ -*-
// $Id: advdlg.h,v 1.1 2009-07-05 21:14:56 robertl Exp $
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
#ifndef ADVDLG_H
#define ADVDLG_H

#include "ui_advui.h"

class AdvDlg: public QDialog {
  Q_OBJECT
  
public:
  AdvDlg(QWidget* parent, 
	 bool &synthShortNames, 
	 bool &forceGPSTypes,
	 bool &enableCharSetXform,
	 bool &previewGmap,
	 int   &debugLevel);
  QPushButton* formatButton() { return ui.formatButton; };

private:
  Ui_AdvUi ui;
  bool &synthShortNames;
  bool &forceGPSTypes;
  bool &enableCharSetXform;
  bool &previewGmap;
  int  &debugLevel;

 protected:

 private slots:
  void acceptClicked();
  void rejectClicked();
};


#endif
