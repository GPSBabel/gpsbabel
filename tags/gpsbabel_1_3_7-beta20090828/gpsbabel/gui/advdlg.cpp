// -*- C++ -*-
// $Id: advdlg.cpp,v 1.2 2009-08-28 17:08:55 robertl Exp $
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
#include <QMessageBox>
#include <QProcess>
#include <QFileDialog>
#include <QSettings>
#include "advdlg.h"

//------------------------------------------------------------------------
AdvDlg::AdvDlg(QWidget* parent,
	       bool &synthShortNames,
	       bool &forceGPSTypes,
	       bool &enableCharSetXform,
	       bool &previewGmap,
	       int  &debugLevel):
  QDialog(parent),
  synthShortNames(synthShortNames),
  forceGPSTypes(forceGPSTypes),
  enableCharSetXform(enableCharSetXform),
  previewGmap(previewGmap),
  debugLevel(debugLevel)
{
  ui.setupUi(this);
  ui.synthShortNames->setChecked(synthShortNames);
  ui.forceGPSTypes->setChecked(forceGPSTypes);
  ui.enableCharSetXform->setChecked(enableCharSetXform);
  ui.previewGmap->setChecked(previewGmap);
  ui.debugCombo->setCurrentIndex(debugLevel+1);
  ui.buttonBox->button(QDialogButtonBox::Ok)->setIcon(QIcon(":images/ok"));
  ui.buttonBox->button(QDialogButtonBox::Cancel)->setIcon(QIcon(":images/cancel"));
  connect(ui.buttonBox, SIGNAL(accepted()), this, SLOT(acceptClicked()));
  connect(ui.buttonBox, SIGNAL(rejected()), this, SLOT(rejectClicked()));
}

void AdvDlg::acceptClicked()
{
  synthShortNames = ui.synthShortNames->isChecked();
  forceGPSTypes = ui.forceGPSTypes->isChecked();
  enableCharSetXform = ui.enableCharSetXform->isChecked();
  previewGmap = ui.previewGmap->isChecked();
  debugLevel = ui.debugCombo->currentIndex()-1;
  accept();
}

void AdvDlg::rejectClicked()
{
  reject();
}
