// -*- C++ -*-
// $Id: advdlg.cpp,v 1.3 2009-11-02 20:38:02 robertl Exp $
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

#include "advdlg.h"
#include <QCheckBox>         // for QCheckBox
#include <QComboBox>         // for QComboBox
#include <QDialogButtonBox>  // for QDialogButtonBox

//------------------------------------------------------------------------
AdvDlg::AdvDlg(QWidget* parent,
               bool& synthShortNames,
               bool mapPreviewEnabled,
               bool& previewGmap,
               int&  debugLevel):
  QDialog(parent),
  synthShortNames_(synthShortNames),
  previewGmap_(previewGmap),
  debugLevel_(debugLevel)
{
  ui_.setupUi(this);
  ui_.synthShortNames->setChecked(synthShortNames);
  ui_.previewGmap->setEnabled(mapPreviewEnabled);
  if (!mapPreviewEnabled) {
    previewGmap = false;
  }
  ui_.previewGmap->setChecked(previewGmap);
  ui_.debugCombo->setCurrentIndex(debugLevel+1);
#if defined (Q_OS_WIN)
  ui_.buttonBox->button(QDialogButtonBox::Ok)->setIcon(QIcon(":/images/ok.png"));
  ui_.buttonBox->button(QDialogButtonBox::Cancel)->setIcon(QIcon(":/images/cancel.png"));
#endif // Q_OS_WIN
  connect(ui_.buttonBox, &QDialogButtonBox::accepted, this, &AdvDlg::acceptClicked);
  connect(ui_.buttonBox, &QDialogButtonBox::rejected, this, &AdvDlg::rejectClicked);

#ifdef DISABLE_MAPPREVIEW
  ui_.previewGmap->hide();
#endif
}

void AdvDlg::acceptClicked()
{
  synthShortNames_ = ui_.synthShortNames->isChecked();
  previewGmap_ = ui_.previewGmap->isChecked();
  debugLevel_ = ui_.debugCombo->currentIndex()-1;
  accept();
}

void AdvDlg::rejectClicked()
{
  reject();
}
