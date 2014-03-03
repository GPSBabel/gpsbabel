// -*- C++ -*-
// $Id: filterdlg.cpp,v 1.4 2009-09-08 16:06:32 robertl Exp $
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

#include <QMessageBox>
#include "filterdlg.h"
#include "help.h"
#include "appname.h"

int FilterDialog::lastPage_ = 0;

FilterDialog::FilterDialog(QWidget*parent, AllFiltersData &fd): QDialog(parent), fd_(fd)
{
  ui_.setupUi(this);
  ui_.filterList->clear();

  widgetStack_ = new QStackedWidget(ui_.frame);
  QHBoxLayout *layout = new QHBoxLayout(ui_.frame);
  layout->addWidget(widgetStack_);
  layout->setContentsMargins(2, 2, 2, 2);

  addFilterPage(tr("Tracks"),
		new TrackWidget(widgetStack_, fd.trackFilterData), &fd.trackFilterData.inUse_);

  addFilterPage(tr("Waypoints"),
		new WayPtsWidget(widgetStack_, fd.wayPtsFilterData), &fd.wayPtsFilterData.inUse_);

  addFilterPage(tr("Routes & Tracks"),
		new RtTrkWidget(widgetStack_, fd.rtTrkFilterData), &fd.rtTrkFilterData.inUse_);

  addFilterPage(tr("Miscellaneous"),
		new MiscFltWidget(widgetStack_, fd.miscFltFilterData), &fd.miscFltFilterData.inUse_);

  connect(ui_.filterList, SIGNAL(currentRowChanged(int)),
	  this, SLOT(pageSelectionChanged(int)));

  connect(ui_.filterList, SIGNAL(itemClicked(QListWidgetItem *)),
	  this, SLOT(itemClickedX(QListWidgetItem*)));

  connect(ui_.helpButton, SIGNAL(clicked()), this, SLOT(helpX()));
  connect(ui_.resetButton, SIGNAL(clicked()), this, SLOT(resetX()));


  ui_.buttonBox->button(QDialogButtonBox::Ok)->setIcon(QIcon(":images/ok"));
  ui_.buttonBox->button(QDialogButtonBox::Cancel)->setIcon(QIcon(":images/cancel"));

  ui_.filterList->setCurrentRow(lastPage_);

  // So that it occupies minimum space.
  this->resize(100, 100);
}


//------------------------------------------------------------------------
void FilterDialog::addFilterPage(const QString &name, FilterWidget *fw, bool*use)
{
  QListWidgetItem *it = new QListWidgetItem(name);
  it->setCheckState(*use? Qt::Checked:Qt::Unchecked);
  fw->setEnabled(*use);
  ui_.filterList->addItem(it);
  widgetStack_->addWidget(fw);
  pages_    << fw;
  usePages_ << use;
}

//------------------------------------------------------------------------
void FilterDialog::itemClickedX(QListWidgetItem *it)
{
  int row = ui_.filterList->row(it);
  bool b = (it->checkState() == Qt::Checked);
  pages_[row]->setEnabled(b);
  pages_[row]->checkChecks();
}
//------------------------------------------------------------------------
void FilterDialog::pageSelectionChanged(int i)
{
  widgetStack_->setCurrentWidget(pages_[i]);
}

//------------------------------------------------------------------------
void FilterDialog::resetX()
{
  int ret = QMessageBox::warning
    (this, QString(appName),
     tr("Are you sure you want to reset all filter options to default values?"),
     QMessageBox::Yes | QMessageBox::No);

  if (ret == QMessageBox::Yes) {
    fd_.defaultAll();
    for (int i=0; i<pages_.size(); i++) {
      pages_[i]->setWidgetValues();
      pages_[i]->setEnabled(*(usePages_[i]));
      pages_[i]->checkChecks();
      ui_.filterList->item(i)->setCheckState(*(usePages_[i]) ? Qt::Checked: Qt::Unchecked);
    }
  }
}

//------------------------------------------------------------------------
void FilterDialog::helpX()
{
  ShowHelp("Data_Filters.html");
}

//------------------------------------------------------------------------
void FilterDialog::runDialog()
{
  if (exec()) {
    for (int i=0; i<pages_.size(); i++) {
      pages_[i]->getWidgetValues();
      *(usePages_[i]) = ui_.filterList->item(i)->checkState() == Qt::Checked;
    }
  }
  lastPage_ = ui_.filterList->currentRow();
}
