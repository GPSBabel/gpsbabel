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

int FilterDialog::lastPage = 0;

FilterDialog::FilterDialog(QWidget*parent, AllFiltersData &fd): QDialog(parent), fd(fd)
{
  ui.setupUi(this);
  ui.filterList->clear();

  widgetStack = new QStackedWidget(ui.frame);
  QHBoxLayout *layout = new QHBoxLayout(ui.frame);
  layout->addWidget(widgetStack);
  layout->setContentsMargins(2, 2, 2, 2);

  addFilterPage(tr("Tracks"),
		new TrackWidget(widgetStack, fd.trackFilterData), &fd.trackFilterData.inUse);

  addFilterPage(tr("Waypoints"),
		new WayPtsWidget(widgetStack, fd.wayPtsFilterData), &fd.wayPtsFilterData.inUse);

  addFilterPage(tr("Routes & Tracks"),
		new RtTrkWidget(widgetStack, fd.rtTrkFilterData), &fd.rtTrkFilterData.inUse);

  addFilterPage(tr("Miscellaneous"),
		new MiscFltWidget(widgetStack, fd.miscFltFilterData), &fd.miscFltFilterData.inUse);

  connect(ui.filterList, SIGNAL(currentRowChanged(int)),
	  this, SLOT(pageSelectionChanged(int)));

  connect(ui.filterList, SIGNAL(itemClicked(QListWidgetItem *)),
	  this, SLOT(itemClickedX(QListWidgetItem*)));

  connect(ui.helpButton, SIGNAL(clicked()), this, SLOT(helpX()));
  connect(ui.resetButton, SIGNAL(clicked()), this, SLOT(resetX()));


  ui.buttonBox->button(QDialogButtonBox::Ok)->setIcon(QIcon(":images/ok"));
  ui.buttonBox->button(QDialogButtonBox::Cancel)->setIcon(QIcon(":images/cancel"));

  ui.filterList->setCurrentRow(lastPage);

  // So that it occupies minimum space.
  this->resize(100, 100);
}


//------------------------------------------------------------------------
void FilterDialog::addFilterPage(const QString &name, FilterWidget *fw, bool*use)
{
  QListWidgetItem *it = new QListWidgetItem(name);
  it->setCheckState(*use? Qt::Checked:Qt::Unchecked);
  fw->setEnabled(*use);
  ui.filterList->addItem(it);
  widgetStack->addWidget(fw);
  pages    << fw;
  usePages << use;
}

//------------------------------------------------------------------------
void FilterDialog::itemClickedX(QListWidgetItem *it)
{
  int row = ui.filterList->row(it);
  bool b = (it->checkState() == Qt::Checked);
  pages[row]->setEnabled(b);
  pages[row]->checkChecks();
}
//------------------------------------------------------------------------
void FilterDialog::pageSelectionChanged(int i)
{
  widgetStack->setCurrentWidget(pages[i]);
}

//------------------------------------------------------------------------
void FilterDialog::resetX()
{
  int ret = QMessageBox::warning
    (this, QString(appName),
     tr("Are you sure you want to reset all filter options to default values?"),
     QMessageBox::Yes | QMessageBox::No);

  if (ret == QMessageBox::Yes) {
    fd.defaultAll();
    for (int i=0; i<pages.size(); i++) {
      pages[i]->setWidgetValues();
      pages[i]->setEnabled(*(usePages[i]));
      pages[i]->checkChecks();
      ui.filterList->item(i)->setCheckState(*(usePages[i]) ? Qt::Checked: Qt::Unchecked);
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
    for (int i=0; i<pages.size(); i++) {
      pages[i]->getWidgetValues();
      *(usePages[i]) = ui.filterList->item(i)->checkState() == Qt::Checked;
    }
  }
  lastPage = ui.filterList->currentRow();
}
