//
// Copyright (C) 2010  Robert Lipe  <robertlipe@gpsbabel.org>
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


#include "preferences.h"
#include "../gbversion.h"

class FormatListEntry : public QListWidgetItem {
 public:
  FormatListEntry(Format& fmt) /*: fmt_(fmt)*/  {
    setText(fmt.getDescription());
    bool enabled = !fmt.isHidden();
    setCheckState(enabled ? Qt::Checked : Qt::Unchecked);
  }

 private:
   //Format& fmt_;
};

Preferences::Preferences(QWidget* parent, QList<Format>& formatList,
                         BabelData& bd) : QDialog(parent),
  formatList_(formatList),
  babelData_(bd)
{
  ui_.setupUi(this);

  ui_.startupCheck->setChecked(babelData_.startupVersionCheck_);
  ui_.reportStatisticsCheck->setChecked(babelData_.reportStatistics_);
  ui_.ignoreVersionMismatchCheck->setChecked(babelData_.ignoreVersionMismatch_);
  // Because of an unfortunate bug in 1.4.0, we turn this off in 1.4.1.
  if (VERSION == QString("1.4.1"))
    babelData_.ignoreVersionMismatch_ = false;

  connect (ui_.buttonBox, SIGNAL(accepted()), this, SLOT(acceptClicked()));
  connect (ui_.buttonBox, SIGNAL(rejected()), this, SLOT(rejectClicked()));

  connect (ui_.enableAllButton, SIGNAL(clicked()), this, SLOT(enableAllClicked()));
  connect (ui_.disableAllButton, SIGNAL(clicked()), this, SLOT(disableAllClicked()));

  for (int i = 0; i < formatList_.size(); i++) {
    FormatListEntry *item = new FormatListEntry(formatList[i]);

    ui_.enabledFormatsList->addItem(item);
  }
}

void Preferences::enableAllClicked()
{
  for (int i = 0; i < ui_.enabledFormatsList->count(); i++) {
    QListWidgetItem* item = ui_.enabledFormatsList->item(i);
    item->setCheckState(Qt::Checked);
  }
}

void Preferences::disableAllClicked()
{
  for (int i = 0; i < ui_.enabledFormatsList->count(); i++) {
    QListWidgetItem* item = ui_.enabledFormatsList->item(i);
    item->setCheckState(Qt::Unchecked);
  }
}

void Preferences::acceptClicked()
{
  for (int i = 0; i < ui_.enabledFormatsList->count(); i++) {
    QListWidgetItem* item = ui_.enabledFormatsList->item(i);
    formatList_[i].setHidden(item->checkState() == Qt::Unchecked);
  }

  babelData_.startupVersionCheck_ = ui_.startupCheck->isChecked();
  babelData_.reportStatistics_ = ui_.reportStatisticsCheck->isChecked();
  babelData_.ignoreVersionMismatch_ = ui_.ignoreVersionMismatchCheck->isChecked();
  accept();
}

void Preferences::rejectClicked()
{
  reject();
}
