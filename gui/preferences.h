#ifndef PREFERENCES_H_
#define PREFERENCES_H_

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
//  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301,
//  USA.

#include "ui_preferences.h"

#include "babeldata.h"
#include "format.h"

class Preferences : public QDialog
{
  Q_OBJECT

public:
  Preferences(QWidget* parent, QList<Format>& formatList, BabelData& bd);

private:
  QList<Format>& formatList_;
  Ui_Preferences ui_;
  BabelData& babelData_;

private slots:
  void enableAllClicked();
  void disableAllClicked();
  void acceptClicked();
  void rejectClicked();
};

#endif //  PREFERENCES_H_
