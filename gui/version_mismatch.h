// -*- C++ -*-
//------------------------------------------------------------------------
//
//  Copyright (C) 2010  Robert Lipe <robertlipe@gpsbabel.org>
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

#ifndef VERSION_MISMATCH_H
#define VERSION_MISMATCH_H

#include <QDialog>

#include "ui_version_mismatch.h"

class VersionMismatch: public QDialog
{
public:
  VersionMismatch(QWidget* parent,  const QString& ver1,
                  const QString& ver2);
  bool neverAgain()
  {
    return ui_.neverAgain->isChecked();
  }

private:
  Ui_VersionMismatch  ui_;
};

#endif
