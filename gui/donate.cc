// -*- C++ -*-
//------------------------------------------------------------------------
//
//  Copyright (C) 2010  Robert Lipe <robertlipe@gpsbabel.org
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

#include "donate.h"
#include <QUrl>
#include <QDesktopServices>

// A completely simple QDialog, in a class of its own for layout.
Donate::Donate(QWidget* parent) : QDialog(parent)
{
  ui_.setupUi(this);
  connect(ui_.contributeButton, SIGNAL(clicked()), this, SLOT(contributeClicked()));
}

void Donate::contributeClicked()
{
  QDesktopServices::openUrl(QUrl("https://www.gpsbabel.org/contribute.html"));
  close();
}
