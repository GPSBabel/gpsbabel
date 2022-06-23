// $Id: serial_unix.cpp,v 1.2 2010-02-13 23:25:23 robertl Exp $
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

#include <QComboBox>        // for QComboBox
#include <QList>            // for QList
#include <QSerialPortInfo>  // for QSerialPortInfo

#include "mainwindow.h"     // for MainWindow


void MainWindow::osLoadDeviceNameCombos(QComboBox* box)
{
  const auto ports = QSerialPortInfo::availablePorts();
  for (const auto& info : ports) {
    box->addItem(info.systemLocation());
  }
}
