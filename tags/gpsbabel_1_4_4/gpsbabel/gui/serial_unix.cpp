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
//  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111
//  USA
//
#include "mainwindow.h"
#if !defined (Q_OS_MAC) // FIXME: find a better way to hide this on Mac.

static const char *deviceNames[] = {
  "/dev/ttyS0",
  "/dev/ttyS1",
  "/dev/ttyS2",
  "/dev/ttyS3",
  "/dev/ttyUSB0",
  "/dev/rfcomm0",
  0
};

void MainWindow::osLoadDeviceNameCombos(QComboBox *box)
{
  for (int i=0; deviceNames[i]; i++) {
    box->addItem(deviceNames[i]);
  }
}
#endif
