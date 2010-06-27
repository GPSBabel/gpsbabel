// $Id: serial_win.cpp,v 1.3 2010-06-21 02:35:06 robertl Exp $
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

#if 0  // Does not require Windows 2000

static const char *deviceNames[] = {
  "com1:",
  "com2:",
  "com3:",
  "com4:",
  0
};

void MainWindow::osLoadDeviceNameCombos(QComboBox *box)
{
  for (int i=0; deviceNames[i]; i++) {
    box->addItem(deviceNames[i]);
  }
}

#else // This code assumes Windows 2000 or later

// Uses QueryDosDevice(), Minimum supported: Windows 2000 Professional/Server
#include <Windows.h> 
#include <stdio.h>

void MainWindow::osLoadDeviceNameCombos(QComboBox *box)
{
  char DevList[64*1024-1];  // a single byte more, and certain versions of windows
                            // always return QueryDosDevice()==0 && GetLastError()==ERROR_MORE_DATA.
                            // see http://support.microsoft.com/kb/931305
  // Get a list of all existing MS-DOS device names. Stores one or more asciiz strings followed by an extra null.
  DWORD res = QueryDosDeviceA(NULL, DevList, sizeof(DevList));
  if (res == 0)
  {
    DWORD err = GetLastError(); // could check for ERROR_INSUFFICIENT_BUFFER, and retry with a larger buffer.
                                // but DevList is already at the maximum size it can be without running into kb 931305.
    // FIXME: This shold be a QMessageBox::warning() - RJL
    // fprintf(stderr,"QueryDosDevice() failed with %d.  GetLastError()==%d.\n", res, err);
    (void) err;
    return;
  }

  for (char *p=DevList; *p;) {
    int len = strlen(p);
    if (strncmp(p,"COM",3)==0)
      box->addItem((PCHAR)p);
    p += len+1; // +1 to also skip the null character of each string
  }
}

#endif
