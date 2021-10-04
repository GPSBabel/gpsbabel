// -*- C++ -*-
// $Id: main.cpp,v 1.8 2010-06-06 00:49:08 robertl Exp $
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
//------------------------------------------------------------------------
#include <QtGlobal>                // for QT_VERSION, QT_VERSION_CHECK
#include <QIcon>                   // for QIcon
#include <QApplication>            // for QApplication

#include "mainwindow.h"             // for MainWindow

//------------------------------------------------------------------------
int main(int argc, char** argv)
{
// MIN_QT_VERSION in GPSBabel.pro should correspond to the QT_VERSION_CHECK
// arguments in main.cc and gui/main.cc and the version check in
// CMakeLists.txt, gui/CMakeLists.txt.
#if (QT_VERSION < QT_VERSION_CHECK(5, 12, 0))
#error this version of Qt is not supported.
#endif

  QApplication app(argc, argv);
  QApplication::setWindowIcon(QIcon(":/images/appicon.png"));
  QApplication::setOrganizationName("GPSBabel");
  QApplication::setOrganizationDomain("gpsbabel.org");
  QApplication::setApplicationName("GPSBabel");

  MainWindow mainWindow(nullptr);
  mainWindow.show();
  return QApplication::exec();
}
