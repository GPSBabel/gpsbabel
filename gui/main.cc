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
//  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111
//  USA
//
//------------------------------------------------------------------------
#define _CRT_SECURE_NO_DEPRECATE 1
#include <QMessageBox>
#include <QFile>
#include <QCoreApplication>
#include <QLibraryInfo>
#include <QIcon>
#include <QTextCodec>

#include "mainwindow.h"
#include "gmapdlg.h"

#ifdef _WIN32
const char *pathSeparator = ";";
#else
const char *pathSeparator = ":";
#endif

#if defined (Q_OS_MAC)
#include <CoreFoundation/CoreFoundation.h>
#endif

//------------------------------------------------------------------------
int main(int argc, char**argv)
{
#if (QT_VERSION < QT_VERSION_CHECK(5, 2, 0))
  #error this version of Qt is not supported.
#endif

  QApplication *app;
  app = new QApplication(argc, argv);
  app->setWindowIcon(QIcon(":/images/appicon.png"));

  QString newPath = "PATH=" + QApplication::applicationDirPath() +
    QString(pathSeparator) + getenv("PATH");
  char *newPathEnv = new char[newPath.length() + 1];
  strcpy(newPathEnv, newPath.toStdString().c_str());
  putenv(newPathEnv);

  QCoreApplication::setOrganizationName("GPSBabel");
  QCoreApplication::setOrganizationDomain("gpsbabel.org");
  QCoreApplication::setApplicationName("GPSBabel");

  MainWindow mainWindow(0);
  mainWindow.show();
  app->exec();

  return 0;
}
