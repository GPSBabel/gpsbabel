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

#ifdef _WIN32
#include <cstddef>                 // for size_t
#include <string>                  // for c_str
#include <stdlib.h>                // for _wgetenv_s, _wputenv_s
#endif

#include <QtCore/QByteArray>       // for QByteArray
#include <QtCore/QDir>             // for QDir
#include <QtCore/QFile>            // for QFile
#include <QtCore/QString>          // for QString
#include <QtCore/QtGlobal>         // for qgetenv, qputenv, QT_VERSION, QT_VERSION_CHECK
#include <QtGui/QIcon>             // for QIcon
#include <QtWidgets/QApplication>  // for QApplication

#include "mainwindow.h"             // for MainWindow

//------------------------------------------------------------------------

// Put the directory where this executable is on the front of the path.
// Who could imagine that this is so tricky!
#ifdef _WIN32
static void set_path()
{
  QString newPath = QDir::toNativeSeparators(QApplication::applicationDirPath());
  newPath += ';';

  // On windows qgetenv may produce data loss.
#if 0 // TODO: Qt >= 5.10
  newPath += qEnvironmentVariable("PATH");
#else
  size_t pathSize;
  _wgetenv_s(&pathSize, nullptr, 0, L"PATH");
  auto oldPath = new wchar_t[pathSize];
  _wgetenv_s(&pathSize, oldPath, pathSize, L"PATH");
  newPath += QString::fromStdWString(oldPath).chopped(1);
  delete[] oldPath;
#endif

  // On windows qputenv has similar, but undocumented, data loss issues.
  _wputenv_s(L"PATH", newPath.toStdWString().c_str());
}
#else // macos, linux
static void set_path()
{
  // QFile::encodeName handles encoding weirdness,
  // normalized utf8 for macos vs. local8bit for linux.
  QByteArray newPath = QFile::encodeName(QDir::toNativeSeparators(QApplication::applicationDirPath()));
  // we assume we don't have to normalize around the separator.
  newPath += ':';
  newPath += qgetenv("PATH");
  qputenv("PATH", newPath);
}
#endif // _WIN32


//------------------------------------------------------------------------
int main(int argc, char** argv)
{
// MIN_QT_VERSION in configure.ac should correspond to the QT_VERSION_CHECK arguments in main.cc and gui/main.cc
#if (QT_VERSION < QT_VERSION_CHECK(5, 9, 0))
#error this version of Qt is not supported.
#endif

  QApplication app(argc, argv);
  QApplication::setWindowIcon(QIcon(":/images/appicon.png"));

  set_path();

  QApplication::setOrganizationName("GPSBabel");
  QApplication::setOrganizationDomain("gpsbabel.org");
  QApplication::setApplicationName("GPSBabel");

  MainWindow mainWindow(nullptr);
  mainWindow.show();
  QApplication::exec();

  return 0;
}
