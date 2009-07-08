// -*- C++ -*-
// $Id: help.cpp,v 1.1 2009-07-05 21:14:56 robertl Exp $
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
#include "help.h"
#include <QApplication>
#include <QString>
#include <QProcess>


//------------------------------------------------------------------------
#ifndef _WIN32
void ShowUrl(const QString &url) 
{
  QString progName = QApplication::applicationDirPath() + "/showUrl.sh";
  QStringList args;
  args << url;
  QProcess::startDetached(progName, args);
}
#else
#include <windows.h>
void ShowUrl(const QString &url) 
{
  ShellExecuteA(0, "open", url.toStdString().c_str(), 0, 0, SW_SHOWNORMAL);
}
#endif

//------------------------------------------------------------------------
void ShowHelp(const char *name)
{
  QString urlname = "file://" + QApplication::applicationDirPath() + "/help/" + name;
  ShowUrl(urlname);
}

