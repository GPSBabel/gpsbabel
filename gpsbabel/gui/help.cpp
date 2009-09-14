// -*- C++ -*-
// $Id: help.cpp,v 1.7 2009-09-14 14:25:14 robertl Exp $
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
#include <QUrl>
#include <QWebView>
#include <QDesktopServices>

//------------------------------------------------------------------------
void ShowHelp(const char *name)
{
  QString urlname("file:///" + QApplication::applicationDirPath() +
		  "/help/" + name);
#ifdef XXXX
  // This has window modality problems.  Unless the problem is solved, just use
  // the native browser.
  QWebView *view = new QWebView();
  view->setWindowTitle("GPSBabel Help");
  view->load(urlname);
  view->show();
#else
  QDesktopServices::openUrl(QUrl(urlname));
#endif
}
