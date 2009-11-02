// -*- C++ -*-
// $Id: help.cpp,v 1.8 2009-11-02 20:38:02 robertl Exp $
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
#include <QUrl>
#include <QDesktopServices>

#include "help.h"
#include "format.h"

//------------------------------------------------------------------------
void ShowHelp(const QString &urlIn)

{
  QString url = urlIn;
  if (!url.contains(QRegExp("^http://"))) {
    url = Format::getHtmlBase() + url;
  }
  QDesktopServices::openUrl(QUrl(url));
}



