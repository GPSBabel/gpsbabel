// -*- C++ -*-
// $Id: aboutdlg.cpp,v 1.2 2010-01-17 21:57:00 robertl Exp $
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

#include "aboutdlg.h"
#include "appname.h"
#include "upgrade.h"

AboutDlg::AboutDlg(QWidget *parent, const QString &ver1,
		   const QString &ver2): QDialog(parent)
{
  ui_.setupUi(this);
  QTextDocument *doc = ui_.textEdit->document();
  ui_.textEdit->setReadOnly(true);
  QString tt = doc->toHtml();
  tt.replace(QRegExp("\\$appname\\$"),  QString(appName));
  tt.replace(QRegExp("\\$babelversion\\$"),  ver1);
  tt.replace(QRegExp("\\$babelfeversion\\$"),  ver2);

  // Not localized as it should never be seen.
  tt.replace(QRegExp("\\$upgradetestmode\\$"),  
    UpgradeCheck::isTestMode() ? "**Upgrade test mode**" : "");
      
  doc->setHtml(tt);
  QTextCursor cur(doc);
  cur.setPosition(0);
  ui_.textEdit->setTextCursor(cur);
  ui_.textEdit->ensureCursorVisible();
}
