// -*- C++ -*-
// $Id: aboutdlg.cpp,v 1.1 2009-07-05 21:14:56 robertl Exp $
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

AboutDlg::AboutDlg(QWidget *parent, const QString &ver1,
		   const QString &ver2): QDialog(parent)
{
  ui.setupUi(this);
  QTextDocument *doc = ui.textEdit->document();
  ui.textEdit->setReadOnly(true);
  QString tt = doc->toHtml();
  tt.replace(QRegExp("\\$appname\\$"),  QString(appName));
  tt.replace(QRegExp("\\$babelversion\\$"),  ver1);
  tt.replace(QRegExp("\\$babelfeversion\\$"),  ver2);
  doc->setHtml(tt);
  QTextCursor cur(doc);
  cur.setPosition(0);
  ui.textEdit->setTextCursor(cur);
  ui.textEdit->ensureCursorVisible();
}
