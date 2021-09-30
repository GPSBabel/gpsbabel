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
//  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301,
//  USA.
//

#include "aboutdlg.h"
#include <QTextCursor>                // for QTextCursor
#include <QTextDocument>              // for QTextDocument
#include <QTextEdit>                  // for QTextEdit
#include "appname.h"                  // for appName
#include "upgrade.h"                  // for UpgradeCheck


AboutDlg::AboutDlg(QWidget* parent, const QString& ver1,
                   const QString& ver2, const QString& installationId): QDialog(parent)
{
  ui_.setupUi(this);
  QTextDocument* doc = ui_.textEdit->document();
  ui_.textEdit->setReadOnly(true);
  QString tt = doc->toHtml();
  tt.replace("$appname$", appName);
  tt.replace("$babelversion$", ver1);
  tt.replace("$babelfeversion$", ver2);
  tt.replace("$installationId$", installationId);

  // Not localized as it should never be seen.
  tt.replace("$upgradetestmode$",
             UpgradeCheck::isTestMode() ? "**Upgrade test mode**" : "");

  doc->setHtml(tt);
  QTextCursor cur(doc);
  cur.setPosition(0);
  ui_.textEdit->setTextCursor(cur);
  ui_.textEdit->ensureCursorVisible();
}
