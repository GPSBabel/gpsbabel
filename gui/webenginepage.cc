//
//  Copyright (C) 2025  Robert Lipe
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 2 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//  General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
// -*- C++ -*-

#include "webenginepage.h"
#include <QDebug>
#include <QDesktopServices>

WebEnginePageWithLogging::WebEnginePageWithLogging(QObject* parent) : QWebEnginePage(parent)
{
}

void
WebEnginePageWithLogging::setLogSink(QPlainTextEdit* textEdit)
{
  textEdit_ = textEdit;
}


void
WebEnginePageWithLogging::javaScriptConsoleMessage(JavaScriptConsoleMessageLevel level,
    const QString &message,
    int lineNumber,
    const QString &sourceID)
{
  QString logMessage = QString("JS Console(%1:%2): %3").arg(sourceID).arg(lineNumber).arg(message);
  qDebug() << logMessage;
  if (textEdit_) {
    textEdit_->appendPlainText(logMessage);
  }
}

bool
WebEnginePageWithLogging::acceptNavigationRequest(const QUrl &url, NavigationType type, bool isMainFrame)
{
  // Only interfere with link clicks in the main frame
  if (type == QWebEnginePage::NavigationTypeLinkClicked && isMainFrame) {
    // Open external web links in the user's default browser
    if (url.scheme() == "http" || url.scheme() == "https") {
      QDesktopServices::openUrl(url);
      return false; // Prevent navigation inside the QWebEngineView
    }
  }
  // Allow all other navigation requests (e.g., loading qrc resources)
  return QWebEnginePage::acceptNavigationRequest(url, type, isMainFrame);
}