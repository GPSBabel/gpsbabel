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