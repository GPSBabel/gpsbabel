// -*- C++ -*-
#ifndef WEBENGINEPAGE_H
#define WEBENGINEPAGE_H

#include <QWebEnginePage>
#include <QPlainTextEdit>

class WebEnginePageWithLogging : public QWebEnginePage
{
  Q_OBJECT
public:
  WebEnginePageWithLogging(QObject* parent = nullptr);
  void setLogSink(QPlainTextEdit* textEdit);

protected:
  bool acceptNavigationRequest(const QUrl &url, NavigationType type, bool isMainFrame) override;
  void javaScriptConsoleMessage(JavaScriptConsoleMessageLevel level, const QString &message, int lineNumber, const QString &sourceID) override;
private:
  QPlainTextEdit* textEdit_{nullptr};
};

#endif // WEBENGINEPAGE_H
