/*
    Copyright (C) 2009  Robert Lipe, robertlipe@gpsbabel.org

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111 USA

 */


#include "upgrade.h"
#include "mainwindow.h"
#include "ui_upgrade.h"

#include <QHttp>
#include <QUrl>
#include <QMessageBox>
#include <QFile>
#include <QDomDocument>
#include <QDomNode>
#include <QDomElement>
#include <QDomNodeList>

Upgrade::Upgrade(QWidget *parent) :
    QDialog(parent),
    m_ui(new Ui::Upgrade)
{
    m_ui->setupUi(this);
}

Upgrade::~Upgrade()
{
    delete m_ui;
}

void Upgrade::changeEvent(QEvent *e)
{
    QDialog::changeEvent(e);
    switch (e->type()) {
    case QEvent::LanguageChange:
        m_ui->retranslateUi(this);
        break;
    default:
        break;
    }
}

Upgrade::updateStatus Upgrade::checkForUpgrade()
{
  http = new QHttp;

  connect(http, SIGNAL(requestFinished(int, bool)),
          this, SLOT(httpRequestFinished(int, bool)));
  connect(http, SIGNAL(responseHeaderReceived(const QHttpResponseHeader &)),
          this, SLOT(readResponseHeader(const QHttpResponseHeader &)));

  http->setHost("www.gpsbabel.org");
  httpRequestId = http->get("/updates.xml");

  return Upgrade::updateUnknown;
}

void Upgrade::readResponseHeader(const QHttpResponseHeader &responseHeader)
{
  switch (responseHeader.statusCode()) {
  case 200:                   // Ok
  case 301:                   // Moved Permanently
  case 302:                   // Found
  case 303:                   // See Other
  case 307:                   // Temporary Redirect
      // these are not error conditions
      break;

  default:
      QMessageBox::information(this, tr("HTTP"),
                               tr("Download failed: %1.")
                               .arg(responseHeader.reasonPhrase()));
      httpRequestAborted = true;
      http->abort();
    }
}

void Upgrade::httpRequestFinished(int requestId, bool error)
{
  if(error or requestId != httpRequestId) {
    return;
  }

  QString oresponse(http->readAll());
  QDomDocument document;
  if (!document.setContent(oresponse)) {
    return;
  }
  
  QString response("snore");

  QString currentVersion =  "1.3.6"; // FIXME: Work with Khai to pry this out of MainWindow 
  bool allowBeta = false;  // TODO: come from prefs or current version...

  QDomNodeList upgrades = document.elementsByTagName("update");

  for (unsigned int i = 0; i < upgrades.length(); i++) {
    QDomNode upgradeNode = upgrades.item(i);
    QDomElement upgrade = upgradeNode.toElement();

    QString updateVersion = upgrade.attribute("version");
    bool updateIsBeta  = upgrade.attribute("type") == "beta";
    bool updateIsMajor = upgrade.attribute("type") == "major";
    bool updateCandidate = updateIsMajor || (updateIsBeta && allowBeta);

    if(updateVersion > currentVersion && updateCandidate) {
      response = "<b>A new version of GPSBabel is available</b><br/ >";
      response += "Your version is " + currentVersion + " <br />";;
      response += "The latest version is " + updateVersion + " <br />";;
      break;  
    }
  }

  // FIXME: this doesn't actually write into the UI's text browser...
  m_ui->textBrowser->setText(response);

  //QMessageBox::information(this, tr("Finished"), response);

}
