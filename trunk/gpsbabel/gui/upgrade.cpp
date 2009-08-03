// -*- C++ -*-
// $Id: upgrade.cpp,v 1.3 2009-08-03 05:16:23 robertl Exp $
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

#include <QHttp>
//#include <QHttpRequestHeader>
#include <QMessageBox>
#include <QDomDocument>

static const bool testing = true;
// static const bool testing = false;

static int versionAsNumber(const QString &s) 
{
  QStringList list = s.split(".");
  return (list[0].toInt()<<16 | list[1].toInt() << 8 | list[2].toInt() );
}

UpgradeCheck::UpgradeCheck(QWidget *parent) :
  QObject(parent),
  http(0)
{
}

UpgradeCheck::~UpgradeCheck()
{
  if (http) {
    http->clearPendingRequests();
    http->abort();
    delete http;
    http = 0;
  }
}

void UpgradeCheck::changeEvent(QEvent *)
{
}

UpgradeCheck::updateStatus UpgradeCheck::checkForUpgrade(const QString &currentVersion,
							 int checkMethod,
							 const QDateTime &lastCheckTime,
							 const QString &installationUuid)
{
  this->currentVersion = currentVersion;
  this->upgradeCheckMethod = checkMethod;

  QDateTime soonestCheckTime = lastCheckTime.addDays(1);
  if (!testing && QDateTime::currentDateTime() < soonestCheckTime) {
    // Not time to check yet.
    return UpgradeCheck::updateUnknown;
  }
  http = new QHttp;
  
  connect(http, SIGNAL(requestFinished(int, bool)),
          this, SLOT(httpRequestFinished(int, bool)));
  connect(http, SIGNAL(responseHeaderReceived(const QHttpResponseHeader &)),
          this, SLOT(readResponseHeader(const QHttpResponseHeader &)));
  
  QHttpRequestHeader header("POST", "/upgrade_check.html");
	header.setValue("Host",  "www.gpsbabel.org");
	        header.setContentType("application/x-www-form-urlencoded");
 QString args = "current_version=" + currentVersion;
   args += "&installation=" + installationUuid;	header.setValue("Host", "www.gpsbabel.org");
	
  http->setHost("www.gpsbabel.org");
//	http->request(header);
	http->request(header, args.toUtf8());
//  httpRequestId = http->get("/upgrade_check.html");

  return UpgradeCheck::updateUnknown;
}

void UpgradeCheck::readResponseHeader(const QHttpResponseHeader &responseHeader)
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
    QMessageBox::information(0, tr("HTTP"),
			     tr("Download failed: %1.")
			     .arg(responseHeader.reasonPhrase()));
    httpRequestAborted = true;
    http->abort();
  }
}

void UpgradeCheck::httpRequestFinished(int requestId, bool error)
{
  if (http == 0 || error)
    return;

  if (requestId != httpRequestId) 
    return;

  QString oresponse(http->readAll());

  QDomDocument document;
  if (!document.setContent(oresponse)) 
    return;
  
  QString response;
  
  if (testing)
    currentVersion =  "1.3.1"; // for testing

  bool allowBeta = false;  // TODO: come from prefs or current version...

  int currentVersionNum = versionAsNumber(currentVersion);
  
  QDomNodeList upgrades = document.elementsByTagName("update");
  
  for (unsigned int i = 0; i < upgrades.length(); i++) {
    QDomNode upgradeNode = upgrades.item(i);
    QDomElement upgrade = upgradeNode.toElement();
    
    QString updateVersion = upgrade.attribute("version");
    bool updateIsBeta  = upgrade.attribute("type") == "beta";
    bool updateIsMajor = upgrade.attribute("type") == "major";
    bool updateCandidate = updateIsMajor || (updateIsBeta && allowBeta);
    int updateVersionNum = versionAsNumber(updateVersion);
    
    if(updateVersionNum > currentVersionNum && updateCandidate) {
      response = tr("<center><b>A new version of GPSBabel is available</b><br>"
		    "Your version is %1 <br>"
		    "The latest version is %2</center>")
	.arg(currentVersion)
	.arg(updateVersion);

      break;  
    }
  }
  if (response.length()) {
    QMessageBox::information(0, tr("Upgrade"), response);
    upgradeWarningTime = QDateTime(QDateTime::currentDateTime());
  }
  delete http;
  http = 0;
}
