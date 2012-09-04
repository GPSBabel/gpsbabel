// -*- C++ -*-
// $Id: upgrade.cpp,v 1.26 2010-06-19 23:59:06 robertl Exp $
/*
    Copyright (C) 2009, 2010  Robert Lipe, robertlipe@gpsbabel.org

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


#include "babeldata.h"
#include "format.h"
#include "upgrade.h"
#include "../gbversion.h"
#if HAVE_CONFIG_H
#include "../config.h"
#endif

#if HAVE_UNAME
#include <sys/utsname.h>
#endif // HAVE_UNAME

#include <QHttp>
#include <QMessageBox>
#include <QDesktopServices>
#include <QDomDocument>
#include <QLocale>
#include <QSysInfo>
#include <QUrl>
#include <stdio.h>

#if 0
static const bool testing = true;
#else
static const bool testing = false;
#endif

UpgradeCheck::UpgradeCheck(QWidget *parent, QList<Format> &formatList,
                           BabelData& bd) :
  QObject(parent),
  http(0), 
  formatList_(formatList), 
  updateStatus_(updateUnknown),
  bd_(bd)
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

bool UpgradeCheck::isTestMode()
{
  return testing;
}

QString UpgradeCheck::getOsName(void)
{
  // Do not translate these strings.
#if defined (Q_OS_LINUX)
    return "Linux";
#elif defined (Q_OS_MAC)
  return "Mac";
#elif defined (Q_OS_WIN)
  return "Windows";
#else
  return "Unknown";
#endif

}
// See http://doc.trolltech.com/4.5/qsysinfo.html to interpret results
QString UpgradeCheck::getOsVersion()
{
#if defined (Q_OS_MAC)
  switch (QSysInfo::MacintoshVersion) {
  case QSysInfo::MV_10_3: return "10.3"; break;
  case QSysInfo::MV_10_4: return "10.4"; break;
  case QSysInfo::MV_10_5: return "10.5"; break;
  case QSysInfo::MV_10_6: return "10.6"; break;
  case QSysInfo::MV_10_7: return "10.7"; break;
  //case QSysInfo::MV_10_8: return "10.8"; break;
  default: 
    if (QSysInfo::MacintoshVersion == 10) {
      return "10.8";
      break;
    }
    return QString("Unknown Mac %1").arg(QSysInfo::MacintoshVersion);
  };
#elif defined (Q_OS_WIN)

  switch (QSysInfo::WindowsVersion) {
  // Wildly improbable...
  case QSysInfo::WV_95: return "95"; break;
  case QSysInfo::WV_98: return "98"; break;
  case QSysInfo::WV_Me: return "Me"; break;

  case QSysInfo::WV_4_0: return "NT 4"; break;
  case QSysInfo::WV_5_0: return "2000"; break;
  case QSysInfo::WV_5_1: return "XP"; break;
  case QSysInfo::WV_5_2: return "2003"; break;
  case QSysInfo::WV_6_0: return "Vista"; break;
  case QSysInfo::WV_6_1: return "7"; break;
  default:
      return "Windows/Unknown";
  }
#endif
  // FIXME: find something appropriately clever to do for Linux, etc. here.
  return "Unknown";
}


UpgradeCheck::updateStatus UpgradeCheck::checkForUpgrade(
               const QString &currentVersionIn,
               const QDateTime &lastCheckTime,
               bool allowBeta)
{
  currentVersion = currentVersionIn;
  currentVersion.remove("GPSBabel Version ");

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

  const QString host("www.gpsbabel.org" );
  header.setValue("Host",  host);

  header.setContentType("application/x-www-form-urlencoded");
  QLocale locale;

  QString args = "current_version=" + currentVersion;
  args += "&current_gui_version=" VERSION;
  args += "&installation=" + bd_.installationUuid;
  args += "&os=" + getOsName();
#if HAVE_UNAME
  struct utsname utsname;
  if (0 == uname(&utsname)) {
    args += "&cpu=" + QString(utsname.machine);
  }
#endif

  args += "&os_ver=" + getOsVersion();
  args += QString("&beta_ok=%1").arg(allowBeta); 
  args += "&lang=" + QLocale::languageToString(locale.language());
  args += "&last_checkin=" + lastCheckTime.toString(Qt::ISODate);
  args += QString("&ugcb=%1").arg(bd_.upgradeCallbacks); 
  args += QString("&ugdec=%1").arg(bd_.upgradeDeclines); 
  args += QString("&ugacc=%1").arg(bd_.upgradeAccept); 
  args += QString("&ugoff=%1").arg(bd_.upgradeOffers); 
  args += QString("&ugerr=%1").arg(bd_.upgradeErrors); 
  args += QString("&rc=%1").arg(bd_.runCount); 

  int j = 0;

  for (int i = 0; i < formatList_.size(); i++) {
    int rc = formatList_[i].getReadUseCount();
    int wc = formatList_[i].getWriteUseCount();
    QString formatName = formatList_[i].getName();
    if (rc)
      args += QString("&uc%1=rd/%2/%3").arg(j++).arg(formatName).arg(rc);
    if (wc)
      args += QString("&uc%1=wr/%2/%3").arg(j++).arg(formatName).arg(wc);
  }
  if (j && bd_.reportStatistics)
    args += QString("&uc=%1").arg(j);

  if (false && testing)
   fprintf(stderr, "Posting %s\n", qPrintable(args));

  http->setHost(host, 80);
  httpRequestId = http->request(header, args.toUtf8());

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

  if (http == 0 || error) {
    bd_.upgradeErrors++;
    return;
  }

  // This is not an error state; it's just the internal state of Qt's network
  // stack flailing around.
  if (requestId != httpRequestId) {
    return;
  }

  bd_.upgradeCallbacks++;
  QString oresponse(http->readAll());

  QDomDocument document;
  int line = -1;
  QString error_text;
  // This shouldn't ever be seen by a user.  
  if (!document.setContent(oresponse, &error_text, &line)) {
    QMessageBox::critical(0, tr("Error"),
           tr("Invalid return data at line %1: %2.")
           .arg(line)
	   .arg( error_text));
    bd_.upgradeErrors++;
    return;
  }

  QString response;
  QString upgradeText;

  if (testing)
    currentVersion =  "1.3.1"; // for testing

  bool allowBeta = true;  // TODO: come from prefs or current version...

  QDomNodeList upgrades = document.elementsByTagName("update");
  QUrl downloadUrl;
  updateStatus_ = updateCurrent;  // Current until proven guilty.

  for (unsigned int i = 0; i < upgrades.length(); i++) {
    QDomNode upgradeNode = upgrades.item(i);
    QDomElement upgrade = upgradeNode.toElement();

    QString updateVersion = upgrade.attribute("version");
    if (upgrade.attribute("downloadURL").isEmpty()) {
      downloadUrl = "http://www.gpsbabel.org/download.html";
    } else {
      downloadUrl = upgrade.attribute("downloadURL");
    }
    bool updateIsBeta  = upgrade.attribute("type") == "beta";
    bool updateIsMajor = upgrade.attribute("type") == "major";
    bool updateIsMinor = upgrade.attribute("type") == "minor";

    bool updateCandidate = updateIsMajor || updateIsMinor || (updateIsBeta && allowBeta);
    upgradeText = upgrade.firstChildElement("overview").text();
    // String compare, not a numeric one.  Server will return "best first".
    if((updateVersion > currentVersion) && updateCandidate) {
      bd_.upgradeOffers++;
      updateStatus_ = updateNeeded;
      response = tr("A new version of GPSBabel is available.<br />"
        "Your version is %1 <br />"
        "The latest version is %2")
          .arg(currentVersion)
          .arg(updateVersion);
      break;
    }
  }

  if (response.length()) {
    QMessageBox information;
    information.setWindowTitle(tr("Upgrade"));

    information.setStandardButtons(QMessageBox::Yes | QMessageBox::No);
    information.setDefaultButton(QMessageBox::Yes);
    information.setText(response);
    
    information.setInformativeText(tr("Do you wish to download an upgrade?"));
    information.setDetailedText(upgradeText);

    switch (information.exec()) {
      case QMessageBox::Yes:
        // downloadUrl.addQueryItem("os", getOsName());
        QDesktopServices::openUrl(downloadUrl);
        bd_.upgradeAccept++;
        break;
      default: ;
        bd_.upgradeDeclines++;
    }
  }

  upgradeWarningTime = QDateTime(QDateTime::currentDateTime());

  for (int i = 0; i < formatList_.size(); i++) {
     formatList_[i].zeroUseCounts();
  }
}
