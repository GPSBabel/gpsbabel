// -*- C++ -*-
// $Id: upgrade.cpp,v 1.13 2009-09-02 19:05:27 robertl Exp $
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
#include "../gbversion.h"

#include <QHttp>
#include <QMessageBox>
#include <QDesktopServices>
#include <QDomDocument>
#include <QLocale>
#include <QSysInfo>
#include <QUrl>

// static const bool testing = true;
static const bool testing = false;

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
  default: return QString("Unknown Mac %1").arg(QSysInfo::MacintoshVersion);
  };
  #elif defined (Q_OS_WIN)

  switch (QSysInfo::WindowsVersion) {
  // Wildly improbable...
  case QSysInfo::WV_95: return "95"; break;
  case QSysInfo::WV_98: return "98"; break;
  case QSysInfo::WV_Me: return "Me"; break;
#ifdef XXX
  case QSysInfo::WV_4_0: return "NT 4"; break;
  case QSysInfo::WV_5_0: return "2000"; break;
  case QSysInfo::WV_5_1: return "XP"; break;
  case QSysInfo::WV_5_2: return "2003"; break;
  case QSysInfo::WV_6_0: return "Vista"; break;
  case QSysInfo::WV_6_1: return "7"; break;
#endif
  default:;
  }
  return "Windows";
  #endif
  // FIXME: find something appropriately clever to do for Linux, etc. here.
  return "Unknown";
}


UpgradeCheck::updateStatus UpgradeCheck::checkForUpgrade(const QString &currentVersionIn,
               int checkMethod,
               const QDateTime &lastCheckTime,
               const QString &installationUuid)
{
  this->currentVersion = currentVersionIn;
  this->currentVersion.remove("GPSBabel Version ");
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
  header.setValue("Host", "www.gpsbabel.org");
  QLocale locale;

  QString args = "current_version=" + currentVersion;
  args += "&current_gui_version=" VERSION;
  args += "&installation=" + installationUuid;
  args += "&os=" + getOsName();
  args += "&os_ver=" + getOsVersion();
  args += "&beta_ok=1";   // Eventually to come from prefs.
  args += "&lang=" + QLocale::languageToString(locale.language());

  http->setHost("www.gpsbabel.org");
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
  if (http == 0 || error)
    return;

  if (requestId != httpRequestId)
    return;

  QString oresponse(http->readAll());

  QDomDocument document;
  if (!document.setContent(oresponse))
    return;

  QString response;
  QString upgradeText;

  if (testing)
    currentVersion =  "1.3.1"; // for testing

  bool allowBeta = false;  // TODO: come from prefs or current version...

  QDomNodeList upgrades = document.elementsByTagName("update");
  QUrl downloadUrl;
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
    bool updateCandidate = updateIsMajor || (updateIsBeta && allowBeta);
    upgradeText = upgrade.firstChildElement("overview").text();

    // String compare, not a numeric one.  Server will return "best first".
    if((updateVersion > currentVersion) && updateCandidate) {
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
      default: ;
    }

    upgradeWarningTime = QDateTime(QDateTime::currentDateTime());
  }
}
