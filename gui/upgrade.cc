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

#include <stdio.h>
#if HAVE_UNAME
#include <sys/utsname.h>
#endif // HAVE_UNAME

#include <QDebug>
#include <QDesktopServices>
#include <QDomDocument>
#include <QLocale>
#include <QMessageBox>
#include <QNetworkAccessManager>
#include <QNetworkReply>
#include <QNetworkRequest>
#include <QSysInfo>
#include <QUrl>
#include <QVariant>


#if 0
static const bool testing = true;
#else
static const bool testing = false;
#endif

UpgradeCheck::UpgradeCheck(QWidget *parent, QList<Format> &formatList,
                           BabelData& bd) :
  QObject(parent),
  manager_(0), 
  replyId_(0),
  upgradeUrl_(QUrl("http://www.gpsbabel.org/upgrade_check.html")),
  formatList_(formatList), 
  updateStatus_(updateUnknown),
  babelData_(bd)
{
}

UpgradeCheck::~UpgradeCheck()
{
  if (replyId_) {
    replyId_->abort();
    replyId_ = 0;
  }
  if (manager_) {
    delete manager_;
    manager_ = 0;
  }
}

bool UpgradeCheck::isTestMode()
{
  return testing;
}

QString UpgradeCheck::getOsName()
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
  case QSysInfo::MV_10_6: return "10.6"; break;  // Snow Leopard.
  case QSysInfo::MV_10_7: return "10.7"; break;  // Lion.
  case QSysInfo::MV_10_8: return "10.8"; break;  // Mountain Lion
  case QSysInfo::MV_10_9: return "10.9"; break;  // Mavericks
  case QSysInfo::MV_10_10: return "10.10"; break;  // Yosemite
  case QSysInfo::MV_10_11: return "10.11"; break;  // El Capitan
  case QSysInfo::MV_10_12: return "10.12"; break;  // Sierra
  default:
    // This probably doesn't work...
    if (QSysInfo::MacintoshVersion == 0x000E) {
      return "10.13";
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
  case QSysInfo::WV_6_2: return "8"; break;
  case QSysInfo::WV_6_3: return "8.1"; break;
//  case QSysInfo::WV_10_0: return "10"; break;
  default:
       if (QSysInfo::WindowsVersion == 0x00a0) return "8";
       if (QSysInfo::WindowsVersion == 0x00b0) return "8.1";
       if (QSysInfo::WindowsVersion == 0x00c0) return "10.0";
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
  currentVersion_ = currentVersionIn;
  currentVersion_.remove("GPSBabel Version ");

  QDateTime soonestCheckTime = lastCheckTime.addDays(1);
  if (!testing && QDateTime::currentDateTime() < soonestCheckTime) {
    // Not time to check yet.
    return UpgradeCheck::updateUnknown;
  }

  manager_ = new QNetworkAccessManager;

  connect(manager_, SIGNAL(finished(QNetworkReply*)),
          this, SLOT(httpRequestFinished(QNetworkReply*)));

  QNetworkRequest request = QNetworkRequest(upgradeUrl_);
  request.setHeader(QNetworkRequest::ContentTypeHeader, "application/x-www-form-urlencoded");
  request.setRawHeader("Accept-Encoding","identity");

  QLocale locale;

  QString args = "current_version=" + currentVersion_;
  args += "&current_gui_version=" VERSION;
  args += "&installation=" + babelData_.installationUuid_;
  args += "&os=" + getOsName();
#if HAVE_UNAME || defined (Q_OS_MAC)
  struct utsname utsname;
  if (0 == uname(&utsname)) {
    args += "&cpu=" + QString(utsname.machine);
  }
#endif

  args += "&os_ver=" + getOsVersion();
  args += QString("&beta_ok=%1").arg(allowBeta); 
  args += "&lang=" + QLocale::languageToString(locale.language());
  args += "&last_checkin=" + lastCheckTime.toString(Qt::ISODate);
  args += QString("&ugcb=%1").arg(babelData_.upgradeCallbacks_); 
  args += QString("&ugdec=%1").arg(babelData_.upgradeDeclines_); 
  args += QString("&ugacc=%1").arg(babelData_.upgradeAccept_); 
  args += QString("&ugoff=%1").arg(babelData_.upgradeOffers_); 
  args += QString("&ugerr=%1").arg(babelData_.upgradeErrors_); 
  args += QString("&rc=%1").arg(babelData_.runCount_); 

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
  if (j && babelData_.reportStatistics_)
    args += QString("&uc=%1").arg(j);

  if (false && testing) {
    qDebug() << "Posting " << args;
  }

  replyId_ = manager_->post(request, args.toUtf8());

  return UpgradeCheck::updateUnknown;
}

QDateTime UpgradeCheck::getUpgradeWarningTime() {
  return upgradeWarningTime_;
}

UpgradeCheck::updateStatus UpgradeCheck::getStatus() {
  return updateStatus_;
}

void UpgradeCheck::httpRequestFinished(QNetworkReply* reply)
{

  if (reply == 0 ) {
    babelData_.upgradeErrors_++;
    return;
  } else if (reply != replyId_) {
    QMessageBox::information(0, tr("HTTP"),
           tr("Unexpected reply."));
  } else if (reply->error() != QNetworkReply::NoError ) {
    babelData_.upgradeErrors_++;
    QMessageBox::information(0, tr("HTTP"),
           tr("Download failed: %1.")
           .arg(reply->errorString()));
    replyId_ = 0;
    reply->deleteLater();
    return;
  }

  // New post 1.4.4: Allow redirects in case a proxy server or something
  // slightly rewrites the post.
  // Note that adding port 80 to the url will cause a redirect, which is useful for testing.
  // Also you use gpsbabel.org instead of www.gpsbabel.org to generate redirects.
  QVariant attributeValue = reply->attribute(QNetworkRequest::RedirectionTargetAttribute);
  if (!attributeValue.isNull() && attributeValue.isValid()) {
    QUrl redirectUrl = attributeValue.toUrl();
    if (redirectUrl.isValid()) {
      if (testing) {
        qDebug() << "redirect to " << redirectUrl.toString();
      }
      // Change the url for the next update check.
      // TOODO: kick off another update check.  
      upgradeUrl_ = redirectUrl;
      replyId_ = 0;
      reply->deleteLater();
      return;
    }
  }
  
  QVariant statusCode = reply->attribute(QNetworkRequest::HttpStatusCodeAttribute);
  if (testing) {
    qDebug() << "http status code " << statusCode.toInt();
  }
  if (statusCode != 200) {
    QVariant reason = reply->attribute(QNetworkRequest::HttpReasonPhraseAttribute);
    QMessageBox::information(0, tr("HTTP"),
           tr("Download failed: %1: %2.")
           .arg(statusCode.toInt())
           .arg(reason.toString()));
    replyId_ = 0;
    reply->deleteLater();
    return;
  }

  babelData_.upgradeCallbacks_++;
  QString oresponse(reply->readAll());

  QDomDocument document;
  int line = -1;
  QString error_text;
  // This shouldn't ever be seen by a user.  
  if (!document.setContent(oresponse, &error_text, &line)) {
    QMessageBox::critical(0, tr("Error"),
           tr("Invalid return data at line %1: %2.")
           .arg(line)
           .arg( error_text));
    babelData_.upgradeErrors_++;
    replyId_ = 0;
    reply->deleteLater();
    return;
  }

  QString response;
  QString upgradeText;

  if (testing)
    currentVersion_ =  "1.3.1"; // for testing

  bool allowBeta = true;  // TODO: come from prefs or current version...

  QDomNodeList upgrades = document.elementsByTagName("update");
  QUrl downloadUrl;
  updateStatus_ = updateCurrent;  // Current until proven guilty.

  for (int i = 0; i < upgrades.length(); i++) {
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
    if((updateVersion > currentVersion_) && updateCandidate) {
      babelData_.upgradeOffers_++;
      updateStatus_ = updateNeeded;
      response = tr("A new version of GPSBabel is available.<br />"
        "Your version is %1 <br />"
        "The latest version is %2")
          .arg(currentVersion_)
          .arg(updateVersion);
      break;
    }
  }

  if (response.length()) {
    QMessageBox information;
    information.setWindowTitle(tr("Upgrade"));

    information.setStandardButtons(QMessageBox::Yes | QMessageBox::No);
    information.setDefaultButton(QMessageBox::Yes);
    information.setTextFormat(Qt::RichText);
    information.setText(response);
    
    information.setInformativeText(tr("Do you wish to download an upgrade?"));
    // The text field can be RichText, but DetailedText can't be. Odd.
    information.setDetailedText(upgradeText);

    switch (information.exec()) {
      case QMessageBox::Yes:
        // downloadUrl.addQueryItem("os", getOsName());
        QDesktopServices::openUrl(downloadUrl);
        babelData_.upgradeAccept_++;
        break;
      default: ;
        babelData_.upgradeDeclines_++;
    }
  }

  upgradeWarningTime_ = QDateTime(QDateTime::currentDateTime());

  for (int i = 0; i < formatList_.size(); i++) {
     formatList_[i].zeroUseCounts();
  }
  replyId_ = 0;
  reply->deleteLater();
}
