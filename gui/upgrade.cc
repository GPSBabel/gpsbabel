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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

 */

#include "upgrade.h"

#include <QByteArray>             // for QByteArray
#include <QDebug>                 // for QDebug
#include <QDesktopServices>       // for QDesktopServices
#include <QDomDocument>           // for QDomDocument
#include <QDomElement>            // for QDomElement
#include <QDomNode>               // for QDomNode
#include <QDomNodeList>           // for QDomNodeList
#include <QLocale>                // for QLocale
#include <QMessageBox>            // for QMessageBox, operator|
#include <QNetworkAccessManager>  // for QNetworkAccessManager
#include <QNetworkReply>          // for QNetworkReply
#include <QNetworkRequest>        // for QNetworkRequest
#include <QSysInfo>               // for QSysInfo
#include <QUrl>                   // for QUrl
#include <QVariant>               // for QVariant, operator!=
#include <QVersionNumber>         // for QVersionNumber, operator<, operator==
#include <Qt>                     // for DateFormat, TextFormat
#include <QtGlobal>               // for qDebug, QT_VERSION, QT_VERSION_CHECK, qsizetype

#include "babeldata.h"            // for BabelData
#include "format.h"               // for Format
#include "gbversion.h"            // for VERSION


#if 0
static const bool testing = true;
#else
static const bool testing = false;
#endif

UpgradeCheck::UpgradeCheck(QWidget* parent, QList<Format>& formatList,
                           BabelData& bd) :
  QObject(parent),
  formatList_(formatList),
  babelData_(bd)
{
}

bool UpgradeCheck::isTestMode()
{
  return testing;
}

// Since Qt 5.4 QSysInfo makes it easy to get the OsName,
// OsVersion and CpuArchitecture.
QString UpgradeCheck::getOsName()
{
  return QSysInfo::productType();
}

QString UpgradeCheck::getOsVersion()
{
  return QSysInfo::productVersion();
}

QString UpgradeCheck::getCpuArchitecture()
{
  return QSysInfo::currentCpuArchitecture();
}

UpgradeCheck::updateStatus UpgradeCheck::checkForUpgrade(
  const QString& currentVersion,
  const QDateTime& lastCheckTime,
  bool allowBeta)
{
  currentVersion_ = currentVersion;

  QDateTime soonestCheckTime = lastCheckTime.addDays(1);
  if (!testing && QDateTime::currentDateTime() < soonestCheckTime) {
    // Not time to check yet.
    return updateUnknown;
  }

  manager_ = new QNetworkAccessManager(this);

  connect(manager_, &QNetworkAccessManager::finished,
          this, &UpgradeCheck::httpRequestFinished);

  QNetworkRequest request = QNetworkRequest(upgradeUrl_);

  // In Qt 5.6 and later, it can reissue with a redirect. With this in
  // place, we don't see the 301 redirect, but the server has to issue
  // one for the thousands of older clients out there.
  request.setAttribute(QNetworkRequest::RedirectPolicyAttribute, QNetworkRequest::NoLessSafeRedirectPolicy);
  request.setHeader(QNetworkRequest::ContentTypeHeader, "application/x-www-form-urlencoded");
  request.setRawHeader("Accept-Encoding","identity");

  QLocale locale;

  QString args = "current_version=" + currentVersion_;
  args += "&current_gui_version=" VERSION;
  args += "&installation=" + babelData_.installationUuid_;
  args += "&os=" + getOsName();
  args += "&cpu=" + getCpuArchitecture();
  args += "&os_ver=" + getOsVersion();
  args += QStringLiteral("&beta_ok=%1").arg(static_cast<int>(allowBeta));
  args += "&lang=" + QLocale::languageToString(locale.language());
  args += "&last_checkin=" + lastCheckTime.toString(Qt::ISODate);
  args += QStringLiteral("&ugcb=%1").arg(babelData_.upgradeCallbacks_);
  args += QStringLiteral("&ugdec=%1").arg(babelData_.upgradeDeclines_);
  args += QStringLiteral("&ugacc=%1").arg(babelData_.upgradeAccept_);
  args += QStringLiteral("&ugoff=%1").arg(babelData_.upgradeOffers_);
  args += QStringLiteral("&ugerr=%1").arg(babelData_.upgradeErrors_);
  args += QStringLiteral("&rc=%1").arg(babelData_.runCount_);

  if (babelData_.reportStatistics_) {
    int j = 0;

    for (int i = 0; i < formatList_.size(); i++) {
      int rc = formatList_[i].getReadUseCount();
      int wc = formatList_[i].getWriteUseCount();
      QString formatName = formatList_[i].getName();
      if (rc != 0) {
        args += QStringLiteral("&uc%1=rd/%2/%3").arg(j++).arg(formatName).arg(rc);
      }
      if (wc != 0) {
        args += QStringLiteral("&uc%1=wr/%2/%3").arg(j++).arg(formatName).arg(wc);
      }
    }
    if (j != 0) {
      args += QStringLiteral("&uc=%1").arg(j);
    }
  }

  if (false && testing) {
    qDebug() << "Posting " << args;
  }

  replyId_ = manager_->post(request, args.toUtf8());

  return updateUnknown;
}

QDateTime UpgradeCheck::getUpgradeWarningTime()
{
  return upgradeWarningTime_;
}

UpgradeCheck::updateStatus UpgradeCheck::getStatus()
{
  return updateStatus_;
}

// GPSBabel version numbers throughout the code mostly predate QVersionNumber
// and are stored as strings. They may be of the form "1.6.0-beta20200413"
// which, if sorted as a string, will be after "1.6.0" which is bad. Use
// this function to sort that out. (See what I did there? Bwaaaahah!)
bool UpgradeCheck::suggestUpgrade(const QString& from, const QString& to)
{
#if (QT_VERSION < QT_VERSION_CHECK(6, 4, 0))
  int fromIndex = 0;
  int toIndex = 0;
#else
  qsizetype fromIndex = 0;
  qsizetype toIndex = 0;
#endif
  QVersionNumber fromVersion = QVersionNumber::fromString(from, &fromIndex);
  QVersionNumber toVersion = QVersionNumber::fromString(to, &toIndex);

  // We don't have to handle every possible range because the server won't
  // have more than a version or two live at any time.
  if (fromVersion < toVersion) {
    return true;
  }
  // Just look for the presence of stuff (not even the contents) of the
  // string. Shorter string (no "-betaXXX" wins)
  if (fromVersion == toVersion) {
    if (from.length() - fromIndex > to.length() - toIndex) {
      return true;
    }
  }
  return false;
}
// Some day when we have Gunit or equiv, add unit tests for:
//suggestUpgrade(updateVersion, currentVersion_);
//suggestUpgrade("1.6.0-beta20190413", "1.6.0");
//suggestUpgrade("1.6.0", "1.6.0-beta20190413");
//suggestUpgrade("1.6.0-beta20190413", "1.7.0");
//suggestUpgrade("1.7.0", "1.6.0-beta20190413");

void UpgradeCheck::httpRequestFinished(QNetworkReply* reply)
{

  if (reply == nullptr) {
    babelData_.upgradeErrors_++;
    return;
  }
  if (reply != replyId_) {
    QMessageBox::information(nullptr, tr("HTTP"),
                             tr("Unexpected reply."));
  } else if (reply->error() != QNetworkReply::NoError) {
    babelData_.upgradeErrors_++;
    QMessageBox::information(nullptr, tr("HTTP"),
                             tr("Download failed: %1.")
                             .arg(reply->errorString()));
    replyId_ = nullptr;
    reply->deleteLater();
    return;
  }

  // redirection is handled by the Network Access API automatically.

  QVariant statusCode = reply->attribute(QNetworkRequest::HttpStatusCodeAttribute);
  if (testing) {
    qDebug() << "http status code " << statusCode.toInt();
  }
  if (statusCode != 200) {
    QVariant reason = reply->attribute(QNetworkRequest::HttpReasonPhraseAttribute);
    QMessageBox::information(nullptr, tr("HTTP"),
                             tr("Download failed: %1: %2.")
                             .arg(statusCode.toInt())
                             .arg(reason.toString()));
    replyId_ = nullptr;
    reply->deleteLater();
    return;
  }

  babelData_.upgradeCallbacks_++;
  QString oresponse(reply->readAll());

  QDomDocument document;
  // This shouldn't ever be seen by a user.
#if (QT_VERSION >= QT_VERSION_CHECK(6, 5, 0))
  if (auto result = document.setContent(oresponse); !result) {
#else
  struct {
    int errorLine{-1};
    QString errorMessage;
  } result;
  if (!document.setContent(oresponse, &result.errorMessage, &result.errorLine)) {
#endif
    QMessageBox::critical(nullptr, tr("Error"),
                          tr("Invalid return data at line %1: %2.")
                          .arg(result.errorLine)
                          .arg(result.errorMessage));
    babelData_.upgradeErrors_++;
    replyId_ = nullptr;
    reply->deleteLater();
    return;
  }

  QString response;
  QString upgradeText;

  if (testing) {
    currentVersion_ = QStringLiteral("1.3.1");  // for testing
  }

  bool allowBeta = true;  // TODO: come from prefs or current version...

  QDomNodeList upgrades = document.elementsByTagName(QStringLiteral("update"));
  QUrl downloadUrl;
  updateStatus_ = updateCurrent;  // Current until proven guilty.

  for (int i = 0; i < upgrades.length(); i++) {
    QDomNode upgradeNode = upgrades.item(i);
    QDomElement upgrade = upgradeNode.toElement();
    QString updateVersion = upgrade.attribute(QStringLiteral("version"));
    if (upgrade.attribute(QStringLiteral("downloadURL")).isEmpty()) {
      downloadUrl = QStringLiteral("https://www.gpsbabel.org/download.html");
    } else {
      downloadUrl = upgrade.attribute(QStringLiteral("downloadURL"));
    }
    bool updateIsBeta = upgrade.attribute(QStringLiteral("type")) == "beta";
    bool updateIsMajor = upgrade.attribute(QStringLiteral("type")) == "major";
    bool updateIsMinor = upgrade.attribute(QStringLiteral("type")) == "minor";

    bool updateCandidate = updateIsMajor || updateIsMinor || (updateIsBeta && allowBeta);
    upgradeText = upgrade.firstChildElement(QStringLiteral("overview")).text();

    // String compare, not a numeric one.  Server will return "best first".
    if (suggestUpgrade(currentVersion_, updateVersion) && updateCandidate) {
      babelData_.upgradeOffers_++;
      updateStatus_ = updateNeeded;
      response = tr("A new version of GPSBabel is available.<br />"
                    "Your version is %1 <br />"
                    "The latest version is %2")
                 .arg(currentVersion_, updateVersion);
      break;
    }
  }

  if (!response.isEmpty()) {
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
    default:
      ;
      babelData_.upgradeDeclines_++;
    }
  }

  upgradeWarningTime_ = QDateTime::currentDateTime();

  for (int i = 0; i < formatList_.size(); i++) {
    formatList_[i].zeroUseCounts();
  }
  replyId_ = nullptr;
  reply->deleteLater();
}
