#ifndef UPGRADE_H
#define UPGRADE_H
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

#include <QDateTime>              // for QDateTime
#include <QList>                  // for QList
#include <QNetworkAccessManager>  // for QNetworkAccessManager
#include <QNetworkReply>          // for QNetworkReply
#include <QObject>                // for QObject, Q_OBJECT, slots
#include <QString>                // for QString
#include <QUrl>                   // for QUrl
#include <QWidget>                // for QWidget
#include "babeldata.h"            // for BabelData
#include "format.h"               // for Format


class UpgradeCheck : public QObject
{
  Q_OBJECT
public:
  UpgradeCheck(QWidget* parent, QList<Format>& formatList, BabelData& bd);

  enum updateStatus {
    updateUnknown,
    updateCurrent,
    updateNeeded,
  };

  UpgradeCheck::updateStatus checkForUpgrade(const QString& currentVersion,
      const QDateTime& lastCheckTime,
      bool allowBeta);
  QDateTime getUpgradeWarningTime();
  UpgradeCheck::updateStatus getStatus();
  static bool isTestMode();

private:
  QString currentVersion_;
  QNetworkAccessManager* manager_{nullptr};
  QNetworkReply* replyId_{nullptr};
  QUrl upgradeUrl_{QStringLiteral("https://www.gpsbabel.org/upgrade_check.html")};
  QDateTime upgradeWarningTime_;  // invalid time if this object never issued.
  QList<Format>& formatList_;
  updateStatus updateStatus_{updateUnknown};
  BabelData& babelData_;

  static QString getOsName();
  static QString getOsVersion();
  static QString getCpuArchitecture();
  static bool suggestUpgrade(const QString& from, const QString& to);

private slots:
  void httpRequestFinished(QNetworkReply* reply);

};

#endif // UPGRADE_H
