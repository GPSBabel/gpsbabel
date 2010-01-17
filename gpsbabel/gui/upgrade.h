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
    Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111 USA

 */

#include "format.h"
#include <QDialog>
#include <QDateTime>
#include <QHttp>

class QHttp;
class QHttpResponseHeader;

class UpgradeCheck : public QObject {
  Q_OBJECT
public:
  UpgradeCheck(QWidget *parent = 0);
  ~UpgradeCheck();

  typedef enum {
    updateUnknown,
    updateCurrent,
    updateNeeded,
  } updateStatus;

  UpgradeCheck::updateStatus checkForUpgrade(const QString &babelVersion, 
					     int upgradeCheckMethod,
					     const QDateTime &lastCheckTime,
					     const QString &installationUuid,
               QList<Format> &formatList
                                             
                                             );
  QDateTime getUpgradeWarningTime() {
    return upgradeWarningTime;
  }
  static bool isTestMode(void);

protected:

 private:
  QString currentVersion;
  int     upgradeCheckMethod;
  QHttp *http;
  int httpRequestId;
  bool httpRequestAborted;
  QString latestVersion;
  QDateTime upgradeWarningTime;  // invalid time if this object never issued.
  QString getOsName(void);
  QString getOsVersion(void);
  QList<Format> *formatList;

private slots:
  void httpRequestFinished(int requestId, bool error);
  void readResponseHeader(const QHttpResponseHeader &responseHeader);


};

#endif // UPGRADE_H
