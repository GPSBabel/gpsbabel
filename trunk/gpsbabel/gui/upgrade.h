#ifndef UPGRADE_H
#define UPGRADE_H
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


#include <QDialog>
#include "ui_upgrade.h"

class QHttp;
class QHttpResponseHeader;

namespace Ui {
    class Upgrade;
}

class Upgrade : public QDialog {
  Q_OBJECT
public:
  Upgrade(QWidget *parent = 0);
  ~Upgrade();

  typedef enum {
    updateUnknown,
    updateCurrent,
    updateNeeded,
  } updateStatus;

  Upgrade::updateStatus checkForUpgrade(void);

protected:
  void changeEvent(QEvent *e);

private:
//  Ui::Upgrade *m_ui;
  Ui_Upgrade     *m_ui;
  QHttp *http;
  int httpRequestId;
  bool httpRequestAborted;
  QString latestVersion;

private slots:
  void httpRequestFinished(int requestId, bool error);
//  void httpStateChanged(int state);
  void readResponseHeader(const QHttpResponseHeader &responseHeader);


};

#endif // UPGRADE_H
