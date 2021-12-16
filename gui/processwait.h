// -*- C++ -*-
// $Id: processwait.h,v 1.1 2009-07-05 21:14:56 robertl Exp $
//------------------------------------------------------------------------
//
//  Copyright (C) 2009  S. Khai Mong <khai@mangrai.com>.
//
//  This program is free software; you can redistribute it and/or
//  modify it under the terms of the GNU General Public License as
//  published by the Free Software Foundation; either version 2 of the
//  License, or (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//  General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301,
//  USA.
//
//------------------------------------------------------------------------
#ifndef PROCESSWAIT_H
#define PROCESSWAIT_H

#include <QByteArray>                  // for QByteArray
#include <QObject>                     // for QObject
#include <QProcess>                    // for QProcess, QProcess::ExitStatus, QProcess::ProcessError
#include <QString>                     // for QString
#include <QTimer>                      // for QTimer
#include <QCloseEvent>                 // for QCloseEvent
#include <QDialog>                     // for QDialog
#include <QDialogButtonBox>            // for QDialogButtonBox
#include <QPlainTextEdit>              // for QPlainTextEdit
#include <QProgressBar>                // for QProgressBar
#include <QWidget>                     // for QWidget


//------------------------------------------------------------------------
class ProcessWaitDialog: public QDialog
{

  Q_OBJECT

public:
  ProcessWaitDialog(QWidget* parent, QProcess* process);

  QString getOutputString()
  {
    return QString::fromLocal8Bit(outputString_);
  }

protected:
  void closeEvent(QCloseEvent* event) override;
  void appendToText(const QByteArray& text);

private slots:
  void timeoutX();

private:
  int progressIndex_{0};
  QProcess* process_{nullptr};
  QProgressBar* progressBar_{nullptr};
  QPlainTextEdit* textEdit_{nullptr};
  QDialogButtonBox* buttonBox_{nullptr};
  QTimer* timer_{nullptr};
  QByteArray outputString_;
};

#endif
