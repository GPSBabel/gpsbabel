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

#include <QtCore/QObject>              // for QObject
#include <QtCore/QProcess>             // for QProcess, QProcess::ExitStatus, QProcess::ProcessError
#include <QtCore/QString>              // for QString
#include <QtCore/QStringList>          // for QStringList
#include <QtCore/QTimer>               // for QTimer
#include <QtGui/QCloseEvent>           // for QCloseEvent
#include <QtWidgets/QDialog>           // for QDialog
#include <QtWidgets/QDialogButtonBox>  // for QDialogButtonBox
#include <QtWidgets/QPlainTextEdit>    // for QPlainTextEdit
#include <QtWidgets/QProgressBar>      // for QProgressBar
#include <QtWidgets/QWidget>           // for QWidget

#include <string>                      // for string


//------------------------------------------------------------------------
class ProcessWaitDialog: public QDialog
{

  Q_OBJECT

public:
  ProcessWaitDialog(QWidget* parent, const QString& program,
                    const QStringList& arguments);

  bool getExitedNormally();
  [[nodiscard]] int getExitCode() const;
  QString getErrorString();
  QString getOutputString()
  {
    return outputString_;
  }

protected:
  void closeEvent(QCloseEvent* event) override;
  void appendToText(const char* ptr);
  static QString processErrorString(QProcess::ProcessError err);

private slots:
  void errorX(QProcess::ProcessError err);
  void finishedX(int exitCode, QProcess::ExitStatus es);
  void readyReadStandardErrorX();
  void readyReadStandardOutputX();
  void timeoutX();
  void stopClickedX();

private:
  int progressIndex_;
  int stopCount_;
  std::string bufferedOut_;
  QProcess::ExitStatus exitStatus_;
  int ecode_;
  QProcess* process_;
  QProgressBar* progressBar_;
  QPlainTextEdit* textEdit_;
  QDialogButtonBox* buttonBox_;
  QTimer* timer_;
  QString errorString_;
  QString outputString_;
};

#endif
