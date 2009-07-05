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
//  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111
//  USA
//
//------------------------------------------------------------------------
#ifndef PROCESSWAIT_H
#define PROCESSWAIT_H


#include <QProcess>
#include <QDialog>
#include <vector>
#include <string>
using std::string;
using std::vector;

class QProgressBar;
class QPlainTextEdit;
class QDialogButtonBox;
class QTimer;
//------------------------------------------------------------------------
class ProcessWaitDialog: public QDialog 
{
  
  Q_OBJECT
  
 public:
  //
  ProcessWaitDialog(QWidget *parent, QProcess *process);
  ~ProcessWaitDialog();
  
  bool getExitedNormally();
  int getExitCode();
  QString getErrorString();
  QString getOutputString() const {return outputString;};

 protected:
  void closeEvent (QCloseEvent*event);
  void appendToText(const char *);
  QString processErrorString(QProcess::ProcessError err);
				 

 private slots:
  void errorX(QProcess::ProcessError);
  void finishedX(int exitCode, QProcess::ExitStatus);
  void readyReadStandardErrorX();
  void readyReadStandardOutputX();
  void timeoutX();
  void stopClickedX();

 private:
  vector <int> progressVals;
  int          progressIndex;
  int          stopCount;
  string       bufferedOut;
  QProcess::ExitStatus estatus;
  int                  ecode;
  QProcess     *process;
  QProgressBar *progressBar;
  QPlainTextEdit *textEdit;
  QDialogButtonBox *buttonBox;
  QTimer           *timer;
  QString          errorString;
  QString          outputString;
};

#endif
