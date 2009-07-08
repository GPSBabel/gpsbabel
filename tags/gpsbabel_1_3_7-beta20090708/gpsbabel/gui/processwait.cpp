// -*- c++ -*-
// $Id: processwait.cpp,v 1.1 2009-07-05 21:14:56 robertl Exp $
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
#include <QProcess>
#include <QStringList>
#include <QPlainTextEdit>
#include <QDialogButtonBox>
#include <QVBoxLayout>
#include <QDialog>
#include <QProgressBar>
#include <QPushButton>
#include <QTimer>
#include "processwait.h"
#include "appname.h"


//------------------------------------------------------------------------

QString ProcessWaitDialog::processErrorString( QProcess::ProcessError err) 
{
  switch (err)
    {
    case QProcess::FailedToStart: 
      return QString(tr("Process failed to start")); 
      break;
    case QProcess::Crashed: 
      return QString(tr("Process crashed"));
      break;
    case QProcess::Timedout:
      return QString(tr("Process timedout"));
      break;
    case QProcess::WriteError:
      return QString(tr("Error while trying to write to process")); 
      break;
    case QProcess::ReadError:
      return QString(tr("Error while trying to read from process"));
      break;
    case QProcess::UnknownError:
    default:
      return QString(tr("Unknown process error")); 
    }
  return QString("");
}
//------------------------------------------------------------------------
ProcessWaitDialog::ProcessWaitDialog(QWidget *parent, QProcess *process):
  QDialog(parent), process(process)
{
  this->resize(400, 220);
  this->setWindowTitle(QString(appName) + tr(" ... Process GpsBabel"));
  QVBoxLayout *layout = new QVBoxLayout(this);
  
  textEdit = new QPlainTextEdit(this);
  textEdit->setReadOnly(true);
  layout->addWidget(textEdit);
  
  progressBar = new QProgressBar(this);
  progressBar->setTextVisible(false);
  layout->addWidget(progressBar);
  
  buttonBox = new QDialogButtonBox(this);
  buttonBox->setOrientation(Qt::Horizontal);
  buttonBox->setStandardButtons(QDialogButtonBox::Abort);
  QPushButton* btn = buttonBox->button(QDialogButtonBox::Abort);
  btn->setText(tr("Stop Process"));
  layout->addWidget(buttonBox);
  
  connect(process, SIGNAL(error(QProcess::ProcessError)), 
	  this,    SLOT  (errorX(QProcess::ProcessError)));
  connect(process, SIGNAL(finished(int, QProcess::ExitStatus)),
	  this,    SLOT  (finishedX(int, QProcess::ExitStatus)));
  connect(process, SIGNAL(readyReadStandardError()),
          this,    SLOT  (readyReadStandardErrorX()));
  connect(process, SIGNAL(readyReadStandardOutput()),
	  this,    SLOT  (readyReadStandardOutputX()));
  connect(btn,     SIGNAL(clicked()),
	  this,    SLOT  (stopClickedX()));
  estatus = QProcess::CrashExit;  // Assume all errors are crashes for now.

  bufferedOut = "";

  //
  for (int i=0; i<=100; i+=2) 
    progressVals.push_back(i);
  for (int i=98; i>0; i-=2) 
    progressVals.push_back(i);
  progressIndex = progressVals.size()/2;

  timer = new QTimer(this);
  timer->setInterval(100);
  timer->setSingleShot(false);
  connect(timer, SIGNAL(timeout()), this, SLOT(timeoutX()));
  stopCount = -1;
  timer->start();
  errorString = "";

}

//------------------------------------------------------------------------
ProcessWaitDialog::~ProcessWaitDialog()
{
};
//------------------------------------------------------------------------
bool ProcessWaitDialog::getExitedNormally() 
{
  return (errorString.length() == 0);
};

//------------------------------------------------------------------------
QString ProcessWaitDialog::getErrorString() 
{
  return errorString;
};

//------------------------------------------------------------------------
int ProcessWaitDialog::getExitCode() 
{
  return ecode;
};

//------------------------------------------------------------------------
void ProcessWaitDialog::stopClickedX() 
{
  process->terminate();
};
//------------------------------------------------------------------------
void ProcessWaitDialog::timeoutX() 
{
  progressIndex++;
  int idx = progressIndex % progressVals.size();
  progressBar->setValue(progressVals[idx]);
  if (stopCount >=0)
    stopCount++;
  if (stopCount > 150) {
    process->kill();
    errorString = QString(tr("Process did not terminate successfully"));
    timer->stop();
    accept();
  }
};

//------------------------------------------------------------------------
void ProcessWaitDialog::errorX(QProcess::ProcessError err)
{
  errorString = processErrorString(err);
  timer->stop();
  accept();
};

//------------------------------------------------------------------------
void ProcessWaitDialog::finishedX(int exitCode, QProcess::ExitStatus es) 
{
  ecode = exitCode;
  if (es == QProcess::CrashExit)
    errorString = QString(tr("Process crashed whle running"));
  timer->stop();
  accept();
};


//------------------------------------------------------------------------
// appendPlainText automatically puts in a new line with every call.  That's
// why you have to buffer it, and only append when we get a real newline.
//
void ProcessWaitDialog::appendToText(const char *ptr) 
{
  outputString += QString(ptr);
  for (const char *cptr = ptr; *cptr; cptr++) {
    if (*cptr == '\r')
      continue;
    if (*cptr == '\n') {
      textEdit->appendPlainText(QString::fromStdString(bufferedOut));
      bufferedOut = "";
      continue;
    }
    bufferedOut += *cptr;
  }
}


//------------------------------------------------------------------------
void ProcessWaitDialog::readyReadStandardErrorX() 
{
  QByteArray d = process->readAllStandardError();
  appendToText(d.data());
};

//------------------------------------------------------------------------
void ProcessWaitDialog::readyReadStandardOutputX()
 {
  QByteArray d = process->readAllStandardOutput();
  appendToText(d.data());
};

void ProcessWaitDialog::closeEvent(QCloseEvent *event)
{
   event->ignore();
};
