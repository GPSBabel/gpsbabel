// -*- c++ -*-
// $Id: processwait.cpp,v 1.3 2009-08-28 17:08:55 robertl Exp $
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
  QDialog(parent), process_(process)
{
  this->resize(400, 220);
  this->setWindowTitle(QString(appName) + tr(" ... Process GPSBabel"));
  QVBoxLayout *layout = new QVBoxLayout(this);

  textEdit_ = new QPlainTextEdit(this);
  textEdit_->setReadOnly(true);
  layout->addWidget(textEdit_);

  progressBar_ = new QProgressBar(this);
  progressBar_->setTextVisible(false);
  layout->addWidget(progressBar_);

  buttonBox_ = new QDialogButtonBox(this);
  buttonBox_->setOrientation(Qt::Horizontal);
  buttonBox_->setStandardButtons(QDialogButtonBox::Abort);
  QPushButton* btn = buttonBox_->button(QDialogButtonBox::Abort);
  btn->setText(tr("Stop Process"));
  layout->addWidget(buttonBox_);

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
  exitStatus_ = QProcess::CrashExit;  // Assume all errors are crashes for now.

  bufferedOut_ = "";

  //
  for (int i=0; i<=100; i+=2)
    progressVals_.push_back(i);
  for (int i=98; i>0; i-=2)
    progressVals_.push_back(i);
  progressIndex_ = progressVals_.size()/2;

  timer_ = new QTimer(this);
  timer_->setInterval(100);
  timer_->setSingleShot(false);
  connect(timer_, SIGNAL(timeout()), this, SLOT(timeoutX()));
  stopCount_ = -1;
  timer_->start();
  errorString_ = "";

}

//------------------------------------------------------------------------
ProcessWaitDialog::~ProcessWaitDialog()
{
};
//------------------------------------------------------------------------
bool ProcessWaitDialog::getExitedNormally()
{
  return (errorString_.length() == 0);
};

//------------------------------------------------------------------------
QString ProcessWaitDialog::getErrorString()
{
  return errorString_;
};

//------------------------------------------------------------------------
int ProcessWaitDialog::getExitCode()
{
  return ecode_;
};

//------------------------------------------------------------------------
void ProcessWaitDialog::stopClickedX()
{
  process_->terminate();
};
//------------------------------------------------------------------------
void ProcessWaitDialog::timeoutX()
{
  progressIndex_++;
  int idx = progressIndex_ % progressVals_.size();
  progressBar_->setValue(progressVals_[idx]);
  if (stopCount_ >=0)
    stopCount_++;
  if (stopCount_ > 150) {
    process_->kill();
    errorString_ = QString(tr("Process did not terminate successfully"));
    timer_->stop();
    accept();
  }
};

//------------------------------------------------------------------------
void ProcessWaitDialog::errorX(QProcess::ProcessError err)
{
  errorString_ = processErrorString(err);
  timer_->stop();
  accept();
};

//------------------------------------------------------------------------
void ProcessWaitDialog::finishedX(int exitCode, QProcess::ExitStatus es)
{
  ecode_ = exitCode;
  if (es == QProcess::CrashExit)
    errorString_ = QString(tr("Process crashed whle running"));
  timer_->stop();
  accept();
};


//------------------------------------------------------------------------
// appendPlainText automatically puts in a new line with every call.  That's
// why you have to buffer it, and only append when we get a real newline.
//
void ProcessWaitDialog::appendToText(const char *ptr)
{
  outputString_ += QString(ptr);
  for (const char *cptr = ptr; *cptr; cptr++) {
    if (*cptr == '\r')
      continue;
    if (*cptr == '\n') {
      textEdit_->appendPlainText(QString::fromStdString(bufferedOut_));
      bufferedOut_ = "";
      continue;
    }
    bufferedOut_ += *cptr;
  }
}


//------------------------------------------------------------------------
void ProcessWaitDialog::readyReadStandardErrorX()
{
  QByteArray d = process_->readAllStandardError();
  appendToText(d.data());
};

//------------------------------------------------------------------------
void ProcessWaitDialog::readyReadStandardOutputX()
 {
  QByteArray d = process_->readAllStandardOutput();
  appendToText(d.data());
};

void ProcessWaitDialog::closeEvent(QCloseEvent *event)
{
   event->ignore();
};
