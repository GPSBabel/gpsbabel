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
//  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301,
//  USA.
//
//------------------------------------------------------------------------
#include "processwait.h"

#include <QtCore/QByteArray>          // for QByteArray
#include <QtCore/QNonConstOverload>   // for QNonConstOverload
#include <QtCore/Qt>                  // for Horizontal
#include <QtCore/QtGlobal>            // for QOverload, qOverload
#include <QtWidgets/QAbstractButton>  // for QAbstractButton
#include <QtWidgets/QPushButton>      // for QPushButton
#include <QtWidgets/QVBoxLayout>      // for QVBoxLayout

#include <cstdlib>                    // for abs

#include "appname.h"                  // for appName


//------------------------------------------------------------------------

QString ProcessWaitDialog::processErrorString(QProcess::ProcessError err)
{
  switch (err) {
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
ProcessWaitDialog::ProcessWaitDialog(QWidget* parent, const QString& program,
                                     const QStringList& arguments):
  QDialog(parent)
{
  this->resize(400, 220);
  this->setWindowTitle(QString(appName) + tr(" ... Process GPSBabel"));
  auto* layout = new QVBoxLayout(this);

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

  process_ = new QProcess(this);
  connect(process_, &QProcess::errorOccurred,
          this,    &ProcessWaitDialog::errorX);
// TODO: Qt6 combined the obsolete overloaded signal QProcess::finished(int exitCode)
// that required using qOverload.
  connect(process_, qOverload<int, QProcess::ExitStatus>(&QProcess::finished),
          this,    &ProcessWaitDialog::finishedX);
  connect(process_, &QProcess::readyReadStandardError,
          this,    &ProcessWaitDialog::readyReadStandardErrorX);
  connect(process_, &QProcess::readyReadStandardOutput,
          this,    &ProcessWaitDialog::readyReadStandardOutputX);
  connect(btn,     &QAbstractButton::clicked,
          this,    &ProcessWaitDialog::stopClickedX);
  exitStatus_ = QProcess::CrashExit;  // Assume all errors are crashes for now.

  bufferedOut_ = "";

  progressIndex_= 0;

  timer_ = new QTimer(this);
  timer_->setInterval(100);
  timer_->setSingleShot(false);
  connect(timer_, &QTimer::timeout, this, &ProcessWaitDialog::timeoutX);
  stopCount_ = -1;
  ecode_ = 0;
  timer_->start();
  errorString_ = "";

  process_->start(program, arguments);
}

//------------------------------------------------------------------------
bool ProcessWaitDialog::getExitedNormally()
{
  return (errorString_.length() == 0);
}

//------------------------------------------------------------------------
QString ProcessWaitDialog::getErrorString()
{
  return errorString_;
}

//------------------------------------------------------------------------
int ProcessWaitDialog::getExitCode() const
{
  return ecode_;
}

//------------------------------------------------------------------------
void ProcessWaitDialog::stopClickedX()
{
  process_->terminate();
}

//------------------------------------------------------------------------
void ProcessWaitDialog::timeoutX()
{
  ++progressIndex_;
  progressIndex_ %= 100;
  int progress = 100 - 2 * std::abs(progressIndex_ - 50);
  progressBar_->setValue(progress);
  if (stopCount_ >=0) {
    stopCount_++;
  }
  if (stopCount_ > 150) {
    process_->kill();
    errorString_ = QString(tr("Process did not terminate successfully"));
    timer_->stop();
    accept();
  }
}

//------------------------------------------------------------------------
void ProcessWaitDialog::errorX(QProcess::ProcessError err)
{
  errorString_ = processErrorString(err);
  timer_->stop();
  accept();
}

//------------------------------------------------------------------------
void ProcessWaitDialog::finishedX(int exitCode, QProcess::ExitStatus es)
{
  ecode_ = exitCode;
  if (es == QProcess::CrashExit) {
    errorString_ = QString(tr("Process crashed while running"));
  }
  timer_->stop();
  accept();
}


//------------------------------------------------------------------------
// appendPlainText automatically puts in a new line with every call.  That's
// why you have to buffer it, and only append when we get a real newline.
//
void ProcessWaitDialog::appendToText(const char* ptr)
{
  outputString_ += QString(ptr);
  for (const char* cptr = ptr; *cptr != 0; cptr++) {
    if (*cptr == '\r') {
      continue;
    }
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
}

//------------------------------------------------------------------------
void ProcessWaitDialog::readyReadStandardOutputX()
{
  QByteArray d = process_->readAllStandardOutput();
  appendToText(d.data());
}

void ProcessWaitDialog::closeEvent(QCloseEvent* event)
{
  event->ignore();
}
