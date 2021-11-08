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

#include <QByteArray>                 // for QByteArray
#include <Qt>                         // for Horizontal, WindowContextHelpButtonHint
#include <QTextCursor>                // for QTextCursor, QTextCursor::End
#include <QAbstractButton>            // for QAbstractButton
#include <QPushButton>                // for QPushButton
#include <QVBoxLayout>                // for QVBoxLayout

#include <cstdlib>                    // for abs
#include <string>                     // for string

#include "appname.h"                  // for appName


//------------------------------------------------------------------------

ProcessWaitDialog::ProcessWaitDialog(QWidget* parent, QProcess* process):
  QDialog(parent), process_(process)
{
  this->resize(400, 220);
  this->setWindowTitle(QString(appName) + tr(" ... Process GPSBabel"));
  // turn off Help Button which can appear on windows as a '?'.
  this->setWindowFlag(Qt::WindowContextHelpButtonHint, false);
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

  connect(process_, &QProcess::readyReadStandardError,
  this, [this]() {
    appendToText(process_->readAllStandardError());
  });
  connect(process_, &QProcess::readyReadStandardOutput,
  this, [this]() {
    appendToText(process_->readAllStandardOutput());
  });
  connect(btn, &QAbstractButton::clicked,
          this, &ProcessWaitDialog::reject);

  timer_ = new QTimer(this);
  timer_->setInterval(100);
  timer_->setSingleShot(false);
  connect(timer_, &QTimer::timeout, this, &ProcessWaitDialog::timeoutX);
  timer_->start();
}

//------------------------------------------------------------------------
void ProcessWaitDialog::timeoutX()
{
  progressIndex_ = (progressIndex_ + 1) % 100;
  int progress = 100 - 2 * std::abs(progressIndex_ - 50);
  progressBar_->setValue(progress);
}

//------------------------------------------------------------------------
// appendPlainText automatically puts in a new line with every call,
// necessitating the need to to buffer it, and only append when we get a real
// newline.
// QPlainTextEdit also gets very slow when the accumulated plain text is
// extensive.
// Thus, we set the plain text to the last bit of output.
//
void ProcessWaitDialog::appendToText(const QByteArray& text)
{
  static constexpr int textLimit = 16384;

  // It's faster if bufferedOut is a std::string compared to a QByteArray.
  std::string bufferedOut;
  bufferedOut.reserve(text.size());
  // Throw out carriage returns which cause double spacing.
  for (const char ch : text) {
    if (ch != '\r') {
      bufferedOut += ch;
    }
  }
  outputString_ += QByteArray::fromStdString(bufferedOut);

  textEdit_->setPlainText(QString::fromLocal8Bit(outputString_.right(textLimit)));
  textEdit_->moveCursor(QTextCursor::End);
}

//------------------------------------------------------------------------
void ProcessWaitDialog::closeEvent(QCloseEvent* event)
{
  event->ignore();
}
