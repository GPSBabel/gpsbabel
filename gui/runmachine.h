/*
    Copyright (C) 2021 Robert Lipe, gpsbabel.org

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

#ifndef RUNMACHINE_H
#define RUNMACHINE_H

#include <QtCore/QObject>      // for QObject
#include <QtCore/QProcess>     // for QProcess, QProcess::ExitStatus, QProcess::ProcessError, qt_getEnumName
#include <QtCore/QString>      // for QString
#include <QtCore/QStringList>  // for QStringList
#include <QtWidgets/QWidget>   // for QWidget

#include <optional>            // for optional, nullopt

#include "processwait.h"       // for ProcessWaitDialog


class RunMachine : public QWidget
{
  Q_OBJECT

public:

  /* Types */

  enum SignalId {
    start,
    processErrorOccurred,
    processStarted,
    processFinished,
    abortRequested
  };
  Q_ENUM(SignalId)

  enum State {
    init,
    starting,
    running,
    done
  };
  Q_ENUM(State)

  /* Special Member Functions */

  RunMachine(QWidget* parent, const QString& program, const QStringList& args);

  /* Member Functions */

  static QString decodeProcessError(QProcess::ProcessError err);
  int exec();
  void open();
  QString getOutputString()
  {
    return progress_->getOutputString();
  }
  QString getErrorString()
  {
    return errorString_;
  }
  bool getRetStatus() const
  {
    return errorString_.isEmpty();
  }

Q_SIGNALS:
  void finished();

private:

  /* Constants */

  static constexpr bool debug = false;
  static constexpr bool finishOnAbort = false;
  static constexpr bool finishOnRunningError = false;

  /* Member Functions */

  void execute(SignalId id,
               std::optional<QProcess::ProcessError> error,
               std::optional<int> exitCode,
               std::optional<QProcess::ExitStatus> exitStatus);

  /* Data Members */

  QProcess* process_{nullptr};
  ProcessWaitDialog* progress_{nullptr};
  State state_{init};
  QString program_;
  QStringList args_;
  QString errorString_;

};
#endif // RUNMACHINE_H
