/*
    Copyright (C) 2021 Robert Lipe, robertlipe+source@gpsbabel.org

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

#include "src/core/logging.h"

#include <cstdio>            // for fflush, fprintf, stderr, stdout

#include <QDebug>            // for QDebug


/* TextStream interface */
[[noreturn]] void gbFatal(QDebug& msginstance)
{
  auto* myinstance = new FatalMsg;
  myinstance->swap(msginstance);
  delete myinstance;
  exit(1);
}

QDebug& operator<< (QDebug& debug, const DebugIndent& indent)
{
  for (int i = 1; i<indent.level_; i++) {
    debug << '.';
  }
  return debug;
}

namespace gpsbabel
{

void Logging::setMessagePattern(const QString& id)
{
  if (id.isEmpty()) {
    qSetMessagePattern("%{if-category}%{category}: %{endif}main: %{message}");
  } else {
    qSetMessagePattern(QStringLiteral("%{if-category}%{category}: %{endif}%1: %{message}").arg(id));
  }
}

void Logging::LegacyLogMessageHandler(QtMsgType type, const QString& msg)
{
  static bool lineInProgress = false;

  if (lineInProgress) {
    fprintf(stderr, "%s", qPrintable(msg));
  } else {
    QString message = qFormatLogMessage(type, QMessageLogContext(), msg);
    fprintf(stderr, "%s", qPrintable(message));
  }
  fflush(stderr);
  lineInProgress = !msg.endsWith('\n');
}

/* The GUI captures standard error and standard output for
 * display in the output window.
 * On windows the Qt supplied default message handler might send messages
 * to the debugger instead.
 * We override the default message handler to ensure that messages go to
 * standard error.
 * We support two types of message:
 * 1) Legacy Messages:
 *     a) messages containing embedded newlines are broken it single line message
 *        so they can be properly formated by our log formatter.
 *     b) accumulation of messages that don't end in a newline. These are output
 *        as they come in in case a newline never shows up.  If they start a line
 *        they are formatted by our log formatter.
 * 2) QDebug style messages where a newline is added automatically to every message. 
 */
void Logging::MessageHandler(QtMsgType type, const QMessageLogContext& context, const QString& msg)
{
  if (msg.startsWith(legacyKey())) {
    QString legacyMsg = msg.mid(legacyKey().size());

    for (auto idx = legacyMsg.indexOf('\n'); idx >= 0; idx = legacyMsg.indexOf('\n')) {
      QString msg = legacyMsg.sliced(0, idx + 1);
      LegacyLogMessageHandler(type, msg);
      legacyMsg.remove(0, idx + 1);
    }
    if (!legacyMsg.isEmpty()) {
      LegacyLogMessageHandler(type, legacyMsg);
    }
  } else {
    QString message = qFormatLogMessage(type, context, msg);
    /* flush any buffered standard output */
    fflush(stdout);
    fprintf(stderr, "%s\n", qPrintable(message));
    fflush(stderr);
  }
}

QString Logging::flaggedLegacyMessage(const QString& msg)
{
  return legacyKey() + msg;
}
} // namespace gpsbabel
