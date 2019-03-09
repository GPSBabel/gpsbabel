// -*- c++ -*-
// $Id: formatload.cpp,v 1.3 2009-11-02 20:38:02 robertl Exp $
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

#include <QtCore/QByteArray>        // for QByteArray
#include <QtCore/QChar>             // for operator==, QChar
#include <QtCore/QCharRef>          // for QCharRef
#include <QtCore/QCoreApplication>  // for QCoreApplication
#ifdef GENERATE_CORE_STRINGS
#include <QtCore/QDebug>
#endif
#include <QtCore/QObject>           // for QObject
#include <QtCore/QProcess>          // for QProcess
#include <QtCore/QRegExp>           // for QRegExp
#include <QtCore/QString>           // for QString, operator+
#include <QtCore/QTextStream>       // for QTextStream
#include <QtCore/QVariant>          // for QVariant
#include <QtWidgets/QApplication>   // for QApplication
#include <QtWidgets/QMessageBox>    // for QMessageBox

#include "formatload.h"
#include "appname.h"                // for appName


//------------------------------------------------------------------------
static QString xlt(const QString& f)
{
#ifdef GENERATE_CORE_STRINGS
  qInfo().nospace() << "QT_TRANSLATE_NOOP(\"core\"," << f << ")";
#endif
  return QCoreApplication::translate("core", f.toUtf8().constData());
}

//------------------------------------------------------------------------
bool FormatLoad::skipToValidLine()
{
  QRegExp regex("^(file|serial)");
  while (currentLine_ <lines_.size() && regex.indexIn(lines_[currentLine_]) != 0) {
    currentLine_++;
  }
  return (currentLine_<lines_.size());
}

//------------------------------------------------------------------------
bool FormatLoad::processFormat(Format& format)
{
  QStringList hfields = lines_[currentLine_++].split("\t");
  if (hfields.size() < 5) {
    return false;
  }
  QString htmlPage = lines_[currentLine_++];
  htmlPage.replace(QRegExp("^[\\s]*"), "");
  htmlPage.replace(QRegExp("[\\s]$"), "");

  QRegExp regex("^option");
  QList <FormatOption> optionList;
  while (currentLine_ <lines_.size() && regex.indexIn(lines_[currentLine_]) == 0) {
    QStringList ofields = lines_[currentLine_].split("\t");
    if (ofields.size() < 9) {
      return false;
    }
    QString name        = ofields[2];
    QString description = ofields[3];
    QString optionType  = ofields[4];
    QString optionDef   = ofields[5];
    QString optionMin   = ofields[6];
    QString optionMax   = ofields[7];
    QString optionHtml  = ofields[8];
    FormatOption::optionType type = FormatOption::OPTbool;
    if (optionType == "boolean") {
      type = FormatOption::OPTbool;
    } else if (optionType == "string") {
      type = FormatOption::OPTstring;
    } else if (optionType == "integer") {
      type = (optionMax != "" && optionMin != "") ? FormatOption::OPTboundedInt : FormatOption::OPTint;
      if (optionMax == "") {
        optionMax = "2147483647";
      }
      if (optionMin == "") {
        optionMin = "-2147483647";
      }
    } else if (optionType == "float") {
      type = FormatOption::OPTfloat;
      if (optionMax == "") {
        optionMax = "1.0E308";
      }
      if (optionMin == "") {
        optionMin = "-1.0E308";
      }
    } else if (optionType == "file") {
      type = FormatOption::OPTinFile;
    } else if (optionType == "outfile") {
      type = FormatOption::OPToutFile;
    } else {
      type = FormatOption::OPTstring;
    }
    optionList << FormatOption(name, xlt(description),
                               type, QVariant(optionDef), QVariant(optionMin),
                               QVariant(optionMax), optionHtml);
    currentLine_++;
  }
  QList <FormatOption> optionList2 = optionList;

  format = Format(hfields[2], xlt(hfields[4]),
                  hfields[1][0] == QChar('r'),  hfields[1][2] == QChar('r'),  hfields[1][4] == QChar('r'),
                  hfields[1][1] == QChar('w'),  hfields[1][3] == QChar('w'),  hfields[1][5] == QChar('w'),
                  hfields[0] == "file",
                  hfields[0] == "serial",
                  hfields[3].split('/'),
                  optionList,
                  optionList2, htmlPage);
#ifndef GENERATE_CORE_STRINGS
  if (htmlPage.length() > 0 && Format::getHtmlBase().length() == 0) {
    QString base = htmlPage;
    base.replace(QRegExp("/[^/]+$"), "/");
    Format::setHtmlBase(base);
  }
#endif
  return true;
}

//------------------------------------------------------------------------
bool FormatLoad::getFormats(QList<Format>& formatList)
{
  formatList.clear();

  QProcess babel;
  babel.start(QApplication::applicationDirPath() + "/gpsbabel", QStringList() << "-^3");
  if (!babel.waitForStarted()) {
    return false;
  }
  babel.closeWriteChannel();
  if (!babel.waitForFinished()) {
    return false;
  }
  if (babel.exitCode() != 0) {
    return false;
  }

  QTextStream tstream(babel.readAll());
  QList<int>lineList;
  int k=0;
  while (!tstream.atEnd()) {
    QString l = tstream.readLine();
    k++;
    if (!QRegExp("^[\\s]*$").exactMatch(l)) {
      lines_ << l;
      lineList<<k;
    }
  }
  currentLine_ = 0;

  for (bool dataPresent = skipToValidLine(); dataPresent; dataPresent=skipToValidLine()) {
    Format format;
    if (!processFormat(format)) {
      QMessageBox::information
      (nullptr, appName,
       QObject::tr("Error processing formats from running process \"gpsbabel -^3\" at line %1").arg(lineList[currentLine_]));
    } else {
      formatList << format;
    }
  }
  return true;
}
