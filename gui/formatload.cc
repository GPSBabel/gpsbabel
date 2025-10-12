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
//  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301,
//  USA.
//
//------------------------------------------------------------------------

#include "formatload.h"
#include <QApplication>        // for QApplication
#include <QByteArray>          // for QByteArray
#include <QChar>               // for QChar, operator==
#include <QCoreApplication>    // for QCoreApplication
#include <QMessageBox>         // for QMessageBox
#include <QObject>             // for QObject
#include <QProcess>            // for QProcess
#include <QRegularExpression>  // for QRegularExpression, QRegularExpressionMatch
#include <QString>             // for QString, operator+
#include <QTextStream>         // for QTextStream
#include <QVariant>            // for QVariant
#include "appname.h"           // for appNam


#ifdef GENERATE_CORE_STRINGS
extern QTextStream* generate_output_stream;
#endif

//------------------------------------------------------------------------
static QString xlt(const QString& f)
{
#ifdef GENERATE_CORE_STRINGS
  *generate_output_stream << "QT_TRANSLATE_NOOP(\"core\",\"" << f << "\")" << Qt::endl;
#endif
  return QCoreApplication::translate("core", f.toUtf8().constData());
}

//------------------------------------------------------------------------
bool FormatLoad::skipToValidLine()
{
  static const QRegularExpression regex("^file|serial");
  while ((currentLine_ < lines_.size()) && !regex.match(lines_[currentLine_]).hasMatch()) {
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
  QString htmlPage = lines_[currentLine_++].trimmed();

  QList <FormatOption> optionList;
  while ((currentLine_ < lines_.size()) && lines_[currentLine_].startsWith("option")) {
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
      type = (!optionMax.isEmpty() && !optionMin.isEmpty()) ? FormatOption::OPTboundedInt : FormatOption::OPTint;
      if (optionMax.isEmpty()) {
        optionMax = "2147483647";
      }
      if (optionMin.isEmpty()) {
        optionMin = "-2147483647";
      }
    } else if (optionType == "float") {
      type = FormatOption::OPTfloat;
      if (optionMax.isEmpty()) {
        optionMax = "1.0E308";
      }
      if (optionMin.isEmpty()) {
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
  if (!htmlPage.isEmpty() && Format::getHtmlBase().isEmpty()) {
    QString base = htmlPage;
    static const QRegularExpression re("/[^/]+$");
    base.replace(re, "/");
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
    if (!l.trimmed().isEmpty()) {
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
