// -*- C++ -*-
// $Id: format.h,v 1.6 2010-02-15 02:57:00 robertl Exp $
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
#ifndef FORMAT_H
#define FORMAT_H

#include <QList>        // for QList
#include <QSettings>    // for QSettings
#include <QString>      // for QString
#include <QStringList>  // for QStringList
#include <QVariant>     // for QVariant

class FormatOption
{
public:

  /* Types */

  enum optionType {
    OPTstring,
    OPTbool,
    OPTint,
    OPTboundedInt,
    OPTfloat,
    OPTinFile,
    OPToutFile,
  };

  /* Special Member Functions */

  FormatOption() = default;
  FormatOption(const QString& name,
               const QString& description,
               optionType type,
               const QVariant& defaultValue = QVariant(),
               const QVariant& minValue = QVariant(),
               const QVariant& maxValue = QVariant(),
               const QString& html = QString()
              ):
    name_(name),
    description_(description),
    type_(type),
    defaultValue_(defaultValue),
    minValue_(minValue),
    maxValue_(maxValue),
    html_(html)
  {
    // Boolean values pay more attention to 'selected' than value.  Make
    // them match here. For non-bools, just make them unchecked.
    if ((type_ == OPTbool) && defaultValue_.toBool()) {
      isSelected_ = true;
    } else {
      isSelected_ = false;
    }
  }

  /* Member Functions */

  QString  getName() const
  {
    return name_;
  }
  QString  getDescription() const
  {
    return description_;
  }
  optionType getType() const
  {
    return type_;
  }
  QVariant getValue() const
  {
    return value_;
  }
  bool     getSelected() const
  {
    return isSelected_;
  }
  QVariant getMinValue() const
  {
    return minValue_;
  }
  QVariant getMaxValue() const
  {
    return maxValue_;
  }
  QVariant getDefaultValue() const
  {
    return defaultValue_;
  }

  void setValue(const QVariant& v)
  {
    value_ = v;
  }

  void setSelected(bool v)
  {
    isSelected_ = v;
  }

  QString getHtml() const
  {
    return html_;
  }

private:

  /* Data Members */

  QString name_;
  QString description_;
  optionType type_{OPTbool};
  QVariant defaultValue_;
  QVariant minValue_;
  QVariant maxValue_;
  QString  html_;
  QVariant value_;
  bool isSelected_{false};
};


//------------------------------------------------------------------------
class Format
{
public:

  /* Special Member Functions */

  Format() = default;
  Format(const QString& name,
         const QString& description,
         bool readWaypoints,
         bool readTracks,
         bool readRoutes,
         bool writeWaypoints,
         bool writeTracks,
         bool writeRoutes,
         bool fileFormat,
         bool deviceFormat,
         const QStringList& extensions,
         QList<FormatOption>& inputOptions,
         QList<FormatOption>& outputOptions,
         const QString& html):
    name_(name),
    description_(description),
    readWaypoints_(readWaypoints),
    readTracks_(readTracks),
    readRoutes_(readRoutes),
    writeWaypoints_(writeWaypoints),
    writeTracks_(writeTracks),
    writeRoutes_(writeRoutes),
    fileFormat_(fileFormat),
    deviceFormat_(deviceFormat),

    extensions_(extensions),
    inputOptions_(inputOptions),
    outputOptions_(outputOptions)
  {
    (void)html; // suppress 'unused' warning.
  }

  /* Member Functions */

  bool isReadWaypoints() const
  {
    return readWaypoints_;
  }

  bool isReadTracks() const
  {
    return readTracks_;
  }

  bool isReadRoutes() const
  {
    return readRoutes_;
  }

  bool isReadSomething() const
  {
    return isReadWaypoints() || isReadTracks() || isReadRoutes();
  }

  bool isWriteWaypoints() const
  {
    return writeWaypoints_;
  }

  bool isWriteTracks() const
  {
    return writeTracks_;
  }

  bool isWriteRoutes() const
  {
    return writeRoutes_;
  }

  bool isWriteSomething() const
  {
    return isWriteWaypoints() || isWriteTracks() || isWriteRoutes();
  }

  QString getName() const
  {
    return name_;
  }

  QString getDescription() const
  {
    return description_;
  }

  QString getHtml() const
  {
    return html_;
  }

  QStringList getExtensions() const
  {
    return extensions_;
  }

  const QList<FormatOption>& getInputOptions()  const
  {
    return inputOptions_;
  }

  const QList<FormatOption>& getOutputOptions() const
  {
    return outputOptions_;
  }

  QList<FormatOption>* getInputOptionsRef()
  {
    return &inputOptions_;
  }

  QList<FormatOption>* getOutputOptionsRef()
  {
    return &outputOptions_;
  }

  bool isDeviceFormat() const
  {
    return deviceFormat_;
  }

  bool isFileFormat() const
  {
    return   fileFormat_;
  }

  bool isHidden() const
  {
    return hidden_;
  }

  void setHidden(bool state)
  {
    hidden_ = state;
  }

  void saveSettings(QSettings& settings);
  void restoreSettings(QSettings& settings);
  void setToDefault();
  static QString getHtmlBase()
  {
    return htmlBase_;
  }
  static void setHtmlBase(const QString& s)
  {
    htmlBase_ = s;
  }

  void bumpReadUseCount(int v)
  {
    readUseCount_ += v;
  }
  void bumpWriteUseCount(int v)
  {
    writeUseCount_ += v;
  }
  int getReadUseCount()  const
  {
    return readUseCount_;
  }
  int getWriteUseCount() const
  {
    return writeUseCount_;
  }
  void zeroUseCounts()
  {
    readUseCount_  = 0;
    writeUseCount_ = 0;
  }

private:

  /* Data Members */

  QString name_;
  QString description_;
  bool readWaypoints_{false};
  bool readTracks_{false};
  bool readRoutes_{false};
  bool writeWaypoints_{false};
  bool writeTracks_{false};
  bool writeRoutes_{false};
  bool fileFormat_{false};
  bool deviceFormat_{false};
  bool hidden_{false};
  QStringList extensions_;
  QList<FormatOption>inputOptions_;
  QList<FormatOption>outputOptions_;
  QString html_;
  static QString htmlBase_;
  int readUseCount_{0};
  int writeUseCount_{0};

};

#endif
