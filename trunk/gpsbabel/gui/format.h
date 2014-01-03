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
//  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111
//  USA
//
//------------------------------------------------------------------------
#ifndef FORMAT_H
#define FORMAT_H

#include <QString>
#include <QVariant>
#include <QStringList>
#include <QSettings>

class FormatOption
{
public:
  typedef enum {
    OPTstring,
    OPTbool,
    OPTint,
    OPTboundedInt,
    OPTfloat,
    OPTinFile,
    OPToutFile,
  } optionType;
  
  FormatOption(): name_(QString()), description_(QString()), type_(OPTbool),
      defaultValue_(QVariant()),
      minValue_(QVariant()), maxValue_(QVariant()),
      html_(QString()), value_(QVariant()), isSelected_(false)
  {
  }
  FormatOption(const QString &name, 
	       const QString &description,
	       optionType type,
	       QVariant defaultValue = QVariant(),
	       QVariant minValue = QVariant(),
	       QVariant maxValue = QVariant(),
	       QString html = QString()
         ): name_(name), description_(description), type_(type),
      defaultValue_(defaultValue), minValue_(minValue), maxValue_(maxValue), html_(html)
  {
    value_ = QVariant();
    // Boolean values pay more atention to 'selected' than value.  Make 
    // them match here. For non-bools, just make them unchecked.
    if (type_ == OPTbool && defaultValue.toBool() == true) {
      isSelected_ = true;
    } else {
      isSelected_ = false;
    }
  }

  FormatOption(const FormatOption & c)
    : name_(c.name_), description_(c.description_), type_(c.type_),
      defaultValue_(c.defaultValue_), minValue_(c.minValue_), maxValue_(c.maxValue_), html_(c.html_),
      value_(c.value_), isSelected_(c.isSelected_)
  {
  }
  
  QString  getName() const {return name_; }
  QString  getDescription() const {return description_; }
  optionType getType() const {return type_; }
  QVariant getValue() const    { return value_; }
  bool     getSelected() const {return isSelected_; }
  QVariant getMinValue() const {return minValue_; }
  QVariant getMaxValue() const {return maxValue_; }
  QVariant getDefaultValue() const {return defaultValue_; }

  void setValue(QVariant v) { value_ = v; };
  void setSelected(bool v)  { isSelected_ = v; };
  QString getHtml() const { return html_; };

private:
  QString name_;
  QString description_;
  optionType type_;
  QVariant defaultValue_;
  QVariant minValue_;
  QVariant maxValue_;
  QString  html_;
  QVariant value_;
  bool     isSelected_;
};


//------------------------------------------------------------------------
class Format 
{
 public:
  Format():name_(QString()),
     description_(QString()),
     readWaypoints_(false),
     readTracks_(false),
     readRoutes_(false),
     writeWaypoints_(false),
     writeTracks_(false),
     writeRoutes_(false),
     fileFormat_(false),
     deviceFormat_(false),
           hidden_(false),
     extensions_(QStringList()),
           html_(QString()),
           readUseCount_(0),
           writeUseCount_(0)
  {
    inputOptions_.clear();
    outputOptions_.clear();
  };

  Format(const QString &name,
	 const QString &description,
	 bool readWaypoints, bool readTracks, bool readRoutes,
	 bool writeWaypoints, bool writeTracks, bool writeRoutes,
	 bool fileFormat, bool deviceFormat,
	 const QStringList &extensions,
	 QList<FormatOption> &inputOptions, 
	 QList<FormatOption> &outputptions,
         const QString &html):
    name_(name), description_(description),
    readWaypoints_(readWaypoints), readTracks_(readTracks), readRoutes_(readRoutes),
    writeWaypoints_(writeWaypoints), writeTracks_(writeTracks), writeRoutes_(writeRoutes),
    fileFormat_(fileFormat), deviceFormat_(deviceFormat),
    hidden_(false),
    extensions_(extensions),
    inputOptions_(inputOptions),
    outputOptions_(outputptions),
    html_(QString()),
    readUseCount_(0),
    writeUseCount_(0)
  {
    (void)html; // suppress 'unused' warning.
  }

  Format(const Format &c):
    name_(c.name_), description_(c.description_),
    readWaypoints_(c.readWaypoints_), readTracks_(c.readTracks_), readRoutes_(c.readRoutes_),
    writeWaypoints_(c.writeWaypoints_), writeTracks_(c.writeTracks_), writeRoutes_(c.writeRoutes_),
    fileFormat_(c.fileFormat_), deviceFormat_(c.deviceFormat_),
    hidden_(false),
    extensions_(c.extensions_),
    inputOptions_(c.inputOptions_),
    outputOptions_(c.outputOptions_),
    html_(c.html_),
    readUseCount_(0),
    writeUseCount_(0)
  {
  }

  ~Format() {};

  bool isReadWaypoints() const { return readWaypoints_; };
  bool isReadTracks() const    { return readTracks_; };
  bool isReadRoutes() const    { return readRoutes_; };
  bool isReadSomething() const { 
    return isReadWaypoints() || isReadTracks () || isReadRoutes(); 
  };

  bool isWriteWaypoints() const { return writeWaypoints_; };
  bool isWriteTracks() const    { return writeTracks_; };
  bool isWriteRoutes() const    { return writeRoutes_; };
  bool isWriteSomething() const { 
    return isWriteWaypoints() || isWriteTracks () || isWriteRoutes(); 
  };

  QString getName() const           { return name_; };
  QString getDescription() const    { return description_; };
  QString getHtml() const           { return html_; };
  QStringList getExtensions() const { return extensions_; };
  const QList<FormatOption> &getInputOptions()  const { return inputOptions_; };
  const QList<FormatOption> &getOutputOptions() const { return outputOptions_; };

  QList<FormatOption> *getInputOptionsRef()  { return &inputOptions_; };
  QList<FormatOption> *getOutputOptionsRef() { return &outputOptions_; };

  bool isDeviceFormat() const { return deviceFormat_; };
  bool isFileFormat() const { return   fileFormat_; };

  bool isHidden() const { return hidden_; };
  void setHidden(bool state) { hidden_ = state; };
  
  void saveSettings(QSettings &settings);
  void restoreSettings(QSettings &settings);
  void setToDefault();
  static QString getHtmlBase() { return htmlBase_; }
  static void setHtmlBase(const QString &s) { htmlBase_ = s; }
  
  void bumpReadUseCount(int v)  { readUseCount_ += v; }
  void bumpWriteUseCount(int v) { writeUseCount_ += v; }
  int getReadUseCount()  const { return readUseCount_; }
  int getWriteUseCount() const { return writeUseCount_; }
  void zeroUseCounts(void) {
    readUseCount_  = 0;
    writeUseCount_ = 0;
  }
  
 private:
  QString name_, description_;
  bool readWaypoints_, readTracks_, readRoutes_;
  bool writeWaypoints_, writeTracks_, writeRoutes_;
  bool fileFormat_, deviceFormat_, hidden_;
  QStringList extensions_;
  QList<FormatOption>inputOptions_;
  QList<FormatOption>outputOptions_;
  QString html_;
  static QString htmlBase_;
  int      readUseCount_;
  int      writeUseCount_;
  
};

#endif
