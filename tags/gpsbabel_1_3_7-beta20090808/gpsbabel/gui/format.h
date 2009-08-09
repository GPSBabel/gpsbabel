// -*- C++ -*-
// $Id: format.h,v 1.1 2009-07-05 21:14:56 robertl Exp $
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
  
  FormatOption(): name(QString()), description(QString()), type(OPTbool), 
		  defaultValue(QVariant()), 
		  minValue(QVariant()), maxValue(QVariant()), 
		  html(QString()), value(QVariant()), selected(false)
  {
  }
  FormatOption(const QString &name, 
	       const QString &description,
	       optionType type,
	       QVariant defaultValue = QVariant(),
	       QVariant minValue = QVariant(),
	       QVariant maxValue = QVariant(),
	       QString html = QString()
	       ): name(name), description(description), type(type),
		  defaultValue(defaultValue), minValue(minValue), maxValue(maxValue), html(html)
  {
    value = QVariant();
    selected = false;
  }
  FormatOption(const FormatOption & c)
    : name(c.name), description(c.description), type(c.type),
      defaultValue(c.defaultValue), minValue(c.minValue), maxValue(c.maxValue), html(c.html),
      value(c.value), selected(c.selected)
  {
  }
  
  QString  getName() const {return name; }
  QString  getDescription() const {return description; }
  optionType getType() const {return type; }
  QVariant getValue() const    { return value; }
  bool     getSelected() const {return selected; }
  QVariant getMinValue() const {return minValue; }
  QVariant getMaxValue() const {return maxValue; }
  QVariant getDefaultValue() const {return defaultValue; }

  void setValue(QVariant v) { value = v; };
  void setSelected(bool v)  { selected = v; };

private:
  QString name;
  QString description;
  optionType type;
  QVariant defaultValue;
  QVariant minValue;
  QVariant maxValue;
  QString  html;
  QVariant value;
  bool     selected;
};


//------------------------------------------------------------------------
class Format 
{
 public:
  Format():name(QString()),
	   description(QString()),
	   readWaypoints(false),
	   readTracks(false),
	   readRoutes(false),
	   writeWaypoints(false),
	   writeTracks(false),
	   writeRoutes(false),
	   fileFormat(false),
	   deviceFormat(false),
	   extensions(QStringList()) 
  {
    inputOptions.clear();
    outputOptions.clear();
  };

  Format(const QString &name,
	 const QString &description,
	 bool readWaypoints, bool readTracks, bool readRoutes,
	 bool writeWaypoints, bool writeTracks, bool writeRoutes,
	 bool fileFormat, bool deviceFormat,
	 const QStringList &extensions,
	 QList<FormatOption> &inputOptions, 
	 QList<FormatOption> &outputOptions):
    name(name), description(description),
    readWaypoints(readWaypoints), readTracks(readTracks), readRoutes(readRoutes),
    writeWaypoints(writeWaypoints), writeTracks(writeTracks), writeRoutes(writeRoutes),
    fileFormat(fileFormat), deviceFormat(deviceFormat),
    extensions(extensions),
    inputOptions(inputOptions),
    outputOptions(outputOptions)
  {
  }

  Format(const Format &c):
    name(c.name), description(c.description),
    readWaypoints(c.readWaypoints), readTracks(c.readTracks), readRoutes(c.readRoutes),
    writeWaypoints(c.writeWaypoints), writeTracks(c.writeTracks), writeRoutes(c.writeRoutes),
    fileFormat(c.fileFormat), deviceFormat(c.deviceFormat),
    extensions(c.extensions),
    inputOptions(c.inputOptions),
    outputOptions(c.outputOptions)
  {
  }

  ~Format() {};

  bool isReadWaypoints() const { return readWaypoints; };
  bool isReadTracks() const    { return readTracks; };
  bool isReadRoutes() const    { return readRoutes; };
  bool isReadSomething() const { 
    return isReadWaypoints() || isReadTracks () || isReadRoutes(); 
  };

  bool isWriteWaypoints() const { return writeWaypoints; };
  bool isWriteTracks() const    { return writeTracks; };
  bool isWriteRoutes() const    { return writeRoutes; };
  bool isWriteSomething() const { 
    return isWriteWaypoints() || isWriteTracks () || isWriteRoutes(); 
  };

  QString getName() const           { return name; };
  QString getDescription() const    { return description; };
  QStringList getExtensions() const { return extensions; };
  const QList<FormatOption> &getInputOptions()  const { return inputOptions; };
  const QList<FormatOption> &getOutputOptions() const { return outputOptions; };

  QList<FormatOption> *getInputOptionsRef()  { return &inputOptions; };
  QList<FormatOption> *getOutputOptionsRef() { return &outputOptions; };

  bool isDeviceFormat() const { return deviceFormat; };
  bool isFileFormat() const { return   fileFormat; };
  
  void saveSettings(QSettings &settings);
  void restoreSettings(QSettings &settings);
  void setToDefault();

 private:
  QString name, description;
  bool readWaypoints, readTracks, readRoutes;
  bool writeWaypoints, writeTracks, writeRoutes;
  bool fileFormat, deviceFormat;
  QStringList extensions;
  QList<FormatOption>inputOptions;
  QList<FormatOption>outputOptions;
  
};

#endif
