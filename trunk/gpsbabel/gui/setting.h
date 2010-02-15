// -*- C++ -*-
// $Id: setting.h,v 1.3 2010-02-15 02:57:00 robertl Exp $
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
#ifndef SETTING_H
#define SETTING_H

#include <QSettings>
#include <QDate>


//------------------------------------------------------------------------
class VarSetting {
 public:
  VarSetting() {};
  virtual ~VarSetting() {};

  virtual void saveSetting(QSettings &) = 0;
  virtual void restoreSetting(QSettings &) = 0;
};


//------------------------------------------------------------------------
class IntSetting: public VarSetting
{
 public:
  IntSetting(const QString &name, int &var): VarSetting(), name(name), var(var) { }
  void saveSetting(QSettings &st) {st.setValue(name, var); }
  void restoreSetting(QSettings &st) {if (st.contains(name)) var = st.value(name).toInt(); }

 private:
  QString name;
  int &var;
};

//------------------------------------------------------------------------
class DoubleSetting: public VarSetting
{
 public:
  DoubleSetting(const QString &name, double &var): VarSetting(), name(name), var(var) { }
  void saveSetting(QSettings &st) {st.setValue(name, var); }
  void restoreSetting(QSettings &st) {if (st.contains(name)) var = st.value(name).toDouble(); }

 private:
  QString name;
  double &var;
};

//------------------------------------------------------------------------
class StringSetting: public VarSetting
{
 public:
  StringSetting(const QString &name, QString &var): VarSetting(), name(name), var(var) { }
  void saveSetting(QSettings &st) {st.setValue(name, var); }
  void restoreSetting(QSettings &st) {if (st.contains(name)) var = st.value(name).toString(); }

 private:
  QString name;
  QString &var;
};

//------------------------------------------------------------------------
class BoolSetting: public VarSetting
{
 public:
  BoolSetting(const QString &name, bool &var): VarSetting(), name(name), var(var) { }
  void saveSetting(QSettings &st) {st.setValue(name, var); }
  void restoreSetting(QSettings &st) {if (st.contains(name)) var = st.value(name).toBool(); }

 private:
  QString name;
  bool &var;
};

//------------------------------------------------------------------------
class DateTimeSetting: public VarSetting
{
 public:
  DateTimeSetting(const QString &name, QDateTime &var):VarSetting(), name(name), var(var) { }
  void saveSetting(QSettings &st) {st.setValue(name, var); }
  void restoreSetting(QSettings &st) {if (st.contains(name)) var = st.value(name).toDateTime(); }

 private:
  QString name;
  QDateTime &var;
};


//------------------------------------------------------------------------
class SettingGroup {
 public:
  SettingGroup() {}
  ~SettingGroup() {
    for (int i=0; i<group.size(); i++) 
      delete group[i];
  }

  void saveSettings(QSettings &st) {
    for (int i=0; i<group.size(); i++) 
      group[i]->saveSetting(st);
  }
  void restoreSettings(QSettings &st) {
    for (int i=0; i<group.size(); i++) 
      group[i]->restoreSetting(st);
  }

  void addVarSetting(VarSetting *vs) {
    group << vs;
  }

 private:
  QList <VarSetting *> group;
};

#endif


