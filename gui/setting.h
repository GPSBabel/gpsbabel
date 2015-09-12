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
  IntSetting(const QString &name, int &var): VarSetting(), name_(name), var_(var) { }
  void saveSetting(QSettings &st) {st.setValue(name_, var_); }
  void restoreSetting(QSettings &st) {if (st.contains(name_)) var_ = st.value(name_).toInt(); }

 private:
  QString name_;
  int &var_;
};

//------------------------------------------------------------------------
class DoubleSetting: public VarSetting
{
 public:
  DoubleSetting(const QString &name, double &var): VarSetting(), name_(name), var_(var) { }
  void saveSetting(QSettings &st) {st.setValue(name_, var_); }
  void restoreSetting(QSettings &st) {if (st.contains(name_)) var_ = st.value(name_).toDouble(); }

 private:
  QString name_;
  double &var_;
};

//------------------------------------------------------------------------
class StringSetting: public VarSetting
{
 public:
  StringSetting(const QString &name, QString &var): VarSetting(), name_(name), var_(var) { }
  void saveSetting(QSettings &st) {st.setValue(name_, var_); }
  void restoreSetting(QSettings &st) {if (st.contains(name_)) var_ = st.value(name_).toString(); }

 private:
  QString name_;
  QString &var_;
};

//------------------------------------------------------------------------
class BoolSetting: public VarSetting
{
 public:
  BoolSetting(const QString &name, bool &var): VarSetting(), name_(name), var_(var) { }
  void saveSetting(QSettings &st) {st.setValue(name_, var_); }
  void restoreSetting(QSettings &st) {if (st.contains(name_)) var_ = st.value(name_).toBool(); }

 private:
  QString name_;
  bool &var_;
};

//------------------------------------------------------------------------
class DateTimeSetting: public VarSetting
{
 public:
  DateTimeSetting(const QString &name, QDateTime &var):VarSetting(), name_(name), var_(var) { }
  void saveSetting(QSettings &st) {st.setValue(name_, var_); }
  void restoreSetting(QSettings &st) {if (st.contains(name_)) var_ = st.value(name_).toDateTime(); }

 private:
  QString name_;
  QDateTime &var_;
};


//------------------------------------------------------------------------
class SettingGroup {
 public:
  SettingGroup() {}
  ~SettingGroup() {
    for (int i=0; i< settingGroup_.size(); i++)
      delete settingGroup_[i];
  }

  void saveSettings(QSettings &st) {
    for (int i=0; i< settingGroup_.size(); i++)
      settingGroup_[i]->saveSetting(st);
  }
  void restoreSettings(QSettings &st) {
    for (int i=0; i< settingGroup_.size(); i++)
      settingGroup_[i]->restoreSetting(st);
  }

  void addVarSetting(VarSetting *vs) {
    settingGroup_ << vs;
  }

 private:
  QList <VarSetting *> settingGroup_;
};

#endif


