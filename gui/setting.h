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
//  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301,
//  USA.
//
//------------------------------------------------------------------------
#ifndef SETTING_H
#define SETTING_H

#include <QSettings>
#include <QDate>


//------------------------------------------------------------------------
class VarSetting
{
public:

  /* Special Member Functions */

  VarSetting() = default;
  VarSetting(const VarSetting&) = default;
  VarSetting& operator=(const VarSetting&) = default;
  VarSetting(VarSetting&&) = default;
  VarSetting& operator=(VarSetting&&) = default;
  virtual ~VarSetting() = default;

  /* Member Functions */

  virtual void saveSetting(QSettings&) = 0;
  virtual void restoreSetting(QSettings&) = 0;
};


//------------------------------------------------------------------------
class IntSetting: public VarSetting
{
public:

  /* Special Member Functions */

  IntSetting(const QString& name, int& var):  name_(name), var_(var) {}

  /* Member Functions */

  void saveSetting(QSettings& st) override
  {
    st.setValue(name_, var_);
  }
  void restoreSetting(QSettings& st) override
  {
    if (st.contains(name_)) {
      var_ = st.value(name_).toInt();
    }
  }

private:

  /* Data Members */

  QString name_;
  int& var_;
};

//------------------------------------------------------------------------
class DoubleSetting: public VarSetting
{
public:

  /* Special Member Functions */

  DoubleSetting(const QString& name, double& var):  name_(name), var_(var) {}

  /* Member Functions */

  void saveSetting(QSettings& st) override
  {
    st.setValue(name_, var_);
  }
  void restoreSetting(QSettings& st) override
  {
    if (st.contains(name_)) {
      var_ = st.value(name_).toDouble();
    }
  }

private:

  /* Data Members */

  QString name_;
  double& var_;
};

//------------------------------------------------------------------------
class StringSetting: public VarSetting
{
public:

  /* Special Member Functions */

  StringSetting(const QString& name, QString& var):  name_(name), var_(var) {}

  /* Member Functions */

  void saveSetting(QSettings& st) override
  {
    st.setValue(name_, var_);
  }
  void restoreSetting(QSettings& st) override
  {
    if (st.contains(name_)) {
      var_ = st.value(name_).toString();
    }
  }

private:

  /* Data Members */

  QString name_;
  QString& var_;
};

//------------------------------------------------------------------------
class BoolSetting: public VarSetting
{
public:

  /* Special Member Functions */

  BoolSetting(const QString& name, bool& var):  name_(name), var_(var) {}

  /* Member Functions */

  void saveSetting(QSettings& st) override
  {
    st.setValue(name_, var_);
  }
  void restoreSetting(QSettings& st) override
  {
    if (st.contains(name_)) {
      var_ = st.value(name_).toBool();
    }
  }

private:

  /* Data Members */

  QString name_;
  bool& var_;
};

//------------------------------------------------------------------------
class DateTimeSetting: public VarSetting
{
public:

  /* Special Member Functions */

  DateTimeSetting(const QString& name, QDateTime& var): name_(name), var_(var) {}

  /* Member Functions */

  void saveSetting(QSettings& st) override
  {
    st.setValue(name_, var_);
  }
  void restoreSetting(QSettings& st) override
  {
    if (st.contains(name_)) {
      var_ = st.value(name_).toDateTime();
    }
  }

private:

  /* Data Members */

  QString name_;
  QDateTime& var_;
};


//------------------------------------------------------------------------
class SettingGroup
{
public:

  /* Special Member Functions */

  SettingGroup() = default;
  SettingGroup(const SettingGroup&) = default;
  // copy assignment would need to free memory like dtor.
  SettingGroup& operator=(const SettingGroup&) = delete;
  SettingGroup(SettingGroup&&) = default;
  // move assignment would need to free memory like dtor.
  SettingGroup& operator=(SettingGroup&&) = delete;
  ~SettingGroup()
  {
    for (auto& i : settingGroup_) {
      delete i;
    }
  }

  /* Member Functions */

  void saveSettings(QSettings& st)
  {
    for (auto& i : settingGroup_) {
      i->saveSetting(st);
    }
  }
  void restoreSettings(QSettings& st)
  {
    for (auto& i : settingGroup_) {
      i->restoreSetting(st);
    }
  }
  void addVarSetting(VarSetting* vs)
  {
    settingGroup_ << vs;
  }

private:

  /* Data Members */

  QList <VarSetting*> settingGroup_;
};

#endif
