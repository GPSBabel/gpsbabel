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

#include <QDateTime>  // for QDateTime
#include <QSettings>  // for QSettings
#include <QString>    // for QAnyStringView::QAnyStringView, QString
#include <QVariant>   // for QVariant
#include <memory>     // for unique_ptr
#include <utility>    // for move
#include <vector>     // for vector


//------------------------------------------------------------------------
class VarSetting
{
public:
  VarSetting() = default;
  /* Reference data members C.12 */
  VarSetting(const VarSetting &) = delete;
  VarSetting &operator=(const VarSetting &) = delete;
  VarSetting(VarSetting &&) = delete;
  VarSetting &operator=(VarSetting &&) = delete;
  virtual ~VarSetting() = default;

  virtual void saveSetting(QSettings&) = 0;
  virtual void restoreSetting(QSettings&) = 0;
};


//------------------------------------------------------------------------
class IntSetting: public VarSetting
{
public:
  IntSetting(const QString& name, int& var):  name_(name), var_(var) { }
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
  QString name_;
  int& var_;
};

//------------------------------------------------------------------------
class DoubleSetting: public VarSetting
{
public:
  DoubleSetting(const QString& name, double& var):  name_(name), var_(var) { }
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
  QString name_;
  double& var_;
};

//------------------------------------------------------------------------
class StringSetting: public VarSetting
{
public:
  StringSetting(const QString& name, QString& var):  name_(name), var_(var) { }
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
  QString name_;
  QString& var_;
};

//------------------------------------------------------------------------
class BoolSetting: public VarSetting
{
public:
  BoolSetting(const QString& name, bool& var):  name_(name), var_(var) { }
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
  QString name_;
  bool& var_;
};

//------------------------------------------------------------------------
class DateTimeSetting: public VarSetting
{
public:
  DateTimeSetting(const QString& name, QDateTime& var): name_(name), var_(var) { }
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
  QString name_;
  QDateTime& var_;
};


//------------------------------------------------------------------------
class SettingGroup
{
public:
  SettingGroup() = default;
  /* Not copyable with unique_ptr */
  SettingGroup(const SettingGroup &) = delete;
  SettingGroup &operator=(const SettingGroup &) = delete;
  SettingGroup(SettingGroup &&) = delete;
  SettingGroup &operator=(SettingGroup &&) = delete;
  ~SettingGroup() = default;

  void saveSettings(QSettings& st)
  {
    for (const auto& setting : settingGroup_) {
      setting->saveSetting(st);
    }
  }
  void restoreSettings(QSettings& st)
  {
    for (const auto& setting : settingGroup_) {
      setting->restoreSetting(st);
    }
  }

  void addVarSetting(std::unique_ptr<VarSetting> vs)
  {
    settingGroup_.push_back(std::move(vs));
  }

private:
  std::vector<std::unique_ptr<VarSetting>> settingGroup_;
};
#endif
