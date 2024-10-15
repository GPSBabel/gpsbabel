/*
    Copyright (C) 2024 Robert Lipe, robertlipe+source@gpsbabel.org

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
#ifndef OPTION_H_INCLUDED_
#define OPTION_H_INCLUDED_

#include "defs.h"


class Option
{
public:
  /* Types */
  enum option_t {
    type_cstring,
    type_boolean,
  };

  /* Special Member Functions */
  Option() = default;
  // Provide virtual public destructor to avoid undefined behavior when
  // an object of derived class type is deleted through a pointer to
  // its base class type.
  // https://wiki.sei.cmu.edu/confluence/display/cplusplus/OOP52-CPP.+Do+not+delete+a+polymorphic+object+without+a+virtual+destructor
  virtual ~Option() = default;
  // And that requires us to explicitly default or delete the move and copy operations.
  // To prevent slicing we delete them.
  // https://github.com/isocpp/CppCoreGuidelines/blob/master/CppCoreGuidelines.md#c21-if-you-define-or-delete-any-default-operation-define-or-delete-them-all.
  Option(const Option&) = delete;
  Option& operator=(const Option&) = delete;
  Option(Option&&) = delete;
  Option& operator=(Option&&) = delete;

  /* Member Functions */
  virtual option_t type() const = 0;
  virtual bool has_value() const = 0;
  virtual void reset() = 0;
  virtual bool isEmpty() const = 0;
  virtual const QString& value() const = 0;
  virtual void set(const QString& s) = 0;
};

class OptionCString : public Option
{
public:
  /* Special Member Functions */
  OptionCString() = default;

  explicit(false) OptionCString(const QString& s) : value_(s), valueb_(s.toUtf8()) {}
  
  explicit(false) operator const char*() const { return value_.isNull()? nullptr : valueb_.constData(); }

  option_t type() const override { return type_cstring; }
  bool has_value() const override { return !value_.isNull(); }
  void reset() override { value_ = QString(); valueb_ = QByteArray(); }
  bool isEmpty() const override { return value_.isEmpty(); }
  const QString& value() const override { return value_; }
  void set(const QString& s) override { value_ = s; valueb_ = s.toUtf8(); }

private:
  QString value_;
  QByteArray valueb_;
};

class OptionBool : public Option
{
public:
  /* Special Member Functions */
  OptionBool() = default;

  explicit(false) OptionBool(const QString& s) : value_(s) {}
  
  explicit(false) operator bool() const { return (!value_.isNull() && (value_ != '0')); }

  option_t type() const override { return type_cstring; }
  bool has_value() const override { return !value_.isNull(); }
  void reset() override { value_ = QString(); }
  bool isEmpty() const override { return value_.isEmpty(); }
  const QString& value() const override { return value_; }
  void set(const QString& s) override { value_ = s;}

private:
  QString value_;
};
#endif // OPTION_H_INCLUDED_
