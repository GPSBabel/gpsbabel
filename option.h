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

#include <QByteArray>  // for QByteArray
#include <QString>     // for QString, operator!=

class Option /* Abstract Class */
{
public:
  /* Types */
  enum option_t {
    type_cstring,
    type_boolean,
    type_string,
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
  [[nodiscard]] virtual option_t type() const = 0;
  [[nodiscard]] virtual bool has_value() const = 0;
  virtual void reset() = 0;
  [[nodiscard]] virtual bool isEmpty() const = 0;
  [[nodiscard]] virtual const QString& get() const = 0;
  virtual void set(const QString& s) = 0;
  virtual void set_id(const QString& id)
  {
  }

  /* Data Members */
  // I.25: Prefer empty abstract classes as interfaces to class hierarchies
  // https://isocpp.github.io/CppCoreGuidelines/CppCoreGuidelines#i25-prefer-empty-abstract-classes-as-interfaces-to-class-hierarchies
};

class OptionCString : public Option
{
public:
  /* Special Member Functions */
  OptionCString() = default;

  /* Traditionally a nullptr value indicated the option was not supplied.
   * This was convenient because a char* can be implicitly converted to bool,
   * although now we also have the equivalent member function has_value().
   * Because QByteArray::constData() != nullptr for a null QByteArray we
   * have to handle that case manually.
   */
  explicit(false) operator const char* () const
  {
    return value_.isNull()? nullptr : valueb_.constData();
  }

  [[nodiscard]] option_t type() const override
  {
    return type_cstring;
  }

  [[nodiscard]] bool has_value() const override
  {
    return !value_.isNull();
  }

  void reset() override
  {
    value_ = QString();
    valueb_ = QByteArray();
  }

  [[nodiscard]] bool isEmpty() const override
  {
    return value_.isEmpty();
  }

  [[nodiscard]] const QString& get() const override
  {
    return value_;
  }

  [[nodiscard]] const QByteArray& getba() const
  {
    return valueb_;
  }

  void set(const QString& s) override
  {
    value_ = s;
    valueb_ = s.toUtf8();
  }

private:
  QString value_;
  QByteArray valueb_;
};

class OptionString : public Option
{
public:
  /* Special Member Functions */
  OptionString() = default;

  explicit(false) operator const QString& () const
  {
    return value_;
  }

  explicit(false) operator bool () const
  {
    return !value_.isNull();
  }

  [[nodiscard]] option_t type() const override
  {
    return type_string;
  }

  [[nodiscard]] bool has_value() const override
  {
    return !value_.isNull();
  }

  void reset() override
  {
    value_ = QString();
  }

  [[nodiscard]] bool isEmpty() const override
  {
    return value_.isEmpty();
  }

  [[nodiscard]] const QString& get() const override
  {
    return value_;
  }

  void set(const QString& s) override
  {
    value_ = s;
  }

  void set_id(const QString& id) override
  {
    id_ = id;
  }

// TODO: add register conversion options for integer base conversion.
// TODO: add register conversion option for trailing data.
// TODO: use conversion options for Vecs::assign_option checks.
// TODO: save result of Vecs assign_option check and provide it to users
// We use overloads instead of default parameters to enable tool visibility into different usages.
  int toInt() const;
  int toInt(bool* ok) const;
  int toInt(bool* ok, QString* end, int base) const;
  double toDouble() const;
  double toDouble(bool* ok) const;
  double toDouble(bool* ok, QString* end) const;

private:
  QString value_;
  QString id_;
};

class OptionBool : public Option
{
public:
  /* Special Member Functions */
  OptionBool() = default;

  /* Traditionally un-supplied bool options without default are considered to be false. */
  explicit(false) operator bool() const
  {
    return (!value_.isNull() && (value_ != '0'));
  }

  [[nodiscard]] option_t type() const override
  {
    return type_boolean;
  }

  /* Note that has_value can be used to distinguish an option that wasn't supplied
   * from one that was supplied and is considered false by Vecs::assign_option.
   */
  [[nodiscard]] bool has_value() const override
  {
    return !value_.isNull();
  }

  void reset() override
  {
    value_ = QString();
  }

  [[nodiscard]] bool isEmpty() const override
  {
    return value_.isEmpty();
  }

  [[nodiscard]] const QString& get() const override
  {
    return value_;
  }

  void set(const QString& s) override
  {
    value_ = s;
  }

private:
  QString value_;
};
#endif // OPTION_H_INCLUDED_
