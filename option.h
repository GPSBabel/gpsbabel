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
  virtual const char* printable() const = 0;
  virtual void set(const char* c) = 0;
};

class OptionCString : public Option
{
public:
  /* Special Member Functions */
  OptionCString() = default;

  explicit(false) OptionCString(const char* c) : data(c) {}
  
  explicit(false) operator const char*() const { return data; }
  // Why can't the above be chained with QString constructor?  Instead we need implicit conversions.
/*
/home/tsteven4/work/option/xcsv.cc:1986:18: error: ambiguous overload for ‘operator=’ (operand types are ‘QString’ and ‘OptionCString’)
 1986 |     datum_name = opt_datum;
      |                  ^~~~~~~~~
/usr/include/x86_64-linux-gnu/qt6/QtCore/qstring.h:455:14: note: candidate: ‘QString& QString::operator=(const QString&)’
  455 |     QString &operator=(const QString &) noexcept;
      |              ^~~~~~~~
/usr/include/x86_64-linux-gnu/qt6/QtCore/qstring.h:459:5: note: candidate: ‘QString& QString::operator=(QString&&)’
  459 |     QT_MOVE_ASSIGNMENT_OPERATOR_IMPL_VIA_PURE_SWAP(QString)
      |     ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/usr/include/x86_64-linux-gnu/qt6/QtCore/qstring.h:1003:40: note: candidate: ‘QString& QString::operator=(const char*)’
 1003 |     QT_ASCII_CAST_WARN inline QString &operator=(const char *ch)
      |                                        ^~~~~~~~
*/

  // But the implict conversions cause
/*
/home/tsteven4/work/option/xcsv.cc: In member function ‘virtual void XcsvFormat::rd_init(const QString&)’:
/home/tsteven4/work/option/xcsv.cc:1885:59: error: cannot convert ‘OptionCString’ to ‘const QString&’
 1885 |     xcsv_style = new XcsvStyle(XcsvStyle::xcsv_read_style(styleopt));
      |                                                           ^~~~~~~~
      |                                                           |
      |                                                           OptionCString
/home/tsteven4/work/option/xcsv.cc:1845:43: note:   initializing argument 1 of ‘static XcsvStyle XcsvStyle::xcsv_read_style(const QString&)’
 1845 | XcsvStyle::xcsv_read_style(const QString& fname)
      |                            ~~~~~~~~~~~~~~~^~~~~
*/
  //explicit(true) operator QString() const { return data; }
  //explicit(true) operator QAnyStringView() const { return data; }

  option_t type() const override { return type_cstring; }
  bool has_value() const override { return data != nullptr; }
  void reset() override { data = nullptr; }
  bool isEmpty() const override { return ((data != nullptr) && (*data != '\0')); }
  const char* printable() const override { return data; }
  void set(const char* c) override { data = c;}

private:
  const char* data{nullptr};
};

class OptionBool : public Option
{
public:
  /* Special Member Functions */
  OptionBool() = default;

  explicit(false) OptionBool(const char* c) : data(c) {}
  
  explicit(false) operator const bool() const { return ((data != nullptr) && (*data != '0')); }

  option_t type() const override { return type_cstring; }
  bool has_value() const override { return data != nullptr; }
  void reset() override { data = nullptr; }
  bool isEmpty() const override { return ((data != nullptr) && (*data != '\0')); }
  const char* printable() const override { return data; }
  void set(const char* c) override { data = c;}

private:
  const char* data{nullptr};
};
#endif // OPTION_H_INCLUDED_
