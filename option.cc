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
#include "option.h"

#include <QDebug>              // for QDebug
#include <QString>             // for QString

#include <cstddef>             // for size_t
#include <stdexcept>           // for invalid_argument, out_of_range
#include <string>              // for stoi

#include "defs.h"              // for fatal
#include "src/core/logging.h"  // for FatalMsg


int OptionString::toInt(QString* end, int base)
{
  const QString& str = this->get();
  auto ss = str.toStdString();
  size_t pos = 0;
  int result = 0;
  try {
    result = stoi(ss, &pos, base);
  } catch (const std::invalid_argument&) {
    fatal(FatalMsg() << "conversion to integer failed: invalid argument" << str);
  } catch (const std::out_of_range&) {
    fatal(FatalMsg() << "conversion to integer failed: out of range" << str);
  } catch (...) {
    fatal(FatalMsg() << "conversion to integer failed: unknown exception" << str);
  }

  QString remainder = QString::fromStdString(ss.erase(0, pos));
  if ((end == nullptr) && !remainder.trimmed().isEmpty()) {
    fatal(FatalMsg() << "conversion to integer failed: conversion of"  << str <<
          "failed due to unexpected trailing data" << remainder);
  }
  if (end != nullptr) { // return possibly empty trailing portion of str
    *end = remainder;
  }

  return result;
}

double OptionString::toDouble(QString* end)
{
  const QString& str = this->get();
  auto ss = str.toStdString();
  size_t pos = 0;
  double result = 0.0;
  try {
    result = stod(ss, &pos);
  } catch (const std::invalid_argument&) {
    fatal(FatalMsg() << "conversion to double failed: invalid argument" << str);
  } catch (const std::out_of_range&) {
    fatal(FatalMsg() << "conversion to double failed: out of range" << str);
  } catch (...) {
    fatal(FatalMsg() << "conversion to double failed: unknown exception" << str);
  }

  QString remainder = QString::fromStdString(ss.erase(0, pos));
  if ((end == nullptr) && !remainder.trimmed().isEmpty()) {
    fatal(FatalMsg() << "conversion to double failed: conversion of"  << str <<
          "failed due to unexpected trailing data" << remainder);
  }
  if (end != nullptr) { // return possibly empty trailing portion of str
    *end = remainder;
  }

  return result;
}
