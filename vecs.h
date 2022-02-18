/*
    Describe vectors containing file operations.

    Copyright (C) 2002, 2004, 2005, 2006, 2007 Robert Lipe, robertlipe+source@gpsbabel.org

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
#ifndef VECS_H_INCLUDED_
#define VECS_H_INCLUDED_

#include <cstdint>
#include <memory>

#include <QString>              // for QString
#include <QStringList>          // for QStringList
#include <QVector>              // for QVector<>::iterator, QVector

#include "defs.h"
#include "format.h"


class Vecs
{
// Meyers Singleton
public:
  static Vecs& Instance()
  {
    static Vecs instance;
    return instance;
  }
  Vecs(const Vecs&) = delete;
  Vecs& operator= (const Vecs&) = delete;
  Vecs(Vecs&&) = delete;
  Vecs& operator=(Vecs&&) = delete;

private:
  Vecs();
  ~Vecs();

private:
  struct Impl;                   // Not defined here
  std::unique_ptr<Impl> d_ptr_;  // Opaque pointer

  struct vecs_t {
    Format* vec;
    QString name;
    QString desc;
    QString extensions; // list of possible extensions separated by '/', first is output default for GUI.
    QString parent;
  };

  struct arginfo_t {
    arginfo_t() = default;
    explicit arginfo_t(const arglist_t& arg) :
      argstring(arg.argstring),
      helpstring(arg.helpstring),
      defaultvalue(arg.defaultvalue),
      argtype(arg.argtype),
      minvalue(arg.minvalue),
      maxvalue(arg.maxvalue)
    {}

    QString argstring;
    QString helpstring;
    QString defaultvalue;
    uint32_t argtype{ARGTYPE_UNKNOWN};
    QString minvalue;
    QString maxvalue;
  };

  struct vecinfo_t {
    QString name;
    QString desc;
    QString extensions;
    QString parent;
    ff_type type{ff_type_file};
    QVector<ff_cap> cap;
    QVector<arginfo_t> arginfo;
  };

  struct style_vec_t {
    QString name;
    QString style_filename;
  };


public:
  void init_vecs();
  void exit_vecs();
  static void assign_option(const QString& module, arglist_t* arg, const char* val);
  static void disp_vec_options(const QString& vecname, const QVector<arglist_t>* args);
  static void validate_options(const QStringList& options, const QVector<arglist_t>* args, const QString& name);
  static QString get_option(const QStringList& options, const char* argname);
  Format* find_vec(const QString& vecname);
  void disp_vecs() const;
  void disp_vec(const QString& vecname) const;
  static const char* name_option(uint32_t type);
  void disp_formats(int version) const;
  static bool validate_args(const QString& name, const QVector<arglist_t>* args);
  bool validate_formats() const;

private:
  static int is_integer(const char* c);
  static QVector<style_vec_t> create_style_vec();
  QVector<vecinfo_t> sort_and_unify_vecs() const;
  static void disp_v1(ff_type t);
  static void disp_v2(const vecinfo_t& v);
  static void disp_help_url(const vecinfo_t& vec, const QString& argstring);
  static void disp_v3(const vecinfo_t& vec);
  static bool validate_vec(const vecs_t& vec);

private:
  QVector<style_vec_t> style_list;

};
#endif // VECS_H_INCLUDED_
