/*
    Describe vectors containing filter operations.

    Copyright (C) 2002-2014 Robert Lipe, robertlipe+source@gpsbabel.org

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
#ifndef FILTER_VECS_H_INCLUDED_
#define FILTER_VECS_H_INCLUDED_

#include <QString>          // for QString

#include "defs.h"           // for arglist_t
#include "filter.h"         // for Filter


class FilterVecs
{
// Meyers Singleton
public:

  /* Types */

  using FilterFactory = Filter* (*)();

  class fltinfo_t {
  public:

    bool isDynamic() {
      return factory != nullptr;
    }
    explicit operator bool() const {
      return ((flt != nullptr) || (factory != nullptr));
    }
    Filter* operator->() const {
      return flt;
    }

    Filter* flt{nullptr};
    QString fltname;
    QStringList options;
    FilterFactory factory{nullptr};
  };

  /* Special Member Functions */

  static FilterVecs& Instance();
  FilterVecs(const FilterVecs&) = delete;
  FilterVecs& operator= (const FilterVecs&) = delete;
  FilterVecs(FilterVecs&&) = delete;
  FilterVecs& operator=(FilterVecs&&) = delete;

  /* Member Functions */

  static void prepare_filter(const fltinfo_t& fltdata);
  fltinfo_t find_filter_vec(const QString& vecname);
  static void free_filter_vec(fltinfo_t& filter);
  static void init_filter_vec(Filter* flt);
  void init_filter_vecs();
  static void exit_filter_vec(Filter* flt);
  void exit_filter_vecs();
  void disp_filter_vecs() const;
  void disp_filter_vec(const QString& vecname) const;
  void disp_filters(int version) const;
  bool validate_filters() const;

private:
  /* Types */

  struct Impl;                   // Not defined here

  struct fl_vecs_t {
    Filter* vec{nullptr};
    QString name;
    QString desc;
    FilterFactory factory{nullptr};
  };

  /* Special Member Functions */

  explicit FilterVecs(Impl* i) : d_ptr_(i) {}
  ~FilterVecs() = default;

  /* Member Functions */

  static void disp_help_url(const fl_vecs_t& vec, const arglist_t* arg);
  static void disp_v1(const fl_vecs_t& vec);
  static bool validate_filter_vec(const fl_vecs_t& vec);

  /* Data Members */

  Impl* d_ptr_;                  // Opaque pointer
};
#endif // FILTER_VECS_H_INCLUDED_
