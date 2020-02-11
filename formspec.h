/*
    Functions to manage the format_specific_data chain

    Copyright (C) 2005 Ron Parker and Robert Lipe.

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
#ifndef FORMSPEC_H_INCLUDED_
#define FORMSPEC_H_INCLUDED_

#include <QtCore/QList>  // for QList

enum FsType {
  FS_UNKNOWN = 0L,
  FS_GPX = 0x67707800L,
  FS_AN1W = 0x616e3177L,
  FS_AN1L = 0x616e316cL,
  FS_AN1V = 0x616e3176L,
  FS_OZI = 0x6f7a6900L,
  FS_GMSD = 0x474d5344L,	/* GMSD = Garmin specific data */
  FS_LOWRANCEUSR4 = 0x615f234cL
};

using fs_destroy = void (*)(void*);
using fs_copy = void (*)(void**, const void*);

struct format_specific_data {
  format_specific_data() = default;
  virtual ~format_specific_data() = default;
  format_specific_data(const format_specific_data& other) = default;
  format_specific_data& operator=(const format_specific_data&) = default;
  format_specific_data(format_specific_data&&) = delete;
  format_specific_data& operator=(format_specific_data&&) = delete;

  FsType fstype{FS_UNKNOWN};

  fs_destroy fsdestroy{nullptr};
  fs_copy fscopy{nullptr};
};

QList<format_specific_data*> fs_chain_copy(const QList<format_specific_data*>& source);
void fs_chain_destroy(QList<format_specific_data*>* chain);
format_specific_data* fs_chain_find(const QList<format_specific_data*>& chain, FsType type);
void fs_chain_add(QList<format_specific_data*>* chain, format_specific_data* data);

#endif // FORMSPEC_H_INCLUDED_
