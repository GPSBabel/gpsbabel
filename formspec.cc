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

#include <QtCore/QList>

#include "defs.h"

QList<format_specific_data*> fs_chain_copy(const QList<format_specific_data*>& source)
{
  QList<format_specific_data*> dest;
  for (const auto* item : source) {
    format_specific_data* copy;
    item->fscopy((void**)&copy, (void*)item);
    dest.append(copy);
  }
  return dest;
}

void fs_chain_destroy(QList<format_specific_data*>* chain)
{
  if (chain != nullptr) {
    while (!chain->isEmpty()) {
      format_specific_data* item = chain->takeFirst();
      item->fsdestroy(item);
    }
  }
}

format_specific_data* fs_chain_find(const QList<format_specific_data*>& chain, long type)
{
  for (auto* item : chain) {
    if (item->fstype == type) {
      return item;
    }
  }
  return nullptr;
}

void fs_chain_add(QList<format_specific_data*>* chain, format_specific_data* data)
{
  if (chain != nullptr) {
    chain->append(data);
  }
}
