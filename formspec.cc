/*
    Functions to manage the FormatSpecificData chain

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

#include <QtCore/QList>  // for QList

#include "defs.h"
#include "formspec.h"    // for FormatSpecificData, FsChainAdd, FsChainCopy, FsChainDestroy, FsChainFind

FormatSpecificDataList FormatSpecificDataList::FsChainCopy() const
{
  FormatSpecificDataList dest;
  for (const auto* item : *this) {
    dest.append(item->clone());
  }
  return dest;
}

void FormatSpecificDataList::FsChainDestroy()
{
  while (!isEmpty()) {
    delete takeFirst();
  }
}

FormatSpecificData* FormatSpecificDataList::FsChainFind(FsType type) const
{
  for (auto* item : *this) {
    if (item->fs_type == type) {
      return item;
    }
  }
  return nullptr;
}

void FormatSpecificDataList::FsChainAdd(FormatSpecificData* data)
{
  if (data != nullptr) {
    append(data);
  }
}
