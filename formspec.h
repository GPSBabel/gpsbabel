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
#ifndef FORMSPEC_H_INCLUDED_
#define FORMSPEC_H_INCLUDED_

#include <QList>         // for QList

enum FsType {
  kFsUnknown = 0L,
  kFsGpx = 0x67707800L,
  kFsAn1W = 0x616e3177L,
  kFsAn1L = 0x616e316cL,
  kFsAn1V = 0x616e3176L,
  kFsOzi = 0x6f7a6900L,
  kFsGmsd = 0x474d5344L,	/* GMSD = Garmin specific data */
  kFsQstarzBl1000 = 0x5173747aL,
  kFsLowranceusr4 = 0x615f234cL
};

struct FormatSpecificData {
  FormatSpecificData() = default;
  explicit FormatSpecificData(FsType type) : fs_type(type) {}
  FormatSpecificData(const FormatSpecificData&) = default;
  FormatSpecificData& operator=(const FormatSpecificData&) = default;
  FormatSpecificData(FormatSpecificData&&) = delete;
  FormatSpecificData& operator=(FormatSpecificData&&) = delete;
  virtual ~FormatSpecificData() = default;

  virtual FormatSpecificData* clone() const = 0;

  FsType fs_type{kFsUnknown};
};

class FormatSpecificDataList : private QList<FormatSpecificData*>
{
public:
  FormatSpecificDataList FsChainCopy() const;
  void FsChainDestroy();
  FormatSpecificData* FsChainFind(FsType type) const;
  void FsChainAdd(FormatSpecificData* data);
};

#endif // FORMSPEC_H_INCLUDED_
