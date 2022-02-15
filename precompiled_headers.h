/*
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

// This list is from defs.h.  Adding other commonly used headers doesn't help.

#if defined __cplusplus
#include <algorithm>              // for sort, stable_sort
#include <cmath>                  // for M_PI
#include <cstdarg>                // for va_list
#include <cstddef>                // for NULL, nullptr_t, size_t
#include <cstdint>                // for int32_t, uint32_t
#include <cstdio>                 // for NULL, fprintf, FILE, stdout
#include <ctime>                  // for time_t
#include <optional>               // for optional
#include <utility>                // for move

#include <QDebug>                 // for QDebug
#include <QList>                  // for QList, QList<>::const_reverse_iterator, QList<>::reverse_iterator
#include <QScopedPointer>         // for QScopedPointer
#include <QString>                // for QString
#include <QTextCodec>             // for QTextCodec
#include <QVector>                // for QVector
#include <Qt>                     // for CaseInsensitive
#include <QtGlobal>               // for foreach
#endif
