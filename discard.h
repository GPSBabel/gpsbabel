/*
    Discard points based on high Degree of Precision (DOP) values.

    Copyright (C) 2005-2014 Robert Lipe, robertlipe+source@gpsbabel.org

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

#ifndef DISCARD_H_INCLUDED_
#define DISCARD_H_INCLUDED_

#include <QRegularExpression>  // for QRegularExpression
#include <QString>             // for QString
#include <QVector>             // for QVector

#include "defs.h"              // for arglist_t, ARG_NOMINMAX, ARGTYPE_BEGIN_REQ, ARGTYPE_STRING, ARGTYPE_BOOL, ARGTYPE_INT, ARGTYPE_FLOAT, route_head, ARGTYPE_END_REQ, Waypoint, gpsdata_type
#include "filter.h"            // for Filter

#if FILTERS_ENABLED
class DiscardFilter:public Filter
{
public:
  [[nodiscard]] QVector<arglist_t>* get_args() override
  {
    return &args;
  }
  void init() override;
  void process() override;

private:
  /* Member Functions */

  void fix_process_wpt(const Waypoint* wpt);
  static QRegularExpression generateRegExp(const QString& glob_pattern);

  /* Data Members */

  char* hdopopt = nullptr;
  char* vdopopt = nullptr;
  char* andopt = nullptr;
  char* satopt = nullptr;
  char* fixnoneopt = nullptr;
  char* fixunknownopt = nullptr;
  char* eleminopt = nullptr;
  char* elemaxopt = nullptr;
  char* nameopt = nullptr;
  QRegularExpression name_regex;
  char* descopt = nullptr;
  QRegularExpression desc_regex;
  char* cmtopt = nullptr;
  QRegularExpression cmt_regex;
  char* iconopt = nullptr;
  QRegularExpression icon_regex;

  double hdopf{};
  double vdopf{};
  int satpf{};
  int eleminpf{};
  int elemaxpf{};

  QVector<arglist_t> args = {
    {
      "hdop", &hdopopt, "Suppress points with higher hdop",
      "-1.0", ARGTYPE_BEGIN_REQ | ARGTYPE_FLOAT, ARG_NOMINMAX, nullptr
    },
    {
      "vdop", &vdopopt, "Suppress points with higher vdop",
      "-1.0", ARGTYPE_END_REQ | ARGTYPE_FLOAT, ARG_NOMINMAX, nullptr
    },
    {
      "hdopandvdop", &andopt, "Link hdop and vdop suppression with AND",
      nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    {
      "sat", &satopt, "Minimum sats to keep points",
      "-1.0", ARGTYPE_BEGIN_REQ | ARGTYPE_INT, ARG_NOMINMAX, nullptr
    },
    {
      "fixnone", &fixnoneopt, "Suppress points without fix",
      nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    {
      "fixunknown", &fixunknownopt, "Suppress points with unknown fix",
      nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    {
      "elemin", &eleminopt, "Suppress points below given elevation in meters",
      nullptr, ARGTYPE_BEGIN_REQ | ARGTYPE_INT, ARG_NOMINMAX, nullptr
    },
    {
      "elemax", &elemaxopt, "Suppress points above given elevation in meters",
      nullptr, ARGTYPE_BEGIN_REQ | ARGTYPE_INT, ARG_NOMINMAX, nullptr
    },
    {
      "matchname", &nameopt,
      "Suppress points where name matches given name", nullptr, ARGTYPE_STRING,
      ARG_NOMINMAX, nullptr
    },
    {
      "matchdesc", &descopt,
      "Suppress points where description matches given name", nullptr, ARGTYPE_STRING,
      ARG_NOMINMAX, nullptr
    },
    {
      "matchcmt", &cmtopt,
      "Suppress points where comment matches given name", nullptr, ARGTYPE_STRING,
      ARG_NOMINMAX, nullptr
    },
    {
      "matchicon", &iconopt,
      "Suppress points where type matches given name", nullptr, ARGTYPE_STRING,
      ARG_NOMINMAX, nullptr
    },
  };

};

#endif
#endif // DISCARD_H_INCLUDED_
