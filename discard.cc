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
    Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111 USA

 */

#include "defs.h"
#include "discard.h"
#include "filterdefs.h"
#include <cstdlib>
// Can't use QRegularExpression because Linux won't get Qt 5 for years.
#include <QtCore/QRegExp>
#include <cstdio>
#include <cstdlib>

#if FILTERS_ENABLED

/*
 * Decide whether to keep or toss this point.
 */
void DiscardFilter::fix_process_wpt(const Waypoint* wpt)
{
  int del = 0;
  int delh = 0;
  int delv = 0;

  Waypoint* waypointp = const_cast<Waypoint*>(wpt);

  if ((hdopf >= 0.0) && (waypointp->hdop > hdopf)) {
    delh = 1;
  }
  if ((vdopf >= 0.0) && (waypointp->vdop > vdopf)) {
    delv = 1;
  }

  if (andopt) {
    del = delh && delv;
  } else {
    del = delh || delv;
  }

  if ((satpf >= 0) && (waypointp->sat < satpf)) {
    del = 1;
  }

  if ((fixnoneopt) && (waypointp->fix == fix_none)) {
    del = 1;
  }

  if ((fixunknownopt) && (waypointp->fix == fix_unknown)) {
    del = 1;
  }

  if ((eleminopt) && (waypointp->altitude < eleminpf)) {
    del = 1;
  }

  if ((elemaxopt) && (waypointp->altitude > elemaxpf)) {
    del = 1;
  }

  if (nameopt && name_regex.indexIn(waypointp->shortname) >= 0) {
    del = 1;
  }
  if (descopt && desc_regex.indexIn(waypointp->description) >= 0) {
    del = 1;
  }
  if (cmtopt && cmt_regex.indexIn(waypointp->notes) >= 0) {
    del = 1;
  }
  if (iconopt && icon_regex.indexIn(waypointp->icon_descr) >= 0) {
    del = 1;
  }

  if (del) {
    switch (what) {
    case wptdata:
      waypt_del(waypointp);
      delete waypointp;
      break;
    case trkdata:
      track_del_wpt(head, waypointp);
      delete waypointp;
      break;
    case rtedata:
      route_del_wpt(head, waypointp);
      delete waypointp;
      break;
    default:
      return;
    }
  }
}

void DiscardFilter::fix_process_head(const route_head* trk)
{
  head = const_cast<route_head*>(trk);
}

void DiscardFilter::process()
{
  WayptFunctor<DiscardFilter> fix_process_wpt_f(this, &DiscardFilter::fix_process_wpt);
  RteHdFunctor<DiscardFilter> fix_process_head_f(this, &DiscardFilter::fix_process_head);

  // Filter waypoints.
  what = wptdata;
  waypt_disp_all(fix_process_wpt_f);

  // Filter tracks
  what = trkdata;
  track_disp_all(fix_process_head_f, nullptr, fix_process_wpt_f);

  // And routes
  what = rtedata;
  route_disp_all(fix_process_head_f, nullptr, fix_process_wpt_f);

}

void DiscardFilter::init()
{
  if (hdopopt) {
    hdopf = atof(hdopopt);
  } else {
    hdopf = -1.0;
  }

  if (vdopopt) {
    vdopf = atof(vdopopt);
  } else {
    vdopf = -1.0;
  }

  if (satopt) {
    satpf = atoi(satopt);
  } else {
    satpf = -1;
  }

  if (eleminopt) {
    eleminpf = atoi(eleminopt);
  }

  if (elemaxopt) {
    elemaxpf = atoi(elemaxopt);
  }

  if (nameopt) {
    name_regex.setCaseSensitivity(Qt::CaseInsensitive);
    name_regex.setPatternSyntax(QRegExp::WildcardUnix);
    name_regex.setPattern(nameopt);
  }
  if (descopt) {
    desc_regex.setCaseSensitivity(Qt::CaseInsensitive);
    desc_regex.setPatternSyntax(QRegExp::WildcardUnix);
    desc_regex.setPattern(descopt);
  }
  if (cmtopt) {
    cmt_regex.setCaseSensitivity(Qt::CaseInsensitive);
    cmt_regex.setPatternSyntax(QRegExp::WildcardUnix);
    cmt_regex.setPattern(cmtopt);
  }
  if (iconopt) {
    icon_regex.setCaseSensitivity(Qt::CaseInsensitive);
    icon_regex.setPatternSyntax(QRegExp::WildcardUnix);
    icon_regex.setPattern(iconopt);
  }
}

#endif
