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

#include "discard.h"

#include <QDebug>              // for QDebug
#include <QRegularExpression>  // for QRegularExpression, QRegularExpression::CaseInsensitiveOption, QRegularExpressionMatch

#include <cstdlib>             // for strtod

#include "defs.h"              // for Waypoint, fatal, route_del_wpt, route_disp_all, track_del_wpt, track_disp_all, waypt_del, waypt_disp_all, route_head, rtedata, trkdata, wptdata, fix_none, fix_unknown
#include "src/core/logging.h"  // for FatalMsg


#if FILTERS_ENABLED

/*
 * Decide whether to keep or toss this point.
 */
void DiscardFilter::fix_process_wpt(const Waypoint* wpt)
{
  bool del = false;
  bool delh = false;
  bool delv = false;

  auto* waypointp = const_cast<Waypoint*>(wpt);

  if ((hdopf >= 0.0) && (waypointp->hdop > hdopf)) {
    delh = true;
  }
  if ((vdopf >= 0.0) && (waypointp->vdop > vdopf)) {
    delv = true;
  }

  if (andopt) {
    del = delh && delv;
  } else {
    del = delh || delv;
  }

  if ((satpf >= 0) && (waypointp->sat < satpf)) {
    del = true;
  }

  if ((fixnoneopt) && (waypointp->fix == fix_none)) {
    del = true;
  }

  if ((fixunknownopt) && (waypointp->fix == fix_unknown)) {
    del = true;
  }

  if ((eleminopt) && (waypointp->altitude < eleminpf)) {
    del = true;
  }

  if ((elemaxopt) && (waypointp->altitude > elemaxpf)) {
    del = true;
  }

  if (nameopt && name_regex.match(waypointp->shortname).hasMatch()) {
    del = true;
  }
  if (descopt && desc_regex.match(waypointp->description).hasMatch()) {
    del = true;
  }
  if (cmtopt && cmt_regex.match(waypointp->notes).hasMatch()) {
    del = true;
  }
  if (iconopt && icon_regex.match(waypointp->icon_descr).hasMatch()) {
    del = true;
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

QRegularExpression DiscardFilter::generateRegExp(const QString& glob_pattern)
{
  QRegularExpression regex;
  regex.setPatternOptions(QRegularExpression::CaseInsensitiveOption);
  QString pattern = QRegularExpression::wildcardToRegularExpression(glob_pattern);
  // un-anchor the pattern, we allow partial matches.
  if (pattern.startsWith("\\A") && pattern.endsWith("\\z")) {
    pattern = pattern.mid(2,pattern.size()-4);
  }
  regex.setPattern(pattern);
  return regex;
}

void DiscardFilter::init()
{
  if (hdopopt) {
    hdopf = strtod(hdopopt, nullptr);
  } else {
    hdopf = -1.0;
  }

  if (vdopopt) {
    vdopf = strtod(vdopopt, nullptr);
  } else {
    vdopf = -1.0;
  }

  if (satopt) {
    satpf = xstrtoi(satopt, nullptr, 10);
  } else {
    satpf = -1;
  }

  if (eleminopt) {
    eleminpf = xstrtoi(eleminopt, nullptr, 10);
  }

  if (elemaxopt) {
    elemaxpf = xstrtoi(elemaxopt, nullptr, 10);
  }

  if (nameopt) {
    name_regex = generateRegExp(nameopt);
    if (!name_regex.isValid()) {
      fatal(FatalMsg() << "discard: matchname option is an invalid expression.");
    }
  }
  if (descopt) {
    desc_regex = generateRegExp(descopt);
    if (!desc_regex.isValid()) {
      fatal(FatalMsg() << "discard: matchdesc option is an invalid expression.");
    }
  }
  if (cmtopt) {
    cmt_regex = generateRegExp(cmtopt);
    if (!cmt_regex.isValid()) {
      fatal(FatalMsg() << "discard: matchcmt option is an invalid expression.");
    }
  }
  if (iconopt) {
    icon_regex = generateRegExp(iconopt);
    if (!icon_regex.isValid()) {
      fatal(FatalMsg() << "discard: matchicon option is an invalid expression.");
    }
  }
}

#endif
