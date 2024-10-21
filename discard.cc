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

#include "defs.h"              // for Waypoint, fatal, route_head (ptr only), xstrtoi, del_marked_wpts, route_del_marked_wpts, route_disp_all, track_del_marked_wpts, track_disp_all, waypt_disp_all, fix_none, fix_unknown
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

  if ((hdopf >= 0.0) && (wpt->hdop > hdopf)) {
    delh = true;
  }
  if ((vdopf >= 0.0) && (wpt->vdop > vdopf)) {
    delv = true;
  }

  if (andopt) {
    del = delh && delv;
  } else {
    del = delh || delv;
  }

  if ((satpf >= 0) && (wpt->sat < satpf)) {
    del = true;
  }

  if ((fixnoneopt) && (wpt->fix == fix_none)) {
    del = true;
  }

  if ((fixunknownopt) && (wpt->fix == fix_unknown)) {
    del = true;
  }

  if ((eleminopt) && (wpt->altitude < eleminpf)) {
    del = true;
  }

  if ((elemaxopt) && (wpt->altitude > elemaxpf)) {
    del = true;
  }

  if (nameopt && name_regex.match(wpt->shortname).hasMatch()) {
    del = true;
  }
  if (descopt && desc_regex.match(wpt->description).hasMatch()) {
    del = true;
  }
  if (cmtopt && cmt_regex.match(wpt->notes).hasMatch()) {
    del = true;
  }
  if (iconopt && icon_regex.match(wpt->icon_descr).hasMatch()) {
    del = true;
  }

  if (del) {
    const_cast<Waypoint*>(wpt)->wpt_flags.marked_for_deletion = 1;
  }
}

void DiscardFilter::process()
{
  auto waypoint_cb_lambda = [this](const Waypoint* wpt) -> void {
    fix_process_wpt(wpt);
  };

  // Filter waypoints.
  waypt_disp_all(waypoint_cb_lambda);
  del_marked_wpts();

  // Filter tracks
  auto track_tlr_lambda = [](const route_head* rte)->void {
    track_del_marked_wpts(const_cast<route_head*>(rte));
  };
  track_disp_all(nullptr, track_tlr_lambda, waypoint_cb_lambda);

  // And routes
  auto route_tlr_lambda = [](const route_head* rte)->void {
    route_del_marked_wpts(const_cast<route_head*>(rte));
  };
  route_disp_all(nullptr, route_tlr_lambda, waypoint_cb_lambda);
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
    hdopf = hdopopt.toDouble();
  } else {
    hdopf = -1.0;
  }

  if (vdopopt) {
    vdopf = vdopopt.toDouble();
  } else {
    vdopf = -1.0;
  }

  if (satopt) {
    satpf = satopt.toInt();
  } else {
    satpf = -1;
  }

  if (eleminopt) {
    eleminpf = eleminopt.toInt();
  }

  if (elemaxopt) {
    elemaxpf = elemaxopt.toInt();
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
