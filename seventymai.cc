/*
    seventymai - 70 mai Dash Cam files

    Copyright (C) 2025 Robert Lipe, robertlipe+source@gpsbabel.org

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

#include "seventymai.h"

#include <QByteArray>              // for QByteArray
#include <QIODevice>               // for QIODevice
#include <QList>                   // for QList
#include <QString>                 // for QString
#include <Qt>                      // for CaseSensitivity
#include <QtGlobal>                // for qPrintable

#include "defs.h"                  // for gbWarning, Waypoint, route_head, track_add_head, track_add_wpt
#include "src/core/textstream.h"   // for TextStream


void SeventymaiFormat::add_track_head(route_head* trk, int& trk_num)
{
  // save or delete existing track
  if (!trk->rte_waypt_empty()) {
    trk->rte_num = ++trk_num;
    trk->rte_name = QStringLiteral("Track %1").arg(trk_num);
    track_add_head(trk);
  } else {
    delete trk;
  }
}

void SeventymaiFormat::read()
{
  int line_no = 0;
  int trk_num = 0;
  auto* trk = new route_head;

  QString buff;
  while (stream->readLineInto(&buff)) {
    line_no++;

    // new track indicator?
    if (buff.startsWith("$V02", Qt::CaseInsensitive)) {
      // save or delete existing track
      add_track_head(trk, trk_num);
      // allocate new track
      trk = new route_head;
      // skip to next record
      continue;
    }

    // We don't need to escape delimters, which allows us to use
    // QString::split, which is much faster than csv_linesplit.
    const auto values = buff.split(',');

    if (values.size() != kNumCols) {
      gbWarning("Expected %d columns, got %lld.  Ignoring line number %d.\n", kNumCols,
                values.size(), line_no);
      continue;
    }

    auto* wpt = new Waypoint;

    for (int i = 0; i < kNumCols; ++i) {
      const auto value = values.at(i).trimmed();
      switch (i + 1) {
      case kColTime: {
        bool ok;
        auto tt  = value.toLongLong(&ok);
        if (ok) {
          wpt->SetCreationTime(tt);
        } else {
          gbWarning("parse of string '%s' on line number %d as time_t failed.\n",
                    qPrintable(values.at(0)), line_no);
        }
        break;
      }
      case kColStatus:
        if (value.compare('A', Qt::CaseInsensitive) == 0) {
          wpt->fix = fix_2d;
        } else {
          wpt->fix = fix_none;
        }
        break;
      case kColLatitude: {
        bool ok;
        auto lat = value.toDouble(&ok);
        if (ok) {
          wpt->latitude = lat;
        } else {
          gbWarning("parse of string '%s' on line number %d as double failed.\n",
                    qPrintable(values.at(2)), line_no);
        }
        break;
      }
      case kColLongitude: {
        bool ok;
        auto lon= value.toDouble(&ok);
        if (ok) {
          wpt->longitude = lon;
        } else {
          gbWarning("parse of string '%s' on line number %d as double failed.\n",
                    qPrintable(values.at(3)), line_no);
        }
        break;
      }
      case kColCourse: {
        bool ok;
        auto course = static_cast<float>(value.toInt(&ok));
        if (ok) {
          wpt->set_course(course / 100.0F);
        } else {
          gbWarning("parse of string '%s' on line number %d as double failed.\n",
                    qPrintable(values.at(4)), line_no);
        }
        break;
      }
      case kColSpeed: {
        bool ok;
        auto speed = static_cast<float>(value.toInt(&ok));
        if (ok) {
          wpt->set_speed(speed / 100.0F);
        } else {
          gbWarning("parse of string '%s' on line number %d as double failed.\n",
                    qPrintable(values.at(5)), line_no);
        }
        break;
      }
      case kColVideo:
        wpt->notes = value;
        break;
      default:
        break;
      }
    }

    // Ignore lines that don't have active satellite status.
    if (wpt->fix == fix_none) {
      delete wpt;
      continue;
    }

    // ignore points from the NULL island.
    if ((wpt->latitude == 0.0) && (wpt->longitude == 0.0)) {
      gbWarning("Point at latitude 0, longitude 0. Ignoring line number %d.\n", line_no);
      delete wpt;
      continue;
    }

    track_add_wpt(trk, wpt);
  }

  // save or delete existing track
  add_track_head(trk, trk_num);
}

void SeventymaiFormat::rd_init(const QString& fname)
{
  stream = new gpsbabel::TextStream;
  stream->open(fname, QIODevice::ReadOnly);
}

void SeventymaiFormat::rd_deinit()
{
  stream->close();
  delete stream;
  stream = nullptr;
}
