/*
	Support for Columbus/Visiontac V900 csv format
        This format pads fields with NULL up to a fixed per field length.
        Because of that, and because xcsv does not allow a regex as a field delimiter,
        a special module is required.

	Copyright (C) 2009 Tal Benavidor

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

	TODO:
		- QUESTION: course = heading ??
                - HEIGHT: Altitude in meters (not corrected to WGS84...) ??
 */

/******************************************************************************
 FILE FORMAT INFO
=================

File has csv extension, and is somewhat csv like creature...
All lines end with \r\n
First line is a header line. It contains no nulls.
Following lines are record lines. They are comma separated, but fields always
have the exact same length (per field), and therefore, the commas are always
at the exact same position on the line. Fields are padded with nulls, in case
they have shorter value then the fixed field length.
Two modes are available: basic and advanced.

The following two examples show "*" where null appears.

------basic mode - start-------------------------
INDEX,TAG,DATE,TIME,LATITUDE N/S,LONGITUDE E/W,HEIGHT,SPEED,HEADING,VOX
1*****,T,090404,063401,31.765931N,035.206969E,821**,0***,0**,*********
2*****,T,090404,063402,31.765931N,035.206969E,821**,0***,0**,*********
3*****,T,090404,063403,31.765933N,035.206971E,821**,0***,0**,*********
4*****,T,090404,063404,31.765933N,035.206971E,822**,0***,0**,*********
5*****,T,090404,063407,31.765934N,035.206971E,824**,0***,0**,*********
------basic mode - end---------------------------


------advanced mode - start-------------------------
INDEX,TAG,DATE,TIME,LATITUDE N/S,LONGITUDE E/W,HEIGHT,SPEED,HEADING,FIX MODE,VALID,PDOP,HDOP,VDOP,VOX
1*****,T,090204,055722,31.768380N,035.209656E,149**,0***,0**,3D,SPS ,2.6**,2.4**,1.0**,*********
2*****,T,090204,055723,31.768380N,035.209656E,149**,0***,0**,3D,SPS ,2.5**,2.3**,0.9**,*********
3*****,T,090204,055724,31.768378N,035.209658E,149**,0***,0**,3D,SPS ,2.5**,2.3**,0.9**,*********
4*****,T,090204,055725,31.768378N,035.209658E,149**,0***,0**,3D,SPS ,2.5**,2.3**,0.9**,*********
5*****,T,090204,055728,31.768376N,035.209660E,150**,0***,0**,3D,SPS ,2.5**,2.3**,0.9**,*********
6*****,T,090204,055729,31.768376N,035.209660E,150**,0***,0**,3D,SPS ,4.0**,2.8**,2.9**,*********
7*****,T,090204,055730,31.768376N,035.209661E,150**,0***,0**,3D,SPS ,2.5**,2.3**,0.9**,*********
8*****,T,090204,055731,31.768376N,035.209661E,150**,0***,0**,3D,SPS ,2.5**,2.3**,0.9**,*********
9*****,T,090204,055737,31.768326N,035.209993E,150**,0***,0**,3D,SPS ,2.5**,2.3**,0.9**,*********
10****,T,090204,055738,31.768339N,035.209976E,153**,0***,0**,3D,SPS ,2.5**,2.3**,0.9**,*********
11****,T,090204,055739,31.768338N,035.209991E,155**,0***,0**,3D,SPS ,2.5**,2.3**,0.9**,*********
42****,C,090724,162320,31.763841N,035.205461E,788**,9***,344,3D,SPS ,1.2**,0.9**,0.8**,*********
121***,V,090724,162502,31.769619N,035.208964E,786**,16**,306,3D,SPS ,1.1**,0.8**,0.8**,VOX00003*
------advanced mode - end---------------------------

for a little more info, see structures:
	one_line_advanced_mode, one_line_basic_mode, one_line_common_start.
******************************************************************************/

#include "v900.h"

#include <cassert>             // for assert
#include <utility>             // for pair

#include <QByteArray>          // for QByteArray
#include <QChar>               // for QChar
#include <QDate>               // for QDate
#include <QDateTime>           // for QDateTime
#include <QHash>               // for QHash
#include <QIODevice>           // for QIODevice
#include <QList>               // for QList
#include <QStringList>         // for QStringList
#include <QStringLiteral>      // for qMakeStringPrivate, QStringLiteral
#include <QTime>               // for QTime
#include <QtGlobal>            // for qPrintable

#include "defs.h"
#include "parse.h"              // for parse_double


void
V900Format::rd_init(const QString& fname)
{
  stream = new gpsbabel::TextStream;
  stream->open(fname, QIODevice::ReadOnly);

  utc_offset = opt_utc? opt_utc.get_result() * SECONDS_PER_HOUR : 0;
}

void
V900Format::rd_deinit()
{
  stream->close();
  delete stream;
  stream = nullptr;
}

QList<V900Format::field_id_t> V900Format::parse_header(const QString& line)
{
  const QHash<QString, field_id_t> field_idxs = {
    {"INDEX", field_id_t::index},
    {"TAG", field_id_t::tag},
    {"DATE", field_id_t::date},
    {"TIME", field_id_t::time},
    {"LATITUDE N/S", field_id_t::latitude},
    {"LONGITUDE E/W", field_id_t::longitude},
    {"HEIGHT", field_id_t::height},
    {"SPEED", field_id_t::speed},
    {"HEADING", field_id_t::heading},
    {"FIX MODE", field_id_t::fix},
    {"VALID", field_id_t::valid},
    {"PDOP", field_id_t::pdop},
    {"HDOP", field_id_t::hdop},
    {"VDOP", field_id_t::vdop},
    {"VOX", field_id_t::vox}
  };

  /* Because at least one field (VOX) can appear in different columns
   * we build a list to get the field type from the column index.
   */
  const QStringList header_parts = line.split(',');
  QList<field_id_t> ids;
  for (const auto& header_part : header_parts) {
    QString column_header = header_part.trimmed().toUpper();
    if (field_idxs.contains(column_header)) {
      ids.append(field_idxs.value(column_header));
    } else {
      ids.append(field_id_t::unknown);
      gbWarning("Ignoring unrecognized field %s\n", qPrintable(column_header));
    }
  }
  return ids;
}

V900Format::V900Map V900Format::parse_line(const QStringList& parts, const QList<field_id_t>& ids)
{
  /* Build a hash to get the field value from the field type */
  V900Map map;
  for (int idx = 0; idx < parts.size(); ++idx) {
    field_id_t fldid = ids.at(idx);
    map[fldid] = parts.at(idx).trimmed();
  }
  return map;
}

bool V900Format::isDupe(const V900Map& a, const V900Map& b)
{
  // While the V900Maps from all the records should have identical keys, the initial
  // previous V900Map won't have any keys.
  if (a.size() != b.size()) {
    return false;
  }
  for (auto it = a.cbegin(); it != a.cend(); ++it) {
    auto key = it.key();
    assert(b.contains(key));
    if ((key != field_id_t::index) && (key != field_id_t::tag)) {
      if (it.value() != b.value(key)) {
        return false;
      }
    }
  }
  return true;
}

void
V900Format::read()
{
  int lc = 0;

  /*
  Traditionally we supported two modes, with rather strict requirements about field order, field lengths,
  field padding and line endings.
  Basic mode:    INDEX,TAG,DATE,TIME,LATITUDE N/S,LONGITUDE E/W,HEIGHT,SPEED,HEADING,VOX
  Advanced mode: INDEX,TAG,DATE,TIME,LATITUDE N/S,LONGITUDE E/W,HEIGHT,SPEED,HEADING,FIX MODE,VALID,PDOP,HDOP,VDOP,VOX
  We are now somewhat more relaxed about things.  This allows us to support Columbus "P-1 Mark II", which
  is similar to Basic Mode but without VOX, without field padding, and with different length Lat/Lon fields.
  */

  QString line;
  if (!stream->readLineInto(&line)) {
    gbFatal("error reading header (first) line from input file\n");
  }
  ++lc;

  /* Build a list to get the field type from the column index. */
  const QList<field_id_t> ids = parse_header(line);

  auto* track = new route_head;
  track->rte_name = "V900 tracklog";
  track->rte_desc = "V900 GPS tracklog data";
  track_add_head(track);

  V900Map prev;
  while (stream->readLineInto(&line)) {
    ++lc;
    const QStringList parts = line.remove(QChar::Null).split(',');

    if (parts.size() != ids.size()) {
      gbWarning("skipping malformed record at line %d.  The number of fields don't match the header.\n", lc);
      continue;
    }

    /* Build a hash to get field value from the field type */
    const V900Map map = parse_line(parts, ids);

    auto* wpt = new Waypoint;

    bool ok;
    QString end;

    /* handle date/time fields.  base year is 2000, default time zone is UTC. */
    QDate date = QDate::fromString(QStringLiteral("20%1").arg(map.value(field_id_t::date)), "yyyyMMdd");
    QTime time = QTime::fromString(map.value(field_id_t::time), "hhmmss");
    QDateTime dt; // invalid
    if (date.isValid() && time.isValid()) {
      // default to UTC by passing
      // is_localtime as static_cast<bool>(opt_utc) &
      // force_utc as static_cast<bool>(opt_utc)
      dt = make_datetime(date, time, opt_utc, opt_utc, utc_offset);
    }
    if (dt.isValid()) {
      wpt->SetCreationTime(dt);
    } else {
      gbWarning("skipping malformed record at line %d.  Failed to parse date and or time.\n", lc);
      delete wpt;
      continue;
    }

    wpt->latitude = parse_double(map.value(field_id_t::latitude), "", &ok, &end);
    if (!ok || !((end == 'N') || (end == 'S'))) {
      gbWarning("skipping malformed record at line %d.  Failed to parse latitude.\n", lc);
      delete wpt;
      continue;
    }
    if (end == 'S') {
      wpt->latitude = -wpt->latitude;
    }

    wpt->longitude = parse_double(map.value(field_id_t::longitude), "", &ok, &end);
    if (!ok || !((end == 'E') || (end == 'W'))) {
      gbWarning("skipping malformed record at line %d.  Failed to parse longitude.\n", lc);
      delete wpt;
      continue;
    }
    if (end == 'W') {
      wpt->longitude = -wpt->longitude;
    }

    wpt->altitude = parse_double(map.value(field_id_t::height), "", &ok);
    if (!ok) {
      gbWarning("skipping malformed record at line %d.  Failed to parse height.\n", lc);
      delete wpt;
      continue;
    }

    wpt->set_speed(KPH_TO_MPS(parse_double(map.value(field_id_t::speed), "", &ok)));
    if (!ok) {
      gbWarning("skipping malformed record at line %d.  Failed to parse speed.\n", lc);
      delete wpt;
      continue;
    }

    wpt->set_course(parse_double(map.value(field_id_t::heading), "", &ok));
    if (!ok) {
      gbWarning("skipping malformed record at line %d.  Failed to parse heading.\n", lc);
      delete wpt;
      continue;
    }

    if (map.contains(field_id_t::pdop)) {
      wpt->pdop = parse_double(map.value(field_id_t::pdop), "", &ok);
      if (!ok) {
        gbWarning("skipping malformed record at line %d.  Failed to parse pdop.\n", lc);
        delete wpt;
        continue;
      }
    }

    if (map.contains(field_id_t::hdop)) {
      wpt->hdop = parse_double(map.value(field_id_t::hdop), "", &ok);
      if (!ok) {
        gbWarning("skipping malformed record at line %d.  Failed to parse hdop.\n", lc);
        delete wpt;
        continue;
      }
    }

    if (map.contains(field_id_t::vdop)) {
      wpt->vdop = parse_double(map.value(field_id_t::vdop), "", &ok);
      if (!ok) {
        gbWarning("skipping malformed record at line %d.  Failed to parse vdop.\n", lc);
        delete wpt;
        continue;
      }
    }

    /* handle fix mode (2d, 3d, etc.) */
    if (map.value(field_id_t::valid) == "DGPS") {
      wpt->fix = fix_dgps;
    } else if (map.value(field_id_t::fix) == "3D") {
      wpt->fix = fix_3d;
    } else if (map.value(field_id_t::fix) == "2D") {
      wpt->fix = fix_2d;
    } else
      /* possible field: fix_unknown,fix_none,fix_2d,fix_3d,fix_dgps,fix_pps */
    {
      wpt->fix = fix_unknown;
    }

    /* The Columbus P-10 Pro Quick Start Guide describes some types.
     * T:Normal point
     * C:POI
     * D:Second type of POI
     * G:Wake-up point
     */
    QString tag = map.value(field_id_t::tag);
    if (tag != 'T') {
      // A 'G' tag appears to be a 'T' tag, but generated on the trailing
      // edge of a DGPS fix as it decays to an SPS fix.  See 1/13/13 email
      // thread on gpsbabel-misc with Jamie Robertson.
      if ((tag == 'C') ||
          (tag == 'G') ||
          (tag == 'V')) {
        auto* wpt2 = new Waypoint(*wpt);
        if (tag == 'V') {	// waypoint with voice recording?
          QString vox = map.value(field_id_t::vox);
          if (!vox.isEmpty()) {
            vox.append(".WAV");
            wpt2->shortname = vox;
            wpt2->description = vox;
            waypt_add_url(wpt2, vox, vox);
          }
        }
        waypt_add(wpt2);
      } else {
        if (!tag.isEmpty()) {
          gbWarning("unrecognized tag \"%s\" at line %d. Skipping waypoint generation.\n", qPrintable(tag), lc);
        } else {
          gbWarning("missing or empty tag at line %d. Skipping waypoint generation.\n", lc);
        }
      }
    }

    // Some lines may be duplicates except for a different index and tag.
    // For example on the Columbus "P-1 Mark II" tag 'T' (normal point) lines may be duplicated
    // as a tag 'C' (POI) line.
    if (isDupe(map, prev)) {
      delete wpt;
    } else {
      track_add_wpt(track, wpt);
    }
    prev = map;
  }
}
