/*
  Access to Lowrance USR files.

  Initial contribution to gpsbabel by Jason Rust (jrust at rustyparts.com)

  Copyright (C) 2005 - 2018 Robert Lipe, robertlipe+source@gpsbabel.org

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

  HISTORY:

  6/21/05 - Ling Nero (rnlnero@yahoo.com)
    - Added Routes, Icons, & Tracks support
    - Fixed waypoint date/time stamp conversion
  02/09/08 - oliskoli
    - gbfile API
    - check for buffer overflows when reading names or comments
  02/25/2008 - Alan Porter (alan@kr4jb.net)
    - Added new icons for Lowrance iFinder Expedition C
    - Categorized geocaching waypoints using different icons
  01/06/2012 - Kris Beevers (beevek at gmail.com)
    - First pass read-write support for USR 4 in lowranceusr4.cc
  10/02/2018 - BJ Kowalski (bj dot kowalski at gmail.com)
    - Added support to lowranceusr4.cc for USR 4 route version 2
      format data.
    - Combined processing of USR 2/3/4/5/6 formats into a single
      file. The previous implementation did not support conversion
      between USR 2/3 and 4/5/6 or between 4/5/6 and 2/3 because of
      the separation in processing between lowranceusr.cc and
      lowranceusr4.cc.

  USR Background Information

  Collected from various WEB sources and Lowrance HOOK2 User Manual.

    Lowrance Support Site - https://www.lowrance.com/help-and-support/
    Hook2 Series Operator Manual (English) - //software.lowrance.com/Documents/Hook2-Series_OM_EN_988-11760-001_w.pdf
    Lowrance Endura FAQs II - http://support.lowrance.com/system/selfservice.controller?CONFIGURATION=1001&PARTITION_ID=1&secureFlag=false&TIMEZONE_OFFSET=&CMD=VIEW_ARTICLE&ARTICLE_ID=2028
    BBCBoards.Net : Lowrance Sonar/GPS Topic : Waypoints; USR Versions Thread : http://www.bbcboards.net/showthread.php?t=855028
    Python USR4 (Version 4) to GPX Converter - http://lowranceusrv4togpxconverter.blogspot.com/2011/12/about-this-blog.html

      User Data File version 6 - USRv6
       Latest format.
       Supports trail characteristics speed and temperature.

      User Data File version 5 - USRv5
        Lowrance introduced universally unique identifiers (UUIDs) in this version.

      User Data File version 4 - USRv4
        Seems to be the best option for transferring data from older Lowrance units.
        Many of the counts (Number of Waypoints, Number of Routes, etc) were expanded from
        16-bit integer values (maximum value of 65,535) to 32-bit (maxumum value 2,147,483,647)
        USRv4 and above support a maximum of 20,000 trail-points (actually 24K and change).
        USRv4 and above and GPX support trails with trail-segments.

      User Data File version 3 - USRv3
        Legacy file format.
        Added depth information to Route waypoints.
        Supports trails with a maximum of 10,000 trail-points.
        Last version that supports Event Marker ICONs.

      User Data File version 2 - USRv2
        Legacy file format.
        This is the default output USR version used by GPSBabel.
        This format contains ony basic information on waypoints, routes, and trails.

      GPX (GPS Exchange)
        Common format supported by many vendors and programs.  Lowrance only provides
        minimal support for GPX export with their HOOK2 series.  Waypoints include
        longitude, latitude, timestamp, name and symbol information.  Routes include
        name and for route points longitude, latitude, timestamp, name, and symbol.
        Tracks include name and for track points longitude, latitude, and timestamp.

*/

#include "lowranceusr.h"

#include <cinttypes>            // for PRId64
#include <cmath>                // for round, atan, exp, log, tan
#include <cstdio>               // for SEEK_CUR
#include <cstdint>              // for int64_t
#include <cstdlib>              // for abs
#include <numbers>              // for pi
#include <utility>              // for as_const

#include <QByteArray>           // for QByteArray
#include <QDate>                // for QDate
#include <QDateTime>            // for QDateTime
#include <QList>                // for QList
#include <QScopedPointer>       // for QScopedPointer
#include <QString>              // for QString, operator+, operator==, operator!=
#include <QTextCodec>           // for QTextCodec, QTextCodec::IgnoreHeader
#include <QTextEncoder>         // for QTextEncoder
#include <QTime>                // for QTime
#include <Qt>                   // for CaseInsensitive, UTC
#include <QtGlobal>             // for qPrintable, uint, QAddConst<>::Type

#include "defs.h"
#include "formspec.h"           // for FsChainFind, FsChainAdd, kFsLowranceusr4, FormatSpecificData
#include "gbfile.h"             // for gbfgetint32, gbfputint32, gbfputint16, gbfgetc, gbfgetint16, gbfwrite, gbfputc, gbfeof, gbfgetflt, gbfclose, gbfgetdbl, gbfopen_le, gbfputdbl, gbfputs, gbfile, gbfputflt, gbfread, gbfseek
#include "geocache.h"           // for Geocache, Geocache::status_t, Geocach...
#include "src/core/datetime.h"  // for DateTime
#include "src/core/logging.h"   // for Warning


/* from waypt.c, we need to iterate over waypoints when extracting routes */
extern WaypointList* global_waypoint_list;

/* below couple of functions mostly borrowed from raymarine.c */

/* make waypoint shortnames unique */
bool
LowranceusrFormat::same_points(const Waypoint* A, const Waypoint* B)
{
  return ( /* !!! We are case-sensitive !!! */
           (A->shortname == B->shortname) &&
           (A->latitude == B->latitude) &&
           (A->longitude == B->longitude));
}

void
LowranceusrFormat::register_waypt(const Waypoint* wpt)
{
  for (const Waypoint* cmp : std::as_const(*waypt_table)) {
    if (same_points(wpt, cmp)) {
      return;
    }
  }

  if (global_opts.debug_level >= 2) {
    gbDebug("adding waypt %s (%s) to table at index %s\n",
           gbLogCStr(wpt->shortname), gbLogCStr(wpt->description), QByteArray::number(waypt_table->size()).constData());
  }

  waypt_table->append(wpt);
}

/* end borrowed from raymarine.c */

const Waypoint*
LowranceusrFormat::lowranceusr4_find_waypt(uint uid_unit, int uid_seq_low, int uid_seq_high)
{
  // Iterate with waypt_disp_all?
  for (const Waypoint* waypointp : std::as_const(*global_waypoint_list)) {
    const auto* fs = reinterpret_cast<lowranceusr4_fsdata*>(waypointp->fs.FsChainFind(kFsLowranceusr4));

    if (fs && fs->uid_unit == uid_unit &&
        fs->uid_seq_low == uid_seq_low &&
        fs->uid_seq_high == uid_seq_high) {
      return waypointp;
    }
  }

  if (global_opts.debug_level >= 1) {
    gbDebug("lowranceusr4_find_waypt: warning, failed finding waypoint with ids %u %d %d\n",
           uid_unit, uid_seq_low, uid_seq_high);
  }
  return nullptr;
}

const Waypoint*
LowranceusrFormat::lowranceusr4_find_global_waypt(uint id1, uint id2, uint id3, uint id4)
{
  // Iterate with waypt_disp_all?
  for (const Waypoint* waypointp : std::as_const(*global_waypoint_list)) {
    const auto* fs = reinterpret_cast<lowranceusr4_fsdata*>(waypointp->fs.FsChainFind(kFsLowranceusr4));

    if (fs && fs->UUID1 == id1 &&
        fs->UUID2 == id2 &&
        fs->UUID3 == id3 &&
        fs->UUID4 == id4) {
      return waypointp;
    }
  }

  if (global_opts.debug_level >= 1) {
    gbDebug("lowranceusr4_find_global_waypt: warning, failed finding waypoint with ids %08x %08x %08x %08x\n",
           id1, id2, id3, id4);
  }
  return nullptr;
}

//  Starting with USR 4, adopted a UTF-16 character string format
QString
LowranceusrFormat::lowranceusr4_readstr(gbfile* file, int bytes_per_char) const
{
  QString retval;

  const int len = gbfgetint32(file); /* bytes */
  if (len < 0) {
    /* seems len=-1 means no string */
    retval = QString();
  } else if (len == 0) {
    /* len == 0 means an empty string */
    retval = QString("");
  } else {
    QByteArray buf;
    buf.resize(len);
    int bytesread = gbfread(buf.data(), 1, len, file);
    buf.truncate(bytesread);
    buf.replace((char)0x01, '*');  // IWay 350C puts 0x01 for the accented o in the street name of the Montreal Holiday Inn.
    if (bytes_per_char == 1) {
      retval = QString(buf);
    } else {
      retval = utf16le_codec->toUnicode(buf);
    }
    if (len > bytesread) {
      (void) gbfseek(file, (len - bytesread), SEEK_CUR);
    }
  }

  return retval;
}

//  Starting with USR 4, adopted a UTF-16 character string format
void
LowranceusrFormat::lowranceusr4_writestr(const QString& buf, gbfile* file, int bytes_per_char) const
{
  QByteArray qba;
  if (bytes_per_char == 1) {
    qba = buf.toUtf8();
    qba.truncate(MAXUSRSTRINGSIZE);
  } else {
    QScopedPointer<QTextEncoder> encoder(utf16le_codec->makeEncoder(QTextCodec::IgnoreHeader));
    qba = encoder->fromUnicode(buf);
  }
  int len = qba.size();
  gbfputint32(len, file_out);
  gbfwrite(qba.constData(), 1, len, file);
}

gpsbabel::DateTime
LowranceusrFormat::lowranceusr4_get_timestamp(unsigned int jd_number, unsigned int msecs)
{
  QDateTime qdt = QDateTime(QDate::fromJulianDay(jd_number), QTime(0, 0, 0), QtUTC).addMSecs(msecs);
  return qdt;
}

LowranceusrFormat::Lowranceusr4Timestamp
LowranceusrFormat::lowranceusr4_jd_from_timestamp(const gpsbabel::DateTime& qdt)
{
  QDateTime jdt = qdt.toUTC();
  unsigned int jd_number = jdt.date().toJulianDay();
  QTime jd_time = jdt.time();
  unsigned int msecs = (((((jd_time.hour() * 60) + jd_time.minute()) * 60) + jd_time.second()) * 1000) + jd_time.msec();
  return Lowranceusr4Timestamp(jd_number, msecs);
}

QString
LowranceusrFormat::lowranceusr_find_desc_from_icon_number(const int icon)
{
  return lowranceusr_common_find_desc_from_icon_number(icon, lowranceusr_icon_value_table);
}

int
LowranceusrFormat::lowranceusr_find_icon_number_from_desc(const QString& desc)
{
  return lowranceusr_common_find_icon_number_from_desc(desc, lowranceusr_icon_value_table, DEF_ICON);
}

QString
LowranceusrFormat::lowranceusr4_find_desc_from_icon_number(const int icon)
{
  return lowranceusr_common_find_desc_from_icon_number(icon, lowranceusr4_icon_value_table);
}

int
LowranceusrFormat::lowranceusr4_find_icon_number_from_desc(const QString& desc)
{
  return lowranceusr_common_find_icon_number_from_desc(desc, lowranceusr4_icon_value_table, DEF_USR4_ICON);
}

const char*
LowranceusrFormat::lowranceusr4_find_color_from_icon_number_plus_color_index(const int icon, const int index)
{
  for (const lowranceusr4_icon_mapping_t* i = lowranceusr4_icon_value_table; i->icon; i++) {
    if (icon == i->value) {
      return i->color[index];
    }
  }

  return nullptr;
}

int
LowranceusrFormat::lowranceusr4_find_index_from_icon_desc_and_color_desc(const QString& icon, const QString& color)
{
  if (icon.isNull()) {
    return DEF_USR4_COLOR;
  }

  /*
   * If we were given a numeric icon number as a description
   * (i.e. 8255), just return DEF_USR4_COLOR.
   */
  int n = icon.toInt();
  if (n)  {
    return DEF_USR4_COLOR;
  }

  for (const lowranceusr4_icon_mapping_t* i = lowranceusr4_icon_value_table; i->icon; i++) {
    if (icon.compare(i->icon,Qt::CaseInsensitive) == 0) {
      // Found ICON, now look for color
      for (int index=0; index<7; index++) {
        if (color.compare(i->color[index],Qt::CaseInsensitive) == 0) {
          return index;
        }
      }
    }
  }

  return DEF_USR4_COLOR;
}

void
LowranceusrFormat::rd_init(const QString& fname)
{
  file_in = gbfopen_le(fname, "rb");
  utf16le_codec = QTextCodec::codecForName("UTF-16LE");
}

void
LowranceusrFormat::rd_deinit()
{
  gbfclose(file_in);
  utf16le_codec = nullptr;
}

void
LowranceusrFormat::wr_init(const QString& fname)
{
  file_out = gbfopen_le(fname, "wb");
  mkshort_handle = new MakeShort;
  waypt_out_count = 0;
  writing_version = opt_wversion.get_result();
  if ((writing_version < 2) || (writing_version > 4)) {
    gbFatal("wversion value %s is not supported !!\n", gbLogCStr(opt_wversion));
  }
  utf16le_codec = QTextCodec::codecForName("UTF-16LE");
  waypt_table = new QList<const Waypoint*>;
}

void
LowranceusrFormat::wr_deinit()
{
  gbfclose(file_out);
  delete mkshort_handle;
  mkshort_handle = nullptr;
  utf16le_codec = nullptr;
  delete waypt_table;
  waypt_table = nullptr;
}

/**
 * Latitude and longitude for USR coords are in the lowrance mercator meter
 * format in WGS84.  The below code converts them to degrees.
 */
double
LowranceusrFormat::lon_mm_to_deg(double x)
{
  return x / (DEGREESTORADIANS * SEMIMINOR);
}

double
LowranceusrFormat::lat_mm_to_deg(double x)
{
  return (2.0 * atan(exp(x / SEMIMINOR)) - std::numbers::pi / 2.0) / DEGREESTORADIANS;
}

long
LowranceusrFormat::lon_deg_to_mm(double x)
{
  return round(x * SEMIMINOR * DEGREESTORADIANS);
}

long
LowranceusrFormat::lat_deg_to_mm(double x)
{
  return round(SEMIMINOR * log(tan((x * DEGREESTORADIANS + std::numbers::pi / 2.0) / 2.0)));
}

void
LowranceusrFormat::lowranceusr_parse_waypt(Waypoint* wpt_tmp, int object_num_present)
{
  /* Object num */
  if (object_num_present) {
    short object_num = gbfgetint16(file_in);
    if (global_opts.debug_level == 99) {
      gbDebug("parse_waypt: %5d", object_num);
    }
  }

  wpt_tmp->latitude = lat_mm_to_deg(gbfgetint32(file_in));
  wpt_tmp->longitude = lon_mm_to_deg(gbfgetint32(file_in));
  /*
   * From available data, looks like stream version = 0 used float to store for altitude in feet
   * and version 1 used int32.  If that is found to not be the case may need to add flag for input.
   * Only have a small sample size.
   */
  if (rstream_version == 0) {
    float read_alt = gbfgetflt(file_in);
    if (std::isnan(read_alt)) {
      wpt_tmp->altitude = unknown_alt;
    } else if (METERS_TO_FEET(wpt_tmp->altitude) <= -10000) {
      wpt_tmp->altitude = unknown_alt;
    } else {
      wpt_tmp->altitude = FEET_TO_METERS(read_alt);
    }
  } else {
    wpt_tmp->altitude = FEET_TO_METERS(gbfgetint32(file_in));
    if (METERS_TO_FEET(wpt_tmp->altitude) <= -10000) {
      wpt_tmp->altitude = unknown_alt;
    }
  }

  /* Waypoint name; input is 1 byte per char */
  QString name = lowranceusr4_readstr(file_in, 1);
  if (!name.isEmpty()) {
    wpt_tmp->shortname = name;
  }

  if (global_opts.debug_level > 1) {
    if (global_opts.debug_level == 99) {
      if (wpt_tmp->shortname.length() > 16) {
        gbDebug(" %.13s...", gbLogCStr(wpt_tmp->shortname));
      } else {
        gbDebug(" %16.16s", gbLogCStr(wpt_tmp->shortname));
      }
      gbDebug(" %+15.10f %+15.10f", wpt_tmp->latitude, wpt_tmp->longitude);
      if (wpt_tmp->altitude == unknown_alt) {
        gbDebug(" %13s", "UNKNOWN ALT");
      } else {
        gbDebug(" %5d %7.1f", (int)METERS_TO_FEET(wpt_tmp->altitude), wpt_tmp->altitude);
      }
    } else {
      gbDebug("parse_waypt: Waypt name = '%s' Lat = %+f Lon = %+f alt = ",
             gbLogCStr(wpt_tmp->shortname), wpt_tmp->latitude, wpt_tmp->longitude);
      if (wpt_tmp->altitude == unknown_alt) {
        gbDebug("UNKNOWN ALT\n");
      } else {
        gbDebug("%d (%f)\n", (int)METERS_TO_FEET(wpt_tmp->altitude), wpt_tmp->altitude);
      }
    }
  }

  /* Description; input is 1 byte per char */
  QString description = lowranceusr4_readstr(file_in, 1);
  if (!description.isEmpty()) {
    wpt_tmp->description = description;
  }

  /* Input is the number of seconds since Jan. 1, 2000 */
  int64_t waypt_time = gbfgetint32(file_in);
  if (waypt_time) {
    /* Waypoint needs the number of seconds since UNIX Epoch (Jan 1, 1970) */
    wpt_tmp->SetCreationTime(waypt_time += base_time_secs);
  }

  if (global_opts.debug_level > 2) {
    if (global_opts.debug_level == 99) {
      gbDebug(" '%s'", gbLogCStr(wpt_tmp->GetCreationTime().toString(u"yyyy/MM/dd hh:mm:ss")));
    } else {
      gbDebug("parse_waypt: creation time '%s', waypt_time %" PRId64 "\n",
             gbLogCStr(wpt_tmp->GetCreationTime().toString(u"yyyy/MM/dd hh:mm:ss")), waypt_time);
    }
  }

  /* Symbol ID */
  int icon_number = gbfgetint32(file_in);
  if (icon_number == 0) {
    // Assume have extra field generated by HOOK2 in USR 2 & 3 formats, read again
    icon_number = gbfgetint32(file_in);
  }
  if (global_opts.debug_level == 99) {
    gbDebug(" %08x (%d)", icon_number, icon_number);
  }
  wpt_tmp->icon_descr = lowranceusr_find_desc_from_icon_number(icon_number);

  /* Waypoint Type (USER, TEMPORARY, POINT_OF_INTEREST) */
  short waypt_type = gbfgetint16(file_in);
  if (global_opts.debug_level > 2) {
    if (global_opts.debug_level == 99) {
      gbDebug(" %04x (%d)", (int)waypt_type, (int)waypt_type);
    } else {
      gbDebug("parse_waypt: waypt_type = %d\n",waypt_type);
    }
  }

  // Version 3 has a depth field here.
  if (reading_version == 3) {
    float depth_feet = gbfgetflt(file_in);
    if (std::abs(depth_feet - 99999.0)  > .1) {
      wpt_tmp->set_depth(FEET_TO_METERS(depth_feet));
      if (global_opts.debug_level == 99) {
        gbDebug("   %10.1f", depth_feet);
      }
    } else {
      if (global_opts.debug_level == 99) {
        gbDebug("      UNKNOWN");
      }
    }
  }

  if (global_opts.debug_level == 99) {
    gbDebug("\n");
  }
}

void
LowranceusrFormat::lowranceusr4_parse_waypt(Waypoint* wpt_tmp)
{
  auto* fsdata = new lowranceusr4_fsdata;
  wpt_tmp->fs.FsChainAdd(fsdata);

  if (reading_version > 4) {
    /* USR 5 and 6 have four additional data values at the start of each Waypoint */
    /* These values are used to identify global way points that define routes */
    fsdata->UUID1 = gbfgetint32(file_in);
    fsdata->UUID2 = gbfgetint32(file_in);
    fsdata->UUID3 = gbfgetint32(file_in);
    fsdata->UUID4 = gbfgetint32(file_in);
  }

  /* UID unit number */
  fsdata->uid_unit = gbfgetint32(file_in);

  /* 64-bit UID sequence number */
  fsdata->uid_seq_low = gbfgetint32(file_in);
  fsdata->uid_seq_high = gbfgetint32(file_in);

  /* Waypt stream version number */
  int waypoint_version = gbfgetint16(file_in);

  /* Waypoint name; input is 2 bytes per char, we convert to 1 */
  QString name = lowranceusr4_readstr(file_in, 2);
  if (!name.isEmpty()) {
    wpt_tmp->shortname = name;
  }

  if (reading_version > 4) {
    /* USR 5 and 6 have a second Unit Number captured in Waypoints */
    fsdata->uid_unit2 = gbfgetint32(file_in);
  }

  /* Long/Lat */
  wpt_tmp->longitude = lon_mm_to_deg(gbfgetint32(file_in));
  wpt_tmp->latitude = lat_mm_to_deg(gbfgetint32(file_in));

  /* Flags, discard for now */
  fsdata->flags = gbfgetint32(file_in);

  /* Icon ID */
  fsdata->icon_num = gbfgetint16(file_in);
  wpt_tmp->icon_descr = lowranceusr4_find_desc_from_icon_number(fsdata->icon_num);

  /* Color ID */
  fsdata->color = gbfgetint16(file_in);
  fsdata->color_desc = lowranceusr4_find_color_from_icon_number_plus_color_index(fsdata->icon_num, fsdata->color);

  /* Waypoint descr; input is 2 bytes per char, we convert to 1 */
  QString desc = lowranceusr4_readstr(file_in, 2);
  if (!desc.isEmpty()) {
    wpt_tmp->description = desc;
  }

  /* Alarm radius; XXX: I'm not sure what the units are here,
     assuming meters but may be feet? */
  wpt_tmp->set_proximity(gbfgetflt(file_in));

  /* Creation date/time */
  /* The date is a Julian day number, and the time is a unix timestamp. */
  uint create_date = gbfgetint32(file_in);
  uint create_time = gbfgetint32(file_in);

  // Julian date 2440487 is 1/1/1970.  If that's the date we're working
  // with, as a practical matter, we have no date, so don't even compute
  // or set it.
  if (create_date > 2440587) {
    wpt_tmp->SetCreationTime(lowranceusr4_get_timestamp(create_date, create_time));
  }

  /* Unused byte */
  char unused_byte = gbfgetc(file_in);

  /* Depth in feet */
  fsdata->depth = gbfgetflt(file_in);

  /* Loran data, discard for now */
  int loran_GRI = gbfgetint32(file_in);
  int loran_Tda = gbfgetint32(file_in);
  int loran_Tdb = gbfgetint32(file_in);

  if (global_opts.debug_level > 1) {
    if (global_opts.debug_level == 99) {
      gbDebug("parse_waypoints: ");
      if (reading_version > 4) {
        gbDebug("%08x %08x %08x %08x ",
               fsdata->UUID1, fsdata->UUID2, fsdata->UUID3, fsdata->UUID4);
      }
      gbDebug(" %10u %8d %8d %8d %6s",
             fsdata->uid_unit, fsdata->uid_seq_low, fsdata->uid_seq_high,
             waypoint_version, QByteArray::number(name.length()).constData());
      if (name.length() > 16) {
        gbDebug(" %13.13s...", gbLogCStr(name));
      } else {
        gbDebug(" %16.16s", gbLogCStr(name));
      }
      if (reading_version > 4) {
        gbDebug("  %10u ", fsdata->uid_unit2);
      }
      gbDebug(" %+15.10f %+15.10f", wpt_tmp->longitude, wpt_tmp->latitude);
      gbDebug(" %08x %4d %4d %7s", fsdata->flags, fsdata->icon_num, fsdata->color,
             (fsdata->color_desc == nullptr ? "unk" : gbLogCStr(fsdata->color_desc)));
      if (desc.length() > 16) {
        gbDebug(" %6s %.13s...", QByteArray::number(desc.length()).constData(), gbLogCStr(desc));
      } else {
        gbDebug(" %6s %16s", QByteArray::number(desc.length()).constData(), gbLogCStr(desc));
      }
      gbDebug(" '%s'", gbLogCStr(wpt_tmp->GetCreationTime().toString(u"yyyy/MM/dd hh:mm:ss")));
      gbDebug(" %08x %8.3f %08x %08x %08x\n",
             unused_byte, fsdata->depth, loran_GRI, loran_Tda, loran_Tdb);
    } else {
      gbDebug("parse_waypoints: version = %d, name = %s, uid_unit = %u, "
             "uid_seq_low = %d, uid_seq_high = %d, lat = %+.10f, lon = %+.10f, depth = %f\n",
             waypoint_version, gbLogCStr(wpt_tmp->shortname), fsdata->uid_unit,
             fsdata->uid_seq_low, fsdata->uid_seq_high,
             wpt_tmp->longitude, wpt_tmp->latitude, fsdata->depth);
    }
  }
}

void
LowranceusrFormat::lowranceusr_parse_waypts()
{
  int NumWaypoints;

  if (reading_version < 4) {
    /* USR versions 2 & 3 have a 16 bit count */
    NumWaypoints = gbfgetint16(file_in);
  } else {
    /* Starting with USR version 4 have 32 bit count */
    NumWaypoints = gbfgetint32(file_in);
  }

  if (global_opts.debug_level >= 1) {
    gbDebug("parse_waypts: Num Waypoints = %d\n", NumWaypoints);
  }

  if (global_opts.debug_level == 99) {
    if (reading_version > 3) {
      gbDebug("parse_waypts: ");
      if (reading_version > 4) {
        gbDebug("Universal ID                        ");
      }
      gbDebug("              Sequence Number  Stream  Waypoint\n");

      gbDebug("parse_waypoints: ");
      if (reading_version > 4) {
        gbDebug("    ID1      ID2      ID3      ID4  ");
      }
      gbDebug("Unit Number     Low      High  Version Length Name            ");
      if (reading_version > 4) {
        gbDebug(" Unit Number2");
      }
      gbDebug(" Latitude        Longitude       Flags    ICON Color        Length Description     ");
      gbDebug(" Date       Time  Unknown  Depth    LoranGRI LoranTda LoranTdb\n");

      gbDebug("parse_waypoints: ");
      if (reading_version > 4) {
        gbDebug("-------- -------- -------- -------- ");
      }
      gbDebug("----------- -------- -------- -------- ------ ----------------");
      if (reading_version > 4) {
        gbDebug(" ------------");
      }
      gbDebug(" --------------- --------------- -------- ---- ------------ ------ ----------------");
      gbDebug(" ---------- ----- -------- -------- -------- -------- --------\n");
    } else {
      gbDebug("parse_waypts: Number Name            Longitude       Latitude       Altitude       Time            ");
      gbDebug(" ICON ID (dec)    Flag (dec)");
      if (reading_version == 3) {
        gbDebug(" Depth (ft)");
      }
      gbDebug("\n");
      gbDebug("parse_waypts: ------ --------------- --------------- -------------- -------------- ----------------");
      gbDebug(" ---------------- ----------");
      if (reading_version == 3) {
        gbDebug(" ----------");
      }
      gbDebug("\n");
    }
  }

  for (int i = 0; i < NumWaypoints && !gbfeof(file_in); i++) {
    auto* wpt_tmp = new Waypoint;

    switch (reading_version) {
    case 2:
    case 3:
      lowranceusr_parse_waypt(wpt_tmp, 1);  /* Indicate object number present */
      break;
    case 4:
    case 5:
    case 6:
      lowranceusr4_parse_waypt(wpt_tmp);
      break;
    default:
      Warning() << "Unknown internal version " << reading_version;
    }
    waypt_add(wpt_tmp);
  }
}

void
LowranceusrFormat::lowranceusr_parse_route()
{
  /* route name */
  QString name = lowranceusr4_readstr(file_in, 1);
  if (!name.isEmpty()) {
    rte_head->rte_name = name;
  }

  /* num Legs */
  short num_legs = gbfgetint16(file_in);

  if (global_opts.debug_level > 1) {
    gbDebug("parse_route: Route '%s', Num Legs = %d", gbLogCStr(name), num_legs);
  }

  /* route reversed */
  char reversed = gbfgetc(file_in);
  if (global_opts.debug_level > 1) {
    gbDebug(", reversed '%x' - %s\n", reversed, (reversed ? "Yes" : "No"));
  }

  if (global_opts.debug_level == 99) {
    gbDebug("parse_route:  Name            Longitude        Latitude       Altitude      Time             Unknown  ICON ID (dec)    Flag (dec) Depth (ft)\n");
    gbDebug("parse_route:  --------------- ---------------  -------------- ------------- ---------------- -------- ---------------- ---------- ----------\n");
  }

  /* waypoints */
  for (int j = 0; j < num_legs; j++) {
    auto* wpt_tmp = new Waypoint;
    if (global_opts.debug_level == 99) {
      gbDebug("parse_route:");
    }
    lowranceusr_parse_waypt(wpt_tmp, 0); /* Indicate object number missing */
    route_add_wpt(rte_head, wpt_tmp);
  }
}

void
LowranceusrFormat::lowranceusr4_parse_route()
{
  int UUID1 = 0;
  int UUID2 = 0;
  int UUID3 = 0;
  int UUID4 = 0;

  auto* fsdata = new lowranceusr4_fsdata;
  rte_head->fs.FsChainAdd(fsdata);

  if (reading_version >= 5) {
    /* Routes have Universal IDs */
    UUID1 = gbfgetint32(file_in);
    UUID2 = gbfgetint32(file_in);
    UUID3 = gbfgetint32(file_in);
    UUID4 = gbfgetint32(file_in);
  }

  /* UID unit number */
  fsdata->uid_unit = gbfgetint32(file_in);
  if (global_opts.debug_level > 1) {
    gbDebug("parse_route: Unit %u (0x%08x)\n", fsdata->uid_unit, fsdata->uid_unit);
  }

  /* 64-bit UID sequence number */
  fsdata->uid_seq_low = gbfgetint32(file_in);
  fsdata->uid_seq_high = gbfgetint32(file_in);

  /* Route stream version number */
  int route_version = gbfgetint16(file_in);
  if (global_opts.debug_level > 1) {
    gbDebug("parse_route: Version = %d\n", route_version);
  }

  /* Route name; input is 2 bytes per char, we convert to 1 */
  QString buff = lowranceusr4_readstr(file_in, 2);
  if (!buff.isEmpty()) {
    rte_head->rte_name = buff;
  }

  if (reading_version >= 5) {
    /* USR Version 5 and greater include unit ID in each route */
    gbfgetint32(file_in);
  }

  int num_legs = gbfgetint32(file_in);

  if (global_opts.debug_level > 1) {
    if (reading_version >= 5) {
      gbDebug("parse_route: route '%s' (UUID %08x %08x %8x %08x) has %d legs\n",
             gbLogCStr(rte_head->rte_name), UUID1, UUID2, UUID3, UUID4, num_legs);
    } else {
      gbDebug("parse_route: route '%s' has %d legs\n",
             gbLogCStr(rte_head->rte_name), num_legs);
    }
  }

  if (reading_version <= 4) {
    /* Use UID based sequence numbers for route */
    for (int j = 0; j < num_legs; ++j) {
      uint uid_unit = gbfgetint32(file_in);
      uint uid_seq_low = gbfgetint32(file_in);
      uint uid_seq_high = gbfgetint32(file_in);
      const Waypoint* wpt_tmp = lowranceusr4_find_waypt(uid_unit, uid_seq_low, uid_seq_high);
      if (wpt_tmp) {
        if (global_opts.debug_level >= 2) {
          gbDebug("parse_route: added leg #%d routepoint %s (%+.10f, %+.10f)\n",
                 j, gbLogCStr(wpt_tmp->shortname), wpt_tmp->longitude, wpt_tmp->latitude);
        }
        route_add_wpt(rte_head, new Waypoint(*wpt_tmp));
      }
    }
  } else {
    /* Use global sequence number for route */
    for (int j = 0; j < num_legs; ++j) {
      UUID1 = gbfgetint32(file_in);
      UUID2 = gbfgetint32(file_in);
      UUID3 = gbfgetint32(file_in);
      UUID4 = gbfgetint32(file_in);
      const Waypoint* wpt_tmp = lowranceusr4_find_global_waypt(UUID1, UUID2, UUID3, UUID4);
      if (wpt_tmp) {
        if (global_opts.debug_level >= 2) {
          gbDebug("parse_route: added leg #%d routepoint %s (%+.10f, %+.10f)\n",
                 j, gbLogCStr(wpt_tmp->shortname), wpt_tmp->longitude, wpt_tmp->latitude);
        }
        route_add_wpt(rte_head, new Waypoint(*wpt_tmp));
      }
    }
  }

  if (reading_version > 4) {
    /* USR Version 5 or greater, more mystery data, ignore for now */
    gbfgetint32(file_in);
    gbfgetint32(file_in);
    gbfgetc(file_in);
  }

  /* Mystery byte, discard */
  if (global_opts.debug_level == 99) {
    gbDebug("parse_route: end of route %02x\n", gbfgetc(file_in));
  } else {
    gbfgetc(file_in);
  }
}

void
LowranceusrFormat::lowranceusr_parse_routes()
{
  short int num_routes;

  if (reading_version < 4) {
    /* USR versions 2 & 3 have a 16 bit count */
    num_routes = gbfgetint16(file_in);
  } else {
    /* Starting with USR version 4 have 32 bit count */
    num_routes = gbfgetint32(file_in);
  }

  if (global_opts.debug_level >= 1) {
    gbDebug("parse_routes: Num Routes = %d\n", num_routes);
  }

  for (int i = 0; i < num_routes; i++) {
    rte_head = new route_head;
    route_add_head(rte_head);
    rte_head->rte_num = i+1;

    if (reading_version < 4) {
      lowranceusr_parse_route();
    } else {
      lowranceusr4_parse_route();
    }
  }
}

/*
 * Icons are automatically converted to waypoints unless
 * option of ignoreicons is used
 */
void
LowranceusrFormat::lowranceusr_parse_icons()
{
  short int num_icons = gbfgetint16(file_in);

  if (global_opts.debug_level >= 1) {
    gbDebug("parse_icons: Num Event Marker Icons = %d\n", num_icons);
  }

  for (int i = 0; i < num_icons && !gbfeof(file_in); i++) {
    double latitude    = lat_mm_to_deg(gbfgetint32(file_in));
    double longitude   = lon_mm_to_deg(gbfgetint32(file_in));
    int    icon_number = gbfgetint32(file_in);

    if (!opt_ignoreicons) {
      auto* wpt_tmp = new Waypoint;

      /* position coord lat & long */
      wpt_tmp->latitude = latitude;
      wpt_tmp->longitude = longitude;
      wpt_tmp->altitude = unknown_alt;

      /* shortname */
      QString name = "Event Marker " + QString::number(i+1);
      wpt_tmp->shortname = name;

      /* symbol */
      wpt_tmp->icon_descr = lowranceusr_find_desc_from_icon_number(icon_number);

      if (global_opts.debug_level > 1) {
        gbDebug("parse_icons: '%s' %d %16.16s %+15.10f %+15.10f\n",
               gbLogCStr(wpt_tmp->shortname), icon_number, gbLogCStr(wpt_tmp->icon_descr), wpt_tmp->latitude, wpt_tmp->longitude);
      }
      waypt_add(wpt_tmp);
    }
  }
}

void
LowranceusrFormat::lowranceusr_parse_trail(int* trail_num)
{
  /* Trail name; input is 1 byte per char */
  QString name = lowranceusr4_readstr(file_in, 1);
  if (!name.isEmpty()) {
    trk_head->rte_name = name;
  }

  if (global_opts.debug_level > 1) {
    gbDebug("parse_trails: Trail '%s'\n", gbLogCStr(trk_head->rte_name));
  }

  /* visible */
  char visible = gbfgetc(file_in);

  if (global_opts.debug_level > 1) {
    gbDebug("parse_trails: Visible '%x' - %s\n", visible, (visible ? "Yes" : "No"));
  }

  /* num trail points */
  short num_trail_points = gbfgetint16(file_in);

  if (global_opts.debug_level > 1) {
    gbDebug("parse_trails: Num Trail Points = %d\n", num_trail_points);
  }

  /* max trail size */
  int itmp = gbfgetint16(file_in);

  if (global_opts.debug_level > 1) {
    gbDebug("parse_trails: Max Trail size = %d\n", itmp);
  }

  if (num_trail_points) {

    while (num_trail_points && !gbfeof(file_in)) {
      /* num section points */
      num_section_points = gbfgetint16(file_in);

      if (global_opts.debug_level > 1) {
        gbDebug("parse_trails: Num Section Points = %d\n", num_section_points);
      }

      for (int j = 0; j < num_section_points && !gbfeof(file_in); j++, num_trail_points--) {
        auto* wpt_tmp = new Waypoint;
        wpt_tmp->latitude = lat_mm_to_deg(gbfgetint32(file_in));
        wpt_tmp->longitude = lon_mm_to_deg(gbfgetint32(file_in));

        char continuous_flag = gbfgetc(file_in);
        if (!continuous_flag) {
          if (opt_seg_break) {
            /* option to break trails into segments was specified */
            if (!trk_head->rte_waypt_empty()) {
              auto* trk_tmp = new route_head;
              trk_tmp->rte_num = ++(*trail_num);
              trk_tmp->rte_name = trk_head->rte_name;
              track_add_head(trk_tmp);
              trk_head = trk_tmp;
            }
          } else {
            wpt_tmp->wpt_flags.new_trkseg = 1;
          }
        }

        /* Track Point */
        track_add_wpt(trk_head, wpt_tmp);

        if (global_opts.debug_level > 2) {
          gbDebug("parse_trails: Trail pt lat %f lon %f\n", wpt_tmp->latitude, wpt_tmp->longitude);
        }
      }
    }
  } else {
    /* remove the trail since it's empty */
    track_del_head(trk_head);
  }
}

void
LowranceusrFormat::lowranceusr4_parse_trail(int* trail_num)
{
  auto* fsdata = new lowranceusr4_fsdata;
  trk_head->fs.FsChainAdd(fsdata);

  /* UID unit number */
  fsdata->uid_unit = gbfgetint32(file_in);

  /* 64-bit UID sequence number */
  fsdata->uid_seq_low = gbfgetint32(file_in);
  fsdata->uid_seq_high = gbfgetint32(file_in);

  /* Trail stream version number */
  int trail_version = gbfgetint16(file_in);
  if (global_opts.debug_level == 99) {
    gbDebug("parse_trails: trail Version %d\n", trail_version);
  }
  if ((trail_version < 3) || (trail_version > 5)) {
    gbFatal("trail version %d not supported!!", trail_version);
  }

  /* Trail name; input is 2 bytes per char, we convert to 1 */
  QString name = lowranceusr4_readstr(file_in, 2);
  if (!name.isEmpty()) {
    trk_head->rte_name = name;
  }
  if (global_opts.debug_level >= 2) {
    gbDebug("parse_trails: Trail '%s'\n", gbLogCStr(trk_head->rte_name));
  }

  /* Flags, discard for now */
  int trail_flags = gbfgetint32(file_in);

  /* Color ID, discard for now */
  int trail_color = gbfgetint32(file_in);

  /* Comment/description; input is 2 bytes per char, we convert to 1 */
  QString desc = lowranceusr4_readstr(file_in, 2);
  if (!desc.isEmpty()) {
    trk_head->rte_desc = desc;
  }
  if (global_opts.debug_level == 99) {
    gbDebug("parse_trails: Comment '%s'\n", gbLogCStr(desc));
  }

  /* Creation date/time, discard for now */
  /* The date is a Julian day number, and the time is a unix timestamp. */
  int create_date = gbfgetint32(file_in);
  int create_time = gbfgetint32(file_in);
  if (global_opts.debug_level == 99) {
    QDateTime qdt = lowranceusr4_get_timestamp(create_date, create_time);
    gbDebug("parse_trails: creation date/time = %s\n", gbLogCStr(qdt.toString(u"yyyy-MM-dd hh:mm:ss AP")));
  }

  /* Some flag bytes */
  if (global_opts.debug_level == 99) {
    gbDebug("parse_trails: unknown flag bytes %02x %02x %02x\n",
           gbfgetc(file_in), gbfgetc(file_in), gbfgetc(file_in));
  } else {
    /* just discard */
    gbfgetc(file_in);
    gbfgetc(file_in);
    gbfgetc(file_in);
  }

  /* Mysterious attribute "data count" */
  int attr_count = gbfgetint32(file_in);
  if (global_opts.debug_level == 99) {
    gbDebug("parse_trails: attribute count %4d : (", attr_count);
    for (int i=0; i<attr_count; i++) {
      if (trail_version == 5) {
        gbDebug("%08x ", gbfgetint32(file_in));
      } else {
        gbDebug("%02x ", gbfgetc(file_in));
      }
    }
    gbDebug(")\n");
  } else {
    /* just discard */
    for (int i=0; i<attr_count; i++) {
      if (trail_version == 5) {
        gbfgetint32(file_in);
      } else {
        gbfgetc(file_in);
      }
    }
  }

  int num_trail_pts = gbfgetint32(file_in);

  if (global_opts.debug_level >= 2) {
    gbDebug("parse_trails: trail %d name='%s' color=%d flags=%d has %d (%x) trailpoints\n",
           *trail_num, gbLogCStr(trk_head->rte_name), trail_color, trail_flags, num_trail_pts, num_trail_pts);

    if (global_opts.debug_level == 99) {
      gbDebug("parse_trails: Longitude      Latitude       Flag/Value pairs (01=Speed)\n");
      gbDebug("parse_trails: -------------- -------------- -- -------- -- -------- -- --------\n");
    }
  }
  for (int j = 0; j < num_trail_pts; ++j) {
    auto* wpt_tmp = new Waypoint;

    /* Some unknown bytes */
    gbfgetint16(file_in);
    gbfgetc(file_in);

    /* POSIX timestamp (a.k.a. UNIX Epoch) - seconds since Jan 1, 1970 */
    wpt_tmp->SetCreationTime(gbfgetint32(file_in));

    /* Long/Lat */
    wpt_tmp->longitude = gbfgetdbl(file_in) / DEGREESTORADIANS; /* rad to deg */
    wpt_tmp->latitude = gbfgetdbl(file_in) / DEGREESTORADIANS;

    if (global_opts.debug_level >= 2) {
      if (global_opts.debug_level == 99) {
        gbDebug("parse_trails: %+14.9f %+14.9f", wpt_tmp->longitude, wpt_tmp->latitude);
        gbDebug(" '%s'", gbLogCStr(wpt_tmp->GetCreationTime().toString(u"yyyy/MM/dd hh:mm:ss")));
      } else {
        gbDebug("parse_trails: added trailpoint %+.9f,%+.9f to trail %s\n",
               wpt_tmp->longitude, wpt_tmp->latitude, gbLogCStr(trk_head->rte_name));
      }
    }

    track_add_wpt(trk_head, wpt_tmp);

    /* Mysterious per-trailpoint data, toss it for now */
    int M = gbfgetint32(file_in);
    for (int k = 0; k < M; ++k) {
      int flag = gbfgetc(file_in);
      float value = gbfgetflt(file_in);
      if (global_opts.debug_level == 99) {
        gbDebug(" %02x %f", flag, value);
      }
    }

    if (global_opts.debug_level == 99) {
      gbDebug("\n");
    }
  }
}

void
LowranceusrFormat::lowranceusr_parse_trails()
{
  int num_trails;
  int trail_num;

  if (reading_version < 4) {
    /* USR versions 2 & 3 have a 16 bit count */
    num_trails = gbfgetint16(file_in);
  } else {
    /* Starting with USR version 4 have 32 bit count */
    num_trails = gbfgetint32(file_in);
  }

  if (global_opts.debug_level >= 1) {
    gbDebug("parse_trails: Num Trails = %d\n", num_trails);
  }

  for (int i = trail_num = 0; i < num_trails && !gbfeof(file_in); i++) {
    trk_head = new route_head;
    trk_head->rte_num = ++trail_num;
    track_add_head(trk_head);

    if (reading_version < 4) {
      lowranceusr_parse_trail(&trail_num);
    } else {
      lowranceusr4_parse_trail(&trail_num);
    }
  }
}

void
LowranceusrFormat::read()
{
  reading_version = gbfgetint16(file_in);
  rstream_version = gbfgetint16(file_in);
  if (global_opts.debug_level >= 1) {
    gbDebug("input_file: USR File Format %d (Version = %d)\n", reading_version, rstream_version);
  }

  if ((reading_version < 2) || (reading_version > 6)) {
    gbFatal("input file is a USR format that is not supported\n");
  }

  if (reading_version >= 4) {

    /* Starting with USR version 4 have an unknown here */
    int unknown = gbfgetint32(file_in);
    if (global_opts.debug_level >= 1) {
      gbDebug("input_file: Unknown %d (%x)\n", unknown, unknown);
    }

    /* USR files also now contain a file title */
    QString title = lowranceusr4_readstr(file_in, 1);
    if (!title.isEmpty() && global_opts.debug_level >= 1) {
      gbDebug("file title: '%s'\n", gbLogCStr(title));
    }

    /* AND a date created string */
    QString creation_date = lowranceusr4_readstr(file_in, 1);
    if (!creation_date.isEmpty()  && global_opts.debug_level >= 1) {
      gbDebug("date string: '%s'\n", gbLogCStr(creation_date));
    }

    /* Creation date/time, discard for now */
    /* The date is a Julian day number, and the time is a unix timestamp. */
    int create_date = gbfgetint32(file_in);
    int create_time = gbfgetint32(file_in);
    if (global_opts.debug_level >= 1) {
      QDateTime qdt = lowranceusr4_get_timestamp(create_date, create_time);
      gbDebug("creation date/time : '%s'\n", gbLogCStr(qdt.toString(u"yyyy-MM-dd hh:mm:ss AP")));
    }

    unsigned char byte = gbfgetc(file_in); /* unused, apparently */
    (void) byte;

    /* AND the serial number of the unit that created the file */
    uint serial_num = gbfgetint32(file_in);
    if (global_opts.debug_level >= 1) {
      gbDebug("device serial number: %u\n", serial_num);
    }

    /* AND a comment on the file contents */
    QString comment = lowranceusr4_readstr(file_in, 1);
    if (!comment.isEmpty() && global_opts.debug_level >= 1) {
      gbDebug("content description: '%s'\n", gbLogCStr(comment));
    }
  }

  lowranceusr_parse_waypts();
  lowranceusr_parse_routes();

  if ((reading_version == 2) || (reading_version == 3)) {
    lowranceusr_parse_icons();      // Event Marker ICONS exists only in USR 2 & 3 format
  }

  lowranceusr_parse_trails();

}

void
LowranceusrFormat::lowranceusr_waypt_disp(const Waypoint* wpt)
{
  int SymbolId;
  int alt;

  int Lat = lat_deg_to_mm(wpt->latitude);
  int Lon = lon_deg_to_mm(wpt->longitude);

  if (wpt->altitude == unknown_alt) {
    alt = UNKNOWN_USR_ALTITUDE;
  } else {
    alt = METERS_TO_FEET(wpt->altitude);
  }

  gbfputint32(Lat, file_out);
  gbfputint32(Lon, file_out);
  gbfputint32(alt, file_out);

  /* Try and make sure we have a name */
// this kind of thing would probably be more readable like
// name = blah.
// if name.isEmpty()
//   name = planB;
// if name.isEmpty()
//   name = planC;
// ...
  QString name;
  if ((wpt->shortname.isEmpty()) || global_opts.synthesize_shortnames) {
    if (!wpt->description.isEmpty() && global_opts.synthesize_shortnames) {
      name = mkshort_handle->mkshort_from_wpt(wpt);
    } else if (!wpt->shortname.isEmpty()) {
      name = wpt->shortname;
    } else if (!wpt->description.isEmpty()) {
      name = wpt->description;
    }
  } else {
    name = wpt->shortname;
  }

  if (global_opts.debug_level > 2) {
    /* print lat/lon/alt on one easily greppable line */
    gbDebug("waypt_disp: Waypt name = '%s' Lat = %+16.10f  Lon = %+16.10f  Alt = %f\n",
           gbLogCStr(wpt->shortname), wpt->latitude, wpt->longitude, wpt->altitude);
  }

  QByteArray name_qba = name.toLatin1();
  int text_len = name_qba.length();
  if (text_len > MAXUSRSTRINGSIZE) {
    text_len = MAXUSRSTRINGSIZE;
  }
  gbfputint32(text_len, file_out);
  gbfwrite(name_qba.constData(), 1, text_len, file_out);

  if (global_opts.debug_level > 1) {
    gbDebug("waypt_disp: Waypt name = '%s' ", gbLogCStr(name));
  }

  /**
   * Comments are now used by the iFinder (Expedition C supports them)
   */
  if (wpt->description != wpt->shortname) {
    QString comment = wpt->description;
    text_len = comment.length();
    if (text_len > MAXUSRSTRINGSIZE) {
      text_len = MAXUSRSTRINGSIZE;
    }
    gbfputint32(text_len, file_out);
    gbfwrite(CSTR(comment), 1, text_len, file_out);
  } else {
    text_len = 0;
    gbfputint32(text_len, file_out);
  }

  /* Waypoint creation time stored as seconds since UNIX Epoch (Jan 1, 1970) */
  int64_t waypt_time;
  if ((waypt_time = wpt->creation_time.toSecsSinceEpoch()) > base_time_secs) {
    /* This should always be true */
    /* Lowrance needs it as seconds since Jan 1, 2000 */
    waypt_time -= base_time_secs;
    if (global_opts.debug_level >= 2) {
      gbDebug("creation_time %" PRId64 ", '%s'", waypt_time, gbLogCStr(wpt->GetCreationTime().toString(u"yyyy-MM-dd hh:mm:ss")));
    }
  } else {
    /* If false, make sure it is an unknown time value */
    waypt_time = 0;
    if (global_opts.debug_level >= 2) {
      gbDebug("creation_time UNKNOWN");
    }
  }

  gbfputint32(waypt_time, file_out);

  if (!wpt->gc_data->get_icon().isEmpty() && wpt->icon_descr.compare(u"Geocache Found") == 0) {
    SymbolId = lowranceusr_find_icon_number_from_desc(wpt->gc_data->get_icon());
  } else {
    SymbolId = lowranceusr_find_icon_number_from_desc(wpt->icon_descr);
  }
  /* If the waypoint is archived or disabled, use a "disabled" icon instead. */
  if ((wpt->gc_data->is_archived == Geocache::status_t::gs_true) ||
      (wpt->gc_data->is_available == Geocache::status_t::gs_false)) {
    SymbolId = lowranceusr_find_icon_number_from_desc(DISABLED_CACHE_TXT);
  }

  gbfputint32(SymbolId, file_out);

  /* USER waypoint type */
  short int WayptType = 0;
  gbfputint16(WayptType, file_out);

  if (writing_version == 3) {
    float depth = wpt->depth_has_value() ?
                  METERS_TO_FEET(wpt->depth_value()) : -99999.0;
    gbfputint32(depth, file_out);
  }

  if (global_opts.debug_level > 1) {
    gbDebug("\n");
  }
}

void
LowranceusrFormat::lowranceusr4_waypt_disp(const Waypoint* wpt)
{
  const auto* fs = reinterpret_cast<lowranceusr4_fsdata*>(wpt->fs.FsChainFind(kFsLowranceusr4));

  /* UID unit number */
  if (opt_serialnum_i > 0) {
    gbfputint32(opt_serialnum_i, file_out);  // use option serial number if specified
  } else if (fs != nullptr) {
    gbfputint32(fs->uid_unit, file_out);  // else use serial number from input if valid
  } else {
    gbfputint32(0, file_out);  // else Write Serial Number = 0
  }

  /* 64-bit UID sequence number */
  gbfputint32(waypt_uid++, file_out);
  gbfputint32(0, file_out);

  /* Waypt stream version number: this always seems to be 2 in my data
     so that's what I'll use */
  gbfputint16(2, file_out);

  /* Waypt name */
  lowranceusr4_writestr(wpt->shortname, file_out, 2);

  /* Long/Lat */
  gbfputint32(lon_deg_to_mm(wpt->longitude), file_out);
  gbfputint32(lat_deg_to_mm(wpt->latitude), file_out);

  /* Flags: this always seems to be 2 or 4 in my data, not sure what
     it means */
  gbfputint32(2, file_out);

  int SymbolId;
  int ColorId;
  if (!wpt->gc_data->get_icon().isEmpty() && wpt->icon_descr.compare(u"Geocache Found") == 0) {
    if (writing_version == 4) {
      SymbolId = lowranceusr4_find_icon_number_from_desc(wpt->icon_descr);
    } else {
      SymbolId = lowranceusr_find_icon_number_from_desc(wpt->gc_data->get_icon());
    }
    ColorId = 0; // default
  } else {
    SymbolId = lowranceusr4_find_icon_number_from_desc(wpt->icon_descr);
    if (fs != nullptr) {
      ColorId = lowranceusr4_find_index_from_icon_desc_and_color_desc(wpt->icon_descr, fs->color_desc);
    } else {
      ColorId = DEF_USR4_COLOR; // default
    }
  }
  /* If the waypoint is archived or disabled, use a "disabled" icon instead. */
  if ((wpt->gc_data->is_archived == Geocache::status_t::gs_true) ||
      (wpt->gc_data->is_available == Geocache::status_t::gs_false)) {
    SymbolId = lowranceusr_find_icon_number_from_desc(DISABLED_CACHE_TXT);
    ColorId = 0; // default
  }
  gbfputint16(SymbolId, file_out);
  gbfputint16(ColorId, file_out);

  /* Waypt description */
  lowranceusr4_writestr(wpt->description, file_out, 2);

  /* Alarm radius */
  gbfputflt(wpt->proximity_value_or(0.0), file_out);

  /* Creation date/time */
  auto ts = lowranceusr4_jd_from_timestamp(wpt->GetCreationTime());
  gbfputint32(ts.julian_day_number, file_out);
  gbfputint32(ts.milliseconds, file_out);

  /* Unused byte */
  gbfputc(0, file_out);

  /* Depth in feet */
  if (fs != nullptr) {
    gbfputint32(fs->depth, file_out);
  } else {
    gbfputint32(0, file_out); // zero seems to indicate no depth
  }

  /* Loran data */
  gbfputint32(0xffffffff, file_out);  // indicate Loran not used
  gbfputint32(0, file_out);
  gbfputint32(0, file_out);
}

void
LowranceusrFormat::lowranceusr_waypt_pr(const Waypoint* wpt)
{

  /* our personal waypoint counter */
  gbfputint16(waypt_out_count, file_out);

  if (global_opts.debug_level >= 3) {
    gbDebug("waypt_pr: waypoint #%d\n",waypt_out_count);
  }

  waypt_out_count++;

  lowranceusr_waypt_disp(wpt);
}

void
LowranceusrFormat::lowranceusr4_write_waypoints()
{
  /* enumerate all waypoints from both the plain old waypoint list and
     also all routes */
  auto register_waypt_lambda = [this](const Waypoint* waypointp)->void {
    register_waypt(waypointp);
  };
  waypt_disp_all(register_waypt_lambda);
  route_disp_all(nullptr, nullptr, register_waypt_lambda);

  if (global_opts.debug_level >= 1) {
    gbDebug("writing %s waypoints\n", QByteArray::number(waypt_table->size()).constData());
  }

  gbfputint32(waypt_table->size(), file_out);
  waypt_uid = 0;
  for (int i = 0; i < waypt_table->size(); ++i) {
    if (global_opts.debug_level >= 2) {
      gbDebug("writing out waypt %d (%s - %s)\n",
             i, gbLogCStr(waypt_table->at(i)->shortname), gbLogCStr(waypt_table->at(i)->description));
    }
    lowranceusr4_waypt_disp((waypt_table->at(i)));
  }
}

/*
 * In Lowrance parlance, an "Icon" is a waypoint but without any
 * kind of a name.  The header count of icons has already been written
 * before we get here, so it's just a matter of spitting out
 * 4 bytes lat
 * 4 bytes long
 * 4 bytes symbol
 */
void
LowranceusrFormat::lowranceusr_write_icon(const Waypoint* wpt)
{
  int latmm = lat_deg_to_mm(wpt->latitude);
  int lonmm = lon_deg_to_mm(wpt->longitude);
  int icon = !wpt->icon_descr.isNull() ?
             lowranceusr_find_icon_number_from_desc(wpt->icon_descr) :
             X_1_ICON;

  gbfputint32(latmm, file_out);
  gbfputint32(lonmm, file_out);
  gbfputint32(icon, file_out);
}

/*
 * Header format:
 *  short num_trails,
 *  int trail_name text length,
 *  char *trail_name,
 *  boolean visible,
 *  short num_trail_points,
 *  short max_trail_size,
 *  short num_section_points
 *      == don't know how many max points per section so
 *      == use num_trail_points for now
 *      == Once this is known then the waypoints ought to be
 *      == broken up into sections
 */

void
LowranceusrFormat::lowranceusr_trail_hdr(const route_head* trk)
{
  QString name;
  char visible=1;

  ++trail_count;
//TODO: This whole function needs to be replaced...
  if (!trk->rte_name.isEmpty()) {
    name = trk->rte_name;
  } else if (!trk->rte_desc.isEmpty()) {
    name = trk->rte_desc;
  } else {
    name = name + QStringLiteral("Babel %1").arg(trail_count);
  }

  int text_len = name.length();
  if (text_len > MAXUSRSTRINGSIZE) {
    text_len = MAXUSRSTRINGSIZE;
  }
  if (global_opts.debug_level >= 1) {
    gbDebug("trail_hdr: trail name '%s' ", gbLogCStr(trk->rte_name));
  }
  gbfputint32(text_len, file_out);
  gbfwrite(CSTR(name), 1, text_len, file_out);

  auto num_trail_points = (short) trk->rte_waypt_ct();
  short max_trail_size = MAX_TRAIL_POINTS;
  if (num_trail_points > max_trail_size) {
    num_trail_points = max_trail_size;
  }
  num_section_points = num_trail_points;

  if (global_opts.debug_level) {
    gbDebug("num_trail_points = %d ", num_trail_points);
    if (global_opts.debug_level > 1) {
      gbDebug("max_trail_size = %d num_section_points = %d\n", max_trail_size, num_section_points);
    } else {
      gbDebug("\n");
    }
  }

  gbfwrite(&visible, 1, 1, file_out);
  gbfputint16(num_trail_points, file_out);
  gbfputint16(max_trail_size, file_out);
  gbfputint16(num_section_points, file_out);
  trail_point_count=0;
}

void
LowranceusrFormat::lowranceusr_route_hdr(const route_head* rte)
{
  QString name;

  /* route name */
  //TODO: This whole function needs to be replaced...
  if (!rte->rte_name.isEmpty()) {
    name = rte->rte_name;
  } else if (!rte->rte_desc.isEmpty()) {
    name = rte->rte_desc;
  } else {
    name = QString::asprintf("Babel R%d", ++lowrance_route_count);
  }
  name.truncate(MAXUSRSTRINGSIZE);
  gbfputint32(name.size(), file_out);
  gbfputs(name, file_out);

  /* num legs */
  auto num_legs = (short) rte->rte_waypt_ct();
  gbfputint16(num_legs, file_out);
  char route_reversed=0;
  gbfwrite(&route_reversed, 1, 1, file_out);

  if (global_opts.debug_level >= 1)
    gbDebug("route_hdr: route name \"%s\" num_legs = %d\n",
           gbLogCStr(rte->rte_name), num_legs);
}

void
LowranceusrFormat::lowranceusr4_route_hdr(const route_head* rte)
{
  if (global_opts.debug_level >= 1) {
    gbDebug("writing route #%d (%s) with %d waypts\n",
           route_uid, gbLogCStr(rte->rte_name), rte->rte_waypt_ct());
  }

  const auto* fs = reinterpret_cast<lowranceusr4_fsdata*>(rte->fs.FsChainFind(kFsLowranceusr4));

  /* UID unit number */
  if (opt_serialnum_i > 0) {
    gbfputint32(opt_serialnum_i, file_out);  // use option serial number if specified
  } else if (fs != nullptr) {
    gbfputint32(fs->uid_unit, file_out);  // else use serial number from input if valid
  } else {
    gbfputint32(0, file_out);  // else Write Serial Number = 0
  }

  /* 64-bit UID sequence number */
  gbfputint32(route_uid++, file_out);
  gbfputint32(0, file_out);

  /* Route stream version number: seems to be 1 in my data */
  gbfputint16(1, file_out);

  /* Route name */
  lowranceusr4_writestr(rte->rte_name, file_out, 2);

  /* Num Legs */
  gbfputint32(rte->rte_waypt_ct(), file_out);
}

void
LowranceusrFormat::lowranceusr4_route_leg_disp(const Waypoint* wpt)
{
  for (int i = 0; i < waypt_table->size(); i++) {
    const Waypoint* cmp = waypt_table->at(i);
    if (cmp->shortname == wpt->shortname) {
      const auto* fs = reinterpret_cast<lowranceusr4_fsdata*>(cmp->fs.FsChainFind(kFsLowranceusr4));

      if (opt_serialnum_i > 0) {
        gbfputint32(opt_serialnum_i, file_out);  // use option serial number if specified
      } else if (fs != nullptr) {
        gbfputint32(fs->uid_unit, file_out);  // else use serial number from input if valid
      } else {
        gbfputint32(0, file_out);  // else Write Serial Number = 0
      }
      gbfputint32(i, file_out); // Sequence Low
      gbfputint32(0, file_out); // Sequence High
      if (global_opts.debug_level > 1) {
        gbDebug("wrote route leg with waypt '%s'\n", gbLogCStr(wpt->shortname));
      }
      break;
    }
  }
}

void
LowranceusrFormat::lowranceusr4_route_trl(const route_head* /*unused*/)
{
  /* Mystery byte */
  gbfputc(0x01, file_out);	// end of Route info ??
}

void
LowranceusrFormat::lowranceusr_trail_disp(const Waypoint* wpt)
{
  if (trail_point_count < MAX_TRAIL_POINTS) {
    trail_point_count++;
    int lat = lat_deg_to_mm(wpt->latitude);
    int lon = lon_deg_to_mm(wpt->longitude);

    if (global_opts.debug_level > 1) {
      gbDebug("trail_disp: Trail point #%d lat = %f long = %f\n",trail_point_count, wpt->latitude, wpt->longitude);
    }

    gbfputint32(lat, file_out);
    gbfputint32(lon, file_out);
    /* If this isn't the first point in the outgoing trail, and
     *   i) the source wpt was the start of a new track segment or
     *  ii) the source wpt is the first waypoint of a track that is being merged
     * then set the continuous flag to 0 to indicate a discontinuity.
     * Otherwise set the continous flag to 1.
     */
    char continuous_flag = !((trail_point_count > 1) && (wpt->wpt_flags.new_trkseg || merge_new_track));
    merge_new_track = false;
    gbfwrite(&continuous_flag, 1, 1, file_out);
  }
}

void
LowranceusrFormat::lowranceusr_merge_trail_hdr(const route_head* trk)
{
  QString name;
  if (++trail_count == 1) {
    if (!trk->rte_name.isEmpty()) {
      name = trk->rte_name;
    } else if (!trk->rte_desc.isEmpty()) {
      name = trk->rte_desc;
    } else {
      name = QString::asprintf("Babel %d", trail_count);
    }

    name.truncate(MAXUSRSTRINGSIZE);
    gbfputint32(name.size(), file_out);
    gbfputs(name, file_out);

    if (global_opts.debug_level >= 1) {
      gbDebug("trail_hdr: trail name = %s\n", CSTR(name));
    }
  }

  trail_point_count += (short) trk->rte_waypt_ct();
}

void
LowranceusrFormat::lowranceusr_merge_trail_tlr(const route_head* /*unused*/)
{
  if (trail_count == (int)track_count()) {  /* last trail */
    short num_trail_points = trail_point_count;
    short max_trail_size = MAX_TRAIL_POINTS;
    if (num_trail_points > max_trail_size) {
      num_trail_points = max_trail_size;
    }
    num_section_points = num_trail_points;

    if (global_opts.debug_level >= 1)
      gbDebug("merge_trail_tlr: num_trail_points = %d\nmax_trail_size = %d\nnum_section_points = %d\n",
             num_trail_points, max_trail_size, num_section_points);

    const char visible=1;
    gbfwrite(&visible, 1, 1, file_out);
    gbfputint16(num_trail_points, file_out);
    gbfputint16(max_trail_size, file_out);
    gbfputint16(num_section_points, file_out);
  }
}
void
LowranceusrFormat::lowranceusr_merge_trail_hdr_2(const route_head* /*unused*/)
{
  merge_new_track = true;
}

void
LowranceusrFormat::lowranceusr4_trail_hdr(const route_head* trail)
{
  if (global_opts.debug_level >= 1) {
    gbDebug("writing trail %d (%s) with %d trailpoints\n",
           trail_uid, gbLogCStr(trail->rte_name), trail->rte_waypt_ct());
  }

  /* UID unit number */
  gbfputint32(opt_serialnum_i, file_out);

  /* 64-bit UID sequence number */
  gbfputint32(trail_uid++, file_out);
  gbfputint32(0, file_out);

  /* Route stream version number: always seems to be 3 in my data */
  gbfputint16(3, file_out);

  /* Track name */
  lowranceusr4_writestr(trail->rte_name, file_out, 2);

  /* Flags: always seems to be 2 in my data */
  gbfputint32(2, file_out);

  /* Color ID */
  gbfputint32(0, file_out);

  /* Comment */
  lowranceusr4_writestr(trail->rte_desc, file_out, 2);

  /* Creation date/time */
  gbfputint32(0, file_out);
  gbfputint32(0, file_out);

  /* Unused byte */
  gbfputc(0, file_out);

  /* Active flag */
  gbfputc(0, file_out);

  /* Visible flag; I'll just assume all trails should be visible for
     now */
  gbfputc(1, file_out);

  /* Mysterious "data count" and "data type" stuff */
  gbfputint32(0, file_out);
//  /* If we hadn't forced the count to zero we would need something like: */
//  for (int i=0; i< attr_count; ++i) {
//    gbfputc(0, file_out);
//  }

  /* Trackpoint count */
  gbfputint32(trail->rte_waypt_ct(), file_out);
}

void
LowranceusrFormat::lowranceusr4_trail_disp(const Waypoint* wpt)
{
  /* Some unknown bytes */
  gbfputint16(0, file_out);
  gbfputc(0, file_out);

  /* Timestamp */
  gbfputint32(wpt->GetCreationTime().toTime_t(), file_out);

  /* Long/Lat */
  gbfputdbl(wpt->longitude * DEGREESTORADIANS, file_out);
  gbfputdbl(wpt->latitude * DEGREESTORADIANS, file_out);

  /* Mysterious per-trailpoint data; we'll just say there are "0"
     mystery entries */
  gbfputint32(0, file_out);
}

void
LowranceusrFormat::write()
{
  QString buf;

  mkshort_handle->set_length(15);

  gbfputint32(writing_version, file_out);

  int NumWaypoints = waypt_count();
  if (global_opts.debug_level >= 1) {
    gbDebug("data_write: Num Waypoints = %d\n", NumWaypoints);
  }

  // If writeasicons option specified then all Waypoints processed are written as
  // Event Marker ICONs so write the number of Waypoints as ZERO but only if
  // USR format 2 or 3
  if ((writing_version == 2) || (writing_version == 3)) {
    if (opt_writeasicons) {
      short zero = 0;
      gbfputint16(zero, file_out);
    } else {
      // USR version 2 and 3 uses 16-bit count
      gbfputint16(NumWaypoints, file_out);
      auto lowranceusr_waypt_pr_lambda = [this](const Waypoint* waypointp)->void {
        lowranceusr_waypt_pr(waypointp);
      };
      waypt_disp_all(lowranceusr_waypt_pr_lambda);
    }
  } else {
    // Ignore writeasicons option for all other USR versions
    // Before adding the Waypoint data need to add the file header information

    // Only support Version 10 of the DataStream right now
    int DataStreamVersion = 10;
    gbfputint32(DataStreamVersion, file_out);

    /* file title */
    buf = opt_title.isEmpty()?
          QStringLiteral("GPSBabel generated USR data file") : opt_title;
    if (global_opts.debug_level >= 1) {
      gbDebug("data_write: Title = '%s'\n", gbLogCStr(buf));
    }
    lowranceusr4_writestr(buf, file_out, 1);

    /* date string */
    gpsbabel::DateTime now = current_time().toUTC();
    lowranceusr4_writestr(now.toString(u"MM/dd/yyyy"), file_out, 1);

    /* creation date/time */
    auto ts = lowranceusr4_jd_from_timestamp(now);
    gbfputint32(ts.julian_day_number, file_out);
    gbfputint32(ts.milliseconds, file_out);

    /* unused byte */
    gbfputc(0, file_out);

    /* device serial number */
    opt_serialnum_i = opt_serialnum.get_result();
    gbfputint32(opt_serialnum_i, file_out);

    /* content description */
    buf = opt_content_descr.isEmpty()?
          QStringLiteral("Waypoints, routes, and trails") : opt_content_descr;
    if (global_opts.debug_level >= 1) {
      gbDebug("data_write: Description = '%s'\n", gbLogCStr(buf));
    }
    lowranceusr4_writestr(buf, file_out, 1);

    lowranceusr4_write_waypoints();
  }

  /*************************************************************************/
  /*
   * Start ROUTE Element
   */

  /* Original Route support added 6/21/05 */
  int NumRoutes = route_count();
  lowrance_route_count=0;

  if (global_opts.debug_level >= 1) {
    gbDebug("data_write: Num routes = %d\n", NumRoutes);
  }

  if ((writing_version == 2) || (writing_version == 3)) {
    // USR version 2 & 3 use 16-bit count
    gbfputint16(NumRoutes, file_out);
    if (NumRoutes) {
      auto lowranceusr_waypt_disp_lambda = [this](const Waypoint* waypointp)->void {
        lowranceusr_waypt_disp(waypointp);
      };
      auto lowranceusr_route_hdr_lambda = [this](const route_head* rte)->void {
        lowranceusr_route_hdr(rte);
      };
      route_disp_all(lowranceusr_route_hdr_lambda, nullptr, lowranceusr_waypt_disp_lambda);
    }
  } else {
    // All other USR formats use 32-bit count
    gbfputint32(NumRoutes, file_out);
    if (NumRoutes) {
      auto lowranceusr4_route_leg_disp_lambda = [this](const Waypoint* waypointp)->void {
        lowranceusr4_route_leg_disp(waypointp);
      };
      auto lowranceusr4_route_hdr_lambda = [this](const route_head* rte)->void {
        lowranceusr4_route_hdr(rte);
      };
      auto lowranceusr4_route_trl_lambda = [this](const route_head* rte)->void {
        lowranceusr4_route_trl(rte);
      };
      route_disp_all(lowranceusr4_route_hdr_lambda, lowranceusr4_route_trl_lambda, lowranceusr4_route_leg_disp_lambda);
    }
  }


  /*
   * End ROUTE Element
   */
  /*************************************************************************/

  /*************************************************************************/
  /*
   * Start EVENT MARKER ICON Element
   */

  if ((writing_version == 2) || (writing_version == 3)) {
    // Only USR versions 2 and 3 supprt Event Marker ICONs
    // Ignore for all other USR versions
    if (NumWaypoints && opt_writeasicons) {
      gbfputint16(NumWaypoints, file_out);
      auto lowranceusr_write_icon_lambda = [this](const Waypoint* waypointp)->void {
        lowranceusr_write_icon(waypointp);
      };
      waypt_disp_all(lowranceusr_write_icon_lambda);
    } else {
      short NumIcons = 0;
      gbfputint16(NumIcons, file_out);
    }
  }

  /*
   * End EVENT MARKER ICON Element
   */
  /*************************************************************************/

  /* Trail support added 6/21/05 */
  short int NumTrails = track_count();

  if (NumTrails && merge) {
    NumTrails = 1;
    if ((writing_version == 2) || (writing_version == 3)) {
      // USR version 2 & 3 use 16-bit count
      gbfputint16(NumTrails, file_out);
    } else {
      // All other USR formats use 32-bit count
      gbfputint32(NumTrails, file_out);
    }

    if ((writing_version == 2) || (writing_version == 3)) {
      trail_point_count = 0;
      trail_count = 0;
      /* count the number of total trail points */
      auto lowranceusr_merge_trail_hdr_lambda = [this](const route_head* rte)->void {
        lowranceusr_merge_trail_hdr(rte);
      };
      auto lowranceusr_merge_trail_tlr_lambda = [this](const route_head* rte)->void {
        lowranceusr_merge_trail_tlr(rte);
      };
      track_disp_all(lowranceusr_merge_trail_hdr_lambda, lowranceusr_merge_trail_tlr_lambda, nullptr);
      /* write out the new trail header */
      trail_point_count = 0;
      auto lowranceusr_trail_disp_lambda = [this](const Waypoint* waypointp)->void {
        lowranceusr_trail_disp(waypointp);
      };
      auto lowranceusr_merge_trail_hdr_2_lambda = [this](const route_head* rte)->void {
        lowranceusr_merge_trail_hdr_2(rte);
      };
      track_disp_all(lowranceusr_merge_trail_hdr_2_lambda, nullptr, lowranceusr_trail_disp_lambda);
    } else {
      /* MERGE NEEDS SOME MORE WORK */
      gbFatal("output file USR %d format is not supported with merge option\n", writing_version);
    }

  } else {
    if (global_opts.debug_level >= 1) {
      gbDebug("data_write: Num trails = %d\n", NumTrails);
    }
    if ((writing_version == 2) || (writing_version == 3)) {
      // USR version 2 & 3 use 16-bit count
      gbfputint16(NumTrails, file_out);
      if (NumTrails) {
        trail_count=0;
        merge_new_track = false;
        auto lowranceusr_trail_disp_lambda = [this](const Waypoint* waypointp)->void {
          lowranceusr_trail_disp(waypointp);
        };
        auto lowranceusr_trail_hdr_lambda = [this](const route_head* rte)->void {
          lowranceusr_trail_hdr(rte);
        };
        track_disp_all(lowranceusr_trail_hdr_lambda, nullptr, lowranceusr_trail_disp_lambda);
      }
    } else {
      // All other USR formats use 32-bit count
      gbfputint32(NumTrails, file_out);
      if (NumTrails) {
        trail_count=0;
        auto lowranceusr4_trail_disp_lambda = [this](const Waypoint* waypointp)->void {
          lowranceusr4_trail_disp(waypointp);
        };
        auto lowranceusr4_trail_hdr_lambda = [this](const route_head* rte)->void {
          lowranceusr4_trail_hdr(rte);
        };
        track_disp_all(lowranceusr4_trail_hdr_lambda, nullptr, lowranceusr4_trail_disp_lambda);
      }
    }
  }
}
