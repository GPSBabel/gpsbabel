/*
    OziExplorer Waypoints/Tracks/Routes
    Comma Delimited

    As described in OziExplorer Help File

    Copyright (C) 2002-2005 Robert Lipe, robertlipe+source@gpsbabel.org

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

    Reference:
    https://www.oziexplorer4.com/eng/help/fileformats.html

    According to the OZI Explorer developer:
    "There is no specified character set, it defaults to whatever 8 bit
     character set "Windows" defaults to - normally CP-1252 but can vary
     depending on Windows regional settings."

    According to the reference, for some text fields:
    "comma's not allowed in text fields, character 209 can be used instead
     and a comma will be substituted."
    This could work for windows-1252, but not for utf-8.
    We don't support any special handling for character 209.

 */

#include "ozi.h"

#include <cmath>                  // for lround

#include <QByteArray>             // for QByteArray
#include <QChar>                  // for operator==, QChar
#include <QFile>                  // for QFile
#include <QFileInfo>              // for QFileInfo
#include <QFlags>                 // for QFlags
#include <QIODevice>              // for operator&, QIODevice
#include <QList>                  // for QList, QList<>::const_iterator
#include <QString>                // for QString, operator==
#include <QStringList>            // for QStringList
#include <QStringLiteral>         // for qMakeStringPrivate, QStringLiteral
#include <QTextStream>            // for QTextStream, operator<<, qSetRealNumberPrecision, QTextStream::FixedNotation
#include <Qt>                     // for CaseInsensitive
#include <QtGlobal>               // for qPrintable

#include "defs.h"
#include "csv_util.h"             // for csv_stringclean
#include "formspec.h"             // for FormatSpecificDataList, kFsOzi
#include "jeeps/gpsmath.h"        // for GPS_Lookup_Datum_Index, GPS_Math_Known_Datum_To_WGS84_M
#include "mkshort.h"              // for MakeShort
#include "src/core/datetime.h"    // for DateTime
#include "src/core/textstream.h"  // for TextStream


#define BADCHARS	",\r\n"
#define DAYS_SINCE_1990	25569

void
OziFormat::ozi_open_io(const QString& fname, QIODevice::OpenModeFlag mode)
{
  stream = new gpsbabel::TextStream;
  stream->open(fname, mode, opt_codec.get().toUtf8());

  if (mode & QFile::WriteOnly) {
    stream->setRealNumberNotation(QTextStream::FixedNotation);
  }
}

void
OziFormat::ozi_close_io()
{
  if (!stream) {
    return;
  }
  stream->close();
  delete stream;
  stream = nullptr;
}

OziFormat::ozi_fsdata*
OziFormat::ozi_alloc_fsdata()
{
  auto* fsdata = new ozi_fsdata;

  /* Provide defaults via command line defaults */
  fsdata->fgcolor = color_to_bbggrr(wptfgcolor);
  fsdata->bgcolor = color_to_bbggrr(wptbgcolor);

  return fsdata;
}

QString
OziFormat::ozi_get_time_str(const Waypoint* waypointp)
{
  if (waypointp->creation_time.isValid()) {
    double time = (waypt_time(waypointp) / SECONDS_PER_DAY) + DAYS_SINCE_1990;
    return QStringLiteral("%1").arg(time, 0, 'f', 7);
  }
  return QString("");
}

void
OziFormat::ozi_set_time_str(const QString& str, Waypoint* waypointp)
{
  double ozi_time = str.toDouble();

  if (ozi_time > DAYS_SINCE_1990) {
    waypointp->SetCreationTime((ozi_time - DAYS_SINCE_1990) * SECONDS_PER_DAY,
                               lround(1000.0 * (ozi_time - (int) ozi_time)));
  }
}

void
OziFormat::ozi_convert_datum(Waypoint* wpt) const
{
  if (datum != kDatumWGS84) {
    double lat;
    double lon;
    double alt;
    GPS_Math_Known_Datum_To_WGS84_M(wpt->latitude, wpt->longitude, 0.0,
                                    &lat, &lon, &alt, datum);
    wpt->latitude = lat;
    wpt->longitude = lon;
  }
}

void
OziFormat::ozi_openfile(const QString& fname)
{
  const char* ozi_extensions[] = {nullptr, "plt", "wpt", "rte"};

  /* if we're doing multi-track output, sequence the filenames like:
   * mytrack.plt, mytrack-1.plt...unless we're writing to stdout.
   */

  if (fname ==  '-') {
    if (stream == nullptr) {
      ozi_open_io(fname, QFile::WriteOnly);
    }
    return;
  }

  QString buff;
  if ((track_out_count) && (ozi_objective == trkdata)) {
    buff = QStringLiteral("-%1").arg(track_out_count);
  } else {
    buff = QString("");
  }

  /* remove extension and add buff + ozi's extension */
  QString sname(fname);
  int suffix_len = QFileInfo(fname).suffix().length();
  if (suffix_len > 0) {
    /* drop the suffix and the period */
    sname.chop(suffix_len + 1);
  }

  QString tmpname = QStringLiteral("%1%2.%3").arg(sname, buff, ozi_extensions[ozi_objective]);

  /* re-open file_out with the new filename */
  if (stream != nullptr) {
    ozi_close_io();
  }

  ozi_open_io(tmpname, QFile::WriteOnly);
}

void
OziFormat::ozi_track_hdr(const route_head* rte)
{
  if ((! pack_opt) || (track_out_count == 0)) {
    ozi_openfile(ozi_ofname);
    *stream << "OziExplorer Track Point File Version 2.1\r\n"
                     << "WGS 84\r\n"
                     << "Altitude is in " << (altunit == 'f' ? "Feet" : "Meters") << "\r\n"
                     << "Reserved 3\r\n"
                     << "0,2,255,"
                     << (rte->rte_name.isEmpty() ? "ComplimentsOfGPSBabel" : rte->rte_name)
                     << ",0,0,2,8421376\r\n"
                     << "0\r\n";
  }

  track_out_count++;
  new_track = 1;
}

void
OziFormat::ozi_track_disp(const Waypoint* waypointp)
{
  double alt;

  QString ozi_time = ozi_get_time_str(waypointp);

  if (waypointp->altitude == unknown_alt) {
    alt = -777;
  } else {
    alt = waypointp->altitude * alt_scale;
  }

  *stream << qSetRealNumberPrecision(6) << waypointp->latitude << ','
                   << waypointp->longitude << ','
                   << new_track << ','
                   << qSetRealNumberPrecision(1) << alt << ','
                   << ozi_time << ",,\r\n";

  new_track = 0;
}

void
OziFormat::ozi_track_pr()
{
  auto ozi_track_hdr_lambda = [this](const route_head* rte)->void {
    ozi_track_hdr(rte);
  };
  auto ozi_track_disp_lambda = [this](const Waypoint* waypointp)->void {
    ozi_track_disp(waypointp);
  };
  track_disp_all(ozi_track_hdr_lambda, nullptr, ozi_track_disp_lambda);
}

void
OziFormat::ozi_route_hdr(const route_head* rte)
{
  /* prologue on 1st pass only */
  if (route_out_count == 0) {
    *stream << "OziExplorer Route File Version 1.0\r\n"
                     << "WGS 84\r\n"
                     << "Reserved 1\r\n"
                     << "Reserved 2\r\n";
  }

  route_out_count++;
  route_wpt_count = 0;

  /*
   * Route Record
   * Field 1 : R - indicating route details
   * Field 2 : Number - this is the location in the array, must be unique, usually start at 0 for Garmins 1 for other and increment.
   * Field 3 : Name - the waypoint name, use the correct length name to suit the GPS type.
   * Field 4 : Description.
   * Field 5 : Route Color as displayed on map (RGB).
   *
   * R, 0,R0 ,,255
   * R, 1, ICP GALHETA,, 16711680
   */

  *stream << "R," << route_out_count << ','
                   << rte->rte_name << ','
                   << rte->rte_desc << ",\r\n";
}

void
OziFormat::ozi_route_disp(const Waypoint* waypointp)
{
  route_wpt_count++;

  QString ozi_time = ozi_get_time_str(waypointp);

/*
  double alt;
  if (waypointp->altitude == unknown_alt) {
    alt = -777;
  } else {
    alt = waypointp->altitude * alt_scale;
  }
*/
  /*
   *   Field 1 : W - indicating route waypoint details.
   *   Field 2 : Route Number - location in array of routes
   *   Field 3 : Number - this is the location in the array of route waypoints, this field is now ignored.
   *   Field 4 : Wp Number - this is the number of the waypoint (the Wp number within the GPS for lowrances)
   *   Field 5 : Name - the waypoint name, use the correct length name to suit the GPS type.
   *   Field 6 : Latitude - decimal degrees.
   *   Field 7 : Longitude - decimal degrees.
   *   Field 8 : Date - see Date Format below, if blank a preset date will be used
   *   Field 9 : Symbol - 0 to number of symbols in GPS
   *   Field 10 : Status - always set to 1
   *   Field 11 : Map Display Format
   *   Field 12 : Foreground Color (RGB value)
   *   Field 13 : Background Color (RGB value)
   *   Field 14 : Description (max 40), no commas
   *   Field 15 : Pointer Direction
   *   Field 16 : Garmin Display Format
   *
   * W,1,7,7,007,-25.581670,-48.316660,36564.54196,10,1,4,0,65535,TR ILHA GALHETA,0,0
   */

  *stream << "W," << route_out_count << ",,"
                   << route_wpt_count << ','
                   << waypointp->shortname << ','
                   << qSetRealNumberPrecision(6) << waypointp->latitude << ','
                   << waypointp->longitude << ','
                   << ozi_time << ",0,1,3,0,65535,"
                   << waypointp->description << ",0,0\r\n";

}

void
OziFormat::ozi_route_pr()
{
  auto ozi_route_hdr_lambda = [this](const route_head* rte)->void {
    ozi_route_hdr(rte);
  };
  auto ozi_route_disp_lambda = [this](const Waypoint* waypointp)->void {
    ozi_route_disp(waypointp);
  };
  route_disp_all(ozi_route_hdr_lambda, nullptr, ozi_route_disp_lambda);
}

void
OziFormat::ozi_init_units(const int direction)	/* 0 = in; 1 = out */
{
  if (altunit_opt.get().startsWith('m', Qt::CaseInsensitive)) {
    altunit = 'm';
    alt_scale = 1.0; /* meters */
  } else if (altunit_opt.get().startsWith('f', Qt::CaseInsensitive)) {
    altunit = 'f';
    alt_scale = FEET_TO_METERS(1.0); /* feet */
  } else {
    gbFatal("Unknown value (%s) for option 'altunit'!\n", gbLogCStr(altunit_opt));
  }
  if (direction != 0) {
    alt_scale = 1.0 / alt_scale;
  }

  if (proxunit_opt.get().startsWith('m')) {
    proxunit = 'm';
    prox_scale = MILES_TO_METERS(1.0); /* miles */
  } else if (proxunit_opt.get().startsWith('n')) {
    proxunit = 'n';
    prox_scale = NMILES_TO_METERS(1.0); /* nautical miles */
  } else if (proxunit_opt.get().startsWith('k')) {
    proxunit = 'k';
    prox_scale = 1000.0; /* kilometers */
  } else {
    gbFatal("Unknown value (%s) for option 'proxunit'!\n", gbLogCStr(proxunit_opt));
  }
  if (direction != 0) {
    prox_scale = 1.0 / prox_scale;
  }
}

void
OziFormat::rd_init(const QString& fname)
{
  ozi_open_io(fname, QFile::ReadOnly);

  ozi_init_units(0);
}

void
OziFormat::rd_deinit()
{
  ozi_close_io();
}

void
OziFormat::wr_init(const QString& fname)
{

  /* At this point, we have no idea whether we'll be writing waypoint,
   * route, or tracks.  So we'll hold off opening any files until
   * we're actually ready to write.
   */

  ozi_ofname = fname;

  mkshort_handle = new MakeShort;

  /* set mkshort options from the command line if applicable */
  if (global_opts.synthesize_shortnames) {

    mkshort_handle->set_length(snlenopt.get_result());

    if (snwhiteopt.has_value()) {
      mkshort_handle->set_whitespace_ok(snwhiteopt);
    }

    if (snupperopt.has_value()) {
      mkshort_handle->set_mustupper(snupperopt);
    }

    if (snuniqueopt.has_value()) {
      mkshort_handle->set_mustuniq(snuniqueopt);
    }

    mkshort_handle->set_badchars("\",");
  }

  ozi_init_units(1);
  parse_distance(proximityarg, &proximity, 1.0 / prox_scale);
}

void
OziFormat::wr_deinit()
{
  ozi_close_io();
  ozi_ofname.clear();

  delete mkshort_handle;
  mkshort_handle = nullptr;
}

void
OziFormat::ozi_parse_waypt(int field, const QString& str, Waypoint* wpt_tmp, ozi_fsdata* fsdata) const
{
  double alt;

  if (str.isEmpty()) {
    return;
  }

  switch (field) {
  case 0:
    /* sequence # */
    break;
  case 1:
    /* waypoint name */
    wpt_tmp->shortname = str.trimmed();
    break;
  case 2:
    /* degrees latitude */
    wpt_tmp->latitude = str.toDouble();
    break;
  case 3:
    /* degrees longitude */
    wpt_tmp->longitude = str.toDouble();
    break;
  case 4:
    /* DAYS since 1900 00:00:00 in days.days (5.5) */
    ozi_set_time_str(str, wpt_tmp);
    break;
  case 5:
    /* icons 0-xx.   Ozi seems to use some kind of internal tables to
    pick numbers for icons based on GPS type.  We don't know what those
       tables are, so we read just the numbers.  This converts badly to
       other types, but it at least maintains fidelity for an ozi->ozi
       operation. */
    if (str.toInt() > 0) {
      wpt_tmp->icon_descr = str;
    }
    break;
  case 6:
    /* unknown - always 1 */
    break;
  case 7:
    /* display format options 0-8 */
    break;
  case 8:
    /* foreground color (0=black) */
    fsdata->fgcolor = str.toInt();
    break;
  case 9:
    /* background color (65535=yellow) */
    fsdata->bgcolor = str.toInt();
    break;
  case 10:
    /* Description */
    wpt_tmp->description = str.trimmed();
    break;
  case 11:
    /* pointer direction 0,1,2,3 bottom,top,left,right */
    break;
  case 12:
    /* garmin gps display flags (0-name w/sym, 1-sym only, 2-comment w/symbol */
    break;
  case 13:
    /* proximity distance - meters */
    wpt_tmp->set_proximity(str.toDouble() * prox_scale);
    break;
  case 14:
    /* altitude */
    alt = str.toDouble();
    if (alt == -777) {
      wpt_tmp->altitude = unknown_alt;
    } else {
      wpt_tmp->altitude = alt * alt_scale;
    }
    break;
  case 15:
    /* waypoint text name size */
    break;
  case 16:
    /* bold checkbox (1=bold, default 0) */
    break;
  case 17:
    /* symbol size - 17 default */
    break;
    /*
     * Fields 18-23 were added around version 3.90.4g of
     * Ozi, but aren't documented.   We silently ignore
     * these or any additional fields we don't need.
     */
  default:
    break;
  }
}

void
OziFormat::ozi_parse_track(int field, const QString& str, Waypoint* wpt_tmp, char* trk_name)
{
  if (str.isEmpty()) {
    return;
  }

  switch (field) {
  case 0:
    /* latitude */
    wpt_tmp->latitude = str.toDouble();
    break;
  case 1:
    /* longitude */
    wpt_tmp->longitude = str.toDouble();
    break;
  case 2:
    /* new track flag */
    if ((str.toInt() == 1) && !trk_head->rte_waypt_empty()) {
      trk_head = new route_head;
      track_add_head(trk_head);
      if (trk_name) {
        trk_head->rte_name = trk_name;
      }
    }
    break;
  case 3: {
    /* altitude */
    double alt = str.toDouble();
    if (alt == -777) {
      wpt_tmp->altitude = unknown_alt;
    } else {
      wpt_tmp->altitude = alt * alt_scale;
    }
    break;
  }
  case 4:
    /* DAYS since 1900 00:00:00 in days.days (5.5) */
    ozi_set_time_str(str, wpt_tmp);
    break;
  default:
    break;
  }
}

void
OziFormat::ozi_parse_routepoint(int field, const QString& str, Waypoint* wpt_tmp)
{
  if (str.isEmpty()) {
    return;
  }

  switch (field) {
  case 0:
    /* W */
    break;
  case 1:
    /* route # */
    break;
  case 2:
    /* waypoint # -- ignored by ozi */
    break;
  case 3:
    /* waypoint # */
    break;
  case 4:
    /* waypoint name */
    wpt_tmp->shortname = csv_stringclean(str, QString(","));
    break;
  case 5:
    /* latitude */
    wpt_tmp->latitude = str.toDouble();
    break;
  case 6:
    /* longitude */
    wpt_tmp->longitude = str.toDouble();
    break;
  case 7:
    /* DAYS since 1900 00:00:00 in days.days (5.5) */
    ozi_set_time_str(str, wpt_tmp);
    break;
  case 8:
    /* symbol */
    break;
  case 9:
    /* status */
    break;
  case 10:
    /* map display format */
    break;
  case 11:
    /* foreground color (RGB) */
    break;
  case 12:
    /* background color (RGB) */
    break;
  case 13:
    /* description */
    wpt_tmp->description = csv_stringclean(str, QString(","));
    break;
  default:
    break;
  }
}

void
OziFormat::ozi_parse_routeheader(int field, const QString& str)
{

  switch (field) {
  case 0:
    /* R */
    rte_head = new route_head;
    route_add_head(rte_head);
    break;
  case 1:
    /* route # */
    rte_head->rte_num = str.toInt();
    break;
  case 2:
    /* route name */
    rte_head->rte_name = csv_stringclean(str, ",");
    break;
  case 3:
    /* route description */
    rte_head->rte_desc = csv_stringclean(str, ",");
    break;
  case 4:
    /* route color */
    break;
  default:
    break;
  }
}

void
OziFormat::read()
{
  QString buff;
  char* trk_name = nullptr;
  int linecount = 0;

  while (buff = stream->readLine(), !buff.isNull()) {
    linecount++;

    /*
     * this is particularly nasty.  use the first line of the file
     * to attempt to divine the data type we are parsing
     */
    if (linecount == 1) {
      if (buff.contains("Track Point")) {
        trk_head = new route_head;
        track_add_head(trk_head);
        ozi_objective = trkdata;
      } else if (buff.contains("Route File")) {
        ozi_objective = rtedata;
      } else {
        ozi_objective = wptdata;
      }
    } else if (linecount == 2) {
      datum = GPS_Lookup_Datum_Index(buff);

      if (datum < 0) {
        gbFatal("Unsupported datum '%s'.\n", gbLogCStr(buff));
      }
    } else if (linecount == 3) {
      if (buff.startsWith( "Altitude is in ", Qt::CaseInsensitive)) {
        QString unit = buff.mid(15);
        if (unit.startsWith("Feet", Qt::CaseInsensitive)) {
          altunit = 'f';
          alt_scale = FEET_TO_METERS(1.0);
        } else if (unit.startsWith("Meter", Qt::CaseInsensitive)) {
          altunit = 'm';
          alt_scale = 1.0;
        } else {
          gbFatal("Unknown unit (%s) used by altitude values!\n", gbLogCStr(unit));
        }
      }
    } else if ((linecount == 5) && (ozi_objective == trkdata)) {
      const QStringList parts = buff.split(',');
      if (parts.size() >= 4) {
          trk_head->rte_name = parts.at(3).trimmed();
      }
    }

    if (buff.contains(',')) {
      bool ozi_fsdata_used = false;
      ozi_fsdata* fsdata = ozi_alloc_fsdata();
      auto* wpt_tmp = new Waypoint;

      /* data delimited by commas. */
      const QStringList parts = buff.split(',');

      int i = 0;
      bool header = false;
      for (const auto& s : parts) {
        switch (ozi_objective) {
        case trkdata:
          ozi_parse_track(i, s, wpt_tmp, trk_name);
          break;
        case rtedata:
          if (buff[0] == 'R') {
            ozi_parse_routeheader(i, QString(s));
            header = true;
          } else {
            ozi_parse_routepoint(i, s, wpt_tmp);
          }

          break;
        case wptdata:
        case unknown_gpsdata:
          ozi_parse_waypt(i, s, wpt_tmp, fsdata);
          break;
        case posndata:
          gbFatal("realtime positioning not supported.\n");
          break;
        }
        i++;
      }

      switch (ozi_objective) {
      case trkdata:
        if (linecount > 6) {/* skipping over file header */
          ozi_convert_datum(wpt_tmp);
          track_add_wpt(trk_head, wpt_tmp);
        } else {
          delete wpt_tmp;
        }
        break;
      case rtedata:
        if ((linecount > 5) && !header) {/* skipping over file header */
          ozi_convert_datum(wpt_tmp);
          route_add_wpt(rte_head, wpt_tmp);
        } else {
          delete wpt_tmp;
        }
        break;
      case wptdata:
      case unknown_gpsdata:
        if (linecount > 4) {  /* skipping over file header */
          ozi_fsdata_used = true;
          wpt_tmp->fs.FsChainAdd(fsdata);
          ozi_convert_datum(wpt_tmp);
          waypt_add(wpt_tmp);
        } else {
          delete wpt_tmp;
        }
        break;
      case posndata:
        gbFatal("realtime positioning not supported.\n");
        break;
      }

      if (!ozi_fsdata_used) {
        delete fsdata;
      }

    } else {
      /* empty line */
    }

  }
}

void
OziFormat::ozi_waypt_pr(const Waypoint* wpt, int index)
{
  double alt;
  QString description;
  QString shortname;
  int faked_fsdata = 0;
  int icon = 0;

  const auto* fs = reinterpret_cast<ozi_fsdata*>(wpt->fs.FsChainFind(kFsOzi));

  if (!fs) {
    fs = ozi_alloc_fsdata();
    faked_fsdata = 1;
  }

  QString ozi_time = ozi_get_time_str(wpt);

  if (wpt->altitude == unknown_alt) {
    alt = -777;
  } else {
    alt = wpt->altitude * alt_scale;
  }
  if ((wpt->shortname.isEmpty()) || (global_opts.synthesize_shortnames)) {
    if (!wpt->description.isEmpty()) {
      if (global_opts.synthesize_shortnames) {
        shortname = mkshort_handle->mkshort_from_wpt(wpt);
      } else {
        shortname = csv_stringclean(wpt->description, BADCHARS);
      }
    } else {
      /* no description available */
      shortname = "";
    }
  } else {
    shortname = csv_stringclean(wpt->shortname, BADCHARS);
  }
  if (wpt->description.isEmpty()) {
    if (!shortname.isEmpty()) {
      description = csv_stringclean(shortname, BADCHARS);
    } else {
      description = "";
    }
  } else {
    description = csv_stringclean(wpt->description, BADCHARS);
  }

  if (wpt->icon_descr.toInt()) {
    icon = wpt->icon_descr.toInt();
  }

  *stream << index << ','
                   << shortname << ','
                   << qSetRealNumberPrecision(6) << wpt->latitude << ','
                   << wpt->longitude << ','
                   << ozi_time << ','
                   << icon << ','
                   << "1,3,"
                   << fs->fgcolor << ','
                   << fs->bgcolor << ','
                   << description << ",0,0,";
  if (wpt->proximity_has_value() && (wpt->proximity_value() > 0)) {
    *stream << qSetRealNumberPrecision(1) << wpt->proximity_value() * prox_scale << ',';
  } else if (proximity > 0) {
    *stream << qSetRealNumberPrecision(1) << proximity * prox_scale << ',';
  } else {
    *stream << "0,";
  }
  *stream << qSetRealNumberPrecision(0) << alt << ",6,0,17\r\n";

  if (faked_fsdata) {
    delete fs;
  }
}

void
OziFormat::write()
{
  if (waypt_count()) {
    track_out_count = route_out_count = 0;
    ozi_objective = wptdata;
    ozi_openfile(ozi_ofname);
    *stream << "OziExplorer Waypoint File Version 1.1\r\n"
                     << "WGS 84\r\n"
                     << "Reserved 2\r\n"
                     << "Reserved 3\r\n";

    int index = 0;
    auto ozi_waypt_pr_lambda = [this, &index](const Waypoint* waypointp)->void {
      ozi_waypt_pr(waypointp, ++index);
    };
    waypt_disp_all(ozi_waypt_pr_lambda);
  }

  if (track_count()) {
    ozi_objective = trkdata;
    ozi_track_pr(); /* ozi_track_hdr handles filenames / file_out */
  }

  if (route_count()) {
    ozi_objective = rtedata;
    ozi_openfile(ozi_ofname); /* ozi routes go in one big file */
    ozi_route_pr();
  }

}
