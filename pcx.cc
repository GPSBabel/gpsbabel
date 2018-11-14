/*
    Access to Garmin PCX5 files.
    Format described in: http://www.garmin.com/manuals/PCX5_OwnersManual.pdf

    Copyright (C) 2002-2017 Robert Lipe, robertlipe+source@gpsbabel.org

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
#include "cet_util.h"
#include "csv_util.h"
#include "garmin_tables.h"
#include <QtCore/QDebug>
#include <cmath>
#include <cstdio>
#include <cstdlib>

static gbfile* file_in, *file_out;
static short_handle mkshort_handle;
static short_handle mkshort_handle2; /* for track and route names */
static char* deficon = nullptr;
static char* cartoexploreur;
static int read_as_degrees;
static int read_gpsu;
static int route_ctr;
static int comment_col = 60; /* This has a default */
static int sym_col;
static int lat_col;
static int lon_col;

#define MYNAME "PCX"

static arglist_t pcx_args[] = {{"deficon", &deficon, "Default icon name",
                                "Waypoint", ARGTYPE_STRING, ARG_NOMINMAX, nullptr},
                               {"cartoexploreur", &cartoexploreur,
                                "Write tracks compatible with Carto Exploreur",
                                nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr},
                               ARG_TERMINATOR};

static void rd_init(const QString& fname) {
  file_in = gbfopen(fname, "rb", MYNAME);
}

static void rd_deinit() { gbfclose(file_in); }

static void wr_init(const QString& fname) {
  file_out = gbfopen(fname, "w", MYNAME);
  mkshort_handle = mkshort_new_handle();
  mkshort_handle2 = mkshort_new_handle();
}

static void wr_deinit() {
  gbfclose(file_out);
  mkshort_del_handle(&mkshort_handle);
  mkshort_del_handle(&mkshort_handle2);
}

// Find the first token in string |in| when there may be leading whitespace.
// These files have weird mixtures of spaces and tabs.
static QString FirstTokenAt(const QString& in, int index) {
  static const QRegExp sep("\\s+");
  return in.mid(index, -1).section(sep, 0, 0, QString::SectionSkipEmpty);
}

// Centralize Date/Time parsing between Waypoint and Trackpoint readers.
static void  SetWaypointTime(Waypoint* wpt, const QString& date, const QString& time) {
  QDate qdate = QDate::fromString(date, "dd-MMM-yy");
  QTime qtime = QTime::fromString(time, "hh:mm:ss");
  if (qdate.isValid() && qtime.isValid()) {
    wpt->SetCreationTime(QDateTime(qdate, qtime, Qt::UTC));
  }
}

// Loop and parse all the lines of the file. This is complicated by the
// variety of programs in the wild that loosely use this format and that
// there are two distinct versions of PCX.
// In the simplest form, white spaces are disallowed in the individual
// fields and everything is just whitespace separated and the fields have
// a fixed order.
// The presence of an "F" or "H" record changes the precedence of parse
// to allow fields in any order and length, based on their position in
// these header lines. Oddly, we've seen only 'W' records take this form.

static void data_read() {
  int symnum;
  Waypoint* wpt_tmp;
  char* buff;
  route_head* track = nullptr;
  route_head* route = nullptr;
  int line_number = 0;

  read_as_degrees = 0;
  int points = 0;

  // Each line is both |buff| as a C string and |line| as a QString.
  while ((buff = gbfgetstr(file_in))) {
    const QString line = QString(buff).trimmed();
    char* ibuf = lrtrim(buff);
    if ((line_number++ == 0) && file_in->unicode) {
      cet_convert_init(CET_CHARSET_UTF8, 1);
    }

    switch (ibuf[0]) {
      case 'W': {
        QStringList tokens =
            line.split(QRegExp("\\s+"), QString::KeepEmptyParts);
        if (tokens.size() < 5) {
          fatal(MYNAME
                ": Unable to parse waypoint, not all required columns "
                "contained\n");
        }
        // tokens[0] = "W".
        QString name = tokens[1];
        QString tbuf = tokens[2];
        QString nbuf = tokens[3];
        QString date = tokens[4];
        QString time = tokens[5];
        long alt = unknown_alt;
        if (tokens.size() == 7) {
          alt = tokens[6].toDouble();
        }

        QString desc;
        if (comment_col > 0) {
          desc = line.mid(comment_col, -1);
        }

        symnum = 18;
        if (sym_col > 0) {
          symnum = atoi(&ibuf[sym_col]);
        }

        // If we have explicit columns for lat and lon,
        // copy those entire words (warning: no spaces)
        // into the respective coord buffers.
        if (lat_col > 0) {
          tbuf = FirstTokenAt(line, lat_col);
        }
        if (lon_col > 0) {
          nbuf = FirstTokenAt(line, lon_col);
        }

        wpt_tmp = new Waypoint;
        wpt_tmp->altitude = alt;
        SetWaypointTime(wpt_tmp, date, time);
        wpt_tmp->shortname = name.trimmed();
        wpt_tmp->description = desc.trimmed();
        wpt_tmp->icon_descr = gt_find_desc_from_icon_number(symnum, PCX);

        double lat = 0;
        double lon = 0;
        if (read_as_degrees || read_gpsu) {
          human_to_dec(tbuf, &lat, &lon, 1);
          human_to_dec(nbuf, &lat, &lon, 2);
          wpt_tmp->longitude = lon;
          wpt_tmp->latitude = lat;
        } else {
          lat = tbuf.midRef(1, -1).toDouble();
          lon = nbuf.midRef(1, -1).toDouble();
          if (tbuf[0] == 'S') {
            lat = -lat;
          }
          if (nbuf[0] == 'W') {
            lon = -lon;
          }
          wpt_tmp->longitude = ddmm2degrees(lon);
          wpt_tmp->latitude = ddmm2degrees(lat);
        }

        if (route != nullptr) {
          route_add_wpt(route, new Waypoint(*wpt_tmp));
        }
        waypt_add(wpt_tmp);
        points++;
        break;
      }
      case 'H':
        /* Garmap2 has headers
        "H(2 spaces)LATITUDE(some spaces)LONGTITUDE(etc... followed by);track
          everything else is
          H(2 chars)TN(tracknane\0)
          */
        if (points > 0) {
          track = nullptr;
          points = 0;
        }
        if (track == nullptr) {
          if (ibuf[3] == 'L' && ibuf[4] == 'A') {
            track = route_head_alloc();
            track->rte_name = "track";
            track_add_head(track);
          } else if (ibuf[3] == 'T' && ibuf[4] == 'N') {
            track = route_head_alloc();
            track->rte_name = &ibuf[6];
            track_add_head(track);
          }
        }
        break;
      case 'R':
        route = route_head_alloc();
        route->rte_name = QString(&ibuf[1]).trimmed();
        route_add_head(route);
        break;
      case 'T': {
        QStringList tokens =
            line.split(QRegExp("\\s+"), QString::KeepEmptyParts);
        if (tokens.size() < 5) {
          fatal(MYNAME
                ": Unable to parse trackpoint, not all required columns "
                "contained\n");
        }

        // tokens[0] = "W".
        QString tbuf = tokens[1];
        QString nbuf = tokens[2];
        QString date = tokens[3];
        QString time = tokens[4];
        double alt = tokens[5].toDouble();

        wpt_tmp = new Waypoint;
        SetWaypointTime(wpt_tmp, date, time);

        double lat, lon;
        if (read_as_degrees) {
          human_to_dec(tbuf, &lat, &lon, 1);
          human_to_dec(nbuf, &lat, &lon, 2);

          wpt_tmp->longitude = lon;
          wpt_tmp->latitude = lat;
        } else {
          lat = tbuf.midRef(1, -1).toDouble();
          lon = nbuf.midRef(1, -1).toDouble();
          if (tbuf[0] == 'S') {
            lat = -lat;
          }
          if (nbuf[0] == 'W') {
            lon = -lon;
          }
          wpt_tmp->longitude = ddmm2degrees(lon);
          wpt_tmp->latitude = ddmm2degrees(lat);
        }
        wpt_tmp->altitude = alt;

        /* Did we get a track point before a track header? */
        if (track == nullptr) {
          track = route_head_alloc();
          track->rte_name = "Default";
          track_add_head(track);
        }
        track_add_wpt(track, wpt_tmp);
        points++;
        break;
      }
      case 'U':
        read_as_degrees = !strncmp("LAT LON DEG", ibuf + 3, 11);
        if (strstr(ibuf, "UTM")) {
          fatal(MYNAME ": UTM is not supported.\n");
        }
        break;
      // GPSU is apparently PCX but with a different definition
      // of "LAT LON DM" - unlike the other, it actually IS decimal
      // minutes.
      case 'I':
        read_gpsu = !(strstr(ibuf, "GPSU") == nullptr);
        break;
      // This is a format specifier.  Use this line to figure out
      // where our other columns start.
      case 'F': {
        comment_col = line.indexOf("comment", 0, Qt::CaseInsensitive);
        sym_col = line.indexOf("symbol", 0, Qt::CaseInsensitive);
        lat_col = line.indexOf("latitude", 0, Qt::CaseInsensitive);
        lon_col = line.indexOf("longitude", 0, Qt::CaseInsensitive);
      } break;
      default:
        break;
        ;
    }
  }
}

static void gpsutil_disp(const Waypoint* wpt) {
  int icon_token = 0;

  double lon = degrees2ddmm(wpt->longitude);
  double lat = degrees2ddmm(wpt->latitude);

  QDateTime dt = wpt->GetCreationTime().toUTC();
  const QString ds = dt.toString("dd-MMM-yy hh:mm:ss").toUpper();

  if (deficon) {
    icon_token = atoi(deficon);
  } else {
    icon_token = gt_find_icon_number_from_desc(wpt->icon_descr, PCX);
    if (get_cache_icon(wpt)) {
      icon_token = gt_find_icon_number_from_desc(get_cache_icon(wpt), PCX);
    }
  }

  gbfprintf(file_out, "W  %-6.6s %c%08.5f %c%011.5f %s %5.f %-40.40s %5e  %d\n",
            global_opts.synthesize_shortnames
                ? CSTRc(mkshort_from_wpt(mkshort_handle, wpt))
                : CSTRc(wpt->shortname),
            lat < 0.0 ? 'S' : 'N', fabs(lat), lon < 0.0 ? 'W' : 'E', fabs(lon),
            CSTR(ds), (wpt->altitude == unknown_alt) ? -9999 : wpt->altitude,
            (wpt->description != nullptr) ? CSTRc(wpt->description) : "", 0.0,
            icon_token);
}

static void pcx_track_hdr(const route_head* trk) {
  route_ctr++;

  QString default_name = QString().sprintf("Trk%03d", route_ctr);
  QString name =
      mkshort(mkshort_handle2,
              trk->rte_name.isEmpty() ? CSTR(default_name) : trk->rte_name);
  /* Carto Exploreur (popular in France) chokes on trackname headers,
   * so provide option to supppress these.
   */
  if (!cartoexploreur) {
    gbfprintf(file_out, "\n\nH  TN %s\n", CSTR(name));
  }
  gbfprintf(file_out,
            "H  LATITUDE    LONGITUDE    DATE      TIME     ALT  ;track\n");
}

static void pcx_route_hdr(const route_head* rte) {
  route_ctr++;
  QString default_name = QString().sprintf("Rte%03d", route_ctr);

  QString name = mkshort(
      mkshort_handle2, rte->rte_name.isEmpty() ? default_name : rte->rte_name);

  /* see pcx_track_hdr */
  if (!cartoexploreur) {
    gbfprintf(file_out, "\n\nR  %s\n", CSTR(name));
  }
  gbfprintf(file_out,
            "\n"
            "H  IDNT   LATITUDE    LONGITUDE    DATE      TIME     ALT   "
            "DESCRIPTION                              PROXIMITY     SYMBOL "
            ";waypts\n");
}

static void pcx_track_disp(const Waypoint* wpt) {
  double lon = degrees2ddmm(wpt->longitude);
  double lat = degrees2ddmm(wpt->latitude);

  QDateTime dt = wpt->GetCreationTime().toUTC();
  const QString ds = dt.toString("dd-MMM-yy hh:mm:ss").toUpper();

  gbfprintf(file_out, "T  %c%08.5f %c%011.5f %s %.f\n", lat < 0.0 ? 'S' : 'N',
            fabs(lat), lon < 0.0 ? 'W' : 'E', fabs(lon), CSTR(ds),
            wpt->altitude);
}

static void data_write() {
  gbfprintf(file_out,
            "H  SOFTWARE NAME & VERSION\n"
            "I  PCX5 2.09\n"
            "\n"
            "H  R DATUM                IDX DA            DF            DX      "
            "      DY            DZ\n"
            "M  G WGS 84               121 +0.000000e+00 +0.000000e+00 "
            "+0.000000e+00 +0.000000e+00 +0.000000e+00\n"
            "\n"
            "H  COORDINATE SYSTEM\n"
            "U  LAT LON DM\n");

  setshort_length(mkshort_handle, 6);

  setshort_length(mkshort_handle2, 20); /* for track and route names */
  setshort_whitespace_ok(mkshort_handle2, 0);
  setshort_mustuniq(mkshort_handle2, 0);

  if (global_opts.objective == wptdata) {
    gbfprintf(file_out,
              "\n"
              "H  IDNT   LATITUDE    LONGITUDE    DATE      TIME     ALT   "
              "DESCRIPTION                              PROXIMITY     SYMBOL "
              ";waypts\n");

    waypt_disp_all(gpsutil_disp);
  } else if (global_opts.objective == trkdata) {
    route_ctr = 0;
    track_disp_all(pcx_track_hdr, nullptr, pcx_track_disp);
  } else if (global_opts.objective == rtedata) {
    route_ctr = 0;
    route_disp_all(pcx_route_hdr, nullptr, gpsutil_disp);
  }
}

ff_vecs_t pcx_vecs = {
    ff_type_file,      FF_CAP_RW_ALL, rd_init,    wr_init, rd_deinit,
    wr_deinit,         data_read,     data_write, nullptr,    pcx_args,
    CET_CHARSET_ASCII, 1 /* CET-REVIEW */
  , NULL_POS_OPS,
  nullptr
};
