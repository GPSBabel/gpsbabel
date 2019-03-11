/*

    Support for Suunto Trackmanager SDF format.

    Copyright (C) 2005,2007 Olaf Klein, o.b.klein@gpsbabel.org

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

/*
    2006/04/05: initial release (not published in GPSBabel)
    2006/07/19: finished reader and writer for type 4,5,28 of ver. 1
    2006/10/31: remove wptdata from case statement (data_write)

    ToDo: Ascending/Descending
*/

#include "defs.h"

#if CSVFMTS_ENABLED

#include <algorithm>                  // for sort
#include <cstdlib>                    // for atoi
#include <cstring>                    // for strchr
#include <ctime>                      // for localtime, strftime

#include <QtCore/QDate>               // for QDate
#include <QtCore/QDateTime>           // for QDateTime
#include <QtCore/QList>               // for QList<>::iterator, QList
#include <QtCore/QRegularExpression>  // for QRegularExpression
#include <QtCore/QString>             // for QString, operator+, QString::KeepEmptyParts
#include <QtCore/QStringList>         // for QStringList
#include <QtCore/QTime>               // for QTime
#include <QtCore/QtGlobal>            // for qAsConst, QAddConst<>::Type

#include "cet_util.h"                 // for cet_convert_init
#include "csv_util.h"                 // for csv_lineparse
#include "gbfile.h"                   // for gbfprintf, gbfclose, gbfopen, gbfgetstr, gbfile
#include "grtcirc.h"                  // for RAD, gcdist, heading_true_degrees, radtometers
#include "jeeps/gpsmath.h"            // for GPS_Lookup_Datum_Index, GPS_Math_WGS84_To_Known_Datum_M
#include "src/core/datetime.h"        // for DateTime
#include "src/core/logging.h"         // for Warning, Fatal


#define MYNAME "stmsdf"

#define ALT(a) (a->altitude != unknown_alt) ? a->altitude : 0

typedef enum {
  sdf_unknown,
  sdf_header,
  sdf_points,
  sdf_custom
} sdf_section_e;

static gbfile* fin, *fout;

static int lineno;
static int datum;
static int filetype;
static route_head* route;
static QList<Waypoint*> trackpts;
static QString rte_name;
static QString rte_desc;

static const Waypoint* trkpt_out;
static const route_head* trk_out;

static double trkpt_dist;
static double minalt, maxalt, maxspeed;
static double this_distance, all_dist;
static time_t this_time, all_time;
static double all_asc, all_desc;
static int this_index;			/* from 1 to ... */
static int all_points;
static int this_points;
static int saved_points;
static time_t start_time;
static bool this_valid;
static short_handle short_h;

#define route_index this_index
#define track_index this_index
#define all_route_points all_points
#define all_track_points all_points
#define route_points this_points
#define track_points this_points
#define saved_track_points saved_points
#define this_route_valid this_valid

/* placeholders for options */

static char* opt_route_index;
static int opt_route_index_value;

static
arglist_t stmsdf_args[] = {
  {
    "index", &opt_route_index,
    "Index of route (if more than one in source)", "1", ARGTYPE_INT, "1", nullptr, nullptr
  },
  ARG_TERMINATOR
};


/* ----------------------------------------------------------- */

static void
parse_header(char* line)
{
  char* str;
  QString key;
  const char* prod = nullptr;
  int column = -1;

  while ((str = csv_lineparse(line, "=", "", lineno))) {
    line = nullptr;
    column++;
    QString qstr(str);
    bool ok;

    switch (column) {
    case 0:
      key = qstr.toUpper();
      break;
    case 1:
      if (key == "DATUM") {
        datum = GPS_Lookup_Datum_Index(str);
      } else if (key == "FILEVERSION") {
        int ver = qstr.toInt(&ok);
        is_fatal(!ok || (ver != 1),
                 MYNAME ": This version '%d' is not yet supported. Please report!", ver);
      } else if (key == "NAME") {
        rte_name = str;
      } else if (key == "NOTES") /* ToDo */;
      else if (key == "SOURCE") {
        rte_desc = str;
      } else if (key == "TYPE") {
        filetype = qstr.toInt(&ok);
        if (!ok) {
          Fatal() << MYNAME << "Unknown file type " << key;
        }
        switch (filetype) {
        case 4:	/* M9 TrackLog (Suunto Sail Manager) */
        case 5: /* route */
        case 28: /* X9 TrackLog (Suunto Trek Manager */
          break;

        // 2014-02-11: Added breaks after 78 and 79 as the author "obviously"
        // meant to treat those as handled.
        case 78:
          prod = "S6 SkiChrono";
          break;
        case 79:
          prod = "S6 Skilog";
          break;

        default:
          if (prod == nullptr) {
            prod = "unknown";
          }
          fatal(MYNAME ": Unsupported file type (%s, type %d)!\n", prod, filetype);
        }
        break;
      default:
        break;
      }
    }
  }
}

static bool
track_sort_cb(const Waypoint* a, const Waypoint* b)
{
  return a->GetCreationTime() < b->GetCreationTime();
}

static void
finalize_tracks()
{
  route_head* track = nullptr;
  int trackno = 0;

  if (trackpts.isEmpty()) {
    return;
  }

  std::sort(trackpts.begin(), trackpts.end(), track_sort_cb);

  foreach (Waypoint* wpt, trackpts) {
    if (wpt->wpt_flags.fmt_use == 2) {	/* log continued */
      track = nullptr;
    }
    if (track == nullptr) {
      track = route_head_alloc();
      track_add_head(track);
      trackno++;
      if (rte_name != nullptr) {
        if (trackno > 1) {
          track->rte_name = QString("%1 (%2)").arg(rte_name).arg(trackno);
        } else {
          track->rte_name = rte_name;
        }
      }
      if (rte_desc != nullptr) {
        track->rte_desc = rte_desc;
      }
    }
    track_add_wpt(track, wpt);
    if (wpt->wpt_flags.fmt_use == 1) { /* log pause */
      track = nullptr;
    }
    wpt->wpt_flags.fmt_use = 0;
  }

  trackpts.clear();
}

static void
parse_point(char *line) {
  char *str;
  int column = -1;
  int what = -1;        /* -1 = unknown, 0 = tp, 1 = mp, 2 = wp, 3 = ap  */
  Waypoint *wpt = nullptr;
  QDate dt;
  QTime tm;

  while ((str = csv_lineparse(line, ",", "", lineno))) {

    line = nullptr;
    column++;
    QString qstr(str);
    bool ok(true);
    // TODO: Several entries use a QString variant. This whole function should just parse it like that.

    switch (column) {
      case 0:
        if (qstr == "\"TP\"") {
          what = 0;
          column++;    /* skip name */
        } else if (qstr == "\"MP\"") {
          what = 1;
        } else if (qstr == "\"WP\"") {
          what = 2;
        } else if (qstr == "\"AP\"") {
          what = 3;
        } else {
          warning(MYNAME ": Unknown point type %s at line %d!\n", str, lineno);
          return;
        }
        wpt = new Waypoint;
        break;

      case 1:
        wpt->shortname = qstr.remove('\"');
        if ((what == 2) || (what == 3)) {
          column += 2;  /* doesn't have date and time */
        }
        break;
      case 2: {
        // Date is in format dd.mm.yyyy
        auto v = qstr.split('.', QString::KeepEmptyParts);

        if (v.size() == 3) {
          auto day = v[0].toInt();
          auto month = v[1].toInt();
          auto year = v[2].toInt();
          dt = QDate(year, month, day);
        } else {
          Fatal() << MYNAME << "Invalid date" << qstr;
        }
        break;
      }
      case 3: {
        // Time is hh:mm.ss - yes, colon and period.
        auto v = qstr.split(QRegularExpression("[.:]"), QString::KeepEmptyParts);
        if (v.size() == 3) {
          auto hour = v[0].toInt();
          auto min = v[1].toInt();
          auto sec = v[2].toInt();
          tm = QTime(hour, min, sec);
        } else {
          Fatal() << MYNAME << "Invalid Time" << qstr;
        }
        break;
      }
      case 4:
        wpt->latitude = qstr.toDouble(&ok);
        if (!ok) {
          Fatal() << MYNAME << "Invalid latitude" << qstr;
        }
        break;
      case 5:
        wpt->longitude = qstr.toDouble(&ok);
        if (!ok) {
          Fatal() << MYNAME << "Invalid longitude" << qstr;
        }
        break;
      case 6: {
        // Not entirely sure if this is optional.
        double alt = qstr.toDouble(&ok);
        if (ok) {
          wpt->altitude = alt;
        }
      }
        break;
      case 7: {
        auto v = qstr.toFloat(&ok);
        if (ok) {
          if (what == 0) {
            WAYPT_SET(wpt, speed, v * 3.6);
          } else if (what == 3) {
            WAYPT_SET(wpt, proximity, v);
            wpt->notes = QString("Alarm point: radius=" + qstr);
          }
        }
        break;
      }
      case 8:
        if (what == 0) {
          auto scourse = qstr.toFloat(&ok);
          if (ok) {
            WAYPT_SET(wpt, course, scourse);
          }
        }
        break;
      case 9:
      case 10:
      default:
        break;
      case 11:
        if (wpt && what == 1) {
          wpt->wpt_flags.fmt_use = qstr.toUInt(&ok);  /* memory point type */
        }
        break;
    }
  }

  if (dt.isValid() && tm.isValid()) {
    wpt->SetCreationTime(QDateTime(dt, tm));
  }

  if (datum != DATUM_WGS84) {
    double ht;
    GPS_Math_WGS84_To_Known_Datum_M(wpt->latitude, wpt->longitude, 0,
                                    &wpt->latitude, &wpt->longitude, &ht, datum);
  }

  switch (what) {
    case 0:
    case 1:
    trackpts.append(wpt);
      break;
    case 2:
    case 3:
      if (route == nullptr) {
        route = route_head_alloc();
        route_add_head(route);
      }
      route_add_wpt(route, wpt);
      break;
    default:
      Warning() << MYNAME << "Invalid internal field type" << what;
  }
}

/* ----------------------------------------------------------- */

static void
rd_init(const QString& fname)
{
  fin = gbfopen(fname, "r", MYNAME);

  lineno = 0;
  route = nullptr;
  datum = DATUM_WGS84;
  filetype = 28;
  rte_name = rte_desc = QString();

  trackpts.clear();
}

static void
rd_deinit()
{
  gbfclose(fin);
  rte_name = QString();
  rte_desc = QString();
}

static void
data_read()
{
  char* buf;
  sdf_section_e section = sdf_unknown;

  while ((buf = gbfgetstr(fin))) {
    char* cin = lrtrim(buf);
    if ((lineno++ == 0) && fin->unicode) {
      cet_convert_init(CET_CHARSET_UTF8, 1);
    }

    if (*cin == '\0') {
      continue;
    }

    if (*cin == '[') {
      char* cend = strchr(++cin, ']');

      if (cend != nullptr) {
        *cend = '\0';
        cin = lrtrim(cin);
      }
      if ((*cin == '\0') || (cend == nullptr)) {
        fatal(MYNAME ": Invalid section header!\n");
      }

      if (case_ignore_strcmp(cin, "HEADER") == 0) {
        section = sdf_header;
      } else if (case_ignore_strcmp(cin, "POINTS") == 0) {
        section = sdf_points;
      } else if (case_ignore_strncmp(cin, "CUSTOM", 6) == 0) {
        section = sdf_custom;
      } else {
        warning(MYNAME ": Unknown section \"%s\". Please report.\n", cin);
        section = sdf_unknown;
      }
    } else switch (section) {
      case sdf_header:
        parse_header(cin);
        break;
      case sdf_points:
        parse_point(cin);
        break;
      case sdf_custom:
      case sdf_unknown:
        break;
      }
  }
  finalize_tracks();	/* memory points can be at the end of all trackpoints */
}


static void
calculate(const Waypoint* wpt, double* dist, double* speed, double* course,
          double* asc, double* desc)
{
  if (trkpt_out != nullptr) {
    *course = heading_true_degrees(
                RAD(trkpt_out->latitude), RAD(trkpt_out->longitude),
                RAD(wpt->latitude), RAD(wpt->longitude));

    *dist = radtometers(gcdist(
                          RAD(trkpt_out->latitude), RAD(trkpt_out->longitude),
                          RAD(wpt->latitude), RAD(wpt->longitude)));
    if (*dist < 0.1) {
      *dist = 0;  /* calc. diffs on 32- and 64-bit hosts */
    }

    time_t time = wpt->creation_time.toTime_t() - trkpt_out->GetCreationTime().toTime_t();
    if (time == 0) {
      *speed = 0;
    } else {
      *speed = *dist / (double)time;
    }

    if (asc && desc && (trkpt_out->altitude != unknown_alt) && (wpt->altitude != unknown_alt)) {
      double dh = wpt->altitude - trkpt_out->altitude;
      if (dh > 0) {
        *asc += dh;
      } else {
        *desc -= dh;
      }
    }
  } else {
    *speed = 0;
    *dist = 0;
    *course = 0;
    if (asc) {
      *asc = 0;
    }
    if (desc) {
      *desc = 0;
    }
  }
  if WAYPT_HAS(wpt, speed) {
    *speed = wpt->speed / 3.6;  /* -> meters per second */
  }
  if WAYPT_HAS(wpt, course) {
    *course = wpt->course;
  }

}

/* pre-calculation callbacks */

static void
any_hdr_calc_cb(const route_head* trk)
{

  trkpt_out = nullptr;
  this_distance = 0;
  this_time = 0;
  this_points = 0;

  this_index++;
  this_valid = ((opt_route_index_value < 1) || (opt_route_index_value == this_index));
  if (! this_valid) {
    return;
  }

  if (rte_name.isEmpty() && !trk->rte_name.isEmpty()) {
    rte_name = trk->rte_name;
    rte_desc = trk->rte_desc;
  }

  trk_out = trk;
}

static void
any_waypt_calc_cb(const Waypoint* wpt)
{
  double speed, course, dist;

  /* we can only write ONE route */
  if (! this_valid) {
    return;
  }

  if ((all_points == 0) && (this_points == 0)) {
    start_time = wpt->GetCreationTime().toTime_t();
  }

  this_points++;

  if ((wpt->altitude != unknown_alt) && (wpt->altitude < minalt)) {
    minalt = wpt->altitude;
  }
  if ((wpt->altitude != unknown_alt) && (wpt->altitude > maxalt)) {
    maxalt = wpt->altitude;
  }
  calculate(wpt, &dist, &speed, &course, &all_asc, &all_desc);
  if (speed > maxspeed) {
    maxspeed = speed;
  }

  this_distance = this_distance + dist;
  if (trkpt_out != nullptr) {
    this_time += (wpt->GetCreationTime().toTime_t() - trkpt_out->GetCreationTime().toTime_t());
  }

  trkpt_out = wpt;
}

static void
any_tlr_calc_cb(const route_head*)
{
  if (! this_valid) {
    return;
  }

  all_dist += this_distance;
  all_time += this_time;
  all_points += this_points;
}

/* write callbacks */

static void
track_disp_hdr_cb(const route_head* trk)
{
  track_index++;
  track_points = 0;
  trk_out = trk;
  trkpt_out = nullptr;
}


static void
track_disp_wpt_cb(const Waypoint* wpt)
{
  struct tm tm;
  char tbuf[32];
  double course, speed, dist;
  int flag = 0;

  track_points++;
  all_track_points++;

  time_t ct = wpt->GetCreationTime().toTime_t();
  tm = *localtime(&ct);
  strftime(tbuf, sizeof(tbuf), "%d.%m.%Y,%H:%M.%S", &tm);

  calculate(wpt, &dist, &speed, &course, nullptr, nullptr);
  trkpt_dist = trkpt_dist + dist;

  if (track_points == trk_out->rte_waypt_ct) {	/* I'm the last in that list */
    if (all_track_points != saved_track_points) {	/* but not the overall latest */
      flag = 1;
    }
  } else if (track_points == 1) {			/* I'm the first in that list */
    if (all_track_points > 1) {			/* but not the first ever seen */
      flag = 2;
    }
  }

  if (flag == 1) {
    QString name = wpt->shortname;
    if (name == nullptr) {
      name = "Log paused";
    }
    gbfprintf(fout, "\"MP\",\"%s\"", CSTR(name));
  } else if (flag == 2) {
    QString name = wpt->shortname;
    if (name == nullptr) {
      name = "Log continued";
    }
    gbfprintf(fout, "\"MP\",\"%s\"", CSTR(name));
  } else {
    gbfprintf(fout, "\"TP\"");
  }

  gbfprintf(fout, ",%s,%.6lf,%.6lf,%.f,%.2f",
            tbuf,
            wpt->latitude, wpt->longitude, ALT(wpt), speed);
  if (flag) {
    gbfprintf(fout, ",0,0,%d", flag);  /* press, temperature, memory point type */
  } else {
    gbfprintf(fout, ",%.1f", course);
  }

  if (trkpt_dist != 0) {
    gbfprintf(fout, ",%.6f\n", trkpt_dist);
  } else {
    gbfprintf(fout, ",0\n");
  }

  trkpt_out = wpt;
}

static void
track_disp_tlr_cb(const route_head*)
{
  trkpt_out = nullptr;
}

static void
route_disp_hdr_cb(const route_head*)
{
  route_index++;
  this_route_valid = ((opt_route_index_value < 1) || (opt_route_index_value == track_index));
}

static void
route_disp_wpt_cb(const Waypoint* wpt)
{
  if (this_route_valid) {
    QString sn;

    if (global_opts.synthesize_shortnames) {
      sn = mkshort_from_wpt(short_h, wpt);
    } else {
      sn = mkshort(short_h, wpt->shortname);
    }
    gbfprintf(fout, "\"WP\",\"%s\",%.8lf,%.8lf,%.f\n",
              CSTR(sn), wpt->latitude, wpt->longitude, ALT(wpt));
  }
}

static void
track_disp_custom_cb(const Waypoint* wpt)
{
  if (wpt->GetCreationTime().isValid() && (wpt->altitude != unknown_alt)) {
    gbfprintf(fout, "%d,%.f\n", (int)(wpt->GetCreationTime().toTime_t() - start_time), wpt->altitude);
  }
}

static void
wr_init(const QString& fname)
{
  fout = gbfopen(fname, "w", MYNAME);
  short_h = mkshort_new_handle();
}

static void
wr_deinit()
{
  mkshort_del_handle(&short_h);
  gbfclose(fout);
}

static void
data_write()
{
  gbfprintf(fout, "[HEADER]\n");
  gbfprintf(fout, "FILEVERSION=1\n");
  gbfprintf(fout, "SOURCE=FILE\n");
  gbfprintf(fout, "DATUM=WGS84\n");

  rte_name = QString();
  rte_desc = QString();
  trkpt_out = nullptr;
  opt_route_index_value = -1;	/* take all tracks from data pool */
  track_index = 0;
  minalt = -unknown_alt;
  maxalt = unknown_alt;
  maxspeed = 0;
  all_dist = 0;
  all_time = 0;
  all_asc = 0;
  all_desc = 0;
  all_points = 0;
  start_time = 0;

  setshort_length(short_h, 100);
  setshort_badchars(short_h, "\r\n");
  setshort_mustupper(short_h, 0);
  setshort_mustuniq(short_h, 0);
  setshort_whitespace_ok(short_h, 1);
  setshort_repeating_whitespace_ok(short_h, 1);

  switch (global_opts.objective) {
  case wptdata:
  case unknown_gpsdata:
    break;

  case rtedata:
    gbfprintf(fout, "TYPE=5\n");

    opt_route_index_value = atoi(opt_route_index);
    route_disp_all(any_hdr_calc_cb, any_tlr_calc_cb, any_waypt_calc_cb);
    gbfprintf(fout, "DISTANCE=%.f\n", all_dist);
    if (!rte_name.isEmpty()) {
      gbfprintf(fout, "NAME=%s\n", CSTR(rte_name));
    }
    gbfprintf(fout, "[POINTS]\n");
    if (route_points > 0) {
      track_index = 0;
      route_disp_all(route_disp_hdr_cb, nullptr, route_disp_wpt_cb);
    }
    break;

  case trkdata:
    gbfprintf(fout, "TYPE=28\n");

    track_disp_all(any_hdr_calc_cb, any_tlr_calc_cb, any_waypt_calc_cb);
    if (all_track_points > 0) {
      if (!rte_name.isEmpty()) {
        gbfprintf(fout, "NAME=%s\n", CSTR(rte_name));
      }
      if (minalt != -unknown_alt) {
        gbfprintf(fout, "MINALT=%.f\n", minalt);
      }
      if (maxalt != unknown_alt) {
        gbfprintf(fout, "MAXALT=%.f\n", maxalt);
      }
      gbfprintf(fout, "MAXSPEED=%.2f\n", maxspeed);
      gbfprintf(fout, "DISTANCE=%.f\n", all_dist);
      gbfprintf(fout, "DURATION=%lu\n", all_time);
//				gbfprintf(fout, "TOTASC=%.f\n", all_asc);
//				gbfprintf(fout, "TOTDSC=%.f\n", all_desc);
      if (start_time) {
        struct tm tm;
        char tbuf[32];

        tm = *localtime(&start_time);
        strftime(tbuf, sizeof(tbuf), "%d.%m.%Y %H:%M.%S", &tm);
        gbfprintf(fout, "DATE=%s\n", tbuf);
      }
      if (all_time) {
        gbfprintf(fout, "AVGSPEED=%.2f\n", all_dist / (double)all_time);
      }
    }
    gbfprintf(fout, "[POINTS]\n");
    if (all_track_points > 0) {

      trkpt_dist = 0;
      saved_track_points = all_track_points;
      all_track_points = 0;
      track_disp_all(track_disp_hdr_cb, track_disp_tlr_cb, track_disp_wpt_cb);

      if (start_time) {
        gbfprintf(fout, "[CUSTOM1]\n");
        track_index = 0;
        track_disp_all(nullptr, nullptr, track_disp_custom_cb);
      }
    }
    break;
  case posndata:
    fatal(MYNAME ": Realtime positioning not supported.\n");
    break;
  }
}

/* ------------------------------------------------------------------ */

ff_vecs_t stmsdf_vecs = {
  ff_type_file,
  {
    ff_cap_none,
    (ff_cap)(ff_cap_read | ff_cap_write),
    (ff_cap)(ff_cap_read | ff_cap_write)
  },
  rd_init,
  wr_init,
  rd_deinit,
  wr_deinit,
  data_read,
  data_write,
  nullptr,
  stmsdf_args,
  CET_CHARSET_MS_ANSI, 0	/* CET-REVIEW */
  , NULL_POS_OPS,
  nullptr
};

/* ================================================================== */

#endif /* CSVFMTS_ENABLED */

