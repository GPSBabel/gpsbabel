/*

   Support for "Suunto Trek Manager" (STM) WaypointPlus files,
   see homepage "http://www.suunto.fi" for more details,

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


#include "defs.h"

#if CSVFMTS_ENABLED

#include "csv_util.h"
#include "cet_util.h"
#include <cctype>
#include <cstdio>
#include <cstdlib>

static gbfile* fin, *fout;
static route_head* track, *route;
static Waypoint* wpt;
static short_handle short_h;

#define MYNAME "STMwpp"

#define STM_NOTHING	0
#define STM_WAYPT 	1
#define STM_TRKPT 	2
#define STM_RTEPT 	3

static int track_index;
static int track_num;
static int what;

static char* index_opt = nullptr;

static
arglist_t stmwpp_args[] = {
  {
    "index", &index_opt, "Index of route/track to write (if more than one in source)",
    nullptr, ARGTYPE_INT, "1", nullptr, nullptr
  },
  ARG_TERMINATOR
};


static void
stmwpp_rd_init(const QString& fname)
{
  fin = gbfopen(fname, "rb", MYNAME);
  track = nullptr;
  route = nullptr;
  wpt = nullptr;
}

static void
stmwpp_rd_deinit()
{
  gbfclose(fin);
}

static void
stmwpp_data_read()
{
  int line = 0;

  what = STM_NOTHING;
  char* buff = gbfgetstr(fin);
  buff = (buff == nullptr) ? (char*) "" : buff;

  if (case_ignore_strncmp(buff, "Datum,WGS 84,WGS 84,", 20) != 0) {
    fatal(MYNAME ": Invalid GPS datum or not \"WaypointPlus\"\" file!\n");
  }

  while ((buff = gbfgetstr(fin))) {
    char* c;
    int column = -1;
    struct tm time;

    if ((line++ == 0) && fin->unicode) {
      cet_convert_init(CET_CHARSET_UTF8, 1);
    }

    buff = lrtrim(buff);
    if (*buff == '\0') {
      continue;
    }

    wpt = nullptr;
    memset(&time, 0, sizeof(time));
    int milliseconds = 0;

    while ((c = csv_lineparse(buff, ",", "", column++))) {
      int new_what;

      buff = nullptr;

      switch (column) {
      case 0:
        if (case_ignore_strcmp(c, "WP") == 0) {
          new_what = STM_WAYPT;
        } else if (case_ignore_strcmp(c, "TP") == 0) {
          new_what = STM_TRKPT;
        } else {
          fatal(MYNAME ": Unknown feature \"%s\"!\n", c);
        }

        if ((what != STM_NOTHING) && (new_what != what)) {
          fatal(MYNAME ": Only one feature (route or track) is supported by STM!\n");
        }

        what = new_what;
        wpt = new Waypoint;
        break;

      case 1:
        if (what == STM_TRKPT) {
          column++;  /* no name -> skip column two */
        }
        break;

      case 2:
        wpt->shortname = QString::fromLatin1(c);
        break;

      case 3:
        wpt->latitude = atof(c);
        break;

      case 4:
        wpt->longitude = atof(c);
        break;

      case 5:
        sscanf(c, "%d/%d/%d", &time.tm_mon, &time.tm_mday, &time.tm_year);
        break;

      case 6:
        sscanf(c, "%d:%d:%d.%d", &time.tm_hour, &time.tm_min, &time.tm_sec, &milliseconds);
        /* makes sense only for recorded trackpoints */
        if (what != STM_TRKPT) {
          milliseconds = 0;
        }
        break;

      default:
        break;
      }
    }
    if (wpt != nullptr) {
      time.tm_year -= 1900;
      time.tm_mon--;
      wpt->SetCreationTime(mkgmtime(&time), milliseconds);

      switch (what) {
      case STM_WAYPT:
        waypt_add(wpt);
        if (global_opts.objective == rtedata) {
          if (route == nullptr) {
            route = route_head_alloc();
            route_add_head(route);
          }
          route_add_wpt(route, new Waypoint(*wpt));
        }
        break;

      case STM_TRKPT:
        if (track == nullptr) {
          track = route_head_alloc();
          track_add_head(track);
        }
        track_add_wpt(track, wpt);
        break;
      }
      wpt = nullptr;
    }
  }
}

static void
stmwpp_rw_init(const QString& fname)
{
  fout = gbfopen(fname, "wb", MYNAME);
  short_h = mkshort_new_handle();
}

static void
stmwpp_rw_deinit()
{
  mkshort_del_handle(&short_h);
  gbfclose(fout);
}

static void
stmwpp_track_hdr(const route_head*)
{
  track_num++;
}

static void
stmwpp_write_double(const double val)
{
  char buff[64];

  char* c = buff + snprintf(buff, sizeof(buff), "%3.7f", val);
  while (*--c == '0') {
    *c = '\0';
  }
  if (*c == '.') {
    *++c = '0';
  }
  gbfprintf(fout, "%s,", buff);
}

static void
stmwpp_waypt_cb(const Waypoint* wpt)
{
  char cdate[16], ctime[16];

  if (track_index != track_num) {
    return;
  }

  const time_t tt = wpt->GetCreationTime().toTime_t();
  struct tm tm = *gmtime(&tt);
  tm.tm_year += 1900;
  tm.tm_mon++;

  snprintf(cdate, sizeof(cdate), "%02d/%02d/%04d", tm.tm_mon, tm.tm_mday, tm.tm_year);
  snprintf(ctime, sizeof(ctime), "%02d:%02d:%02d", tm.tm_hour, tm.tm_min, tm.tm_sec);

  QString sn;
  switch (what) {

  case STM_WAYPT:
  case STM_RTEPT:
    if (global_opts.synthesize_shortnames) {
      sn = mkshort_from_wpt(short_h, wpt);
    } else {
      sn = mkshort(short_h, wpt->shortname);
    }
    gbfprintf(fout, "WP,D,%s,", CSTRc(sn));
    break;

  case STM_TRKPT:
    gbfprintf(fout, "TP,D,");
    break;
  }
  stmwpp_write_double(wpt->latitude);
  stmwpp_write_double(wpt->longitude);
  gbfprintf(fout, "%s,%s", cdate, ctime);
  switch (what) {
  case STM_WAYPT:
  case STM_RTEPT:
    gbfprintf(fout, ".%02d", 0);
    break;
  case STM_TRKPT:
    gbfprintf(fout, ".%03d", wpt->GetCreationTime().time().msec());
    break;
  }
  gbfprintf(fout, ",\r\n");
}

static void
stmwpp_data_write()
{
  setshort_length(short_h, 100);
  setshort_badchars(short_h, ",\r\n");
  setshort_mustupper(short_h, 0);
  setshort_mustuniq(short_h, 0);
  setshort_whitespace_ok(short_h, 1);
  setshort_repeating_whitespace_ok(short_h, 1);

  track_num = 0;
  if (index_opt != nullptr) {
    track_index = atoi(index_opt);
  } else {
    track_index = 1;
  }

  gbfprintf(fout, "Datum,WGS 84,WGS 84,0,0,0,0,0\r\n");

  switch (global_opts.objective) {
  case wptdata:
  case unknown_gpsdata:
    what = STM_WAYPT;
    track_index = track_num;	/* disable filter */
    setshort_defname(short_h, "WPT");
    waypt_disp_all(stmwpp_waypt_cb);
    break;
  case rtedata:
    what = STM_RTEPT;
    setshort_defname(short_h, "RPT");
    route_disp_all(stmwpp_track_hdr, nullptr, stmwpp_waypt_cb);
    break;
  case trkdata:
    what = STM_TRKPT;
    track_disp_all(stmwpp_track_hdr, nullptr, stmwpp_waypt_cb);
    break;
  case posndata:
    fatal(MYNAME ": Realtime positioning not supported.\n");
    break;
  }
}

ff_vecs_t stmwpp_vecs = {
  ff_type_file,
  FF_CAP_RW_ALL,
  stmwpp_rd_init,
  stmwpp_rw_init,
  stmwpp_rd_deinit,
  stmwpp_rw_deinit,
  stmwpp_data_read,
  stmwpp_data_write,
  nullptr,
  stmwpp_args,
  CET_CHARSET_MS_ANSI, 0
  , NULL_POS_OPS,
  nullptr
};

#endif /* CSVFMTS_ENABLED */

