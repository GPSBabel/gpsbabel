/*

    Support for SkymapII / SkymapIIIC & KMD150 ascii files

    Copyright (C) 2008 Olaf Klein, o.b.klein@gpsbabel.org

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
#include <math.h>

#define MYNAME "skyforce"


static
arglist_t skyforce_args[] = {
  ARG_TERMINATOR
};

static gbfile* fin, *fout;
static int rte_num, wpt_num;
static short_handle short_h;
static const Waypoint* prev_wpt;


static Waypoint*
skyforce_parse_coords(const char* str)
{
  Waypoint* wpt;

  if (strlen(str) < 38) {
    fatal(MYNAME ": Incomplete line!\n");
  }

  wpt = new Waypoint;

  wpt->latitude = atof(str + 21);
  if (str[20] == 'S') {
    wpt->latitude = -wpt->latitude;
  }
  wpt->latitude = ddmm2degrees(wpt->latitude);

  wpt->longitude = atof(str + 30);
  if (str[29] == 'W') {
    wpt->longitude = -wpt->longitude;
  }
  wpt->longitude = ddmm2degrees(wpt->longitude);

  return wpt;
}


static Waypoint*
skyforce_parse_wpt(const char* str, int* rte_num)
{
  Waypoint* wpt;

  wpt = skyforce_parse_coords(str);
  if (wpt == NULL) {
    return NULL;
  }

  // The line has fixed columns and starts like:
  // R 001 029 BEARHILL  N42...
  // Grab "BEARHILL" and whack trailing space.
  wpt->shortname = QString(str).mid(10,9).trimmed();

  if (rte_num) {
    *rte_num = atoi(str + 2);
  }

  return wpt;
}


static Waypoint*
skyforce_parse_trk(const char* str)
{
  char buf[15];
  int len;

  Waypoint* wpt;

  wpt = skyforce_parse_coords(str);
  if (wpt == NULL) {
    return NULL;
  }
  strncpy(buf, str + 2, sizeof(buf) - 1);
  buf[14] = 0;

  QDateTime dt = QDateTime::fromString(buf, "ddMMyy  hhmmss");
  dt.setTimeSpec(Qt::UTC);
  dt = dt.addYears(100);

  wpt->SetCreationTime(dt);
  len = strlen(str);

  if (len >= 45) {
    WAYPT_SET(wpt, speed, KNOTS_TO_MPS(atof(str + 39)));
  }
  if (len >= 59) {
    wpt->altitude = FEET_TO_METERS(atof(str + 54));
    if (str[53] == '-') {
      wpt->altitude = -wpt->altitude;
    }
  }

  return wpt;
}


static void
skyforce_head_disp_cb(const route_head* head)
{
  prev_wpt = NULL;
  if (head->rte_waypt_ct <= 0) {
    return;
  }

  wpt_num = 0;
  rte_num++;

  if (rte_num > 999) {
    if (rte_num == 1000) {
      warning(MYNAME ": Can't store more than 999 routes. Some routes skipped!\n");
    }
    return;
  }
}


static void
skyforce_waypt_disp_cb(const Waypoint* wpt)
{
  char buf[75];	/* long enough for all data types */
  double lat, lon;


  memset(buf, ' ', sizeof(buf));
  buf[sizeof(buf) - 1] = '\0';

  switch (global_opts.objective) {
  case wptdata:
    buf[0] = 'W';
    break;
  case trkdata:
    buf[0] = 'L';
    break;
  case rtedata:
    buf[0] = 'R';
    break;
  default: ; /* should never happen */
  }

  if (global_opts.objective == trkdata) {
    struct tm tm;

    const time_t tt = wpt->GetCreationTime().toTime_t();
    tm = *gmtime(&tt);
    strftime(buf + 2, sizeof(buf) - 2, "%d%m%y  %H%M%S    ", &tm);
  } else {
    if (rte_num > 999) {
      return;
    }

    wpt_num++;
    if (wpt_num > 999) {
      if (wpt_num == 1000) {
        warning(MYNAME ": Can't store more than 999 waypoints. Some waypoints skipped!\n");
      }
      return;
    }
    QString name;
    if (global_opts.synthesize_shortnames) {
      name = mkshort_from_wpt(short_h, wpt);
    } else {
      name = mkshort(short_h, wpt->shortname);
    }

    if (global_opts.objective == rtedata) {
      snprintf(buf + 2, sizeof(buf) - 2, "%03d ", rte_num);
    }
    snprintf(buf + 6, sizeof(buf) - 6, "%03d %-9s ", wpt_num, CSTR(name));
  }


  lat = degrees2ddmm(wpt->latitude);
  buf[20] = (wpt->latitude < 0) ? 'S' : 'N';
  snprintf(&buf[21], sizeof(buf) - 21, "%06.2f ", fabs(lat));

  lon = degrees2ddmm(wpt->longitude);
  buf[29] = (wpt->longitude < 0) ? 'W' : 'E';
  snprintf(&buf[30], sizeof(buf) - 30, "%08.2f ", fabs(lon));

  if (global_opts.objective == trkdata) {
    double alt, speed;

    if (wpt->altitude == unknown_alt) {
      alt = 0;
    } else {
      alt = METERS_TO_FEET(wpt->altitude);
    }
    speed = MPS_TO_KNOTS(waypt_speed(prev_wpt, wpt));

    snprintf(&buf[39], sizeof(buf) - 39, "%06.2f 000.00 %c%05d",
             speed,
             alt < 0 ? '-' : '+', si_round(fabs(alt)));
  }

  rtrim(buf);
  gbfprintf(fout, "%s\n", buf);

  prev_wpt = wpt;
}

/*******************************************************************************
* %%%        global callbacks called by gpsbabel main process              %%% *
*******************************************************************************/

static void
skyforce_rd_init(const char* fname)
{
  fin = gbfopen(fname, "r", MYNAME);
}


static void
skyforce_rd_deinit(void)
{
  gbfclose(fin);
}


static void
skyforce_read(void)
{
  char* str;
  route_head* rte, *trk;

  wpt_num = 0;
  rte = trk = NULL;
  rte_num = -1;

  while ((str = gbfgetstr(fin))) {

    Waypoint* wpt;
    int i;

    str = lrtrim(str);
    if (*str == '\0') {
      continue;
    }

    switch (*str) {

    case 'W':
      wpt = skyforce_parse_wpt(str, NULL);
      if (wpt == NULL) {
        continue;
      }
      waypt_add(wpt);
      break;

    case 'R':
      wpt = skyforce_parse_wpt(str, &i);
      if (wpt == NULL) {
        continue;
      }

      if (i != rte_num) {
        rte_num = i;
        rte = NULL;
      }

      if (rte == NULL) {
        rte = route_head_alloc();
        route_add_head(rte);
        rte->rte_num = rte_num;
      }
      route_add_wpt(rte, wpt);
      break;

    case 'L':
      wpt = skyforce_parse_trk(str);
      if (wpt == NULL) {
        continue;
      }
      if (trk == NULL) {
        trk = route_head_alloc();
        track_add_head(trk);
      }
      track_add_wpt(trk, wpt);
      break;

    default:
      fatal(MYNAME ": Invalid line marker '%c'!\n", *str);
    }
  }
}


static void
skyforce_wr_init(const char* fname)
{
  fout = gbfopen(fname, "w", MYNAME);

  short_h = mkshort_new_handle();

  setshort_length(short_h, 9);
  setshort_badchars(short_h, "\r\n\t");
  setshort_mustupper(short_h, 1);
  setshort_mustuniq(short_h, 1);
  setshort_whitespace_ok(short_h, 0);
  setshort_repeating_whitespace_ok(short_h, 0);

  wpt_num = 0;
  rte_num = 0;
}


static void
skyforce_wr_deinit(void)
{
  mkshort_del_handle(&short_h);
  gbfclose(fout);
}


static void
skyforce_write(void)
{
  switch (global_opts.objective) {	/* We can only write one data type at a time */

  case wptdata:
    setshort_defname(short_h, "WPT");
    waypt_disp_all(skyforce_waypt_disp_cb);
    break;

  case rtedata:
    setshort_defname(short_h, "RTE");
    setshort_mustuniq(short_h, 0);
    route_disp_all(skyforce_head_disp_cb, NULL, skyforce_waypt_disp_cb);
    break;

  case trkdata:
    track_disp_all(skyforce_head_disp_cb, NULL, skyforce_waypt_disp_cb);
    break;

  case posndata:
    fatal(MYNAME ": Realtime positioning not supported.\n");
    break;

  default:
    fatal(MYNAME ": Unknown data mode!\n");
  }
}


/**************************************************************************/

ff_vecs_t skyforce_vecs = {
  ff_type_file,
  FF_CAP_RW_ALL,	/* read and write waypoints, tracks and routes*/
  skyforce_rd_init,
  skyforce_wr_init,
  skyforce_rd_deinit,
  skyforce_wr_deinit,
  skyforce_read,
  skyforce_write,
  NULL,
  skyforce_args,
  CET_CHARSET_ASCII, 1
};

/**************************************************************************/
