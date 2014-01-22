/*
    Access to Garmin PCX5 files.
    Format described in: http://www.garmin.com/manuals/PCX5_OwnersManual.pdf

    Copyright (C) 2002-2006 Robert Lipe, robertlipe@usa.net

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

#include <ctype.h>
#include <math.h>

#include "defs.h"
#include "garmin_tables.h"
#include "csv_util.h"

static gbfile* file_in, *file_out;
static short_handle mkshort_handle;
static short_handle mkshort_handle2;	/* for track and route names */
static char* deficon = NULL;
static char* cartoexploreur;
static int read_as_degrees;
static int read_gpsu;
static int route_ctr;
static int comment_col = 60;  /* This has a default */
static int sym_col;
static int lat_col;
static int lon_col;

#define MYNAME "PCX"

static
arglist_t pcx_args[] = {
  {
    "deficon", &deficon, "Default icon name", "Waypoint",
    ARGTYPE_STRING, ARG_NOMINMAX
  },
  {
    "cartoexploreur", &cartoexploreur,
    "Write tracks compatible with Carto Exploreur", NULL,
    ARGTYPE_BOOL, ARG_NOMINMAX
  },
  ARG_TERMINATOR
};

static void
rd_init(const char* fname)
{
  file_in = gbfopen(fname, "rb", MYNAME);
}

static void
rd_deinit(void)
{
  gbfclose(file_in);
}

static void
wr_init(const char* fname)
{
  file_out = gbfopen(fname, "w", MYNAME);
  mkshort_handle = mkshort_new_handle();
  mkshort_handle2 = mkshort_new_handle();
}

static void
wr_deinit(void)
{
  gbfclose(file_out);
  mkshort_del_handle(&mkshort_handle);
  mkshort_del_handle(&mkshort_handle2);
}

static void
data_read(void)
{
  char name[7], desc[41];
  double lat = 0, lon = 0;
  long alt;
  int symnum;
  char date[10];
  char time[9];
  char month[4];
  Waypoint* wpt_tmp;
  char* buff;
  struct tm tm;
  route_head* track = NULL;
  route_head* route = NULL;
  int n;
  char lathemi, lonhemi;
  char tbuf[20];
  char nbuf[20];
  int points;
  int line = 0;

  read_as_degrees  = 0;
  points = 0;

  while ((buff = gbfgetstr(file_in))) {
    char* ibuf = lrtrim(buff);
    char* cp;

    if ((line++ == 0) && file_in->unicode) {
      cet_convert_init(CET_CHARSET_UTF8, 1);
    }

    switch (ibuf[0]) {
    case 'W':
      time[0] = 0;
      date[0] = 0;
      desc[0] = 0;
      alt = -9999;
      sscanf(ibuf, "W  %6c %s %s %s %s %ld",
             name, tbuf, nbuf, date, time, &alt);
      if (alt == -9999) {
        alt = unknown_alt;
      }

      if (comment_col) {
        strncpy(desc, &ibuf[comment_col], sizeof(desc)-1);
      } else {
        desc[0] = 0;
      }


      symnum = 18;
      if (sym_col) {
        symnum = atoi(&ibuf[sym_col]);
      }

      // If we have explicit columns for lat and lon,
      // copy those entire words (warning: no spaces)
      // into the respective coord buffers.
      if (lat_col) {
        sscanf(tbuf, "%s", ibuf + lat_col);
      }
      if (lon_col) {
        sscanf(nbuf, "%s", ibuf + lon_col);
      }

      name[sizeof(name)-1] = '\0';
      desc[sizeof(desc)-1] = '\0';

      wpt_tmp = new Waypoint;
      wpt_tmp->altitude = alt;
      cp = lrtrim(name);
      if (*cp != '\0') {
        wpt_tmp->shortname = cp;
      }
      cp = lrtrim(desc);
      if (*cp != '\0') {
        wpt_tmp->description = cp;
      }
      wpt_tmp->icon_descr = gt_find_desc_from_icon_number(symnum, PCX, NULL);

      if (read_as_degrees || read_gpsu) {
        human_to_dec(tbuf, &lat, &lon, 1);
        human_to_dec(nbuf, &lat, &lon, 2);

        wpt_tmp->longitude = lon;
        wpt_tmp->latitude = lat;
      } else {
        lat = atof(&tbuf[1]);
        lon = atof(&nbuf[1]);
        if (tbuf[0] == 'S') {
          lat = -lat;
        }
        if (nbuf[0] == 'W') {
          lon = -lon;
        }
        wpt_tmp->longitude = ddmm2degrees(lon);
        wpt_tmp->latitude = ddmm2degrees(lat);
      }
      if (route != NULL) {
        route_add_wpt(route, new Waypoint(*wpt_tmp));
      }
      waypt_add(wpt_tmp);
      points++;
      break;
    case 'H':
      /* Garmap2 has headers
      "H(2 spaces)LATITUDE(some spaces)LONGTITUDE(etc... followed by);track
      	everything else is
        H(2 chars)TN(tracknane\0)
      	*/
      if (points > 0) {
        track = NULL;
        points = 0;
      }
      if (track == NULL) {
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
      n = 1;
      while (ibuf[n] == ' ') {
        n++;
      }
      route = route_head_alloc();
      route->rte_name = &ibuf[n];
      route_add_head(route);
      break;
    case 'T':
      n = sscanf(ibuf, "T %lf %lf %s %s %ld",
                 &lat, &lon, date, time, &alt);

      if (n == 0) {
        /* Attempt alternate PCX format used by
         * www.radroutenplaner.nrw.de */
        n = sscanf(ibuf, "T %c%lf %c%lf %s %s %ld",
                   &lathemi, &lat, &lonhemi, &lon, date,
                   time, &alt);
        if (lathemi == 'S') {
          lat = -lat;
        }
        if (lonhemi == 'W') {
          lon = -lon;
        }
      } else if (n == 0) {
        fatal(MYNAME ":Unrecognized track line '%s'\n",
              ibuf);
      }

      memset(&tm, 0, sizeof(tm));
      tm.tm_hour = atoi(time);
      tm.tm_min = atoi(time+3);
      tm.tm_sec = atoi(time+6);
      tm.tm_mday = atoi(date);
      strncpy(month, date+3, 3);
      month[3] = 0;
      tm.tm_mon = month_lookup(month);
      tm.tm_year = atoi(date + 7);
      if (tm.tm_year < 70) {
        tm.tm_year += 100;
      }
      wpt_tmp = new Waypoint;
      wpt_tmp->SetCreationTime(mkgmtime(&tm));
      if (read_as_degrees) {
        wpt_tmp->longitude = lon;
        wpt_tmp->latitude = lat;
      } else {
        wpt_tmp->longitude = ddmm2degrees(lon);
        wpt_tmp->latitude = ddmm2degrees(lat);
      }
      wpt_tmp->altitude = alt;
      /* Did we get a track point before a track header? */
      if (track == NULL) {
        track = route_head_alloc();
        track->rte_name = "Default";
        track_add_head(track);
      }
      track_add_wpt(track, wpt_tmp);
      points++;
      break;
    case 'U':
      read_as_degrees = ! strncmp("LAT LON DEG", ibuf + 3, 11);
      if (strstr(ibuf, "UTM")) {
        fatal(MYNAME ": UTM is not supported.\n");
      }
      break;
      // GPSU is apparently PCX but with a different definition
      // of "LAT LON DM" - unlike the other, it actually IS decimal
      // minutes.
    case 'I':
      read_gpsu = !(strstr(ibuf, "GPSU") == NULL) ;
      break;
      // This is a format specifier.  Use this line to figure out
      // where our other columns start.
    case 'F': {
      int col;
      char* i = ibuf;
      sym_col = 0;

      for (col = 0, i = ibuf; *i; col++, i++) {
        if (0 == case_ignore_strncmp(i, "comment", 7)) {
          comment_col = col;
        }
        if (0 == case_ignore_strncmp(i, "symbol", 6)) {
          sym_col = col;
        }
        if (0 == case_ignore_strncmp(i, "latitude", 8)) {
          lat_col = col;
        }
        if (0 == case_ignore_strncmp(i, "longitude", 9)) {
          lon_col = col;
        }
      }
    }
    break;
    default:
      break;
      ;

    }
  }
}

static void
gpsutil_disp(const Waypoint* wpt)
{
  double lon,lat;
  int icon_token = 0;
  char tbuf[1024];
  time_t tm = wpt->GetCreationTime().toTime_t();

  lon = degrees2ddmm(wpt->longitude);
  lat = degrees2ddmm(wpt->latitude);

  if (tm == 0) {
    tm = current_time().toTime_t();
  }
  strftime(tbuf, sizeof(tbuf), "%d-%b-%y %I:%M:%S", localtime(&tm));
  strupper(tbuf);

  if (deficon) {
    icon_token = atoi(deficon);
  } else {
    icon_token = gt_find_icon_number_from_desc(wpt->icon_descr, PCX);
    if (get_cache_icon(wpt)) {
      icon_token = gt_find_icon_number_from_desc(get_cache_icon(wpt), PCX);
    }
  }


  gbfprintf(file_out, "W  %-6.6s %c%08.5f %c%011.5f %s %5.f %-40.40s %5e  %d\n",
            global_opts.synthesize_shortnames ?
            CSTRc(mkshort_from_wpt(mkshort_handle, wpt)) :
            CSTRc(wpt->shortname),
            lat < 0.0 ? 'S' : 'N',
            fabs(lat),
            lon < 0.0 ? 'W' : 'E',
            fabs(lon),
            tbuf,
            (wpt->altitude == unknown_alt) ? -9999 : wpt->altitude,
            (wpt->description != NULL) ? CSTRc(wpt->description) : "",
            0.0,
            icon_token);
}

static void
pcx_track_hdr(const route_head* trk)
{
  char buff[20];

  route_ctr++;
  snprintf(buff, sizeof(buff)-1, "Trk%03d", route_ctr);
// NEW_STRINGS - remove extr ctor below.
  QString name = mkshort(mkshort_handle2, (trk->rte_name != NULL) ? QString(trk->rte_name) : buff);
  /* Carto Exploreur (popular in France) chokes on trackname headers,
   * so provide option to supppress these.
   */
  if (!cartoexploreur) {
    gbfprintf(file_out, "\n\nH  TN %s\n", CSTR(name));
  }
  gbfprintf(file_out, "H  LATITUDE    LONGITUDE    DATE      TIME     ALT  ;track\n");
}

static void
pcx_route_hdr(const route_head* rte)
{
  char buff[20];

  route_ctr++;
  snprintf(buff, sizeof(buff)-1, "Rte%03d", route_ctr);

  QString name = mkshort(mkshort_handle2, (rte->rte_name != NULL) ? rte->rte_name : buff);

  /* see pcx_track_hdr */
  if (!cartoexploreur) {
    gbfprintf(file_out, "\n\nR  %s\n", CSTR(name));
  }
  gbfprintf(file_out, "\n"
            "H  IDNT   LATITUDE    LONGITUDE    DATE      TIME     ALT   DESCRIPTION                              PROXIMITY     SYMBOL ;waypts\n");
}

void
pcx_track_disp(const Waypoint* wpt)
{
  double lon,lat;
  char tbuf[100];
  struct tm* tm;
  char* tp;

  lon = degrees2ddmm(wpt->longitude);
  lat = degrees2ddmm(wpt->latitude);

  const time_t ct = wpt->GetCreationTime().toTime_t();
  tm = gmtime(&ct);

  strftime(tbuf, sizeof(tbuf), "%d-%b-%y %H:%M:%S", tm);	/* currently ...%T does nothing under Windows */
  for (tp = tbuf; *tp; tp++) {
    *tp = toupper(*tp);
  }
  gbfprintf(file_out, "T  %c%08.5f %c%011.5f %s %.f\n",
            lat < 0.0 ? 'S' : 'N',
            fabs(lat),
            lon < 0.0 ? 'W' : 'E',
            fabs(lon),
            tbuf, wpt->altitude);
}


static void
data_write(void)
{
  gbfprintf(file_out,
            "H  SOFTWARE NAME & VERSION\n"
            "I  PCX5 2.09\n"
            "\n"
            "H  R DATUM                IDX DA            DF            DX            DY            DZ\n"
            "M  G WGS 84               121 +0.000000e+00 +0.000000e+00 +0.000000e+00 +0.000000e+00 +0.000000e+00\n"
            "\n"
            "H  COORDINATE SYSTEM\n"
            "U  LAT LON DM\n");

  setshort_length(mkshort_handle, 6);

  setshort_length(mkshort_handle2, 20);	/* for track and route names */
  setshort_whitespace_ok(mkshort_handle2, 0);
  setshort_mustuniq(mkshort_handle2, 0);

  if (global_opts.objective == wptdata) {
    gbfprintf(file_out,
              "\n"
              "H  IDNT   LATITUDE    LONGITUDE    DATE      TIME     ALT   DESCRIPTION                              PROXIMITY     SYMBOL ;waypts\n");

    waypt_disp_all(gpsutil_disp);
  } else if (global_opts.objective == trkdata) {
    route_ctr = 0;
    track_disp_all(pcx_track_hdr, NULL, pcx_track_disp);
  } else if (global_opts.objective == rtedata) {
    route_ctr = 0;
    route_disp_all(pcx_route_hdr, NULL, gpsutil_disp);
  }
}


ff_vecs_t pcx_vecs = {
  ff_type_file,
  FF_CAP_RW_ALL,
  rd_init,
  wr_init,
  rd_deinit,
  wr_deinit,
  data_read,
  data_write,
  NULL,
  pcx_args,
  CET_CHARSET_ASCII, 1	/* CET-REVIEW */
};
