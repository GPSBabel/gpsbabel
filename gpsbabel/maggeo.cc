/*
    Magellan ".gs" files as they appear on USB of Explorist 400,500,600.

    Copyright (C) 2005, 2006, 2008 robertlipe@usa.net

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
#include "csv_util.h"
#include "xmlgeneric.h"
#include "magellan.h"

#define MYNAME "maggeo"

/* Turn this on (remove) after 5.2 becomes widespread. */
#define FIRMWARE_DOES_88591 0

static gbfile* maggeofile_in;
static gbfile* maggeofile_out;
static short_handle desc_handle = NULL;

static time_t maggeo_parsedate(char* dmy);

static void
maggeo_writemsg(const char* const buf)
{
  unsigned int osum = mag_checksum(buf);
  gbfprintf(maggeofile_out, "$%s*%02X\r\n",buf, osum);
}

static void
maggeo_rd_init(const char* fname)
{
  maggeofile_in = gbfopen(fname, "rb", MYNAME);
}

static void
maggeo_rd_deinit(void)
{
  gbfclose(maggeofile_in);
}

static void
maggeo_wr_init(const char* fname)
{
  if (waypt_count() > 200) {
    fatal(MYNAME ": eXplorist does not support more than 200 waypoints in one .gs file.\nDecrease the number of waypoints sent.\n");
  }
  maggeofile_out = gbfopen(fname, "wb", MYNAME);
  desc_handle = mkshort_new_handle();
  setshort_length(desc_handle, 20);
  setshort_badchars(desc_handle, "\"$,");
}

static void
maggeo_wr_deinit(void)
{
  maggeo_writemsg("PMGNCMD,END");
  mkshort_del_handle(&desc_handle);
  gbfclose(maggeofile_out);
}

static void
maggeo_read(void)
{
  char* buff;

  while ((buff = gbfgetstr(maggeofile_in))) {
    waypoint* wpt_tmp;
    geocache_data* gcdata;
    char* s = NULL;
    int fld;

    buff = lrtrim(buff);
    if (*buff == '\0') {
      continue;
    }
    if (strncmp(buff, "$PMGNGEO,", 9)) {
      continue;
    }

    buff += 9; /* skip field no. 1 */
    fld = 1;

    wpt_tmp = waypt_new();
    gcdata = waypt_alloc_gc_data(wpt_tmp);

    while ((s = csv_lineparse(buff, ",", "", fld++))) {
      buff = NULL;

      s = lrtrim(s);
      if (*s == '\0') {
        continue;
      }

      switch (fld) {
      case 2:
        wpt_tmp->latitude = ddmm2degrees(atof(s));
        break;
      case 3:
        if (s[0] == 'S') {
          wpt_tmp->latitude = -wpt_tmp->latitude;
        }
        break;
      case 4:
        wpt_tmp->longitude = ddmm2degrees(atof(s));
        break;
      case 5:
        if (s[0] == 'W') {
          wpt_tmp->longitude = -wpt_tmp->longitude;
        }
        break;
      case 6:
        wpt_tmp->altitude = atof(s);
        break;
      case 7:
        if (s[0] == 'F') {
          wpt_tmp->altitude = METERS_TO_FEET(wpt_tmp->altitude);
        }
        break;
      case 8:
        wpt_tmp->shortname = xstrdup(s);
        break;
      case 9:
        wpt_tmp->description = xstrdup(s);
        break;
      case 10:
        gcdata->placer = s;
        break;
      case 11:
        gcdata->hint = s;
        break;
      case 12: // cache type
        if (strcmp(s, "Mystery Cache") == 0) {
          gcdata->type = gt_suprise;
        } else {
          gcdata->type = gs_mktype(s);
        }
        break;
      case 13:
        wpt_tmp->creation_time = maggeo_parsedate(s);
        break;
      case 14: // last found date was ignored.  Implemented 2013-02-27.
        gcdata->last_found = maggeo_parsedate(s);
        break;
      case 15:
        gcdata->diff = 10 * atof(s);
        break;
      case 16:
        gcdata->terr = 10 * atof(s);
        break;
      }
    }
    waypt_add(wpt_tmp);
  }

}

/*
 * Note: returns allocated buffer that must be freed by caller.
 */
static
char*
maggeo_fmtdate(time_t t)
{
#define SZ 16

  struct tm* tm = NULL;
  int date;
  char* cbuf = (char*) xmalloc(SZ);

  cbuf[0] = '\0';
  if (t > 0) {
    tm = localtime(&t);
    if (tm) {
      date = tm->tm_mday * 100000 + (1+tm->tm_mon) * 1000 +
             tm->tm_year;
      snprintf(cbuf, SZ, "%07d", date);
    }
  }
  return cbuf;
}

/*
 * The maggeo date format s DDMMYYY where "YYY" is the number
 * of years since 1900.  This, of course, means anything in this
 * century is three digits but anything from before 2000, we'd have
 * two digit years.  This makes this easier to parse as strings.
 */
static time_t maggeo_parsedate(char* dmy)
{
  struct tm tm;
  char dd[3];
  char mm[3];

  if (strlen(dmy) < 5) {
    return 0;
  }

  memset(&tm, 0, sizeof(tm));

  dd[0] = dmy[0];
  dd[1] = dmy[1];
  dd[2] = 0;

  mm[0] = dmy[2];
  mm[1] = dmy[3];
  mm[2] = 0;

  tm.tm_mday = atoi(dd);
  tm.tm_mon = atoi(mm) - 1;
  tm.tm_year = atoi(dmy + 4);

  return mktime(&tm);
}

/*
 * Append an optional UTF string to buf, prepending a comma,
 * cleansing it of NMEA-isms and decomposing to ASCII as we go.
 */
static
void
append(char* buf, const char* str)
{
  char* cleansed1, *cleansed2;

  strcat(buf, ",");

  if (!str) {
    return;
  }

  cleansed1 = xstrdup(str);
#if FIRMWARE_DOES_88591
  /* Actually, this function needs needs refactored... */
  cleansed2 = xstrdup(cleansed1);
#else
  cleansed2 = m330_cleanse(cleansed1);
#endif

  strcat(buf, cleansed2);

  xfree(cleansed1);
  xfree(cleansed2);

}

static void
maggeo_waypt_pr(const waypoint* waypointp)
{
  char obuf[4096];
  double ilon, ilat;
  double lon, lat;
  int lon_deg, lat_deg;
  char* shortname;
  char* cname = NULL;
  const char* ctype = NULL;
  QString placer;
  char* lfounddate = NULL;
  char* placeddate = NULL;

  ilat = waypointp->latitude;
  ilon = waypointp->longitude;
  shortname = waypointp->shortname;

  lon = fabs(ilon);
  lat = fabs(ilat);

  lon_deg = lon;
  lat_deg = lat;

  lon = (lon - lon_deg) * 60.0;
  lat = (lat - lat_deg) * 60.0;

  lon = (lon_deg * 100.0 + lon);
  lat = (lat_deg * 100.0 + lat);

  /*
   * For some reason, Magellan used exactly the GPX spellings of
   * everything except this one...
   */
  if (waypointp->gc_data->type == gt_suprise) {
    ctype = "Mystery Cache";
  } else {
    ctype = gs_get_cachetype(waypointp->gc_data->type);
  }
  placeddate = maggeo_fmtdate(waypointp->creation_time);
  lfounddate = maggeo_fmtdate(waypointp->gc_data->last_found);
  cname = mkshort(desc_handle, waypointp->notes ? waypointp->notes : waypointp->description);
  placer = waypointp->gc_data->placer;

  /*
   * As of this writing on 05/04, the firmware in the units will
   * let you write fields of just about any width, but appears to
   * only use the following:
   * shortname - 8 chars
   * cname - 20 chars (scrolls in some places, not others)
   * placer - display limited by width
   * hint - 50 chars
   * cache type - appears to be parsed by f/w for icon matching.
   *
   *
   */
  snprintf(obuf, sizeof(obuf),
           "PMGNGEO,%4.3f,%c,%08.3f,%c,%04.0f,F",
           lat, ilat < 0 ? 'S' : 'N',
           lon, ilon < 0 ? 'W' : 'E',
           waypointp->altitude == unknown_alt ?
           0 : waypointp->altitude);
  append(obuf, shortname);
  append(obuf, cname);
  append(obuf, placer.toUtf8().data());
  append(obuf, waypointp->gc_data->hint.toUtf8().data());
  append(obuf, ctype);
  append(obuf, placeddate);
  append(obuf, lfounddate);

  if (waypointp->gc_data->diff/10.0)
    sprintf(obuf + strlen(obuf), ",%3.1f",
            waypointp->gc_data->diff/10.0);
  else {
    strcat(obuf, ",");
  }

  if (waypointp->gc_data->terr/10.0)
    sprintf(obuf + strlen(obuf), ",%3.1f",
            waypointp->gc_data->terr/10.0);
  else {
    strcat(obuf, ",");
  }

  if (lfounddate) {
    xfree(lfounddate);
  }
  if (placeddate) {
    xfree(placeddate);
  }
  if (cname) {
    xfree(cname);
  }

  maggeo_writemsg(obuf);
}

static void
maggeo_write(void)
{
  waypt_disp_all(maggeo_waypt_pr);
}

ff_vecs_t maggeo_vecs = {
  ff_type_file,
  { (ff_cap)(ff_cap_read | ff_cap_write), ff_cap_none, ff_cap_none },
  maggeo_rd_init,
  maggeo_wr_init,
  maggeo_rd_deinit,
  maggeo_wr_deinit,
  maggeo_read,
  maggeo_write,
  NULL,
  NULL,
#if FIRMWARE_DOES_88591
  CET_CHARSET_LATIN1, 0	/* CET-REVIEW */
#else
  CET_CHARSET_ASCII, 0	/* CET-REVIEW */
#endif
};
