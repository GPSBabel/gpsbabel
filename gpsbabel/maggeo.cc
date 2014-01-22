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

#include <QtCore/QXmlStreamAttributes>

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

static QDateTime maggeo_parsedate(char* dmy);

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

    wpt_tmp = new waypoint;
    gcdata = wpt_tmp->AllocGCData();

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
        wpt_tmp->shortname = s;
        break;
      case 9:
        wpt_tmp->description = s;
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

static
QString
maggeo_fmtdate(QDateTime dt)
{
  QDate date = dt.date();
  int y = date.year() - 1900;
  int m = date.month();
  int d = date.day();
  int r = d * 100000 + m * 1000 + y;
  return QString("%1").arg(r, 7, 10, QChar('0'));
}

/*
 * The maggeo date format s DDMMYYY where "YYY" is the number
 * of years since 1900.  This, of course, means anything in this
 * century is three digits but anything from before 2000, we'd have
 * two digit years.  This makes this easier to parse as strings.
 */
static QDateTime maggeo_parsedate(char* dmy)
{
  QString date(dmy);
  int d = date.mid(0,2).toInt();
  int m = date.mid(2,2).toInt();
  int y = date.mid(4,3).toInt();
  QDateTime r(QDate(y + 1900, m, d));
  return r;
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
  cleansed2 = xstrdup(m330_cleanse(cleansed1));
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
  const char* ctype = NULL;
  QString placer;

  ilat = waypointp->latitude;
  ilon = waypointp->longitude;

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
  QString placeddate = maggeo_fmtdate(waypointp->creation_time);
  QString lfounddate = maggeo_fmtdate(waypointp->gc_data->last_found);
  QString cname = mkshort(desc_handle,
#if NEW_STRINGS
                  waypointp->notes.isEmpty() ? waypointp->description : waypointp->notes);
#else
                  QString(waypointp->notes ? waypointp->notes : waypointp->description));
#endif
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
  append(obuf, CSTRc(waypointp->shortname));
  append(obuf, CSTR(cname));
  append(obuf, placer.toUtf8().data());
  append(obuf, waypointp->gc_data->hint.toUtf8().data());
  append(obuf, ctype);
  append(obuf, placeddate.toUtf8());
  append(obuf, lfounddate.toUtf8());

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
