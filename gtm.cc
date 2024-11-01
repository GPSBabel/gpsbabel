/*
    Support for GPS TrackMaker data file.

    Copyright (C) 2005  Gustavo Niemeyer <gustavo@niemeyer.net>.

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
*/

/*
 * Documentation can be found at
 * https://www.trackmaker.com/download/ref_guide_eng.pdf
 * https://www.trackmaker.com/download/GTM211_format.pdf
 */

#include "gtm.h"

#include <cstdio>               // for SEEK_CUR
#include <cstring>              // for strlen, memset

#include <QList>                // for QList
#include <QString>              // for QString

#include "defs.h"
#include "gbfile.h"             // for gbfseek, gbfputc, gbfputint32, gbfputflt, gbfputint16, gbfputuint16, gbfgetint32, gbfgetdbl, gbfputdbl, gbfgetint16, gbfwrite, gbfgetflt, gbfile, gbfclose, gbfgetc, gbfopen_le, gbfreadbuf
#include "jeeps/gpsmath.h"      // for GPS_Math_Known_Datum_To_WGS84_M
#include "src/core/datetime.h"  // for DateTime


#define EPOCH89DIFF 631065600
/* was 631076400 but that seems to include a three-hour bias */
#define WAYPOINTSTYLES \
	"\xf5\xff\xff\xff\x0f\x00Times New Roman\x00\x00\x00\x00\x00\x90\x01"\
	"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"\
	"\xf5\xff\xff\xff\x0f\x00Times New Roman\x01\x00\x00\x00\x00\x90\x01"\
	"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"\
	"\xf5\xff\xff\xff\x0f\x00Times New Roman\x02\x00\x00\x00\x00\x90\x01"\
	"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"\
	"\xf5\xff\xff\xff\x0f\x00Times New Roman\x03\x00\x00\x00\x00\x90\x01"\
	"\x00\x00\x00\x00\x00\x00\x8b\xff\xff\xff\xff\x00\x00\x00\x00\x00\x01"

#define unknown_alt_gtm -10000000

/* Read functions, according to specification. */

#define fread_discard(a,b) gbfseek(a, (b), SEEK_CUR)
#define fread_byte(a) (unsigned char) gbfgetc(a)

#if 0
/* not used */
short int
GtmFormat::fread_bool(gbfile* fd)
{
  char buf[2];
  gbfread(buf, 2, 1, fd);
  return le_read16(buf) ? 1 : 0;
}
#endif

#define fread_integer(a) gbfgetint16(a)
#define fread_long(a) gbfgetint32(a)
#define fread_single(a) gbfgetflt(a)
#define fread_double(a) gbfgetdbl(a)

QString
GtmFormat::fread_string(gbfile* fd)
{
  int len = fread_integer(fd);
  return gbfreadbuf(len, fd);
}

void
GtmFormat::fread_string_discard(gbfile* fd)
{
  fread_string(fd);
}

QString
GtmFormat::fread_fixedstring(gbfile* fd, int len)
{
  return gbfreadbuf(len, fd);
}

/* Write functions, according to specification. */

void
GtmFormat::fwrite_null(gbfile* fd, int len)
{
  char buf[1024];

  memset(buf, 0, len);
  gbfwrite(buf, 1, len, fd);
}

#define fwrite_byte(a,b) gbfputc((signed char)(b), a)
#define fwrite_bool(a,b) gbfputuint16((b) ? 0xffff : 0, a)
#define fwrite_integer(a,b) gbfputint16((b), a)
#define fwrite_long(a,b) gbfputint32((b), a)
#define fwrite_single(a,b) gbfputflt((b), a)
#define fwrite_double(a,b) gbfputdbl((b), a)

void
GtmFormat::fwrite_string(gbfile* fd, const char* str)
{
  if (str && str[0]) {
    int len = strlen(str);
    fwrite_integer(fd, len);
    gbfwrite(str, 1, len, fd);
  } else {
    fwrite_integer(fd, 0);
  }
}

void
GtmFormat::fwrite_string(gbfile* fd, const QString& str)
{
  if (str.isEmpty()) {
    fwrite_integer(fd, 0);
  } else {
    fwrite_integer(fd, str.length());
    gbfwrite(CSTRc(str), 1, str.length(), fd);
  }
}

void
GtmFormat::fwrite_fixedstring(gbfile* fd, const char* str, int fieldlen)
{
  int len = str ? strlen(str) : 0;

  if (len > fieldlen) {
    len = fieldlen;
  }
  if (str) {
    gbfwrite(str, 1, len, fd);
  }
  for (; len != fieldlen; len++) {
    gbfputc(' ', fd);
  }
}

void
GtmFormat::fwrite_fixedstring(gbfile* fd, const QString& str, int fieldlen)
{
  fwrite_fixedstring(fd, CSTR(str), fieldlen);
}

/* Auxiliary functions */

void GtmFormat::set_datum(int n)
{
  indatum = -1;
  if (n > 0 && n < MAX_INDATUM_INDEX) {
    indatum = indatum_array[n];
  }

  if (indatum == -1) {
    warning("Unsupported datum (%d), won't convert to WGS84\n", n);
  }
}

void GtmFormat::convert_datum(double* lat, double* lon) const
{
  double amt;
  if (indatum != -1 && indatum != 118) {
    GPS_Math_Known_Datum_To_WGS84_M(*lat, *lon, 0.0,
                                    lat, lon, &amt, indatum);
  }
}

/* Callbacks */

void
GtmFormat::rd_init(const QString& fname)
{
  file_in = gbfopen_le(fname, "rb");
  int version = fread_integer(file_in);
  QString name = fread_fixedstring(file_in, 10);
  if (version == -29921) {
    fatal("Uncompress the file first\n");
  }
  if (name != "TrackMaker") {
    fatal("Invalid file format\n");
  }
  if (version != 211) {
    fatal("Invalid format version\n");
  }

  /* Header */
  fread_discard(file_in, 15);
  ws_count = fread_long(file_in);
  fread_discard(file_in, 4);
  wp_count = fread_long(file_in);
  tr_count = fread_long(file_in);
  rt_count = fread_long(file_in);
  fread_discard(file_in, 16);
  im_count = fread_long(file_in);
  ts_count = fread_long(file_in);
  fread_discard(file_in, 28);
  fread_string_discard(file_in);
  fread_string_discard(file_in);
  fread_string_discard(file_in);
  fread_string_discard(file_in);

  /* User Grid and Datum */
  fread_discard(file_in, 34);
  set_datum(fread_integer(file_in));
  fread_discard(file_in, 22);
}

void
GtmFormat::rd_deinit()
{
  gbfclose(file_in);
}

void GtmFormat::count_track_styles(const route_head* rte)
{
  if (!rte->rte_waypt_empty()) {
    ts_count++;
  }
}

void
GtmFormat::wr_init(const QString& fname)
{
  // Count the number of track style entires.
  // We don't output a track style for any track that doesn't have any
  // waypoints.
  // Note that it is impossible to store a track without any waypoints
  // in this format as every tracklog entry represents a waypoint,
  // and a new track is defined by a tracklog entry with the tracklog flag set.
  ts_count = 0;
  auto count_track_styles_lambda = [this](const route_head* rte)->void {
    count_track_styles(rte);
  };
  track_disp_all(count_track_styles_lambda, nullptr, nullptr);

  file_out = gbfopen_le(fname, "wb");	/* little endian */

  /* Header */
  fwrite_integer(file_out, 211);
  fwrite_fixedstring(file_out, "TrackMaker", 10);
  fwrite_byte(file_out, 0);
  fwrite_byte(file_out, 0);
  fwrite_byte(file_out, 8);
  fwrite_byte(file_out, 0);
  fwrite_byte(file_out, 0);
  fwrite_byte(file_out, 0);
  fwrite_byte(file_out, 0);
  fwrite_long(file_out, 0);
  fwrite_long(file_out, 16777215);
  fwrite_long(file_out, waypt_count() ? 4 : 0); /* num waypoint styles */
  fwrite_long(file_out, 0);
  fwrite_long(file_out, waypt_count()); /* num waypoints */
  fwrite_long(file_out, track_waypt_count());
  fwrite_long(file_out, route_waypt_count());
  fwrite_single(file_out, 0); /* maxlon */
  fwrite_single(file_out, 0); /* minlon */
  fwrite_single(file_out, 0); /* maxlat */
  fwrite_single(file_out, 0); /* minlat */
  fwrite_long(file_out, 0);
  fwrite_long(file_out, ts_count); /* num tracklog styles */
  fwrite_single(file_out, 0);
  fwrite_single(file_out, 0);
  fwrite_bool(file_out, 0);
  fwrite_bool(file_out, 0);
  fwrite_bool(file_out, 0);
  fwrite_bool(file_out, 0);
  fwrite_bool(file_out, 0);
  fwrite_bool(file_out, 0);
  fwrite_bool(file_out, 0);
  fwrite_bool(file_out, 0);
  fwrite_bool(file_out, 0);
  fwrite_bool(file_out, 0);
  fwrite_string(file_out, "Times New Roman");
  fwrite_string(file_out, "");
  fwrite_string(file_out, "");
  fwrite_string(file_out, "");

  /* User Grid and Datum */
  fwrite_null(file_out, 34);
  fwrite_integer(file_out, 217); /* WGS84 */
  fwrite_null(file_out, 22);
}

void
GtmFormat::wr_deinit()
{
  gbfclose(file_out);
}

void
GtmFormat::read()
{
  route_head* trk_head = nullptr;
  route_head* rte_head = nullptr;
  Waypoint* wpt;
  QList<route_head*> real_track_list;
  unsigned int icon;
  int i;

  /* Image information */
  for (i = 0; i != im_count; i++) {
    fread_string_discard(file_in);
    fread_string_discard(file_in);
    fread_discard(file_in, 30);
  }

  /* Waypoints */
  for (i = 0; i != wp_count; i++) {
    wpt = new Waypoint;
    wpt->latitude = fread_double(file_in);
    wpt->longitude = fread_double(file_in);
    convert_datum(&wpt->latitude, &wpt->longitude);
    wpt->shortname = fread_fixedstring(file_in, 10);
    wpt->description = fread_string(file_in);
    icon = fread_integer(file_in);
    if (icon < sizeof(icon_descr)/sizeof(char*)) {
      wpt->icon_descr = icon_descr[icon];
    }
    fread_discard(file_in, 1);
    wpt->SetCreationTime(fread_long(file_in));
    if (wpt->creation_time.isValid()) {
      wpt->creation_time = wpt->creation_time.addSecs(EPOCH89DIFF);
    }
    fread_discard(file_in, 2);
    wpt->altitude = fread_single(file_in);
    if (wpt->altitude == unknown_alt_gtm) {
      wpt->altitude = unknown_alt;
    }
    fread_discard(file_in, 2);
    waypt_add(wpt);
  }

  /* Waypoint Styles */
  if (wp_count) {
    for (i = 0; i != ws_count; i++) {
      fread_discard(file_in, 4);
      fread_string_discard(file_in);
      fread_discard(file_in, 24);
    }
  }

  /* Tracklogs */
  for (i = 0; i != tr_count; i++) {
    wpt = new Waypoint;
    wpt->latitude = fread_double(file_in);
    wpt->longitude = fread_double(file_in);
    convert_datum(&wpt->latitude, &wpt->longitude);
    wpt->SetCreationTime(fread_long(file_in));
    if (wpt->creation_time.isValid()) {
      wpt->creation_time = wpt->creation_time.addSecs(EPOCH89DIFF);
    }
    start_new = fread_byte(file_in);
    wpt->altitude = fread_single(file_in);
    if (wpt->altitude == unknown_alt_gtm) {
      wpt->altitude = unknown_alt;
    }
    if (start_new || !trk_head) {
      trk_head = new route_head;
      track_add_head(trk_head);
      real_track_list.append(trk_head);
    }
    track_add_wpt(trk_head, wpt);
  }

  /* Tracklog styles */
  // TODO: The format document states there are ts_count tracklog style entries,
  //       and tr_count tracklog entries.
  //       Some tracklog entries may be continuation entries, so we turn these
  //       into real_track_list.size() <= tr_count tracks.
  //       If ts_count != real_track_list.size() we don't know how to line up
  //       the tracklogs, and the real tracks, with the tracklog styles.
  if (ts_count != real_track_list.size()) {
    warning("The number of tracklog entries with the new flag "
           "set doesn't match the number of tracklog style entries.\n"
           "  This is unexpected and may indicate a malformed input file.\n"
           "  As a result the track names may be incorrect.\n");
  }
  // Read the entire tracklog styles section whether we use it or not.
  for (i = 0; i != ts_count; i++) {
    QString tname = fread_string(file_in);
    fread_discard(file_in, 12);
    if (i < real_track_list.size()) {
      trk_head = real_track_list.at(i);
      trk_head->rte_name = tname;
    }
  }

  /* Routes */
  for (i = 0; i != rt_count; i++) {
    wpt = new Waypoint;
    wpt->latitude = fread_double(file_in);
    wpt->longitude = fread_double(file_in);
    convert_datum(&wpt->latitude, &wpt->longitude);
    wpt->shortname = fread_fixedstring(file_in, 10);
    wpt->description = fread_string(file_in);
    QString route_name = fread_string(file_in);
    icon = fread_integer(file_in);
    if (icon < sizeof(icon_descr)/sizeof(char*)) {
      wpt->icon_descr = icon_descr[icon];
    }
    fread_discard(file_in, 1);
    start_new = fread_byte(file_in);
    fread_discard(file_in, 6);
    wpt->altitude = fread_single(file_in);
    if (wpt->altitude == unknown_alt_gtm) {
      wpt->altitude = unknown_alt;
    }
    fread_discard(file_in, 2);

    if (start_new || !rte_head) {
      rte_head = new route_head;
      rte_head->rte_name = route_name;
      route_add_head(rte_head);
    }
    route_add_wpt(rte_head, wpt);
  }
}

int GtmFormat::icon_from_descr(const QString& descr)
{
  for (int i = 0; icon_descr[i]; i++) {
    if (descr.compare(icon_descr[i]) == 0) {
      return i;
    }
  }
  return 48;
}

void GtmFormat::write_waypt(const Waypoint* wpt)
{
  fwrite_double(file_out, wpt->latitude);
  fwrite_double(file_out, wpt->longitude);
  fwrite_fixedstring(file_out, wpt->shortname, 10);
  fwrite_string(file_out, wpt->description);
  fwrite_integer(file_out, icon_from_descr(wpt->icon_descr));
  fwrite_byte(file_out, 3);
  if (wpt->creation_time.isValid()) {
    fwrite_long(file_out, wpt->GetCreationTime().toTime_t()-EPOCH89DIFF);
  } else {
    fwrite_long(file_out, 0);
  }
  fwrite_integer(file_out, 0);
  if (wpt->altitude == unknown_alt) {
    fwrite_single(file_out, unknown_alt_gtm);
  } else {
    fwrite_single(file_out, wpt->altitude);
  }
  fwrite_integer(file_out, 0);
}

void GtmFormat::start_rte(const route_head* rte)
{
  rte_active = rte;
  start_new = 1;
}

void GtmFormat::write_trk_waypt(const Waypoint* wpt)
{
  fwrite_double(file_out, wpt->latitude);
  fwrite_double(file_out, wpt->longitude);
  fwrite_long(file_out, wpt->GetCreationTime().toTime_t()-EPOCH89DIFF);
  fwrite_byte(file_out, start_new);
  if (wpt->altitude == unknown_alt) {
    fwrite_single(file_out, unknown_alt_gtm);
  } else {
    fwrite_single(file_out, wpt->altitude);
  }
  start_new = 0;
}

void GtmFormat::write_trk_style(const route_head* trk)
{
  if (!trk->rte_waypt_empty()) {
    fwrite_string(file_out, trk->rte_name);
    fwrite_byte(file_out, 1);
    fwrite_long(file_out, 0);
    fwrite_single(file_out, 0);
    fwrite_byte(file_out, 0);
    fwrite_integer(file_out, 0);
  }
}

void GtmFormat::write_rte_waypt(const Waypoint* wpt)
{
  fwrite_double(file_out, wpt->latitude);
  fwrite_double(file_out, wpt->longitude);
  fwrite_fixedstring(file_out, wpt->shortname, 10);
  fwrite_string(file_out, wpt->description);
  fwrite_string(file_out, rte_active->rte_name);
  fwrite_integer(file_out, icon_from_descr(wpt->icon_descr));
  fwrite_byte(file_out, 3);
  fwrite_byte(file_out, start_new);
  fwrite_long(file_out, 0);
  fwrite_integer(file_out, 0);
  if (wpt->altitude == unknown_alt) {
    fwrite_single(file_out, unknown_alt_gtm);
  } else {
    fwrite_single(file_out, wpt->altitude);
  }
  fwrite_integer(file_out, 0);
  start_new = 0;
}

void
GtmFormat::write()
{
  auto write_waypt_lambda = [this](const Waypoint* waypointp)->void {
    write_waypt(waypointp);
  };
  waypt_disp_all(write_waypt_lambda);

  if (waypt_count()) {
    gbfwrite(WAYPOINTSTYLES, 1, sizeof(WAYPOINTSTYLES)-1, file_out);
  }

  auto start_rte_lambda = [this](const route_head* rte)->void {
    start_rte(rte);
  };
  auto write_trk_style_lambda = [this](const route_head* rte)->void {
    write_trk_style(rte);
  };
  auto write_trk_waypt_lambda = [this](const Waypoint* waypointp)->void {
    write_trk_waypt(waypointp);
  };
  auto write_rte_waypt_lambda = [this](const Waypoint* waypointp)->void {
    write_rte_waypt(waypointp);
  };
  track_disp_all(start_rte_lambda, nullptr, write_trk_waypt_lambda);
  track_disp_all(write_trk_style_lambda, nullptr, nullptr);
  route_disp_all(start_rte_lambda, nullptr, write_rte_waypt_lambda);
}
