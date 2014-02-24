/*
    National Geographic Topo! TPG file support (Waypoints/Routes)
    Contributed to gpsbabel by Alex Mottram

    For Topo! version 2.x.  Routes are currently not implemented.

    Copyright (C) 2002 Alex Mottram, geo_alexm at cox-internet.com

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
#include <string.h>
#include <ctype.h>
#include "jeeps/gpsmath.h" /* for datum conversions */

#define MYNAME	"TPG"

#define MAXTPGSTRINGSIZE	256
#define MAXTPGOUTPUTPINS	65535

static gbfile* tpg_file_in;
static gbfile* tpg_file_out;
static short_handle mkshort_handle;
static char* tpg_datum_opt;
static int tpg_datum_idx;

static unsigned int waypt_out_count;

static
arglist_t tpg_args[] = {
  {"datum", &tpg_datum_opt, "Datum (default=NAD27)", "N. America 1927 mean", ARGTYPE_STRING, ARG_NOMINMAX },
  ARG_TERMINATOR
};

static int
valid_tpg_header(char* header, int len)
{
  unsigned char header_bytes[] = { 0xFF, 0xFF, 0x01, 0x00, 0x0D,
                                   0x00, 0x43, 0x54, 0x6F, 0x70,
                                   0x6F, 0x57, 0x61, 0x79, 0x70,
                                   0x6F, 0x69, 0x6E, 0x74
                                 };
  if (len != 19) {
    return (-1);
  }

  return memcmp(header_bytes, header, len);
}

static void
tpg_common_init(void)
{
  tpg_datum_idx = GPS_Lookup_Datum_Index(tpg_datum_opt);
  if (tpg_datum_idx < 0) {
    fatal(MYNAME ": Datum '%s' is not recognized.\n", tpg_datum_opt);
  }
}

static void
tpg_rd_init(const char* fname)
{
  tpg_common_init();
  tpg_file_in = gbfopen_le(fname, "rb", MYNAME);
}

static void
tpg_rd_deinit(void)
{
  gbfclose(tpg_file_in);
}

static void
tpg_wr_init(const char* fname)
{
  tpg_common_init();
  tpg_file_out = gbfopen_le(fname, "wb", MYNAME);
  mkshort_handle = mkshort_new_handle();
  waypt_out_count = 0;
}

static void
tpg_wr_deinit(void)
{
  mkshort_del_handle(&mkshort_handle);
  gbfclose(tpg_file_out);
}

static void
tpg_read(void)
{
  char buff[MAXTPGSTRINGSIZE + 1];
  Waypoint* wpt_tmp;
  double lat, lon, elev;
  double amt;
  short int pointcount;

  pointcount = gbfgetint16(tpg_file_in);

  /* the rest of the header */
  gbfread(&buff[0], 19, 1, tpg_file_in);

  if (valid_tpg_header(buff, 19) != 0) {
    fatal(MYNAME ": input file does not appear to be a valid .TPG file.\n");
  }


  while (pointcount--) {
    wpt_tmp = new Waypoint;

    /* pascal-like shortname */
    wpt_tmp->shortname = gbfgetpstr(tpg_file_in);

    /* for some very odd reason, signs on longitude are swapped */
    /* coordinates are in NAD27/CONUS datum                     */

    /* 8 bytes - longitude, sign swapped  */
    lon = gbfgetdbl(tpg_file_in);

    /* 8 bytes - latitude */
    lat = gbfgetdbl(tpg_file_in);

    /* swap sign before we do datum conversions */
    lon *= -1.0;

    /* 2 bytes - elevation in feet */
    elev = FEET_TO_METERS(gbfgetint16(tpg_file_in));

    /* convert incoming NAD27/CONUS coordinates to WGS84 */
    GPS_Math_Known_Datum_To_WGS84_M(
      lat,
      lon,
      0.0,
      &wpt_tmp->latitude,
      &wpt_tmp->longitude,
      &amt,
      tpg_datum_idx);

    wpt_tmp->altitude = elev;


    /* 4 bytes? */
    (void) gbfgetint32(tpg_file_in);

    /* pascal-like description */
    wpt_tmp->description = gbfgetpstr(tpg_file_in);

    /* 2 bytes */
    (void) gbfgetint16(tpg_file_in);

    waypt_add(wpt_tmp);
  }
}

static void
tpg_waypt_pr(const Waypoint* wpt)
{
  double lon, lat;
  double amt;
  short int elev;
  char tbuf[64];
  char c,ocount;
  QString shortname;
  QString description;
  int i;

  /* these unknown 4 are probably point properties (color, icon, etc..) */
  unsigned char unknown4[] = { 0x78, 0x56, 0x34, 0x12 };

  /* these 2 appear to be constant across test files */
  unsigned char unknown2[] = { 0x01, 0x80 };

  /* our personal waypoint counter */
  waypt_out_count++;

  /* this output format pretty much requires a description
   * and a shortname
   */

  if ((wpt->shortname.isEmpty()) || (global_opts.synthesize_shortnames)) {
    if (!wpt->description.isEmpty()) {
      if (global_opts.synthesize_shortnames) {
        shortname = mkshort_from_wpt(mkshort_handle, wpt);
      } else {
        shortname = xstrdup(wpt->description);
      }
    } else {
      /* no description available */
      shortname = xstrdup("");
    }
  } else {
    shortname = wpt->shortname;
  }
  if (wpt->description.isEmpty()) {
    if (!shortname.isEmpty()) {
      description = shortname;
    } else {
      description = "";
    }
  } else {
    description = wpt->description;
  }

  /* convert lat/long to NAD27/CONUS datum */
  GPS_Math_WGS84_To_Known_Datum_M(
    wpt->latitude,
    wpt->longitude,
    0.0,
    &lat,
    &lon,
    &amt,
    tpg_datum_idx);


  /* swap the sign back *after* the datum conversion */
  lon *= -1.0;

  /* convert meters back to feets */
  elev = (short int) METERS_TO_FEET(wpt->altitude);

  /* 1 bytes stringsize for shortname */
  c = shortname.length();
  ocount = 0;
  /*
   * It's reported the only legal characters are upper case
   * A-Z and 0-9.  Wow.   We have to make two passes: one to
   * count and one to output.
   */
  for (i = 0; i < c; i++) {
    char oc = shortname[i].toUpper().cell();
    if (isalnum(oc) || oc == ' ') {
      ocount++;
    }
  }

  gbfwrite(&ocount, 1, 1, tpg_file_out);

  for (i = 0; i < c; i++) {
    char oc = shortname[i].toUpper().cell();
    if (isalnum(oc) || oc == ' ') {
      gbfputc(oc, tpg_file_out);
    }
  }

  /* 8 bytes - longitude */
  gbfputdbl(lon, tpg_file_out);

  /* 8 bytes - latitude */
  gbfputdbl(lat, tpg_file_out);

  /* 2 bytes - elevation_feet */
  gbfputint16(elev, tpg_file_out);

  /* 4 unknown bytes */
  memset(tbuf, '\0', sizeof(tbuf));
  gbfwrite(unknown4, 1, 4, tpg_file_out);

  /* pascal-like description */
  gbfputpstr(description, tpg_file_out);

  /* and finally 2 unknown bytes */

  if (waypt_out_count == waypt_count()) {
    /* last point gets 0x0000 instead of 0x0180 */
    gbfputint16(0, tpg_file_out);
  } else {
    gbfwrite(unknown2, 1, 2, tpg_file_out);
  }
}

static void
tpg_write(void)
{
  int s;
  unsigned char header_bytes[] = { 0xFF, 0xFF, 0x01, 0x00, 0x0D,
                                   0x00, 0x43, 0x54, 0x6F, 0x70,
                                   0x6F, 0x57, 0x61, 0x79, 0x70,
                                   0x6F, 0x69, 0x6E, 0x74
                                 };

  s = waypt_count();

  if (global_opts.synthesize_shortnames) {
    setshort_length(mkshort_handle, 32);
    setshort_whitespace_ok(mkshort_handle, 1);
    setshort_mustupper(mkshort_handle, 1);
  }

  if (s > MAXTPGOUTPUTPINS) {
    fatal(MYNAME ": attempt to output too many points (%d).  The max is %d.  Sorry.\n", s, MAXTPGOUTPUTPINS);
  }

  /* write the waypoint count */
  gbfputint16(s, tpg_file_out);

  /* write the rest of the header */
  gbfwrite(header_bytes, 1, 19, tpg_file_out);

  waypt_disp_all(tpg_waypt_pr);
}

ff_vecs_t tpg_vecs = {
  ff_type_file,
  FF_CAP_RW_WPT,
  tpg_rd_init,
  tpg_wr_init,
  tpg_rd_deinit,
  tpg_wr_deinit,
  tpg_read,
  tpg_write,
  NULL,
  tpg_args,
  CET_CHARSET_ASCII, 0	/* CET-REVIEW */
};
