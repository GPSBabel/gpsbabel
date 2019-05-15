/*

    Support for Destinator POI's, Itineraries and Tracklogs.
    ( as described at "http://mozoft.com/d3log.html" )

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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

 */

#include "defs.h"
#include "cet.h"
#include "cet_util.h"
#include "garmin_fs.h"
#include "strptime.h"
#include <cmath>

#define MYNAME 		"destinator"
#define DST_DYN_POI 	"Dynamic POI"
#define DST_ITINERARY 	"City->Street"

static
arglist_t destinator_args[] = {
  ARG_TERMINATOR
};

static gbfile* fin, *fout;
static gpsdata_type data_type;


/*******************************************************************************/
/*                                   READER                                    */
/*-----------------------------------------------------------------------------*/

static garmin_fs_t*
gmsd_init(Waypoint* wpt)
{
  garmin_fs_t* gmsd = GMSD_FIND(wpt);
  if (gmsd == nullptr) {
    gmsd = garmin_fs_alloc(-1);
    fs_chain_add(&wpt->fs, (format_specific_data*) gmsd);
  }
  return gmsd;
}

static QString
read_wcstr(const int discard)
{
  int16_t* buff = nullptr, c;
  int size = 0, pos = 0;

  while (gbfread(&c, sizeof(c), 1, fin) && (c != 0)) {
    if (size == 0) {
      size = 16;
      buff = (int16_t*) xmalloc(size * sizeof(*buff));
    } else if (pos == size) {
      size += 16;
      buff = (int16_t*) xrealloc(buff, size * sizeof(*buff));
    }
    buff[pos] = c;
    pos += 1;
  }

  if (pos != 0) {
    char* res;
    if (discard) {
      res = nullptr;
    } else {
      res = cet_str_uni_to_utf8(buff, pos);
      res = lrtrim(res);
      if (*res == '\0') {
        xfree(res);
        res = nullptr;
      }
    }
    xfree(buff);
    QString rv = QString::fromUtf8(res);
    xfree(res);
    return rv;
    //return res;
  } else {
    return nullptr;
  }
}

static void
write_wcstr(const QString& str)
{
  int len;

  short* unicode = cet_str_utf8_to_uni(CSTR(str), &len);
  gbfwrite((void*)unicode, 2, len + 1, fout);
  xfree(unicode);
}

static int
read_until_wcstr(const char* str)
{
  int eos = 0, res = 0;

  int len = strlen(str);
  int sz = (len + 1) * 2;
  char* buff = (char*) xcalloc(sz, 1);

  while (! gbfeof(fin)) {

    char c = gbfgetc(fin);
    memmove(buff, buff + 1, sz - 1);
    buff[sz - 1] = c;

    if (c == 0) {
      eos++;
      if (eos >= 2) {	/* two or more zero bytes => end of string */
        char* test = cet_str_uni_to_utf8((short*)buff, len);
        if (test) {
          res = (strcmp(str, test) == 0);
          xfree(test);
          if (res) {
            break;
          }
        }
      }
    } else {
      eos = 0;
    }
  }
  xfree(buff);
  return res;
}

static void
destinator_read_poi()
{
  int count = 0;

  gbfrewind(fin);

  while (!(gbfeof(fin))) {
    QString str;
    garmin_fs_t* gmsd;

    if (count == 0) {
      str = read_wcstr(0);
      if ((str != DST_DYN_POI)) {
        fatal(MYNAME "_poi: Invalid record header!\n");
      }
    } else if (! read_until_wcstr(DST_DYN_POI)) {
      break;
    }

    count++;

    Waypoint* wpt = new Waypoint;

    wpt->shortname = read_wcstr(0);
    wpt->notes = read_wcstr(0);		/* comment */

    QString hnum = read_wcstr(0);			/* house number */

    str = read_wcstr(0); 			/* street */
    if (str.isEmpty()) {
      str = hnum;
      hnum = QString();
    }
    if (!str.isEmpty()) {
      gmsd = gmsd_init(wpt);
      if (!hnum.isEmpty()) {
        str += " ";
        str += hnum;
      }
      GMSD_SETSTRQ(addr, str);
    }

    if ((str = read_wcstr(0), !str.isEmpty())) {		/* city */
      gmsd = gmsd_init(wpt);
      GMSD_SETSTRQ(city, str);
    }

    (void) read_wcstr(1);			/* unknown */

    if ((str = read_wcstr(0), !str.isEmpty())) {		/* postcode */
      gmsd = gmsd_init(wpt);
      GMSD_SETSTRQ(postal_code, str);
    }

    (void) read_wcstr(1);			/* unknown */

    (void) gbfgetdbl(fin);

    wpt->longitude = gbfgetdbl(fin);
    wpt->latitude = gbfgetdbl(fin);
    double ll = gbfgetdbl(fin);
    if (ll != wpt->longitude) {
      fatal(MYNAME "_poi: Invalid file!\n");
    }
    ll = gbfgetdbl(fin);
    if (ll != wpt->latitude) {
      fatal(MYNAME "_poi: Invalid file!\n");
    }

    waypt_add(wpt);
  }
}

static void
destinator_read_rte()
{
  int count = 0;
  route_head* rte = nullptr;

  gbfrewind(fin);

  while (!(gbfeof(fin))) {
    if (count == 0) {
      QString str = read_wcstr(0);
      if ((str != DST_ITINERARY)) {
        fatal(MYNAME "_itn: Invalid record header!\n");
      }
    } else if (! read_until_wcstr(DST_ITINERARY)) {
      break;
    }

    count++;

    Waypoint* wpt = new Waypoint;

    wpt->shortname = read_wcstr(0);
    wpt->notes = read_wcstr(0);

    (void) gbfgetint32(fin);
    (void) gbfgetdbl(fin);
    (void) gbfgetdbl(fin);

    wpt->longitude = gbfgetdbl(fin);
    wpt->latitude = gbfgetdbl(fin);
    if (gbfgetdbl(fin) != wpt->longitude) {
      fatal(MYNAME "_itn: Invalid file!\n");
    }
    if (gbfgetdbl(fin) != wpt->latitude) {
      fatal(MYNAME "_itn: Invalid file!\n");
    }

    if (! rte) {
      rte = route_head_alloc();
      route_add_head(rte);
    }
    route_add_wpt(rte, wpt);

    (void) gbfgetdbl(fin);
    (void) gbfgetdbl(fin);
  }
}

static void
destinator_read_trk()
{
  char TXT[4] = "TXT";
  int recno = -1;
  route_head* trk = nullptr;

  gbfrewind(fin);

  while (!(gbfeof(fin))) {
    struct tm tm;
    char buff[20];

    recno++;

    if (gbfeof(fin)) {
      break;
    }

    Waypoint* wpt = new Waypoint;

    wpt->longitude = gbfgetdbl(fin);
    wpt->latitude = gbfgetdbl(fin);
    wpt->altitude = gbfgetdbl(fin);

    (void) gbfgetdbl(fin);				/* unknown */
    (void) gbfgetdbl(fin);				/* unknown */
    (void) gbfgetdbl(fin);				/* unknown */

    wpt->fix = (fix_type) gbfgetint32(fin);
    wpt->sat = gbfgetint32(fin);

    gbfseek(fin, 12 * sizeof(int32_t), SEEK_CUR);	/* SAT info */

    int date = gbfgetint32(fin);
    double milliseconds = gbfgetflt(fin);

    gbfseek(fin, 2 * 12, SEEK_CUR);			/* SAT info */

    gbfread(TXT, 1, 3, fin);
    if (strcmp(TXT, "TXT") != 0) {
      fatal(MYNAME "_trk: No (or unknown) file!\n");
    }

    gbfseek(fin, 13, SEEK_CUR);			/* unknown */

    memset(&tm, 0, sizeof(tm));

    snprintf(buff, sizeof(buff), "%06d%.f", date, milliseconds);
    strptime(buff, "%d%m%y%H%M%S", &tm);
    int millisecs = lround(milliseconds) % 1000;
    wpt->SetCreationTime(mkgmtime(&tm), millisecs);

    if (wpt->fix > 0) {
      wpt->fix = (fix_type)(wpt->fix + 1);
    }

    if (! trk) {
      trk = route_head_alloc();
      track_add_head(trk);
    }
    track_add_wpt(trk, wpt);
  }
}

static void
destinator_read()
{
  double d0, d1;
  char buff[16];

  if (! gbfread(buff, 1, sizeof(buff), fin)) {
    fatal(MYNAME ": Unexpected EOF (end of file)!\n");
  }

  int i0 = le_read32(&buff[0]);
  int i1 = le_read32(&buff[4]);

  if ((i0 == 0x690043) && (i1 == 0x790074)) {
    if (data_type != rtedata) {
      warning(MYNAME ": Using Destinator Itinerary Format!\n");
    }
    destinator_read_rte();
  } else if ((i0 == 0x790044) && (i1 == 0x61006e)) {
    if (data_type != wptdata) {
      warning(MYNAME ": Using Destinator POI Format!\n");
    }
    destinator_read_poi();
  } else {
    if (data_type != trkdata) {
      warning(MYNAME ": Using Destinator Tracklog Format!\n");
    }

    le_read64(&d0, &buff[0]);
    le_read64(&d1, &buff[8]);
    if ((fabs(d0) > 180) || (fabs(d1) > 90)) {
      fatal(MYNAME ": No Destinator (.dat) file!\n");
    }
    destinator_read_trk();
  }
}

/*******************************************************************************/
/*                                   WRITER                                    */
/*-----------------------------------------------------------------------------*/

static void
destinator_wpt_disp(const Waypoint* wpt)
{
  garmin_fs_t* gmsd = GMSD_FIND(wpt);

  write_wcstr(DST_DYN_POI);
  write_wcstr((!wpt->shortname.isEmpty()) ? wpt->shortname : "WPT");
  write_wcstr((!wpt->notes.isEmpty()) ? wpt->notes : wpt->description);

  write_wcstr(nullptr);				/* house number */
  write_wcstr(GMSD_GET(addr, NULL));		/* street */
  write_wcstr(GMSD_GET(city, NULL));		/* city */
  write_wcstr(nullptr);				/* unknown */
  write_wcstr(GMSD_GET(postal_code, NULL));	/* postcode */
  write_wcstr(nullptr);				/* unknown */

  gbfputint32(0, fout);
  gbfputint32(0, fout);

  gbfputdbl(wpt->longitude, fout);
  gbfputdbl(wpt->latitude, fout);
  gbfputdbl(wpt->longitude, fout);
  gbfputdbl(wpt->latitude, fout);

  gbfputdbl(0, fout);
  gbfputdbl(0, fout);
}

static void
destinator_trkpt_disp(const Waypoint* wpt)
{
  int i;

  gbfputdbl(wpt->longitude, fout);
  gbfputdbl(wpt->latitude, fout);
  gbfputdbl(wpt->altitude, fout);
  gbfputdbl(0, fout);
  gbfputdbl(0, fout);
  gbfputdbl(0, fout);
  gbfputint32(wpt->fix > fix_unknown ? wpt->fix - 1 : 0, fout);
  gbfputint32(wpt->sat, fout);
  for (i = 0; i < 12; i++) {
    gbfputint32(0, fout);
  }

  if (wpt->creation_time.isValid()) {
    const time_t ct = wpt->GetCreationTime().toTime_t();
    struct tm tm = *gmtime(&ct);
    tm.tm_mon += 1;
    tm.tm_year -= 100;
    int date = (tm.tm_mday * 10000) + (tm.tm_mon * 100) + tm.tm_year;
    gbfputint32(date, fout);
    double milliseconds = (tm.tm_hour * 10000) +
      (tm.tm_min * 100) + tm.tm_sec;
    milliseconds = (milliseconds * 1000) + (wpt->GetCreationTime().time().msec());

    gbfputflt(milliseconds, fout);
  } else {
    gbfputint32(0, fout);	/* Is this invalid ? */
    gbfputflt(0, fout);
  }

  for (i = 0; i < 12; i++) {
    gbfputint16(0, fout);
  }
  gbfputcstr("TXT", fout);
  for (i = 0; i < 12; i++) {
    gbfputc(0, fout);
  }
}

static void
destinator_rtept_disp(const Waypoint* wpt)
{
  write_wcstr(DST_ITINERARY);
  write_wcstr((!wpt->shortname.isEmpty()) ? wpt->shortname : "RTEPT");
  write_wcstr((!wpt->notes.isEmpty()) ? wpt->notes : wpt->description);

  gbfputint32(0, fout);
  gbfputdbl(0, fout);
  gbfputdbl(0, fout);

  gbfputdbl(wpt->longitude, fout);
  gbfputdbl(wpt->latitude, fout);
  gbfputdbl(wpt->longitude, fout);
  gbfputdbl(wpt->latitude, fout);

  gbfputdbl(0, fout);
  gbfputdbl(0, fout);
}

/*******************************************************************************
* %%%        global callbacks called by gpsbabel main process              %%% *
*******************************************************************************/

static void
destinator_rd_init(const QString& fname)
{
  fin = gbfopen_le(fname, "rb", MYNAME);
}

static void
destinator_rd_deinit()
{
  gbfclose(fin);
}

static void
destinator_read_poi_wrapper()
{
  data_type = wptdata;
  destinator_read();
}

static void
destinator_read_rte_wrapper()
{
  data_type = rtedata;
  destinator_read();
}

static void
destinator_read_trk_wrapper()
{
  data_type = trkdata;
  destinator_read();
}

static void
destinator_wr_init(const QString& fname)
{
  fout = gbfopen_le(fname, "wb", MYNAME);
}

static void
destinator_wr_deinit()
{
  gbfclose(fout);
}

static void
destinator_write_poi()
{
  waypt_disp_all(destinator_wpt_disp);
}

static void
destinator_write_rte()
{
  route_disp_all(nullptr, nullptr, destinator_rtept_disp);
}

static void
destinator_write_trk()
{
  track_disp_all(nullptr, nullptr, destinator_trkpt_disp);
}

/**************************************************************************/

ff_vecs_t destinator_poi_vecs = {
  ff_type_file,
  {
    (ff_cap)(ff_cap_read | ff_cap_write)	/* waypoints */,
    ff_cap_none		 	/* tracks */,
    ff_cap_none 			/* routes */
  },
  destinator_rd_init,
  destinator_wr_init,
  destinator_rd_deinit,
  destinator_wr_deinit,
  destinator_read_poi_wrapper,
  destinator_write_poi,
  nullptr,
  destinator_args,
  CET_CHARSET_UTF8, 1			/* fixed */
  , NULL_POS_OPS,
  nullptr};

ff_vecs_t destinator_itn_vecs = {
  ff_type_file,
  {
    ff_cap_none 			/* waypoints */,
    ff_cap_none		 	/* tracks */,
    (ff_cap)(ff_cap_read | ff_cap_write)	/* routes */
  },
  destinator_rd_init,
  destinator_wr_init,
  destinator_rd_deinit,
  destinator_wr_deinit,
  destinator_read_rte_wrapper,
  destinator_write_rte,
  nullptr,
  destinator_args,
  CET_CHARSET_UTF8, 1			/* fixed */
  , NULL_POS_OPS,
  nullptr};

ff_vecs_t destinator_trl_vecs = {
  ff_type_file,
  {
    ff_cap_none 			/* waypoints */,
    (ff_cap)(ff_cap_read | ff_cap_write)	/* tracks */,
    ff_cap_none 			/* routes */
  },
  destinator_rd_init,
  destinator_wr_init,
  destinator_rd_deinit,
  destinator_wr_deinit,
  destinator_read_trk_wrapper,
  destinator_write_trk,
  nullptr,
  destinator_args,
  CET_CHARSET_UTF8, 1			/* fixed */
  , NULL_POS_OPS,
  nullptr };

/**************************************************************************/
