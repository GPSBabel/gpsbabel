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
#include <QDebug>

#include <cassert>                 // for assert
#include <cmath>                   // for fabs, lround
#include <cstdio>                  // for NULL, SEEK_CUR, snprintf
#include <cstdint>
#include <cstring>                 // for strcmp, memmove, memset, strlen

#include <QByteArray>              // for QByteArray
#include <QScopedPointer>          // for QScopedPointer
#include <QString>                 // for QString
#include <QTextCodec>              // for QTextCodec, QTextCodec::IgnoreHeader
#include <QTextDecoder>            // for QTextDecoder
#include <QTextEncoder>            // for QTextEncoder
#include <QTime>                   // for QTime
#include <QVector>                 // for QVector

#include "defs.h"
#include "garmin_fs.h"             // for garmin_fs_t, garmin_fs_flags_t, GMSD_GET, GMSD_SETSTRQ, garmin_fs_alloc, GMSD_FIND
#include "gbfile.h"                // for gbfputdbl, gbfgetdbl, gbfputint32, gbfeof, gbfgetint32, gbfread, gbfrewind, gbfseek, gbfclose, gbfputflt, gbfgetc, gbfgetflt, gbfputc, gbfputcstr, gbfputint16, gbfwrite, gbfile, gbfopen_le
#include "src/core/datetime.h"     // for DateTime
#include "strptime.h"              // for strptime


#define MYNAME 		"destinator"
#define DST_DYN_POI 	"Dynamic POI"
#define DST_ITINERARY 	"City->Street"

static
QVector<arglist_t> destinator_args = {
};

static gbfile* fin, *fout;
static gpsdata_type data_type;
static QTextCodec* utf16le_codec{nullptr};


/*******************************************************************************/
/*                                   READER                                    */
/*-----------------------------------------------------------------------------*/

static garmin_fs_t*
gmsd_init(Waypoint* wpt)
{
  garmin_fs_t* gmsd = garmin_fs_t::find(wpt);
  if (gmsd == nullptr) {
    gmsd = garmin_fs_alloc(-1);
    wpt->fs.FsChainAdd(gmsd);
  }
  return gmsd;
}

static QString
read_wcstr()
{
  QScopedPointer<QTextDecoder> decoder(utf16le_codec->makeDecoder(QTextCodec::IgnoreHeader));
  QString result;
  bool done;
  do {
    QByteArray chunk = gbfreadbuf(2, fin);
    assert(chunk.size() == 2);
    if ((chunk.at(0) != 0) || (chunk.at(1) != 0)) {
      result += decoder->toUnicode(chunk);
      done = false;
    } else {
      done = true;
    }
  } while (!done);
  return result.trimmed();
}

static void
write_wcstr(const QString& str)
{
  /* use an encoder to avoid generating a BOM. */
  QScopedPointer<QTextEncoder> encoder(utf16le_codec->makeEncoder(QTextCodec::IgnoreHeader));
  QByteArray qba = encoder->fromUnicode(str).append(2, 0);
  assert((qba.size() % 2) == 0);
  gbfwrite(qba.constData(), 1, qba.size(), fout);
}

static int
read_until_wcstr(const QString& str)
{
  QScopedPointer<QTextEncoder> encoder(utf16le_codec->makeEncoder(QTextCodec::IgnoreHeader));
  QByteArray target = encoder->fromUnicode(str).append(2, 0);
  assert((target.size() % 2) == 0);

  int eos = 0;

  int sz = target.size();
  QByteArray buff(sz, 0);

  while (! gbfeof(fin)) {

    char c = gbfgetc(fin);
    buff = buff.right(sz-1);
    buff.append(c);

    if (c == 0) {
      eos++;
      if (eos >= 2) {	/* two or more zero bytes => end of string */
        // QByteArray::compare introduced in Qt 5.12, but we can use
        // QByteArray::startsWith as buff.size() == target.size().
        if (buff.startsWith(target)) {
          return 1;
        }
      }
    } else {
      eos = 0;
    }
  }
  return 0;
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
      str = read_wcstr();
      if ((str != DST_DYN_POI)) {
        fatal(MYNAME "_poi: Invalid record header!\n");
      }
    } else if (! read_until_wcstr(DST_DYN_POI)) {
      break;
    }

    count++;

    auto* wpt = new Waypoint;

    wpt->shortname = read_wcstr();
    wpt->notes = read_wcstr();		/* comment */

    QString hnum = read_wcstr();			/* house number */

    str = read_wcstr(); 			/* street */
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
      garmin_fs_t::set_addr(gmsd, str);
    }

    if (!(str = read_wcstr()).isEmpty()) {		/* city */
      gmsd = gmsd_init(wpt);
      garmin_fs_t::set_city(gmsd, str);
    }

    (void) read_wcstr();			/* unknown */

    if (!(str = read_wcstr()).isEmpty()) {		/* postcode */
      gmsd = gmsd_init(wpt);
      garmin_fs_t::set_postal_code(gmsd, str);
    }

    (void) read_wcstr();			/* unknown */

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
      QString str = read_wcstr();
      if ((str != DST_ITINERARY)) {
        fatal(MYNAME "_itn: Invalid record header!\n");
      }
    } else if (! read_until_wcstr(DST_ITINERARY)) {
      break;
    }

    count++;

    auto* wpt = new Waypoint;

    wpt->shortname = read_wcstr();
    wpt->notes = read_wcstr();

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
      rte = new route_head;
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

    recno++;

    if (gbfeof(fin)) {
      break;
    }

    auto* wpt = new Waypoint;

    wpt->longitude = gbfgetdbl(fin);
    wpt->latitude = gbfgetdbl(fin);
    wpt->altitude = gbfgetdbl(fin);

    (void) gbfgetdbl(fin);				/* unknown */
    (void) gbfgetdbl(fin);				/* unknown */
    (void) gbfgetdbl(fin);				/* unknown */

    wpt->fix = (fix_type) gbfgetint32(fin);
    wpt->sat = gbfgetint32(fin);

    gbfseek(fin, 12 * sizeof(int32_t), SEEK_CUR);	/* SAT info */

    int date_dmy = gbfgetint32(fin);
    double time_ms = gbfgetflt(fin);

    gbfseek(fin, 2 * 12, SEEK_CUR);			/* SAT info */

    gbfread(TXT, 1, 3, fin);
    if (strcmp(TXT, "TXT") != 0) {
      fatal(MYNAME "_trk: No (or unknown) file!\n");
    }

    gbfseek(fin, 13, SEEK_CUR);			/* unknown */

    int year = 100 + date_dmy % 100 + 1900;
    date_dmy = date_dmy / 100;
    int mon = date_dmy % 100;
    date_dmy = date_dmy / 100;
    int day = date_dmy % 100;
    QDate date(year, mon, day);

    int hhms = (int) time_ms/1000;
    int sec = hhms % 100;
    hhms = hhms / 100;
    int min = hhms % 100;
    hhms = hhms / 100;
    int hour = hhms % 100;
    QTime tim(hour, min, sec);

    int ms = (int) time_ms % 1000 ;
    tim = tim.addMSecs(ms);

    QDateTime datetime(date, tim, Qt::UTC);
    wpt->SetCreationTime(datetime);

    if (wpt->fix > 0) {
      wpt->fix = (fix_type)(wpt->fix + 1);
    }

    if (! trk) {
      trk = new route_head;
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
  garmin_fs_t* gmsd = garmin_fs_t::find(wpt);

  write_wcstr(DST_DYN_POI);
  write_wcstr((!wpt->shortname.isEmpty()) ? wpt->shortname : "WPT");
  write_wcstr((!wpt->notes.isEmpty()) ? wpt->notes : wpt->description);

  write_wcstr(nullptr);				/* house number */
  write_wcstr(garmin_fs_t::get_addr(gmsd, nullptr));		/* street */
  write_wcstr(garmin_fs_t::get_city(gmsd, nullptr));		/* city */
  write_wcstr(nullptr);				/* unknown */
  write_wcstr(garmin_fs_t::get_postal_code(gmsd, nullptr));	/* postcode */
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
    QDate dt = wpt->GetCreationTime().date();
    double milliseconds = 0;
    int date = dt.day() * 10000 + (dt.month() - 1) * 100 + (dt.year() - 1900);
    gbfputint32(date, fout);

    QTime tm = wpt->GetCreationTime().time();
    milliseconds = tm.hour() * 10000 + tm.minute() * 100 + tm.second();
    milliseconds = milliseconds * 1000 + tm.msec();

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
  utf16le_codec = QTextCodec::codecForName("UTF-16LE");
}

static void
destinator_rd_deinit()
{
  gbfclose(fin);
  utf16le_codec = nullptr;
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
  utf16le_codec = QTextCodec::codecForName("UTF-16LE");
}

static void
destinator_wr_deinit()
{
  gbfclose(fout);
  utf16le_codec = nullptr;
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
  &destinator_args,
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
  &destinator_args,
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
  &destinator_args,
  CET_CHARSET_UTF8, 1			/* fixed */
  , NULL_POS_OPS,
  nullptr };

/**************************************************************************/
