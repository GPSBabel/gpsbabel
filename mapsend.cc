/*
    Access Magellan Mapsend files.

    Copyright (C) 2002-2006 Robert Lipe, robertlipe+source@gpsbabel.org

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

#include <cmath>                // for lround
#include <cstdio>               // for sprintf
#include <cstdlib>              // for atoi
#include <cstring>              // for strncpy
#include <ctime>

#include <QtCore/QCharRef>      // for QCharRef
#include <QtCore/QString>       // for QString
#include <QtCore/QTime>         // for QTime
#include <QtCore/QtGlobal>      // for Q_UNUSED

#include "defs.h"
#include "mapsend.h"
#include "gbfile.h"             // for gbfputint32, gbfgetint32, gbfgetdbl, gbfputdbl, gbfgetpstr, gbfwrite, gbfputpstr, gbfputc, gbfread, gbfclose, gbfgetc, gbfgetflt, gbfopen, gbfputflt, gbfile, gbfgetuint32, gbfopen_le, gbsize_t
#include "magellan.h"           // for mag_find_token_from_descr, mag_find_descr_from_token
#include "src/core/datetime.h"  // for DateTime


static gbfile* mapsend_file_in;
static gbfile* mapsend_file_out;
static short_handle mkshort_handle;
static short_handle wpt_handle;

static int route_wp_count;
static int mapsend_infile_version;
static int trk_version = 30;

#define MYNAME "mapsend"

static char* mapsend_opt_trkver = nullptr;
#define MAPSEND_TRKVER_MIN 3
#define MAPSEND_TRKVER_MAX 4

static
arglist_t mapsend_args[] = {
  {
    "trkver", &mapsend_opt_trkver,
    "MapSend version TRK file to generate (3,4)",
    "4", ARGTYPE_INT, "3", "4", nullptr
  },
  ARG_TERMINATOR
};

static void
mapsend_init_opts(const char isReading)  	/* 1=read, 2=write */
{
  /* read & write options here */

  if (isReading) {
    /* reading-only options here */
  } else {
    /* writing-only options here */

    // TRK MapSend version
    int opt_trkver = atoi(mapsend_opt_trkver);
    if ((opt_trkver < MAPSEND_TRKVER_MIN) || (opt_trkver > MAPSEND_TRKVER_MAX)) {
      fatal(MYNAME ": Unsupported MapSend TRK version \"%s\"!\n", mapsend_opt_trkver);
    }
    trk_version = opt_trkver * 10;
  }
}

static void
mapsend_rd_init(const QString& fname)
{
  mapsend_init_opts(1);
  mapsend_file_in = gbfopen_le(fname, "rb", MYNAME);
}

static void
mapsend_rd_deinit()
{
  gbfclose(mapsend_file_in);
}

static void
mapsend_wr_init(const QString& fname)
{
  mapsend_init_opts(0);
  mapsend_file_out = gbfopen(fname, "wb", MYNAME);
  mkshort_handle = mkshort_new_handle();

  wpt_handle = mkshort_new_handle();
  setshort_whitespace_ok(wpt_handle, 1);
  setshort_length(wpt_handle, 8);

  route_wp_count = 0;
}

static void
mapsend_wr_deinit()
{
  gbfclose(mapsend_file_out);
  mkshort_del_handle(&mkshort_handle);
  mkshort_del_handle(&wpt_handle);
}

static void
mapsend_wpt_read()
{
  char tbuf[256];
  char wpt_icon;
  Waypoint* wpt_tmp;

  int wpt_count = gbfgetint32(mapsend_file_in);

  while (wpt_count--) {
    wpt_tmp = new Waypoint;

    wpt_tmp->shortname = gbfgetpstr(mapsend_file_in);
    wpt_tmp->description = gbfgetpstr(mapsend_file_in);

    int wpt_number = gbfgetint32(mapsend_file_in);
    (void) wpt_number; // hush warning.
    wpt_icon = gbfgetc(mapsend_file_in);
    char wpt_status = gbfgetc(mapsend_file_in);
    (void) wpt_status; // hush warning.

    wpt_tmp->altitude = gbfgetdbl(mapsend_file_in);
    wpt_tmp->longitude = gbfgetdbl(mapsend_file_in);
    wpt_tmp->latitude = -gbfgetdbl(mapsend_file_in);

    if (wpt_icon < 26) {
      sprintf(tbuf, "%c", wpt_icon + 'a');
    } else {
      sprintf(tbuf, "a%c", wpt_icon - 26 + 'a');
    }
    wpt_tmp->icon_descr = mag_find_descr_from_token(tbuf);

    waypt_add(wpt_tmp);
  }

  /* now read the routes... */
  int rte_count = gbfgetint32(mapsend_file_in);

  while (rte_count--) {
    route_head* rte_head = route_head_alloc();
    route_add_head(rte_head);

    /* route name */
    rte_head->rte_name = gbfgetpstr(mapsend_file_in);

    /* route # */
    int rte_num = gbfgetint32(mapsend_file_in);
    rte_head->rte_num = rte_num;

    /* points this route */
    wpt_count = gbfgetint32(mapsend_file_in);

    while (wpt_count--) {
      wpt_tmp = new Waypoint;

      /* waypoint name */
      wpt_tmp->shortname = gbfgetpstr(mapsend_file_in);

      /* waypoint # */
      int wpt_number = gbfgetint32(mapsend_file_in);
      Q_UNUSED(wpt_number)
      wpt_tmp->longitude = gbfgetdbl(mapsend_file_in);
      wpt_tmp->latitude = -gbfgetdbl(mapsend_file_in);

      gbfread(&wpt_icon, 1, sizeof(wpt_icon), mapsend_file_in);

      if (wpt_icon < 26) {
        sprintf(tbuf, "%c", wpt_icon + 'a');
      } else {
        sprintf(tbuf, "a%c", wpt_icon - 26 + 'a');
      }
      wpt_tmp->icon_descr = mag_find_descr_from_token(tbuf);

      route_add_wpt(rte_head, wpt_tmp);
    }
  }
}

static void
mapsend_track_read()
{
  route_head* track_head = route_head_alloc();
  track_head->rte_name = gbfgetpstr(mapsend_file_in);
  track_add_head(track_head);

  unsigned int trk_count = gbfgetuint32(mapsend_file_in);

  while (trk_count--) {
    Waypoint* wpt_tmp = new Waypoint;

    wpt_tmp->longitude = gbfgetdbl(mapsend_file_in);
    wpt_tmp->latitude = -gbfgetdbl(mapsend_file_in);

    if (mapsend_infile_version < 36) { /* < version 4.0 */
      wpt_tmp->altitude = gbfgetint32(mapsend_file_in);
    } else {
      wpt_tmp->altitude = gbfgetflt(mapsend_file_in);
    }
    if (wpt_tmp->altitude < unknown_alt + 1) {
      wpt_tmp->altitude = unknown_alt;
    }
    time_t t = gbfgetint32(mapsend_file_in);
    int32_t valid = gbfgetint32(mapsend_file_in);
    Q_UNUSED(valid);

    /* centiseconds only in >= version 3.0 */
    unsigned char centisecs;
    if (mapsend_infile_version >= 34) {
      gbfread(&centisecs, 1, 1, mapsend_file_in);
    } else {
      centisecs = 0;
    }
    wpt_tmp->SetCreationTime(t, 10 * centisecs);

    track_add_wpt(track_head, wpt_tmp);
  }
}

static void
mapsend_read()
{
  mapsend_hdr hdr;
  char buf[3];

  /*
   * Because of the silly struct packing and the goofy variable-length
   * strings, each member has to be read in one at a time.  Grrr.
   */

  gbsize_t len = gbfread(&hdr, 1, sizeof(hdr), mapsend_file_in);
  is_fatal(len < sizeof(hdr), MYNAME ": No mapsend or empty file!");

  int type = le_read16(&hdr.ms_type);
  strncpy(buf, hdr.ms_version, 2);
  buf[2] = '\0';

  mapsend_infile_version = atoi(buf);

  switch (type) {
  case ms_type_wpt:
    mapsend_wpt_read();
    break;
  case ms_type_track:
    mapsend_track_read();
    break;
  case ms_type_log:
    fatal(MYNAME ", GPS logs not supported.\n");
  case ms_type_rgn:
    fatal(MYNAME ", GPS regions not supported.\n");
  default:
    fatal(MYNAME ", unknown file type %d\n", type);
  }
}


static void
mapsend_waypt_pr(const Waypoint* waypointp)
{
  static int cnt = 0;
  QString sn = global_opts.synthesize_shortnames ?
                   mkshort_from_wpt(mkshort_handle, waypointp) :
                   waypointp->shortname;

  /*
   * The format spec doesn't call out the character set of waypoint
   * name and description.  Empirically, we can see that it's 8859-1,
   * but if we create mapsend files containing those, Mapsend becomes
   * grumpy uploading the resulting waypoints and being unable to deal
   * with the resulting comm errors.
   *
   * Ironically, our own Magellan serial module strips the "naughty"
   * characters, keeping it more in definition with their own serial
   * spec. :-)
   *
   * So we just decompose the utf8 strings to ascii before stuffing
   * them into the Mapsend file.
   */


  QString tmp1 = mkshort(wpt_handle, sn);
  gbfputpstr(tmp1, mapsend_file_out);

  // This is funny looking to ensure that no more than 30 bytes
  // get written to the file.
  unsigned int c = waypointp->description.length();
  if (c > 30) {
    c = 30;
  }
  gbfputc(c, mapsend_file_out);
  gbfwrite(CSTR(waypointp->description), 1, c, mapsend_file_out);

  /* #, icon, status */
  gbfputint32(++cnt, mapsend_file_out);


  QString iconp;
  if (!waypointp->icon_descr.isNull()) {
    iconp = mag_find_token_from_descr(waypointp->icon_descr);
    if (1 == iconp.size()) {
      c = iconp[0].toLatin1() - 'a';
    } else {
      c = iconp[1].toLatin1() - 'a' + 26;
    }
  } else  {
    c = 0;
  }
  if (get_cache_icon(waypointp)) {
    iconp = mag_find_token_from_descr(get_cache_icon(waypointp));
    if (1 == iconp.size()) {
      c = iconp[0].toLatin1() - 'a';
    } else {
      c = iconp[1].toLatin1() - 'a' + 26;
    }
  }

  gbfputc(c, mapsend_file_out);
  gbfputc(1, mapsend_file_out);

  double falt = waypointp->altitude;
  if (falt == unknown_alt) {
    falt = 0;
  }
  gbfputdbl(falt, mapsend_file_out);

  gbfputdbl(waypointp->longitude, mapsend_file_out);
  gbfputdbl(-waypointp->latitude, mapsend_file_out);
}

static void
mapsend_route_hdr(const route_head* rte)
{
  QString rname;
  QString r = rte->rte_name;

  /* route name -- mapsend really seems to want something here.. */
  if (r.isEmpty()) {
    rname = "Route";
  } else {
    rname = CSTRc(rte->rte_name);
  }
  gbfputpstr(rname, mapsend_file_out);

  /* route # */
  gbfputint32(rte->rte_num, mapsend_file_out);

  /* # of waypoints to follow... */
  gbfputint32(rte->rte_waypt_ct, mapsend_file_out);
}

static void
mapsend_noop(const route_head*)
{
  /* no-op */
}

static void
mapsend_route_disp(const Waypoint* waypointp)
{
  unsigned char c;

  route_wp_count++;

  /* waypoint name */
  gbfputpstr(waypointp->shortname, mapsend_file_out);

  /* waypoint number */
  gbfputint32(route_wp_count, mapsend_file_out);

  gbfputdbl(waypointp->longitude, mapsend_file_out);
  gbfputdbl(-waypointp->latitude, mapsend_file_out);

  if (!waypointp->icon_descr.isNull()) {
    QString iconp = mag_find_token_from_descr(waypointp->icon_descr);
    if (1 == iconp.size()) {
      c = iconp[0].toLatin1() - 'a';
    } else {
      c = iconp[1].toLatin1() - 'a' + 26;
    }
  } else  {
    c = 0;
  }
  gbfwrite(&c, 1, 1, mapsend_file_out);
}

static void mapsend_track_hdr(const route_head* trk)
{
  /*
   * we write mapsend v3.0 tracks as mapsend v2.0 tracks get
   * tremendously out of whack time/date wise.
   */
  const char* verstring = "30";
  mapsend_hdr hdr = {13, {'4','D','5','3','3','3','3','4',' ','M','S'},
    {'3','0'}, ms_type_track, {0, 0, 0}
  };

  switch (trk_version) {
  case 20:
    verstring = "30";
    break;
  case 30:
    verstring = "34";
    break;
  case 40:
    /* the signature seems to change with the versions, even though it
     * shouldn't have according to the document. MapSend V4 doesn't
     * like the old version.
     */
    hdr.ms_signature[7] = '6';
    verstring = "36";
    break;
  default:
    fatal("Unknown track version.\n");
    break;
  }

  hdr.ms_version[0] = verstring[0];
  hdr.ms_version[1] = verstring[1];

  gbfwrite(&hdr, sizeof(hdr), 1, mapsend_file_out);
  QString tname = trk->rte_name.isEmpty() ? "Track" : trk->rte_name;
  gbfputpstr(tname, mapsend_file_out);

  /* total nodes (waypoints) this track */
  int i = trk->waypoint_list.count();

  gbfputint32(i, mapsend_file_out);

}

static void mapsend_track_disp(const Waypoint* wpt)
{
  unsigned char c;
  static int last_time;

  /*
   * Firmware Ver 4.06 (at least) has a defect when it's set for .01km
   * tracking that will sometimes result in timestamps in the track
   * going BACKWARDS.   When mapsend sees this, it (stupidly) advances
   * the date by one, ignoring the date on the TRK lines.   This looks
   * for time travel and just uses the previous time - it's better to
   * be thought to be standing still than to be time-travelling!
   *
   * This is rumoured (but yet unconfirmed) to be fixed in f/w 5.12.
   */
  int32_t t = wpt->GetCreationTime().toTime_t();
  if (t < last_time)  {
    t = last_time;
  }

  /* x = longitude */
  gbfputdbl(wpt->longitude, mapsend_file_out);

  /* x = latitude */
  gbfputdbl(-wpt->latitude, mapsend_file_out);

  /* altitude
   * in V4.0+ this field is a float, it was previously an int
   */
  if (trk_version < 40) {
    gbfputint32((int) wpt->altitude, mapsend_file_out);
  } else {
    gbfputflt((float) wpt->altitude, mapsend_file_out);
  }

  /* time */
  gbfputint32(t, mapsend_file_out);
  last_time = t;

  /* validity */
  gbfputint32(1, mapsend_file_out);

  /* 0 centiseconds */
  if (trk_version >= 30) {
    c = lround(wpt->GetCreationTime().time().msec() / 10.0);
    gbfwrite(&c, 1, 1, mapsend_file_out);
  }
}

static void
mapsend_track_write()
{
  track_disp_all(mapsend_track_hdr, mapsend_noop, mapsend_track_disp);
}

static void
mapsend_wpt_write()
{
  mapsend_hdr hdr = {13, {'4','D','5','3','3','3','3','0',' ','M','S'},
    {'3', '0'}, ms_type_wpt, {0, 0, 0}
  };
  int wpt_count = waypt_count();

  if (global_opts.objective == trkdata) {
    mapsend_track_write();
  } else {
    gbfwrite(&hdr, sizeof(hdr), 1, mapsend_file_out);

    if (global_opts.objective == wptdata) {
      gbfputint32(wpt_count, mapsend_file_out);
      waypt_disp_all(mapsend_waypt_pr);
    } else if (global_opts.objective == rtedata) {

      /* # of points - all routes */
      gbfputint32(route_waypt_count(), mapsend_file_out);

      /* write points - all routes */
      route_disp_all(mapsend_noop, mapsend_noop, mapsend_waypt_pr);
    }

    int n = route_count();

    gbfputint32(n, mapsend_file_out);

    if (n) {
      route_disp_all(mapsend_route_hdr, mapsend_noop, mapsend_route_disp);
    }
  }
}



ff_vecs_t mapsend_vecs = {
  ff_type_file,
  FF_CAP_RW_ALL,
  mapsend_rd_init,
  mapsend_wr_init,
  mapsend_rd_deinit,
  mapsend_wr_deinit,
  mapsend_read,
  mapsend_wpt_write,
  nullptr,
  mapsend_args,
  CET_CHARSET_ASCII, 0	/* CET-REVIEW */
  , NULL_POS_OPS,
  nullptr
};
