/*
    Jeeps wrapper for Garmin serial protocol.

    Copyright (C) 2002, 2003, 2004, 2005, 2006  Robert Lipe, robertlipe@usa.net

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
#include <limits.h>
#include <math.h>

#include "defs.h"
#include "grtcirc.h"
#include "jeeps/gps.h"
#include "garmin_tables.h"
#include "garmin_fs.h"
#include "garmin_device_xml.h"

#define MYNAME "GARMIN"
static const char* portname;
static short_handle mkshort_handle;
static GPS_PWay* tx_waylist;
static GPS_PWay* tx_routelist;
static GPS_PWay* cur_tx_routelist_entry;
static GPS_PTrack* tx_tracklist;
static GPS_PTrack* cur_tx_tracklist_entry;
static int my_track_count = 0;
static char* getposn = NULL;
static char* poweroff = NULL;
static char* eraset = NULL;
static char* resettime = NULL;
static char* snlen = NULL;
static char* snwhiteopt = NULL;
static char* deficon = NULL;
static char* category = NULL;
static char* categorybitsopt = NULL;
static int categorybits;
static int receiver_must_upper = 1;

static ff_vecs_t* gpx_vec;

#define MILITANT_VALID_WAYPT_CHARS "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

/* Technically, even this is a little loose as spaces arent allowed */
static const char* valid_waypt_chars = MILITANT_VALID_WAYPT_CHARS " ";

static
arglist_t garmin_args[] = {
  {
    "snlen", &snlen, "Length of generated shortnames", NULL,
    ARGTYPE_INT, "1", NULL
  },
  {
    "snwhite", &snwhiteopt, "Allow whitespace synth. shortnames",
    NULL, ARGTYPE_BOOL, ARG_NOMINMAX
  },
  { "deficon", &deficon, "Default icon name", NULL, ARGTYPE_STRING, ARG_NOMINMAX },
  {
    "get_posn", &getposn, "Return current position as a waypoint",
    NULL, ARGTYPE_BOOL, ARG_NOMINMAX
  },
  {
    "power_off", &poweroff, "Command unit to power itself down",
    NULL, ARGTYPE_BOOL, ARG_NOMINMAX
  },
  {
    "erase_t", &eraset, "Erase existing courses when writing new ones",
    NULL, ARGTYPE_BOOL, ARG_NOMINMAX
  },
  {
    "resettime", &resettime, "Sync GPS time to computer time",
    NULL, ARGTYPE_BOOL, ARG_NOMINMAX
  },
  {
    "category", &category, "Category number to use for written waypoints",
    NULL, ARGTYPE_INT, "1", "16"
  },
  {
    "bitscategory", &categorybitsopt, "Bitmap of categories",
    NULL, ARGTYPE_INT, "1", "65535"
  },
  ARG_TERMINATOR
};

static const char* d103_symbol_from_icon_number(unsigned int n);
static int d103_icon_number_from_symbol(const QString& s);


static void
rw_init(const char* fname)
{
  int receiver_short_length;
  int receiver_must_upper = 1;
  const char* receiver_charset = NULL;

  if (!mkshort_handle) {
    mkshort_handle = mkshort_new_handle();
  }

  if (global_opts.debug_level > 0)  {
    GPS_Enable_Warning();
    GPS_Enable_User();
  }
  if (global_opts.debug_level > 1)  {
    GPS_Enable_Diagnose();
  }
  GPS_Enable_Error();

  if (poweroff) {
    GPS_Command_Off(fname);
    return;
  }

  /*
   * THis is Gross. The B&W Vista sometimes sets its time decades into
   * the future with no way to reset it.  This apparently can "cure"
   * an affected unit.
   */
  if (resettime) {
    GPS_Command_Send_Time(fname, current_time().toTime_t());
    return;
  }

  if (categorybitsopt) {
    categorybits = strtol(categorybitsopt, NULL, 0);
  }

  if (GPS_Init(fname) < 0) {
    fatal(MYNAME ":Can't init %s\n", fname);
  }
  portname = fname;

  /*
   * Grope the unit we're talking to to set setshort_length to
   * 	20 for  the V,
   * 	10 for Street Pilot, (old) Rhino, 76
   * 	6 for the III, 12, emap, and etrex
   * Fortunately, getting this "wrong" only results in ugly names
   * when we're using the synthesize_shortname path.
   */
  receiver_short_length = 10;

  switch (gps_waypt_type) {	/* waypoint type as defined by jeeps */
  case 0:
    fatal("Garmin unit %d does not support waypoint xfer.",
          gps_save_id);

    break;
  case 100:	/* The GARMIN GPS Interface Specification, */
  case 101:	/* says these waypoint types use an ident */
  case 102:	/* length of 6.  Waypoint types 106, 108 */
  case 103:	/* and 109 are all variable  length    */
  case 104:
  case 105:
  case 107:
  case 150:
  case 151:
  case 152:
  case 154:
  case 155:
    receiver_short_length = 6;
    break;
  case 106:	/* Waypoint types with variable ident length */
  case 108: 	/* Need GPSr id to know the actual length */
  case 109:
  case 110:
    switch (gps_save_id) {
    case 130:	/* Garmin Etrex (yellow) */
      receiver_short_length = 6;
      break;
    case 295: 	/* eTrex (yellow, fw v. 3.30) */
    case 696: 	/* eTrex HC */
    case 574: 	/* Geko 201 */
      receiver_short_length = 6;
      valid_waypt_chars =
        MILITANT_VALID_WAYPT_CHARS " +-";
      setshort_badchars(mkshort_handle, "\"$.,'!");
      break;

    case 155:	/* Garmin V */
    case 404:	/* SP2720 */
    case 520:	/* SP2820 */
      receiver_short_length = 20;
      break;
    case 382: 	/* C320 */
      receiver_short_length = 30;
      receiver_must_upper = 0;
      break;
    case 292: /* (60|76)C[S]x series */
    case 421: /* Vista|Legend Cx */
    case 694: /* Legend HCx */
    case 695: /* Vista HC */
    case 786: /* HC model */
    case 957: /* Legend HC */
      receiver_short_length = 14;
      snwhiteopt = xstrdup("1");
      receiver_must_upper = 0;
      /* This might be 8859-1 */
      receiver_charset = CET_CHARSET_MS_ANSI;
      break;
    case 291: /* GPSMAP 60CS, probably others */
    case 1095: /* GPS 72H */
      receiver_short_length = 10;
      valid_waypt_chars = MILITANT_VALID_WAYPT_CHARS " +-";
      setshort_badchars(mkshort_handle, "\"$.,'!");
      break;
    case 231: /* Quest */
    case 463: /* Quest 2 */
      receiver_must_upper = 0;
      receiver_short_length = 30;
      receiver_charset = CET_CHARSET_MS_ANSI;
      break;
    case 577: // Rino 530HCx Version 2.50
      receiver_must_upper = 0;
      receiver_short_length = 14;
      break;
    case 429: // Streetpilot i3
      receiver_must_upper = 0;
      receiver_charset = CET_CHARSET_MS_ANSI;
      receiver_short_length = 30;
      break;
    case 484: // Forerunner 305
      receiver_short_length = 8;
      break;
    case 260: /* GPSMap 296 */
    default:
      break;
    }
    break;
  default:
    break;

  }

  // If a user has specified a non-default character set, we'll trust
  // them to sort our the wreckage of violating the Garmin protocol and
  // ship characters to the device in that character set.
  if (global_opts.charset != &cet_cs_vec_utf8) {
    receiver_charset = global_opts.charset_name;
  }
  if (global_opts.debug_level > 0)  {
    fprintf(stderr, "Waypoint type: %d\n"
            "Chosen waypoint length %d\n",
            gps_waypt_type, receiver_short_length);
    if (gps_category_type) {
      fprintf(stderr, "Waypoint category type: %d\n",
              gps_category_type);
    }
  }

  // Allow override of sent character set for internationalized GPSes.
  if (global_opts.charset != &cet_cs_vec_utf8) {
    receiver_charset = xstrdup(global_opts.charset_name);
  }

  /*
   * If the user provided a short_length, override the calculated value.
   */
  if (snlen) {
    setshort_length(mkshort_handle, atoi(snlen));
  } else {
    setshort_length(mkshort_handle, receiver_short_length);
  }

  if (snwhiteopt) {
    setshort_whitespace_ok(mkshort_handle, atoi(snwhiteopt));
  }

  /*
   * Until Garmins documents how to determine valid character space
   * for the new models, we just release this safety check manually.
   */
  if (receiver_must_upper) {
    setshort_goodchars(mkshort_handle, valid_waypt_chars);
  } else {
    setshort_badchars(mkshort_handle, "");
  }

  setshort_mustupper(mkshort_handle, receiver_must_upper);

  if (receiver_charset) {
    cet_convert_init(receiver_charset, 1);
  }
}

static void
rd_init(const char* fname)
{
  if (setjmp(gdx_jmp_buf)) {
    const char* vec_opts = NULL;
    const gdx_info* gi = gdx_get_info();
    gpx_vec = find_vec("gpx", &vec_opts);
    gpx_vec->rd_init(gi->from_device.canon);
  } else {
    gpx_vec = NULL;
    rw_init(fname);
  }
}

static void
rw_deinit(void)
{
  if (mkshort_handle) {
    mkshort_del_handle(&mkshort_handle);
  }
}

static int
waypt_read_cb(int total_ct, GPS_PWay* )
{
  static int i;

  if (global_opts.verbose_status) {
    i++;
    waypt_status_disp(total_ct, i);
  }
  return 0;
}

static void
waypt_read(void)
{
  int i,n;
  GPS_PWay* way = NULL;

  if (getposn) {
    Waypoint* wpt = new Waypoint;
    wpt->latitude = gps_save_lat;
    wpt->longitude = gps_save_lon;
    wpt->shortname = "Position";
    if (gps_save_time) {
      wpt->SetCreationTime(gps_save_time);
    }
    waypt_add(wpt);
    return;
  }

  if ((n = GPS_Command_Get_Waypoint(portname, &way, waypt_read_cb)) < 0) {
    fatal(MYNAME  ":Can't get waypoint from %s\n", portname);
  }

  for (i = 0; i < n; i++) {
    Waypoint* wpt_tmp = new Waypoint;

    wpt_tmp->shortname = way[i]->ident;
    wpt_tmp->description = QString(way[i]->cmnt).simplified();
    wpt_tmp->shortname = wpt_tmp->shortname.simplified();
    wpt_tmp->description = wpt_tmp->description.simplified();
    wpt_tmp->longitude = way[i]->lon;
    wpt_tmp->latitude = way[i]->lat;
    if (gps_waypt_type == 103) {
      wpt_tmp->icon_descr = d103_symbol_from_icon_number(
                              way[i]->smbl);
    } else {
      int dyn = 0;
      wpt_tmp->icon_descr = gt_find_desc_from_icon_number(
                              way[i]->smbl, PCX, &dyn);
    }
    /*
     * If a unit doesn't store altitude info (i.e. a D103)
     * gpsmem will default the alt to INT_MAX.   Other units
     * (I can't recall if it was the V (D109) hor the Vista (D108)
     * return INT_MAX+1, contrary to the Garmin protocol doc which
     * says they should report 1.0e25.   So we'll try to trap
     * all the cases here.     Yes, libjeeps should probably
     * do this and not us...
     */
    if ((way[i]->alt == (float)(1U<<31)) ||
        (way[i]->alt == INT_MAX) ||
        (way[i]->alt >= (float) 1.0e20)
       ) {
      wpt_tmp->altitude = unknown_alt;
    } else {
      wpt_tmp->altitude = way[i]->alt;
    }
    if (way[i]->time_populated) {
      wpt_tmp->SetCreationTime(way[i]->time);
    }
    garmin_fs_garmin_after_read(way[i], wpt_tmp, gps_waypt_type);
    waypt_add(wpt_tmp);
    GPS_Way_Del(&way[i]);
  }
  if (way) {
    xfree(way);
  }
}

static int lap_read_nop_cb(int, struct GPS_SWay**)
{
  return 0;
}

// returns 1 if the waypoint's start_time can be found
// in the laps array, 0 otherwise
unsigned int checkWayPointIsAtSplit(Waypoint* wpt, GPS_PLap* laps, int nlaps)
{
  int result = 0;

  if ((laps != NULL) && (nlaps > 0)) {
    int i;
    for (i=(nlaps-1); i >= 0; i--) {
      GPS_PLap lap = laps[i];
      time_t delta = lap->start_time - wpt->GetCreationTime().toTime_t();
      if ((delta >= -1) && (delta <= 1)) {
        result = 1;
        break;

        // as an optimization this will stop going through
        // the lap array when the negative delta gets too
        // big. It assumes that laps is sorted by time in
        // ascending order (which appears to be the case for
        // Forerunners. Don't know about other devices.
      } else if (delta < -1) {
        break;
      }
    }
  }

  return result;
}

static
void
track_read(void)
{
  int32 ntracks;
  GPS_PTrack* array;
  route_head* trk_head = NULL;
  int trk_num = 0;
  int i;
  const char* trk_name = "";
  GPS_PLap* laps = NULL;
  int nlaps = 0;
  int next_is_new_trkseg = 0;

  if (gps_lap_type != -1) {
    nlaps = GPS_Command_Get_Lap(portname, &laps, &lap_read_nop_cb);
  }


  ntracks = GPS_Command_Get_Track(portname, &array, waypt_read_cb);

  if (ntracks <= 0) {
    return;
  }

  for (i = 0; i < ntracks; i++) {
    Waypoint* wpt;

    /*
     * This is probably always in slot zero, but the Garmin
     * serial spec says these can appear anywhere.  Toss them
     * out so we don't treat it as an extraneous trackpoint.
     */
    if (array[i]->ishdr) {
      trk_name = array[i]->trk_ident;
      if (!trk_name) {
        trk_name = "";
      }
    }

    if (trk_head == NULL || array[i]->ishdr) {
      trk_head = route_head_alloc();
      trk_head->rte_num = trk_num;
      trk_head->rte_name = trk_name;
      trk_num++;
      track_add_head(trk_head);
    }

    /* Need to do this here because fitness devices set tnew
     * on a trackpoint without lat/lon.
     */
    if (array[i]->tnew) {
      next_is_new_trkseg = 1;
    }

    if (array[i]->no_latlon || array[i]->ishdr) {
      continue;
    }
    wpt = new Waypoint;

    wpt->longitude = array[i]->lon;
    wpt->latitude = array[i]->lat;
    wpt->altitude = array[i]->alt;
    wpt->heartrate = array[i]->heartrate;
    wpt->cadence = array[i]->cadence;
    wpt->shortname = array[i]->trk_ident;
    wpt->SetCreationTime(array[i]->Time);
    wpt->wpt_flags.is_split = checkWayPointIsAtSplit(wpt, laps,
                              nlaps);
    wpt->wpt_flags.new_trkseg = next_is_new_trkseg;
    next_is_new_trkseg = 0;

    if (array[i]->dpth < 1.0e25f) {
      WAYPT_SET(wpt, depth, array[i]->dpth);
    }
    if (array[i]->temperature_populated) {
      WAYPT_SET(wpt, temperature, array[i]->temperature);
    }

    track_add_wpt(trk_head, wpt);
  }

  while (ntracks) {
    GPS_Track_Del(&array[--ntracks]);
  }
  xfree(array);
}

static
void
route_read(void)
{
  int32 nroutepts;
  int i;
  GPS_PWay* array;
  /* TODO: Fixes warning but is it right?
   * RJL:  No, the warning isn't right; GCC's flow analysis is broken.
   * still, it's good taste...
   */
  route_head* rte_head = NULL;

  nroutepts = GPS_Command_Get_Route(portname, &array);

//	fprintf(stderr, "Routes %d\n", (int) nroutepts);
#if 1
  for (i = 0; i < nroutepts; i++) {
    if (array[i]->isrte) {
      char* csrc = NULL;
      /* What a horrible API has libjeeps for making this
       * my problem.
       */
      switch (array[i]->rte_prot) {
      case 201:
        csrc = array[i]->rte_cmnt;
        break;
      case 202:
        csrc = array[i]->rte_ident;
        break;
      default:
        break;
      }
      rte_head = route_head_alloc();
      route_add_head(rte_head);
      if (csrc) {
        rte_head->rte_name = csrc;
      }
    } else {
      if (array[i]->islink)  {
        continue;
      } else {
        Waypoint* wpt_tmp = new Waypoint;
        wpt_tmp->latitude = array[i]->lat;
        wpt_tmp->longitude = array[i]->lon;
        wpt_tmp->shortname = array[i]->ident;
        route_add_wpt(rte_head, wpt_tmp);
      }
    }
  }
#else
  GPS_Fmt_Print_Route(array, nroutepts, stderr);
#endif

}

#if 0
static
void
lap_read_as_track(void)
{
  int32 ntracks;
  GPS_PLap* array;
  route_head* trk_head = NULL;
  int trk_num = 0;
  int index;
  int i;
  char tbuf[128];

  ntracks = GPS_Command_Get_Lap(portname, &array, waypt_read_cb);
  if (ntracks <= 0) {
    return;
  }
  for (i = 0; i < ntracks; i++) {
    Waypoint* wpt;
    if (array[i]->index == -1) {
      index=i;
    } else {
      index=array[i]->index;
      index=i;
    }

    if ((trk_head == NULL) || (i == 0) ||
        /* D906 - last track:index is the track index */
        (array[i]->index == -1 && array[i]->track_index != 255) ||
        /* D10xx - no real separator, use begin/end time to guess */
        (abs(array[i-1]->start_time + array[i]->total_time/100-array[i]->start_time) > 2)
       ) {
      static struct tm* stmp;
      stmp = gmtime(&array[i]->start_time);
      trk_head = route_head_alloc();
      /*For D906, we would like to use the track_index in the last packet instead...*/
      trk_head->rte_num = ++trk_num;
      strftime(tbuf, 32, "%Y-%m-%dT%H:%M:%SZ", stmp);
      trk_head->rte_name = tbuf;
      track_add_head(trk_head);

      wpt = new Waypoint;

      wpt->longitude = array[i]->begin_lon;
      wpt->latitude = array[i]->begin_lat;
      wpt->heartrate = array[i]->avg_heart_rate;
      wpt->cadence = array[i]->avg_cadence;
      wpt->speed = array[i]->max_speed;
      wpt->creation_time = array[i]->start_time;
      wpt->microseconds = 0;

      sprintf(tbuf, "#%d-0", index);
      wpt->shortname = tbuf;
      sprintf(tbuf, "D:%f Cal:%d MS:%f AH:%d MH:%d AC:%d I:%d T:%d",
              array[i]->total_distance, array[i]->calories, array[i]->max_speed, array[i]->avg_heart_rate,
              array[i]->max_heart_rate, array[i]->avg_cadence, array[i]->intensity, array[i]->trigger_method);
      wpt->description = tbuf;
      track_add_wpt(trk_head, wpt);
    }
    /*Allow even if no correct location, no skip if invalid */
    /*		if (array[i]->no_latlon) {
    *			continue;
    *		}
    */
    wpt = new Waypoint;

    wpt->longitude = array[i]->end_lon;
    wpt->latitude = array[i]->end_lat;
    wpt->heartrate = array[i]->avg_heart_rate;
    wpt->cadence = array[i]->avg_cadence;
    wpt->speed = array[i]->max_speed;
    wpt->creation_time = array[i]->start_time + array[i]->total_time/100;
    wpt->microseconds = 10000*(array[i]->total_time % 100);
    /*Add fields with no mapping in the description */
    sprintf(tbuf, "#%d", index);
    wpt->shortname = tbuf;
    sprintf(tbuf, "D:%f Cal:%d MS:%f AH:%d MH:%d AC:%d I:%d T:%d (%f,%f)",
            array[i]->total_distance, array[i]->calories, array[i]->max_speed, array[i]->avg_heart_rate,
            array[i]->max_heart_rate, array[i]->avg_cadence, array[i]->intensity, array[i]->trigger_method,
            array[i]->begin_lon, array[i]->begin_lat);
    wpt->description = tbuf;

    track_add_wpt(trk_head, wpt);
  }
  while (ntracks) {
    GPS_Lap_Del(&array[--ntracks]);
  }
  xfree(array);
}
#endif

/*
 * Rather than propogate Garmin-specific data types outside of the Garmin
 * code, we convert the PVT (position/velocity/time) data from the receiver
 * to the data type we use throughout.   Yes, we do lose some data that way.
 */
static void
pvt2wpt(GPS_PPvt_Data pvt, Waypoint* wpt)
{
  double wptime, wptimes;

  wpt->altitude = pvt->alt;
  wpt->latitude = pvt->lat;
  wpt->longitude = pvt->lon;
  WAYPT_SET(wpt,course,1);
  WAYPT_SET(wpt,speed,1);

  wpt->course = 180 + DEG(atan2(-pvt->east, -pvt->north));

  /* velocity in m/s */
  WAYPT_SET(wpt,speed, sqrt(pvt->north*pvt->north + pvt->east*pvt->east));
  // wpt->vs = pvt->up;

  /*
   * The unit reports time in three fields:
   * 1) The # of days to most recent Sun. since  1989-12-31 midnight UTC.
   * 2) The number of seconds (fractions allowed) since that Sunday.
   * 3) The number of leap seconds that offset the current UTC and GPS
   *    reference clocks.
   */
  wptime = 631065600.0 + pvt->wn_days * 86400.0  +
           pvt->tow
           - pvt->leap_scnds;
  wptimes = floor(wptime);
  wpt->SetCreationTime(wptimes, 1000000.0 * (wptime - wptimes));

  /*
   * The Garmin spec fifteen different models that use a different
   * table for 'fix' without a really good way to tell if the model
   * we're talking to happens to be one of those...By inspection,
   * it looks like even though the models (Summit, Legend, etc.) may
   * be popular, it's older (2001 and earlier or so) versions that
   * are affected and I think there are relatively few readers of
   * the fix field anyway.   Time will tell if this is a good plan.
   */
  switch (pvt->fix) {
  case 0:
    wpt->fix = fix_unknown;
    break;
  case 1:
    wpt->fix = fix_none;
    break;
  case 2:
    wpt->fix = fix_2d;
    break;
  case 3:
    wpt->fix = fix_3d;
    break;
  case 4:
    wpt->fix = fix_dgps;
    break; /* 2D_diff */
  case 5:
    wpt->fix = fix_dgps;
    break; /* 3D_diff */
  default:
    /* undocumented type. */
    break;
  }
}

static gpsdevh* pvt_fd;

static void
pvt_init(const char* fname)
{
  rw_init(fname);
  GPS_Command_Pvt_On(fname, &pvt_fd);
}

static Waypoint*
pvt_read(posn_status* posn_status)
{
  Waypoint* wpt = new Waypoint;
  GPS_PPvt_Data pvt = GPS_Pvt_New();

  if (GPS_Command_Pvt_Get(&pvt_fd, &pvt)) {
    pvt2wpt(pvt, wpt);
    GPS_Pvt_Del(&pvt);

    wpt->shortname = "Position";

    if (gps_errno && posn_status) {
      posn_status->request_terminate = 1;
    }

    return wpt;
  }

  /*
   * If the caller has not given us a better way to return the
   * error, do it now.
   */
  if (gps_errno) {
    fatal(MYNAME ": Fatal error reading position.\n");
  }

  delete wpt;
  GPS_Pvt_Del(&pvt);

  return NULL;
}

static void
data_read(void)
{
  if (gpx_vec) {
    gpx_vec->read();
    return;
  }

  if (poweroff) {
    return;
  }

  if (global_opts.masked_objective & WPTDATAMASK) {
    waypt_read();
  }
  if (global_opts.masked_objective & TRKDATAMASK) {
    track_read();
  }
  if (global_opts.masked_objective & RTEDATAMASK) {
    route_read();
  }
  if (!(global_opts.masked_objective &
        (WPTDATAMASK | TRKDATAMASK | RTEDATAMASK | POSNDATAMASK))) {
    fatal(MYNAME ": Nothing to do.\n");
  }
}

static GPS_PWay
sane_GPS_Way_New(void)
{
  GPS_PWay way;
  way = GPS_Way_New();
  if (!way) {
    fatal(MYNAME ":not enough memory\n");
  }

  /*
   *  Undo less than helpful defaults from Way_New.
   */
  way->rte_ident[0] = 0;
  way->rte_cmnt[0] = 0;
  way->rte_link_subclass[0] = 0;
  way->rte_link_ident[0] = 0;
  way->city[0] = 0;
  way->state[0] = 0;
  way->facility[0] = 0;
  way->addr[0] = 0;
  way->cross_road[0] = 0;
  way->cross_road[0] = 0;
  way->dpth = 1.0e25f;
  way->wpt_class = 0;  // user waypoint by default.

  return way;
}

static int
waypt_write_cb(GPS_PWay*)
{
  static int i;
  int n = waypt_count();

  if (global_opts.verbose_status) {
    i++;
    waypt_status_disp(n, i);
  }
  return 0;
}

/*
 * If we're using smart names, try to put the cache info in the
 * description.
 */
const char*
get_gc_info(Waypoint* wpt)
{
  if (global_opts.smart_names) {
    if (wpt->gc_data->type == gt_virtual) {
      return  "V ";
    }
    if (wpt->gc_data->type == gt_unknown) {
      return  "? ";
    }
    if (wpt->gc_data->type == gt_multi) {
      return  "Mlt ";
    }
    if (wpt->gc_data->type == gt_earth) {
      return  "EC ";
    }
    if (wpt->gc_data->type == gt_event) {
      return  "Ev ";
    }
    if (wpt->gc_data->container == gc_micro) {
      return  "M ";
    }
    if (wpt->gc_data->container == gc_small) {
      return  "S ";
    }
  }
  return "";
}

static int
waypoint_prepare(void)
{
  int i;
  int n = waypt_count();
#if NEWQ
  extern QList<Waypoint*> waypt_list;
#else
  queue* elem, *tmp;
  extern queue waypt_head;
#endif
  int icon;

  tx_waylist = (struct GPS_SWay**) xcalloc(n,sizeof(*tx_waylist));

  for (i = 0; i < n; i++) {
    tx_waylist[i] = sane_GPS_Way_New();
  }

  i = 0;

#if NEWQ
  // Iterate with waypt_disp_all?
  foreach(Waypoint* wpt, waypt_list) {
#else
  QUEUE_FOR_EACH(&waypt_head, elem, tmp) {
    Waypoint* wpt = (Waypoint*) elem;
#endif
    char* ident;
    char obuf[256];

    QString src;
    if (!wpt->description.isEmpty()) {
      src = wpt->description;
    }
    if (!wpt->notes.isEmpty()) {
      src = wpt->notes;
    }

    /*
     * mkshort will do collision detection and namespace
     * cleaning
     */
    ident = mkshort(mkshort_handle,
                    global_opts.synthesize_shortnames ? CSTRc(src) :
                    CSTRc(wpt->shortname));
    /* Should not be a strcpy as 'ident' isn't really a C string,
     * but rather a garmin "fixed length" buffer that's padded
     * to the end with spaces.  So this is NOT (strlen+1).
     */
    memcpy(tx_waylist[i]->ident, ident, strlen(ident));

    if (global_opts.synthesize_shortnames) {
      xfree(ident);
    }
    tx_waylist[i]->ident[sizeof(tx_waylist[i]->ident)-1] = 0;

    // If we were explictly given a comment from GPX, use that.
    //  This logic really is horrible and needs to be untangled.
    if (!wpt->description.isEmpty() &&
        global_opts.smart_names && !wpt->gc_data->diff) {
      memcpy(tx_waylist[i]->cmnt, CSTRc(wpt->description), strlen(CSTRc(wpt->description)));
    } else {
      if (global_opts.smart_names &&
          wpt->gc_data->diff && wpt->gc_data->terr) {
#if 0
        xasprintf(&src, "%s %s", &wpt->shortname[2], src);
#endif
        snprintf(obuf, sizeof(obuf), "%s%d/%d %s",
                 get_gc_info(wpt),
                 wpt->gc_data->diff, wpt->gc_data->terr,
                 CSTRc(src));
        memcpy(tx_waylist[i]->cmnt, obuf, strlen(obuf));
      } else  {
        memcpy(tx_waylist[i]->cmnt, CSTRc(src), strlen(CSTRc(src)));
      }
    }



    tx_waylist[i]->lon = wpt->longitude;
    tx_waylist[i]->lat = wpt->latitude;

    if (deficon) {
      icon = gt_find_icon_number_from_desc(deficon, PCX);
    } else {
      if (get_cache_icon(wpt)) {
        icon = gt_find_icon_number_from_desc(get_cache_icon(wpt), PCX);
      } else {
        icon = gt_find_icon_number_from_desc(wpt->icon_descr, PCX);
      }
    }

    /* For units that support tiny numbers of waypoints, just
     * overwrite that and go very literal.
     */
    if (gps_waypt_type == 103) {
      icon = d103_icon_number_from_symbol(wpt->icon_descr);
    }
    tx_waylist[i]->smbl = icon;
    if (wpt->altitude == unknown_alt) {
      tx_waylist[i]->alt_is_unknown = 1;
      tx_waylist[i]->alt = 0;
    } else {
      tx_waylist[i]->alt = wpt->altitude;
    }
    gpsbabel::DateTime t = wpt->GetCreationTime();
    if (t.isValid()) {
      tx_waylist[i]->time = t.toTime_t();
      tx_waylist[i]->time_populated = 1;
    }
    if (category) {
      tx_waylist[i]->category = 1 << (atoi(category) - 1);
    }
    if (categorybits) {
      tx_waylist[i]->category = categorybits;
    }
    garmin_fs_garmin_before_write(wpt, tx_waylist[i], gps_waypt_type);
    i++;
  }

  return n;
}

static void
waypoint_write(void)
{
  int i, n;
  int32 ret;

  n = waypoint_prepare();

  if ((ret = GPS_Command_Send_Waypoint(portname, tx_waylist, n, waypt_write_cb)) < 0) {
    fatal(MYNAME ":communication error sending wayoints..\n");
  }

  for (i = 0; i < n; ++i) {
    GPS_Way_Del(&tx_waylist[i]);
  }
  if (global_opts.verbose_status) {
    fprintf(stdout, "\r\n");
    fflush(stdout);
  }
  xfree(tx_waylist);
}

static void
route_hdr_pr(const route_head* rte)
{
  (*cur_tx_routelist_entry)->rte_num = rte->rte_num;
  (*cur_tx_routelist_entry)->isrte = 1;
  if (!rte->rte_name.isEmpty()) {
    strncpy((*cur_tx_routelist_entry)->rte_ident, CSTRc(rte->rte_name),
            sizeof((*cur_tx_routelist_entry)->rte_ident));
  }
}

static void
route_waypt_pr(const Waypoint* wpt)
{
  GPS_PWay rte = *cur_tx_routelist_entry;

  /*
   * As stupid as this is, libjeeps seems to want an empty
   * waypoint between every link in a route that has nothing
   * but the 'islink' member set.   Rather than "fixing" libjeeps,
   * we just double them up (sigh) and do that here.
   */
  rte->islink = 1;
  rte->lon = wpt->longitude;
  rte->lat = wpt->latitude;
  cur_tx_routelist_entry++;
  rte = *cur_tx_routelist_entry;

  rte->lon = wpt->longitude;
  rte->lat = wpt->latitude;
  rte->smbl = gt_find_icon_number_from_desc(wpt->icon_descr, PCX);

  // map class so unit doesn't duplicate routepoints as a waypoint.
  rte->wpt_class = 0x80;

  if (wpt->altitude != unknown_alt) {
    rte->alt = wpt->altitude;
  } else {
    rte->alt_is_unknown = 1;
    rte->alt = 0;
  }

  // Garmin protocol spec says no spaces, no lowercase, etc. in a route.
  // enforce that here, since jeeps doesn't.
  //
  // This was strncpy(rte->ident, wpt->shortname, sizeof(rte->ident));
  char* d;
  d = rte->ident;
  for (int idx = 0; idx < wpt->shortname.length(); idx++) {
    int c = wpt->shortname[idx].toLatin1();
    if (receiver_must_upper && isalpha(c)) {
      c = toupper(c);
    }
    if (strchr(valid_waypt_chars, c)) {
      *d++ = c;
    }
  }

  rte->ident[sizeof(rte->ident)-1] = 0;
#if NEW_STRINGS
  if (wpt->description.isEmpty()) {
    strncpy(rte->cmnt, CSTR(wpt->description), sizeof(rte->cmnt));
    rte->cmnt[sizeof(rte->cmnt)-1] = 0;
  } else {
  }
#else
  if (wpt->description) {
    strncpy(rte->cmnt, wpt->description, sizeof(rte->cmnt));
    rte->cmnt[sizeof(rte->cmnt)-1] = 0;
  } else  {
    rte->cmnt[0] = 0;
  }
#endif
  cur_tx_routelist_entry++;
}

static void
route_noop(const route_head* )
{
}

static void
route_write(void)
{
  int i;
  int n = 2 * route_waypt_count(); /* Doubled for the islink crap. */

  tx_routelist = (struct GPS_SWay**) xcalloc(n,sizeof(GPS_PWay));
  cur_tx_routelist_entry = tx_routelist;

  for (i = 0; i < n; i++) {
    tx_routelist[i] = sane_GPS_Way_New();
  }

  route_disp_all(route_hdr_pr, route_noop, route_waypt_pr);
  GPS_Command_Send_Route(portname, tx_routelist, n);
}

static void
track_hdr_pr(const route_head* trk_head)
{
  (*cur_tx_tracklist_entry)->ishdr = gpsTrue;
  if (!trk_head->rte_name.isEmpty()) {
    strncpy((*cur_tx_tracklist_entry)->trk_ident, CSTRc(trk_head->rte_name), sizeof((*cur_tx_tracklist_entry)->trk_ident));
    (*cur_tx_tracklist_entry)->trk_ident[sizeof((*cur_tx_tracklist_entry)->trk_ident)-1] = 0;
  } else {
    sprintf((*cur_tx_tracklist_entry)->trk_ident, "TRACK%02d", my_track_count);
  }
  cur_tx_tracklist_entry++;
  my_track_count++;
}

static void
track_waypt_pr(const Waypoint* wpt)
{
  (*cur_tx_tracklist_entry)->lat = wpt->latitude;
  (*cur_tx_tracklist_entry)->lon = wpt->longitude;
  (*cur_tx_tracklist_entry)->alt = (wpt->altitude != unknown_alt) ? wpt->altitude : 1e25;
  (*cur_tx_tracklist_entry)->Time = wpt->GetCreationTime().toTime_t();;
  if (!wpt->shortname.isEmpty()) {
    strncpy((*cur_tx_tracklist_entry)->trk_ident, CSTRc(wpt->shortname), sizeof((*cur_tx_tracklist_entry)->trk_ident));
    (*cur_tx_tracklist_entry)->trk_ident[sizeof((*cur_tx_tracklist_entry)->trk_ident)-1] = 0;
  }
  (*cur_tx_tracklist_entry)->tnew = wpt->wpt_flags.new_trkseg;
  cur_tx_tracklist_entry++;
}

static int
track_prepare(void)
{
  int i;
  int32 n = track_waypt_count() + track_count();

  tx_tracklist = (struct GPS_STrack**) xcalloc(n, sizeof(GPS_PTrack));
  cur_tx_tracklist_entry = tx_tracklist;
  for (i = 0; i < n; i++) {
    tx_tracklist[i] = GPS_Track_New();
  }
  my_track_count = 0;
  track_disp_all(track_hdr_pr, route_noop, track_waypt_pr);

  GPS_Prepare_Track_For_Device(&tx_tracklist, &n);

  return n;
}

static void
track_write(void)
{
  int i, n;

  n = track_prepare();
  GPS_Command_Send_Track(portname, tx_tracklist, n, (eraset)? 1 : 0);

  for (i = 0; i < n; i++) {
    GPS_Track_Del(&tx_tracklist[i]);
  }
  xfree(tx_tracklist);
}

static void
course_write(void)
{
  int i, n_trk, n_wpt;

  n_wpt = waypoint_prepare();
  n_trk = track_prepare();

  GPS_Command_Send_Track_As_Course(portname, tx_tracklist, n_trk,
                                   tx_waylist, n_wpt, (eraset)? 1 : 0);

  for (i = 0; i < n_wpt; ++i) {
    GPS_Way_Del(&tx_waylist[i]);
  }
  xfree(tx_waylist);

  for (i = 0; i < n_trk; i++) {
    GPS_Track_Del(&tx_tracklist[i]);
  }
  xfree(tx_tracklist);
}

static void
data_write(void)
{
  if (poweroff) {
    return;
  }

  /* If we have both trackpoints and waypoints and the device
   * supports courses, combine them to a course. Otherwise,
   * send tracks & waypoints separately.
   */
  if ((global_opts.masked_objective & WPTDATAMASK) &&
      (global_opts.masked_objective & TRKDATAMASK) &&
      gps_course_transfer != -1) {
    course_write();
  } else {
    if (global_opts.masked_objective & WPTDATAMASK) {
      waypoint_write();
    }
    if (global_opts.masked_objective & TRKDATAMASK) {
      track_write();
    }
  }
  if (global_opts.masked_objective & RTEDATAMASK) {
    route_write();
  }
}


ff_vecs_t garmin_vecs = {
  ff_type_serial,
  FF_CAP_RW_ALL,
  rd_init,
  rw_init,
  rw_deinit,
  rw_deinit,
  data_read,
  data_write,
  NULL,
  garmin_args,
  CET_CHARSET_ASCII, 0,
  { pvt_init, pvt_read, rw_deinit, NULL, NULL, NULL }
};

static const char* d103_icons[16] = {
  "dot",
  "house",
  "gas",
  "car",
  "fish",
  "boat",
  "anchor",
  "wreck",
  "exit",
  "skull",
  "flag",
  "camp",
  "circle_x",
  "deer",
  "1st_aid",
  "back-track"
};

static const char*
d103_symbol_from_icon_number(unsigned int n)
{
  if (n  <= 15) {
    return d103_icons[n];
  } else {
    return "unknown";
  }
}

static int
d103_icon_number_from_symbol(const QString& s)
{
  unsigned int i;

  if (s.isNull()) {
    return 0;
  }

  for (i = 0; i < sizeof(d103_icons) / sizeof(d103_icons[0]); i++) {
    if (0 == (s.compare(d103_icons[i], Qt::CaseInsensitive))) {
      return i;
    }
  }
  return 0;
}
