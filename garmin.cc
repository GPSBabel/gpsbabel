/*
    Jeeps wrapper for Garmin serial protocol.

    Copyright (C) 2002, 2003, 2004, 2005, 2006  Robert Lipe, robertlipe+source@gpsbabel.org

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

#include "garmin.h"

#include <climits>               // for INT_MAX
#include <cmath>                 // for atan2, modf, sqrt
#include <cstdio>                // for fprintf, fflush, snprintf, snprintf
#include <cstdint>               // for int32_t
#include <cstdlib>               // for strtol
#include <cstring>               // for memcpy, strlen, strncpy, strchr
#include <ctime>                 // for time_t
#include <utility>               // for as_const

#include <QByteArray>            // for QByteArray
#include <QChar>                 // for QChar
#include <QList>                 // for QList<>::const_iterator
#include <QString>               // for QString
#include <QTextCodec>            // for QTextCodec
#include <Qt>                    // for CaseInsensitive
#include <QtGlobal>              // for qPrintable, qRound64, Q_INT64_C, qint64

#include "defs.h"
#include "formspec.h"            // for FormatSpecificDataList
#include "garmin_fs.h"           // for garmin_fs_garmin_after_read, garmin_fs_garmin_before_write
#include "garmin_tables.h"       // for gt_find_icon_number_from_desc, PCX, gt_find_desc_from_icon_number
#include "geocache.h"            // for Geocache, Geocache::type_t, Geocache...
#include "grtcirc.h"             // for DEG
#include "jeeps/gpsapp.h"        // for GPS_Set_Baud_Rate, GPS_Init, GPS_Pre...
#include "jeeps/gpscom.h"        // for GPS_Command_Get_Lap, GPS_Command_Get...
#include "jeeps/gpsmem.h"        // for GPS_Track_Del, GPS_Way_Del, GPS_Pvt_Del
#include "jeeps/gpsprot.h"       // for gps_waypt_type, gps_category_type
#include "jeeps/gpssend.h"       // for GPS_SWay, GPS_PWay, GPS_STrack, GPS_...
#include "jeeps/gpsserial.h"     // for DEFAULT_BAUD
#include "jeeps/gpsutil.h"       // for GPS_User, GPS_Enable_Diagnose, GPS_E...
#include "src/core/datetime.h"   // for DateTime
#include "mkshort.h"             // for MakeShort


#define MYNAME "GARMIN"

#define MILITANT_VALID_WAYPT_CHARS "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"


QByteArray GarminFormat::str_from_unicode(const QString& qstr)
{
  return codec->fromUnicode(qstr);
}
QString GarminFormat::str_to_unicode(const QByteArray& cstr)
{
  return codec->toUnicode(cstr);
}

void
GarminFormat::write_char_string(char* dest, const char* source, size_t destsize)
{
  // we zero fill and always terminate within the dest buffer.
  strncpy(dest, source, destsize - 1);
  dest[destsize-1] = 0;
}

void
GarminFormat::rw_init(const QString& fname)
{
  receiver_must_upper = true;
  const char* receiver_charset = "US-ASCII";
  /* Technically, even this is a little loose as spaces aren't allowed */
  const char* valid_waypt_chars = MILITANT_VALID_WAYPT_CHARS " ";

  if (!mkshort_handle) {
    mkshort_handle = new MakeShort;
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
    GPS_Command_Off(qPrintable(fname));
    return;
  }

  if (categorybitsopt) {
    categorybits = strtol(categorybitsopt, nullptr, 0);
  }

  if (baudopt) {
    baud = strtol(baudopt, nullptr, 0);
    switch (baud) {
    case 9600:
    case 19200:
    case 38400:
    case 57600:
    case 115200:
      break;
    default:
      fatal("Baud rate %d is not supported\n", baud);
    }
  }

  if (GPS_Init(qPrintable(fname)) < 0) {
    fatal(MYNAME ":Can't init %s\n", qPrintable(fname));
  }

  /*
   * This is Gross. The B&W Vista sometimes sets its time decades into
   * the future with no way to reset it.  This apparently can "cure"
   * an affected unit.
   */
  if (resettime) {
    GPS_User("Issuing Time Reset...\n");
    GPS_Command_Send_Time(qPrintable(fname), current_time().toTime_t());
    GPS_User("done.\n");
  }

  portname = xstrdup(qPrintable(fname));

  if (baud && baud != DEFAULT_BAUD) {
    if (0 == GPS_Set_Baud_Rate(portname, baud)) {
      gps_baud_rate = baud;
    }
  }

  /*
   * Grope the unit we're talking to to set set_length to
   * 	20 for  the V,
   * 	10 for Street Pilot, (old) Rhino, 76
   * 	6 for the III, 12, emap, and etrex
   * Fortunately, getting this "wrong" only results in ugly names
   * when we're using the synthesize_shortname path.
   */
  int receiver_short_length = 10;

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
      valid_waypt_chars = MILITANT_VALID_WAYPT_CHARS " +-";
      mkshort_handle->set_badchars("\"$.,'!");
      break;

    case 155:	/* Garmin V */
    case 404:	/* SP2720 */
    case 520:	/* SP2820 */
      receiver_short_length = 20;
      break;
    case 382: 	/* C320 */
      receiver_short_length = 30;
      receiver_must_upper = false;
      break;
    case 292: /* (60|76)C[S]x series */
    case 421: /* Vista|Legend Cx */
    case 694: /* Legend HCx */
    case 695: /* Vista HC */
    case 786: /* HC model */
    case 957: /* Legend HC */
      receiver_short_length = 14;
      snwhiteopt = xstrdup("1");
      receiver_must_upper = false;
      /* This might be 8859-1 */
      receiver_charset = "windows-1252";
      break;
    case 291: /* GPSMAP 60CS, probably others */
    case 1095: /* GPS 72H */
      receiver_short_length = 10;
      valid_waypt_chars = MILITANT_VALID_WAYPT_CHARS " +-";
      mkshort_handle->set_badchars("\"$.,'!");
      break;
    case 231: /* Quest */
    case 463: /* Quest 2 */
      receiver_must_upper = false;
      receiver_short_length = 30;
      receiver_charset = "windows-1252";
      break;
    case 577: // Rino 530HCx Version 2.50
      receiver_must_upper = false;
      receiver_short_length = 14;
      break;
    case 429: // Streetpilot i3
      receiver_must_upper = false;
      receiver_charset = "windows-1252";
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

  if (global_opts.debug_level > 0)  {
    fprintf(stderr, "Waypoint type: %d\n"
            "Chosen waypoint length %d\n",
            gps_waypt_type, receiver_short_length);
    if (gps_category_type) {
      fprintf(stderr, "Waypoint category type: %d\n",
              gps_category_type);
    }
  }

  /*
   * If the user provided a short_length, override the calculated value.
   */
  if (snlen) {
    mkshort_handle->set_length(xstrtoi(snlen, nullptr, 10));
  } else {
    mkshort_handle->set_length(receiver_short_length);
  }

  if (snwhiteopt) {
    mkshort_handle->set_whitespace_ok(xstrtoi(snwhiteopt, nullptr, 10));
  }

  /*
   * Until Garmin documents how to determine valid character space
   * for the new models, we just release this safety check manually.
   */
  if (receiver_must_upper) {
    mkshort_handle->set_goodchars(valid_waypt_chars);
  } else {
    mkshort_handle->set_badchars("");
  }

  mkshort_handle->set_mustupper(receiver_must_upper);

  /*
   * This used to mean something when we used cet, but these days this
   * format either uses implicit QString conversions (utf8),
   * or we have hard coded QString::fromLatin1, CSTRc, or CSTR.  These
   * are likely bugs.
   * However, this is still used for garmin_fs_garmin_after_read,
   * garmin_fs_garmin_before_write.
   */
  if (opt_codec != nullptr) {
    // override expected codec with user supplied choice.
    receiver_charset = opt_codec;
  }
  codec = get_codec(receiver_charset);
  if (global_opts.verbose_status) {
    fprintf(stdout, "receiver charset detected as %s.\r\n", receiver_charset);
  }

  valid_chars = valid_waypt_chars;
}

void
GarminFormat::rd_init(const QString& fname)
{
  rw_init(fname);
}

void
GarminFormat::rw_deinit()
{
  if (gps_baud_rate != DEFAULT_BAUD) {
    if (0 == GPS_Set_Baud_Rate(portname, DEFAULT_BAUD)) {
      gps_baud_rate = baud;
    }
  }

  delete mkshort_handle;
  mkshort_handle = nullptr;

  xfree(portname);
  portname = nullptr;

  valid_chars = QString();
}

int
GarminFormat::waypt_read_cb(int total_ct, GPS_PWay* /*unused*/)
{
  if (global_opts.verbose_status) {
    static int i;
    i++;
    waypt_status_disp(total_ct, i);
  }
  return 0;
}

void
GarminFormat::waypt_read()
{
  int n;
  GPS_PWay* way = nullptr;

  if (getposn) {
    auto* wpt = new Waypoint;
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

  for (int i = 0; i < n; i++) {
    auto* wpt_tmp = new Waypoint;

    wpt_tmp->shortname = str_to_unicode(way[i]->ident);
    wpt_tmp->description = str_to_unicode(way[i]->cmnt);
    wpt_tmp->shortname = wpt_tmp->shortname.simplified();
    wpt_tmp->description = wpt_tmp->description.simplified();
    wpt_tmp->longitude = way[i]->lon;
    wpt_tmp->latitude = way[i]->lat;
    if (gps_waypt_type == 103) {
      wpt_tmp->icon_descr = d103_symbol_from_icon_number(
                              way[i]->smbl);
    } else {
      wpt_tmp->icon_descr = gt_find_desc_from_icon_number(
                              way[i]->smbl, PCX);
    }
    /*
     * If a unit doesn't store altitude info (i.e. a D103)
     * gpsmem will default the alt to INT_MAX.  Other units
     * (I can't recall if it was the V (D109) or the Vista (D108)
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

int GarminFormat::lap_read_nop_cb(int /*unused*/, GPS_SWay** /*unused*/)
{
  return 0;
}

// returns 1 if the waypoint's start_time can be found
// in the laps array, 0 otherwise
unsigned int GarminFormat::checkWayPointIsAtSplit(Waypoint* wpt, GPS_PLap* laps, int nlaps)
{
  int result = 0;

  if ((laps != nullptr) && (nlaps > 0)) {
    for (int i = (nlaps-1); i >= 0; i--) {
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

void
GarminFormat::track_read()
{
  GPS_PTrack* array;
  route_head* trk_head = nullptr;
  int trk_num = 0;
  const char* trk_name = "";
  GPS_PLap* laps = nullptr;
  int nlaps = 0;
  int next_is_new_trkseg = 0;

  if (gps_lap_type != -1) {
    nlaps = GPS_Command_Get_Lap(portname, &laps, &lap_read_nop_cb);
  }


  int32_t ntracks = GPS_Command_Get_Track(portname, &array, waypt_read_cb);

  if (ntracks <= 0) {
    return;
  }

  for (int i = 0; i < ntracks; i++) {
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

    if (trk_head == nullptr || array[i]->ishdr) {
      trk_head = new route_head;
      trk_head->rte_num = trk_num;
      trk_head->rte_name = str_to_unicode(trk_name);
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
    auto* wpt = new Waypoint;

    wpt->longitude = array[i]->lon;
    wpt->latitude = array[i]->lat;
    wpt->altitude = array[i]->alt;
    wpt->heartrate = array[i]->heartrate;
    wpt->cadence = array[i]->cadence;
    wpt->shortname = str_to_unicode(array[i]->trk_ident);
    wpt->SetCreationTime(array[i]->Time);
    wpt->wpt_flags.is_split = checkWayPointIsAtSplit(wpt, laps,
                              nlaps);
    wpt->wpt_flags.new_trkseg = next_is_new_trkseg;
    next_is_new_trkseg = 0;

    if (array[i]->dpth < 1.0e25f) {
      wpt->set_depth(array[i]->dpth);
    }
    if (array[i]->temperature_populated) {
      wpt->set_temperature(array[i]->temperature);
    }

    track_add_wpt(trk_head, wpt);
  }

  while (ntracks) {
    GPS_Track_Del(&array[--ntracks]);
  }
  xfree(array);
}

void
GarminFormat::route_read()
{
  GPS_PWay* array;
  /* TODO: Fixes warning but is it right?
   * RJL:  No, the warning isn't right; GCC's flow analysis is broken.
   * still, it's good taste...
   */
  route_head* rte_head = nullptr;

  int32_t nroutepts = GPS_Command_Get_Route(portname, &array);

//	fprintf(stderr, "Routes %d\n", (int) nroutepts);
#if 1
  for (int i = 0; i < nroutepts; i++) {
    if (array[i]->isrte) {
      char* csrc = nullptr;
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
      rte_head = new route_head;
      route_add_head(rte_head);
      if (csrc) {
        rte_head->rte_name = str_to_unicode(csrc);
      }
    } else {
      if (array[i]->islink)  {
        continue;
      } else {
        auto* wpt_tmp = new Waypoint;
        wpt_tmp->latitude = array[i]->lat;
        wpt_tmp->longitude = array[i]->lon;
        wpt_tmp->shortname = str_to_unicode(array[i]->ident);
        route_add_wpt(rte_head, wpt_tmp);
      }
    }
  }
#else
  GPS_Fmt_Print_Route(array, nroutepts, stderr);
#endif

}

/*
 * Rather than propagate Garmin-specific data types outside of the Garmin
 * code, we convert the PVT (position/velocity/time) data from the receiver
 * to the data type we use throughout.   Yes, we do lose some data that way.
 */
void
GarminFormat::pvt2wpt(GPS_PPvt_Data pvt, Waypoint* wpt)
{
  // pvt->alt is height (in meters) above the WGS84 elipsoid.
  // pvt->msl_hght is height (in meters) of WGS84 elipsoid above MSL.
  // wpt->altitude is height (in meters) above geoid (mean sea level).
  // wpt->geoidheight is "Height (in meters) of geoid (mean sea level) above WGS84 earth ellipsoid."
  wpt->set_geoidheight(-pvt->msl_hght);
  wpt->altitude = pvt->alt + pvt->msl_hght;

  wpt->latitude = pvt->lat;
  wpt->longitude = pvt->lon;

  wpt->set_course(180 + DEG(std::atan2(-pvt->east, -pvt->north)));

  /* velocity in m/s */
  wpt->set_speed(std::sqrt(pvt->north*pvt->north + pvt->east*pvt->east));
  // wpt->vs = pvt->up;

  /*
   * The unit reports time in three fields:
   * 1) The # of days to most recent Sun. since  1989-12-31 midnight UTC.
   * 2) The number of seconds (fractions allowed) since that Sunday.
   * 3) The number of leap seconds that offset the current UTC and GPS
   *    reference clocks.
   */
  double tow_integral_part;
  double tow_fractional_part = modf(pvt->tow, &tow_integral_part);
  qint64 seconds = Q_INT64_C(631065600) + pvt->wn_days * Q_INT64_C(86400)  +
                   qRound64(tow_integral_part)
                   - pvt->leap_scnds;
  qint64 milliseconds = qRound64(1000.0 * tow_fractional_part);
  wpt->SetCreationTime(seconds, milliseconds);

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

void
GarminFormat::rd_position_init(const QString& fname)
{
  rw_init(fname);
  GPS_Command_Pvt_On(qPrintable(fname), &pvt_fd);
}

Waypoint*
GarminFormat::rd_position(posn_status* posn_status)
{
  auto* wpt = new Waypoint;
  GPS_PPvt_Data pvt = GPS_Pvt_New();

  if (GPS_Command_Pvt_Get(pvt_fd, &pvt)) {
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

  return nullptr;
}

void
GarminFormat::read()
{
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

GPS_PWay
GarminFormat::sane_GPS_Way_New()
{
  GPS_PWay way = GPS_Way_New();
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
  way->dpth = 1.0e25f;
  way->wpt_class = 0;  // user waypoint by default.

  return way;
}

int
GarminFormat::waypt_write_cb(GPS_PWay* /*unused*/)
{
  int n = waypt_count();

  if (global_opts.verbose_status) {
    static int i;
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
GarminFormat::get_gc_info(const Waypoint* wpt)
{
  if (global_opts.smart_names) {
    if (wpt->gc_data->type == Geocache::type_t::gt_virtual) {
      return  "V ";
    }
    if (wpt->gc_data->type == Geocache::type_t::gt_unknown) {
      return  "? ";
    }
    if (wpt->gc_data->type == Geocache::type_t::gt_multi) {
      return  "Mlt ";
    }
    if (wpt->gc_data->type == Geocache::type_t::gt_earth) {
      return  "EC ";
    }
    if (wpt->gc_data->type == Geocache::type_t::gt_event) {
      return  "Ev ";
    }
    if (wpt->gc_data->container == Geocache::container_t::gc_micro) {
      return  "M ";
    }
    if (wpt->gc_data->container == Geocache::container_t::gc_small) {
      return  "S ";
    }
  }
  return "";
}

int
GarminFormat::waypoint_prepare()
{
  int i;
  int n = waypt_count();
  extern WaypointList* global_waypoint_list;
  int icon;

  tx_waylist = (GPS_SWay**) xcalloc(n,sizeof(*tx_waylist));

  for (i = 0; i < n; i++) {
    tx_waylist[i] = sane_GPS_Way_New();
  }

  i = 0;

  // Iterate with waypt_disp_all?
  for (const Waypoint* wpt : std::as_const(*global_waypoint_list)) {
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
    QByteArray ident = mkshort_handle->mkshort(
                         global_opts.synthesize_shortnames ?
                         str_from_unicode(src) :
                         str_from_unicode(wpt->shortname),
                         false);
    /* Should not be a strcpy as 'ident' isn't really a C string,
     * but rather a garmin "fixed length" buffer that's padded
     * to the end with spaces.  So this is NOT (strlen+1).
     */
    write_char_string(tx_waylist[i]->ident, ident.constData(), sizeof(tx_waylist[i]->ident));

    // If we were explicitly given a comment from GPX, use that.
    //  This logic really is horrible and needs to be untangled.
    if (!wpt->description.isEmpty() &&
        global_opts.smart_names && !wpt->gc_data->diff) {
      write_char_string(tx_waylist[i]->cmnt,
                        str_from_unicode(wpt->description).constData(),
                        sizeof(tx_waylist[i]->cmnt));
    } else {
      if (global_opts.smart_names &&
          wpt->gc_data->diff && wpt->gc_data->terr) {
        static_assert(sizeof(obuf) >= sizeof(tx_waylist[i]->cmnt));
        snprintf(obuf, sizeof(obuf), "%s%u/%u %s",
                 get_gc_info(wpt),
                 wpt->gc_data->diff, wpt->gc_data->terr,
                 str_from_unicode(src).constData());
        write_char_string(tx_waylist[i]->cmnt, obuf, sizeof(tx_waylist[i]->cmnt));
      } else  {
        write_char_string(tx_waylist[i]->cmnt,
                          str_from_unicode(src).constData(),
                          sizeof(tx_waylist[i]->cmnt));
      }
    }



    tx_waylist[i]->lon = wpt->longitude;
    tx_waylist[i]->lat = wpt->latitude;

    if (deficon) {
      icon = gt_find_icon_number_from_desc(deficon, PCX);
    } else {
      if (!wpt->gc_data->get_icon().isEmpty()) {
        icon = gt_find_icon_number_from_desc(wpt->gc_data->get_icon(), PCX);
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
      tx_waylist[i]->category = 1 << (xstrtoi(category, nullptr, 10) - 1);
    }
    if (categorybits) {
      tx_waylist[i]->category = categorybits;
    }
    garmin_fs_garmin_before_write(wpt, tx_waylist[i], gps_waypt_type);
    i++;
  }

  return n;
}

void
GarminFormat::waypoint_write()
{
  int n = waypoint_prepare();

  if (int32_t ret = GPS_Command_Send_Waypoint(portname, tx_waylist, n, waypt_write_cb); ret < 0) {
    fatal(MYNAME ":communication error sending waypoints..\n");
  }

  for (int i = 0; i < n; ++i) {
    GPS_Way_Del(&tx_waylist[i]);
  }
  if (global_opts.verbose_status) {
    fprintf(stdout, "\r\n");
    fflush(stdout);
  }
  xfree(tx_waylist);
}

void
GarminFormat::route_hdr_pr(const route_head* rte)
{
  (*cur_tx_routelist_entry)->rte_num = rte->rte_num;
  (*cur_tx_routelist_entry)->isrte = 1;
  if (!rte->rte_name.isEmpty()) {
    /* for devices that use D201_Rte_Hdr_Type */
    write_char_string((*cur_tx_routelist_entry)->rte_cmnt,
                      str_from_unicode(rte->rte_name).constData(),
                      sizeof((*cur_tx_routelist_entry)->rte_cmnt));
    /* for devices that use D202_Rte_Hdr_Type */
    write_char_string((*cur_tx_routelist_entry)->rte_ident,
                      str_from_unicode(rte->rte_name).constData(),
                      sizeof((*cur_tx_routelist_entry)->rte_ident));
  }
}

void
GarminFormat::route_waypt_pr(const Waypoint* wpt)
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
  if (gps_rte_type == 103) {
    rte->smbl = d103_icon_number_from_symbol(wpt->icon_descr);
  } else {
    rte->smbl = gt_find_icon_number_from_desc(wpt->icon_descr, PCX);
  }

  // map class so unit doesn't duplicate routepoints as a waypoint.
  rte->wpt_class = 0x80;

  if (wpt->altitude != unknown_alt) {
    rte->alt = wpt->altitude;
  } else {
    rte->alt_is_unknown = 1;
    rte->alt = 0;
  }

  QString cleanname = wpt->shortname;
  /*
   * Until Garmin documents how to determine valid character space
   * for the new models, we just release this safety check manually.
   */
  if (receiver_must_upper) {
    auto isInvalidChar = [this](const QChar &ch)->bool {
      return !valid_chars.contains(ch);
    };
    cleanname = cleanname.toUpper().removeIf(isInvalidChar);
  }
  write_char_string(rte->ident,
                    str_from_unicode(cleanname).constData(),
                    sizeof(rte->ident));

  if (wpt->description.isEmpty()) {
    rte->cmnt[0] = 0;
  } else {
    write_char_string(rte->cmnt,
                      str_from_unicode(wpt->description).constData(),
                      sizeof(rte->cmnt));
  }
  cur_tx_routelist_entry++;
}

void
GarminFormat::route_write()
{
  const int n = 2 * route_waypt_count(); /* Doubled for the islink crap. */

  tx_routelist = (GPS_SWay**) xcalloc(n,sizeof(GPS_PWay));
  cur_tx_routelist_entry = tx_routelist;

  for (int i = 0; i < n; i++) {
    tx_routelist[i] = sane_GPS_Way_New();
  }

  auto route_hdr_pr_lambda = [this](const route_head* rte)->void {
    route_hdr_pr(rte);
  };
  auto route_waypt_pr_lambda = [this](const Waypoint* waypointp)->void {
    route_waypt_pr(waypointp);
  };
  route_disp_all(route_hdr_pr_lambda, nullptr, route_waypt_pr_lambda);
  GPS_Command_Send_Route(portname, tx_routelist, n);

  for (int i = 0; i < n; i++) {
    GPS_Way_Del(&tx_routelist[i]);
  }

  xfree(tx_routelist);
}

void
GarminFormat::track_hdr_pr(const route_head* trk_head)
{
  (*cur_tx_tracklist_entry)->ishdr = true;
  if (!trk_head->rte_name.isEmpty()) {
    write_char_string((*cur_tx_tracklist_entry)->trk_ident,
                      str_from_unicode(trk_head->rte_name).constData(),
                      sizeof((*cur_tx_tracklist_entry)->trk_ident));
  } else {
    snprintf((*cur_tx_tracklist_entry)->trk_ident, sizeof((*cur_tx_tracklist_entry)->trk_ident), "TRACK%02d", my_track_count);
  }
  cur_tx_tracklist_entry++;
  my_track_count++;
}

void
GarminFormat::track_waypt_pr(const Waypoint* wpt)
{
  (*cur_tx_tracklist_entry)->lat = wpt->latitude;
  (*cur_tx_tracklist_entry)->lon = wpt->longitude;
  (*cur_tx_tracklist_entry)->alt = (wpt->altitude != unknown_alt) ? wpt->altitude : 1e25;
  (*cur_tx_tracklist_entry)->Time = wpt->GetCreationTime().toTime_t();
  if (!wpt->shortname.isEmpty()) {
    write_char_string((*cur_tx_tracklist_entry)->trk_ident,
                      str_from_unicode(wpt->shortname).constData(),
                      sizeof((*cur_tx_tracklist_entry)->trk_ident));
  }
  (*cur_tx_tracklist_entry)->tnew = wpt->wpt_flags.new_trkseg;
  cur_tx_tracklist_entry++;
}

int
GarminFormat::track_prepare()
{
  int32_t n = track_waypt_count() + track_count();

  tx_tracklist = (GPS_STrack**) xcalloc(n, sizeof(GPS_PTrack));
  cur_tx_tracklist_entry = tx_tracklist;
  for (int i = 0; i < n; i++) {
    tx_tracklist[i] = GPS_Track_New();
  }
  my_track_count = 0;
  auto track_hdr_pr_lambda = [this](const route_head* rte)->void {
    track_hdr_pr(rte);
  };
  auto track_waypt_pr_lambda = [this](const Waypoint* waypointp)->void {
    track_waypt_pr(waypointp);
  };
  track_disp_all(track_hdr_pr_lambda, nullptr, track_waypt_pr_lambda);

  GPS_Prepare_Track_For_Device(&tx_tracklist, &n);

  return n;
}

void
GarminFormat::track_write()
{
  int n = track_prepare();
  GPS_Command_Send_Track(portname, tx_tracklist, n, (eraset)? 1 : 0);

  for (int i = 0; i < n; i++) {
    GPS_Track_Del(&tx_tracklist[i]);
  }
  xfree(tx_tracklist);
}

void
GarminFormat::course_write()
{
  int i;

  int n_wpt = waypoint_prepare();
  int n_trk = track_prepare();

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

void
GarminFormat::write()
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

const char*
GarminFormat::d103_symbol_from_icon_number(unsigned int n)
{
  if (n  <= 15) {
    return d103_icons[n];
  } else {
    return "unknown";
  }
}

int
GarminFormat::d103_icon_number_from_symbol(const QString& s)
{
  if (s.isNull()) {
    return 0;
  }

  for (unsigned int i = 0; i < sizeof(d103_icons) / sizeof(d103_icons[0]); i++) {
    if (0 == (s.compare(d103_icons[i], Qt::CaseInsensitive))) {
      return i;
    }
  }
  return 0;
}

void
GarminFormat::garmin_fs_garmin_after_read(const GPS_PWay way, Waypoint* wpt, const int protoid)
{
  auto* gmsd = new garmin_fs_t(protoid);
  wpt->fs.FsChainAdd(gmsd);

  /* nothing happens until gmsd is allocated some lines above */

  /* !!! class needs protocol specific conversion !!! (ToDo)
  garmin_fs_t::set_wpt_class(gmsd, way[i]->wpt_class);
  */
  /* flagged data fields */
  garmin_fs_t::set_display(gmsd, gt_switch_display_mode_value(way->dspl, gps_waypt_type, 1));
  if (way->category != 0) {
    garmin_fs_t::set_category(gmsd, way->category);
  }
  if (way->dst < 1.0e25f) {
    wpt->set_proximity(way->dst);
  }
  if (way->temperature_populated) {
    wpt->set_temperature(way->temperature);
  }
  if (way->dpth < 1.0e25f) {
    wpt->set_depth(way->dpth);
  }
  /* will copy until a null character or the end of the fixed length way field is reached, whichever comes first. */
  garmin_fs_t::set_cc(gmsd, str_to_unicode(QByteArray(way->cc, qstrnlen(way->cc, sizeof(way->cc)))));
  garmin_fs_t::set_city(gmsd, str_to_unicode(QByteArray(way->city, qstrnlen(way->city, sizeof(way->city)))));
  garmin_fs_t::set_state(gmsd, str_to_unicode(QByteArray(way->state, qstrnlen(way->state, sizeof(way->state)))));
  garmin_fs_t::set_facility(gmsd, str_to_unicode(QByteArray(way->facility, qstrnlen(way->facility, sizeof(way->facility)))));
  garmin_fs_t::set_cross_road(gmsd, str_to_unicode(QByteArray(way->cross_road, qstrnlen(way->cross_road, sizeof(way->cross_road)))));
  garmin_fs_t::set_addr(gmsd, str_to_unicode(QByteArray(way->addr, qstrnlen(way->addr, sizeof(way->addr)))));
}

void
GarminFormat::garmin_fs_garmin_before_write(const Waypoint* wpt, GPS_PWay way, const int protoid)
{
  const garmin_fs_t* gmsd = garmin_fs_t::find(wpt);

  (void)protoid; // unused for now.

  if (gmsd == nullptr) {
    return;
  }

  /* ToDo: protocol specific conversion of class
  way[i]->wpt_class = garmin_fs_t::get_wpt_class(gmsd, way[i]->wpt_class);
  	*/
  way->dspl = gt_switch_display_mode_value(
                garmin_fs_t::get_display(gmsd, way->dspl), gps_waypt_type, 0);
  way->category = garmin_fs_t::get_category(gmsd, way->category);
  if (wpt->depth_has_value()) {
    way->dpth = wpt->depth_value();
  }
  if (wpt->proximity_has_value()) {
    way->dst = wpt->proximity_value();
  }
  if (wpt->temperature_has_value()) {
    way->temperature = wpt->temperature_value();
  }

  /* destination may not be null terminated, but we will fill with nulls if necessary */
  strncpy(way->cc, str_from_unicode(garmin_fs_t::get_cc(gmsd, nullptr)).constData(), sizeof(way->cc));
  strncpy(way->city, str_from_unicode(garmin_fs_t::get_city(gmsd, nullptr)).constData(), sizeof(way->city));
  strncpy(way->state, str_from_unicode(garmin_fs_t::get_state(gmsd, nullptr)).constData(), sizeof(way->state));
  strncpy(way->facility, str_from_unicode(garmin_fs_t::get_facility(gmsd, nullptr)).constData(), sizeof(way->facility));
  strncpy(way->cross_road, str_from_unicode(garmin_fs_t::get_cross_road(gmsd, nullptr)).constData(), sizeof(way->cross_road));
  strncpy(way->addr, str_from_unicode(garmin_fs_t::get_addr(gmsd, nullptr)).constData(), sizeof(way->addr));
}
