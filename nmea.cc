/*
	Read files containing selected NMEA 0183 sentences.
	Based on information by Eino Uikkanenj

	Copyright (C) 2004-2015 Robert Lipe, robertlipe+source@gpsbabel.org

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

#include <cctype>                  // for isprint
#include <cmath>                   // for fabs
#include <cstdio>                  // for snprintf, sscanf, fprintf, fputc, stderr
#include <cstdlib>                 // for atoi, atof, strtod
#include <cstring>                 // for strncmp, strchr, strlen, strstr, memset, strrchr
#include <iostream>                // for std::cout
#include <iterator>                // for operator!=, reverse_iterator

#include <QtCore/QByteArray>       // for QByteArray
#include <QtCore/QChar>            // for QChar, operator==, operator!=
#include <QtCore/QDateTime>        // for QDateTime
#include <QtCore/QDebug>           // for QDebug
#include <QtCore/QList>            // for QList
#include <QtCore/QString>          // for QString
#include <QtCore/QStringList>      // for QStringList
#include <QtCore/QTextStream>      // for hex
#include <QtCore/QThread>          // for QThread
#include <QtCore/QTime>            // for QTime
#include <QtCore/Qt>               // for UTC
#include <QtCore/QtGlobal>         // for qPrintable, foreach

#include "defs.h"
#include "nmea.h"
#include "cet_util.h"              // for cet_convert_init
#include "gbfile.h"                // for gbfprintf, gbfflush, gbfclose, gbfopen, gbfgetstr, gbfile
#include "gbser.h"                 // for gbser_set_speed, gbser_flush, gbser_read_line, gbser_deinit, gbser_init, gbser_write
#include "jeeps/gpsmath.h"         // for GPS_Lookup_Datum_Index, GPS_Math_Known_Datum_To_WGS84_M
#include "src/core/datetime.h"     // for DateTime
#include "src/core/logging.h"      // for Warning


/**********************************************************

   ' 1      2      3        4 5         6 7 8  9   10   11 12  13 14 15
   ' $GPGGA - Global Positioning System Fix Data
   ' $GPGGA,155537,6006.718,N,02426.290,E,1,05,2.4,50.5,M,19.7,M,,*79
   '  2    123519       Fix taken at 12:35:19 UTC
   '  3,4  4807.038,N   Latitude 48 deg 07.038' N
   '  5,6  01131.324,E  Longitude 11 deg 31.324' E
   '  7    1            Fix quality: 0 = invalid
   '                                 1 = GPS fix
   '                                 2 = DGPS fix
   '  8    08           Number of satellites being tracked
   '  9    0.9          Horizontal dilution of position
   ' 10,11 545.4,M      Altitude, Metres, above mean sea level
   ' 12,13 46.9,M       Height of geoid (mean sea level) above WGS84 ellipsoid
   ' 14    (empty field) time in seconds since last DGPS update
   ' 15    (empty field) DGPS station ID number

   ' $GPWPL - waypoint location
   ' $GPWPL,4917.16,N,12310.64,W,003*65
   '  2,3  4917.16,N    Latitude of waypoint
   '  4,5  12310.64,W   Longitude of waypoint
   '  6    003          Waypoint ID

   ' $GPGLL - Geographic position, Latitude and Longitude
   ' $GPGLL,4916.45,N,12311.12,W,225444,A
   '  2,3  4916.46,N    Latitude 49 deg. 16.45 min. North
   '  4,5  12311.12,W   Longitude 123 deg. 11.12 min. West
   '  6    225444       Fix taken at 22:54:44 UTC
   '  7    A            Data valid

   ' $GPRMC - Recommended minimum specific GNSS Data
   ' $GPRMC,085721.194,A,5917.7210,N,01103.9227,E,21.42,50.33,300504,,*07
   '  2    085721       Fix taken at 08:57:21 UTC
   '  3    A		Fix valid (this field reads V if fix is not valid)
   '  4,5  5917.7210,N   Latitude 59 deg 17.7210' N
   '  6,7  01103.9227,E  Longitude 11 deg 03.9227' E
   '  8    21.42	Speed over ground (knots)
   '  9    50.33	Course over ground (true)
   ' 10   300504	Date 30/05-2004
   '  11   Empty field	Magnetic variation

	  GSA - GPS DOP and active satellites
	  $GPGSA,A,3,04,05,,09,12,,,24,,,,,2.5,1.3,2.1*39
	       A            Auto selection of 2D or 3D fix (M = manual)
	       3            3D fix
	       04,05...     PRNs of satellites used for fix (space for 12)
	       2.5          PDOP (dilution of precision)
	       1.3          Horizontal dilution of precision (HDOP)
	       2.1          Vertical dilution of precision (VDOP)
	         DOP is an indication of the effect of satellite geometry on
	         the accuracy of the fix.

	  VTG - Track made good and ground speed
	  $GPVTG,054.7,T,034.4,M,005.5,N,010.2,K
	       054.7,T      True track made good
	       034.4,M      Magnetic track made good
	       005.5,N      Ground speed, knots
	       010.2,K      Ground speed, Kilometers per hour

	  WPL - waypoint location
	  $GPWPL,4917.16,N,12310.64,W,003*65
	       4917.16,N    Latitude of waypoint
	       12310.64,W   Longitude of waypoint
	       003          Waypoint ID
	         When a route is active, this sentence is sent once for each
	         waypoint in the route, in sequence. When all waypoints have
	         been reported, GPR00 is sent in the next data set. In any
	         group of sentences, only one WPL sentence, or an R00
	         sentence, will be sent.


   ' The optional checksum field consists of a "*" and two hex digits repre-
   ' senting the exclusive OR of all characters between, but not including,
   ' the "$" and "*".  A checksum is required on some sentences.

****************************************/

/*
 * An input file may have both GGA and GLL and RMC sentences for the exact
 * same position fix. If we see a single GGA, start ignoring GLL's and RMC's.
 *	GLL's will also be ignored if RMC's are found and GGA's not found.
 */

/*
Zmarties notes:

In practice, all fields of the NMEA sentences should be treated as optional -
if the data is not available, then the field can be omitted (hence leading
to the output of two consecutive commas).

An NMEA recording can start anywhere in the stream of data.  It is therefore
necessary to discard sentences until sufficient data has been processed to
have all the necessary data to construct a waypoint.  In practice, this means
discarding data until we have had the first sentence that provides the date.
(We could scan forwards in the stream of data to find the first date, and
then back apply it to all previous sentences, but that is probably more
complexity that is necessary - the lost of one waypoint at the start of the
stream can normally be tolerated.)

If a sentence is received without a checksum, but previous sentences have
had checksums, it is best to discard that sentence.  In practice, the only
time I have seen this is when the recording stops suddenly, where the last
sentence is truncated - and missing part of the line, including the checksum.
*/

#define MYNAME "nmea"
#define CHECK_BOOL(a) if ((a) && (*(a) == '0')) (a) = NULL

/*
 * Slightly different than the Magellan checksum fn.
 */
int
NmeaFormat::nmea_cksum(const char* const buf)
{
  int x = 0 ;

  for (const char* p = buf; *p; p++) {
    x ^= *p;
  }
  return x;
}

Waypoint*
NmeaFormat::nmea_new_wpt()
{
  auto* wpt = new Waypoint();
  // Set extra data to something other than nullptr to indicate to
  // nmea_release_wpt that ownership of the waypoint is our
  // repsonsibility.
  wpt->extra_data = &wpt_not_added_yet;
  return wpt;
}

void
NmeaFormat::nmea_add_wpt(Waypoint* wpt, route_head* trk) const
{
  // Reset extra data.
  // This also indicates to nmea_release_wpt that ownership has been
  // transferred to either the global_waypoint_list or global_track_list.
  wpt->extra_data = nullptr;
  if (datum != DATUM_WGS84) {
    double lat, lon, alt;
    GPS_Math_Known_Datum_To_WGS84_M(
      wpt->latitude, wpt->longitude, 0,
      &lat, &lon, &alt, datum);
    wpt->latitude = lat;
    wpt->longitude = lon;
  }
  if (trk != nullptr) {
    track_add_wpt(trk, wpt);
  } else {
    waypt_add(wpt);
  }
}

void
NmeaFormat::nmea_release_wpt(Waypoint* wpt)
{
  if (wpt && (wpt->extra_data != nullptr)) {
    // Ownership of this waypoint hasn't been transferred to a global
    // list, so we must delete it to avoid a memory leak.
    delete wpt;
  }
}

void
NmeaFormat::rd_init(const QString& fname)
{
  curr_waypt = nullptr;
  last_waypt = nullptr;
  datum = DATUM_WGS84;
  had_checksum = false;

  CHECK_BOOL(opt_gprmc);
  CHECK_BOOL(opt_gpgga);
  CHECK_BOOL(opt_gpvtg);
  CHECK_BOOL(opt_gpgsa);
  CHECK_BOOL(opt_gisteq);

  pcmpt_head.clear();

  if (getposnarg) {
    getposn = 1;
  }

  /* A special case hack that gets our current position and returns
   * it as one waypoint.
   */
  if (getposn) {
    posn_status st;
    rd_position_init(fname);
    Waypoint* wpt = rd_position(&st);
    if (!wpt) {
      return;
    }
    wpt->shortname = "Position";
    nmea_add_wpt(wpt, nullptr);
    return;
  }

  read_mode = rm_file;
  file_in = gbfopen(fname, "rb", MYNAME);
}

void
NmeaFormat::rd_deinit()
{
  switch (read_mode) {
  case rm_serial:
    gbser_deinit(gbser_handle);
    break;
  case rm_file:
    gbfclose(file_in);
    file_in = nullptr;
    break;
  default:
    fatal("nmea_rd_deinit: illegal read_mode.\n");
    break;
  }

  posn_fname.clear();

  int warnings = 0;
  for (const auto& [key, value] : counter) {
    if (value > 0) {
      warnings++;
      std::cout << MYNAME << ": discarded lines with " << key << " = " << value << "\n";
    }
  }

  if (warnings) {
    std::cout << "\n";
  }

}

void
NmeaFormat::wr_init(const QString& fname)
{
  CHECK_BOOL(opt_gprmc);
  CHECK_BOOL(opt_gpgga);
  CHECK_BOOL(opt_gpvtg);
  CHECK_BOOL(opt_gpgsa);
  CHECK_BOOL(opt_gisteq);

  append_output = strtod(opt_append, nullptr);

  file_out = gbfopen(fname, append_output ? "a+" : "w+", MYNAME);

  sleepms = -1;
  if (opt_sleep) {
    if (*opt_sleep) {
      sleepms = 1e3 * atof(opt_sleep);
    } else {
      sleepms = -1;
    }
  }

  mkshort_handle = mkshort_new_handle();
  setshort_length(mkshort_handle, atoi(snlenopt));

  if (opt_gisteq) {
    opt_gpgga = nullptr;
    opt_gpvtg = nullptr;
    opt_gpgsa = nullptr;
  }
}

void
NmeaFormat::wr_deinit()
{
  gbfclose(file_out);
  mkshort_del_handle(&mkshort_handle);
}

void
NmeaFormat::nmea_set_waypoint_time(Waypoint* wpt, QDateTime* prev, const QDate& date, const QTime& time)
{
  if (date.isValid()) {
    wpt->SetCreationTime(QDateTime(date, time, Qt::UTC));
    if (wpt->wpt_flags.fmt_use != 0) {
      wpt->wpt_flags.fmt_use = 0;
      without_date--;
    }
    *prev = wpt->GetCreationTime();
  } else if (prev->date().isValid()) {
    wpt->SetCreationTime(QDateTime(prev->date(), time, Qt::UTC));
    if (*prev > wpt->creation_time) {
      /* go over midnight ? */
      wpt->creation_time = wpt->creation_time.addDays(1);
    }
    if (wpt->wpt_flags.fmt_use != 0) {
      wpt->wpt_flags.fmt_use = 0;
      without_date--;
    }
    *prev = wpt->GetCreationTime();
  } else {
    wpt->SetCreationTime(QDateTime(QDate(), time, Qt::UTC));
    if (wpt->wpt_flags.fmt_use == 0) {
      wpt->wpt_flags.fmt_use = 1;
      without_date++;
    }
  }
}

QTime NmeaFormat::nmea_parse_hms(const QString& str)
{
  QString format = str.contains('.')? "hhmmss.zzz" : "hhmmss";
  return QTime::fromString(str, format);
}

void
NmeaFormat::gpgll_parse(const QString& ibuf)
{
  if (trk_head == nullptr) {
    trk_head = new route_head;
    track_add_head(trk_head);
  }

  const QStringList fields = ibuf.split(',');

  double latdeg = 0;
  if (fields.size() > 1) latdeg = fields[1].toDouble();
  QChar latdir = 'N';
  if (fields.size() > 2) latdir = fields[2][0];
  double lngdeg = 0;
  if (fields.size() > 3) lngdeg = fields[3].toDouble();
  QChar lngdir = 'E';
  if (fields.size() > 4) lngdir = fields[4][0];
  QTime hms;
  if (fields.size() > 5) hms = nmea_parse_hms(fields[5]);
  bool valid = false;
  if (fields.size() > 6) valid = fields[6].startsWith('A');

  if (!valid) {
    counter["invalid_gpgll"]++;
    return;
  }

  last_read_time = hms;

  Waypoint* waypt = nmea_new_wpt();

  nmea_set_waypoint_time(waypt, &prev_datetime, QDate(), hms);

  if (latdir == 'S') {
    latdeg = -latdeg;
  }
  waypt->latitude = ddmm2degrees(latdeg);

  if (lngdir == 'W') {
    lngdeg = -lngdeg;
  }
  waypt->longitude = ddmm2degrees(lngdeg);

  nmea_release_wpt(curr_waypt);
  curr_waypt = waypt;
}

void
NmeaFormat::gpgga_parse(const QString& ibuf)
{
  if (trk_head == nullptr) {
    trk_head = new route_head;
    track_add_head(trk_head);
  }

  const QStringList fields = ibuf.split(',');
  QTime hms;
  if (fields.size() > 1) hms = nmea_parse_hms(fields[1]);
  double latdeg = 0;
  if (fields.size() > 2) latdeg = fields[2].toDouble();
  QChar latdir = 'N';
  if (fields.size() > 3) latdir = fields[3][0];
  double lngdeg = 0;
  if (fields.size() > 4) lngdeg = fields[4].toDouble();
  QChar lngdir = 'W';
  if (fields.size() > 5) lngdir = fields[5][0];
  int fix = fix_unknown;
  if (fields.size() > 6) fix = fields[6].toInt();
  int nsats = 0;
  if (fields.size() > 7) nsats = fields[7].toInt();
  float hdop = 0;
  if (fields.size() > 8) hdop = fields[8].toFloat();
  double alt = unknown_alt;
  if (fields.size() > 9) alt = fields[9].toDouble();
  QChar altunits ='M';
  if (fields.size() > 10) altunits = fields[10][0];
  double geoidheight = unknown_alt;
  if (fields.size() > 11) geoidheight = fields[11].toDouble();
  QChar geoidheightunits = 'M';
  if (fields.size() > 12) geoidheightunits = fields[12][0];

  /*
   * In serial mode, allow the fix with an invalid position through
   * (unless if the position lat/lng is absent or completely bogus)
   * as serial units will often spit a remembered position up and
   * that is more comfortable than nothing at all...
   */
  CHECK_BOOL(opt_ignorefix);
  if ((fix <= 0) && (read_mode != rm_serial || (latdeg == 0.0 && lngdeg == 0.0)) && (!opt_ignorefix)) {
    counter["invalid_gpgga"]++;
    return;
  }

  last_read_time = hms;

  Waypoint* waypt = nmea_new_wpt();

  nmea_set_waypoint_time(waypt, &prev_datetime, QDate(), hms);

  if (latdir == 'S') {
    latdeg = -latdeg;
  }
  waypt->latitude = ddmm2degrees(latdeg);

  if (lngdir == 'W') {
    lngdeg = -lngdeg;
  }
  waypt->longitude = ddmm2degrees(lngdeg);

  waypt->altitude = alt;

  WAYPT_SET(waypt, geoidheight, geoidheight);

  waypt->sat = nsats;

  waypt->hdop = hdop;

  switch (fix) {
    case 0:
      waypt->fix = fix_none;
      break;
    case 1:
      waypt->fix = (nsats > 3) ? (fix_3d) : (fix_2d);
      break;
    case 2:
      waypt->fix = fix_dgps;
      break;
    case 3:
      waypt->fix = fix_pps;
      break;
    default:
      break;
  }

  nmea_release_wpt(curr_waypt);
  curr_waypt = waypt;
}

void
NmeaFormat::gprmc_parse(const QString& ibuf)
{
  if (trk_head == nullptr) {
    trk_head = new route_head;
    track_add_head(trk_head);
  }

  const QStringList fields = ibuf.split(',');
  QTime hms;
  if (fields.size() > 1) hms = nmea_parse_hms(fields[1]);
  QChar fix = 'V'; // V == "Invalid"
  if (fields.size() > 2) fix = fields[2][0];
  double latdeg = 0;
  if (fields.size() > 3) latdeg = fields[3].toDouble();
  QChar latdir = 'N';
  if (fields.size() > 4) latdir = fields[4][0];
  double lngdeg = 0;
  if (fields.size() > 5) lngdeg = fields[5].toDouble();
  QChar lngdir = 'W';
  if (fields.size() > 6) lngdir = fields[6][0];
  double speed = 0;
  if (fields.size() > 7) speed = fields[7].toDouble();
  double course = 0;
  if (fields.size() > 8) course = fields[8].toDouble();
  QDate dmy;
  if (fields.size() > 9) {
    QString datestr(fields[9]);
    datestr.insert(4, "20");
    dmy = QDate::fromString(datestr, "ddMMyyyy");
  }
  if (fix != 'A') {
    /* ignore this fix - it is invalid */
    counter["invalid_gprmc"] = 0;
    return;
  }

  last_read_time = hms;

  if (posn_type == gpgga) {
    /* capture useful data update and exit */
    if (curr_waypt) {
      if (! WAYPT_HAS(curr_waypt, speed)) {
        WAYPT_SET(curr_waypt, speed, KNOTS_TO_MPS(speed));
      }
      if (! WAYPT_HAS(curr_waypt, course)) {
        WAYPT_SET(curr_waypt, course, course);
      }
      /* The change of date wasn't recorded when
       * going from 235959 to 000000. */
      nmea_set_waypoint_time(curr_waypt, &prev_datetime, dmy, hms);
    }
    /* This point is both a waypoint and a trackpoint. */
    if (amod_waypoint) {
      nmea_add_wpt(new Waypoint(*curr_waypt), nullptr);
      amod_waypoint = false;
    }
    return;
  }

  Waypoint* waypt = nmea_new_wpt();

  WAYPT_SET(waypt, speed, KNOTS_TO_MPS(speed));
  WAYPT_SET(waypt, course, course);

  nmea_set_waypoint_time(waypt, &prev_datetime, dmy, hms);

  if (latdir == 'S') {
    latdeg = -latdeg;
  }
  waypt->latitude = ddmm2degrees(latdeg);

  if (lngdir == 'W') {
    lngdeg = -lngdeg;
  }
  waypt->longitude = ddmm2degrees(lngdeg);

  nmea_release_wpt(curr_waypt);
  curr_waypt = waypt;

  /* This point is both a waypoint and a trackpoint. */
  if (amod_waypoint) {
    nmea_add_wpt(new Waypoint(*waypt), nullptr);
    amod_waypoint = false;
  }
}

void
NmeaFormat::gpwpl_parse(const QString& ibuf)
{
  const QStringList fields = ibuf.split(',');

  double latdeg = 0;
  if (fields.size() > 1) latdeg = fields[1].toDouble();
  QChar latdir = 'N';
  if (fields.size() > 2) latdir = fields[2][0];
  double lngdeg = 0;
  if (fields.size() > 3) lngdeg = fields[3].toDouble();
  QChar lngdir = 'E';
  if (fields.size() > 4) lngdir = fields[4][0];
  QString sname;
  if (fields.size() > 5) sname = fields[5];

  if (latdir == 'S') {
    latdeg = -latdeg;
  }
  if (lngdir == 'W') {
    lngdeg = -lngdeg;
  }

  Waypoint* waypt = nmea_new_wpt();
  waypt->latitude = ddmm2degrees(latdeg);
  waypt->longitude = ddmm2degrees(lngdeg);
  waypt->shortname = sname;

  curr_waypt = nullptr; /* waypoints won't be updated with GPS fixes */
  nmea_add_wpt(waypt, nullptr);
}

void
NmeaFormat::gpzda_parse(const QString& ibuf)
{
  const QStringList fields = ibuf.split(',');
  if (fields.size() > 4) {
    QTime time = nmea_parse_hms(fields[1]);
    QString datestr = QString("%1%2%3").arg(fields[2], fields[3], fields[4]);
    QDate date = QDate::fromString(datestr, "ddMMyyyy");

    // The prev_datetime data member might be used by
    // nmea_fix_timestamps and nmea_set_waypoint_time.
    prev_datetime = QDateTime(date, time, Qt::UTC);
  }
}

// This function has had a hard life. It began as a moderately terrifying
// bunch of nested sscanfs. In 2017, robertl replaced the scanf stuff with
// a QString::split() to make it more tolerant of really empty fields from
// certain GPS implementations, but didn't replace the (somewhat funky) back
// half to match the parse. There are definitely some readability issues
// here.
// The numbering as per http://aprs.gids.nl/nmea/#gsa was the reference as
// the field numbers conveniently match our index.
void
NmeaFormat::gpgsa_parse(const QString& ibuf) const
{
  int  prn[12] = {0};
  memset(prn,0xff,sizeof(prn));

  const QStringList fields = ibuf.split(',');
  int nfields = fields.size();
  // 0 = "GPGSA"
  // 1 = Mode. Ignored
  QChar fix;
  if (nfields > 2) {
    fix = fields[2][0];
  }

  // 12 fields, index 3 through 14.
  for (int cnt = 0; cnt <= 11; cnt++) {
    if (nfields > cnt + 3) prn[cnt] = fields[cnt + 3].toInt();
  }

  float pdop = 0, hdop = 0, vdop = 0;
  if (nfields > 15) pdop = fields[15].toFloat();
  if (nfields > 16) hdop = fields[16].toFloat();
  if (nfields > 17) vdop = fields[17].toFloat();

  if (curr_waypt) {
    if (curr_waypt->fix!=fix_dgps) {
      if (fix=='3') {
        curr_waypt->fix = fix_3d;
      } else if (fix=='2') {
        curr_waypt->fix = fix_2d;
      }
    }

    curr_waypt->pdop = pdop;
    curr_waypt->hdop = hdop;
    curr_waypt->vdop = vdop;

    if (curr_waypt->sat <= 0) {
      for (int cnt = 0; cnt <= 11; cnt++) {
        curr_waypt->sat += (prn[cnt] > 0) ? 1 : 0;
      }
    }
  }

}

void
NmeaFormat::gpvtg_parse(const QString& ibuf) const
{
  const QStringList fields = ibuf.split(',');
  double course = 0;
  if (fields.size() > 1) course = fields[1].toDouble();
  double speed_n = 0;
  if (fields.size() > 5) speed_n = fields[5].toDouble();
  double speed_k = 0;
  if (fields.size() > 7) speed_k = fields[7].toDouble();

  if (curr_waypt) {
    WAYPT_SET(curr_waypt, course, course);
    if (speed_k > 0) {
      WAYPT_SET(curr_waypt, speed, KPH_TO_MPS(speed_k));
    } else {
      WAYPT_SET(curr_waypt, speed, KNOTS_TO_MPS(speed_n));
    }
  }

}

/*
 *  AVMAP EKP-IV Tracks - a proprietary (and very weird) extended NMEA.
 * https://sourceforge.net/tracker/?func=detail&atid=489478&aid=1640814&group_id=58972
 */
double NmeaFormat::pcmpt_deg(int d)
{
  int deg = d  / 100000;
  double minutes = (((d / 100000.0) - deg) * 100) / 60.0;
  return (double) deg + minutes;
}

void
NmeaFormat::pcmpt_parse(const char* ibuf)
{
  int i, j1, j2, j3, j4, j5, j6;
  int lat, lon;
  char altflag, u1, u2;
  float alt, f1, f2;
  char coords[20] = {0};
  int dmy, hms;

  dmy = hms = 0;

  sscanf(ibuf,"$PCMPT,%d,%d,%d,%c,%f,%d,%19[^,],%d,%f,%d,%f,%c,%d,%c,%d",
         &j1, &j2, &j3, &altflag, &alt, &j4, coords,
         &j5, &f1, &j6, &f2, &u1, &dmy, &u2, &hms);

  if (altflag == 'D' && curr_waypt && alt > 0) {
    curr_waypt->altitude =  alt /*+ 500*/;
    return;
  }

  /*
   * There are a couple of different second line records, but we
   * don't care about them.
   */
  if (j2 != 1) {
    return;
  }

  sscanf(coords, "%d%n", &lat, &i);
  if (coords[i] == 'S') {
    lat = -lat;
  }
  sscanf(coords + i + 1, "%d%n", &lon, &i);
  if (coords[i] == 'W') {
    lon= -lon;
  }

  if (lat || lon) {
    curr_waypt = nmea_new_wpt();
    curr_waypt->longitude = pcmpt_deg(lon);
    curr_waypt->latitude = pcmpt_deg(lat);

    int sec = hms % 100;
    hms = hms / 100;
    int min = hms % 100;
    hms = hms / 100;
    int hour =  hms % 100;
    QTime time = QTime(hour, min, sec);

    int year = dmy % 10000;
    dmy = dmy / 10000;
    int mon  = dmy % 100;
    dmy = dmy / 100;
    int mday = dmy;
    QDate date = QDate(year, mon, mday);

    nmea_set_waypoint_time(curr_waypt, &prev_datetime, date, time);
    pcmpt_head.prepend(curr_waypt);
  } else {

    if (pcmpt_head.isEmpty()) {
      return;
    }

    /*
     * Since we oh-so-cleverly inserted points at the head,
     * we can rip through the queue forward now to get our
     * handy-dandy reversing effect.
     */
    auto* trk_head_2 = new route_head;
    track_add_head(trk_head_2);
    while (!pcmpt_head.isEmpty()) {
      Waypoint* wpt = pcmpt_head.takeFirst();
      nmea_add_wpt(wpt, trk_head_2);
    }
  }
}

void
NmeaFormat::nmea_fix_timestamps(route_head* track)
{
  if ((trk_head == nullptr) || (without_date == 0)) {
    return;
  }

  if (!prev_datetime.date().isValid()) {
    if (optdate == nullptr) {
      warning(MYNAME ": No date found within track (all points dropped)!\n");
      warning(MYNAME ": Please use option \"date\" to preset a valid date for those tracks.\n");
      track_del_head(track);
      return;
    }

    QDateTime prev = QDateTime(opt_tm, QTime(0, 0), Qt::UTC);

    foreach (Waypoint* wpt, track->waypoint_list) {

      wpt->creation_time.setDate(prev.date());
      if (prev > wpt->creation_time) {
        /* go over midnight ? */
        wpt->creation_time = wpt->creation_time.addDays(1);
      }
      prev = wpt->GetCreationTime();
    }
  } else {
    QDateTime prev = prev_datetime;

    /* go backward through the track and complete timestamps */

    for (auto it = track->waypoint_list.crbegin(); it != track->waypoint_list.crend(); ++it) {
      Waypoint* wpt = *it;

      if (wpt->wpt_flags.fmt_use != 0) {
        wpt->wpt_flags.fmt_use = 0; /* reset flag */

        wpt->creation_time.setDate(prev.date());
        if (wpt->creation_time > prev) {
          wpt->creation_time = wpt->creation_time.addDays(-1);
        }
      }
      prev = wpt->GetCreationTime();
    }
  }
}

bool
NmeaFormat::notalkerid_strmatch(const QByteArray& s1, const char *sentenceFormatterMnemonicCode)
{
/*
 * compare leading start of parametric sentence character ('$'), sentence
 * address field, and trailing comma to the desired sentence formatter mnemonic
 * code (the 3rd-5th characters of the sentence address field).  The talker
 * identifier mnemonic (the 1st-2nd characters of the sentence address field)
 * is likely "GP" for Global Positioning System (GPS) but other talkers like
 * "IN" for Integrated Navigation can emit relevant sentences, so we ignore the
 * talker identifier mnemonic.
 */
  return (s1.size() > 6) && (s1.at(0) == '$') && (s1.at(6) == ',') &&
         (s1.mid(3,3) == sentenceFormatterMnemonicCode);
}

void
NmeaFormat::nmea_parse_one_line(const QByteArray& ibuf)
{
  QByteArray tbuf = ibuf.trimmed();

  /*
   * GISTEQ PhotoTracker (stupidly) puts a bogus field in front
   * of the line.  Look for it and toss it.
   */
  if (tbuf.startsWith("---,")) {
    tbuf.remove(0, 4);
  }

  if (tbuf.at(0) != '$') {
    return;
  }

  if (int ckidx = tbuf.lastIndexOf('*'); ckidx >= 0) {
    bool checked = false;
    if ((ckidx + 2) < tbuf.size()) {
      QByteArray ckstring = tbuf.mid(ckidx + 1, 2);
      bool ok;
      int ckcmp = ckstring.toInt(&ok, 16);
      if (ok) {
        int ckval = nmea_cksum(tbuf.mid(1, ckidx - 1));
        if (ckval != ckcmp) {
          Warning().nospace() <<  hex << "Invalid NMEA checksum.  Computed 0x" << ckval << " but found 0x" << ckcmp << ".  Ignoring sentence.";
          return;
        }
        checked = true;
      }
    }
    if (!checked) {
      Warning().nospace()  << "Unrecoverable NMEA checksum in line " << tbuf << ". Ignoring sentence.";
      return;
    }
    // hide checksum from sentence parsers.
    tbuf.truncate(ckidx);
    had_checksum = true;
  } else if (had_checksum) {
    /* we have had a checksum on all previous sentences, but not on this
    one, which probably indicates this line is truncated */
    had_checksum = false;
    return;
  }

  if (tbuf.count("$") > 1) {
    /* If line has more than one $, there is probably an error in it. */
    return;
  }

  /* @@@ zmarties: The parse routines all assume all fields are present, but
     the NMEA format allows any field to be missed out if there is no data
     for that field.  Rather than change all the parse routines, we first
     substitute a default value of zero for any missing field.
  */
  tbuf.replace(",,", ",0,");

  if (notalkerid_strmatch(tbuf, "WPL")) {
    gpwpl_parse(tbuf);
  } else if (opt_gpgga && notalkerid_strmatch(tbuf, "GGA")) {
    posn_type = gpgga;
    gpgga_parse(tbuf);
  } else if (opt_gprmc && notalkerid_strmatch(tbuf, "RMC")) {
    if (posn_type != gpgga) {
      posn_type = gprmc;
    }
    /*
     * Always call gprmc_parse() because like GPZDA
     * it contains the full date.
     */
    gprmc_parse(tbuf);
  } else if (notalkerid_strmatch(tbuf, "GLL")) {
    if ((posn_type != gpgga) && (posn_type != gprmc)) {
      gpgll_parse(tbuf);
    }
  } else if (notalkerid_strmatch(tbuf, "ZDA")) {
    gpzda_parse(tbuf);
  } else if (tbuf.startsWith("$PCMPT,")) {
    pcmpt_parse(tbuf.constData());
  } else if (opt_gpvtg && notalkerid_strmatch(tbuf, "VTG")) {
    gpvtg_parse(tbuf); /* speed and course */
  } else if (opt_gpgsa && notalkerid_strmatch(tbuf, "GSA")) {
    gpgsa_parse(tbuf); /* GPS fix */
  } else if (tbuf.startsWith("$ADPMB,5,0")) {
    amod_waypoint = true;
  }
}

void
NmeaFormat::read()
{
  char* ibuf;
  QTime lt;
  int line = -1;

  posn_type = gp_unknown;
  trk_head = nullptr;
  without_date = 0;
  prev_datetime = QDateTime();
  opt_tm = QDate();

  /* This was done in rd_init() */
  if (getposn) {
    return;
  }

  counter["invalid_gprmc"] = 0;
  counter["invalid_gpgll"] = 0;
  counter["invalid_gpgga"] = 0;

  if (optdate) {
    opt_tm = QDate::fromString(optdate, "yyyyMMdd");
    if (!opt_tm.isValid()) {
      fatal(MYNAME ": Invalid date \"%s\"!\n", optdate);
    }
  }

  curr_waypt = nullptr;

  while ((ibuf = gbfgetstr(file_in))) {
    line++;

    if ((line == 0) & file_in->unicode) {
      cet_convert_init(CET_CHARSET_UTF8, 1);
    }

    if ((line == 0) && (case_ignore_strncmp(ibuf, "@SonyGPS/ver", 12) == 0)) {
      /* special hack for Sony GPS-CS1 files:
         they are fully (?) nmea compatible, but come with a header line like
         "@Sonygps/ver1.0/wgs-84". */
      /* The Sony GPS-CS3KA extends that line even further
         so we now look for the second field to be /
         delimited.
         @Sonygps/ver1.0/wgs-84/gps-cs3.0
       */

      /* Check the GPS datum */
      char* cx = strchr(&ibuf[12], '/');
      if (cx != nullptr) {
        char* sdatum = cx + 1;
        char* edatum = strchr(sdatum, '/');
        if (edatum) {
          *edatum = 0;
        }
        datum = GPS_Lookup_Datum_Index(sdatum);
        if (datum < 0) {
          fatal(MYNAME "/SonyGPS: Unsupported datum \"%s\" in source data!\n", sdatum);
        }
      }
      continue;
    }

    nmea_parse_one_line(ibuf);
    if (lt != last_read_time && curr_waypt && trk_head) {
      if (curr_waypt != last_waypt) {
        nmea_add_wpt(curr_waypt, trk_head);
        last_waypt = curr_waypt;
      }
      lt = last_read_time;
    }
  }

  /* try to complete date-less trackpoints */
  nmea_fix_timestamps(trk_head);
}

void
NmeaFormat::rd_position_init(const QString& fname)
{
  if ((gbser_handle = gbser_init(qPrintable(fname))) != nullptr) {
    read_mode = rm_serial;
    gbser_set_speed(gbser_handle, 4800);
  } else {
    fatal(MYNAME ": Could not open '%s' for position tracking.\n", qPrintable(fname));
  }

  gbser_flush(gbser_handle);

  if (opt_baud) {
    if (!gbser_set_speed(gbser_handle, atoi(opt_baud))) {
      fatal(MYNAME ": Unable to set baud rate %s\n", opt_baud);
    }
  }
  posn_fname = fname;
}

void
NmeaFormat::safe_print(int cnt, const char* b)
{
  for (int i = 0; i < cnt; i++) {
    char c = isprint(b[i]) ? b[i] : '.';
    fputc(c, stderr);
  }
}

int NmeaFormat::hunt_sirf()
{
  /* Try to place the common BR's first to speed searching */
  static int br[] = {38400, 9600, 57600, 115200, 19200, 4800, -1};
  static int* brp = &br[0];
  char ibuf[1024];

  for (brp = br; *brp > 0; brp++) {
    if (global_opts.debug_level > 1) {
      fprintf(stderr, "Trying %d\n", *brp);
    }

    /*
     * Cycle our port's data speed and spray the "change to NMEA
     * mode to the device.
     */
    gbser_set_speed(gbser_handle, *brp);
    reset_sirf_to_nmea(*brp);

    int rv = gbser_read_line(gbser_handle, ibuf, sizeof(ibuf),
                             1000, 0x0a, 0x0d);
    /*
     * If we didn't get a read error but did get a string that
     * started with a dollar sign, we're probably in NMEA mode
     * now.
     */
    if ((rv > -1) && (strlen(ibuf) > 0) && ibuf[0] == '$') {
      return 1;
    }

    /*
     * If nothing was received, it's not a sirf part.  Fast exit.
     */
    if (rv < 0) {
      return 0;
    }
  }
  return 0;
}

Waypoint*
NmeaFormat::rd_position(posn_status* /*unused*/)
{
  char ibuf[1024];
  static QTime lt;
  int am_sirf = 0;

  /*
   * Read a handful of sentences, collecting the best info we
   * can.  If the timestamp changes (indicating the sequence is
   * about to restart and thus the one we're collecting isn't going
   * to get any better than we now have) hand that back to the caller.
   */

  for (int i = 0; i < 10; i++) {
    ibuf[0] = 0;
    int rv = gbser_read_line(gbser_handle, ibuf, sizeof(ibuf), 2000, 0x0a, 0x0d);
    if (global_opts.debug_level > 1) {
      safe_print(strlen(ibuf), ibuf);
    }
    if (rv < 0) {
      if (am_sirf == 0) {
        if (global_opts.debug_level > 1) {
          warning(MYNAME ": Attempting sirf mode.\n");
        }
        /* This is tacky, we have to change speed
         * to 9600bps to tell it to speak NMEA at
         * 4800.
         */
        am_sirf = hunt_sirf();
        if (am_sirf) {
          i = 0;
          continue;
        }
      }
      fatal(MYNAME ": No data received on %s.\n", qPrintable(posn_fname));
    }
    nmea_parse_one_line(ibuf);
    if (lt != last_read_time) {
      if (last_read_time.isValid()) {
        Waypoint* w = curr_waypt;

        lt = last_read_time;
        curr_waypt = nullptr;

        return w;
      }
    }
  }
  return nullptr;
}

void
NmeaFormat::rd_position_deinit()
{
  rd_deinit();
}

void
NmeaFormat::nmea_wayptpr(const Waypoint* wpt) const
{
  char obuf[200];
  QString s;

  double lat = degrees2ddmm(wpt->latitude);
  double lon = degrees2ddmm(wpt->longitude);
  if (global_opts.synthesize_shortnames) {
    s = mkshort_from_wpt(mkshort_handle, wpt);
  } else {
    s = mkshort(mkshort_handle, wpt->shortname);
  }

  snprintf(obuf, sizeof(obuf),  "GPWPL,%08.3f,%c,%09.3f,%c,%s",
           fabs(lat), lat < 0 ? 'S' : 'N',
           fabs(lon), lon < 0 ? 'W' : 'E', CSTRc(s)
          );
  int cksum = nmea_cksum(obuf);
  gbfprintf(file_out, "$%s*%02X\n", obuf, cksum);
  if (sleepms >= 0) {
    gbfflush(file_out);
    QThread::msleep(sleepms);
  }
}

void
NmeaFormat::nmea_track_init(const route_head* /*unused*/)
{
  first_trkpt = true;
}

void
NmeaFormat::nmea_trackpt_pr(const Waypoint* wpt)
{
  char obuf[200];
  char fix='0';
  int cksum;

  if (opt_sleep) {
    gbfflush(file_out);
    if (!first_trkpt) {
      if (sleepms >= 0) {
        QThread::msleep(sleepms);
      } else {
        // wait_time will be 0 if either creation time is invalid.
        long wait_time = last_write_time.msecsTo(wpt->GetCreationTime());
        if (wait_time > 0) {
          QThread::msleep(wait_time);
        }
      }
    }
    last_write_time = wpt->GetCreationTime();
    first_trkpt = false;
  }

  double lat = degrees2ddmm(wpt->latitude);
  double lon = degrees2ddmm(wpt->longitude);

  QByteArray dmy("");
  QByteArray hms("");
  if (wpt->GetCreationTime().isValid()) {
    dmy = wpt->GetCreationTime().toUTC().toString("ddMMyy").toUtf8();
    hms = wpt->GetCreationTime().toUTC().toString("hhmmss.zzz").toUtf8();
  }

  switch (wpt->fix) {
  case fix_dgps:
    fix='2';
    break;
  case fix_3d:
  case fix_2d:
    fix='1';
    break;
  case fix_pps:
    fix='3';
    break;
  default:
    fix='0';
  }

  if (opt_gprmc) {
    snprintf(obuf, sizeof(obuf), "GPRMC,%s,%c,%08.3f,%c,%09.3f,%c,%.2f,%.2f,%s,,",
             hms.constData(),
             fix=='0' ? 'V' : 'A',
             fabs(lat), lat < 0 ? 'S' : 'N',
             fabs(lon), lon < 0 ? 'W' : 'E',
             WAYPT_HAS(wpt, speed) ? MPS_TO_KNOTS(wpt->speed):(0),
             WAYPT_HAS(wpt, course) ? (wpt->course):(0),
             dmy.constData());
    cksum = nmea_cksum(obuf);

    /* GISTeq doesn't care about the checksum, but wants this prefixed, so
     * we can write it with abandon.
     */
    if (opt_gisteq) {
      gbfprintf(file_out, "---,");
    }
    gbfprintf(file_out, "$%s*%02X\n", obuf, cksum);
  }
  if (opt_gpgga) {
    snprintf(obuf, sizeof(obuf), "GPGGA,%s,%08.3f,%c,%09.3f,%c,%c,%02d,%.1f,%.3f,M,%.1f,M,,",
             hms.constData(),
             fabs(lat), lat < 0 ? 'S' : 'N',
             fabs(lon), lon < 0 ? 'W' : 'E',
             fix,
             (wpt->sat>0)?(wpt->sat):(0),
             (wpt->hdop>0)?(wpt->hdop):(0.0),
             wpt->altitude == unknown_alt ? 0 : wpt->altitude,
             WAYPT_HAS(wpt, geoidheight)? (wpt->geoidheight) : (0)); /* TODO: we could look up the geoidheight if needed */
    cksum = nmea_cksum(obuf);
    gbfprintf(file_out, "$%s*%02X\n", obuf, cksum);
  }
  if ((opt_gpvtg) && (WAYPT_HAS(wpt, course) || WAYPT_HAS(wpt, speed))) {
    snprintf(obuf,sizeof(obuf),"GPVTG,%.3f,T,0,M,%.3f,N,%.3f,K",
             WAYPT_HAS(wpt, course) ? (wpt->course):(0),
             WAYPT_HAS(wpt, speed) ? MPS_TO_KNOTS(wpt->speed):(0),
             WAYPT_HAS(wpt, speed) ? MPS_TO_KPH(wpt->speed):(0));

    cksum = nmea_cksum(obuf);
    gbfprintf(file_out, "$%s*%02X\n", obuf, cksum);
  }

  if ((opt_gpgsa) && (wpt->fix!=fix_unknown)) {

    switch (wpt->fix) {
    case fix_dgps:
      /* or */
    case fix_3d:
      fix='3';
      break;
    case fix_2d:
      fix='2';
      break;
    default:
      fix=0;
    }
    snprintf(obuf,sizeof(obuf),"GPGSA,A,%c,,,,,,,,,,,,,%.1f,%.1f,%.1f",
             fix,
             (wpt->pdop > 0) ? (wpt->pdop) : (0),
             (wpt->hdop > 0) ? (wpt->hdop) : (0),
             (wpt->vdop > 0) ? (wpt->vdop) : (0));
    cksum = nmea_cksum(obuf);
    gbfprintf(file_out, "$%s*%02X\n", obuf, cksum);
  }
  gbfflush(file_out);
}

void
NmeaFormat::write()
{
  auto nmea_wayptpr_lambda = [this](const Waypoint* waypointp)->void {
    nmea_wayptpr(waypointp);
  };
  waypt_disp_all(nmea_wayptpr_lambda);
  auto nmea_track_init_lambda = [this](const route_head* rte)->void {
    nmea_track_init(rte);
  };
  auto nmea_trackpt_pr_lambda = [this](const Waypoint* waypointp)->void {
    nmea_trackpt_pr(waypointp);
  };
  track_disp_all(nmea_track_init_lambda, nullptr, nmea_trackpt_pr_lambda);
}

void
NmeaFormat::wr_position_init(const QString& fname)
{
  wr_init(fname);
}

void
NmeaFormat::wr_position(Waypoint* wpt)
{
  nmea_trackpt_pr(wpt);
}

void
NmeaFormat::wr_position_deinit()
{
// nmea_wr_deinit();
}

/*
 * If we later decide to implement a "real" Sirf module, this code should
 * go there.  For now, we try a kind of heavy handed thing - if we don't
 * see NMEA-isms from the device, we'll go on the premise that it MAY be
 * a SiRF Star device and send it the "speak NMEA, please" command.
 */

void
NmeaFormat::sirf_write(unsigned char* buf) const
{
  int chksum = 0;
  int len = buf[2] << 8 | buf[3];

  for (int i = 0; i < len; i++) {
    chksum += buf[4 + i];
  }
  chksum &= 0x7fff;

  buf[len + 4] = chksum  >> 8;
  buf[len + 5] = chksum  & 0xff;

  gbser_write(gbser_handle, buf, len + 8);  /* 4 at front, 4 at back */
}

void NmeaFormat::reset_sirf_to_nmea(int br)
{
  static unsigned char pkt[] = {0xa0, 0xa2, 0x00, 0x18,
                                0x81, 0x02,
                                0x01, 0x01, /* GGA */
                                0x00, 0x00, /* suppress GLL */
                                0x01, 0x00, /* suppress GSA */
                                0x05, 0x00, /* suppress GSV */
                                0x01, 0x01, /* use RMC for date*/
                                0x00, 0x00, /* suppress VTG */
                                0x00, 0x01, /* output rate */
                                0x00, 0x01, /* unused recommended values */
                                0x00, 0x01,
                                0x00, 0x01, /* ZDA */
                                0x12, 0xc0, /* 4800 bps */
                                0x00, 0x00,  /* checksum */
                                0xb0, 0xb3
                               }; /* packet end */
  /* repopulate bit rate */
  pkt[26] = br >> 8;
  pkt[27] = br & 0xff;

  sirf_write(pkt);
  QThread::msleep(250);
  gbser_flush(gbser_handle);
}
