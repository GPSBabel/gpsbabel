/*
 * FAI/IGC data format translation.
 *
 * Refer to Appendix 1 of
 * http://www.fai.org:81/gliding/gnss/tech_spec_gnss.asp for the
 * specification of the IGC data format.  This translation code was
 * written when the latest amendment list for the specification was AL6.
 *
 * Copyright (C) 2004 Chris Jones
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2 of the License, or (at your
 * option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 */

#include <cmath>                     // for fabs, lround
#include <cstdio>                    // for sscanf, snprintf, fputs, printf, stdout, putchar, size_t
#include <cstdlib>                   // for labs, ldiv, ldiv_t, abs
#include <cstring>                   // for strcmp, strlen, strtok, strcat, strchr, strcpy, strncat
#include <ctime>                     // for gmtime, ctime
#include <iterator>                  // for reverse_iterator, operator==, prev, next

#include <QtCore/QByteArray>         // for QByteArray
#include <QtCore/QList>              // for QList<>::const_iterator
#include <QtCore/QStaticStringData>  // for QStaticStringData
#include <QtCore/QString>            // for QString, operator+, QStringLiteral
#include <QtCore/QtGlobal>           // for foreach, qPrintable

#include "defs.h"
#include "cet_util.h"                // for cet_convert_init
#include "gbfile.h"                  // for gbfprintf, gbfclose, gbfopen, gbfputs, gbfgetstr, gbfile
#include "src/core/datetime.h"       // for DateTime
#include "src/core/optional.h"       // for optional


static gbfile* file_in, *file_out;
static char manufacturer[4];
static const route_head* head;
static char* timeadj = nullptr;
static int lineno;

#define MYNAME "IGC"
#define MAXRECLEN 79		// Includes null terminator and CR/LF
#define MAXDESCLEN 1024
#define PRESTRKNAME "PRESALTTRK"
#define GNSSTRKNAME "GNSSALTTRK"
#define HDRMAGIC "IGCHDRS"
#define HDRDELIM "~"
#define DATEMAGIC "IGCDATE"

/*
 * IGC record types.
 * These appear as the first char in each record.
 */
enum igc_rec_type_t {
  rec_manuf_id = 'A',		// FR manufacturer and identification
  rec_fix = 'B',		// Fix
  rec_task = 'C',		// Task/declaration
  rec_diff_gps = 'D',		// Differential GPS
  rec_event = 'E',		// Event
  rec_constel = 'F',		// Constellation
  rec_security = 'G',		// Security
  rec_header = 'H',		// File header
  rec_fix_defn = 'I',		// List of extension data included at end of each fix (B) record
  rec_extn_defn = 'J',	// List of data included in each extension (K) record
  rec_extn_data = 'K',	// Extension data
  rec_log_book = 'L',		// Logbook/comments

  // M..Z are spare

  rec_none = 0,		// No record
  rec_bad = 1,		// Bad record
};

/*
 * See if two lat/lon pairs are approximately equal.
 * @param  lat1  The latitude of coordinate pair 1
 * @param  lon1  The longitude of coordinate pair 1
 * @param  lat2  The latitude of coordinate pair 2
 * @param  lon2  The longitude of coordinate pair 2
 * @retval  1  The coordinates are approximately equal
 * @retval  0  The coordinates are significantly different
 */
static unsigned char coords_match(double lat1, double lon1, double lat2, double lon2)
{
  return (fabs(lat1 - lat2) < 0.0001 && fabs(lon1 - lon2) < 0.0001) ? 1 : 0;
}

/*************************************************************************************************
 * Input file processing
 */

/*
 * Get an IGC record from the input file
 * @param  rec  Caller allocated storage for the record.  At least MAXRECLEN chars must be allocated.
 * @return the record type.  rec_none on EOF, rec_bad on fgets() or parse error.
 */
static igc_rec_type_t get_record(char** rec)
{
  char* c;
retry:
  *rec = c = gbfgetstr(file_in);
  if ((lineno++ == 0) && file_in->unicode) {
    cet_convert_init(CET_CHARSET_UTF8, 1);
  }
  if (c == nullptr) {
    return rec_none;
  }

  size_t len = strlen(c);

  /* Trackwiev writes (bogus) blank links between each record */
  if (len == 0) {
    goto retry;
  }

  if (len < 3 || c[0] < 'A' || c[0] > 'Z') {
    warning(MYNAME " bad input record: '%s'\n", c);
    return rec_bad;
  }
  return (igc_rec_type_t) c[0];
}

static void rd_init(const QString& fname)
{
  char* ibuf;

  file_in = gbfopen(fname, "r", MYNAME);
  lineno = 0;
  // File must begin with a manufacturer/ID record
  if (get_record(&ibuf) != rec_manuf_id || sscanf(ibuf, "A%3[A-Z]", manufacturer) != 1) {
    fatal(MYNAME ": %s is not an IGC file\n", qPrintable(fname));
  }
}

static void rd_deinit()
{
  gbfclose(file_in);
}

enum state_t { id, takeoff, start, turnpoint, finish, landing };
inline state_t& operator++(state_t& s) // prefix
{
  return s = static_cast<state_t>(s + 1);
}
inline const state_t operator++(state_t& s, int) // postfix
{
  state_t ret(s);
  s = ++s;
  return ret;
}

/**
 * Handle pre- or post-flight task declarations.
 * A route is created for each set of waypoints in a task declaration.
 * @param rec A single task record
 */
static void igc_task_rec(const char* rec)
{
  static char flight_date[7];
  static unsigned int num_tp, tp_ct;
  static route_head* rte_head;
  static time_t creation;

  char task_num[5];
  char task_desc[MAXRECLEN];
  unsigned int lat_deg, lat_min, lat_frac;
  unsigned int lon_deg, lon_min, lon_frac;
  char lat_hemi[2], lon_hemi[2];
  char short_name[8];
  char tmp_str[MAXRECLEN];
  struct tm tm;
  static state_t state = id;

  // First task record identifies the task to follow
  if (id == state) {
    task_desc[0] = '\0';
    if (sscanf(rec, "C%2u%2u%2u%2u%2u%2u%6[0-9]%4c%2u%78[^\r]\r\n",
               &tm.tm_mday, &tm.tm_mon, &tm.tm_year,
               &tm.tm_hour, &tm.tm_min, &tm.tm_sec,
               flight_date, task_num, &num_tp, task_desc) < 9) {
      fatal(MYNAME ": task id (C) record parse error\n'%s'", rec);
    }
    task_num[4] = '\0';
    tm.tm_mon -= 1;
    if (tm.tm_year < 70) {
      tm.tm_year += 100;
    }
    tm.tm_isdst = 0;
    creation = mkgmtime(&tm);

    // Create a route to store the task data in.
    rte_head = new route_head;
    rte_head->rte_name = task_num;
    rte_head->rte_desc = QStringLiteral(DATEMAGIC) + flight_date + QStringLiteral(": ") + task_desc;
    route_add_head(rte_head);
    ++state;
    return;
  }
  // Get the waypoint
  tmp_str[0] = '\0';
  if (sscanf(rec, "C%2u%2u%3u%1[NS]%3u%2u%3u%1[WE]%78[^\r]\r\n",
             &lat_deg, &lat_min, &lat_frac, lat_hemi,
             &lon_deg, &lon_min, &lon_frac, lon_hemi, tmp_str) < 8) {
    fatal(MYNAME ": task waypoint (C) record parse error\n%s", rec);
  }

  auto* wpt = new Waypoint;
  wpt->latitude = ('N' == lat_hemi[0] ? 1 : -1) *
                  (lat_deg + (lat_min * 1000 + lat_frac) / 1000.0 / 60);

  wpt->longitude = ('E' == lon_hemi[0] ? 1 : -1) *
                   (lon_deg + (lon_min * 1000 + lon_frac) / 1000.0 / 60);

  wpt->SetCreationTime(creation);
  wpt->description = tmp_str;

  // Name the waypoint according to the order of the task record
  switch (state) {
  case takeoff:
    snprintf(short_name, 8, "TAKEOFF");
    ++state;
    break;

  case start:
    snprintf(short_name, 8, "START");
    tp_ct = 0;
    ++state;
    break;

  case turnpoint:
    if (++tp_ct == num_tp) {
      ++state;
    }
    snprintf(short_name, 8, "TURN%02u", tp_ct);
    break;

  case finish:
    snprintf(short_name, 8, "FINISH");
    ++state;
    break;

  case landing:
    snprintf(short_name, 8, "LANDING");
    state = id;
    break;

  default:
    fatal(MYNAME ": task id (C) record internal error\n%s", rec);
    break;
  }

  // Zero lat and lon indicates an unknown waypoint
  if (coords_match(wpt->latitude, wpt->longitude, 0.0, 0.0)) {
    delete wpt;
    return;
  }
  wpt->shortname = short_name;
  route_add_wpt(rte_head, wpt);
}

static void data_read()
{
  char* ibuf;
  unsigned int hours, mins, secs;
  unsigned int lat_deg, lat_min, lat_frac;
  unsigned int lon_deg, lon_min, lon_frac;
  char lat_hemi[2], lon_hemi[2];
  char validity;
  route_head* pres_head = nullptr;
  route_head* gnss_head = nullptr;
  int pres_alt, gnss_alt;
  char pres_valid = 0;
  char gnss_valid = 0;
  Waypoint* pres_wpt = nullptr;
  Waypoint* gnss_wpt = nullptr;
  time_t date = 0;
  time_t prev_tod = 0;
  time_t tod;
  struct tm tm;
  char tmp_str[20];
  char* hdr_data;
  size_t remain;
  char trk_desc[MAXDESCLEN + 1];

  strcpy(trk_desc, HDRMAGIC HDRDELIM);

  while (true) {
    igc_rec_type_t rec_type = get_record(&ibuf);
    switch (rec_type) {
    case rec_manuf_id:
      // Manufacturer/ID record already found in rd_init().
      warning(MYNAME ": duplicate manufacturer/ID record\n");
      break;

    case rec_header:
      // Get the header sub type
      if (sscanf(ibuf, "H%*1[FOPS]%3s", tmp_str) != 1) {
        fatal(MYNAME ": header (H) record parse error\n%s\n%s\n", ibuf, tmp_str);
      }
      // Optional long name of record sub type is followed by a
      // colon.  Actual header data follows that.
      if (nullptr == (hdr_data = strchr(ibuf, ':'))) {
        hdr_data = ibuf + 5;
      } else {
        hdr_data++;
      }

      // Date sub type
      if (strcmp(tmp_str, "DTE") == 0) {
        if (sscanf(hdr_data, "%2u%2u%2u", &tm.tm_mday, &tm.tm_mon, &tm.tm_year) != 3) {
          fatal(MYNAME ": date (H) record parse error\n'%s'\n", ibuf);
        }
        tm.tm_sec = tm.tm_min = tm.tm_hour = 0;
        tm.tm_mon -= 1;
        if (tm.tm_year < 70) {
          tm.tm_year += 100;
        }
        tm.tm_isdst = 0;
        date = mkgmtime(&tm);
      } else {
        // Store other header data in the track descriptions
        if (strlen(trk_desc) < MAXDESCLEN) {
          remain = MAXDESCLEN - strlen(trk_desc);
          strncat(trk_desc, ibuf, remain);
          remain = MAXDESCLEN - strlen(trk_desc);
          strncat(trk_desc, HDRDELIM, remain);
        }
      }
      break;

    case rec_fix:
      // Date must appear in file before the first fix record
      if (date < 1000000L) {
        fatal(MYNAME ": bad date %d\n", (int)date);
      }
      // Create a track for pressure altitude waypoints
      if (!pres_head) {
        pres_head = new route_head;
        pres_head->rte_name = PRESTRKNAME;
        pres_head->rte_desc = trk_desc;
        track_add_head(pres_head);
      }
      // Create a second track for GNSS altitude waypoints
      if (!gnss_head) {
        gnss_head = new route_head;
        gnss_head->rte_name = GNSSTRKNAME;
        gnss_head->rte_desc = trk_desc;
        track_add_head(gnss_head);
      }
      // Create a waypoint from the fix record data
      if (sscanf(ibuf,
                 "B%2u%2u%2u%2u%2u%3u%1[NS]%3u%2u%3u%1[WE]%c%5d%5d",
                 &hours, &mins, &secs, &lat_deg, &lat_min, &lat_frac,
                 lat_hemi, &lon_deg, &lon_min, &lon_frac, lon_hemi,
                 &validity, &pres_alt, &gnss_alt) != 14) {
        fatal(MYNAME ": fix (B) record parse error\n%s\n", ibuf);
      }
      pres_wpt = new Waypoint;

      pres_wpt->latitude = ('N' == lat_hemi[0] ? 1 : -1) *
                           (lat_deg + (lat_min * 1000 + lat_frac) / 1000.0 / 60);

      pres_wpt->longitude = ('E' == lon_hemi[0] ? 1 : -1) *
                            (lon_deg + (lon_min * 1000 + lon_frac) / 1000.0 / 60);

      // Increment date if we pass midnight UTC
      tod = (hours * 60 + mins) * 60 + secs;
      if (tod < prev_tod) {
        date += 24 * 60 * 60;
      }
      prev_tod = tod;
      pres_wpt->SetCreationTime(date + tod);

      // Add the waypoint to the pressure altitude track
      if (pres_alt) {
        pres_valid = 1;
        pres_wpt->altitude = pres_alt;
      } else {
        pres_wpt->altitude = unknown_alt;
      }
      track_add_wpt(pres_head, pres_wpt);

      // Add the same waypoint with GNSS altitude to the second
      // track
      gnss_wpt = new Waypoint(*pres_wpt);

      if (gnss_alt) {
        gnss_valid = 1;
        gnss_wpt->altitude = gnss_alt;
      } else {
        gnss_wpt->altitude = unknown_alt;
      }
      track_add_wpt(gnss_head, gnss_wpt);
      break;

    case rec_task:
      // Create a route for each pre-flight declaration
      igc_task_rec(ibuf);
      break;

    case rec_log_book:
      // Get the log book sub type
      if (sscanf(ibuf, "L%3s", tmp_str) != 1) {
        fatal(MYNAME ": log book (L) record parse error\n'%s'\n", ibuf);
      }

      if (strcmp(tmp_str, "PFC") == 0) {
        // Create a route for each post-flight declaration
        igc_task_rec(ibuf + 4);
        break;
      } else if (global_opts.debug_level) {
        if (strcmp(tmp_str, "OOI") == 0) {
          fputs(MYNAME ": Observer Input> ", stdout);
        } else if (strcmp(tmp_str, "PLT") == 0) {
          fputs(MYNAME ": Pilot Input> ", stdout);
        } else if (strcmp(tmp_str, manufacturer) == 0) {
          fputs(MYNAME ": Manufacturer Input> ", stdout);
        } else {
          fputs(MYNAME ": Anonymous Input> ", stdout);
          fputs(ibuf + 1, stdout);
          break;
        }
        fputs(ibuf + 4, stdout);
        putchar('\n');
      }
      break;

      // These record types are discarded
    case rec_diff_gps:
    case rec_event:
    case rec_constel:
    case rec_security:
    case rec_fix_defn:
    case rec_extn_defn:
    case rec_extn_data:
      break;

      // No more records
    case rec_none:

      // Include pressure altitude track only if it has useful
      // altitude data or if it is the only track available.
      if (pres_head && !pres_valid && gnss_head) {
        track_del_head(pres_head);
        pres_head = nullptr;
      }
      // Include GNSS altitude track only if it has useful altitude
      // data or if it is the only track available.
      if (gnss_head && !gnss_valid && pres_head) {
        track_del_head(gnss_head);
      }
      return;		// All done so bail

    default:
    case rec_bad:
      fatal(MYNAME ": failure reading file\n");
      break;
    }
  }
}

/*************************************************************************************************
 * Output file processing
 */

/*************************************************
 * Callbacks used to scan for specific track types
 */

static void detect_pres_track(const route_head* rh)
{
  if (rh->rte_name.startsWith(PRESTRKNAME)) {
    head = rh;
  }
}

static void detect_gnss_track(const route_head* rh)
{
  if (rh->rte_name.startsWith(GNSSTRKNAME)) {
    head = rh;
  }
}

static void detect_other_track(const route_head* rh)
{
  static int max_waypt_ct;

  if (!head) {
    max_waypt_ct = 0;
  }
  // Find other track with the most waypoints
  if (rh->rte_waypt_ct > max_waypt_ct &&
      (rh->rte_name.isEmpty() || 
       (!rh->rte_name.startsWith(PRESTRKNAME) &&
       !rh->rte_name.startsWith(GNSSTRKNAME)))) {
    head = rh;
    max_waypt_ct = rh->rte_waypt_ct;
  }
}

/*
 * Identify the pressure altitude and GNSS altitude tracks.
 * @param  pres_track  Set by the function to the pressure altitude track
 *                     head.  NULL if not found.
 * @param  gnss_track  Set by the function to the GNSS altitude track
 *                     head.  NULL if not found.
 */
static void get_tracks(const route_head** pres_track, const route_head** gnss_track)
{
  head = nullptr;
  track_disp_all(detect_pres_track, nullptr, nullptr);
  *pres_track = head;

  head = nullptr;
  track_disp_all(detect_gnss_track, nullptr, nullptr);
  *gnss_track = head;

  head = nullptr;
  track_disp_all(detect_other_track, nullptr, nullptr);

  if (!*pres_track && *gnss_track && head) {
    *pres_track = head;
  }

  if (!*gnss_track && head) {
    *gnss_track = head;
  }
}

/*************************************************
 * IGC string formatting functions
 */

static char* latlon2str(const Waypoint* wpt)
{
  static char str[18] = "";
  // We use lround here because it:
  // "Returns the integral value that is nearest to x, with halfway cases rounded away from zero."
  // The halfway rounding cases of *printf are not precisely defined, and can vary with implementation.
  // We don't really care which way the halfway cases go, but we want them to go that way consistently
  // across implementations.
  // We also try to use a minimum of floating point arithmetic to minimize accumulated fp math errors.
  long lat_milliminutes = lround(wpt->latitude * 60000.0);
  long lon_milliminutes = lround(wpt->longitude * 60000.0);
  char lat_hemi = lat_milliminutes < 0 ? 'S' : 'N';
  char lon_hemi = lon_milliminutes < 0 ? 'W' : 'E';
  ldiv_t lat_digits = ldiv(labs(lat_milliminutes), 60000L);
  ldiv_t lon_digits = ldiv(labs(lon_milliminutes), 60000L);

  if (snprintf(str, 18, "%02ld%05ld%c%03ld%05ld%c",
               lat_digits.quot, lat_digits.rem, lat_hemi, lon_digits.quot, lon_digits.rem, lon_hemi) != 17) {
    fatal(MYNAME ": Bad waypoint format '%s'\n", str);
  }
  return str;
}

static char* date2str(struct tm* dt)
{
  static char str[7] = "";

  if (snprintf(str, 7, "%02u%02u%02u", dt->tm_mday, dt->tm_mon + 1, dt->tm_year % 100) != 6) {
    fatal(MYNAME ": Bad date format '%s'\n", str);
  }
  return str;
}

static char* tod2str(struct tm* tod)
{
  static char str[7] = "";

  if (snprintf(str, 7, "%02u%02u%02u", tod->tm_hour, tod->tm_min, tod->tm_sec) != 6) {
    fatal(MYNAME ": Bad time of day format '%s'\n", str);
  }
  return str;
}

/*
 * Write header records
 */
static void wr_header()
{
  const route_head* pres_track;
  const route_head* track;
  struct tm* tm;
  time_t date;
  static const char dflt_str[] = "Unknown";
  const char* str = nullptr;
  Waypoint* wpt;

  get_tracks(&pres_track, &track);
  if (!track && pres_track) {
    track = pres_track;
  }
  // Date in header record is that of the first fix record
  date = !track ? current_time().toTime_t() :
         track->waypoint_list.front()->GetCreationTime().toTime_t();

  if (nullptr == (tm = gmtime(&date))) {
    fatal(MYNAME ": Bad track timestamp\n");
  }
  gbfprintf(file_out, "HFDTE%s\r\n", date2str(tm));

  // Other header data may have been stored in track description
  if (track && track->rte_desc.startsWith(HDRMAGIC)) {
    char *rd = xstrdup(track->rte_desc);
    for (str = strtok(rd + strlen(HDRMAGIC) + strlen(HDRDELIM), HDRDELIM);
         str; str = strtok(nullptr, HDRDELIM)) {
      gbfprintf(file_out, "%s\r\n", str);
    }
    xfree(rd);
    rd = nullptr;
  } else {
// FIXME: This almost certainly introduces a memory leak because str
// is a c string that's used for totally too many things.  Just let it
// leak for now. 2013-12-31 robertl
    if (nullptr != (wpt = find_waypt_by_name("PILOT")) && !wpt->description.isEmpty()) {
      xfree(str);
      str = xstrdup(CSTRc(wpt->description));
    } else {
      // IGC header info not found so synthesise it.
      // If a waypoint is supplied with a short name of "PILOT", use
      // its description as the pilot's name in the header.
      str = xstrdup(dflt_str);
    }
    gbfprintf(file_out, "HFPLTPILOT:%s\r\n", str);
    xfree(str);
  }
}

/*************************************************
 * Generation of IGC task declaration records
 */

static void wr_task_wpt_name(const Waypoint* wpt, const char* alt_name)
{
  gbfprintf(file_out, "C%s%s\r\n", latlon2str(wpt),
            !wpt->description.isEmpty() ? CSTR(wpt->description) : !wpt->shortname.isEmpty() ? CSTR(wpt->shortname) : alt_name);
}

static void wr_task_hdr(const route_head* rte)
{
  unsigned char have_takeoff = 0;
  char flight_date[7] = "000000";
  char task_desc[MAXRECLEN] = "";
  int num_tps = rte->rte_waypt_ct - 2;
  struct tm* tm;
  time_t rte_time;
  static unsigned int task_num = 1;

  if (num_tps < 0) {
    fatal(MYNAME ": Empty task route\n");
  }
  // See if the takeoff and landing waypoints are there or if we need to
  // generate them.
  const Waypoint* wpt = rte->waypoint_list.back();
  if (wpt->shortname.startsWith("LANDING")) {
    num_tps--;
  }
  wpt = rte->waypoint_list.front();
  if (wpt->shortname.startsWith("TAKEOFF")) {
    have_takeoff = 1;
    num_tps--;
  }
  if (num_tps < 0) {
    fatal(MYNAME ": Too few waypoints in task route\n");
  } else if (num_tps > 99) {
    fatal(MYNAME ": Too much waypoints (more than 99) in task route.\n");
  }
  // Gather data to write to the task identification (first) record
  rte_time = wpt->GetCreationTime().isValid() ? wpt->GetCreationTime().toTime_t() : current_time().toTime_t();
  if (nullptr == (tm = gmtime(&rte_time))) {
    fatal(MYNAME ": Bad task route timestamp\n");
  }

  if (!rte->rte_desc.isEmpty()) {
    // desc will be something like "IGCDATE160701: 500KTri" 
    sscanf(CSTR(rte->rte_desc), DATEMAGIC "%6[0-9]: %s", flight_date, task_desc);
  }

  gbfprintf(file_out, "C%s%s%s%04u%02u%s\r\n", date2str(tm),
            tod2str(tm), flight_date, task_num++, num_tps, task_desc);

  if (!have_takeoff) {
    // Generate the takeoff waypoint
    wr_task_wpt_name(wpt, "TAKEOFF");
  }
}

static void wr_task_wpt(const Waypoint* wpt)
{
  wr_task_wpt_name(wpt, "");
}

static void wr_task_tlr(const route_head* rte)
{
  // If the landing waypoint is not supplied we need to generate it.
  const Waypoint* wpt = rte->waypoint_list.back();
  QString sn = wpt->shortname;
//  if (!wpt->shortname || strncmp(wpt->shortname, "LANDIN", 6) != 0) {
  if (sn.isEmpty() || !sn.startsWith("LANDIN")) {
    wr_task_wpt_name(wpt, "LANDING");
  }
}

static void wr_tasks()
{
  route_disp_all(wr_task_hdr, wr_task_tlr, wr_task_wpt);
}

/*
 * Write a single fix record
 */
static void wr_fix_record(const Waypoint* wpt, int pres_alt, int gnss_alt)
{
  const time_t tt = wpt->GetCreationTime().toTime_t();
  struct tm* tm = gmtime(&tt);

  if (nullptr == tm) {
    fatal(MYNAME ": bad track timestamp\n");
  }

  if (unknown_alt == pres_alt) {
    pres_alt = 0;
  }
  if (unknown_alt == gnss_alt) {
    gnss_alt = 0;
  }
  gbfprintf(file_out, "B%02u%02u%02u%sA%05d%05d\r\n", tm->tm_hour,
            tm->tm_min, tm->tm_sec, latlon2str(wpt), pres_alt, gnss_alt);
}

/**
 * Attempt to align the pressure and GNSS tracks in time.
 * This is useful when trying to merge a track (lat/lon/time) recorded by a
 * GPS with a barograph (alt/time) recorded by a separate instrument with
 * independent clocks which are not closely synchronised.
 * @return The number of seconds to add to the GNSS track in order to align
 *         it with the pressure track.
 */
static int correlate_tracks(const route_head* pres_track, const route_head* gnss_track)
{
  double alt_diff;
  double speed;
  time_t pres_time;
  time_t gnss_time;
  int time_diff;

  // Deduce the landing time from the pressure altitude track based on
  // when we last descended to within 10m of the final track altitude.
  WaypointList::const_reverse_iterator wpt_rit = pres_track->waypoint_list.crbegin();
  double last_alt = (*wpt_rit)->altitude;
  do {
    ++wpt_rit;
    if (pres_track->waypoint_list.crend() == wpt_rit) {
      // No track left
      return 0;
    }
    alt_diff = last_alt - (*wpt_rit)->altitude;
    if (alt_diff > 10.0) {
      // Last part of track was ascending
      return 0;
    }
  } while (alt_diff > -10.0);
  pres_time = (*std::prev(wpt_rit))->GetCreationTime().toTime_t();
  if (global_opts.debug_level >= 1) {
    printf(MYNAME ": pressure landing time %s", ctime(&pres_time));
  }

  // Deduce the landing time from the GNSS altitude track based on
  // when the groundspeed last dropped below a certain level.
  wpt_rit = gnss_track->waypoint_list.crbegin();
  do {
    const Waypoint* wpt = *wpt_rit;
    ++wpt_rit;
    if (gnss_track->waypoint_list.crend() == wpt_rit) {
      // No track left
      return 0;
    }
    // Get a crude indication of groundspeed from the change in lat/lon
    time_diff = wpt->GetCreationTime().toTime_t() - (*wpt_rit)->GetCreationTime().toTime_t();
    speed = !time_diff ? 0 :
            (fabs(wpt->latitude - (*wpt_rit)->latitude) +
             fabs(wpt->longitude - (*wpt_rit)->longitude)) / time_diff;
    if (global_opts.debug_level >= 2) {
      printf(MYNAME ": speed=%f\n", speed);
    }
  } while (speed < 0.00003);
  gnss_time = (*std::prev(wpt_rit))->GetCreationTime().toTime_t();
  if (global_opts.debug_level >= 1) {
    printf(MYNAME ": gnss landing time %s", ctime(&gnss_time));
  }
  // Time adjustment is difference between the two estimated landing times
  if (15 * 60 < abs(time_diff = pres_time - gnss_time)) {
    warning(MYNAME ": excessive time adjustment %ds\n", time_diff);
  }
  return time_diff;
}

/**
 * Interpolate altitude from a track at a given time.
 * @param  track  The track containing altitude data.
 * @param  time   The time that we are interested in.
 * @return  The altitude interpolated from the track.
 */
static double interpolate_alt(const route_head* track, time_t time)
{
  static gpsbabel_optional::optional<WaypointList::const_iterator> prev_wpt;
  static gpsbabel_optional::optional<WaypointList::const_iterator> curr_wpt;
  int time_diff;

  // Start search at the beginning of the track
  if (!prev_wpt.has_value()) {
    prev_wpt = track->waypoint_list.cbegin();
    curr_wpt = track->waypoint_list.cbegin();
  }
  // Find the track points either side of the requested time
  while ((track->waypoint_list.cend() != curr_wpt.value()) &&
         ((*curr_wpt.value())->GetCreationTime().toTime_t() < time)) {
    prev_wpt = curr_wpt.value();
    curr_wpt = std::next(prev_wpt.value());
  }
  if (track->waypoint_list.cend() == curr_wpt.value()) {
    // Requested time later than all track points, we can't interpolate
    return unknown_alt;
  }

  if (track->waypoint_list.cbegin() == curr_wpt.value()) {
    if ((*curr_wpt.value())->GetCreationTime().toTime_t() == time) {
      // First point's creation time is an exact match so use it's altitude
      return (*curr_wpt.value())->altitude;
    } else {
      // Requested time is prior to any track points, we can't interpolate
      return unknown_alt;
    }
  }
  // Interpolate
  if (0 == (time_diff = (*curr_wpt.value())->GetCreationTime().toTime_t() - (*prev_wpt.value())->GetCreationTime().toTime_t())) {
    // Avoid divide by zero
    return (*curr_wpt.value())->altitude;
  }
  double alt_diff = (*curr_wpt.value())->altitude - (*prev_wpt.value())->altitude;
  return (*prev_wpt.value())->altitude + (alt_diff / time_diff) * (time - (*prev_wpt.value())->GetCreationTime().toTime_t());
}

/*
 * Pressure altitude and GNSS altitude may be provided in two separate
 * tracks.  This function attempts to merge them into one.
 */
static void wr_track()
{
  const route_head* pres_track;
  const route_head* gnss_track;
  int time_adj;

  // Find pressure altitude and GNSS altitude tracks
  get_tracks(&pres_track, &gnss_track);

  // If both found, attempt to merge them
  if (pres_track && gnss_track) {
    if (timeadj) {
      if (strcmp(timeadj, "auto") == 0) {
        time_adj = correlate_tracks(pres_track, gnss_track);
      } else if (sscanf(timeadj, "%d", &time_adj) != 1) {
        fatal(MYNAME ": bad timeadj argument '%s'\n", timeadj);
      }
    } else {
      time_adj = 0;
    }
    if (global_opts.debug_level >= 1) {
      printf(MYNAME ": adjusting time by %ds\n", time_adj);
    }
    // Iterate through waypoints in both tracks simultaneously
    foreach (const Waypoint* wpt, gnss_track->waypoint_list) {
      double pres_alt = interpolate_alt(pres_track, wpt->GetCreationTime().toTime_t() + time_adj);
      wr_fix_record(wpt, pres_alt, wpt->altitude);
    }
  } else {
    if (pres_track) {
      // Only the pressure altitude track was found so generate fix
      // records from it alone.
      foreach (const Waypoint* wpt, pres_track->waypoint_list) {
        wr_fix_record(wpt, wpt->altitude, unknown_alt);
      }
    } else if (gnss_track) {
      // Only the GNSS altitude track was found so generate fix
      // records from it alone.
      foreach (const Waypoint* wpt, gnss_track->waypoint_list) {
        wr_fix_record(wpt, unknown_alt, wpt->altitude);
      }
    } else {
      // No tracks found so nothing to do
      return;
    }
  }
}

static void wr_init(const QString& fname)
{
  file_out = gbfopen(fname, "wb", MYNAME);
}

static void wr_deinit()
{
  gbfclose(file_out);
}

static void data_write()
{
  gbfputs("AXXXZZZGPSBabel\r\n", file_out);
  wr_header();
  wr_tasks();
  wr_track();
  gbfprintf(file_out, "LXXXGenerated by GPSBabel Version %s\r\n", gpsbabel_version);
  gbfputs("GGPSBabelSecurityRecordGuaranteedToFailVALIChecks\r\n", file_out);
}


static QVector<arglist_t> igc_args = {
  {
    "timeadj", &timeadj,
    "(integer sec or 'auto') Barograph to GPS time diff",
    nullptr, ARGTYPE_STRING, ARG_NOMINMAX, nullptr
  },
};

ff_vecs_t igc_vecs = {
  ff_type_file,
  { ff_cap_none , (ff_cap)(ff_cap_read | ff_cap_write), (ff_cap)(ff_cap_read | ff_cap_write) },
  rd_init,
  wr_init,
  rd_deinit,
  wr_deinit,
  data_read,
  data_write,
  nullptr,
  &igc_args,
  CET_CHARSET_ASCII, 0	/* CET-REVIEW */
  , NULL_POS_OPS,
  nullptr
};
