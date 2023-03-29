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

#include "igc.h"

#include <cassert>              // for assert
#include <cmath>                // for fabs, lround
#include <cstdio>               // for sscanf, printf, snprintf, size_t
#include <cstdlib>              // for labs, ldiv, ldiv_t, abs
#include <cstring>              // for strcmp, strlen, strtok, strcat, strchr, strcpy, strncat
#include <iterator>             // for reverse_iterator, operator==, prev, next
#include <optional>             // for optional

#include <QByteArray>           // for QByteArray
#include <QChar>                // for QChar
#include <QLatin1Char>          // for QLatin1Char
#include <QDate>                // for QDate
#include <QDateTime>            // for QDateTime
#include <QList>                // for QList<>::const_iterator
#include <QString>              // for QString, operator+, QStringLiteral
#include <QStringList>          // for QStringList
#include <QTime>                // for operator<, operator==, QTime
#include <Qt>                   // for UTC, SkipEmptyParts
#include <QtGlobal>             // for foreach, qPrintable
#include <QStringView>          // for QStringView
#include <QDebug>               // DELETEME for debugging
#include <QRegularExpression>   // for QRegularExpression

#include "defs.h"
#include "gbfile.h"             // for gbfprintf, gbfclose, gbfopen, gbfputs, gbfgetstr, gbfile
#include "grtcirc.h"            // for RAD, gcdist, radtometers
#include "src/core/datetime.h"  // for DateTime
#include "formspec.h"           // for FormatSpecificData, kFsIGC



#define MYNAME "IGC"
#define HDRMAGIC "IGCHDRS"
#define HDRDELIM "~"
#define DATEMAGIC "IGCDATE"

/*
 * See if two lat/lon pairs are approximately equal.
 * @param  lat1  The latitude of coordinate pair 1
 * @param  lon1  The longitude of coordinate pair 1
 * @param  lat2  The latitude of coordinate pair 2
 * @param  lon2  The longitude of coordinate pair 2
 * @retval  1  The coordinates are approximately equal
 * @retval  0  The coordinates are significantly different
 */
unsigned char IgcFormat::coords_match(double lat1, double lon1, double lat2, double lon2)
{
  return (fabs(lat1 - lat2) < 0.0001 && fabs(lon1 - lon2) < 0.0001) ? 1 : 0;
}

/*************************************************************************************************
 * Input file processing
 */

/*
 * Get an IGC record from the input file
 * @param  rec  Caller allocated storage for the record.  At least kMaxRecLen chars must be allocated.
 * @return the record type.  rec_none on EOF, rec_bad on fgets() or parse error.
 */
IgcFormat::igc_rec_type_t IgcFormat::get_record(char** rec)
{
  char* c;
retry:
  *rec = c = gbfgetstr(file_in);
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

void IgcFormat::rd_init(const QString& fname)
{
  char* ibuf;

  file_in = gbfopen(fname, "r", MYNAME);
  // File must begin with a manufacturer/ID record
  if (get_record(&ibuf) != rec_manuf_id || sscanf(ibuf, "A%3[A-Z]", manufacturer) != 1) {
    fatal(MYNAME ": %s is not an IGC file\n", qPrintable(fname));
  }
}

void IgcFormat::rd_deinit()
{
  gbfclose(file_in);
}

/**
 * Handle pre- or post-flight task declarations.
 * A route is created for each set of waypoints in a task declaration.
 * @param rec A single task record
 */
void IgcFormat::TaskRecordReader::igc_task_rec(const char* rec)
{
  unsigned int lat_deg, lat_min, lat_frac;
  unsigned int lon_deg, lon_min, lon_frac;
  char lat_hemi[2], lon_hemi[2];
  char tmp_str[kMaxRecLen];

  // First task record identifies the task to follow
  if (state_t::id == state) {
    char task_num[5];
    char flight_date[7];
    char task_desc[kMaxRecLen];
    task_desc[0] = '\0';
    int day;
    int month;
    int year;
    int hour;
    int minute;
    int second;
    if (sscanf(rec, "C%2d%2d%2d%2d%2d%2d%6[0-9]%4c%2u%78[^\r]\r\n",
               &day, &month, &year,
               &hour, &minute, &second,
               flight_date, task_num, &num_tp, task_desc) < 9) {
      fatal(MYNAME ": task id (C) record parse error A. \n'%s'", rec);
    }
    task_num[4] = '\0';
    if (year < 70) {
      year += 2000;
    } else {
      year += 1900;
    }
    creation = QDateTime(QDate(year, month, day), QTime(hour, minute, second), Qt::UTC);
    if (!creation.isValid()) {
      fatal(MYNAME ": bad date time\n%s\n", rec);
    }

    // Create a route to store the task data in.
    rte_head = new route_head;
    rte_head->rte_name = task_num;
    rte_head->rte_desc = QStringLiteral(DATEMAGIC) + flight_date + QStringLiteral(": ") + task_desc;
    route_add_head(rte_head);
    state = state_t::takeoff;
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
  case state_t::takeoff:
    wpt->shortname = "TAKEOFF";
    state = state_t::start;
    break;

  case state_t::start:
    wpt->shortname = "START";
    tp_ct = 0;
    state = state_t::turnpoint;
    break;

  case state_t::turnpoint:
    if (++tp_ct == num_tp) {
      state = state_t::finish;
    }
    wpt->shortname = QStringLiteral("TURN%1").arg(tp_ct, 2, 10, QLatin1Char('0'));
    break;

  case state_t::finish:
    wpt->shortname = "FINISH";
    state = state_t::landing;
    break;

  case state_t::landing:
    wpt->shortname = "LANDING";
    state = state_t::id;
    break;

  default:
    fatal(MYNAME ": task id (C) record internal error B\n%s", rec);
    break;
  }

  // Zero lat and lon indicates an unknown waypoint
  if (coords_match(wpt->latitude, wpt->longitude, 0.0, 0.0)) {
    delete wpt;
    return;
  }
  route_add_wpt(rte_head, wpt);
}

void IgcFormat::read()
{
  char* ibuf;
  int hours, mins, secs;
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
  QDate date;
  QTime prev_tod;
  QTime tod;
  char tmp_str[20];
  char* hdr_data;
  char trk_desc[kMaxDescLen + 1];
  TaskRecordReader task_record_reader;
  QStringView ext_data;
  int current_line = 1; // For error reporting. Line numbering is off by one for some reason.
  IgcMetaData igc_metadata;
  ExtensionDefinition* extension;

  strcpy(trk_desc, HDRMAGIC HDRDELIM);

  while (true) {
    igc_rec_type_t rec_type = get_record(&ibuf);
    current_line++;
    QString ibuf_q = QString::fromUtf8(ibuf);
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
        int day;
        int month;
        int year;
        if (sscanf(hdr_data, "%2d%2d%2d", &day, &month, &year) != 3) {
          fatal(MYNAME ": date (H) record parse error\n'%s'\n", ibuf);
        }
        if (year < 70) {
          year += 2000;
        } else {
          year += 1900;
        }
        date = QDate(year, month, day);
        if (!date.isValid()) {
          fatal(MYNAME ": bad date\n%s\n", ibuf);
        }
      } else {
        // Store other header data in the track descriptions
        if (strlen(trk_desc) < kMaxDescLen) {
          size_t remain = kMaxDescLen - strlen(trk_desc);
          strncat(trk_desc, ibuf, remain);
          remain = kMaxDescLen - strlen(trk_desc);
          strncat(trk_desc, HDRDELIM, remain);
        }
      }
      break;

    case rec_fix:
    {
      // Date must appear in file before the first fix record
      if (!date.isValid()) {
        fatal(MYNAME ": bad date\n");
      }
      // Create a track for pressure altitude waypoints
      if (!pres_head) {
        pres_head = new route_head;
        pres_head->rte_name = kPresTrkName;
        pres_head->rte_desc = trk_desc;
        track_add_head(pres_head);
      }
      // Create a second track for GNSS altitude waypoints
      if (!gnss_head) {
        gnss_head = new route_head;
        gnss_head->rte_name = kGNSSTrkName;
        gnss_head->rte_desc = trk_desc;
        track_add_head(gnss_head);
      }
      // Create a waypoint from the fix record data
      if (sscanf(ibuf,
                 "B%2d%2d%2d%2u%2u%3u%1[NS]%3u%2u%3u%1[WE]%c%5d%5d",
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

      tod = QTime(hours, mins, secs);
      if (!tod.isValid()) {
        fatal(MYNAME ": bad time\n%s\n", ibuf);
      }

      // Increment date if we pass midnight UTC
      if (prev_tod.isValid() && (tod < prev_tod)) {
        date = date.addDays(1);
      }
      prev_tod = tod;

      /*
       * Parse any extension data present. If no extensions are used (unlikely,
       * but possible in the case of homebrew flight recorders), then skip the
       * whole thing.
      */

      // TODO: Iterate over the ext_types_hash hash map and check all extension data
      if (igc_metadata.flags.has_igc_exts) {
        auto* fsdata = new igc_fsdata;
        if (igc_metadata.flags.enl){
          bool int_ok;
          int len = igc_metadata.extension("ENL")->end - igc_metadata.extension("ENL")->start+1;  // The difference results in a fencepost error
          ext_data = QStringView(ibuf_q).mid(igc_metadata.extension("ENL")->start, len);
          fsdata->enl = ext_data.toInt(&int_ok);
          if (!int_ok) {
            printf(MYNAME ": Fatal parsing error at line %i, character %i. Problem line:\n",current_line,  igc_metadata.extension("ENL")->start);
            printf(MYNAME ": %s\n",ibuf_q.toUtf8().constData());
            printf(MYNAME ": Cannot convert engine noise data %s to integer value.\n", ext_data.toUtf8().constData());
            fatal(MYNAME ": Fatal error processing IGC file.\n");
          } else {
            pres_wpt->fs.FsChainAdd(fsdata);
          }
        }
      }

      pres_wpt->SetCreationTime(QDateTime(date, tod, Qt::UTC));

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
    }
    case rec_task:
      // Create a route for each pre-flight declaration
      task_record_reader.igc_task_rec(ibuf);
      break;

    case rec_log_book:
      // Get the log book sub type
      if (sscanf(ibuf, "L%3s", tmp_str) != 1) {
        fatal(MYNAME ": log book (L) record parse error\n'%s'\n", ibuf);
      }

      if (strcmp(tmp_str, "PFC") == 0) {
        // Create a route for each post-flight declaration
        task_record_reader.igc_task_rec(ibuf + 4);
        break;
      } else if (global_opts.debug_level) {
        if (strcmp(tmp_str, "OOI") == 0) {
          printf(MYNAME ": Observer Input> %s\n", ibuf + 4);
        } else if (strcmp(tmp_str, "PLT") == 0) {
          printf(MYNAME ": Pilot Input> %s\n", ibuf + 4);
        } else if (strcmp(tmp_str, manufacturer) == 0) {
          printf(MYNAME ": Manufacturer Input> %s\n", ibuf + 4);
        } else {
          printf(MYNAME ": Anonymous Input> %s\n", ibuf + 1);
        }
      }
      break;

    case rec_fix_defn:
    { // We need to scope this, or the compiler complains "transfer of control bypasses initialization of:"
      // Not sure exactly what that means... something something scoping and initialization
      /*
         The first three characters define the number of extensions present.
         We don't particularly care about that. After that, every group of seven
         bytes is 4 digits followed by three letters, specifying start end end
         bytes of each extension, and the kind of extension (always three chars)
      */

      /*
       * First, construct a hash of all available extensions.
       * It may be better to construct the map and parse the record at the same time.
       * However, for now we are contructing the hash map first, then iterating through it.
       * The values in this hash contain all the necessary information on various extensions.
      */
      // TODO: Return this hash map for later use
      QHash<QString, short> ext_types_hash;
      QHash<QString, short>::const_iterator i;
      for (unsigned i=3; i < ibuf_q.length(); i+=7) {
        QString ext_type = ibuf_q.mid(i+4, 3);
        if (ext_type == "ENL") {
          ext_types_hash.insert(ibuf_q.mid(i, 7), ext_rec_enl);
        } else if (ext_type == "TAS") {
          ext_types_hash.insert(ibuf_q.mid(i, 7), ext_rec_tas);
        } else if (ext_type == "VAT") {
          ext_types_hash.insert(ibuf_q.mid(i, 7), ext_rec_vat);
        } else if (ext_type == "OAT") {
          ext_types_hash.insert(ibuf_q.mid(i, 7), ext_rec_oat);
        } else if (ext_type == "TRT") {
          ext_types_hash.insert(ibuf_q.mid(i, 7), ext_rec_trt);
        } else if (ext_type == "GSP") {
          ext_types_hash.insert(ibuf_q.mid(i, 7), ext_rec_gsp);
        } else if (ext_type == "FXA") {
          ext_types_hash.insert(ibuf_q.mid(i, 7), ext_rec_fxa);
        }
      }
      /*
       * Now we have a hash list. The values of that list contain the extension
       * type and begin and end bytes of each extension definition. Iterate through
       * the hash and parse individual hash values.
      */
      for (i = ext_types_hash.constBegin(); i != ext_types_hash.constEnd(); ++i) {
        short begin = QStringView(i.key().mid(0,2)).toInt();
        short end = QStringView(i.key().mid(2,2)).toInt();
        QStringView ext_record_type = QStringView(i.key().mid(4,3)).toString();

        extension = igc_metadata.extension(ext_record_type.toString());
        extension->start = begin;
        extension->end = end;
        extension->name = ext_record_type.toString();
        extension->exists = true;
      }
    }

    // These record types are discarded
    case rec_diff_gps:
    case rec_event:
    case rec_constel:
    case rec_security:
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

void IgcFormat::detect_pres_track(const route_head* rh)
{
  if (rh->rte_name.startsWith(kPresTrkName)) {
    head = rh;
  }
}

void IgcFormat::detect_gnss_track(const route_head* rh)
{
  if (rh->rte_name.startsWith(kGNSSTrkName)) {
    head = rh;
  }
}

void IgcFormat::detect_other_track(const route_head* rh, int& max_waypt_ct)
{
  if (!head) {
    max_waypt_ct = 0;
  }
  // Find other track with the most waypoints
  if (rh->rte_waypt_ct() > max_waypt_ct &&
      (rh->rte_name.isEmpty() ||
       (!rh->rte_name.startsWith(kPresTrkName) &&
       !rh->rte_name.startsWith(kGNSSTrkName)))) {
    head = rh;
    max_waypt_ct = rh->rte_waypt_ct();
  }
}

/*
 * Identify the pressure altitude and GNSS altitude tracks.
 * @param  pres_track  Set by the function to the pressure altitude track
 *                     head.  NULL if not found.
 * @param  gnss_track  Set by the function to the GNSS altitude track
 *                     head.  NULL if not found.
 */
void IgcFormat::get_tracks(const route_head** pres_track, const route_head** gnss_track)
{
  head = nullptr;
  auto detect_pres_track_lambda = [this](const route_head* rte)->void {
    detect_pres_track(rte);
  };
  track_disp_all(detect_pres_track_lambda, nullptr, nullptr);
  *pres_track = head;

  head = nullptr;
  auto detect_gnss_track_lambda = [this](const route_head* rte)->void {
    detect_gnss_track(rte);
  };
  track_disp_all(detect_gnss_track_lambda, nullptr, nullptr);
  *gnss_track = head;

  head = nullptr;
  int max_waypt_ct{};
  auto detect_other_track_lambda = [this, &max_waypt_ct](const route_head* rte)->void {
    detect_other_track(rte, max_waypt_ct);
  };
  track_disp_all(detect_other_track_lambda, nullptr, nullptr);

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

QByteArray IgcFormat::latlon2str(const Waypoint* wpt)
{
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

  char str[18];
  if (snprintf(str, sizeof(str), "%02ld%05ld%c%03ld%05ld%c",
               lat_digits.quot, lat_digits.rem, lat_hemi, lon_digits.quot, lon_digits.rem, lon_hemi) != 17) {
    fatal(MYNAME ": Bad waypoint format '%s'\n", str);
  }
  return str;
}

QByteArray IgcFormat::date2str(const gpsbabel::DateTime& dt) const
{
  QByteArray str = dt.toUTC().toString("ddMMyy").toUtf8();
  if (str.size() != 6) {
    fatal(MYNAME ": Bad date format '%s'\n", str.constData());
  }
  return str;
}

QByteArray IgcFormat::tod2str(const gpsbabel::DateTime& tod) const
{
  QByteArray str = tod.toUTC().toString("hhmmss").toUtf8();
  if (str.size() != 6) {
    fatal(MYNAME ": Bad time of day format '%s'\n", str.constData());
  }
  return str;
}

/*
 * Write header records
 */
void IgcFormat::wr_header()
{
  const route_head* pres_track;
  const route_head* track;

  get_tracks(&pres_track, &track);
  if (!track && pres_track) {
    track = pres_track;
  }

  gpsbabel::DateTime date;
  if (track != nullptr) {
    // Date in header record is that of the first fix record
    date = track->waypoint_list.front()->GetCreationTime();
    if (!date.isValid()) {
      fatal(MYNAME ": Bad track timestamp\n");
    }
  } else {
    // This is a bit silly, there aren't any tracks!
    assert(track_count() == 0);
    // During test, creation_time is 0 seconds since epoch
    // which gpsbabel::DateTime considers invalid but QDateTime considers valid.
    date = current_time();
    assert(date.isValid() || gpsbabel_testmode());
  }
  
  gbfprintf(file_out, "HFDTE%s\r\n", date2str(date).constData());

  // Other header data may have been stored in track description
  if (track && track->rte_desc.startsWith(HDRMAGIC)) {
    QString desc = track->rte_desc.mid(QString(HDRMAGIC).size());
#if (QT_VERSION < QT_VERSION_CHECK(5, 15, 0))
    const QStringList fields = desc.split(HDRDELIM, QString::SkipEmptyParts);
#else
    const QStringList fields = desc.split(HDRDELIM, Qt::SkipEmptyParts);
#endif
    for (const auto& field : fields) {
      gbfprintf(file_out, "%s\r\n", CSTR(field));
    }
  } else {
    // IGC header info not found so synthesise it.
    QString pilot;
    // If a waypoint is supplied with a short name of "PILOT", use
    // its description as the pilot's name in the header.
    const Waypoint* wpt = find_waypt_by_name("PILOT");
    if ((nullptr != wpt) && !wpt->description.isEmpty()) {
      pilot = wpt->description;
    } else {
      pilot = "Unknown";
    }
    gbfprintf(file_out, "HFPLTPILOT:%s\r\n", CSTRc(pilot));
  }
}

/*************************************************
 * Generation of IGC task declaration records
 */

void IgcFormat::wr_task_wpt_name(const Waypoint* wpt, const char* alt_name)
{
  gbfprintf(file_out, "C%s%s\r\n", latlon2str(wpt).constData(),
            !wpt->description.isEmpty() ? CSTR(wpt->description) : !wpt->shortname.isEmpty() ? CSTR(wpt->shortname) : alt_name);
}

void IgcFormat::wr_task_hdr(const route_head* rte, unsigned int task_num)
{
  unsigned char have_takeoff = 0;
  char flight_date[7] = "000000";
  char task_desc[kMaxRecLen] = "";
  int num_tps = rte->rte_waypt_ct() - 2;

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
  gpsbabel::DateTime rte_time = wpt->GetCreationTime().isValid() ? wpt->GetCreationTime() : current_time();
  // Either rte_time is valid, or during test, it is 0 seconds since epoch
  // which gpsbabel::DateTime considers invalid but QDateTime considers valid.
  assert(rte_time.isValid() || gpsbabel_testmode());

  if (!rte->rte_desc.isEmpty()) {
    // desc will be something like "IGCDATE160701: 500KTri"
    sscanf(CSTR(rte->rte_desc), DATEMAGIC "%6[0-9]: %s", flight_date, task_desc);
  }

  gbfprintf(file_out, "C%s%s%s%04u%02u%s\r\n", date2str(rte_time).constData(),
            tod2str(rte_time).constData(), flight_date, task_num, num_tps, task_desc);

  if (!have_takeoff) {
    // Generate the takeoff waypoint
    wr_task_wpt_name(wpt, "TAKEOFF");
  }
}

void IgcFormat::wr_task_wpt(const Waypoint* wpt)
{
  wr_task_wpt_name(wpt, "");
}

void IgcFormat::wr_task_tlr(const route_head* rte)
{
  // If the landing waypoint is not supplied we need to generate it.
  const Waypoint* wpt = rte->waypoint_list.back();
  QString sn = wpt->shortname;
//  if (!wpt->shortname || strncmp(wpt->shortname, "LANDIN", 6) != 0) {
  if (sn.isEmpty() || !sn.startsWith("LANDIN")) {
    wr_task_wpt_name(wpt, "LANDING");
  }
}

void IgcFormat::wr_tasks()
{
  unsigned int task_num = 1;
  auto wr_task_hdr_lambda = [this, &task_num](const route_head* rte)->void {
    wr_task_hdr(rte, task_num++);
  };
  auto wr_task_tlr_lambda = [this](const route_head* rte)->void {
    wr_task_tlr(rte);
  };
  auto wr_task_wpt_lambda = [this](const Waypoint* waypointp)->void {
    wr_task_wpt(waypointp);
  };
  route_disp_all(wr_task_hdr_lambda, wr_task_tlr_lambda, wr_task_wpt_lambda);
}

/*
 * Write a single fix record
 */
void IgcFormat::wr_fix_record(const Waypoint* wpt, int pres_alt, int gnss_alt)
{
  gpsbabel::DateTime tt = wpt->GetCreationTime();
  if (!tt.isValid()) {
    fatal(MYNAME ": Bad track timestamp\n");
  }

  if (unknown_alt == pres_alt) {
    pres_alt = 0;
  }
  if (unknown_alt == gnss_alt) {
    gnss_alt = 0;
  }
  gbfprintf(file_out, "B%s%sA%05d%05d\r\n", tod2str(tt).constData(),
            latlon2str(wpt).constData(), pres_alt, gnss_alt);
}

/**
 * Attempt to align the pressure and GNSS tracks in time.
 * This is useful when trying to merge a track (lat/lon/time) recorded by a
 * GPS with a barograph (alt/time) recorded by a separate instrument with
 * independent clocks which are not closely synchronised.
 * @return The number of seconds to add to the GNSS track in order to align
 *         it with the pressure track.
 */
int IgcFormat::correlate_tracks(const route_head* pres_track, const route_head* gnss_track) const
{
  double alt_diff;
  double speed;

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
  gpsbabel::DateTime pres_time = (*std::prev(wpt_rit))->GetCreationTime();
  if (global_opts.debug_level >= 1) {
    printf(MYNAME ": pressure landing time %s\n", CSTR(pres_time.toPrettyString()));
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
    int deltat_msec = (*wpt_rit)->GetCreationTime().msecsTo(wpt->GetCreationTime());
    speed = (deltat_msec == 0) ? 0:
            radtometers(gcdist(RAD(wpt->latitude), RAD(wpt->longitude),
                               RAD((*wpt_rit)->latitude), RAD((*wpt_rit)->longitude))) /
                        (0.001 * deltat_msec);
    if (global_opts.debug_level >= 2) {
      printf(MYNAME ": speed=%.2fm/s\n", speed);
    }
  } while (speed < 2.5);
  gpsbabel::DateTime gnss_time = (*std::prev(wpt_rit))->GetCreationTime();
  if (global_opts.debug_level >= 1) {
    printf(MYNAME ": gnss landing time %s\n", CSTR(gnss_time.toPrettyString()));
  }
  // Time adjustment is difference between the two estimated landing times
  int time_diff = gnss_time.secsTo(pres_time);
  if (15 * 60 < abs(time_diff)) {
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
double IgcFormat::Interpolater::interpolate_alt(const route_head* track, const gpsbabel::DateTime& time)
{
  int time_diff;

  // Start search at the beginning of the track
  if (!prev_wpt.has_value()) {
    prev_wpt = track->waypoint_list.cbegin();
    curr_wpt = track->waypoint_list.cbegin();
  }
  // Find the track points either side of the requested time
  while ((track->waypoint_list.cend() != *curr_wpt) &&
         ((**curr_wpt)->GetCreationTime() < time)) {
    prev_wpt = *curr_wpt;
    curr_wpt = std::next(*prev_wpt);
  }
  if (track->waypoint_list.cend() == *curr_wpt) {
    // Requested time later than all track points, we can't interpolate
    return unknown_alt;
  }

  if (track->waypoint_list.cbegin() == *curr_wpt) {
    if ((**curr_wpt)->GetCreationTime() == time) {
      // First point's creation time is an exact match so use it's altitude
      return (**curr_wpt)->altitude;
    } else {
      // Requested time is prior to any track points, we can't interpolate
      return unknown_alt;
    }
  }
  // Interpolate
  if (0 == (time_diff = (**prev_wpt)->GetCreationTime().secsTo((**curr_wpt)->GetCreationTime()))) {
    // Avoid divide by zero
    return (**curr_wpt)->altitude;
  }
  double alt_diff = (**curr_wpt)->altitude - (**prev_wpt)->altitude;
  return (**prev_wpt)->altitude + (alt_diff / time_diff) * ((**prev_wpt)->GetCreationTime().secsTo(time));
}

/*
 * Pressure altitude and GNSS altitude may be provided in two separate
 * tracks.  This function attempts to merge them into one.
 */
void IgcFormat::wr_track()
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
    Interpolater interpolater;
    foreach (const Waypoint* wpt, gnss_track->waypoint_list) {
      double pres_alt = interpolater.interpolate_alt(pres_track, wpt->GetCreationTime().addSecs(time_adj));
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

void IgcFormat::wr_init(const QString& fname)
{
  file_out = gbfopen(fname, "wb", MYNAME);
}

void IgcFormat::wr_deinit()
{
  gbfclose(file_out);
}

void IgcFormat::write()
{
  gbfputs("AXXXZZZGPSBabel\r\n", file_out);
  wr_header();
  wr_tasks();
  wr_track();
  gbfprintf(file_out, "LXXXGenerated by GPSBabel Version %s\r\n", gpsbabel_version);
  gbfputs("GGPSBabelSecurityRecordGuaranteedToFailVALIChecks\r\n", file_out);
}
