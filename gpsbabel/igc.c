/*
 * FAI/IGC data format translation.
 * 
 * Refer to Appendix 1 of
 * http://www.fai.org:81/gliding/gnss/tech_spec_gnss.asp for the
 * specification of the IGC data format.  This translation code was
 * written when the latest ammendment list for the specification was AL6.
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
 * 59 Temple Place - Suite 330, Boston, MA 02111 USA 
 */

#include "defs.h"
#include <errno.h>

static gbfile *file_in, *file_out;
static char manufacturer[4];
static const route_head *head;
static char *timeadj = NULL;

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
typedef enum {
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
} igc_rec_type_t;

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
static igc_rec_type_t get_record(char **rec)
{
    size_t len;
    char *c;
retry:
    *rec = c = gbfgetstr(file_in);
    if (c == NULL) return rec_none;

    len = strlen(c);

    /* Trackwiev writes (bogus) blank links between each record */
    if (len == 0) goto retry;

    if (len < 3 || c[0] < 'A' || c[0] > 'Z') {
	warning(MYNAME " bad input record: '%s'\n", c);
	return rec_bad;
    }
    return (igc_rec_type_t) c[0];
}

static void rd_init(const char *fname)
{
    char *ibuf;

    file_in = gbfopen(fname, "r", MYNAME);
    if (gbfunicode(file_in)) cet_convert_init(CET_CHARSET_UTF8, 1);

    // File must begin with a manufacturer/ID record
    if (get_record(&ibuf) != rec_manuf_id || sscanf(ibuf, "A%3[A-Z]", manufacturer) != 1) {
	fatal(MYNAME ": %s is not an IGC file\n", fname);
    }
}

static void rd_deinit(void)
{
    gbfclose(file_in);
}

/**
 * Handle pre- or post-flight task declarations.
 * A route is created for each set of waypoints in a task declaration.
 * @param rec A single task record
 */
static void igc_task_rec(const char *rec)
{
    static char flight_date[7];
    static unsigned int num_tp, tp_ct;
    static route_head *rte_head;
    static time_t creation;

    char task_num[5];
    char task_desc[MAXRECLEN];
    waypoint *wpt;
    unsigned int lat_deg, lat_min, lat_frac;
    unsigned int lon_deg, lon_min, lon_frac;
    char lat_hemi[2], lon_hemi[2];
    char short_name[8];
    char tmp_str[MAXRECLEN];
    struct tm tm;

    static enum { id, takeoff, start, turnpoint, finish, landing } state = id;

    // First task record identifies the task to follow
    if (id == state) {
	task_desc[0] = '\0';
	if (sscanf(rec, "C%2u%2u%2u%2u%2u%2u%6[0-9]%4c%2u%[^\r]\r\n",
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
	rte_head = route_head_alloc();
	rte_head->rte_name = xstrdup(task_num);
	sprintf(tmp_str, DATEMAGIC "%s: %s", flight_date, task_desc);
	rte_head->rte_desc = xstrdup(tmp_str);
	route_add_head(rte_head);
	state++;
	return;
    }
    // Get the waypoint
    tmp_str[0] = '\0';
    if (sscanf(rec, "C%2u%2u%3u%1[NS]%3u%2u%3u%1[WE]%[^\r]\r\n",
	       &lat_deg, &lat_min, &lat_frac, lat_hemi,
	       &lon_deg, &lon_min, &lon_frac, lon_hemi, tmp_str) < 8) {
	fatal(MYNAME ": task waypoint (C) record parse error\n%s", rec);
    }

    wpt = waypt_new();
    wpt->latitude = ('N' == lat_hemi[0] ? 1 : -1) *
	(lat_deg + (lat_min * 1000 + lat_frac) / 1000.0 / 60);

    wpt->longitude = ('E' == lon_hemi[0] ? 1 : -1) *
	(lon_deg + (lon_min * 1000 + lon_frac) / 1000.0 / 60);

    wpt->creation_time = creation;
    wpt->description = xstrdup(tmp_str);

    // Name the waypoint according to the order of the task record
    switch (state) {
    case takeoff:
	snprintf(short_name, 8, "TAKEOFF");
	state++;
	break;

    case start:
	snprintf(short_name, 8, "START");
	tp_ct = 0;
	state++;
	break;

    case turnpoint:
	if (++tp_ct == num_tp) {
	    state++;
	}
	snprintf(short_name, 8, "TURN%02u", tp_ct);
	break;

    case finish:
	snprintf(short_name, 8, "FINISH");
	state++;
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
	waypt_free(wpt);
	return;
    }
    wpt->shortname = xstrdup(short_name);
    route_add_wpt(rte_head, wpt);
}

static void data_read(void)
{
    char *ibuf;
    igc_rec_type_t rec_type;
    unsigned int hours, mins, secs;
    unsigned int lat_deg, lat_min, lat_frac;
    unsigned int lon_deg, lon_min, lon_frac;
    char lat_hemi[2], lon_hemi[2];
    char validity;
    route_head *pres_head = NULL;
    route_head *gnss_head = NULL;
    int pres_alt, gnss_alt;
    char pres_valid = 0;
    char gnss_valid = 0;
    waypoint *pres_wpt = NULL;
    waypoint *gnss_wpt = NULL;
    time_t date = 0;
    time_t prev_tod = 0;
    time_t tod;
    struct tm tm;
    char tmp_str[20];
    char *hdr_data;
    size_t remain;
    char trk_desc[MAXDESCLEN + 1];

    strcpy(trk_desc, HDRMAGIC HDRDELIM);

    while (1) {
	rec_type = get_record(&ibuf);
	switch (rec_type) {
	case rec_manuf_id:
	    // Manufacturer/ID record already found in rd_init().
	    warning(MYNAME ": duplicate manufacturer/ID record\n");
	    break;

	case rec_header:
	    // Get the header sub type
	    if (sscanf(ibuf, "H%*1[FOP]%3s", tmp_str) != 1) {
		fatal(MYNAME ": header (H) record parse error\n%s\n%s\n", ibuf, tmp_str);
	    }
	    // Optional long name of record sub type is followed by a
	    // colon.  Actual header data follows that.
	    if (NULL == (hdr_data = strchr(ibuf, ':'))) {
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
		    strcat(ibuf, HDRDELIM);
		    remain = MAXDESCLEN - strlen(trk_desc);
		    strncat(trk_desc, ibuf, remain);
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
		pres_head = route_head_alloc();
		pres_head->rte_name = xstrdup(PRESTRKNAME);
		pres_head->rte_desc = xstrdup(trk_desc);
		track_add_head(pres_head);
	    }
	    // Create a second track for GNSS altitude waypoints
	    if (!gnss_head) {
		gnss_head = route_head_alloc();
		gnss_head->rte_name = xstrdup(GNSSTRKNAME);
		gnss_head->rte_desc = xstrdup(trk_desc);
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
	    pres_wpt = waypt_new();

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
	    pres_wpt->creation_time = date + tod;

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
	    gnss_wpt = waypt_dupe(pres_wpt);

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
		pres_head = NULL;
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

static void detect_pres_track(const route_head * rh)
{
    if (rh->rte_name && strncmp(rh->rte_name, PRESTRKNAME, 6) == 0) {
	head = rh;
    }
}

static void detect_gnss_track(const route_head * rh)
{
    if (rh->rte_name && strncmp(rh->rte_name, GNSSTRKNAME, 6) == 0) {
	head = rh;
    }
}

static void detect_other_track(const route_head * rh)
{
    static int max_waypt_ct;

    if (!head) {
	max_waypt_ct = 0;
    }
    // Find other track with the most waypoints
    if (rh->rte_waypt_ct > max_waypt_ct &&
	(!rh->rte_name ||
	 (strncmp(rh->rte_name, PRESTRKNAME, 6) != 0 &&
	  strncmp(rh->rte_name, GNSSTRKNAME, 6) != 0))) {
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
static void get_tracks(const route_head ** pres_track, const route_head ** gnss_track)
{
    head = NULL;
    track_disp_all(detect_pres_track, NULL, NULL);
    *pres_track = head;

    head = NULL;
    track_disp_all(detect_gnss_track, NULL, NULL);
    *gnss_track = head;

    head = NULL;
    track_disp_all(detect_other_track, NULL, NULL);

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

static char *latlon2str(const waypoint * wpt)
{
    static char str[18] = "";
    char lat_hemi = wpt->latitude < 0 ? 'S' : 'N';
    char lon_hemi = wpt->longitude < 0 ? 'W' : 'E';
    unsigned char lat_deg = fabs(wpt->latitude);
    unsigned char lon_deg = fabs(wpt->longitude);
    unsigned int lat_min = (fabs(wpt->latitude) - lat_deg) * 60000 + 0.500000000001;
    unsigned int lon_min = (fabs(wpt->longitude) - lon_deg) * 60000 + 0.500000000001;

    if (snprintf(str, 18, "%02u%05u%c%03u%05u%c",
		 lat_deg, lat_min, lat_hemi, lon_deg, lon_min, lon_hemi) != 17) {
	fatal(MYNAME ": Bad waypoint format '%s'\n", str);
    }
    return str;
}

static char *date2str(struct tm *dt)
{
    static char str[7] = "";

    if (snprintf(str, 7, "%02u%02u%02u", dt->tm_mday, dt->tm_mon + 1, dt->tm_year % 100) != 6) {
	fatal(MYNAME ": Bad date format '%s'\n", str);
    }
    return str;
}

static char *tod2str(struct tm *tod)
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
static void wr_header(void)
{
    const route_head *pres_track;
    const route_head *track;
    struct tm *tm;
    time_t date;
    static const char dflt_str[] = "Unknown";
    const char *str;
    waypoint *wpt;

    get_tracks(&pres_track, &track);
    if (!track && pres_track) {
	track = pres_track;
    }
    // Date in header record is that of the first fix record
    date = !track ? current_time() :
	((waypoint *) QUEUE_FIRST(&track->waypoint_list))->creation_time;

    if (NULL == (tm = gmtime(&date))) {
	fatal(MYNAME ": Bad track timestamp\n");
    }
    gbfprintf(file_out, "HFDTE%s\r\n", date2str(tm));

    // Other header data may have been stored in track description
    if (track && track->rte_desc && strncmp(track->rte_desc, HDRMAGIC, strlen(HDRMAGIC)) == 0) {
	for (str = strtok(track->rte_desc + strlen(HDRMAGIC) + strlen(HDRDELIM), HDRDELIM);
	     str; str = strtok(NULL, HDRDELIM)) {
	    gbfprintf(file_out, "%s\r\n", str);
	}
    } else {
	// IGC header info not found so synthesise it.
	// If a waypoint is supplied with a short name of "PILOT", use
	// its description as the pilot's name in the header.
	str = dflt_str;
	if (NULL != (wpt = find_waypt_by_name("PILOT")) && wpt->description) {
	    str = wpt->description;
	}
	gbfprintf(file_out, "HFPLTPILOT:%s\r\n", str);
    }
}

/*************************************************
 * Generation of IGC task declaration records
 */

static void wr_task_wpt_name(const waypoint * wpt, const char *alt_name)
{
    gbfprintf(file_out, "C%s%s\r\n", latlon2str(wpt),
	     wpt->description ? wpt->description : wpt->shortname ? wpt->shortname : alt_name);
}

static void wr_task_hdr(const route_head * rte)
{
    unsigned char have_takeoff = 0;
    const waypoint *wpt;
    char flight_date[7] = "000000";
    char task_desc[MAXRECLEN] = "";
    int num_tps = rte->rte_waypt_ct - 2;
    struct tm *tm;
    time_t rte_time;
    static unsigned int task_num = 1;

    if (num_tps < 0) {
	fatal(MYNAME ": Empty task route\n");
    }
    // See if the takeoff and landing waypoints are there or if we need to
    // generate them.
    wpt = (waypoint *) QUEUE_LAST(&rte->waypoint_list);
    if (wpt->shortname && strncmp(wpt->shortname, "LANDING", 6) == 0) {
	num_tps--;
    }
    wpt = (waypoint *) QUEUE_FIRST(&rte->waypoint_list);
    if (wpt->shortname && strncmp(wpt->shortname, "TAKEOFF", 6) == 0) {
	have_takeoff = 1;
	num_tps--;
    }
    if (num_tps < 0) {
	fatal(MYNAME ": Too few waypoints in task route\n");
    }
    else if (num_tps > 99) {
	fatal(MYNAME ": Too much waypoints (more than 99) in task route.\n");
    }
    // Gather data to write to the task identification (first) record
    rte_time = wpt->creation_time ? wpt->creation_time : current_time();
    if (NULL == (tm = gmtime(&rte_time))) {
	fatal(MYNAME ": Bad task route timestamp\n");
    }

    if (rte->rte_desc) {
	sscanf(rte->rte_desc, DATEMAGIC "%6[0-9]: %s", flight_date, task_desc);
    }

    gbfprintf(file_out, "C%s%s%s%04u%02u%s\r\n", date2str(tm),
	     tod2str(tm), flight_date, task_num++, num_tps, task_desc);

    if (!have_takeoff) {
	// Generate the takeoff waypoint
	wr_task_wpt_name(wpt, "TAKEOFF");
    }
}

static void wr_task_wpt(const waypoint * wpt)
{
    wr_task_wpt_name(wpt, "");
}

static void wr_task_tlr(const route_head * rte)
{
    // If the landing waypoint is not supplied we need to generate it.
    const waypoint *wpt = (waypoint *) QUEUE_LAST(&rte->waypoint_list);
    if (!wpt->shortname || strncmp(wpt->shortname, "LANDIN", 6) != 0) {
	wr_task_wpt_name(wpt, "LANDING");
    }
}

static void wr_tasks(void)
{
    route_disp_all(wr_task_hdr, wr_task_tlr, wr_task_wpt);
}

/*
 * Write a single fix record
 */
static void wr_fix_record(const waypoint * wpt, int pres_alt, int gnss_alt)
{
    struct tm *tm;

    if (NULL == (tm = gmtime(&wpt->creation_time))) {
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
 * GPS with a barograph (alt/time) recorded by a seperate instrument with
 * independent clocks which are not closely synchronised.
 * @return The number of seconds to add to the GNSS track in order to align
 *         it with the pressure track.
 */
static int correlate_tracks(const route_head * pres_track, const route_head * gnss_track)
{
    const queue *elem;
    double last_alt, alt_diff;
    double speed;
    time_t pres_time, gnss_time;
    int time_diff;
    const waypoint *wpt;

    // Deduce the landing time from the pressure altitude track based on
    // when we last descended to within 10m of the final track altitude.
    elem = QUEUE_LAST(&pres_track->waypoint_list);
    last_alt = ((waypoint *) elem)->altitude;
    do {
	elem = elem->prev;
	if (&pres_track->waypoint_list == elem) {
	    // No track left
	    return 0;
	}
	alt_diff = last_alt - ((waypoint *) elem)->altitude;
	if (alt_diff > 10.0) {
	    // Last part of track was ascending
	    return 0;
	}
    } while (alt_diff > -10.0);
    pres_time = ((waypoint *) elem->next)->creation_time;
    if (global_opts.debug_level >= 1) {
	printf(MYNAME ": pressure landing time %s", ctime(&pres_time));
    }
    // Deduce the landing time from the GNSS altitude track based on
    // when the groundspeed last dropped below a certain level.
    elem = QUEUE_LAST(&gnss_track->waypoint_list);
    last_alt = ((waypoint *) elem)->altitude;
    do {
	wpt = (waypoint *) elem;
	elem = elem->prev;
	if (&gnss_track->waypoint_list == elem) {
	    // No track left
	    return 0;
	}
	// Get a crude indication of groundspeed from the change in lat/lon
	time_diff = wpt->creation_time - ((waypoint *) elem)->creation_time;
	speed = !time_diff ? 0 :
	    (fabs(wpt->latitude - ((waypoint *) elem)->latitude) +
	     fabs(wpt->longitude - ((waypoint *) elem)->longitude)) / time_diff;
	if (global_opts.debug_level >= 2) {
	    printf(MYNAME ": speed=%f\n", speed);
	}
    } while (speed < 0.00003);
    gnss_time = ((waypoint *) elem->next)->creation_time;
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
static double interpolate_alt(const route_head * track, time_t time)
{
    static const queue *prev_elem = NULL;
    static const queue *curr_elem = NULL;
    const waypoint *prev_wpt;
    const waypoint *curr_wpt;
    int time_diff;
    double alt_diff;

    // Start search at the beginning of the track
    if (!prev_elem) {
	curr_elem = prev_elem = QUEUE_FIRST(&track->waypoint_list);
    }
    // Find the track points either side of the requested time
    while (((waypoint *) curr_elem)->creation_time < time) {
	if (QUEUE_LAST(&track->waypoint_list) == curr_elem) {
	    // Requested time later than all track points, we can't interpolate
	    return unknown_alt;
	}
	prev_elem = curr_elem;
	curr_elem = QUEUE_NEXT(prev_elem);
    }

    prev_wpt = (waypoint *) prev_elem;
    curr_wpt = (waypoint *) curr_elem;

    if (QUEUE_FIRST(&track->waypoint_list) == curr_elem) {
	if (curr_wpt->creation_time == time) {
	    // First point's creation time is an exact match so use it's altitude
	    return curr_wpt->altitude;
	} else {
	    // Requested time is prior to any track points, we can't interpolate
	    return unknown_alt;
	}
    }
    // Interpolate
    if (0 == (time_diff = curr_wpt->creation_time - prev_wpt->creation_time)) {
	// Avoid divide by zero
	return curr_wpt->altitude;
    }
    alt_diff = curr_wpt->altitude - prev_wpt->altitude;
    return prev_wpt->altitude + (alt_diff / time_diff) * (time - prev_wpt->creation_time);
}

/*
 * Pressure altitude and GNSS altitude may be provided in two seperate
 * tracks.  This function attempts to merge them into one.
 */
static void wr_track(void)
{
    const route_head *pres_track;
    const route_head *gnss_track;
    const waypoint *wpt;
    const queue *elem;
    const queue *tmp;
    int time_adj;
    double pres_alt;

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
	QUEUE_FOR_EACH(&gnss_track->waypoint_list, elem, tmp) {
	    wpt = (waypoint *) elem;
	    pres_alt = interpolate_alt(pres_track, wpt->creation_time + time_adj);
	    wr_fix_record(wpt, (int) pres_alt, (int) wpt->altitude);
	}
    } else {
	if (pres_track) {
	    // Only the pressure altitude track was found so generate fix
	    // records from it alone.
	    QUEUE_FOR_EACH(&pres_track->waypoint_list, elem, tmp) {
		wr_fix_record((waypoint *) elem, (int) ((waypoint *) elem)->altitude, (int) unknown_alt);
	    }
	} else if (gnss_track) {
	    // Only the GNSS altitude track was found so generate fix
	    // records from it alone.
	    QUEUE_FOR_EACH(&gnss_track->waypoint_list, elem, tmp) {
		wr_fix_record((waypoint *) elem, (int) unknown_alt, (int) ((waypoint *) elem)->altitude);
	    }
	} else {
	    // No tracks found so nothing to do
	    return;
	}
    }
}

static void wr_init(const char *fname)
{
    file_out = gbfopen(fname, "wb", MYNAME);
}

static void wr_deinit(void)
{
    gbfclose(file_out);
}

static void data_write(void)
{
    gbfputs("AXXXZZZGPSBabel\r\n", file_out);
    wr_header();
    wr_tasks();
    wr_track();
    gbfprintf(file_out, "LXXXGenerated by GPSBabel Version %s\r\n", gpsbabel_version);
    gbfputs("GGPSBabelSecurityRecordGuaranteedToFailVALIChecks\r\n", file_out);
}


static arglist_t igc_args[] = {
    {"timeadj", &timeadj,
     "(integer sec or 'auto') Barograph to GPS time diff", 
     NULL, ARGTYPE_STRING, ARG_NOMINMAX},
    ARG_TERMINATOR
};

ff_vecs_t igc_vecs = {
    ff_type_file,
    { ff_cap_none , ff_cap_read | ff_cap_write, ff_cap_read | ff_cap_write },
    rd_init,
    wr_init,
    rd_deinit,
    wr_deinit,
    data_read,
    data_write,
    NULL, 
    igc_args,
    CET_CHARSET_ASCII, 0	/* CET-REVIEW */
};
