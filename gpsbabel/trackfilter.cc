/*

    Track manipulation filter
    Copyright (c) 2009 - 2013 Robert Lipe, robertlipe@gpsbabel.org
    Copyright (C) 2005-2006 Olaf Klein, o.b.klein@gpsbabel.org

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
/*
   2005-07-20: implemented interval option from Etienne Tasse
   2005-07-26: implemented range option
   2005-07-26: implemented move option
   2005-07-26: implemented merge option
   2005-07-29: warning fixes
   2005-08-01: Add 'static' qualifier when we can (robertl)
   2005-10-04: Add filterdefs to hold protos for filter functions... (robertl)
   2005-10-04: Fix range-check max. value; exit filter, if no more tracks left
   2006-04-06: Add fix, course, and speed options (parkrrrr)
   2006-06-01: Add name option
   2007-01-08: if not really needed disable check for valid timestamps
	(based on patch from Vladimir Kondratiev)
   2007-07-26: Allow 'range' together with trackpoints without timestamp
   2010-06-02: Add specified timestamp to each trackpoint (added by sven_luzar)
   2012-05-04: Added 'discard' option to 'merge' to throw out track points without timestamp
*/

#include <ctype.h>
#include <math.h>

#include "defs.h"
#include "filterdefs.h"
#include "strptime.h"
#include "grtcirc.h"
#include "xmlgeneric.h"

#if FILTERS_ENABLED || MINIMAL_FILTERS
#define MYNAME "trackfilter"

#define TRACKFILTER_PACK_OPTION		"pack"
#define TRACKFILTER_SPLIT_OPTION	"split"
#define TRACKFILTER_SDIST_OPTION	"sdistance"
#define TRACKFILTER_TITLE_OPTION	"title"
#define TRACKFILTER_MERGE_OPTION	"merge"
#define TRACKFILTER_NAME_OPTION		"name"
#define TRACKFILTER_STOP_OPTION		"stop"
#define TRACKFILTER_START_OPTION	"start"
#define TRACKFILTER_MOVE_OPTION		"move"
#define TRACKFILTER_FIX_OPTION          "fix"
#define TRACKFILTER_COURSE_OPTION       "course"
#define TRACKFILTER_SPEED_OPTION        "speed"
#define TRACKFILTER_SEG2TRK_OPTION      "seg2trk"
#define TRACKFILTER_TRK2SEG_OPTION      "trk2seg"
#define TRACKFILTER_SEGMENT_OPTION      "segment"
#define TRACKFILTER_FAKETIME_OPTION     "faketime"
#define TRACKFILTER_DISCARD_OPTION      "discard"

#undef TRACKF_DBG

static char *opt_merge = NULL;
static char *opt_pack = NULL;
static char *opt_split = NULL;
static char *opt_sdistance = NULL;
static char *opt_move = NULL;
static char *opt_title = NULL;
static char *opt_start = NULL;
static char *opt_stop = NULL;
static char *opt_fix = NULL;
static char *opt_course = NULL;
static char *opt_speed = NULL;
static char *opt_name = NULL;
static char *opt_seg2trk = NULL;
static char *opt_trk2seg = NULL;
static char *opt_segment = NULL;
static char *opt_faketime = NULL;
static char *opt_discard = NULL;

static
arglist_t trackfilter_args[] = {
  {
    TRACKFILTER_MOVE_OPTION, &opt_move,
    "Correct trackpoint timestamps by a delta", NULL, ARGTYPE_STRING,
    ARG_NOMINMAX
  },
  {
    TRACKFILTER_PACK_OPTION,  &opt_pack,
    "Pack all tracks into one", NULL, ARGTYPE_BOOL, ARG_NOMINMAX
  },
  {
    TRACKFILTER_SPLIT_OPTION, &opt_split,
    "Split by date or time interval (see README)", NULL,
    ARGTYPE_STRING, ARG_NOMINMAX
  },
  {
    TRACKFILTER_SDIST_OPTION, &opt_sdistance,
    "Split by distance", NULL,
    ARGTYPE_STRING, ARG_NOMINMAX
  },
  {
    TRACKFILTER_MERGE_OPTION, &opt_merge,
    "Merge multiple tracks for the same way", NULL, ARGTYPE_STRING,
    ARG_NOMINMAX
  },
  {
    TRACKFILTER_NAME_OPTION, &opt_name,
    "Use only track(s) where title matches given name", NULL, ARGTYPE_STRING,
    ARG_NOMINMAX
  },
  {
    TRACKFILTER_START_OPTION, &opt_start,
    "Use only track points after this timestamp", NULL, ARGTYPE_INT,
    ARG_NOMINMAX
  },
  {
    TRACKFILTER_STOP_OPTION, &opt_stop,
    "Use only track points before this timestamp", NULL, ARGTYPE_INT,
    ARG_NOMINMAX
  },
  {
    TRACKFILTER_TITLE_OPTION, &opt_title,
    "Basic title for new track(s)", NULL, ARGTYPE_STRING, ARG_NOMINMAX
  },
  {
    TRACKFILTER_FIX_OPTION, &opt_fix,
    "Synthesize GPS fixes (PPS, DGPS, 3D, 2D, NONE)", NULL,
    ARGTYPE_STRING, ARG_NOMINMAX
  },
  {
    TRACKFILTER_COURSE_OPTION, &opt_course, "Synthesize course",
    NULL, ARGTYPE_BOOL, ARG_NOMINMAX
  },
  {
    TRACKFILTER_SPEED_OPTION, &opt_speed, "Synthesize speed",
    NULL, ARGTYPE_BOOL, ARG_NOMINMAX
  },
  {
    TRACKFILTER_SEG2TRK_OPTION, &opt_seg2trk,
    "Split track at segment boundaries into multiple tracks",
    NULL, ARGTYPE_BOOL, ARG_NOMINMAX
  },
  {
    TRACKFILTER_TRK2SEG_OPTION, &opt_trk2seg,
    "Merge tracks inserting segment separators at boundaries",
    NULL, ARGTYPE_BOOL, ARG_NOMINMAX
  },
  {
    TRACKFILTER_SEGMENT_OPTION, &opt_segment,
    "segment tracks with abnormally long gaps",
    NULL, ARGTYPE_BOOL, ARG_NOMINMAX
  },
  {
    TRACKFILTER_FAKETIME_OPTION, &opt_faketime,
    "Add specified timestamp to each trackpoint",
    NULL, ARGTYPE_STRING, ARG_NOMINMAX
  },
  {
    TRACKFILTER_DISCARD_OPTION,  &opt_discard,  
    "Discard track points without timestamps during merge",
    NULL, ARGTYPE_BOOL, ARG_NOMINMAX
  },
  ARG_TERMINATOR
};


typedef struct trkflt_s {
  route_head *track;
  QDateTime first_time;
  QDateTime last_time;
} trkflt_t;

static trkflt_t *track_list = NULL;
static int track_ct = 0;
static int track_pts = 0;
static int timeless_pts = 0;
static int opt_interval = 0;
static int opt_distance = 0;
static char need_time;		/* initialized within trackfilter_init */

/*******************************************************************************
* helpers
*******************************************************************************/

static int
trackfilter_opt_count(void)
{
  int res = 0;
  arglist_t *a = trackfilter_args;

  while (a->argstring) {
    if (*a->argval != NULL) {
      res++;
    }
    a++;
  }
  return res;
}

static int
trackfilter_parse_time_opt(const char *arg)
{
  time_t t0, t1;
  int sign = 1;
  char *cin = (char *)arg;
  char c;

  t0 = t1 = 0;

  while ((c = *cin++)) {
    time_t seconds;

    if (c >= '0' && c <= '9') {
      t1 = (t1 * 10) + (c - '0');
      continue;
    }
    switch (tolower(c)) {
    case 'd':
      seconds = SECONDS_PER_DAY;
      break;
    case 'h':
      seconds = SECONDS_PER_HOUR;
      break;
    case 'm':
      seconds = 60;
      break;
    case 's':
      seconds = 1;
      break;
    case '+':
      sign = +1;
      continue;
    case '-':
      sign = -1;
      continue;
    default:
      fatal(MYNAME "-time: invalid character in time option!\n");
    }
    t0 += (t1 * seconds * sign);
    sign = +1;
    t1 = 0;
  }
  t0 += t1;
  return t0;
}

static int
trackfilter_init_qsort_cb(const void *a, const void *b)
{
  const trkflt_t *ra = (const trkflt_t*) a;
  const trkflt_t *rb = (const trkflt_t*) b;
  const QDateTime dta = ra->first_time;
  const QDateTime dtb = rb->first_time;

  if (dta > dtb) {
    return 1;
  }
  if (dta == dtb) {
    return 0;
  }
  return -1;
}

static int
trackfilter_merge_qsort_cb(const void *a, const void *b)
{
  const waypoint *wa = *(waypoint **)a;
  const waypoint *wb = *(waypoint **)b;
  const QDateTime dta = wa->GetCreationTime();
  const QDateTime dtb = wb->GetCreationTime();

  if (dta > dtb) {
    return 1;
  }
  if (dta == dtb) {
    return 0;
  }
  return -1;
}

static fix_type
trackfilter_parse_fix(int *nsats)
{
  if (!opt_fix) {
    return fix_unknown;
  }
  if (!case_ignore_strcmp(opt_fix, "pps")) {
    *nsats = 4;
    return fix_pps;
  }
  if (!case_ignore_strcmp(opt_fix, "dgps")) {
    *nsats = 4;
    return fix_dgps;
  }
  if (!case_ignore_strcmp(opt_fix, "3d")) {
    *nsats = 4;
    return fix_3d;
  }
  if (!case_ignore_strcmp(opt_fix, "2d")) {
    *nsats = 3;
    return fix_2d;
  }
  if (!case_ignore_strcmp(opt_fix, "none")) {
    *nsats = 0;
    return fix_none;
  }
  fatal(MYNAME ": invalid fix type\n");
  return fix_unknown;
}

static void
trackfilter_fill_track_list_cb(const route_head *track) 	/* callback for track_disp_all */
{
  int i;
  waypoint *wpt, *prev;
  queue *elem, *tmp;

  if (track->rte_waypt_ct == 0) {
    track_del_head((route_head *)track);
    return;
  }

  if (opt_name != NULL) {
    if ((track->rte_name == NULL) ||
        (case_ignore_str_match(track->rte_name, opt_name) == 0)) {
      QUEUE_FOR_EACH((queue *)&track->waypoint_list, elem, tmp) {
        waypoint *wpt = (waypoint *)elem;
        track_del_wpt((route_head *)track, wpt);
        waypt_free(wpt);
      }
      track_del_head((route_head *)track);
      return;
    }
  }

  track_list[track_ct].track = (route_head *)track;

  i = 0;
  prev = NULL;

  QUEUE_FOR_EACH((queue *)&track->waypoint_list, elem, tmp) {
    track_pts++;

    wpt = (waypoint *)elem;
    if(wpt->creation_time == 0) timeless_pts++;
    if (!(opt_merge && opt_discard) && (need_time != 0) && (wpt->creation_time == 0)) {
      fatal(MYNAME "-init: Found track point at %f,%f without time!\n",
            wpt->latitude, wpt->longitude);
    }

    i++;
    if (i == 1) {
      track_list[track_ct].first_time = wpt->GetCreationTime();
    } else if (i == track->rte_waypt_ct) {
      track_list[track_ct].last_time = wpt->GetCreationTime();
    }

    if ((need_time != 0) && (prev != NULL) && (prev->GetCreationTime() > wpt->GetCreationTime())) {
      if (opt_merge == NULL) {
        QString t1 = prev->CreationTimeXML();
        QString t2 = wpt->CreationTimeXML();
        fatal(MYNAME "-init: Track points badly ordered (timestamp %s > %s)!\n", qPrintable(t1), qPrintable(t2));
      }
    }
    prev = wpt;
  }
  track_ct++;
}

/*******************************************************************************
* track title producers
*******************************************************************************/

static void
trackfilter_split_init_rte_name(route_head *track, const QDateTime dt)
{
  char buff[128], tbuff[128];
  struct tm tm;

  time_t time = dt.toUTC().toTime_t();
  tm = *localtime(&time);

  (opt_interval != 0) ?
  strftime(tbuff, sizeof(tbuff), "%Y%m%d%H%M%S", &tm) :
  strftime(tbuff, sizeof(tbuff), "%Y%m%d", &tm);

  if ((opt_title != NULL) && (strlen(opt_title) > 0)) {
    if (strchr(opt_title, '%') != NULL) {
      strftime(buff, sizeof(buff), opt_title, &tm);
    } else {
      snprintf(buff, sizeof(buff), "%s-%s", opt_title, tbuff);
    }
  } else if ((track->rte_name != NULL) && (strlen(track->rte_name) > 0)) {
    snprintf(buff, sizeof(buff), "%s-%s", track->rte_name, tbuff);
  } else {
    strncpy(buff, tbuff, sizeof(buff));
  }

  if (track->rte_name != NULL) {
    xfree(track->rte_name);
  }
  track->rte_name = xstrdup(buff);
}

static void
trackfilter_pack_init_rte_name(route_head *track, const time_t default_time)
{
  char buff[128];

  if (strchr(opt_title, '%') != NULL) {
    struct tm tm;
    waypoint *wpt;

    if (track->rte_waypt_ct == 0) {
      tm = *localtime(&default_time);
    } else {
      wpt = (waypoint *) QUEUE_FIRST((queue *)&track->waypoint_list);
      tm = *localtime(&wpt->creation_time);
    }
    strftime(buff, sizeof(buff), opt_title, &tm);
  } else {
    strncpy(buff, opt_title, sizeof(buff));
  }

  if (track->rte_name != NULL) {
    xfree(track->rte_name);
  }
  track->rte_name = xstrdup(buff);
}

/*******************************************************************************
* option "title"
*******************************************************************************/

static void
trackfilter_title(void)
{
  int i;

  if (opt_title == NULL) {
    return;
  }

  if (strlen(opt_title) == 0) {
    fatal(MYNAME "-title: Missing your title!\n");
  }
  for (i = 0; i < track_ct; i++) {
    route_head *track = track_list[i].track;
    trackfilter_pack_init_rte_name(track, 0);
  }
}

/*******************************************************************************
* option "pack" (default)
*******************************************************************************/

static void
trackfilter_pack(void)
{
  int i, j;
  trkflt_t prev;
  route_head *master;

  for (i = 1, j = 0; i < track_ct; i++, j++) {
    prev = track_list[j];
    if (prev.last_time >= track_list[i].first_time) {
      fatal(MYNAME "-pack: Tracks overlap in time!\n");
    }
  }

  /* we fill up the first track by all other track points */

  master = track_list[0].track;

  for (i = 1; i < track_ct; i++) {
    queue *elem, *tmp;
    route_head *curr = track_list[i].track;

    QUEUE_FOR_EACH((queue *)&curr->waypoint_list, elem, tmp) {
      waypoint *wpt = (waypoint *)elem;
      track_del_wpt(curr, wpt);
      track_add_wpt(master, wpt);
    }
    track_del_head(curr);
    track_list[i].track = NULL;
  }
  track_ct = 1;
}

/*******************************************************************************
* option "merge"
*******************************************************************************/

static void
trackfilter_merge(void)
{
  int i, j, dropped;

  queue *elem, *tmp;
  waypoint **buff;
  waypoint *prev, *wpt;
  route_head *master = track_list[0].track;

  if (track_pts-timeless_pts < 1) {
    return;
  }

  buff = (waypoint **)xcalloc(track_pts-timeless_pts, sizeof(*buff));

  j = 0;
  for (i = 0; i < track_ct; i++) {	/* put all points into temp buffer */
    route_head *track = track_list[i].track;
    QUEUE_FOR_EACH((queue *)&track->waypoint_list, elem, tmp) {
      wpt = (waypoint *)elem;
      if(wpt->creation_time != 0) {
        buff[j++] = waypt_dupe(wpt);
      }
      track_del_wpt(track, wpt);
      waypt_free(wpt);
    }
    if (track != master) {	/* i > 0 */
      track_del_head(track);
    }
  }
  track_ct = 1;

  qsort(buff, track_pts-timeless_pts, sizeof(*buff), trackfilter_merge_qsort_cb);

  dropped = timeless_pts;
  prev = NULL;

  for (i = 0; i < track_pts-timeless_pts; i++) {
    wpt = buff[i];
    if ((prev == NULL) || (prev->GetCreationTime() != wpt->GetCreationTime())) {
      route_add_wpt(master, wpt);
      prev = wpt;
    } else {
      waypt_free(wpt);
      dropped++;
    }
  }
  xfree(buff);

  if (global_opts.verbose_status > 0) {
    printf(MYNAME "-merge: %d track point(s) merged, %d dropped.\n", track_pts - dropped, dropped);
  }
}

/*******************************************************************************
* option "split"
*******************************************************************************/

static void
trackfilter_split(void)
{
  route_head *curr;
  route_head *master = track_list[0].track;
  int count = master->rte_waypt_ct;

  waypoint **buff;
  waypoint *wpt;
  queue *elem, *tmp;
  int i, j;
  double interval = -1;
  double distance = -1;

  if (count <= 1) {
    return;
  }

  /* check additional options */

  opt_interval = (opt_split && (strlen(opt_split) > 0) && (0 != strcmp(opt_split, TRACKFILTER_SPLIT_OPTION)));
  opt_distance = (opt_sdistance && (strlen(opt_sdistance) > 0) && (0 != strcmp(opt_sdistance, TRACKFILTER_SDIST_OPTION)));

  if (opt_interval != 0) {
    double base;
    char   unit;

    switch (strlen(opt_split)) {
    case 0:
      fatal(MYNAME ": No time interval specified.\n");
      break; /* ? */

    case 1:
      unit = *opt_split;
      interval = 1;
      break;

    default:
      i = sscanf(opt_split,"%lf%c", &interval, &unit);
      if (i == 0) {
        /* test reverse order */
        i = sscanf(opt_split,"%c%lf", &unit, &interval);
      }
      if ((i != 2) || (interval <= 0)) {
        fatal(MYNAME ": invalid time interval specified, must be one a positive number.\n");
      }
      break;
    }

    switch (tolower(unit)) {
    case 's':
      base = 1;
      break;
    case 'm':
      base = 60;
      break;
    case 'h':
      base = 60 * 60;
      break;
    case 'd':
      base = 24 * 60 * 60;
      break;
    default:
      fatal(MYNAME ": invalid time interval specified, must be one of [dhms].\n");
      break;
    }
#ifdef TRACKF_DBG
    printf(MYNAME ": unit \"%c\", interval %g -> %g\n", unit, interval, base * interval);
#endif
    interval *= base;
  }

  if (opt_distance != 0) {
    double base;
    char   unit;

    switch (strlen(opt_sdistance)) {
    case 0:
      fatal(MYNAME ": No distance specified.\n");
      break; /* ? */

    case 1:
      unit = *opt_sdistance;
      distance = 1;
      break;

    default:
      i = sscanf(opt_sdistance,"%lf%c", &distance, &unit);
      if (i == 0) {
        /* test reverse order */
        i = sscanf(opt_sdistance,"%c%lf", &unit, &distance);
      }
      if ((i != 2) || (distance <= 0)) {
        fatal(MYNAME ": invalid distance specified, must be one a positive number.\n");
      }
      break;
    }

    switch (tolower(unit)) {
    case 'k': /* kilometers */
      base = 0.6214;
      break;
    case 'm': /* miles */
      base = 1;
      break;
    default:
      fatal(MYNAME ": invalid distance specified, must be one of [km].\n");
      break;
    }
#ifdef TRACKF_DBG
    printf(MYNAME ": unit \"%c\", distance %g -> %g\n", unit, distance, base * distance);
#endif
    distance *= base;
  }

  trackfilter_split_init_rte_name(master, track_list[0].first_time);

  buff = (waypoint **) xcalloc(count, sizeof(*buff));

  i = 0;
  QUEUE_FOR_EACH((queue *)&master->waypoint_list, elem, tmp) {
    wpt = (waypoint *)elem;
    buff[i++] = wpt;
  }

  curr = NULL;	/* will be set by first new track */

  for (i=0, j=1; j<count; i++, j++) {
    int new_track_flag;

    if ((opt_interval == 0) && (opt_distance == 0)) {
      struct tm t1, t2;

      t1 = *localtime(&buff[i]->creation_time);
      t2 = *localtime(&buff[j]->creation_time);

      new_track_flag = ((t1.tm_year != t2.tm_year) || (t1.tm_mon != t2.tm_mon) ||
                        (t1.tm_mday != t2.tm_mday));
#ifdef TRACKF_DBG
      if (new_track_flag != 0) {
        printf(MYNAME ": new day %02d.%02d.%04d\n", t2.tm_mday, t2.tm_mon+1, t2.tm_year+1900);
      }
#endif
    } else {
      new_track_flag = 1;

      if (distance > 0) {
        double rt1 = RAD(buff[i]->latitude);
        double rn1 = RAD(buff[i]->longitude);
        double rt2 = RAD(buff[j]->latitude);
        double rn2 = RAD(buff[j]->longitude);
        double curdist = gcdist(rt1, rn1, rt2, rn2);
        curdist = radtomiles(curdist);
        if (curdist <= distance) {
          new_track_flag = 0;
        }
#ifdef TRACKF_DBG
        else {
          printf(MYNAME ": sdistance, %g > %g\n", curdist, distance);
        }
#endif
      }

      if (interval > 0) {
        double tr_interval = difftime(buff[j]->GetCreationTime(),buff[i]->GetCreationTime());
        if (tr_interval <= interval) {
          new_track_flag = 0;
        }
#ifdef TRACKF_DBG
        else {
          printf(MYNAME ": split, %g > %g\n", tr_interval, interval);
        }
#endif
      }

    }
    if (new_track_flag != 0) {
#ifdef TRACKF_DBG
      printf(MYNAME ": splitting new track\n");
#endif
      curr = (route_head *) route_head_alloc();
      trackfilter_split_init_rte_name(curr, buff[j]->GetCreationTime());
      track_add_head(curr);
    }
    if (curr != NULL) {
      wpt = buff[j];
      track_del_wpt(master, wpt);
      track_add_wpt(curr, wpt);
      buff[j] = wpt;
    }
  }
  xfree(buff);
}

/*******************************************************************************
* option "move"
*******************************************************************************/

static void
trackfilter_move(void)
{
  int i;
  queue *elem, *tmp;
  waypoint *wpt;
  time_t delta;

  delta = trackfilter_parse_time_opt(opt_move);
  if (delta == 0) {
    return;
  }

  for (i = 0; i < track_ct; i++) {
    route_head *track = track_list[i].track;
    QUEUE_FOR_EACH((queue *)&track->waypoint_list, elem, tmp) {
      wpt = (waypoint *)elem;
      wpt->creation_time += delta;
    }

    track_list[i].first_time = track_list[i].first_time.addSecs(delta);
    track_list[i].last_time = track_list[i].last_time.addSecs(delta);
  }
}

/*******************************************************************************
* options "fix", "course", "speed"
*******************************************************************************/

static void
trackfilter_synth(void)
{
  int i;
  queue *elem, *tmp;
  waypoint *wpt;

  double oldlat = -999;
  double oldlon = -999;
  time_t oldtime = 0;
  int first = 1;
  fix_type fix;
  int nsats = 0;

  fix = trackfilter_parse_fix(&nsats);

  for (i = 0; i < track_ct; i++) {
    route_head *track = track_list[i].track;
    first = 1;
    QUEUE_FOR_EACH((queue *)&track->waypoint_list, elem, tmp) {
      wpt = (waypoint *)elem;
      if (opt_fix) {
        wpt->fix = fix;
        if (wpt->sat == 0) {
          wpt->sat = nsats;
        }
      }
      if (first) {
        if (opt_course) {
          WAYPT_SET(wpt, course, 0);
        }
        if (opt_speed) {
          WAYPT_SET(wpt, speed, 0);
        }
        first = 0;
      } else {
        if (opt_course) {
          WAYPT_SET(wpt, course, heading_true_degrees(RAD(oldlat),
                    RAD(oldlon),RAD(wpt->latitude),
                    RAD(wpt->longitude)));
        }
        if (opt_speed) {
          if (oldtime != wpt->GetCreationTime()) {
            WAYPT_SET(wpt, speed, radtometers(gcdist(
                                                RAD(oldlat), RAD(oldlon),
                                                RAD(wpt->latitude),
                                                RAD(wpt->longitude))) /
                      labs(wpt->GetCreationTime()-oldtime));
          } else {
            WAYPT_UNSET(wpt, speed);
          }
        }
      }
      oldlat = wpt->latitude;
      oldlon = wpt->longitude;
      oldtime = wpt->GetCreationTime();
    }
  }
}


/*******************************************************************************
* option: "start" / "stop"
*******************************************************************************/

static time_t
trackfilter_range_check(const char *timestr)
{
  int i;
  char fmt[20];
  char c;
  char *cin;
  struct tm time;


  i = 0;
  strncpy(fmt, "00000101000000", sizeof(fmt));
  cin = (char *)timestr;

  while ((c = *cin++)) {
    if (fmt[i] == '\0') {
      fatal(MYNAME "-range: parameter too long \"%s\"!\n", timestr);
    }
    if (isdigit(c) == 0) {
      fatal(MYNAME "-range: invalid character \"%c\"!\n", c);
    }
    fmt[i++] = c;
  }
  cin = strptime(fmt, "%Y%m%d%H%M%S", &time);
  if ((cin != NULL) && (*cin != '\0')) {
    fatal(MYNAME "-range-check: Invalid time stamp (stopped at %s of %s)!\n", cin, fmt);
  }

  return mkgmtime(&time);
}

static int
trackfilter_range(void)		/* returns number of track points left after filtering */
{
  time_t start, stop;
  queue *elem, *tmp;
  int i, dropped, inside = 0;

  if (opt_start != 0) {
    start = trackfilter_range_check(opt_start);
  } else {
    start = 0;
  }

  if (opt_stop != 0) {
    stop = trackfilter_range_check(opt_stop);
  } else {
    stop = 0x7FFFFFFF;
  }

  dropped = inside = 0;

  for (i = 0; i < track_ct; i++) {
    route_head *track = track_list[i].track;

    QUEUE_FOR_EACH((queue *)&track->waypoint_list, elem, tmp) {
      waypoint *wpt = (waypoint *)elem;
      if (wpt->creation_time > 0) {
        inside = ((wpt->GetCreationTime() >= start) && (wpt->GetCreationTime() <= stop));
      }
      // If the time is mangled so horribly that it's
      // negative, toss it.
      if (wpt->creation_time < 0) {
        inside = 0;
      }

      if (! inside) {
        track_del_wpt(track, wpt);
        waypt_free(wpt);
        dropped++;
      }
    }

    if (track->rte_waypt_ct == 0) {
      track_del_head(track);
      track_list[i].track = NULL;
    }
  }

  if ((track_pts > 0) && (dropped == track_pts)) {
    warning(MYNAME "-range: All %d track points have been dropped!\n", track_pts);
  }

  return track_pts - dropped;
}

/*******************************************************************************
* option "seg2trk"
*******************************************************************************/

static void
trackfilter_seg2trk(void)
{
  int i;

  for (i = 0; i < track_ct; i++) {
    queue *elem, *tmp;
    route_head *src = track_list[i].track;
    route_head *dest = NULL;
    route_head *insert_point = src;
    int trk_seg_num = 1, first = 1;

    QUEUE_FOR_EACH((queue *)&src->waypoint_list, elem, tmp) {
      waypoint *wpt = (waypoint *)elem;
      if (wpt->wpt_flags.new_trkseg && !first) {

        dest = route_head_alloc();
        dest->rte_num = src->rte_num;
        /* name in the form TRACKNAME #n */
        if (src->rte_name) {
          xasprintf(&dest->rte_name, "%s #%d", src->rte_name, ++trk_seg_num);
        }

        /* Insert after original track or after last newly
         * created track */
        track_insert_head(dest, insert_point);
        insert_point = dest;
      }

      /* If we found a track separator, transfer from original to
       * new track. We have to reset new_trkseg temporarily to
       * prevent track_del_wpt() from copying it to the next track
       * point.
       */
      if (dest) {
        int orig_new_trkseg = wpt->wpt_flags.new_trkseg;
        wpt->wpt_flags.new_trkseg = 0;
        track_del_wpt(src, wpt);
        wpt->wpt_flags.new_trkseg = orig_new_trkseg;
        track_add_wpt(dest, wpt);
      }
      first = 0;
    }
  }
}

/*******************************************************************************
* option "trk2seg"
*******************************************************************************/

static void
trackfilter_trk2seg(void)
{
  int i, first;
  route_head *master;

  master = track_list[0].track;

  for (i = 1; i < track_ct; i++) {
    queue *elem, *tmp;
    route_head *curr = track_list[i].track;

    first = 1;
    QUEUE_FOR_EACH((queue *)&curr->waypoint_list, elem, tmp) {
      waypoint *wpt = (waypoint *)elem;


      int orig_new_trkseg = wpt->wpt_flags.new_trkseg;
      wpt->wpt_flags.new_trkseg = 0;
      track_del_wpt(curr, wpt);
      wpt->wpt_flags.new_trkseg = orig_new_trkseg;
      track_add_wpt(master, wpt);
      if (first) {
        wpt->wpt_flags.new_trkseg = 1;
        first = 0;
      }
    }
    track_del_head(curr);
    track_list[i].track = NULL;
  }
  track_ct = 1;
}

/*******************************************************************************
* option: "faketime"
*******************************************************************************/

typedef struct faketime_s {
  time_t start;
  int    step;
  int   force;
} faketime_t;

static faketime_t
trackfilter_faketime_check(const char *timestr)
{
  int i, j;
  char fmtstart[20];
  char fmtstep[20];
  char c;
  const char *cin;
  struct tm time;
  int timeparse = 1;
  faketime_t result;
  result.force = 0;

  i = j = 0;
  strncpy(fmtstart, "00000101000000", sizeof(fmtstart));
  strncpy(fmtstep,  "00000000000000", sizeof(fmtstep));
  cin = timestr;

  while ((c = *cin++)) {
    if (c=='f') {
      result.force = 1;
      continue;
    }

    if (c!='+' && isdigit(c) == 0) {
      fatal(MYNAME "-faketime: invalid character \"%c\"!\n", c);
    }

    if (timeparse) {
      if (c == '+') {
        fmtstart[i++] = '\0';
        timeparse = 0;
      } else {
        if (fmtstart[i] == '\0') {
          fatal(MYNAME "-faketime: parameter too long \"%s\"!\n", timestr);
        }
        fmtstart[i++] = c;
      }
    } else {
      if (fmtstep[j] == '\0') {
        fatal(MYNAME "-faketime: parameter too long \"%s\"!\n", timestr);
      }
      fmtstep[j++] = c;
    }
  }
  fmtstep[j++] = '\0';

  cin = strptime(fmtstart, "%Y%m%d%H%M%S", &time);
  result.step = atoi(fmtstep);
  if ((cin != NULL) && (*cin != '\0')) {
    fatal(MYNAME "-faketime-check: Invalid time stamp (stopped at %s of %s)!\n", cin, fmtstart);
  }

  result.start = mkgmtime(&time);
  return result;
}

static int
trackfilter_faketime(void)             /* returns number of track points left after filtering */
{
  faketime_t faketime;

  queue *elem, *tmp;
  int i, dropped, inside = 0;

  if (opt_faketime != 0) {
    faketime = trackfilter_faketime_check(opt_faketime);
  }

  dropped = inside = 0;

  for (i = 0; i < track_ct; i++) {
    route_head *track = track_list[i].track;

    QUEUE_FOR_EACH((queue *)&track->waypoint_list, elem, tmp) {
      waypoint *wpt = (waypoint *)elem;

      if (opt_faketime != 0 && (wpt->creation_time == 0 || faketime.force)) {
        wpt->creation_time = faketime.start;
        faketime.start += faketime.step;
      }
    }
  }

  return track_pts - dropped;
}

static int
trackfilter_points_are_same(const waypoint *wpta, const waypoint *wptb)
{
  // We use a simpler (non great circle) test for lat/lon here as this
  // is used for keeping the 'bookends' of non-moving points.
  //
  // Latitude spacing is about 27 feet per .00001 degree.
  // Longitude spacing varies, but the reality is that anything closer
  // than 27 feet does little but clutter the output.
  // As this is about the limit of consumer grade GPS, it seems a
  // reasonable tradeoff.

  return
    fabs(wpta->latitude - wptb->latitude) < .00001 &&
    fabs(wpta->longitude - wptb->longitude) < .00001 &&
    abs(wpta->altitude - wptb->altitude) < 20 &&
    (WAYPT_HAS(wpta,course) == WAYPT_HAS(wptb,course)) &&
    (wpta->course == wptb->course) &&
    (wpta->speed == wptb->speed) &&
    (wpta->heartrate == wptb->heartrate) &&
    (wpta->cadence == wptb->cadence) &&
    (wpta->temperature == wptb->temperature);
}

static void
trackfilter_segment_head(const route_head *rte)
{
  queue *elem, *tmp;
  double avg_dist = 0;
  int index = 0;
  waypoint *prev_wpt = NULL;
  // Consider tossing trackpoints closer than this in radians.
  // (Empirically determined; It's a few dozen feet.)
  const double ktoo_close = 0.000005;

  QUEUE_FOR_EACH(&rte->waypoint_list, elem, tmp) {
    waypoint *wpt = (waypoint *)elem;
    if (index > 0) {
      double cur_dist = gcdist(RAD(prev_wpt->latitude),
                               RAD(prev_wpt->longitude),
                               RAD(wpt->latitude),
                               RAD(wpt->longitude));
      // Denoise points that are on top of each other.
      if (avg_dist == 0) {
        avg_dist = cur_dist;
      }

      if (cur_dist < ktoo_close) {
        if (wpt != (waypoint *) QUEUE_LAST(&rte->waypoint_list)) {
          waypoint *next_wpt = (waypoint *) QUEUE_NEXT(&wpt->Q);
          if (trackfilter_points_are_same(prev_wpt, wpt) &&
              trackfilter_points_are_same(wpt, next_wpt)) {
            track_del_wpt((route_head *)rte, wpt);
            continue;
          }
        }
      }
      if (cur_dist > .001 && cur_dist > 1.2* avg_dist) {
        avg_dist = cur_dist = 0;
        wpt->wpt_flags.new_trkseg = 1;
      }
      // Update weighted moving average;
      avg_dist = (cur_dist + 4.0 * avg_dist) / 5.0;
    }
    prev_wpt = wpt;
    index++;
  }
}

/*******************************************************************************
* global cb's
*******************************************************************************/

static void
trackfilter_init(const char *args)
{

  int count = track_count();

  /*
   * check time presence only if required. Options that NOT require time:
   *
   * - opt_title (!!! only if no format specifier is present !!!)
   * - opt_course
   * - opt_name
   */
  need_time = (
                opt_merge || opt_pack || opt_split || opt_sdistance ||
                opt_move || opt_fix || opt_speed ||
                (trackfilter_opt_count() == 0)	/* do pack by default */
              );
  /* in case of a formated title we also need valid timestamps */
  if ((opt_title != NULL) && (strchr(opt_title, '%') != NULL)) {
    need_time = 1;
  }

  track_ct = 0;
  track_pts = 0;

  // Perform segmenting first.
  if (opt_segment) {
    track_disp_all(trackfilter_segment_head, NULL, NULL);
  }

  if (count > 0) {
    track_list = new trkflt_t[count];

    /* check all tracks for time and order (except merging) */

    track_disp_all(trackfilter_fill_track_list_cb, NULL, NULL);
    if (need_time) {
      qsort(track_list, track_ct, sizeof(*track_list), trackfilter_init_qsort_cb);
    }
  }
}

static void
trackfilter_deinit(void)
{
  delete[] track_list;
  track_ct = 0;
  track_pts = 0;
}

/*******************************************************************************
* trackfilter_process: called from gpsbabel central engine
*******************************************************************************/

static void
trackfilter_process(void)
{
  int opts, something_done;

  if (track_ct == 0) {
    return;  /* no track(s), no fun */
  }

  opts = trackfilter_opt_count();
  if (opts == 0) {
    opts = -1;  /* flag for do "pack" by default */
  }

  if (opt_name != NULL) {
    if (--opts == 0) {
      return;
    }
  }

  if (opt_move != NULL) {		/* Correct timestamps before any other op */
    trackfilter_move();
    if (--opts == 0) {
      return;
    }
  }

  if (opt_speed || opt_course || opt_fix) {
    trackfilter_synth();
    if (opt_speed) {
      opts--;
    }
    if (opt_course) {
      opts--;
    }
    if (opt_fix) {
      opts--;
    }
    if (!opts) {
      return;
    }
  }

  if ((opt_faketime != NULL)) {
    opts--;

    trackfilter_faketime();

    if (opts == 0) {
      return;
    }

    trackfilter_deinit();       /* reinitialize */
    trackfilter_init(NULL);

    if (track_ct == 0) {
      return;  /* no more track(s), no more fun */
    }
  }

  if ((opt_stop != NULL) || (opt_start != NULL)) {
    if (opt_start != NULL) {
      opts--;
    }
    if (opt_stop != NULL) {
      opts--;
    }

    trackfilter_range();

    if (opts == 0) {
      return;
    }

    trackfilter_deinit();	/* reinitialize */
    trackfilter_init(NULL);

    if (track_ct == 0) {
      return;  /* no more track(s), no more fun */
    }

  }

  if (opt_seg2trk != NULL) {
    trackfilter_seg2trk();
    if (--opts == 0) {
      return;
    }

    trackfilter_deinit();	/* reinitialize */
    trackfilter_init(NULL);
  }

  if (opt_trk2seg != NULL) {
    trackfilter_trk2seg();
    if (--opts == 0) {
      return;
    }
  }

  if (opt_title != NULL) {
    if (--opts == 0) {
      trackfilter_title();
      return;
    }
  }

  something_done = 0;

  if ((opt_pack != NULL) || (opts == -1)) {	/* call our default option */
    trackfilter_pack();
    something_done = 1;
  } else if (opt_merge != NULL) {
    trackfilter_merge();
    something_done = 1;
  }

  if ((something_done == 1) && (--opts <= 0)) {
    if (opt_title != NULL) {
      trackfilter_title();
    }
    return;
  }

  if ((opt_split != NULL) || (opt_sdistance != NULL)) {
    if (track_ct > 1) {
      fatal(MYNAME "-split: Cannot split more than one track, please pack (or merge) before!\n");
    }

    trackfilter_split();
  }
}

/******************************************************************************************/

filter_vecs_t trackfilter_vecs = {
  trackfilter_init,
  trackfilter_process,
  trackfilter_deinit,
  NULL,
  trackfilter_args
};

/******************************************************************************************/
#endif // FILTERS_ENABLED
