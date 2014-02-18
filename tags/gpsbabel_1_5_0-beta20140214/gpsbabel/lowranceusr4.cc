/*
	Access to Lowrance USR version 4 files.
	Contributed to gpsbabel by Kris Beevers (beevek at gmail.com)

	Copyright (C) 2011 Robert Lipe, robertlipe@usa.net

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

	HISTORY:

	01/06/2012 - Kris Beevers (beevek at gmail.com)
	- First pass read-write support
*/


#include "defs.h"
#include <string.h>
#include <math.h> /* for lat/lon conversion */
#include <time.h> /* for gmtime */

/* from waypt.c, we need to iterate over waypoints when extracting
   routes */
#if NEWQ
extern QList<Waypoint*> waypt_list;
#else
extern queue waypt_head;
#endif

static gbfile* file_in;
static gbfile* file_out;
static short_handle mkshort_handle;

static route_head* trk_head;
static route_head* rte_head;
static int reading_version;

static int waypt_uid;
static int route_uid;
static int track_uid;

static Waypoint** waypt_table;
static int waypt_table_sz, waypt_table_ct;

static char* opt_title;
static char* opt_serialnum;
static int opt_serialnum_i;
static char* opt_content_descr;

#define MYNAME "Lowrance USR4"

#define MAXUSRSTRINGSIZE	256
#define SEMIMINOR		   6356752.3142
#define DEGREESTORADIANS	0.017453292


typedef struct {
  format_specific_data fs;
  int uid_unit;
  int uid_seq_low;
  int uid_seq_high;
} lowranceusr4_fsdata;


static int
lowranceusr4_readstr(char* buf, const int maxlen, gbfile* file, int bytes_per_char)
{
  int org, len;

  org = len = gbfgetint32(file);
  if (len < 0) {
    buf[0] = '\0'; /* seems len=-1 means no string */
    return 0;
  } else if (len) {
    if (len/bytes_per_char > maxlen) {
      len = maxlen*bytes_per_char;
    }
    if (bytes_per_char == 1) {
      (void) gbfread(buf, 1, len, file);
    } else {
      /* simple adjustment to read strings where characters are 16
         bits (or more).  for now let's just project the characters
         down onto utf-8 space by ignoring all but the most
         significant byte. */
      int i, j;
      char discard;
      for (i = 0; i < len/bytes_per_char; ++i) {
        gbfread(&buf[i], 1, 1, file);
        for (j = 1; j < bytes_per_char; ++j) {
          gbfread(&discard, 1, 1, file);
        }
      }
      buf[len/bytes_per_char] = '\0';
    }
    if (org > maxlen) {
      (void) gbfseek(file, bytes_per_char * (org - maxlen), SEEK_CUR);
    }
  }

  return len;
}

static void
lowranceusr4_writestr(const QString& buf, gbfile* file, unsigned int bytes_per_char)
{
  unsigned int len = 0;

  len = buf.length();

  if (0xffffffff / bytes_per_char < len) {
    /* be pedantic and check for the unlikely event that we are asked
       to write more than 2^32 bytes */
    len = 0xffffffff / bytes_per_char;
  }

  gbfputint32(len*bytes_per_char, file_out);

  if (bytes_per_char == 1) {
    (void) gbfwrite(CSTR(buf), 1, len, file);
  } else {
    for (unsigned int i = 0; i < len; ++i) {
      gbfputc(buf[i].cell(), file_out);
      for (unsigned int j = 1; j < bytes_per_char; ++j) {
        gbfputc('\0', file_out);
      }
    }
  }
}


static
arglist_t lowranceusr4_args[] = {
  {
    "title", &opt_title, "(output) Output title string",
    "", ARGTYPE_STRING, ARG_NOMINMAX
  },
  {
    "serialnum", &opt_serialnum, "(output) Device serial number",
    "0", ARGTYPE_INT, ARG_NOMINMAX
  },
  {
    "description", &opt_content_descr, "(output) Content description",
    "", ARGTYPE_STRING, ARG_NOMINMAX
  },
  ARG_TERMINATOR
};

static void
rd_init(const char* fname)
{
  file_in = gbfopen_le(fname, "rb", MYNAME);
}

static void
rd_deinit(void)
{
  gbfclose(file_in);
}

static void
wr_init(const char* fname)
{
  file_out = gbfopen_le(fname, "wb", MYNAME);
  mkshort_handle = mkshort_new_handle();
}

static void
wr_deinit(void)
{
  gbfclose(file_out);
  mkshort_del_handle(&mkshort_handle);
}


/**
 * Latitude and longitude for USR coords are in the lowrance mercator meter
 * format in WGS84.  The below code converts them to degrees.
 */
static double
lon_mm_to_deg(double x)
{
  return x / (DEGREESTORADIANS * SEMIMINOR);
}

static double
lat_mm_to_deg(double x)
{
  return (2.0 * atan(exp(x / SEMIMINOR)) - M_PI / 2.0) / DEGREESTORADIANS;
}

/* will be useful for write support */
static long
lon_deg_to_mm(double x)
{
  return (long)(x * SEMIMINOR * DEGREESTORADIANS);
}

static long
lat_deg_to_mm(double x)
{
  return (long)(SEMIMINOR * log(tan((x * DEGREESTORADIANS + M_PI / 2.0) / 2.0)));
}

static time_t
lowranceusr4_get_timestamp(int jd_number, time_t t)
{
  int a, b, c, d, e, m;
  struct tm* ptm, ntm;
  time_t out;

  /* get UTC time from time_t */
  ptm = gmtime(&t);
  memset(&ntm, 0, sizeof(ntm));
  ntm.tm_hour = ptm->tm_hour;
  ntm.tm_min = ptm->tm_min;
  ntm.tm_sec = ptm->tm_sec;

  /* convert the JD number to get day/month/year */
  a = jd_number + 32044;
  b = (4*a + 3) / 146097;
  c = a - (146097*b) / 4;
  d = (4*c + 3) / 1461;
  e = c - (1461*d) / 4;
  m = (5*e + 2) / 153;
  ntm.tm_mday = e + 1 - (153*m + 2) / 5;
  ntm.tm_mon = m + 3 - 12 * (m / 10) - 1;
  ntm.tm_year = 100 * b + d - 4800 + m / 10 - 1900;

  /* put it all back together into a unix timestamp in UTC */
  out = mkgmtime(&ntm);

  return out;
}

static int
lowranceusr4_jd_from_timestamp(time_t t)
{
  return (int)round((float)t / 86400.0 + 2440587.0);
}


static void
lowranceusr4_copy_fsdata(lowranceusr4_fsdata** dest, lowranceusr4_fsdata* src)
{
  *dest = (lowranceusr4_fsdata*)xmalloc(sizeof(*src));
  ** dest = *src;
  (*dest)->fs.next = NULL;
}

static void
lowranceusr4_free_fsdata(void* fsdata)
{
  xfree(fsdata);
}

static
lowranceusr4_fsdata*
lowranceusr4_alloc_fsdata(void)
{
  lowranceusr4_fsdata* fsdata = (lowranceusr4_fsdata*) xcalloc(sizeof(*fsdata), 1);
  fsdata->fs.type = FS_LOWRANCEUSR4;
  fsdata->fs.copy = (fs_copy) lowranceusr4_copy_fsdata;
  fsdata->fs.destroy = lowranceusr4_free_fsdata;
  fsdata->fs.convert = NULL;

  fsdata->uid_unit = 0;
  fsdata->uid_seq_low = 0;
  fsdata->uid_seq_high = 0;

  return fsdata;
}


/* below couple of functions mostly borrowed from raymarine.c */

/* make waypoint shortnames unique */
static char
same_points(const Waypoint* A, const Waypoint* B)
{
  return ( /* !!! We are case-sensitive !!! */
#if NEW_STRINGS
           (A->shortname == B->shortname) &&
#else
           (strcmp(A->shortname, B->shortname) == 0) &&
#endif
           (A->latitude == B->latitude) &&
           (A->longitude == B->longitude));
}

static void
register_waypt(const Waypoint* ref)
{
  int i;
  Waypoint* wpt = (Waypoint*) ref;

  for (i = 0; i < waypt_table_ct; i++) {
    Waypoint* cmp = waypt_table[i];

    if (same_points(wpt, cmp)) {
      return;
    }
  }

  if (waypt_table_ct >= waypt_table_sz) {
    waypt_table_sz += 32;
    if (waypt_table) {
      waypt_table = (Waypoint**) xrealloc(waypt_table, waypt_table_sz * sizeof(wpt));
    } else {
      waypt_table = (Waypoint**) xmalloc(waypt_table_sz * sizeof(wpt));
    }
  }

  if (global_opts.debug_level >= 2) {
    printf(MYNAME " adding waypt %s (%s) to table at index %d\n",
           CSTRc(wpt->shortname), CSTRc(wpt->description), waypt_table_ct);
  }

  waypt_table[waypt_table_ct] = (Waypoint*)wpt;
  waypt_table_ct++;
}

/* end borrowed from raymarine.c */

static int
lowranceusr4_find_waypt_index(const Waypoint* wpt)
{
  int i;
  for (i = 0; i < waypt_table_ct; ++i) {
    if (same_points(wpt, (const Waypoint*)waypt_table[i])) {
      return i;
    }
  }
  return waypt_table_ct+1; /* should never happen */
}



static void
lowranceusr4_parse_waypoints(void)
{
  short int icon_num;
  unsigned int i, num_waypts, create_date, create_time;
  int text_len;
  char buff[MAXUSRSTRINGSIZE + 1];

  num_waypts = gbfgetint32(file_in);

  if (global_opts.debug_level >= 1) {
    printf(MYNAME " parse_waypoints: Num waypoints %d\n", num_waypts);
  }

  for (i = 0; i < num_waypts; ++i) {
    Waypoint* wpt_tmp;

    wpt_tmp = new Waypoint;
    lowranceusr4_fsdata* fsdata = lowranceusr4_alloc_fsdata();
    fs_chain_add(&(wpt_tmp->fs), (format_specific_data*) fsdata);

    /* read/parse waypoint, with fields as follows (taken mostly
       from http://lowranceusrv4togpxconverter.blogspot.com/):

         UID unit number             - uint32
         UID sequence number         - int64
         Waypt stream version number - uint16
         Waypt name length (bytes)   - uint32
         Waypoint name               - utf-16 string w/above length (w->shortname)
         Longitude (mercator meters) - int32 (w->longitude)
         Latitude (mercator meters)  - int32 (w->latitude)
         Flags                       - uint32
         Icon ID                     - uint16 (to w->icon_descr via conversion)
         Color ID                    - uint16
         Description length (bytes)  - uint32
         Description                 - utf-16 string w/above length (w->description)
         Alarm radius                - float (w->proximity)
         Creation date               - uint32 (w->creation_time)
         Creation time               - uint32 (w->creation_time)
         Unused                      - uint8
         Depth (feet)                - float (w->depth)
         Loran GRI                   - int32
         Loran TdA                   - int32
         Loran TdB                   - int32
     */

    /* UID unit number */
    fsdata->uid_unit = gbfgetint32(file_in);

    /* 64-bit UID sequence number */
    fsdata->uid_seq_low = gbfgetint32(file_in);
    fsdata->uid_seq_high = gbfgetint32(file_in);

    /* Waypt stream version number, discard for now */
    gbfgetint16(file_in);

    /* Waypoint name; input is 2 bytes per char, we convert to 1 */
    text_len = lowranceusr4_readstr(&buff[0], MAXUSRSTRINGSIZE, file_in, 2);
    if (text_len) {
      buff[text_len] = '\0';
      wpt_tmp->shortname = buff;
    }

    /* Long/Lat */
    wpt_tmp->longitude = lon_mm_to_deg(gbfgetint32(file_in));
    wpt_tmp->latitude = lat_mm_to_deg(gbfgetint32(file_in));

    /* Flags, discard for now */
    gbfgetint32(file_in);

    /* Icon ID; TODO: need to run this through something like
       lowranceusr_find_desc_from_icon_number to convert to a gpsbabel
       icon description; however it doesn't seem that the icon ids
       used in usr4 match those from usr{2,3} so we need a new
       mapping. */
    icon_num = gbfgetint16(file_in);
    /* wpt_tmp->icon_descr = lowranceusr_find_desc_from_icon_number(icon_num); */

    /* Color ID, discard for now */
    gbfgetint16(file_in);

    /* Waypoint descr; input is 2 bytes per char, we convert to 1 */
    text_len = lowranceusr4_readstr(&buff[0], MAXUSRSTRINGSIZE, file_in, 2);
    if (text_len) {
      buff[text_len] = '\0';
      wpt_tmp->description = buff;
    }

    /* Alarm radius; XXX: I'm not sure what the units are here,
       assuming meters but may be feet? */
    WAYPT_SET(wpt_tmp, proximity, gbfgetflt(file_in));

    /* Creation date/time; the date is a Julian day number, and the
       time is a unix timestamp. */
    create_date = gbfgetint32(file_in);
    create_time = gbfgetint32(file_in);

    // Julian date 2440487 is 1/1/1970.  If that's the date we're working
    // with, as a practical matter, we have no date, so don't even compute
    // or set it.
    if (create_date > 2440587) {
      wpt_tmp->SetCreationTime(lowranceusr4_get_timestamp(create_date,
                               create_time));
    }

    /* Unused byte */
    gbfgetc(file_in);

    /* Depth in feet */
    WAYPT_SET(wpt_tmp, depth, FEET_TO_METERS(gbfgetflt(file_in)));

    /* Loran data, discard for now */
    gbfgetint32(file_in);
    gbfgetint32(file_in);
    gbfgetint32(file_in);

    if (global_opts.debug_level >= 1) {
      printf(MYNAME " parse_waypoints: name = %s, uid_unit = %u, "
             "uid_seq_low = %d, uid_seq_high = %d, lat = %f, lon = %f, depth = %f\n",
             CSTRc(wpt_tmp->shortname), fsdata->uid_unit,
             fsdata->uid_seq_low, fsdata->uid_seq_high,
             wpt_tmp->latitude, wpt_tmp->longitude, wpt_tmp->depth);
    }

    waypt_add(wpt_tmp);
  }
}

static Waypoint*
lowranceusr4_find_waypt(int uid_unit, int uid_seq_low, int uid_seq_high)
{
#if !NEWQ
  queue* elem, *tmp;
#endif
  lowranceusr4_fsdata* fs = NULL;

#if NEWQ
  // Iterate with waypt_disp_all?
  foreach(Waypoint* waypointp, waypt_list) {
#else
  QUEUE_FOR_EACH(&waypt_head, elem, tmp) {
    Waypoint* waypointp = (Waypoint*) elem;
#endif
    fs = (lowranceusr4_fsdata*) fs_chain_find(waypointp->fs, FS_LOWRANCEUSR4);

    if (fs && fs->uid_unit == uid_unit &&
        fs->uid_seq_low == uid_seq_low &&
        fs->uid_seq_high == uid_seq_high) {
      return waypointp;
    }
  }

  if (global_opts.debug_level >= 1) {
    printf(MYNAME " lowranceusr4_find_waypt: warning, failed finding waypoint with ids %d %d %d\n",
           uid_unit, uid_seq_low, uid_seq_high);
  }
  return NULL;
}

static void
lowranceusr4_parse_routes(void)
{
  unsigned int num_routes, i, j, text_len;
  unsigned int num_legs;
  char buff[MAXUSRSTRINGSIZE + 1];
  Waypoint* wpt_tmp;
  unsigned int uid_unit, uid_seq_low, uid_seq_high;

  num_routes = gbfgetint32(file_in);

  if (global_opts.debug_level >= 1) {
    printf(MYNAME " parse_routes: Num routes = %d\n", num_routes);
  }

  for (i = 0; i < num_routes; ++i) {
    rte_head = route_head_alloc();
    route_add_head(rte_head);
    rte_head->rte_num = i+1;

    lowranceusr4_fsdata* fsdata = lowranceusr4_alloc_fsdata();
    fs_chain_add(&(rte_head->fs), (format_specific_data*) fsdata);

    /* read/parse route, with fields as follows (taken mostly
       from http://lowranceusrv4togpxconverter.blogspot.com/):

         UID unit number             - uint32
         UID sequence number         - int64
         Route stream version number - uint16
         Route name length (bytes)   - uint32
         Route name                  - utf-16 string w/above length (r->rte_name)
         Number of waypoints         - uint32 (N)
         Waypoint list               - sequence of N (uint32, uint64) waypoint UIDs
     */

    /* UID unit number */
    fsdata->uid_unit = gbfgetint32(file_in);

    /* 64-bit UID sequence number */
    fsdata->uid_seq_low = gbfgetint32(file_in);
    fsdata->uid_seq_high = gbfgetint32(file_in);

    /* Route stream version number, discard for now */
    gbfgetint16(file_in);

    /* Route name; input is 2 bytes per char, we convert to 1 */
    text_len = lowranceusr4_readstr(&buff[0], MAXUSRSTRINGSIZE, file_in, 2);
    if (text_len) {
      buff[text_len] = '\0';
      rte_head->rte_name = buff;
    }

    num_legs = gbfgetint32(file_in);

    if (global_opts.debug_level >= 1) {
      printf(MYNAME " parse_routes: route name=%s has %d waypoints\n",
             CSTRc(rte_head->rte_name), num_legs);
    }

    for (j = 0; j < num_legs; ++j) {
      uid_unit = gbfgetint32(file_in);
      uid_seq_low = gbfgetint32(file_in);
      uid_seq_high = gbfgetint32(file_in);
      wpt_tmp = lowranceusr4_find_waypt(uid_unit, uid_seq_low, uid_seq_high);
      if (wpt_tmp) {
        if (global_opts.debug_level >= 2) {
          printf(MYNAME " parse_routes: added wpt %s to route %s\n",
                 CSTRc(wpt_tmp->shortname), CSTRc(rte_head->rte_name));
        }
        route_add_wpt(rte_head, new Waypoint(*wpt_tmp));
      }
    }

    /* Mystery byte, discard */
    gbfgetc(file_in);
  }
}

static void
lowranceusr4_parse_trails(void)
{
  int num_trails, num_trail_pts, M, i, j, k, trk_num, text_len;
  char buff[MAXUSRSTRINGSIZE + 1];
  Waypoint* wpt_tmp;

  /* num trails */
  num_trails = gbfgetint32(file_in);

  if (global_opts.debug_level >= 1) {
    printf(MYNAME " parse_trails: num trails = %d\n", num_trails);
  }

  for (i = trk_num = 0; i < num_trails; ++i) {
    trk_head = route_head_alloc();
    trk_head->rte_num = ++trk_num;
    track_add_head(trk_head);

    lowranceusr4_fsdata* fsdata = lowranceusr4_alloc_fsdata();
    fs_chain_add(&(trk_head->fs), (format_specific_data*) fsdata);

    /* read/parse trail, with fields as follows (taken mostly from
       http://lowranceusrv4togpxconverter.blogspot.com/):

         UID unit number             - uint32
         UID sequence number         - int64
         Trail stream version number - uint16
         Trail name length (bytes)   - uint32
         Trail name                  - utf-16 string w/above length (t->rte_name)
         Flags                       - uint32
         Color ID                    - uint32
         Comment length (bytes)      - uint32
         Comment                     - utf-16 string w/above length (t->rte_desc)
         Creation date               - uint32
         Creation time               - uint32
         Unused                      - uint8
         Active flag                 - uint8
         Visible flag                - uint8
         Data count (?)              - uint32
         Data type depth (?)         - uint8
         Data type water temp (?)    - uint8
         Data type SOG (?)           - uint8
         Trackpoint count            - int32 (N)
         Trackpoint list             - sequence of N objects as follows:
           Unknown (?)               - uint16
           Unknown (?)               - uint8
           POSIX timestamp (?)       - uint32 (w->creation_time)
           Longitude (radians)       - double (w->longitude)
           Latitude (radians)        - double (w->latitude)
           Data item count           - uint32 (M)
           Data items                - sequence of M objects as follows:
             Unknown (?)             - uint8
             Unknown (?)             - float
     */

    /* UID unit number */
    fsdata->uid_unit = gbfgetint32(file_in);

    /* 64-bit UID sequence number */
    fsdata->uid_seq_low = gbfgetint32(file_in);
    fsdata->uid_seq_high = gbfgetint32(file_in);

    /* Trail stream version number, discard for now */
    gbfgetint16(file_in);

    /* Trail name; input is 2 bytes per char, we convert to 1 */
    text_len = lowranceusr4_readstr(&buff[0], MAXUSRSTRINGSIZE, file_in, 2);
    if (text_len) {
      buff[text_len] = '\0';
      trk_head->rte_name = buff;
    }

    /* Flags, discard for now */
    gbfgetint32(file_in);

    /* Color ID, discard for now */
    gbfgetint32(file_in);

    /* Comment/description */
    text_len = lowranceusr4_readstr(&buff[0], MAXUSRSTRINGSIZE, file_in, 2);
    if (text_len) {
      buff[text_len] = '\0';
      trk_head->rte_desc = buff;
    }

    /* Creation date/time, discard for now */
    gbfgetint32(file_in);
    gbfgetint32(file_in);

    /* Some flag bytes, discard for now */
    gbfgetc(file_in);
    gbfgetc(file_in);
    gbfgetc(file_in);

    /* Some mysterious "data count" and "data type" stuff, not sure
       what it's for, need dox */
    gbfgetint32(file_in);
    gbfgetc(file_in);
    gbfgetc(file_in);
    gbfgetc(file_in);

    num_trail_pts = gbfgetint32(file_in);

    if (global_opts.debug_level >= 1) {
      printf(MYNAME " parse_trails: trail %d name=%s has %d trackpoints\n",
             trk_num, CSTRc(trk_head->rte_name), num_trail_pts);
    }

    for (j = 0; j < num_trail_pts; ++j) {
      wpt_tmp = new Waypoint;

      /* Some unknown bytes */
      gbfgetint16(file_in);
      gbfgetc(file_in);

      /* POSIX timestamp */
      wpt_tmp->SetCreationTime(QDateTime::fromTime_t(gbfgetint32(file_in)));

      /* Long/Lat */
      wpt_tmp->longitude = gbfgetdbl(file_in) / DEGREESTORADIANS; /* rad to deg */
      wpt_tmp->latitude = gbfgetdbl(file_in) / DEGREESTORADIANS;

      /* Mysterious per-trackpoint data, toss it for now */
      M = gbfgetint32(file_in);
      for (k = 0; k < M; ++k) {
        gbfgetc(file_in);
        gbfgetflt(file_in);
      }

      track_add_wpt(trk_head, wpt_tmp);

      if (global_opts.debug_level >= 2) {
        printf(MYNAME " parse_routes: added trackpoint %f,%f to route %s\n",
               wpt_tmp->latitude, wpt_tmp->longitude, CSTRc(trk_head->rte_name));
      }
    }
  }
}


static void
data_read(void)
{
  short int MajorVersion, MinorVersion;
  int text_len, DataStreamVersion;
  unsigned int create_date, create_time, serial_num;
  unsigned char byte;
  char buff[MAXUSRSTRINGSIZE + 1];


  MajorVersion = gbfgetint16(file_in);
  reading_version = MajorVersion;
  MinorVersion = gbfgetint16(file_in);
  DataStreamVersion = gbfgetint32(file_in);

  if (global_opts.debug_level >= 1) {
    printf(MYNAME " data_read: Major Version %d Minor Version %d Data Stream Version %d\n",
           MajorVersion, MinorVersion, DataStreamVersion);
  }

  if (MajorVersion != 4) {
    fatal(MYNAME ": input file is from an unsupported version of the USR format (must be version 4)\n");
  }

  text_len = lowranceusr4_readstr(&buff[0], MAXUSRSTRINGSIZE, file_in, 1);
  if (text_len > 0 && global_opts.debug_level >= 1) {
    buff[text_len] = '\0';
    printf(MYNAME " file title: %s\n", buff);
  }

  text_len = lowranceusr4_readstr(&buff[0], MAXUSRSTRINGSIZE, file_in, 1);
  if (text_len > 0 && global_opts.debug_level >= 1) {
    buff[text_len] = '\0';
    printf(MYNAME " date string: %s\n", buff);
  }

  /* for now we won't use these for anything */
  create_date = gbfgetint32(file_in);
  create_time = gbfgetint32(file_in);
  byte = gbfgetc(file_in); /* unused, apparently */

  serial_num = gbfgetint32(file_in);
  if (global_opts.debug_level >= 1) {
    printf(MYNAME " device serial number %u\n", (unsigned int)serial_num);
  }

  text_len = lowranceusr4_readstr(&buff[0], MAXUSRSTRINGSIZE, file_in, 1);
  if (text_len > 0 && global_opts.debug_level >= 1) {
    buff[text_len] = '\0';
    printf(MYNAME " content description: %s\n", buff);
  }

  lowranceusr4_parse_waypoints();
  lowranceusr4_parse_routes();
  lowranceusr4_parse_trails();
}


static void
lowranceusr4_waypt_disp(const Waypoint* wpt)
{
  /* UID unit number */
  gbfputint32(opt_serialnum_i, file_out);

  /* 64-bit UID sequence number */
  gbfputint32(waypt_uid++, file_out);
  gbfputint32(0, file_out);

  /* Waypt stream version number: this always seems to be 2 in my data
     so that's what I'll use */
  gbfputint16(2, file_out);

  /* Waypt name */
  lowranceusr4_writestr(wpt->shortname, file_out, 2);

  /* Long/Lat */
  gbfputint32(lon_deg_to_mm(wpt->longitude), file_out);
  gbfputint32(lat_deg_to_mm(wpt->latitude), file_out);

  /* Flags: this always seems to be 2 or 4 in my data, not sure what
     it means */
  gbfputint32(2, file_out);

  /* Icon ID; TODO: need to invert icon description to an icon number,
     see parse_waypoints above */
  gbfputint16(0, file_out);

  /* Color ID */
  gbfputint16(0, file_out);

  /* Waypt description */
  lowranceusr4_writestr(wpt->description, file_out, 2);

  /* Alarm radius */
  gbfputflt(WAYPT_GET(wpt, proximity, 0.0), file_out);

  /* Creation date/time */
  gbfputint32(lowranceusr4_jd_from_timestamp(wpt->GetCreationTime().toTime_t()), file_out);
  gbfputint32(wpt->GetCreationTime().toTime_t(), file_out);

  /* Unused byte */
  gbfputc(0, file_out);

  /* Depth in feet */
  gbfputflt(METERS_TO_FEET(WAYPT_GET(wpt, depth, 0.0)), file_out);

  /* Loran data */
  gbfputint32(0, file_out);
  gbfputint32(0, file_out);
  gbfputint32(0, file_out);
}

static void
lowranceusr4_write_waypoints(void)
{
  int i;

  /* enumerate all waypoints from both the plain old waypoint list and
     also all routes */
  waypt_table_sz = 0;
  waypt_table_ct = 0;
  waypt_table = NULL;
  waypt_disp_all(register_waypt);
  route_disp_all(NULL, NULL, register_waypt);

  if (global_opts.debug_level >= 1) {
    printf(MYNAME " writing %d waypoints\n", waypt_table_ct);
  }

  gbfputint32(waypt_table_ct, file_out);
  waypt_uid = 0;
  for (i = 0; i < waypt_table_ct; ++i) {
    if (global_opts.debug_level >= 2) {
      printf(MYNAME " writing out waypt %d (%s - %s)\n",
             i, CSTRc(waypt_table[i]->shortname), CSTRc(waypt_table[i]->description));
    }
    lowranceusr4_waypt_disp((const Waypoint*)waypt_table[i]);
  }
}

static void
lowranceusr4_write_route_hdr(const route_head* rte)
{
  if (global_opts.debug_level >= 1) {
    printf(MYNAME " writing route #%d (%s) with %d waypts\n",
           route_uid, CSTRc(rte->rte_name), rte->rte_waypt_ct);
  }

  /* UID unit number */
  gbfputint32(opt_serialnum_i, file_out);

  /* 64-bit UID sequence number */
  gbfputint32(route_uid++, file_out);
  gbfputint32(0, file_out);

  /* Route stream version number: seems to be 1 in my data */
  gbfputint16(1, file_out);

  /* Waypt name */
  lowranceusr4_writestr(rte->rte_name, file_out, 2);

  /* Num waypoints */
  gbfputint32(rte->rte_waypt_ct, file_out);
}

static void
lowranceusr4_write_wpt_uids(const Waypoint* wpt)
{
  int waypt_idx;

  /* find the index of wpt in our table */
  waypt_idx = lowranceusr4_find_waypt_index(wpt);
  if (global_opts.debug_level >= 2) {
    if (waypt_idx > waypt_table_ct) {
      printf(MYNAME " WARNING: failed finding waypoint %s in waypoint table\n",
             CSTRc(wpt->shortname));
    } else {
      printf(MYNAME " adding waypt %d (%s) to route\n",
             waypt_idx, CSTRc(waypt_table[waypt_idx]->shortname));
    }
  }

  gbfputint32(opt_serialnum_i, file_out);
  gbfputint32(waypt_idx, file_out);
  gbfputint32(0, file_out);
}

static void
lowranceusr4_write_route_trl(const route_head* rte)
{
  /* Mystery byte */
  gbfputc(0, file_out);
}

static void
lowranceusr4_write_routes(void)
{
  if (global_opts.debug_level >= 1) {
    printf(MYNAME " writing %d routes\n", route_count());
  }
  gbfputint32(route_count(), file_out);
  route_uid = 0;
  route_disp_all(lowranceusr4_write_route_hdr,
                 lowranceusr4_write_route_trl,
                 lowranceusr4_write_wpt_uids);
}

static void
lowranceusr4_write_track_hdr(const route_head* trk)
{
  if (global_opts.debug_level >= 1) {
    printf(MYNAME " writing track %d (%s) with %d trackpoints\n",
           track_uid, CSTRc(trk->rte_name), trk->rte_waypt_ct);
  }

  /* UID unit number */
  gbfputint32(opt_serialnum_i, file_out);

  /* 64-bit UID sequence number */
  gbfputint32(track_uid++, file_out);
  gbfputint32(0, file_out);

  /* Route stream version number: always seems to be 3 in my data */
  gbfputint16(3, file_out);

  /* Track name */
  lowranceusr4_writestr(trk->rte_name, file_out, 2);

  /* Flags: always seems to be 2 in my data */
  gbfputint32(2, file_out);

  /* Color ID */
  gbfputint32(0, file_out);

  /* Comment */
  lowranceusr4_writestr(trk->rte_desc, file_out, 2);

  /* Creation date/time */
  gbfputint32(0, file_out);
  gbfputint32(0, file_out);

  /* Unused byte */
  gbfputc(0, file_out);

  /* Active flag */
  gbfputc(0, file_out);

  /* Visible flag; I'll just assume all tracks should be visible for
     now */
  gbfputc(1, file_out);

  /* Mysterious "data count" and "data type" stuff */
  gbfputint32(0, file_out);
  gbfputc(0, file_out);
  gbfputc(0, file_out);
  gbfputc(0, file_out);

  /* Trackpoint count */
  gbfputint32(trk->rte_waypt_ct, file_out);
}

static void
lowranceusr4_write_track_waypt(const Waypoint* wpt)
{
  /* Some unknown bytes */
  gbfputint16(0, file_out);
  gbfputc(0, file_out);

  /* Timestamp */
  gbfputint32(wpt->GetCreationTime().toTime_t(), file_out);

  /* Long/Lat */
  gbfputdbl(wpt->longitude * DEGREESTORADIANS, file_out);
  gbfputdbl(wpt->latitude * DEGREESTORADIANS, file_out);

  /* Mysterious per-trackpoint data; we'll just say there are "0"
     mystery entries */
  gbfputint32(0, file_out);
}

static void
lowranceusr4_write_trails(void)
{
  if (global_opts.debug_level >= 1) {
    printf(MYNAME " writing %d tracks\n", track_count());
  }
  gbfputint32(track_count(), file_out);
  track_uid = 0;
  track_disp_all(lowranceusr4_write_track_hdr, NULL, lowranceusr4_write_track_waypt);
}

static void
data_write(void)
{
  short int MajorVersion, MinorVersion;
  int DataStreamVersion;
  time_t now;
  struct tm* now_tm;
  char buf[256];

  setshort_length(mkshort_handle, 15);

  MajorVersion = 4;
  MinorVersion = 0;
  DataStreamVersion = 10;

  gbfputint16(MajorVersion, file_out);
  gbfputint16(MinorVersion, file_out);
  gbfputint32(DataStreamVersion, file_out);

  /* file title */
  lowranceusr4_writestr(opt_title, file_out, 1);

  /* date string */
  now = time(NULL);
  now_tm = gmtime(&now);
  sprintf(buf, "%d/%d/%d", now_tm->tm_mon+1, now_tm->tm_mday, now_tm->tm_year+1900);
  lowranceusr4_writestr(buf, file_out, 1);

  /* creation date/time */
  gbfputint32(lowranceusr4_jd_from_timestamp(now), file_out);
  gbfputint32(now, file_out);

  /* unused byte */
  gbfputc(0, file_out);

  /* device serial number */
  opt_serialnum_i = atoi(opt_serialnum);
  gbfputint32(opt_serialnum_i, file_out);

  /* content description */
  lowranceusr4_writestr(opt_content_descr, file_out, 1);

  lowranceusr4_write_waypoints();
  lowranceusr4_write_routes();
  lowranceusr4_write_trails();
}


ff_vecs_t lowranceusr4_vecs = {
  ff_type_file,
  FF_CAP_RW_ALL,
  rd_init,
  wr_init,
  rd_deinit,
  wr_deinit,
  data_read,
  data_write,
  NULL,
  lowranceusr4_args,
  CET_CHARSET_ASCII, 0	/* CET-REVIEW */
};
