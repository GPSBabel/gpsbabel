/*
	Access to Lowrance USR version 4/5/6 files.
	Contributed to gpsbabel by Kris Beevers (beevek at gmail.com)

	Copyright (C) 2011 Robert Lipe, robertlipe+source@gpsbabel.org

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
        09/12/2018 - BJ Kowalski (bj dot kowalski at gmail.com)
        - Correct USR 4 decode
        - Added initial support for USR 5 and USR 6 formats

        USRv3 supported trails with a maximum of 10,000 track-points.
        USRv4 and above support a maximum of 20,000 track-points(actually 24K and change).
        USRv4 and above & GPX support trails with track-segments.  
        USRv6 supports trail characteristics speed & temperature.

        USR Data File Description from Lowrance Manual
        ----------------------------------------------
          User Data File version 6 - USRv6
            This is used to import and export waypoints, routes and colored Trails.

          User Data File version 5 - USRv5
            This is used to import and export waypoints and routes with a
            standardized universally unique identifier (UUID), which is very
            reliable and easy to use. The data includes such information as
            the time and date when a route was created, and so on.

          User Data File version 4 - USRv4
            This is best used when transferring data from one system to
            another, since it contains all the extra bits of information these
            systems store about items.

          User Data File version 3 - USRv3 (w/depth)
            Should be used when transferring user data from one system to
            a legacy product (Lowrance LMS, LCX, and so on.)

          User Data File version 2 - USRv2 (no depth)
            Can be used when transferring user data from one system to a
            legacy product (Lowrance LMS, LCX, and so on.)

          GPX (GPS Exchange, no depth)
            This is the format most used on the web that shares among
            most GPS systems in the world. Use this format if you are taking
            data to a competitors unit. 
*/


#include "defs.h"
#include <cmath> /* for lat/lon conversion */
#include <cstdio> /* for gmtime */
#include <cstdlib> // atoi
#include <ctime> /* for gmtime */

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

#define MAXUSRSTRINGSIZE   256
#define SEMIMINOR          6356752.3142
#define DEGREESTORADIANS   0.01745329252


typedef struct {
  format_specific_data fs;
  int uid_unit;
  int uid_unit2;
  int uid_seq_low;
  int uid_seq_high;
  uint UUID1;
  uint UUID2;
  uint UUID3;
  uint UUID4;
  int  flags;
  int  color;
  int  icon_num;
} lowranceusr4_fsdata;


static int
lowranceusr4_readstr(char* buf, const int maxlen, gbfile* file, int bytes_per_char)
{
  int len;
  int bytesread = 0;

  int org = len = gbfgetint32(file); /* bytes */
  if (len < 0) {
    buf[0] = '\0'; /* seems len=-1 means no string */
    return 0;
  } else if (len) {
    if (len > maxlen) {
      len = maxlen;
    }
    if (bytes_per_char == 1) {
      bytesread += gbfread(buf, 1, len, file);
    } else {
      /* simple adjustment to read strings where characters are 16
         bits (or more).  for now let's just project the characters
         down onto utf-8 space by ignoring all but the most
         significant byte. */
      char discard;
      for (int i = 0; i < len/bytes_per_char; ++i) {
        bytesread += gbfread(&buf[i], 1, 1, file);
        for (int j = 1; j < bytes_per_char; ++j) {
          bytesread +=gbfread(&discard, 1, 1, file);
        }
      }
      buf[len/bytes_per_char] = '\0';
    }
    if (org > bytesread) {
      (void) gbfseek(file, (org - bytesread), SEEK_CUR);
    }
  }

  return len;
}

static void
lowranceusr4_writestr(const QString& buf, gbfile* file, unsigned int bytes_per_char)
{
  unsigned int len = buf.length();

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
    "", ARGTYPE_STRING, ARG_NOMINMAX, nullptr
  },
  {
    "serialnum", &opt_serialnum, "(output) Device serial number",
    "0", ARGTYPE_INT, ARG_NOMINMAX, nullptr
  },
  {
    "description", &opt_content_descr, "(output) Content description",
    "", ARGTYPE_STRING, ARG_NOMINMAX, nullptr
  },
  ARG_TERMINATOR
};

static void
rd_init(const QString& fname)
{
  file_in = gbfopen_le(fname, "rb", MYNAME);
}

static void
rd_deinit()
{
  gbfclose(file_in);
}

static void
wr_init(const QString& fname)
{
  file_out = gbfopen_le(fname, "wb", MYNAME);
  mkshort_handle = mkshort_new_handle();
}

static void
wr_deinit()
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
  struct tm ntm;

  /* get UTC time from time_t */
  struct tm* ptm = gmtime(&t);
  memset(&ntm, 0, sizeof(ntm));
  ntm.tm_hour = ptm->tm_hour;
  ntm.tm_min = ptm->tm_min;
  ntm.tm_sec = ptm->tm_sec;

  /* convert the JD number to get day/month/year */
  int a = jd_number + 32044;
  int b = (4*a + 3) / 146097;
  int c = a - (146097*b) / 4;
  int d = (4*c + 3) / 1461;
  int e = c - (1461*d) / 4;
  int m = (5*e + 2) / 153;
  ntm.tm_mday = e + 1 - (153*m + 2) / 5;
  ntm.tm_mon = m + 3 - 12 * (m / 10) - 1;
  ntm.tm_year = 100 * b + d - 4800 + m / 10 - 1900;

  /* put it all back together into a unix timestamp in UTC */
  time_t out = mkgmtime(&ntm);

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
  (*dest)->fs.next = nullptr;
}

static void
lowranceusr4_free_fsdata(void* fsdata)
{
  xfree(fsdata);
}

static
lowranceusr4_fsdata*
lowranceusr4_alloc_fsdata()
{
  lowranceusr4_fsdata* fsdata = (lowranceusr4_fsdata*) xcalloc(sizeof(*fsdata), 1);
  fsdata->fs.type = FS_LOWRANCEUSR4;
  fsdata->fs.copy = (fs_copy) lowranceusr4_copy_fsdata;
  fsdata->fs.destroy = lowranceusr4_free_fsdata;
  fsdata->fs.convert = nullptr;

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
           (A->shortname == B->shortname) &&
           (A->latitude == B->latitude) &&
           (A->longitude == B->longitude));
}

static void
register_waypt(const Waypoint* ref)
{
  Waypoint* wpt = const_cast<Waypoint*>(ref);

  for (int i = 0; i < waypt_table_ct; i++) {
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
           qPrintable(wpt->shortname), qPrintable(wpt->description), waypt_table_ct);
  }

  waypt_table[waypt_table_ct] = wpt;
  waypt_table_ct++;
}

/* end borrowed from raymarine.c */

static int
lowranceusr4_find_waypt_index(const Waypoint* wpt)
{
  for (int i = 0; i < waypt_table_ct; ++i) {
    if (same_points(wpt, (const Waypoint*)waypt_table[i])) {
      return i;
    }
  }
  return waypt_table_ct+1; /* should never happen */
}

/* read/parse waypoint, with fields as follows (taken mostly
   from http://lowranceusrv4togpxconverter.blogspot.com/):

     UUID 1 value                - uint32   *** UNIQUE to USR 5/6 ***
     UUID 2 value                - uint32   *** UNIQUE to USR 5/6 ***
     UUID 3 value                - uint32   *** UNIQUE to USR 5/6 ***
     UUID 4 value                - uint32   *** UNIQUE to USR 5/6 ***
     UID unit number             - uint32
     UID sequence number         - int64
     Waypt stream version number - uint16
     Waypt name length (bytes)   - uint32
     Waypoint name               - utf-16 string w/above length (w->shortname)
     Unit Number                 - uint32   *** UNIQUE to USR 5/6 ***
     Longitude (mercator meters) - int32 (w->longitude)
     Latitude (mercator meters)  - int32 (w->latitude)
     Flags                       - uint32 (0 - Hidden ICON, 1 - Display ICON, 2 - Display ICON and Name, 4 - Route Point)
     Icon ID                     - uint16 (to w->icon_descr via conversion)
                                     0  Circle
                                     1  Diamond
                                     2  X
                                    22  Stop Sign
                                    30  Campsite
                                    37  Skull and Cross Bones
                                    40  Diver Flag
                                    42  Anchor
                                    61  Star
     Color ID                    - uint16
     Description length (bytes)  - uint32
     Description                 - utf-16 string w/above length (w->description)
     Alarm radius                - float (w->proximity)
     Creation date               - uint32 (w->creation_time)
     Creation time               - uint32 (w->creation_time)
     Unused                      - uint8
     Depth (feet)                - float (w->depth)
                                          Use of Loran based navigation became obsolete Worldwide in 2015 due to adoption of GPS
     Loran GRI                   - int32  Loran Group Repetition Interval
     Loran TdA                   - int32  Loran Time Differential A
     Loran TdB                   - int32  Loran Time Differential B
 */

static void
lowranceusr4_parse_waypoints(int USR_version)
{
  char name_buff[MAXUSRSTRINGSIZE + 1];
  char desc_buff[MAXUSRSTRINGSIZE + 1];
  unsigned int waypoint_version;
/*
  time_t now;
  struct tm ts;
  char   time_buf[32];
*/

  unsigned int num_waypts = gbfgetint32(file_in);

  if (global_opts.debug_level >= 1) {
    printf(MYNAME " parse_waypoints: Num waypoints %d\n", num_waypts);
  }

  if (global_opts.debug_level == 99) {
    printf(MYNAME " parse_waypoints: ");
    if (USR_version > 4) {
      printf("Universal ID                        ");
    }
    printf("              Sequence Number  Stream  Waypoint\n");

    printf(MYNAME " parse_waypoints: ");
    if (USR_version > 4) {
      printf("    ID1      ID2      ID3      ID4  ");
    }
    printf("Unit Number     Low      High  Version Length Name            ");
    if (USR_version > 4) {
      printf(" Unit Number2");
    }
    printf(" Longitude      Latitude       Flags    ICON   Color  Length Description     ");
    printf(" Time     Date     Unused   Depth    LoranGRI LoranTda LoranTdb\n");

    printf(MYNAME " parse_waypoints: ");
    if (USR_version > 4) {
      printf("-------- -------- -------- -------- ");
    }
    printf("----------- -------- -------- -------- ------ ----------------");
    if (USR_version > 4) {
      printf(" ------------");
    }
    printf(" -------------- -------------- -------- ------ ------ ------ ----------------");
    printf(" -------- -------- -------- -------- -------- -------- --------\n");
  }

  for (unsigned int i = 0; i < num_waypts; ++i) {
    Waypoint* wpt_tmp = new Waypoint;
    lowranceusr4_fsdata* fsdata = lowranceusr4_alloc_fsdata();
    fs_chain_add(&(wpt_tmp->fs), (format_specific_data*) fsdata);

    if (USR_version > 4) {
      /* USR 5 and 6 have four additional data values at the start of each Waypoint */
      /* These values are used to identify global way points that define routes */
      fsdata->UUID1 = gbfgetint32(file_in);
      fsdata->UUID2 = gbfgetint32(file_in);
      fsdata->UUID3 = gbfgetint32(file_in);
      fsdata->UUID4 = gbfgetint32(file_in);
    }

    /* UID unit number */
    fsdata->uid_unit = gbfgetint32(file_in);

    /* 64-bit UID sequence number */
    fsdata->uid_seq_low = gbfgetint32(file_in);
    fsdata->uid_seq_high = gbfgetint32(file_in);

    /* Waypt stream version number */
    waypoint_version = gbfgetint16(file_in);

    /* Waypoint name; input is 2 bytes per char, we convert to 1 */
    int name_len = lowranceusr4_readstr(&name_buff[0], MAXUSRSTRINGSIZE, file_in, 2);
    if (name_len) {
      name_buff[name_len] = '\0';
      wpt_tmp->shortname = name_buff;
    }

    if (USR_version > 4) {
      /* USR 5 and 6 have a second Unit Number captured in Waypoints */
      fsdata->uid_unit2 = gbfgetint32(file_in);
    }

    /* Long/Lat */
    wpt_tmp->longitude = lon_mm_to_deg(gbfgetint32(file_in));
    wpt_tmp->latitude = lat_mm_to_deg(gbfgetint32(file_in));

    /* Flags, discard for now */
    fsdata->flags = gbfgetint32(file_in);

    /* Icon ID; TODO: need to run this through something like
       lowranceusr_find_desc_from_icon_number to convert to a gpsbabel
       icon description; however it doesn't seem that the icon ids
       used in usr4 match those from usr{2,3} so we need a new
       mapping. */
    fsdata->icon_num = gbfgetint16(file_in);
    /* wpt_tmp->icon_descr = lowranceusr_find_desc_from_icon_number(icon_num); */

    /* Color ID, discard for now */
    fsdata->color = gbfgetint16(file_in);

    /* Waypoint descr; input is 2 bytes per char, we convert to 1 */
    int desc_len = lowranceusr4_readstr(&desc_buff[0], MAXUSRSTRINGSIZE, file_in, 2);
    if (desc_len) {
      desc_buff[desc_len] = '\0';
      wpt_tmp->description = desc_buff;
    }

    /* Alarm radius; XXX: I'm not sure what the units are here,
       assuming meters but may be feet? */
    WAYPT_SET(wpt_tmp, proximity, gbfgetflt(file_in));

    /* Creation date/time; the date is a Julian day number, and the
       time is a unix timestamp. */
    unsigned int create_date = gbfgetint32(file_in);
    unsigned int create_time = gbfgetint32(file_in);

    // Julian date 2440487 is 1/1/1970.  If that's the date we're working
    // with, as a practical matter, we have no date, so don't even compute
    // or set it.
    if (create_date > 2440587) {
      wpt_tmp->SetCreationTime(lowranceusr4_get_timestamp(create_date,
                               create_time));
    }

    /* Unused byte */
    char unused_byte = gbfgetc(file_in);

    /* Depth in feet */
    float waypt_depth = FEET_TO_METERS(gbfgetflt(file_in));
    WAYPT_SET(wpt_tmp, depth, waypt_depth);

    /* Loran data, discard for now */
    int loran_GRI = gbfgetint32(file_in);
    int loran_Tda = gbfgetint32(file_in);
    int loran_Tdb = gbfgetint32(file_in);

    if (global_opts.debug_level >= 1) {
      if (global_opts.debug_level == 99) {
        printf(MYNAME " parse_waypoints: ");
        if (USR_version > 4) {
          printf("%08x %08x %08x %08x ",
               fsdata->UUID1, fsdata->UUID2, fsdata->UUID3, fsdata->UUID4);
        }
        printf(" %10u %8d %8d %8d %6d %16s",
               fsdata->uid_unit, fsdata->uid_seq_low, fsdata->uid_seq_high,
               waypoint_version, name_len, name_buff);
        if (USR_version > 4) {
          printf("  %10u ", fsdata->uid_unit2);
        }
        printf(" %+14.9f %+14.9f", wpt_tmp->longitude, wpt_tmp->latitude);
        printf(" %08x %6d %6d", fsdata->flags, fsdata->icon_num, fsdata->color);
        printf(" %6d %16s", desc_len, desc_buff);
        printf(" %08x %0x8 %08x %f %08x %08x %08x\n",
            create_date, create_time, unused_byte, waypt_depth, loran_GRI, loran_Tda, loran_Tdb);
      } else {
        printf(MYNAME " parse_waypoints: version = %d, name = %s, uid_unit = %u, "
             "uid_seq_low = %d, uid_seq_high = %d, lat = %+.9f, lon = %+.9f, depth = %f\n",
             waypoint_version, qPrintable(wpt_tmp->shortname), fsdata->uid_unit,
             fsdata->uid_seq_low, fsdata->uid_seq_high,
             wpt_tmp->longitude, wpt_tmp->latitude, wpt_tmp->depth);
      }
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
  lowranceusr4_fsdata* fs = nullptr;

#if NEWQ
  // Iterate with waypt_disp_all?
  foreach(Waypoint* waypointp, waypt_list) {
#else
  QUEUE_FOR_EACH(&waypt_head, elem, tmp) {
    Waypoint* waypointp = reinterpret_cast<Waypoint *>(elem);
#endif
    fs = (lowranceusr4_fsdata*) fs_chain_find(waypointp->fs, FS_LOWRANCEUSR4);

    if (fs && fs->uid_unit == uid_unit &&
        fs->uid_seq_low == uid_seq_low &&
        fs->uid_seq_high == uid_seq_high) {
      return waypointp;
    }
  }

  if (global_opts.debug_level >= 1) {
    printf(MYNAME " lowranceusr4_find_waypt: warning, failed finding waypoint with ids %u %d %d\n",
           uid_unit, uid_seq_low, uid_seq_high);
  }
  return nullptr;
}

static Waypoint*
lowranceusr4_find_global_waypt(uint id1, uint id2, uint id3, uint id4)
{
#if !NEWQ
  queue* elem, *tmp;
#endif
  lowranceusr4_fsdata* fs = nullptr;

#if NEWQ
  // Iterate with waypt_disp_all?
  foreach(Waypoint* waypointp, waypt_list) {
#else
  QUEUE_FOR_EACH(&waypt_head, elem, tmp) {
    Waypoint* waypointp = reinterpret_cast<Waypoint *>(elem);
#endif
    fs = (lowranceusr4_fsdata*) fs_chain_find(waypointp->fs, FS_LOWRANCEUSR4);

    if (fs && fs->UUID1 == id1 &&
        fs->UUID2 == id2 &&
        fs->UUID3 == id3 &&
        fs->UUID4 == id4) {
      return waypointp;
    }
  }

  if (global_opts.debug_level >= 1) {
    printf(MYNAME " lowranceusr4_find_global_waypt: warning, failed finding waypoint with ids %08x %08x %08x %08x\n",
           id1, id2, id3, id4);
  }
  return nullptr;
}


/* Route - with fields as follows (taken mostly
   from http://lowranceusrv4togpxconverter.blogspot.com/):

     UID unit number             - uint32
     UID sequence number         - int64
     Route stream version number - uint16
     Route name length (bytes)   - uint32
     Route name                  - utf-16 string w/above length (r->rte_name)
     Number of waypoints         - uint32 (N)
     Waypoint list               - sequence of N (uint32, uint64) waypoint UIDs

     USR Version 4 or less, Waypoint list format uses local sequence number
     UID unit number             - uint32
     UID sequence number high    - uint32 (used for waypoint matching)
     UID sequence number low     - uint32 (used for waypoint matching)

     USR Version 5 or greater, Waypoints are identified by Universal IDs
     UUID ID 1                   - uint32 (used for waypoint matching)
     UUID ID 2                   - uint32 (used for waypoint matching)
     UUID ID 3                   - uint32 (used for waypoint matching)
     UUID ID 4                   - uint32 (used for waypoint matching)

    USR 5 or greater also has some additional unidentified fields at the end of the route data
 */

static void
lowranceusr4_parse_routes(int USR_version)
{
  char buff[MAXUSRSTRINGSIZE + 1];
  int route_version;
  int UUID1, UUID2, UUID3, UUID4;

  unsigned int num_routes = gbfgetint32(file_in);

  if (global_opts.debug_level >= 1) {
    printf(MYNAME " parse_routes: Num routes = %d\n", num_routes);
  }

  for (unsigned int i = 0; i < num_routes; ++i) {
    rte_head = route_head_alloc();
    route_add_head(rte_head);
    rte_head->rte_num = i+1;

    lowranceusr4_fsdata* fsdata = lowranceusr4_alloc_fsdata();
    fs_chain_add(&(rte_head->fs), (format_specific_data*) fsdata);

    if (USR_version >= 5) {
      /* Routes have Universal IDs */
        UUID1 = gbfgetint32(file_in);
        UUID2 = gbfgetint32(file_in);
        UUID3 = gbfgetint32(file_in);
        UUID4 = gbfgetint32(file_in);
    }

    /* UID unit number */
    fsdata->uid_unit = gbfgetint32(file_in);
    if (global_opts.debug_level >= 1) {
      printf(MYNAME " parse_routes: Unit %u (0x%08x)\n", fsdata->uid_unit, fsdata->uid_unit);
    }

    /* 64-bit UID sequence number */
    fsdata->uid_seq_low = gbfgetint32(file_in);
    fsdata->uid_seq_high = gbfgetint32(file_in);

    /* Route stream version number */
    route_version = gbfgetint16(file_in);
    if (global_opts.debug_level >= 1) {
      printf(MYNAME " parse_routes: Version = %d\n", route_version);
    }

    /* Route name; input is 2 bytes per char, we convert to 1 */
    unsigned int text_len = lowranceusr4_readstr(&buff[0], MAXUSRSTRINGSIZE, file_in, 2);
    if (text_len) {
      buff[text_len] = '\0';
      rte_head->rte_name = buff;
    }

    if (USR_version >= 5) {
      /* USR Version 5 and greater include unit ID in each route */
      gbfgetint32(file_in);
    }

    unsigned int num_legs = gbfgetint32(file_in);

    if (global_opts.debug_level >= 1) {
      if (USR_version >= 5) {
        printf(MYNAME " parse_routes: route name=%s (UUID %08x %08x %8x %08x) has %d legs\n",
             qPrintable(rte_head->rte_name), UUID1, UUID2, UUID3, UUID4, num_legs);
      } else {
        printf(MYNAME " parse_routes: route name=%s has %d legs\n",
             qPrintable(rte_head->rte_name), num_legs);
      }
    }

    if (USR_version <= 4) {
      /* Use UID based sequence numbers for route */
      for (unsigned int j = 0; j < num_legs; ++j) {
        unsigned int uid_unit = gbfgetint32(file_in);
        unsigned int uid_seq_low = gbfgetint32(file_in);
        unsigned int uid_seq_high = gbfgetint32(file_in);
        Waypoint* wpt_tmp = lowranceusr4_find_waypt(uid_unit, uid_seq_low, uid_seq_high);
        if (wpt_tmp) {
          if (global_opts.debug_level >= 2) {
            printf(MYNAME " parse_routes: added leg #%d routepoint %s (%+.9f, %+.9f) to route %s\n",
                   j, qPrintable(wpt_tmp->shortname),
                   wpt_tmp->longitude, wpt_tmp->latitude, qPrintable(rte_head->rte_name));
          }
          route_add_wpt(rte_head, new Waypoint(*wpt_tmp));
        }
      }
    } else {
      /* Use global sequence number for route */
      for (unsigned int j = 0; j < num_legs; ++j) {
        UUID1 = gbfgetint32(file_in);
        UUID2 = gbfgetint32(file_in);
        UUID3 = gbfgetint32(file_in);
        UUID4 = gbfgetint32(file_in);
        Waypoint* wpt_tmp = lowranceusr4_find_global_waypt(UUID1, UUID2, UUID3, UUID4);
        if (wpt_tmp) {
          if (global_opts.debug_level >= 2) {
            printf(MYNAME " parse_routes: added leg #%d wpt %s (%+.9f, %+.9f) to route %s\n",
                   j, qPrintable(wpt_tmp->shortname), 
                   wpt_tmp->longitude, wpt_tmp->latitude, qPrintable(rte_head->rte_name));
          }
          route_add_wpt(rte_head, new Waypoint(*wpt_tmp));
        }
      }
    }

    if (USR_version > 4) {
      /* USR Version 5 or greater, more mystery data, ignore for now */
      gbfgetint32(file_in);
      gbfgetint32(file_in);
      gbfgetc(file_in);
    }

    /* Mystery byte, discard */
    if (global_opts.debug_level == 99) {
      printf(MYNAME " parse_routes: end of route %02x\n", gbfgetc(file_in));
    } else {
      gbfgetc(file_in);
    }
  }
}

/* read/parse trail, with fields as follows (taken mostly from
   http://lowranceusrv4togpxconverter.blogspot.com/):

     UID unit number             - uint32
     UID sequence number         - int64
     Trail stream version number - uint16 (USR 4/5 value = 4; USR 6 value = 5)
     Trail name length (bytes)   - uint32
     Trail name                  - utf-16 string w/above length (t->rte_name)
     Flags                       - uint32
     Color ID                    - uint32
     Comment length (bytes)      - uint32
     Comment                     - utf-16 string w/above length (t->rte_desc)
     Creation date               - uint32
     Creation time               - uint32
     Unknown                     - uint8
     Active flag                 - uint8
     Visible flag                - uint8
     Data count                  - uint32
                                          *** START Trail Stream Version 4 ***
     Data type depth (?)         - uint8    total number of byte values based on Data count above
     Data type water temp (?)    - uint8
     Data type SOG (?)           - uint8
     Unknown data                - uint8
                                          *** END Trail Stream Version 4 ***
                                          *** START Trail Stream Version 5 ***
     Data type depth (?)         - uint32    total number of int32 values based on Data count above
     Data type water temp (?)    - uint32
     Data type SOG (?)           - uint32
     Unknown data                - uint32
                                          *** END Trail Stream Version 5 ***
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

static void
lowranceusr4_parse_trails()
{
  int trk_num;
  int trail_version;
  int data_count;
  char name_buff[MAXUSRSTRINGSIZE + 1];
  char desc_buff[MAXUSRSTRINGSIZE + 1];
  int trail_color;
  int trail_flags;

  /* num trails */
  int num_trails = gbfgetint32(file_in);

  if (global_opts.debug_level >= 1) {
    printf(MYNAME " parse_trails: num trails = %d\n", num_trails);
  }

  for (int i = trk_num = 0; i < num_trails; ++i) {
    trk_head = route_head_alloc();
    trk_head->rte_num = ++trk_num;
    track_add_head(trk_head);

    lowranceusr4_fsdata* fsdata = lowranceusr4_alloc_fsdata();
    fs_chain_add(&(trk_head->fs), (format_specific_data*) fsdata);

    /* UID unit number */
    fsdata->uid_unit = gbfgetint32(file_in);

    /* 64-bit UID sequence number */
    fsdata->uid_seq_low = gbfgetint32(file_in);
    fsdata->uid_seq_high = gbfgetint32(file_in);

    /* Trail stream version number */
    trail_version = gbfgetint16(file_in);
    if (global_opts.debug_level >= 1)
    {
      printf(MYNAME " parse_trails: trail Version %d\n", trail_version);
    }

    /* Trail name; input is 2 bytes per char, we convert to 1 */
    int name_len = lowranceusr4_readstr(&name_buff[0], MAXUSRSTRINGSIZE, file_in, 2);
    if (name_len) {
      name_buff[name_len] = '\0';
      trk_head->rte_name = name_buff;
    }

    /* Flags, discard for now */
    trail_flags = gbfgetint32(file_in);

    /* Color ID, discard for now */
    trail_color = gbfgetint32(file_in);

    /* Comment/description */
    int desc_len = lowranceusr4_readstr(&desc_buff[0], MAXUSRSTRINGSIZE, file_in, 2);
    if (desc_len) {
      desc_buff[desc_len] = '\0';
      trk_head->rte_desc = desc_buff;
    }

    /* Creation date/time, discard for now */
    gbfgetint32(file_in);
    gbfgetint32(file_in);

    /* Some flag bytes, discard for now */
    gbfgetc(file_in);
    gbfgetc(file_in);
    gbfgetc(file_in);

    /* Some mysterious "data count" */
    data_count = gbfgetint32(file_in);

    /* Structure differences between versions */
    if (trail_version < 5)
    {
      /* All values are byte values - ignore for now */
      for (int j = 0; j < data_count; j++)
        gbfgetc(file_in);
    } else {
      /* All values are int32 - ignore for now */
      for (int j = 0; j < data_count; j++)
        gbfgetint32(file_in);
    }

    int num_trail_pts = gbfgetint32(file_in);

    if (global_opts.debug_level >= 1) {
      printf(MYNAME " parse_trails: trail %d name=%s color=%d flags=%d has %d trackpoints\n",
             trk_num, qPrintable(trk_head->rte_name), trail_color, trail_flags, num_trail_pts);
    }

    if (global_opts.debug_level == 99) {
          printf(MYNAME " parse_trails: Trail %s\n", qPrintable(trk_head->rte_name));
          printf(MYNAME " parse_trails: Longitude      Latitude       Flag/Value pairs (01=Speed)\n");
          printf(MYNAME " parse_trails: -------------- -------------- -- -------- -- -------- -- --------\n");
    }
    for (int j = 0; j < num_trail_pts; ++j) {
      Waypoint* wpt_tmp = new Waypoint;

      /* Some unknown bytes */
      gbfgetint16(file_in);
      gbfgetc(file_in);

      /* POSIX timestamp */
      wpt_tmp->SetCreationTime(QDateTime::fromTime_t(gbfgetint32(file_in)));

      /* Long/Lat */
      wpt_tmp->longitude = gbfgetdbl(file_in) / DEGREESTORADIANS; /* rad to deg */
      wpt_tmp->latitude = gbfgetdbl(file_in) / DEGREESTORADIANS;

      if (global_opts.debug_level >= 2) {
        if (global_opts.debug_level == 99) {
          printf(MYNAME " parse_trails: %+14.9f %+14.9f", wpt_tmp->longitude, wpt_tmp->latitude);
        } else {
          printf(MYNAME " parse_trails: added trackpoint %+.9f,%+.9f to trail %s",
               wpt_tmp->longitude, wpt_tmp->latitude, qPrintable(trk_head->rte_name));
        }
      }

      track_add_wpt(trk_head, wpt_tmp);

      /* Mysterious per-trackpoint data, toss it for now */
      int M = gbfgetint32(file_in);
      for (int k = 0; k < M; ++k) {
        int flag = gbfgetc(file_in);
        float value = gbfgetflt(file_in);
        if (global_opts.debug_level == 99) {
          printf(" %02x %f", flag, value);
        }
      }

      if (M && (global_opts.debug_level == 99)) {
        printf("\n");
      }
    }
  }
}

static void
data_read()
{
  char buff[MAXUSRSTRINGSIZE + 1];

  int USR_version = gbfgetint32(file_in);
  int DataStream_version = gbfgetint32(file_in);

  if (global_opts.debug_level >= 1) {
    printf(MYNAME " data_read: USR Version %d Data Stream Version %d\n",
           USR_version, DataStream_version);
  }

  switch (USR_version)
  {
    case 4:       /* Lowrance USR Version 4 */
    case 5:       /* Lowrance USR Version 5 */
    case 6:       /* Lowrance USR Version 6 */
      break;
    default:
      fatal(MYNAME ": input file is USR Version %d format - try the 'lowranceusr' input option\n", USR_version);
      break;
  }

  int text_len = lowranceusr4_readstr(&buff[0], MAXUSRSTRINGSIZE, file_in, 1);
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
  unsigned int create_date = gbfgetint32(file_in);
  (void) create_date;
  unsigned int create_time = gbfgetint32(file_in);
  (void) create_time;
  unsigned char byte = gbfgetc(file_in); /* unused, apparently */
  (void) byte;

  unsigned int serial_num = gbfgetint32(file_in);
  if (global_opts.debug_level >= 1) {
    printf(MYNAME " device serial number %u\n", serial_num);
  }

  text_len = lowranceusr4_readstr(&buff[0], MAXUSRSTRINGSIZE, file_in, 1);
  if (text_len > 0 && global_opts.debug_level >= 1) {
    buff[text_len] = '\0';
    printf(MYNAME " content description: %s\n", buff);
  }

  lowranceusr4_parse_waypoints(USR_version);
  lowranceusr4_parse_routes(USR_version);
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
lowranceusr4_write_waypoints()
{
  /* enumerate all waypoints from both the plain old waypoint list and
     also all routes */
  waypt_table_sz = 0;
  waypt_table_ct = 0;
  waypt_table = nullptr;
  waypt_disp_all(register_waypt);
  route_disp_all(nullptr, nullptr, register_waypt);

  if (global_opts.debug_level >= 1) {
    printf(MYNAME " writing %d waypoints\n", waypt_table_ct);
  }

  gbfputint32(waypt_table_ct, file_out);
  waypt_uid = 0;
  for (int i = 0; i < waypt_table_ct; ++i) {
    if (global_opts.debug_level >= 2) {
      printf(MYNAME " writing out waypt %d (%s - %s)\n",
             i, qPrintable(waypt_table[i]->shortname), qPrintable(waypt_table[i]->description));
    }
    lowranceusr4_waypt_disp((static_cast<const Waypoint*>(waypt_table[i])));
  }
}

static void
lowranceusr4_write_route_hdr(const route_head* rte)
{
  if (global_opts.debug_level >= 1) {
    printf(MYNAME " writing route #%d (%s) with %d waypts\n",
           route_uid, qPrintable(rte->rte_name), rte->rte_waypt_ct);
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
  /* find the index of wpt in our table */
  int waypt_idx = lowranceusr4_find_waypt_index(wpt);
  if (global_opts.debug_level >= 2) {
    if (waypt_idx > waypt_table_ct) {
      printf(MYNAME " WARNING: failed finding waypoint %s in waypoint table\n",
             qPrintable(wpt->shortname));
    } else {
      printf(MYNAME " adding waypt %d (%s) to route\n",
             waypt_idx, qPrintable(waypt_table[waypt_idx]->shortname));
    }
  }

  gbfputint32(opt_serialnum_i, file_out);
  gbfputint32(waypt_idx, file_out);
  gbfputint32(0, file_out);
}

static void
lowranceusr4_write_route_trl(const route_head*)
{
  /* Mystery byte */
  gbfputc(0, file_out);
}

static void
lowranceusr4_write_routes()
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
           track_uid, qPrintable(trk->rte_name), trk->rte_waypt_ct);
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
lowranceusr4_write_trails()
{
  if (global_opts.debug_level >= 1) {
    printf(MYNAME " writing %d tracks\n", track_count());
  }
  gbfputint32(track_count(), file_out);
  track_uid = 0;
  track_disp_all(lowranceusr4_write_track_hdr, nullptr, lowranceusr4_write_track_waypt);
}

static void
data_write()
{
  time_t now;
  char buf[256];

  setshort_length(mkshort_handle, 15);

  short int MajorVersion = 4;
  short int MinorVersion = 0;
  int DataStreamVersion = 10;

  gbfputint16(MajorVersion, file_out);
  gbfputint16(MinorVersion, file_out);
  gbfputint32(DataStreamVersion, file_out);

  /* file title */
  lowranceusr4_writestr(opt_title, file_out, 1);

  /* date string */
  now = time(nullptr);
  struct tm* now_tm = gmtime(&now);
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
  nullptr,
  lowranceusr4_args,
  CET_CHARSET_ASCII, 0	/* CET-REVIEW */
  , NULL_POS_OPS,
  nullptr
};
