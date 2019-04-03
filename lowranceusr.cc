/*
  Access to Lowrance USR files.

  Initial contribution to gpsbabel by Jason Rust (jrust at rustyparts.com)

  Copyright (C) 2005 - 2018 Robert Lipe, robertlipe+source@gpsbabel.org

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

  6/21/05 - Ling Nero (rnlnero@yahoo.com)
    - Added Routes, Icons, & Tracks support
    - Fixed waypoint date/time stamp conversion
  02/09/08 - oliskoli
    - gbfile API
    - check for buffer overflows when reading names or comments
  02/25/2008 - Alan Porter (alan@kr4jb.net)
    - Added new icons for Lowrance iFinder Expedition C
    - Categorized geocaching waypoints using different icons
  01/06/2012 - Kris Beevers (beevek at gmail.com)
    - First pass read-write support for USR 4 in lowranceusr4.cc
  10/02/2018 - BJ Kowalski (bj dot kowalski at gmail.com)
    - Added support to lowranceusr4.cc for USR 4 route version 2
      format data.
    - Combined processing of USR 2/3/4/5/6 formats into a single
      file. The previous implementation did not support conversion
      between USR 2/3 and 4/5/6 or between 4/5/6 and 2/3 beause of
      the seperation in processing between lowranceusr.cc and
      lowranceusr4.cc.

  USR Background Information

  Collected from various WEB sources and Lowrance HOOK2 User Manual.

    Lowrance Support Site - https://www.lowrance.com/help-and-support/
    Hook2 Series Operator Manual (English) - //software.lowrance.com/Documents/Hook2-Series_OM_EN_988-11760-001_w.pdf
    Lowrance Endura FAQs II - http://support.lowrance.com/system/selfservice.controller?CONFIGURATION=1001&PARTITION_ID=1&secureFlag=false&TIMEZONE_OFFSET=&CMD=VIEW_ARTICLE&ARTICLE_ID=2028
    BBCBoards.Net : Lowrance Sonar/GPS Topic : Waypoints; USR Versions Thread : http://www.bbcboards.net/showthread.php?t=855028
    Python USR4 (Version 4) to GPX Converter - http://lowranceusrv4togpxconverter.blogspot.com/2011/12/about-this-blog.html

      User Data File version 6 - USRv6
       Latest format.
       Supports trail characteristics speed and temperature.

      User Data File version 5 - USRv5
        Lowrance introduced universally unique identifiers (UUIDs) in this version.

      User Data File version 4 - USRv4
        Seems to be the best option for transferring data from older Lowrance units.
        Many of the counts (Number of Waypoints, Number of Routes, etc) were exanded from
        16-bit integer values (maximum value of 65,535) to 32-bit (maxumum value 2,147,483,647)
        USRv4 and above support a maximum of 20,000 trail-points (actually 24K and change).
        USRv4 and above and GPX support trails with trail-segments.

      User Data File version 3 - USRv3
        Legacy file format.
        Added depth information to Route waypoints.
        Supports trails with a maximum of 10,000 trail-points.
        Last version that supports Event Marker ICONs.

      User Data File version 2 - USRv2
        Legacy file format.
        This is the default output USR version used by GPSBabel.
        This format contains ony basic information on waypoints, routes, and trails.

      GPX (GPS Exchange)
        Common format supported by many vendors and programs.  Lowrance only provides
        minimal support for GPX export with their HOOK2 series.  Waypoints include 
        longitude, latitude, timestamp, name and symbol information.  Routes include
        name and for route points longitude, latitude, timestamp, name, and symbol.
        Tracks include name and for track points longitude, latitude, and timestamp.

*/

#include <cmath>                 // for M_PI, atan, exp, log, tan
#include <cstdio>                // for printf, snprintf, sprintf, SEEK_CUR
#include <cstdlib>               // for atoi, abs
#include <cstring>               // for strlen, strcmp
#include <ctime>

#include <QtCore/QByteArray>     // for QByteArray
#include <QtCore/QDate>          // for QDate
#include <QtCore/QDateTime>      // for QDateTime
#include <QtCore/QLatin1String>  // for QLatin1String
#include <QtCore/QString>        // for QString, operator+, operator==, operator!=
#include <QtCore/QTextCodec>     // for QTextCodec
#include <QtCore/QTime>          // for QTime
#include <QtCore/Qt>             // for CaseInsensitive, UTC
#include <QtCore/QtGlobal>       // for qPrintable, uint, foreach

#include "defs.h"
#include "gbfile.h"              // for gbfgetint32, gbfputint32, gbfputint16, gbfgetc, gbfgetint16, gbfputc, gbfwrite, gbfeof, gbfgetflt, gbfclose, gbfgetdbl, gbfputdbl, gbfile, gbfputflt, gbfread, gbfseek, gbfopen_le
#include "src/core/datetime.h"   // for DateTime


typedef struct lowranceusr_icon_mapping {
  const int    value;
  const char*  icon;
} lowranceusr_icon_mapping_t;

#define DEF_ICON            10001
#define X_1_ICON            10003
#define DISABLED_CACHE_TXT "Disabled Cache"

const lowranceusr_icon_mapping_t lowranceusr_icon_value_table[] = {

  /* Taken from iFinder 1.8 */

  { 10000,    "diamond 1" },           // a.k.a. WPTa on iFinder Hunt C
  { 10001,    "diamond 2" },           // a.k.a. WPTb on iFinder Hunt C
  { 10002,    "diamond 3" },
  { X_1_ICON, "x 1" },                 // a.k.a. Xa on iFinder Hunt C
  { 10004,    "x 2" },                 // a.k.a. Xb on iFinder Hunt C
  { 10005,    "x 3" },
  { 10006,    "red cross" },
  { 10007,    "house" },
  { 10008,    "car" },                 // a.k.a. vehicle on iFinder Hunt C
  { 10009,    "store" },
  { 10010,    "gas station" },         // a.k.a. gas on iFinder Hunt C
  { 10011,    "fork and spoon" },      // a.k.a. food on iFinder Hunt C
  { 10012,    "telephone" },           // a.k.a. phone on iFinder Hunt C
  { 10013,    "airplane" },
  { 10014,    "exit sign" },
  { 10015,    "stop sign" },
  { 10016,    "exclamation" },
  { 10017,    "traffic light" },
  { 10018,    "american flag" },
  { 10019,    "person" },              // a.k.a. partner on iFinder Hunt C
  { 10020,    "restrooms" },
  { 10021,    "tree" },
  { 10022,    "mountains" },
  { 10023,    "campsite" },            // a.k.a. camp on iFinder Hunt C
  { 10024,    "picnic table" },        // a.k.a. picnic on iFinder Hunt C
  { 10025,    "deer" },                // a.k.a. big game on iFInder Hunt C
  { 10026,    "deer tracks" },
  { 10027,    "turkey tracks" },
  { 10028,    "tree stand" },
  { 10029,    "bridge" },
  { 10030,    "skull and crossbones" }, // a.k.a. skull on iFinder Hunt C
  { 10031,    "fish" },
  { 10032,    "two fish" },
  { 10033,    "dive flag" },
  { 10034,    "wreck" },
  { 10035,    "anchor" },
  { 10036,    "boat" },
  { 10037,    "boat ramp" },
  { 10038,    "flag buoy" },
  { 10039,    "dam" },
  { 10040,    "swimmer" },
  { 10041,    "pier"},

  /* The descriptions come from Alan Porter <alan@kr4jb.net>, using an iFinder Expedition C */
  /* Updated by bj.kowalski with ICON names from iFinder Hunt C SW Version 1.2.0 */

  { 10042,    "icon42" },               // black box with red X
  { 10043,    "red dot" },              // small red dot (route point)
  { 10044,    "atv" },                  // 4-wheeler
  { 10045,    "blind" },                // hiding hunter
  { 10046,    "oak tree" },             // tree (yellow base)
  { 10047,    "icon47" },               // windmill
  { 10048,    "camera" },               // camera
  { 10049,    "rub" },                  // tree (something in front of base)
  { 10050,    "scrape" },               // tree (something hanging from left side)
  { 10051,    "droppings" },            // 4 dots in rhombus shape
  { 10052,    "roost tree" },           // bare winter tree
  { 10053,    "animal bed" },           // hiding deer head peeking over bushes
  { 10054,    "feeder" },               // piston? over a pile of salt?
  { 10055,    "food plot" },            // corn
  { 10056,    "turkey" },               // turkey
  { 10057,    "waterfowl" },            // duck
  { 10058,    "upland" },               // hen
  { 10059,    "small game" },           // rabbit
  { 10060,    "predator paw" },         // paw print
  { 10061,    "blood" },                // 2 red flames?

  /* This list comes from 'wifinder' from ifinder H20 Color */

  { 10062,    "Interesting Land Feature" },
  { 10063,    "Global Location" },
  { 10064,    "Note" },
  { 10065,    "Ghost" },
  { 10066,    "Letter" },
  { 10067,    "Multi-Treasure" },
  { 10068,    "Mystery Or Puzzle" },
  { 10069,    "Treasure" },
  { 10070,    "Webmail" },
  { 10071,    "Sun" },
  { 10072,    "Musical Note" },
  { 10073,    "Camera/Movie Theater" },
  { 10074,    "Star" },
  { 10075,    "Coffee Mug" },
  { 10076,    "Books" },
  { 10077,    "Historical Marker" },
  { 10078,    "Tools/Repair" },
  { 10079,    "Favorite" },
  { 10080,    "Arena" },
  { 10081,    "Golf Course" },
  { 10082,    "Money/Atm" },

  /* This list comes from HOOK2 provided by BJ.Kowalski */

  { 10083,    "longgrass" },
  { 10084,    "rocks" },
  { 10086,    "hump" },
  { 10087,    "hole" },

  /* The following list is from TopoFusion */

  { 10000,    "Waypoint" },             /* diamond 1 */
  { DEF_ICON, "Text Label (No Dot)" },
  { 10018,    "Trailhead" },            /* american flag */
  { 10023,    "Campground" },           /* campsite */
  { 10022,    "Summit" },               /* mountains */
  { DEF_ICON, "Tall Tower" },
  { DEF_ICON, "Short Tower" },
  { 10021,    "Forest" },               /* tree */
  { DEF_ICON, "Mine" },
//{ 10038,    "Geocache" },            /* flag buoy */
//{ 10016,    "Geocache Found" },      /* exclamation */
  { DEF_ICON, "Skiing Area" },
  { 10029,    "Crossing" },             /* bridge */
  { 10007,    "House" },                /* house */
  { X_1_ICON, "Dot" },                  /* x 1 */
  { 10025,    "Hunting Area" },         /* deer */
  { 10031,    "Fishing Area" },         /* fish */
  { 10040,    "Swimming Area" },        /* swimmer */
  { 10012,    "Telephone" },            /* telephone */
  { 10024,    "Rest Area" },            /* picnic table */
  { 10021,    "Park" },                 /* tree */
  { 10007,    "Information" },          /* house */
  { 10022,    "Scenic Area" },          /* mountains */
  { DEF_ICON, "Bank/Dollar" },
  { 10009,    "Hotel" },                /* store */
  { 10011,    "Restaurant" },           /* fork and spoon */
  { 10030,    "Danger Area" },          /* skull and crossbones */
  { 10035,    "Anchor" },               /* anchor */
  { 10002,    "City (Large)" },         /* diamond 3 */
  { 10001,    "City (Medium)" },        /* diamond 2 */
  { 10000,    "City (Small)" },         /* diamond 1 */
  { DEF_ICON, "Drinking Water" },
  { 10008,    "Parking Area" },         /* car */
  { 10023,    "RV Park" },              /* campsite */
  { 10020,    "Rest Room" },            /* restroom */
  { 10019,    "Shower" },               /* person */
  { DEF_ICON, "Tunnel" },

  /* These are the icons that gpsbabel will use */

  { 10038,    "Geocache" },             // flag buoy
  { 10016,    "Geocache Found" },       // exclamation
  { 10043,    "Micro-Cache" },          // small red dot
  { 10065,    "Virtual cache" },        // ghost
  { 10051,    "Multi-Cache" },          // 4 dots in rhombus shape
  { 10068,    "Unknown Cache" },        // ? mark
  { 10045,    "Locationless (Reverse) Cache" },  // hiding hunter
  { 10066,    "Post Office" },          // letter
  { 10019,    "Event Cache" },          // person
  { 10070,    "Webcam Cache" },         // webcam
  { 10042,    DISABLED_CACHE_TXT },     // black box with red X

  // END OF ICON MAPPING
  {    -1,    nullptr }
};

#define DEF_USR4_ICON  2
#define DEF_USR4_COLOR 0

typedef struct lowranceusr4_icon_mapping {
  const int      value;
  const char*    icon;
  const char*    color[7];
} lowranceusr4_icon_mapping_t;

const lowranceusr4_icon_mapping_t lowranceusr4_icon_value_table[] = {

  /*  USR     GPX Symbol                COLOR1     COLOR2     COLOR3    COLOR4     COLOR5    COLOR6      COLOR7         HOOK2 Displays */

  {     1,    "diamond 1"           , { "blue",   "magenta", "orange", "yellow",  "green",   "aqua",    "white" }},   // diamond
  {     0,    "diamond 1"           , { "blue",   "magenta", "orange", "yellow",  "green",   "aqua",    "white" }},   // diamond
  {     1,    "diamond 2"           , { "blue",   "magenta", "orange", "yellow",  "green",   "aqua",    "white" }},   // diamond
  {     1,    "diamond 3"           , { "blue",   "magenta", "orange", "yellow",  "green",   "aqua",    "white" }},   // diamond
  {     2,    "x 1"                 , { "blue",   "magenta", "orange", "yellow",  "green",   "aqua",    "white" }},   // X
  {     2,    "x 2"                 , { "blue",   "magenta", "orange", "yellow",  "green",   "aqua",    "white" }},   // X
  {     2,    "x 3"                 , { "blue",   "magenta", "orange", "yellow",  "green",   "aqua",    "white" }},   // X
  {     4,    "fish"                , { "green",  "aqua",    "blue",   "magenta", "red",     "yellow",  "white" }},   // single fish
  {     5,    "two fish"            , { "aqua",   "blue",    "red",    "orange",  "yellow",  "green",   "white" }},   // schoolfish
  {     8,    "hole"                , { "aqua",   "blue",    "red",    "orange",  "yellow",  "green",   "white" }},   // dip sign
  {     9,    "hump"                , { "aqua",   "blue",    "red",    "orange",  "yellow",  "green",   "white" }},   // bump sign
  {    10,    "longgrass"           , { "green",  "aqua",    "blue",   "red",     "orange",  "yellow",  "white" }},   // long grass
  {    12,    "rocks"               , { "blue",   "magenta", "orange", "yellow",  "green",   "aqua",    "white" }},   // rocks
  {    17,    "gas station"         , { "red",    "yellow",  "green",  "aqua",    "blue",    "magenta", "white" }},   // gas pump
  {    28,    "tree"                , { "green",  "aqua",    "blue",   "magenta", "red",     "yellow",  "white" }},   // tree
  {    30,    "campsite"            , { "yellow", "green",   "aqua",   "blue",    "magenta", "red",     "white" }},   // tent
  {    37,    "skull and crossbones", { "blue",   "magenta", "orange", "yellow",  "green",   "aqua",    "white" }},   // skull and crossbones
  {    40,    "dive flag"           , { "red",    "yellow",  "green",  "aqua",    "blue",    "magenta", "white" }},   // diveflag
  {    42,    "anchor"              , { "blue",   "magenta", "orange", "yellow",  "green",   "aqua",    "white" }},   // anchor
  {    44,    "boat ramp"           , { "red",    "yellow",  "green",  "aqua",    "blue",    "magenta", "white" }},   // boatramp
  {    48,    "pier"                , { "blue",   "magenta", "orange", "yellow",  "green",   "aqua",    "white" }},   // pier

  // END OF ICON MAPPING
  {    -1,    nullptr               , { nullptr,  nullptr,   nullptr,  nullptr,   nullptr,   nullptr,  nullptr  }}
};

static gbfile*        file_in;
static gbfile*        file_out;
static short_handle   mkshort_handle;

static route_head*    trk_head;
static route_head*    rte_head;

static int            waypt_uid;
static int            route_uid;
static int            trail_uid;

static char*          opt_ignoreicons;
static char*          opt_writeasicons;
static char*          opt_seg_break;
static char*          opt_wversion;
static char*          opt_title;
static char*          opt_content_descr;
static char*          opt_serialnum;
static int            opt_serialnum_i;

static Waypoint**     waypt_table;
static int            waypt_table_sz;
static int            waypt_table_ct;

/* from waypt.c, we need to iterate over waypoints when extracting routes */
extern WaypointList* global_waypoint_list;

static unsigned short waypt_out_count;
static int            trail_count, lowrance_route_count;
static int            trail_point_count;
static char           continuous = 1;
static short          num_section_points;
static char*          merge;
static int            reading_version;
static int            rstream_version;
static int            writing_version;
static QTextCodec*    utf16le_codec{nullptr};

#define MYNAME "Lowrance USR"

#define MAXUSRSTRINGSIZE        256
#define SEMIMINOR               6356752.3142
//#define DEGREESTORADIANS        0.017453292
//#define DEGREESTORADIANS        0.01745329252   // increased accuracy in calculations
#define DEGREESTORADIANS        (M_PI/180.0)
#define MAX_TRAIL_POINTS        9999
#define UNKNOWN_USR_ALTITUDE    METERS_TO_FEET(-10000) /* -10000ft is how the unit stores unknown */

/* Jan 1, 2000 00:00:00 */
const time_t base_time_secs = 946706400;

typedef struct {
  format_specific_data fs;
  uint uid_unit;
  uint uid_unit2;
  int uid_seq_low;
  int uid_seq_high;
  uint UUID1;
  uint UUID2;
  uint UUID3;
  uint UUID4;
  int  flags;
  int  color;
  const char *color_desc;
  int  icon_num;
  float depth;
} lowranceusr4_fsdata;

class Lowranceusr4Timestamp {
  public:
  unsigned int julian_day_number;
  unsigned int milliseconds;

  Lowranceusr4Timestamp(unsigned int jd, unsigned int ms) : julian_day_number{jd}, milliseconds{ms} {}
};


/* fsdata manipulation functions */
static void
lowranceusr4_free_fsdata(void* fsdata)
{
  xfree(fsdata);
}

static void
lowranceusr4_copy_fsdata(lowranceusr4_fsdata** dest, lowranceusr4_fsdata* src)
{
  *dest = (lowranceusr4_fsdata*)xmalloc(sizeof(*src));
  ** dest = *src;
  (*dest)->fs.next = nullptr;
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

static Waypoint*
lowranceusr4_find_waypt(uint uid_unit, int uid_seq_low, int uid_seq_high)
{
  lowranceusr4_fsdata* fs = nullptr;

  // Iterate with waypt_disp_all?
  foreach (Waypoint* waypointp, *global_waypoint_list) {
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
  lowranceusr4_fsdata* fs = nullptr;

  // Iterate with waypt_disp_all?
  foreach (Waypoint* waypointp, *global_waypoint_list) {
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

//  Starting with USR 4, adopted a UTF-16 character string format
static QString
lowranceusr4_readstr(gbfile* file, int bytes_per_char)
{
  QString retval;

  const int len = gbfgetint32(file); /* bytes */
  if (len < 0) {
    /* seems len=-1 means no string */
    retval = QString();
  } else if (len == 0) {
    /* len == 0 means an empty string */
    retval = QString("");
  } else {
    QByteArray buf;
    buf.resize(len);
    int bytesread = gbfread(buf.data(), 1, len, file);
    buf.truncate(bytesread);
    buf.replace((char)0x01, '*');  // IWay 350C puts 0x01 for the accented o in the street name of the Montreal Holiday Inn.
    if (bytes_per_char == 1) {
      retval = QString(buf);
    } else {
      retval = utf16le_codec->toUnicode(buf);
    }
    if (len > bytesread) {
      (void) gbfseek(file, (len - bytesread), SEEK_CUR);
    }
  }

  return retval;
}

//  Starting with USR 4, adopted a UTF-16 character string format
static void
lowranceusr4_writestr(const QString& buf, gbfile* file, int bytes_per_char)
{
  QByteArray qba;
  if (bytes_per_char == 1) {
    qba = buf.toUtf8();
  } else {
    qba = utf16le_codec->fromUnicode(buf);
  }
  int len = qba.size();
  gbfputint32(len, file_out);
  gbfwrite(qba.constData(), 1, len, file);
}

static gpsbabel::DateTime
lowranceusr4_get_timestamp(unsigned int jd_number, unsigned int msecs)
{
  QDateTime qdt = QDateTime(QDate::fromJulianDay(jd_number), QTime(0, 0, 0), Qt::UTC).addMSecs(msecs);
  return qdt;
}

static Lowranceusr4Timestamp
lowranceusr4_jd_from_timestamp(gpsbabel::DateTime qdt)
{
  QDateTime jdt = qdt.toUTC().addSecs(-60 * 60 * 12);
  unsigned int jd_number = jdt.date().toJulianDay();
  QTime jd_time = jdt.time();
  unsigned int msecs = (((((jd_time.hour() * 60) + jd_time.minute()) * 60) + jd_time.second()) * 1000) + jd_time.msec();
  return Lowranceusr4Timestamp(jd_number, msecs);
}


const QString
lowranceusr_find_desc_from_icon_number(const int icon)
{
  for (const lowranceusr_icon_mapping_t* i = lowranceusr_icon_value_table; i->icon; i++) {
    if (icon == i->value) {
      return i->icon;
    }
  }

  // Didn't find it in table, default to leave it as the number found
  return QString("icon-%1").arg(icon);
}

static int
lowranceusr_find_icon_number_from_desc(const QString& desc)
{
  if (desc.isNull()) {
    return DEF_ICON;
  }

  /*
   * If we were given a numeric icon number as a description
   * (i.e. 8255), just return that.
   */
  int n = desc.toInt();
  if (n)  {
    return n;
  }

  for (const lowranceusr_icon_mapping_t* i = lowranceusr_icon_value_table; i->icon; i++) {
    if (desc.compare(i->icon,Qt::CaseInsensitive) == 0) {
      return i->value;
    }
  }

  return DEF_ICON;
}

const QString
lowranceusr4_find_desc_from_icon_number(const int icon)
{
  for (const lowranceusr4_icon_mapping_t* i = lowranceusr4_icon_value_table; i->icon; i++) {
    if (icon == i->value) {
      return i->icon;
    }
  }

  // Didn't find it in table, default to leave it as the number found
  return QString("icon-%1").arg(icon);
}

static int
lowranceusr4_find_icon_number_from_desc(const QString& desc)
{
  if (desc.isNull()) {
    return DEF_USR4_ICON;
  }

  /*
   * If we were given a numeric icon number as a description
   * (i.e. 8255), just return that.
   */
  int n = desc.toInt();
  if (n)  {
    return n;
  }

  for (const lowranceusr4_icon_mapping_t* i = lowranceusr4_icon_value_table; i->icon; i++) {
    if (desc.compare(i->icon,Qt::CaseInsensitive) == 0) {
      return i->value;
    }
  }

  return DEF_USR4_ICON;
}

const char *
lowranceusr4_find_color_from_icon_number_plus_color_index(const int icon, const int index)
{
  for (const lowranceusr4_icon_mapping_t* i = lowranceusr4_icon_value_table; i->icon; i++) {
    if (icon == i->value) {
      return i->color[index];
    }
  }

  return nullptr;
}

static int
lowranceusr4_find_index_from_icon_desc_and_color_desc(const QString& icon, const QString& color)
{
  if (icon.isNull()) {
    return DEF_USR4_COLOR;
  }

  /*
   * If we were given a numeric icon number as a description
   * (i.e. 8255), just return DEF_USR4_COLOR.
   */
  int n = icon.toInt();
  if (n)  {
    return DEF_USR4_COLOR;
  }

  for (const lowranceusr4_icon_mapping_t* i = lowranceusr4_icon_value_table; i->icon; i++) {
    if (icon.compare(i->icon,Qt::CaseInsensitive) == 0) {
      // Found ICON, now look for color
      for (int index=0; index<7; index++) {
        if (color.compare(i->color[index],Qt::CaseInsensitive) == 0) 
          return index;
      }
    }
  }

  return DEF_USR4_COLOR;
}

// Combined arguments from previous lowranceusr and lowranceusr4 into single set.
// Use output format specified to determine if args are ignored.
static
arglist_t lowranceusr_args[] = {
  {
    // Ignore Event Marker ICONs in input data
    "ignoreicons", &opt_ignoreicons, "(USR input) Ignore event marker icons on read",
    nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
  },
  {
    // Write Waypoint data as Event Marker ICONs
    "writeasicons", &opt_writeasicons, "(USR output) Treat waypoints as icons on write",
    nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
  },
  {
    "merge", &merge, "(USR output) Merge into one segmented trail",
    nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
  },
  {
    "break", &opt_seg_break, "(USR input) Break segments into separate trails",
    nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
  },
  {
    // Specify the Output USR Version to be generated
    "wversion", &opt_wversion, "(USR output) Write version",
    "2", ARGTYPE_INT, "2", "4", nullptr
  },
  {
    // Only used if Write Version is 4/5/6
    "title", &opt_title, "(USR output) Output file title string",
    "", ARGTYPE_STRING, ARG_NOMINMAX, nullptr
  },
  {
    // Only used if Write Version is 4/5/6
    "serialnum", &opt_serialnum, "(USR output) Device serial number",
    "0", ARGTYPE_INT, ARG_NOMINMAX, nullptr
  },
  {
    // Only used if Write Version is 4/5/6
    "description", &opt_content_descr, "(USR output) Output file content description",
    "", ARGTYPE_STRING, ARG_NOMINMAX, nullptr
  },

  ARG_TERMINATOR
};

static void
rd_init(const QString& fname)
{
  file_in = gbfopen_le(fname, "rb", MYNAME);
  utf16le_codec = QTextCodec::codecForName("UTF-16LE");
}

static void
rd_deinit()
{
  gbfclose(file_in);
  utf16le_codec = nullptr;
}

static void
wr_init(const QString& fname)
{
  file_out = gbfopen_le(fname, "wb", MYNAME);
  mkshort_handle = mkshort_new_handle();
  waypt_out_count = 0;
  writing_version = atoi(opt_wversion);
  if ((writing_version < 2) || (writing_version > 4)) {
    fatal(MYNAME " wversion value %s is not supported !!\n", opt_wversion);
  }
  utf16le_codec = QTextCodec::codecForName("UTF-16LE");
}

static void
wr_deinit()
{
  gbfclose(file_out);
  mkshort_del_handle(&mkshort_handle);
  utf16le_codec = nullptr;
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


static void
lowranceusr_parse_waypt(Waypoint* wpt_tmp, int object_num_present)
{
  /* Object num */
  if (object_num_present) {
    short object_num = gbfgetint16(file_in);
    if (global_opts.debug_level == 99) {
      printf(MYNAME " parse_waypt: %5d", object_num);
    }
  }

  wpt_tmp->latitude = lat_mm_to_deg(gbfgetint32(file_in));
  wpt_tmp->longitude = lon_mm_to_deg(gbfgetint32(file_in));
  /* 
   * From available data, looks like stream version = 0 used float to store for altitude in feet
   * and version 1 used int32.  If that is found to not be the case may need to add flag for input.
   * Only have a small sample size.
   */
  if (rstream_version == 0) {
    float read_alt = gbfgetflt(file_in);
    char buf[32];
    sprintf(buf, "%15.10f", read_alt);
    // test for both cases to avoid compiler differences
    if ((strcmp(buf, "-nan") == 0) || (strcmp(buf, "nan") == 0)) {
      wpt_tmp->altitude = unknown_alt;
    } else if (METERS_TO_FEET(wpt_tmp->altitude) <= -10000) {
      wpt_tmp->altitude = unknown_alt;
    } else {
      wpt_tmp->altitude = FEET_TO_METERS(read_alt);
    }
  } else {
    wpt_tmp->altitude = FEET_TO_METERS(gbfgetint32(file_in));
    if (METERS_TO_FEET(wpt_tmp->altitude) <= -10000) {
      wpt_tmp->altitude = unknown_alt;
    }
  }

  /* Waypoint name; input is 1 byte per char */
  QString name = lowranceusr4_readstr(file_in, 1);
  if (!name.isEmpty()) {
    wpt_tmp->shortname = name;
  }

  if (global_opts.debug_level > 1) {
    if (global_opts.debug_level == 99) {
      if (wpt_tmp->shortname.length() > 16) {
        printf(" %.13s...", qPrintable(wpt_tmp->shortname));
      } else {
        printf(" %16.16s", qPrintable(wpt_tmp->shortname));
      }
      printf(" %+15.10f %+15.10f", wpt_tmp->latitude, wpt_tmp->longitude);
      if (wpt_tmp->altitude == unknown_alt) {
          printf(" %13s", "UNKNOWN ALT");
      } else {
          printf(" %5d %7.1f", (int)METERS_TO_FEET(wpt_tmp->altitude), wpt_tmp->altitude);
      }
    } else {
      printf(MYNAME " parse_waypt: Waypt name = '%s' Lat = %+f Lon = %+f alt = ",
             qPrintable(wpt_tmp->shortname), wpt_tmp->latitude, wpt_tmp->longitude);
      if (wpt_tmp->altitude == unknown_alt) {
        printf("UNKNOWN ALT\n");
      } else {
        printf("%d (%f)\n", (int)METERS_TO_FEET(wpt_tmp->altitude), wpt_tmp->altitude);
      }
    }
  }

  /* Description; input is 1 byte per char */
  QString description = lowranceusr4_readstr(file_in, 1);
  if (!description.isEmpty()) {
    wpt_tmp->description = description;
  }

  /* Input is the number of seconds since Jan. 1, 2000 */
  time_t waypt_time = gbfgetint32(file_in);
  if (waypt_time) {
    /* Waypoint needs the number of seconds since UNIX Epoch (Jan 1, 1970) */
    wpt_tmp->SetCreationTime(waypt_time += base_time_secs);
  }

  if (global_opts.debug_level > 2) {
    if (global_opts.debug_level == 99) {
      printf(" '%s'", qPrintable(wpt_tmp->GetCreationTime().toString("yyyy/MM/dd hh:mm:ss")));
    } else {
      printf(MYNAME " parse_waypt: creation time '%s', waypt_time %ld\n",
        qPrintable(wpt_tmp->GetCreationTime().toString("yyyy/MM/dd hh:mm:ss")), waypt_time);
    }
  }

  /* Symbol ID */
  int icon_number = gbfgetint32(file_in);
  if (icon_number == 0) {
    // Assume have extra field generated by HOOK2 in USR 2 & 3 formats, read again
    icon_number = gbfgetint32(file_in);
  }
  if (global_opts.debug_level == 99) {
    printf(" %08x (%d)", icon_number, icon_number);
  }
  wpt_tmp->icon_descr = lowranceusr_find_desc_from_icon_number(icon_number);

  /* Waypoint Type (USER, TEMPORARY, POINT_OF_INTEREST) */
  short waypt_type = gbfgetint16(file_in);
  if (global_opts.debug_level > 2) {
    if (global_opts.debug_level == 99) {
      printf(" %04x (%d)", (int)waypt_type, (int)waypt_type);
    } else {
      printf(MYNAME " parse_waypt: waypt_type = %d\n",waypt_type);
    }
  }

  // Version 3 has a depth field here.
  if (reading_version == 3) {
    float depth_feet = gbfgetflt(file_in);
    if (std::abs(depth_feet - 99999.0)  > .1) {
      WAYPT_SET(wpt_tmp, depth, FEET_TO_METERS(depth_feet));
      if (global_opts.debug_level == 99) {
        printf("   %10.1f", depth_feet);
      }
    } else {
      if (global_opts.debug_level == 99) {
        printf("      UNKNOWN");
      }
    }
  }

  if (global_opts.debug_level == 99) {
    printf("\n");
  }
}

static void
lowranceusr4_parse_waypt(Waypoint* wpt_tmp)
{
  int waypoint_version;

  lowranceusr4_fsdata* fsdata = lowranceusr4_alloc_fsdata();
  fs_chain_add(&(wpt_tmp->fs), (format_specific_data*) fsdata);

  if (reading_version > 4) {
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
  QString name = lowranceusr4_readstr(file_in, 2);
  if (!name.isEmpty()) {
    wpt_tmp->shortname = name;
  }

  if (reading_version > 4) {
    /* USR 5 and 6 have a second Unit Number captured in Waypoints */
    fsdata->uid_unit2 = gbfgetint32(file_in);
  }

  /* Long/Lat */
  wpt_tmp->longitude = lon_mm_to_deg(gbfgetint32(file_in));
  wpt_tmp->latitude = lat_mm_to_deg(gbfgetint32(file_in));

  /* Flags, discard for now */
  fsdata->flags = gbfgetint32(file_in);

  /* Icon ID */
  fsdata->icon_num = gbfgetint16(file_in);
  wpt_tmp->icon_descr = lowranceusr4_find_desc_from_icon_number(fsdata->icon_num);

  /* Color ID */
  fsdata->color = gbfgetint16(file_in);
  fsdata->color_desc = lowranceusr4_find_color_from_icon_number_plus_color_index(fsdata->icon_num, fsdata->color);

  /* Waypoint descr; input is 2 bytes per char, we convert to 1 */
  QString desc = lowranceusr4_readstr(file_in, 2);
  if (!desc.isEmpty()) {
    wpt_tmp->description = desc;
  }

  /* Alarm radius; XXX: I'm not sure what the units are here,
     assuming meters but may be feet? */
  WAYPT_SET(wpt_tmp, proximity, gbfgetflt(file_in));

  /* Creation date/time */
  /* The date is a Julian day number, and the time is a unix timestamp. */
  uint create_date = gbfgetint32(file_in);
  uint create_time = gbfgetint32(file_in);

  // Julian date 2440487 is 1/1/1970.  If that's the date we're working
  // with, as a practical matter, we have no date, so don't even compute
  // or set it.
  if (create_date > 2440587) {
    wpt_tmp->SetCreationTime(lowranceusr4_get_timestamp(create_date, create_time));
  }

  /* Unused byte */
  char unused_byte = gbfgetc(file_in);

  /* Depth in feet */
  fsdata->depth = gbfgetflt(file_in);

  /* Loran data, discard for now */
  int loran_GRI = gbfgetint32(file_in);
  int loran_Tda = gbfgetint32(file_in);
  int loran_Tdb = gbfgetint32(file_in);

  if (global_opts.debug_level > 1) {
    if (global_opts.debug_level == 99) {
      printf(MYNAME " parse_waypoints: ");
      if (reading_version > 4) {
        printf("%08x %08x %08x %08x ",
               fsdata->UUID1, fsdata->UUID2, fsdata->UUID3, fsdata->UUID4);
      }
      printf(" %10u %8d %8d %8d %6d",
             fsdata->uid_unit, fsdata->uid_seq_low, fsdata->uid_seq_high,
             waypoint_version, name.length());
      if (name.length() > 16) {
        printf(" %13.13s...", qPrintable(name));
      } else {
        printf(" %16.16s", qPrintable(name));
      }
      if (reading_version > 4) {
        printf("  %10u ", fsdata->uid_unit2);
      }
      printf(" %+15.10f %+15.10f", wpt_tmp->longitude, wpt_tmp->latitude);
      printf(" %08x %4d %4d %7s", fsdata->flags, fsdata->icon_num, fsdata->color,
        (fsdata->color_desc == nullptr ? "unk" : qPrintable(fsdata->color_desc)));
      if (desc.length() > 16) {
        printf(" %6d %.13s...", desc.length(), qPrintable(desc));
      } else {
        printf(" %6d %16s", desc.length(), qPrintable(desc));
      }
      printf(" '%s'", qPrintable(wpt_tmp->GetCreationTime().toString("yyyy/MM/dd hh:mm:ss")));
      printf(" %08x %8.3f %08x %08x %08x\n",
             unused_byte, fsdata->depth, loran_GRI, loran_Tda, loran_Tdb);
    } else {
      printf(MYNAME " parse_waypoints: version = %d, name = %s, uid_unit = %u, "
             "uid_seq_low = %d, uid_seq_high = %d, lat = %+.10f, lon = %+.10f, depth = %f\n",
             waypoint_version, qPrintable(wpt_tmp->shortname), fsdata->uid_unit,
             fsdata->uid_seq_low, fsdata->uid_seq_high,
             wpt_tmp->longitude, wpt_tmp->latitude, fsdata->depth);
    }
  }
}


static void
lowranceusr_parse_waypts()
{
  int NumWaypoints;

  if (reading_version < 4) {
    /* USR versions 2 & 3 have a 16 bit count */
    NumWaypoints = gbfgetint16(file_in);
  } else {
    /* Starting with USR version 4 have 32 bit count */
    NumWaypoints = gbfgetint32(file_in);
  }

  if (global_opts.debug_level >= 1) {
    printf(MYNAME " parse_waypts: Num Waypoints = %d\n", NumWaypoints);
  }

  if (global_opts.debug_level == 99) {
    if (reading_version > 3) {
      printf(MYNAME " parse_waypts: ");
      if (reading_version > 4) {
        printf("Universal ID                        ");
      }
      printf("              Sequence Number  Stream  Waypoint\n");

      printf(MYNAME " parse_waypoints: ");
      if (reading_version > 4) {
        printf("    ID1      ID2      ID3      ID4  ");
      }
      printf("Unit Number     Low      High  Version Length Name            ");
      if (reading_version > 4) {
        printf(" Unit Number2");
      }
      printf(" Latitude        Longitude       Flags    ICON Color        Length Description     ");
      printf(" Date       Time  Unknown  Depth    LoranGRI LoranTda LoranTdb\n");

      printf(MYNAME " parse_waypoints: ");
      if (reading_version > 4) {
        printf("-------- -------- -------- -------- ");
      }
      printf("----------- -------- -------- -------- ------ ----------------");
      if (reading_version > 4) {
        printf(" ------------");
      }
      printf(" --------------- --------------- -------- ---- ------------ ------ ----------------");
      printf(" ---------- ----- -------- -------- -------- -------- --------\n");
    } else {
      printf(MYNAME " parse_waypts: Number Name            Longitude       Latitude       Altitude       Time            ");
      printf(" ICON ID (dec)    Flag (dec)");
      if (reading_version == 3) {
        printf(" Depth (ft)");
      }
      printf("\n");
      printf(MYNAME " parse_waypts: ------ --------------- --------------- -------------- -------------- ----------------");
      printf(" ---------------- ----------");
      if (reading_version == 3) {
        printf(" ----------");
      }
      printf("\n");
    }
  }

  for (int i = 0; i < NumWaypoints && !gbfeof(file_in); i++) {
    Waypoint* wpt_tmp = new Waypoint;

    switch (reading_version) {
    case 2:
    case 3:
      lowranceusr_parse_waypt(wpt_tmp, 1);  /* Indicate object number present */
      break;
    case 4:
    case 5:
    case 6:
      lowranceusr4_parse_waypt(wpt_tmp);
      break;
    }
    waypt_add(wpt_tmp);
  }
}

static void
lowranceusr_parse_route()
{
  /* route name */
  QString name = lowranceusr4_readstr(file_in, 1);
  if (!name.isEmpty()) {
    rte_head->rte_name = name;
  }

  /* num Legs */
  short num_legs = gbfgetint16(file_in);

  if (global_opts.debug_level > 1) {
    printf(MYNAME " parse_route: Route '%s', Num Legs = %d", qPrintable(name), num_legs);
  }

  /* route reversed */
  char reversed = gbfgetc(file_in);
  if (global_opts.debug_level > 1) {
    printf(", reversed '%x' - %s\n", reversed, (reversed ? "Yes" : "No"));
  }

  if (global_opts.debug_level == 99) {
    printf(MYNAME " parse_route:  Name            Longitude        Latitude       Altitude      Time             Unknown  ICON ID (dec)    Flag (dec) Depth (ft)\n");
    printf(MYNAME " parse_route:  --------------- ---------------  -------------- ------------- ---------------- -------- ---------------- ---------- ----------\n");
  }

  /* waypoints */
  for (int j = 0; j < num_legs; j++) {
    Waypoint* wpt_tmp = new Waypoint;
    if (global_opts.debug_level == 99) {
      printf(MYNAME " parse_route:");
    }
    lowranceusr_parse_waypt(wpt_tmp, 0); /* Indicate object number missing */
    route_add_wpt(rte_head, wpt_tmp);
  }
}

static void
lowranceusr4_parse_route()
{
  int route_version;
  int UUID1, UUID2, UUID3, UUID4;

  lowranceusr4_fsdata* fsdata = lowranceusr4_alloc_fsdata();
  fs_chain_add(&(rte_head->fs), (format_specific_data*) fsdata);

  if (reading_version >= 5) {
    /* Routes have Universal IDs */
    UUID1 = gbfgetint32(file_in);
    UUID2 = gbfgetint32(file_in);
    UUID3 = gbfgetint32(file_in);
    UUID4 = gbfgetint32(file_in);
  }

  /* UID unit number */
  fsdata->uid_unit = gbfgetint32(file_in);
  if (global_opts.debug_level > 1) {
    printf(MYNAME " parse_route: Unit %u (0x%08x)\n", fsdata->uid_unit, fsdata->uid_unit);
  }

  /* 64-bit UID sequence number */
  fsdata->uid_seq_low = gbfgetint32(file_in);
  fsdata->uid_seq_high = gbfgetint32(file_in);

  /* Route stream version number */
  route_version = gbfgetint16(file_in);
  if (global_opts.debug_level > 1) {
    printf(MYNAME " parse_route: Version = %d\n", route_version);
  }

  /* Route name; input is 2 bytes per char, we convert to 1 */
  QString buff = lowranceusr4_readstr(file_in, 2);
  if (!buff.isEmpty()) {
    rte_head->rte_name = buff;
  }

  if (reading_version >= 5) {
    /* USR Version 5 and greater include unit ID in each route */
    gbfgetint32(file_in);
  }

  int num_legs = gbfgetint32(file_in);

  if (global_opts.debug_level > 1) {
    if (reading_version >= 5) {
      printf(MYNAME " parse_route: route '%s' (UUID %08x %08x %8x %08x) has %d legs\n",
             qPrintable(rte_head->rte_name), UUID1, UUID2, UUID3, UUID4, num_legs);
    } else {
      printf(MYNAME " parse_route: route '%s' has %d legs\n",
             qPrintable(rte_head->rte_name), num_legs);
    }
  }

  if (reading_version <= 4) {
    /* Use UID based sequence numbers for route */
    for (int j = 0; j < num_legs; ++j) {
      uint uid_unit = gbfgetint32(file_in);
      uint uid_seq_low = gbfgetint32(file_in);
      uint uid_seq_high = gbfgetint32(file_in);
      Waypoint* wpt_tmp = lowranceusr4_find_waypt(uid_unit, uid_seq_low, uid_seq_high);
      if (wpt_tmp) {
        if (global_opts.debug_level >= 2) {
          printf(MYNAME " parse_route: added leg #%d routepoint %s (%+.10f, %+.10f)\n",
                 j, qPrintable(wpt_tmp->shortname), wpt_tmp->longitude, wpt_tmp->latitude);
        }
        route_add_wpt(rte_head, new Waypoint(*wpt_tmp));
      }
    }
  } else {
    /* Use global sequence number for route */
    for (int j = 0; j < num_legs; ++j) {
      UUID1 = gbfgetint32(file_in);
      UUID2 = gbfgetint32(file_in);
      UUID3 = gbfgetint32(file_in);
      UUID4 = gbfgetint32(file_in);
      Waypoint* wpt_tmp = lowranceusr4_find_global_waypt(UUID1, UUID2, UUID3, UUID4);
      if (wpt_tmp) {
        if (global_opts.debug_level >= 2) {
          printf(MYNAME " parse_route: added leg #%d routepoint %s (%+.10f, %+.10f)\n",
                 j, qPrintable(wpt_tmp->shortname), wpt_tmp->longitude, wpt_tmp->latitude);
        }
        route_add_wpt(rte_head, new Waypoint(*wpt_tmp));
      }
    }
  }

  if (reading_version > 4) {
    /* USR Version 5 or greater, more mystery data, ignore for now */
    gbfgetint32(file_in);
    gbfgetint32(file_in);
    gbfgetc(file_in);
  }

  /* Mystery byte, discard */
  if (global_opts.debug_level == 99) {
    printf(MYNAME " parse_route: end of route %02x\n", gbfgetc(file_in));
  } else {
    gbfgetc(file_in);
  }
}

static void
lowranceusr_parse_routes()
{
  short int num_routes;

  if (reading_version < 4) {
    /* USR versions 2 & 3 have a 16 bit count */
    num_routes = gbfgetint16(file_in);
  } else {
    /* Starting with USR version 4 have 32 bit count */
    num_routes = gbfgetint32(file_in);
  }

  if (global_opts.debug_level >= 1) {
    printf(MYNAME " parse_routes: Num Routes = %d\n", num_routes);
  }

  for (int i = 0; i < num_routes; i++) {
    rte_head = route_head_alloc();
    route_add_head(rte_head);
    rte_head->rte_num = i+1;

    if (reading_version < 4) {
      lowranceusr_parse_route();
    } else {
      lowranceusr4_parse_route();
    }
  }
}


/*
 * Icons are automatically converted to waypoints unless
 * option of ignoreicons is used
 */
static void
lowranceusr_parse_icons()
{
  short int num_icons = gbfgetint16(file_in);

  if (global_opts.debug_level >= 1) {
    printf(MYNAME " parse_icons: Num Event Marker Icons = %d\n", num_icons);
  }

  for (int i = 0; i < num_icons && !gbfeof(file_in); i++) {
    double latitude    = lat_mm_to_deg(gbfgetint32(file_in));
    double longitude   = lon_mm_to_deg(gbfgetint32(file_in));
    int    icon_number = gbfgetint32(file_in);

    if (opt_ignoreicons == nullptr) {          /* Option not specified if NULL */
      Waypoint* wpt_tmp = new Waypoint;

      /* position coord lat & long */
      wpt_tmp->latitude = latitude;
      wpt_tmp->longitude = longitude;
      wpt_tmp->altitude = unknown_alt;

      /* shortname */
      QString name = "Event Marker " + QString::number(i+1);
      wpt_tmp->shortname = name;

      /* symbol */
      wpt_tmp->icon_descr = lowranceusr_find_desc_from_icon_number(icon_number);

      if (global_opts.debug_level > 1) {
        printf(MYNAME " parse_icons: '%s' %d %16.16s %+15.10f %+15.10f\n",
               qPrintable(wpt_tmp->shortname), icon_number, qPrintable(wpt_tmp->icon_descr), wpt_tmp->latitude, wpt_tmp->longitude);
      }
      waypt_add(wpt_tmp);
    }
  }
}

static void
lowranceusr_parse_trail(int* trail_num)
{
  /* Trail name; input is 1 byte per char */
  QString name = lowranceusr4_readstr(file_in, 1);
  if (!name.isEmpty()) {
    trk_head->rte_name = name;
  }

  if (global_opts.debug_level > 1) {
    printf(MYNAME " parse_trails: Trail '%s'\n", qPrintable(trk_head->rte_name));
  }

  /* visible */
  char visible = gbfgetc(file_in);

  if (global_opts.debug_level > 1) {
    printf(MYNAME " parse_trails: Visible '%x' - %s\n", visible, (visible ? "Yes" : "No"));
  }

  /* num trail points */
  short num_trail_points = gbfgetint16(file_in);

  if (global_opts.debug_level > 1) {
    printf(MYNAME " parse_trails: Num Trail Points = %d\n", num_trail_points);
  }

  /* max trail size */
  int itmp = gbfgetint16(file_in);

  if (global_opts.debug_level > 1) {
    printf(MYNAME " parse_trails: Max Trail size = %d\n", itmp);
  }

  if (num_trail_points) {

    while (num_trail_points && !gbfeof(file_in)) {
      /* num section points */
      short int num_section_points = gbfgetint16(file_in);

      if (global_opts.debug_level > 1) {
        printf(MYNAME " parse_trails: Num Section Points = %d\n", num_section_points);
      }

      for (int j = 0; j < num_section_points && !gbfeof(file_in); j++, num_trail_points--) {
        Waypoint* wpt_tmp = new Waypoint;
        wpt_tmp->latitude = lat_mm_to_deg(gbfgetint32(file_in));
        wpt_tmp->longitude = lon_mm_to_deg(gbfgetint32(file_in));

        /* continuous */
        char continuous = gbfgetc(file_in);
        if (!continuous && opt_seg_break && j) {
          /* option to break trails into segments was specified */
          auto trk_tmp = route_head_alloc();
          trk_tmp->rte_num = ++(*trail_num);
          trk_tmp->rte_name = trk_head->rte_name;
          track_add_head(trk_tmp);
          trk_head = trk_tmp;
        }

        /* Track Point */
        track_add_wpt(trk_head, wpt_tmp);

        if (global_opts.debug_level > 2) {
          printf(MYNAME " parse_trails: Trail pt lat %f lon %f\n", wpt_tmp->latitude, wpt_tmp->longitude);
        }
      }
    }
  } else {
    /* remove the trail since it's empty */
    track_del_head(trk_head);
  }
}

static void
lowranceusr4_parse_trail(int* trail_num)
{
  int trail_version;
  int trail_color;
  int trail_flags;

  lowranceusr4_fsdata* fsdata = lowranceusr4_alloc_fsdata();
  fs_chain_add(&(trk_head->fs), (format_specific_data*) fsdata);

  /* UID unit number */
  fsdata->uid_unit = gbfgetint32(file_in);

  /* 64-bit UID sequence number */
  fsdata->uid_seq_low = gbfgetint32(file_in);
  fsdata->uid_seq_high = gbfgetint32(file_in);

  /* Trail stream version number */
  trail_version = gbfgetint16(file_in);
  if (global_opts.debug_level == 99) {
    printf(MYNAME " parse_trails: trail Version %d\n", trail_version);
  }
  if ((trail_version < 3) || (trail_version > 5)) {
    fatal(MYNAME " trail version %d not supported!!", trail_version);
  }

  /* Trail name; input is 2 bytes per char, we convert to 1 */
  QString name = lowranceusr4_readstr(file_in, 2);
  if (!name.isEmpty()) {
    trk_head->rte_name = name;
  }
  if (global_opts.debug_level >= 2) {
      printf(MYNAME " parse_trails: Trail '%s'\n", qPrintable(trk_head->rte_name));
  }

  /* Flags, discard for now */
  trail_flags = gbfgetint32(file_in);

  /* Color ID, discard for now */
  trail_color = gbfgetint32(file_in);

  /* Comment/description; input is 2 bytes per char, we convert to 1 */
  QString desc = lowranceusr4_readstr(file_in, 2);
  if (!desc.isEmpty()) {
    trk_head->rte_desc = desc;
  }
  if (global_opts.debug_level == 99) {
    printf(MYNAME " parse_trails: Comment '%s'\n", qPrintable(desc));
  }

  /* Creation date/time, discard for now */
  /* The date is a Julian day number, and the time is a unix timestamp. */
  int create_date = gbfgetint32(file_in);
  int create_time = gbfgetint32(file_in);
  if (global_opts.debug_level == 99) {
    QDateTime qdt = lowranceusr4_get_timestamp(create_date, create_time);
    printf(MYNAME " parse_trails: creation date/time = %s\n", qPrintable(qdt.toString("yyyy-MM-dd hh:mm:ss AP")));
  }

  /* Some flag bytes */
  if (global_opts.debug_level == 99) {
    printf (MYNAME " parse_trails: unknown flag bytes %02x %02x %02x\n",
      gbfgetc(file_in), gbfgetc(file_in), gbfgetc(file_in));
  } else {
    /* just discard */
    gbfgetc(file_in);
    gbfgetc(file_in);
    gbfgetc(file_in);
  }

  /* Mysterious attribute "data count" */
  int attr_count = gbfgetint32(file_in);
  if (global_opts.debug_level == 99) {
    printf (MYNAME " parse_trails: attribute count %4d : (", attr_count);
    for (int i=0; i<attr_count; i++) {
      if (trail_version == 5) {
        printf("%08x ", gbfgetint32(file_in));
      } else {
        printf("%02x ", gbfgetc(file_in));
      }
    }
    printf(")\n");
  } else {
    /* just discard */
    for (int i=0; i<attr_count; i++) {
      if (trail_version == 5) {
        gbfgetint32(file_in);
      } else {
        gbfgetc(file_in);
      }
    }
  }

  int num_trail_pts = gbfgetint32(file_in);

  if (global_opts.debug_level >= 2) {
    printf(MYNAME " parse_trails: trail %d name='%s' color=%d flags=%d has %d (%x) trailpoints\n",
           *trail_num, qPrintable(trk_head->rte_name), trail_color, trail_flags, num_trail_pts, num_trail_pts);

    if (global_opts.debug_level == 99) {
      printf(MYNAME " parse_trails: Longitude      Latitude       Flag/Value pairs (01=Speed)\n");
      printf(MYNAME " parse_trails: -------------- -------------- -- -------- -- -------- -- --------\n");
    }
  }
  for (int j = 0; j < num_trail_pts; ++j) {
    Waypoint* wpt_tmp = new Waypoint;

    /* Some unknown bytes */
    gbfgetint16(file_in);
    gbfgetc(file_in);

    /* POSIX timestamp (a.k.a. UNIX Epoch) - seconds since Jan 1, 1970 */
    wpt_tmp->SetCreationTime(QDateTime::fromTime_t(gbfgetint32(file_in)));

    /* Long/Lat */
    wpt_tmp->longitude = gbfgetdbl(file_in) / DEGREESTORADIANS; /* rad to deg */
    wpt_tmp->latitude = gbfgetdbl(file_in) / DEGREESTORADIANS;

    if (global_opts.debug_level >= 2) {
      if (global_opts.debug_level == 99) {
        printf(MYNAME " parse_trails: %+14.9f %+14.9f", wpt_tmp->longitude, wpt_tmp->latitude);
        printf(" '%s'", qPrintable(wpt_tmp->GetCreationTime().toString("yyyy/MM/dd hh:mm:ss")));
      } else {
        printf(MYNAME " parse_trails: added trailpoint %+.9f,%+.9f to trail %s\n",
               wpt_tmp->longitude, wpt_tmp->latitude, qPrintable(trk_head->rte_name));
      }
    }

    track_add_wpt(trk_head, wpt_tmp);

    /* Mysterious per-trailpoint data, toss it for now */
    int M = gbfgetint32(file_in);
    for (int k = 0; k < M; ++k) {
      int flag = gbfgetc(file_in);
      float value = gbfgetflt(file_in);
      if (global_opts.debug_level == 99) {
        printf(" %02x %f", flag, value);
      }
    }

    if (global_opts.debug_level == 99) {
      printf("\n");
    }
  }
}


static void
lowranceusr_parse_trails()
{
  int num_trails;
  int trail_num;

  if (reading_version < 4) {
    /* USR versions 2 & 3 have a 16 bit count */
    num_trails = gbfgetint16(file_in);
  } else {
    /* Starting with USR version 4 have 32 bit count */
    num_trails = gbfgetint32(file_in);
  }

  if (global_opts.debug_level >= 1) {
    printf(MYNAME " parse_trails: Num Trails = %d\n", num_trails);
  }

  for (int i = trail_num = 0; i < num_trails && !gbfeof(file_in); i++) {
    trk_head = route_head_alloc();
    trk_head->rte_num = ++trail_num;
    track_add_head(trk_head);

    if (reading_version < 4) {
      lowranceusr_parse_trail(&trail_num);
    } else {
      lowranceusr4_parse_trail(&trail_num);
    }
  }
}

static void
data_read()
{
  reading_version = gbfgetint16(file_in);
  rstream_version = gbfgetint16(file_in);
  if (global_opts.debug_level >= 1) {
    printf(MYNAME " input_file: USR File Format %d (Version = %d)\n", reading_version, rstream_version);
  }

  if ((reading_version < 2) || (reading_version > 6)) {
    fatal(MYNAME " input file is a USR format that is not supported\n");
  }

  if (reading_version >= 4) {

    /* Starting with USR version 4 have an unknown here */
    int unknown = gbfgetint32(file_in);
    if (global_opts.debug_level >= 1) {
      printf(MYNAME " input_file: Unknown %d (%x)\n", unknown, unknown);
    }

    /* USR files also now contain a file title */
    QString title = lowranceusr4_readstr(file_in, 1);
    if (!title.isEmpty() && global_opts.debug_level >= 1) {
      printf(MYNAME " file title: '%s'\n", qPrintable(title));
    }

    /* AND a date created string */
    QString creation_date = lowranceusr4_readstr(file_in, 1);
    if (!creation_date.isEmpty()  && global_opts.debug_level >= 1) {
      printf(MYNAME " date string: '%s'\n", qPrintable(creation_date));
    }

    /* Creation date/time, discard for now */
    /* The date is a Julian day number, and the time is a unix timestamp. */
    int create_date = gbfgetint32(file_in);
    int create_time = gbfgetint32(file_in);
    if (global_opts.debug_level >= 1) {
      QDateTime qdt = lowranceusr4_get_timestamp(create_date, create_time);
      printf(MYNAME " creation date/time : '%s'\n", qPrintable(qdt.toString("yyyy-MM-dd hh:mm:ss AP")));
    }

    unsigned char byte = gbfgetc(file_in); /* unused, apparently */
    (void) byte;

    /* AND the serial number of the unit that created the file */
    uint serial_num = gbfgetint32(file_in);
    if (global_opts.debug_level >= 1) {
      printf(MYNAME " device serial number: %u\n", serial_num);
    }

    /* AND a comment on the file contents */
    QString comment = lowranceusr4_readstr(file_in, 1);
    if (!comment.isEmpty() && global_opts.debug_level >= 1) {
      printf(MYNAME " content description: '%s'\n", qPrintable(comment));
    }
  }

  lowranceusr_parse_waypts();
  lowranceusr_parse_routes();

  if ((reading_version == 2) || (reading_version == 3)) {
    lowranceusr_parse_icons();      // Event Marker ICONS exists only in USR 2 & 3 format
  }

  lowranceusr_parse_trails();

}

static void
lowranceusr_waypt_disp(const Waypoint* wpt)
{
  int Time, SymbolId, alt;

  int Lat = lat_deg_to_mm(wpt->latitude);
  int Lon = lon_deg_to_mm(wpt->longitude);

  if (wpt->altitude == unknown_alt) {
    alt = UNKNOWN_USR_ALTITUDE;
  } else {
    alt = METERS_TO_FEET(wpt->altitude);
  }

  gbfputint32(Lat, file_out);
  gbfputint32(Lon, file_out);
  gbfputint32(alt, file_out);

  /* Try and make sure we have a name */
// this kind of thing would probably be more readable like
// name = blah.
// if name.isEmpty()
//   name = planB;
// if name.isEmpty()
//   name = planC;
// ...
  QString name;
  if ((wpt->shortname.isEmpty()) || global_opts.synthesize_shortnames) {
    if (!wpt->description.isEmpty() && global_opts.synthesize_shortnames) {
      name = mkshort_from_wpt(mkshort_handle, wpt);
    } else if (!wpt->shortname.isEmpty()) {
      name = wpt->shortname;
    } else if (!wpt->description.isEmpty()) {
      name = wpt->description;
    }
  } else {
    name = wpt->shortname;
  }

  if (global_opts.debug_level > 2) {
    /* print lat/lon/alt on one easily greppable line */
    printf(MYNAME " waypt_disp: Waypt name = '%s' Lat = %+16.10f  Lon = %+16.10f  Alt = %f\n",
           qPrintable(wpt->shortname), wpt->latitude, wpt->longitude, wpt->altitude);
  }

  QByteArray name_qba = name.toLatin1();
  int text_len = name_qba.length();
  if (text_len > MAXUSRSTRINGSIZE) {
    text_len = MAXUSRSTRINGSIZE;
  }
  gbfputint32(text_len, file_out);
  gbfwrite(name_qba.constData(), 1, text_len, file_out);

  if (global_opts.debug_level > 1) {
    printf(MYNAME " waypt_disp: Waypt name = '%s' ", qPrintable(name));
  }

  /**
   * Comments are now used by the iFinder (Expedition C supports them)
   */
  if (wpt->description != wpt->shortname) {
    QString comment = wpt->description;
    text_len = comment.length();
    if (text_len > MAXUSRSTRINGSIZE) {
      text_len = MAXUSRSTRINGSIZE;
    }
    gbfputint32(text_len, file_out);
    gbfwrite(CSTR(comment), 1, text_len, file_out);
  } else {
    text_len = 0;
    gbfputint32(text_len, file_out);
  }

  /* Waypoint creation time stored as seconds since UNIX Epoch (Jan 1, 1970) */
  if ((Time = wpt->creation_time.toTime_t()) > base_time_secs) {
    /* This should always be true */
    /* Lowrance needs it as seconds since Jan 1, 2000 */
    Time -= base_time_secs;
    if (global_opts.debug_level >= 2) {
      printf("creation_time %d, '%s'", (int)Time, qPrintable(wpt->GetCreationTime().toString("yyyy-MM-dd hh:mm:ss")));
    }
  } else {
    /* If false, make sure it is an unknown time value */
    Time = 0;
    if (global_opts.debug_level >= 2) {
      printf("creation_time UNKNOWN");
    }
  }

  gbfputint32(Time, file_out);

  if (get_cache_icon(wpt) && wpt->icon_descr.compare(QLatin1String("Geocache Found")) == 0) {
    SymbolId = lowranceusr_find_icon_number_from_desc(get_cache_icon(wpt));
  } else {
    SymbolId = lowranceusr_find_icon_number_from_desc(wpt->icon_descr);
  }
  /* If the waypoint is archived or disabled, use a "disabled" icon instead. */
  if ((wpt->gc_data->is_archived==status_true) || (wpt->gc_data->is_available==status_false)) {
    SymbolId = lowranceusr_find_icon_number_from_desc(DISABLED_CACHE_TXT);
  }

  gbfputint32(SymbolId, file_out);

  /* USER waypoint type */
  short int WayptType = 0;
  gbfputint16(WayptType, file_out);

  if (writing_version == 3) {
    float depth = WAYPT_HAS(wpt, depth) ?
                  METERS_TO_FEET(wpt->depth) : -99999.0;
    gbfputint32(depth, file_out);
  }

  if (global_opts.debug_level > 1) {
    printf("\n");
  }
}

static void
lowranceusr4_waypt_disp(const Waypoint* wpt)
{
  /* UID unit number */
  if (opt_serialnum_i > 0) {
    gbfputint32(opt_serialnum_i, file_out);  // use option serial number if specified
  } else if (wpt->fs != nullptr) {
    gbfputint32(((lowranceusr4_fsdata*)(wpt->fs))->uid_unit, file_out);  // else use serial number from input if valid
  } else {
    gbfputint32(0, file_out);  // else Write Serial Number = 0
  }

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

  int SymbolId, ColorId;
  if (get_cache_icon(wpt) && wpt->icon_descr.compare(QLatin1String("Geocache Found")) == 0) {
    if(writing_version == 4) {
      SymbolId = lowranceusr4_find_icon_number_from_desc(wpt->icon_descr);
    } else {
      SymbolId = lowranceusr_find_icon_number_from_desc(get_cache_icon(wpt));
    }
    ColorId = 0; // default
  } else {
    SymbolId = lowranceusr4_find_icon_number_from_desc(wpt->icon_descr);
    if (wpt->fs != nullptr) {
      ColorId = lowranceusr4_find_index_from_icon_desc_and_color_desc(wpt->icon_descr, ((lowranceusr4_fsdata*)(wpt->fs))->color_desc);
    } else {
      ColorId = DEF_USR4_COLOR; // default
    }
  }
  /* If the waypoint is archived or disabled, use a "disabled" icon instead. */
  if ((wpt->gc_data->is_archived==status_true) || (wpt->gc_data->is_available==status_false)) {
    SymbolId = lowranceusr_find_icon_number_from_desc(DISABLED_CACHE_TXT);
    ColorId = 0; // default
  }
  gbfputint16(SymbolId, file_out);
  gbfputint16(ColorId, file_out);

  /* Waypt description */
  lowranceusr4_writestr(wpt->description, file_out, 2);

  /* Alarm radius */
  gbfputflt(WAYPT_GET(wpt, proximity, 0.0), file_out);

  /* Creation date/time */
  auto ts = lowranceusr4_jd_from_timestamp(wpt->GetCreationTime());
  gbfputint32(ts.julian_day_number, file_out);
  gbfputint32(ts.milliseconds, file_out);

  /* Unused byte */
  gbfputc(0, file_out);

  /* Depth in feet */
  if (wpt->fs != nullptr) {
    gbfputint32(((lowranceusr4_fsdata*)(wpt->fs))->depth, file_out);
  } else {
    gbfputint32(0, file_out); // zero seems to indicate no depth
  }

  /* Loran data */
  gbfputint32(0xffffffff, file_out);  // indicate Loran not used
  gbfputint32(0, file_out);
  gbfputint32(0, file_out);
}


static void
lowranceusr_waypt_pr(const Waypoint* wpt)
{

  /* our personal waypoint counter */
  gbfputint16(waypt_out_count, file_out);

  if (global_opts.debug_level >= 3) {
    printf(MYNAME " waypt_pr: waypoint #%d\n",waypt_out_count);
  }

  waypt_out_count++;

  lowranceusr_waypt_disp(wpt);
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


/*
 * In Lowrance parlance, an "Icon" is a waypoint but without any
 * kind of a name.  The header count of icons has already been written
 * before we get here, so it's just a matter of spitting out
 * 4 bytes lat
 * 4 bytes long
 * 4 bytes symbol
 */
static void
lowranceusr_write_icon(const Waypoint* wpt)
{
  int latmm = lat_deg_to_mm(wpt->latitude);
  int lonmm = lon_deg_to_mm(wpt->longitude);
  int icon = !wpt->icon_descr.isNull() ?
             lowranceusr_find_icon_number_from_desc(wpt->icon_descr) :
             X_1_ICON;

  gbfputint32(latmm, file_out);
  gbfputint32(lonmm, file_out);
  gbfputint32(icon, file_out);
}

/*
 * Header format:
 *  short num_trails,
 *  int trail_name text length,
 *  char *trail_name,
 *  boolean visible,
 *  short num_trail_points,
 *  short max_trail_size,
 *  short num_section_points
 *      == don't know how many max points per section so
 *      == use num_trail_points for now
 *      == Once this is known then the waypoints ought to be
 *      == broken up into sections
 */

static void
lowranceusr_trail_hdr(const route_head* trk)
{
  QString name;
  char visible=1;

  ++trail_count;
//TODO: This whole function needs to be replaced...
  if (!trk->rte_name.isEmpty()) {
    name = trk->rte_name;
  } else if (!trk->rte_desc.isEmpty()) {
    name = trk->rte_desc;
  } else {
    name = name + QString("Babel %1").arg(trail_count);
  }

  int text_len = name.length();
  if (text_len > MAXUSRSTRINGSIZE) {
    text_len = MAXUSRSTRINGSIZE;
  }
  if (global_opts.debug_level >= 1) {
    printf(MYNAME " trail_hdr: trail name '%s' ", qPrintable(trk->rte_name));
  }
  gbfputint32(text_len, file_out);
  gbfwrite(CSTR(name), 1, text_len, file_out);

  short num_trail_points = (short) trk->rte_waypt_ct;
  short max_trail_size = MAX_TRAIL_POINTS;
  if (num_trail_points > max_trail_size) {
    num_trail_points = max_trail_size;
  }
  num_section_points = num_trail_points;

  if (global_opts.debug_level) {
    printf("num_trail_points = %d ", num_trail_points);
    if (global_opts.debug_level > 1) {
      printf("max_trail_size = %d num_section_points = %d\n", max_trail_size, num_section_points);
    } else {
      printf("\n");
    }
  }

  gbfwrite(&visible, 1, 1, file_out);
  gbfputint16(num_trail_points, file_out);
  gbfputint16(max_trail_size, file_out);
  gbfputint16(num_section_points, file_out);
  trail_point_count=1;
}

static void
lowranceusr_route_hdr(const route_head* rte)
{
  char* name, tmp_name[20];
  char route_reversed=0;

  /* route name */
  //TODO: This whole function needs to be replaced...
  if (!rte->rte_name.isEmpty()) {
    name = xstrdup(rte->rte_name);
  } else if (!rte->rte_desc.isEmpty()) {
    name = xstrdup(rte->rte_desc);
  } else {
    tmp_name[0]='\0';
    snprintf(tmp_name, sizeof(tmp_name), "Babel R%d", ++lowrance_route_count);
    name = xstrdup(tmp_name);
  }
  int text_len = strlen(name);
  if (text_len > MAXUSRSTRINGSIZE) {
    text_len = MAXUSRSTRINGSIZE;
  }
  gbfputint32(text_len, file_out);
  gbfwrite(name, 1, text_len, file_out);
  xfree(name);

  /* num legs */
  short num_legs = (short) rte->rte_waypt_ct;
  gbfputint16(num_legs, file_out);
  gbfwrite(&route_reversed, 1, 1, file_out);

  if (global_opts.debug_level >= 1)
    printf(MYNAME " route_hdr: route name \"%s\" num_legs = %d\n",
           qPrintable(rte->rte_name), num_legs);
}

static void
lowranceusr4_route_hdr(const route_head* rte)
{
  if (global_opts.debug_level >= 1) {
    printf(MYNAME " writing route #%d (%s) with %d waypts\n",
           route_uid, qPrintable(rte->rte_name), rte->rte_waypt_ct);
  }

  /* UID unit number */
  if (opt_serialnum_i > 0) {
    gbfputint32(opt_serialnum_i, file_out);  // use option serial number if specified
  } else if (rte->fs != nullptr) {
    gbfputint32(((lowranceusr4_fsdata*)(rte->fs))->uid_unit, file_out);  // else use serial number from input if valid
  } else {
    gbfputint32(0, file_out);  // else Write Serial Number = 0
  }

  /* 64-bit UID sequence number */
  gbfputint32(route_uid++, file_out);
  gbfputint32(0, file_out);

  /* Route stream version number: seems to be 1 in my data */
  gbfputint16(1, file_out);

  /* Route name */
  lowranceusr4_writestr(rte->rte_name, file_out, 2);

  /* Num Legs */
  gbfputint32(rte->rte_waypt_ct, file_out);
}

static void
lowranceusr4_route_leg_disp(const Waypoint* wpt)
{
  for (int i = 0; i < waypt_table_ct; i++) {
    Waypoint* cmp = waypt_table[i];
    if (cmp->shortname == wpt->shortname) {
      lowranceusr4_fsdata* fsdata = (lowranceusr4_fsdata*)cmp->fs;
      gbfputint32(fsdata->uid_unit, file_out);  // serial number from input if valid
      gbfputint32(i, file_out); // Sequence Low
      gbfputint32(0, file_out); // Sequence High
      if (global_opts.debug_level > 1) {
        printf(MYNAME " wrote route leg with waypt '%s'\n", qPrintable(wpt->shortname));
      }
      break;
    }
  }
}


static void
lowranceusr4_route_trl(const route_head*)
{
  /* Mystery byte */
  gbfputc(0x01, file_out);	// end of Route info ??
}



static void
lowranceusr_trail_disp(const Waypoint* wpt)
{
  if (trail_point_count <= MAX_TRAIL_POINTS) {
    int lat = lat_deg_to_mm(wpt->latitude);
    int lon = lon_deg_to_mm(wpt->longitude);

    if (global_opts.debug_level > 1) {
      printf(MYNAME " trail_disp: Trail point #%d lat = %f long = %f\n",trail_point_count, wpt->latitude, wpt->longitude);
    }

    gbfputint32(lat, file_out);
    gbfputint32(lon, file_out);
    gbfwrite(&continuous, 1, 1, file_out);
    if (!continuous) {
      continuous = 1;
    }
    trail_point_count++;
  }
}

static void
lowranceusr_merge_trail_hdr(const route_head* trk)
{
  char* name, tmp_name[20];

  if (++trail_count == 1) {
    if (!trk->rte_name.isEmpty()) {
      name = xstrdup(trk->rte_name);
    } else if (!trk->rte_desc.isEmpty()) {
      name = xstrdup(trk->rte_desc);
    } else {
      tmp_name[0]='\0';
      snprintf(tmp_name, sizeof(tmp_name), "Babel %d", trail_count);
      name = xstrdup(tmp_name);
    }
    int text_len = strlen(name);
    if (text_len > MAXUSRSTRINGSIZE) {
      text_len = MAXUSRSTRINGSIZE;
    }
    gbfputint32(text_len, file_out);

    if (global_opts.debug_level >= 1) {
      printf(MYNAME " trail_hdr: trail name = %s\n", name);
    }

    gbfwrite(name, 1, text_len, file_out);
  }

  trail_point_count += (short) trk->rte_waypt_ct;
}

static void
lowranceusr_merge_trail_tlr(const route_head*)
{
  if (trail_count == (int)track_count()) {  /* last trail */
    short num_trail_points = trail_point_count;
    short max_trail_size = MAX_TRAIL_POINTS;
    if (num_trail_points > max_trail_size) {
      num_trail_points = max_trail_size;
    }
    num_section_points = num_trail_points;

    if (global_opts.debug_level >= 1)
      printf(MYNAME " merge_trail_tlr: num_trail_points = %d\nmax_trail_size = %d\nnum_section_points = %d\n",
             num_trail_points, max_trail_size, num_section_points);

    const char visible=1;
    gbfwrite(&visible, 1, 1, file_out);
    gbfputint16(num_trail_points, file_out);
    gbfputint16(max_trail_size, file_out);
    gbfputint16(num_section_points, file_out);
  }
}
static void

lowranceusr_merge_trail_hdr_2(const route_head*)
{
  continuous = 0;
}

static void
lowranceusr4_trail_hdr(const route_head* trail)
{
  if (global_opts.debug_level >= 1) {
    printf(MYNAME " writing trail %d (%s) with %d trailpoints\n",
           trail_uid, qPrintable(trail->rte_name), trail->rte_waypt_ct);
  }

  /* UID unit number */
  gbfputint32(opt_serialnum_i, file_out);

  /* 64-bit UID sequence number */
  gbfputint32(trail_uid++, file_out);
  gbfputint32(0, file_out);

  /* Route stream version number: always seems to be 3 in my data */
  gbfputint16(3, file_out);

  /* Track name */
  lowranceusr4_writestr(trail->rte_name, file_out, 2);

  /* Flags: always seems to be 2 in my data */
  gbfputint32(2, file_out);

  /* Color ID */
  gbfputint32(0, file_out);

  /* Comment */
  lowranceusr4_writestr(trail->rte_desc, file_out, 2);

  /* Creation date/time */
  gbfputint32(0, file_out);
  gbfputint32(0, file_out);

  /* Unused byte */
  gbfputc(0, file_out);

  /* Active flag */
  gbfputc(0, file_out);

  /* Visible flag; I'll just assume all trails should be visible for
     now */
  gbfputc(1, file_out);

  /* Mysterious "data count" and "data type" stuff */
  gbfputint32(0, file_out);
  gbfputc(0, file_out);
  gbfputc(0, file_out);
  gbfputc(0, file_out);

  /* Trackpoint count */
  gbfputint32(trail->rte_waypt_ct, file_out);
}

static void
lowranceusr4_trail_disp(const Waypoint* wpt)
{
  /* Some unknown bytes */
  gbfputint16(0, file_out);
  gbfputc(0, file_out);

  /* Timestamp */
  gbfputint32(wpt->GetCreationTime().toTime_t(), file_out);

  /* Long/Lat */
  gbfputdbl(wpt->longitude * DEGREESTORADIANS, file_out);
  gbfputdbl(wpt->latitude * DEGREESTORADIANS, file_out);

  /* Mysterious per-trailpoint data; we'll just say there are "0"
     mystery entries */
  gbfputint32(0, file_out);
}

static void
data_write()
{
  QString buf;
  int len;

  setshort_length(mkshort_handle, 15);

  gbfputint32(writing_version, file_out);

  int NumWaypoints = waypt_count();
  if (global_opts.debug_level >= 1) {
    printf(MYNAME " data_write: Num Waypoints = %d\n", NumWaypoints);
  }

  // If writeasicons option specified then all Waypoints processed are written as
  // Event Marker ICONs so write the number of Waypoints as ZERO but only if
  // USR format 2 or 3
  if ((writing_version == 2) || (writing_version == 3)) {
    if (opt_writeasicons) {
      short zero = 0;
      gbfputint16(zero, file_out);
    } else {
      // USR version 2 and 3 uses 16-bit count
      gbfputint16(NumWaypoints, file_out);
      waypt_disp_all(lowranceusr_waypt_pr);
    }
  } else {
    // Ignore writeasicons option for all other USR versions
    // Before adding the Waypoint data need to add the file header information

    // Only support Version 10 of the DataStream right now
    int DataStreamVersion = 10;
    gbfputint32(DataStreamVersion, file_out);

    /* file title */
    if ((len = strlen(opt_title)) == 0) {
      buf = QString("GPSBabel generated USR data file");
    } else {
      if (len > MAXUSRSTRINGSIZE) {
        opt_title[MAXUSRSTRINGSIZE] = '\000';  // truncate it before copy
      }
      buf = opt_title;
    }
    if (global_opts.debug_level >= 1) {
      printf(MYNAME " data_write: Title = '%s'\n", qPrintable(buf));
    }
    lowranceusr4_writestr(buf, file_out, 1);

    /* date string */
    gpsbabel::DateTime now = current_time().toUTC();
    lowranceusr4_writestr(now.toString("MM/dd/yyyy"), file_out, 1);

    /* creation date/time */
    auto ts = lowranceusr4_jd_from_timestamp(now);
    gbfputint32(ts.julian_day_number, file_out);
    gbfputint32(ts.milliseconds, file_out);

    /* unused byte */
    gbfputc(0, file_out);

    /* device serial number */
    opt_serialnum_i = atoi(opt_serialnum);
    gbfputint32(opt_serialnum_i, file_out);

    /* content description */
    if ((len = strlen(opt_content_descr)) == 0) {
      buf = QString("Waypoints, routes, and trails");
    } else {
      if(len > MAXUSRSTRINGSIZE) {
        opt_content_descr[MAXUSRSTRINGSIZE] = '\000';  // truncate it before copy
      }
      buf = opt_content_descr;
    }
    if (global_opts.debug_level >= 1) {
      printf(MYNAME " data_write: Description = '%s'\n", qPrintable(buf));
    }
    lowranceusr4_writestr(buf, file_out, 1);

    lowranceusr4_write_waypoints();
  }

  /*************************************************************************/
  /*
   * Start ROUTE Element
   */

  /* Original Route support added 6/21/05 */
  int NumRoutes = route_count();
  lowrance_route_count=0;

  if (global_opts.debug_level >= 1) {
    printf(MYNAME " data_write: Num routes = %d\n", NumRoutes);
  }

  if ((writing_version == 2) || (writing_version == 3)) {
    // USR version 2 & 3 use 16-bit count
    gbfputint16(NumRoutes, file_out);
    if (NumRoutes) {
      route_disp_all(lowranceusr_route_hdr, nullptr, lowranceusr_waypt_disp);
    }
  } else {
    // All other USR formats use 32-bit count
    gbfputint32(NumRoutes, file_out);
    if (NumRoutes) {
      route_disp_all(lowranceusr4_route_hdr, lowranceusr4_route_trl, lowranceusr4_route_leg_disp);
    }
  }


  /*
   * End ROUTE Element
   */
  /*************************************************************************/

  /*************************************************************************/
  /*
   * Start EVENT MARKER ICON Element
   */

  if ((writing_version == 2) || (writing_version == 3)) {
    // Only USR versions 2 and 3 supprt Event Marker ICONs
    // Ignore for all other USR versions
    if (NumWaypoints && opt_writeasicons) {
      gbfputint16(NumWaypoints, file_out);
      waypt_disp_all(lowranceusr_write_icon);
    } else {
      short NumIcons = 0;
      gbfputint16(NumIcons, file_out);
    }
  }

  /*
   * End EVENT MARKER ICON Element
   */
  /*************************************************************************/

  /* Trail support added 6/21/05 */
  short int NumTrails = track_count();

  if (NumTrails && merge) {
    NumTrails = 1;
    if ((writing_version == 2) || (writing_version == 3)) {
      // USR version 2 & 3 use 16-bit count
      gbfputint16(NumTrails, file_out);
    } else {
      // All other USR formats use 32-bit count
      gbfputint32(NumTrails, file_out);
    }

    if ((writing_version == 2) || (writing_version == 3)) {
      trail_point_count = 0;
      trail_count = 0;
      /* count the number of total trail points */
      track_disp_all(lowranceusr_merge_trail_hdr, lowranceusr_merge_trail_tlr, nullptr);
      /* write out the new trail header */
      trail_point_count = 0;
      track_disp_all(lowranceusr_merge_trail_hdr_2, nullptr, lowranceusr_trail_disp);
    } else {
      /* MERGE NEEDS SOME MORE WORK */
      fatal(MYNAME " output file USR %d format is not supported with merge option\n", writing_version);
    }

  } else {
    if (global_opts.debug_level >= 1) {
      printf(MYNAME " data_write: Num trails = %d\n", NumTrails);
    }
    if ((writing_version == 2) || (writing_version == 3)) {
      // USR version 2 & 3 use 16-bit count
      gbfputint16(NumTrails, file_out);
      if (NumTrails) {
        trail_count=0;
        track_disp_all(lowranceusr_trail_hdr, nullptr, lowranceusr_trail_disp);
      }
    } else {
      // All other USR formats use 32-bit count
      gbfputint32(NumTrails, file_out);
      if (NumTrails) {
        trail_count=0;
        track_disp_all(lowranceusr4_trail_hdr, nullptr, lowranceusr4_trail_disp);
      }
    }
  }
}


ff_vecs_t lowranceusr_vecs = {
  ff_type_file,
  FF_CAP_RW_ALL,
  rd_init,
  wr_init,
  rd_deinit,
  wr_deinit,
  data_read,
  data_write,
  nullptr,
  lowranceusr_args,
  CET_CHARSET_ASCII, 0   /* CET-REVIEW */
  , NULL_POS_OPS,
  nullptr
};
