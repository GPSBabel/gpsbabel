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
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

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
      between USR 2/3 and 4/5/6 or between 4/5/6 and 2/3 because of
      the separation in processing between lowranceusr.cc and
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
        Many of the counts (Number of Waypoints, Number of Routes, etc) were expanded from
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
#ifndef LOWRANCEUSR_H_INCLUDED_
#define LOWRANCEUSR_H_INCLUDED_

#include <cmath>                // for M_PI, round, atan, exp, log, tan
#include <cstdint>              // for int64_t

#include <QList>                // for QList
#include <QString>              // for QString
#include <QTextCodec>           // for QTextCodec
#include <QVector>              // for QVector
#include <Qt>                   // for CaseInsensitive
#include <QtGlobal>             // for uint

#include "defs.h"
#include "format.h"
#include "formspec.h"             // for FsChainFind, FsChainAdd, kFsLowranceusr4, FormatSpecificData
#include "gbfile.h"               // for gbfgetint32, gbfputint32, gbfputint16, gbfgetc, gbfgetint16, gbfwrite, gbfputc, gbfeof, gbfgetflt, gbfclose, gbfgetdbl, gbfopen_le, gbfputdbl, gbfputs, gbfile, gbfputflt, gbfread, gbfseek
#include "mkshort.h"              // for MakeShort
#include "src/core/datetime.h"    // for DateTime


class LowranceusrFormat : public Format
{
public:
  /* Member Functions */

  [[nodiscard]] QVector<arglist_t>* get_args() override
  {
    return &lowranceusr_args;
  }

  [[nodiscard]] ff_type get_type() const override
  {
    return ff_type_file;
  }

  [[nodiscard]] QVector<ff_cap> get_cap() const override
  {
    return FF_CAP_RW_ALL;
  }

  void rd_init(const QString& fname) override;
  void read() override;
  void rd_deinit() override;
  void wr_init(const QString& fname) override;
  void write() override;
  void wr_deinit() override;

private:
  /* Types */

  struct lowranceusr_icon_mapping_t {
    int    value;
    const char*  icon;
  };

  struct lowranceusr4_icon_mapping_t {
    int      value;
    const char*    icon;
    const char*    color[7];
  };

  struct lowranceusr4_fsdata : FormatSpecificData {
    lowranceusr4_fsdata() : FormatSpecificData(kFsLowranceusr4) {}

    lowranceusr4_fsdata* clone() const override
    {
      return new lowranceusr4_fsdata(*this);
    }

    uint uid_unit{0};
    uint uid_unit2{0};
    int uid_seq_low{0};
    int uid_seq_high{0};
    uint UUID1{0};
    uint UUID2{0};
    uint UUID3{0};
    uint UUID4{0};
    int  flags{0};
    int  color{0};
    const char* color_desc{nullptr};
    int  icon_num{0};
    float depth{0.0};
  };

  struct Lowranceusr4Timestamp
  {
    unsigned int julian_day_number;
    unsigned int milliseconds;

    Lowranceusr4Timestamp(unsigned int jd, unsigned int ms) : julian_day_number{jd}, milliseconds{ms} {}
  };

  /* Constants */

  static constexpr int DEF_ICON = 10001;
  static constexpr int X_1_ICON = 10003;
  static constexpr const char* DISABLED_CACHE_TXT = "Disabled Cache";

  static constexpr lowranceusr_icon_mapping_t lowranceusr_icon_value_table[] = {

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
//  { 10038,    "Geocache" },            /* flag buoy */
//  { 10016,    "Geocache Found" },      /* exclamation */
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

  static constexpr int DEF_USR4_ICON = 2;
  static constexpr int DEF_USR4_COLOR = 0;

  static constexpr lowranceusr4_icon_mapping_t lowranceusr4_icon_value_table[] = {

    /*  USR     GPX Symbol                COLOR1     COLOR2     COLOR3    COLOR4     COLOR5    COLOR6      COLOR7         HOOK2 Displays */

    {     1,    "diamond 1",            { "blue",   "magenta", "orange", "yellow",  "green",   "aqua",    "white" }},   // diamond
    {     0,    "diamond 1",            { "blue",   "magenta", "orange", "yellow",  "green",   "aqua",    "white" }},   // diamond
    {     1,    "diamond 2",            { "blue",   "magenta", "orange", "yellow",  "green",   "aqua",    "white" }},   // diamond
    {     1,    "diamond 3",            { "blue",   "magenta", "orange", "yellow",  "green",   "aqua",    "white" }},   // diamond
    {     2,    "x 1",                  { "blue",   "magenta", "orange", "yellow",  "green",   "aqua",    "white" }},   // X
    {     2,    "x 2",                  { "blue",   "magenta", "orange", "yellow",  "green",   "aqua",    "white" }},   // X
    {     2,    "x 3",                  { "blue",   "magenta", "orange", "yellow",  "green",   "aqua",    "white" }},   // X
    {     4,    "fish",                 { "green",  "aqua",    "blue",   "magenta", "red",     "yellow",  "white" }},   // single fish
    {     5,    "two fish",             { "aqua",   "blue",    "red",    "orange",  "yellow",  "green",   "white" }},   // schoolfish
    {     8,    "hole",                 { "aqua",   "blue",    "red",    "orange",  "yellow",  "green",   "white" }},   // dip sign
    {     9,    "hump",                 { "aqua",   "blue",    "red",    "orange",  "yellow",  "green",   "white" }},   // bump sign
    {    10,    "longgrass",            { "green",  "aqua",    "blue",   "red",     "orange",  "yellow",  "white" }},   // long grass
    {    12,    "rocks",                { "blue",   "magenta", "orange", "yellow",  "green",   "aqua",    "white" }},   // rocks
    {    17,    "gas station",          { "red",    "yellow",  "green",  "aqua",    "blue",    "magenta", "white" }},   // gas pump
    {    28,    "tree",                 { "green",  "aqua",    "blue",   "magenta", "red",     "yellow",  "white" }},   // tree
    {    30,    "campsite",             { "yellow", "green",   "aqua",   "blue",    "magenta", "red",     "white" }},   // tent
    {    37,    "skull and crossbones", { "blue",   "magenta", "orange", "yellow",  "green",   "aqua",    "white" }},   // skull and crossbones
    {    40,    "dive flag",            { "red",    "yellow",  "green",  "aqua",    "blue",    "magenta", "white" }},   // diveflag
    {    42,    "anchor",               { "blue",   "magenta", "orange", "yellow",  "green",   "aqua",    "white" }},   // anchor
    {    44,    "boat ramp",            { "red",    "yellow",  "green",  "aqua",    "blue",    "magenta", "white" }},   // boatramp
    {    48,    "pier",                 { "blue",   "magenta", "orange", "yellow",  "green",   "aqua",    "white" }},   // pier

    // END OF ICON MAPPING
    {    -1,    nullptr,                { nullptr,  nullptr,   nullptr,  nullptr,   nullptr,   nullptr,  nullptr  }}
  };

  static constexpr int MAXUSRSTRINGSIZE = 256;
  static constexpr double SEMIMINOR = 6356752.3142;
  static constexpr double DEGREESTORADIANS = M_PI/180.0;
  static constexpr int MAX_TRAIL_POINTS = 9999;
  static constexpr double UNKNOWN_USR_ALTITUDE = METERS_TO_FEET(-10000); /* -10000ft is how the unit stores unknown */
  static constexpr int64_t base_time_secs = 946706400; /* Jan 1, 2000 00:00:00 */

  /* Member Functions */

  static bool same_points(const Waypoint* A, const Waypoint* B);
  void register_waypt(const Waypoint* wpt) const;
  static const Waypoint* lowranceusr4_find_waypt(uint uid_unit, int uid_seq_low, int uid_seq_high);
  static const Waypoint* lowranceusr4_find_global_waypt(uint id1, uint id2, uint id3, uint id4);
  QString lowranceusr4_readstr(gbfile* file, int bytes_per_char) const;
  void lowranceusr4_writestr(const QString& buf, gbfile* file, int bytes_per_char) const;
  static gpsbabel::DateTime lowranceusr4_get_timestamp(unsigned int jd_number, unsigned int msecs);
  static Lowranceusr4Timestamp lowranceusr4_jd_from_timestamp(const gpsbabel::DateTime& qdt);
  static QString lowranceusr_find_desc_from_icon_number(int icon);
  static int lowranceusr_find_icon_number_from_desc(const QString& desc);
  static QString lowranceusr4_find_desc_from_icon_number(int icon);
  static int lowranceusr4_find_icon_number_from_desc(const QString& desc);
  static const char* lowranceusr4_find_color_from_icon_number_plus_color_index(int icon, int index);
  static int lowranceusr4_find_index_from_icon_desc_and_color_desc(const QString& icon, const QString& color);
  static double lon_mm_to_deg(double x);
  static double lat_mm_to_deg(double x);
  static long int lon_deg_to_mm(double x);
  static long int lat_deg_to_mm(double x);
  void lowranceusr_parse_waypt(Waypoint* wpt_tmp, int object_num_present) const;
  void lowranceusr4_parse_waypt(Waypoint* wpt_tmp) const;
  void lowranceusr_parse_waypts() const;
  void lowranceusr_parse_route() const;
  void lowranceusr4_parse_route() const;
  void lowranceusr_parse_routes();
  void lowranceusr_parse_icons() const;
  void lowranceusr_parse_trail(int* trail_num);
  void lowranceusr4_parse_trail(int* trail_num) const;
  void lowranceusr_parse_trails();
  void lowranceusr_waypt_disp(const Waypoint* wpt) const;
  void lowranceusr4_waypt_disp(const Waypoint* wpt);
  void lowranceusr_waypt_pr(const Waypoint* wpt);
  void lowranceusr4_write_waypoints();
  void lowranceusr_write_icon(const Waypoint* wpt) const;
  void lowranceusr_trail_hdr(const route_head* trk);
  void lowranceusr_route_hdr(const route_head* rte);
  void lowranceusr4_route_hdr(const route_head* rte);
  void lowranceusr4_route_leg_disp(const Waypoint* wpt) const;
  void lowranceusr4_route_trl(const route_head* /*unused*/) const;
  void lowranceusr_trail_disp(const Waypoint* wpt);
  void lowranceusr_merge_trail_hdr(const route_head* trk);
  void lowranceusr_merge_trail_tlr(const route_head* /*unused*/);
  void lowranceusr_merge_trail_hdr_2(const route_head* /*unused*/);
  void lowranceusr4_trail_hdr(const route_head* trail);
  void lowranceusr4_trail_disp(const Waypoint* wpt) const;

  /* Data Members */

  gbfile*        file_in{};
  gbfile*        file_out{};
  MakeShort*     mkshort_handle{};

  route_head*    trk_head{};
  route_head*    rte_head{};

  int            waypt_uid{};
  int            route_uid{};
  int            trail_uid{};

  char*          opt_ignoreicons{};
  char*          opt_writeasicons{};
  char*          opt_seg_break{};
  char*          opt_wversion{};
  char*          opt_title{};
  char*          opt_content_descr{};
  char*          opt_serialnum{};
  int            opt_serialnum_i{};

  QList<const Waypoint*>* waypt_table{nullptr};

  unsigned short waypt_out_count{};
  int            trail_count{}, lowrance_route_count{};
  int            trail_point_count{};
  bool           merge_new_track{false};
  short          num_section_points{};
  char*          merge{};
  int            reading_version{};
  int            rstream_version{};
  int            writing_version{};
  QTextCodec*    utf16le_codec{nullptr};

  template <typename T>
  static QString lowranceusr_common_find_desc_from_icon_number(const int icon, const T icon_value_table[])
  {
    for (const T* i = icon_value_table; i->icon; ++i) {
      if (icon == i->value) {
        return i->icon;
      }
    }

    // Didn't find it in table, default to leave it as the number found
    return QStringLiteral("icon-%1").arg(icon);
  }

  template <typename T>
  static int lowranceusr_common_find_icon_number_from_desc(const QString& desc, const T icon_value_table[], const int def_icon)
  {
    if (desc.isNull()) {
      return def_icon;
    }

    /*
     * If we were given a numeric icon number as a description
     * (i.e. 8255), just return that.
     * Also return the icon number for descriptions of "icon-"
     * followed by a numeric icon number.
     */
    int n = desc.mid(desc.startsWith("icon-") ? 5 : 0).toInt();
    if (n)  {
      return n;
    }

    for (const T* i = icon_value_table; i->icon; ++i) {
      if (desc.compare(i->icon,Qt::CaseInsensitive) == 0) {
        return i->value;
      }
    }

    return def_icon;
  }

// Combined arguments from previous lowranceusr and lowranceusr4 into single set.
// Use output format specified to determine if args are ignored.
  QVector<arglist_t> lowranceusr_args = {
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

  };
};
#endif // LOWRANCEUSR_H_INCLUDED_
