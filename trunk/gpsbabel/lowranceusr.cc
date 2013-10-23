/*
	Access to Lowrance USR files.
	Contributed to gpsbabel by Jason Rust (jrust at rustyparts.com)

	Copyright (C) 2005, 2006, 2007, 2008 Robert Lipe, robertlipe@usa.net

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
*/


#include "defs.h"
#include <string.h>
#include <math.h> /* for lat/lon conversion */

typedef struct lowranceusr_icon_mapping {
  const int	value;
  const char*	icon;
} lowranceusr_icon_mapping_t;

#define DEF_ICON 10001

/* Taken from iFinder 1.8 */
const lowranceusr_icon_mapping_t lowranceusr_icon_value_table[] = {
  { 10000, "diamond 1" },
  { 10001, "diamond 2" },
  { 10002, "diamond 3" },
  { 10003, "x 1" },
  { 10004, "x 2" },
  { 10005, "x 3" },
  { 10006, "cross" },
  { 10007, "house" },
  { 10008, "car" },
  { 10009, "store" },
  { 10010, "gas station" },
  { 10011, "fork and spoon" },
  { 10012, "telephone" },
  { 10013, "airplane" },
  { 10014, "exit sign" },
  { 10015, "stop sign" },
  { 10016, "exclamation" },
  { 10017, "traffic light" },
  { 10018, "american flag" },
  { 10019, "person" },
  { 10020, "restrooms" },
  { 10021, "tree" },
  { 10022, "mountains" },
  { 10023, "campsite" },
  { 10024, "picnic table" },
  { 10025, "deer" },
  { 10026, "deer tracks" },
  { 10027, "turkey tracks" },
  { 10028, "tree stand" },
  { 10029, "bridge" },
  { 10030, "skull and crossbones" },
  { 10031, "fish" },
  { 10032, "two fish" },
  { 10033, "dive flag" },
  { 10034, "wreck" },
  { 10035, "anchor" },
  { 10036, "boat" },
  { 10037, "boat ramp" },
  { 10038, "flag buoy" },
  { 10039, "dam" },
  { 10040, "swimmer" },
  { 10041, "pier"},

  /* The following list is from TopoFusion */

  { 10000, "Waypoint" },		/* diamond 1 */
  { DEF_ICON, "Text Label (No Dot)" },
  { 10018, "Trailhead" },		/* american flag */
  { 10023, "Campground" },	/* campsite */
  { 10022, "Summit" },		/* mountains */
  { DEF_ICON, "Tall Tower" },
  { DEF_ICON, "Short Tower" },
  { 10021, "Forest" },		/* tree */
  { DEF_ICON, "Mine" },
//	{ 10038, "Geocache" },		/* flag buoy */
//	{ 10016, "Geocache Found" },	/* exclamation */
  { DEF_ICON, "Skiing Area" },
  { 10029, "Crossing" },		/* bridge */
  { 10007, "House" },			/* house */
  { 10003, "Dot" },			/* x 1 */
  { 10025, "Hunting Area" },	/* deer */
  { 10031, "Fishing Area" },	/* fish */
  { 10040, "Swimming Area" },	/* swimmer */
  { 10012, "Telephone" },		/* telephone */
  { 10024, "Rest Area" },		/* picnic table */
  { 10021, "Park" },			/* tree */
  { 10007, "Information" },	/* house */
  { 10022, "Scenic Area" },	/* mountains */
  { DEF_ICON, "Bank/Dollar" },
  { 10009, "Hotel" },			/* store */
  { 10011, "Restaurant" },	/* fork and spoon */
  { 10030, "Danger Area" },	/* skull and crossbones */
  { 10035, "Anchor" },		/* anchor */
  { 10002, "City (Large)" },	/* diamond 3 */
  { 10001, "City (Medium)" },	/* diamond 2 */
  { 10000, "City (Small)" },	/* diamond 1 */
  { DEF_ICON, "Drinking Water" },
  { 10008, "Parking Area" },	/* car */
  { 10023, "RV Park" },		/* campsite */
  { 10020, "Rest Room" },		/* restroom */
  { 10019, "Shower" },		/* person */
  { DEF_ICON, "Tunnel" },

  /* This list comes from 'wifinder' from ifinder H20 Color */

  { 10062, "Interesting Land Feature" },
  { 10063, "Global Location" },
  { 10064, "Note" },
  { 10065, "Ghost" },
  { 10066, "Letter" },
  { 10067, "Multi-Treasure" },
  { 10068, "Mystery Or Puzzle" },
  { 10069, "Treasure" },
  { 10070, "Webmail" },
  { 10071, "Sun" },
  { 10072, "Musical Note" },
  { 10073, "Camera/Movie Theater" },
  { 10074, "Star" },
  { 10075, "Coffee Mug" },
  { 10076, "Books" },
  { 10077, "Historical Marker" },
  { 10078, "Tools/Repair" },
  { 10079, "Favorite" },
  { 10080, "Arena" },
  { 10081, "Golf Course" },
  { 10082, "Money/Atm" },

  /* This list comes from Alan Porter <alan@kr4jb.net>, using an iFinder Expedition C */

  { 10042, "icon42" },  // black box with red X
  { 10043, "icon43" },  // small red dot
  { 10044, "icon44" },  // 4-wheeler
  { 10045, "icon45" },  // hiding hunter
  { 10046, "icon46" },  // tree (yellow base)
  { 10047, "icon47" },  // windmill
  { 10048, "icon48" },  // camera
  { 10049, "icon49" },  // tree (something in front of base)
  { 10050, "icon50" },  // tree (something hanging from left side)
  { 10051, "icon51" },  // 4 dots in rhombus shape
  { 10052, "icon52" },  // bare winter tree
  { 10053, "icon53" },  // hiding deer head peeking over bushes
  { 10054, "icon54" },  // piston? over a pile of salt?
  { 10055, "icon55" },  // corn
  { 10056, "icon56" },  // turkey
  { 10057, "icon57" },  // duck
  { 10058, "icon58" },  // hen
  { 10059, "icon59" },  // rabbit
  { 10060, "icon60" },  // paw print
  { 10061, "icon61" },  // 2 red flames?

  /* These are the icons that gpsbabel will use */

  { 10038, "Geocache" },                      // flag buoy
  { 10016, "Geocache Found" },                // exclamation
  { 10043, "Micro-Cache" },                   // small red dot
  { 10065, "Virtual cache" },                 // ghost
  { 10051, "Multi-Cache" },                   // 4 dots in rhombus shape
  { 10068, "Unknown Cache" },                 // ? mark
  { 10045, "Locationless (Reverse) Cache" },  // hiding hunter
  { 10066, "Post Office" },                   // letter
  { 10019, "Event Cache" },                   // person
  { 10070, "Webcam Cache" },                  // webcam
  { 10042, "Disabled Cache" },                // black box with red X

  {	 -1, NULL }
};

static gbfile* file_in;
static gbfile* file_out;
static short_handle mkshort_handle;

static unsigned short waypt_out_count;
static unsigned int trail_count, lowrance_route_count;
static int trail_point_count;
static char continuous = 1;
static short num_section_points;
static route_head* trk_head;
static route_head* rte_head;
static char* ignoreicons;
static char* writeasicons;
static char* merge;
static char* seg_break;
static char* wversion_arg;
static int reading_version;
static int writing_version;

#define MYNAME "Lowrance USR"

#define MAXUSRSTRINGSIZE	256
#define SEMIMINOR		   6356752.3142
#define DEGREESTORADIANS	0.017453292
#define SECSTO2000			946713600
#define MAX_TRAIL_POINTS 9999
#define UNKNOWN_USR_ALTITUDE	METERS_TO_FEET(-10000) /* -10000ft is how the unit stores unknown */

/* Jan 1, 2000 00:00:00 */
const time_t base_time_secs = 946706400;

static int
lowranceusr_readstr(char* buf, const int maxlen, gbfile* file)
{
  int org, len;

  org = len = gbfgetint32(file);
  if (len < 0) {
    fatal(MYNAME ": Invalid item length (%d)!\n", len);
  } else if (len) {
    int i;
    if (len > maxlen) {
      len = maxlen;
    }
    (void) gbfread(buf, 1, len, file);
    if (org > maxlen) {
      (void) gbfseek(file, org - maxlen, SEEK_CUR);
    }
    // IWay 350C puts 0x01 for the accented o in the street name
    // of the Montreal Holiday Inn.
    for (i = 0; i < len; i++) {
      if (buf[i] == 0x01) {
        buf[i] = '*';
      }
    }

  }

  return len;
}

const char*
lowranceusr_find_desc_from_icon_number(const int icon)
{
  const lowranceusr_icon_mapping_t* i;

  for (i = lowranceusr_icon_value_table; i->icon; i++) {
    if (icon == i->value) {
      return i->icon;
    }
  }

  return "";
}

static int
lowranceusr_find_icon_number_from_desc(const QString& desc)
{
  const lowranceusr_icon_mapping_t* i;
  int n;

  if (desc.isNull()) {
    return DEF_ICON;
  }

  /*
   * If we were given a numeric icon number as a description
   * (i.e. 8255), just return that.
   */
  n = desc.toInt();
  if (n)  {
    return n;
  }


  for (i = lowranceusr_icon_value_table; i->icon; i++) {
    if (desc.compare(i->icon,Qt::CaseInsensitive) == 0) {
      return i->value;
    }
  }

  return DEF_ICON;
}

static
arglist_t lowranceusr_args[] = {
  {
    "ignoreicons", &ignoreicons, "Ignore event marker icons on read",
    NULL, ARGTYPE_BOOL, ARG_NOMINMAX
  },
  {
    "writeasicons", &writeasicons, "Treat waypoints as icons on write",
    NULL, ARGTYPE_BOOL, ARG_NOMINMAX
  },
  {
    "merge", &merge, "(USR output) Merge into one segmented track",
    NULL, ARGTYPE_BOOL, ARG_NOMINMAX
  },
  {
    "break", &seg_break, "(USR input) Break segments into separate tracks",
    NULL, ARGTYPE_BOOL, ARG_NOMINMAX
  },
  {
    "wversion", &wversion_arg, "(USR output) Write version",
    "2", ARGTYPE_INT, "2", "3"
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
  waypt_out_count = 0;
  writing_version = atoi(wversion_arg);
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

static long
lon_deg_to_mm(double x)
{
  return lround(x * SEMIMINOR * DEGREESTORADIANS);
}

static double
lat_mm_to_deg(double x)
{
  return (2.0 * atan(exp(x / SEMIMINOR)) - M_PI / 2.0) / DEGREESTORADIANS;
}

static long
lat_deg_to_mm(double x)
{
  return (long)(SEMIMINOR * log(tan((x * DEGREESTORADIANS + M_PI / 2.0) / 2.0)));
}

static void
lowranceusr_parse_waypt(waypoint* wpt_tmp)
{
  char buff[MAXUSRSTRINGSIZE + 1];
  int text_len;
  time_t waypt_time;
  short waypt_type;

  wpt_tmp->latitude = lat_mm_to_deg(gbfgetint32(file_in));
  wpt_tmp->longitude = lon_mm_to_deg(gbfgetint32(file_in));
  wpt_tmp->altitude = FEET_TO_METERS(gbfgetint32(file_in));
  if (METERS_TO_FEET(wpt_tmp->altitude) <= -10000) {
    wpt_tmp->altitude = unknown_alt;
  }

  text_len = lowranceusr_readstr(&buff[0], MAXUSRSTRINGSIZE, file_in);
  if (text_len) {
    buff[text_len] = '\0';
    wpt_tmp->shortname = xstrdup(buff);
  }

  if (global_opts.debug_level >= 1)
    printf(MYNAME " parse_waypt: Waypt name = %s Lat = %f Lon = %f alt = %f\n",CSTRc(wpt_tmp->shortname), wpt_tmp->latitude,
           wpt_tmp->longitude, wpt_tmp->altitude);

  text_len = lowranceusr_readstr(&buff[0], MAXUSRSTRINGSIZE, file_in);
  if (text_len) {
    buff[text_len] = '\0';
    wpt_tmp->description = xstrdup(buff);
  }
  /* Time is number of seconds since Jan. 1, 2000 */
  waypt_time = gbfgetint32(file_in);
  if (waypt_time) {
    wpt_tmp->SetCreationTime(base_time_secs + waypt_time);
  }

  if (global_opts.debug_level >= 2) {
    printf(MYNAME " parse_waypt: creation time %d\n",
           (int)wpt_tmp->creation_time.toTime_t());
    printf(MYNAME " parse_waypt: base_time %d\n", (int)base_time_secs);
    printf(MYNAME " parse_waypt: waypt time %d\n", (int)waypt_time);
  }

  /* Symbol ID */
  wpt_tmp->icon_descr = lowranceusr_find_desc_from_icon_number(gbfgetint32(file_in));
  if (wpt_tmp->icon_descr.isNull()) {
    char nbuf[10];
    snprintf(nbuf, sizeof(nbuf), "%d", le_read32(buff));
    wpt_tmp->icon_descr = nbuf;
  }

  /* Waypoint Type (USER, TEMPORARY, POINT_OF_INTEREST) */
  waypt_type = gbfgetint16(file_in);
  if (global_opts.debug_level >= 1) {
    printf(MYNAME " parse_waypt: waypt_type = %d\n",waypt_type);
  }

  // Version 3 has a depth field here.
  if (reading_version >= 3) {
    float depth_feet = gbfgetflt(file_in);
    if (abs(depth_feet - 99999.0)  > .1) {
      WAYPT_SET(wpt_tmp, depth, FEET_TO_METERS(depth_feet));
    }
  }

}



static void
lowranceusr_parse_routes(void)
{
  char buff[MAXUSRSTRINGSIZE + 1];
  short int num_routes, num_legs;
  int i,j;
  int text_len;
  waypoint* wpt_tmp;

  num_routes = gbfgetint16(file_in);

  if (global_opts.debug_level >= 1) {
    printf(MYNAME " parse_routes: Num Routes = %d\n", num_routes);
  }

  for (i=0; i < num_routes; i++) {
    rte_head = route_head_alloc();
    route_add_head(rte_head);
    rte_head->rte_num = i+1;

    /* route name */
    text_len = lowranceusr_readstr(&buff[0], MAXUSRSTRINGSIZE, file_in);
    if (text_len) {
      buff[text_len] = '\0';
      rte_head->rte_name = xstrdup(buff);
    }

    /* num Legs */
    num_legs = gbfgetint16(file_in);

    /* route reversed */
    (void) gbfread(&buff[0], 1, 1, file_in);

    /* waypoints */
    for (j=0; j < num_legs; j++) {
      wpt_tmp = waypt_new();
      lowranceusr_parse_waypt(wpt_tmp);
      route_add_wpt(rte_head, wpt_tmp);
    }
  }
}

/*
 * Icons are automatically converted to waypoints unless
 * option of ignoreicons is used
 */
static void
lowranceusr_parse_icons(void)
{
  char buff[MAXUSRSTRINGSIZE + 1];
  short int num_icons;
  int i;

  num_icons = gbfgetint16(file_in);

  if (global_opts.debug_level >= 1) {
    printf(MYNAME " parse_icons: num Icons = %d\n", num_icons);
  }

  for (i=0; i < num_icons; i++) {
    if (ignoreicons) {
      /* position coord lat & long */
      (void) gbfread(&buff[0], 4, 2, file_in);
      /* symbol */
      (void) gbfread(&buff[0], 4, 1, file_in);
    } else {
      waypoint* wpt_tmp;
      wpt_tmp = waypt_new();

      /* position coord lat & long */
      wpt_tmp->latitude = lat_mm_to_deg(gbfgetint32(file_in));
      wpt_tmp->longitude = lon_mm_to_deg(gbfgetint32(file_in));
      wpt_tmp->altitude = 0;
      snprintf(buff, sizeof(buff), "Icon %d", i+1);
      wpt_tmp->shortname = xstrdup(buff);
      /* symbol */
      wpt_tmp->icon_descr = lowranceusr_find_desc_from_icon_number(gbfgetint32(file_in));
      waypt_add(wpt_tmp);
    }
  }

}

static void
lowranceusr_parse_trails(void)
{
  char buff[MAXUSRSTRINGSIZE + 1];
  short int num_trails, num_trail_points, num_section_points;
  int i,j, trk_num, itmp;
  int text_len;
  waypoint* wpt_tmp;
  route_head* trk_tmp;

  /* num trails */
  num_trails = gbfgetint16(file_in);

  if (global_opts.debug_level >= 1) {
    printf(MYNAME " parse_trails: num trails = %d\n", num_trails);
  }

  for (i=trk_num=0; i < num_trails; i++) {
    trk_head = route_head_alloc();
    trk_head->rte_num = ++trk_num;
    track_add_head(trk_head);

    /* trail name */
    text_len = lowranceusr_readstr(&buff[0], MAXUSRSTRINGSIZE, file_in);

    if (global_opts.debug_level >= 1) {
      printf(MYNAME " parse_trails: name text len = %d\n", text_len);
    }

    if (text_len) {
      buff[text_len] = '\0';
      trk_head->rte_name = xstrdup(buff);
    }

    if (global_opts.debug_level >= 1) {
      printf(MYNAME " parse_trails: trail name = %s\n", CSTRc(trk_head->rte_name));
    }

    /* visible */
    (void) gbfread(&buff[0], 1, 1, file_in);
    /* num trail points */
    num_trail_points = gbfgetint16(file_in);

    if (global_opts.debug_level >= 1) {
      printf(MYNAME " parse_trails: num trail points = %d\n", num_trail_points);
    }

    /* max trail size */
    itmp = gbfgetint16(file_in);

    if (global_opts.debug_level >= 1) {
      printf(MYNAME " parse_trails: max trail size = %d\n", itmp);
    }

    if (num_trail_points) {

      while (num_trail_points) {
        /* num section points */
        num_section_points = gbfgetint16(file_in);

        if (global_opts.debug_level >= 1) {
          printf(MYNAME " parse_trails: num section points = %d\n", num_section_points);
        }

        for (j=0; j < num_section_points; j++, num_trail_points--) {
          wpt_tmp = waypt_new();
          wpt_tmp->latitude = lat_mm_to_deg(gbfgetint32(file_in));
          wpt_tmp->longitude = lon_mm_to_deg(gbfgetint32(file_in));
          /* continuous */
          (void) gbfread(&buff[0], 1, 1, file_in);
          if (!buff[0] && seg_break && j) {
            trk_tmp = route_head_alloc();
            trk_tmp->rte_num = ++trk_num;
            trk_tmp->rte_name = xstrdup(trk_head->rte_name);
            track_add_head(trk_tmp);
            trk_head = trk_tmp;
          }
          track_add_wpt(trk_head, wpt_tmp);

          if (global_opts.debug_level >= 1) {
            printf(MYNAME " parse_trails: Trail pt lat %f lon %f\n", wpt_tmp->latitude, wpt_tmp->longitude);
          }
        }
      }
    }
    /* remove the trail since it's empty */
    else {
      track_del_head(trk_head);
    }
  }
}

static void
data_read(void)
{
  short int NumWaypoints, MajorVersion, MinorVersion, object_num;
  int i;

  MajorVersion = gbfgetint16(file_in);
  reading_version = MajorVersion;
  MinorVersion = gbfgetint16(file_in);

  if (global_opts.debug_level >= 1) {
    printf(MYNAME " data_read: Major Version %d Minor Version %d\n", MajorVersion, MinorVersion);
  }

  if (MajorVersion < 2) {
    fatal(MYNAME ": input file is from an old version of the USR file and is not supported\n");
  }
  if (MajorVersion == 4) {
    fatal(MYNAME ": this appears to be a USR v4 file: use the lowranceusr4 format\n");
  }
  if (MajorVersion > 4) {
    fatal(MYNAME ": input file version %d is not supported\n",
          MajorVersion);
  }

  NumWaypoints = gbfgetint16(file_in);

  if (global_opts.debug_level >= 1) {
    printf(MYNAME " data_read: Num waypoints %d\n", NumWaypoints);
  }

  for (i = 0; i < NumWaypoints; i++) {
    waypoint* wpt_tmp;

    wpt_tmp = waypt_new();

    /* Object num */
    object_num = gbfgetint16(file_in);
    if (global_opts.debug_level >= 1) {
      printf(MYNAME " data_read: object_num = %d\n", object_num);
    }

    /* waypoint */
    lowranceusr_parse_waypt(wpt_tmp);

    waypt_add(wpt_tmp);
  }

  lowranceusr_parse_routes();
  lowranceusr_parse_icons();
  lowranceusr_parse_trails();
}

static void
lowranceusr_waypt_disp(const waypoint* wpt)
{
  int text_len, Lat, Lon, Time, SymbolId;
  short int WayptType;
  char* comment;
  int alt = METERS_TO_FEET(wpt->altitude);

  if (wpt->altitude == unknown_alt) {
    alt = UNKNOWN_USR_ALTITUDE;
  }

  Lat = lat_deg_to_mm(wpt->latitude);
  Lon = lon_deg_to_mm(wpt->longitude);
  gbfputint32(Lat, file_out);
  gbfputint32(Lon, file_out);
  gbfputint32(alt, file_out);

  if (writing_version >= 3) {
    float depth = WAYPT_HAS(wpt, depth) ?
                  METERS_TO_FEET(wpt->depth) : -99999.0;
    gbfputflt(depth, file_out);
  }

  if (global_opts.debug_level >= 1) {
    /* print lat/lon/alt on one easily greppable line */
    printf(MYNAME " waypt_disp: Lat = %d   Lon = %d   Alt = %d\n",Lat, Lon, alt);
  }

  /* Try and make sure we have a name */
#if NEW_STRINGS
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
#else
  char* name = NULL;
  if ((! wpt->shortname) || global_opts.synthesize_shortnames) {
    if (wpt->description && global_opts.synthesize_shortnames) {
      name = mkshort_from_wpt(mkshort_handle, wpt);
    } else if (wpt->shortname) {
      name = wpt->shortname;
    } else if (wpt->description) {
      name = wpt->description;
#endif
    }
  } else {
    name = wpt->shortname;
  }

#if NEW_STRINGS
  text_len = name.length();
#else
  text_len = strlen(name);
#endif
  if (text_len > MAXUSRSTRINGSIZE) {
    text_len = MAXUSRSTRINGSIZE;
  }
  gbfputint32(text_len, file_out);
  gbfwrite(CSTRc(name), 1, text_len, file_out);

  if (global_opts.debug_level >= 1) {
    printf(MYNAME " waypt_disp: Waypt name = %s\n", CSTRc(name));
  }

  /**
   * Comments are now used by the iFinder (Expedition C supports them)
   */
#if NEW_STRINGS
  if (wpt->description != wpt->shortname) {
#else
  if (wpt->description && strcmp(wpt->description, wpt->shortname) != 0) {
#endif
    comment = xstrdup(wpt->description);
    text_len = strlen(comment);
    if (text_len > MAXUSRSTRINGSIZE) {
      text_len = MAXUSRSTRINGSIZE;
    }
    gbfputint32(text_len, file_out);
    gbfwrite(comment, 1, text_len, file_out);
    xfree(comment);
  } else {
    text_len = 0;
    gbfputint32(text_len, file_out);
  }

  if (wpt->creation_time.toTime_t() > base_time_secs) {
    Time = wpt->creation_time.toTime_t() - base_time_secs;
  } else {
    Time = 0;
  }

  if (global_opts.debug_level >= 2) {
    time_t wpt_time = Time;
    printf(MYNAME " waypt_disp: base_time : %d\n", (int)base_time_secs);
    printf(MYNAME " waypt_disp: creation time : %d\n", (int)wpt->creation_time.toTime_t());
    printf(MYNAME " waypt_disp: waypt time : %d\n", (int)wpt_time);
    printf(MYNAME " waypt_disp: waypt time (local): %s\n", ctime(&wpt_time));
  }

  gbfputint32(Time, file_out);

  if (get_cache_icon(wpt) && wpt->icon_descr.compare("Geocache Found") == 0) {
    SymbolId = lowranceusr_find_icon_number_from_desc(get_cache_icon(wpt));
  } else {
    SymbolId = lowranceusr_find_icon_number_from_desc(wpt->icon_descr);
  }
  /* If the waypoint is archived or disabled, use a "disabled" icon instead. */
  if ((wpt->gc_data->is_archived==status_true) || (wpt->gc_data->is_available==status_false)) {
    SymbolId = lowranceusr_find_icon_number_from_desc("Disabled Cache");
  }

  gbfputint32(SymbolId, file_out);

  /* USER waypoint type */
  WayptType = 0;
  gbfputint16(WayptType, file_out);
}

static void
lowranceusr_waypt_pr(const waypoint* wpt)
{

  /* our personal waypoint counter */
  gbfputint16(waypt_out_count, file_out);

  if (global_opts.debug_level >= 1) {
    printf(MYNAME " waypt_pr: waypoint #%d ",waypt_out_count);
  }

  waypt_out_count++;

  lowranceusr_waypt_disp(wpt);
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
lowranceusr_write_icon(const waypoint* wpt)
{
  int latmm = lat_deg_to_mm(wpt->latitude);
  int lonmm = lon_deg_to_mm(wpt->longitude);
  int icon = !wpt->icon_descr.isNull() ?
             lowranceusr_find_icon_number_from_desc(wpt->icon_descr) :
             10003;

  gbfputint32(latmm, file_out);
  gbfputint32(lonmm, file_out);
  gbfputint32(icon, file_out);
}

/*
 * Header format:
 *	short num_trails,
 *  int trail_name text length,
 *  char *trail_name,
 *  boolean visible,
 *  short num_trail_points,
 *  short max_trail_size,
 *  short num_section_points
 *	  == don't know how many max points per section so
 *	  == use num_trail_points for now
 *	  == Once this is known then the waypoints ought to be
 *	  == broken up into sections
 */

static void
lowranceusr_track_hdr(const route_head* trk)
{
  int text_len;
  char* name, tmp_name[20];
  short num_trail_points, max_trail_size;
  char visible=1;

  ++trail_count;
#if NEW_STRINGS
// This whole function needs to be replaced...
  if (!trk->rte_name.isEmpty()) {
    name = xstrdup(trk->rte_name);
  } else if (!trk->rte_desc.isEmpty()) {
    name = xstrdup(trk->rte_desc);
#else
  if (trk->rte_name) {
    name = xstrdup(trk->rte_name);
  } else if (trk->rte_desc) {
    name = xstrdup(trk->rte_desc);
#endif
  } else {
    tmp_name[0]='\0';
    snprintf(tmp_name, sizeof(tmp_name), "Babel %d", trail_count);
    name = xstrdup(tmp_name);
  }

  text_len = strlen(name);
  if (text_len > MAXUSRSTRINGSIZE) {
    text_len = MAXUSRSTRINGSIZE;
  }
  if (global_opts.debug_level >= 1) {
    printf(MYNAME " track_hdr: trail name text len = %d\n", text_len);
  }
  gbfputint32(text_len, file_out);

  if (global_opts.debug_level >= 1) {
    printf(MYNAME " track_hdr: trail name = %s\n", name);
  }

  gbfwrite(name, 1, text_len, file_out);

  num_trail_points = (short) trk->rte_waypt_ct;
  max_trail_size = MAX_TRAIL_POINTS;
  if (num_trail_points > max_trail_size) {
    num_trail_points = max_trail_size;
  }
  num_section_points = num_trail_points;

  if (global_opts.debug_level >= 1)
    printf(MYNAME " track_hdr: num_trail_points = %d\nmax_trail_size = %d\nnum_section_points = %d\n",
           num_trail_points, max_trail_size, num_section_points);

  gbfwrite(&visible, 1, 1, file_out);
  gbfputint16(num_trail_points, file_out);
  gbfputint16(max_trail_size, file_out);
  gbfputint16(num_section_points, file_out);
  xfree(name);
  trail_point_count=1;
}

static void
lowranceusr_route_hdr(const route_head* rte)
{
  int text_len;
  char* name, tmp_name[20];
  short num_legs;
  char route_reversed=0;

  /* route name */
#if NEW_STRINGS
// This whole function needs to be replaced...
  if (!rte->rte_name.isEmpty()) {
    name = xstrdup(rte->rte_name);
  } else if (!rte->rte_desc.isEmpty()) {
    name = xstrdup(rte->rte_desc);
#else
  if (rte->rte_name) {
    name = xstrdup(rte->rte_name);
  } else if (rte->rte_desc) {
    name = xstrdup(rte->rte_desc);
#endif
  } else {
    tmp_name[0]='\0';
    snprintf(tmp_name, sizeof(tmp_name), "Babel R%d", ++lowrance_route_count);
    name = xstrdup(tmp_name);
  }
  text_len = strlen(name);
  if (text_len > MAXUSRSTRINGSIZE) {
    text_len = MAXUSRSTRINGSIZE;
  }
  gbfputint32(text_len, file_out);
  gbfwrite(name, 1, text_len, file_out);
  xfree(name);

  /* num legs */
  num_legs = (short) rte->rte_waypt_ct;
  gbfputint16(num_legs, file_out);
  gbfwrite(&route_reversed, 1, 1, file_out);

  if (global_opts.debug_level >= 1)
    printf(MYNAME " route_hdr: route name \"%s\" num_legs = %d\n",
           CSTRc(rte->rte_name), num_legs);

}

static void
lowranceusr_track_disp(const waypoint* wpt)
{
  int lat, lon;

  if (++trail_point_count <= MAX_TRAIL_POINTS) {
    lat = lat_deg_to_mm(wpt->latitude);
    lon = lon_deg_to_mm(wpt->longitude);

    if (global_opts.debug_level >= 1) {
      printf(MYNAME " track_disp: Trail point #%d lat = %d long = %d\n",trail_point_count, lat, lon);
    }

    gbfputint32(lat, file_out);
    gbfputint32(lon, file_out);
    gbfwrite(&continuous, 1, 1, file_out);
    if (!continuous) {
      continuous = 1;
    }
  }
}

static void
lowranceusr_merge_track_hdr(const route_head* trk)
{
  int text_len;
  char* name, tmp_name[20];

  if (++trail_count == 1) {
#if NEW_STRINGS
    if (!trk->rte_name.isEmpty()) {
      name = xstrdup(trk->rte_name);
    } else if (!trk->rte_desc.isEmpty()) {
      name = xstrdup(trk->rte_desc);
#else
    if (trk->rte_name) {
      name = xstrdup(trk->rte_name);
    } else if (trk->rte_desc) {
      name = xstrdup(trk->rte_desc);
#endif
    } else {
      tmp_name[0]='\0';
      snprintf(tmp_name, sizeof(tmp_name), "Babel %d", trail_count);
      name = xstrdup(tmp_name);
    }
    text_len = strlen(name);
    if (text_len > MAXUSRSTRINGSIZE) {
      text_len = MAXUSRSTRINGSIZE;
    }
    gbfputint32(text_len, file_out);

    if (global_opts.debug_level >= 1) {
      printf(MYNAME " track_hdr: trail name = %s\n", name);
    }

    gbfwrite(name, 1, text_len, file_out);
  }

  trail_point_count += (short) trk->rte_waypt_ct;
}

static void
lowranceusr_merge_track_tlr(const route_head* trk)
{
  short num_trail_points, max_trail_size;
  char visible=1;

  if (trail_count == track_count()) {	/* last trail */
    num_trail_points = trail_point_count;
    max_trail_size = MAX_TRAIL_POINTS;
    if (num_trail_points > max_trail_size) {
      num_trail_points = max_trail_size;
    }
    num_section_points = num_trail_points;

    if (global_opts.debug_level >= 1)
      printf(MYNAME " merge_track_tlr: num_trail_points = %d\nmax_trail_size = %d\nnum_section_points = %d\n",
             num_trail_points, max_trail_size, num_section_points);

    gbfwrite(&visible, 1, 1, file_out);
    gbfputint16(num_trail_points, file_out);
    gbfputint16(max_trail_size, file_out);
    gbfputint16(num_section_points, file_out);
  }
}
static void

lowranceusr_merge_track_hdr_2(const route_head* trk)
{
  continuous = 0;
}

static void
data_write(void)
{
  short int NumWaypoints, MajorVersion, MinorVersion, NumRoutes, NumTrails, NumIcons;
  setshort_length(mkshort_handle, 15);
  MajorVersion = writing_version;
  MinorVersion = 0;

  NumWaypoints = waypt_count();

  gbfputint16(MajorVersion, file_out);
  gbfputint16(MinorVersion, file_out);

  if (global_opts.debug_level >= 1) {
    printf(MYNAME " data_write: Num waypoints = %d\n", NumWaypoints);
  }

  if (writeasicons) {
    short zero = 0;
    gbfputint16(zero, file_out);
  } else {
    gbfputint16(NumWaypoints, file_out);
    waypt_disp_all(lowranceusr_waypt_pr);
  }

  /* Route support added 6/21/05 */
  NumRoutes = route_count();
  gbfputint16(NumRoutes, file_out);

  if (global_opts.debug_level >= 1) {
    printf(MYNAME " data_write: Num routes = %d\n", NumRoutes);
  }

  if (NumRoutes) {
    lowrance_route_count=0;
    route_disp_all(lowranceusr_route_hdr, NULL, lowranceusr_waypt_disp);
  }

  if (NumWaypoints && writeasicons) {
    gbfputint16(NumWaypoints, file_out);
    waypt_disp_all(lowranceusr_write_icon);
  } else {
    NumIcons = 0;
    gbfputint16(NumIcons, file_out);
  }

  /* Track support added 6/21/05 */
  NumTrails = track_count();

  if (NumTrails && merge) {
    NumTrails = 1;
    gbfputint16(NumTrails, file_out);
    trail_point_count = 0;
    trail_count = 0;
    /* count the number of total track points */
    track_disp_all(lowranceusr_merge_track_hdr, lowranceusr_merge_track_tlr, NULL);
    /* write out the new track header */
    trail_point_count = 0;
    track_disp_all(lowranceusr_merge_track_hdr_2, NULL, lowranceusr_track_disp);

  } else {

    gbfputint16(NumTrails, file_out);

    if (global_opts.debug_level >= 1) {
      printf(MYNAME " data_write: Num tracks = %d\n", NumTrails);
    }

    if (NumTrails) {
      trail_count=0;
      track_disp_all(lowranceusr_track_hdr, NULL, lowranceusr_track_disp);
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
  NULL,
  lowranceusr_args,
  CET_CHARSET_ASCII, 0	/* CET-REVIEW */
};
