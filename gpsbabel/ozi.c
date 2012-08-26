/*
    OziExplorer Waypoints/Tracks/Routes
    Comma Delimited

    As described in OziExplorer Help File

    Copyright (C) 2002-2005 Robert Lipe, robertlipe@usa.net

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

#include "defs.h"
#include "csv_util.h"
#include "jeeps/gpsmath.h"
#include <ctype.h>
#include <math.h>                /* for floor */

#define MYNAME        "OZI"
#define BADCHARS	",\r\n"
#define DAYS_SINCE_1990	25569

typedef struct {
  format_specific_data fs;
  int fgcolor;
  int bgcolor;
} ozi_fsdata;


static gbfile *file_in, *file_out;
static short_handle mkshort_handle;
static route_head *trk_head;
static route_head *rte_head;

static int track_out_count;
static int route_out_count;
static int route_wpt_count;
static int new_track;

static char *snlenopt = NULL;
static char *snwhiteopt = NULL;
static char *snupperopt = NULL;
static char *snuniqueopt = NULL;
static char *wptfgcolor = NULL;
static char *wptbgcolor = NULL;
static char *pack_opt = NULL;
static int datum;
static char *proximityarg = NULL;
static double proximity;
static char *altunit_opt;
static char *proxunit_opt;
static char altunit;
static char proxunit;
static double alt_scale;
static double prox_scale;

static
arglist_t ozi_args[] = {
  {
    "pack", &pack_opt, "Write all tracks into one file",
    NULL, ARGTYPE_BOOL, ARG_NOMINMAX
  },
  {
    "snlen", &snlenopt, "Max synthesized shortname length",
    "32", ARGTYPE_INT, "1", NULL
  },
  {
    "snwhite", &snwhiteopt, "Allow whitespace synth. shortnames",
    NULL, ARGTYPE_BOOL, ARG_NOMINMAX
  },
  {
    "snupper", &snupperopt, "UPPERCASE synth. shortnames",
    NULL, ARGTYPE_BOOL, ARG_NOMINMAX
  },
  {
    "snunique", &snuniqueopt, "Make synth. shortnames unique",
    NULL, ARGTYPE_BOOL, ARG_NOMINMAX
  },
  {
    "wptfgcolor", &wptfgcolor, "Waypoint foreground color",
    "black", ARGTYPE_STRING, ARG_NOMINMAX
  },
  {
    "wptbgcolor", &wptbgcolor, "Waypoint background color",
    "yellow", ARGTYPE_STRING, ARG_NOMINMAX
  },
  {
    "proximity", &proximityarg, "Proximity distance",
    "0", ARGTYPE_STRING, ARG_NOMINMAX
  },
  {
    "altunit", &altunit_opt, "Unit used in altitude values",
    "feet", ARGTYPE_STRING, ARG_NOMINMAX
  },
  {
    "proxunit", &proxunit_opt, "Unit used in proximity values",
    "miles", ARGTYPE_STRING, ARG_NOMINMAX
  },
  ARG_TERMINATOR
};

static gpsdata_type ozi_objective;

static char *ozi_ofname = NULL;

static void
ozi_copy_fsdata(ozi_fsdata **dest, ozi_fsdata *src)
{
  /* No strings to mess with.  Straight forward copy. */
  *dest = (ozi_fsdata *)xmalloc(sizeof(*src));
  **dest = *src;
  (*dest)->fs.next = NULL;
}

static void
ozi_free_fsdata(void *fsdata)
{
  xfree(fsdata);
}

static
ozi_fsdata *
ozi_alloc_fsdata(void)
{
  ozi_fsdata *fsdata = (ozi_fsdata*) xcalloc(sizeof(*fsdata), 1);
  fsdata->fs.type = FS_OZI;
  fsdata->fs.copy = (fs_copy) ozi_copy_fsdata;
  fsdata->fs.destroy = ozi_free_fsdata;
  fsdata->fs.convert = NULL;

  /* Provide defaults via command line defaults */
  fsdata->fgcolor = color_to_bbggrr(wptfgcolor);
  fsdata->bgcolor = color_to_bbggrr(wptbgcolor);

  return fsdata;
}

void
ozi_get_time_str(const waypoint *waypointp, char *buff, gbsize_t buffsz)
{
  if (waypointp->creation_time) {
    double time = (waypt_time(waypointp) / SECONDS_PER_DAY) + DAYS_SINCE_1990;
    snprintf(buff, buffsz, "%.7f", time);
  } else {
    *buff = '\0';
  }
}

void
ozi_set_time_str(const char *str, waypoint *waypointp)
{
  double ozi_time;
  char *dot;
  int len;

  ozi_time = atof(str);
  waypointp->creation_time = (ozi_time - DAYS_SINCE_1990) * SECONDS_PER_DAY;

  dot = strchr(str, '.');
  /* get number of characters after dot */
  len = (dot) ? strlen(str) - (dot - str) - 1 : 0;
  if (len >= 7) {
    /* with default ozi time precision (%.7f) we can only handle tenths of second */
    ozi_time -= ((double)waypointp->creation_time / SECONDS_PER_DAY) + DAYS_SINCE_1990;
    ozi_time *= SECONDS_PER_DAY;
    waypointp->microseconds = (ozi_time * 10) + 0.5;
    if (waypointp->microseconds == 10) {
      waypointp->creation_time++;
      waypointp->microseconds = 0;
    }
    waypointp->microseconds *= 100000;
  }
}

static void
ozi_convert_datum(waypoint *wpt)
{
  if (datum != DATUM_WGS84) {
    double lat, lon, alt;
    GPS_Math_Known_Datum_To_WGS84_M(wpt->latitude, wpt->longitude, 0.0,
                                    &lat, &lon, &alt, datum);
    wpt->latitude = lat;
    wpt->longitude = lon;
  }
}

static void
ozi_openfile(char *fname)
{
  char *c, *cx, *tmpname;
  char *ozi_extensions[] = {0, "plt", "wpt", "rte"};
  char buff[32];

  /* if we're doing multi-track output, sequence the filenames like:
   * mytrack.plt, mytrack-1.plt...unless we're writing to stdout.
   */

  if (0 == strcmp(fname, "-")) {
    if (! file_out) {
      file_out = gbfopen(fname, "wb", MYNAME);
    }
    return;
  }

  if ((track_out_count) && (ozi_objective == trkdata)) {
    sprintf(buff, "-%d", track_out_count);
  } else {
    buff[0] = '\0';
  }

  /* remove extension and add buff + ozi's extension */
  c = strrchr(fname, '.');
  if (c && (cx = strrchr(fname, '/')) && (cx > c)) {
    c = NULL;
  }
  if (c && (cx = strrchr(fname, '\\')) && (cx > c)) {
    c = NULL;
  }
  if (c == NULL) {
    c = fname + strlen(fname);
  }
  xasprintf(&tmpname, "%*.*s%s.%s", (int)(c - fname),(int)(c - fname), fname, buff, ozi_extensions[ozi_objective]);

  /* re-open file_out with the new filename */
  if (file_out) {
    gbfclose(file_out);
    file_out = NULL;
  }

  file_out = gbfopen(tmpname, "wb", MYNAME);

  xfree(tmpname);

  return;
}

static void
ozi_track_hdr(const route_head * rte)
{
  static char *ozi_trk_header =
    "OziExplorer Track Point File Version 2.1\r\n"
    "WGS 84\r\n"
    "Altitude is in %s\r\n"
    "Reserved 3\r\n"
    "0,2,255,%s,0,0,2,8421376\r\n"
    "0\r\n";

  if ((! pack_opt) || (track_out_count == 0)) {
    ozi_openfile(ozi_ofname);
    gbfprintf(file_out, ozi_trk_header,
              altunit == 'f' ? "Feet" : "Meters",
              rte->rte_name ? rte->rte_name : "ComplimentsOfGPSBabel");
  }

  track_out_count++;
  new_track = 1;
}

static void
ozi_track_disp(const waypoint * waypointp)
{
  double alt;
  char ozi_time[16];

  ozi_get_time_str(waypointp, ozi_time, sizeof(ozi_time));

  if (waypointp->altitude == unknown_alt) {
    alt = -777;
  } else {
    alt = waypointp->altitude * alt_scale;
  }

  gbfprintf(file_out, "%.6f,%.6f,%d,%.0f,%s,,\r\n",
            waypointp->latitude, waypointp->longitude, new_track,
            alt, ozi_time);

  new_track = 0;
}

static void
ozi_track_tlr(const route_head * rte)
{
}

static void
ozi_track_pr()
{
  track_disp_all(ozi_track_hdr, ozi_track_tlr, ozi_track_disp);
}

static void
ozi_route_hdr(const route_head * rte)
{
  static char *ozi_route_header =
    "OziExplorer Route File Version 1.0\r\n"
    "WGS 84\r\n"
    "Reserved 1\r\n"
    "Reserved 2\r\n";

  /* prologue on 1st pass only */
  if (route_out_count == 0) {
    gbfprintf(file_out, ozi_route_header);
  }

  route_out_count++;
  route_wpt_count = 0;

  /*
   * Route Record
   * Field 1 : R - indicating route details
   * Field 2 : Number - this is the location in the array, must be unique, usually start at 0 for Garmins 1 for other and increment.
   * Field 3 : Name - the waypoint name, use the correct length name to suit the GPS type.
   * Field 4 : Description.
   * Field 5 : Route Color as displayed on map (RGB).
   *
   * R, 0,R0 ,,255
   * R, 1, ICP GALHETA,, 16711680
   */

  gbfprintf(file_out, "R,%d,%s,%s,\r\n",
            route_out_count,
            rte->rte_name ? rte->rte_name : "",
            rte->rte_desc ? rte->rte_desc : "");

}

static void
ozi_route_disp(const waypoint * waypointp)
{
  double alt;
  char ozi_time[16];

  route_wpt_count++;

  ozi_get_time_str(waypointp, ozi_time, sizeof(ozi_time));

  if (waypointp->altitude == unknown_alt) {
    alt = -777;
  } else {
    alt = waypointp->altitude * alt_scale;
  }

  /*
   *   Field 1 : W - indicating route waypoint details.
   *   Field 2 : Route Number - location in array of routes
   *   Field 3 : Number - this is the location in the array of route waypoints, this field is now ignored.
   *   Field 4 : Wp Number - this is the number of the waypoint (the Wp number within the GPS for lowrances)
   *   Field 5 : Name - the waypoint name, use the correct length name to suit the GPS type.
   *   Field 6 : Latitude - decimal degrees.
   *   Field 7 : Longitude - decimal degrees.
   *   Field 8 : Date - see Date Format below, if blank a preset date will be used
   *   Field 9 : Symbol - 0 to number of symbols in GPS
   *   Field 10 : Status - always set to 1
   *   Field 11 : Map Display Format
   *   Field 12 : Foreground Color (RGB value)
   *   Field 13 : Background Color (RGB value)
   *   Field 14 : Description (max 40), no commas
   *   Field 15 : Pointer Direction
   *   Field 16 : Garmin Display Format
   *
   * W,1,7,7,007,-25.581670,-48.316660,36564.54196,10,1,4,0,65535,TR ILHA GALHETA,0,0
   */

  gbfprintf(file_out, "W,%d,%d,,%s,%.6f,%.6f,%s,0,1,3,0,65535,%s,0,0\r\n",
            route_out_count,
            route_wpt_count,
            waypointp->shortname ? waypointp->shortname : "",
            waypointp->latitude,
            waypointp->longitude,
            ozi_time,
            waypointp->description ? waypointp->description : "");

}

static void
ozi_route_tlr(const route_head * rte)
{
}

static void
ozi_route_pr()
{
  route_disp_all(ozi_route_hdr, ozi_route_tlr, ozi_route_disp);
}

static void
ozi_init_units(const int direction)	/* 0 = in; 1 = out */
{
  altunit = tolower(*altunit_opt);
  switch (altunit) {
  case 'm': /* meters, okay */
    alt_scale = 1.0;
    break;
  case 'f': /* feet, okay */
    alt_scale = FEET_TO_METERS(1.0);
    break;
  default:
    fatal(MYNAME ": Unknown value (%s) for option 'altunit'!\n", altunit_opt);
  }
  if (direction != 0) {
    alt_scale = 1 / alt_scale;
  }

  proxunit = tolower(*proxunit_opt);
  switch (proxunit) {
  case 'm': /* miles, okay */
    prox_scale = MILES_TO_METERS(1.0);
    break;
  case 'n': /* nautical miles, okay */
    prox_scale = NMILES_TO_METERS(1.0);
    break;
  case 'k': /* kilometers, okay */
    prox_scale = 1000.0;
    break;
  default:
    fatal(MYNAME ": Unknown value (%s) for option 'proxunit'!\n", proxunit_opt);
  }
  if (direction != 0) {
    prox_scale = 1 / prox_scale;
  }
}

static void
rd_init(const char *fname)
{
  file_in = gbfopen(fname, "rb", MYNAME);

  mkshort_handle = mkshort_new_handle();
  ozi_init_units(0);
}

static void
rd_deinit(void)
{
  gbfclose(file_in);
  file_in = NULL;
  mkshort_del_handle(&mkshort_handle);
}

static void
wr_init(const char *fname)
{

  /* At this point, we have no idea whether we'll be writing waypoint,
   * route, or tracks.  So we'll hold off opening any files until
   * we're actually ready to write.
   */

  ozi_ofname = (char *)fname;

  mkshort_handle = mkshort_new_handle();

  /* set mkshort options from the command line if applicable */
  if (global_opts.synthesize_shortnames) {

    setshort_length(mkshort_handle, atoi(snlenopt));

    if (snwhiteopt) {
      setshort_whitespace_ok(mkshort_handle, atoi(snwhiteopt));
    }

    if (snupperopt) {
      setshort_mustupper(mkshort_handle, atoi(snupperopt));
    }

    if (snuniqueopt) {
      setshort_mustuniq(mkshort_handle, atoi(snuniqueopt));
    }

    setshort_badchars(mkshort_handle, "\",");
  }

  ozi_init_units(1);
  parse_distance(proximityarg, &proximity, 1 / prox_scale, MYNAME);

  file_out = NULL;
}

static void
wr_deinit(void)
{
  if (file_out != NULL) {

    gbfclose(file_out);
    file_out = NULL;
  }
  ozi_ofname = NULL;

  mkshort_del_handle(&mkshort_handle);
}

static void
ozi_parse_waypt(int field, char *str, waypoint * wpt_tmp, ozi_fsdata *fsdata)
{
  double alt;

  if (*str == '\0') {
    return;
  }

  switch (field) {
  case 0:
    /* sequence # */
    break;
  case 1:
    /* waypoint name */
    wpt_tmp->shortname = csv_stringtrim(str, "", 0);
    break;
  case 2:
    /* degrees latitude */
    wpt_tmp->latitude = atof(str);
    break;
  case 3:
    /* degrees longitude */
    wpt_tmp->longitude = atof(str);
    break;
  case 4:
    /* DAYS since 1900 00:00:00 in days.days (5.5) */
    ozi_set_time_str(str, wpt_tmp);
    break;
  case 5:
    /* icons 0-xx.   Ozi seems to use some kind of internal tables to
    pick numbers for icons based on GPS type.  We don't know what those
       tables are, so we read just the numbers.  This converts badly to
       other types, but it at least maintains fidelity for an ozi->ozi
       operation. */
    if (str && isdigit(str[0])) {
      wpt_tmp->icon_descr = xstrdup(str);
      wpt_tmp->wpt_flags.icon_descr_is_dynamic = 1;
    }
    break;
  case 6:
    /* unknown - always 1 */
    break;
  case 7:
    /* display format options 0-8 */
    break;
  case 8:
    /* foreground color (0=black) */
    fsdata->fgcolor = atoi(str);
    break;
  case 9:
    /* background color (65535=yellow) */
    fsdata->bgcolor = atoi(str);
    break;
  case 10:
    /* Description */
    wpt_tmp->description = csv_stringtrim(str, "", 0);
    break;
  case 11:
    /* pointer direction 0,1,2,3 bottom,top,left,right */
    break;
  case 12:
    /* garmin gps display flags (0-name w/sym, 1-sym only, 2-comment w/symbol */
    break;
  case 13:
    /* proximity distance - meters */
    WAYPT_SET(wpt_tmp, proximity, atof(str) * prox_scale);
    break;
  case 14:
    /* altitude */
    alt = atof(str);
    if (alt == -777) {
      wpt_tmp->altitude = unknown_alt;
    } else {
      wpt_tmp->altitude = alt * alt_scale;
    }
    break;
  case 15:
    /* waypoint text name size */
    break;
  case 16:
    /* bold checkbox (1=bold, default 0) */
    break;
  case 17:
    /* symbol size - 17 default */
    break;
    /*
     * Fields 18-23 were added around version 3.90.4g of
     * Ozi, but aren't documented.   We silently ignore
     * these or any additional fields we don't need.
     */
  default:
    break;
  }
}

static void
ozi_parse_track(int field, char *str, waypoint * wpt_tmp, char *trk_name)
{
  double alt;

  if (*str == '\0') {
    return;
  }

  switch (field) {
  case 0:
    /* latitude */
    wpt_tmp->latitude = atof(str);
    break;
  case 1:
    /* longitude */
    wpt_tmp->longitude = atof(str);
    break;
  case 2:
    /* new track flag */
    if ((atoi(str) == 1) && (trk_head->rte_waypt_ct > 0)) {
      trk_head = route_head_alloc();
      track_add_head(trk_head);
      if (trk_name) {
        trk_head->rte_name = trk_name;
      }
    }
    break;
  case 3:
    /* altitude */
    alt = atof(str);
    if (alt == -777) {
      wpt_tmp->altitude = unknown_alt;
    } else {
      wpt_tmp->altitude = alt * alt_scale;
    }
    break;
  case 4:
    /* DAYS since 1900 00:00:00 in days.days (5.5) */
    ozi_set_time_str(str, wpt_tmp);
    break;
  default:
    break;
  }
}

static void
ozi_parse_routepoint(int field, char *str, waypoint * wpt_tmp)
{
  if (*str == '\0') {
    return;
  }

  switch (field) {
  case 0:
    /* W */
    break;
  case 1:
    /* route # */
    break;
  case 2:
    /* waypoint # -- ignored by ozi */
    break;
  case 3:
    /* waypoint # */
    break;
  case 4:
    /* waypoint name */
    wpt_tmp->shortname = csv_stringclean(str, ",");
    break;
  case 5:
    /* latitude */
    wpt_tmp->latitude = atof(str);
    break;
  case 6:
    /* longitude */
    wpt_tmp->longitude = atof(str);
    break;
  case 7:
    /* DAYS since 1900 00:00:00 in days.days (5.5) */
    ozi_set_time_str(str, wpt_tmp);
    break;
  case 8:
    /* symbol */
    break;
  case 9:
    /* status */
    break;
  case 10:
    /* map display format */
    break;
  case 11:
    /* foreground color (RGB) */
    break;
  case 12:
    /* background color (RGB) */
    break;
  case 13:
    /* description */
    wpt_tmp->description = csv_stringclean(str, ",");
    break;
  default:
    break;
  }
}

static void
ozi_parse_routeheader(int field, char *str, waypoint * wpt_tmp)
{

  switch (field) {
  case 0:
    /* R */
    rte_head = route_head_alloc();
    route_add_head(rte_head);
    break;
  case 1:
    /* route # */
    rte_head->rte_num = atoi(str);
    break;
  case 2:
    /* route name */
    rte_head->rte_name = csv_stringclean(str, ",");
    break;
  case 3:
    /* route description */
    rte_head->rte_desc = csv_stringclean(str, ",");
    break;
  case 4:
    /* route color */
    break;
  default:
    break;
  }
}

static void
data_read(void)
{
  char *buff;
  char *s = NULL;
  char *trk_name = NULL;
  waypoint *wpt_tmp;
  int i;
  int linecount = 0;

  while ((buff = gbfgetstr(file_in))) {

    if ((linecount++ == 0) && file_in->unicode) {
      cet_convert_init(CET_CHARSET_UTF8, 1);
    }

    /*
     * this is particularly nasty.  use the first line of the file
     * to attempt to divine the data type we are parsing
     */
    if (linecount == 1) {
      if (strstr(buff, "Track Point") != NULL) {
        trk_head = route_head_alloc();
        track_add_head(trk_head);
        ozi_objective = trkdata;
      } else if (strstr(buff, "Route File") != NULL) {
        ozi_objective = rtedata;
      } else {
        ozi_objective = wptdata;
      }
    } else if (linecount == 2) {
      datum = GPS_Lookup_Datum_Index(buff);
      if (datum < 0) {
        fatal(MYNAME ": Unsupported datum '%s'.\n", buff);
      }
    } else if (linecount == 3) {
      if (case_ignore_strncmp(buff, "Altitude is in ", 15) == 0) {
        char *unit = &buff[15];
        if (case_ignore_strncmp(unit, "Feet", 4) == 0) {
          altunit = 'f';
          alt_scale = FEET_TO_METERS(1.0);
        } else if (case_ignore_strncmp(unit, "Meter", 5) == 0) {
          altunit = 'm';
          alt_scale = 1.0;
        } else {
          fatal(MYNAME ": Unknown unit (%s) used by altitude values!\n", unit);
        }
      }
    } else if ((linecount == 5) && (ozi_objective == trkdata)) {
      int field = 0;
      s = csv_lineparse(buff, ",", "", linecount);
      while (s) {
        field ++;
        if (field == 4) {
          trk_head->rte_name = xstrdup(lrtrim(s));
        }
        s = csv_lineparse(NULL, ",", "", linecount);
      }
    }

    if ((strlen(buff)) && (strstr(buff, ",") != NULL)) {
      ozi_fsdata *fsdata = ozi_alloc_fsdata();
      wpt_tmp = waypt_new();

      /* data delimited by commas, possibly enclosed in quotes.  */
      s = buff;
      s = csv_lineparse(s, ",", "", linecount);

      i = 0;
      while (s) {
        switch (ozi_objective) {
        case trkdata:
          ozi_parse_track(i, s, wpt_tmp, trk_name);
          break;
        case rtedata:
          if (buff[0] == 'R') {
            ozi_parse_routeheader(i, s, wpt_tmp);
          } else {
            ozi_parse_routepoint(i, s, wpt_tmp);
          }

          break;
        case wptdata:
        case unknown_gpsdata:
          ozi_parse_waypt(i, s, wpt_tmp, fsdata);
          break;
        case posndata:
          fatal(MYNAME ": realtime positioning not supported.\n");
          break;
        }
        i++;
        s = csv_lineparse(NULL, ",", "", linecount);
      }

      switch (ozi_objective) {
      case trkdata:
        if (linecount > 6) {/* skipping over file header */
          ozi_convert_datum(wpt_tmp);
          track_add_wpt(trk_head, wpt_tmp);
        } else {
          waypt_free(wpt_tmp);
        }
        break;
      case rtedata:
        if (linecount > 5) {/* skipping over file header */
          ozi_convert_datum(wpt_tmp);
          route_add_wpt(rte_head, wpt_tmp);
        } else {
          waypt_free(wpt_tmp);
        }
        break;
      case wptdata:
      case unknown_gpsdata:
        if (linecount > 4) {  /* skipping over file header */
          fs_chain_add(&(wpt_tmp->fs),
                       (format_specific_data *) fsdata);
          ozi_convert_datum(wpt_tmp);
          waypt_add(wpt_tmp);
        } else {
          waypt_free(wpt_tmp);
        }
        break;
      case posndata:
        fatal(MYNAME ": realtime positioning not supported.\n");
        break;
      }

    } else {
      /* empty line */
    }

  }
}

static void
ozi_waypt_pr(const waypoint * wpt)
{
  static int index = 0;
  double alt;
  char ozi_time[16];
  char *description;
  char *shortname;
  int faked_fsdata = 0;
  ozi_fsdata *fs = NULL;
  int icon = 0;

  fs = (ozi_fsdata *) fs_chain_find(wpt->fs, FS_OZI);

  if (!fs) {
    fs = ozi_alloc_fsdata();
    faked_fsdata = 1;
  }

  ozi_get_time_str(wpt, ozi_time, sizeof(ozi_time));

  if (wpt->altitude == unknown_alt) {
    alt = -777;
  } else {
    alt = wpt->altitude * alt_scale;
  }

  if ((!wpt->shortname) || (global_opts.synthesize_shortnames)) {
    if (wpt->description) {
      if (global_opts.synthesize_shortnames) {
        shortname = mkshort_from_wpt(mkshort_handle, wpt);
      } else {
        shortname = csv_stringclean(wpt->description, BADCHARS);
      }
    } else {
      /* no description available */
      shortname = xstrdup("");
    }
  } else {
    shortname = csv_stringclean(wpt->shortname, BADCHARS);
  }

  if (!wpt->description) {
    if (shortname) {
      description = csv_stringclean(shortname, BADCHARS);
    } else {
      description = xstrdup("");
    }
  } else {
    description = csv_stringclean(wpt->description, BADCHARS);
  }

  index++;

  if (wpt->icon_descr && isdigit(wpt->icon_descr[0])) {
    icon = atoi(wpt->icon_descr);
  }

  gbfprintf(file_out,
            "%d,%s,%.6f,%.6f,%s,%d,%d,%d,%d,%d,%s,%d,%d,",
            index, shortname, wpt->latitude, wpt->longitude, ozi_time, icon,
            1, 3, fs->fgcolor, fs->bgcolor, description, 0, 0);
  if (WAYPT_HAS(wpt, proximity) && (wpt->proximity > 0)) {
    gbfprintf(file_out, "%.1f,", wpt->proximity * prox_scale);
  } else if (proximity > 0) {
    gbfprintf(file_out,"%.1f,", proximity * prox_scale);
  } else {
    gbfprintf(file_out,"%d,", 0);
  }
  gbfprintf(file_out, "%.0f,%d,%d,%d\r\n", alt, 6, 0, 17);

  xfree(description);
  xfree(shortname);

  if (faked_fsdata) {
    xfree(fs);
  }
}

static void
data_write(void)
{
  static char *ozi_wpt_header =
    "OziExplorer Waypoint File Version 1.1\r\n"
    "WGS 84\r\n"
    "Reserved 2\r\n"
    "Reserved 3\r\n";

  track_out_count = route_out_count = 0;

  if (waypt_count()) {
    ozi_objective = wptdata;
    ozi_openfile(ozi_ofname);
    gbfprintf(file_out, ozi_wpt_header);
    waypt_disp_all(ozi_waypt_pr);
  }

  if (track_count()) {
    ozi_objective = trkdata;
    ozi_track_pr(); /* ozi_track_hdr handles filenames / file_out */
  }

  if (route_count()) {
    ozi_objective = rtedata;
    ozi_openfile(ozi_ofname); /* ozi routes go in one big file */
    ozi_route_pr();
  }

}

ff_vecs_t ozi_vecs = {
  ff_type_file,
  FF_CAP_RW_ALL,
  rd_init,
  wr_init,
  rd_deinit,
  wr_deinit,
  data_read,
  data_write,
  NULL,
  ozi_args,
  CET_CHARSET_ASCII, 0	/* CET-REVIEW */
};
