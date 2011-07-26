/*
	Garmin GPS Database Reader/Writer

	Copyright (C) 2005-2008 Olaf Klein, o.b.klein@gpsbabel.org
	Mainly based on mapsource.c,
	Copyright (C) 2005 Robert Lipe, robertlipe@usa.net


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
	History:

	    2005/06/27: initial release (reader only)
	    2005/07/26: added write support
	    2005/07/27: replaced "tricky code" in route reader
	    2005/07/28: fixed handling of single point routes
			new option "via"
			new option "ver"
			fixed compiler warnings
	    2005/07/29: fixed compiler warnings
	    2005/08/04: Read/write URL (reference data changed)
	    2005/08/11: Display sym and name in GDB
	    2005/08/12: Neuter proximity and depth for now
	    2005/08/29: big CET merge
	    2005/09/13: Make sure routes have unique wpt names
	    2005/10/10: MSVC fixes from Andrew
	    2005/10/17: RJL: Tighten up types of a short handle.  It's now a "real" type and not a void *
	    2005/10/31: RJL: Add v3 format, min/max, provide defaults, data types, etc
	    2005/11/09: RJL: Clarify help text for dropping via points
	    2005/12/01: changed waypt's URL to descr for hidden waypoints (-> reference data changed)
	                removed unused procedure gdb_add_to_hidden
	    2005/12/04: additional testo sequences
	    2006/02/24: last field of a route is rte url
	    2006/02/25: rte_read_loop: zero check replaced with a dummy read (8 unknown bytes)
	    2006/03/05: first implementation of Garmin special data (garmin_fs)
	    2006/04/04: Use track_add_wpt for all tracks
	    2006/04/19: add url i/o to tracks and routes
	    2006/04/19: check for empty waypoint shortnames (paranioa)
	    2006/11/01: Use version of GPSBabel and date/time of gdb.c (managed by CVS) for watermark
	    2007/01/23: add support for GDB version 3
	    2007/02/07: Add special code for unknown bytes in waypoints with class GE 8 (calculated points)
	    2007/02/15: Nearly full rewrite. Full support for GDB V3. New option roadbook.
	    2007/05/03: Add code for tricky V3 descriptions
	    2007/06/18: Tweak some forgotten "flagged" fields
	    2007/07/07: Better support for new fields since V3 (postal code/street address/instruction)
	    2008/01/09: Fix handling of option category (cat)
	    2008/04/27: Add zero to checklist of "unknown bytes"
	    2008/08/17: Add concept of route/track line colors
	    2008/09/11: Make format 'pipeable' (cached writes using gbfile memapi)
*/

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <time.h>

#include "defs.h"

#include "cet.h"
#include "cet_util.h"
#include "csv_util.h"
#include "garmin_fs.h"
#include "garmin_tables.h"
#include "grtcirc.h"
#include "jeeps/gpsmath.h"

#define MYNAME "gdb"

#define GDB_VER_1		1
#define GDB_VER_2		2
#define GDB_VER_3		3

#define GDB_VER_UTF8		GDB_VER_3
#define GDB_VER_MIN		GDB_VER_1
#define GDB_VER_MAX		GDB_VER_3

#define GDB_DEF_CLASS		gt_waypt_class_user_waypoint
#define GDB_DEF_HIDDEN_CLASS	gt_waypt_class_map_point
#define GDB_DEF_ICON		18

#define GDB_NAME_BUFFERLEN	1024

#define GDB_DBG_WPT		1
#define GDB_DBG_RTE		2
#define GDB_DBG_TRK		4

#define GDB_DBG_WPTe		8
#define GDB_DBG_RTEe		16
#define GDB_DBG_TRKe		32

#define GDB_DEBUG		(GDB_DBG_WPTe) /* | GDB_DBG_RTE) */
#undef GDB_DEBUG
// #define GDB_DEBUG 0xff

#define DBG(a,b)		if ((GDB_DEBUG & (a)) && (b))

/*******************************************************************************/

/* static char gdb_release[] = "$Revision: 1.74 $"; */
static char gdb_release_date[] = "$Date: 2011-04-14 01:30:01 $";

static gbfile* fin, *fout, *ftmp;
static int gdb_ver, gdb_category, gdb_via, gdb_roadbook;

static queue wayptq_in, wayptq_out, wayptq_in_hidden;
static short_handle short_h;

static char* gdb_opt_category;
static char* gdb_opt_ver;
static char* gdb_opt_via;
static char* gdb_opt_roadbook;
static char* gdb_opt_bitcategory;

static int waypt_flag;
static int route_flag;

static int waypt_ct;	/* informational: total number of waypoints in/out */
static int waypth_ct;	/* informational: total number of hidden waypoints in/out */
static int rtept_ct;	/* informational: total number of route points in/out */
static int trkpt_ct;	/* informational: total number of track points in/out */
static int rte_ct;	/* informational: total number of routes in/out */
static int trk_ct;	/* informational: total number of tracks in/out */

/*******************************************************************************/

#define ELEMENTS(a) a->rte_waypt_ct
#define NOT_EMPTY(a) (a && *a)

static void
gdb_flush_waypt_queue(queue* Q)
{
  queue* elem, *tmp;

  QUEUE_FOR_EACH(Q, elem, tmp) {
    waypoint* wpt = (waypoint*)elem;
    dequeue(elem);
    if (wpt->extra_data) {
      xfree(wpt->extra_data);
    }
    waypt_free(wpt);
  }
}


#if GDB_DEBUG
static void
disp_summary(const gbfile* f)
{
  int i, len;

  len = strlen(f->name);

  warning(MYNAME ": =====================");
  for (i = 0; i < len; i++) {
    warning("=");
  }
  warning("\n" MYNAME ": %s summary for \"%s\"\n",
          (f->mode == 'r') ? "Reader" : "Writer", f->name);

  warning(MYNAME ": ---------------------");
  for (i = 0; i < len; i++) {
    warning("-");
  }

  warning("\n" MYNAME ": %d waypoint(s)\n", waypt_ct - waypth_ct);
  warning(MYNAME ": %d hidden waypoint(s)\n", waypth_ct);
  warning(MYNAME ": %d route(s) with total %d point(s)\n", rte_ct, rtept_ct);
  warning(MYNAME ": %d track(s) with total %d point(s)\n", trk_ct, trkpt_ct);
  warning(MYNAME ": ---------------------");

  for (i = 0; i < len; i++) {
    warning("-");
  }
  warning("\n");
}
#else
#define disp_summary(a)
#endif

/*******************************************************************************/
/* TOOLS AND MACROS FOR THE READER */
/*-----------------------------------------------------------------------------*/

#define FREAD_C gbfgetc(fin)
#define FREAD(a,b) gbfread(a,(b),1,fin)
#define FREAD_i32 gbfgetint32(fin)
#define FREAD_i16 gbfgetint16(fin)
#define FREAD_STR(a) gdb_fread_str(a,sizeof(a),fin)
#define FREAD_CSTR gdb_fread_cstr(fin)
#define FREAD_DBL gbfgetdbl(fin)
#define FREAD_LATLON GPS_Math_Semi_To_Deg(gbfgetint32(fin))

#if GDB_DEBUG
static char*
nice(const char* str)
{
  char* res, *env;
  cet_cs_vec_t* vec;

  if (!(str && *str)) {
    return "";
  }

  env = getenv("LANG");
  if (env == NULL) {
    return (char*)str;
  }

  if ((res = strchr(env, '.'))) {
    env = ++res;
  }
  vec = cet_find_cs_by_name(env);

  if ((vec != NULL) && (vec != global_opts.charset)) {
    static char buf[128];
    res = cet_str_any_to_any(str, global_opts.charset, vec);
    strncpy(buf, res, sizeof(buf));
    xfree(res);
    return buf;
  } else {
    return (char*)str;
  }
}
#endif

static char*
gdb_fread_cstr(gbfile* fin)
{
  char* result = gbfgetcstr(fin);

  if (result && (*result == '\0')) {
    xfree(result);
    result = NULL;
  }
  return result;
}

static int
gdb_fread_str(char* buf, int size, gbfile* fin)
{
  char c;
  int res = 0;

  while (size--) {
    gbfread(&c, 1, 1, fin);
    buf[res] = c;
    if (c == '\0') {
      return res;
    }
    res++;
  }
  buf[res] = '\0';
  return res;
}

static char*
gdb_fread_strlist(void)
{
  char* res = NULL;
  int count;

  count = FREAD_i32;

  while (count > 0) {
    char* str = FREAD_CSTR;
    if (str != NULL) {
      if (*str && (res == NULL)) {
        res = str;
      } else {
        xfree(str);
      }
    }
    count--;
  }

  return res;
}

static waypoint*
gdb_find_wayptq(const queue* Q, const waypoint* wpt, const char exact)
{
  queue* elem, *tmp;
  const char* name = wpt->shortname;

  QUEUE_FOR_EACH(Q, elem, tmp) {
    waypoint* tmp = (waypoint*)elem;
    if (case_ignore_strcmp(name, tmp->shortname) == 0) {

      if (! exact) {
        return tmp;
      }

      if ((tmp->latitude == wpt->latitude) &&
          (tmp->longitude == wpt->longitude)) {
        return tmp;
      }
    }
  }
  return NULL;
}

static waypoint*
gdb_reader_find_waypt(const waypoint* wpt, const char exact)
{
  waypoint* res;
  res = gdb_find_wayptq(&wayptq_in, wpt, exact);
  if (res == NULL) {
    res = gdb_find_wayptq(&wayptq_in_hidden, wpt, exact);
  }
  return res;
}

static waypoint*
gdb_add_route_waypt(route_head* rte, waypoint* ref, const int wpt_class)
{
  waypoint* tmp, *res;
  int turn_point;

  tmp = gdb_reader_find_waypt(ref, 1);
  if (tmp == NULL) {
    double dist;

    tmp = find_waypt_by_name(ref->shortname);
    if (tmp == NULL) {
      route_add_wpt(rte, ref);
      return ref;
    }

    /* At this point we have found a waypoint with same name,
       but probably from another data stream. Check coordinates!
    */
    dist = radtometers(gcdist(
                         RAD(ref->latitude), RAD(ref->longitude),
                         RAD(tmp->latitude), RAD(tmp->longitude)));

    if (fabs(dist) > 100) {
      warning(MYNAME ": Route point mismatch!\n");
      warning(MYNAME ": \"%s\" from waypoints differs to \"%s\"\n",
              tmp->shortname, ref->shortname);
      fatal(MYNAME ": from route table by more than %0.1f meters!\n",
            dist);

    }
  }
  res = NULL;
  turn_point = (gdb_roadbook && (wpt_class > gt_waypt_class_map_point) && tmp->description);
  if (turn_point || (gdb_via == 0) || (wpt_class < gt_waypt_class_map_point)) {
    res = waypt_dupe(tmp);
    route_add_wpt(rte, res);
  }
  waypt_free(ref);
  return res;
}

/*******************************************************************************/
/* TOOLS AND MACROS FOR THE WRITER */
/*-----------------------------------------------------------------------------*/

#define FWRITE_CSTR(a) ((a) == NULL) ? gbfputc(0,fout) : gbfputcstr((a),fout)
#define FWRITE_i16(a) gbfputint16((a),fout)
#define FWRITE_i32(a) gbfputint32((a),fout)
#define FWRITE(a, b) gbfwrite(a,(b),1,fout)
#define FWRITE_C(a) gbfputc((a),fout)
#define FWRITE_DBL(a,b) gdb_write_dbl((a),(b))
#define FWRITE_TIME(a) gdb_write_time((a))
#define FWRITE_CSTR_LIST(a) gdb_write_cstr_list((a))
#define FWRITE_LATLON(a) gbfputint32(GPS_Math_Deg_To_Semi((a)),fout)

static void
gdb_write_cstr_list(const char* str)
{
  if NOT_EMPTY(str) {
    gbfputint32(1, fout);
    gbfputcstr(str, fout);
  } else {
    gbfputint32(0, fout);
  }
}

static void
gdb_write_dbl(const double value, const double def)
{
  if (value == def) {
    gbfputc(0, fout);
  } else {
    gbfputc(1, fout);
    gbfputdbl(value, fout);
  }
}

static void
gdb_write_time(const int time)
{
  if (time > 0) {
    gbfputc(1, fout);
    gbfputint32(time, fout);
  } else {
    gbfputc(0, fout);
  }
}

/*******************************************************************************/
/* GDB "Garmin Database" READER CODE */
/*-----------------------------------------------------------------------------*/

static void
read_file_header(void)
{
  char buf[128];
  int i, reclen;

  /*
  	We are beginning with a simple binary read.
  */
  FREAD(buf, 6);
  /*
  	A "gbfgetcstr" (FREAD_CSTR) works too, but if we get a wrong file as input,
  	the file validation my be comes too late. For example a XML base file normally
  	has no binary zeros inside and produce, if big enought, a buffer overflow.
  	The following message "local buffer overflow detected..." could be
  	misinterpreted.
  */
  is_fatal(strcmp(buf, "MsRcf") != 0, MYNAME ": Invalid file \"%s\"!", fin->name);

  reclen = FREAD_i32;
  i = FREAD_STR(buf);
  is_fatal(buf[0] != 'D', MYNAME ": Invalid file \"%s\"!", fin->name);

  gdb_ver = buf[1] - 'k' + 1;
  is_fatal((gdb_ver < GDB_VER_MIN) || (gdb_ver > GDB_VER_MAX),
           MYNAME ": Unknown or/and unsupported GDB version (%d.0)!", gdb_ver);

  if (global_opts.verbose_status > 0) {
    printf(MYNAME ": Reading Garmin GPS Database version %d.0\n", gdb_ver);
  }

  reclen = FREAD_i32;
  i = FREAD(buf, reclen + 1);
  if (global_opts.verbose_status > 0) {
    char* name = buf+2;
    if (strstr(name, "SQA") == 0) {
      name = "MapSource";
    } else if (strstr(name, "neaderhi") == 0) {
      name = "MapSource BETA";
    }
    warning(MYNAME ": File created with \"%s\"\n", name);
  }

  i = FREAD_STR(buf);
  is_fatal(!(((i == 9) && (strcmp(buf, "MapSource") == 0)) || ((i == 8) && (strcmp(buf, "BaseCamp") == 0))), MYNAME ": Not a recognized signature in header");
}

/*-----------------------------------------------------------------------------*/

static waypoint*
read_waypoint(gt_waypt_classes_e* waypt_class_out)
{
  char buf[128];		/* used for temporary stuff */
  int display, icon, dynamic;
  gt_waypt_classes_e wpt_class;
  int i;
  waypoint* res;
  garmin_fs_t* gmsd;
  char* str;
  char* bufp = buf;
#ifdef GMSD_EXPERIMENTAL
  char subclass[22];
#endif
#if GDB_DEBUG
  char* sn;
#endif
  waypt_ct++;
  res = waypt_new();

  gmsd = garmin_fs_alloc(-1);
  fs_chain_add(&res->fs, (format_specific_data*) gmsd);

  res->shortname = FREAD_CSTR;
#if GDB_DEBUG
  sn = xstrdup(nice(res->shortname));
#endif
  wpt_class = FREAD_i32;
  GMSD_SET(wpt_class, wpt_class);
  if (wpt_class != 0) {
    waypth_ct++;
  }

  FREAD_STR(buf);					/* Country code */
  GMSD_SETSTR(cc, bufp);

#ifdef GMSD_EXPERIMENTAL
  FREAD(subclass, sizeof(subclass));
  if (gmsd && (wpt_class >= gt_waypt_class_map_point)) {
    memcpy(gmsd->subclass, subclass, sizeof(gmsd->subclass));
    gmsd->flags.subclass = 1;
  }
#else
  FREAD(buf, 22);
#endif
  res->latitude = FREAD_LATLON;
  res->longitude = FREAD_LATLON;

  if (FREAD_C == 1) {
    double alt = FREAD_DBL;
    if (alt < 1.0e24) {
      res->altitude = alt;
#if GDB_DEBUG
      DBG(GDB_DBG_WPTe, 1)
      printf(MYNAME "-wpt \"%s\" (%d): Altitude = %.1f\n",
             sn, wpt_class, alt);
#endif
    }
  }
#if GDB_DEBUG
  DBG(GDB_DBG_WPT, 1)
  printf(MYNAME "-wpt \"%s\": coordinates = %c%0.6f %c%0.6f\n",
         sn,
         res->latitude < 0 ? 'S' : 'N', res->latitude,
         res->longitude < 0 ? 'W' : 'E', res->longitude);
#endif
  res->notes = FREAD_CSTR;
#if GDB_DEBUG
  DBG(GDB_DBG_WPTe, res->notes) {
    char* str = gstrsub(res->notes, "\r\n", ", ");
    printf(MYNAME "-wpt \"%s\" (%d): notes = %s\n",
           sn, wpt_class, nice(str));
    xfree(str);
  }
#endif
  if (FREAD_C == 1) {
    WAYPT_SET(res, proximity, FREAD_DBL);
#if GDB_DEBUG
    DBG(GDB_DBG_WPTe, 1)
    printf(MYNAME "-wpt \"%s\" (%d): Proximity = %.1f\n",
           sn, wpt_class, res->proximity / 1000);
#endif
  }
  i = FREAD_i32;
#if GDB_DEBUG
  DBG(GDB_DBG_WPTe, i)
  printf(MYNAME "-wpt \"%s\" (%d): display = %d\n",
         sn, wpt_class, i);
#endif
  switch (i) {			/* display value */
  case gt_gdb_display_mode_symbol:
    display = gt_display_mode_symbol;
    break;
  case gt_gdb_display_mode_symbol_and_comment:
    display = gt_display_mode_symbol_and_comment;
    break;
  default:
    display = gt_display_mode_symbol_and_name;
    break;
  }
  GMSD_SET(display, display);

  FREAD_i32;				/* color !not implemented! */
  icon = FREAD_i32;
  GMSD_SET(icon, icon);			/* icon */
  FREAD_STR(buf);				/* city */
  GMSD_SETSTR(city, bufp);
  FREAD_STR(buf);				/* state */
  GMSD_SETSTR(state, bufp);
  FREAD_STR(buf);				/* facility */
  GMSD_SETSTR(facility, bufp);

  FREAD(buf, 1);

  if (FREAD_C == 1) {
    WAYPT_SET(res, depth, FREAD_DBL);
#if GDB_DEBUG
    DBG(GDB_DBG_WPTe, 1)
    printf(MYNAME "-wpt \"%s\" (%d): Depth = %.1f\n",
           sn, wpt_class, res->depth);
#endif
  }

  /* VERSION DEPENDENT CODE */

  if (gdb_ver <= GDB_VER_2) {
    char* temp;

    FREAD(buf, 2);				/* ?????????????????????????????????? */
    waypt_flag = FREAD_C;
    if (waypt_flag == 0) {
      FREAD(buf, 3);
    } else {
      FREAD(buf, 2);
    }

    temp = FREAD_CSTR;				/* undocumented & unused string */
#if GDB_DEBUG
    DBG(GDB_DBG_WPTe, temp)
    printf(MYNAME "-wpt \"%s\" (%d): Unknown string = %s\n",
           sn, wpt_class, nice(temp));
#endif
    if (temp) {
      xfree(temp);
    }

    res->url = FREAD_CSTR;
    if (wpt_class != 0) {
      res->description = res->url;
      res->url = NULL;
    }
  } else { // if (gdb_ver >= GDB_VER_3)
    int i, url_ct;

    waypt_flag = 0;

    FREAD_STR(buf);				/* street address */
    GMSD_SETSTR(addr, bufp);

    FREAD(buf, 5);				/* instruction depended */
    res->description = FREAD_CSTR;		/* instruction */

    url_ct = FREAD_i32;
    for (i = url_ct; (i); i--) {
      char* str = FREAD_CSTR;
      if (str && *str) {
        waypt_add_url(res, str, NULL);
#if GDB_DEBUG
        DBG(GDB_DBG_WPTe, 1)
        printf(MYNAME "-wpt \"%s\" (%d): url(%d) = %s\n",
               sn, wpt_class, url_ct - i, str);
#endif
      }
    }
  }

#if GDB_DEBUG
  DBG(GDB_DBG_WPTe, res->description)
  printf(MYNAME "-wpt \"%s\" (%d): description = %s\n",
         sn, wpt_class, nice(res->description));
  DBG(GDB_DBG_WPTe, res->url)
  printf(MYNAME "-wpt \"%s\" (%d): url = %s\n",
         sn, wpt_class, nice(res->url));
#endif
  i = FREAD_i16;
  if (i != 0) {
    GMSD_SET(category, i);
  }
#if GDB_DEBUG
  DBG(GDB_DBG_WPTe, i)
  printf(MYNAME "-wpt \"%s\" (%d): category = %d\n",
         sn, wpt_class, i);
#endif

  if (FREAD_C == 1) {
    WAYPT_SET(res, temperature, FREAD_DBL);
#if GDB_DEBUG
    DBG(GDB_DBG_WPTe, 1)
    printf(MYNAME "-wpt \"%s\" (%d): temperature = %.1f\n",
           sn, wpt_class, res->temperature);
#endif
  }

  /* VERSION DEPENDENT CODE */
  if (gdb_ver <= GDB_VER_2) {
    if (waypt_flag != 0) {
      FREAD(buf, 1);
    }
  }
  if (FREAD_C == 1) {
    res->creation_time = FREAD_i32;
  }

  /* VERSION DEPENDENT CODE */
  if (gdb_ver >= GDB_VER_3) {
    if (FREAD_i32 == 1) {
      FREAD_STR(buf);		/* phone number */
      GMSD_SETSTR(phone_nr, bufp);
      FREAD_STR(buf);		/* ?? fax / mobile ?? */
    }
    FREAD_STR(buf);			/* country */
    GMSD_SETSTR(country, bufp);
    FREAD_STR(buf);			/* postal code */
    GMSD_SETSTR(postal_code, bufp);
  }

  res->icon_descr = gt_find_desc_from_icon_number(icon, GDB, &dynamic);
  res->wpt_flags.icon_descr_is_dynamic = dynamic;

#if GDB_DEBUG
  DBG(GDB_DBG_WPTe, icon != GDB_DEF_ICON)
  printf(MYNAME "-wpt \"%s\" (%d): icon = \"%s\" (MapSource symbol %d)\n",
         sn, wpt_class, nice(res->icon_descr), icon);
#endif
  if ((str = GMSD_GET(cc, NULL))) {
    if (! GMSD_HAS(country)) {
      GMSD_SETSTR(country, gt_get_icao_country(str));
    }
  }

  if (gdb_roadbook && (wpt_class > gt_waypt_class_map_point) && res->description) {
    wpt_class = gt_waypt_class_user_waypoint;
    GMSD_SET(wpt_class, wpt_class);
#ifdef GMSD_EXPERIMENTAL
    GMSD_UNSET(subclass);
#endif
  }
#if GDB_DEBUG
  xfree(sn);
#endif
  *waypt_class_out = wpt_class;
  return res;
}

/*-----------------------------------------------------------------------------*/

static route_head*
read_route(void)
{
  route_head* rte;
  int points, warnings, links, i;
  char buf[128];
  bounds bounds;
  int color_idx;

  rte_ct++;
  warnings = 0;

  rte = route_head_alloc();
  rte->rte_name = FREAD_CSTR;
  FREAD(buf, 1);			/* display/autoname - 1 byte */

  if (FREAD_C == 0) {		/* max. data flag */
    /* maxlat = */ (void) FREAD_i32;
    /* maxlon = */
    (void) FREAD_i32;
    if (FREAD_C == 1) { /* maxalt = */
      FREAD_DBL;
    }
    /* minlat = */ (void) FREAD_i32;
    /* minlon = */
    (void) FREAD_i32;
    if (FREAD_C == 1) { /* minalt = */
      FREAD_DBL;
    }
  }

  links = 0;
  points = FREAD_i32;

#if GDB_DEBUG
  DBG(GDB_DBG_RTE, 1)
  printf(MYNAME "-rte \"%s\": loading route with %d point(s)...\n",
         nice(rte->rte_name), points);
#endif

  for (i = 0; i < points; i++) {
    int wpt_class, j;
    char buf[128];
    garmin_ilink_t* il_root, *il_anchor;

    waypoint* wpt;

    wpt = waypt_new();
    rtept_ct++;

    wpt->shortname = FREAD_CSTR;	/* shortname */
    wpt_class = FREAD_i32;		/* waypoint class */
    FREAD_STR(buf);			/* country code */
    FREAD(buf, 18 + 4);		/* subclass part 1-3 / unknown */

    if (FREAD_C != 0) {
      FREAD(buf, 8);		/* aviation data (?); only seen with class "1" (Airport) */
      /* VERSION DEPENDENT CODE */
      if (gdb_ver >= GDB_VER_3) {
        FREAD(buf, 8);  /* a second block since V3 */
      }
    }

    FREAD(buf, 18);			/* unknown 18 bytes; but first should be 0x01 or 0x03 */
    /* seen also 0 with VER3 */
    if ((buf[0] != 0x00) && (buf[0] != 0x01) && (buf[0] != 0x03)) {
      int i;

      warnings++;
      if (warnings > 3) {
        fatal(MYNAME "-rte_pt \"%s\": too many warnings!\n", wpt->shortname);
      }
      warning(MYNAME "-rte_pt \"%s\" (class %d): possible error in route.\n", wpt->shortname, wpt_class);
      warning(MYNAME "-rte_pt (dump):");
      for (i = 0; i < 18; i++) {
        warning(" %02x", (unsigned char)buf[i]);
      }
      warning("\n");
    }

    links = FREAD_i32;
    il_anchor = NULL;
    il_root = NULL;
#if GDB_DEBUG
    DBG(GDB_DBG_RTE, links)
    printf(MYNAME "-rte_pt \"%s\" (%d): %d interlink step(s)\n",
           nice(wpt->shortname), wpt_class, links);
#endif
    for (j = 0; j < links; j++) {
      garmin_ilink_t* il_step = (garmin_ilink_t*) xmalloc(sizeof(*il_step));

      il_step->ref_count = 1;

      il_step->lat = FREAD_LATLON;
      il_step->lon = FREAD_LATLON;
      if (FREAD_C == 1) {
        il_step->alt = FREAD_DBL;
      } else {
        il_step->alt = unknown_alt;
      }

      if (j == 0) {
        wpt->latitude = il_step->lat;
        wpt->longitude = il_step->lon;
        wpt->altitude = il_step->alt;
      }

      il_step->next = NULL;
      if (il_anchor == NULL) {
        il_root = il_step;
      } else {
        il_anchor->next = il_step;
      }
      il_anchor = il_step;

#if GDB_DEBUG
      DBG(GDB_DBG_RTEe, 1) {
        printf(MYNAME "-rte_il \"%s\" (%d of %d): %c%0.6f %c%0.6f\n",
               nice(wpt->shortname), j + 1, links,
               il_step->lat < 0 ? 'S' : 'N', il_step->lat,
               il_step->lon < 0 ? 'W' : 'E', il_step->lon);
      }
#endif
    }

    waypt_init_bounds(&bounds);

    if (FREAD_C == 0) {		/* interlink bounds */
      bounds.max_lat = FREAD_LATLON;
      bounds.max_lon = FREAD_LATLON;
      if (FREAD_C == 1) {
        bounds.max_alt = FREAD_DBL;
      }
      bounds.min_lat = FREAD_LATLON;
      bounds.min_lat = FREAD_LATLON;
      if (FREAD_C == 1) {
        bounds.min_alt = FREAD_DBL;
      }
    }

    if (links == 0) {
      /* Without links we need all informations from wpt */
      waypoint* tmp = gdb_reader_find_waypt(wpt, 0);
      if (tmp != NULL) {
        waypt_free(wpt);
        wpt = waypt_dupe(tmp);
      } else {
        if (waypt_bounds_valid(&bounds)) {
          warning(MYNAME ": (has bounds)\n");
        }

        warning(MYNAME ": Data corruption detected!\n");
        fatal(MYNAME ": Sleeping route point without coordinates!\n");
      }
    }

    /* VERSION DEPENDENT CODE */
    if (gdb_ver >= GDB_VER_2) {
      FREAD(buf, 8);
      if (gdb_ver >= GDB_VER_3) {
        FREAD(buf, 2);
      }
    }
#if GDB_DEBUG
    DBG(GDB_DBG_RTE, 1)
    printf(MYNAME "-rte_pt \"%s\": coordinates = %c%0.6f, %c%0.6f\n",
           nice(wpt->shortname),
           wpt->latitude < 0 ? 'S' : 'N', wpt->latitude,
           wpt->longitude < 0 ? 'W' : 'E', wpt->longitude);
#endif
    wpt = gdb_add_route_waypt(rte, wpt, wpt_class);
    if (wpt != NULL) {
      garmin_fs_t* gmsd = GMSD_FIND(wpt);
      if (gmsd == NULL) {
        gmsd = garmin_fs_alloc(-1);
        fs_chain_add(&wpt->fs, (format_specific_data*) gmsd);
      }
      GMSD_SET(wpt_class, wpt_class);
      gmsd->ilinks = il_root;
      il_root = NULL;
    }

    while (il_root) {
      garmin_ilink_t* il = il_root;
      il_root = il_root->next;
      xfree(il);
    }
  } /* ENDFOR: for (i = 0; i < points; i++) */

  /* VERSION DEPENDENT CODE */
  if (gdb_ver <= GDB_VER_2) {
    rte->rte_url = FREAD_CSTR;
  } else {
    rte->rte_url = gdb_fread_strlist();

    color_idx = FREAD_i32;
    rte->line_color.bbggrr = gt_color_value(color_idx);
    FREAD(buf, 1);			/* ?????????????????????????????????? */

    rte->rte_desc = FREAD_CSTR;
#if 0
    /* replace CRLF's with ", " */
    if (rte->rte_desc) {
      char* c = rte->rte_desc;
      while ((c = strstr(c, "\r\n"))) {
        *c++ = ',';
        *c++ = ' ';
      }
    }
#endif
  }
  return rte;
}

/*-----------------------------------------------------------------------------*/

static route_head*
read_track(void)
{
  route_head* res;
  int points, index;
  char dummy;
  int color_idx;

  trk_ct++;

  res = route_head_alloc();
  res->rte_name = FREAD_CSTR;
//	res->rte_num = trk_ct;

  FREAD(&dummy, 1);		/* display - 1 byte */
  color_idx = FREAD_i32;		/* color -  1 dword */
  res->line_color.bbggrr = gt_color_value(color_idx);

  points = FREAD_i32;

  for (index = 0; index < points; index++) {
    waypoint* wpt = waypt_new();

    trkpt_ct++;

    wpt->latitude = FREAD_LATLON;
    wpt->longitude = FREAD_LATLON;
    if (FREAD_C == 1) {
      double alt = FREAD_DBL;
      if (alt < 1.0e24) {
        wpt->altitude = alt;
      }
    }
    if (FREAD_C == 1) {
      wpt->creation_time = FREAD_i32;
    }
    if (FREAD_C == 1) {
      WAYPT_SET(wpt, depth, FREAD_DBL);
    }
    if (FREAD_C == 1) {
      WAYPT_SET(wpt, temperature, FREAD_DBL);
    }

    track_add_wpt(res, wpt);
  }

  /* VERSION DEPENDENT CODE */
  if (gdb_ver >= GDB_VER_3) {
    res->rte_url = gdb_fread_strlist();
  } else { /* if (gdb_ver <= GDB_VER_2) */
    res->rte_url = FREAD_CSTR;
  }
#if GDB_DEBUG
  DBG(GDB_DBG_TRK, res->rte_url)
  printf(MYNAME "-trk \"%s\": url = %s\n",
         res->rte_name, res->rte_url);
#endif
  return res;
}

/*******************************************************************************/

static void
gdb_rd_init(const char* fname)
{
  fin = gbfopen_le(fname, "rb", MYNAME);
  ftmp = gbfopen_le(NULL, "wb", MYNAME);
  read_file_header();
  /* VERSION DEPENDENT CODE */
  if (gdb_ver >= GDB_VER_UTF8) {
    cet_convert_init(CET_CHARSET_UTF8, 1);
  }

  QUEUE_INIT(&wayptq_in);
  QUEUE_INIT(&wayptq_in_hidden);

  gdb_via = (gdb_opt_via && *gdb_opt_via) ? atoi(gdb_opt_via) : 0;
  gdb_roadbook = (gdb_opt_roadbook && *gdb_opt_roadbook) ? atoi(gdb_opt_roadbook) : 0;
  if (gdb_roadbook) { /* higher priority */
    gdb_via = 1;
  }

  waypt_ct = 0;
  waypth_ct = 0;
  rtept_ct = 0;
  trkpt_ct = 0;
  rte_ct = 0;
  trk_ct = 0;
}

static void
gdb_rd_deinit(void)
{
  disp_summary(fin);
  gdb_flush_waypt_queue(&wayptq_in);
  gdb_flush_waypt_queue(&wayptq_in_hidden);
  gbfclose(ftmp);
  gbfclose(fin);
}

static void
read_data(void)
{
  gbfile* fsave;
  int incomplete = 0;	/* number of incomplete reads */

  for (;;) {
    int len, delta;
    char typ, dump;
    gt_waypt_classes_e wpt_class;
    waypoint* wpt;
    route_head* trk, *rte;

    len = FREAD_i32;
    if (FREAD(&typ, 1) < 1) {
      fatal(MYNAME ": Attempt to read past EOF.");
    }
    if (typ == 'V') {
      break;  /* break the loop */
    }

    gbfrewind(ftmp);
    gbfwrite(NULL, 0, 0, ftmp);	/* truncate */
    gbfcopyfrom(ftmp, fin, len);
    gbfrewind(ftmp);

    fsave = fin;			/* swap standard 'fin' with cached input */
    fin = ftmp;

    dump = 1;
    wpt_class = GDB_DEF_CLASS;

    switch (typ) {
    case 'W':
      wpt = read_waypoint(&wpt_class);
      if ((gdb_via == 0) || (wpt_class == 0)) {
        waypoint* dupe;
        waypt_add(wpt);
        dupe = waypt_dupe(wpt);
        ENQUEUE_TAIL(&wayptq_in, &dupe->Q);
      } else {
        ENQUEUE_TAIL(&wayptq_in_hidden, &wpt->Q);
      }
      break;
    case 'R':
      rte = read_route();
      if (rte) {
        route_add_head(rte);
      }
      break;
    case 'T':
      trk = read_track();
      if (trk) {
        track_add_head(trk);
      }
      break;
    default:
      dump = 0;	/* make a dump only for main types */
      break;
    }

    fin = fsave;
    delta = len - gbftell(ftmp);
    is_fatal(delta > 1000000, "Internal consistency error.  Delta too big");

    // Avoid finite loop on bogus beta files from '06.
    // THe 100000 is totally pulled from my hat.
    // is_fatal((delta > 1000000) || (delta < 0), "Internal GDB error; invalid delta.");

    if (dump && delta) {
      if (! incomplete++) {
        warning(MYNAME ":==========================================\n");
        warning(MYNAME ":===          W A R N I N G             ===\n");
      }
      if (typ == 'W')
        warning(MYNAME ":(%d%c-%02d): delta = %d (flag=%3d/%02x)-",
                gdb_ver, typ, wpt_class, delta, waypt_flag, waypt_flag);
      else {
        warning(MYNAME ":(%d%c): delta = %d -", gdb_ver, typ, delta);
      }
      if (delta > 0) {
        int i;
        char* buf = (char*) xmalloc(delta);
        if (FREAD(buf, delta) < 1) {
          fatal(MYNAME ": Attempt to read past EOF.\n");
        }
        for (i = 0; i < delta; i++) {
          warning(" %02x", (unsigned char)buf[i]);
        }
        xfree(buf);
      }
      warning("\n");
    }
  }


  if (incomplete) {
    warning(MYNAME ":------------------------------------------\n");
    warning(MYNAME ": \"%s\"\n", fin->name);
    warning(MYNAME ":------------------------------------------\n");
    warning(MYNAME ":       Please mail this information\n");
    warning(MYNAME "     and, if you can, the used GDB file\n");
    warning(MYNAME ":  to gpsbabel-misc@lists.sourceforge.net\n");
    warning(MYNAME ":==========================================\n");
  }
}

/*******************************************************************************/

/*
 * reset_short_handle: used for waypoint, route and track names
 */
static void
reset_short_handle(const char* defname)
{
  if (short_h != NULL) {
    mkshort_del_handle(&short_h);
  }

  short_h = mkshort_new_handle();

  setshort_length(short_h, GDB_NAME_BUFFERLEN);
  setshort_badchars(short_h, "\r\n\t");
  setshort_mustupper(short_h, 0);
  setshort_mustuniq(short_h, 1);
  setshort_whitespace_ok(short_h, 1);
  setshort_repeating_whitespace_ok(short_h, 1);
  setshort_defname(short_h, defname);
}

/* ----------------------------------------------------------------------------*/

static void
write_header(void)
{
  char buff[128], tbuff[32];
  char* c;
  int len;
  struct tm tm;

  FWRITE_CSTR("MsRcf");
  FWRITE_i32(2);

  strncpy(buff, "Dx", sizeof(buff));
  buff[1] = 'k' - 1 + gdb_ver;
  FWRITE_CSTR(buff);

#if 0
  /* Take this if anything is wrong with our self generated watermark */
  strncpy(buff, "A].SQA*Dec 27 2004*17:40:51", sizeof(buff));	/* MapSource V6.5 */
#else
  /* This is our "Watermark" to show this file was created by GPSbabel */

  /* history:

  "A].GPSBabel_1.2.7-beta*Sep 13 2005*20:10:00" - gpsbabel V1.2.7 BETA
  "A].GPSBabel_1.2.8-beta*Jan 18 2006*20:11:00" - gpsbabel 1.2.8-beta01182006_clyde
  "A].GPSBabel_1.2.8-beta*Apr 18 2006*20:12:00" - gpsbabel 1.2.8-beta20060405
  "A].GPSBabel-1.3*Jul 02 2006*20:13:00" -        gpsbabel 1.3.0
  "A].GPSBabel-1.3.1*Sep 03 2006*20:14:00" -      gpsbabel 1.3.1
  "A].GPSBabel-1.3.2*Nov 01 2006*22:23:39" -      gpsbabel 1.3.2

  New since 11/01/2006:
  version:   version and release of gpsbabel (defined in configure.in)
  timestamp: date and time of gdb.c (handled by CVS)

  "A].GPSBabel-1.3.2*Nov 01 2006*22:23:39" -      gpsbabel 1.3.2
  "A].GPSBabel-beta20061125*Feb 06 2007*23:24:14" gpsbabel beta20061125
  "A].GPSBabel-1.3.3*Feb 20 2007*20:51:15" -      gpsbabel 1.3.3

  */

  memset(&tm, 0, sizeof(tm));
  sscanf(gdb_release_date+7, "%d/%d/%d %d:%d:%d", &tm.tm_year, &tm.tm_mon, &tm.tm_mday, &tm.tm_hour, &tm.tm_min, &tm.tm_sec);
  tm.tm_year -= 1900;
  tm.tm_mon -= 1;
  strftime(tbuff, sizeof(tbuff), "%b %d %Y*%H:%M:%S", &tm);
  snprintf(buff, sizeof(buff), "A].GPSBabel-%s*%s", gpsbabel_version, tbuff);
#endif
  len = strlen(buff);
  buff[2] = 2;

  c = buff;
  while ((c = strchr(c, '*'))) {
    *c++ = '\0';
  }

  FWRITE_i32(len);
  FWRITE(buff, len + 1);
  FWRITE_CSTR("MapSource");		/* MapSource magic */
}

/*-----------------------------------------------------------------------------*/

/*
 * gdb_check_waypt: As implemented in waypt_add, but we have some leaks where
 *                  waypoints are modified after waypt_add. Maybe we need a data check
 *                  after each input module.
 */

static void
gdb_check_waypt(waypoint* wpt)
{
  double lat_orig = wpt->latitude;
  double lon_orig = wpt->longitude;

  if (wpt->latitude < -90) {
    wpt->latitude += 180;
  } else if (wpt->latitude > +90) {
    wpt->latitude -= 180;
  }
  if (wpt->longitude < -180) {
    wpt->longitude += 360;
  } else if (wpt->longitude > +180) {
    wpt->longitude -= 360;
  }

  if ((wpt->latitude < -90) || (wpt->latitude > 90.0))
    fatal("Invalid latitude %f in waypoint %s.\n",
          lat_orig, wpt->shortname ? wpt->shortname : "<no name>");
  if ((wpt->longitude < -180) || (wpt->longitude > 180.0))
    fatal("Invalid longitude %f in waypoint %s.\n",
          lon_orig, wpt->shortname ? wpt->shortname : "<no name>");
}

/*-----------------------------------------------------------------------------*/

static void
write_waypoint(
  const waypoint* wpt, const char* shortname, garmin_fs_t* gmsd,
  const int icon, const int display)
{
  char zbuf[32], ffbuf[32];
  int wpt_class;

  waypt_ct++;	/* increase informational number of written waypoints */

  memset(zbuf, 0, sizeof(zbuf));
  memset(ffbuf, 0xFF, sizeof(ffbuf));

  wpt_class = wpt->microseconds;		/* trick */

  FWRITE_CSTR(shortname);			/* uniqe (!!!) shortname */
  FWRITE_i32(wpt_class);			/* waypoint class */
  FWRITE_CSTR(GMSD_GET(cc, ""));		/* country code */

  if (wpt_class != 0) {
    waypth_ct++;
  }

#ifdef GMSD_EXPERIMENTAL
  if (gmsd && gmsd->flags.subclass && (wpt_class >= gt_waypt_class_map_point)) {
    FWRITE(gmsd->subclass, sizeof(gmsd->subclass));
  } else
#endif
  {
    FWRITE(zbuf, 4);		/* subclass part 1 */
    FWRITE(ffbuf, 12);		/* subclass part 2 */
    FWRITE(zbuf, 2);		/* subclass part 3 */
    FWRITE(ffbuf, 4);		/* unknown */
  }

  FWRITE_LATLON(wpt->latitude);		/* latitude */
  FWRITE_LATLON(wpt->longitude);		/* longitude */
  FWRITE_DBL(wpt->altitude, unknown_alt);	/* altitude */
  if (wpt->notes) {
    FWRITE_CSTR(wpt->notes);
  } else {
    FWRITE_CSTR(wpt->description);
  }
  FWRITE_DBL(WAYPT_GET(wpt, proximity, unknown_alt), unknown_alt);	/* proximity */
  FWRITE_i32(display);			/* display */
  FWRITE_i32(0);				/* color */
  FWRITE_i32(icon);			/* icon */
  FWRITE_CSTR(GMSD_GET(city, ""));	/* city */
  FWRITE_CSTR(GMSD_GET(state, ""));	/* state */
  FWRITE_CSTR(GMSD_GET(facility, ""));	/* facility */
  FWRITE_C(0);				/* unknown */
  FWRITE_DBL(WAYPT_GET(wpt, depth, unknown_alt), unknown_alt);	/* depth */

  /* VERSION DEPENDENT CODE */
  if (gdb_ver <= GDB_VER_2) {
    char* descr;

    FWRITE(zbuf, 3);
    FWRITE(zbuf, 4);
    descr = (wpt_class < gt_waypt_class_map_point) ?
            wpt->url : wpt->description;
    if ((descr != NULL) && (wpt_class >= gt_waypt_class_map_point) && \
        (strcmp(descr, wpt->shortname) == 0)) {
      descr = NULL;
    }
    FWRITE_CSTR(descr);
  } else { /* if (gdb_ver > GDB_VER_3) */
    int cnt;
    url_link* url_next;
    const char* str;

    if (wpt_class < gt_waypt_class_map_point) {	/* street address */
      str  = GMSD_GET(addr, "");
    } else {
      str = "";
    }
    FWRITE_CSTR(str);
    FWRITE(zbuf, 5);				/* instruction dependend */

    /* GBD doesn't have a native description field */
    /* here we misuse the instruction field */

    str = wpt->description;
    if (str && (strcmp(str, wpt->shortname) == 0)) {
      str = NULL;
    }
    if (str && wpt->notes && (strcmp(str, wpt->notes) == 0)) {
      str = NULL;
    }
    FWRITE_CSTR(str);				/* instruction */

    cnt = 0;
    if (wpt->url) {
      cnt++;
    }
    for (url_next = wpt->url_next; (url_next); url_next = url_next->url_next)
      if (url_next->url) {
        cnt++;
      }
    FWRITE_i32(cnt);
    if (wpt->url) {
      FWRITE_CSTR(wpt->url);
    }
    for (url_next = wpt->url_next; (url_next); url_next = url_next->url_next)
      if (url_next->url) {
        FWRITE_CSTR(url_next->url);
      }
  }

  FWRITE_i16(GMSD_GET(category, gdb_category));
  FWRITE_DBL(WAYPT_GET(wpt, temperature, 0), 0);
  FWRITE_TIME(wpt->creation_time);

  /* VERSION DEPENDENT CODE */
  if (gdb_ver >= GDB_VER_3) {
    const char* str = GMSD_GET(phone_nr, "");
    if (*str) {
      FWRITE_i32(1);
      FWRITE_CSTR(str);
      FWRITE_CSTR("");
    } else {
      FWRITE_i32(0);
    }
    FWRITE_CSTR(GMSD_GET(country, ""));
    FWRITE_CSTR(GMSD_GET(postal_code, ""));
  }
}

static void
route_compute_bounds(const route_head* rte, bounds* bounds)
{
  queue* elem, *tmp;
  waypt_init_bounds(bounds);
  QUEUE_FOR_EACH((queue*)&rte->waypoint_list, elem, tmp) {
    waypoint* wpt = (waypoint*)elem;
    gdb_check_waypt(wpt);
    waypt_add_to_bounds(bounds, wpt);
  }
}

static void
route_write_bounds(bounds* bounds)
{
  if (waypt_bounds_valid(bounds)) {
    FWRITE_C(0);
    FWRITE_LATLON(bounds->max_lat);
    FWRITE_LATLON(bounds->max_lon);
    FWRITE_DBL(bounds->max_alt, unknown_alt);
    FWRITE_LATLON(bounds->min_lat);
    FWRITE_LATLON(bounds->min_lon);
    FWRITE_DBL(bounds->min_alt, -(unknown_alt));
  } else {
    FWRITE_C(1);
  }
}

static void
write_route(const route_head* rte, const char* rte_name)
{
  bounds bounds;
  int points, index;
  queue* elem, *tmp;
  char zbuf[32], ffbuf[32];

  memset(zbuf, 0, sizeof(zbuf));
  memset(ffbuf, 0xFF, sizeof(ffbuf));

  FWRITE_CSTR(rte_name);
  FWRITE_C(0);				/* display/autoname - 1 byte */

  route_compute_bounds(rte, &bounds);
  route_write_bounds(&bounds);

  points = ELEMENTS(rte);
  FWRITE_i32(points);

  index = 0;

  QUEUE_FOR_EACH((queue*)&rte->waypoint_list, elem, tmp) {

    waypoint* wpt = (waypoint*)elem;
    waypoint* next = (waypoint*)tmp;
    waypoint* test;
    garmin_fs_t* gmsd = NULL;
    int wpt_class;

    index++;
    rtept_ct++;	/* increase informational number of written route points */

    if (index == 1) {
      gdb_check_waypt(wpt);
    }
    if (index < points) {
      gdb_check_waypt(next);
    }

    test = gdb_find_wayptq(&wayptq_out, wpt, 1);
    if (test != NULL) {
      wpt = test;
    } else {
      fatal(MYNAME ": Sorry, that should never happen!!!\n");
    }

    gmsd = GMSD_FIND(wpt);

    /* extra_data may contain a modified shortname */
    FWRITE_CSTR((wpt->extra_data) ? (char*)wpt->extra_data : wpt->shortname);

    wpt_class = wpt->microseconds;			/* trick */

    FWRITE_i32(wpt_class);				/* waypoint class */
    FWRITE_CSTR(GMSD_GET(cc, ""));			/* country */
#ifdef GMSD_EXPERIMENTAL
    if (gmsd && gmsd->flags.subclass && (wpt_class >= gt_waypt_class_map_point)) {
      FWRITE(gmsd->subclass, sizeof(gmsd->subclass));
    } else
#endif
    {
      FWRITE(zbuf, 4);				/* subclass part 1 */
      FWRITE(ffbuf, 12);				/* subclass part 2 */
      FWRITE(zbuf, 2);				/* subclass part 3 */
      FWRITE(ffbuf, 4);				/* unknown */
    }

    FWRITE_C(0);					/* unknown value or string */
    FWRITE_C(3);					/* unknown 18 bytes starting with 0x03 */
    FWRITE(zbuf, 3);
    FWRITE(ffbuf, 4);
    FWRITE(zbuf, 10);

    if (index == points) {
      FWRITE_i32(0);		/* no more steps */
      FWRITE_C(1);		/* skip bounds */
    } else { /* if (index < points) */
      FWRITE_i32(2);		/* two interstep links */

      FWRITE_LATLON(wpt->latitude);
      FWRITE_LATLON(wpt->longitude);
      FWRITE_DBL(wpt->altitude, unknown_alt);
      FWRITE_LATLON(next->latitude);
      FWRITE_LATLON(next->longitude);
      FWRITE_DBL(next->altitude, unknown_alt);

      waypt_init_bounds(&bounds);
      waypt_add_to_bounds(&bounds, wpt);
      waypt_add_to_bounds(&bounds, next);
      route_write_bounds(&bounds);

    }

    /* VERSION DEPENDENT CODE */
    if (gdb_ver >= GDB_VER_2) {
      FWRITE(ffbuf, 8);
      if (gdb_ver >= GDB_VER_3) {
        FWRITE(zbuf, 2);
      }
    }
  }

  /* VERSION DEPENDENT CODE */
  if (gdb_ver <= GDB_VER_2) {
    FWRITE_CSTR(rte->rte_url);
  } else { /* if (gdb_ver >= GDB_VER_3) */
    FWRITE_CSTR_LIST(rte->rte_url);
    /* "Magenta" (14) is MapSource default */
    FWRITE_i32((rte->line_color.bbggrr < 0) ? 14 : gt_color_index_by_rgb(rte->line_color.bbggrr));
    FWRITE_C(0);
    FWRITE_CSTR(rte->rte_desc);
  }
}

static void
write_track(const route_head* trk, const char* trk_name)
{
  queue* elem, *tmp;
  int points = ELEMENTS(trk);

  FWRITE_CSTR(trk_name);
  FWRITE_C(0);
  /* "Unknown" (0) is MapSource default */
  FWRITE_i32(gt_color_index_by_rgb(trk->line_color.bbggrr));

  FWRITE_i32(points);	/* total number of waypoints in waypoint list */

  QUEUE_FOR_EACH((queue*)&trk->waypoint_list, elem, tmp) {
    double d;
    waypoint* wpt = (waypoint*)elem;

    trkpt_ct++;	/* increase informational number of written route points */

    FWRITE_LATLON(wpt->latitude);
    FWRITE_LATLON(wpt->longitude);
    FWRITE_DBL(wpt->altitude, unknown_alt);
    FWRITE_TIME(wpt->creation_time);
    d = WAYPT_GET(wpt, depth, unknown_alt);
    FWRITE_DBL(d, unknown_alt);
    d = WAYPT_GET(wpt, temperature, -99999);
    FWRITE_DBL(d, -99999);
  }

  /* finalize track */

  /* VERSION DEPENDENT CODE */
  if (gdb_ver <= GDB_VER_2) {
    FWRITE_CSTR(trk->rte_url);
  } else { /* if (gdb_ver >= GDB_VER_3 */
    FWRITE_CSTR_LIST(trk->rte_url);
  }
}

/*-----------------------------------------------------------------------------*/

static void
finalize_item(gbfile* origin, const char identifier)
{
  int len = gbftell(fout);

  fout = origin;
  gbfseek(ftmp, 0, SEEK_SET);

  FWRITE_i32(len);
  FWRITE_C(identifier);
  gbfcopyfrom(fout, ftmp, len);

  gbfseek(ftmp, 0, SEEK_SET);	/* Truncate memory stream */
  gbfwrite(NULL, 0, 0, ftmp);
}

/*-----------------------------------------------------------------------------*/

static char
str_not_equal(const char* s1, const char* s2)
{
  if (s1) {
    if (!s2) {
      return 1;
    }
    if (strcmp(s1, s2) != 0) {
      return 1;
    } else {
      return 0;
    }
  } else if (s2) {
    return 1;
  } else {
    return 0;
  }
}

static void
write_waypoint_cb(const waypoint* refpt)
{
  garmin_fs_t* gmsd;
  waypoint* test;
  gbfile* fsave;

  /* do this when backup always happens in main */

  rtrim(((waypoint*)refpt)->shortname);
  test = gdb_find_wayptq(&wayptq_out, refpt, 1);

  if ((test != NULL) && (route_flag == 0)) {
    if ((str_not_equal(test->notes, refpt->notes)) ||
        (str_not_equal(test->url, refpt->url))) {
      test = NULL;
    }
  }

  if (test == NULL) {
    int icon, display, wpt_class;
    char* name;
    waypoint* wpt = waypt_dupe(refpt);

    gdb_check_waypt(wpt);
    ENQUEUE_TAIL(&wayptq_out, &wpt->Q);

    fsave = fout;
    fout = ftmp;

    /* prepare the waypoint */
    gmsd = GMSD_FIND(wpt);

    wpt_class = GMSD_GET(wpt_class, -1);
    if (wpt_class == -1) {
      wpt_class = (route_flag) ? GDB_DEF_HIDDEN_CLASS : GDB_DEF_CLASS;
    }
    wpt->microseconds = wpt_class; 	/* trick, we need this for the route(s) */

    icon = GMSD_GET(icon, -1);
    if (icon < 0) {
      if (wpt->icon_descr) {
        icon = gt_find_icon_number_from_desc(wpt->icon_descr, GDB);
      } else {
        icon = GDB_DEF_ICON;
      }
    }

    switch (GMSD_GET(display, -1)) {		/* display */
    case -1:
      if (wpt_class < 8) {
        display = gt_gdb_display_mode_symbol_and_name;
      } else {
        display = gt_gdb_display_mode_symbol;
      }
      break;
    case gt_display_mode_symbol:
      display = gt_gdb_display_mode_symbol;
      break;
    case gt_display_mode_symbol_and_comment:
      display = gt_gdb_display_mode_symbol_and_comment;
      break;
    default:
      display = gt_gdb_display_mode_symbol_and_name;
      break;
    }

    name = wpt->shortname;

    if (global_opts.synthesize_shortnames || (*name == '\0')) {
      name = wpt->notes;
      if (!name) {
        name = wpt->description;
      }
      if (!name) {
        name = wpt->shortname;
      }
    }

    name = mkshort(short_h, name);
    wpt->extra_data = (void*)name;
    write_waypoint(wpt, name, gmsd, icon, display);

    finalize_item(fsave, 'W');
  }
}

static void
write_route_cb(const route_head* rte)
{
  gbfile* fsave;
  char* name;
  char buf[32];

  if (ELEMENTS(rte) <= 0) {
    return;
  }

  if (rte->rte_name == NULL) {
    snprintf(buf, sizeof(buf), "Route%04d", rte->rte_num);
    name = mkshort(short_h, buf);
  } else {
    name = mkshort(short_h, rte->rte_name);
  }

  rte_ct++;	/* increase informational number of written routes */

  fsave = fout;
  fout = ftmp;
  write_route(rte, name);
  finalize_item(fsave, 'R');

  xfree(name);
}

static void
write_track_cb(const route_head* trk)
{
  gbfile* fsave;
  char* name;
  char buf[32];

  if (ELEMENTS(trk) <= 0) {
    return;
  }

  if (trk->rte_name == NULL) {
    snprintf(buf, sizeof(buf), "Track%04d", trk->rte_num);
    name = mkshort(short_h, buf);
  } else {
    name = mkshort(short_h, trk->rte_name);
  }

  trk_ct++;	/* increase informational number of written tracks */

  fsave = fout;
  fout = ftmp;
  write_track(trk, name);
  finalize_item(fsave, 'T');

  xfree(name);
}

/*-----------------------------------------------------------------------------*/

static void
gdb_wr_init(const char* fname)
{
  fout = gbfopen_le(fname, "wb", MYNAME);
  ftmp = gbfopen_le(NULL, "wb", MYNAME);

  gdb_category = (gdb_opt_category) ? atoi(gdb_opt_category) : 0;
  gdb_ver = (gdb_opt_ver && *gdb_opt_ver) ? atoi(gdb_opt_ver) : 0;

  if (gdb_category) {
    is_fatal((gdb_category < 1) || (gdb_category > 16),
             MYNAME ": cat must be between 1 and 16!");
    gdb_category = 1 << (gdb_category - 1);
  }

  if (gdb_opt_bitcategory) {
    gdb_category = strtol(gdb_opt_bitcategory, NULL, 0);
  }

  if (gdb_ver >= GDB_VER_UTF8) {
    cet_convert_init(CET_CHARSET_UTF8, 1);
  }

  QUEUE_INIT(&wayptq_out);
  short_h = NULL;

  waypt_ct = 0;
  waypth_ct = 0;
  rtept_ct = 0;
  trkpt_ct = 0;
  rte_ct = 0;
  trk_ct = 0;
}

static void
gdb_wr_deinit(void)
{
  disp_summary(fout);
  gdb_flush_waypt_queue(&wayptq_out);
  mkshort_del_handle(&short_h);
  gbfclose(fout);
  gbfclose(ftmp);
}

static void
write_data(void)
{
  if (gdb_opt_ver) {
    gdb_ver = atoi(gdb_opt_ver);
  }
  write_header();

  reset_short_handle("WPT");
  route_flag = 0;
  waypt_disp_all(write_waypoint_cb);
  route_flag = 1;
  route_disp_all(NULL, NULL, write_waypoint_cb);

  reset_short_handle("Route");
  route_disp_all(write_route_cb, NULL, NULL);

  reset_short_handle("Track");
  track_disp_all(write_track_cb, NULL, NULL);

  FWRITE_i32(2);			/* finalize gdb with empty map segment */
  FWRITE_CSTR("V");
  FWRITE_C(1);
}

/*******************************************************************************/

#define GDB_OPT_VER		"ver"
#define GDB_OPT_VIA		"via"
#define GDB_OPT_CATEGORY	"cat"
#define GDB_OPT_BITCATEGORY	"bitscategory"
#define GDB_OPT_ROADBOOK	"roadbook"

static arglist_t gdb_args[] = {
  {
    GDB_OPT_CATEGORY, &gdb_opt_category,
    "Default category on output (1..16)",
    NULL, ARGTYPE_INT, "1", "16"
  },
  {
    GDB_OPT_BITCATEGORY, &gdb_opt_bitcategory, "Bitmap of categories",
    NULL, ARGTYPE_INT, "1", "65535"
  },
  {
    GDB_OPT_VER, &gdb_opt_ver,
    "Version of gdb file to generate (1..3)",
    "2", ARGTYPE_INT, "1", "3"
  },
  {
    GDB_OPT_VIA, &gdb_opt_via,
    "Drop route points that do not have an equivalent waypoint (hidden points)",
    NULL, ARGTYPE_BOOL, ARG_NOMINMAX
  },
  {
    GDB_OPT_ROADBOOK, &gdb_opt_roadbook,
    "Include major turn points (with description) from calculated route",
    NULL, ARGTYPE_BOOL, ARG_NOMINMAX
  },

  ARG_TERMINATOR
};

ff_vecs_t gdb_vecs = {
  ff_type_file,
  FF_CAP_RW_ALL,
  gdb_rd_init,
  gdb_wr_init,
  gdb_rd_deinit,
  gdb_wr_deinit,
  read_data,
  write_data,
  NULL,
  gdb_args,
  CET_CHARSET_MS_ANSI, 0	/* O.K.: changed to NON-FIXED */
  /* because of utf8 strings since GDB V3 */
};

/*******************************************************************************/
