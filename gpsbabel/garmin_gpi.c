/*

    Support for Garmin Points of Interest (.gpi files)

    Copyright (C) 2007 Olaf Klein, o.b.klein@gpsbabel.org

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

	* 2007/05/18: initial release (only a reader)
	* 2007/05/20: added writer code with embedded bitmap
	* 2007/05/22: add support for multiple bounding boxes
	              (useful / required!) for large waypoints lists
	* 2007/05/23: add optional user bitmap
	* 2007/06/02: new method to compute center (mean) of bounds
	              avoid endless loop in group splitting
	* 2007/07/10: put address fields (i.e. city) into GMSD
	* 2007/07/12: add write support for new address fields
	* 2007/10/20: add option unique
	* 2007/12/02: support speed and proximity distance (+ alerts)
	* 2008/01/14: fix structure error after adding speed/proximity
	* 2008/03/22: add options "speed" and "proximity" (default values) and "sleep"

	ToDo:

	* Display mode ("Symbol & Name") ??? not in gpi ???
	* support category from GMSD "Garmin Special Data"
*/

#include "defs.h"
#include "cet_util.h"
#include "jeeps/gpsmath.h"
#include "garmin_fs.h"
#include "garmin_gpi.h"
#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#define MYNAME "garmin_gpi"

#define GPI_DBG 1
#undef GPI_DBG

#define DEFAULT_ICON	"Waypoint"
#define WAYPOINTS_PER_BLOCK	128

/* flags used in the gpi address mask */
#define GPI_ADDR_CITY		1
#define GPI_ADDR_COUNTRY	2
#define GPI_ADDR_STATE		4
#define GPI_ADDR_POSTAL_CODE	8
#define GPI_ADDR_ADDR		16

static char* opt_cat, *opt_pos, *opt_notes, *opt_hide_bitmap, *opt_descr, *opt_bitmap;
static char* opt_unique, *opt_alerts, *opt_units, *opt_speed, *opt_proximity, *opt_sleep;
static double defspeed, defproximity;
static int alerts;

static arglist_t garmin_gpi_args[] = {
  {
    "alerts", &opt_alerts, "Enable alerts on speed or proximity distance",
    NULL, ARGTYPE_BOOL, ARG_NOMINMAX
  },
  {
    "bitmap", &opt_bitmap, "Use specified bitmap on output",
    NULL, ARGTYPE_FILE, ARG_NOMINMAX
  },
  {
    "category", &opt_cat, "Default category on output",
    "My points", ARGTYPE_STRING, ARG_NOMINMAX
  },
  {
    "hide", &opt_hide_bitmap, "Don't show gpi bitmap on device",
    NULL, ARGTYPE_BOOL, ARG_NOMINMAX
  },
  {
    "descr", &opt_descr, "Write description to address field",
    NULL, ARGTYPE_BOOL, ARG_NOMINMAX
  },
  {
    "notes", &opt_notes, "Write notes to address field",
    NULL, ARGTYPE_BOOL, ARG_NOMINMAX
  },
  {
    "position", &opt_pos, "Write position to address field",
    NULL, ARGTYPE_BOOL, ARG_NOMINMAX
  },
  {
    "proximity", &opt_proximity, "Default proximity",
    NULL, ARGTYPE_FLOAT, ARG_NOMINMAX
  },
  {
    "sleep", &opt_sleep, "After output job done sleep n second(s)",
    NULL, ARGTYPE_INT, "1", NULL
  },
  {
    "speed", &opt_speed, "Default speed",
    NULL, ARGTYPE_FLOAT, ARG_NOMINMAX
  },
  {
    "unique", &opt_unique, "Create unique waypoint names (default = yes)",
    "Y", ARGTYPE_BOOL, ARG_NOMINMAX
  },
  {
    "units", &opt_units, "Units used for names with @speed ('s'tatute or 'm'etric)",
    "m", ARGTYPE_STRING, ARG_NOMINMAX
  },
  ARG_TERMINATOR
};

typedef struct {
  int D2;
  char S3[9];		/* "GRMRECnn" */
  time_t crdate;		/* creation date and time */
  char POI[4];		/* "POI" */
  char S8[3];
  char* group;
  char* category;
} reader_data_t;

typedef struct writer_data_s {
  queue Q;
  int ct;
  int sz;
  int alert;
  bounds bds;
  struct writer_data_s* top_left;
  struct writer_data_s* top_right;
  struct writer_data_s* buttom_left;
  struct writer_data_s* buttom_right;
} writer_data_t;

typedef struct gpi_waypt_data_s {
  int sz;
  char* addr;
  char* postal_code;
} gpi_waypt_data_t;

typedef struct {
  gbint32 size;
  gbint16 res1;
  gbint16 res2;
  gbint32 image_offset;
  gbint32 header_size;
  gbint32 width;
  gbint32 height;
  gbint16 planes;
  gbint16 bpp;
  gbint32 compression_type;
  gbint32 image_data_size;
  gbint32 resolution_h;
  gbint32 resolution_v;
  gbint32 used_colors;
  gbint32 important_colors;
} bmp_header_t;

typedef struct {
  gbint16 index;
  gbint16 height;
  gbint16 width;
  gbint16 line_sz;
  gbint16 bpp;
  gbint16 fixed_0;
  gbint32 image_size;
  gbint32 fixed_2c;
  gbint32 flag1;
  gbint32 tr_color;
  gbint32 flag2;
  gbint32 size_2c;
} gpi_bitmap_header_t;

typedef struct {
  int sz;
  int alerts;
  short mask;
  char addr_is_dynamic;
  char* addr;
  char* city;
  char* country;
  char* phone_nr;
  char* postal_code;
  char* state;
} gpi_waypt_t;

static gbfile* fin, *fout;
static gbint16 codepage;	/* code-page, i.e. 1252 */
static reader_data_t* rdata;
static writer_data_t* wdata;
static short_handle short_h;
static char units;
static time_t gpi_timestamp = 0;

#ifdef GPI_DBG
# define PP warning("@%1$6x (%1$8d): ", gbftell(fin))
# define dbginfo warning
#else
# define PP
#endif

/*******************************************************************************
* %%%                             gpi reader                               %%% *
*******************************************************************************/

/* look for or initialize GMSD */
static garmin_fs_t*
gpi_gmsd_init(waypoint* wpt)
{
  garmin_fs_t* gmsd = GMSD_FIND(wpt);
  if (wpt == NULL) {
    fatal(MYNAME ": Error in file structure.\n");
  }
  if (gmsd == NULL) {
    gmsd = garmin_fs_alloc(-1);
    fs_chain_add(&wpt->fs, (format_specific_data*) gmsd);
  }
  return gmsd;
}

/* read a standard string with or without 'EN' (or whatever) header */
static char*
gpi_read_string(const char* field)
{
  int l1;
  char* res = NULL;

  l1 = gbfgetint16(fin);
  if (l1 > 0) {
    short l2;
    char first;

    first = gbfgetc(fin);
    if (first == 0) {
      char en[2];

      is_fatal((gbfgetc(fin) != 0),
               MYNAME ": Error reading field '%s'!", field);

      gbfread(en, 1, sizeof(en), fin);
      l2 = gbfgetint16(fin);
      is_fatal((l2 + 4 != l1),
               MYNAME ": Error out of sync (wrong size %d/%d) on field '%s'!", l1, l2, field);

      if ((en[0] < 'A') || (en[0] > 'Z') || (en[1] < 'A') || (en[1] > 'Z')) {
        fatal(MYNAME ": Invalid country code!\n");
      }
      res = (char*) xmalloc(l2 + 1);
      res[l2] = '\0';
      PP;
      if (l2 > 0) {
        gbfread(res, 1, l2, fin);
      }
    } else {
      res = (char*) xmalloc(l1 + 1);
      *res = first;
      *(res + l1) = '\0';
      PP;
      l1--;
      if (l1 > 0) {
        gbfread(res + 1, 1, l1, fin);
      }

    }
  }
#ifdef GPI_DBG
  dbginfo("%s: %s\n", field, (res == NULL) ? "<NULL>" : res);
#endif
  return res;
}

static void
read_header(void)
{
  int len, i;
#ifdef GPI_DBG
  struct tm tm;
  char stime[32];
#endif

  i = gbfgetint32(fin);
  if (i != 0) {
    i = gbfgetint32(fin);
  }
  rdata->D2 = gbfgetint32(fin);

  gbfread(&rdata->S3, 1, sizeof(rdata->S3) - 1, fin);	/* GRMRECnn */
  if (strncmp(rdata->S3, "GRMREC", 6) != 0) {
    fatal(MYNAME ": No GPI file!\n");
  }

  PP;
  rdata->crdate = gbfgetint32(fin);
#ifdef GPI_DBG
  tm = *localtime(&rdata->crdate);
  tm.tm_year += 20;	/* !!! */
  tm.tm_mday -= 1;	/* !!! */
  strftime(stime, sizeof(stime), "%Y/%m/%d %H:%M:%S", &tm);
  dbginfo("crdate = %lu (%s)\n", rdata->crdate, stime);
#endif

  (void) gbfgetint16(fin);	/* 0 */

  len = gbfgetint16(fin);
  gbfseek(fin, len, SEEK_CUR);	/* "my.gpi" */

  i =  gbfgetint32(fin);	/* 1 */
  (void) gbfgetint32(fin);	/* 12 */
  /* There are two dwords next.  On most typical files, they're
  * "1" and "12".  On files from garminoneline.de/extras/poi, the
  * next two words are "15" and "5" and there's 17 additional bytes
  * that I can't identify.   So hardcode a seek here for now.
  */
  if (i == 15) {
    gbfseek(fin, 17, SEEK_CUR);
  }

  gbfread(&rdata->POI, 1, sizeof(rdata->POI) - 1, fin);
  if (strcmp(rdata->POI, "POI") != 0) {
    fatal(MYNAME ": Wrong or unsupported GPI file!\n");
  }

  for (i = 0; i < 3; i++) {
    (void)gbfgetc(fin);
  }
  gbfread(&rdata->S8, 1, sizeof(rdata->S8) - 1, fin);

  codepage = gbfgetint16(fin);
  (void) gbfgetint16(fin);   	/* typically 0, but  0x11 in
  					Garminonline.de files.  */

#ifdef GPI_DBG
  PP;
  dbginfo("< leaving header\n");
#endif
}

/* gpi tag handler */
static int read_tag(const char* caller, const int tag, waypoint* wpt);


/* read a single poi with all options */
static void
read_poi(const int sz, const int tag)
{
  int pos, len;
  waypoint* wpt;

#ifdef GPI_DBG
  PP;
  dbginfo("> reading poi (size %d)\n", sz);
#endif
  PP;
  len = 0;
  if (tag == 0x80002) {
    len = gbfgetint32(fin);	/* sub-header size */
  }
#ifdef GPI_DBG
  dbginfo("poi sublen = %1$d (0x%1$x)\n", len);
#endif
  pos = gbftell(fin);

  wpt = waypt_new();
  wpt->icon_descr = DEFAULT_ICON;

  wpt->latitude = GPS_Math_Semi_To_Deg(gbfgetint32(fin));
  wpt->longitude = GPS_Math_Semi_To_Deg(gbfgetint32(fin));

  (void) gbfgetint16(fin);	/* ? always 1 ? */
  (void) gbfgetc(fin);		/* seems to 1 when extra options present */
  wpt->shortname = gpi_read_string("Shortname");

  while (gbftell(fin) < (gbsize_t)(pos + sz - 4)) {
    int tag = gbfgetint32(fin);
    if (! read_tag("read_poi", tag, wpt)) {
      break;
    }
  }

  if (wpt->notes && !wpt->description) {
    wpt->description = xstrdup(wpt->notes);
  }
  if (wpt->description && !wpt->notes) {
    wpt->notes = xstrdup(wpt->description);
  }

  waypt_add(wpt);

#ifdef GPI_DBG
  PP;
  dbginfo("< leaving poi\n");
#endif
}

/* read poi's following a group header */
static void
read_poi_list(const int sz)
{
  int pos, i;

  pos = gbftell(fin);
#ifdef GPI_DBG
  PP;
  dbginfo("> reading poi list (-> %1$x / %1$d )\n", pos + sz);
#endif
  PP;
  i = gbfgetint32(fin);	/* mostly 23 (0x17) */
#ifdef GPI_DBG
  dbginfo("list sublen = %1$d (0x%1$x)\n", i);
#endif
  (void) gbfgetint32(fin);	/* max-lat */
  (void) gbfgetint32(fin);	/* max-lon */
  (void) gbfgetint32(fin);	/* min-lat */
  (void) gbfgetint32(fin);	/* min-lon */

  (void) gbfgetc(fin);		/* three unknown bytes */
  (void) gbfgetc(fin);		/* ? should be zero ? */
  (void) gbfgetc(fin);

  (void) gbfgetint32(fin);	/* ? const 0x1000100 ? */

  while (gbftell(fin) < (gbsize_t)(pos + sz - 4)) {
    int tag = gbfgetint32(fin);
    if (! read_tag("read_poi_list", tag, NULL)) {
      return;
    }
  }
#ifdef GPI_DBG
  PP;
  dbginfo("< leaving poi list\n");
#endif
}


static void
read_poi_group(const int sz, const int tag)
{
  int pos;

  pos = gbftell(fin);
#ifdef GPI_DBG
  PP;
  dbginfo("> reading poi group (-> %1$x / %1$d)\n", pos + sz);
#endif
  if (tag == 0x80009) {
    int subsz;

    PP;
    subsz = gbfgetint32(fin);	/* ? offset to category data ? */
#ifdef GPI_DBG
    dbginfo("group sublen = %d (-> %x / %d)\n", subsz, pos + subsz + 4, pos + subsz + 4);
#endif
  }
  if (rdata->group) {
    xfree(rdata->group);  /* currently unused */
  }
  rdata->group = gpi_read_string("Group");

  while (gbftell(fin) < (gbsize_t)(pos + sz)) {
    int subtag = gbfgetint32(fin);
    if (! read_tag("read_poi_group", subtag, NULL)) {
      break;
    }
  }

#ifdef GPI_DBG
  PP;
  dbginfo("< leaving poi group\n");
#endif
}

// TODO: 'tag' is probably not a 32 bit value.
// most likely it's a pair of 16's: the first pair is the tag number.
// if the second 16 is "eight", then it's an
// extended thingy and it has a 4-byte extended record length (total number
// of bytes for all record fields and all nested records, starting after the
// length field)
/* gpi tag handler */
static int
read_tag(const char* caller, const int tag, waypoint* wpt)
{
  int pos, sz, dist;
  double speed;
  short mask;
  char* str;
  garmin_fs_t* gmsd;

  sz = gbfgetint32(fin);
  pos = gbftell(fin);

#ifdef GPI_DBG
  PP;
  dbginfo("%s: tag = 0x%x (size %d)\n", caller, tag, sz);
#endif
  if ((tag >= 0x80000) && (tag <= 0x800ff)) {
    sz += 4;
  }

  switch (tag) {
  case 0x3:	/* size = 12 */
  case 0x80003:	/* size = 12 */

    dist = gbfgetint16(fin);		/* proximity distance in meters */
    speed = (double)gbfgetint16(fin) / 100;	/* speed in meters per second */

    if (dist > 0) {
      WAYPT_SET(wpt, proximity, dist);
    }
    if (speed > 0) {
      /* speed isn't part of a normal waypoint
      WAYPT_SET(wpt, speed, speed);
      */
      if ((wpt->shortname == NULL) || (! strchr(wpt->shortname, '@'))) {
        if (units == 's') {
          speed = MPS_TO_MPH(speed);
        } else {
          speed = MPS_TO_KPH(speed);
        }
        xasprintf(&str, "%s@%.f", wpt->shortname ? wpt->shortname : "WPT", speed);
        if (wpt->shortname) {
          xfree(wpt->shortname);
        }
        wpt->shortname = str;
      }
    }

    (void) gbfgetint32(fin);
    (void) gbfgetint32(fin);
    break;

  case 0x4:	/* size = 2  ? */
  case 0x6:	/* size = 2  ? */
    break;

  case 0x5:	/* group bitmap */
    break;

  case 0x7:
    (void) gbfgetint16(fin);	/* category number */
    if (rdata->category) {
      xfree(rdata->category);
    }
    rdata->category = gpi_read_string("Category");
    break;

  case 0xa:
    wpt->description = gpi_read_string("Description");
    break;

  case 0xe:	/* ? notes or description / or both ? */
    mask = gbfgetc(fin);
    if (mask == 0x01) {
      str = gpi_read_string("Notes");
    } else if (mask == 0x32) {
      str = gpi_read_string("Notes");
    } else {
      break;
    }

    if (wpt->description) {
      wpt->notes = str;
    } else {
      wpt->description = str;
    }
    break;

  case 0x2:
  case 0x80002:
    read_poi(sz, tag);
    break;

  case 0x80008:
    read_poi_list(sz);
    break;

  case 0x9:	/* ? older versions / no category data ? */
  case 0x80009:	/* current POI loader */
    read_poi_group(sz, tag);
    break;

  case 0x8000b:	/* address (street/city...) */
    (void) gbfgetint32(fin);
  case 0xb:	/* as seen in German POI files. */
    PP;
    mask = gbfgetint16(fin); /* address fields mask */
#ifdef GPI_DBG
    dbginfo("GPI Address field mask: %d (0x%02x)\n", mask, mask);
#endif
    if ((mask & GPI_ADDR_CITY) && (str = gpi_read_string("City"))) {
      gmsd = gpi_gmsd_init(wpt);
      GMSD_SET(city, str);
    }
    if ((mask & GPI_ADDR_COUNTRY) && (str = gpi_read_string("Country"))) {
      gmsd = gpi_gmsd_init(wpt);
      GMSD_SET(country, str);
    }
    if ((mask & GPI_ADDR_STATE) && (str = gpi_read_string("State"))) {
      gmsd = gpi_gmsd_init(wpt);
      GMSD_SET(state, str);
    }
    if ((mask & GPI_ADDR_POSTAL_CODE) && (str = gpi_read_string("Postal code"))) {
      gmsd = gpi_gmsd_init(wpt);
      GMSD_SET(postal_code, str);
    }
    if ((mask & GPI_ADDR_ADDR) && (str = gpi_read_string("Street address"))) {
      gmsd = gpi_gmsd_init(wpt);
      GMSD_SET(addr, str);
    }
    break;

  case 0xc:
    mask = gbfgetint16(fin);
    if ((mask & 1) && (str = gpi_read_string("Phone"))) {
      gmsd = gpi_gmsd_init(wpt);
      GMSD_SET(phone_nr, str);
    }
    if ((mask & 2) && (str = gpi_read_string("Phone2"))) {
      gmsd = gpi_gmsd_init(wpt);
      GMSD_SET(phone_nr2, str);
    }
    if ((mask & 4) && (str = gpi_read_string("Fax"))) {
      gmsd = gpi_gmsd_init(wpt);
      GMSD_SET(fax_nr, str);
    }
    if ((mask & 8) && (str = gpi_read_string("Email"))) {
      gmsd = gpi_gmsd_init(wpt);
      GMSD_SET(email, str);
    }
    if ((mask & 0x10) && (str = gpi_read_string("Link"))) {
      waypt_add_url(wpt, xstrdup(str), xstrdup(str));
    }
    break;

  case 0x8000c:	/* phone-number */
    (void) gbfgetint32(fin);
    PP;

    mask = gbfgetint16(fin); /* phone fields mask */
#ifdef GPI_DBG
    dbginfo("GPI Phone field mask: %d (0x%02x)\n", mask, mask);
#endif
    if ((mask & 1) && (str = gpi_read_string("Phone"))) {
      gmsd = gpi_gmsd_init(wpt);
      GMSD_SET(phone_nr, str);
    }
    break;

  case 0x80012:	/* ? sounds / images ? */
    break;

    /* Images? Seen in http://geepeeex.com/Stonepages.gpi */
  case 0xd:
    break;

  case 0x11:
  case 0x80007:
    /* Looks like some kind of calendar information. */
#ifdef GPI_DBG
  {
    int x;
    unsigned char* b = xmalloc(sz);
    fprintf(stderr, "Tag: %x\n", tag);
    gbfread(b, 1, sz, fin);
    fprintf(stderr, "\n");
    for (x = 0; x < sz; x++) {
      fprintf(stderr, "%02x ", b[x]);
    }
    fprintf(stderr, "\n");
    for (x = 0; x < sz; x++) {
      fprintf(stderr, "%c", isalnum(b[x]) ? b[x] : '.');
    }
    fprintf(stderr, "\n");
  }
#endif // GPI_DBG
  break;
  default:
    warning(MYNAME ": Unknown tag (0x%x). Please report!\n", tag);
    return 0;
  }
  gbfseek(fin, pos + sz, SEEK_SET);
  return 1;
}

/*******************************************************************************
* %%%                             gpi writer                               %%% *
*******************************************************************************/

static void
write_string(const char* str, const char long_format)
{
  int len;

  len = strlen(str);
  if (long_format) {
    gbfputint32(len + 4, fout);
    gbfwrite("EN", 1, 2, fout);
  }
  gbfputint16(len, fout);
  gbfwrite(str, 1, len, fout);
}


static int
compare_wpt_cb(const queue* a, const queue* b)
{
  const waypoint* wa = (waypoint*) a;
  const waypoint* wb = (waypoint*) b;

  return strcmp(wa->shortname, wb->shortname);
}


static char
compare_strings(const char* s1, const char* s2)
{
  if (s1 == s2) {
    return 0;
  } else if (s1) {
    if (s2) {
      return strcmp(s1, s2);
    } else {
      return 1;
    }
  } else {
    return 1;
  }
}


static writer_data_t*
wdata_alloc()
{
  writer_data_t* res;

  res = (writer_data_t*) xcalloc(1, sizeof(*res));
  QUEUE_INIT(&res->Q);
  waypt_init_bounds(&res->bds);

  return res;
}


static void
wdata_free(writer_data_t* data)
{
  queue* elem, *tmp;

  QUEUE_FOR_EACH(&data->Q, elem, tmp) {
    waypoint* wpt = (waypoint*)elem;

    if (wpt->extra_data) {
      gpi_waypt_t* dt = (gpi_waypt_t*) wpt->extra_data;
      if (dt->addr_is_dynamic) {
        xfree(dt->addr);
      }
      xfree(dt);
    }
    waypt_free(wpt);
  }

  if (data->top_left) {
    wdata_free(data->top_left);
  }
  if (data->top_right) {
    wdata_free(data->top_right);
  }
  if (data->buttom_left) {
    wdata_free(data->buttom_left);
  }
  if (data->buttom_right) {
    wdata_free(data->buttom_right);
  }

  xfree(data);
}


static void
wdata_add_wpt(writer_data_t* data, waypoint* wpt)
{
  data->ct++;
  ENQUEUE_TAIL(&data->Q, &wpt->Q);
  waypt_add_to_bounds(&data->bds, wpt);
}


static void
wdata_check(writer_data_t* data)
{
  queue* elem, *tmp;
  double center_lat, center_lon;

  if ((data->ct <= WAYPOINTS_PER_BLOCK) ||
      /* avoid endless loop for points (more than WAYPOINTS_PER_BLOCK)
         at same coordinates */
      ((data->bds.min_lat >= data->bds.max_lat) && (data->bds.min_lon >= data->bds.max_lon))) {
    if (data->ct > 1) {
      sortqueue(&data->Q, compare_wpt_cb);
    }
    return;
  }

  /* compute the (mean) center of current bounds */

  center_lat = center_lon = 0;
  QUEUE_FOR_EACH(&data->Q, elem, tmp) {
    waypoint* wpt = (waypoint*) elem;
    center_lat += wpt->latitude;
    center_lon += wpt->longitude;
  }
  center_lat /= data->ct;
  center_lon /= data->ct;

  QUEUE_FOR_EACH(&data->Q, elem, tmp) {
    waypoint* wpt = (waypoint*) elem;
    writer_data_t** ref;

    if (wpt->latitude < center_lat) {
      if (wpt->longitude < center_lon) {
        ref = &data->buttom_left;
      } else {
        ref = &data->buttom_right;
      }
    } else {
      if (wpt->longitude < center_lon) {
        ref = &data->top_left;
      } else {
        ref = &data->top_right;
      }
    }

    if (*ref == NULL) {
      *ref = wdata_alloc();
    }

    data->ct--;
    dequeue(&wpt->Q);

    wdata_add_wpt(*ref, wpt);
  }

  if (data->top_left) {
    wdata_check(data->top_left);
  }
  if (data->top_right) {
    wdata_check(data->top_right);
  }
  if (data->buttom_left) {
    wdata_check(data->buttom_left);
  }
  if (data->buttom_right) {
    wdata_check(data->buttom_right);
  }
}


static int
wdata_compute_size(writer_data_t* data)
{
  queue* elem, *tmp;
  int res;

  res = 23;	/* bounds, ... of tag 0x80008 */

  QUEUE_FOR_EACH(&data->Q, elem, tmp) {
    waypoint* wpt = (waypoint*) elem;
    gpi_waypt_t* dt;
    garmin_fs_t* gmsd;
    char* str;

    res += 12;		/* tag/sz/sub-sz */
    res += 19;		/* poi fixed size */
    res += strlen(wpt->shortname);
    if (! opt_hide_bitmap) {
      res += 10;  /* tag(4) */
    }

    dt = (gpi_waypt_t*) xcalloc(1, sizeof(*dt));
    wpt->extra_data = dt;

    if (alerts) {
      char* pos;

      if ((pos = strchr(wpt->shortname, '@'))) {
        double speed, scale;
        if (units == 's') {
          scale = MPH_TO_MPS(1);
        } else {
          scale = KPH_TO_MPS(1);
        }
        parse_speed(pos + 1, &speed, scale, MYNAME);
        if (speed > 0) {
          WAYPT_SET(wpt, speed, speed);
        }
#if 0
        if (pos > wpt->shortname) {
          wpt->shortname[pos - wpt->shortname] = '\0';
        }
#endif
      } else if ((opt_speed) && (! WAYPT_HAS(wpt, speed))) {
        WAYPT_SET(wpt, speed, defspeed);
      }

      if ((opt_proximity) && (! WAYPT_HAS(wpt, proximity))) {
        WAYPT_SET(wpt, proximity, defproximity);
      }

      if ((WAYPT_HAS(wpt, speed) && (wpt->speed > 0)) ||
          (WAYPT_HAS(wpt, proximity) && (wpt->proximity > 0))) {
        data->alert = 1;
        dt->alerts++;
        res += 20;		/* tag(3) */
      }
    }

    str = NULL;
    if (opt_descr) {
      if (wpt->description && *wpt->description) {
        str = xstrdup(wpt->description);
      }
    } else if (opt_notes) {
      if (wpt->notes && *wpt->notes) {
        str = xstrdup(wpt->notes);
      }
    } else if (opt_pos) {
      str = pretty_deg_format(wpt->latitude, wpt->longitude, 's', " ", 0);
    }


    if (str) {
      dt->addr_is_dynamic = 1;
      dt->addr = str;
      dt->mask |= GPI_ADDR_ADDR;
      dt->sz += (8 + strlen(dt->addr));
    }

    if ((gmsd = GMSD_FIND(wpt))) {
      if ((dt->mask == 0) && ((dt->addr = GMSD_GET(addr, NULL)))) {
        dt->mask |= GPI_ADDR_ADDR;
        dt->sz += (8 + strlen(dt->addr));
      }
      if ((dt->city = GMSD_GET(city, NULL))) {
        dt->mask |= GPI_ADDR_CITY;
        dt->sz += (8 + strlen(dt->city));
      }
      if ((dt->country = GMSD_GET(country, NULL))) {
        dt->mask |= GPI_ADDR_COUNTRY;
        dt->sz += (8 + strlen(dt->country));
      }
      if ((dt->state = GMSD_GET(state, NULL))) {
        dt->mask |= GPI_ADDR_STATE;
        dt->sz += (8 + strlen(dt->state));
      }
      if ((dt->postal_code = GMSD_GET(postal_code, NULL))) {
        dt->mask |= GPI_ADDR_POSTAL_CODE;
        dt->sz += (2 + strlen(dt->postal_code));	/* short form */
      }

      if ((dt->phone_nr = GMSD_GET(phone_nr, NULL))) {
        res += (12 + 4 +  strlen(dt->phone_nr));
      }
    }
    if (dt->mask) {
      dt->sz += 2;  /* + mask (two bytes) */
    }
    if (dt->sz) {
      res += (dt->sz + 12);  /* + header size */
    }

    str = wpt->description;
    if (! str) {
      str = wpt->notes;
    }
//		if (str && (strcmp(str, wpt->shortname) == 0)) str = NULL;
    if (str) {
      res += (12 + 4 + strlen(str));
    }
  }

  if (data->top_left) {
    res += wdata_compute_size(data->top_left);
  }
  if (data->top_right) {
    res += wdata_compute_size(data->top_right);
  }
  if (data->buttom_left) {
    res += wdata_compute_size(data->buttom_left);
  }
  if (data->buttom_right) {
    res += wdata_compute_size(data->buttom_right);
  }

  data->sz = res;

  return res + 12;	/* + 12 = caller needs info about tag header size */
}


static void
wdata_write(const writer_data_t* data)
{
  queue* elem, *tmp;

  gbfputint32(0x80008, fout);
  gbfputint32(data->sz, fout);
  gbfputint32(23, fout);	/* bounds + three bytes */

  gbfputint32(GPS_Math_Deg_To_Semi(data->bds.max_lat), fout);
  gbfputint32(GPS_Math_Deg_To_Semi(data->bds.max_lon), fout);
  gbfputint32(GPS_Math_Deg_To_Semi(data->bds.min_lat), fout);
  gbfputint32(GPS_Math_Deg_To_Semi(data->bds.min_lon), fout);

  gbfputint32(0, fout);
  gbfputint16(1, fout);
  gbfputc(data->alert, fout);

  QUEUE_FOR_EACH(&data->Q, elem, tmp) {
    char* str;
    int s0, s1;
    waypoint* wpt = (waypoint*)elem;
    gpi_waypt_t* dt = (gpi_waypt_t*) wpt->extra_data;

    str = wpt->description;
    if (! str) {
      str = wpt->notes;
    }
//		if (str && (strcmp(str, wpt->shortname) == 0)) str = NULL;

    gbfputint32(0x80002, fout);

    s0 = s1 = 19 + strlen(wpt->shortname);
    if (! opt_hide_bitmap) {
      s0 += 10;  /* tag(4) */
    }
    if (str) {
      s0 += (12 + 4 + strlen(str));  /* descr */
    }
    if (dt->sz) {
      s0 += (12 + dt->sz);  /* address part */
    }
    if (dt->phone_nr) {
      s0 += (12 + 4 + strlen(dt->phone_nr));
    }
    if (dt->alerts) {
      s0 += 20;  /* tag(3) */
    }

    gbfputint32(s0, fout);	/* size of following data (tag) */
    gbfputint32(s1, fout);	/* basic size (without options) */

    gbfputint32(GPS_Math_Deg_To_Semi(wpt->latitude), fout);
    gbfputint32(GPS_Math_Deg_To_Semi(wpt->longitude), fout);

    gbfputint16(1, fout);	/* ? always 1 ? */
    gbfputc(alerts, fout);	/* seems to be 1 when extra options present */

    write_string(wpt->shortname, 1);

    if (dt->alerts) {
      char flag = 0;

      gbfputint32(3, fout);	/* tag(3) */
      gbfputint32(12, fout);	/* always 12 */

      if (WAYPT_HAS(wpt, proximity) && (wpt->proximity > 0)) {
        gbfputint16((int) wpt->proximity, fout);
        flag = 4;
      } else {
        gbfputint16(0, fout);
      }
      if (WAYPT_HAS(wpt, speed) && (wpt->speed > 0)) {
        gbfputint16((int)(wpt->speed * 100), fout);
        flag = 5;
      } else {
        gbfputint16(0, fout);
      }

      gbfputint32(0x100100, fout);	/* ??? */
      gbfputc(1, fout);		/* ??? */
      gbfputc(1, fout);		/* ??? */
      gbfputc(flag, fout);
      gbfputc(0x10, fout);		/* ??? */
    }

    if (! opt_hide_bitmap) {
      gbfputint32(4, fout);	/* tag(4) */
      gbfputint32(2, fout);	/* ? always 2 == version ??? */
      gbfputint16(0, fout);
    }

    if (str) {
      gbfputint32(0xa, fout);
      gbfputint32(strlen(str) + 8, fout);	/* string + string header */
      write_string(str, 1);
    }

    if (dt->sz) {					/* gpi address */
      gbfputint32(0x8000b, fout);
      gbfputint32(dt->sz, fout);
      gbfputint32(0x2, fout);			/* ? always 2 ? */
      gbfputint16(dt->mask, fout);
      if (dt->mask & GPI_ADDR_CITY) {
        write_string(dt->city, 1);
      }
      if (dt->mask & GPI_ADDR_COUNTRY) {
        write_string(dt->country, 1);
      }
      if (dt->mask & GPI_ADDR_STATE) {
        write_string(dt->state, 1);
      }
      if (dt->mask & GPI_ADDR_POSTAL_CODE) {
        write_string(dt->postal_code, 0);
      }
      if (dt->mask & GPI_ADDR_ADDR) {
        write_string(dt->addr, 1);
      }
    }

    if (dt->phone_nr) {
      gbfputint32(0x8000c, fout);
      gbfputint32(strlen(dt->phone_nr) + 2 + 2, fout);
      gbfputint32(0x2, fout);			/* ? always 2 ? */
      gbfputint16(1, fout);			/* mask */
      write_string(dt->phone_nr, 0);
    }
  }

  if (data->top_left) {
    wdata_write(data->top_left);
  }
  if (data->top_right) {
    wdata_write(data->top_right);
  }
  if (data->buttom_left) {
    wdata_write(data->buttom_left);
  }
  if (data->buttom_right) {
    wdata_write(data->buttom_right);
  }
}


static void
write_category(const char* category, const char* image, const int image_sz)
{
  int sz;

  sz = wdata_compute_size(wdata);
  sz += 8;	/* string header */
  sz += strlen(opt_cat);

  gbfputint32(0x80009, fout);
  if ((! opt_hide_bitmap) && image_sz) {
    gbfputint32(sz + image_sz + 8, fout);
  } else {
    gbfputint32(sz, fout);
  }
  gbfputint32(sz, fout);
  write_string(opt_cat, 1);

  wdata_write(wdata);

  if ((! opt_hide_bitmap) && image_sz) {
    gbfputint32(5, fout);
    gbfputint32(image_sz, fout);
    gbfwrite(image, 1, image_sz, fout);
  }
}


static void
write_header(void)
{
  time_t time = gpi_timestamp;

  if (time != 0) {
    struct tm tm;
    tm = *gmtime(&time);
    tm.tm_year -= 20;
    time = mkgmtime(&tm);
    time += SECONDS_PER_DAY;
  }

  gbfputint32(0, fout);
  gbfputint32(0x16, fout);
  gbfwrite("GRMREC00", 1, 8, fout);
  gbfputint32(time, fout);
  gbfputint16(0, fout);
  gbfputint16(6, fout);
  gbfwrite("my.gpi", 1, 6, fout);
  gbfputint32(1, fout);
  gbfputint32(0xc, fout);
  gbfwrite("POI", 1, 3, fout);
  gbfputc(0, fout);
  gbfputc(0, fout);
  gbfputc(0, fout);
  gbfwrite("00", 1, 2, fout);
  gbfputint16(codepage, fout);
  gbfputint16(0, fout);
}


static void
enum_waypt_cb(const waypoint* ref)
{
  waypoint* wpt;
  char* str;
  queue* elem, *tmp;

  QUEUE_FOR_EACH(&wdata->Q, elem, tmp) {
    waypoint* cmp = (waypoint*) elem;

    /* sort out nearly equal waypoints */
    if ((compare_strings(cmp->shortname, ref->shortname) == 0) &&
        (cmp->latitude == ref->latitude) &&
        (cmp->longitude == ref->longitude) &&
        (compare_strings(cmp->description, ref->description) == 0) &&
        (compare_strings(cmp->notes, ref->notes) == 0)) {
      return;
    }
  }

  wpt = waypt_dupe(ref);

  if (*opt_unique == '1') {
    str = mkshort(short_h, wpt->shortname);
    xfree(wpt->shortname);
    wpt->shortname = str;
  }

  wdata_add_wpt(wdata, wpt);
}


static void
load_bitmap_from_file(const char* fname, char** data, int* data_sz)
{
  gbfile* f;
  int i, sz;
  int dest_bpp;
  int src_line_sz, dest_line_sz;
  bmp_header_t src_h;
  int* color_table = NULL;
  gpi_bitmap_header_t* dest_h;
  char* ptr;

  f = gbfopen_le(fname, "rb", MYNAME);
  is_fatal(gbfgetint16(f) != 0x4d42, MYNAME ": No BMP image.");

  /* read a standard bmp file header */
  src_h.size = gbfgetint32(f);
  src_h.res1 = gbfgetint16(f);
  src_h.res2 = gbfgetint16(f);
  src_h.image_offset = gbfgetint32(f);
  src_h.header_size = gbfgetint32(f);
  src_h.width = gbfgetint32(f);
  src_h.height = gbfgetint32(f);
  src_h.planes = gbfgetint16(f);
  src_h.bpp = gbfgetint16(f);
  src_h.compression_type = gbfgetint32(f);
  src_h.image_data_size = gbfgetint32(f);
  src_h.resolution_h = gbfgetint32(f);
  src_h.resolution_v = gbfgetint32(f);
  src_h.used_colors = gbfgetint32(f);
  src_h.important_colors = gbfgetint32(f);

  /* Workaround for indexed BMP's with used_colors = 0 */
  if ((src_h.bpp == 8) && (src_h.used_colors == 0)) {
    src_h.used_colors = (src_h.image_offset - gbftell(f)) / 4;
  }

#ifdef GPI_DBG
  printf("data size:             0x%1$x (%1$d)\n", src_h.size);
  printf("image data offset:     0x%1$x (%1$d)\n", src_h.image_offset);
  printf("header size:           0x%1$x (%1$d)\n", src_h.header_size);
  printf("image width:           0x%1$x (%1$d)\n", src_h.width);
  printf("image height:          0x%1$x (%1$d)\n", src_h.height);
  printf("number of planes:      0x%1$x (%1$d)\n", src_h.planes);
  printf("bits per pixel:        0x%1$x (%1$d)\n", src_h.bpp);
  printf("compression type:      0x%1$x (%1$d)\n", src_h.compression_type);
  printf("image size:            0x%1$x (%1$d)\n", src_h.image_data_size);
  printf("horizontal resolution: 0x%1$x (%1$d)\n", src_h.resolution_h);
  printf("vertical resolution:   0x%1$x (%1$d)\n", src_h.resolution_v);
  printf("number of colors:      0x%1$x (%1$d)\n", src_h.used_colors);
  printf("important colors:      0x%1$x (%1$d)\n", src_h.important_colors);
#endif

  /* sort out unsupported files */
  if (!((src_h.width <= 24) && (src_h.height <= 24) &&
        (src_h.width > 0) && (src_h.height > 0))) {
    fatal(MYNAME ": Unsupported format (%dx%d)!\n", src_h.width, src_h.height);
  }
  if (!((src_h.bpp == 8) || (src_h.bpp == 24) || (src_h.bpp == 32))) {
    fatal(MYNAME ": Unsupported color depth (%d)!\n", src_h.bpp);
  }
  if (!(src_h.compression_type == 0)) {
    fatal(MYNAME ": Sorry, we don't support compressed bitmaps.\n");
  }

  if (src_h.used_colors > 0) {
    color_table = (int*) xmalloc(4 * src_h.used_colors);
    gbfread(color_table, 1, 4 * src_h.used_colors, f);
    for (i = 0; i < src_h.used_colors; i++) {
      int color = color_table[i];
      /* swap blue and red value */
      color = (color >> 16) | (color << 16) | (color & 0x00ff00);
      color_table[i] = color & 0xffffff;
    }
  }

  /* calculate line-size for source and destination */
  src_line_sz = (src_h.width * src_h.bpp) / 8;
  src_line_sz = ((int)((src_line_sz + 3) / 4)) * 4;

  if (src_h.bpp == 24) {
    dest_bpp = 32;
  } else {
    dest_bpp = src_h.bpp;
  }

  dest_line_sz = (src_h.width * dest_bpp) / 8;
  dest_line_sz = ((int)((dest_line_sz + 3) / 4)) * 4;

  sz = sizeof(*dest_h) + (src_h.height * dest_line_sz);
  if (src_h.used_colors) {
    sz += (src_h.used_colors * 4);
  }

  ptr = (char*) xmalloc(sz);
  dest_h = (gpi_bitmap_header_t*)ptr;
  *data = ptr;
  *data_sz = sz;

  le_write16(&dest_h->index, 0);
  le_write16(&dest_h->height, src_h.height);
  le_write16(&dest_h->width, src_h.width);
  le_write16(&dest_h->line_sz, dest_line_sz);
  le_write16(&dest_h->bpp, dest_bpp);
  le_write16(&dest_h->fixed_0, 0);		/* seems to be fixed */
  le_write32(&dest_h->image_size, dest_line_sz * src_h.height);
  le_write32(&dest_h->fixed_2c, 0x2c);		/* seems to be fixed */
  le_write32(&dest_h->flag1, (dest_bpp == 8) ? 0x100 : 0);
  le_write32(&dest_h->tr_color, 0xff00ff);	/* magenta = transparent color */
  le_write32(&dest_h->flag2, 0x1);		/* ? enable transparent mode ? */
  le_write32(&dest_h->size_2c, (dest_line_sz * src_h.height) + 0x2c);

  /* copy and revert order of BMP lines */
  ptr = (char*)dest_h;
  ptr += (sizeof(*dest_h) + (dest_line_sz * (src_h.height - 1)));

  if (src_h.bpp == 24) {
    /* 24 bpp seems to be not supported, convert to 32 bpp */
    for (i = 0; i < src_h.height; i++) {
      int j;
      char* p = ptr;

      for (j = 0; j < src_h.width; j++) {
        int color;
        color = (gbint32)gbfgetint16(f) | (gbfgetc(f) << 16);
        le_write32(p, color);
        p += 4;
      }
      for (j = (src_h.width * src_h.bpp) / 8; j < src_line_sz; j++) {
        gbfgetc(f);  /* drop fill-in bytes */
      }
      ptr -= dest_line_sz;
    }
  } else for (i = 0; i < src_h.height; i++) {
      gbfread(ptr, 1, src_line_sz, f);
      ptr -= dest_line_sz;
    }

  if (src_h.used_colors > 0) {
    ptr = (char*)dest_h;
    ptr += (sizeof(*dest_h) + (src_h.height * src_line_sz));

    for (i = 0; i < src_h.used_colors; i++) {
      le_write32(ptr, color_table[i]);
      ptr += 4;
    }
  }

  if (color_table) {
    xfree(color_table);
  }
  gbfclose(f);
}

/*******************************************************************************
* %%%        global callbacks called by gpsbabel main process              %%% *
*******************************************************************************/

static void
garmin_gpi_rd_init(const char* fname)
{
  char cp[8];

  fin = gbfopen_le(fname, "rb", MYNAME);
  rdata = (reader_data_t*) xcalloc(1, sizeof(*rdata));

  read_header();

  if ((codepage >= 1250) && (codepage <= 1257)) {
    snprintf(cp, sizeof(cp), "CP%d", codepage);
    cet_convert_init(cp, 1);
  } else {
    warning(MYNAME ": Unsupported code page (%d).\n", codepage);
  }

  units = tolower(opt_units[0]);
  if ((units != 'm') && (units != 's')) {
    fatal(MYNAME ": Unknown units parameter (%c).\n", opt_units[0]);
  }
}


static void
garmin_gpi_wr_init(const char* fname)
{
  char cp[8];
  cet_cs_vec_t* vec;
  int i;

  if (gpi_timestamp != 0) {			/* not the first gpi output session */
    time_t t = time(NULL);
    if (t <= gpi_timestamp) {
      gpi_timestamp++;  /* don't create files with same timestamp */
    } else {
      gpi_timestamp = t;
    }
  } else {
    gpi_timestamp = gpsbabel_time;  /* always ZERO during 'testo' */
  }

  fout = gbfopen_le(fname, "wb", MYNAME);

  short_h = mkshort_new_handle();

  setshort_length(short_h, 1024);
  setshort_badchars(short_h, "\r\n");
  setshort_mustupper(short_h, 0);
  setshort_mustuniq(short_h, 1);
  setshort_whitespace_ok(short_h, 1);
  setshort_repeating_whitespace_ok(short_h, 0);
  setshort_defname(short_h, "POI");

  codepage = 0;

  for (i = 1250; i <= 1257; i++) {
    snprintf(cp, sizeof(cp), "CP%d", i);
    vec = cet_find_cs_by_name(cp);
    if (vec == global_opts.charset) {
      codepage = i;
      break;
    }
  }

  if (! codepage) {
    warning(MYNAME ": Unsupported character set (%s)!\n", global_opts.charset_name);
    fatal(MYNAME ": Valid values are CP1250 to CP1257.\n");
  }

  units = tolower(opt_units[0]);
  if ((units != 'm') && (units != 's')) {
    fatal(MYNAME ": Unknown units parameter (%c).\n", opt_units[0]);
  }

  alerts = (opt_alerts) ? 1 : 0;

  if (opt_speed) {
    double scale;
    alerts = 1;					/* Force alerts to be enabled */
    if (units == 's') {
      scale = MPH_TO_MPS(1);  /* We need speed in meters per second */
    } else {
      scale = KPH_TO_MPS(1);
    }
    parse_speed(opt_speed, &defspeed, scale, MYNAME);
  }

  if (opt_proximity) {
    double scale;
    alerts = 1; 					/* Force alerts to be enabled */
    if (units == 's') {
      scale = MILES_TO_METERS(1);  /* We need proximity in meters */
    } else {
      scale = 1000.0;  /* one kilometer in meters */
    }
    parse_distance(opt_proximity, &defproximity, scale, MYNAME);
  }
  wdata = wdata_alloc();
}


static void
garmin_gpi_rd_deinit(void)
{
  if (rdata->category) {
    xfree(rdata->category);
  }
  if (rdata->group) {
    xfree(rdata->group);
  }
  xfree(rdata);
  gbfclose(fin);
}


static void
garmin_gpi_wr_deinit(void)
{
  wdata_free(wdata);
  mkshort_del_handle(&short_h);
  gbfclose(fout);

  if ((opt_sleep) && (gpi_timestamp != 0)) {	/* don't sleep during 'testo' */
    int sleep = atoi(opt_sleep);
    if (sleep < 1) {
      sleep = 1;
    }
    gpi_timestamp += sleep;
    while (gpi_timestamp > time(NULL)) {
      gb_sleep(100);
    }
  }
}


static void
garmin_gpi_read(void)
{
  while (1) {
    int tag = gbfgetint32(fin);
    if (tag == 0xffff) {
      return;
    }
    if (! read_tag("garmin_gpi_read", tag, NULL)) {
      return;
    }
  };
}


static void
garmin_gpi_write(void)
{
  char* image;
  int image_sz;

  if (strlen(opt_cat) == 0) {
    fatal(MYNAME ": Can't write empty category!\n");
  }

  if (opt_hide_bitmap) {
    image = NULL;
    image_sz = 0;
  } else if (opt_bitmap && *opt_bitmap) {
    load_bitmap_from_file(opt_bitmap, &image, &image_sz);
  } else {
    image = gpi_bitmap;	/* embedded GPSBabel icon in gpi format */
    image_sz = GPI_BITMAP_SIZE;
  }
  waypt_disp_all(enum_waypt_cb);

  wdata_check(wdata);
  write_header();
  write_category(opt_cat, image, image_sz);

  gbfputint32(0xffff, fout);	/* final tag */
  gbfputint32(0, fout);		/* ? dummy size ? */

  if (image != gpi_bitmap) {
    xfree(image);
  }
}

/**************************************************************************/

ff_vecs_t garmin_gpi_vecs = {
  ff_type_file,
  {
    (ff_cap)(ff_cap_read | ff_cap_write) 	/* waypoints */,
    ff_cap_none 			/* tracks */,
    ff_cap_none 			/* routes */
  },
  garmin_gpi_rd_init,
  garmin_gpi_wr_init,
  garmin_gpi_rd_deinit,
  garmin_gpi_wr_deinit,
  garmin_gpi_read,
  garmin_gpi_write,
  NULL,
  garmin_gpi_args,
  CET_CHARSET_MS_ANSI, 0		/* WIN-CP1252 */
};

/**************************************************************************/
