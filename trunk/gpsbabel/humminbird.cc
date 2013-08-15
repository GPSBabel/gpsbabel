/*
    Copyright (C) 2008  Björn Augustsson, oggust@gmail.com
    Copyright (C) 2008  Olaf Klein, o.b.klein@gpsbabel.org
    Copyright (C) 2005-2013 Robert Lipe, robertlipe@gpsbabel.org

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

#include <ctype.h>
#include <math.h>
#include <string.h>
#include <QtCore/QMap>

#include "defs.h"

#define MYNAME "humminbird"

#define WPT_NAME_LEN		12
#define RTE_NAME_LEN		20
#define TRK_NAME_LEN		20
#define MAX_RTE_POINTS		50
#define MAX_ITEMS_PER_GROUP	12

/*
I suspect that these are actually
struct signature {
	uint8_t format, // 1 = track, 2 = waypoint, 3 = route, 4 = iTrack
	uint8_t version,
	gpuint16 record_length
}

The v3 TRK_MAGIC doesn't have a length, probably because it wouldn't fit.
(It would be 0x200008)

Still, they're useful in the code as a plain signature.
*/
#define TRK_MAGIC		0x01030000L
#define TRK_MAGIC2		0x01021F70L
#define WPT_MAGIC		0x02020024L
#define WPT_MAGIC2		0x02030024L // New for 2013.  No visible diff?!
#define RTE_MAGIC		0x03030088L

#define EAST_SCALE		20038297.0 /* this is i1924_equ_axis*M_PI */
#define i1924_equ_axis		6378388.0
#define i1924_polar_axis	6356911.946

#define BAD_CHARS		"\r\n\t"

/* The hwr data format is records-based, and the records are 36 bytes long. */

typedef struct humminbird_waypt_s {
  /* O.K.: the file can also contain routes with a different magic. */
  /* uint32_t signature; */   /* Just for error checking(?) */
  uint16_t num;          /* Always ascending in the file. */
  uint16_t zero;         /* Always seems to be zero. */
  uint8_t  status;       /* Always seems to be 1. Ends up as <h:status>
	                          in gpx files exported by HumminbirdPC. */
  uint8_t  icon;         /* See below */
  uint16_t depth;        /* Water depth. These are fishfinders. In centimeters */
  uint32_t time;         /* This is a time_t. In UTC */
  int32_t  east;
  int32_t  north;
  char     name[WPT_NAME_LEN];
} humminbird_waypt_t;

typedef struct humminbird_rte_s {
  /* O.K.: the file can contain also routes with a different magic. */
  /* uint32_t signature; */   /* Just for error checking(?) */
  uint16_t num;
  uint16_t zero;
  uint8_t  status;
  uint8_t  U0;
  uint8_t  U1;
  int8_t   count;
  uint32_t time;
  char     name[RTE_NAME_LEN];
  uint16_t points[MAX_RTE_POINTS];
} humminbird_rte_t;

typedef struct humminbird_trk_header_s {      /* 68 bytes, incl signature */
  /* uint32_t signature; */
  uint16_t trk_num;
  uint16_t zero;
  uint16_t num_points;
  uint16_t unknown;	/* Always zero so far. */
  uint32_t time;		/* a time_t, in UTC */

  int32_t start_east;	/* Start of track */
  int32_t start_north;
  int32_t end_east;	/* end of track */
  int32_t end_north;

  int32_t sw_east; 	/* Bounding box, enclosing the track */
  int32_t sw_north;	/* sw is the south-west point */
  int32_t ne_east;	/* ne is the north-east point */
  int32_t ne_north;

  char     name[20];
} humminbird_trk_header_t;


typedef struct humminbird_trk_point_s {
  int16_t  deltaeast;
  int16_t  deltanorth;
  uint16_t depth;		/* in centimeters */
} humminbird_trk_point_t;

typedef struct humminbird_trk_header_old_s {      /* 16 bytes, incl signature */
  /* uint32_t signature; */
  uint16_t trk_num;
  uint16_t zero;
  uint16_t num_points;
  uint16_t unknown;	/* Always zero so far. */
  uint32_t time;		/* a time_t, in UTC */

  int32_t start_east;	/* Start of track */
  int32_t start_north;
  int32_t end_east;	/* end of track */
  int32_t end_north;

} humminbird_trk_header_old_t;

typedef struct humminbird_trk_point_old_s {
  int16_t  deltaeast;
  int16_t  deltanorth;
} humminbird_trk_point_old_t;

typedef struct group_header {
  uint8_t status;
  uint8_t icon;
  uint16_t depth;
  uint32_t time;		/* a time_t, in UTC */
  uint16_t parent_idx;
  uint16_t reserved1;
  uint16_t first_body_index;
  uint16_t reserved2;
  char name[WPT_NAME_LEN];
} group_header_t;

typedef struct group_body {
  uint8_t status;
  uint8_t icon;
  uint16_t next_idx;
  uint16_t item[MAX_ITEMS_PER_GROUP];
} group_body_t;


static const char* humminbird_icons[] = {
  "Normal",       /*  0 */
  "House",        /*  1 */
  "Red cross",    /*  2 */
  "Fish",         /*  3 */
  "Duck",         /*  4 */
  "Anchor",       /*  5 */
  "Buoy",         /*  6 */
  "Airport",      /*  7 */
  "Camping",      /*  8 */
  "Danger",       /*  9 */
  "Fuel",         /* 10 */
  "Rock",         /* 11 */
  "Weed",         /* 12 */
  "Wreck",        /* 13 */
  "Phone",        /* 14 */
  "Coffee",       /* 15 */
  "Beer",         /* 16 */
  "Mooring",      /* 17 */
  "Pier",         /* 18 */
  "Slip",         /* 19 */
  "Ramp",         /* 20 */
  "Circle",       /* 21 */
  "Diamond",      /* 22 */
  "Flag",         /* 23 */
  "Pattern",      /* 24 */
  "Shower",       /* 25 */
  "Water tap",    /* 26 */
  "Tree",         /* 27 */
  "Recording",    /* 28 */
  "Snapshot"      /* 29 */
};

static gbfile* fin;
static gbfile* fout;
static int waypoint_num;
static short_handle wptname_sh, rtename_sh, trkname_sh;
static humminbird_rte_t* humrte;
static int rte_num;
static QMap<QString, waypoint*> map;

static
arglist_t humminbird_args[] = {
  ARG_TERMINATOR
};

/* Takes a latitude in degrees,
 * returns a latitude in degrees. */
static double
geodetic_to_geocentric_hwr(const double gd_lat)
{
  const double cos_ae = 0.9966349016452;
  const double cos2_ae = cos_ae * cos_ae;
  const double gdr = gd_lat *M_PI / 180.0;

  return atan(cos2_ae * tan(gdr)) * 180.0/M_PI;
}

/* Takes a latitude in degrees,
 * returns a latitude in degrees. */
static double
geocentric_to_geodetic_hwr(const double gc_lat)
{
  const double cos_ae = 0.9966349016452;
  const double cos2_ae = cos_ae * cos_ae;
  const double gcr = gc_lat *M_PI / 180.0;

  return atan(tan(gcr)/cos2_ae) * 180.0/M_PI;
}

/* Takes a projected "north" value, returns latitude in degrees. */
static double
gudermannian_i1924(const double x)
{
  const double norm_x = x/i1924_equ_axis;

  return atan(sinh(norm_x)) * 180.0/M_PI;
}

/* Takes latitude in degrees, returns projected "north" value. */
static double
inverse_gudermannian_i1924(const double x)
{
  const double x_r = x/180.0 * M_PI;
  const double guder = log(tan(M_PI/4.0 + x_r/2.0));

  return guder * i1924_equ_axis;
}

/*******************************************************************************
* %%%        global callbacks called by gpsbabel main process              %%% *
*******************************************************************************/

static void
humminbird_rd_init(const char* fname)
{
  fin = gbfopen_be(fname, "rb", MYNAME);
}

static void
humminbird_rd_deinit(void)
{
  gbfclose(fin);
}

static void
humminbird_read_wpt(gbfile* fin)
{
  humminbird_waypt_t w;
  double guder;
  int num_icons;
  waypoint* wpt;

  if (! gbfread(&w, 1, sizeof(w), fin)) {
    fatal(MYNAME ": Unexpected end of file!\n");
  }

  /* Fix endianness - these are now BE */
  w.num = be_read16(&w.num);
  w.zero = be_read16(&w.zero);
  w.depth = be_read16(&w.depth);
  w.time = be_read32(&w.time);
  w.north = be_read32(&w.north);
  w.east = be_read32(&w.east);

  /* All right! Copy the data to the gpsbabel struct... */

  wpt = waypt_new();

  wpt->shortname = xstrndup(w.name, sizeof(w.name));
  wpt->SetCreationTime(w.time);

  guder = gudermannian_i1924(w.north);
  wpt->latitude = geocentric_to_geodetic_hwr(guder);
  wpt->longitude = (double)w.east / EAST_SCALE * 180.0;

  wpt->altitude  = 0.0; /* It's from a fishfinder... */

  if (w.depth != 0) {
    WAYPT_SET(wpt,depth,(double)w.depth / 100.0);
  }

  num_icons = sizeof(humminbird_icons) / sizeof(humminbird_icons[0]);
  if (w.icon < num_icons) {
    wpt->icon_descr = humminbird_icons[w.icon];
  }

  // In newer versions, this is an enum (though it looks like a bitfield)
  // that describes a sub-status
  switch (w.status) {
  case 0: // Waypoint not used.  So why do we have one?
    waypt_free(wpt);
    break;
  case 1: // Waypoint permanent.
  case 2: // Waypoint temporary.
  case 3: // Waypoint man-overboard.
    waypt_add(wpt);
    break;
  case 16: // Waypoint group header.
  case 17: // Waypoint group body.
  case 63: // Waypoint group invalid.
  default:
    waypt_free(wpt);
    break;
  }

  /* register the point over his internal Humminbird "Number" */
  QString buff = QString::number(w.num);
  map[buff] = wpt;
}

static void
humminbird_read_route(gbfile* fin)
{

  humminbird_rte_t hrte;

  if (! gbfread(&hrte, 1, sizeof(hrte), fin)) {
    fatal(MYNAME ": Unexpected end of file!\n");
  }

  hrte.time = be_read32(&hrte.time);
  hrte.num = be_read16(&hrte.num);

  if (hrte.count > 0) {
    int i;
    route_head* rte = NULL;

    for (i = 0; i < hrte.count; i++) {
      const waypoint* wpt;
      char buff[10];
      hrte.points[i] = be_read16(&hrte.points[i]);

      /* locate the point over his internal Humminbird "Number" */
      snprintf(buff, sizeof(buff), "%d", hrte.points[i]);
      if ((map.value(buff))) {
        wpt = map.value(buff);
        if (rte == NULL) {
          rte = route_head_alloc();
          route_add_head(rte);
          rte->rte_name = xstrndup(hrte.name, sizeof(hrte.name));
          /* rte->rte_num = hrte.num + 1; only internal number */
        }
        route_add_wpt(rte, waypt_dupe(wpt));
      }
    }
  }
}

static void
humminbird_read_track(gbfile* fin)
{

  humminbird_trk_header_t th;
  humminbird_trk_point_t* points;
  route_head* trk;
  waypoint* first_wpt;
  int i;
  int max_points = 0;
  int32_t accum_east;
  int32_t accum_north;
  double g_lat;

  if (! gbfread(&th, 1, sizeof(th), fin)) {
    fatal(MYNAME ": Unexpected end of file reading header!\n");
  }

  th.trk_num     = be_read16(&th.trk_num);
  th.num_points  = be_read16(&th.num_points);
  th.time        = be_read32(&th.time);

  th.start_east  = be_read32(&th.start_east);
  th.start_north = be_read32(&th.start_north);
  th.end_east    = be_read32(&th.end_east);
  th.end_north   = be_read32(&th.end_north);

  th.sw_east     = be_read32(&th.sw_east);
  th.sw_north    = be_read32(&th.sw_north);
  th.ne_east     = be_read32(&th.ne_east);
  th.ne_north    = be_read32(&th.ne_north);

  max_points = (131080 - sizeof(uint32_t) - sizeof(th)) / sizeof(humminbird_trk_point_t);

  if (th.num_points > max_points) {
    fatal(MYNAME ": Too many track points! (%d)\n", th.num_points);
  }

  /* num_points is actually one too big, because it includes the value in
     the header. But we want the extra point at the end because the
     freak-value filter below looks at points[i+1] */
  points = (humminbird_trk_point_t*) xcalloc(th.num_points, sizeof(humminbird_trk_point_t));
  if (! gbfread(points, sizeof(humminbird_trk_point_t), th.num_points-1, fin)) {
    fatal(MYNAME ": Unexpected end of file reading points!\n");
  }

  accum_east  = th.start_east;
  accum_north = th.start_north;

  trk = route_head_alloc();
  track_add_head(trk);

  trk->rte_name = xstrndup(th.name, sizeof(th.name));
  trk->rte_num  = th.trk_num;

  /* We create one wpt for the info in the header */

  first_wpt = waypt_new();
  g_lat = gudermannian_i1924(accum_north);
  first_wpt->latitude  = geocentric_to_geodetic_hwr(g_lat);
  first_wpt->longitude = accum_east/EAST_SCALE * 180.0;
  first_wpt->altitude  = 0.0;
  /* No depth info in the header. */
  track_add_wpt(trk, first_wpt);

  for (i=0 ; i<th.num_points-1 ; i++) {
    waypoint* wpt = waypt_new();
    int16_t next_deltaeast, next_deltanorth;
    double guder;

    points[i].depth      = be_read16(&points[i].depth);
    points[i].deltaeast  = be_read16(&points[i].deltaeast);
    points[i].deltanorth = be_read16(&points[i].deltanorth);

    /* Every once in a while the delta values are
       32767 followed by -32768. Filter that. */

    next_deltaeast = be_read16(&points[i+1].deltaeast);
    if (points[ i ].deltaeast ==  32767 &&
        next_deltaeast        == -32768) {
      points[ i ].deltaeast = -1;
      points[i+1].deltaeast =  0; /* BE 0 == LE 0 */
    }
    next_deltanorth = be_read16(&points[i+1].deltanorth);
    if (points[ i ].deltanorth ==  32767 &&
        next_deltanorth        == -32768) {
      points[ i ].deltanorth = -1;
      points[i+1].deltanorth =  0;
    }

    accum_east  += points[i].deltaeast;
    accum_north += points[i].deltanorth;

    guder = gudermannian_i1924(accum_north);
    wpt->latitude  = geocentric_to_geodetic_hwr(guder);
    wpt->longitude = accum_east/EAST_SCALE * 180.0;
    wpt->altitude  = 0.0;

    if (points[i].depth != 0) {
      WAYPT_SET(wpt,depth,(double)points[i].depth / 100.0);
    }

    if (i == th.num_points-2 && th.time != 0) {
      /* Last point. Add the date from the header. */
      /* Unless it's zero. Sometimes happens, possibly if
         the gps didn't have a lock when the track was
         saved. */
      wpt->SetCreationTime(th.time);
    }
    track_add_wpt(trk, wpt);
  }
  xfree(points);
}

static void
humminbird_read_track_old(gbfile* fin)
{

  humminbird_trk_header_old_t th;
  humminbird_trk_point_old_t* points;
  route_head* trk;
  waypoint* first_wpt;
  int i;
  int max_points = 0;
  int32_t accum_east;
  int32_t accum_north;
  double g_lat;
  const int file_len = 8048;
  char namebuf[TRK_NAME_LEN];


  if (! gbfread(&th, 1, sizeof(th), fin)) {
    fatal(MYNAME ": Unexpected end of file reading header!\n");
  }

  th.trk_num     = be_read16(&th.trk_num);
  th.num_points  = be_read16(&th.num_points);
  th.time        = be_read32(&th.time);

  th.start_east  = be_read32(&th.start_east);
  th.start_north = be_read32(&th.start_north);
  th.end_east    = be_read32(&th.end_east);
  th.end_north   = be_read32(&th.end_north);

  // These files are always 8048 bytes long. Note that that's the value
  // of the second 16-bit word in the signature.
  max_points = (file_len - (sizeof(th) + sizeof(uint32_t) + TRK_NAME_LEN)) / sizeof(humminbird_trk_point_old_t);

  if (th.num_points > max_points) {
    fatal(MYNAME ": Too many track points! (%d)\n", th.num_points);
  }

  /* num_points is actually one too big, because it includes the value in
     the header. But we want the extra point at the end because the
     freak-value filter below looks at points[i+1] */
  points = (humminbird_trk_point_old_t*)xcalloc(th.num_points, sizeof(humminbird_trk_point_old_t));
  if (! gbfread(points, sizeof(humminbird_trk_point_old_t), th.num_points-1, fin)) {
    fatal(MYNAME ": Unexpected end of file reading points!\n");
  }

  accum_east  = th.start_east;
  accum_north = th.start_north;

  trk = route_head_alloc();
  track_add_head(trk);

  /* The name is not in the header, but at the end of the file.
     (The last 20 bytes.) */

  gbfseek(fin, file_len-TRK_NAME_LEN, SEEK_SET);
  gbfread(&namebuf, 1, TRK_NAME_LEN, fin);
  trk->rte_name = xstrndup(namebuf, sizeof(namebuf));

  trk->rte_num  = th.trk_num;

  /* We create one wpt for the info in the header */

  first_wpt = waypt_new();
  g_lat = gudermannian_i1924(accum_north);
  first_wpt->latitude  = geocentric_to_geodetic_hwr(g_lat);
  first_wpt->longitude = accum_east/EAST_SCALE * 180.0;
  first_wpt->altitude  = 0.0;
  track_add_wpt(trk, first_wpt);

  for (i=0 ; i<th.num_points-1 ; i++) {
    waypoint* wpt = waypt_new();
//		int16_t next_deltaeast, next_deltanorth;
    double guder;

    points[i].deltaeast  = be_read16(&points[i].deltaeast);
    points[i].deltanorth = be_read16(&points[i].deltanorth);

//		I've commented this out, don't know if it happens in this
//		format. It happens in the newer version though.

//		/* Every once in a while the delta values are
//		   32767 followed by -32768. Filter that. */
//
//		next_deltaeast = be_read16(&points[i+1].deltaeast);
//		if (points[ i ].deltaeast ==  32767 &&
//		    next_deltaeast        == -32768) {
//			points[ i ].deltaeast = -1;
//			points[i+1].deltaeast =  0; /* BE 0 == LE 0 */
//		}
//		next_deltanorth = be_read16(&points[i+1].deltanorth);
//		if (points[ i ].deltanorth ==  32767 &&
//		    next_deltanorth        == -32768) {
//			points[ i ].deltanorth = -1;
//			points[i+1].deltanorth =  0;
//		}
//
    accum_east  += points[i].deltaeast;
    accum_north += points[i].deltanorth;

    guder = gudermannian_i1924(accum_north);
    wpt->latitude  = geocentric_to_geodetic_hwr(guder);
    wpt->longitude = accum_east/EAST_SCALE * 180.0;
    wpt->altitude  = 0.0;

    if (i == th.num_points-2 && th.time != 0) {
      /* Last point. Add the date from the header. */
      /* Unless it's zero. Sometimes happens, possibly if
         the gps didn't have a lock when the track was
         saved. */
      wpt->SetCreationTime(th.time);
    }
    track_add_wpt(trk, wpt);
  }
  xfree(points);
}

static void
humminbird_read(void)
{
  while (! gbfeof(fin)) {
    uint32_t signature;

    signature = gbfgetuint32(fin);

    switch (signature) {
    case WPT_MAGIC:
    case WPT_MAGIC2:
      humminbird_read_wpt(fin);
      break;
    case RTE_MAGIC:
      humminbird_read_route(fin);
      break;
    case TRK_MAGIC:
      humminbird_read_track(fin);
      return; /* Don't continue. The rest of the file is all zeores */
    case TRK_MAGIC2:
      humminbird_read_track_old(fin);
      return; /* Don't continue. The rest of the file is all zeores */
    default:
      fatal(MYNAME ": Invalid record header \"0x%08X\" (no or unknown humminbird file)!\n", signature);
    }
  }
}


/************************************************************************************************/

static void
humminbird_wr_init(const char* fname)
{
  fout = gbfopen_be(fname, "wb", MYNAME);

  wptname_sh = mkshort_new_handle();

  setshort_length(wptname_sh, WPT_NAME_LEN - 1);
  setshort_badchars(wptname_sh, BAD_CHARS);
  setshort_mustupper(wptname_sh, 0);
  setshort_mustuniq(wptname_sh, 0);
  setshort_whitespace_ok(wptname_sh, 1);
  setshort_repeating_whitespace_ok(wptname_sh, 1);
  setshort_defname(wptname_sh, "WPT");

  rtename_sh = mkshort_new_handle();
  setshort_length(rtename_sh, RTE_NAME_LEN - 1);
  setshort_badchars(rtename_sh, BAD_CHARS);
  setshort_mustupper(rtename_sh, 0);
  setshort_mustuniq(rtename_sh, 0);
  setshort_whitespace_ok(rtename_sh, 1);
  setshort_repeating_whitespace_ok(rtename_sh, 1);
  setshort_defname(rtename_sh, "Route");

  trkname_sh = mkshort_new_handle();
  setshort_length(trkname_sh, RTE_NAME_LEN - 1);
  setshort_badchars(trkname_sh, BAD_CHARS);
  setshort_mustupper(trkname_sh, 0);
  setshort_mustuniq(trkname_sh, 0);
  setshort_whitespace_ok(trkname_sh, 1);
  setshort_repeating_whitespace_ok(trkname_sh, 1);
  setshort_defname(trkname_sh, "Track");

  waypoint_num = 0;
  rte_num = 0;
}

static void
humminbird_wr_deinit(void)
{
  mkshort_del_handle(&wptname_sh);
  mkshort_del_handle(&rtename_sh);
  mkshort_del_handle(&trkname_sh);
  gbfclose(fout);
}

static void
humminbird_write_waypoint(const waypoint* wpt)
{
  humminbird_waypt_t hum;
  double lat, north, east;
  int i;
  int num_icons = sizeof(humminbird_icons) / sizeof(humminbird_icons[0]);
  char* name;

  be_write16(&hum.num, waypoint_num++);
  hum.zero   = 0;
  hum.status = 1;
  hum.icon   = 255;

  // Icon....
  if (!wpt->icon_descr.isNull()) {
    for (i = 0; i < num_icons; i++) {
      if (!wpt->icon_descr.compare(humminbird_icons[i], Qt::CaseInsensitive)) {
        hum.icon = i;
        break;
      }
    }
    if (hum.icon == 255) {	/* no success, no try to find the item in a more comlex name */
      hum.icon = 0;	/* i.e. "Diamond" as part of "Diamond, Green" or "Green Diamond" */
      for (i = 0; i < num_icons; i++) {
        char* match;
        int j;
        xasprintf(&match, "*%s*", humminbird_icons[i]);
        j = wpt->icon_descr.compare(match, Qt::CaseInsensitive);
        xfree(match);
        if (j != 0) {
          hum.icon = i;
          break;
        }
      }
    }
  }

  hum.depth = si_round(WAYPT_GET(wpt, depth, 0)*100.0);
  be_write16(&hum.depth, hum.depth);

  be_write32(&hum.time, wpt->GetCreationTime().toTime_t());

  east = wpt->longitude / 180.0 * EAST_SCALE;
  be_write32(&hum.east, si_round((east)));

  lat = geodetic_to_geocentric_hwr(wpt->latitude);
  north = inverse_gudermannian_i1924(lat);
  be_write32(&hum.north, si_round(north));

  name = (global_opts.synthesize_shortnames)
         ? mkshort_from_wpt(wptname_sh, wpt)
         : mkshort(wptname_sh, wpt->shortname);
  memset(&hum.name, 0, sizeof(hum.name));
  memcpy(&hum.name, name, strlen(name));
  xfree(name);

  gbfputuint32(WPT_MAGIC, fout);
  gbfwrite(&hum, sizeof(hum), 1, fout);
}

static humminbird_trk_header_t* trk_head;
static humminbird_trk_point_t* trk_points;
static int32_t last_east;
static int32_t last_north;
static uint32_t last_time;


static void
humminbird_track_head(const route_head* trk)
{
  int max_points = (131080 - sizeof(uint32_t)- sizeof(humminbird_trk_header_t)) / sizeof(humminbird_trk_point_t);

  trk_head = NULL;
  last_time = 0;
  if (trk->rte_waypt_ct > 0) {
    char* name;
    trk_head = (humminbird_trk_header_t*) xcalloc(1, sizeof(humminbird_trk_header_t));
    trk_points = (humminbird_trk_point_t*) xcalloc(max_points, sizeof(humminbird_trk_point_t));

    name = mkshort(trkname_sh, trk->rte_name);
    strncpy(trk_head->name, name, sizeof(trk_head->name));
    xfree(name);
    be_write16(&trk_head->trk_num, trk->rte_num);
  }
}

static void
humminbird_track_tail(const route_head* rte)
{
  int max_points = (131080 - sizeof(uint32_t)- sizeof(humminbird_trk_header_t)) / sizeof(humminbird_trk_point_t);

  if (trk_head == NULL) {
    return;
  }

  be_write32(&trk_head->end_east, last_east);
  be_write32(&trk_head->end_north, last_north);
  be_write32(&trk_head->time, last_time);

  /* Fix some endianness */

  be_write32(&trk_head->sw_east, trk_head->sw_east);
  be_write32(&trk_head->ne_east, trk_head->ne_east);
  be_write32(&trk_head->sw_north, trk_head->sw_north);
  be_write32(&trk_head->ne_north, trk_head->ne_north);

  be_write16(&trk_head->num_points, trk_head->num_points);

  /* Actually write it out */


  gbfputuint32(TRK_MAGIC, fout);
  gbfwrite(trk_head, 1, sizeof(humminbird_trk_header_t), fout);
  gbfwrite(trk_points, max_points, sizeof(humminbird_trk_point_t), fout);
  gbfputuint16(0, fout); /* Odd but true. The format doesn't fit an int nr of entries. */

  xfree(trk_head);
  xfree(trk_points);

  trk_head   = NULL;
  trk_points = NULL;
}

static void
humminbird_track_cb(const waypoint* wpt)
{
  int32_t north, east;
  double lat;
  int i;

  if (trk_head == NULL) {
    return;
  }

  i = trk_head->num_points;

  east  = si_round(wpt->longitude / 180.0 * EAST_SCALE);
  lat = geodetic_to_geocentric_hwr(wpt->latitude);
  north = si_round(inverse_gudermannian_i1924(lat));

  if (wpt->creation_time.isValid()) {
    last_time = wpt->GetCreationTime().toTime_t();
  }

  if (i == 0) {
    /* It's the first point. That info goes in the header. */

    be_write32(&trk_head->start_east, east);
    be_write32(&trk_head->start_north, north);

    /* Bounding box. Easy for one point. */
    /* These are not BE yet, fixed in the end. */
    trk_head->sw_east = east;
    trk_head->ne_east = east;
    trk_head->sw_north = north;
    trk_head->ne_north = north;

    /* No depth info in the header. */
  } else {
    /* These points are 16-bit differential. */
    int j = i-1;
    trk_points[j].deltaeast = east - last_east;
    trk_points[j].deltanorth = north - last_north;
    trk_points[j].depth = si_round(WAYPT_GET(wpt, depth, 0)*100.0);

    /* BE-ify */
    be_write16(&trk_points[j].deltaeast, trk_points[j].deltaeast);
    be_write16(&trk_points[j].deltanorth, trk_points[j].deltanorth);
    be_write16(&trk_points[j].depth, trk_points[j].depth);

    /* Update bounding box in header if neccessary */
    if (east > trk_head->ne_east) {
      trk_head->ne_east = east;
    }
    if (east < trk_head->sw_east) {
      trk_head->sw_east = east;
    }
    if (north > trk_head->ne_north) {
      trk_head->ne_north = north;
    }
    if (north < trk_head->sw_north) {
      trk_head->sw_north = north;
    }
  }

  last_east = east;
  last_north = north;

  trk_head->num_points++;
}


static void
humminbird_track_write(void)
{

  track_disp_all(humminbird_track_head, humminbird_track_tail, humminbird_track_cb);
}

static void
humminbird_rte_head(const route_head* rte)
{
  humrte = NULL;
  if (rte->rte_waypt_ct > 0) {
    humrte = (humminbird_rte_t*) xcalloc(1, sizeof(*humrte));
  }
}

static void
humminbird_rte_tail(const route_head* rte)
{
  if (humrte == NULL) {
    return;
  }

  if (humrte->count > 0) {
    int i;
    char* name;

    humrte->num = rte_num++;
    humrte->time = gpsbabel_time;
    for (i = 0; i < humrte->count; i++) {
      be_write16(&humrte->points[i], humrte->points[i]);
    }

    be_write16(&humrte->num, humrte->num);
    be_write32(&humrte->time, humrte->time);

    name = mkshort(rtename_sh, rte->rte_name);
    strncpy(humrte->name, name, sizeof(humrte->name));
    xfree(name);

    gbfputuint32(RTE_MAGIC, fout);
    gbfwrite(humrte, sizeof(*humrte), 1, fout);
  }

  xfree(humrte);
  humrte = NULL;
}

static void
humminbird_write_rtept(const waypoint* wpt)
{
  int i;

  if (humrte == NULL) {
    return;
  }
  i = gb_ptr2int(wpt->extra_data);
  if (i <= 0) {
    return;
  }

  if (humrte->count < MAX_RTE_POINTS) {
    humrte->points[humrte->count] = i - 1;
    humrte->count++;
  } else {
    warning(MYNAME ": Sorry, routes are limited to %d points!\n", MAX_RTE_POINTS);
    fatal(MYNAME ": You can use our simplify filter to reduce the number of route points.\n");
  }
}

static void
humminbird_write_waypoint_wrapper(const waypoint* wpt)
{
  char* key;
  waypoint* tmpwpt;

  xasprintf(&key, "%s\01%.9f\01%.9f", wpt->shortname, wpt->latitude, wpt->longitude);
  if (!(tmpwpt = map[key])) {
    tmpwpt = (waypoint*)wpt;
    map[key] = (waypoint*) wpt;
    tmpwpt->extra_data = gb_int2ptr(waypoint_num + 1);	/* NOT NULL */
    humminbird_write_waypoint(wpt);
  } else {
    void* p = tmpwpt->extra_data;
    tmpwpt = (waypoint*)wpt;
    tmpwpt->extra_data = p;
  }

  xfree(key);
}

static void
humminbird_write(void)
{
  waypt_disp_all(humminbird_write_waypoint_wrapper);
  route_disp_all(NULL, NULL, humminbird_write_waypoint_wrapper);
  route_disp_all(humminbird_rte_head, humminbird_rte_tail, humminbird_write_rtept);
}

/**************************************************************************/

ff_vecs_t humminbird_vecs = {
  ff_type_file,
  {
    (ff_cap)(ff_cap_read | ff_cap_write) 	/* waypoints */,
    ff_cap_read 			/* tracks */,
    (ff_cap)(ff_cap_read | ff_cap_write)	/* routes */
  },
  humminbird_rd_init,
  humminbird_wr_init,
  humminbird_rd_deinit,
  humminbird_wr_deinit,
  humminbird_read,
  humminbird_write,
  NULL, // humminbird_exit,
  humminbird_args,
  CET_CHARSET_ASCII, 1			/* ascii is the expected character set */
  /* currently fixed !!! */
};

/**************************************************************************/

/**************************************************************************/

ff_vecs_t humminbird_ht_vecs = {
  ff_type_file,
  {
    ff_cap_read		 	/* waypoints */,
    (ff_cap)(ff_cap_read | ff_cap_write)	/* tracks */,
    ff_cap_read			/* routes */
  },
  humminbird_rd_init,
  humminbird_wr_init,
  humminbird_rd_deinit,
  humminbird_wr_deinit,
  humminbird_read,
  humminbird_track_write,
  NULL, // humminbird_exit,
  humminbird_args,
  CET_CHARSET_ASCII, 1			/* ascii is the expected character set */
  /* currently fixed !!! */
};

/**************************************************************************/
