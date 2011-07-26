/*
    Access to Garmin MapSource files.
    Based on information provided by Ian Cowley & Mark Bradley

    Copyright (C) 2002 Robert Lipe, robertlipe@usa.net

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

/* #define	MPS_DEBUG	0 */

#include <stdio.h>
#include <string.h>

#include "defs.h"
#include "garmin_tables.h"
#include "jeeps/gpsmath.h"
#include <ctype.h>

static	gbfile*	mps_file_in;
static	gbfile*	mps_file_out;
static	gbfile*	mps_file_temp;
static	short_handle mkshort_handle;

static	int		mps_ver_in = 0;
static	int		mps_ver_out = 0;
static	int		mps_ver_temp = 0;

/* Temporary pathname used when merging gpsbabel output with an existing file */
static char* tempname;
static char* fin_name;

static	const waypoint*	prevRouteWpt;
/* Private queues of written out waypoints */
static queue written_wpt_head;
static queue written_route_wpt_head;
static short_handle written_wpt_mkshort_handle;

/* Private queue of read in waypoints assumed to be used only for routes */
static queue read_route_wpt_head;
static short_handle read_route_wpt_mkshort_handle;

#define MPSDEFAULTWPTCLASS		0
#define MPSHIDDENROUTEWPTCLASS	8

#define MYNAME "MAPSOURCE"
#define ISME 0
#define NOTME 1

#define DEFAULTICONDESCR		"Waypoint"
#define DEFAULTICONVALUE		18

#define MPSNAMEBUFFERLEN	1024
#define MPSNOTESBUFFERLEN	4096
#define MPSDESCBUFFERLEN	4096


char* snlen = NULL;
char* snwhiteopt = NULL;
char* mpsverout = NULL;
char* mpsmergeouts = NULL;
int   mpsmergeout;
char* mpsusedepth = NULL;
char* mpsuseprox = NULL;

static
arglist_t mps_args[] = {
  {"snlen", &snlen, "Length of generated shortnames", "10", ARGTYPE_INT, "1", NULL },
  {
    "snwhite", &snwhiteopt, "Allow whitespace synth. shortnames",
    NULL, ARGTYPE_BOOL, ARG_NOMINMAX
  },
  {
    "mpsverout", &mpsverout,
    "Version of mapsource file to generate (3,4,5)", NULL,
    ARGTYPE_INT, ARG_NOMINMAX
  },
  {
    "mpsmergeout", &mpsmergeouts, "Merge output with existing file",
    NULL, ARGTYPE_BOOL, ARG_NOMINMAX
  },
  {
    "mpsusedepth", &mpsusedepth,
    "Use depth values on output (default is ignore)", NULL,
    ARGTYPE_BOOL, ARG_NOMINMAX
  },
  {
    "mpsuseprox", &mpsuseprox,
    "Use proximity values on output (default is ignore)",
    NULL, ARGTYPE_BOOL, ARG_NOMINMAX
  },
  ARG_TERMINATOR
};

static void
mps_noop(const route_head* wp)
{
  /* no-op */
}

void
mps_wpt_q_init(queue* whichQueue)
{
  QUEUE_INIT(whichQueue);
}

void
mps_wpt_q_deinit(queue* whichQueue)
{
  queue* elem, *tmp;

  QUEUE_FOR_EACH(whichQueue, elem, tmp) {
    waypoint* q = (waypoint*) dequeue(elem);
    waypt_free(q);
  }
}

/*
 * Find a waypoint that we've already written out
 *
 */
waypoint*
mps_find_wpt_q_by_name(const queue* whichQueue, const char* name)
{
  queue* elem, *tmp;
  waypoint* waypointp;

  QUEUE_FOR_EACH(whichQueue, elem, tmp) {
    waypointp = (waypoint*) elem;
    if (0 == strcmp(waypointp->shortname, name)) {
      return waypointp;
    }
  }
  return NULL;
}

/*
 * Add a waypoint that we've already written out to our list
 *
 */
void
mps_wpt_q_add(const queue* whichQueue, const waypoint* wpt)
{
  waypoint* written_wpt = waypt_dupe(wpt);
  ENQUEUE_TAIL(whichQueue, &written_wpt->Q);
}

static int
mps_converted_icon_number(const int icon_num, const int mpsver, garmin_formats_e garmin_format)
{
  int def_icon = DEFAULTICONVALUE;

  switch (garmin_format) {
  case MAPSOURCE:
    if (mpsver == 5) {
      return icon_num;
    }
    if (mpsver == 4) {
      /* Water hydrant */
      if (icon_num == 139) {
        return def_icon;
      } else {
        return icon_num;
      }
    } else {
      /* the Contact icons - V3 doesn't have anything like this */
      if ((icon_num >= 119) && (icon_num <= 138)) {
        return def_icon;
      }
      /* the Geocache icons - V3 use the Circle with X */
      if ((icon_num >= 117) && (icon_num <= 118)) {
        return 65;
      }
      /* Water hydrant */
      if (icon_num == 139) {
        return def_icon;
      }
      return icon_num;
    }

  case PCX:
  case GARMIN_SERIAL:
    if (mpsver == 5) {
      return icon_num;
    }
    if (mpsver == 4) {
      /* Water hydrant */
      if (icon_num == 8282) {
        return def_icon;
      } else {
        return icon_num;
      }
    }
    /* the Contact icons - V3 doesn't have anything like this */
    if ((icon_num >= 8257) && (icon_num <= 8276)) {
      return def_icon;
    }
    /* the Geocache icons - V3 use the Circle with X */
    if ((icon_num >= 8255) && (icon_num <= 8256)) {
      return 179;
    }
    /* Water hydrant */
    if (icon_num == 8282) {
      return def_icon;
    }
    return icon_num;

  default:
    fatal(MYNAME ": unknown garmin format.\n");
  }
  return def_icon;
}

static void
mps_rd_init(const char* fname)
{
  mps_file_in = gbfopen_le(fname, "rb", MYNAME);

  read_route_wpt_mkshort_handle = mkshort_new_handle();
  /* initialise the "private" queue of waypoints read for routes */
  mps_wpt_q_init(&read_route_wpt_head);
}

static void
mps_rd_deinit(void)
{
  gbfclose(mps_file_in);
  if (read_route_wpt_mkshort_handle) {
    mkshort_del_handle(&read_route_wpt_mkshort_handle);
  }
  /* flush the "private" queue of waypoints read for routes */
  mps_wpt_q_deinit(&read_route_wpt_head);
}

static void
mps_wr_init(const char* fname)
{
  fin_name = xstrdup(fname);
  if (mpsmergeouts) {
    mpsmergeout = atoi(mpsmergeouts);
  }

  if (mpsmergeout) {
    mps_file_out = gbfopen_le(fname, "rb", MYNAME);
    if (mps_file_out == NULL) {
      mpsmergeout = 0;
    } else {
      gbfclose(mps_file_out);
      srand((unsigned) current_time());

      for (;;) {
        /* create a temporary name  based on a random char and the existing name */
        /* then test if it already exists, if so try again with another rand num */
        /* yeah, yeah, so there's probably a library function for this           */
        xasprintf(&tempname, "%s.%08x", fname, rand());
        mps_file_temp = gbfopen_le(tempname, "rb", MYNAME);
        if (mps_file_temp == NULL) {
          break;
        }
        gbfclose(mps_file_temp);
      }
      rename(fname, tempname);
      mps_file_temp = gbfopen_le(tempname, "rb", MYNAME);
    }
  }

  mps_file_out = gbfopen_le(fname, "wb", MYNAME);

  written_wpt_mkshort_handle = mkshort_new_handle();
  /* initialise the "private" queue of waypoints written */
  mps_wpt_q_init(&written_wpt_head);
  mps_wpt_q_init(&written_route_wpt_head);
}

static void
mps_wr_deinit(void)
{
  gbfclose(mps_file_out);

  if (mpsmergeout) {
    gbfclose(mps_file_temp);
    remove(tempname);
    xfree(tempname);
  }

  if (written_wpt_mkshort_handle) {
    mkshort_del_handle(&written_wpt_mkshort_handle);
  }
  /* flush the "private" queue of waypoints written */
  mps_wpt_q_deinit(&written_wpt_head);
  mps_wpt_q_deinit(&written_route_wpt_head);
  xfree(fin_name);
}

/*
 * get characters until and including terminating NULL from mps_file_in
 * and write into buf.
 */
static void
mps_readstr(gbfile* mps_file, char* buf, size_t sz)
{
  int c;
  while (sz-- && (c = gbfgetc(mps_file)) != EOF) {
    *buf++ = c;
    if (c == 0)  {
      return;
    }
  }
}

/*
 * read in from file to check a) valid format b) version of data formating
 * MRCB
 */
static void
mps_fileHeader_r(gbfile* mps_file, int* mps_ver)
{
  char hdr[100];
  int reclen;

  mps_readstr(mps_file, hdr, sizeof(hdr));
  if (strcmp(hdr, "MsRcd")) {
    fatal(MYNAME ": This doesn't look like a mapsource file.\n");
  }
  /* Read record length of "format details" section */
  reclen = gbfgetint32(mps_file);
  /* Read the "format details" in plus the trailing null */
  gbfread(hdr, 3, 1, mps_file);
  if (hdr[0] != 'D')  {
    /* No flag for the "data" section */
    fatal(MYNAME ": This doesn't look like a mapsource file.\n");
  }
  if (hdr[1] == 'd')  {
    *mps_ver = 3;
  } else if ((hdr[1] > 'd') && (hdr[1] <= 'h')) {
    *mps_ver = 4;
  } else if ((hdr[1] > 'h') && (hdr[1] <= 'i')) {
    *mps_ver = 5;
  } else {
    fatal(MYNAME ": Unsuppported version of mapsource file.\n");
  }
  /* Skip reliably over the "format details" section */
  gbfseek(mps_file, reclen+1-3, SEEK_CUR);
  /* Read record length of "program signature" section */
  reclen = gbfgetint32(mps_file);
  /* Skip reliably over the "program signature" section */
  if (reclen >= 0) {
    gbfseek(mps_file, reclen+1, SEEK_CUR);
  }
}

/*
 * write out to file
 * MRCB
 */
static void
mps_fileHeader_w(gbfile* mps_file, int mps_ver)
{
  char hdr[100];
  int reclen;

  strcpy(hdr, "MsRc");
  gbfwrite(hdr, 4, 1, mps_file);

  /* Between versions 3 & 5 this value is 'd', but might change in the future */
  strcpy(hdr, "d");
  gbfwrite(hdr, 2, 1, mps_file);	/* include trailing NULL char */

  /* Start of a "Data" section */
  hdr[0] = 'D';
  /* if (mps_ver == 3) */
  hdr[1] = 'd';						/* equates to V3.02 */
  if (mps_ver == 4) {
    hdr[1] = 'g';  /* equates to V4.06 */
  }
  if (mps_ver == 5) {
    hdr[1] = 'i';  /* equates to V5.0 */
  }
  hdr[2] = 0;

  reclen = 2;							/* this is 3 byte record */
  gbfputint32(reclen, mps_file);
  gbfwrite(hdr, 3, 1, mps_file);		/* reclen + 1 */

  hdr[0] = 'A';
  /* if (mps_ver == 3) */
  hdr[1] = 0x2E;
  hdr[2] = 0x01;		/* equates to V3.02 */
  hdr[3] = 'S';
  hdr[4] = 'Q';
  hdr[5] = 'A';
  hdr[6] = 0;
  strcpy(hdr+7,"Oct 20 1999");
  strcpy(hdr+19,"12:50:33");
  if (mps_ver == 4) {
    hdr[1] = (char) 0x96;					/* equates to V4.06 */
    strcpy(hdr+7,"Oct 22 2001");
    strcpy(hdr+19,"15:45:33");
  }
  if (mps_ver == 5) {
    hdr[1] = (char) 0xF4;					/* equates to V5.0 */
    strcpy(hdr+7,"Jul  3 2003");
    strcpy(hdr+19,"08:35:33");
  }

  reclen = 27;						/* pre measured! */
  gbfputint32(reclen, mps_file);
  gbfwrite(hdr, 28, 1, mps_file);		/* reclen + 1  - can't use this as reclen may be wrongendian now */
}

/*
 * read in from file a map segment record
 * MRCB
 */
static void
mps_mapsegment_r(gbfile* mps_file, int mps_ver)
{
  int reclen;

#if 0
  /* At the moment we're not doing anything with map segments, but here's the template code as if we were */
  char hdr[100];
  gbfread(&CDid, 4, 1, mps_file);
  reclen = le_read32(&CDid);

  gbfread(&CDSegmentid, 4, 1, mps_file);
  reclen = le_read32(&CDSegmentid);

  mps_readstr(mps_file, CDName, sizeof(CDName));
  mps_readstr(mps_file, CDSegmentName, sizeof(CDSegmentName));
  mps_readstr(mps_file, CDAreaName, sizeof(CDAreaName));

  gbfread(hdr, 4, 1, mps_file); /* trailing long value */
#endif

  gbfseek(mps_file, -5, SEEK_CUR);
  reclen = gbfgetint32(mps_file);
  if (reclen >= 0) {
    gbfseek(mps_file, reclen+1, SEEK_CUR);
  }
  return;
}


/*
 * read in from file a mapsetname record
 * there should always be one of these at the end of the file
 * MRCB
 */
static void
mps_mapsetname_r(gbfile* mps_file, int mps_ver)
{
  int reclen;

  /* At the moment we're not doing anything with mapsetnames, but here's the template code as if we were
  char hdr[100];
  mps_readstr(mps_file, hdr, sizeof(hdr));
  char mapsetnamename[very large number?];
  strcpy(mapsetnamename,hdr);
  char mapsetnameAutonameFlag;
  gbfread(&mapsetnameAutonameFlag, 1, 1, mps_file); */

  gbfseek(mps_file, -5, SEEK_CUR);
  reclen = gbfgetint32(mps_file);
  gbfseek(mps_file, reclen+1, SEEK_CUR);
  return;
}


/*
 * write out to file a mapsetname record
 * there should always be one of these at the end of the file
 * MRCB
 */
static void
mps_mapsetname_w(gbfile* mps_file, int mps_ver)
{
  char hdr[100];
  int reclen;

  hdr[0] = 'V';	/* mapsetname start of record indicator			*/
  hdr[1] = 0;		/* zero length null terminated string			*/
  hdr[2] = 1;		/* mapsetname autoname flag set to DO autoname	*/
  reclen = 2;		/* three bytes of the V record					*/
  gbfputint32(reclen, mps_file);
  gbfwrite(hdr, 3, 1, mps_file);		/* reclen + 1 */
}


/*
 * read in from file a waypoint record
 * MRCB
 */
static void
mps_waypoint_r(gbfile* mps_file, int mps_ver, waypoint** wpt, unsigned int* mpsclass)
{
  char tbuf[100];
  char wptname[MPSNAMEBUFFERLEN];
  char* wptdesc = NULL;
  char* wptnotes = NULL;
  int lat;
  int lon;
  int	icon;
  int dynamic;

  waypoint*	thisWaypoint = NULL;
  double	mps_altitude = unknown_alt;
  double	mps_proximity = unknown_alt;
  double	mps_depth = unknown_alt;

  thisWaypoint = waypt_new();
  *wpt = thisWaypoint;

  mps_readstr(mps_file, wptname, sizeof(wptname));

  (*mpsclass) = gbfgetint32(mps_file);			/* class */
  mps_readstr(mps_file, tbuf, sizeof(tbuf));	/* country */

  gbfread(tbuf,17, 1, mps_file);				/* subclass data (17) */

  if ((mps_ver == 4) || (mps_ver == 5)) {
    gbfread(tbuf, 5, 1, mps_file);			/* additional subclass data (1) & terminator? (4) */
  }

  lat = gbfgetint32(mps_file);
  lon = gbfgetint32(mps_file);

  if (gbfgetc(mps_file) == 1) {				/* altitude validity */
    mps_altitude = gbfgetdbl(mps_file);
  } else {
    mps_altitude = unknown_alt;
    gbfseek(mps_file, 8, SEEK_CUR);
  }

  wptdesc = gbfgetcstr(mps_file);

  if (gbfgetc(mps_file) == 1) {				/* proximity validity */
    mps_proximity = gbfgetdbl(mps_file);
  } else {
    mps_proximity = unknown_alt;
    gbfseek(mps_file, 8, SEEK_CUR);
  }

  (void) gbfgetint32(mps_file);					/* display flag */
  (void) gbfgetint32(mps_file);					/* colour */
  icon = gbfgetint32(mps_file);					/* display symbol */

  mps_readstr(mps_file, tbuf, sizeof(tbuf));		/* city */
  mps_readstr(mps_file, tbuf, sizeof(tbuf));		/* state */
  mps_readstr(mps_file, tbuf, sizeof(tbuf));		/*facility */

  gbfread(tbuf, 1, 1, mps_file);					/* unknown */

  if (gbfgetc(mps_file) == 1) {					/* depth validity */
    mps_depth = gbfgetdbl(mps_file);
  } else {
    mps_depth = unknown_alt;
    (void) gbfseek(mps_file, 8, SEEK_CUR);
  }

  if ((mps_ver == 4) || (mps_ver == 5)) {
    gbfread(tbuf, 6, 1, mps_file);				/* unknown */
    wptnotes = gbfgetcstr(mps_file);
  } else {
    gbfread(tbuf, 2, 1, mps_file);				/* unknown */
  }

  thisWaypoint->shortname = xstrdup(wptname);
  thisWaypoint->description = wptdesc;
  thisWaypoint->notes = wptnotes;
  thisWaypoint->latitude = GPS_Math_Semi_To_Deg(lat);
  thisWaypoint->longitude = GPS_Math_Semi_To_Deg(lon);
  thisWaypoint->altitude = mps_altitude;
  if (mps_proximity != unknown_alt) {
    WAYPT_SET(thisWaypoint, proximity, mps_proximity);
  }
  if (mps_depth != unknown_alt) {
    WAYPT_SET(thisWaypoint, depth, mps_depth);
  }

  /* might need to change this to handle version dependent icon handling */
  thisWaypoint->icon_descr = gt_find_desc_from_icon_number(icon, MAPSOURCE, &dynamic);
  thisWaypoint->wpt_flags.icon_descr_is_dynamic = dynamic;

  /* The following Now done elsewhere since it can be useful to read in and
    perhaps not add to the list */
  /* waypt_add(thisWaypoint); */

  return;
}

/*
 * write out to file a waypoint record
 * MRCB
 */
static void
mps_waypoint_w(gbfile* mps_file, int mps_ver, const waypoint* wpt, const int isRouteWpt)
{
  int reclen;
  int lat, lon;
  int icon;
  char* src = "";         /* default to empty string */
  char* ident;
  char* ascii_description;
  char zbuf[25];
  char ffbuf[25];
  int display = 1;
  int colour = 0;			/*  (unknown colour) black is 1, white is 16 */

  double	mps_altitude = wpt->altitude;
  double	mps_proximity = (mpsuseprox ? WAYPT_GET(wpt, proximity, unknown_alt) : unknown_alt);
  double	mps_depth = unknown_alt;

  lat = GPS_Math_Deg_To_Semi(wpt->latitude);
  lon = GPS_Math_Deg_To_Semi(wpt->longitude);
  if (WAYPT_HAS(wpt, depth) && mpsusedepth) {
    mps_depth = wpt->depth;
  }

  if (wpt->description) {
    src = wpt->description;
  }
  if (wpt->notes) {
    src = wpt->notes;
  }
  ident = global_opts.synthesize_shortnames ?
          mkshort(mkshort_handle, src) :
          wpt->shortname;

  memset(zbuf, 0, sizeof(zbuf));
  memset(ffbuf, 0xff, sizeof(ffbuf));

  /* might need to change this to handle version dependent icon handling */
  icon = gt_find_icon_number_from_desc(wpt->icon_descr, MAPSOURCE);

  if (get_cache_icon(wpt) /* && wpt->icon_descr && (strcmp(wpt->icon_descr, "Geocache Found") != 0)*/) {
    icon = gt_find_icon_number_from_desc(get_cache_icon(wpt), MAPSOURCE);
  }

  icon = mps_converted_icon_number(icon, mps_ver, MAPSOURCE);

  /* two NULL (0x0) bytes at end of each string */
  ascii_description = wpt->description ? xstrdup(wpt->description) : xstrdup("");
  reclen = strlen(ident) + strlen(ascii_description) + 2;
  if ((mps_ver == 4) || (mps_ver == 5)) {
    /* v4.06 & V5.0*/
    reclen += 85;				/* "W" (1) + strlen(name) + NULL (1) + class(4) + country(sz) +
										subclass(18) + unknown(4) + lat(4) + lon(4) + alt(9) + strlen(desc)
										+ NULL (1) + prox(9) + display(4) + colour(4) + symbol(4) + city(sz) +
										state(sz) + facility(sz) + unknown2(1) + depth(9) + unknown3(7) */
    /* -1 as reclen is interpreted from zero meaning a reclength of one */
    if (wpt->notes) {
      reclen += strlen(wpt->notes);
    }
  } else {
    /* v3.02 */
    reclen += 75;				/* "W" (1) + strlen(name) + NULL (1) + + class(4) + country(sz) +
										subclass(17) + lat(4) +  lon(4) + alt(9) + strlen(desc) +
										NULL (1) + prox(9) + display(4) +
										colour(4) + symbol(4) + city(sz) + state(sz) + facility(sz) +
										unknown2(1) + depth(9) + unknown3(2) */
    /* -1 as reclen is interpreted from zero meaning a reclength of one */
  }

  gbfputint32(reclen, mps_file);
  gbfwrite("W", 1, 1, mps_file);
  gbfputs(ident, mps_file);
  gbfwrite(zbuf, 1, 1, mps_file);		/* NULL termination to ident */

  if (isRouteWpt)	{
    zbuf[0] = (char)MPSHIDDENROUTEWPTCLASS;
  } else {
    zbuf[0] = (char)MPSDEFAULTWPTCLASS;
  }
  gbfwrite(zbuf, 4, 1, mps_file);		/* class */

  zbuf[0]=0;
  gbfwrite(zbuf, 1, 1, mps_file);		/* country empty string */

  if ((mps_ver == 4) || (mps_ver == 5)) {
    gbfwrite(zbuf, 4, 1, mps_file);	/* subclass part 1 */
    gbfwrite(ffbuf, 12, 1, mps_file);	/* subclass part 2 */
    gbfwrite(zbuf, 2, 1, mps_file);	/* subclass part 3 */
    gbfwrite(ffbuf, 4, 1, mps_file);	/* unknown */
  } else {
    gbfwrite(zbuf, 8, 1, mps_file);
    gbfwrite(ffbuf, 8, 1, mps_file);
    gbfwrite(zbuf, 1, 1, mps_file);
  }

  gbfputint32(lat, mps_file);
  gbfputint32(lon, mps_file);

  if (mps_altitude == unknown_alt) {
    gbfwrite(zbuf, 9, 1, mps_file);
  } else {
    gbfputc(1, mps_file);
    gbfputdbl(mps_altitude, mps_file);
  }
  if (wpt->description) {
    gbfputs(ascii_description, mps_file);
  }
  gbfwrite(zbuf, 1, 1, mps_file);	/* NULL termination */
  xfree(ascii_description);
  ascii_description = NULL;

  if (mps_proximity == unknown_alt) {
    gbfwrite(zbuf, 9, 1, mps_file);
  } else {
    gbfputc(1, mps_file);
    gbfputdbl(mps_proximity, mps_file);
  }

  gbfputint32(display, mps_file);	/* Show waypoint w/ name */
  gbfputint32(colour, mps_file);
  gbfputint32(icon, mps_file);

  gbfwrite(zbuf, 3, 1, mps_file);		/* city, state, facility */

  gbfwrite(zbuf, 1, 1, mps_file);		/* unknown */

  if (mps_depth == unknown_alt) {
    gbfwrite(zbuf, 9, 1, mps_file);
  } else {
    gbfputc(1, mps_file);
    gbfputdbl(mps_depth, mps_file);
  }

  gbfwrite(zbuf, 2, 1, mps_file);		/* unknown */
  if ((mps_ver == 4) || (mps_ver == 5)) {
    gbfwrite(zbuf, 4, 1, mps_file);	/* unknown */
    if (wpt->notes) {
      gbfputs(wpt->notes, mps_file);
    }
    gbfwrite(zbuf, 1, 1, mps_file);	/* string termination */
  }
}

/*
 * wrapper to include the mps_ver_out information
 * A waypoint is only written if it hasn't been written before
 * based on it shortname alone
 *
 */
static void
mps_waypoint_w_unique_wrapper(const waypoint* wpt)
{
  waypoint* wptfound = NULL;

  /* Search for this waypoint in the ones already written */
  wptfound = mps_find_wpt_q_by_name(&written_wpt_head, wpt->shortname);
  /* is the next line necessary? Assumes we know who's called us and in what order */
  if (wptfound == NULL) {
    wptfound = mps_find_wpt_q_by_name(&written_route_wpt_head, wpt->shortname);
  }

  /* if this waypoint hasn't been written then it is okay to do so */
  if (wptfound == NULL) {
    mps_waypoint_w(mps_file_out, mps_ver_out, wpt, (1==0));

    /* ensure we record in our "private" queue what has been
    written so that we don't write it again */
    mps_wpt_q_add(&written_wpt_head, wpt);
  }
}

/*
 * wrapper to include the mps_ver_out information
 * A waypoint is only written if it hasn't been written before
 * based on it shortname alone
 * Provided as a separate function from above in case we find
 * have to do other things
 *
 */
static void
mps_route_wpt_w_unique_wrapper(const waypoint* wpt)
{
  waypoint* wptfound = NULL;

  /* Search for this waypoint in the ones already written */
  wptfound = mps_find_wpt_q_by_name(&written_wpt_head, wpt->shortname);
  if (wptfound == NULL)
    /* so, not a real wpt, so must check route wpts already written as reals */
  {
    wptfound = mps_find_wpt_q_by_name(&written_route_wpt_head, wpt->shortname);
  }

  /* if this waypoint hasn't been written then it is okay to do so
     but assume it is only required for the route
    */
  if (wptfound == NULL) {
    /* Although we haven't written one out, this might still be a "real" waypoint
       If so, we need to write it out now accordingly */
    wptfound = find_waypt_by_name(wpt->shortname);

    if (wptfound == NULL) {
      /* well, we tried to find: it wasn't written and isn't a real waypoint */
      mps_waypoint_w(mps_file_out, mps_ver_out, wpt, (1==1));
      mps_wpt_q_add(&written_route_wpt_head, wpt);
    } else {
      mps_waypoint_w(mps_file_out, mps_ver_out, wpt, (1==0));
      /* Simulated real user waypoint */
      mps_wpt_q_add(&written_wpt_head, wpt);
    }
  }
}
#if 0
/*
 * wrapper to include the mps_ver_out information
 * This one always writes a waypoint. If it has been written before
 * then generate a unique name before writing
 *
 */
static void
mps_waypoint_w_uniqloc_wrapper(waypoint* wpt)
{
  waypoint* wptfound = NULL;
  char*			newName;

  /* Search for this waypoint in the ones already written */
  wptfound = mps_find_wpt_q_by_name(&written_wpt_head, wpt->shortname);
  /* is the next line necessary? Assumes we know who's called us and in what order */
  if (wptfound == NULL) {
    wptfound = mps_find_wpt_q_by_name(&written_route_wpt_head, wpt->shortname);
  }

  if (wptfound != NULL) {
    /* check if this is the same waypoint by looking at the lat lon
    	not ideal, but better then having two same named waypoints
    	that kills MapSource.  If it is the same then don't bother
    	adding it in. If it isn't, then rename it
    */
    if (((wpt->latitude - wptfound->latitude) != 0) ||
        ((wpt->longitude - wptfound->longitude) != 0)) {
      /* Not the same lat lon, so rename and add */
      newName = mkshort(written_wpt_mkshort_handle, wpt->shortname);
      wptfound = waypt_dupe(wpt);
      xfree(wptfound->shortname);
      wptfound->shortname = newName;
      mps_waypoint_w(mps_file_out, mps_ver_out, wptfound, (1==0));
      mps_wpt_q_add(&written_wpt_head, wpt);
    }
  } else {
    mps_waypoint_w(mps_file_out, mps_ver_out, wpt, (1==0));
    /* ensure we record in out "private" queue what has been
    written so that we don't write it again */
    mps_wpt_q_add(&written_wpt_head, wpt);
  }
}
#endif

/*
 * read in from file a route record
 * MRCB
 */
static void
mps_route_r(gbfile* mps_file, int mps_ver, route_head** rte)
{
  char tbuf[100];
  char* rtename;
  char wptname[MPSNAMEBUFFERLEN];
  int lat = 0;
  int lon = 0;
  char rte_autoname;
  int	interlinkStepCount;
  int	thisInterlinkStep;
  unsigned int	mpsclass;

  route_head* rte_head;
  int rte_count;

  waypoint*	thisWaypoint;
  waypoint*	tempWpt;

  double	mps_altitude = unknown_alt;
  double	mps_depth = unknown_alt;

  rtename = gbfgetcstr(mps_file);
#ifdef	MPS_DEBUG
  fprintf(stderr, "mps_route_r: reading route %s\n", rtename);
#endif

  gbfread(&rte_autoname, 1, 1, mps_file);	/* autoname flag */

  gbfread(tbuf, 1, 1, mps_file);		/* skip min/max values */
  if (tbuf[0] == 0) {

    lat = gbfgetint32(mps_file);			/* max lat of whole route */
    lon = gbfgetint32(mps_file);			/* max lon of whole route */

    if (gbfgetc(mps_file) == 1) {			/* altitude validity */
      mps_altitude = gbfgetdbl(mps_file);
    } else {
      mps_altitude = unknown_alt;
      gbfseek(mps_file, 8, SEEK_CUR);
    }

    lat = gbfgetint32(mps_file);			/* min lat of whole route */
    lon = gbfgetint32(mps_file);			/* min lon of whole route */

    if (gbfgetc(mps_file) == 1) {			/* altitude validity */
      mps_altitude = gbfgetdbl(mps_file);
    } else {
      mps_altitude = unknown_alt;
      gbfseek(mps_file, 8, SEEK_CUR);
    }
  }

  rte_count = gbfgetint32(mps_file);			/* number of waypoints in route */

  /* This might be rather presumptuous, but is it valid in any format to have route with no points? */
  /* Let's assume not, so if the route count is zero or less, let's get out of here and allow the   */
  /* caller to do any file resync                                                                   */
  if (rte_count < 0) {
    return;
  }

#ifdef	MPS_DEBUG
  fprintf(stderr, "mps_route_r: route contains %d waypoints\n", rte_count);
#endif

  rte_head = route_head_alloc();
  rte_head->rte_name = rtename;
  route_add_head(rte_head);
  *rte = rte_head;

  rte_count--;			/* need to loop round for one less than the number of waypoints */

  while (rte_count--) {

    mps_readstr(mps_file, wptname, sizeof(wptname));
#ifdef	MPS_DEBUG
    fprintf(stderr, "mps_route_r: reading route waypoint %s\n", wptname);
#endif

    mpsclass = gbfgetint32(mps_file);			/* class */
    mps_readstr(mps_file, tbuf, sizeof(tbuf));	/* country */

    if ((mps_ver == 4) || (mps_ver == 5)) {
      gbfread(tbuf, 22, 1, mps_file);				/* subclass data */

      /* This is a bit unpleasant. Routes have a variable length of
         data (min 22 bytes) terminated by a zero */
      do {
        gbfread(tbuf, 1, 1, mps_file);
      } while (tbuf[0]);

      /* The next thing is the unknown 0x03 0x00 .. 0x00 (18 bytes) */
      gbfread(tbuf, 18, 1, mps_file);
    } else {
      gbfread(tbuf, 17, 1, mps_file);				/* subclass data */
      gbfread(tbuf, 18, 1, mps_file);				/* unknown 0x00 0x03 0x00 .. 0x00 */
    }

    /* link details */
    interlinkStepCount = gbfgetint32(mps_file);					/* NOT always 2, but will assume > 0 */

#ifdef	MPS_DEBUG
    fprintf(stderr, "mps_route_r: interlink steps are %d\n", interlinkStepCount);
#endif

    /* Basically we're knackered if the step count is less than one since we hard code reading of the */
    /* first, so if there isn't one, we'd lose sync on the file and read junk                         */
    /* Given we've already done some route head allocation, do we return or do we die? It'd be good   */
    /* do some clean up before returning.                                                             */
    if (interlinkStepCount < 1) {
      /* For RJL - are the following lines correct ? */
      /* route_free(rte_head);
      route_del_head(rte_head); */
      return;
    }

    /* first end of link */
    lat = gbfgetint32(mps_file);
    lon = gbfgetint32(mps_file);

    if (gbfgetc(mps_file) == 1) {			/* altitude validity */
      mps_altitude = gbfgetdbl(mps_file);
    } else {
      mps_altitude = unknown_alt;
      gbfseek(mps_file, 8, SEEK_CUR);
    }

    /* with MapSource routes, the real waypoint details are held as a separate waypoint, so copy from there
       if found. With MapSource, one should consider the real waypoint list as definitive */
    tempWpt = find_waypt_by_name(wptname);

    if (tempWpt != NULL) {
      thisWaypoint = waypt_dupe(tempWpt);
    } else {
      tempWpt = mps_find_wpt_q_by_name(&read_route_wpt_head, wptname);

      if (tempWpt != NULL) {
        thisWaypoint = waypt_dupe(tempWpt);
      } else {
        /* should never reach here, but we do need a fallback position */
#ifdef	MPS_DEBUG
        fprintf(stderr, "mps_route_r: reached the point we never should\n");
#endif
        thisWaypoint = waypt_new();
        thisWaypoint->shortname = xstrdup(wptname);
        thisWaypoint->latitude = GPS_Math_Semi_To_Deg(lat);
        thisWaypoint->longitude = GPS_Math_Semi_To_Deg(lon);
        thisWaypoint->altitude = mps_altitude;
        if (mps_depth != unknown_alt) {
          WAYPT_SET(thisWaypoint, depth, mps_depth);
        }
      }
    }

    route_add_wpt(rte_head, thisWaypoint);

    /* take two off the count since we separately read the start and end parts of the link */
    /* MRCB 2004/09/15 - NOPE, sorry, this needs to one, since interlink steps can be > 0 */
    for (thisInterlinkStep = interlinkStepCount - 1; thisInterlinkStep > 0; thisInterlinkStep--) {
      /* Could do this by doing a calculation on length of each co-ordinate and just doing one read
         but doing it this way makes it easier in the future to make use of this data */
      lat = gbfgetint32(mps_file);
      lon = gbfgetint32(mps_file);

      if (gbfgetc(mps_file) == 1) {			/* altitude validity */
        mps_altitude = gbfgetdbl(mps_file);
      } else {
        mps_altitude = unknown_alt;
        gbfseek(mps_file, 8, SEEK_CUR);
      }
    }

    gbfread(tbuf, 1, 1, mps_file);			/* NULL */

    gbfread(tbuf, 4, 1, mps_file);			/* link max lat */
    gbfread(tbuf, 4, 1, mps_file);			/* link max lon */
    gbfread(tbuf, 9, 1, mps_file);			/* link max alt validity + alt */

    gbfread(tbuf, 4, 1, mps_file);			/* link min lat */
    gbfread(tbuf, 4, 1, mps_file);			/* link min lon */
    gbfread(tbuf, 9, 1, mps_file);			/* link min alt validity + alt */

  }		/* while (trk_count--) */

  /* when the loop is done, there's still one waypoint to read with a small trailer */
  /* all we want is the waypoint name; lat, lon and alt are already set from above  */
  mps_readstr(mps_file, wptname, sizeof(wptname));
#ifdef	MPS_DEBUG
  fprintf(stderr, "mps_route_r: reading final route waypoint %s\n", wptname);
#endif


  mpsclass = gbfgetint32(mps_file);			/* class */
  mps_readstr(mps_file, tbuf, sizeof(tbuf));	/* country */

  if ((mps_ver == 4) || (mps_ver == 5)) {
    gbfread(tbuf, 22, 1, mps_file);				/* subclass data */

    /* This is a bit unpleasant. Routes have a variable length of
    	data (min 22 bytes) terminated by a zero */
    do {
      gbfread(tbuf, 1, 1, mps_file);
    } while (tbuf[0]);

    /* The next thing is the unknown 0x03 0x00 .. 0x00 (18 bytes) */
    gbfread(tbuf, 18, 1, mps_file);
  } else {
    gbfread(tbuf, 17, 1, mps_file);				/* subclass data */
    gbfread(tbuf, 18, 1, mps_file);				/* unknown 0x00 0x03 0x00 .. 0x00 */
  }

  gbfread(tbuf, 5, 1, mps_file);					/* 5 byte trailer */
  /* with MapSource routes, the real waypoint details are held as a separate waypoint, so copy from there
  	if found because there is more info held in a real waypoint than in its route counterpart,
  	e.g. the display symbol (aka icon)
  */
  tempWpt = find_waypt_by_name(wptname);

  if (tempWpt != NULL) {
    thisWaypoint = waypt_dupe(tempWpt);
  } else {
    tempWpt = mps_find_wpt_q_by_name(&read_route_wpt_head, wptname);

    if (tempWpt != NULL) {
      thisWaypoint = waypt_dupe(tempWpt);
    } else {
      /* should never reach here, but we do need a fallback position */
      thisWaypoint = waypt_new();
      thisWaypoint->shortname = xstrdup(wptname);
      thisWaypoint->latitude = GPS_Math_Semi_To_Deg(lat);
      thisWaypoint->longitude = GPS_Math_Semi_To_Deg(lon);
      thisWaypoint->altitude = mps_altitude;
    }
  }

  route_add_wpt(rte_head, thisWaypoint);

  return;
}

/*
 * write out to file a route header
 * MRCB
 */
static void
mps_routehdr_w(gbfile* mps_file, int mps_ver, const route_head* rte)
{
  unsigned int reclen;
  unsigned int rte_datapoints;
  int			rname_len;
  char*		rname;
  char		hdr[20];
  char		zbuf[20];
  char*		src = "";
  char*		ident;

  waypoint*	testwpt;
  time_t		uniqueValue = 0;
  int			allWptNameLengths;

  double		maxlat=-90.0;
  double		maxlon=-180.0;
  double		minlat=90.0;
  double		minlon=180.0;
  double		maxalt=unknown_alt;
  double		minalt=-unknown_alt;

  int lat;
  int lon;

  queue* elem, *tmp;

  prevRouteWpt = NULL;		/* clear the stateful flag used to know when the start of route wpts happens */

  memset(zbuf, 0, sizeof(zbuf));

  /* total nodes (waypoints) this route */
  rte_datapoints = 0;
  allWptNameLengths = 0;

  if (rte->waypoint_list.next) {		/* this test doesn't do what I want i.e test if this is a valid route - treat as a placeholder for now */
    QUEUE_FOR_EACH(&rte->waypoint_list, elem, tmp) {
      testwpt = (waypoint*)elem;
      if (rte_datapoints == 0) {
        uniqueValue = testwpt->creation_time;
      }
      if (testwpt->latitude > maxlat) {
        maxlat = testwpt->latitude;
      }
      if (testwpt->latitude < minlat) {
        minlat = testwpt->latitude;
      }
      if (testwpt->longitude > maxlon) {
        maxlon = testwpt->longitude;
      }
      if (testwpt->longitude < minlon) {
        minlon = testwpt->longitude;
      }
      if (testwpt->altitude != unknown_alt) {
        if ((testwpt->altitude > maxalt) ||
            (maxalt == unknown_alt)) {
          maxalt = testwpt->altitude;
        }
        if ((testwpt->altitude < minalt) ||
            (minalt == -unknown_alt)) {
          minalt = testwpt->altitude;
        }
      }

      if (testwpt->description) {
        src = testwpt->description;
      }
      if (testwpt->notes) {
        src = testwpt->notes;
      }
      ident = global_opts.synthesize_shortnames ?
              mkshort(mkshort_handle, src) :
              testwpt->shortname;
      allWptNameLengths += strlen(ident) + 1;

      rte_datapoints++;
    }

    if (uniqueValue == 0) {
      uniqueValue = current_time();
    }

    /* route name */
    if (!rte->rte_name) {
      sprintf(hdr, "Route%04x", (unsigned) uniqueValue);
      rname = xstrdup(hdr);
    } else {
      rname = xstrdup(rte->rte_name);
    }

    rname_len = strlen(rname);
    reclen = rname_len + 42;		/* "T" (1) + strlen(tname) + NULL (1) + autoname flag (2) +
										route lat lon max (2x4) + route max alt (9) +
										route lat lon min (2x4) + route min alt (9) +
										num route datapoints value (4) */

    /* V3 - each waypoint: waypoint name + NULL (1) + class (4) + country + NULL (1) +
    						subclass (17) + unknown (18) */
    /* V4,5 - each waypoint: waypoint name + NULL (1) + class (4) + country + NULL (1) +
    						subclass (18) + unknown (4) + unknown (19) */
    /* V* - each route link: 0x00000002 (4) + end 1 lat (4) + end 1 lon (4) + end 1 alt (9) +
    						end 2 lat (4) + end 2 lon (4) + end 2 alt (9) + NULL (1) +
    						link max lat (4) + link max lon (4) + link max alt (9) +
    						link min lat (4) + link min lon (4) + link min alt (9) */

    if ((mps_ver == 4) || (mps_ver == 5)) {
      reclen += allWptNameLengths + rte_datapoints * 46 +
                (rte_datapoints - 1) * 73 + 4;				/* link details plus overall trailing bytes */
    } else {
      reclen += allWptNameLengths + rte_datapoints * 40 +
                (rte_datapoints - 1) * 73 + 4;				/* link details plus overall trailing bytes */
    }

    gbfputint32(reclen, mps_file);
    gbfputc('R', mps_file);
    gbfwrite(rname, 1, rname_len, mps_file);

    xfree(rname);

    hdr[0] = 0;						/* NULL of string termination */
    hdr[1] = 0;						/* don't autoname */
    hdr[2] = 0;						/* MSB of don't autoname */
    gbfwrite(hdr, 3, 1, mps_file);	/* NULL string terminator + route autoname flag */

    lat = GPS_Math_Deg_To_Semi(maxlat);
    lon = GPS_Math_Deg_To_Semi(maxlon);

    gbfputint32(lat, mps_file);
    gbfputint32(lon, mps_file);

    if (maxalt == unknown_alt) {
      gbfwrite(zbuf, 9, 1, mps_file);
    } else {
      gbfputc(1, mps_file);
      gbfputdbl(maxalt, mps_file);
    }

    lat = GPS_Math_Deg_To_Semi(minlat);
    lon = GPS_Math_Deg_To_Semi(minlon);

    gbfputint32(lat, mps_file);
    gbfputint32(lon, mps_file);

    if (minalt == -unknown_alt) {
      gbfwrite(zbuf, 9, 1, mps_file);
    } else {
      gbfputc(1, mps_file);
      gbfputdbl(minalt, mps_file);
    }

    gbfputint32(rte_datapoints, mps_file);
  }
}

static void
mps_routehdr_w_wrapper(const route_head* rte)
{
  mps_routehdr_w(mps_file_out, mps_ver_out, rte);
}


/*
 * write out to file a route datapoint
 * MRCB
 */
static void
mps_routedatapoint_w(gbfile* mps_file, int mps_ver, const waypoint* rtewpt)
{
  int			lat;
  int			lon;
  char		zbuf[20];
  char		ffbuf[20];
  char*		src = "";
  char*		ident;
  int			reclen;

  int			maxlat;
  int			maxlon;
  int			minlat;
  int			minlon;
  double		maxalt=unknown_alt;
  double		minalt=-unknown_alt;

  double		mps_altitude;
  waypoint*	wptfound;

  memset(zbuf, 0, sizeof(zbuf));
  memset(ffbuf, 0xff, sizeof(ffbuf));

  if (prevRouteWpt != NULL) {
    /* output the route link details */
    reclen = 2;
    gbfputint32(reclen, mps_file);

    /* output end point 1 */
    lat = GPS_Math_Deg_To_Semi(prevRouteWpt->latitude);
    lon = GPS_Math_Deg_To_Semi(prevRouteWpt->longitude);

    gbfputint32(lat, mps_file);
    gbfputint32(lon, mps_file);

    mps_altitude = prevRouteWpt->altitude;
    if (mps_altitude == unknown_alt) {
      gbfwrite(zbuf, 9, 1, mps_file);
    } else {
      gbfputc(1, mps_file);
      gbfputdbl(mps_altitude, mps_file);
    }

    /* output end point 2 */
    lat = GPS_Math_Deg_To_Semi(rtewpt->latitude);
    lon = GPS_Math_Deg_To_Semi(rtewpt->longitude);

    gbfputint32(lat, mps_file);
    gbfputint32(lon, mps_file);

    mps_altitude = rtewpt->altitude;
    if (mps_altitude == unknown_alt) {
      gbfwrite(zbuf, 9, 1, mps_file);
    } else {
      gbfputc(1, mps_file);
      gbfputdbl(mps_altitude, mps_file);
    }

    if (rtewpt->latitude > prevRouteWpt->latitude) {
      maxlat = GPS_Math_Deg_To_Semi(rtewpt->latitude);
      minlat = GPS_Math_Deg_To_Semi(prevRouteWpt->latitude);
    } else {
      minlat = GPS_Math_Deg_To_Semi(rtewpt->latitude);
      maxlat = GPS_Math_Deg_To_Semi(prevRouteWpt->latitude);
    }

    if (rtewpt->longitude > prevRouteWpt->longitude) {
      maxlon = GPS_Math_Deg_To_Semi(rtewpt->longitude);
      minlon = GPS_Math_Deg_To_Semi(prevRouteWpt->longitude);
    } else {
      minlon = GPS_Math_Deg_To_Semi(rtewpt->longitude);
      maxlon = GPS_Math_Deg_To_Semi(prevRouteWpt->longitude);
    }

    if (rtewpt->altitude != unknown_alt) {
      maxalt = rtewpt->altitude;
    }
    if (rtewpt->altitude != unknown_alt) {
      minalt = rtewpt->altitude;
    }
    if (prevRouteWpt->altitude != unknown_alt) {
      if ((prevRouteWpt->altitude > maxalt) ||
          (maxalt == unknown_alt)) {
        maxalt = prevRouteWpt->altitude;
      }
      if ((prevRouteWpt->altitude < minalt) ||
          (minalt == -unknown_alt)) {
        minalt = prevRouteWpt->altitude;
      }
    }

    gbfwrite(zbuf, 1, 1, mps_file);

    /* output max coords of the link */
    gbfputint32(maxlat, mps_file);
    gbfputint32(maxlon, mps_file);

    if (maxalt == unknown_alt) {
      gbfwrite(zbuf, 9, 1, mps_file);
    } else {
      gbfputc(1, mps_file);
      gbfputdbl(maxalt, mps_file);
    }

    /* output min coords of the link */
    gbfputint32(minlat, mps_file);
    gbfputint32(minlon, mps_file);

    if (minalt == -unknown_alt) {
      gbfwrite(zbuf, 9, 1, mps_file);
    } else {
      gbfputc(1, mps_file);
      gbfputdbl(minalt, mps_file);
    }

  }

  if (rtewpt->description) {
    src = rtewpt->description;
  }
  if (rtewpt->notes) {
    src = rtewpt->notes;
  }
  ident = global_opts.synthesize_shortnames ?
          mkshort(mkshort_handle, src) :
          rtewpt->shortname;

  gbfputs(ident, mps_file);
  gbfwrite(zbuf, 1, 1, mps_file);	/* NULL termination to ident */

  wptfound = mps_find_wpt_q_by_name(&written_route_wpt_head, ident);
  if (wptfound != NULL)	{
    zbuf[0] = (char)MPSHIDDENROUTEWPTCLASS;
  } else {
    zbuf[0] = (char)MPSDEFAULTWPTCLASS;
  }
  gbfwrite(zbuf, 4, 1, mps_file);			/* class */

  zbuf[0]=0;
  gbfwrite(zbuf, 1, 1, mps_file);			/* country - i.e. empty string */

  if ((mps_ver == 4) || (mps_ver == 5)) {
    gbfwrite(zbuf, 4, 1, mps_file);		/* subclass part 1 */
    gbfwrite(ffbuf, 12, 1, mps_file);		/* subclass part 2 */
    gbfwrite(zbuf, 2, 1, mps_file);		/* subclass part 3 */
    gbfwrite(ffbuf, 4, 1, mps_file);		/* unknown */

    gbfwrite(zbuf, 1, 1, mps_file);
    gbfputc(3, mps_file);
    gbfwrite(zbuf, 17, 1, mps_file);
  } else {
    gbfwrite(zbuf, 8, 1, mps_file);		/* subclass part 1 */
    gbfwrite(ffbuf, 8, 1, mps_file);		/* subclass part 2 */
    gbfwrite(zbuf, 1, 1, mps_file);		/* subclass part 3 */

    /* unknown */
    gbfwrite(zbuf, 1, 1, mps_file);
    gbfputc(3, mps_file);
    gbfwrite(zbuf, 16, 1, mps_file);
  }

  prevRouteWpt = rtewpt;
}

static void
mps_routedatapoint_w_wrapper(const waypoint* rte)
{
  mps_routedatapoint_w(mps_file_out, mps_ver_out, rte);
}


/*
 * write out to file a route trailer
 * MRCB
 */
static void
mps_routetrlr_w(gbfile* mps_file, int mps_ver, const route_head* rte)
{
  char		hdr[2];
  int			value = 0;

  hdr[0] = 1;

  if (rte->waypoint_list.next) {		/* this test doesn't do what I want i.e test if this is a valid route - treat as a placeholder for now */
    gbfwrite(&value, 4, 1, mps_file);
    gbfwrite(hdr, 1, 1, mps_file);
  }
}

static void
mps_routetrlr_w_wrapper(const route_head* rte)
{
  mps_routetrlr_w(mps_file_out, mps_ver_out, rte);
}


/*
 * read in from file a track record
 * MRCB
 */
static void
mps_track_r(gbfile* mps_file, int mps_ver, route_head** trk)
{
  char* trkname;
  int lat;
  int lon;

  int	dateTime = 0;
  route_head* track_head;
  int trk_count;

  waypoint*	thisWaypoint;
  double	mps_altitude = unknown_alt;
  double	mps_depth = unknown_alt;

  trkname = gbfgetcstr(mps_file);
#ifdef	MPS_DEBUG
  fprintf(stderr, "mps_track_r: reading track %s\n", trkname);
#endif

  (void) gbfgetc(mps_file);				/* display flag */
  (void) gbfgetint32(mps_file);				/* colour */

  trk_count = gbfgetint32(mps_file);			/* number of datapoints in tracklog */

  /* I don't know, but perhaps it's valid to have a track with no waypoints   */
  /* Seems dumb, but truth is stranger than fiction. Of course, it could be   */
  /* that there are more than MAXINT / 2 waypoints, yeah sure                 */
  /* Allow the caller the perform the file resync caused by bombing out early */
  if (trk_count < 0) {
    return;
  }
#ifdef	MPS_DEBUG
  fprintf(stderr, "mps_track_r: there are %d track waypoints %d\n", trk_count);
#endif

  track_head = route_head_alloc();
  track_head->rte_name = trkname;
  track_add_head(track_head);
  *trk = track_head;

  while (trk_count--) {

    lat = gbfgetint32(mps_file);
    lon = gbfgetint32(mps_file);

    if (gbfgetc(mps_file) == 1) {			/* altitude validity */
      mps_altitude = gbfgetdbl(mps_file);
    } else {
      mps_altitude = unknown_alt;
      gbfseek(mps_file, 8, SEEK_CUR);
    }

    if (gbfgetc(mps_file) == 1) {			/* date/time validity */
      dateTime = gbfgetint32(mps_file);
    } else {
      (void) gbfgetint32(mps_file);
    }

    if (gbfgetc(mps_file) == 1) {			/* depth validity */
      mps_depth = gbfgetdbl(mps_file);
    } else {
      mps_depth = unknown_alt;
      gbfseek(mps_file, 8, SEEK_CUR);
    }

    thisWaypoint = waypt_new();
    thisWaypoint->latitude = GPS_Math_Semi_To_Deg(lat);
    thisWaypoint->longitude = GPS_Math_Semi_To_Deg(lon);
    thisWaypoint->creation_time = dateTime;
    thisWaypoint->microseconds = 0;
    thisWaypoint->altitude = mps_altitude;
    if (mps_depth != unknown_alt) {
      WAYPT_SET(thisWaypoint, depth, mps_depth);
    }
    track_add_wpt(track_head, thisWaypoint);

  }		/* while (trk_count--) */

  return;

}

/*
 * write out to file a tracklog header
 * MRCB
 */
static void
mps_trackhdr_w(gbfile* mps_file, int mps_ver, const route_head* trk)
{
  unsigned int reclen;
  unsigned int trk_datapoints;
  unsigned int colour = 0;		/* unknown colour */
  int			tname_len;
  char*		tname;
  char		hdr[20];
  waypoint*	testwpt;
  time_t		uniqueValue = 0;

  queue* elem, *tmp;

  /* total nodes (waypoints) this track */
  trk_datapoints = 0;
  if (trk->waypoint_list.next) {	/* this test doesn't do what I want i.e test if this is a valid track - treat as a placeholder for now */
    QUEUE_FOR_EACH(&trk->waypoint_list, elem, tmp) {
      if (trk_datapoints == 0) {
        testwpt = (waypoint*)elem;
        uniqueValue = testwpt->creation_time;
      }
      trk_datapoints++;
    }

    if (uniqueValue == 0) {
      uniqueValue = current_time();
    }

    /* track name */
    if (!trk->rte_name) {
      sprintf(hdr, "Track%04x", (unsigned) uniqueValue);
      tname = xstrdup(hdr);
    } else {
      tname = xstrdup(trk->rte_name);
    }

    tname_len = strlen(tname);
    reclen = tname_len + 11;		/* "T" (1) + strlen(tname) + NULL (1) + display flag (1) + colour (4) +
										num track datapoints value (4) */

    reclen += (trk_datapoints * 31) - 1;	/* lat (4) + lon (4) + alt (9) + date (5) + depth (9) ;*/
    /* -1 is because reclen starts from 0 which means a length of 1 */
    gbfputint32(reclen, mps_file);
    gbfputc('T', mps_file);
    gbfwrite(tname, 1, tname_len, mps_file);

    xfree(tname);

    hdr[0] = 0;
    hdr[1] = 1;
    gbfwrite(hdr, 2, 1, mps_file);	/* NULL string terminator + display flag */

    gbfputint32(colour, mps_file);

    gbfputint32(trk_datapoints, mps_file);
  }

}

static void
mps_trackhdr_w_wrapper(const route_head* trk)
{
  mps_trackhdr_w(mps_file_out, mps_ver_out, trk);
}


/*
 * write out to file a tracklog datapoint
 * MRCB
 */
static void
mps_trackdatapoint_w(gbfile* mps_file, int mps_ver, const waypoint* wpt)
{
  int lat, lon;
  time_t	t = wpt->creation_time;
  char zbuf[10];

  double	mps_altitude = wpt->altitude;
  double	mps_depth = unknown_alt;

  lat = GPS_Math_Deg_To_Semi(wpt->latitude);
  lon = GPS_Math_Deg_To_Semi(wpt->longitude);
  if (WAYPT_HAS(wpt, depth) && mpsusedepth) {
    mps_depth = wpt->depth;
  }

  memset(zbuf, 0, sizeof(zbuf));

  gbfputint32(lat, mps_file);
  gbfputint32(lon, mps_file);

  if (mps_altitude == unknown_alt) {
    gbfwrite(zbuf, 9, 1, mps_file);
  } else {
    gbfputc(1, mps_file);
    gbfputdbl(mps_altitude, mps_file);
  }

  if (t > 0) {					/* a valid time is assumed to > 0 */
    gbfputc(1, mps_file);
    gbfputint32(t, mps_file);
  } else {
    gbfwrite(zbuf, 5, 1, mps_file);
  }

  if (mps_depth == unknown_alt) {
    gbfwrite(zbuf, 9, 1, mps_file);
  } else {
    gbfputc(1, mps_file);
    gbfputdbl(mps_depth, mps_file);
  }
}

static void
mps_trackdatapoint_w_wrapper(const waypoint* wpt)
{
  mps_trackdatapoint_w(mps_file_out, mps_ver_out, wpt);
}


static void
mps_read(void)
{
  waypoint*		wpt;
  route_head*		rte;
  route_head*		trk;

  char			recType;
  int				reclen;
  int				morework;
  unsigned int	mpsWptClass;
  long			mpsFileInPos;

  mps_ver_in = 0;		/* although initialised at declaration, what happens if there are two mapsource
						   input files? */
  mps_fileHeader_r(mps_file_in, &mps_ver_in);

#ifdef DUMP_ICON_TABLE
  printf("static icon_mapping_t garmin_icon_table[] = {\n");
#endif

  morework = 1;
  while (morework && !gbfeof(mps_file_in)) {

    /* Read record length of next section */
    reclen = gbfgetint32(mps_file_in);

    if (reclen < 0) {
      fatal(MYNAME ": a record length read from the input file is invalid. \nEither the file is corrupt or unsupported.\n");
    }

    /* Read the record type "flag" in - using gbfread in case in the future need more than one char */
    gbfread(&recType, 1, 1, mps_file_in);
    mpsFileInPos = gbftell(mps_file_in);
    switch (recType) {
    case 'W':
      /* Waypoint record */
      /* With routes, we need the waypoint info that reveals, for example, the symbol type */
      mps_waypoint_r(mps_file_in, mps_ver_in, &wpt, &mpsWptClass);

#ifdef	MPS_DEBUG
      fprintf(stderr,"Read a waypoint - %s\n", wpt->shortname);
#endif

      if (gbftell(mps_file_in) != mpsFileInPos + reclen) {
        /* should junk this record and not save it since we're out of sync with the file */
        /* Should and how do we warn the user?                                           */
#ifdef	MPS_DEBUG
        fprintf(stderr,"Lost sync with the file reading waypoint - %s\n", wpt->shortname);
#endif
        gbfseek(mps_file_in, mpsFileInPos + reclen, SEEK_SET);
        waypt_free(wpt);
      } else {
        /* only add to the "real" list if a "user" waypoint otherwise add to the private list */
        if (mpsWptClass == MPSDEFAULTWPTCLASS) {
          waypt_add(wpt);
        } else {
          mps_wpt_q_add(&read_route_wpt_head, wpt);
          waypt_free(wpt);
        }
#ifdef DUMP_ICON_TABLE
        printf("\t{  %4u, \"%s\" },\n", icon, wpt->shortname);
#endif
      }
      break;

    case 'R':
      /* Route record */
      mps_route_r(mps_file_in, mps_ver_in, &rte);
      if (gbftell(mps_file_in) != mpsFileInPos + reclen) {
        /* should junk this record and not save it since we're out of sync with the file */
        /* Should and how do we warn the user?                                           */
#ifdef	MPS_DEBUG
        fprintf(stderr,"Lost sync with the file reading route - %s\n", rte->rte_name);
#endif
        gbfseek(mps_file_in, mpsFileInPos + reclen, SEEK_SET);
      }
      break;

    case 'T':
      /* Track record */
      mps_track_r(mps_file_in, mps_ver_in, &trk);
      if (gbftell(mps_file_in) != mpsFileInPos + reclen) {
        /* should junk this record and not save it since we're out of sync with the file */
        /* Should and how do we warn the user?                                           */
#ifdef	MPS_DEBUG
        fprintf(stderr,"Lost sync with the file reading track - %s\n", trk->rte_name);
#endif
        gbfseek(mps_file_in, mpsFileInPos + reclen, SEEK_SET);
      }
      break;

    case 'L':
      /* Map segment record */
      mps_mapsegment_r(mps_file_in, mps_ver_in);
      if (gbftell(mps_file_in) != mpsFileInPos + reclen) {
        /* should junk this record and not save it since we're out of sync with the file */
        /* Should and how do we warn the user?                                           */
        gbfseek(mps_file_in, mpsFileInPos + reclen, SEEK_SET);
      }
      break;

    case 'V':
      /* Mapset record */
      mps_mapsetname_r(mps_file_in, mps_ver_in);
      /* Last record in the file */
      morework = 0;
      break;
    default:
      /* Unknown record type.  Skip over it. */
      gbfseek(mps_file_in, reclen, SEEK_CUR);
    }

  }	/* while (!gbfeof(mps_file_in)) */

#ifdef DUMP_ICON_TABLE
  printf("\t{ -1, NULL },\n");
  printf("};\n");
#endif

  return ;

}

void
mps_write(void)
{
  int				short_length;
  waypoint*		wpt;
  route_head*		rte;
  route_head*		trk;

  char			recType;
  int				reclen;
  /* TODO: This kills a compiler warning but I'm not sure it's right */
  int				reclen2 = 0;
  unsigned int	tocopy;
  unsigned int	block;

  long			tempFilePos;
  unsigned int	mpsWptClass;

  unsigned char	copybuf[8192];

  short_length = atoi(snlen);

  if (mpsmergeout) {
    /* need to skip over the merging header and test merge version */
    mps_fileHeader_r(mps_file_temp, &mps_ver_temp);

    if (mpsverout) {
      if (mps_ver_temp != atoi(mpsverout)) {
        /* Need to clean up after a junk version specified */
        /* close the real output file + renamed original output file */
        /* then delete the "real" file and rename the temporarily renamed file back */
        gbfclose(mps_file_temp);
        gbfclose(mps_file_out);
        remove(fin_name);
        rename(tempname, fin_name);
        fatal(MYNAME ": merge source version is %d, requested out version is %d\n", mps_ver_temp, atoi(mpsverout));
      }
    } else {
      mpsverout = (char*) xmalloc(10);
      sprintf(mpsverout,"%d", mps_ver_temp);
    }
  }

  if (mpsverout) {
    mps_ver_out = atoi(mpsverout);
  } else {
    mps_ver_out = 5;
  }

  mkshort_handle = mkshort_new_handle();

  setshort_length(mkshort_handle, short_length);

  if (snwhiteopt) {
    setshort_whitespace_ok(mkshort_handle, atoi(snwhiteopt));
  } else {
    setshort_whitespace_ok(mkshort_handle, 0);
  }

  mps_fileHeader_w(mps_file_out, mps_ver_out);

  /* .mps file order is wpts, rtes, trks then mapsets. If we've not been asked to write
     wpts, but we are merging, then read in the waypoints from the original file and
     write them out, prior to doing rtes.
  */
  /* if ((mpsmergeout) && (global_opts.objective != wptdata)) { */
  if ((mpsmergeout) && (! doing_wpts)) {
    while (!gbfeof(mps_file_temp)) {

      reclen2 = gbfgetint32(mps_file_temp);

      /* Read the record type "flag" in - using gbfread in case in the future need more than one char */
      gbfread(&recType, 1, 1, mps_file_temp);

      if (recType == 'W')  {
        gbfwrite(&reclen, 4, 1, mps_file_out);	/* write out untouched */
        gbfwrite(&recType, 1, 1, mps_file_out);

        tempFilePos = gbftell(mps_file_temp);
        /* need to read in the waypoint info only because later we may need to check for uniqueness
           since we're here because the user didn't request waypoints, this should be acceptable */
        mps_waypoint_r(mps_file_temp, mps_ver_temp, &wpt, &mpsWptClass);
        mps_wpt_q_add(&written_wpt_head, wpt);
        waypt_free(wpt);
        /* now return to the start of the waypoint data to do a "clean" copy */
        gbfseek(mps_file_temp, tempFilePos, SEEK_SET);

        /* copy the data using a "reasonably" sized buffer */
        for (tocopy = reclen2; tocopy > 0; tocopy -= block) {
          block = (tocopy > sizeof(copybuf) ? sizeof(copybuf) : tocopy);
          gbfread(copybuf, block, 1, mps_file_temp);
          gbfwrite(copybuf, block, 1, mps_file_out);
        }
      } else {
        break;
      }
    }	/* while (!gbfeof(mps_file_temp)) */
  }	/* if (mpsmergeout) */

  /* irrespective of merging, now write out any waypoints */
  /* if (global_opts.objective == wptdata) { */
  if (doing_wpts) {

    if (mpsmergeout) {
      /* since we're processing waypoints, we should read in from whatever version and write out */
      /* in the selected version */
      while (!gbfeof(mps_file_temp)) {

        reclen2 = gbfgetint32(mps_file_temp);

        /* Read the record type "flag" in - using gbfread in case in the future need more than one char */
        gbfread(&recType, 1, 1, mps_file_temp);

        if (recType == 'W')  {
          /* need to be careful that we aren't duplicating a wpt defined from elsewhere */
          mps_waypoint_r(mps_file_temp, mps_ver_temp, &wpt, &mpsWptClass);
          if (mpsWptClass == MPSDEFAULTWPTCLASS) {
            waypt_add(wpt);
          } else {
            waypt_free(wpt);
          }
        } else {
          break;
        }
      }
    }
    waypt_disp_all(mps_waypoint_w_unique_wrapper);
  }

  /* prior to writing any tracks as requested, if we're doing a merge, read in the rtes
     from the original file and then write them out, ready for tracks to follow
  */

  /* if ((mpsmergeout) && (global_opts.objective != rtedata)) { */
  if ((mpsmergeout) && (! doing_rtes)) {
    while (!gbfeof(mps_file_temp)) {

      /* this might all fail if the relevant waypoints haven't been written */
      if (recType == 'R')  {
        gbfwrite(&reclen, 4, 1, mps_file_out);	/* write out untouched */
        gbfwrite(&recType, 1, 1, mps_file_out);

        /* copy the data using a "reasonably" sized buffer */
        for (tocopy = reclen2; tocopy > 0; tocopy -= block) {
          block = (tocopy > sizeof(copybuf) ? sizeof(copybuf) : tocopy);
          gbfread(copybuf, block, 1, mps_file_temp);
          gbfwrite(copybuf, block, 1, mps_file_out);
        }
      } else {
        break;
      }
      reclen2 = gbfgetint32(mps_file_temp);

      /* Read the record type "flag" in - using gbfread in case in the future need more than one char */
      gbfread(&recType, 1, 1, mps_file_temp);

    }	/* while (!gbfeof(mps_file_temp)) */
  }	/* if (mpsmergeout) */

  /* routes are next in the wpts, rtes, trks, mapset sequence */
  /* if (global_opts.objective == rtedata) { */
  if (doing_rtes) {

    if (mpsmergeout) {
      /* since we're processing routes, we should read in from whatever version and write out */
      /* in the selected version */
      while (!gbfeof(mps_file_temp)) {

        if (recType == 'R')  {
          mps_route_r(mps_file_temp, mps_ver_temp, &rte);
        } else {
          break;
        }

        reclen2 = gbfgetint32(mps_file_temp);

        /* Read the record type "flag" in - using gbfread in case in the future need more than one char */
        gbfread(&recType, 1, 1, mps_file_temp);
      }
    }
    /* need to make sure there is a "real" waypoint for each route waypoint
       Need to be careful about creating duplicate wpts as MapSource chokes on these
       so, if the user requested waypoints to be output too, then write the route
       waypoints only if unique in the total list of waypoints ("real" and route derived)
       If the user didn't request waypoints to be output, then output the route derived
       waypoints without consideration for uniqueness for "real" waypoints that haven't
       been output (phew!)
    */
    route_disp_all(mps_noop, mps_noop, mps_route_wpt_w_unique_wrapper);

    route_disp_all(mps_routehdr_w_wrapper, mps_routetrlr_w_wrapper, mps_routedatapoint_w_wrapper);
  }

  /* If merging but we haven't been requested to write out tracks, then read in tracks from
     the original file and write these out prior to any mapset writes later on
  */
  /* if ((mpsmergeout) && (global_opts.objective != trkdata)) { */
  if ((mpsmergeout) && (! doing_trks)) {
    while (!gbfeof(mps_file_temp)) {

      if (recType == 'T')  {
        gbfwrite(&reclen, 4, 1, mps_file_out);	/* write out untouched */
        gbfwrite(&recType, 1, 1, mps_file_out);

        /* copy the data using a "reasonably" sized buffer */
        for (tocopy = reclen2; tocopy > 0; tocopy -= block) {
          block = (tocopy > sizeof(copybuf) ? sizeof(copybuf) : tocopy);
          gbfread(copybuf, block, 1, mps_file_temp);
          gbfwrite(copybuf, block, 1, mps_file_out);
        }
      } else {
        break;
      }
      reclen2 = gbfgetint32(mps_file_temp);

      /* Read the record type "flag" in - using gbfread in case in the future need more than one char */
      gbfread(&recType, 1, 1, mps_file_temp);

    }	/* while (!gbfeof(mps_file_temp)) */
  }	/* if (mpsmergeout) */

  /* tracks are next in the wpts, rte, trks, mapset sequence in .mps files */
  /* if (global_opts.objective == trkdata) { */
  if (doing_trks) {
    if (mpsmergeout) {
      /* since we're processing tracks, we should read in from whatever version and write out
         in the selected version */
      while (!gbfeof(mps_file_temp)) {

        if (recType == 'T')  {
          mps_track_r(mps_file_temp, mps_ver_temp, &trk);
        } else {
          break;
        }

        reclen2 = gbfgetint32(mps_file_temp);

        /* Read the record type "flag" in - using gbfread in case in the future need more than one char */
        gbfread(&recType, 1, 1, mps_file_temp);
      }
    }
    track_disp_all(mps_trackhdr_w_wrapper, mps_noop, mps_trackdatapoint_w_wrapper);
  }

  if (mpsmergeout) {
    /* should now be reading a either a map segment or a mapset - since we would write out an empty one,
       let's use the one from the merge file which may well have decent data in */
    for (;;) {
      gbfwrite(&reclen, 4, 1, mps_file_out);	/* write out untouched */
      gbfwrite(&recType, 1, 1, mps_file_out);

      /* copy the data using a "reasonably" sized buffer */
      for (tocopy = reclen2; tocopy > 0; tocopy -= block) {
        block = (tocopy > sizeof(copybuf) ? sizeof(copybuf) : tocopy);
        gbfread(copybuf, block, 1, mps_file_temp);
        gbfwrite(copybuf, block, 1, mps_file_out);
      }

      if (recType != 'V') {
        reclen2 = gbfgetint32(mps_file_temp);

        /* Read the record type "flag" in - using gbfread in case in the future need more than one char */
        gbfread(&recType, 1, 1, mps_file_temp);
      } else {
        break;
      }
    }

  } else {
    mps_mapsetname_w(mps_file_out, mps_ver_out);
  }

  mkshort_del_handle(&mkshort_handle);

}

ff_vecs_t mps_vecs = {
  ff_type_file,
  FF_CAP_RW_ALL,
  mps_rd_init,
  mps_wr_init,
  mps_rd_deinit,
  mps_wr_deinit,
  mps_read,
  mps_write,
  NULL,
  mps_args,
  CET_CHARSET_MS_ANSI	/* CET-REVIEW */
};
