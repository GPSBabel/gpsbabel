/*
    Read and write Cetus files.

    Copyright (C) 2002-2008 Robert Lipe, robertlipe@usa.net

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

	2005/08/03:	Added track_read by O.K.
			(Thanx to Adam Schneider for additional information)

*/

#include "defs.h"
#if PDBFMTS_ENABLED
#include "pdbfile.h"

#define MYNAME "Cetus"
#define MYTYPE_WPT  	0x43577074  	/* CWpt */
#define MYTYPE_TRK 	0x7374726d	/* strm */

#define MYCREATOR 	0x63475053 	/* cGPS */
#define MYTRACK		0x44424c4b	/* DBLK */

#define NOTESZ 4096
#define DESCSZ 4096

typedef enum {
  WptEdit = 0, 	/* the position has been edited or it was */
  /* imported from another source */
  WptGPS2D = 1,   /* retrieved from a GPS with a 2D fix */
  WptGPS3D = 2,	/* retrieved from a GPS with a 3D fix */
  WptDGPS2D = 3,	/* retrieved from a GPS with a 2D fix and DGPS signal */
  WptDGPS3D = 4, 	/* retrieved from a GPS with a 3D fix and DGPS signal */
  WptAverage = 5,	/* averaging over 3D positions */
  WptCache = 50,	/* this position is a geocache reference */
  WptGarmin = 70	/* this position was imported from a Garmin GPS */
              /* the icon field contains the garmin symbol number */
} wpt_type;

struct cetus_wpt_s {
  char type;

  char   readonly;

  pdb_32 latitude; /* Big endian, degrees*1e7, s=negative */
  pdb_32 longitude; /* same as lat; w=negative */
  pdb_32 elevation; /* Big endian, meters*100. blank=-1e8 */

  pdb_16        year; /* sample time, UTC */
  unsigned char mon;
  unsigned char day;
  unsigned char hour;
  unsigned char min;
  unsigned char sec;

  /* accuracy and precision information for use where applicable */
  unsigned char  sat; /* ff if averaged or unknown */
  pdb_16 pdop; /* pdop * 100 */
  pdb_16 hdop;
  pdb_16 vdop;
  pdb_16 dgpstime;
  pdb_32 dgpsstn;
  pdb_32 avgtime;
  pdb_32 avgite;

  pdb_16 dopmask;
  pdb_16 elevmask;

  pdb_16 radius;
  pdb_32 distance;

  pdb_16 vyear; /* date visited */
  unsigned char vmon;
  unsigned char vday;
  unsigned char vhour;
  unsigned char vmin;
  unsigned char vsec;

  char   flagged;

  pdb_32 icon;
  pdb_16 category;
};

typedef struct cetus_track_head_s {
  char 		id[2];
  char 		version;
  unsigned char 	interval;
  unsigned short 	gps;
  char 		year;
  char 		month;
  char 		day;
  char 		hour;
  char 		min;
  char 		sec;
  char 		dsec;
  char 		tz;
  char 		desc;
} cetus_track_head_t;

#define TRACK_HEAD_SIZE sizeof(struct cetus_track_head_s)

typedef struct cetus_track_point_s {
  signed char hour;
  signed char min;
  signed char sec;
  signed char dsec;
  signed char sat;
  signed char hdop;
  pdb_32 latitude;
  pdb_32 longitude;
  short speed;
  short course;
  pdb_32 elevation;
} cetus_track_point_t;

#define TRACK_POINT_SIZE sizeof(struct cetus_track_point_s)

static pdbfile* file_in, *file_out;
static const char* out_fname;
static short_handle mkshort_wr_handle;
static int ct;

static char* dbname = NULL;
static char* appendicon = NULL;

static
arglist_t cetus_args[] = {
  {
    "dbname", &dbname, "Database name", NULL, ARGTYPE_STRING,
    ARG_NOMINMAX
  },
  {
    "appendicon", &appendicon, "Append icon_descr to description",
    NULL, ARGTYPE_BOOL, ARG_NOMINMAX
  },
  ARG_TERMINATOR
};

static waypoint*
read_track_point(cetus_track_point_t* data, const time_t basetime)
{
  int i, ilat, ilon;
  waypoint* wpt;

  ilat = be_read32(&data->latitude);
  ilon = be_read32(&data->longitude);

  if (data->hour == -1 || data->min == -1 || data->sec == -1 ||
      ilat == 2000000000 || ilon == 2000000000) {
    return NULL;  /* At least one of basic data is not available */
  }

  wpt = waypt_new();

  wpt->latitude = (double)ilat / 10000000.0;
  wpt->longitude = (double)ilon / 10000000.0;

  i = be_read32(&data->elevation);
  wpt->altitude = (i == -100000000) ? unknown_alt : (double) i / 100.0;

  if (data->sat != -1) {
    wpt->sat = data->sat;
  }
  if (data->hdop != -1) {
    wpt->hdop = (float) data->hdop / 10;
  }

  i = be_read16(&data->speed);
  if (i != 10000) {
    WAYPT_SET(wpt, speed, KNOTS_TO_MPS((float) i / 10));  /* meters/second */
  }
  i = be_read16(&data->course);
  if (i != 4000) {
    WAYPT_SET(wpt, course, (float) i / 10);
  }

  switch (data->hour >> 5) {	/* extract fix */
  case 1:
    wpt->fix = fix_none;
    break;
  case 2:
    wpt->fix = fix_2d;
    break;
  case 3:
    wpt->fix = fix_3d;
    break;
  case 4:
    wpt->fix = fix_dgps;
    break;
  default:
    break;			/* no GPS */
  }

  wpt->creation_time = basetime +
                       ((data->hour & 0x1F) * 3600) + (data->min * 60) + data->sec;
  if (data->dsec) {
    wpt->microseconds = (int)data->dsec * 10000;
  }

  return wpt;
}


static void
read_tracks(const pdbfile* pdb)
{
  pdbrec_t* pdb_rec;
  int reclen, records, total, points, dropped;
  char descr[(2 * TRACK_POINT_SIZE) + 1];
  char temp_descr[TRACK_POINT_SIZE + 1];
  cetus_track_head_t* head;
  waypoint* wpt;
  route_head* track;
  time_t basetime;

  track = route_head_alloc();
  track_add_head(track);

  total = 0;
  points = 0;
  dropped = 0;
  basetime = 0;

  for (pdb_rec = pdb->rec_list; pdb_rec; pdb_rec = pdb_rec->next) {
    int i, magic;
    char* c = (char*)pdb_rec->data;

    magic = be_read32(c);
    if (magic != MYTRACK) {
      fatal(MYNAME ": Invaid track data or unsupported version!\n");
    }

    reclen = be_read32(c+4);
    records = reclen / TRACK_POINT_SIZE;

    c += 8;

    for (i = 0; i < records; i++, c += TRACK_POINT_SIZE) {
      switch (total++) {
        struct tm tm;

      case 0: 	/* track header */
        head = (cetus_track_head_t*)c;
        if (head->id[0] != 'C' || head->id[1] != 'G') {
          fatal(MYNAME ": Invalid track header!\n");
        }

        memset(&tm, 0, sizeof(tm));
        tm.tm_mday = head->day;
        tm.tm_mon = head->month - 1;
        tm.tm_year = head->year + 100;
        basetime = mkgmtime(&tm);
        break;

      case 1: 	/* first part of description */
        strncpy(descr, c, TRACK_POINT_SIZE);
        break;

      case 2: 	/* continued description */
        strncpy(temp_descr, c, TRACK_POINT_SIZE);
        strcat(descr, temp_descr);	/* here is no need to check target size */
        if (strlen(descr) > 0) {
          track->rte_desc = xstrdup(descr);
        }
        break;

      default:
        wpt = read_track_point((cetus_track_point_t*)c, basetime);
        if (wpt != NULL) {
          track_add_wpt(track, wpt);
          points++;
        } else {
          dropped++;
        }
      }

    }
  }

  if (global_opts.verbose_status > 0) {
    printf(MYNAME ": Loaded %d track point(s) from source.\n", points);
    if (dropped > 0) {
      printf(MYNAME ": ! %d dropped because of missing data (no time, no coordinates) !\n", dropped);
    }
  }
}

static void
read_waypts(const pdbfile* pdb)
{
  struct cetus_wpt_s* rec;
  pdbrec_t* pdb_rec;
  char* vdata;

  for (pdb_rec = pdb->rec_list; pdb_rec; pdb_rec = pdb_rec->next) {
    waypoint* wpt_tmp;
    int i;

    wpt_tmp = waypt_new();

    rec = (struct cetus_wpt_s*) pdb_rec->data;
    if (be_read32(&rec->elevation) == -100000000) {
      wpt_tmp->altitude = unknown_alt;
    } else {
      wpt_tmp->altitude = be_read32(&rec->elevation) / 100.0;
    }

    wpt_tmp->latitude = be_read32(&rec->latitude) / 10000000.0;
    wpt_tmp->longitude = be_read32(&rec->longitude) / 10000000.0;

    if (rec->sat != 0xff) {
      wpt_tmp->sat = rec->sat;
    }

    i = be_read16(&rec->pdop);
    if (i != 0xffff) {
      wpt_tmp->pdop = i / 100.0;
    }
    i = be_read16(&rec->hdop);
    if (i != 0xffff) {
      wpt_tmp->hdop = i / 100.0;
    }
    i = be_read16(&rec->vdop);
    if (i != 0xffff) {
      wpt_tmp->vdop = i / 100.0;
    }

    switch (rec->type) {
    case WptGPS2D:
      wpt_tmp->fix = fix_2d;
      break;
    case WptGPS3D:
      wpt_tmp->fix = fix_3d;
      break;
    case WptDGPS2D:
      wpt_tmp->fix = fix_dgps;
      break;
    case WptDGPS3D:
      wpt_tmp->fix = fix_dgps;
      break;
    }

    if (be_read16(&rec->year) != 0xff) {
      struct tm tm;

      memset(&tm, 0, sizeof(tm));
      tm.tm_min = rec->min;
      tm.tm_hour = rec->hour;
      tm.tm_mday = rec->day;
      tm.tm_mon = rec->mon - 1;
      tm.tm_year = be_read16(&rec->year) - 1900;

      wpt_tmp->creation_time = mkgmtime(&tm);

    }

    vdata = (char*) pdb_rec->data + sizeof(*rec);

    wpt_tmp->shortname = xstrdup(vdata);
    vdata = vdata + strlen(vdata) + 1;

    wpt_tmp->description = xstrdup(vdata);
    vdata = vdata + strlen(vdata) + 1;

    wpt_tmp->notes = xstrdup(vdata);

    waypt_add(wpt_tmp);

  }
}

/* --------------------------------------------------------------------------- */

static void
rd_init(const char* fname)
{
  file_in = pdb_open(fname, MYNAME);
}

static void
rd_deinit(void)
{
  pdb_close(file_in);
  if (dbname) {
    xfree(dbname);
    dbname = NULL;
  }
}

static void
wr_init(const char* fname)
{
  file_out = pdb_create(fname, MYNAME);
  out_fname = fname;
  ct = 0;
}

static void
wr_deinit(void)
{
  pdb_close(file_out);
  if (dbname) {
    xfree(dbname);
    dbname = NULL;
  }
}

static void
data_read(void)
{
  if (file_in->creator != MYCREATOR) {
    fatal(MYNAME ": Not a Cetus file.\n");
  }

  switch (file_in->type) {
  case MYTYPE_TRK:
    read_tracks(file_in);
    break;

  case MYTYPE_WPT:
    read_waypts(file_in);
    break;
  }
}


static void
cetus_writewpt(const waypoint* wpt)
{
  struct cetus_wpt_s* rec;
  char* vdata;
  char* desc_long;
  char* desc_short;
  char* desc_geo;
  char* desc;

  rec = (struct cetus_wpt_s*) xcalloc(sizeof(*rec)+18 + NOTESZ + DESCSZ,1);

#if NEWTIME
  QDate date(wpt->creation_time.date());
  rec->day = date.day();
  rec->mon = date.month();
  be_write16(&rec->year, date.year());

  QTime time(wpt->creation_time.time());
  rec->min = time.minute();
  rec->hour = time.hour();
  rec->sec = time.second();

#else
  struct tm* tm;
  if (wpt->creation_time && (NULL != (tm = gmtime(&wpt->creation_time)))) {
    rec->min = tm->tm_min;
    rec->hour = tm->tm_hour;
    rec->sec = tm->tm_sec;
    rec->day = tm->tm_mday;
    rec->mon = tm->tm_mon + 1;
    be_write16(&rec->year, tm->tm_year + 1900);
  } else {
    rec->min = 0xff;
    rec->hour = 0xff;
    rec->sec = 0xff;
    rec->day = 0xff;
    rec->mon = 0xff;
    be_write16(&rec->year, 0xff);
  }
#endif
  be_write32(&rec->longitude, (unsigned int)(int)(wpt->longitude * 10000000.0));
  be_write32(&rec->latitude, (unsigned int)(wpt->latitude * 10000000.0));
  if (wpt->altitude == unknown_alt) {
    be_write32(&rec->elevation, -100000000);
  } else {
    be_write32(&rec->elevation, (unsigned int)(wpt->altitude * 100.0));
  }

  be_write16(&rec->pdop, wpt->pdop ? wpt->pdop * 100 : 0xffff);
  be_write16(&rec->hdop, wpt->hdop ? wpt->hdop * 100 : 0xffff);
  be_write16(&rec->vdop, wpt->vdop ? wpt->vdop * 100 : 0xffff);
  be_write16(&rec->dgpstime, 0xffff);
  be_write32(&rec->distance, 0xffffffff);

  rec->vmin = 0xff;
  rec->vhour = 0xff;
  rec->vsec = 0xff;
  rec->vday = 0xff;
  rec->vmon = 0xff;
  be_write16(&rec->vyear, 0xff);

  rec->sat = wpt->sat ? wpt->sat : 0xff;

  vdata = (char*)rec + sizeof(*rec);
  if (wpt->shortname) {
    char* sn = xstrdup(wpt->shortname);
    strncpy(vdata, sn, 16);
    vdata[15] = '\0';
    xfree(sn);
  } else {
    vdata[0] ='\0';
  }
  vdata += strlen(vdata) + 1;

  if (wpt->gc_data->diff) {
    xasprintf(&desc_geo, "%s%s by %s\n%.4s/%.4s %3.1f/%3.1f\n",
              wpt->gc_data->is_available==status_true ?
              "" : " (Disabled)",
              wpt->gc_data->is_archived==status_true ?
              " (Archived)" : "",
              wpt->gc_data->placer,
              gs_get_cachetype(wpt->gc_data->type),
              gs_get_container(wpt->gc_data->container),
              wpt->gc_data->diff/10.0,
              wpt->gc_data->terr/10.0);
  } else {
    desc_geo = xstrdup("");
  }

  if (wpt->gc_data->desc_short.utfstring) {
    char* stripped_html = strip_html(&wpt->gc_data->desc_short);
    desc_short = xstrdup(wpt->gc_data->diff == 0 ? "\n\n" : "");
    desc_short = xstrappend(desc_short, xstrdup(stripped_html));
    xfree(stripped_html);
  } else {
    desc_short = xstrdup("");
  }

  if (wpt->gc_data->desc_long.utfstring) {
    char* stripped_html = strip_html(&wpt->gc_data->desc_long);
    desc_long = xstrdup("\n\n");
    desc_long = xstrappend(desc_long, xstrdup(stripped_html));
    xfree(stripped_html);
  } else {
    desc_long = xstrdup("");
  }

  desc = wpt->description ? xstrdup(wpt->description) :
         xstrdup("");

  snprintf(vdata, DESCSZ, "%s%s%s%s",
           desc,
           desc_geo,
           desc_short,
           desc_long);

  xfree(desc);
  xfree(desc_geo);
  xfree(desc_short);
  xfree(desc_long);

  if (appendicon && !wpt->icon_descr.isNull()) {
    int left = DESCSZ - strlen(vdata);
    int ilen = strlen(wpt->icon_descr.toUtf8().data());
    if (ilen && left > (ilen+3)) {
      strcat(vdata, " (");
      strcat(vdata, wpt->icon_descr.toUtf8().data());
      strcat(vdata, ")");
    }
  }
  vdata += strlen(vdata) + 1;

  if (wpt->gc_data->hint) {
    char* hint = xstrdup(wpt->gc_data->hint);
    rec->type = WptCache;
    strncpy(vdata, hint, NOTESZ + 1) ;
    xfree(hint);
    vdata[NOTESZ] = '\0';
  } else {
    rec->type = WptEdit;
    vdata[0] ='\0';
  }
  vdata += strlen(vdata) + 1;

  pdb_write_rec(file_out, 0, 2, ct++, rec, (char*)vdata - (char*)rec);

  xfree(rec);
}

struct hdr {
  char* wpt_name;
  waypoint* wpt;
};

static
int
compare(const void* a, const void* b)
{
  const struct hdr* wa = (const struct hdr*) a;
  const struct hdr* wb = (const struct hdr*) b;

  return strcmp(wa->wpt->shortname, wb->wpt->shortname);
}

static void
data_write(void)
{
  int i, ct = waypt_count();
  struct hdr* htable, *bh;
  queue* elem, *tmp;
  extern queue waypt_head;
  waypoint* waypointp;
  mkshort_wr_handle = mkshort_new_handle();
  setshort_length(mkshort_wr_handle, 15);
  setshort_whitespace_ok(mkshort_wr_handle, 0);

  if (dbname) {
    strncpy(file_out->name, dbname, PDB_DBNAMELEN);
  } else {
    strncpy(file_out->name, out_fname, PDB_DBNAMELEN);
  }
  file_out->name[PDB_DBNAMELEN-1] = 0;
  file_out->attr = PDB_FLAG_BACKUP;
  file_out->ctime = file_out->mtime = current_time() + 2082844800U;
  file_out->type = MYTYPE_WPT;  /* CWpt */
  file_out->creator = MYCREATOR; /* cGPS */
  file_out->version = 1;

  /*
   * All this is to sort by waypoint names before going to Cetus.
   * Turns out plain old strcmp will do the trick...
   */

  htable = (struct hdr*) xmalloc(ct * sizeof(*htable));
  bh = htable;

  QUEUE_FOR_EACH(&waypt_head, elem, tmp) {
    waypointp = (waypoint*) elem;
    bh->wpt = waypointp;
    if (global_opts.synthesize_shortnames && waypointp->description) {
      if (waypointp->shortname) {
        xfree(waypointp->shortname);
      }
      waypointp->shortname = mkshort_from_wpt(mkshort_wr_handle, waypointp);
    }
    bh->wpt_name = waypointp->shortname;
    bh ++;
  }
  qsort(htable, ct, sizeof(*bh), compare);

  for (i=0; i<ct; i++) {
    cetus_writewpt(htable[i].wpt);
  }

  xfree(htable);
  mkshort_del_handle(&mkshort_wr_handle);
}


ff_vecs_t cetus_vecs = {
  ff_type_file,
  { (ff_cap)(ff_cap_write | ff_cap_read), ff_cap_read, ff_cap_none },
  rd_init,
  wr_init,
  rd_deinit,
  wr_deinit,
  data_read,
  data_write,
  NULL,
  cetus_args,
  CET_CHARSET_MS_ANSI, 0	/* CET-REVIEW */
};
#endif
