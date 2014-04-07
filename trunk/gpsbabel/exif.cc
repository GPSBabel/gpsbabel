/*

    Support for embedded (JPEG) Exif-GPS information.

    Copyright (C) 2008 Olaf Klein, o.b.klein@gpsbabel.org

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
 * Exif specifications can be found at
 * 2012, version 2.3: http://www.cipa.jp/english/hyoujunka/kikaku/pdf/DC-008-2012_E.pdf
 * 2010, version 2.3: http://www.cipa.jp/english/hyoujunka/kikaku/pdf/DC-008-2010_E.pdf
 * 2002, version 2.2: http://www.exif.org/Exif2-2.PDF
 * 1998, version 2.1: http://www.exif.org/Exif2-1.PDF
 */

#include <ctype.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>

#include "defs.h"
#include "garmin_tables.h"
#include "jeeps/gpsmath.h"

#define MYNAME "exif"

// #define EXIF_DBG

#define UNKNOWN_TIMESTAMP 999999999

#define IFD0		0
#define IFD1		1
#define EXIF_IFD	2		/* dummy index */
#define GPS_IFD		3		/* dummy index */
#define INTER_IFD	4		/* dummy index */

#define EXIF_TYPE_BYTE		1
#define EXIF_TYPE_ASCII		2
#define EXIF_TYPE_SHORT		3
#define EXIF_TYPE_LONG		4
#define EXIF_TYPE_RAT		5
/* TIFF 6.0 */
#define EXIF_TYPE_SBYTE		6
#define EXIF_TYPE_UNK		7
#define EXIF_TYPE_SSHORT	8
#define EXIF_TYPE_SLONG		9
#define EXIF_TYPE_SRAT		10
#define EXIF_TYPE_FLOAT		11
#define EXIF_TYPE_DOUBLE	12
#define EXIF_TYPE_IFD		13
#define EXIF_TYPE_UNICODE	14
#define EXIF_TYPE_COMPLEX	15
#define EXIF_TYPE_LONG8		16
#define EXIF_TYPE_SLONG8	17
#define EXIF_TYPE_IFD8		18

#define BYTE_TYPE(a) ( (a==EXIF_TYPE_BYTE) || (a==EXIF_TYPE_ASCII) || (a==EXIF_TYPE_UNK) )
#define WORD_TYPE(a) ( (a==EXIF_TYPE_SHORT) || (a==EXIF_TYPE_SSHORT) )
#define LONG_TYPE(a) ( (a==EXIF_TYPE_LONG) || (a==EXIF_TYPE_SLONG) || (a==EXIF_TYPE_IFD) )

#define IFD0_TAG_EXIF_IFD_OFFS		0x8769
#define IFD0_TAG_GPS_IFD_OFFS		0x8825

#define IFD1_TAG_STRIP_OFFS		0x0111
#define IFD1_TAG_JPEG_OFFS		0x0201
#define IFD1_TAG_JPEG_SIZE		0x0202

#define EXIF_IFD_TAG_USER_CMT		0x9286
#define EXIF_IFD_TAG_INTER_IFD_OFFS	0xA005

#define GPS_IFD_TAG_VERSION		0x0000
#define GPS_IFD_TAG_LATREF		0x0001
#define GPS_IFD_TAG_LAT			0x0002
#define GPS_IFD_TAG_LONREF		0x0003
#define GPS_IFD_TAG_LON			0x0004
#define GPS_IFD_TAG_ALTREF		0x0005
#define GPS_IFD_TAG_ALT			0x0006
#define GPS_IFD_TAG_TIMESTAMP		0x0007
#define GPS_IFD_TAG_SAT			0x0008
#define GPS_IFD_TAG_MODE		0x000A
#define GPS_IFD_TAG_DOP			0x000B
#define GPS_IFD_TAG_SPEEDREF		0x000C
#define GPS_IFD_TAG_SPEED		0x000D
#define GPS_IFD_TAG_DATUM		0x0012
#define GPS_IFD_TAG_DATESTAMP		0x001D

typedef struct exif_tag_s {
  queue Q;
  uint16_t id;
  uint16_t type;
  uint32_t count;
  uint32_t value;
  uint32_t origin;
  uint32_t size;
#ifdef EXIF_DBG
  uint32_t offs;
#endif
  unsigned char data_is_dynamic:1;
  void* data;
} exif_tag_t;

typedef struct exif_ifd_s {
  queue Q;
  uint32_t next_ifd;
  uint16_t nr;
  uint16_t count;
  queue tags;
} exif_ifd_t, *exif_ifd_p;

typedef struct exif_app_s {
  queue Q;
  uint16_t marker;
  gbsize_t len;
  gbfile* fcache;
  gbfile* fexif;
  queue ifds;
} exif_app_t;

static gbfile* fin, *fout;
static queue exif_apps;
static exif_app_t* exif_app;
const Waypoint* exif_wpt_ref;
time_t exif_time_ref;
static char exif_success;
static char* exif_fout_name;

static char* opt_filename, *opt_overwrite, *opt_frame, *opt_name;

static uint8_t writer_gps_tag_version[4] = {2, 0, 0, 0};

arglist_t exif_args[] = {
  { "filename", &opt_filename, "Set waypoint name to source filename", "Y", ARGTYPE_BOOL, ARG_NOMINMAX },
  { "frame", &opt_frame, "Time-frame (in seconds)", "10", ARGTYPE_INT, "0", NULL },
  { "name", &opt_name, "Locate waypoint for tagging by this name", NULL, ARGTYPE_STRING, ARG_NOMINMAX },
  { "overwrite", &opt_overwrite, "!OVERWRITE! the original file. Default=N", "N", ARGTYPE_BOOL, ARG_NOMINMAX },
  ARG_TERMINATOR
};

#ifdef EXIF_DBG
static void
print_buff(const char* buf, int sz, const char* cmt)
{
  int i;

  printf("%s: ", cmt);
  for (i = 0; i < sz; i++) {
    printf("%02x ", buf[i] & 0xFF);
  }
  for (i = 0; i < sz; i++) {
    char c = buf[i];
    if (isspace(c)) {
      c = ' ';
    } else if (! isprint(c)) {
      c = '.';
    }
    printf("%c", c);
  }
}
#endif

static uint16_t
exif_type_size(const uint16_t type)
{
  uint16_t size;

  switch (type) {
  case EXIF_TYPE_BYTE:
  case EXIF_TYPE_ASCII:
  case EXIF_TYPE_UNK:
    size = 1;
    break;

  case EXIF_TYPE_SHORT:
  case EXIF_TYPE_SSHORT:
  case EXIF_TYPE_UNICODE:
    size = 2;
    break;
  case EXIF_TYPE_IFD:
  case EXIF_TYPE_LONG:
  case EXIF_TYPE_SLONG:
  case EXIF_TYPE_FLOAT:
    size = 4;
    break;

  case EXIF_TYPE_RAT:
  case EXIF_TYPE_SRAT:
  case EXIF_TYPE_DOUBLE:
  case EXIF_TYPE_LONG8:
  case EXIF_TYPE_SLONG8:
  case EXIF_TYPE_IFD8:
    size = 8;
    break;

  default:
    fatal(MYNAME ": Unknown data type %d! Please report.\n", type);
  }
  return size;
}

// TODO: If this were actually ever used (!?!?!) it could probably be 
// replaced by return QDateTime(time).toString("yyyy/MM/dd, hh:mm:ss);
static QString
exif_time_str(const time_t time)
{
  struct tm tm;

  tm = *localtime(&time);
  tm.tm_year += 1900;
  tm.tm_mon += 1;

  return QString().sprintf("%04d/%02d/%02d, %02d:%02d:%02d",
         tm.tm_year, tm.tm_mon, tm.tm_mday, tm.tm_hour, tm.tm_min, tm.tm_sec);
}

static char*
exif_read_str(exif_tag_t* tag)
{
  // Panasonic DMC-TZ10 stores datum with trailing spaces.
  char* buf = xstrndup((char*)tag->data, tag->size);
  rtrim(buf);
  return buf;
}

static double
exif_read_double(const exif_tag_t* tag, const int index)
{
  unsigned int num, den;
  int32_t* data = (int32_t*)tag->data;

  num = data[index * 2];
  den = data[(index * 2) + 1];
  if (den == 0) {
    den = 1;
  }

  return (double)num / (double)den;
}

static double
exif_read_coord(const exif_tag_t* tag)
{
  double res, min, sec;

  res = exif_read_double(tag, 0);
  if (tag->count == 1) {
    return res;
  }

  min = exif_read_double(tag, 1);
  res += (min / 60);
  if (tag->count == 2) {
    return res;
  }

  sec = exif_read_double(tag, 2);
  res += (sec / 3600);

  return res;
}

static time_t
exif_read_timestamp(const exif_tag_t* tag)
{
  double hour, min, sec;

  hour = exif_read_double(tag, 0);
  min = exif_read_double(tag, 1);
  sec = exif_read_double(tag, 2);

  return ((int)hour * SECONDS_PER_HOUR) + ((int)min * 60) + (int)sec;
}

static time_t
exif_read_datestamp(const exif_tag_t* tag)
{
  struct tm tm;
  char* str;

  memset(&tm, 0, sizeof(tm));
  str = xstrndup((char*)tag->data, tag->size);
  sscanf(str, "%d:%d:%d", &tm.tm_year, &tm.tm_mon, &tm.tm_mday);
  xfree(str);

  tm.tm_year -= 1900;
  tm.tm_mon -= 1;

  return mkgmtime(&tm);
}

static void
exif_release_tag(exif_tag_t* tag)
{
  dequeue(&tag->Q);
  if (tag->data_is_dynamic) {
    xfree(tag->data);
  }
  xfree(tag);
}

static void
exif_release_ifd(exif_ifd_t* ifd)
{
  if (ifd != NULL) {
    queue* elem, *tmp;

    QUEUE_FOR_EACH(&ifd->tags, elem, tmp) {
      exif_release_tag((exif_tag_t*)elem);
    }
    xfree(ifd);
  }
}

static void
exif_release_apps(void)
{
  queue* e0, *t0;

  QUEUE_FOR_EACH(&exif_apps, e0, t0) {
    queue* e1, *t1;
    exif_app_t* app = (exif_app_t*)dequeue(e0);

    if (app->fcache) {
      gbfclose(app->fcache);
    }
    if (app->fexif) {
      gbfclose(app->fexif);
    }
    QUEUE_FOR_EACH(&app->ifds, e1, t1) {
      exif_ifd_t* ifd = (exif_ifd_t*)dequeue(e1);
      exif_release_ifd(ifd);
    }
    xfree(app);
  }
}

static uint32_t
exif_ifd_size(exif_ifd_t* ifd)
{
  queue* elem, *tmp;
  uint32_t res = 6; 	/* nr of tags + next_ifd */

  res += (ifd->count * 12);
  QUEUE_FOR_EACH(&ifd->tags, elem, tmp) {
    exif_tag_t* tag = (exif_tag_t*)elem;
    if (tag->size > 4) {
      uint32_t size = tag->size;
      if (size & 1) {
        size++;
      }
      res += size;
    }
  }

  return res;
}

static exif_app_t*
exif_load_apps(void)
{
  exif_app_t* exif_app = NULL;

  while (! gbfeof(fin)) {
    exif_app_t* app = (exif_app_t*) xcalloc(sizeof(*app), 1);

    ENQUEUE_TAIL(&exif_apps, &app->Q);
    QUEUE_INIT(&app->ifds);
    app->fcache = gbfopen(NULL, "wb", MYNAME);

    app->marker = gbfgetuint16(fin);
    app->len = gbfgetuint16(fin);
#ifdef EXIF_DBG
    printf(MYNAME ": api = %02X, len = %d, offs = %04X\n", app->marker & 0xFF, app->len, gbftell(fin));
#endif
    if (exif_app || (app->marker == 0xFFDA)) /* compressed data */ {
      gbfcopyfrom(app->fcache, fin, 0x7FFFFFFF);
#ifdef EXIF_DBG
      printf(MYNAME ": compressed data size = %d\n", gbftell(app->fcache));
#endif
    } else {
      gbfcopyfrom(app->fcache, fin, app->len - 2);
      if (app->marker == 0xFFE1) {
        exif_app = app;
      }
    }
  }

  return exif_app;
}

static exif_ifd_t*
exif_read_ifd(exif_app_t* app, const uint16_t ifd_nr, gbsize_t offs,
              uint32_t* exif_ifd_ofs, uint32_t* gps_ifd_ofs, uint32_t* inter_ifd_ofs)
{
  queue* elem, *tmp;
  uint16_t i;
  exif_ifd_t* ifd;
  gbfile* fin = app->fexif;

  ifd = (exif_ifd_t*) xcalloc(sizeof(*ifd), 1);
  QUEUE_INIT(&ifd->tags);
  ENQUEUE_TAIL(&app->ifds, &ifd->Q);
  ifd->nr = ifd_nr;

  gbfseek(fin, offs, SEEK_SET);
  ifd->count = gbfgetuint16(fin);

#ifdef EXIF_DBG
  {
    char* name;
    switch (ifd_nr) {
    case IFD0:
      name = "IFD0";
      break;
    case IFD1:
      name = "IFD1";
      break;
    case GPS_IFD:
      name = "GPS";
      break;
    case EXIF_IFD:
      name = "EXIF";
      break;
    case INTER_IFD:
      name = "INTER";
      break;
    default:
      name = "private";
      break;
    }
    printf(MYNAME "-offs 0x%04X: Number of items in IFD%d \"%s\" = %d (0x%2x)\n",
           offs, ifd_nr, name, ifd->count, ifd->count);
  }
#endif
  if (ifd->count == 0) {
    return ifd;
  }

  for (i = 0; i < ifd->count; i++) {
    exif_tag_t* tag;

    tag = (exif_tag_t*) xcalloc(sizeof(*tag), 1);
#ifdef EXIF_DBG
    tag->offs = offs;
    offs = gbftell(fin);
#endif

    ENQUEUE_TAIL(&ifd->tags, &tag->Q);

    tag->id = gbfgetuint16(fin);
    tag->type = gbfgetuint16(fin);
    tag->count = gbfgetuint32(fin);
    tag->size = exif_type_size(tag->type) * tag->count;
    tag->data = &tag->value;

    if (BYTE_TYPE(tag->type) && (tag->count <= 4)) {
      gbfread(tag->data, 4, 1, fin);
    } else {
      tag->value = gbfgetuint32(fin);
      tag->origin = tag->value;
    }

    if (ifd_nr == IFD0) {
      if (tag->id == IFD0_TAG_EXIF_IFD_OFFS) {
        *exif_ifd_ofs = tag->value;
      } else if (tag->id == IFD0_TAG_GPS_IFD_OFFS) {
        *gps_ifd_ofs = tag->value;
      }
    } else if (ifd_nr == EXIF_IFD) {
      if (tag->id == EXIF_IFD_TAG_INTER_IFD_OFFS) {
        *inter_ifd_ofs = tag->value;
      }
    }
  }

  ifd->next_ifd = gbfgetuint16(fin);

  QUEUE_FOR_EACH(&ifd->tags, elem, tmp) {
    exif_tag_t* tag = (exif_tag_t*)elem;
    if ((tag->size > 4) && (tag->value)) {
      uint16_t i;
      char* ptr;

      tag->data = xmalloc(tag->size);
      tag->data_is_dynamic = 1;

      ptr = (char*) tag->data;
      gbfseek(fin, tag->value, SEEK_SET);

      if (BYTE_TYPE(tag->type)) {
        gbfread(ptr, tag->count, 1, fin);
      } else for (i = 0; i < tag->count; i++) {
          switch (tag->type) {
          case EXIF_TYPE_SHORT:
          case EXIF_TYPE_SSHORT:
            *(int16_t*)ptr = gbfgetuint16(fin);
            break;
          case EXIF_TYPE_IFD:
          case EXIF_TYPE_LONG:
          case EXIF_TYPE_SLONG:
            *(int32_t*)ptr = gbfgetuint32(fin);
            break;
          case EXIF_TYPE_RAT:
          case EXIF_TYPE_SRAT:
            *(int32_t*)ptr = gbfgetuint32(fin);
            *(int32_t*)(ptr+4) = gbfgetuint32(fin);
            break;
          case EXIF_TYPE_FLOAT:
            *(float*)ptr = gbfgetflt(fin);
            break;
          case EXIF_TYPE_DOUBLE:
            *(double*)ptr = gbfgetdbl(fin);
            break;
          default:
            gbfread(ptr, 1, 1, fin);
            break;
          }
          ptr += (tag->size / tag->count);
        }
    }
#ifdef EXIF_DBG
    printf(MYNAME "-offs 0x%04X: ifd=%d id=0x%04X t=0x%04X c=%4d s=%4d v=0x%08X",
           tag->offs, ifd->nr, tag->id, tag->type, tag->count, tag->size, tag->value);
    if (tag->type == EXIF_TYPE_ASCII) {
      printf(" \"%s\"", exif_read_str(tag));
    }
    printf("\n");
#endif
  }

  return ifd;
}

static void
exif_read_app(exif_app_t* app)
{
  gbsize_t offs;
  uint32_t exif_ifd_ofs, gps_ifd_ofs, inter_ifd_ofs;
  exif_ifd_t* ifd;
  gbfile* fin = app->fexif;

#ifdef EXIF_DBG
  printf(MYNAME ": read_app...\n");
  print_buff((const char*)fin->handle.mem, 16, MYNAME);
  printf("\n");
#endif
  exif_ifd_ofs = gps_ifd_ofs = inter_ifd_ofs = 0;

  gbfseek(fin, 4, SEEK_SET);
  offs = gbfgetuint32(fin);

  ifd = exif_read_ifd(app, IFD0, offs, &exif_ifd_ofs, &gps_ifd_ofs, &inter_ifd_ofs);
  if (ifd == NULL) {
    return;
  }
  if (ifd->next_ifd) {
    ifd = exif_read_ifd(app, IFD1, ifd->next_ifd, &exif_ifd_ofs, &gps_ifd_ofs, &inter_ifd_ofs);
  }
  if (exif_ifd_ofs) {
    ifd = exif_read_ifd(app, EXIF_IFD, exif_ifd_ofs, NULL, NULL, &inter_ifd_ofs);
  }
  if (gps_ifd_ofs) {
    ifd = exif_read_ifd(app, 3, gps_ifd_ofs, NULL, NULL, NULL);
  }
  if (inter_ifd_ofs) {
    ifd = exif_read_ifd(app, 4, inter_ifd_ofs, NULL, NULL, NULL);
  }
  // The return values of exif_read_ifd above aren't actually used.  
  // Warning hush.
  (void) ifd;
}

static void
exif_examine_app(exif_app_t* app)
{
  uint16_t endianess;
  uint32_t ident;
  gbfile* ftmp = exif_app->fcache;

  gbfrewind(ftmp);
  ident = gbfgetuint32(ftmp);
  is_fatal(ident != 0x66697845, MYNAME ": Invalid EXIF header magic.");
  is_fatal(gbfgetint16(ftmp) != 0, MYNAME ": Error in EXIF header.");
  endianess = gbfgetint16(ftmp);

#ifdef EXIF_DBG
  printf(MYNAME ": endianess = 0x%04X\n", endianess);
#endif
  if (endianess == 0x4949) {
    ftmp->big_endian = 0;
  } else if (endianess == 0x4D4D) {
    ftmp->big_endian = 1;
  } else {
    fatal(MYNAME ": Invalid endianess identifier 0x%04X!\n", endianess);
  }

  gbfseek(ftmp, 6, SEEK_SET);
  app->fexif = gbfopen(NULL, "wb", MYNAME);
  app->fexif->big_endian = ftmp->big_endian;
  gbfcopyfrom(app->fexif, ftmp, 0x7FFFFFFF);

  exif_read_app(exif_app);
}

static exif_ifd_t*
exif_find_ifd(exif_app_t* app, const uint16_t ifd_nr)
{
  queue* e0, *t0;

  QUEUE_FOR_EACH(&app->ifds, e0, t0) {
    exif_ifd_t* ifd = (exif_ifd_t*)e0;

    if (ifd->nr == ifd_nr) {
      return ifd;
    }
  }
  return NULL;
}

static exif_tag_t*
exif_find_tag(exif_app_t* app, const uint16_t ifd_nr, const uint16_t tag_id)
{
  exif_ifd_t* ifd = exif_find_ifd(app, ifd_nr);
  if (ifd != NULL) {
    queue* elem, *tmp;
    QUEUE_FOR_EACH(&ifd->tags, elem, tmp) {
      exif_tag_t* tag = (exif_tag_t*)elem;
      if (tag->id == tag_id) {
        return tag;
      }
    }
  }
  return NULL;
}

static time_t
exif_get_exif_time(exif_app_t* app)
{
  QDateTime res;

  exif_tag_t* tag;

  tag = exif_find_tag(app, EXIF_IFD, 0x9003);			/* DateTimeOriginal from EXIF */
  if (! tag) {
    tag = exif_find_tag(app, IFD0, 0x0132);  /* DateTime from IFD0 */
  }
  if (! tag) {
    tag = exif_find_tag(app, EXIF_IFD, 0x9004);  /* DateTimeDigitized from EXIF */
  }

  if (tag) {
    char* str;

    str = exif_read_str(tag);
    res = QDateTime::fromString(str, "yyyy:MM:dd hh:mm:ss");
    xfree(str);
  }
  return res.toTime_t();
}

static Waypoint*
exif_waypt_from_exif_app(exif_app_t* app)
{
  Waypoint* wpt;
  queue* elem, *tmp;
  exif_ifd_t* ifd;
  exif_tag_t* tag;
  char lat_ref = '\0';
  char lon_ref = '\0';
  char alt_ref = 0;
  char speed_ref = 'K';
  char* datum = NULL;
  char mode = '\0';
  double gpsdop = unknown_alt;
  double alt = unknown_alt;
  time_t timestamp = UNKNOWN_TIMESTAMP;
  time_t datestamp = UNKNOWN_TIMESTAMP;

  ifd = exif_find_ifd(app, GPS_IFD);
  if (ifd == NULL) {
    return NULL;
  }

  wpt = new Waypoint;

  wpt->latitude = unknown_alt;
  wpt->longitude = unknown_alt;

  QUEUE_FOR_EACH(&ifd->tags, elem, tmp) {
    tag = (exif_tag_t*)elem;

    switch (tag->id) {
    case GPS_IFD_TAG_VERSION:
      break;
    case GPS_IFD_TAG_LATREF:
      lat_ref = *(char*)tag->data;
      break;
    case GPS_IFD_TAG_LAT:
      wpt->latitude = exif_read_coord(tag);
      break;
    case GPS_IFD_TAG_LONREF:
      lon_ref = *(char*)tag->data;
      break;
    case GPS_IFD_TAG_LON:
      wpt->longitude = exif_read_coord(tag);
      break;
    case GPS_IFD_TAG_ALTREF:
      alt_ref = *(char*)tag->data;
      break;
    case GPS_IFD_TAG_ALT:
      alt = exif_read_double(tag, 0);
      break;
    case GPS_IFD_TAG_TIMESTAMP:
      timestamp = exif_read_timestamp(tag);
      break;
    case GPS_IFD_TAG_SAT:
      wpt->sat = atoi((char*)tag->data);
      break;
    case GPS_IFD_TAG_MODE:
      mode = *(char*)tag->data;
      break;
    case GPS_IFD_TAG_DOP:
      gpsdop = exif_read_double(tag, 0);
      break;
    case GPS_IFD_TAG_SPEEDREF:
      speed_ref = *(char*)tag->data;
      break;
    case GPS_IFD_TAG_SPEED:
      WAYPT_SET(wpt, speed, exif_read_double(tag, 0));
      break;
    case GPS_IFD_TAG_DATUM:
      datum = exif_read_str(tag);
      break;
    case GPS_IFD_TAG_DATESTAMP:
      datestamp = exif_read_datestamp(tag);
      break;
    }
  }

  if ((wpt->latitude == unknown_alt) || (wpt->longitude == unknown_alt)) {
    fatal(MYNAME ": Missing GPSLatitude and/or GPSLongitude!\n");
  }

  if (lat_ref == 'S') {
    wpt->latitude *= -1;
  } else if (lat_ref != 'N') {
    warning(MYNAME ": GPSLatitudeRef not set! Using N(orth).\n");
  }

  if (lon_ref == 'W') {
    wpt->longitude *= -1;
  } else if (lon_ref != 'E') {
    warning(MYNAME ": GPSLongitudeRef not set! Using E(east).\n");
  }

#ifdef EXIF_DBG
  printf(MYNAME "-GPSLatitude =  %12.7f\n", wpt->latitude);
  printf(MYNAME "-GPSLongitude = %12.7f\n", wpt->longitude);
#endif
  if (datum) {
    int idatum = gt_lookup_datum_index(datum, MYNAME);
    if (idatum < 0) {
      fatal(MYNAME ": Unknown GPSMapDatum \"%s\"!\n", datum);
    }
    if (idatum != DATUM_WGS84) {
      double alt;
      GPS_Math_WGS84_To_Known_Datum_M(wpt->latitude, wpt->longitude, 0.0,
                                      &wpt->latitude, &wpt->longitude, &alt, idatum);
    }
    xfree(datum);
  }

  if (alt != unknown_alt) {
    double sign;
    switch (alt_ref) {
    case 0:
      sign = 1.0;
      break;

    case 1:
      sign = -1.0;
      break;

    default:
      warning(MYNAME ": Invalid GPSAltitudeRef (%d)! Using default value 0 (= Sea level).\n", alt_ref);
      sign = 1.0;
    }
    wpt->altitude = sign * alt;
#ifdef EXIF_DBG
    printf(MYNAME "-GPSAltitude =  %12.7f m\n", wpt->altitude);
#endif
  }

  if WAYPT_HAS(wpt, speed) {
    switch (speed_ref) {
    case 'K':
      wpt->speed = KPH_TO_MPS(wpt->speed);
      break;
    case 'M':
      wpt->speed = MPH_TO_MPS(wpt->speed);
      break;
    case 'N':
      wpt->speed = KNOTS_TO_MPS(wpt->speed);
      break;
    default:
      wpt->speed = 0;
      WAYPT_UNSET(wpt, speed);
      warning(MYNAME ": Unknown GPSSpeedRef unit %c (0x%02x)!\n", speed_ref, speed_ref);
    }
#ifdef EXIF_DBG
    if WAYPT_HAS(wpt, speed) {
      printf(MYNAME "-GPSSpeed = %12.2f m/s\n", wpt->speed);
    }
#endif
  }

  if (mode == '2') {
    wpt->fix = fix_2d;
    if (gpsdop != unknown_alt) {
      wpt->hdop = gpsdop;
    }
  } else if (mode == '3') {
    wpt->fix = fix_3d;
    if (gpsdop != unknown_alt) {
      wpt->pdop = gpsdop;
    }
  }

  if (timestamp != UNKNOWN_TIMESTAMP) {
    if (datestamp != UNKNOWN_TIMESTAMP) {
      timestamp += datestamp;
    }
  } else {
    timestamp = datestamp;
  }

  if (timestamp != UNKNOWN_TIMESTAMP) {
#ifdef EXIF_DBG
    char* str = exif_time_str(timestamp);
    printf(MYNAME "-GPSTimeStamp =   %s\n", str);
    xfree(str);
#endif
    wpt->SetCreationTime(timestamp);
  } else {
    wpt->SetCreationTime(exif_get_exif_time(app));
  }

  tag = exif_find_tag(app, EXIF_IFD, EXIF_IFD_TAG_USER_CMT); /* UserComment */
  if (tag && (tag->size > 8)) {
    char* str = NULL;
    if (memcmp(tag->data, "ASCII\0\0\0", 8) == 0) {
      str = xstrndup((char*)tag->data + 8, tag->size - 8);
    } else if (memcmp(tag->data, "UNICODE\0", 8) == 0) {
      int i, len = (tag->size - 8) / 2;
      int16_t* s = (int16_t*)((char*)tag->data + 8);
      for (i = 0; i < len; i++) {
        s[i] = be_read16(&s[i]);  /* always BE ? */
      }
      str = cet_str_uni_to_any(s, len, global_opts.charset);
    }
    if (str != NULL) {
      wpt->notes = str;
    }
  }

  if (opt_filename) {
    char* c, *cx;
    char* str = xstrdup(fin->name);

    cx = str;
    if ((c = strrchr(cx, ':'))) {
      cx = c + 1;
    }
    if ((c = strrchr(cx, '\\'))) {
      cx = c + 1;
    }
    if ((c = strrchr(cx, '/'))) {
      cx = c + 1;
    }
    if (((c = strchr(cx, '.'))) && (c != cx)) {
      *c = '\0';
    }
    wpt->shortname = cx;
    xfree(str);
  }

  return wpt;
}

static void
exif_dec2frac(double val, int32_t* num, int32_t* den)
{
  char sval[16], snum[16];
  char dot = 0;
  int den1 = 1;
  int num1, num2, den2, rem;
  char* cx;
  double vx;

  if (val < 0.000000001) {
    val = 0.0;
  } else if (val > 999999999.0) {
    fatal(MYNAME ": Value (%f) to big for a rational representation!\n", val);
  }

  num1 = 0;
  vx = fabs(val);
  while (vx > 1) {
    num1++;
    vx = vx / 10;
  }

  snprintf(sval, sizeof(sval), "%*.*f", 9, 9 - num1, fabs(val));
  snum[0] = '\0';

  cx = sval;
  while (*cx) {
    if (dot) {
      den1 *= 10;
    }
    if (*cx == '.') {
      dot = 1;
    } else {
      strncat(snum, cx, 1);
    }
    cx++;
  }

  num1 = atoi(snum);
  if (den1 == 1) {
    *num = num1;
    *den = den1;
  }

  num2 = num1;
  den2 = den1;
  rem  = 1;

  /* Euclid's Algorithm to find the gcd */
  while (num2 % den2) {
    rem = num2 % den2;
    num2 = den2;
    den2 = rem;
  }
  if (den2 != den1) {
    rem = den2;
  }

  *num = num1 / rem;
  *den = den1 / rem;
}

static exif_tag_t*
exif_put_value(const int ifd_nr, const uint16_t tag_id, const uint16_t type, const uint32_t count, const int index, const void* data)
{
  exif_tag_t* tag = NULL;
  exif_ifd_t* ifd;
  uint16_t item_size, size;

  ifd = exif_find_ifd(exif_app, ifd_nr);
  if (ifd == NULL) {
    ifd = (exif_ifd_t*) xcalloc(sizeof(*ifd), 1);
    ifd->nr = ifd_nr;
    QUEUE_INIT(&ifd->tags);
    ENQUEUE_TAIL(&exif_app->ifds, &ifd->Q);
  } else {
    tag = exif_find_tag(exif_app, ifd_nr, tag_id);
  }

  item_size = exif_type_size(type);

  if ((data == NULL) || (count < 1) || (index < 0)) {
    size = 0;
  } else {
    size = (index + count) * item_size;
  }

  if (tag == NULL) {
    if (size == 0) {
      return NULL;
    }

    tag = (exif_tag_t*) xcalloc(sizeof(*tag), 1);

    tag->id = tag_id;
    tag->type = type;
    tag->count = index + count;
    tag->size = size;
    tag->data = xcalloc((size < 4) ? 4 : size, 1);
    tag->data_is_dynamic = 1;
    ifd->count++;

    ENQUEUE_TAIL(&ifd->tags, &tag->Q);
  } else {
    if (size == 0) {	/* remove this element */
      ifd->count--;
      exif_release_tag(tag);
      return NULL;
    }
    if (tag->count < (index + count)) {
      if (! tag->data_is_dynamic) {
        void* tmp = xmalloc(tag->size < 4 ? 4 : tag->size);
        memcpy(tmp, tag->data, tag->size);
        tag->data = tmp;
        tag->data_is_dynamic = 1;
      }
      tag->size = size;
      tag->count = index + count;
      tag->data = xrealloc(tag->data, size < 4 ? 4 : size);
    }
  }

  switch (type) {
  case EXIF_TYPE_RAT:
  case EXIF_TYPE_SRAT: {
    double val = *(double*)data;
    uint32_t* dest = (uint32_t*) tag->data;

    if ((int)val == val) {
      dest[index * 2] = (int)val;
      dest[(index * 2) + 1] = 1;
    } else {
      int32_t Nom, Den;
      exif_dec2frac(val, &Nom, &Den);
      if ((type == EXIF_TYPE_SRAT) && (val < 0.0)) {
        Nom *= -1;
      }
      dest[index * 2] = Nom;
      dest[(index * 2) + 1] = Den;
    }
  }
  break;
  default: {
    char* dest = (char*) tag->data;
    memcpy(&dest[index * item_size], data, count * item_size);
  }
  }
  return tag;
}


static void
exif_put_double(const int ifd_nr, const int tag_id, const int index, const double val)
{
  double d = fabs(val);
  exif_put_value(ifd_nr, tag_id, EXIF_TYPE_RAT, 1, index, &d);
}


static void
exif_put_str(const int ifd_nr, const int tag_id, const char* val)
{
  int len = (val) ? strlen(val) + 1 : 0;
  exif_put_value(ifd_nr, tag_id, EXIF_TYPE_ASCII, len, 0, val);
}

static void
exif_put_coord(const int ifd_nr, const int tag_id, const double val)
{
  double  vmin, vsec;
  int     vint;

  vint = abs((int) val);
  vmin = 60.0 * (fabs(val) - vint);
  vsec = 60.0 * (vmin - floor(vmin));
  vmin = floor(vmin);

  exif_put_double(ifd_nr, tag_id, 0, (double)vint);
  exif_put_double(ifd_nr, tag_id, 1, (double)vmin);
  exif_put_double(ifd_nr, tag_id, 2, (double)vsec);
}

static void
exif_put_long(const int ifd_nr, const int tag_id, const int index, const int32_t val)
{
  exif_put_value(ifd_nr, tag_id, EXIF_TYPE_LONG, 1, index, &val);
}

static void
exif_remove_tag(const int ifd_nr, const int tag_id)
{
  exif_put_value(ifd_nr, tag_id, EXIF_TYPE_BYTE, 0, 0, NULL);
}

static void
exif_find_wpt_by_time(const Waypoint* wpt)
{
  if (!wpt->creation_time.isValid()) {
    return;
  }

  if (exif_wpt_ref == NULL) {
    exif_wpt_ref = wpt;
  } else if (abs(exif_time_ref - wpt->creation_time.toTime_t()) < abs(exif_time_ref - exif_wpt_ref->creation_time.toTime_t())) {
    exif_wpt_ref = wpt;
  }
}

static void
exif_find_wpt_by_name(const Waypoint* wpt)
{
  if (exif_wpt_ref != NULL) {
    return;
  } else if ((wpt->shortname != NULL) && (case_ignore_strcmp(wpt->shortname, opt_name) == 0)) {
    exif_wpt_ref = wpt;
  }
}


static int
exif_sort_tags_cb(const queue* A, const queue* B)
{
  exif_tag_t* ta = (exif_tag_t*)A;
  exif_tag_t* tb = (exif_tag_t*)B;

  return ta->id - tb->id;
}

static int
exif_sort_ifds_cb(const queue* A, const queue* B)
{
  exif_ifd_t* ia = (exif_ifd_t*)A;
  exif_ifd_t* ib = (exif_ifd_t*)B;

  return ia->nr - ib->nr;
}

static void
exif_write_value(exif_tag_t* tag, gbfile* fout)
{
  if (tag->size > 4) {
    gbfputuint32(tag->value, fout);  /* offset to data */
  } else {
    char* data = (char*) tag->data;

    if BYTE_TYPE(tag->type) {
      gbfwrite(data, 4, 1, fout);
    } else if WORD_TYPE(tag->type) {
      gbfputuint16(*(uint16_t*)data, fout);
      gbfputuint16(*(uint16_t*)(data+2), fout);
    } else if LONG_TYPE(tag->type) {
      gbfputuint32(*(uint32_t*)data, fout);
    } else if (tag->type == EXIF_TYPE_FLOAT) {
      gbfputflt(*(float*)data, fout);
    } else {
      fatal(MYNAME ": Unknown data type %d!\n", tag->type);
    }
  }
}

static void
exif_write_ifd(const exif_ifd_t* ifd, const char next, gbfile* fout)
{
  gbsize_t offs;
  queue* elem, *tmp;

  gbfputuint16(ifd->count, fout);
  offs = gbftell(fout) + (ifd->count * 12) + 4;

  QUEUE_FOR_EACH(&ifd->tags, elem, tmp) {
    exif_tag_t* tag = (exif_tag_t*)elem;

    gbfputuint16(tag->id, fout);
    gbfputuint16(tag->type, fout);
    gbfputuint32(tag->count, fout);
    if (tag->size > 4) {
      tag->value = offs;
      offs += tag->size;
      if (offs & 1) {
        offs++;
      }
      gbfputuint32(tag->value, fout);
    } else {
      exif_write_value(tag, fout);
    }
  }

  if (next) {
    gbfputuint32(offs, fout);
  } else {
    gbfputuint32(0, fout);
  }

  QUEUE_FOR_EACH(&ifd->tags, elem, tmp) {
    exif_tag_t* tag = (exif_tag_t*)elem;

    if (tag->size > 4) {
      uint16_t i;
      char* ptr = (char*) tag->data;

      if BYTE_TYPE(tag->type) {
        gbfwrite(tag->data, tag->size, 1, fout);
      } else for (i = 0; i < tag->count; i++) {
          switch (tag->type) {
          case EXIF_TYPE_SHORT:
          case EXIF_TYPE_SSHORT:
            gbfputuint16(*(int16_t*)ptr, fout);
            break;
          case EXIF_TYPE_LONG:
          case EXIF_TYPE_SLONG:
          case EXIF_TYPE_IFD:
            gbfputuint32(*(int32_t*)ptr, fout);
            break;
          case EXIF_TYPE_RAT:
          case EXIF_TYPE_SRAT:
            gbfputuint32(*(int32_t*)ptr, fout);
            gbfputuint32(*(int32_t*)(ptr+4), fout);
            break;
          case EXIF_TYPE_FLOAT:
            gbfputflt(*(float*)ptr, fout);
            break;
          case EXIF_TYPE_DOUBLE:
            gbfputdbl(*(double*)ptr, fout);
            break;
          default:
            gbfwrite(ptr, exif_type_size(tag->type), 1, fin);
            break;
          }
          ptr += (tag->size / tag->count);
        }
      if (gbftell(fout) & 1) {
        gbfputc(0, fout);
      }
    }
  }
}

static void
exif_write_apps(void)
{
  queue* e0, *t0;

  gbfputuint16(0xFFD8, fout);

  QUEUE_FOR_EACH(&exif_apps, e0, t0) {
    exif_app_t* app = (exif_app_t*)e0;

    gbfputuint16(app->marker, fout);

    if (app == exif_app) {
      queue* e1, *t1;
      uint16_t len = 8;
      gbfile* ftmp;
      exif_tag_t* tag;

      exif_put_long(IFD0, IFD0_TAG_GPS_IFD_OFFS, 0, 0);
      exif_put_value(GPS_IFD, GPS_IFD_TAG_VERSION, EXIF_TYPE_BYTE, 4, 0, writer_gps_tag_version);

      sortqueue(&exif_app->ifds, exif_sort_ifds_cb);

      QUEUE_FOR_EACH(&app->ifds, e1, t1) {
        exif_ifd_t* ifd = (exif_ifd_t*)e1;

        if (ifd->nr == GPS_IFD) {
          exif_put_long(IFD0, IFD0_TAG_GPS_IFD_OFFS, 0, len);
        } else if (ifd->nr == EXIF_IFD) {
          exif_put_long(IFD0, IFD0_TAG_EXIF_IFD_OFFS, 0, len);
        } else if (ifd->nr == INTER_IFD) {
          exif_put_long(EXIF_IFD, EXIF_IFD_TAG_INTER_IFD_OFFS, 0, len);
        }

        len += exif_ifd_size(ifd);
      }

      len += 4; /* DWORD(0) after last ifd */

      if ((exif_find_tag(app, IFD1, IFD1_TAG_JPEG_OFFS))) {
        exif_put_long(IFD1, IFD1_TAG_JPEG_OFFS, 0, len);
      }

      QUEUE_FOR_EACH(&app->ifds, e1, t1) {
        exif_ifd_t* ifd = (exif_ifd_t*)e1;
        sortqueue(&ifd->tags, exif_sort_tags_cb);
      }

      ftmp = gbfopen_be(NULL, "wb", MYNAME);
      ftmp->big_endian = app->fcache->big_endian;

      gbfwrite((ftmp->big_endian) ? "MM" : "II", 2, 1, ftmp);
      gbfputuint16(0x2A, ftmp);
      gbfputuint32(0x08, ftmp);	/* offset to first IFD */

      QUEUE_FOR_EACH(&app->ifds, e1, t1) {
        exif_ifd_t* ifd = (exif_ifd_t*)e1;
        exif_ifd_t* ifd_next = (exif_ifd_t*)t1;
        char next;

        if ((ifd->nr == IFD0) && (ifd_next->nr == IFD1)) {
          next = 1;
        } else {
          next = 0;
        }

        exif_write_ifd(ifd, next, ftmp);
        len = gbftell(ftmp);
      }

      gbfputuint32(0, ftmp); /* DWORD(0) after last ifd */

      if ((tag = exif_find_tag(app, IFD1, IFD1_TAG_JPEG_OFFS))) {
        gbsize_t offs = tag->origin;
        if ((tag = exif_find_tag(app, IFD1, IFD1_TAG_JPEG_SIZE))) {
          gbfseek(app->fexif, offs, SEEK_SET);
          gbfcopyfrom(ftmp, app->fexif, tag->value);
        }
      }

      len = gbftell(ftmp);
      gbfrewind(ftmp);
      gbfputuint16(len + 8, fout);
      gbfwrite("Exif\0\0", 6, 1, fout);
      gbfcopyfrom(fout, ftmp, len);

      gbfclose(ftmp);
    } else {
      gbfputuint16(app->len, fout);
      gbfrewind(app->fcache);
      gbfcopyfrom(fout, app->fcache, 0x7FFFFFFF);
    }
  }
}

/*******************************************************************************
* %%%        global callbacks called by gpsbabel main process              %%% *
*******************************************************************************/

static void
exif_rd_init(const char* fname)
{
  fin = gbfopen_be(fname, "rb", MYNAME);
  QUEUE_INIT(&exif_apps);
}

static void
exif_rd_deinit(void)
{
  exif_release_apps();
  gbfclose(fin);
}

static void
exif_read(void)
{
  uint16_t soi;
  Waypoint* wpt;

  soi = gbfgetuint16(fin);
  is_fatal(soi != 0xFFD8, MYNAME ": Unknown image file.");	/* only jpeg for now */

  exif_app = exif_load_apps();
  is_fatal(exif_app == NULL, MYNAME ": No EXIF header in source file \"%s\".", fin->name);

  exif_examine_app(exif_app);
  wpt = exif_waypt_from_exif_app(exif_app);
  if (wpt) {
    waypt_add(wpt);
  }
}

static void
exif_wr_init(const char* fname)
{
  uint16_t soi;
  char* tmpname;

  exif_success = 0;
  exif_fout_name = xstrdup(fname);

  QUEUE_INIT(&exif_apps);

  fin = gbfopen_be(fname, "rb", MYNAME);
  is_fatal(fin->is_pipe, MYNAME ": Sorry, this format cannot be used with pipes!");

  soi = gbfgetuint16(fin);
  is_fatal(soi != 0xFFD8, MYNAME ": Unknown image file.");
  exif_app = exif_load_apps();
  is_fatal(exif_app == NULL, MYNAME ": No EXIF header found in source file \"%s\".", fin->name);
  exif_examine_app(exif_app);
  gbfclose(fin);

  exif_time_ref = exif_get_exif_time(exif_app);
  if (exif_time_ref == 0) {
    fatal(MYNAME ": No valid timestamp found in picture!\n");
  }
abort();
  xasprintf(&tmpname, "%s.jpg", fname);
  fout = gbfopen_be(tmpname, "wb", MYNAME);
  xfree(tmpname);
}

static void
exif_wr_deinit(void)
{
  char* tmpname;

  exif_release_apps();
  tmpname = xstrdup(fout->name);
  gbfclose(fout);

  if (exif_success) {
    if (*opt_overwrite == '1') {
      remove(exif_fout_name);
      rename(tmpname, exif_fout_name);
    }
  } else {
    remove(tmpname);
  }

  xfree(exif_fout_name);
  xfree(tmpname);
}

static void
exif_write(void)
{
  time_t frame;

  exif_wpt_ref = NULL;

  if (opt_name) {
    waypt_disp_all(exif_find_wpt_by_name);
    if (exif_wpt_ref == NULL) {
      route_disp_all(NULL, NULL, exif_find_wpt_by_name);
    }
    if (exif_wpt_ref == NULL) {
      track_disp_all(NULL, NULL, exif_find_wpt_by_name);
    }
    if (exif_wpt_ref == NULL) {
      warning(MYNAME ": No matching point with name \"%s\" found.\n", opt_name);
    }
  } else {
    QString str = exif_time_str(exif_time_ref);

    track_disp_all(NULL, NULL, exif_find_wpt_by_time);
    route_disp_all(NULL, NULL, exif_find_wpt_by_time);
    waypt_disp_all(exif_find_wpt_by_time);

    frame = atoi(opt_frame);

    if (exif_wpt_ref == NULL) {
      warning(MYNAME ": No point with a valid timestamp found.\n");
    } else if (abs(exif_time_ref - exif_wpt_ref->creation_time.toTime_t()) > frame) {
      warning(MYNAME ": No matching point found for image date %s!\n", CSTR(str));
      if (exif_wpt_ref != NULL) {
        QString str = exif_time_str(exif_wpt_ref->creation_time.toTime_t());
        warning(MYNAME ": Best is from %s, %d second(s) away.\n",
                CSTR(str), abs(exif_time_ref - exif_wpt_ref->creation_time.toTime_t()));
      }
      exif_wpt_ref = NULL;
    }
  }

  if (exif_wpt_ref != NULL) {
    const Waypoint* wpt = exif_wpt_ref;

    exif_put_long(IFD0, IFD0_TAG_GPS_IFD_OFFS, 0, 0);
    exif_put_value(GPS_IFD, GPS_IFD_TAG_VERSION, EXIF_TYPE_BYTE, 4, 0, writer_gps_tag_version);
    exif_put_str(GPS_IFD, GPS_IFD_TAG_DATUM, "WGS-84");

    exif_put_str(GPS_IFD, GPS_IFD_TAG_LATREF, wpt->latitude < 0 ? "S" : "N");
    exif_put_coord(GPS_IFD, GPS_IFD_TAG_LAT, fabs(wpt->latitude));
    exif_put_str(GPS_IFD, GPS_IFD_TAG_LONREF, wpt->longitude < 0 ? "W" : "E");
    exif_put_coord(GPS_IFD, GPS_IFD_TAG_LON, fabs(wpt->longitude));

    if (wpt->altitude == unknown_alt) {
      exif_remove_tag(GPS_IFD, GPS_IFD_TAG_ALT);
      exif_remove_tag(GPS_IFD, GPS_IFD_TAG_ALTREF);
    } else {
      uint8_t alt_ref;
      if (wpt->altitude >= 0.0) {
        alt_ref = 0;
      } else {
        alt_ref = 1;
      }
      exif_put_value(GPS_IFD, GPS_IFD_TAG_ALTREF, EXIF_TYPE_BYTE, 1, 0, &alt_ref);
      exif_put_double(GPS_IFD, GPS_IFD_TAG_ALT, 0, wpt->altitude);
    }

    if (wpt->creation_time.isValid()) {
      struct tm tm;
      char buf[32];

      const time_t tt = wpt->GetCreationTime().toTime_t();
      tm = *gmtime(&tt);

      tm.tm_year += 1900;
      tm.tm_mon += 1;

      exif_put_double(GPS_IFD, GPS_IFD_TAG_TIMESTAMP, 0, tm.tm_hour);
      exif_put_double(GPS_IFD, GPS_IFD_TAG_TIMESTAMP, 1, tm.tm_min);
      exif_put_double(GPS_IFD, GPS_IFD_TAG_TIMESTAMP, 2, tm.tm_sec);

      snprintf(buf, sizeof(buf), "%04d:%02d:%02d", tm.tm_year, tm.tm_mon, tm.tm_mday);
      exif_put_str(GPS_IFD, GPS_IFD_TAG_DATESTAMP, buf);
    } else {
      exif_remove_tag(GPS_IFD, GPS_IFD_TAG_TIMESTAMP);
      exif_remove_tag(GPS_IFD, GPS_IFD_TAG_DATESTAMP);
    }

    if (wpt->sat > 0) {
      char buf[16];
      snprintf(buf, sizeof(buf), "%d", wpt->sat);
      exif_put_str(GPS_IFD, GPS_IFD_TAG_SAT, buf);
    } else {
      exif_remove_tag(GPS_IFD, GPS_IFD_TAG_SAT);
    }

    if (wpt->fix == fix_2d) {
      exif_put_str(GPS_IFD, GPS_IFD_TAG_MODE, "2");
    } else if (wpt->fix == fix_3d) {
      exif_put_str(GPS_IFD, GPS_IFD_TAG_MODE, "3");
    } else {
      exif_remove_tag(GPS_IFD, GPS_IFD_TAG_MODE);
    }

    if (wpt->hdop > 0) {
      exif_put_double(GPS_IFD, GPS_IFD_TAG_DOP, 0, wpt->hdop);
    } else {
      exif_remove_tag(GPS_IFD, GPS_IFD_TAG_DOP);
    }

    if WAYPT_HAS(wpt, speed) {
      exif_put_str(GPS_IFD, GPS_IFD_TAG_SPEEDREF, "K");
      exif_put_double(GPS_IFD, GPS_IFD_TAG_SPEED, 0, MPS_TO_KPH(wpt->speed));
    } else {
      exif_remove_tag(GPS_IFD, GPS_IFD_TAG_SPEEDREF);
      exif_remove_tag(GPS_IFD, GPS_IFD_TAG_SPEED);
    }

    exif_write_apps();	/* Success, write the new file */

    exif_success = 1;
  }

}

/**************************************************************************/

ff_vecs_t exif_vecs = {
  ff_type_file,
  {
    (ff_cap)(ff_cap_read | ff_cap_write)	/* waypoints */,
    ff_cap_none 			/* tracks */,
    ff_cap_none 			/* routes */
  },
  exif_rd_init,
  exif_wr_init,
  exif_rd_deinit,
  exif_wr_deinit,
  exif_read,
  exif_write,
  NULL,
  exif_args,
  CET_CHARSET_UTF8, 0
};

/**************************************************************************/
