/*
    Read and write TomTom .ov2 files.

    Copyright (C) 2005  Ronald Parker (babeltomtom@parkrrrr.com) and
                        Robert Lipe (robertlipe+source@gpsbabel.org)

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

 */


/*
    This module is based on my reverse-engineering of the .ov2 format, so
    it might not be aware of all record types.  In particular, I've seen
    a type-3 record that may contain additional strings, but since I haven't
    seen any of those from a legitimate source, I don't know what they are
    supposed to contain.  Thus, they are not currently supported.  (The one
    I saw was due to an errant pair of double-quotes in the input to
    makeov2.exe.)   -- Ron Parker, 28 April 2005

    Because they've been seen in the wild, I have updated the reader to
    deal with type 3 as if they were type 2.  I still haven't seen any
    records that fill in the other two strings, so until I know for sure
    that they are indeed strings, I'm just putting them on the end of the
    description string beyond the NUL terminator.  -- Ron Parker, 17 July 2006
*/

#include <cstdio>           // for printf, snprintf, SEEK_CUR, EOF
#include <cstdlib>          // for qsort
#include <cstring>          // for strlen

#include <QtCore/QString>   // for QString
#include <QtCore/QtGlobal>  // for foreach

#include "defs.h"
#include "gbfile.h"         // for gbfgetint32, gbfputint32, gbfclose, gbfgetc, gbfputc, gbfseek, gbfile, gbfeof, gbfread, gbftell, gbfwrite, gbfopen_le


#define MYNAME "TomTom"

static gbfile* file_in;
static gbfile* file_out;

static
QVector<arglist_t> tomtom_args = {
};

static void
rd_init(const QString& fname)
{
  file_in = gbfopen_le(fname, "rb", MYNAME);
}

static void
rd_deinit()
{
  gbfclose(file_in);
}

static void
wr_init(const QString& fname)
{
  file_out = gbfopen_le(fname, "wb", MYNAME);
}

static void
wr_deinit()
{
  gbfclose(file_out);
}

#define read_long(f) gbfgetint32((f))
#define read_char(f) gbfgetc((f))

#ifdef DEAD_CODE_IS_REBORN
/*
 *  Decode a type 8 compressed record
 */
static char*
decode_8(int sz, const unsigned char* inbuf)
{
  static const char encoding_8[] = "X. SaerionstldchumgpbkfzvACBMPG-";
  static const int encoding_8_high[8] = {0x2,0x3,0x4,0x5,0x6,0x7,0xe,0xf};

  // Maximally sized for laziness.
  char* rval = (char*) xmalloc(sz * 3 + 1);
  char* out = rval;

  int i;
  for (i = 0; i < sz;) {
    if (inbuf[0] & 0x80) {
      int idx;
      int res;
      idx = (inbuf[0] & 0x70) >> 4;
      res = inbuf[0] & 0x0f;
      res |= encoding_8_high[idx] << 4;

      *out++ = res;

      inbuf++;
      i++;
    } else {
      int c1 = (inbuf[0] & 0x7c) >> 2;
      int c2 = ((inbuf[0] & 3) << 3) | (inbuf[1] & 0xe0) >> 5;
      int c3 = inbuf[1] &  0x1f;
      if ((c1 | c2 | c3) > 0x1f) {
        fatal("bit unpacking error");
      }
      *out++ = encoding_8[c1];
      *out++ = encoding_8[c2];
      *out++ = encoding_8[c3];
      inbuf+=2;
      i+=2;
    }
  }
  return rval;
}
#endif

#ifdef DEAD_CODE_IS_REBORN
static void
decode_latlon(double* lat, double* lon)
{
  unsigned char latbuf[3];
  unsigned char lonbuf[3];
  double rlat, rlon;

  gbfread(&lonbuf, 3, 1, file_in);
  gbfread(&latbuf, 3, 1, file_in);
  rlat = ((latbuf[2] << 16) + (latbuf[1] << 8) + latbuf[0]) / 1000000.0;

  *lat = 80 - rlat;
  *lon = rlon = 123.456;

}
#endif

static void
check_recsize(int sz)
{
  if ((sz > 100000) || (sz < 0)) {
    fatal(MYNAME ":malformed file.  Bad record size.");
  }
}

static void
data_read()
{
  long recsize;
  long x;
  long y;
  char* desc;
  Waypoint* wpt_tmp;
  while (!gbfeof(file_in)) {
    int rectype = read_char(file_in);
    if (rectype == EOF) {
      fatal(MYNAME ":Unexpected EOF.");
    }
    if (global_opts.debug_level >= 5) {
      printf("Reading record type %d\n", rectype);
    }
    switch (rectype) {
    case 0:
    case 100:
      if (global_opts.debug_level >= 5) {
        printf("Skipping deleted record\n");
      }
      recsize = read_long(file_in) - 5;
      check_recsize(recsize);
      if (global_opts.debug_level >= 5) {
        printf("Skipping %li bytes\n", recsize);
      }
      gbfseek(file_in, recsize, SEEK_CUR);
      break;
    case 1:
      /* a block header; ignored on read */
      read_long(file_in);
      read_long(file_in);
      read_long(file_in);
      read_long(file_in);
      read_long(file_in);
      break;
    case 2:
    case 3:
      recsize = read_long(file_in);
      check_recsize(recsize);
      x = read_long(file_in);
      y = read_long(file_in);
      desc = (char*)xmalloc(recsize - 13);
      gbfread(desc, recsize-13, 1, file_in);

      wpt_tmp = new Waypoint;

      wpt_tmp->longitude = x/100000.0;
      wpt_tmp->latitude = y/100000.0;
      wpt_tmp->description = STRTOUNICODE(desc);
      xfree(desc);
      desc = nullptr;
      // TODO:: description in rectype 3 contains two zero-terminated strings
      // First is same as rectype 2, second apparently contains the unique ID of the waypoint
      // See http://www.tomtom.com/lib/doc/PRO/TTN6_SDK_documentation.zip
      if (rectype == 3) {
        warning("Unexpected waypoint record type %d encountered.\nThe unique ID of the POI may have been dropped.\n", rectype);
      }

      waypt_add(wpt_tmp);
      break;
    case 8:
    case 24:
#if 0 // Fallthrough for now to silently ignore these until this is done.
      recsize = read_char(file_in) ;
      check_recsize(recsize);
      wpt_tmp = new Waypoint;
      decode_latlon(&wpt_tmp->latitude, &wpt_tmp->longitude);
      gbfread(tbuf, 3, 1, file_in);
      gbfread(tbuf, 3, 1, file_in);
      gbfread(tbuf, recsize, 1, file_in);
      wpt_tmp->shortname = decode_8(recsize, tbuf);
      waypt_add(wpt_tmp);
      break;
#else
#endif
    case 9:
    case 25:
      recsize = read_char(file_in) + 6;
      check_recsize(recsize);
      if (global_opts.debug_level >= 5)
        warning("Unknown record type 0x%x; skipping %ld bytes.\n",
                rectype, recsize);
      gbfseek(file_in, recsize, SEEK_CUR);
      break;
    default:
      if (global_opts.debug_level >= 1) {
        warning("Unexpected waypoint record type: %d at offset 0x%x\n", rectype, gbftell(file_in));
      }
    }
  }
}


struct hdr {
  const Waypoint* wpt;
};

static int compare_lon(const void* a, const void* b);

static
int
compare_lat(const void* a, const void* b)
{
  const auto* wa = (const struct hdr*) a;
  const auto* wb = (const struct hdr*) b;

  double difference = wa->wpt->latitude - wb->wpt->latitude;
  if (difference < 0) {
    return -1;
  }
  if (difference) {
    return 1;
  }
  if (wa->wpt->longitude - wb->wpt->longitude == 0) {
    return wa->wpt->shortname.compare(wb->wpt->shortname);
  }
  return compare_lon(a,b);
}

static
int
compare_lon(const void* a, const void* b)
{
  const auto* wa = (const struct hdr*)a;
  const auto* wb = (const struct hdr*)b;

  double difference = wa->wpt->longitude - wb->wpt->longitude;
  if (difference < 0) {
    return -1;
  }
  if (difference) {
    return 1;
  }
  if (wa->wpt->latitude - wb->wpt->latitude == 0) {
    return wa->wpt->shortname.compare(wb->wpt->shortname);
  }
  return compare_lat(a,b);
}

#define write_long(f,v) gbfputint32((v),f)

static void
write_float_as_long(gbfile* file, double value)
{
  long tmp = (value + 0.500000000001);
  write_long(file, tmp);
}

#define write_char(f,c) gbfputc((c),f)
#define write_string(f,s) gbfwrite((s),1,strlen(s)+1,f)

struct blockheader {
  struct hdr* start;
  long count;
  long size;
  double minlat;
  double maxlat;
  double minlon;
  double maxlon;
  struct blockheader* ch1;
  struct blockheader* ch2;
};

static void
write_blocks(gbfile* f, struct blockheader* blocks)
{
  write_char(f, 1);
  write_long(f, blocks->size);
  write_float_as_long(f, blocks->maxlon*100000);
  write_float_as_long(f, blocks->maxlat*100000);
  write_float_as_long(f, blocks->minlon*100000);
  write_float_as_long(f, blocks->minlat*100000);
  if (blocks->ch1) {
    write_blocks(f, blocks->ch1);
  }
  if (blocks->ch2) {
    write_blocks(f, blocks->ch2);
  }
  if (!blocks->ch1 && !blocks->ch2) {
    for (int i = 0; i < blocks->count; i++) {
      char desc_field [256];
      write_char(f, 2);
      if (global_opts.smart_names &&
          blocks->start[i].wpt->gc_data->diff &&
          blocks->start[i].wpt->gc_data->terr) {
        snprintf(desc_field,sizeof(desc_field),"%s(t%ud%u)%s(type%dcont%d)",STRFROMUNICODE(blocks->start[i].wpt->description),
                 blocks->start[i].wpt->gc_data->terr/10,
                 blocks->start[i].wpt->gc_data->diff/10,
                 STRFROMUNICODE(blocks->start[i].wpt->shortname),
                 (int) blocks->start[i].wpt->gc_data->type,
                 (int) blocks->start[i].wpt->gc_data->container);
        //Unfortunately enums mean we get numbers for cache type and container.
      } else {
        snprintf(desc_field, sizeof(desc_field), "%s",
                 STRFROMUNICODE(blocks->start[i].wpt->description));
      }
      write_long(f, strlen(desc_field) + 14);
      write_float_as_long(f, blocks->start[i].wpt->longitude*100000);
      write_float_as_long(f, blocks->start[i].wpt->latitude*100000);
      write_string(f, desc_field);
    }
  }
}

static struct blockheader*
compute_blocks(struct hdr* start, int count,
               double minlon, double maxlon, double minlat, double maxlat)
{
  auto* newblock = (struct blockheader*)xcalloc(1, sizeof(struct blockheader));
  newblock->start = start;
  newblock->count = count;
  newblock->minlon = minlon;
  newblock->maxlon = maxlon;
  newblock->minlat = minlat;
  newblock->maxlat = maxlat;
  newblock->size = 4 * 5 + 1;   /* hdr is 5 longs, 1 char */
  if (count < 20) {
    for (int i = 0; i < count; i++) {
      newblock->size += 4 * 3 + 1;
      /* wpt const part 3 longs, 1 char */
      const Waypoint* wpt = start[i].wpt;
      newblock->size += wpt->description.length() + 1;
    }
  } else {
    if ((maxlat-minlat)>(maxlon-minlon)) {
      /* split along lats */
      qsort(start, count, sizeof(*start), compare_lat);
      newblock->ch1 = compute_blocks(start, count/2,
                                     minlon, maxlon, minlat,
                                     start[count/2].wpt->latitude);
      newblock->ch2 = compute_blocks(start+count/2,
                                     count-count/2, minlon, maxlon,
                                     start[count/2].wpt->latitude, maxlat);
    } else {
      /* split along lons */
      qsort(start, count, sizeof(*start), compare_lon);
      newblock->ch1 = compute_blocks(start, count/2,
                                     minlon, start[count/2].wpt->longitude,
                                     minlat, maxlat);
      newblock->ch2 = compute_blocks(start+count/2,
                                     count-count/2, start[count/2].wpt->longitude,
                                     maxlon, minlat, maxlat);
    }
    if (newblock->ch1) {
      newblock->size += newblock->ch1->size;
    }
    if (newblock->ch2) {
      newblock->size += newblock->ch2->size;
    }
  }
  return newblock;
}

static void
free_blocks(struct blockheader* block)
{
  if (block->ch1) {
    free_blocks(block->ch1);
  }
  if (block->ch2) {
    free_blocks(block->ch2);
  }
  xfree(block);
}

static void
data_write()
{
  int ct = waypt_count();
  struct hdr* htable, *bh;
  extern WaypointList* global_waypoint_list;
  double minlon = 200;
  double maxlon = -200;
  double minlat = 200;
  double maxlat = -200;
  struct blockheader* blocks = nullptr;

  htable = (struct hdr*) xmalloc(ct * sizeof(*htable));
  bh = htable;

  // Iterate with waypt_disp_all?
  for (const Waypoint* waypointp : qAsConst(*global_waypoint_list)) {
    bh->wpt = waypointp;
    if (waypointp->longitude > maxlon) {
      maxlon = waypointp->longitude;
    }
    if (waypointp->longitude < minlon) {
      minlon = waypointp->longitude;
    }
    if (waypointp->latitude > maxlat) {
      maxlat = waypointp->latitude;
    }
    if (waypointp->latitude < minlat) {
      minlat = waypointp->latitude;
    }
    bh ++;
  }

  blocks = compute_blocks(htable, ct, minlon, maxlon, minlat, maxlat);
  write_blocks(file_out, blocks);
  free_blocks(blocks);

  xfree(htable);
}

ff_vecs_t tomtom_vecs = {
  ff_type_file,
  FF_CAP_RW_WPT,
  rd_init,
  wr_init,
  rd_deinit,
  wr_deinit,
  data_read,
  data_write,
  nullptr,
  &tomtom_args,
  CET_CHARSET_MS_ANSI, 0	/* CET-REVIEW */
  , NULL_POS_OPS,
  nullptr
};
