/*
    IGO8 Track Format

    Copyright (C) 2008 Dustin Johnson, Dustin@Dustinj.us

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

	July 26, 2008 - Dustin: Added tracknum, title, and description options
	July 26, 2008 - Dustin: Validated the new code for char to Unicode conversion
 */


/*
    iGo8 (*.trk) Format Overview

  |------------------------------| <--\
  |        ID Block (20B)        |    |
  |------------------------------|    |
  |                              |    |
  |                              |
  |                              |    H
  |                              |    e
  |                              |    a
  |    Description Block (256B)  |    d
  |                              |    e
  |                              |    r
  |                              |
  |                              |    |
  |                              |    |
  |                              |    |
  |------------------------------| <--/
  |    Information Block (12B)   |
  |------------------------------|
  |          Waypoint 1          |
  |------------------------------|
  |          Waypoint 2          |
  |------------------------------|
  |          Waypoint 3          |
  |------------------------------|
  |             ...              |
  |------------------------------|

  ID Block: defined by igo8_id_block
  Description Block: Two null-terminated unicode 2 strings.
                     The first is the title of the track,
					 the second is the description.
  Information Block: defined by igo8_information_block
  Waypoint: defined by igo8_point

*/

#include <ctype.h>
#include <time.h>
#include "defs.h"
#include "cet.h"
#include "cet_util.h"

#define FLOAT_TO_INT(x) ((int)((x) + ((x)<0?-0.5:0.5)))
#define IGO8_HEADER_SIZE (sizeof(igo8_id_block) + 256)
#define MYNAME "IGO8"

typedef struct _igo8_id_block {
  uint32_t unknown_1;
  uint32_t unknown_2;
  uint32_t unknown_3;
  uint32_t track_number;
  uint32_t unknown_4;
} igo8_id_block, *p_igo8_id_block;

typedef struct _igo8_information_block {
  uint32_t start_time;       // In Unix time
  uint32_t zero;             // Doesn't appear to serve a purpose
  uint32_t total_file_size;  // In bytes
} igo8_information_block, *p_igo8_information_block;

typedef struct _igo8_point {
  uint32_t unix_time;
  uint32_t lon;
  uint32_t lat;
} igo8_point, *p_igo8_point;

// Files
static gbfile* igo8_file_in;
static gbfile* igo8_file_out;

// Options
static char* igo8_option_tracknum = NULL;
static char* igo8_option_title = NULL;
static char* igo8_option_description = NULL;

// Internal state
static uint32_t invented_time;
static uint32_t point_count;
static int in_point_count;

// Exported options list
static arglist_t igo8_options[] = {
  { "tracknum", &igo8_option_tracknum, "Track identification number", NULL, ARGTYPE_INT, ARG_NOMINMAX },
  { "title", &igo8_option_title, "Track title", NULL, ARGTYPE_STRING, ARG_NOMINMAX },
  { "description", &igo8_option_description, "Track description", NULL, ARGTYPE_STRING, ARG_NOMINMAX },
  ARG_TERMINATOR
};

// Sanity check
static void igo8_check_type_sizes()
{
  if (sizeof(igo8_point) != 12) {
    fatal(MYNAME ": igo8_point is %ld bytes instead of the required 12.\n",
          (long) sizeof(igo8_point));
  }

  if (sizeof(igo8_information_block) != 12) {
    fatal(MYNAME ": igo8_information_block is %ld bytes instead of the required 12.\n",
          (long) sizeof(igo8_information_block));
  }

  if (sizeof(igo8_id_block) != 20) {
    fatal(MYNAME ": igo8_id_block is %ld bytes instead of the required 20.\n",
          (long) sizeof(igo8_id_block));
  }
}

// Reader initialization callback
static void igo8_read_init(const char* fname)
{
  igo8_file_in = gbfopen_le(fname, "rb", MYNAME);

  // Make sure that we are in the environment we expect and require
  igo8_check_type_sizes();

  // Seek past the header and most of the Information Block.  Read
  // the last word for trackpoint count since latest igo8 seems to
  // zero-pad the files.
  gbfseek(igo8_file_in, IGO8_HEADER_SIZE + sizeof(igo8_information_block) - 4, SEEK_SET);
  in_point_count = (gbfgetint32(igo8_file_in) - IGO8_HEADER_SIZE -
                    sizeof(igo8_information_block)) / sizeof(igo8_point);
}

// Reader callback
static void igo8_read(void)
{
  waypoint* wpt_tmp;
  route_head* track_head;
  igo8_point point;

  track_head = route_head_alloc();
  track_add_head(track_head);

  while (in_point_count &&
         gbfread(&point, sizeof(point), 1, igo8_file_in) > 0) {
    in_point_count--;
    wpt_tmp = waypt_new();

    wpt_tmp->latitude = le_read32(&point.lat) / (double)0x800000;
    wpt_tmp->longitude = le_read32(&point.lon) / (double)0x800000;
    wpt_tmp->creation_time = le_read32(&point.unix_time);

    track_add_wpt(track_head, wpt_tmp);
  }
}

// Reader close callback
static void igo8_read_deinit(void)
{
  gbfclose(igo8_file_in);
}

// Writer initialize callback
static void igo8_write_init(const char* fname)
{
  igo8_file_out = gbfopen_le(fname, "wb", MYNAME);

  igo8_check_type_sizes();

  invented_time = 1;
  point_count = 0;
}

// Writer close callback
static void igo8_write_deinit(void)
{
  uint32_t normalized_file_size;

  // Seek to the start of the third long in the Information Block, this is
  // where we will write out the total size of the file.
  gbfseek(igo8_file_out, IGO8_HEADER_SIZE + sizeof(uint32_t)*2, SEEK_SET);

  // The total size of the file is the number of points written + Information block + Header
  le_write32(&normalized_file_size, sizeof(igo8_point)*(point_count) + sizeof(igo8_information_block) + IGO8_HEADER_SIZE);

  // Write the size
  gbfwrite(&normalized_file_size, sizeof(normalized_file_size), 1, igo8_file_out);

  gbfclose(igo8_file_out);
}

// Write point callback
static void write_igo8_track_point(const waypoint* wpt)
{
  igo8_point point;

  memset(&point, 0, sizeof(point));

  // iGo8 appears to expect a time, if one isn't provided
  // then we shall make our own, where each point is one
  // second apart.
  if (wpt->creation_time.isValid()) {
    le_write32(&point.unix_time, wpt->GetCreationTime().toTime_t());
  } else {
    le_write32(&point.unix_time, invented_time++);
  }

  // Write the first part of the Information Block, the start time
  if (point_count == 0) {
    gbfwrite(&point, sizeof(point), 1, igo8_file_out);
  }

  le_write32(&point.lon, FLOAT_TO_INT(wpt->longitude * 0x800000));
  le_write32(&point.lat, FLOAT_TO_INT(wpt->latitude * 0x800000));

  gbfwrite(&point, sizeof(point), 1, igo8_file_out);

  // Count the number of point printed, we will use this at the end to
  // finish filling out the Information Block.
  point_count++;
}

// Write src unicode str to the dst cstring using unicode characters
// All lengths are in bytes
unsigned int print_unicode(char* dst, const unsigned int dst_max_length, short* src, unsigned int src_len)
{
  // Check to see what length we were passed, if the length doesn't include the null
  // then we make it include the null
  if (src[(src_len/2) - 1] != 0) {
    // If the last character isn't null check the next one
    if (src[(src_len/2)] != 0) {
      // If the next character also inst' null, make it null
      src[(src_len/2)] = 0;
    } else {
      // The next character is null, adjust the total length of the str to account for this
      src_len += 2;
    }
  }

  // Make sure we fit in our dst size
  if (src_len > dst_max_length) {
    src_len = dst_max_length;
    src[(src_len/2) - 1] = 0; // Make sure we keep that terminating null around
  }

  // Copy the str
  memcpy(dst, src, src_len);

  return src_len;
}

// This is a sort of hacked together ascii-> unicode 2 converter.  I have no idea
// if iGo8 even supports real unicode 2, but is does look like it as every ascii
// character is a short with the ascii character as the least significant 7 bits
//
// Please replace this with a much more filled out and correct version if you see
// fit.

/* 2008/06/24, O.K.: Use CET library for ascii-> unicode 2 converter */
// 2008/07/25, Dustin: Slight fix to make sure that we always null terminate the
//                     string, validate that the use of the CET library provides
//                     conmforming output, remove my old junk converter code.

unsigned int ascii_to_unicode_2(char* dst, const unsigned int dst_max_length, const char* src)
{
  short* unicode;
  int len;

  unicode = cet_str_any_to_uni(src, &cet_cs_vec_ansi_x3_4_1968, &len);

  len *= 2;	/* real size in bytes */
  len = print_unicode(dst, dst_max_length, unicode, len);

  xfree(unicode);

  return len;
}

void write_header()
{
  char header[IGO8_HEADER_SIZE] = {'\0'};
  igo8_id_block tmp_id_block;
  p_igo8_id_block id_block = (p_igo8_id_block)header;
  uint32_t current_position = 0;
  const char* title = "Title";
  const char* description = "Description";

  // These values seem to be constant for me, but I have no idea what they are.
  tmp_id_block.unknown_1 = 0x0000029B;
  tmp_id_block.unknown_2 = 0x000003E7;
  tmp_id_block.unknown_3 = 0x00000003;

  // This appears to be a unique number that IDs the track.
  // It is mono-incrementing and offset by 2 above the track number.
  // e.g. "Track 1" --> track_number = 3
  // XXX - Dustin: My guess is that this number is used as the key for the track color, if
  // XXX - Dustin: multiple tracks have the same color they will be colored the same, just
  // XXX - Dustin: a guess though.
  if (igo8_option_tracknum) {
    tmp_id_block.track_number = atoi(igo8_option_tracknum);
  } else {
    tmp_id_block.track_number = 0x00000010;
  }
  tmp_id_block.unknown_4 = 0x00000001;

  // Byte swap out to the header buffer.
  le_write32(&id_block->unknown_1, tmp_id_block.unknown_1);
  le_write32(&id_block->unknown_2, tmp_id_block.unknown_2);
  le_write32(&id_block->unknown_3, tmp_id_block.unknown_3);
  le_write32(&id_block->track_number, tmp_id_block.track_number);
  le_write32(&id_block->unknown_4, tmp_id_block.unknown_4);

  // Move past the ID block, we have just filled it.
  current_position += sizeof(*id_block);

  // Set the title of the track
  // Note: we shorten the length of the potential title by 2 because we need to leave at
  //       least enough room to have a null for the description string that follows it.
  if (igo8_option_title) {
    title = igo8_option_title;
  }
  current_position += ascii_to_unicode_2((char*)(header+current_position), IGO8_HEADER_SIZE - current_position - 2, title);

  // Set the description of the track
  if (igo8_option_description) {
    description = igo8_option_description;
  }
  current_position += ascii_to_unicode_2((char*)(header+current_position), IGO8_HEADER_SIZE - current_position, description);

  gbfwrite(&header, IGO8_HEADER_SIZE, 1, igo8_file_out);
}

// Writer callback
static void igo8_write(void)
{
  write_header();
  track_disp_all(NULL, NULL, write_igo8_track_point);
}

// Callback definitions
ff_vecs_t igo8_vecs = {
  ff_type_file,
  { ff_cap_none, (ff_cap)(ff_cap_read | ff_cap_write), ff_cap_none },
  igo8_read_init,
  igo8_write_init,
  igo8_read_deinit,
  igo8_write_deinit,
  igo8_read,
  igo8_write,
  NULL,
  igo8_options,
  CET_CHARSET_UTF8,
  1
};
