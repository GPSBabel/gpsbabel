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

#define TRUE 1
#define FALSE 0
#define FLOAT_TO_INT(x) ((int)((x) + ((x)<0?-0.5:0.5)))
#define IGO8_HEADER_SIZE (sizeof(igo8_id_block) + 256)
#define MYNAME "IGO8"

typedef struct _igo8_id_block 
{
	gbuint32 unknown_1;
	gbuint32 unknown_2;
	gbuint32 unknown_3;
	gbuint32 track_number;
	gbuint32 unknown_4;
} igo8_id_block, *p_igo8_id_block;

typedef struct _igo8_information_block
{
	gbuint32 start_time;       // In Unix time
	gbuint32 zero;             // Doesn't appear to serve a purpose
	gbuint32 total_file_size;  // In bytes
} igo8_information_block, *p_igo8_information_block;

typedef struct _igo8_point 
{
	gbuint32 unix_time;
	gbuint32 lon;
	gbuint32 lat;
} igo8_point, *p_igo8_point;

static gbfile *igo8_file_in;
static gbfile *igo8_file_out;
gbuint32 invented_time;
gbuint32 point_count;

// Sanity check
static void igo8_check_type_sizes()
{
	if (sizeof(igo8_point) != 12)
	{
		fatal(MYNAME ": igo8_point is %ld bytes instead of the required 12.\n",
			sizeof(igo8_point));
	}

	if (sizeof(igo8_information_block) != 12)
	{
		fatal(MYNAME ": igo8_information_block is %ld bytes instead of the required 12.\n",
			sizeof(igo8_information_block));
	}

	if (sizeof(igo8_id_block) != 20)
	{
		fatal(MYNAME ": igo8_id_block is %ld bytes instead of the required 20.\n",
			sizeof(igo8_id_block));
	}
}

// Reader initialization callback
static void igo8_read_init(const char *fname)
{
	igo8_file_in = gbfopen_le(fname, "rb", MYNAME);
	
	// Make sure that we are in the environment we expect and require
	igo8_check_type_sizes();

	// Seek past the header and the Information Block
	gbfseek(igo8_file_in, IGO8_HEADER_SIZE + sizeof(igo8_information_block), SEEK_SET);
}

// Reader callback
static void igo8_read(void)
{
	waypoint *wpt_tmp;
	route_head *track_head;
	igo8_point point;

	track_head = route_head_alloc();
	track_add_head(track_head);

	while (gbfread(&point, sizeof(point), 1, igo8_file_in) > 0) {
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
static void igo8_write_init(const char *fname)
{
	igo8_file_out = gbfopen_le(fname, "wb", MYNAME);

	igo8_check_type_sizes();

	invented_time = 1;
	point_count = 0;
}

// Writer close callback
static void igo8_write_deinit(void)
{
	unsigned long normalized_file_size;

	// Seek to the start of the third long in the Information Block, this is
	// where we will write out the total size of the file.
	gbfseek(igo8_file_out, IGO8_HEADER_SIZE + sizeof(gbuint32)*2, SEEK_SET);

	// The total size of the file is the number of points written + Information block + Header
	le_write32(&normalized_file_size, sizeof(igo8_point)*(point_count) + sizeof(igo8_information_block) + IGO8_HEADER_SIZE);

	// Write the size
	gbfwrite(&normalized_file_size, sizeof(normalized_file_size), 1, igo8_file_out);

	gbfclose(igo8_file_out);
}

// Write point callback
static void write_igo8_track_point(const waypoint *wpt)
{
	igo8_point point;

	memset(&point, 0, sizeof(point));

	// iGo8 appears to expect a time, if one isn't provided
	// then we shall make our own, where each point is one
	// second apart.
	if (wpt->creation_time == 0)
	{
		le_write32(&point.unix_time, invented_time++);
	}
	else
	{
		le_write32(&point.unix_time, wpt->creation_time);
	}

	// Write the first part of the Information Block, the start time
	if (point_count == 0)
	{
		gbfwrite(&point, sizeof(point), 1, igo8_file_out);
	}

	le_write32(&point.lon, FLOAT_TO_INT(wpt->longitude * 0x800000));
	le_write32(&point.lat, FLOAT_TO_INT(wpt->latitude * 0x800000));

	gbfwrite(&point, sizeof(point), 1, igo8_file_out);

	// Count the number of point printed, we will use this at the end to
	// finish filling out the Information Block.
	point_count++;
}

// This is a sort of hacked together ascii-> unicode 2 converter.  I have no idea
// if iGo8 even supports real unicode 2, but is does look like it as every ascii
// character is a short with the ascii character as the least significant 7 bits
//
// Please replace this with a much more filled out and correct version if you see 
// fit.
unsigned int ascii_to_unicode_2(char* dst, unsigned int dst_max_length, char* src)
{
	unsigned int current_src_position = 0;
	unsigned int current_dst_position = 0;
	unsigned short current_unicode_char;

	while (src[current_src_position] != '\0' && current_dst_position+4 <= dst_max_length)
	{
		le_write16(&current_unicode_char, src[current_src_position]);
		*(gbuint16*)&dst[current_dst_position] = current_unicode_char;
		current_src_position++;
		current_dst_position += 2;
	}

	if (src[current_src_position] == '\0' && current_dst_position+2 <= dst_max_length)
	{
		*(gbuint16*)&dst[current_dst_position] = 0;
		current_dst_position += 2;
	}

	return current_dst_position;
}

void write_header()
{
	char header[IGO8_HEADER_SIZE] = {'\0'};
	igo8_id_block tmp_id_block;
	p_igo8_id_block id_block = (p_igo8_id_block)header;
	gbuint32 current_position = 0;

	tmp_id_block.unknown_1 = 0x0000029B;
	tmp_id_block.unknown_2 = 0x000003E7;
	tmp_id_block.unknown_3 = 0x00000003;

	// This appears to be a unique number that IDs the track.
	// It is mono-incrementing and offset by 2 above the track number.
	// e.g. "Track 1" --> track_number = 3
	// XXX - Dustin: This might need to be modified to print a number that is user-provided.
	// XXX - Dustin: My guess is that this number is used as the key for the track color, if 
	// XXX - Dustin: multiple tracks have the same color they will be colored the same, just
	// XXX - Dustin: a guess though.
	tmp_id_block.track_number = 0x00000010;
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
	// XXX - Dustin: This needs to modified to print a title that the user provides.
	current_position += ascii_to_unicode_2((char*)(header+current_position), IGO8_HEADER_SIZE - current_position, "Title");

	// Set the description of the track
	// XXX - Dustin: This needs to modified to print a description that the user provides.
	current_position += ascii_to_unicode_2((char*)(header+current_position), IGO8_HEADER_SIZE - current_position, "Description");

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
	{ ff_cap_none, ff_cap_read | ff_cap_write, ff_cap_none },
	igo8_read_init,	
	igo8_write_init,	
	igo8_read_deinit,	
	igo8_write_deinit,	
	igo8_read,
	igo8_write,
	NULL,
	NULL,
	CET_CHARSET_UTF8, 
	1
};

 	  	 
