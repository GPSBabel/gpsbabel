/*
    Locosys NaviGPS GT-31/BGT-31 binary datalog format (SBP)

    Copyright (C) 2008  Rodney Lorrimar <rodney@rodney.id.au>
    Copyright (C) 2005  Robert Lipe, robertlipe@usa.net

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
#include "navilink.h"
#include <ctype.h>

#define MYNAME "sbp"

static gbfile *file_handle = NULL;

static
arglist_t sbp_args[] = {
	ARG_TERMINATOR
};

/*******************************************************************************
* %%%        global callbacks called by gpsbabel main process              %%% *
*******************************************************************************/

static void
sbp_rd_init(const char *fname)
{
	file_handle = gbfopen(fname, "r", MYNAME);
}

static void 
sbp_rd_deinit(void)
{
	gbfclose(file_handle);
}

#ifdef SBP_PARSE_HEADER
static size_t
hdrcpy(char *dest, const char *src,
       size_t max_len, char delim)
{
	size_t i;
	
	for (i = 0; i < max_len && *src != delim; i++) 
		*dest++ = *src++;
	*dest++ = 0;

	return ++i;
}
#endif

static void
decode_header(route_head *track)
{
  char header[64];

  if (gbfread(header, sizeof(header), 1, file_handle) == 1) {
#ifdef SBP_PARSE_HEADER
		/*
		 * A complete SBP file contains 64 bytes header,
		 *
		 * Here is the definition of the SBP header 
		 * BYTE 0 ~1 : true SBP header length 
		 * BYTE 2~63:  MID_FILE_ID(0xfd) with following payload :
		 *             User Name, Serial Number, Log Rate, Firmware Version
		 *               >field separator:","
		 *               >User Name : MAX CHAR(13)
		 *               >Serial Number : MAX CHAR(8)
		 *               >Log Rate : MAX CHAR 3, 0..255 in seconds
		 *               >Firmware Version  :  MAX CHAR (13)
		 *               // will stuff 0xff for remaining bytes
		 */

#define HDR_LEN         2
#define PACKET_HDR_LEN  5       /* 0xa0 0xa2 len 0xfd */
#define USERNAME_LEN    13
#define SERIAL_NUM_LEN  9
#define LOG_RATE_LEN    3
#define VERSION_LEN     13
#define SEP             ','
#define PAD             0xFF

		int hdr_len;
		char username[USERNAME_LEN + 1];
		char serial_num[SERIAL_NUM_LEN + 1];
		char log_rate[LOG_RATE_LEN + 1];
		char version[VERSION_LEN + 1];
		char *p = header;

		hdr_len = le_read16(p);
		p += HDR_LEN;
		p += PACKET_HDR_LEN;

		p += hdrcpy(username,   p, USERNAME_LEN,   SEP);
		p += hdrcpy(serial_num, p, SERIAL_NUM_LEN, SEP);
		p += hdrcpy(log_rate,   p, LOG_RATE_LEN,   SEP);
		p += hdrcpy(version,    p, VERSION_LEN,	   PAD);
    /* fixme: version includes checksum and end sequence bytes */

		printf("Username: %s\n", username);
		printf("Serial Number: %s\n", serial_num);
		printf("Log rate (seconds): %s\n", log_rate);
		printf("Firmware version: %s\n", version);
#endif /* SBP_PARSE_HEADER */
	} else {
		fatal(MYNAME ": Couldn't read SBP header. "
		     "This probably isn't a SBP file.\n");
	}
}

static waypoint *
read_logpoint(void)
{
	unsigned char buffer[SBP_RECORD_LEN];

	if (gbfread(buffer, sizeof(buffer), 1, file_handle) == 1) {
		return navilink_decode_logpoint(buffer);
	}

	return NULL;
}

static void
sbp_read(void)
{
	if (global_opts.masked_objective & TRKDATAMASK) {
		waypoint *logpoint;
		route_head     *track;

		track = route_head_alloc();
		track_add_head(track);

		decode_header(track);
  
		while ((logpoint = read_logpoint())) {
			track_add_wpt(track, logpoint);
		}
	}
}

static void
sbp_exit(void)
{
}

/**************************************************************************/

ff_vecs_t sbp_vecs = {
	ff_type_file,
	{ 
		ff_cap_none         /* waypoints */, 
		ff_cap_read					/* tracks */, 
		ff_cap_none					/* routes */
	},
	sbp_rd_init,	
	NULL,	
	sbp_rd_deinit,	
	NULL,	
	sbp_read,
	NULL,
	sbp_exit,
	sbp_args,
	CET_CHARSET_ASCII, 0			/* ascii is the expected character set */
						/* not fixed, can be changed through command line parameter */
};
/**************************************************************************/
