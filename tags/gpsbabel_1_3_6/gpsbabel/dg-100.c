/*

    GlobalSat DG-100 GPS data logger download.

    Copyright (C) 2007  Mirko Parthey, mirko.parthey@informatik.tu-chemnitz.de
    Copyright (C) 2005-2008  Robert Lipe, robertlipe@gpsbabel.org

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
    DG-100 communication protocol specification:
    http://www.usglobalsat.com/forum/topic.asp?TOPIC_ID=607#1375
 */

#include "defs.h"
#include <ctype.h>

#include "gbser.h"
#include <assert.h>
#include <string.h>

#define MYNAME "DG-100"

static void *serial_handle;

/* maximum frame size observed so far: 1817 bytes
 *   (dg100cmd_getfileheader returning 150 entries)
 * dg100cmd_getfileheader is the only answer type of variable length,
 * answers of other types are always shorter than 1817 bytes */
#define FRAME_MAXLEN 4096

enum dg100_command_id {
	dg100cmd_getconfig     = 0xB7,
	dg100cmd_setconfig     = 0xB8,
	dg100cmd_getfileheader = 0xBB,
	dg100cmd_getfile       = 0xB5,
	dg100cmd_erase         = 0xBA,
	dg100cmd_getid         = 0xBF,
	dg100cmd_setid         = 0xC0,
	dg100cmd_gpsmouse      = 0xBC
};

struct dg100_command {
	int  id;
	int  sendsize;
	int  recvsize;
	int  trailing_bytes;
	const char *text;	/* Textual description for debugging */
};

struct dg100_command dg100_commands[] = {
	{ dg100cmd_getconfig,      0,   44+2,    2, "getconfig" },
	{ dg100cmd_setconfig,     41,    4+2,    2, "setconfig"  },
	/* the getfileheader answer has variable length, -1 is a dummy value */
	{ dg100cmd_getfileheader,  2,   -1  ,    2, "getfileheader"  },
	{ dg100cmd_getfile,        2, 1024+2,    2, "getfile" },
	{ dg100cmd_erase,          2,    4+2,    2, "erase" },
	{ dg100cmd_getid,          0,    8+2,    2, "getid" },
	{ dg100cmd_setid,          8,    4+2,    2, "setid" },
	{ dg100cmd_gpsmouse,       1,    0  ,    0, "gpsmouse" }
};
const unsigned dg100_numcommands = sizeof(dg100_commands) / sizeof(dg100_commands[0]);

struct dynarray16 {
	unsigned count; /* number of elements used */
	unsigned limit; /* number of elements allocated */
	gbint16 *data;
};

/* helper functions */
static struct dg100_command *
dg100_findcmd(int id)
{
	unsigned int i;

	/* linear search should be OK as long as dg100_numcommands is small */
	for (i = 0; i < dg100_numcommands; i++) {
		if (dg100_commands[i].id == id)
			return(&dg100_commands[i]);
	}

	return NULL;
}

static void
dynarray16_init(struct dynarray16 *a, unsigned limit)
{
	a->count = 0;
	a->limit = limit;
	a->data = xmalloc(sizeof(a->data[0]) * a->limit);
}

static gbint16 *
dynarray16_alloc(struct dynarray16 *a, unsigned n)
{
	unsigned oldcount, need;
	const unsigned elements_per_chunk = 4096 / sizeof(a->data[0]);
	
	oldcount = a->count;
	a->count += n;
	
	if (a->count > a->limit) {
		need = a->count - a->limit;
		need = (need > elements_per_chunk) ? need : elements_per_chunk;
		a->limit += need;
		a->data = xrealloc(a->data, sizeof(a->data[0]) * a->limit);
	}
	return(a->data + oldcount);
}

static time_t
bintime2utc(int date, int time)
{
	struct tm gpstime;

	gpstime.tm_sec   = time % 100;
	time /= 100;
	gpstime.tm_min   = time % 100;
	time /= 100;
	gpstime.tm_hour  = time;

	/*
	 * GPS year: 2000+; struct tm year: 1900+
	 * GPS month: 1-12, struct tm month: 0-11
	 */
	gpstime.tm_year  = date % 100 + 100;
	date /= 100;
	gpstime.tm_mon   = date % 100 - 1;
	date /= 100;
	gpstime.tm_mday  = date;

	return(mkgmtime(&gpstime));
}

static void 
dg100_debug(const char *hdr, int include_nl, size_t sz, unsigned char *buf)
{
	unsigned int i;

	/* Only give byte dumps for higher debug levels */
	if (global_opts.debug_level < 5) {
		return;
	}

	fprintf(stderr, "%s", hdr);

	for (i = 0; i < sz; i++)  {
		fprintf(stderr, "%02x ", buf[i]);
	}

	if (include_nl) {
		fprintf(stderr, "\n");
	}
}

static void
dg100_log(const char *fmt, ...)
{
	va_list ap;
	va_start (ap, fmt);
	if (global_opts.debug_level < 1) {
		return;
	}
	
	vfprintf(stderr, fmt, ap);
	va_end(ap);
}


/* TODO: check whether negative lat/lon (West/South) are handled correctly */
static float
bin2deg(int val)
{
	/* Assume that val prints in decimal digits as [-]dddmmffff
	 * ddd:  degrees
	 * mm:   the integer part of minutes
	 * ffff: the fractional part of minutes (decimal fraction 0.ffff)
	 */

	float deg;
	int deg_int, min_scaled, isneg;
	unsigned absval;
	
	/* avoid division of negative integers,
	 * which has platform-dependent results */
	absval = abs(val);
	isneg = (val < 0);

	deg_int = absval / 1000000;      /* extract ddd */
	min_scaled = absval % 1000000;   /* extract mmffff (minutes * 10^4) */
	deg = deg_int + (double) min_scaled / (10000 * 60);

	/* restore the sign */
	deg = isneg ? -deg : deg;
	return(deg);
}

static void
process_gpsfile(gbuint8 data[], route_head *track)
{
	const int recordsizes[3] = {8, 20, 32};
	int i, style, recsize;
	int lat, lon, bintime, bindate;
	waypoint *wpt;

	/* the first record of each file is always full-sized; its style field
	 * determines the format of all subsequent records in the file */
	style = be_read32(data + 28);
	if (style > 2) {
		fprintf(stderr, "unknown GPS record style %d", style);
		return;
	}
	recsize = recordsizes[style];

	for (i = 0; i <= 2048 - recsize; i += (i == 0) ? 32 : recsize) {

		lat = be_read32(data + i + 0);
		lon = be_read32(data + i + 4);

		/* skip invalid trackpoints (blank records) */
		if (lat == -1 && lon == -1) {
			continue;
		}

		wpt = waypt_new();
		wpt->latitude = bin2deg(lat);
		wpt->longitude = bin2deg(lon);

		if (style >= 1) {
			bintime = be_read32(data + i +  8);
			bindate = be_read32(data + i + 12);
			wpt->creation_time = bintime2utc(bindate, bintime);
			/* The device presents the speed as a fixed-point number
			 * with a scaling factor of 100, in km/h.
			 * The waypoint struct wants the speed as a
			 * floating-point number, in m/s. */
			wpt->speed = KPH_TO_MPS(be_read32(data + i + 16) / 100.0);
			wpt->wpt_flags.speed = 1;
		}

		if (style >= 2) {
			wpt->altitude = be_read32(data + i + 20) / 10000.0;
		}

		track_add_wpt(track, wpt);
	}
}

static gbuint16
dg100_checksum(gbuint8 buf[], int count)
{
	gbuint16 sum = 0;
	int i;

	for (i = 0; i < count; i++) {
		sum += buf[i];
	}
	sum &= (1<<15) - 1;

	return(sum);
}

/* communication functions */
static size_t
dg100_send(gbuint8 cmd, const void *payload, size_t count)
{
	gbuint8 frame[FRAME_MAXLEN];
	gbuint16 checksum, payload_len;
	size_t framelen, param_len;
	int n;
	
	param_len = count;
	payload_len = 1 + count;
	/* Frame length calculation:
	 * frame start sequence(2), payload length field(2), command id(1),
	 * param(variable length),
	 * checksum(2), frame end sequence(2) */
	framelen = 2 + 2 + 1 + count + 2 + 2;
	assert(framelen <= FRAME_MAXLEN);

	/* create frame head + command */
	be_write16(frame + 0, 0xA0A2);
	be_write16(frame + 2, payload_len);
	frame[4] = cmd;

	/* copy payload */
	memcpy(frame + 5, payload, count);

	/* create frame tail */
	checksum = dg100_checksum(frame + 4, framelen - 8);
	be_write16(frame + framelen - 4, checksum);
	be_write16(frame + framelen - 2, 0xB0B3);

	n = gbser_write(serial_handle, frame, framelen);

	if (global_opts.debug_level) {
		struct dg100_command *cmdp = dg100_findcmd(cmd);

		dg100_debug(n == 0 ? "Sent: " : "Error Sending:", 
			1, framelen, frame);
		dg100_log("TX: Frame Start %02x %02x Payload_Len %04x Cmd: %s\n", 
			frame[0], frame[1], payload_len, cmdp->text);
	}
	
	if (n == gbser_ERROR) {
	    	fatal("dg_100_send: write failed\n");
	}
	return (n);
}

static int
dg100_recv_byte()
{
	int result;

	/* allow for a delay of 40s; 
	 *  erasing the whole DG-100 memory takes about 21s */
	result = gbser_readc_wait(serial_handle, 40000);
	switch(result){
		case gbser_ERROR:
			fatal("dg100_recv_byte(): error reading one byte\n");
		case gbser_NOTHING:
			fatal("dg100_recv_byte(): read timeout\n");
	}
	return result;
}

/* payload returns a pointer into a static buffer (which also contains the
 * framing around the data), so the caller must copy the data before calling
 * this function again */
static int
dg100_recv_frame(struct dg100_command **cmdinfo_result, gbuint8 **payload)
{
	static gbuint8 buf[FRAME_MAXLEN];
	gbuint16 frame_start_seq, payload_len_field;
	gbuint16 payload_end_seq, payload_checksum, frame_end_seq;
	gbuint16 frame_head, numheaders, sum;
	gbuint8 c, cmd;
	int i, param_len, frame_len;
	struct dg100_command *cmdinfo;

	/* consume input until frame head sequence 0xA0A2 was received */
	frame_head = 0;
	dg100_debug("Receiving ", 0, 0, NULL);
	do {
		c = dg100_recv_byte();
		dg100_debug("", 0, 1, &c);
		frame_head <<= 8;
		frame_head |= c;

	} while (frame_head != 0xA0A2);

	be_write16(buf + 0, frame_head);

	/* To read the remaining data, we need to know how long the frame is.
	 *
	 * The obvious source of this information would be the payload length
	 * field, but the spec says that this field should be ignored in answers.
	 * Indeed, its value differs from the actual payload length.
	 *
	 * We could scan for the frame end sequences,
	 * but there is no guarantee that they do not appear within valid data.
	 *
	 * This means we can only calculate the length using information from
	 * the beginning of the frame, other than the payload length.
	 *
	 * The solution implemented here is to derive the frame length from the
	 * Command ID field, which is more of an answer ID. This is possible
	 * since for each answer ID, the frame length is either constant or it
	 * can be derived from the first two bytes of payload data.
	 */

	/* read Payload Length, Command ID, and two further bytes */
	i = gbser_read_wait(serial_handle, &buf[2], 5, 1000);
	if (i < 5) {
		fatal("Expected to read 5 bytes, but got %d\n", i);
	}
	dg100_debug("", 0, 5, &buf[2]);

	payload_len_field = be_read16(buf + 2);
	cmd = buf[4];

	/*
	 * getconfig/setconfig have the same answer ID -
	 * this seems to be a firmware bug we must work around.
	 * Distinguish them by the (otherwise ignored) Payload Len field,
	 * which was observed as 53 for getconfig and 5 for setconfig.
	 */
	if (cmd == dg100cmd_getconfig && payload_len_field <= 20) {
		cmd = dg100cmd_setconfig;
	}

	cmdinfo = dg100_findcmd(cmd);
	if (!cmdinfo) {
		/* TODO: consume data until frame end signature,
		 * then report failure to the caller? */
		fatal("unknown answer ID %02x\n", cmd);
	}

	param_len = cmdinfo->recvsize;

	/*
	 * the getfileheader answer has a varying param_len,
	 * we need to calculate it
	 */
	if (cmd == dg100cmd_getfileheader) {
		numheaders = be_read16(buf + 5);
		param_len = 2 + 2 + 12 * numheaders + 2;
	}

	/* Frame length calculation:
	 * frame start sequence(2), payload length field(2), command id(1),
	 * param(variable length),
	 * payload end seqence(2), checksum(2), frame end sequence(2) */
	frame_len = 2 + 2 + 1 + param_len + 2 + 2 + 2;
	 
	if (frame_len > FRAME_MAXLEN) {
		fatal("frame too large (frame_len=%d, FRAME_MAXLEN=%d)\n",
				frame_len, FRAME_MAXLEN);
	}

	i = gbser_read_wait(serial_handle, &buf[7], frame_len - 7, 1000);
	if (i < frame_len - 7) {
		fatal("Expected to read %d bytes, but got %d\n", 
			frame_len - 7, i);
	}
	dg100_debug("", 0, frame_len - 7, &buf[7]);

	frame_start_seq   = be_read16(buf + 0);
	payload_len_field = be_read16(buf + 2);
	payload_end_seq   = be_read16(buf + frame_len - 6);
	payload_checksum  = be_read16(buf + frame_len - 4);
	frame_end_seq     = be_read16(buf + frame_len - 2);

	dg100_log("RX: Start %04x Len %04x Cmd: %s\n",
		frame_start_seq, payload_len_field, cmdinfo->text);

	/* calculate checksum */
	sum = dg100_checksum(buf + 4, frame_len - 8);
	if (sum != payload_checksum) {
		fatal("checksum mismatch: data sum is 0x%04x, checksum received is 0x%04x\n",
				sum, payload_checksum);
	}

	/*
	 * TODO: check signatures;
	 * on failure, flush input or scan for end sequence
	 */

	*cmdinfo_result = cmdinfo;
	*payload = buf + 5;
	dg100_debug("\n", 0, 0, &buf[i]);
	return(param_len);
}

/* return value: number of bytes copied into buf, -1 on error */
static int
dg100_recv(gbuint8 expected_id, void *buf, unsigned int len)
{
	int n;
	struct dg100_command *cmdinfo;
	gbuint8 *data;
	unsigned int copysize, trailing_bytes;

	n = dg100_recv_frame(&cmdinfo, &data);

	/* check whether the received frame matches the expected answer type */
	if (cmdinfo->id != expected_id) {
		fprintf(stderr, "ERROR: answer type %02x, expecting %02x", cmdinfo->id, expected_id);
		return -1;
	}

	trailing_bytes = cmdinfo->trailing_bytes;
	copysize = n - trailing_bytes;

	/* check for buffer overflow */
	if (len < copysize) {
		fprintf(stderr, "ERROR: buffer too small, size=%d, need=%d", len, copysize);
		return -1;
	}

	memcpy(buf, data, copysize);
	return(copysize);
}

/* the number of bytes to be sent is determined by cmd,
 * count is the size of recvbuf */
static int
dg100_request(gbuint8 cmd, const void *sendbuf, void *recvbuf, size_t count)
{
	struct dg100_command *cmdinfo;
	int n, i, frames, fill;
	gbuint8 *buf;

	cmdinfo = dg100_findcmd(cmd);
	assert (cmdinfo != NULL);
	dg100_send(cmd, sendbuf, cmdinfo->sendsize);

	/* the number of frames the answer will comprise */
	frames = (cmd == dg100cmd_getfile) ? 2 : 1;
	/* alias pointer for easy typecasting */
	buf = recvbuf;
	fill = 0;
	for (i = 0; i < frames; i++) {
		n = dg100_recv(cmd, buf + fill, count - fill);
		if (n < 0)
			return(-1);
		fill += n;
	}
	return(fill);
}

/* higher level communication functions */
static void
dg100_getfileheaders(struct dynarray16 *headers)
{
	gbuint8 request[2];
	gbuint8 answer[FRAME_MAXLEN];
	int seqnum;
	gbint16 numheaders, nextheader, *h;
	int i, offset;

	nextheader = 0;
	do {
		/* request the next batch of headers */
		be_write16(request, nextheader);
		dg100_request(dg100cmd_getfileheader, request, answer, sizeof(answer));

		/* process the answer */
		numheaders = be_read16(answer);
		nextheader = be_read16(answer + 2);
		dg100_log("found %d headers, nextheader=%d\n",
			numheaders, nextheader);
		if (numheaders <= 0) {
			dg100_log("no further headers, aborting the loop\n");
			break;
		}

		h = dynarray16_alloc(headers, numheaders);
		for (i = 0; i < numheaders; i++) {
			offset = 4 + i * 12;
			seqnum = be_read32(answer + offset + 8);
			h[i] = seqnum;
			if (global_opts.debug_level) {
				int time   = be_read32(answer + offset);
				int date   = be_read32(answer + offset + 4);
				time_t ti = bintime2utc(date, time);
				dg100_log("Header #%d: Seq: %d Time: %s", 
					i, seqnum, ctime(&ti));
			}
		}
	} while (nextheader != 0);
}

static void
dg100_getfile(gbint16 num, route_head *track)
{
	gbuint8 request[2];
	gbuint8 answer[2048];

	be_write16(request, num);
	dg100_request(dg100cmd_getfile, request, answer, sizeof(answer));
	process_gpsfile(answer, track);
}

static void
dg100_getfiles()
{
	unsigned int i;
	int filenum;
	struct dynarray16 headers;
	route_head *track;

	track = route_head_alloc();
	track->rte_name = xstrdup("DG-100 tracklog");
	track->rte_desc = xstrdup("DG-100 GPS tracklog data");
	track_add_head(track);      

	/* maximum number of headers observed so far: 672
	 * if necessary, the dynarray will grow even further */
	dynarray16_init(&headers, 1024);

	dg100_getfileheaders(&headers);

	for (i = 0; i < headers.count; i++) {
		filenum = headers.data[i];
		dg100_getfile(filenum, track);
	}
}

static int
dg100_erase()
{
	gbuint8 request[2] = { 0xFF, 0xFF };
	gbuint8 answer[4];

	dg100_request(dg100cmd_erase, request, answer, sizeof(answer));
	if (be_read32(answer) != 1) {
		fprintf(stderr, "dg100_erase() FAILED\n");
		return(-1);
	}
	return(0);
}

/* GPSBabel integration */

static char *erase;

static
arglist_t dg100_args[] = {
	{ "erase", &erase, "Erase device data after download", 
		"0", ARGTYPE_BOOL, ARG_NOMINMAX }, 
	ARG_TERMINATOR
};

/*******************************************************************************
* %%%        global callbacks called by gpsbabel main process              %%% *
*******************************************************************************/

static void
dg100_rd_init(const char *fname)
{
	if (serial_handle = gbser_init(fname), NULL == serial_handle) {
		fatal(MYNAME ": Can't open port '%s'\n", fname);
	}
	if (gbser_set_speed(serial_handle, 115200) != gbser_OK) {
		fatal(MYNAME ": Can't configure port '%s'\n", fname);
	}
}

static void 
dg100_rd_deinit(void)
{
	gbser_deinit(serial_handle);
	serial_handle = NULL;
}

static void
dg100_read(void)
{
	dg100_getfiles();
	if (*erase == '1') {
		dg100_erase();
	}
}

/**************************************************************************/

// capabilities below means: we can only read tracks

ff_vecs_t dg100_vecs = {
	ff_type_serial,
	{ 
		ff_cap_none			/* waypoints */, 
	  	ff_cap_read 			/* tracks */, 
	  	ff_cap_none 			/* routes */
	},
	dg100_rd_init,	
	NULL,	
	dg100_rd_deinit,	
	NULL,	
	dg100_read,
	NULL,
	NULL,
	dg100_args,
	CET_CHARSET_ASCII, 0			/* ascii is the expected character set */
						/* not fixed, can be changed through command line parameter */
};
/**************************************************************************/
