/*

    Serial download of track data from GPS loggers with Skytraq chipset.

    6) Add sample files (it's better when they're created by the "real" 
       application and not our own output) to reference/ along with 
       files in a well supported (preferably non-binary) format and 
       entries in our 'testo' program.   This allows users of different
       OSes and hardware to exercise your module.

    Copyright (C) 2008  Mathias Adam, m.adam (at) adamis.de
    Copyright (C) 2008  J.C Haessig, jean-christophe.haessig (at) dianosis.org
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

#include <ctype.h>

#include "defs.h"
#include "gbser.h"
//#include "jeeps/gpsmath.h"

#define MYNAME "skytraq"

#define SECTOR_SIZE		4096
#define TIMEOUT			5000

/* Maximum number of chars to skip while waiting for a reply: */
#define RETRIES			250
/* Maximum number of messages to read while expecting a specific message or ACK/NACK: */
#define MSG_RETRIES		3
/* Abort when reading a specific sector fails this many times: */
#define SECTOR_RETRIES		3

#define res_OK			0
#define res_ERROR		-1
#define res_NACK		-2
#define res_PROTOCOL_ERR	-3
#define res_NOTFOUND		-4

#define MIN(X,Y) ((X) < (Y) ? (X) : (Y))
#define MAX(X,Y) ((X) > (Y) ? (X) : (Y))


static char *port;		/* port name */
static void *serial_handle;	/* IO file descriptor */
static int skytraq_baud = 0;	/* detected baud rate */

static char *opt_erase;		/* erase after read? (0/1) */
static char *opt_initbaud;	/* baud rate used to init device */
static char *opt_dlbaud;	/* baud rate used for downloading tracks */
static char *opt_read_at_once;	/* number of sectors to read at once (Venus6 only) */

static
arglist_t skytraq_args[] = {
	{ "erase", &opt_erase, "Erase device data after download",
	  "0", ARGTYPE_BOOL, ARG_NOMINMAX },
	{ "initbaud", &opt_initbaud, "Baud rate used to init device, 0=autodetect",
	  "0", ARGTYPE_INT, "4800", "230400" },
	{ "baud", &opt_dlbaud, "Baud rate used for download",
	  "230400", ARGTYPE_INT, "4800", "230400" },
	{ "read-at-once", &opt_read_at_once, "Number of sectors to read at once (Venus6 only)",
	  "255", ARGTYPE_INT, "0", "255" },
	ARG_TERMINATOR
};

static void
db(int l, const char *msg, ...)
{
	va_list ap;
	va_start(ap, msg);
	if (global_opts.debug_level >= l) {
		vprintf(msg, ap);
	}
	va_end(ap);
}

static void
rd_drain(void)
{
	if (gbser_flush(serial_handle)) {
		db(1, MYNAME ": rd_drain(): Comm error\n");
	}
}

static int
rd_char(int *errors)
{
	int c;
	while (*errors > 0) {
		c = gbser_readc_wait(serial_handle, TIMEOUT);
		if (c < 0) {
			db(1, MYNAME ": rd_char(): Got error: %d\n", c);
			(*errors)--;
		} else {
			db(4, "rd_char(): Got char: %02x '%c'\n", c, isprint(c) ? c : '.');
			return c;
		}
	}
	fatal(MYNAME ": Too many read errors on serial port\n");
}

static int
rd_buf(const gbuint8 *buf, int len)
{
	int rc, timeout, i;
	char dump[16*3+16+2];

	/* Allow TIMEOUT plus the time needed to actually receive the data bytes:
	 * baudrate/10 bytes per second (8 data bits, start and stop bit)
	 * TODO: use dlbaud if selected.
	 */
	timeout = TIMEOUT + len;//*1000/(skytraq_baud/10);
/*TODO: timeout gets <0 e.g. when len~=250000 --> 32bit signed int is too small.
	if (skytraq_baud > 0)  timeout = TIMEOUT + (long long int)len*1000*10/(long long int)skytraq_baud;
printf("len=%i  skytraq_baud=%i  timeout=%i\n", len, skytraq_baud, timeout);*/
	rc = gbser_read_wait(serial_handle, (void*)buf, len, timeout);
	if (rc < 0) {
		db(1, MYNAME ": rd_buf(): Read error (%d)\n", rc);
		return res_ERROR;
	} else if (rc < len) {
		db(1, MYNAME ": rd_buf(): Read timout\n");
		return res_ERROR;
	}

	if (global_opts.debug_level >= 4) {
		db(4, "rd_buf():  dump follows:\n");
		dump[sizeof(dump)-1] = 0;
		for (i = 0; i < (len+15)/16*16; i++) {		// count to next 16-byte boundary
			if (i < len) {
				snprintf(&dump[(i%16)*3], 4, "%02x ", buf[i]);
				snprintf(&dump[3*16+1+(i%16)], 2, "%c", isprint(buf[i]) ? buf[i] : '.');
			} else {
				memset(&dump[(i%16)*3], ' ', 3);
				dump[3*16+1+(i%16)] = ' ';
			}
			if ((i+1)%16 == 0) {
				dump[16*3] = ' ';	// gets overwritten with 0 by snprintf
				db(4, "%s\n", dump);
			}
		}
	}

	return res_OK;
}

static int
rd_word(void)
{
	int errors = 5;		/* allow this many errors */
	gbuint8 buffer[2];

	buffer[0] = rd_char(&errors);
	buffer[1] = rd_char(&errors);
/*	if (rd_buf(buffer, 2) != res_OK) {
		db(1, MYNAME ": rd_word(): Read error\n");
		return res_ERROR;
	}*/

	return (buffer[0] << 8) | buffer[1];
}

static void
wr_char(int c)
{
	int rc;
	db(4, "Sending: %02x '%c'\n", (unsigned)c, isprint(c) ? c : '.');
	if (rc = gbser_writec(serial_handle, c), gbser_OK != rc) {
		fatal(MYNAME ": Write error (%d)\n", rc);
	}
}

static void
wr_buf(const unsigned char *str, int len)
{
	int i;
	for (i = 0; i < len; i++) {
		wr_char(str[i]);
	}
}

/*******************************************************************************
* %%%        SkyTraq protocol implementation                               %%% *
*******************************************************************************/

gbuint8 NL[2] = { 0x0D, 0x0A };
gbuint8 MSG_START[2] = { 0xA0, 0xA1 };
gbuint8 SECTOR_READ_END[13] = "END\0CHECKSUM=";

static int
skytraq_calc_checksum(const unsigned char *buf, int len)
{
	int i, cs = 0;
	for (i = 0; i < len; i++) {
		cs ^= buf[i];
	}
	return cs;
}

static int
skytraq_rd_msg(const void *payload, int len)
{
	int errors = 5;		/* allow this many errors */
	int c, i, state;
	int rcv_len, calc_cs, rcv_cs;

	for (i = 0, state = 0; i < RETRIES && state < sizeof(MSG_START); i++) {
		c = rd_char(&errors);
		if (c == MSG_START[state]) {
			state++;
		} else if (c == MSG_START[0]) {
			state = 1;
		} else {
			state = 0;
		}
	}
	if (state < sizeof(MSG_START)) {
		db(1, MYNAME ": Didn't get message start tag\n");
		return res_ERROR;
	}

	if ((rcv_len = rd_word()) < len) {
		if (rcv_len >= 0) {	/* negative values indicate receive errors */
			db(1, MYNAME ": Received message too short (got %i bytes, expected %i)\n",
				rcv_len, len);
			return res_PROTOCOL_ERR;
		}
		return res_ERROR;
	}

	db(2, "Receiving message with %i bytes of payload (expected >=%i)\n", rcv_len, len);
	rd_buf(payload, MIN(rcv_len, len));

	calc_cs = skytraq_calc_checksum(payload, MIN(rcv_len, len));
	for (i = 0; i < rcv_len-len; i++) {
		c = rd_char(&errors);
		calc_cs ^= c;
	}

	rcv_cs = rd_char(&errors);
	if (rcv_cs != calc_cs) {
		fatal(MYNAME ": Checksum error: got 0x%02x, expected 0x%02x\n", rcv_cs, calc_cs);
	}

	if (rd_word() != 0x0D0A) {
		fatal(MYNAME ": Didn't get message end tag (CR/LF)\n");
	}

//	return MIN(rcv_len, len);
	return res_OK;
}

static void
skytraq_wr_msg(const gbuint8 *payload, int len)
{
	int cs;

	rd_drain();

	wr_buf(MSG_START, sizeof(MSG_START));
	wr_char((len>>8) & 0x0FF);
	wr_char(len & 0x0FF);
	wr_buf(payload, len);

	cs = skytraq_calc_checksum(payload, len);
	wr_char(cs);
	wr_buf(NL, sizeof(NL));
}

static int
skytraq_expect_ack(gbuint8 id)
{
	gbuint8 ack_msg[2];
	int i/*, rcv_len*/;

	for (i = 0; i < MSG_RETRIES; i++) {
//		rcv_len = skytraq_rd_msg(ack_msg, sizeof(ack_msg));
//		if (rcv_len == sizeof(ack_msg)) {
		if (skytraq_rd_msg(ack_msg, sizeof(ack_msg)) == res_OK) {
			if (ack_msg[0] == 0x83) {
				if (ack_msg[1] == id) {
					db(3, "Got ACK (id=0x%02x)\n", id);
					return res_OK;
				} else if (ack_msg[1] == 0) {
					/* some (all?) devices first send an ACK with id==0, skip that */
					continue;
				} else {
					db(1, MYNAME ": Warning: Got unexpected ACK (id=0x%02x)\n", ack_msg[1]);
					continue;
				}
			} else if (ack_msg[0] == 0x84) {
				db(3, "Warning: Got NACK (id=0x%02x)\n", ack_msg[1]);
				return res_NACK;
			} else {
				db(3, "Warning: Got unexpected message (id=0x%02x), expected ACK (id=0x%02x)\n",
					ack_msg[0], id);
			}
		} else {
			/* payload too short or didn't receive a message at all
			    -> caller should either resend request or give up.
			*/
			break;
		}
	}

	return res_PROTOCOL_ERR;
}

static int
skytraq_expect_msg(gbuint8 id, const gbuint8 *payload, int len)
{
	int i, rc;

	for (i = 0; i < MSG_RETRIES; i++) {
		rc = skytraq_rd_msg(payload, len);
		if (rc < 0) {
			return rc;
		}
		if (payload[0] == id) {
			return len;
		}
	}

	return res_PROTOCOL_ERR;
}

static int
skytraq_wr_msg_verify(const gbuint8 *payload, int len)
{
	int i, rc;

	for (i = 0; i < MSG_RETRIES; i++) {
		if (i > 0) {
			db(1, "resending msg (id=0x%02x)...\n", payload[0]);
		}
		skytraq_wr_msg(payload, len);
		rc = skytraq_expect_ack(payload[0]);
		if (rc == res_OK  ||  rc == res_NACK) {
			return rc;
		}
		db(1, MYNAME ": Got neither ACK nor NACK, ");
	}
	db(1, "aborting (msg id was 0x%02x).\n", payload[0]);

	return res_ERROR;
}

static int
skytraq_system_restart(void)
{
	gbuint8 MSG_SYSTEM_RESTART[15] =
		{ 0x01, 0x01, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };

	return skytraq_wr_msg_verify(MSG_SYSTEM_RESTART, sizeof(MSG_SYSTEM_RESTART));
}

static int
skytraq_set_baud(int baud)
{
	/* Note: according to AN0003_v3.pdf, attrib == 0x00 means write to SRAM only, however
	 * it seems to write to flash too. The Windows software sends 0x02 so we do here too.
	 */
	gbuint8 MSG_CONFIGURE_SERIAL_PORT[4]
		= { 0x05, 0x00, 0x00, 0x02 }; 
	int rc;

	db(2, "Setting baud rate to %i\n", baud);

	switch (baud) {
	case 4800:
		MSG_CONFIGURE_SERIAL_PORT[2] = 0;
		break;
	case 9600:
		MSG_CONFIGURE_SERIAL_PORT[2] = 1;
		break;
	case 19200:
		MSG_CONFIGURE_SERIAL_PORT[2] = 2;
		break;
	case 38400:
		MSG_CONFIGURE_SERIAL_PORT[2] = 3;
		break;
	case 57600:
		MSG_CONFIGURE_SERIAL_PORT[2] = 4;
		break;
	case 115200:
		MSG_CONFIGURE_SERIAL_PORT[2] = 5;
		break;
	case 230400:
		MSG_CONFIGURE_SERIAL_PORT[2] = 6;
		break;
	default:
		fatal(MYNAME ": Unsupported baud rate: %ibd\n", baud);
	}

	rc = skytraq_wr_msg_verify(MSG_CONFIGURE_SERIAL_PORT, sizeof(MSG_CONFIGURE_SERIAL_PORT));
	if (rc != res_OK) {
		db(2, "Warning: error setting skytraq device baud rate\n");
		return rc;
	}

	db(3, "Now setting UART baud rate to %i\n", baud);
	rd_drain();
	if (gbser_set_speed(serial_handle, baud) != gbser_OK) {
		db(2, "Warning: error setting uart baud rate\n");
		return res_ERROR;
	}

	gb_sleep(50);		/* allow UART to settle. */

	return res_OK;
}

static int
skytraq_get_log_buffer_status(gbuint32 *log_wr_ptr, gbuint16 *sectors_free, gbuint16 *sectors_total)
{
	gbuint8 MSG_LOG_STATUS_CONTROL = 0x17;
	struct {
		gbuint8 id[1];
		gbuint8 log_wr_ptr[4];
		gbuint8 sectors_free[2];
		gbuint8 sectors_total[2];
	} MSG_LOG_STATUS_OUTPUT;
	int rc;

	if ((rc = skytraq_wr_msg_verify(&MSG_LOG_STATUS_CONTROL, 1)) != res_OK) {	/* get memory status */
		db(1, MYNAME ": Error sending LOG STATUS CONTROL message (%d)\n", rc);
		return res_ERROR;
	}

	rc = skytraq_expect_msg(0x94, (gbuint8*)&MSG_LOG_STATUS_OUTPUT, sizeof(MSG_LOG_STATUS_OUTPUT));
	if (rc < sizeof(MSG_LOG_STATUS_OUTPUT)) {
		db(1, MYNAME ": Didn't receive expected reply (%d)\n", rc);
		return res_ERROR;
	}

	*log_wr_ptr = le_readu32(&MSG_LOG_STATUS_OUTPUT.log_wr_ptr);
	*sectors_free = le_readu16(&MSG_LOG_STATUS_OUTPUT.sectors_free);
	*sectors_total = le_readu16(&MSG_LOG_STATUS_OUTPUT.sectors_total);

	return res_OK;
}

/* reads 32-bit "middle-endian" fields */
static unsigned int me_read32(const void *p) {
 return ((unsigned)be_read16(p+2) << 16) | ((unsigned)be_read16(p));
}

struct read_state {
	route_head          *route_head;
	unsigned            wpn, tpn;

	time_t ts;
	long x, y, z;
};

static void state_init(struct read_state *pst) {
	pst->route_head = NULL;
	pst->wpn        = 0;
	pst->tpn        = 0;

	pst->ts         = 0;
	pst->x          = 0;
	pst->y          = 0;
	pst->z          = 0;
}

static waypoint *
make_trackpoint(struct read_state *st, double lat, double lon, double alt)
{
	waypoint *wpt = waypt_new();

	xasprintf(&wpt->shortname, "TP%04d", ++st->tpn);

	wpt->latitude       = lat;;
	wpt->longitude      = lon;
	wpt->altitude       = alt;
	wpt->creation_time  = st->ts;
	
	return wpt;
}

static time_t
gpstime_to_timet(int week, int sec)
{
	/* TODO: make leap second compensation more general 
	 * (the windows software seems to correct by 13).
	 */
	return (315964800 + (week+1024)*7*SECONDS_PER_DAY + sec - 13);
}

static void
ECEF_to_LLA(double x, double y, long z, double *lat, double *lon, double *alt)
{
/* constants: */
#define CA     6378137.0
#define CB     6356752.31424518
#define CE2    (CA*CA - CB*CB) / (CA*CA)    /* =e^2 */
#define CE_2   (CA*CA - CB*CB) / (CB*CB)    /* =e'^2 */
/* auxiliary values: */
#define AP     sqrt(x*x + y*y)
#define ATHETA atan2(z*CA, AP*CB)
#define AN     CA / sqrt(1 - CE2 * pow(sin(*lat), 2))       /* must calc *lat before using AN! */

	// latitude (in radians):
	*lat = atan2(z + CE_2 * CB * pow(sin(ATHETA), 3), AP - CE2 * CA * pow(cos(ATHETA), 3));

	// longitude (in radians):
	*lon = atan2(y, x);

	// height above ellipsoid (in meters):
	*alt = AP/cos(*lat) - AN;

	*lat = *lat /M_PI*180;
	*lon = *lon /M_PI*180;
}

typedef struct {
	short gps_week;
	long gps_sec;
	unsigned long x;
	unsigned long y;
	unsigned long z;
} full_item;

typedef struct {
	short dt;
	short dx;
	short dy;
	short dz;
} compact_item;

struct full_item_frame {
	unsigned char ts[4];
	unsigned char x[4];
	unsigned char y[4];
	unsigned char z[4];
};
struct compact_item_frame {
	unsigned char dt[2]; /* big endian unsigned short */
	unsigned char dpos[4];
};

typedef struct {
	unsigned char type_and_speed[2];
	union {
		struct full_item_frame full;
		struct compact_item_frame comp;
	};
} item_frame;

#define ITEM_TYPE(item) (item->type_and_speed[0] >> 4)
#define ITEM_SPEED(item) (item->type_and_speed[1] | ((item->type_and_speed[0] & 0x0F) << 8))

static int
process_data_item(struct read_state *pst, const item_frame *pitem, int len)
{
	int res = 0;
	double lat, lon, alt;
	unsigned int ts;
	int poi = 0;
	full_item f;
	compact_item c;
	waypoint *tpt = NULL;

	switch (ITEM_TYPE(pitem)) {
	case 0x6:	/* POI item (same structure as full) */
		poi = 1;
		/* fall through: */

	case 0x4:	/* full item */
		if (len < 18) {
			db(1, MYNAME ": Not enough bytes in sector for a full item.\n");
			return res_ERROR;
		}
		ts = me_read32(pitem->full.ts);
		f.gps_week = ts & 0x00000FFF;
		f.gps_sec = ts >> 12;
		f.x = me_read32(pitem->full.x);
		f.y = me_read32(pitem->full.y);
		f.z = me_read32(pitem->full.z);

		pst->ts = gpstime_to_timet(f.gps_week, f.gps_sec);
		pst->x = f.x;
		pst->y = f.y;
		pst->z = f.z;

		db(4, "Got %s item: week=%i sec=%i (time_t=%i)  x=%i  y=%i  z=%i  speed=%i\n",
			poi ? "POI" : "full",
			f.gps_week, f.gps_sec, pst->ts,
			f.x, f.y, f.z,
			ITEM_SPEED(pitem));

		res = 18;
		break;

	case 0x8:	/* compact item */
		if (len < 8) {
			db(1, MYNAME ": Not enough bytes in sector for a compact item.\n");
			return res_ERROR;
		}
		c.dx = (pitem->comp.dpos[1] >> 6) | (pitem->comp.dpos[0] << 2);
		c.dy = (pitem->comp.dpos[1] & 0x3F) | ((pitem->comp.dpos[2] & 0xF0) << 2);
		c.dz = pitem->comp.dpos[3] | ((pitem->comp.dpos[2] & 0x03) << 8);
		if (c.dx > 511)  c.dx = 511-c.dx; /* make proper signed values */
		if (c.dy > 511)  c.dy = 511-c.dy;
		if (c.dz > 511)  c.dz = 511-c.dz;
		c.dt = (pitem->comp.dt[0] << 8) | pitem->comp.dt[1];

		db(4, "Got compact item: dt=%i  dx=%i  dy=%i  dz=%i  speed=%i uu=%i\n",
			c.dt, c.dx, c.dy, c.dz,
			ITEM_SPEED(pitem), (pitem->comp.dpos[2] & 0x0F)>>2);

		pst->ts += c.dt;
		pst->x += c.dx;
		pst->y += c.dy;
		pst->z += c.dz;

		res = 8;
		break;

	default:
		db(1, MYNAME ": Unknown item type encountered: 0x%02x\n", ITEM_TYPE(pitem));
		return 0;
	}

	if (res == 8  ||  res == 18) {
		ECEF_to_LLA(pst->x, pst->y, pst->z, &lat, &lon, &alt);
//		GPS_Math_XYZ_To_WGS84LatLonH(&lat, &lon, &alt, pst->x, pst->y, pst->z);
		tpt = make_trackpoint(pst, lat, lon, alt);
		WAYPT_SET(tpt, speed, KPH_TO_MPS(ITEM_SPEED(pitem))); /* convert speed to m/s */

		if (poi) waypt_add(waypt_dupe(tpt));

		if (0 == pst->route_head) {
			db(1, MYNAME ": New Track\n");
			pst->route_head = route_head_alloc();
			track_add_head(pst->route_head);
		}

		track_add_wpt(pst->route_head, tpt);
	}

	return res;
}

static int	/* returns number of bytes processed (terminates on 0xFF i.e. empty or padding bytes) */
process_data_sector(struct read_state *pst, const gbuint8 *buf, int len)
{
	int plen, ilen;

	for (plen = 0; plen < len  &&  buf[plen] != 0xFF; plen += ilen) {
		ilen = process_data_item(pst, (item_frame*)&buf[plen], len-plen);
		if (ilen <= 0) {
			fatal(MYNAME ": Error processing data item (%i)\n", ilen);
		}
	}

	return plen;
}

static int	/* returns number of bytes read, negative values indicate errors */
skytraq_read_single_sector(int sector, gbuint8 *buf)
{
	gbuint8 MSG_LOG_SECTOR_READ_CONTROL[2] = { 0x1B, sector };
	int errors = 5;		/* allow this many errors */
	int c, i, j, cs;
	gbuint8 buffer[16];

	if (sector < 0  ||  sector > 0xFF) {
		fatal(MYNAME ": Invalid sector number (%i)\n", sector);
	}

	db(2, "Reading sector #%i...\n", sector);

	if (skytraq_wr_msg_verify((gbuint8*)&MSG_LOG_SECTOR_READ_CONTROL, sizeof(MSG_LOG_SECTOR_READ_CONTROL)) != res_OK) {
		db(1, MYNAME ": Didn't receive ACK\n");
		return res_ERROR;
	}

#ifdef READ_SINGLE_CHARS
	for (i = 0, j = 0; i-j < SECTOR_SIZE && j < sizeof(SECTOR_READ_END); i++) {
		c = rd_char(&errors);
		buf[i] = c;
		if (c == SECTOR_READ_END[j]) {
			j++;
		} else if (c == SECTOR_READ_END[0]) {
			j = 1;
		} else {
			j = 0;
		}
	}
	if (j < sizeof(SECTOR_READ_END)) {
		db(1, MYNAME ": Didn't get sector end tag\n");
		return res_ERROR;
	}
	c = rd_char(&errors);	/* read checksum byte */
	buf[i] = c;
#else
	for (i = 0, j = 0; i-j < SECTOR_SIZE && j < sizeof(SECTOR_READ_END); i+=c) {
		rd_buf(buffer, 16);
		for (c = 0; c < 16 && j < sizeof(SECTOR_READ_END); c++) {
			buf[i+c] = buffer[c];
			if (buffer[c] == SECTOR_READ_END[j]) {
				j++;
			} else if (buffer[c] == SECTOR_READ_END[0]) {
				j = 1;
			} else {
				j = 0;
			}
		}
	}
	if (j < sizeof(SECTOR_READ_END)) {
		db(1, MYNAME ": Didn't get sector end tag\n");
		return res_ERROR;
	}
	if (c < 16) {
		buf[i] = buffer[c];
	} else {
		c = rd_char(&errors);	/* read checksum byte */
		buf[i] = c;
	}
#endif
	i = i-j;
	db(3, "Received %i bytes of log data\n", i);

#define SINGLE_READ_WORKAROUND
#ifdef SINGLE_READ_WORKAROUND
	gbser_set_speed(serial_handle, skytraq_baud);
	rd_char(&errors);
	rd_char(&errors);
	rd_char(&errors);
	rd_char(&errors);
	rd_char(&errors);
	rd_char(&errors);
	skytraq_set_baud(atoi(opt_dlbaud));
#endif

	cs = skytraq_calc_checksum(buf, i);
	if (cs != buf[i+sizeof(SECTOR_READ_END)]) {
		db(1, MYNAME ": Checksum error while reading sector: got 0x%02x, expected 0x%02x\n",
			buf[i+sizeof(SECTOR_READ_END)], cs);
		return res_ERROR;
	}

	for (; i < SECTOR_SIZE; i++) {
		buf[i] = 0xFF;
	}

	return i;
}

static int
skytraq_read_multiple_sectors(int first_sector, int sector_count, gbuint8 *buf)
{
	gbuint8 MSG_LOG_READ_MULTI_SECTORS[5] = { 0x1D };
	gbuint8 *buf_end_tag;
	int cs, i, read_result;

	if (first_sector < 0  ||  first_sector > 0xFFFF) {
		fatal(MYNAME ": Invalid sector number (%i)\n", first_sector);
	}
	be_write16(&MSG_LOG_READ_MULTI_SECTORS[1], first_sector);
	if (sector_count < 0  ||  sector_count > 0xFFFF) {
		fatal(MYNAME ": Invalid sector count (%i)\n", sector_count);
	}
	be_write16(&MSG_LOG_READ_MULTI_SECTORS[3], sector_count);

	db(2, "Reading %i sectors beginning from #%i...\n", sector_count, first_sector);

	read_result = skytraq_wr_msg_verify((gbuint8*)&MSG_LOG_READ_MULTI_SECTORS, sizeof(MSG_LOG_READ_MULTI_SECTORS));
	if (read_result != res_OK) {
		return read_result;
	}

	for (i = 0; i < sector_count; i++) {
		db(2, "Receiving data of sector #%i...\n", first_sector+i);
		rd_buf(buf+i*SECTOR_SIZE, SECTOR_SIZE);
	}
	rd_buf(buf+SECTOR_SIZE*sector_count, sizeof(SECTOR_READ_END)+6);

	buf_end_tag = buf + SECTOR_SIZE*sector_count;
	for (i = 0; i < sizeof(SECTOR_READ_END); i++) {
		if (buf_end_tag[i] != SECTOR_READ_END[i]) {
			db(1, MYNAME ": Wrong end tag: got 0x%02x ('%c'), expected 0x%02x ('%c')\n",
				buf_end_tag[i], isprint(buf_end_tag[i]) ? buf_end_tag[i] : '.',
				SECTOR_READ_END[i], isprint(SECTOR_READ_END[i]) ? SECTOR_READ_END[i] : '.');
			return res_ERROR;
		}
	}

	cs = skytraq_calc_checksum(buf, SECTOR_SIZE*sector_count);
	if (cs != buf_end_tag[sizeof(SECTOR_READ_END)]) {
		db(1, MYNAME ": Checksum error while reading sector: got 0x%02x, expected 0x%02x\n",
			buf_end_tag[sizeof(SECTOR_READ_END)], cs);
		return res_ERROR;
	}

	return res_OK;
}

static void
skytraq_read_tracks(route_head *track)
{
	struct read_state st;
	gbuint32 log_wr_ptr;
	gbuint16 sectors_free, sectors_total, /*sectors_used_a, sectors_used_b,*/ sectors_used;
	int i, t, s, rc, got_bytes;
	int read_at_once = MAX(atoi(opt_read_at_once), 1);
	int sectors_read, multi_read_supported = 1;
	gbuint8 *buffer;

	state_init(&st);
	st.route_head = track;

	if (skytraq_get_log_buffer_status(&log_wr_ptr, &sectors_free, &sectors_total) != res_OK) {
		fatal(MYNAME ": Can't get log buffer status\n");
	}
	
	db(1, MYNAME ": Device status: free sectors: %i / total sectors: %i / %i%% used / write ptr: %i\n",
		sectors_free, sectors_total, 100 - sectors_free*100 / sectors_total, log_wr_ptr);

/* Workaround: sectors_free is sometimes reported wrong. Tried to use log_wr_ptr as an
   indicator for how many sectors are currently used. However this isn't correct in every case too.
   The current read logic is aware of that so this shouldn't be necessary anymore.
	sectors_used_a = sectors_total - sectors_free;
	sectors_used_b = (log_wr_ptr + SECTOR_SIZE - 1) / SECTOR_SIZE;
	if (sectors_used_a != sectors_used_b) {
		db(1, "Warning: device reported inconsistent number of used sectors (a=%i, b=%i), "\
		   "using max=%i\n", sectors_used_a, sectors_used_b, MAX(sectors_used_a, sectors_used_b));
	}
	sectors_used = MAX(sectors_used_a, sectors_used_b);
*/
	sectors_used = sectors_total - sectors_free + 1 /*+5*/;

	while (read_at_once > 0 && !(buffer = malloc(SECTOR_SIZE*read_at_once+sizeof(SECTOR_READ_END)+6))) {
		read_at_once--;
	}
	if (read_at_once == 0) fatal(MYNAME ": Can't allocate buffer for reading\n");

	db(1, MYNAME ": Reading log data from device...\n");
	for (i = 0; i < sectors_used; i += sectors_read) {
		for (t = 0, got_bytes = 0; (t < SECTOR_RETRIES) && (got_bytes <= 0); t++) {
			if (atoi(opt_read_at_once) == 0  ||  multi_read_supported == 0) {
				rc = skytraq_read_single_sector(i, buffer);
				if (rc > 0)  got_bytes = rc;
			} else {
				/* Try to read read_at_once sectors at once.
				 * If tere aren't any so many interesting ones, read the remainder (sectors_used-i).
				 * And read at least 1 sector.
				 */
				read_at_once = MAX(MIN(read_at_once, sectors_used-i), 1);

				rc = skytraq_read_multiple_sectors(i, read_at_once, buffer);
				switch (rc) {
				case res_OK:
					got_bytes = read_at_once*SECTOR_SIZE;
					read_at_once = MIN(read_at_once*2, atoi(opt_read_at_once));
					break;

				case res_NACK:
					db(1, MYNAME ": Device doesn't seem to support reading multiple "
						"sectors at once, falling back to single read.\n");
					multi_read_supported = 0;
					break;

				default:
					/* On failure, try with less sectors */
					read_at_once = MAX(read_at_once/2, 1);
				}
			}
		}
		if (got_bytes <= 0) {
			fatal(MYNAME ": Error reading sector %i\n", i);
		}
		sectors_read = (got_bytes-1)/SECTOR_SIZE + 1;

		for (s = 0; s < sectors_read; s++) {
			db(4, MYNAME ": Decoding sector #%i...\n", i+s);
			rc = process_data_sector(&st, buffer+s*SECTOR_SIZE, MIN(got_bytes, SECTOR_SIZE));
			if (rc == 0) {
				db(1, MYNAME ": Empty sector encountered: apparently only %i sectors "
					"used but device reported %i.\n",
					i+s, sectors_used);
				i = sectors_used;	/* terminate to avoid reading stale data still in the logger */
				break;
			} else if (rc >= (4096-18) && i+s+1 >= sectors_used && i+s+1 < sectors_total) {
				db(1, MYNAME ": Last sector is nearly full, reading one more sector (some "
					"devices report free sector count wrong)\n");
				sectors_used++;
			}
		}
	}
	free(buffer);
	db(1, MYNAME ": Got %i trackpoints.\n", st.tpn);
}

static int
skytraq_probe(void)
{
	int baud_rates[] = { 9600, 230400, 115200, 57600, 4800, 19200, 38400 };
	int baud_rates_count = sizeof(baud_rates)/sizeof(baud_rates[0]);
	int initbaud = atoi(opt_initbaud);
	gbuint8 MSG_QUERY_SOFTWARE_VERSION[2] = { 0x02, 0x01 };
	struct {
		gbuint8 id;
		gbuint8 sw_type;
		gbuint8 kernel_ver[4];
		gbuint8 odm_ver[4];
		gbuint8 revision[4];
	} MSG_SOFTWARE_VERSION;
	int i, rc;

	// TODO: get current serial port baud rate and try that first
	// (only sensible if init to 4800 can be disabled...)

	if (initbaud > 0) {
		baud_rates[0] = initbaud;
		baud_rates_count = 1;
	}

	for (i = 0; i < baud_rates_count; i++) {
		db(1, MYNAME ": Probing SkyTraq Venus at %ibaud...\n", baud_rates[i]);

		rd_drain();
		if ((rc = gbser_set_speed(serial_handle, baud_rates[i])) != gbser_OK) {
			db(1, MYNAME ": Set baud rate to %d failed (%d), retrying...\n", baud_rates[i], rc);
			if ((rc = gbser_set_speed(serial_handle, baud_rates[i])) != gbser_OK) {
				db(1, MYNAME ": Set baud rate to %d failed (%d)\n", baud_rates[i], rc);
				continue;
			}
		}

		gb_sleep(50);		/* allow UART to settle. */

		skytraq_wr_msg(MSG_QUERY_SOFTWARE_VERSION,	/* get firmware version */
			sizeof(MSG_QUERY_SOFTWARE_VERSION));
		if ((rc = skytraq_expect_ack(0x02)) != res_OK) {
			db(2, "Didn't receive ACK (%d), retrying...\n", rc);
			skytraq_wr_msg(MSG_QUERY_SOFTWARE_VERSION,	/* get firmware version */
				sizeof(MSG_QUERY_SOFTWARE_VERSION));
			if ((rc = skytraq_expect_ack(0x02)) != res_OK) {
				db(2, "Didn't receive ACK (%d)\n", rc);
				continue;
			}
		}
/*		note: _verify retries on errors, probe takes too long.
		if (skytraq_wr_msg_verify(MSG_QUERY_SOFTWARE_VERSION,   
			sizeof(MSG_QUERY_SOFTWARE_VERSION)) != res_OK)
		{
			continue;
		}*/
		rc = skytraq_expect_msg(0x80, (gbuint8*)&MSG_SOFTWARE_VERSION, sizeof(MSG_SOFTWARE_VERSION));
		if (rc < (int)sizeof(MSG_SOFTWARE_VERSION)) {
			db(2, "Didn't receive expected reply (%d)\n", rc);
		} else {
			db(1, MYNAME ": Venus device found: Kernel version = %i.%i.%i, ODM version = %i.%i.%i, "\
				"revision (Y/M/D) = %02i/%02i/%02i\n",
				MSG_SOFTWARE_VERSION.kernel_ver[1], MSG_SOFTWARE_VERSION.kernel_ver[2],
				MSG_SOFTWARE_VERSION.kernel_ver[3],
				MSG_SOFTWARE_VERSION.odm_ver[1], MSG_SOFTWARE_VERSION.odm_ver[2],
				MSG_SOFTWARE_VERSION.odm_ver[3],
				MSG_SOFTWARE_VERSION.revision[1], MSG_SOFTWARE_VERSION.revision[2],
				MSG_SOFTWARE_VERSION.revision[3]);

			return baud_rates[i];
		}
	}

	return res_NOTFOUND;
}

static int
skytraq_erase()
{
	gbuint8 MSG_LOG_ERASE = 0x19;

	db(1, MYNAME ": Erasing logger memory...\n");
	if (skytraq_wr_msg_verify(&MSG_LOG_ERASE, sizeof(MSG_LOG_ERASE)) != res_OK) {
		db(1, MYNAME ": Didn't receive ACK\n");
		return res_ERROR;
	}

	return res_OK;
}


/*******************************************************************************
* %%%        global callbacks called by gpsbabel main process              %%% *
*******************************************************************************/

static void
skytraq_rd_init(const char *fname)
{
	port = xstrdup(fname);
	if ((serial_handle = gbser_init(fname)) == NULL) {
		fatal(MYNAME ": Can't open port '%s'\n", fname);
	}
	if ((skytraq_baud = skytraq_probe()) <= 0) {
		fatal(MYNAME ": Can't find skytraq device on '%s'\n", fname);
	}
}

static void 
skytraq_rd_deinit(void)
{
	gbser_deinit(serial_handle);
	serial_handle = NULL;
	xfree(port);
}

static void
skytraq_read(void)
{
	int dlbaud;
	route_head *track;

	track = route_head_alloc();
	track->rte_name = xstrdup("SkyTraq tracklog");
	track->rte_desc = xstrdup("SkyTraq GPS tracklog data");
	track_add_head(track);      

	dlbaud = atoi(opt_dlbaud);
	if (dlbaud != skytraq_baud) {
		skytraq_set_baud(dlbaud);
	}

	skytraq_read_tracks(track);

	if (*opt_erase == '1') {
		skytraq_erase();
	}

	skytraq_set_baud(skytraq_baud);		// note that _system_restart resets baud rate anyway...
	skytraq_system_restart();
}

/**************************************************************************/

// capabilities below means: we can only read tracks

ff_vecs_t skytraq_vecs = {
	ff_type_serial,
	{
		ff_cap_read			/* waypoints */,
	  	ff_cap_read 			/* tracks */,
	  	ff_cap_none 			/* routes */
	},
	skytraq_rd_init,
	NULL,
	skytraq_rd_deinit,
	NULL,
	skytraq_read,
	NULL,
	NULL,
	skytraq_args,
	CET_CHARSET_UTF8, 1         /* master process: don't convert anything */
};
/**************************************************************************/
