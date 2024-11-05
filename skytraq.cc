/*

    Serial download of track data from GPS loggers with Skytraq chipset.

    Copyright (C) 2008-2019  Mathias Adam, m.adam (at) adamis.de

    2008         J.C Haessig, jean-christophe.haessig (at) dianosis.org
    2009-09-06 | Josef Reisinger | Added "set target location", i.e. -i skytrag,targetlocation=<lat>:<lng>
    2010-10-23 | Josef Reisinger | Added read/write for miniHomer POI

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

#include <algorithm>       // for max, min
#include <cctype>          // for isprint
#include <cmath>           // for cos, sin, atan2, pow, sqrt
#include <cstdarg>         // for va_end, va_list, va_start
#include <cstdio>          // for sscanf, snprintf, vprintf, SEEK_SET
#include <cstdlib>         // for free
#include <cstring>         // for memset
#include <numbers>         // for inv_pi, pi

#include <QByteArray>      // for QByteArray
#include <QChar>           // for QChar
#include <QLatin1Char>     // for QLatin1Char
#include <QThread>         // for QThread
#include <QtGlobal>        // for qPrintable

#include "defs.h"
#include "skytraq.h"
#include "gbfile.h"        // for gbfclose, gbfopen, gbfread, gbfseek, gbfwrite
#include "gbser.h"         // for gbser_set_speed, gbser_OK, gbser_deinit


#define TIMEOUT			5000
#define SECTOR_SIZE		4096
#define FULL_ITEM_LEN		18
#define COMPACT_ITEM_LEN	8
#define MULTI_HZ_ITEM_LEN		20

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


void
SkytraqBase::dbg(int l, const char* msg, ...)
{
  if (global_opts.debug_level >= l) {
    va_list ap;
    va_start(ap, msg);
    gbVLog(QtDebugMsg, msg, ap);
    va_end(ap);
  }
}

void
SkytraqBase::rd_drain()
{
  if (gbser_flush(serial_handle)) {
    dbg(1, "rd_drain(): Comm error\n");
  }
}

int
SkytraqBase::rd_char(int* errors)
{
  while (*errors > 0) {
    int c = gbser_readc_wait(serial_handle, TIMEOUT);
    if (c < 0) {
      dbg(1, "rd_char(): Got error: %d\n", c);
      (*errors)--;
    } else {
      dbg(4, "rd_char(): Got char: %02x '%c'\n", c, isprint(c) ? c : '.');
      return c;
    }
  }
  gbFatal("Too many read errors on serial port\n");
  return -1;
}

int
SkytraqBase::rd_buf(uint8_t* buf, int len)
{
  char dump[16*3+16+2];

  /* Allow TIMEOUT plus the time needed to actually receive the data bytes:
   * baudrate/10 bytes per second (8 data bits, start and stop bit)
   * TODO: use dlbaud if selected.
   */
  int timeout = TIMEOUT + len;//*1000/(skytraq_baud/10);
  /*TODO: timeout gets <0 e.g. when len~=250000 --> 32bit signed int is too small.
  	if (skytraq_baud > 0)  timeout = TIMEOUT + (long long int)len*1000*10/(long long int)skytraq_baud;
  printf("len=%i  skytraq_baud=%i  timeout=%i\n", len, skytraq_baud, timeout);*/
  int rc = gbser_read_wait(serial_handle, (void*)buf, len, timeout);
  if (rc < 0) {
    dbg(1, "rd_buf(): Read error (%d)\n", rc);
    return res_ERROR;
  } else if (rc < len) {
    dbg(1, "rd_buf(): Read timeout\n");
    return res_ERROR;
  }

  if (global_opts.debug_level >= 4) {
    dbg(4, "rd_buf():  dump follows:\n");
    dump[sizeof(dump)-1] = 0;
    for (int i = 0; i < (len+15)/16*16; i++) {		// count to next 16-byte boundary
      if (i < len) {
        snprintf(&dump[(i%16)*3], 4, "%02x ", buf[i]);
        snprintf(&dump[3*16+1+(i%16)], 2, "%c", isprint(buf[i]) ? buf[i] : '.');
      } else {
        memset(&dump[(i%16)*3], ' ', 3);
        dump[3*16+1+(i%16)] = ' ';
      }
      if ((i+1)%16 == 0) {
        dump[16*3] = ' ';	// gets overwritten with 0 by snprintf
        dbg(4, "%s\n", dump);
      }
    }
  }

  return res_OK;
}

int
SkytraqBase::rd_word()
{
  int errors = 5;		/* allow this many errors */
  int c;
  uint8_t buffer[2];

  c = rd_char(&errors);
  if (c < 0) {
    dbg(1, "rd_word(): Got error: %d\n", c);
    return -1;
  }
  buffer[0] = c;
  c = rd_char(&errors);
  if (c < 0) {
    dbg(1, "rd_word(): Got error: %d\n", c);
    return -1;
  }
  buffer[1] = c;
  /*	if (rd_buf(buffer, 2) != res_OK) {
  		db(1, "rd_word(): Read error\n");
  		return res_ERROR;
  	}*/

  return (buffer[0] << 8) | buffer[1];
}

void
SkytraqBase::wr_char(int c)
{
  int rc;
  dbg(4, "Sending: %02x '%c'\n", (unsigned)c, isprint(c) ? c : '.');
  if (rc = gbser_writec(serial_handle, c), gbser_OK != rc) {
    gbFatal("Write error (%d)\n", rc);
  }
}

void
SkytraqBase::wr_buf(const unsigned char* str, int len)
{
  for (int i = 0; i < len; i++) {
    wr_char(str[i]);
  }
}

/*******************************************************************************
* %%%        SkyTraq protocol implementation                               %%% *
*******************************************************************************/

int
SkytraqBase::skytraq_calc_checksum(const unsigned char* buf, int len)
{
  int cs = 0;
  for (int i = 0; i < len; i++) {
    cs ^= buf[i];
  }
  return cs;
}

int
SkytraqBase::skytraq_rd_msg(void* payload, unsigned int len)
{
  int errors = 5;		// Allow this many receiver errors silently.
  unsigned int c;
  unsigned int i;
  unsigned int state;
  signed int rcv_len;		// Negative length is read error.

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
    dbg(1, "Didn't get message start tag\n");
    return res_ERROR;
  }

  if ((rcv_len = rd_word()) < (signed int)len) {
    if (rcv_len >= 0) {	/* negative values indicate receive errors */
      dbg(1, "Received message too short (got %i bytes, expected %u)\n",
         rcv_len, len);
      return res_PROTOCOL_ERR;
    }
    return res_ERROR;
  }
  /* at this point, we have rcv_len >= len >= 0 */

  dbg(2, "Receiving message with %i bytes of payload (expected >=%u)\n", rcv_len, len);
  rd_buf((uint8_t*) payload, len);

  unsigned int calc_cs = skytraq_calc_checksum((const unsigned char*) payload, len);
  for (i = 0; i < rcv_len-len; i++) {
    c = rd_char(&errors);
    calc_cs ^= c;
  }

  unsigned int rcv_cs = rd_char(&errors);
  if (rcv_cs != calc_cs) {
    gbFatal("Checksum error: got 0x%02x, expected 0x%02x\n", rcv_cs, calc_cs);
  }

  if (rd_word() != 0x0D0A) {
    gbFatal("Didn't get message end tag (CR/LF)\n");
  }

  return res_OK;
}

void
SkytraqBase::skytraq_wr_msg(const uint8_t* payload, int len)
{
  rd_drain();

  wr_buf(MSG_START, sizeof(MSG_START));
  wr_char((len>>8) & 0x0FF);
  wr_char(len & 0x0FF);
  wr_buf(payload, len);

  int cs = skytraq_calc_checksum(payload, len);
  wr_char(cs);
  wr_buf(NL, sizeof(NL));
}

int
SkytraqBase::skytraq_expect_ack(uint8_t id)
{
  uint8_t ack_msg[2];
  //int rcv_len;

  for (int i = 0; i < MSG_RETRIES; i++) {
//		rcv_len = skytraq_rd_msg(ack_msg, sizeof(ack_msg));
//		if (rcv_len == sizeof(ack_msg)) {
    if (skytraq_rd_msg(ack_msg, sizeof(ack_msg)) == res_OK) {
      if (ack_msg[0] == 0x83) {
        if (ack_msg[1] == id) {
          dbg(3, "Got ACK (id=0x%02x)\n", id);
          return res_OK;
        } else if (ack_msg[1] == 0) {
          /* some (all?) devices first send an ACK with id==0, skip that */
          continue;
        } else {
          dbg(1, "Warning: Got unexpected ACK (id=0x%02x)\n", ack_msg[1]);
          continue;
        }
      } else if (ack_msg[0] == 0x84) {
        dbg(3, "Warning: Got NACK (id=0x%02x)\n", ack_msg[1]);
        return res_NACK;
      } else {
        dbg(3, "Warning: Got unexpected message (id=0x%02x), expected ACK (id=0x%02x)\n",
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

int
SkytraqBase::skytraq_expect_msg(uint8_t id, uint8_t* payload, int len)
{
  for (int i = 0; i < MSG_RETRIES; i++) {
    int rc = skytraq_rd_msg(payload, len);
    if (rc < 0) {
      return rc;
    }
    if (payload[0] == id) {
      return len;
    }
  }

  return res_PROTOCOL_ERR;
}

int
SkytraqBase::skytraq_wr_msg_verify(const uint8_t* payload, int len)
{
  for (int i = 0; i < MSG_RETRIES; i++) {
    if (i > 0) {
      dbg(1, "resending msg (id=0x%02x)...\n", payload[0]);
    }
    skytraq_wr_msg(payload, len);
    int rc = skytraq_expect_ack(payload[0]);
    if (rc == res_OK  ||  rc == res_NACK) {
      return rc;
    }
    dbg(1, "Got neither ACK nor NACK, ");
  }
  dbg(1, "aborting (msg id was 0x%02x).\n", payload[0]);

  return res_ERROR;
}

int
SkytraqBase::skytraq_system_restart()
{
  uint8_t MSG_SYSTEM_RESTART[15] =
  { 0x01, 0x01, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };

  dbg(2, "restart system\n");
  return skytraq_wr_msg_verify(MSG_SYSTEM_RESTART, sizeof(MSG_SYSTEM_RESTART));
}

int
SkytraqBase::skytraq_set_baud(int baud)
{
  /* Note: according to AN0003_v3.pdf, attrib == 0x00 means write to SRAM only, however
   * it seems to write to flash too. The Windows software sends 0x02 so we do here too.
   */
  uint8_t MSG_CONFIGURE_SERIAL_PORT[4]
    = { 0x05, 0x00, 0x00, 0x02 };

  dbg(2, "Setting baud rate to %i\n", baud);

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
    gbFatal("Unsupported baud rate: %ibd\n", baud);
  }

  int rc = skytraq_wr_msg_verify(MSG_CONFIGURE_SERIAL_PORT, sizeof(MSG_CONFIGURE_SERIAL_PORT));
  if (rc != res_OK) {
    dbg(2, "Warning: error setting skytraq device baud rate\n");
    return rc;
  }

  dbg(3, "Now setting UART baud rate to %i\n", baud);
  rd_drain();
  if (gbser_set_speed(serial_handle, baud) != gbser_OK) {
    dbg(2, "Warning: error setting uart baud rate\n");
    return res_ERROR;
  }

  QThread::usleep(50);		/* allow UART to settle. */

  return res_OK;
}

int
SkytraqBase::skytraq_configure_logging()
{
  // an0008-1.4.14: logs if
  // (dt > tmin & dd >= dmin & v >= vmin) | dt > tmax | dd > dmax | v > vmax
  unsigned int tmin=6;
  unsigned int tmax=3600;
  unsigned int dmin=0;
  unsigned int dmax=10000;
  static uint8_t MSG_LOG_CONFIGURE_CONTROL[] = {
    0x18,			// message_id
    0x00, 0x00, 0x0e, 0x10,	// max_time: was 0x0000ffff (big endian!)
    0x00, 0x00, 0x00, 0x06,	// min_time: was 0x00000005
    0x00, 0x00, 0x27, 0x10,	// max_distance: was 0x0000ffff
    0x00, 0x00, 0x00, 0x00,	// min_distance
    0x00, 0x00, 0xff, 0xff,	// max_speed
    0x00, 0x00, 0x00, 0x00,	// min_speed
    0x01,			// datalog_enable: NOTE: always ON
    0x00			// reserved
  };

  if (!opt_configure_logging.isEmpty()) {
    unsigned int nn = sscanf(opt_configure_logging.get().toUtf8(), "%u:%u:%u:%u", &tmin, &tmax, &dmin, &dmax);
    if (nn>3) {
      dbg(0, "Reconfiguring logging to: tmin=%u, tmax=%u, dmin=%u, dmax=%u\n", tmin, tmax, dmin, dmax);
      be_write32(MSG_LOG_CONFIGURE_CONTROL+5, tmin);
      be_write32(MSG_LOG_CONFIGURE_CONTROL+1, tmax);
      be_write32(MSG_LOG_CONFIGURE_CONTROL+13, dmin);
      be_write32(MSG_LOG_CONFIGURE_CONTROL+9, dmax);
    } else {
      dbg(1, "Option usage: configlog=tmin:tmax:dmin:dmax");
      return -1;
    }
  }

  return skytraq_wr_msg_verify(MSG_LOG_CONFIGURE_CONTROL, sizeof(MSG_LOG_CONFIGURE_CONTROL));
}

int
SkytraqBase::skytraq_get_log_buffer_status(uint32_t* log_wr_ptr, uint16_t* sectors_free, uint16_t* sectors_total)
{
  uint8_t MSG_LOG_STATUS_CONTROL = 0x17;
  struct {
    uint8_t id[1];
    uint8_t log_wr_ptr[4];
    uint8_t sectors_free[2];
    uint8_t sectors_total[2];
    uint8_t max_time[4], min_time[4], max_dist[4], min_dist[4], max_speed[4], min_speed[4];
    uint8_t datalog_enable[1], log_fifo_mode[1];
  } MSG_LOG_STATUS_OUTPUT;
  unsigned int rc;

  if ((rc = skytraq_wr_msg_verify(&MSG_LOG_STATUS_CONTROL, 1)) != res_OK) {	/* get memory status */
    dbg(1, "Error sending LOG STATUS CONTROL message (%d)\n", rc);
    return res_ERROR;
  }

  rc = skytraq_expect_msg(0x94, (uint8_t*)&MSG_LOG_STATUS_OUTPUT, sizeof(MSG_LOG_STATUS_OUTPUT));
  if (rc < sizeof(MSG_LOG_STATUS_OUTPUT)) {
    dbg(1, "Didn't receive expected reply (%d)\n", rc);
    return res_ERROR;
  }

  *log_wr_ptr = le_readu32(&MSG_LOG_STATUS_OUTPUT.log_wr_ptr);
  *sectors_free = le_readu16(&MSG_LOG_STATUS_OUTPUT.sectors_free);
  *sectors_total = le_readu16(&MSG_LOG_STATUS_OUTPUT.sectors_total);

  // unsigned char log_bool, fifo_mode;
  unsigned int tmax = le_readu32(&MSG_LOG_STATUS_OUTPUT.max_time);
  unsigned int tmin = le_readu32(&MSG_LOG_STATUS_OUTPUT.min_time);
  unsigned int dmax = le_readu32(&MSG_LOG_STATUS_OUTPUT.max_dist);
  unsigned int dmin = le_readu32(&MSG_LOG_STATUS_OUTPUT.min_dist);
  unsigned int vmax = le_readu32(&MSG_LOG_STATUS_OUTPUT.max_speed);
  unsigned int vmin = le_readu32(&MSG_LOG_STATUS_OUTPUT.min_speed);
  // log_bool = *(MSG_LOG_STATUS_OUTPUT.datalog_enable);
  // fifo_mode = *(MSG_LOG_STATUS_OUTPUT.log_fifo_mode);
  dbg(1, "#logging: tmin=%u, tmax=%u, dmin=%u, dmax=%u, vmin=%u, vmax=%u\n", tmin, tmax, dmin, dmax, vmin, vmax);

  return res_OK;
}

/* reads 32-bit "middle-endian" fields */
unsigned int SkytraqBase::me_read32(const unsigned char* p)
{
  return ((unsigned)be_read16(p+2) << 16) | ((unsigned)be_read16(p));
}

QDateTime
SkytraqBase::gpstime_to_qdatetime(int week, int sec)
{
  /* Notes:
   *   * week rollover period can be specified using option
   *     gps-week-rollover, otherwise input timestamps are
   *     assumed to be within previous 1024 weeks from today
   *   * list of leap seconds taken from
   *     <http://maia.usno.navy.mil/ser7/tai-utc.dat>
   *     as of 2019-01-19. Please update when necessary.
   *     Announcement of leap seconds:
   *     <http://hpiers.obspm.fr/iers/bul/bulc/bulletinc.dat>
   *   * leap seconds of 1999 JAN  1 and before are not reflected
   *     here, beware when using this for really old data
   *   * overflow of sec into next week is allowed
   *     (i.e. sec >= 7*24*3600 = 604800 is allowed)
   */
  qint64 gps_timet = 315964800;     /* Jan 06 1980 0:00 UTC */

  int week_rollover = opt_gps_week_rollover.get_result();
  if (week_rollover < 0) {
    int current_week = (QDateTime::currentSecsSinceEpoch() - gps_timet)/
                       (7*SECONDS_PER_DAY);
    week_rollover = current_week/1024 - (week > current_week%1024 ? 1 : 0);
  }
  gps_timet += (week+week_rollover*1024)*7*SECONDS_PER_DAY + sec;

  int override = opt_gps_utc_offset.get_result();
  if (override) {
    gps_timet -= override;
    return QDateTime::fromSecsSinceEpoch(gps_timet, QtUTC);
  }

  /* leap second compensation: */
  gps_timet -= 13;  /* diff GPS-UTC=13s (valid from Jan 01 1999 on) */
  if (gps_timet >= 1136073600) {    /* Jan 01 2006 0:00 UTC */
    gps_timet--;                    /*   GPS-UTC = 14s      */
  }
  if (gps_timet >= 1230768000) {    /* Jan 01 2009 0:00 UTC */
    gps_timet--;                    /*   GPS-UTC = 15s      */
  }
  if (gps_timet >= 1341100800) {    /* Jul 01 2012 0:00 UTC */
    gps_timet--;                    /*   GPS-UTC = 16s      */
  }
  if (gps_timet >= 1435708800) {    /* Jul 01 2015 0:00 UTC */
    gps_timet--;                    /*   GPS-UTC = 17s      */
  }
  if (gps_timet >= 1483228800) {    /* Jan 01 2017 0:00 UTC */
    gps_timet--;                    /*   GPS-UTC = 18s      */
  }
  // Future: Consult http://maia.usno.navy.mil/ser7/tai-utc.dat
  // use http://www.stevegs.com/utils/jd_calc/ for Julian to UNIX sec

  return QDateTime::fromSecsSinceEpoch(gps_timet, QtUTC);
}

void
SkytraqBase::ECEF_to_LLA(double x, double y, long z, double* lat, double* lon, double* alt)
{
  /* constants: */
  constexpr double CA   = 6378137.0;
  constexpr double CB   = 6356752.31424518;
  constexpr double CE2  = (CA*CA - CB*CB) / (CA*CA);    /* =e^2 */
  constexpr double CE_2 = (CA*CA - CB*CB) / (CB*CB);    /* =e'^2 */

  /* auxiliary values: */
  double AP = sqrt(x*x + y*y);
  double ATHETA = atan2(z*CA, AP*CB);

  /* latitude (in radians): */
  *lat = atan2(z + CE_2 * CB * pow(sin(ATHETA), 3), AP - CE2 * CA * pow(cos(ATHETA), 3));

  /* longitude (in radians): */
  *lon = atan2(y, x);

  /* height above ellipsoid (in meters): */
  *alt = AP/cos(*lat) - CA/sqrt(1 - CE2 * pow(sin(*lat), 2));

  *lat = *lat * std::numbers::inv_pi * 180;
  *lon = *lon * std::numbers::inv_pi * 180;
}

void
SkytraqBase::state_init(read_state* pst)
{
  auto* track = new route_head;
  track->rte_name = "SkyTraq tracklog";
  track->rte_desc = "SkyTraq GPS tracklog data";
  track_add_head(track);

  pst->route_head_ = track;
  pst->wpn        = 0;
  pst->tpn        = 0;

  pst->gps_week   = 0;
  pst->gps_sec    = 0;
  pst->x          = 0;
  pst->y          = 0;
  pst->z          = 0;
}

Waypoint*
SkytraqBase::make_trackpoint(read_state* st, double lat, double lon, double alt)
{
  auto* wpt = new Waypoint;

  wpt->shortname = QStringLiteral("TP%1").arg(++st->tpn, 4, 10, QLatin1Char('0'));

  wpt->latitude       = lat;
  wpt->longitude      = lon;
  wpt->altitude       = alt;
  wpt->SetCreationTime(gpstime_to_qdatetime(st->gps_week, st->gps_sec));

  return wpt;
}

#define ITEM_WEEK_NUMBER(item) (item->type_and_speed[1] | ((item->type_and_speed[0] & 0x03) << 8))

#define POW_2_M20 0.000000953674316
#define POW_2_M7 0.0078125

#define ITEM_TYPE(item) (item->type_and_speed[0] >> 4)
#define ITEM_SPEED(item) (item->type_and_speed[1] | ((item->type_and_speed[0] & 0x0F) << 8))

int
SkytraqBase::process_data_item(read_state* pst, const item_frame* pitem, int len)
{
  int res = 0;
  double lat;
  double lon;
  double alt;
  double spe;
  unsigned int ts;
  int poi = 0;
  full_item f;
  compact_item c;
  multi_hz_item m;
  Waypoint* tpt = nullptr;

  switch (ITEM_TYPE(pitem)) {

  case 0xc:	/* POI item (same structure as full) */
    poi = 1;
    [[fallthrough]];

  case 0x2:	/* Multi HZ item */
    if (len < MULTI_HZ_ITEM_LEN) {
      dbg(1, "Not enough bytes in sector for a full item.\n");
      return res_ERROR;
    }
    m.gps_week = ITEM_WEEK_NUMBER(pitem);
    ts = me_read32(pitem->multi_hz.ts);
    m.gps_sec = ((int)(ts & 0x3FFFFFFF)) / 1000;
    m.lat = me_read32(pitem->multi_hz.lat);
    m.lon = me_read32(pitem->multi_hz.lon);
    m.alt = me_read32(pitem->multi_hz.alt);

    pst->gps_week = m.gps_week;
    pst->gps_sec = m.gps_sec;

    spe = KPH_TO_MPS(be_read16(pitem->multi_hz.v_kmh));

    dbg(4, "Got multi hz item: week=%i sec=%i lat=%i  lon=%i  alt=%i  speed=%f\n",
       m.gps_week, m.gps_sec,
       m.lat, m.lon, m.alt,
       spe);

    lat = m.lat * POW_2_M20;
    lon = m.lon * POW_2_M20;
    alt = m.alt * POW_2_M7;

    tpt = make_trackpoint(pst, lat, lon, alt);
    tpt->set_speed(spe); /* convert speed to m/s */
    track_add_wpt(pst->route_head_, tpt);

    res = MULTI_HZ_ITEM_LEN;
    break;

  case 0x6:	/* POI item (same structure as full) */
    poi = 1;
    [[fallthrough]];

  case 0x4:	/* full item */
    if (len < FULL_ITEM_LEN) {
      dbg(1, "Not enough bytes in sector for a full item.\n");
      return res_ERROR;
    }
    ts = me_read32(pitem->full.ts);
    f.gps_week = ts & 0x000003FF;
    f.gps_sec = ts >> 12;
    f.x = me_read32(pitem->full.x);
    f.y = me_read32(pitem->full.y);
    f.z = me_read32(pitem->full.z);

    pst->gps_week = f.gps_week;
    pst->gps_sec = f.gps_sec;
    pst->x = f.x;
    pst->y = f.y;
    pst->z = f.z;

    dbg(4, "Got %s item: week=%i  sec=%i  x=%i  y=%i  z=%i  speed=%i\n",
       poi ? "POI" : "full",
       f.gps_week, f.gps_sec,
       f.x, f.y, f.z,
       ITEM_SPEED(pitem));

    res = FULL_ITEM_LEN;
    break;

  case 0x8:	/* compact item */
    if (len < COMPACT_ITEM_LEN) {
      dbg(1, "Not enough bytes in sector for a compact item.\n");
      return res_ERROR;
    }
    c.dx = (pitem->comp.dpos[1] >> 6) | (pitem->comp.dpos[0] << 2);
    c.dy = (pitem->comp.dpos[1] & 0x3F) | ((pitem->comp.dpos[2] & 0xF0) << 2);
    c.dz = pitem->comp.dpos[3] | ((pitem->comp.dpos[2] & 0x03) << 8);
    if (c.dx > 511) {
      c.dx = 511-c.dx;  /* make proper signed values */
    }
    if (c.dy > 511) {
      c.dy = 511-c.dy;
    }
    if (c.dz > 511) {
      c.dz = 511-c.dz;
    }
    c.dt = (pitem->comp.dt[0] << 8) | pitem->comp.dt[1];

    dbg(4, "Got compact item: dt=%i  dx=%i  dy=%i  dz=%i  speed=%i uu=%i\n",
       c.dt, c.dx, c.dy, c.dz,
       ITEM_SPEED(pitem), (pitem->comp.dpos[2] & 0x0F)>>2);

    pst->gps_sec += c.dt;
    pst->x += c.dx;
    pst->y += c.dy;
    pst->z += c.dz;

    res = COMPACT_ITEM_LEN;
    break;

  default:
    dbg(1, "Unknown item type encountered: 0x%02x\n", ITEM_TYPE(pitem));
    return 0;
  }

  if (res == COMPACT_ITEM_LEN  ||  res == FULL_ITEM_LEN) {
    ECEF_to_LLA(pst->x, pst->y, pst->z, &lat, &lon, &alt);
//		GPS_Math_XYZ_To_WGS84LatLonH(&lat, &lon, &alt, pst->x, pst->y, pst->z);
    tpt = make_trackpoint(pst, lat, lon, alt);
    tpt->set_speed(KPH_TO_MPS(ITEM_SPEED(pitem))); /* convert speed to m/s */

    if (poi) {
      waypt_add(new Waypoint(*tpt));
    }

    if (nullptr == pst->route_head_) {
      dbg(1, "New Track\n");
      pst->route_head_ = new route_head;
      track_add_head(pst->route_head_);
    }

    track_add_wpt(pst->route_head_, tpt);
  }

  return res;
}

int	/* returns number of bytes processed (terminates on 0xFF i.e. empty or padding bytes) */
SkytraqBase::process_data_sector(read_state* pst, const uint8_t* buf, int len)
{
  int plen;
  int ilen;

  for (plen = 0; plen < len  &&  buf[plen] != 0xFF; plen += ilen) {
    ilen = process_data_item(pst, reinterpret_cast<const item_frame*>(&buf[plen]), len-plen);
    if (ilen <= 0) {
      gbFatal("Error %i while processing data item #%i (starts at %i)\n",
            ilen, pst->tpn, plen);
    }
  }

  return plen;
}

/* Note: the buffer is being padded with 0xFFs if necessary so there are always SECTOR_SIZE valid bytes */
int
SkytraqBase::skytraq_read_single_sector(unsigned int sector, uint8_t* buf)
{
  uint8_t MSG_LOG_SECTOR_READ_CONTROL[2] = { 0x1B, (uint8_t)(sector) };
  int errors = 5;		/* allow this many errors */
  unsigned int c;
  unsigned int i;
  unsigned int j;
  unsigned int cs;
  uint8_t buffer[16];

  if (sector > 0xFF) {
    gbFatal("Invalid sector number (%i)\n", sector);
  }

  dbg(2, "Reading sector #%i...\n", sector);

  if (skytraq_wr_msg_verify((uint8_t*)&MSG_LOG_SECTOR_READ_CONTROL, sizeof(MSG_LOG_SECTOR_READ_CONTROL)) != res_OK) {
    dbg(1, "Didn't receive ACK\n");
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
    db(1, "Didn't get sector end tag\n");
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
    dbg(1, "Didn't get sector end tag\n");
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
  dbg(3, "Received %i bytes of log data\n", i);

//#define SINGLE_READ_WORKAROUND
#ifdef SINGLE_READ_WORKAROUND
  gbser_set_speed(serial_handle, skytraq_baud);
  rd_char(&errors);
  rd_char(&errors);
  rd_char(&errors);
  rd_char(&errors);
  rd_char(&errors);
  rd_char(&errors);
  skytraq_set_baud(opt_dlbaud.get_result());
#endif

  cs = skytraq_calc_checksum(buf, i);
  if (cs != buf[i+sizeof(SECTOR_READ_END)]) {
    dbg(1, "Checksum error while reading sector: got 0x%02x, expected 0x%02x\n",
       buf[i+sizeof(SECTOR_READ_END)], cs);
    return res_ERROR;
  }

  for (; i < SECTOR_SIZE; i++) {
    buf[i] = 0xFF;
  }

  return res_OK;
}

int
SkytraqBase::skytraq_read_multiple_sectors(int first_sector, unsigned int sector_count, uint8_t* buf)
{
  uint8_t MSG_LOG_READ_MULTI_SECTORS[5] = { 0x1D };
  unsigned int i;

  if (first_sector < 0  ||  first_sector > 0xFFFF) {
    gbFatal("Invalid sector number (%i)\n", first_sector);
  }
  be_write16(&MSG_LOG_READ_MULTI_SECTORS[1], first_sector);
  if (sector_count > 0xFFFF) {
    gbFatal("Invalid sector count (%i)\n", sector_count);
  }
  be_write16(&MSG_LOG_READ_MULTI_SECTORS[3], sector_count);

  dbg(2, "Reading %i sectors beginning from #%i...\n", sector_count, first_sector);

  unsigned int read_result = skytraq_wr_msg_verify((uint8_t*)&MSG_LOG_READ_MULTI_SECTORS, sizeof(MSG_LOG_READ_MULTI_SECTORS));
  if (read_result != res_OK) {
    return read_result;
  }

  for (i = 0; i < sector_count; i++) {
    dbg(2, "Receiving data of sector #%i...\n", first_sector+i);
    rd_buf(buf+i*SECTOR_SIZE, SECTOR_SIZE);
  }
  rd_buf(buf+SECTOR_SIZE*sector_count, sizeof(SECTOR_READ_END)+6);

  uint8_t* buf_end_tag = buf + SECTOR_SIZE*sector_count;
  for (i = 0; i < sizeof(SECTOR_READ_END); i++) {
    if (buf_end_tag[i] != SECTOR_READ_END[i]) {
      dbg(1, "Wrong end tag: got 0x%02x ('%c'), expected 0x%02x ('%c')\n",
         buf_end_tag[i], isprint(buf_end_tag[i]) ? buf_end_tag[i] : '.',
         SECTOR_READ_END[i], isprint(SECTOR_READ_END[i]) ? SECTOR_READ_END[i] : '.');
      return res_ERROR;
    }
  }

  unsigned int cs = skytraq_calc_checksum(buf, SECTOR_SIZE*sector_count);
  if (cs != buf_end_tag[sizeof(SECTOR_READ_END)]) {
    dbg(1, "Checksum error while reading sector: got 0x%02x, expected 0x%02x\n",
       buf_end_tag[sizeof(SECTOR_READ_END)], cs);
    return res_ERROR;
  }

  return res_OK;
}

void
SkytraqBase::skytraq_read_tracks()
{
  read_state st;
  uint32_t log_wr_ptr;
  uint16_t sectors_free;
  uint16_t sectors_total;
  /* uint16_t sectors_used_a; */
  /* uint16_t sectors_used_b; */
  uint16_t sectors_used;
  int t;
  int rc;
  int got_sectors;
  int total_sectors_read = 0;
  int read_at_once = std::max(opt_read_at_once.get_result(), 1);
  int opt_first_sector_val = opt_first_sector.get_result();
  int opt_last_sector_val = opt_last_sector.get_result();
  int multi_read_supported = 1;
  gbfile* dumpfile = nullptr;

  state_init(&st);

  if (skytraq_get_log_buffer_status(&log_wr_ptr, &sectors_free, &sectors_total) != res_OK) {
    gbFatal("Can't get log buffer status\n");
  }

  dbg(1, "Device status: free sectors: %i / total sectors: %i / %i%% used / write ptr: %i\n",
     sectors_free, sectors_total, 100 - sectors_free*100 / sectors_total, log_wr_ptr);

  if (opt_first_sector_val >= sectors_total) {
    dbg(1, "Warning: sector# specified by option first-sector (%i) is beyond reported total sector count (%i)",
       opt_first_sector_val, sectors_total);
  }
  /* Workaround: sectors_free is sometimes reported wrong. Tried to use log_wr_ptr as an
     indicator for how many sectors are currently used. However this isn't correct in every case too.
     The current read logic is aware of that so this shouldn't be necessary anymore.
  	sectors_used_a = sectors_total - sectors_free;
  	sectors_used_b = (log_wr_ptr + SECTOR_SIZE - 1) / SECTOR_SIZE;
  	if (sectors_used_a != sectors_used_b) {
  		db(1, "Warning: device reported inconsistent number of used sectors (a=%i, b=%i), "\
  		   "using max=%i\n", sectors_used_a, sectors_used_b, std::max(sectors_used_a, sectors_used_b));
  	}
  	sectors_used = std::max(sectors_used_a, sectors_used_b);
  */
  if (opt_last_sector_val < 0) {
    sectors_used = sectors_total - sectors_free + 1 /*+5*/;
    if (opt_first_sector_val >= sectors_used) {
      sectors_used = opt_first_sector_val + 1;
    }
  } else {
    sectors_used = opt_last_sector_val;
    if (opt_last_sector_val >= sectors_total) {
      dbg(1, "Warning: sector# specified by option last-sector (%i) is beyond reported total sector count (%i)",
         opt_last_sector_val, sectors_total);
    }
  }

  auto* buffer = (uint8_t*) xmalloc(SECTOR_SIZE*read_at_once+sizeof(SECTOR_READ_END)+6);
  // m.ad/090930: removed code that tried reducing read_at_once if necessary since doesn't work with xmalloc

  if (opt_dump_file) {
    dumpfile = gbfopen(opt_dump_file, "w");
  }

  dbg(1, "Reading log data from device...\n");
  dbg(1, "start=%d used=%d\n", opt_first_sector_val, sectors_used);
  dbg(1, "opt_last_sector_val=%d\n", opt_last_sector_val);
  for (int i = opt_first_sector_val; i < sectors_used; i += got_sectors) {
    for (t = 0, got_sectors = 0; (t < SECTOR_RETRIES) && (got_sectors <= 0); t++) {
      if (opt_read_at_once.get_result() == 0  ||  multi_read_supported == 0) {
        rc = skytraq_read_single_sector(i, buffer);
        if (rc == res_OK) {
          got_sectors = 1;
        }
      } else {
        /* Try to read read_at_once sectors at once.
         * If there aren't so many interesting ones, read the remainder (sectors_used-i).
         * And read at least 1 sector.
         */
        read_at_once = std::max(std::min(read_at_once, sectors_used-i), 1);

        rc = skytraq_read_multiple_sectors(i, read_at_once, buffer);
        switch (rc) {
        case res_OK:
          got_sectors = read_at_once;
          read_at_once = std::min(read_at_once*2, opt_read_at_once.get_result());
          break;

        case res_NACK:
          dbg(1, "Device doesn't seem to support reading multiple "
             "sectors at once, falling back to single read.\n");
          multi_read_supported = 0;
          break;

        default:
          /* On failure, try with less sectors */
          read_at_once = std::max(read_at_once/2, 1);
        }
      }
    }
    if (got_sectors <= 0) {
      gbFatal("Error reading sector %i\n", i);
    }

    total_sectors_read += got_sectors;

    if (dumpfile) {
      gbfwrite(buffer, SECTOR_SIZE, got_sectors, dumpfile);
    }

    if (opt_no_output) {
      continue;		// skip decoding
    }

    for (int s = 0; s < got_sectors; s++) {
      dbg(4, "Decoding sector #%i...\n", i+s);
      rc = process_data_sector(&st, buffer+s*SECTOR_SIZE, SECTOR_SIZE);
      if (rc == 0) {
        dbg(1, "Empty sector encountered: apparently only %i sectors are "
           "used but device reported %i.\n",
           i+s, sectors_used);
        i = sectors_used;	/* terminate to avoid reading stale data still in the logger */
        break;
      } else if (rc >= (4096-FULL_ITEM_LEN) && i+s+1 >= sectors_used && i+s+1 < sectors_total) {
        dbg(1, "Last sector is nearly full, reading one more sector\n");
        sectors_used++;
      }
    }
  }
  free(buffer);
  dbg(1, "Got %i trackpoints from %i sectors.\n", st.tpn, total_sectors_read);

  if (dumpfile) {
    gbfclose(dumpfile);
  }
}

int
SkytraqBase::skytraq_probe()
{
  int baud_rates[] = { 9600, 230400, 115200, 57600, 4800, 19200, 38400 };
  int baud_rates_count = sizeof(baud_rates)/sizeof(baud_rates[0]);
  int initbaud = opt_initbaud.get_result();
  uint8_t MSG_QUERY_SOFTWARE_VERSION[2] = { 0x02, 0x01 };
  struct {
    uint8_t id;
    uint8_t sw_type;
    uint8_t kernel_ver[4];
    uint8_t odm_ver[4];
    uint8_t revision[4];
  } MSG_SOFTWARE_VERSION;

  // TODO: get current serial port baud rate and try that first
  // (only sensible if init to 4800 can be disabled...)

  if (initbaud > 0) {
    baud_rates[0] = initbaud;
    baud_rates_count = 1;
  }

  for (int i = 0; i < baud_rates_count; i++) {
    dbg(1, "Probing SkyTraq Venus at %ibaud...\n", baud_rates[i]);

    rd_drain();
    if (int rc = gbser_set_speed(serial_handle, baud_rates[i]); rc != gbser_OK) {
      dbg(1, "Set baud rate to %d failed (%d), retrying...\n", baud_rates[i], rc);
      if (int rc = gbser_set_speed(serial_handle, baud_rates[i]); rc != gbser_OK) {
        dbg(1, "Set baud rate to %d failed (%d)\n", baud_rates[i], rc);
        continue;
      }
    }

    QThread::usleep(50);		/* allow UART to settle. */

    skytraq_wr_msg(MSG_QUERY_SOFTWARE_VERSION,	/* get firmware version */
                   sizeof(MSG_QUERY_SOFTWARE_VERSION));
    if (int rc = skytraq_expect_ack(0x02); rc != res_OK) {
      dbg(2, "Didn't receive ACK (%d), retrying...\n", rc);
      skytraq_wr_msg(MSG_QUERY_SOFTWARE_VERSION,	/* get firmware version */
                     sizeof(MSG_QUERY_SOFTWARE_VERSION));
      if (int rc = skytraq_expect_ack(0x02); rc != res_OK) {
        dbg(2, "Didn't receive ACK (%d)\n", rc);
        continue;
      }
    }
    /*		note: _verify retries on errors, probe takes too long.
    		if (skytraq_wr_msg_verify(MSG_QUERY_SOFTWARE_VERSION,
    			sizeof(MSG_QUERY_SOFTWARE_VERSION)) != res_OK)
    		{
    			continue;
    		}*/
    if (int rc = skytraq_expect_msg(0x80, (uint8_t*)&MSG_SOFTWARE_VERSION, sizeof(MSG_SOFTWARE_VERSION));
        rc < (int)sizeof(MSG_SOFTWARE_VERSION)) {
      dbg(2, "Didn't receive expected reply (%d)\n", rc);
    } else {
      dbg(1, "Venus device found: Kernel version = %i.%i.%i, ODM version = %i.%i.%i, "\
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

int
SkytraqBase::skytraq_erase()
{
  uint8_t MSG_LOG_ERASE = 0x19;

  dbg(1, "Erasing logger memory...\n");
  if (skytraq_wr_msg_verify(&MSG_LOG_ERASE, sizeof(MSG_LOG_ERASE)) != res_OK) {
    dbg(1, "Didn't receive ACK\n");
    return res_ERROR;
  }

  return res_OK;
}

void
SkytraqBase::skytraq_set_location()
{
  double lat;
  double lng;
  uint8_t MSG_SET_LOCATION[17] = { 0x36, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
  uint8_t MSG_GET_LOCATION = 0x35;

  dbg(3, "set_location='%s'\n", gbLogCStr(opt_set_location));

  sscanf(opt_set_location.get().toUtf8(), "%lf:%lf", &lat, &lng);
  le_write_double(&MSG_SET_LOCATION[1], lat);
  le_write_double(&MSG_SET_LOCATION[9], lng);
  for (unsigned char i : MSG_SET_LOCATION) {
    dbg(3, "%02x ", i);
  }
  dbg(3, "\n");
  if (skytraq_wr_msg_verify((uint8_t*)&MSG_SET_LOCATION, sizeof(MSG_SET_LOCATION)) != res_OK) {
    gbFatal("cannot set new location\n");
  }
  {
    char buf[32];
    skytraq_wr_msg_verify(&MSG_GET_LOCATION, 1);
    skytraq_rd_msg(buf, 32);
  }
}

/*******************************************************************************
* %%%        global callbacks called by gpsbabel main process              %%% *
*******************************************************************************/

void
SkytraqBase::skytraq_rd_init(const QString& fname)
{
  if ((serial_handle = gbser_init(qPrintable(fname))) == nullptr) {
    gbFatal("Can't open port '%s'\n", gbLogCStr(fname));
  }
  if ((skytraq_baud = skytraq_probe()) <= 0) {
    gbFatal("Can't find skytraq device on '%s'\n", gbLogCStr(fname));
  }
}

void
SkytraqBase::skytraq_rd_deinit()
{
  gbser_deinit(serial_handle);
  serial_handle = nullptr;
}

void
SkytraqBase::skytraq_read()
{
  if (opt_set_location) {
    skytraq_set_location();
    return;
  }

  if (opt_configure_logging) {
    skytraq_configure_logging();
    return;
  }

  int dlbaud = opt_dlbaud.get_result();
  if (dlbaud != 0  &&  dlbaud != skytraq_baud) {
    skytraq_set_baud(dlbaud);
  }

  // read device unless no-output=true and dump-file=0 (i.e. no data needed at all)
  if (!opt_no_output ||  opt_dump_file) {
    skytraq_read_tracks();
  }

  if (opt_erase) {
    skytraq_erase();
  }

  if (dlbaud != 0  &&  dlbaud != skytraq_baud) {
    skytraq_set_baud(skytraq_baud);		// note that _system_restart resets baud rate anyway...
  }
  skytraq_system_restart();
}

void
SkytraqfileFormat::rd_init(const QString& fname)
{
  dbg(1, "Opening file...\n");
  if ((file_handle = gbfopen(fname, "rb")) == nullptr) {
    gbFatal("Can't open file '%s'\n", gbLogCStr(fname));
  }
}

void
SkytraqfileFormat::rd_deinit()
{
  dbg(1, "Closing file...\n");
  gbfclose(file_handle);
  file_handle = nullptr;
}

void
SkytraqfileFormat::read()
{
  read_state st;
  int got_bytes;
  int opt_first_sector_val = opt_first_sector.get_result();
  int opt_last_sector_val = opt_last_sector.get_result();

  state_init(&st);
  auto* buffer = (uint8_t*) xmalloc(SECTOR_SIZE);

  if (opt_first_sector_val > 0) {
    dbg(4, "Seeking to first-sector index %i\n", opt_first_sector_val*SECTOR_SIZE);
    gbfseek(file_handle, opt_first_sector_val*SECTOR_SIZE, SEEK_SET);
  }

  dbg(1, "Reading log data from file...\n");
  int sectors_read = 0;
  while ((got_bytes = gbfread(buffer, 1, SECTOR_SIZE, file_handle)) > 0) {
    dbg(4, "Decoding sector #%i...\n", sectors_read++);
    int rc = process_data_sector(&st, buffer, got_bytes);
    if (opt_last_sector_val < 0) {
      if (rc < (4096-FULL_ITEM_LEN)) {
        dbg(1, "Empty sector encountered, terminating.\n");
        break;
      }
    } else if (sectors_read-1 >= opt_last_sector_val) {
      dbg(1, "desired last-sector #%i reached, terminating.\n", sectors_read-1);
      break;
    }
  }
  xfree(buffer);
  dbg(1, "Got %i trackpoints from %i sectors.\n", st.tpn, sectors_read);
}

/**************************************************************************/
/*
 * support POI of skytraq based miniHomer device
 * http://navin.com.tw/miniHomer.htm
 * 2010-10-23	Josef Reisinger
 */
#define NUMPOI (sizeof poinames/sizeof poinames[0])
#ifdef DEAD_CODE_IS_REBORN
int MinihomerFormat::getPoiByName(char* name)
{
  unsigned int i;
  for (i=0; i<NUMPOI; i++) {
    if (strcmp(poinames[i], name) == 0) {
      return i;
    }
  }
  return -1;
}
#endif
// Convert lla (lat, lng, alt) to ECEF
// Algorithm taken from these sources:
// http://www.mathworks.com/matlabcentral/fileexchange/7942-covert-lat-lon-alt-to-ecef-cartesian
// http://en.wikipedia.org/wiki/Geodetic_system#From_ECEF_to_geodetic
// http://earth-info.nga.mil/GandG/publications/tr8350.2/wgs84fin.pdf
void MinihomerFormat::lla2ecef(double lat, double lng, double alt, double* ecef_x, double* ecef_y, double* ecef_z)
{
  long double a = 6378137.0;
  long double esqr = 6.69437999014e-3;

  long double llat = lat * std::numbers::pi / 180;
  long double llng = lng * std::numbers::pi / 180;
  long double lalt = alt;

  long double s = sin(llat);
  long double n = a / sqrt(1 - esqr * s*s);

  *ecef_x = (double)((n+lalt) * cos(llat) * cos(llng));
  *ecef_y = (double)((n+lalt) * cos(llat) * sin(llng));
  *ecef_z = (double)((n*(1-esqr) + lalt)* sin(llat));
}
void MinihomerFormat::miniHomer_get_poi()
{
  uint8_t MSG_GET_POI[3] = { 0x4D, 0, 0};
  uint8_t buf[32];
  double lat;
  double lng;
  double alt;

  for (unsigned int poi = 0; poi<NUMPOI; poi++) {
    MSG_GET_POI[1]=(poi>>8)&0xff;
    MSG_GET_POI[2]=(poi)&0xff;
    if (skytraq_wr_msg_verify((uint8_t*)&MSG_GET_POI, sizeof(MSG_GET_POI)) != res_OK) {
      gbWarning("cannot read poi %d '%s'\n", poi, poinames[poi]);
    }
    skytraq_rd_msg(buf, 25);
    double ecef_x = be_read_double(buf+1);
    double ecef_y = be_read_double(buf+9);
    double ecef_z = be_read_double(buf+17);

    // todo - how to determine not-set POIs ?
    if (ecef_x < 100.0 && ecef_y < 100.0 && ecef_z < 100.0) {
      dbg(2, "skipped poi %u for X=%f, y=%f, Z=%f\n", poi, ecef_x, ecef_y, ecef_z);
    } else {
      ECEF_to_LLA(ecef_x, ecef_y, ecef_z, &lat, &lng, &alt);

      auto* wpt = new Waypoint;
      wpt->shortname      = QString::asprintf("POI_%s", poinames[poi]);
      wpt->description    = QString::asprintf("miniHomer points to this coordinates if the %s symbol is on", poinames[poi]);
      wpt->latitude       = lat;
      wpt->longitude      = lng;
      wpt->altitude       = alt;
      waypt_add(wpt);
      dbg(1, "got POI[%s]='%f %f %f/%f %f %f'\n", poinames[poi], lat, lng, alt, ecef_x, ecef_y, ecef_z);
    }
  }
}
/*
 * set lla (lat/lng/alt) specified as <lat>:<lng>[:<alt] for a given poi [0..4] in miniHomer
 * returns
 * 1  if poi was set
 * 0  if opt_poi was not set
 * -1 in case of errors
 *  the number of the POI will not be checked - if it is not correct, miniHome will send NACK
 */
int MinihomerFormat::miniHomer_set_poi(uint16_t poinum, const QString& opt_poi)
{
#define MSG_SET_POI_SIZE (sizeof(uint8_t)+sizeof(uint16_t)+3*sizeof(double)+sizeof(uint8_t))
  uint8_t MSG_SET_POI[MSG_SET_POI_SIZE] = {
    0x4C, 0,  0, 		// cmd + poi (u16)
    0, 0, 0, 0, 0, 0, 0, 0,	//lat (double ecef)
    0, 0, 0, 0, 0, 0, 0, 0,	//lng (double ecef)
    0, 0, 0, 0, 0, 0, 0, 0,	//alt (double ecef)
    0 			// attr (u8, 1-> to flash, 0->ro sram)
  };
  double lat;
  double lng;
  double alt;
  double ecef_x;
  double ecef_y;
  double ecef_z;


  int result = 0;		// result will be 0 if opt_poi isn't set
  if (!opt_poi.isEmpty()) { 	// first check opt_poi
    lat=lng=alt=0.0;
    /*
     * parse format of <lat>:<lng>[:alt]
     * we assume at least two elements in the value string
     */
    int n = sscanf(opt_poi.toUtf8(), "%lf:%lf:%lf", &lat, &lng, &alt);
    if (n >= 2) {
      dbg(3, "found %d elems '%s':poi=%s@%d, lat=%f, lng=%f, alt=%f\n", n, gbLogCStr(opt_poi), poinames[poinum], poinum, lat, lng, alt);
      lla2ecef(lat, lng, alt, &ecef_x, &ecef_y, &ecef_z);
      dbg(1, "set POI[%s]='%f %f %f/%f %f %f'\n", poinames[poinum], lat, lng, alt, ecef_x, ecef_y, ecef_z);
      be_write16(MSG_SET_POI+1, poinum);
      be_write_double(MSG_SET_POI+3, ecef_x);
      be_write_double(MSG_SET_POI+11, ecef_y);
      be_write_double(MSG_SET_POI+19, ecef_z);
      MSG_SET_POI[27]=0;
      if (skytraq_wr_msg_verify((uint8_t*)&MSG_SET_POI, sizeof(MSG_SET_POI)) == res_OK) {
        result=1;
      } else {
        gbWarning("cannot set poi %d '%s'\n", poinum, poinames[poinum]);
        result=-1;
      }
    } else {
      gbWarning("argument to %s needs to be like <lat>:<lng>[:<alt>]\n", poinames[poinum]);
      result=-1;
    }
  }
  return result;
}
void
MinihomerFormat::rd_init(const QString& fname)
{
  opt_set_location.reset();	// otherwise it will lead to bus error
  skytraq_rd_init(fname);	// sets global var serial_handle
  mhport=fname;
}
void
MinihomerFormat::rd_deinit()
{
  skytraq_rd_deinit();
  mhport.clear();
}
void
MinihomerFormat::read()
{
  int npoi=0;
  /*
   * read tracks and POI from miniHomer
   */
  if (miniHomer_set_poi(0, opt_set_poi_home) > 0) {
    npoi++;
  }
  if (miniHomer_set_poi(1, opt_set_poi_car) > 0) {
    npoi++;
  }
  if (miniHomer_set_poi(2, opt_set_poi_boat) > 0) {
    npoi++;
  }
  if (miniHomer_set_poi(3, opt_set_poi_heart) > 0) {
    npoi++;
  }
  if (miniHomer_set_poi(4, opt_set_poi_bar) > 0) {
    npoi++;
  }
  if (npoi == 0) {				// do not read if POIs are set (consider set & read distinct operations)
    skytraq_read();				// first read tracks (if not suppressed by cmd line params)
    // we need this call it initialized waypoint list etc...
    skytraq_rd_deinit(); 		// skytraq_read called system_reset, which changes the baud rate.

    skytraq_rd_init(mhport);    // Lets start from scratch and re-init the port
    miniHomer_get_poi();		// add POI as waypoints to the waypoints of the track
  }
}
