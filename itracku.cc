/*
    Copyright (C) 2008 Andreas Grimme, andreas.grimme(at)gmx.net
    Copyright (C) 2005  Robert Lipe, robertlipe+source@gpsbabel.org

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
  This module will download track data from a

  XAiOX iTrackU BLUETOOTH GPS-RECEIVER SiRF III
  http://www.xaiox.com/itracku_sirf3.htm

 Example usage::

   # Read from USB port, output trackpoints & waypoints in GPX format
  ./gpsbabel -i itracku -f com14 -o gpx -F out.gpx

 */
#include <cmath>                   // for lround, round, floor
#include <cstdarg>                 // for va_end, va_list, va_start
#include <cstdio>                  // for fprintf, stderr, SEEK_END, fflush, sscanf, vfprintf
#include <cstdint>
#include <cstring>                 // for memcpy, strcmp, strlen, strncmp
#include <ctime>                   // for gmtime

#include <QtCore/QByteArray>       // for QByteArray
#include <QtCore/QDate>            // for QDate
#include <QtCore/QDateTime>        // for QDateTime
#include <QtCore/QString>          // for QString
#include <QtCore/QTime>            // for QTime
#include <QtCore/Qt>               // for UTC
#include <QtCore/QtGlobal>         // for qPrintable

#include "defs.h"
#include "gbser.h"                 // for gbser_read_line, gbser_write, gbser_deinit, gbser_flush, gbser_init, gbser_is_serial, gbser_read_wait, gbser_ERROR, gbser_OK
#include "gbfile.h"                // for gbfile, gbfclose, gbfopen, gbfseek, gbfread, gbfwrite, gbftell, gbsize_t
#include "src/core/datetime.h"     // for DateTime


#define MYNAME "itracku"

/* memory layout of the iTrackU data record */
struct itracku_data_record {
  uint8_t longitude[4];
  uint8_t latitude[4];
  uint8_t creation_time[4];
  uint8_t altitude[2];
  uint8_t speed;
  uint8_t flag;
};

static int itracku_is_valid_data_record(itracku_data_record* d);
static void to_itracku_data_record(const Waypoint* wp, itracku_data_record* d);
static Waypoint* to_waypoint(itracku_data_record* d);

/* itracku file access */
static void itracku_file_read_data_record(gbfile* fin, itracku_data_record* d);
static uint32_t itracku_file_read_last_time(gbfile* fin);
static void itracku_file_read_waypts(gbfile* fin, void (*waypt_add)(Waypoint* wpt));
static void itracku_file_write_waypt(gbfile* fout, const Waypoint* wpt);

/* itracku device access */
static const unsigned char read_update_data_command[] = { 0x60, 0xb5, 0, 0, 0, 0, 0 }; /* command string to start memory dump */
static const int timeout = 1000; /* timeout for all read operations */
static const char update_end_marker[] = "WP Update Over"; /* end marker for the memory dump */
static const int update_end_marker_size = sizeof(update_end_marker);
#if LATER
static const int port_auto_detect_max_port = 32;
/* Special port name for auto detect. If used, gpsbabel will try to detect the serial
port with the itracku device automatically. */
static const char port_auto_detect_filename[] = "auto:";
#endif

static int update_data_buffer_read_count = 0;
static char update_data_buffer[1024];
static char* update_data_buffer_read;
static char* update_data_buffer_write;
static char* update_data_buffer_end;

static void itracku_device_dump_waypts(void* fd, void (*waypt_add)(Waypoint* wpt));
static int itracku_device_update_data_init();
static int itracku_device_update_data_read(void* buf, int len);
static void itracku_device_write_string(const char* s);
static const char* itracku_device_read_string();

/* global variables */
static void* fd;  /* serial fd */
static gbfile* fin; /* input file handle */
static gbfile* fout; /* output file handle */
static gbfile* fbackup; /* backup file handle */
static uint32_t backup_last_creation_time; /* time of last data record in backup file */
static uint32_t new_waypoint_count; /* count of new waypoints */
static char* port; /* serial port name */
static char* backup_file_name; /* "backup" command option */
static char* only_new; /* "new" command option */

static void
dbg(int l, const char* msg, ...)
{
  va_list ap;
  va_start(ap, msg);
  if (global_opts.debug_level >= l) {
    fprintf(stderr, MYNAME ": ");
    vfprintf(stderr,msg, ap);
    fprintf(stderr, "\n");
    fflush(stderr);
  }
  va_end(ap);
}

static void
itracku_device_write_string(const char* s)
{
  int size = strlen(s) + 1;
  dbg(1, "write to device: %s", s);
  gbser_write(fd, s, size);
}

static const char*
itracku_device_read_string()
{
  const int size = 1024;
  char* s = (char*) xmalloc(size);
  gbser_read_line(fd, s, size, 1000, 0, 0);
  dbg(1, "read from device: %s", s);
  return s;
}

static int
itracku_device_update_data_init()
{
  update_data_buffer_read = update_data_buffer;
  update_data_buffer_write = update_data_buffer;
  update_data_buffer_end = update_data_buffer + sizeof(update_data_buffer);
  update_data_buffer_read_count = 0;
  dbg(1, "start memory dump");
  return 0;
}

static int
itracku_device_update_data_read(void* buf, int len)
{
  if (update_data_buffer_write - update_data_buffer_read >= len) {
    memcpy(buf, update_data_buffer_read, len);
    update_data_buffer_read += len;
    return len;
  }

  if (update_data_buffer_read + update_end_marker_size > update_data_buffer_end) {
    memcpy(update_data_buffer, update_data_buffer_read, update_data_buffer_write - update_data_buffer_read);
    update_data_buffer_write = update_data_buffer + (update_data_buffer_write - update_data_buffer_read);
    update_data_buffer_read = update_data_buffer;
  }

  int rc = gbser_read_wait(fd, update_data_buffer_write, update_data_buffer_end - update_data_buffer_write, timeout);
  if (rc == gbser_ERROR) {
    return 0;
  }

  update_data_buffer_write += rc;
  update_data_buffer_read_count += rc;
  dbg(1, "%5d kbyte read", update_data_buffer_read_count / 1024);

  if (0 == strncmp(update_end_marker, update_data_buffer_write - update_end_marker_size, update_end_marker_size - 1)) {
    dbg(1, "end memory dump");
    return 0;
  }

  return itracku_device_update_data_read(buf, len);
}

/*
	Convert the degrees format of itracku to double.

	itracku stores degrees in a
	32-bit unsigned integer. The lower
	6 digits in 10-base notation denote the
	minutes multiplied by 10000, and digits
	7-9 denote the degrees.

	To express a negative number 0x80000000 is added
	to integer.

	Example: the integer 49347687 is interpreted
	as

	ddmmmmmm
	49347687

	d=49
	m=34.7687

	49 degrees 34.7687 minutes
*/
// The argument is marked 'volatile' because of an issue in Apple's v1.5 clang.
// Without this, the sign of 'x' mysteriously changes while in the function.
// adding a printf inside branches not taken changes the behaviour.   Very
// mysterious, but not worth tracking down at this time.   When xcode 4 comes
// along (or anyone really cares about mega performance of this fairly obscure
// target, we should revisit this.
static double
deg_min_to_deg(volatile uint32_t x)
{
  double sign;
  // determine the sign
  if (x > 0x80000000) {
    sign = -1.0;
    x -= 0x80000000;
  } else {
    sign = 1.0;
  }

  uint32_t sep = 1000000;

  // extract degrees
  uint32_t d = (unsigned int) x / (unsigned int) sep;
  // extract (minutes * 10000)
  uint32_t m10000 = x - d * sep;

  // convert minutes and degrees to a double
  return sign * ((double)d + ((double)m10000) / 600000.0);
}

/*
	Convert degrees to the degrees format of itracku.
*/
static uint32_t
deg_to_deg_min(double x)
{
  int32_t sign;

  // determine sign
  if (x >= 0) {
    sign = 1;
  } else {
    sign = -1;
    x = -x;
  }

  // integer degrees
  double d = floor(x);

  // fractional part
  double f = x - d;

  return
    (uint32_t)d * 1000000 + // multiply integer degrees to shift it to the right digits.
    (uint32_t)round((f * 600000.0)) + // multiply fractional part to convert to minutes and to to shift it to the right digits.
    ((sign > 0) ? 0 : 0x80000000); // add 0x80000000 for negative degrees
}

/*
	Convert the itracku time format to time_t.
*/
static QDateTime
decode_itracku_time(uint32_t date)
{
  int seconds = date & 63;
  int minutes = (date >> 6) & 63;
  int hours = (date >> 12) & 31;
  QTime qtime(hours, minutes, seconds);

  int day = (date >> 17) & 31;
  int month = ((date >> 22) & 15);
  int year = ((date >> 26) & 63) + 2000;
  QDate qdate(year, month, day); 

  return QDateTime(qdate, qtime, Qt::UTC);
}

/*
	Convert time_t to the itracku time format.
*/
static uint32_t
encode_itracku_time(time_t time)
{
  struct tm* t = gmtime(&time);
  return
    (t->tm_sec) +
    (t->tm_min << 6) +
    (t->tm_hour << 12) +
    (t->tm_mday << 17) +
    ((t->tm_mon + 1) << 22) +
    ((t->tm_year - 100) << 26);
}

/*
	Converts a itracku waypoint record to a gpsbabel waypoint.
*/
static Waypoint*
to_waypoint(itracku_data_record* d)
{
  auto* wp = new Waypoint;
  wp->longitude = deg_min_to_deg(le_read32(d->longitude));
  wp->latitude = deg_min_to_deg(le_read32(d->latitude));
  wp->SetCreationTime(decode_itracku_time(le_read32(d->creation_time)));
  wp->speed = KNOTS_TO_MPS((float)d->speed);
  wp->wpt_flags.speed = 1;
  wp->altitude = le_read16(d->altitude);
  return wp;
}

static void
to_itracku_data_record(const Waypoint* wp, itracku_data_record* d)
{
  le_write32(d->longitude, deg_to_deg_min(wp->longitude));
  le_write32(d->latitude, deg_to_deg_min(wp->latitude));
  le_write32(d->creation_time, encode_itracku_time(wp->creation_time.toTime_t()));
  d->speed = round(MPS_TO_KNOTS(wp->speed));
  le_write16(d->altitude, wp->altitude);
  d->flag = 0xff;
}

/*
	Tries to initialize an itracku device attached to
	serial port fd. fd must already be opened.

	Returns gbser_OK if the initialization is successful, a
	non-zero integer otherwise.
*/
static int
init_device()
{
  int rc;
  // verify that we have a MTK based logger...
  dbg(1, "verifying device on port %s", port);

  itracku_device_write_string("WP AP-Exit");
  gbser_flush(fd);
  itracku_device_write_string("W'P Camera Detect");
  const char* greeting = itracku_device_read_string();

  if (0 == strcmp(greeting , "WP GPS+BT")) {
    dbg(1, "device recognised on port %s", port);
    rc = gbser_OK;
  } else {
    dbg(1, "device not recognised on port %s", port);
    rc = gbser_ERROR;
  }
  xfree((void*)greeting);
  return rc;
}

// Any arg in this list will appear in command line help and will be
// populated for you.
// Values for ARGTYPE_xxx can be found in defs.h and are used to
// select the type of option.
static
QVector<arglist_t> itracku_args = {
  { "backup", &backup_file_name, "Appends the input to a backup file", nullptr, ARGTYPE_STRING, ARG_NOMINMAX, nullptr},
  { "new", &only_new, "Only waypoints that are not the backup file", nullptr, ARGTYPE_STRING, ARG_NOMINMAX, nullptr },
//   "default", ARGYTPE_STRING, ARG_NOMINMAX} ,
};

/*******************************************************************************
* %%%        global callbacks called by gpsbabel main process              %%% *
*******************************************************************************/

static void
itracku_rd_init_common(const QString&)
{
  new_waypoint_count = 0;

  if (backup_file_name != nullptr) {
    fbackup = gbfopen(backup_file_name, "a+", MYNAME);
    backup_last_creation_time = itracku_file_read_last_time(fbackup);
    gbfseek(fbackup, 0, SEEK_END);
  } else {
    fbackup = nullptr;
    backup_last_creation_time = 0;
  }
}

static void
itracku_rd_ser_init(const QString& fname)
{
#if LATER
  if (0 == strcmp(qPrintable(fname), port_auto_detect_filename)) {
    dbg(1, "auto detecting port for iTrackU device");
    for (int i=1; !fd && i<port_auto_detect_max_port; ++i) {
      xasprintf(&port, "com%d", i);
      if (!gbser_is_serial(port)) {
        break;
      }
      dbg(1, "trying port %s", port);
      if ((fd = gbser_init(port)) == NULL) {
        dbg(1, "port %s not available.", port);
        continue;
      }

      if (gbser_OK == init_device()) {
        break;
      }

      gbser_deinit(fd);
      fd = NULL;
      xfree(port);
    }
    for (int i=0; !fd && i<port_auto_detect_max_port; ++i) {
      xasprintf(&port, "/dev/ttyUSB%d", i);
      if (!gbser_is_serial(port)) {
        break;
      }
      dbg(1, "trying port %s", port);
      if ((fd = gbser_init(port)) == NULL) {
        dbg(1, "port %s not available.", port);
        continue;
      }

      if (gbser_OK == init_device()) {
        break;
      }

      gbser_deinit(fd);
      fd = NULL;
      xfree(port);
    }
    if (fd == NULL) {
      fatal(MYNAME ": could not find device");
    }
  } else
#endif
  {

    if (gbser_is_serial(qPrintable(fname))) {
      port = xstrdup(qPrintable(fname));

      dbg(1, "opening port %s", qPrintable(fname));
      if ((fd = gbser_init(port)) == nullptr) {
        fatal(MYNAME ": can't initialise port \"%s\"", port);
      }

      if (gbser_OK != init_device()) {
        fatal(MYNAME ": can't initialise device on port \"%s\"", port);
      }
    } else {
      fatal(MYNAME ": \"%s\" is not a valid serial port", qPrintable(fname));
    }
  }

  itracku_rd_init_common(fname);
}

static void
itracku_rd_init(const QString& fname)
{
  fin = gbfopen(fname, "r", MYNAME);
  itracku_rd_init_common(fname);
}

static void
itracku_rd_deinit()
{
  dbg(1, "%d new waypoints", new_waypoint_count);
  if (fd) {
    dbg(3, "closing port %s", port);
    gbser_deinit(fd);
    fd = nullptr;
    xfree(port);
    port = nullptr;
  }
  if (fin) {
    gbfclose(fin);
    fin = nullptr;
  }
  if (fbackup) {
    gbfclose(fbackup);
    fbackup = nullptr;
  }
}

/* Returns true if the waypoint is new, i.e. if it is not already in the
backup file. */
static int
import_data_record(itracku_data_record* d)
{
  int result = 0;

  if (!itracku_is_valid_data_record(d)) {
    result = 0;
  } else {
    if (fbackup) {
      if ((uint32_t)le_read32(d->creation_time) > backup_last_creation_time) {
        backup_last_creation_time = le_read32(d->creation_time);
        gbfwrite(d, sizeof(*d), 1, fbackup);
        result = -1;
      } else {
        result = (only_new == nullptr);
      }
    } else {
      result = -1;
    }
  }
  if (result) {
    ++new_waypoint_count;
  }
  return result;
}

static int
itracku_is_valid_data_record(itracku_data_record* d)
{
  return !(le_read32(d->longitude) == -1);
}

static void
itracku_device_dump_waypts(void* fd, void (*waypt_add)(Waypoint*))
{
  itracku_data_record d;

  dbg(1, "reading memory");
  gbser_write(fd, read_update_data_command, sizeof(read_update_data_command));

  itracku_device_update_data_init();

  while (itracku_device_update_data_read(&d, sizeof(d))) {
    if (itracku_is_valid_data_record(&d)) {
      if (import_data_record(&d)) {
        waypt_add(to_waypoint(&d));
      }
    }
  }
}

static void
itracku_file_read_data_record(gbfile* fin, itracku_data_record* d)
{
  gbfread(d, sizeof(*d), 1, fin);
}

static uint32_t
itracku_file_read_last_time(gbfile* fin)
{
  itracku_data_record d;
  gbsize_t s = sizeof(itracku_data_record);
  gbfseek(fin, 0, SEEK_END);
  if (gbftell(fin) < s) {
    return 0;
  }
  gbfseek(fin, -(int)s, SEEK_END);
  itracku_file_read_data_record(fin, &d);
  return (uint32_t) le_read32(d.creation_time);
}

static void
itracku_file_read_waypts(gbfile* fin, void (*waypt_add)(Waypoint*))
{
  itracku_data_record d;

  while (gbfread(&d, sizeof(d), 1, fin)) {
    if (le_read32(d.longitude) == -1) {
      continue;
    }
    if (import_data_record(&d)) {
      waypt_add(to_waypoint(&d));
    }
  }
}

static void
itracku_file_write_waypt(gbfile* fout, const Waypoint* wpt)
{
  itracku_data_record d;
  to_itracku_data_record(wpt, &d);
  gbfwrite(&d, sizeof(d), 1, fout);
}

static void
itracku_waypt_input(void (*waypt_add)(Waypoint*))
{
  if (fd) {
    itracku_device_dump_waypts(fd, waypt_add);
  } else {
    itracku_file_read_waypts(fin, waypt_add);
  }
}

static void
itracku_read_waypt()
{
  itracku_waypt_input(&waypt_add);
}

static route_head* itracku_read_trk_track;

static void
itracku_read_trk_waypt_add(Waypoint* wpt)
{
  track_add_wpt(itracku_read_trk_track, wpt);
}

static void
itracku_read_trk()
{
  itracku_read_trk_track = new route_head;
  track_add_head(itracku_read_trk_track);
  itracku_waypt_input(&itracku_read_trk_waypt_add);
}

static void
itracku_read()
{
  switch (global_opts.objective) {
  case wptdata:
  case unknown_gpsdata:
    itracku_read_waypt();
    break;
  case trkdata:
    itracku_read_trk();
    break;
  case rtedata:
    fatal(MYNAME ": reading routes is not supported.\n");
    break;
  case posndata:
    break;
  }
}

static void
itracku_wr_init(const QString& fname)
{
  fout = gbfopen(fname, "w", MYNAME);
}

static void
itracku_wr_deinit()
{
  gbfclose(fout);
}

static void
itracku_output_waypoint(const Waypoint* wp)
{
  itracku_file_write_waypt(fout, wp);
}

static void
itracku_write()
{
  waypt_disp_all(itracku_output_waypoint);
}

static void
itracku_rt_init(const QString& fname)
{
  itracku_rd_ser_init(fname);
  itracku_device_write_string("WP AP-Exit");
}

static void
nmea_set_waypoint_time(Waypoint* wpt, struct tm* time, double fsec)
{
  if (time->tm_year == 0) {
    wpt->SetCreationTime(((((time_t)time->tm_hour * 60) + time->tm_min) * 60) + time->tm_sec, lround(1000.0 * fsec));
    if (wpt->wpt_flags.fmt_use == 0) {
      wpt->wpt_flags.fmt_use = 1;
    }
  } else {
    wpt->SetCreationTime(mkgmtime(time), lround(1000.0 * fsec));
    if (wpt->wpt_flags.fmt_use != 0) {
      wpt->wpt_flags.fmt_use = 0;
    }
  }
}

static Waypoint*
gprmc_parse(char* ibuf)
{
  double latdeg, lngdeg;
  char lngdir, latdir;
  double hms;
  char fix;
  unsigned int dmy;
  double speed,course;
  struct tm tm;

  int rc = sscanf(ibuf,"$GPRMC,%lf,%c,%lf,%c,%lf,%c,%lf,%lf,%u",
                  &hms, &fix, &latdeg, &latdir,
                  &lngdeg, &lngdir,
                  &speed, &course, &dmy);

  if (rc == 0) {
    return nullptr;
  }

  double fsec = hms - (int)hms;

  tm.tm_sec = (long) hms % 100;
  hms = hms / 100;
  tm.tm_min = (long) hms % 100;
  hms = hms / 100;
  tm.tm_hour = (long) hms % 100;

  tm.tm_year = dmy % 100 + 100;
  dmy = dmy / 100;
  tm.tm_mon  = dmy % 100 - 1;
  dmy = dmy / 100;
  tm.tm_mday = dmy;

  auto* waypt = new Waypoint;

  WAYPT_SET(waypt, speed, KNOTS_TO_MPS(speed));

  WAYPT_SET(waypt, course, course);

  nmea_set_waypoint_time(waypt, &tm, fsec);

  if (latdir == 'S') {
    latdeg = -latdeg;
  }
  waypt->latitude = ddmm2degrees(latdeg);

  if (lngdir == 'W') {
    lngdeg = -lngdeg;
  }
  waypt->longitude = ddmm2degrees(lngdeg);

  return waypt;
}

/*
	TODO: this function should rather call code from
	nmea.c instead of using a local copy of
	gprmc_parse

	andreas.grimme@gmx.net
*/
static Waypoint*
itracku_rt_position(posn_status*)
{
  char line[1024];
  while (true) {
    gbser_read_line(fd, line, sizeof(line), 5000, 13, 10);
    dbg(1, line);
    Waypoint* wpt = gprmc_parse(line);
    if (wpt) {
      return wpt;
    }
  }
}

static void
itracku_rt_deinit()
{
  itracku_rd_deinit();
}

/**************************************************************************/


ff_vecs_t itracku_vecs = {
  ff_type_serial,
  {
    ff_cap_read /* waypoints */,
    ff_cap_read /* tracks */,
    ff_cap_none /* routes */
  },
  itracku_rd_ser_init,
  nullptr,
  itracku_rd_deinit,
  nullptr,
  itracku_read,
  nullptr,
  nullptr,
  &itracku_args,
  CET_CHARSET_ASCII, 0, /* ascii is the expected character set */
  /* not fixed, can be changed through command line parameter */
  { itracku_rt_init, itracku_rt_position, itracku_rt_deinit, nullptr, nullptr, nullptr },
  nullptr
};

ff_vecs_t itracku_fvecs = {
  ff_type_file,
  {
    (ff_cap)(ff_cap_read | ff_cap_write) /* waypoints */,
    (ff_cap)(ff_cap_read | ff_cap_write) /* tracks */,
    ff_cap_none /* routes */
  },
  itracku_rd_init,
  itracku_wr_init,
  itracku_rd_deinit,
  itracku_wr_deinit,
  itracku_read,
  itracku_write,
  nullptr,
  &itracku_args,
  CET_CHARSET_ASCII, 0, /* ascii is the expected character set */
  /* not fixed, can be changed through command line parameter */
  { nullptr, nullptr, nullptr, nullptr, nullptr, nullptr },
  nullptr
};


/**************************************************************************/
