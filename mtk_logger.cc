/*
  Download track data from GPS loggers based in MTK chipset.

    Copyright (C) 2007 Per Borgentun, e4borgen(at)yahoo.com
    With lot of inspiration from wbt-200.c
       Copyright (C) 2006 Andy Armstrong

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
   --------------------------------------------------------------
  This module will download track data from a MTK chipset based GPS logger.
  It will also convert the raw data.bin file to MTK compatible CSV and
  gpsbabel output of your choice.
  It has been tested with Transystem i-Blue 747 but other devices should
  work as well (Qstarz BT-Q1000, iTrek Z1, ...)

  For more info and tweaks on MTK based loggers
   <http://www.gpspassion.com/forumsen/topic.asp?TOPIC_ID=81990>
   <http://www.gpspassion.com/forumsen/topic.asp?TOPIC_ID=81315>
 For info about the used log format:
  <http://spreadsheets.google.com/pub?key=pyCLH-0TdNe-5N-5tBokuOA&gid=5>

  Module updated 2008-2009. Now also handles Holux M-241 and
  Holux GR-245 aka. GPSport-245 devices. These devices have small differences
  in the log format and use a lower baudrate to transfer the data.

 Example usage::
   # Read from USB port, output trackpoints & waypoints in GPX format
  ./gpsbabel -D 2 -t -w -i mtk -f /dev/ttyUSB0 -o gpx -F out.gpx

   # Parse an existing .bin file (data_2007_09_04.bin), output trackpoints as
   #  both CSV file and GPX, discard points without fix
  ./gpsbabel -D 2 -t -i mtk-bin,csv=data__2007_09_04.csv -f data_2007_09_04.bin -x discard,fixnone -o gpx -F out.gpx

  Tip: Check out the -x height,wgs84tomsl filter to correct the altitude.
  Todo:
    o ....

 */



#include "defs.h"
#include "gbfile.h" /* used for csv output */
#include "gbser.h"
#include <QtCore/QDir>
#include <QtCore/QFile>
#include <QtCore/QThread>
#include <cerrno>
#include <cmath>
#include <cstdlib>
#if __WIN32__
#include <io.h>
#else
#include <unistd.h>
#endif

#define MYNAME "mtk_logger"

#ifdef __WIN32__
#include <io.h>
#define ftruncate _chsize
#endif

/* MTK packet id's -- currently unused... */
enum MTK_NMEA_PACKET {
  PMTK_TEST = 0,
  PMTK_ACK  = 1,
  PMTK_SYS_MSG = 10,
  PMTK_CMD_HOT_START  = 101,
  PMTK_CMD_WARM_START = 102,
  PMTK_CMD_COLD_START = 103,
  PMTK_CMD_FULL_COLD_START  = 104,
  PMTK_CMD_LOG                = 182, /* Data log commands */
  PMTK_SET_NMEA_BAUDRATE      = 251,
  PMTK_API_SET_DGPS_MODE    = 301,
  PMTK_API_SET_SBAS_ENABLED = 313,
  PMTK_API_SET_NMEA_OUTPUT  = 314,
  PMTK_API_SET_PWR_SAV_MODE = 320,
  PMTK_API_SET_DATUM          = 330,
  PMTK_API_SET_DATUM_ADVANCE  = 331,
  PMTK_API_SET_USER_OPTION    = 390,
  PMTK_API_Q_FIX_CTL          = 400,
  PMTK_API_Q_DGPS_MODE    = 401,
  PMTK_API_Q_SBAS_ENABLED = 413,
  PMTK_API_Q_NMEA_OUTPUT  = 414,
  PMTK_API_Q_PWR_SAV_MODE = 420,
  PMTK_API_Q_DATUM            = 430,
  PMTK_API_Q_DATUM_ADVANCE    = 431,
  PMTK_API_GET_USER_OPTION    = 490,
  PMTK_DT_FIX_CTL             = 500,
  PMTK_DT_DGPS_MODE    = 501,
  PMTK_DT_SBAS_ENABLED = 513,
  PMTK_DT_NMEA_OUTPUT  = 514,
  PMTK_DT_PWR_SAV_MODE = 520,
  PMTK_DT_DATUM               = 530,
  PMTK_DT_FLASH_USER_OPTION   = 590,
  PMTK_Q_VERSION       = 604,
  PMTK_Q_RELEASE              = 605,
  PMTK_DT_VERSION      = 704,
  PMTK_DT_RELEASE             = 705
};

static const unsigned char LOG_RST[16] = {
  0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, /* start marker */
  0x00, 0x00, 0x00, 0x00, 0x00,       /* data */
  0xbb, 0xbb, 0xbb, 0xbb
};          /* end marker */

static const char* MTK_ACK[] = { /* Flags returned from PMTK001 ack packet */
  "Invalid packet", "Unsupported packet type",
  "Valid packet but action failed", "Valid packet, action success"
};

#define MTK_EVT_BITMASK  (1<<0x02)
#define MTK_EVT_PERIOD   (1<<0x03)
#define MTK_EVT_DISTANCE (1<<0x04)
#define MTK_EVT_SPEED    (1<<0x05)
#define MTK_EVT_START    (1<<0x07)
#define MTK_EVT_WAYPT    (1<<0x10)  /* Holux waypoint follows... */

/* *************************************** */

/* Data id, type, sizes used by MTK chipset - don't touch.... */
enum {
  UTC = 0,
  VALID,
  LATITUDE,
  LONGITUDE,
  HEIGHT,
  SPEED,
  HEADING,
  DSTA,
  DAGE,
  PDOP,
  HDOP,
  VDOP,
  NSAT,
  SID,
  ELEVATION,
  AZIMUTH,
  SNR,
  RCR,
  MILLISECOND,
  DISTANCE,
} /* DATA_TYPES */;

static struct log_type {
  int id;
  int size;
  const char* name;
} log_type[32] =  {
  { 0, 4, "UTC" },
  { 1, 2, "VALID" },
  { 2, 8, "LATITUDE,N/S"},
  { 3, 8, "LONGITUDE,E/W"},
  { 4, 4, "HEIGHT" },
  { 5, 4, "SPEED" },
  { 6, 4, "HEADING" },
  { 7, 2, "DSTA" },
  { 8, 4, "DAGE" },
  { 9, 2, "PDOP" },
  { 10, 2, "HDOP"},
  { 11, 2, "VDOP"},
  { 12, 2, "NSAT (USED/VIEW)"},
  { 13, 4, "SID",},
  { 14, 2, "ELEVATION" },
  { 15, 2, "AZIMUTH" },
  { 16, 2, "SNR"},
  { 17, 2, "RCR"},
  { 18, 2, "MILLISECOND"},
  { 19, 8, "DISTANCE" },
  { 20, 0, nullptr},
};

struct sat_info {
  char id, used;
  short elevation, azimut, snr;
};

struct data_item {
  time_t timestamp;
  short valid;
  double lat;
  double lon;
  float height;
  float speed;
  float heading;
  short dsta; // differential station id
  float dage;  // differential data age
  float pdop, hdop, vdop;
  char sat_used, sat_view, sat_count;
  short rcr;
  unsigned short timestamp_ms;
  double distance;
  struct sat_info sat_data[32];
};

struct mtk_loginfo {
  unsigned int bitmask;
  int logLen;
  int period, distance, speed; /* in 10:ths of sec, m, km/h */
  int track_event;
};

/* *************************************** */

/* MTK chip based devices with different baudrate, tweaks, ... */
enum MTK_DEVICE_TYPE {
  MTK_LOGGER,
  HOLUX_M241,
  HOLUX_GR245
};

#define TIMEOUT        1500
#define MTK_BAUDRATE 115200
#define MTK_BAUDRATE_M241 38400

#define HOLUX245_MASK (1 << 27)

static void* fd;  /* serial fd */
static FILE* fl;  /* bin.file fd */
static char* port; /* serial port name */
static char* OPT_erase;  /* erase ? command option */
static char* OPT_erase_only;  /* erase_only ? command option */
static char* OPT_log_enable;  /* enable ? command option */
static char* csv_file; /* csv ? command option */
static char* OPT_block_size_kb; /* block_size_kb ? command option */
static enum MTK_DEVICE_TYPE mtk_device = MTK_LOGGER;

static struct mtk_loginfo mtk_info;

const char LIVE_CHAR[4] = {'-', '\\','|','/'};

const char CMD_LOG_DISABLE[]= "$PMTK182,5*20\r\n";
const char CMD_LOG_ENABLE[] = "$PMTK182,4*21\r\n";
const char CMD_LOG_FORMAT[] = "$PMTK182,2,2*39\r\n";
const char CMD_LOG_ERASE[]  = "$PMTK182,6,1*3E\r\n";
const char CMD_LOG_STATUS[] = "$PMTK182,2,7*3C\r\n";

static int  mtk_log_len(unsigned int bitmask);
static void mtk_rd_init(const QString& fname);
static void file_init(const QString& fname);
static void file_deinit() ;
static void holux245_init();
static void file_read();
static int mtk_parse_info(const unsigned char* data, int dataLen);


// Arguments for log fetch 'mtk' command..

static QVector<arglist_t> mtk_sargs = {
  {
    "erase", &OPT_erase, "Erase device data after download",
    "0", ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
  },
  {
    "erase_only", &OPT_erase_only, "Only erase device data, do not download anything",
    "0", ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
  },
  {
    "log_enable", &OPT_log_enable, "Enable logging after download",
    "0", ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
  },
  {
    "csv",   &csv_file, "MTK compatible CSV output file",
    nullptr, ARGTYPE_STRING, ARG_NOMINMAX, nullptr
  },
  {
    "block_size_kb", &OPT_block_size_kb, "Size of blocks in KB to request from device",
    "1", ARGTYPE_INT, "1", "64", nullptr
  },
};

static void dbg(int l, const char* msg, ...)
{
  va_list ap;
  va_start(ap, msg);
  if (global_opts.debug_level >= l) {
    vfprintf(stderr,msg, ap);
    fflush(stderr);
  }
  va_end(ap);
}

// Returns a fully qualified pathname to a temporary file that is a copy
// of the data downloaded from the device. Only two copies are ever in play,
// the primary (e.g. "/tmp/data.bin") and the backup ("/tmp/data_old.bin").
//
// It returns a temporary C string - it's totally kludged in to replace
// TEMP_DATA_BIN being string constants.
static const QString GetTempName(bool backup)
{
  const char kData[]= "data.bin";
  const char kDataBackup[]= "data_old.bin";
  return QDir::tempPath() + QDir::separator() + (backup ? kDataBackup : kData);
}
#define TEMP_DATA_BIN GetTempName(false)
#define TEMP_DATA_BIN_OLD GetTempName(true)

static int do_send_cmd(const char* cmd, int cmdLen)
{
  dbg(6, "Send %s ", cmd);
  int rc = gbser_print(fd, cmd);
  if (rc != gbser_OK) {
    fatal(MYNAME ": Write error (%d)\n", rc);
  }

  return cmdLen;
}


static int do_cmd(const char* cmd, const char* expect, char** rslt, time_t timeout_sec)
{
  char line[256];
  int len;
  int expect_len;
  time_t tout;

  if (expect) {
    expect_len = strlen(expect);
  } else {
    expect_len = 0;
  }

  time(&tout);
  if (timeout_sec > 0) {
    tout += timeout_sec;
  } else {
    tout += 1;
  }

  int cmd_erase = 0;
  if (strncmp(cmd, CMD_LOG_ERASE, 12) == 0) {
    cmd_erase = 1;
    if (global_opts.verbose_status || global_opts.debug_level > 0) {
      fprintf(stderr, "Erasing    ");
    }
  }
  // dbg(6, "## Send '%s' -- Expect '%s' in %d sec\n", cmd, expect, timeout_sec);

  do_send_cmd(cmd, strlen(cmd)); // success or fatal()...

  int done = 0;
  int loops = 0;
  memset(line, '\0', sizeof(line));
  do {
    int rc = gbser_read_line(fd, line, sizeof(line)-1, TIMEOUT, 0x0A, 0x0D);
    if (rc != gbser_OK) {
      if (rc == gbser_TIMEOUT && time(nullptr) > tout) {
        dbg(2, "NMEA command '%s' timeout !\n", cmd);
        return -1;
        // fatal(MYNAME "do_cmd(): Read error (%d)\n", rc);
      }
      len = -1;
    } else {
      len = strlen(line);
    }
    loops++;
    dbg(8, "Read %d bytes: '%s'\n", len, line);
    if (cmd_erase && (global_opts.verbose_status || (global_opts.debug_level > 0 && global_opts.debug_level <= 3))) {
      // erase cmd progress wheel -- only for debug level 1-3
      fprintf(stderr,"\b%c", LIVE_CHAR[loops%4]);
      fflush(stderr);
    }
    if (len > 5 && line[0] == '$') {
      if (expect_len > 0 && strncmp(&line[1], expect, expect_len) == 0) {
        if (cmd_erase && (global_opts.verbose_status || global_opts.debug_level > 0)) {
          fprintf(stderr,"\n");
        }
        dbg(6, "NMEA command success !\n");
        if ((len - 4) > expect_len) {  // alloc and copy data segment...
          if (line[len-3] == '*') {
            line[len-3] = '\0';
          }
          // printf("Data segment: #%s#\n", &line[expect_len+1]);
          if (rslt) {
            *rslt = (char*) xmalloc(len-3-expect_len+1);
            strcpy(*rslt, &line[expect_len+1]);
          }
        }
        done = 1;
      } else if (strncmp(line, "$PMTK", 5) == 0) {
        /* A quick parser for ACK packets */
        if (!cmd_erase && strncmp(line, "$PMTK001,", 9) == 0 && line[9] != '\0') {
          char* pType = &line[9];
          char* pRslt = strchr(&line[9], ',') + 1;
          if (memcmp(&cmd[5], pType, 3) == 0 && pRslt != nullptr && *pRslt != '\0') {
            int pAck = *pRslt - '0';
            if (pAck != 3 && pAck >= 0 && pAck < 4) {  // Erase will return '2'
              dbg(1, "NMEA command '%s' failed - %s\n", cmd, MTK_ACK[pAck]);
              return -1;
            }
          }

        }
        dbg(6, "RECV: '%s'\n", line);
      }

    }
    if (!done && time(nullptr) > tout) {
      dbg(1, "NMEA command '%s' timeout !\n", cmd);
      return -1;
    }
  }  while (len != 0 && loops > 0 && !done);
  return done?0:1;
}

/*******************************************************************************
* %%%        global callbacks called by gpsbabel main process              %%% *
*******************************************************************************/
static void mtk_rd_init_m241(const QString& fname)
{
  mtk_device = HOLUX_M241;
  mtk_rd_init(fname);
}

static void mtk_rd_init(const QString& fname)
{
  int rc;
  char* model;

  port = xstrdup(qPrintable(fname));

  errno = 0;
  dbg(1, "Opening port %s...\n", port);
  if ((fd = gbser_init(port)) == nullptr) {
    fatal(MYNAME ": Can't initialise port \"%s\" (%s)\n", port, strerror(errno));
  }

  // verify that we have a MTK based logger...
  dbg(1, "Verifying MTK based device...\n");

  switch (mtk_device) {
  case HOLUX_M241:
  case HOLUX_GR245:
    log_type[LATITUDE].size = log_type[LONGITUDE].size = 4;
    log_type[HEIGHT].size = 3;
    rc = gbser_set_port(fd, MTK_BAUDRATE_M241, 8, 0, 1);
    break;
  case MTK_LOGGER:
  default:
    rc = gbser_set_port(fd, MTK_BAUDRATE, 8, 0, 1);
    break;
  }
  if (rc) {
    dbg(1, "Set baud rate to %d failed (%d)\n", MTK_BAUDRATE, rc);
    fatal(MYNAME ": Failed to set baudrate !\n");
  }

  rc = do_cmd("$PMTK605*31\r\n", "PMTK705,", &model, 10);
  if (rc != 0) {
    fatal(MYNAME ": This is not a MTK based GPS ! (or is device turned off ?)\n");
  }

  // say hello to GR245 to make it display "USB PROCESSING"
  if (strstr(model, "GR-245")) {
    holux245_init();	// remember we have a GR245 for mtk_rd_deinit()
    rc |= do_cmd("$PHLX810*35\r\n", "PHLX852,", nullptr, 10);
    rc |= do_cmd("$PHLX826*30\r\n", "PHLX859*38", nullptr, 10);
    if (rc != 0) {
      dbg(2, "Greeting not successfull.\n");
    }
  }
  xfree(model);
}

static void mtk_rd_deinit()
{
  if (mtk_device == HOLUX_GR245) {
    int rc = do_cmd("$PHLX827*31\r\n", "PHLX860*32", nullptr, 10);
    if (rc != 0) {
      dbg(2, "Goodbye not successfull.\n");
    }
  }

  dbg(3, "Closing port...\n");
  gbser_deinit(fd);
  fd = nullptr;
  xfree(port);
}

static int mtk_erase()
{
  char* lstatus = nullptr;

  int log_status = 0;
  // check log status - is logging disabled ?
  do_cmd(CMD_LOG_STATUS, "PMTK182,3,7,", &lstatus, 2);
  if (lstatus) {
    log_status = atoi(lstatus);
    dbg(3, "LOG Status '%s'\n", lstatus);
    xfree(lstatus);
    lstatus = nullptr;
  }

  do_cmd(CMD_LOG_FORMAT, "PMTK182,3,2,", &lstatus, 2);
  if (lstatus) {
    int log_mask = strtoul(lstatus, nullptr, 16);
    dbg(3, "LOG Mask '%s' - 0x%.8x \n", lstatus, log_mask);
    xfree(lstatus);
    lstatus = nullptr;
  }

  dbg(1, "Start flash erase..\n");
  do_cmd(CMD_LOG_DISABLE, "PMTK001,182,5,3", nullptr, 1);
  QThread::usleep(10 * 1000);

  // Erase log....
  do_cmd(CMD_LOG_ERASE, "PMTK001,182,6", nullptr, 30);
  QThread::usleep(100 * 1000);

  if ((log_status & 2)) {  // auto-log were enabled before..re-enable log.
    int err = do_cmd(CMD_LOG_ENABLE, "PMTK001,182,4,3", nullptr, 2);
    dbg(3, "re-enable log %s\n", err == 0 ? "Success" : "Fail");
  }
  return 0;
}

static void mtk_read()
{
  char cmd[256];
  char* line = nullptr;
  unsigned char* data = nullptr;
  int i;
  unsigned int bsize;
  unsigned long dpos = 0;
  char* fusage = nullptr;


  if (*OPT_erase_only != '0') {
    mtk_erase();
    return;
  }

  int log_enabled = 0;
  int init_scan = 0;
  FILE* dout = ufopen(TEMP_DATA_BIN, "r+b");
  if (dout == nullptr) {
    dout = ufopen(TEMP_DATA_BIN, "wb");
    if (dout == nullptr) {
      fatal(MYNAME ": Can't create temporary file %s",
            qPrintable(TEMP_DATA_BIN));
    }
  }
  fseek(dout, 0L,SEEK_END);
  unsigned long dsize = ftell(dout);
  if (dsize > 1024) {
    dbg(1, "Temp %s file exists. with size %d\n", qPrintable(TEMP_DATA_BIN),
        dsize);
    dpos = 0;
    init_scan = 1;
  }
  dbg(1, "Download %s -> %s\n", port, qPrintable(TEMP_DATA_BIN));

  // check log status - is logging disabled ?
  do_cmd(CMD_LOG_STATUS, "PMTK182,3,7,", &fusage, 2);
  if (fusage) {
    log_enabled = (atoi(fusage) & 2)?1:0;
    dbg(3, "LOG Status '%s' -- log %s \n", fusage, log_enabled?"enabled":"disabled");
    xfree(fusage);
    fusage = nullptr;
  }

  QThread::usleep(10 * 1000);
  if (true || log_enabled) {
    i = do_cmd(CMD_LOG_DISABLE, "PMTK001,182,5,3", nullptr, 2);
    dbg(3, " ---- LOG DISABLE ---- %s\n", i==0?"Success":"Fail");
  }
  QThread::usleep(100 * 1000);

  unsigned int addr_max = 0;
  // get flash usage, current log address..cmd only works if log disabled.
  do_cmd("$PMTK182,2,8*33\r\n", "PMTK182,3,8,", &fusage, 2);
  if (fusage) {
    addr_max = strtoul(fusage, nullptr, 16);
    if (addr_max > 0) {
      addr_max =  addr_max - addr_max%65536 + 65535;
    }
    xfree(fusage);
  }

  if (addr_max == 0) {   // get flash usage failed...
    addr_max = 0x200000;  // 16Mbit/2Mbyte/32x64kByte block. -- fixme Q1000-ng has 32Mbit
    init_scan = 1;
  }
  dbg(1, "Download %dkB from device\n", (addr_max+1) >> 10);

  if (dsize > addr_max) {
    dbg(1, "Temp %s file (%ld) is larger than data size %d. Data erased since last download !\n", qPrintable(TEMP_DATA_BIN), dsize, addr_max);
    fclose(dout);
    dsize = 0;
    init_scan = 0;
    QFile::rename(TEMP_DATA_BIN, TEMP_DATA_BIN_OLD);
    dout = ufopen(TEMP_DATA_BIN, "wb");
    if (dout == nullptr) {
      fatal(MYNAME ": Can't create temporary file %s",
            qPrintable(TEMP_DATA_BIN));
    }
  }

  unsigned int scan_step = 0x10000;
  unsigned int scan_bsize = 0x0400;
  unsigned int read_bsize_kb = strtol(OPT_block_size_kb, nullptr, 10);
  if (errno == ERANGE || read_bsize_kb < 1) {
    read_bsize_kb = 1;
  } else if (read_bsize_kb > 64) {
    read_bsize_kb = 64;
  }
  unsigned int read_bsize = read_bsize_kb * 1024;
  dbg(2, "Download block size is %d bytes\n", read_bsize);
  if (init_scan) {
    bsize = scan_bsize;
  } else {
    bsize = read_bsize;
  }
  unsigned int addr = 0x0000;

  unsigned int line_size = 2*read_bsize + 32; // logdata as nmea/hex.
  unsigned int data_size = read_bsize + 32;
  if ((line = (char*) xmalloc(line_size)) == nullptr) {
    fatal(MYNAME ": Can't allocate %u bytes for NMEA buffer\n",  line_size);
  }
  if ((data = (unsigned char*) xmalloc(data_size)) ==  nullptr) {
    fatal(MYNAME ": Can't allocate %u bytes for data buffer\n",  data_size);
  }
  memset(line, '\0', line_size);
  memset(data, '\0', data_size);
  int retry_cnt = 0;

  while (init_scan || addr < addr_max) {
    // generate - read address NMEA command, add crc.
    unsigned char crc = 0;
    int cmdLen = snprintf(cmd, sizeof(cmd), "$PMTK182,7,%.8x,%.8x", addr, bsize);
    for (i=1; i<cmdLen; i++) {
      crc ^= cmd[i];
    }

    cmdLen += snprintf(&cmd[cmdLen], sizeof(cmd)-cmdLen,  "*%.2X\r\n", crc);
mtk_retry:
    do_send_cmd(cmd, cmdLen);

    memset(line, '\0', line_size);
    unsigned int rcvd_addr = addr;
    do {
      int rc = gbser_read_line(fd, line, line_size-1, TIMEOUT, 0x0A, 0x0D);
      if (rc != gbser_OK) {
        if (rc == gbser_TIMEOUT && retry_cnt < 3) {
          dbg(2, "\nRetry %d at 0x%.8x\n", retry_cnt, addr);
          retry_cnt++;
          goto mtk_retry;
        } // else
        fatal(MYNAME "mtk_read(): Read error (%d)\n", rc);
      }
      int len = strlen(line);
      dbg(8, "Read %d bytes: '%s'\n", len, line);

      if (len > 0) {
        line[len] = '\0';
        if (strncmp(line, "$PMTK182,8", 10) == 0) { //  $PMTK182,8,00005000,FFFFFFF
          retry_cnt = 0;
          unsigned int data_addr = strtoul(&line[11], nullptr, 16);
          // fixme - we should check if all data before data_addr is already received
          i = 20;
          unsigned int j = data_addr - addr;
          unsigned int ff_len = 0; // number of 0xff bytes.
          unsigned int null_len = 0; // number of 0x00 bytes.
          while (i + 3 < len && j < data_size) {
            data[j] = (isdigit(line[i])?(line[i]-'0'):(line[i]-'A'+0xA))*0x10 +
                      (isdigit(line[i+1])?(line[i+1]-'0'):(line[i+1]-'A'+0xA));
            if (data[j] == 0xff) {
              ff_len++;
            }
            if (data[j] == 0x00) {
              null_len++;
            }
            i += 2;
            j++;
          }
          rcvd_addr = addr + j;
          unsigned int chunk_size = rcvd_addr - data_addr;
          if (init_scan) {
            if (ff_len == chunk_size) {  // data in sector - we've found max sector..
              addr_max = data_addr;
              rcvd_addr = data_addr;
              dbg(1, "Initial scan done - Download %dkB from device\n", (addr_max+1) >> 10);
              break;
            }
          } else {
            if (null_len == chunk_size) {  // 0x00 block - bad block....
              fprintf(stderr, "FIXME -- read bad block at 0x%.6x - retry ? skip ?\n%s\n", data_addr, line);
            }
            if (ff_len == chunk_size) {  // 0xff block - read complete...
              len = ff_len;
              addr_max = data_addr;
              rcvd_addr = data_addr;
              break;
            }
          }
        } else if (strncmp(line, "$PMTK001,182,7,", 15) == 0) {  // Command ACK
          if (line[15] != '3') {
            // fixme - we should timeout here when no log data has been received...
            dbg(2, "\nLog req. failed (%c)\n", line[15]);
            QThread::usleep(10 * 1000);
            retry_cnt++;
            goto mtk_retry;
          }
        }
      }
    } while (rcvd_addr < addr + bsize);

    unsigned int rcvd_bsize = rcvd_addr - addr;
    dbg(2, "Received %d bytes\n", rcvd_bsize);

    if (init_scan) {
      if (dsize > 0 && addr < dsize) {
        fseek(dout, addr, SEEK_SET);
        if (fread(line, 1, rcvd_bsize, dout) == rcvd_bsize && memcmp(line, data, rcvd_bsize) == 0) {
          dpos = addr;
          dbg(2, "%s same at %d\n", qPrintable(TEMP_DATA_BIN), addr);
        } else {
          dbg(2, "%s differs at %d\n", qPrintable(TEMP_DATA_BIN), addr);
          init_scan = 0;
          addr = dpos;
          bsize = read_bsize;
        }
      }
      if (init_scan) {
        addr += scan_step;
        if (addr >= addr_max) {  // initial scan complete...
          init_scan = 0;
          addr = dpos;
          bsize = read_bsize;
        }
      }
    } else {
      fseek(dout, addr, SEEK_SET);
      if (fwrite(data, 1, rcvd_bsize, dout) != rcvd_bsize) {
        fatal(MYNAME ": Failed to write temp. binary file\n");
      }
      addr += rcvd_bsize;
      if (global_opts.verbose_status || (global_opts.debug_level >= 2 && global_opts.debug_level < 5)) {
        int perc = 100 - 100*(addr_max-addr)/addr_max;
        if (addr >= addr_max) {
          perc = 100;
        }
        fprintf(stderr, "\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\bReading 0x%.6x %3d %%", addr, perc);
      }
    }
  }
  if (dout != nullptr) {
#if __WIN32__
    _chsize(fileno(dout), addr_max);
#else
    ftruncate(fileno(dout), addr_max);
#endif
    fclose(dout);
  }
  if (global_opts.verbose_status || (global_opts.debug_level >= 2 && global_opts.debug_level < 5)) {
    fprintf(stderr,"\n");
  }

  // Fixme - Order or. Enable - parse - erase ??
  if (log_enabled || *OPT_log_enable=='1') {
    i = do_cmd(CMD_LOG_ENABLE, "PMTK001,182,4,3", nullptr, 2);
    dbg(3, " ---- LOG ENABLE ----%s\n", i==0?"Success":"Fail");
  } else {
    dbg(1, "Note !!! -- Logging is DISABLED !\n");
  }
  if (line != nullptr) {
    xfree(line);
  }
  if (data != nullptr) {
    xfree(data);
  }

  file_init(TEMP_DATA_BIN);
  file_read();
  file_deinit();

  /* fixme -- we're assuming all went well - erase flash.... */
  if (*OPT_erase != '0') {
    mtk_erase();
  }

}


static route_head*  trk_head = nullptr;
static int add_trackpoint(int idx, unsigned long bmask, struct data_item* itm)
{
  auto* trk = new Waypoint;

  if (global_opts.masked_objective& TRKDATAMASK && (trk_head == nullptr || (mtk_info.track_event & MTK_EVT_START))) {
    char spds[50];
    trk_head = new route_head;
    trk_head->rte_name = QString("track-%1").arg(1 + track_count());

    spds[0] = '\0';
    if (mtk_info.speed > 0) {
      sprintf(spds, " when moving above %.0f km/h", mtk_info.speed/10.);
    }
    trk_head->rte_desc = QString::asprintf("Log every %.0f sec, %.0f m%s"
                                           , mtk_info.period/10., mtk_info.distance/10., spds);
    track_add_head(trk_head);
  }

  if (bmask & (1U<<LATITUDE) && bmask & (1U<<LONGITUDE)) {
    trk->latitude       = itm->lat;
    trk->longitude      = itm->lon;
  } else {
    delete trk;
    return -1; // GPX requires lat/lon...
  }

  if (bmask & (1U<<HEIGHT)) {
    trk->altitude       = itm->height;
  }
  trk->SetCreationTime(itm->timestamp); // in UTC..
  if (bmask & (1U<<MILLISECOND)) {
    trk->creation_time = trk->creation_time.addMSecs(itm->timestamp_ms);
  }

  if (bmask & (1U<<PDOP)) {
    trk->pdop = itm->pdop;
  }
  if (bmask & (1U<<HDOP)) {
    trk->hdop = itm->hdop;
  }
  if (bmask & (1U<<VDOP)) {
    trk->vdop = itm->vdop;
  }

  if (bmask & (1U<<HEADING)) {
    WAYPT_SET(trk, course, itm->heading);
  }
  if (bmask & (1U<<SPEED)) {
    WAYPT_SET(trk, speed, KPH_TO_MPS(itm->speed));
  }
  if (bmask & (1U<<VALID)) {
    switch (itm->valid) {
    case 0x0040:
      trk->fix = fix_unknown;
      break; /* Estimated mode */
    case 0x0001:
      trk->fix = fix_none;
      break; /* "No Fix" */
    case 0x0002:
      trk->fix = fix_3d;
      break; /* "SPS" - 2d/3d ?*/
    case 0x0004:
      trk->fix = fix_dgps;
      break;
    case 0x0008:
      trk->fix = fix_pps;
      break; /* Military GPS */

    case 0x0010: /* "RTK" */
    case 0x0020: /* "FRTK" */
    case 0x0080: /* "Manual input mode"  */
    case 0x0100: /* "Simulator";*/
    default:
      trk->fix = fix_unknown;
      break;
    }
    /* This is a flagrantly bogus position; don't queue it.
     * The 747 does log some "real" positions with fix_none, though,
     * so keep those.
     */
    if ((trk->fix == fix_unknown || trk->fix == fix_none)  &&
        trk->latitude - 90.0 < .000001 && trk->longitude < 0.000001) {
      delete trk;
      return -1;
    }
  }
  if (bmask & (1U<<NSAT)) {
    trk->sat = itm->sat_used;
  }

  // RCR is a bitmask of possibly several log reasons..
  // Holux devices use a Event prefix for each waypt.
  if (global_opts.masked_objective & WPTDATAMASK
      && ((bmask & (1U<<RCR) && itm->rcr & 0x0008)
          || (mtk_info.track_event & MTK_EVT_WAYPT)
         )
     ) {
    /* Button press -- create waypoint, start count at 1 */
    auto* w = new Waypoint(*trk);

    w->shortname = QString::asprintf("WP%06d", waypt_count()+1);
    waypt_add(w);
  }
  // In theory we would not add the waypoint to the list of
  // trackpoints. But as the MTK logger restart the
  // log session from the button press we would loose a
  // trackpoint unless we include/duplicate it.

  if (global_opts.masked_objective & TRKDATAMASK) {
    trk->shortname = QString::asprintf("TP%06d", idx);

    track_add_wpt(trk_head, trk);
  } else {
    delete trk;
  }
  return 0;
}


/********************** MTK Logger -- CSV output *************************/
static gbfile* cd;
static void mtk_csv_init(char* csv_fname, unsigned long bitmask)
{
  FILE* cf;

  dbg(1, "Opening csv output file %s...\n", csv_fname);

  // can't use gbfopen here - it will fatal() if file doesn't exist
  if ((cf = ufopen(QString::fromUtf8(csv_fname), "r")) != nullptr) {
    fclose(cf);
    warning(MYNAME ": CSV file %s already exist ! Cowardly refusing to overwrite.\n", csv_fname);
    return;
  }

  if ((cd = gbfopen(csv_fname, "w", MYNAME)) == nullptr) {
    fatal(MYNAME ": Can't open csv file '%s'\n", csv_fname);
  }

  /* Add the header line */
  gbfprintf(cd, "INDEX,%s%s", ((1U<<RCR) & bitmask)?"RCR,":"",
            ((1U<<UTC) & bitmask)?"DATE,TIME,":"");
  for (int i = 0; i<32; i++) {
    if ((1U<<i) & bitmask) {
      switch (i) {
      case RCR:
      case UTC:
      case MILLISECOND:
        break;
      case SID:
        gbfprintf(cd, "SAT INFO (SID");
        break;
      case ELEVATION:
        gbfprintf(cd, "-ELE");
        break;
      case AZIMUTH:
        gbfprintf(cd, "-AZI");
        break;
      case SNR:
        gbfprintf(cd, "-SNR");
        break;
      default:
        gbfprintf(cd, "%s,", log_type[i].name);
        break;
      }
    }
    if (i == SNR && (1U<<SID) & bitmask) {
      gbfprintf(cd, "),");
    }
  }
  gbfprintf(cd, "\n");
}

static void mtk_csv_deinit()
{
  if (cd != nullptr) {
    gbfclose(cd);
    cd = nullptr;
  }
}

/* Output a single data line in MTK application compatible format - i.e ignore any locale settings... */
static int csv_line(gbfile* csvFile, int idx, unsigned long bmask, struct data_item* itm)
{
  char ts_str[30];
  const char* fix_str = "";

  struct tm* ts_tm = gmtime(&(itm->timestamp));
  strftime(ts_str, sizeof(ts_str)-1, "%Y/%m/%d,%H:%M:%S", ts_tm);

  if (bmask & (1U<<VALID)) {
    switch (itm->valid) {
    case 0x0001:
      fix_str = "No fix";
      break;
    case 0x0002:
      fix_str = "SPS";
      break;
    case 0x0004:
      fix_str = "DGPS";
      break;
    case 0x0008:
      fix_str = "PPS";
      break; /* Military GPS */
    case 0x0010:
      fix_str = "RTK";
      break; /* RealTime Kinematic */
    case 0x0020:
      fix_str = "FRTK";
      break;
    case 0x0040:
      fix_str = "Estimated mode";
      break;
    case 0x0080:
      fix_str = "Manual input mode";
      break;
    case 0x0100:
      fix_str = "Simulator";
      break;
    default:
      fix_str = "???";
      break;
    }
  }
  gbfprintf(csvFile, "%d,", idx);

  // RCR is a bitmask of possibly several log reasons..
  if (bmask & (1U<<RCR))
    gbfprintf(csvFile, "%s%s%s%s,"
              , (itm->rcr&0x0001)?"T":"", (itm->rcr&0x0002)?"S":""
              , (itm->rcr&0x0004)?"D":"", (itm->rcr&0x0008)?"B":"");

  if (bmask & (1U<<UTC)) {
    gbfprintf(csvFile, "%s.%.3d,", ts_str, (bmask & (1U<<MILLISECOND))?itm->timestamp_ms:0);
  }

  if (bmask & (1U<<VALID)) {
    gbfprintf(csvFile, "%s,", fix_str);
  }

  if (bmask & (1U<<LATITUDE | 1U<<LONGITUDE))
    gbfprintf(csvFile, "%.6f,%c,%.6f,%c,", fabs(itm->lat), itm->lat>0?'N':'S',
              fabs(itm->lon), itm->lon>0?'E':'W');

  if (bmask & (1U<<HEIGHT)) {
    gbfprintf(csvFile, "%.3f m,",  itm->height);
  }

  if (bmask & (1U<<SPEED)) {
    gbfprintf(csvFile, "%.3f km/h,", itm->speed);
  }

  if (bmask & (1U<<HEADING)) {
    gbfprintf(csvFile, "%.6f,", itm->heading);
  }

  if (bmask & (1U<<DSTA)) {
    gbfprintf(csvFile, "%d,", itm->dsta);
  }
  if (bmask & (1U<<DAGE)) {
    gbfprintf(csvFile, "%.6f,", itm->dage);
  }

  if (bmask & (1U<<PDOP)) {
    gbfprintf(csvFile, "%.2f,", itm->pdop);
  }
  if (bmask & (1U<<HDOP)) {
    gbfprintf(csvFile, "%.2f,", itm->hdop);  // note bug in MTK appl. 1.02 is output as 1.2 !
  }
  if (bmask & (1U<<VDOP)) {
    gbfprintf(csvFile, "%.2f,", itm->vdop);
  }
  if (bmask & (1U<<NSAT)) {
    gbfprintf(csvFile, "%d(%d),", itm->sat_used, itm->sat_view);
  }

  if (bmask & (1U<<SID)) {
    int do_sc = 0;
    char sstr[40];
    for (int l=0; l<itm->sat_count; l++) {
      int slen = 0;
      slen += sprintf(&sstr[slen], "%s%.2d"
                      , itm->sat_data[l].used?"#":""
                      , itm->sat_data[l].id);
      if (bmask & (1U<<ELEVATION)) {
        slen += sprintf(&sstr[slen], "-%.2d", itm->sat_data[l].elevation);
      }
      if (bmask & (1U<<AZIMUTH)) {
        slen += sprintf(&sstr[slen], "-%.2d", itm->sat_data[l].azimut);
      }
      if (bmask & (1U<<SNR)) {
        slen += sprintf(&sstr[slen], "-%.2d", itm->sat_data[l].snr);
      }

      gbfprintf(csvFile, "%s%s", do_sc?";":"", sstr);
      do_sc = 1;
    }
    gbfprintf(csvFile, ",");
  }

  if (bmask & (1U<<DISTANCE)) {
    gbfprintf(csvFile, "%10.2f m,", itm->distance);
  }

  gbfprintf(csvFile, "\n");
  return 0;
}


/********************* MTK Logger -- Parse functions *********************/
static int mtk_parse(unsigned char* data, int dataLen, unsigned int bmask)
{
  static int count = 0;
  int sat_id;
  int hspd;
  unsigned char hbuf[4];
  struct data_item itm;

  dbg(5,"Entering mtk_parse, count = %i, dataLen = %i\n", count, dataLen);
  if (global_opts.debug_level > 5) {
    fprintf(stderr,"# Data block:");
    for (int j = 0; j<dataLen; j++) {
      fprintf(stderr,"%.2x ", data[j]);
    }
    fprintf(stderr,"\n");
    fflush(stderr);
  }

  memset(&itm, 0, sizeof(itm));
  int i = 0;
  unsigned char crc = 0;
  for (int k = 0; k<32; k++) {
    switch (((1U<<k) & bmask)) {
    case 1U<<UTC:
      itm.timestamp = le_read32(data + i);
      break;
    case 1U<<VALID:
      itm.valid = le_read16(data + i);
      break;
    case 1U<<LATITUDE:
      if (log_type[LATITUDE].size == 4) {
        itm.lat = endian_read_float(data + i, 1 /* le */); // M-241
      } else {
        itm.lat = endian_read_double(data + i, 1 /* le */);
      }
      break;
    case 1U<<LONGITUDE:
      if (log_type[LONGITUDE].size == 4) {
        itm.lon = endian_read_float(data + i, 1 /* le */); // M-241
      } else {
        itm.lon = endian_read_double(data + i, 1 /* le */);
      }
      break;
    case 1U<<HEIGHT:
      switch (mtk_device) {
      case HOLUX_GR245: // Stupid Holux GPsport 245 - log speed as centimeters/sec. (in height position !)
        hspd = data[i] + data[i+1]*0x100 + data[i+2]*0x10000 + data[i+3]*0x1000000;
        itm.speed =  MPS_TO_KPH(hspd)/100.; // convert to km/h..
        break;
      case HOLUX_M241:
        hbuf[0] = 0x0;
        hbuf[1] = *(data + i);
        hbuf[2] = *(data + i + 1);
        hbuf[3] = *(data + i + 2);
        itm.height = endian_read_float(hbuf, 1 /* le */);
        break;
      case MTK_LOGGER:
      default:
        itm.height = endian_read_float(data + i, 1 /* le */);
        break;
      }
      break;
    case 1U<<SPEED:
      if (mtk_device == HOLUX_GR245) {  // Stupid Holux GPsport 245 - log height in speed position...
        hbuf[0] = 0x0;
        hbuf[1] = *(data + i);
        hbuf[2] = *(data + i + 1);
        hbuf[3] = *(data + i + 2);
        itm.height = endian_read_float(hbuf, 1 /* le */);
      } else {
        itm.speed = endian_read_float(data + i, 1 /* le */);
      }
      break;
    case 1U<<HEADING:
      itm.heading = endian_read_float(data + i, 1 /* le */);
      break;
    case 1U<<DSTA:
      itm.dsta = le_read16(data + i);
      break;
    case 1U<<DAGE:  // ?? fixme - is this a float ?
      itm.dage = endian_read_float(data + i, 1 /* le */);
      break;
    case 1U<<PDOP:
      itm.pdop = le_read16(data + i) / 100.;
      break;
    case 1U<<HDOP:
      itm.hdop = le_read16(data + i) / 100.;
      break;
    case 1U<<VDOP:
      itm.vdop = le_read16(data + i) / 100.;
      break;
    case 1U<<NSAT:
      itm.sat_view = data[i];
      itm.sat_used = data[i+1];
      break;
    case 1U<<SID: {
      int sat_count;
      int sat_idx;
      int sid_size;
      int l;
      int azoffset;
      int snroffset;

      sat_count = le_read16(data + i + 2);
      if (sat_count > 32) {
        sat_count = 32;  // this can't happen ? or...
      }

      itm.sat_count = sat_count;
      sid_size = log_type[SID].size;
      azoffset = 0;
      snroffset = 0;
      if (sat_count > 0) {  // handle 'Zero satellites in view issue'
        if (bmask & (1U<<ELEVATION)) {
          sid_size += log_type[ELEVATION].size;
          azoffset += log_type[ELEVATION].size;
          snroffset += log_type[ELEVATION].size;
        }
        if (bmask & (1U<<AZIMUTH)) {
          sid_size += log_type[AZIMUTH].size;
          snroffset += log_type[AZIMUTH].size;
        }
        if (bmask & (1U<<SNR)) {
          sid_size += log_type[SNR].size;
        }
      }
      l = 0;
      sat_idx = 0;
      do {
        sat_id = data[i];
        itm.sat_data[sat_idx].id   = sat_id;
        itm.sat_data[sat_idx].used = data[i + 1];
        // get_word(&data[i+2], &smask); // assume - nr of satellites...

        if (sat_count > 0) {
          if (bmask & (1U<<ELEVATION)) {
            itm.sat_data[sat_idx].elevation = le_read16(data + i + 4);
          }
          if (bmask & (1U<<AZIMUTH)) {
            itm.sat_data[sat_idx].azimut = le_read16(data + i + 4 + azoffset);
          }
          if (bmask & (1U<<SNR)) {
            itm.sat_data[sat_idx].snr    = le_read16(data + i + 4 + snroffset);
          }
        }
        sat_idx++;
        // duplicated checksum and length calculations...for simplicity...
        for (l = 0; l < sid_size; l++) {
          crc ^= data[i + l];
        }
        i += sid_size;
        sat_count--;
      } while (sat_count > 0);
    }
    continue; // dont do any more checksum calc..
    break;
    case 1U<<ELEVATION:
    case 1U<<AZIMUTH:
    case 1U<<SNR:
      // handled in SID
      continue; // avoid checksum calc
      break;
    case 1U<<RCR:
      itm.rcr = le_read16(data + i);
      break;
    case 1U<<MILLISECOND:
      itm.timestamp_ms = le_read16(data + i);
      break;
    case 1U<<DISTANCE:
      itm.distance = endian_read_double(data + i, 1 /* le */);
      break;
    default:
      // if ( ((1U<<k) & bmask) )
      //   printf("Unknown ID %d: %.2x %.2x %.2x %.2x\n", k, data[i], data[i+1], data[i+2], data[i+3]);
      break;
    } /* End: switch (bmap) */

    /* update item checksum and length */
    if (((1U<<k) & bmask)) {
      for (int j = 0; j<log_type[k].size; j++) {
        crc ^= data[i+j];
      }
      i += log_type[k].size;
    }
  } /* for (bmap,...) */

  if (mtk_device == MTK_LOGGER) {   // Holux skips '*' checksum separator
    if (data[i] == '*') {
      i++; // skip '*' separator
    } else {
      dbg(1,"Missing '*' !\n");
      if (data[i] == 0xff) {  // in some case star-crc hasn't been written on power off.
        dbg(1, "Bad data point @0x%.6x - skip %d bytes\n", (fl!=nullptr)?ftell(fl):-1, i+2);
        return i+2; // include '*' and crc
      }
    }
  }
  if (memcmp(&data[0], &LOG_RST[0], 6) == 0
      && memcmp(&data[12], &LOG_RST[12], 4) == 0) {
    mtk_parse_info(data, dataLen);
    dbg(1," Missed Log restart ?? skipping 16 bytes\n");
    return 16;
  }

  if (data[i] != crc) {
    dbg(0,"%2d: Bad CRC %.2x != %.2x (pos 0x%.6x)\n", count, data[i], crc, (fl!=nullptr)?ftell(fl):-1);
  }
  i++; // crc
  count++;

  if (cd != nullptr) {
    csv_line(cd, count, bmask, &itm);
  }

  add_trackpoint(count, bmask, &itm);

  mtk_info.track_event = 0;
  return i;
}

/*
  Description: Parse an info block
  Globals: mtk_info - bitmask/period/speed/... may be affected if updated.
 */
static int mtk_parse_info(const unsigned char* data, int dataLen)
{
  unsigned int bm;

  if (dataLen >= 16
      && memcmp(&data[0], &LOG_RST[0], 6) == 0
      && memcmp(&data[12], &LOG_RST[12], 4) == 0) {

    unsigned short cmd = le_read16(data + 8);
    switch (data[7]) {
    case 0x02:
      bm = le_read32(data + 8);
      dbg(1, "# Log bitmask is: %.8x\n", bm);
      if (mtk_device != MTK_LOGGER) {
        bm &= 0x7fffffffU;
      }
      if (mtk_device == HOLUX_GR245) {
        bm &= ~HOLUX245_MASK;
      }
      if (mtk_info.bitmask != bm) {
        dbg(1," ########## Bitmask Change   %.8x -> %.8x ###########\n", mtk_info.bitmask, bm);
        mtk_info.track_event |= MTK_EVT_BITMASK;
      }
      mtk_info.bitmask = bm;
      mtk_info.logLen = mtk_log_len(mtk_info.bitmask);
      break;
    case 0x03:
      dbg(1, "# Log period change %.0f sec\n", cmd/10.);
      mtk_info.track_event |= MTK_EVT_PERIOD;
      if (mtk_device != MTK_LOGGER) {
        mtk_info.track_event |= MTK_EVT_START;
      }
      mtk_info.period = cmd;
      break;
    case 0x04:
      dbg(1, "# Log distance change %.1f m\n", cmd/10.);
      mtk_info.track_event |= MTK_EVT_DISTANCE;
      if (mtk_device != MTK_LOGGER) {
        mtk_info.track_event |= MTK_EVT_START;
      }
      mtk_info.distance = cmd;
      break;
    case 0x05:
      dbg(1, "# Log speed change %.1f km/h\n", cmd/10.);
      mtk_info.track_event |= MTK_EVT_SPEED;
      mtk_info.speed  = cmd;
      break;
    case 0x06:
      dbg(1, "# Log policy change 0x%.4x\n", cmd);
      if (cmd == 0x01) {
        dbg(1, "# Log policy change to OVERWRITE\n");
      }
      if (cmd == 0x02) {
        dbg(1, "# Log policy change to STOP\n");
      }
      break;
    case 0x07:
      if (cmd == 0x0106) {
        dbg(5, "# GPS Logger# Turned On\n");
        if (mtk_device == MTK_LOGGER) {
          mtk_info.track_event |= MTK_EVT_START;
        }
      }
      if (cmd == 0x0104) {
        dbg(5, "# GPS Logger# Log disabled\n");
      }
      break;
    default:
      dbg(1, "## Unknown INFO 0x%.2x\n", data[7]);
      break;
    }
  } else {
    if (global_opts.debug_level > 0) {
      fprintf(stderr,"#!! Invalid INFO block !! %d bytes\n >> ", dataLen);
      for (bm=0; bm<16; bm++) {
        fprintf(stderr, "%.2x ", data[bm]);
      }
      fprintf(stderr,"\n");
    }
    return 0;
  }
  return 16;
}

static int mtk_log_len(unsigned int bitmask)
{
  int len;

  /* calculate the length of a binary log item. */
  switch (mtk_device) {
  case HOLUX_M241:
  case HOLUX_GR245:
    len = 1; // add crc
    break;
  case MTK_LOGGER:
  default:
    len = 2; // add '*' + crc
    break;
  }
  for (int i = 0; i<32; i++) {
    if ((1U<<i) & bitmask) {
      if (i > DISTANCE && global_opts.debug_level > 0) {
        warning(MYNAME ": Unknown size/meaning of bit %d\n", i);
      }
      if ((i == SID || i == ELEVATION || i == AZIMUTH || i == SNR) && (1U<<SID) & bitmask) {
        len += log_type[i].size*32;  // worst case, max sat. count..
      } else {
        len += log_type[i].size;
      }
    }
  }
  dbg(3, "Log item size %d bytes\n", len);
  return len;
}

/********************** File-in interface ********************************/

static void file_init_m241(const QString& fname)
{
  mtk_device = HOLUX_M241;
  file_init(fname);
}

static void file_init(const QString& fname)
{
  dbg(4, "Opening file %s...\n", qPrintable(fname));
  if (fl = ufopen(fname, "rb"), nullptr == fl) {
    fatal(MYNAME ": Can't open file '%s'\n", qPrintable(fname));
  }
  switch (mtk_device) {
  case HOLUX_M241:
  case HOLUX_GR245:
    log_type[LATITUDE].size = log_type[LONGITUDE].size = 4;
    log_type[HEIGHT].size = 3;
    break;
  default:
    break;
  }
}

static void file_deinit()
{
  dbg(4, "Closing file...\n");
  fclose(fl);
}

static void holux245_init()
{
  mtk_device = HOLUX_GR245;

  // stupid workaround for a broken Holux-245 device....
  // Height & speed have changed position in bitmask and data on Holux 245 Argh !!!
  log_type[HEIGHT].id   = SPEED;
  log_type[HEIGHT].size = 4; // speed size - unit: cm/sec
  log_type[SPEED].id    = HEIGHT;
  log_type[SPEED].size  = 3; // height size..
}

static int is_holux_string(const unsigned char* data, int dataLen)
{
  if (mtk_device != MTK_LOGGER &&
      dataLen >= 5 &&
      data[0] == (0xff & 'H') &&
      data[1] == (0xff & 'O') &&
      data[2] == (0xff & 'L') &&
      data[3] == (0xff & 'U') &&
      data[4] == (0xff & 'X')) {
    return 1;
  }
  return 0;
}

static void file_read()
{
  //  int i, j, k, bLen;
  unsigned char buf[512];

  memset(buf, '\0', sizeof(buf));

  /* Get size of file to  parse */
  fseek(fl, 0L, SEEK_END);
  long fsize = ftell(fl);
  if (fsize <= 0) {
    fatal(MYNAME ": File has size %ld\n", fsize);
  }

  fseek(fl, 0L, SEEK_SET);

  /* Header: 20 bytes
      47 05 | 7f 1e 0e 00 |  04 01 | 32 00 00 00 e8 03 00 00 00 00 00 00

      u16: Log count 'this 64kByte block' - ffff if not complete.
      u32: Bitmask for logging. (default mask)
      u16; ?? ??  Overwrite/Stop policy
      u32:  log period, sec*10
      u32:  log distance , meters*10
      u32:  log speed , km/h*10
   */

  int j = 0;
  long pos = 0;

  /* get default bitmask, log period/speed/distance */
  int bLen = fread(buf, 1, 20, fl);
  if (bLen == 20) {
    unsigned int log_policy = le_read16(buf + 6);

    if (!(log_policy == 0x0104 || log_policy == 0x0106) && fsize > 0x10000) {
      dbg(1, "Invalid initial log policy 0x%.4x - check next block\n", log_policy);
      fseek(fl, 0x10000, SEEK_SET);
      bLen = fread(buf, 1, 20, fl);
      log_policy   = le_read16(buf + 6);
    }
    unsigned int mask = le_read32(buf + 2);
    if (mtk_device != MTK_LOGGER) {   // clear Holux-specific 'low precision' bit
      mask &= 0x7fffffffU;
    }
    unsigned int log_period = le_read32(buf + 8);
    unsigned int log_distance = le_read32(buf + 12);
    unsigned int log_speed = le_read32(buf + 16);

    dbg(1, "Default Bitmask %.8x, Log every %.0f sec, %.0f m, %.0f km/h\n",
        mask, log_period/10., log_distance/10., log_speed/10.);
    mtk_info.bitmask = mask;
    dbg(3, "Using initial bitmask %.8x for parsing the .bin file\n", mtk_info.bitmask);

    mtk_info.period = log_period;
    mtk_info.distance = log_distance;
    mtk_info.speed  = log_speed;
  }
  mtk_info.track_event = 0;

  pos = 0x200; // skip header...first data position
  fseek(fl, pos, SEEK_SET);

  /* read initial info blocks -- if any */
  do {
    bLen = fread(buf, 1, 16, fl);
    j = 0;
    if (buf[0] == 0xaa) {  // pre-validate to avoid error...
      j = mtk_parse_info(buf, bLen);
      pos += j;
    } else if (is_holux_string(buf, bLen)) {
      pos += j;
      // Note -- Holux245 will have <SP><SP><SP><SP> here...handled below..
    }
  } while (j == 16);
  j = bLen;
  pos += j;

  mtk_info.logLen = mtk_log_len(mtk_info.bitmask);
  dbg(3, "Log item size %d bytes\n", mtk_info.logLen);
  if (csv_file && *csv_file) {
    mtk_csv_init(csv_file, mtk_info.bitmask);
  }

  while (pos < fsize && (bLen = fread(&buf[j], 1, sizeof(buf)-j, fl)) > 0) {
    bLen += j;
    int i = 0;
    while ((bLen - i) >= mtk_info.logLen) {
      int k = 0;
      if ((bLen - i) >= 16 && memcmp(&buf[i], &LOG_RST[0], 6) == 0
          && memcmp(&buf[i+12], &LOG_RST[12], 4) == 0) {
        mtk_parse_info(&buf[i], (bLen-i));
        k = 16;
      } else if (is_holux_string(&buf[i], (bLen - i))) {
        if (memcmp(&buf[i+10], "WAYPNT", 6) == 0) {
          mtk_info.track_event |= MTK_EVT_WAYPT;
        }

        k = 16;
        // m241  - HOLUXGR241LOGGER or HOLUXGR241WAYPNT or HOLUXGR241LOGGER<SP><SP><SP><SP>
        // gr245 - HOLUXGR245LOGGER<SP><SP><SP><SP> or HOLUXGR245WAYPNT<SP><SP><SP><SP>
        if (memcmp(&buf[i], "HOLUXGR245", 10) == 0) {
          dbg(2, "Detected Holux GR245 !\n");
          holux245_init();
        }

        // Tobias Verbree reports that an M-12ee is like a 245.
        if (memcmp(&buf[i], "HOLUXM1200", 10) == 0) {
          dbg(2, "Detected Holux HOLUXM1200 !\n");
          holux245_init();
        }

        // skip the 4 spaces that may occur on every device
        if (memcmp(&buf[i+16], "    ", 4) == 0) {  // Assume loglen >= 20...
          k += 4;
        }
      } else if (buf[i] == 0xff && buf[i+1] == 0xff  && buf[i+2] == 0xff && buf[i+3] == 0xff
                 /* && ((pos + 2*mtk_info.logLen) & 0xffff) < mtk_info.logLen */) {
        /* End of 64k block segment -- realign to next data area */

        k = ((pos+mtk_info.logLen+1024)/0x10000) *0x10000 + 0x200;
        i = sizeof(buf);
        if (k <= pos) {
          k += 0x10000;
        }
        dbg(3, "Jump %ld -> %d / 0x%.6x  (fsize %ld)   --- \n", pos, k, k, fsize);
        if (k > fsize) {
          dbg(3, "File parse complete !\n");
          pos = k;
          break;
        } else {
          fseek(fl, k, SEEK_SET);
        }
        pos = k;
        continue;
      } else {
        k = mtk_parse(&buf[i], mtk_info.logLen, mtk_info.bitmask);
      }

      i += k;
      pos += k;
    }
    memmove(buf, &buf[i], sizeof(buf)-i);
    j = sizeof(buf)-i;
  }
  mtk_csv_deinit();

}


/**************************************************************************/
// GPS logger will only handle tracks - neither waypoints or tracks...
// Actually, some of the Holux devices will read waypoints.

ff_vecs_t mtk_vecs = {
  ff_type_serial,
  {
    ff_cap_read 	/* waypoints */,
    ff_cap_read 	/* tracks */,
    ff_cap_none 	/* routes */
  },
  mtk_rd_init,
  nullptr,
  mtk_rd_deinit,
  nullptr,
  mtk_read,
  nullptr,
  nullptr,
  &mtk_sargs,
  CET_CHARSET_ASCII, 0			/* ascii is the expected character set */
  /* not fixed, can be changed through command line parameter */
  , NULL_POS_OPS,
  nullptr
};

ff_vecs_t mtk_m241_vecs = {
  ff_type_serial,
  {
    ff_cap_none 	/* waypoints */,
    ff_cap_read 	/* tracks */,
    ff_cap_none 	/* routes */
  },
  mtk_rd_init_m241,
  nullptr,
  mtk_rd_deinit,
  nullptr,
  mtk_read,
  nullptr,
  nullptr,
  &mtk_sargs,
  CET_CHARSET_ASCII, 0			/* ascii is the expected character set */
  /* not fixed, can be changed through command line parameter */
  , NULL_POS_OPS,
  nullptr
};

/* used for mtk-bin */

static QVector<arglist_t> mtk_fargs = {
  {
    "csv",   &csv_file, "MTK compatible CSV output file",
    nullptr, ARGTYPE_STRING, ARG_NOMINMAX, nullptr
  },
};

ff_vecs_t mtk_fvecs = {
  ff_type_file,
  { ff_cap_read, ff_cap_read, ff_cap_none },
  file_init,
  nullptr,
  file_deinit,
  nullptr,
  file_read,
  nullptr,
  nullptr,
  &mtk_fargs,
  CET_CHARSET_UTF8, 1         /* master process: don't convert anything | CET-REVIEW */
  , NULL_POS_OPS,
  nullptr
};

ff_vecs_t mtk_m241_fvecs = {
  ff_type_file,
  { ff_cap_read, ff_cap_read, ff_cap_none },
  file_init_m241,
  nullptr,
  file_deinit,
  nullptr,
  file_read,
  nullptr,
  nullptr,
  &mtk_fargs,
  CET_CHARSET_UTF8, 1         /* master process: don't convert anything | CET-REVIEW */
  , NULL_POS_OPS,
  nullptr
};
/* End file: mtk_logger.c */
/**************************************************************************/
