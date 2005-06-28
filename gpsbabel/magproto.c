/*
    Communicate Thales/Magellan serial protocol.

    Copyright (C) 2002, 2003, 2004, 2005 Robert Lipe, robertlipe@usa.net

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
#include <time.h>

#include "defs.h"
#include "magellan.h"

int bitrate = 4800;
int wptcmtcnt;
int wptcmtcnt_max;
#define MYNAME "MAGPROTO"
#define MAXCMTCT 200

#define debug_serial  (global_opts.debug_level > 1)

static char * termread(char *ibuf, int size);
static void termwrite(char *obuf, int size);
static void mag_readmsg(void);
static void mag_handon(void);
static void mag_handoff(void);
static void *mkshort_handle = NULL;
static char *deficon = NULL;
static char *bs = NULL;
static char *cmts = NULL;
static char *noack = NULL;
static char *nukewpt = NULL;
static int route_out_count;
static int waypoint_read_count;
static int wpt_len = 8;

typedef enum {
	mrs_handoff = 0,
	mrs_handon,
	mrs_awaiting_ack
} mag_rxstate;

/*
 *   An individual element of a route.
 */
typedef struct mag_rte_elem {
	queue Q;	 		/* My link pointers */
	char *wpt_name;
	char *wpt_icon;
} mag_rte_elem;

/*
 *  A header of a route.  Related elements of a route belong to this.
 */
typedef struct mag_rte_head {
	queue Q;			/* Queue head for child rte_elems */
	int nelems;
} mag_rte_head;

static queue rte_wpt_tmp; /* temporary PGMNWPL msgs for routes */

static FILE *magfile_in;
static FILE *magfile_out;
static int magfd;
static mag_rxstate magrxstate;
static int mag_error;
static unsigned int last_rx_csum;
static int found_done;
static int got_version;
static int is_file = 0;
static route_head *trk_head;
static int ignore_unable;

static waypoint * mag_wptparse(char *);
typedef char * (cleanse_fn) (char *);
static cleanse_fn *mag_cleanse;

static icon_mapping_t gps315_icon_table[] = {
	{ "a", "filled circle" },
	{ "b", "box" },
	{ "c", "red buoy" },
	{ "d", "green buoy" },
	{ "e", "buoy" },
	{ "f", "rocks" },
	{ "g", "red daymark" },
	{ "h", "green daymark" },
	{ "i", "bell" },
	{ "j", "danger" },
	{ "k", "diver down" },
	{ "l", "fish" },
	{ "m", "house" },
	{ "n", "mark" },
	{ "o", "car" },
	{ "p", "tent" },
	{ "q", "boat" },
	{ "r", "food" },
	{ "s", "fuel" },
	{ "t", "tree" },
	{ NULL, NULL }
};

static icon_mapping_t map330_icon_table[] = {
	{ "a", "crossed square" },
	{ "b", "box" },
	{ "c", "house" },
	{ "d", "aerial" },
	{ "e", "airport" },
	{ "f", "amusement park" },
	{ "g", "ATM" },
	{ "g", "Bank" },
	{ "h", "auto repair" },
	{ "i", "boating" },
	{ "j", "camping" },
	{ "k", "exit ramp" },
	{ "l", "first aid" },
	{ "m", "nav aid" },
	{ "n", "buoy" },
	{ "o", "fuel" },
	{ "p", "garden" },
	{ "q", "golf" },
	{ "r", "hotel" },
	{ "s", "hunting/fishing" },
	{ "t", "large city" },
	{ "u", "lighthouse" },
	{ "v", "major city" },
	{ "w", "marina" },
	{ "x", "medium city" },
	{ "y", "museum" },
	{ "z", "obstruction" },
	{ "aa", "park" },
	{ "ab", "resort" },
	{ "ac", "restaurant" },
	{ "ad", "rock" },
	{ "ae", "scuba" },
	{ "af", "RV service" },
	{ "ag", "shooting" },
	{ "ah", "sight seeing" },
	{ "ai", "small city" },
	{ "aj", "sounding" },
	{ "ak", "sports arena" },
	{ "al", "tourist info" },
	{ "am", "truck service" },
	{ "an", "winery" },
	{ "ao", "wreck" },
	{ "ap", "zoo" },
	{ "ah", "Virtual cache"}, 	/* Binos: because you "see" them. */
	{ "ak", "Micro-Cache" },	/* Looks like a film canister. */
	{ "an", "Multi-Cache"}, 	/* Winery: grapes 'coz they "bunch" */
	{ "s",  "Unknown Cache"}, 	/* 'Suprise' cache: use a target. */
	{ "ac",  "Event Cache"}, 	/* Event caches.  May be food. */
	{ NULL, NULL } 
};

pid_to_model_t pid_to_model[] = 
{
	{ mm_gps315320, 19, "ColorTrak" },
	{ mm_gps315320, 24, "GPS 315/320" },
	{ mm_map410, 25, "Map 410" },
	{ mm_map330, 30, "Map 330" },
	{ mm_gps310, 31, "GPS 310" },
	{ mm_meridian, 33, "Meridian" },
	{ mm_meridian, 35, "ProMark 2" },
	{ mm_sportrak, 36, "SporTrak Map/Pro" },
	{ mm_sportrak, 37, "SporTrak" },
	{ mm_meridian, 38, "FX324 Plotter" },
	{ mm_meridian, 39, "Meridian Color" },
	{ mm_meridian, 40, "FX324C Plotter" },
	{ mm_sportrak, 41, "Sportrak Color" },
	{ mm_sportrak, 42, "Sportrak Marine" },
	{ mm_meridian, 43, "Meridian Marine" },
	{ mm_sportrak, 44, "Sportrak Topo" },
	{ mm_sportrak, 45, "Mystic" },
	{ mm_meridian, 46, "MobileMapper" },
	{ mm_unknown, 0, NULL }
};

static icon_mapping_t *icon_mapping = map330_icon_table;

/*
 *   For each receiver type, return a "cleansed" version of the string
 *   that's valid for a waypoint name or comment.   The string should be
 *   freed when you're done with it.
 */
static char *
m315_cleanse(char *istring)
{
	char *rstring = xmalloc(strlen(istring)+1);
	char *i,*o;
	static char m315_valid_chars[] = 
		"ABCDEFGHIJKLMNOPQRSTUVWXYZ 0123456789";
	for (o=rstring,i=istring; *i; i++) {
		if (strchr(m315_valid_chars, toupper(*i))) {
			*o++ = toupper(*i);
		}
	}
	*o = 0;
	return rstring;
}

/*
 * Do same for 330, Meridian, and SportTrak.
 */
char *
m330_cleanse(char *istring)
{
	static char m330_valid_chars[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZ "
			"abcdefghijklmnopqrstuvwxyz"
			"0123456789+-.'/!@#<%^&>()=:\\";
	char *rstring = xmalloc(strlen(istring)+1);
	char *o, *i;

	for (o=rstring,i=istring; *i;i++) {
		if (strchr(m330_valid_chars, *i)) {
			*o++ = *i;
		}
	}
	*o = 0;
	return rstring;
}

/*
 * Given a protocol message, compute the checksum as needed by 
 * the Magellan protocol.
 */
unsigned int 
mag_checksum(const char * const buf)
{
	int csum = 0;
	const char *p;

	for(p = buf; *p; p++) {
		csum  ^= *p;
	}
	
	return csum;
}
static unsigned int
mag_pchecksum(const char * const buf, int len)
{
	int csum = 0;
	const char *p = buf;
	for (; len ; len--) {
		csum ^= *p++;
	}
	return csum;
}

static void
mag_writemsg(const char * const buf)
{
	unsigned int osum = mag_checksum(buf);
	int retry_cnt = 20;
	int i;
	char obuf[1000];

	if (debug_serial) {
		warning("WRITE: $%s*%02X\r\n",buf, osum);
	}

    retry:

	i = sprintf(obuf, "$%s*%02X\r\n",buf, osum);
	termwrite(obuf, i);
	if (magrxstate == mrs_handon || magrxstate == mrs_awaiting_ack) {
		magrxstate = mrs_awaiting_ack;
		mag_readmsg();
		if (last_rx_csum != osum) {
			if (debug_serial) {
				warning("COMM ERROR: Expected %02x, got %02x", 
						osum, last_rx_csum);
			}
			if (retry_cnt--)
				goto retry;
			else {
				mag_handoff();
				fatal(MYNAME 
					": Too many communication errors.\n");
				}
		}
	}
} 

static void
mag_writeack(int osum)
{
	char obuf[200];
	char nbuf[200];
	int i;
	unsigned int nsum;

	if (is_file) {
		return;
	}

	i = sprintf(nbuf, "PMGNCSM,%02X", osum);
	nsum = mag_checksum(nbuf);
	i = sprintf(obuf, "$%s*%02X\r\n",nbuf, nsum);

	if (debug_serial) {
		warning("ACK WRITE: %s",obuf);
	}
	/*
	 * Don't call mag_writemsg here so we don't get into ack feedback
	 * loops.
	 */
	termwrite(obuf, i);
}

static void
mag_handon(void)
{
	if (!is_file) {
		mag_writemsg("PMGNCMD,HANDON");
	}
	magrxstate = mrs_handon;
	
}

static void
mag_handoff(void)
{
	if (!is_file) {
		mag_writemsg("PMGNCMD,HANDOFF");
	}
	magrxstate = mrs_handoff;
}

void
mag_verparse(char *ibuf)
{
	int prodid = mm_unknown; 
	char version[1024];
	pid_to_model_t *pp = pid_to_model;

	got_version = 1;
	sscanf(ibuf,"$PMGNVER,%d,%[^,]", &prodid, version);

	for (pp = pid_to_model; pp->model != mm_unknown; pp++) {
		if (pp->pid == prodid) {
			break;
		}
	}
	switch (pp->model) {
		case mm_gps315320:
		case mm_map410:
			icon_mapping = gps315_icon_table;
			setshort_length(mkshort_handle, 6);
			setshort_mustupper(mkshort_handle, 1);
			mag_cleanse = m315_cleanse;
			break;
		case mm_map330:
		case mm_meridian:
		case mm_sportrak:
			icon_mapping = map330_icon_table;
			setshort_length(mkshort_handle, wpt_len);
			setshort_mustupper(mkshort_handle, 0);
			mag_cleanse = m330_cleanse;
			break;
		default:
			fatal(MYNAME ": Unknown receiver type %d, model version '%s'.\n", prodid, version);
	}
}

#define IS_TKN(x) (strncmp(ibuf,x, sizeof(x)-1) == 0)

static void
mag_readmsg(void)
{
	char ibuf[200];
	int isz;
	unsigned int isum;
	char *isump;
	char *gr;
	int retrycnt = 20;

retry:
	gr = termread(ibuf, sizeof(ibuf));

	if (!gr) {
		if (!got_version) {
			/*
			 * The 315 can take up to six seconds to respond to 
			 * a VERSION command.   Since this is on startup,
			 * we'll be fairly persistent in retrying.
			 */
			if (retrycnt--) {
				goto retry;
			} else {
				fatal(MYNAME ": No data received from GPS.\n");
			}
		} else {
			if (is_file)  {
				found_done = 1;
			}
			return;
		}
	}

	isz = strlen(ibuf);

	if (isz < 5) {
		if (debug_serial)
			warning( "SHORT READ %d\n", isz);
		return;
	}
	mag_error = 0;
	while (!isprint(ibuf[isz]))
		isz--;
	isump = &ibuf[isz-1];
	isum  = strtoul(isump, NULL,16); 
	if (isum != mag_pchecksum(&ibuf[1], isz-3)) {
		if (debug_serial)
			warning( "RXERR %02x/%02x: '%s'\n", isum, mag_pchecksum(&ibuf[1],isz-5), ibuf);
			/* Special case receive errors early on. */
		if (!got_version) {
			fatal(MYNAME ": bad communication.  Check bit rate.\n");
		}
	}
	if (debug_serial) {
		warning( "READ: %s\n", ibuf);
	}
	if (IS_TKN("$PMGNCSM,")) {
		last_rx_csum = strtoul(&ibuf[9], NULL, 16);
		magrxstate = mrs_handon;
		return;
	} 
	if (strncmp(ibuf, "$PMGNWPT,", 7) == 0) {
		waypoint *wpt = mag_wptparse(ibuf);
		waypoint_read_count++;
		if (global_opts.verbose_status) {
			waypt_status_disp(waypoint_read_count, 
					waypoint_read_count);
		}
		switch (global_opts.objective)
		{
			case wptdata:
				waypt_add(wpt);
				break;
			case rtedata:
				ENQUEUE_TAIL(&rte_wpt_tmp, &wpt->Q);
				break;
			default:
				break;
		}
	}
	if (strncmp(ibuf, "$PMGNTRK,", 7) == 0) {
		waypoint *wpt = mag_trkparse(ibuf);
		/*
		 * Allow lazy allocation of track head.
		 */
		if (trk_head == NULL) {
			trk_head = route_head_alloc();
			track_add_head(trk_head);
		}

		route_add_wpt(trk_head, wpt);
	}
	if (strncmp(ibuf, "$PMGNRTE,", 7) == 0) {
		mag_rteparse(ibuf);
	}
	if (IS_TKN("$PMGNVER,")) {
		mag_verparse(ibuf);
	} 
	mag_error = 0;
	if (!ignore_unable && IS_TKN("$PMGNCMD,UNABLE")) {
		warning( "Unable to send\n");
		found_done = 1;
		mag_error = 1;
		ignore_unable = 0;
		return;
	}
	if (IS_TKN("$PMGNCMD,END") || (is_file && (feof(magfile_in)))) {
		found_done = 1;
		return;
	} 
	if (magrxstate != mrs_handoff)
		mag_writeack(isum);
}

/* 
 * termio on Cygwin is apparently broken, so we revert to Windows serial.
 */
#if defined (__WIN32__) || defined (__CYGWIN__)

#include <windows.h>

DWORD 
mkspeed(bitrate)
{
	switch (bitrate) {
		case 1200: return CBR_1200;
		case 2400: return CBR_2400;
		case 4800: return CBR_4800;
		case 9600: return CBR_9600;
		case 19200: return CBR_19200;
		case 57600: return CBR_57600;
		case 115200: return CBR_115200;
		default: return CBR_4800;
	}
}

HANDLE comport = NULL;

#define xCloseHandle(a) if (a) { CloseHandle(a); } a = NULL;

static
int
terminit(const char *portname, int create_ok)
{
	DCB tio;	
	COMMTIMEOUTS timeout;

        is_file = 0;

	xCloseHandle(comport);

	comport = CreateFile(portname, GENERIC_READ|GENERIC_WRITE, 0, NULL,
			  OPEN_EXISTING, 0, NULL);

	if (comport == INVALID_HANDLE_VALUE) {
		goto try_as_file;
	}
	tio.DCBlength = sizeof(DCB);
	GetCommState (comport, &tio);
	tio.BaudRate = mkspeed(bitrate);
	tio.fBinary = TRUE;
	tio.fParity = TRUE;
	tio.fOutxCtsFlow = FALSE;
	tio.fOutxDsrFlow = FALSE;
	tio.fDtrControl = DTR_CONTROL_ENABLE;
	tio.fDsrSensitivity = FALSE;
	tio.fTXContinueOnXoff = TRUE;
	tio.fOutX = FALSE;
	tio.fInX = FALSE;
	tio.fErrorChar = FALSE;
	tio.fNull = FALSE;
	tio.fRtsControl = RTS_CONTROL_ENABLE;
	tio.fAbortOnError = FALSE;
	tio.ByteSize = 8;
	tio.Parity = NOPARITY;
	tio.StopBits = ONESTOPBIT;

	if (!SetCommState (comport, &tio)) {
		xCloseHandle(comport);

		/*
		 *  Probably not a com port.   Try it as a file.
		 */
try_as_file:
		magfile_in = xfopen(portname, create_ok ? "w+b" : "rb", MYNAME);
		is_file = 1;
		icon_mapping = map330_icon_table;
		mag_cleanse = m330_cleanse;
		got_version = 1;
		return 0;
	}

	GetCommTimeouts (comport, &timeout);
	/* We basically do single character reads and simulate line input
	 * mode, so these values are kind of fictional.
	 */
	timeout.ReadIntervalTimeout = 1000;
	timeout.ReadTotalTimeoutMultiplier = 1000;
	timeout.ReadTotalTimeoutConstant = 1000;
	timeout.WriteTotalTimeoutMultiplier = 1000;
	timeout.WriteTotalTimeoutConstant = 1000;
	if (!SetCommTimeouts (comport, &timeout)) {
		xCloseHandle (comport);
		fatal(MYNAME ": set timeouts\n");
	}
	return 1;
}

static char * 
termread(char *ibuf, int size)
{
	int i=0;
	DWORD cnt;

	if (is_file) {
		return fgets(ibuf, size, magfile_in);
	}

	ibuf[i]='a';
	for(;i < size;i++) {
		if (ReadFile (comport, &ibuf[i], 1, &cnt, NULL) != TRUE)
			break;
		if (cnt < 1) 
			return NULL;
		if (ibuf[i] == '\n') 
			break;
	}
	ibuf[i] = 0;
	return ibuf;
}

static void
termwrite(char *obuf, int size)
{
	DWORD len;

	if (is_file) {
		fwrite(obuf, size, 1, magfile_out);
		return;
	}
	WriteFile (comport, obuf, size, &len, NULL);
	if ((int) len != size) {
		fatal(MYNAME ":.  Wrote %d of %d bytes.\n", len, size);
	}
}

static
void
termdeinit()
{
        xCloseHandle(comport);
}

#else

#include <termios.h>
#include <unistd.h>

speed_t 
mkspeed(unsigned br)
{
	switch (br) {
		case 1200: return B1200;
		case 2400: return B2400;
		case 4800: return B4800;
		case 9600: return B9600;
		case 19200: return B19200;
#if defined B57600
		case 57600: return B57600;
#endif
#if defined B115200
		case 115200: return B115200;
#endif
		default: return B4800;
	}
}


static struct termios orig_tio;
static void
terminit(const char *portname, int create_ok)
{
	struct termios new_tio;

        magfile_in = xfopen(portname, "rb", MYNAME);

	is_file = !isatty(fileno(magfile_in));
	if (is_file) {
		icon_mapping = map330_icon_table;
		mag_cleanse = m330_cleanse;
		got_version = 1;
		return;
	} 

	magfile_out = xfopen(portname, "w+b", MYNAME);
	magfd = fileno(magfile_in);

	tcgetattr(magfd, &orig_tio);
	new_tio = orig_tio;
	new_tio.c_iflag &= ~(IGNBRK|BRKINT|PARMRK|ISTRIP|INLCR|
		IGNCR|ICRNL|IXON);
	new_tio.c_oflag &= ~OPOST;
	new_tio.c_lflag &= ~(ECHO|ECHONL|ICANON|ISIG|IEXTEN);
	new_tio.c_cflag &= ~(CSIZE|PARENB);
	new_tio.c_cflag |= CS8;
	new_tio.c_cc[VTIME] = 10;
	new_tio.c_cc[VMIN] = 0;

	cfsetospeed(&new_tio, mkspeed(bitrate));
	cfsetispeed(&new_tio, mkspeed(bitrate));
	tcsetattr(magfd, TCSAFLUSH, &new_tio);
}

static void
termdeinit()
{
	if (!is_file) {
		tcsetattr(magfd, TCSANOW, &orig_tio);
	}
}

static char * 
termread(char *ibuf, int size)
{
	return fgets(ibuf, size, magfile_in);
}

static void
termwrite(char *obuf, int size)
{
	fwrite(obuf, size, 1, magfile_out);
}
#endif

/*
 *  Arg tables are doubled up so that -? can output appropriate help
 */
static
arglist_t mag_sargs[] = {
	{"baud", &bs, "Numeric value of bitrate (baud=4800)", NULL,
		ARGTYPE_INT },
	{"maxcmts", &cmts, "Max number of comments to write (maxcmts=200)", 
		NULL, ARGTYPE_INT },
	{"noack", &noack, "Suppress use of handshaking in name of speed",
		NULL, ARGTYPE_BOOL},
	{"deficon", &deficon, "Default icon name", NULL, ARGTYPE_STRING },
	{"nukewpt", &nukewpt, "Delete all waypoints", NULL, ARGTYPE_BOOL },
	{0, 0, 0, 0, 0}
};

static
arglist_t mag_fargs[] = {
	{"deficon", &deficon, "Default icon name", NULL, ARGTYPE_STRING },
	{0, 0, 0, 0, 0}
};

static void
mag_rd_init(const char *portname)
{
	time_t now, later;
	waypoint_read_count = 0;

	if (bs) {
		bitrate=atoi(bs);
	}

	terminit(portname, 0);
	if (!mkshort_handle) {
		mkshort_handle = mkshort_new_handle();
	}

	if (!noack)
		mag_handon();

	now = current_time();
	/*
	 * The 315 can take up to 4.25 seconds to respond to initialization
	 * commands.   Time out on the side of caution.
	 */
	later = now + 6;
	if (!is_file) {
		got_version = 0;
		mag_writemsg("PMGNCMD,VERSION");
	}

	while (!got_version) {
		mag_readmsg();
		if (current_time() > later) {
			fatal(MYNAME ": No acknowledgment from GPS on %s\n",
				portname);
		}
	}

	if (!is_file && (icon_mapping != gps315_icon_table)) {
		/*
		 * The 315 can't handle this command, so we set a global
		 * to ignore the NAK on it.
		 */
		ignore_unable = 1;
		mag_writemsg("PMGNCMD,NMEAOFF");
		ignore_unable = 0;
	}
	if (nukewpt) {
		/* The unit will send us an "end" message upon completion */
		mag_writemsg("PMGNCMD,DELETE,WAYPOINT");
		mag_readmsg();
		if (!found_done) {
			fatal(MYNAME ": Unexpected response to waypoint delete command.\n");
		}
		found_done = 0;
	}

	QUEUE_INIT(&rte_wpt_tmp);

	return;
}

static void
mag_wr_init_common(const char *portname)
{
	if (bs) {
		bitrate=atoi(bs);
	}

	if (cmts) {
		wptcmtcnt_max = atoi(cmts);
	} else {
		wptcmtcnt_max = MAXCMTCT ;
	}

#if __WIN32__
	if (!terminit(portname, 1)) {
		is_file = 1;
	}
#else
	magfile_out = xfopen(portname, "w+b", MYNAME);
	is_file = !isatty(fileno(magfile_out));
#endif

	if (!mkshort_handle) {
		mkshort_handle = mkshort_new_handle();
	}
	if (is_file) {
		magfile_out = xfopen(portname, "w+b", MYNAME);
		icon_mapping = map330_icon_table;
		mag_cleanse = m330_cleanse;
		got_version = 1;
	} else {
		/*
		 *  This is a serial device.   The line has to be open for
		 *  reading and writing, so we let rd_init do the dirty work.
		 */
		if (magfile_out) {
			fclose(magfile_out);
		}
#if __WIN32__
		if (comport) {
			xCloseHandle(comport);
		}
#endif
		mag_rd_init(portname);
	}
	QUEUE_INIT(&rte_wpt_tmp);
}

/*
 * Entry point for extended (explorist) points.
 */
static void
magX_wr_init(const char *portname)
{
	wpt_len = 20;
	mag_wr_init_common(portname);
	setshort_length(mkshort_handle, wpt_len);
}

static void
mag_wr_init(const char *portname)
{
	wpt_len = 8;
	mag_wr_init_common(portname);
}

static void
mag_deinit(void)
{
	mag_handoff();
	termdeinit();
	if(magfile_in)
		fclose(magfile_in);
	magfile_in = NULL;
	if(mkshort_handle)
		mkshort_del_handle(mkshort_handle);
	mkshort_handle = NULL;

	waypt_flush(&rte_wpt_tmp);
}


/*
 * Given an incoming track messages of the form:
 * $PMGNTRK,3605.259,N,08644.389,W,00151,M,201444.61,A,,020302*66
 * create and return a populated waypoint.
 */
waypoint * 
mag_trkparse(char *trkmsg)
{
	double latdeg, lngdeg;
	int alt;
	char altunits;
	char lngdir, latdir;
	int dmy;
	int hms;
	int fracsecs;
	struct tm tm;
	waypoint *waypt;

	waypt  = xcalloc(sizeof *waypt, 1);

	memset(&tm, 0, sizeof(tm));

	sscanf(trkmsg,"$PMGNTRK,%lf,%c,%lf,%c,%d,%c,%d.%d,A,,%d", 
		&latdeg,&latdir,
		&lngdeg,&lngdir,
		&alt,&altunits,&hms,&fracsecs,&dmy);

	tm.tm_sec = hms % 100;
	hms = hms / 100;
	tm.tm_min = hms % 100;
	hms = hms / 100;
	tm.tm_hour = hms % 100;

	tm.tm_year = 100 + dmy % 100;
	dmy = dmy / 100;
	tm.tm_mon =  dmy % 100 - 1;
	dmy = dmy / 100;
	tm.tm_mday = dmy % 100; 

	waypt->creation_time = mkgmtime(&tm);
	waypt->centiseconds = fracsecs;

	if (latdir == 'S') latdeg = -latdeg;
	waypt->latitude = ddmm2degrees(latdeg);

	if (lngdir == 'W') lngdeg = -lngdeg;
	waypt->longitude = ddmm2degrees(lngdeg);

	waypt->altitude = alt;

	return waypt;
	
}

/*
 * Given an incoming route messages of the form:
 * $PMGNRTE,4,1,c,1,DAD,a,Anna,a*61
 * generate a route.
 */
void
mag_rteparse(char *rtemsg)
{
	char descr[100];
	int n;
	int frags,frag,rtenum;
	char xbuf[100],next_stop[100],abuf[100];
	char *currtemsg;
	static mag_rte_head *mag_rte_head;
	mag_rte_elem *rte_elem;
	char *p;
	
	descr[0] = 0;

	sscanf(rtemsg,"$PMGNRTE,%d,%d,%c,%d%n", 
		&frags,&frag,xbuf,&rtenum,&n);

	/*
	 * This is the first component of a route.  Allocate a new
	 * queue head.   
	 */
	if (frag == 1) {
		mag_rte_head = xcalloc(sizeof (*mag_rte_head),1);
		QUEUE_INIT(&mag_rte_head->Q);
		mag_rte_head->nelems = frags;
	}

	currtemsg = rtemsg + n;

	/*
	 * The individual line may contain several route elements.
	 * loop and pick those up.
	 */
	while (sscanf(currtemsg,",%[^,],%[^,]%n",next_stop, abuf,&n)) {
		if ((next_stop[0] == 0) || (next_stop[0] == '*')) {
			break;
		}
		
		/* trim CRC from waypoint icon string */
		if ((p = strchr(abuf, '*')) != NULL)
			*p = '\0';

		rte_elem = xcalloc(sizeof (*rte_elem),1);
		QUEUE_INIT(&rte_elem->Q);

		rte_elem->wpt_name = xstrdup(next_stop);
		rte_elem->wpt_icon = xstrdup(abuf);

		ENQUEUE_TAIL(&mag_rte_head->Q, &rte_elem->Q);
		next_stop[0] = 0;
		currtemsg += n;
	}

	/*
	 * If this was the last fragment of the route, add it to the
	 * gpsbabel internal structs now.
	 */
	if (frag == mag_rte_head->nelems) {
		queue *elem, *tmp;
		route_head *rte_head;

		rte_head = route_head_alloc();
		route_add_head(rte_head);
		rte_head->rte_num = rtenum;

		/* 
		 * It is quite feasible that we have 200 waypoints,
		 * 3 of which are used in the route.  We'll need to find
		 * those in the queue for SD routes...
		 */

		QUEUE_FOR_EACH(&mag_rte_head->Q, elem, tmp) {
			mag_rte_elem *re = (mag_rte_elem *) elem;
			waypoint *waypt;
			queue *welem, *wtmp;

			/* 
			 * Copy route points from temp wpt queue. 
			 */
			QUEUE_FOR_EACH(&rte_wpt_tmp, welem, wtmp) {
				waypt = (waypoint *)welem;
				if (strcmp(waypt->shortname, re->wpt_name) == 0) {
					waypoint * wpt = waypt_dupe(waypt);
					route_add_wpt(rte_head, wpt);
					break;
				}
			}

			dequeue(&re->Q);
			xfree(re->wpt_name);
			xfree(re->wpt_icon);
			xfree(re);
		}
		xfree(mag_rte_head);
	}
}

const char *
mag_find_descr_from_token(const char *token)
{
	icon_mapping_t *i = icon_mapping;

	if (icon_mapping == NULL) {
		return "unknown";
	}

	for (i = icon_mapping; i->token; i++) {
		if (case_ignore_strcmp(token, i->token) == 0)
			return i->icon;
	}
	return icon_mapping[0].icon;
}

const char *
mag_find_token_from_descr(const char *icon)
{
	icon_mapping_t *i = icon_mapping;

	if (i == NULL || icon == NULL) {
		return "a";
	}

	for (i = icon_mapping; i->token; i++) {
		if (case_ignore_strcmp(icon, i->icon) == 0)
			return i->token;
	}
		return icon_mapping[0].token;
}

/*
 * Given an incoming waypoint messages of the form:
 * $PMGNWPL,3549.499,N,08650.827,W,0000257,M,HOME,HOME,c*4D
 * create and return a populated waypoint.
 */
static waypoint * 
mag_wptparse(char *trkmsg)
{
	double latdeg, lngdeg;
	char latdir;
	char lngdir;
	int alt;
	char altunits;
	char shortname[100];
	char descr[100];
	char icon_token[100];
	waypoint *waypt;
	char *icons;
	char *icone;
	char *blah;
	int i = 0;

	descr[0] = 0;
	icon_token[0] = 0;

	waypt  = xcalloc(sizeof *waypt, 1);

	sscanf(trkmsg,"$PMGNWPL,%lf,%c,%lf,%c,%d,%c,%[^,],%[^,]", 
		&latdeg,&latdir,
		&lngdeg,&lngdir,
		&alt,&altunits,shortname,descr);
	icone = strrchr(trkmsg, '*');
	icons = strrchr(trkmsg, ',')+1;
	
	for (blah = icons ; blah < icone; blah++)
		icon_token[i++] = *blah;
	icon_token[i++] = '\0';
	
	if (latdir == 'S') latdeg = -latdeg;
	waypt->latitude = ddmm2degrees(latdeg);

	if (lngdir == 'W') lngdeg = -lngdeg;
	waypt->longitude = ddmm2degrees(lngdeg);

	waypt->altitude = alt;
	waypt->shortname = xstrdup(shortname);
	waypt->description = xstrdup(descr);
	waypt->icon_descr = mag_find_descr_from_token(icon_token);

	return waypt;
}

static void
mag_read(void)
{
	found_done = 0;

	switch (global_opts.objective)
	{
		case trkdata:
			if (!is_file) 
				mag_writemsg("PMGNCMD,TRACK,2");

			while (!found_done) {
				mag_readmsg();
			}

			break;
		case wptdata:
			if (!is_file) 
				mag_writemsg("PMGNCMD,WAYPOINT");

			while (!found_done) {
				mag_readmsg();
			}

			break;
		case rtedata:
			if (!is_file) {
				/* 
				 * serial routes require waypoint & routes 
				 * messages commands.
				 */
				mag_writemsg("PMGNCMD,WAYPOINT");

				while (!found_done) {
					mag_readmsg();
				}

				mag_writemsg("PMGNCMD,ROUTE");

				found_done = 0;
				while (!found_done) {
					mag_readmsg();
				}
			} else {
				/*
				 * SD routes are a stream of PMGNWPL and 
				 * PMGNRTE messages, in that order.
				 */
				while (!found_done) {
					mag_readmsg();
				}
			}

			break;
		default:
			fatal(MYNAME ": Unknown objective\n");
	}
}

static
void
mag_waypt_pr(const waypoint *waypointp)
{
	double lon, lat;
	double ilon, ilat;
	int lon_deg, lat_deg;
	char obuf[200];
	char ofmtdesc[200];
	const char *icon_token=NULL;
	char *owpt;
	char *odesc;
	char *isrc = NULL;

	ilat = waypointp->latitude;
	ilon = waypointp->longitude;

	lon = fabs(ilon);
	lat = fabs(ilat);

	lon_deg = lon;
	lat_deg = lat;

	lon = (lon - lon_deg) * 60.0;
	lat = (lat - lat_deg) * 60.0;

	lon = (lon_deg * 100.0 + lon);
	lat = (lat_deg * 100.0 + lat);
	
	if (deficon)  {
		icon_token = mag_find_token_from_descr(deficon);
	} else {
		icon_token = mag_find_token_from_descr(waypointp->icon_descr);
	}

	if (get_cache_icon(waypointp)) {
		icon_token = mag_find_token_from_descr(get_cache_icon(waypointp));
	}

	isrc = waypointp->notes ? waypointp->notes : waypointp->description;
	owpt = global_opts.synthesize_shortnames ?
                        mkshort_from_wpt(mkshort_handle, waypointp) : waypointp->shortname;
	odesc = isrc ? isrc : "";
	owpt = mag_cleanse(owpt);

	if (!global_opts.no_smart_icons &&
	    waypointp->gc_data.diff && waypointp->gc_data.terr) {
		sprintf(ofmtdesc, "%d/%d %s", waypointp->gc_data.diff, 
			waypointp->gc_data.terr, odesc);
		odesc = mag_cleanse(ofmtdesc);
	} else {
		odesc = mag_cleanse(odesc);
	}

	/*
	 * For the benefit of DirectRoute (which uses waypoint comments
	 * to deliver turn-by-turn popups for street routing) allow a 
	 * cap on the comments delivered so we leave space for it to route.
	 */
	if (odesc && /* !is_file && */ (wptcmtcnt++ >= wptcmtcnt_max))
		odesc[0] = 0;

	sprintf(obuf, "PMGNWPL,%4.3f,%c,%09.3f,%c,%07.lf,M,%-.*s,%-.46s,%s",
		lat, ilat < 0 ? 'S' : 'N',
		lon, ilon < 0 ? 'W' : 'E',
		waypointp->altitude == unknown_alt ?
			0 : waypointp->altitude,
		wpt_len,
		owpt,
		odesc,
		icon_token);
	mag_writemsg(obuf);
	xfree(owpt);
	xfree(odesc);

	if (!is_file) {
		if (mag_error) {
			warning( "Protocol error Writing '%s'\n", obuf);
		}
	}
}

static
void mag_track_nop(const route_head *rte)
{
	return;
}

static
void mag_track_disp(const waypoint *waypointp)
{
	double ilon, ilat;
	double lon, lat;
	int lon_deg, lat_deg;
	char obuf[200];
	int hms=0;
	int fracsec=0;
	int date=0;
	struct tm *tm = NULL;

	ilat = waypointp->latitude;
	ilon = waypointp->longitude;
	tm = NULL;
	if (waypointp->creation_time) {
		tm = gmtime(&waypointp->creation_time);
		if ( tm ) {
			hms = tm->tm_hour * 10000 + tm->tm_min  * 100 + 
				   tm->tm_sec;
			date = tm->tm_mday * 10000 + tm->tm_mon * 100 + 
				   tm->tm_year;
			fracsec = waypointp->centiseconds;
		}
	}
        if (!tm) {
		date = 0;
		fracsec = 0;
	}

	lon = fabs(ilon);
	lat = fabs(ilat);

	lon_deg = lon;
	lat_deg = lat;

	lon = (lon - lon_deg) * 60.0;
	lat = (lat - lat_deg) * 60.0;

	lon = (lon_deg * 100.0 + lon);
	lat = (lat_deg * 100.0 + lat);

	sprintf(obuf,"PMGNTRK,%4.3f,%c,%09.3f,%c,%05.f,%c,%06d.%02d,A,,%06d", 
		lat, ilat < 0 ? 'S' : 'N',
		lon, ilon < 0 ? 'W' : 'E',
		waypointp->altitude == unknown_alt ?
                        0 : waypointp->altitude,
			'M',hms,fracsec,date);
	mag_writemsg(obuf);
}

static
void mag_track_pr()
{
	track_disp_all(mag_track_nop, mag_track_nop, mag_track_disp);
}

/*
The spec says to stack points:
	$PMGNRTE,2,1,c,1,FOO,POINT1,b,POINT2,c,POINT3,d*6C<CR><LF>

Meridian SD card and serial (at least) writes in pairs:
	$PMGNRTE,4,1,c,1,HOME,c,I49X73,a*15
	...
	$PMGNRTE,4,4,c,1,RON273,a,MYCF93,a*7B
	
The spec also says that some units don't like single-legged pairs,
and to replace the 2nd name with "<<>>", but I haven't seen one of those.
*/

static void
mag_route_trl(const route_head * rte)
{
	queue *elem, *tmp;
	waypoint *waypointp;
	char obuff[256];
	char buff1[64], buff2[64];
	char *pbuff, *owpt;
	const char * icon_token;
	int i, numlines, thisline;
	
	/* count waypoints for this route */
	i = rte->rte_waypt_ct;

	/* number of output PMGNRTE messages at 2 points per line */
	numlines = (i / 2) + (i % 2);
	
	/* increment the route counter. */
	route_out_count++;
	
	thisline = i = 0;
	QUEUE_FOR_EACH(&rte->waypoint_list, elem, tmp) {
		waypointp = (waypoint *) elem;
		i++;

		if (deficon)
			icon_token = mag_find_token_from_descr(deficon);
		else
			icon_token = mag_find_token_from_descr(waypointp->icon_descr);
			
		if (i == 1)
			pbuff = buff1;
		else
			pbuff = buff2;

		owpt = waypointp->shortname;
		owpt = mag_cleanse(owpt);

		sprintf(pbuff, "%s,%s", owpt, icon_token);
		
		xfree(owpt);
		
		if ((tmp == &rte->waypoint_list) || ((i % 2) == 0)) {
			thisline++;

			sprintf(obuff, "PMGNRTE,%d,%d,c,%d,%s,%s", 
				numlines, thisline, 
				rte->rte_num ? rte->rte_num : route_out_count,
				buff1, buff2);

			mag_writemsg(obuff);
			buff1[0] = '\0';
			buff2[0] = '\0';
			i = 0;
		}
	}
}

static void
mag_route_hdr(const route_head *rh)
{
}

static void
mag_route_pr()
{
	route_out_count = 0;
	route_disp_all(mag_route_hdr, mag_route_trl, mag_waypt_pr);

}

static void
mag_write(void)
{
	/* 
	 * Whitespace is actually legal, but since waypoint name length is
	 * only 8 bytes, we'll conserve them.
	 */

	setshort_whitespace_ok(mkshort_handle, 0);

	wptcmtcnt = 0;

	switch (global_opts.objective)
	{
		case trkdata:
			mag_track_pr();
			break;
		case wptdata:
			waypt_disp_all(mag_waypt_pr);
			break;
		case rtedata:
			mag_route_pr();
			break;
		default:
			fatal(MYNAME ": Unknown objective.\n");
	}
}

/*
 *  This is repeated just so it shows up as separate menu options
 *  for the benefit of GUI wrappers.
 */
ff_vecs_t mag_svecs = {
	ff_type_serial,
	FF_CAP_RW_ALL,
	mag_rd_init,	
	mag_wr_init,	
	mag_deinit,	
	mag_deinit,	
	mag_read,
	mag_write,
	NULL, 
	mag_sargs
};

ff_vecs_t mag_fvecs = {
	ff_type_file,
	FF_CAP_RW_ALL,
	mag_rd_init,	
	mag_wr_init,	
	mag_deinit,	
	mag_deinit,	
	mag_read,
	mag_write,
	NULL, 
	mag_fargs
};

/*
 * Extended (Explorist) entry tables.
 */
ff_vecs_t magX_fvecs = {
	ff_type_file,
	FF_CAP_RW_ALL,
	mag_rd_init,	
	magX_wr_init,	
	mag_deinit,	
	mag_deinit,	
	mag_read,
	mag_write,
	NULL, 
};
