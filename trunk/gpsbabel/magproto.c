/*
    Communicate Thales/Magellan serial protocol.

    Copyright (C) 2002 Robert Lipe, robertlipe@usa.net

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
#include <errno.h>
#include <sys/stat.h>

#include "defs.h"
#include "magellan.h"

int bitrate = 4800;
#define MYNAME "MAGPROTO"

#define debug_serial  (global_opts.debug_level > 1)

extern gpsdata_type objective;

static char * termread(char *ibuf, int size);
static void termwrite(char *obuf, int size);
static double mag2degrees(double mag_val);
static void mag_readmsg(void);
static void *mkshort_handle;
static char *deficon;

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

static FILE *magfile_in;
static FILE *magfile_out;
static int magfd;
static mag_rxstate magrxstate;
static int mag_error;
static int last_rx_csum;
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
	{ "ac", "restaraunt" },
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
	{ "ah", "Virtual cache"}, 	/* Binos: becuase you "see" them. */
	{ "ak", "Micro-Cache" },	/* Looks like a film canister. */
	{ "an", "Multi-Cache"}, 	/* Winery: grapes 'coz they "bunch" */
	{ NULL, NULL } 
};

pid_to_model_t pid_to_model[] = 
{
	{ mm_gps315320, 24, "GPS 315/320" },
	{ mm_map410, 25, "Map 410" },
	{ mm_map330, 30, "Map 330" },
	{ mm_gps310, 31, "GPS 310" },
	{ mm_meridian, 33, "Meridian" },
	{ mm_sportrak, 36, "SporTrak" },
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
static char *
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
static unsigned int 
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
		fprintf(stderr,"WRITE: $%s*%02X\r\n",buf, osum);
	}

    retry:

	i = sprintf(obuf, "$%s*%02X\r\n",buf, osum);
	termwrite(obuf, i);
	if (magrxstate == mrs_handon || magrxstate == mrs_awaiting_ack) {
		magrxstate = mrs_awaiting_ack;
		mag_readmsg();
		if (last_rx_csum != osum) {
			if (debug_serial) {
				fprintf(stderr, "COMM ERROR: Expected %02x, got %02x", 
						osum, last_rx_csum);
			}
			if (retry_cnt--)
				goto retry;
			else
				fatal(MYNAME 
					": Too many communication errors.\n");
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
		fprintf(stderr,"ACK WRITE: %s",obuf);
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
			setshort_length(mkshort_handle, 8);
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
				fprintf(stderr, "%d\n", retrycnt);
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
			fprintf(stderr, "SHORT READ %d\n", isz);
		return;
	}
	mag_error = 0;
	while (!isprint(ibuf[isz]))
		isz--;
	isump = &ibuf[isz-1];
	isum  = strtoul(isump, NULL,16); 
	if (isum != mag_pchecksum(&ibuf[1], isz-3)) {
		if (debug_serial)
			fprintf(stderr, "RXERR %02x/%02x: '%s'\n", isum, mag_pchecksum(&ibuf[1],isz-5), ibuf);
			/* Special case receive errors early on. */
		if (!got_version) {
			fatal(MYNAME ": bad communication.  Check bit rate.\n");
		}
	}
	if (debug_serial) {
		fprintf(stderr, "READ: %s\n", ibuf);
	}
	if (IS_TKN("$PMGNCSM,")) {
		last_rx_csum = strtoul(&ibuf[9], NULL, 16);
		magrxstate = mrs_handon;
		return;
	} 
	if (strncmp(ibuf, "$PMGNWPT,", 7) == 0) {
		waypoint *wpt = mag_wptparse(ibuf);
		waypt_add(wpt);
	} 
	if (strncmp(ibuf, "$PMGNTRK,", 7) == 0) {
		waypoint *wpt = mag_trkparse(ibuf);
		route_add_wpt(trk_head, wpt);
	} 
	if (IS_TKN("$PMGNVER,")) {
		mag_verparse(ibuf);
	} 
	mag_error = 0;
	if (!ignore_unable && IS_TKN("$PMGNCMD,UNABLE")) {
		fprintf(stderr, "Unable to send\n");
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

#if __WIN32__

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
terminit(const char *portname)
{
	DCB tio;	
	COMMTIMEOUTS timeout;

        is_file = 0;

	xCloseHandle(comport);

	comport = CreateFile(portname, GENERIC_READ|GENERIC_WRITE, 0, NULL,
			  OPEN_EXISTING, 0, NULL);

	if (comport == INVALID_HANDLE_VALUE) {
		is_file = 1;
		return 0;
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
		magfile_in = fopen(portname, "rb");
		if (magfile_in == NULL) {
			fatal(MYNAME ": Cannot open %s.%s\n",
				portname, strerror(errno));
		}
		is_file = 1;
		icon_mapping = map330_icon_table;
		mag_cleanse = m330_cleanse;
		got_version = 1;
		return 0;
	}

	GetCommTimeouts (comport, &timeout);
	timeout.ReadIntervalTimeout = 10;
	timeout.WriteTotalTimeoutMultiplier = 10;
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
		if (ibuf[i] == '\n') break;
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
	if (len != size) {
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
terminit(const char *portname)
{
	struct termios new_tio;
	struct stat sbuf;

        magfile_in = fopen(portname, "rb");

        if (magfile_in == NULL) {
                fatal(MYNAME ": Cannot open %s.%s\n",
                        portname, strerror(errno));
        }

	fstat(fileno(magfile_in), &sbuf);
	is_file = S_ISREG(sbuf.st_mode);
	if (is_file) {
		icon_mapping = map330_icon_table;
		mag_cleanse = m330_cleanse;
		got_version = 1;
		return;
	} 

	magfile_out = fopen(portname, "w+b");
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


static void
mag_rd_init(const char *portname, const char *args)
{
	time_t now, later;
	char * bs = get_option(args, "baud");
	char * noack = get_option(args, "noack");
	deficon = get_option(args, "deficon");

	if (bs) {
		bitrate=atoi(bs);
	}

	terminit(portname);
	if (!mkshort_handle) {
		mkshort_handle = mkshort_new_handle();
	}

	if (!noack)
		mag_handon();

	now = time(NULL);
	/*
	 * The 315 can take up to 4.25 seconds to respond to initialization
	 * commands.   Time out on the side of caution.
	 */
	later = now + 6;
	if (!is_file) {
		mag_writemsg("PMGNCMD,VERSION");
	}

	while (!got_version) {
		mag_readmsg();
		if (time(NULL) > later) {
			fatal(MYNAME ": No acknowledgment from GPS on %s\n",
				portname);
		}
	}

	if (!is_file) {
		/*
		 * The 315 can't handle this command, so we set a global
		 * to ignore the NAK on it.
		 */
		ignore_unable = 1;
		mag_writemsg("PMGNCMD,NMEAOFF");
	}

	return;
}

static void
mag_wr_init(const char *portname, const char *args)
{
#if __WIN32__
	if (!terminit(portname)) {
		is_file = 1;
	}
#else
	struct stat sbuf;
	deficon = get_option(args, "deficon");
	magfile_out = fopen(portname, "w+b");
	fstat(fileno(magfile_out), &sbuf);
	is_file = S_ISREG(sbuf.st_mode);
#endif

	if (!mkshort_handle) {
		mkshort_handle = mkshort_new_handle();
	}
	if (is_file) {
		magfile_out = fopen(portname, "w+b");
		if (magfile_out == NULL) {
			fatal(MYNAME ": '%s' cannot be opened for writing.\n",
				       	portname);
		}
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
		mag_rd_init(portname, args);
	}
}

static void
mag_deinit(void)
{
	mag_handoff();
	termdeinit();
	if(magfile_in)
		fclose(magfile_in);
	magfile_in = NULL;
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

	/*
	 * FIXME: mktime assumes the struct tm is in local time, which 
	 * ours is not...
 	 */
	waypt->creation_time = mktime(&tm);

	if (latdir == 'S') latdeg = -latdeg;
	waypt->position.latitude.degrees = mag2degrees(latdeg);

	if (lngdir == 'W') lngdeg = -lngdeg;
	waypt->position.longitude.degrees = mag2degrees(lngdeg);

	waypt->position.altitude.altitude_meters = alt;

	return waypt;
	
}

/*
 * Given an incoming route messages of the form:
 * $PMGNRTE,4,1,c,1,DAD,a,Anna,a*61
 * generate a route.
 */
waypoint * 
mag_rteparse(char *rtemsg)
{
	char descr[100];
	int n;
	int frags,frag,rtenum;
	char xbuf[100],next_stop[100],abuf[100];
	char *currtemsg;
	static mag_rte_head *mag_rte_head;
	mag_rte_elem *rte_elem;

	descr[0] = 0;

	sscanf(rtemsg,"$PMGNRTE,%d,%d,%c,%d%n", 
		&frags,&frag,xbuf,&rtenum,&n);

	/*
	 * This is the first component of a route.  Allocate a new
	 * queue head.   It's kind of unfortunate that we can't know
	 * a priori how many items are on this track, so we have to
	 * alloc and chain those as we go.
	 */
	if (frag == 1) {
		mag_rte_head = xmalloc(sizeof (*mag_rte_head));
		QUEUE_INIT(&mag_rte_head->Q);
		mag_rte_head->nelems = frags;
	}

	currtemsg = rtemsg + n;

	/*
	 * The individual line may contain several route elements.
	 * loop and pick those up.
	 */
	while (sscanf(currtemsg,",%[^,],%[^,]%n",next_stop, abuf,&n)) {
		if (next_stop[0] == 0) {
			break;
		}
		rte_elem = xmalloc(sizeof (*rte_elem));
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

		/*
		 *  TODO: I suppose we have to fetch the waypoints to
		 *  get the underlying data for each stop in the route.
		 */
		QUEUE_FOR_EACH(&mag_rte_head->Q, elem, tmp) {
			static int lat; /* Dummy data */
			mag_rte_elem *re = (mag_rte_elem *) elem;
			waypoint *waypt;

			waypt  = xcalloc(sizeof *waypt, 1);

			/* TODO Populate rest of waypoint. */
			waypt->shortname = re->wpt_name;
			waypt->position.latitude.degrees = ++lat;

			route_add_wpt(rte_head, waypt);
			dequeue(&re->Q);
			free(re);
		}
	}
	return 0;
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

static double 
mag2degrees(double mag_val)
{
	double minutes;
	double tmp_val;
	double return_value;
	int deg;

	/* 
	 * magellan value is DDMM.MM
	 * e.g. 36 3.85 would be coded as 3603.85
	 */
	tmp_val = mag_val / 100.0;
	deg = (int) tmp_val;
	minutes = (tmp_val - deg) * 100.0;
	minutes /= 60.0;
	return_value = (double) deg + minutes;
	return return_value;
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

	if (latdir == 'S') latdeg = -latdeg;
	waypt->position.latitude.degrees = mag2degrees(latdeg);

	if (lngdir == 'W') lngdeg = -lngdeg;
	waypt->position.longitude.degrees = mag2degrees(lngdeg);

	waypt->position.altitude.altitude_meters = alt;
	waypt->shortname = xstrdup(shortname);
	waypt->description = xstrdup(descr);
	waypt->icon_descr = mag_find_descr_from_token(icon_token);

	return waypt;
}

static void
mag_read(void)
{
	switch (global_opts.objective)
	{
		case trkdata:
			trk_head = route_head_alloc();
			route_add_head(trk_head);

			if (!is_file) 
				mag_writemsg("PMGNCMD,TRACK,2");

			break;
		case wptdata:
			if (!is_file) 
				mag_writemsg("PMGNCMD,WAYPOINT");
			break;
		default:
			fatal(MYNAME ": Routes are not yet supported\n");
	}

	while (!found_done) {
		mag_readmsg();
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
	char *isrc;

	ilat = waypointp->position.latitude.degrees;
	ilon = waypointp->position.longitude.degrees;

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

	/*
	 * For icons, type overwrites container.  So a multi-micro will 
	 * get the icons for "multi".
 	 */
	switch (waypointp->gc_data.container) {
		case gc_micro: 
			icon_token = mag_find_token_from_descr("Micro-Cache");
			break;
	}
	switch (waypointp->gc_data.type) {
		case gt_virtual:
			icon_token = mag_find_token_from_descr("Virtual cache");
			break;
		case gt_multi:
			icon_token = mag_find_token_from_descr("Multi-Cache");
			break;
		default:
			break;
	}
	isrc = waypointp->notes ? waypointp->notes : waypointp->description;
	owpt = global_opts.synthesize_shortnames ?
                        mkshort(mkshort_handle, isrc) : waypointp->shortname,
	odesc = isrc ? isrc : "";
	owpt = mag_cleanse(owpt);

	if (waypointp->gc_data.diff && waypointp->gc_data.terr) {
		sprintf(ofmtdesc, "%d/%d %s", waypointp->gc_data.diff, 
			waypointp->gc_data.terr, odesc);
		odesc = mag_cleanse(ofmtdesc);
	} else {
		odesc = mag_cleanse(odesc);
	}

	sprintf(obuf, "PMGNWPL,%4.3f,%c,%09.3f,%c,%07.lf,M,%-.8s,%-.30s,%s",
		lat, ilon < 0 ? 'N' : 'S',
		lon, ilat < 0 ? 'E' : 'W',
		waypointp->position.altitude.altitude_meters == unknown_alt ?
			0 : waypointp->position.altitude.altitude_meters,
		owpt,
		odesc,
		icon_token);
	mag_writemsg(obuf);
	free(owpt);
	free(odesc);

	if (!is_file) {
		if (mag_error) {
			fprintf(stderr, "Protocol error Writing '%s'\n", obuf);
		}
	}
}


static void
mag_write(void)
{
	/* 
	 * Whitespace is actually legal, but since waypoint name length is
	 * only 8 bytes, we'll conserve them.
	 */
	setshort_whitespace_ok(mkshort_handle, 0);
	waypt_disp_all(mag_waypt_pr);
}

ff_vecs_t mag_vecs = {
	mag_rd_init,	
	mag_wr_init,	
	mag_deinit,	
	mag_deinit,	
	mag_read,
	mag_write,
};
