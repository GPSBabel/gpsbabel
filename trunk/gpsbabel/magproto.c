/*
    Communicate Thales/Magellan serial protocol.

    Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007, 2008 Robert Lipe, robertlipe@usa.net

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
#include "gbser.h"

static int bitrate = 4800;
static int wptcmtcnt;
static int wptcmtcnt_max;
static int explorist;
static int broken_sportrak;
#define MYNAME "MAGPROTO"
#define MAXCMTCT 200

#define debug_serial  (global_opts.debug_level > 1)

static char *termread(char *ibuf, int size);
static void termwrite(char *obuf, int size);
static void mag_readmsg(gpsdata_type objective);
static void mag_handon(void);
static void mag_handoff(void);
static short_handle mkshort_handle = NULL;
static char *deficon = NULL;
static char *bs = NULL;
static char *cmts = NULL;
static char *noack = NULL;
static char *nukewpt = NULL;
static int route_out_count;
static int waypoint_read_count;
static int wpt_len = 8;
static const char *curfname;
static int extension_hint;

/*
 * Magellan's firmware is *horribly* slow to send the next packet after
 * we turn around an ack while we are reading from the device.  It's
 * quite spiffy when we're writing to the device.   Since we're *way*
 * less likely to lose data while reading from it than it is to lose data
 * when we write to it, we turn off the acks when we are predominatly
 *  reading.
 */
static int suppress_ack;

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
	char *rte_name;
	int nelems;
} mag_rte_head;

static queue rte_wpt_tmp; /* temporary PGMNWPL msgs for routes */

static gbfile *magfile_h;
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
	{ mm_meridian, 110, "Explorist 100" },
	{ mm_meridian, 111, "Explorist 200" },
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
	int retry_cnt = 5;
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
		mag_readmsg(trkdata);
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

	if (prodid == 37) {
		broken_sportrak = 1;
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
mag_readmsg(gpsdata_type objective)
{
	char ibuf[512];	/* oliskoli: corrupted data (I've seen descr with a lot
				     of escaped FFFFFFFF) may need more size  */
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

	/* If column zero isn't a dollar sign, it's not for us */
	if (ibuf[0] != '$') {
		fatal(MYNAME ": line doesn't start with '$'.\n");
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
	if (strncmp(ibuf, "$PMGNWPL,", 7) == 0) {
		waypoint *wpt = mag_wptparse(ibuf);
		waypoint_read_count++;
		if (global_opts.verbose_status) {
			waypt_status_disp(waypoint_read_count, 
					waypoint_read_count);
		}

		if (extension_hint) {
			if (extension_hint == WPTDATAMASK) {
				waypt_add(wpt);
			} else if (extension_hint == RTEDATAMASK) {
					ENQUEUE_TAIL(&rte_wpt_tmp, &wpt->Q);
			}
		} else {
			switch (objective) {
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
	}
	if (strncmp(ibuf, "$PMGNTRK,", 7) == 0) {
		waypoint *wpt = mag_trkparse(ibuf);
		/*
		 * Allow lazy allocation of track head.
		 */
		if (trk_head == NULL) {
			/* These tracks don't have names, so derive one
			 * from input filename.
			 */
			char *e;
			const char *s = get_filename(curfname);

			trk_head = route_head_alloc();

			/* Whack trailing extension if present. */
			trk_head->rte_name = xstrdup(s);
			e = strrchr(trk_head->rte_name, '.');
			if (e) {
				*e = '\0';
			}
			
			track_add_head(trk_head);
		}

		track_add_wpt(trk_head, wpt);
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
	if (IS_TKN("$PMGNCMD,END") || (is_file && (gbfeof(magfile_h)))) {
		found_done = 1;
		return;
	} 

	if (magrxstate != mrs_handoff) {
		mag_writeack(isum);
	}
}

static void *serial_handle = NULL;

static int 
terminit(const char *portname, int create_ok) 
{
	if (gbser_is_serial(portname)) {
		if (serial_handle = gbser_init(portname), NULL != serial_handle) {
			int rc;
			if (rc = gbser_set_port(serial_handle, bitrate, 8, 0, 1), gbser_OK != rc) {
				fatal(MYNAME ": Can't configure port\n");
			}
		}
		is_file = 0;
		if (serial_handle == NULL) {
			fatal(MYNAME ": Could not open serial port %s\n", portname);
		}
		return 1;
	} else {
		/* Does this check for an error? */
		magfile_h = gbfopen(portname, create_ok ? "w+b" : "rb", MYNAME);
		is_file = 1;
		icon_mapping = map330_icon_table;
		mag_cleanse = m330_cleanse;
		got_version = 1;
		return 0;
	}
}

static char *termread(char *ibuf, int size) 
{
	if (is_file) {
		return gbfgets(ibuf, size, magfile_h);
	} else {
		int rc;
		rc = gbser_read_line(serial_handle, ibuf, size, 2000, 0x0a, 0x0d);
		if (rc != gbser_OK) {
			fatal(MYNAME ": Read error\n");
		}
		return ibuf;
	}
}

/* Though not documented in the protocol spec, if the unit itself
 * wants to create a field containing a comma, it will encode it 
 * as <escape>2C.  We extrapolate that any 2 digit hex encoding may
 * be valid.  We don't do this in termread() since we need to do it 
 * after the scanf.  This means we have to do it field-by-field
 * basis.
 *
 * The buffer is modified in place and shortened by copying the remaining
 * string including the terminator.
 */
static
void
mag_dequote(char *ibuf) 
{
	char *esc = NULL;

	while ((esc = strchr (ibuf, 0x1b))) {
		int nremains = strlen(esc);
		if (nremains >= 3) {
			static const char hex[16] = "0123456789ABCDEF";
			char *c1 = strchr(hex, esc[1]);
			char *c2 = strchr(hex, esc[2]);
			if (c1 && c2) {
				int escv = (c1 - hex) * 16 + (c2 - hex);
				if (escv == 255) {	/* corrupted data */
					char *tmp = esc + 1;
					while (*tmp == 'F') tmp++;
					memmove(esc, tmp, strlen(tmp) + 1);
				}
				else {
					*esc++ = (isprint(escv)) ? escv : '$';
					/* buffers overlap */
					memmove(esc, esc+2, nremains - 2);
				}
			}
		}
		else {
			*esc = '\0';	/* trim corrupted data, 
					   otherwise we get an endless loop */
		}
	}
}

static void 
termwrite(char *obuf, int size) 
{
	if (is_file) {
		size_t nw;
		if (nw = gbfwrite(obuf, 1, size, magfile_h), nw < (size_t) size) {
			fatal(MYNAME ": Write error");
		}
	} else {
		int rc;
		if (rc = gbser_write(serial_handle, obuf, size), rc < 0) {
			fatal(MYNAME ": Write error");
		}
	}
}

static void termdeinit() 
{
	if (is_file) {
		gbfclose(magfile_h);
		magfile_h = NULL;
	} else {
		gbser_deinit(serial_handle);
		serial_handle = NULL;
	}
}

/*
 *  Arg tables are doubled up so that -? can output appropriate help
 */
static
arglist_t mag_sargs[] = {
	{"deficon", &deficon, "Default icon name", NULL, ARGTYPE_STRING,
		ARG_NOMINMAX },
	{"maxcmts", &cmts, "Max number of comments to write (maxcmts=200)", 
		"200", ARGTYPE_INT, ARG_NOMINMAX },
	{"baud", &bs, "Numeric value of bitrate (baud=4800)", "4800",
		ARGTYPE_INT, ARG_NOMINMAX },
	{"noack", &noack, "Suppress use of handshaking in name of speed",
		NULL, ARGTYPE_BOOL, ARG_NOMINMAX},
	{"nukewpt", &nukewpt, "Delete all waypoints", NULL, ARGTYPE_BOOL,
		ARG_NOMINMAX },
	ARG_TERMINATOR
};

static
arglist_t mag_fargs[] = {
	{"deficon", &deficon, "Default icon name", NULL, ARGTYPE_STRING,
		ARG_NOMINMAX },
	{"maxcmts", &cmts, "Max number of comments to write (maxcmts=200)", 
		NULL, ARGTYPE_INT, ARG_NOMINMAX },
	ARG_TERMINATOR
};

/* 
 * The part of the serial init that's common to read and write.
 */
static void
mag_serial_init_common(const char *portname)
{
	time_t now, later;

	if (is_file) {
		return;
	}

	mag_handoff();
	if (!noack && !suppress_ack) 
		mag_handon();

	now = current_time();
	/*
	 * The 315 can take up to 4.25 seconds to respond to initialization
	 * commands.   Time out on the side of caution.
	 */
	later = now + 6;
	got_version = 0;
	mag_writemsg("PMGNCMD,VERSION");

	while (!got_version) {
		mag_readmsg(trkdata);
		if (current_time() > later) {
			fatal(MYNAME ": No acknowledgment from GPS on %s\n",
				portname);
		}
	}

	if ((icon_mapping != gps315_icon_table)) {
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
		mag_readmsg(trkdata);
		if (!found_done) {
			fatal(MYNAME ": Unexpected response to waypoint delete command.\n");
		}
		found_done = 0;
	}

}
static void
mag_rd_init_common(const char *portname)
{
	char *ext;
	waypoint_read_count = 0;

	if (bs) {
		bitrate=atoi(bs);
	}

	if (!mkshort_handle) {
		mkshort_handle = mkshort_new_handle();
	}

	terminit(portname, 0);
	mag_serial_init_common(portname);

	QUEUE_INIT(&rte_wpt_tmp);

	/* find the location of the tail of the path name,
	 * make a copy of it, then lop off the file extension
	 */

	curfname = get_filename(portname);

	/*
	 * I'd rather not derive behaviour from filenames but since
	 * we can't otherwise tell if we should put a WPT on the route
	 * queue or the WPT queue in the presence of (-w -r -t) we 
	 * divine a hint from the filename extension when we can.
	 */
	ext = strrchr(curfname, '.');
	if (ext) {
		ext++;
		if (0 == case_ignore_strcmp(ext, "upt")) {
			extension_hint = WPTDATAMASK;
		} else if (0 == case_ignore_strcmp(ext, "log")) {
			extension_hint = TRKDATAMASK;
		} else if (0 == case_ignore_strcmp(ext, "rte")) {
			extension_hint = RTEDATAMASK;
		} 
	}

	return;
}

static void
mag_rd_init(const char *portname)
{
	explorist = 0;
	suppress_ack = 1;
	mag_rd_init_common(portname);
}

static void
magX_rd_init(const char *portname)
{
	explorist = 1;
	mag_rd_init_common(portname);
}

static void
mag_wr_init_common(const char *portname)
{
	suppress_ack = 0;
	if (bs) {
		bitrate=atoi(bs);
	}

	if (waypt_count() > 500) {
		fatal(MYNAME ": Meridian/Explorist does not support more than 500 waypoints in one file. Only\n200 waypoints may have comments.\nDecrease the number of waypoints sent.\n");
	}

	if (cmts) {
		wptcmtcnt_max = atoi(cmts);
	} else {
		wptcmtcnt_max = MAXCMTCT ;
	}

	if (!mkshort_handle) {
		mkshort_handle = mkshort_new_handle();
	}

	terminit(portname, 1);
	mag_serial_init_common(portname);

	QUEUE_INIT(&rte_wpt_tmp);
}

/*
 * Entry point for extended (explorist) points.
 */
static void
magX_wr_init(const char *portname)
{
	wpt_len = 20;
	explorist = 1;
	mag_wr_init_common(portname);
	setshort_length(mkshort_handle, wpt_len);
	setshort_whitespace_ok(mkshort_handle, 1);
}

static void
mag_wr_init(const char *portname)
{
	explorist = 0;
	wpt_len = 8;
	mag_wr_init_common(portname);
	/* 
	 * Whitespace is actually legal, but since waypoint name length is
	 * only 8 bytes, we'll conserve them.
	 */

	setshort_whitespace_ok(mkshort_handle, 0);
}

static void
mag_deinit(void)
{
	mag_handoff();
	termdeinit();
	if(mkshort_handle)
		mkshort_del_handle(&mkshort_handle);

	waypt_flush(&rte_wpt_tmp);

	trk_head = NULL;
}

/*
 * I'm tired of arguing with scanf about optional fields .  Detokenize 
 * an incoming string that may contain empty fields.
 * 
 * Probably should be cleaned up and moved to common code, but
 * making it deal with an arbitrary number of fields of arbitrary
 * size is icky.  We don't have to solve the general case here...
 */

static char ifield[20][100];
static
void parse_istring(char *istring)
{
	int f = 0;
	int n,x;
	while (istring[0]) {
	    char *fp = ifield[f];
		x = sscanf(istring, "%[^,]%n", fp, &n);
		f++;
		if (x) {
			istring += n;
			/* IF more in this string, skip delim */
			if (istring[0]) istring++;
		} else {
			istring ++;
		}
	}
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

	waypt  = waypt_new();

	memset(&tm, 0, sizeof(tm));

	/* 
	 * As some of the fields are optional, sscanf works badly
	 * for us.
	 */
	parse_istring(trkmsg);
	latdeg = atof(ifield[1]);
	latdir = ifield[2][0];
	lngdeg = atof(ifield[3]);
	lngdir = ifield[4][0];
	alt = atof(ifield[5]);
	altunits = ifield[6][0];
	sscanf(ifield[7], "%d.%d", &hms, &fracsecs);
	/* Field 8 is constant */
	/* Field nine is optional track name */
	dmy = atoi(ifield[10]);

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
	waypt->microseconds = CENTI_TO_MICRO(fracsecs);

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
	char *rte_name = NULL;
	
	descr[0] = 0;
#if 0
	sscanf(rtemsg,"$PMGNRTE,%d,%d,%c,%d%n", 
		&frags,&frag,xbuf,&rtenum,&n);
#else
	sscanf(rtemsg,"$PMGNRTE,%d,%d,%c,%d%n", 
		&frags,&frag,xbuf,&rtenum,&n);

	/* Explorist has a route name here */
	if (explorist) {
		char *ca, *ce;
		
		ca = rtemsg + n;
		is_fatal(*ca++ != ',', MYNAME ": Incorrectly formatted route line '%s'", rtemsg);

		ce = strchr(ca, ',');
		is_fatal(ce == NULL, MYNAME ": Incorrectly formatted route line '%s'", rtemsg);

		if (ca == ce)
			xasprintf(&rte_name, "Route%d", rtenum);
		else
			rte_name = xstrndup(ca, ce - ca);
		
		n += ((ce - ca) + 1);
	}

#endif

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

		/* Sportrak (the non-mapping unit) creates malformed
		 * RTE sentence with no icon info after the routepoint
		 * name.  So if we saw an "icon" treat that as new 
		 * routepoint.
		 */
		if (broken_sportrak && abuf[0]) {
			rte_elem = xcalloc(sizeof (*rte_elem),1);
			QUEUE_INIT(&rte_elem->Q);
			rte_elem->wpt_name = xstrdup(abuf);

			ENQUEUE_TAIL(&mag_rte_head->Q, &rte_elem->Q);
		}

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
		rte_head->rte_name = xstrdup(rte_name);

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
	if (rte_name) xfree(rte_name);
}

const char *
mag_find_descr_from_token(const char *token)
{
	icon_mapping_t *i = icon_mapping;

	if (icon_mapping == NULL) {
		return "unknown";
	}

	for (i = icon_mapping; i->token; i++) {
		if (token[0] == 0) break;
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
	char descr[256];
	char icon_token[100];
	waypoint *waypt;
	char *icons;
	char *icone;
	char *blah;
	int i = 0;

	descr[0] = 0;
	icon_token[0] = 0;

	waypt  = waypt_new();

	sscanf(trkmsg,"$PMGNWPL,%lf,%c,%lf,%c,%d,%c,%[^,],%[^,]", 
		&latdeg,&latdir,
		&lngdeg,&lngdir,
		&alt,&altunits,shortname,descr);
	icone = strrchr(trkmsg, '*');
	icons = strrchr(trkmsg, ',')+1;

	mag_dequote(descr);
	
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
        if (global_opts.masked_objective & TRKDATAMASK) {
		  magrxstate = mrs_handoff;
          if (!is_file) 
            mag_writemsg("PMGNCMD,TRACK,2");
          
          while (!found_done) {
            mag_readmsg(trkdata);
          }
        }

	found_done = 0;
        if (global_opts.masked_objective & WPTDATAMASK) {
		  magrxstate = mrs_handoff;
          if (!is_file) 
            mag_writemsg("PMGNCMD,WAYPOINT");
          
          while (!found_done) {
            mag_readmsg(wptdata);
          }
        }

	found_done = 0;
        if (global_opts.masked_objective & RTEDATAMASK) {
		  magrxstate = mrs_handoff;
          if (!is_file) {
            /* 
             * serial routes require waypoint & routes 
             * messages commands.
             */
            mag_writemsg("PMGNCMD,WAYPOINT");
            
            while (!found_done) {
              mag_readmsg(rtedata);
            }
            
            mag_writemsg("PMGNCMD,ROUTE");
            
            found_done = 0;
            while (!found_done) {
              mag_readmsg(rtedata);
            }
          } else {
            /*
             * SD routes are a stream of PMGNWPL and 
             * PMGNRTE messages, in that order.
             */
            while (!found_done) {
              mag_readmsg(rtedata);
            }
          }
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

	if (global_opts.smart_icons &&
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

	sprintf(obuf, "PMGNWPL,%4.3f,%c,%09.3f,%c,%07.0f,M,%-.*s,%-.46s,%s",
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
			fracsec = MICRO_TO_CENTI(waypointp->microseconds);
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

	sprintf(obuf,"PMGNTRK,%4.3f,%c,%09.3f,%c,%05.0f,%c,%06d.%02d,A,,%06d", 
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
		if (strlen(owpt) > sizeof(buff1) - 3) {
			owpt[sizeof(buff1) - 3] = 0;
		}
		owpt = mag_cleanse(owpt);

		sprintf(pbuff, "%s,%s", owpt, icon_token);
		
		xfree(owpt);
		
		if ((tmp == &rte->waypoint_list) || ((i % 2) == 0)) {
			char expbuf[1024];
			thisline++;
			expbuf[0] = 0;
			if (explorist) {
				snprintf(expbuf, sizeof(expbuf), "%s,",
					rte->rte_name ? rte->rte_name : "");
			}
			sprintf(obuff, "PMGNRTE,%d,%d,c,%d,%s%s,%s", 
				numlines, thisline, 
				rte->rte_num ? rte->rte_num : route_out_count,
				expbuf,
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
	mag_sargs,
	CET_CHARSET_ASCII, 0	/* CET-REVIEW */
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
	mag_fargs,
	CET_CHARSET_ASCII, 0	/* CET-REVIEW */
};

/*
 * Extended (Explorist) entry tables.
 */
ff_vecs_t magX_fvecs = {
	ff_type_file,
	FF_CAP_RW_ALL,
	magX_rd_init,	
	magX_wr_init,	
	mag_deinit,	
	mag_deinit,	
	mag_read,
	mag_write,
	NULL,
	mag_fargs,
	CET_CHARSET_ASCII, 0	/* CET-REVIEW */
};
