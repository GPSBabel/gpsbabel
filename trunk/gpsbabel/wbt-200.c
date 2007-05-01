/*
 * Serial download of track data from a Wintec WBT-200.
 *
 * Copyright (C) 2006 Andy Armstrong
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2 of the License, or (at your
 * option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place - Suite 330, Boston, MA 02111 USA
 */

#include "defs.h"
#include "gbser.h"
#include "grtcirc.h"
#include <errno.h>

#define MYNAME      "WBT-100/200"
#define NL          "\x0D\x0A"

#define WBT200BAUD   9600

#define WBT201BAUD  57600
#define WBT201CHUNK  4096

#define TIMEOUT      5000

#define RECLEN_V1      12
#define RECLEN_V2      16

#define RECLEN_WBT201  16

/* Used to sanity check data - from
 *   http://hypertextbook.com/facts/2001/DanaWollman.shtml
 * The MAXALT check doesn't need to be enabled unless there's
 * a format with larger records than V2.
 */
/*#define MAXALT      120000*/

#define _MAX(a, b) ((a) > (b) ? (a) : (b))
#define RECLEN_MAX  _MAX(RECLEN_V1, RECLEN_V2)

/* Flags for WBT201 */
enum {
    WBT201_TRACK_START = 0x01,
    WBT201_WAYPOINT    = 0x02,
    WBT201_OVER_SPEED  = 0x04
};

#define BUFSPEC(b) b, sizeof(b)

/* The formats here must be in ascending record length order so that
 * each format identification attempt can read more data from the
 * device if necessary. If that proves to be a bad order to try the
 * heuristics the format matching code will have to be rejigged.
 */
static struct {
    size_t      reclen;
} fmt_version[] = {
    {   RECLEN_V1   },
    {   RECLEN_V2   },
    {   0           }
};

/* Number of lines to skip while waiting for an ACK from a command. I've seen
 * conversations with up to 30 lines of cruft before the response so 60 isn't
 * too crazy.
 */
#define RETRIES     60

/*
    A conversation looks like this

    >> $PFST,FIRMWAREVERSION
    << $PFST,FIRMWAREVERSION,WBT200,3,31,6090,R2*77
    >> $PFST,NORMAL
    << $PFST,NORMAL,*02
    >> $PFST,READLOGGER
    << $PFST,READLOGGER,*17
    << 0xFFFF, <length>, 0xFFFF
    << (length + 1) * 12 or 16 bytes of data
    << ====
    >> $PFST,NORMAL
    << $PFST,NORMAL,*02
*/

static void *fd;
static FILE *fl;
static char *port;
static char *erase;

static enum { 
    UNKNOWN, WBT200, WBT201 
} dev_type = UNKNOWN;

struct buf_chunk {
    struct buf_chunk    *next;
    size_t              size;
    size_t              used;
    /* data follows in memory */
};

#define buf_CHUNK_DATA(c) \
    ((void *) ((struct buf_chunk *) (c) + 1))

#define buf_CHUNK_PTR(c, offset) \
    ((void *) ((char *) buf_CHUNK_DATA(c) + (offset)))

struct buf_head {
    struct buf_chunk    *head;
    struct buf_chunk    *tail;
    size_t              alloc;
    size_t              used;
    /* read position */
    struct buf_chunk    *current;
    unsigned long       offset;
    /* shoehorned in here primarily out of laziness */
    unsigned            checksum;
};

struct read_state {
	route_head	        *route_head;
    unsigned            wpn, tpn;

    struct buf_head     data;
};

static void db(int l, const char *msg, ...) {
    va_list ap;
    va_start(ap, msg);
    if (global_opts.debug_level >= l) {
        vprintf(msg, ap);
    }
    va_end(ap);
}

/* Growable buffer support. TODO: Put this in a separate file and
 * tidy up its interface.
 */

static void buf_init(struct buf_head *h, size_t alloc) {
    h->head     = NULL;
    h->tail     = NULL;
    h->alloc    = alloc;
    h->used     = 0;
    h->checksum = 0;
}

static void buf_empty(struct buf_head *h) {
    struct buf_chunk *chunk, *next;
    for (chunk = h->head; chunk; chunk = next) {
        next = chunk->next;
        xfree(chunk);
    }
    h->head     = NULL;
    h->tail     = NULL;
    h->used     = 0;
    h->checksum = 0;
}

static void buf_rewind(struct buf_head *h) {
    h->current = h->head;
    h->offset  = 0;
}

static size_t buf_read(struct buf_head *h, void *data, size_t len) {
    char *bp = data;

    while (len != 0 && h->current != NULL) {
        size_t avail = h->current->used - h->offset;
        if (avail > len) { avail = len; }

        memcpy(bp, buf_CHUNK_PTR(h->current, h->offset), avail);
        h->offset   += avail;
        bp          += avail;
        len         -= avail;

        if (h->offset == h->current->used) {
            h->current  = h->current->next;
            h->offset   = 0;
        }
    }

    return bp - (char *) data;
}

static void buf_extend(struct buf_head *h, size_t amt) {
    struct buf_chunk *c;
    size_t sz = amt + sizeof(struct buf_chunk);
    if (c = xmalloc(sz), NULL == c) {
        fatal(MYNAME ": Can't allocate %lu bytes for buffer\n", (unsigned long) sz);
    }

    c->next = NULL;
    c->size = amt;
    c->used = 0;

    if (NULL == h->head) {
        h->head = c;
    } else {
        h->tail->next = c;
    }

    h->tail = c;
}

static void buf_update_checksum(struct buf_head *h, const void *data, size_t len) {
    unsigned char *cp = (unsigned char *) data;
    unsigned i;

    db(4, "Updating checksum with %p, %lu, before: %02x ",
        data, (unsigned long) len, h->checksum);
    for (i = 0; i < len; i++) {
        h->checksum ^= cp[i];
    }
    db(4, "after: %02x\n", h->checksum);
}

static void buf_write(struct buf_head *h, const void *data, size_t len) {
    size_t avail;
    const char *bp = data;
    
    buf_update_checksum(h, data, len);

    h->used += len;

    if (NULL == h->tail) {
        buf_extend(h, h->alloc);
    }

    for (;;) {
        avail = h->tail->size - h->tail->used;
        if (avail > len) { avail = len; }

        memcpy((char *) buf_CHUNK_PTR(h->tail, h->tail->used), bp, avail);
        h->tail->used   += avail;
        bp              += avail;
        len             -= avail;
        if (len == 0) {
            break;
        }
        buf_extend(h, h->alloc);
    }
}

static void rd_drain() {
    if (gbser_flush(fd)) {
        fatal(MYNAME ": Comm error\n");
    }
}

static void rd_line(char *buf, int len) {
    int rc;
    if (rc = gbser_read_line(fd, buf, len, TIMEOUT, 0x0A, 0x0D), rc != gbser_OK) {
        fatal(MYNAME ": Read error (%d)\n", rc);
    }
    db(3, "Got response: \"%s\"\n", buf);
}

static void wr_cmd(const char *cmd) {
    int rc;
    db(3, "Sending: %s\n", cmd);
    if (rc = gbser_print(fd, cmd), gbser_OK != rc) {
        fatal(MYNAME ": Write error (%d)\n", rc);
    }
}

static void wr_cmdl(const char *cmd) {
    wr_cmd(cmd);
    wr_cmd(NL);
}

static int expect(const char *str) {
    int state = 0;
    int c, i;
    int errors = 5; /* allow this many errors */
    
    for (i = 0; i < 5000; i++) {
        /* reached end of string */
        if (str[state] == '\0') {
            return 1;
        }
        
        c = gbser_readc_wait(fd, 500);
        if (c < 0) {
            db(3, "Got error: %d\n", c);
            if (--errors <= 0) {
                return 0;
            }
        } else {
            db(3, "Got char: %02x '%c'\n", c, isprint(c) ? c : '.');
            if (c == str[state]) {
                state++;    /* carry on */
            } else {
                state = 0;  /* go back to start */
            }
        }
    }
    
    return 0;
}

static int wbt200_try() {
    int rc;

    db(1, "Trying WBT100/200\n");

    if ((rc = gbser_set_port(fd, WBT200BAUD, 8, 0, 1))) {
        db(1, "Set baud rate to %d failed (%d)\n", WBT200BAUD, rc);
        return 0;
    }

    wr_cmdl("$PFST,NORMAL");
    return expect("$PFST");
}

static int wbt201_try() {
    int rc;
    
    db(1, "Trying WBT201/G-Rays 2\n");

    if ((rc = gbser_set_port(fd, WBT201BAUD, 8, 0, 1))) {
        db(1, "Set baud rate to %d failed (%d)\n", WBT201BAUD, rc);
        return 0;
    }
    
    wr_cmdl("@AL");
    return expect("@AL");
}

static int guess_device() {
    int i;
    db(1, "Guessing device...\n");
    for (i = 0; i < 5; i++) {
        if (wbt200_try()) {
            return WBT200;
        }
        if (wbt201_try()) {
            return WBT201;
        }
    }
    return UNKNOWN;
}

static void rd_init(const char *fname) {
    port = xstrdup(fname);

    db(1, "Opening port...\n");
    if (fd = gbser_init(port), NULL == fd) {
        fatal(MYNAME ": Can't initialise port \"%s\"\n", port);
    }
    
    dev_type = guess_device();
    if (UNKNOWN == dev_type) {
        fatal(MYNAME ": Can't determine device type\n");
    }
}

static void rd_deinit(void) {
    db(1, "Closing port...\n");
    gbser_deinit(fd);
    fd = NULL;
    xfree(port);
}

static void rd_buf(void *buf, int len) {
    int rc;
    if (rc = gbser_read_wait(fd, buf, len, TIMEOUT), rc < 0) {
        fatal(MYNAME ": Read error (%d)\n", rc);
    } else if (rc < len) {
        fatal(MYNAME ": Read timout\n");
    }
}

static void file_init(const char *fname) {
    db(1, "Opening file...\n");
    if (fl = fopen(fname, "rb"), NULL == fl) {
        fatal(MYNAME ": Can't open file '%s'\n", fname);
    }
}

static void file_deinit(void) {
    db(1, "Closing file...\n");
    fclose(fl);
}

static int starts_with(const char *buf, const char *pat) {
    return memcmp(buf, pat, strlen(pat)) == 0;
}

/* Send a command then wait for a line starting with the command string
 * to be returned.
 */
static int do_cmd(const char *cmd, const char *expect, char *buf, int len) {
    int try;

    rd_drain();
    wr_cmdl(cmd);

    db(2, "Cmd: %s\n", cmd);

    /* We may need to skip a load of data to start with - the unit streams
     * NMEA data all the time so it's highly likely that it'll be in the
     * middle of an NMEA sentence when we start listening.
     */
    for (try = 0; try < RETRIES; try++) {
        rd_line(buf, len);
        db(3, "Got: %s\n", buf);
        if (starts_with(buf, expect)) {
            db(2, "Matched: %s\n", buf);
            return strlen(expect);
        }
        db(2, "Skip %d: %s\n", try, buf);
    }

    fatal(MYNAME ": Bad response from unit\n");
    return 0;   /* keep compiler quiet */
}

/* Issue a command that expects the same string to be echoed
 * back as an ACK
 */
static int do_simple(const char *cmd, char *buf, int len) {
    return do_cmd(cmd, cmd, buf, len);
}

static char *get_param(const char *cmd, char *buf, int len) {
    int cl = do_simple(cmd, buf, len);
    return buf + cl + 1;
}

static int get_param_int(const char *cmd) {
    char buf[80];
    return atoi(get_param(cmd, buf, sizeof(buf)));
}

static double get_param_float(const char *cmd) {
    char buf[80];
    return atof(get_param(cmd, buf, sizeof(buf)));
}

/* Decompose binary date into discreet fields */
#define _SPLIT_DATE(tim) \
    int sec    = (((tim) >>  0) & 0x3F); \
    int min    = (((tim) >>  6) & 0x3F); \
    int hour   = (((tim) >> 12) & 0x1F); \
    int mday   = (((tim) >> 17) & 0x1F); \
    int mon    = (((tim) >> 22) & 0x0F); \
    int year   = (((tim) >> 26) & 0x3F);

static time_t decode_date(gbuint32 tim) {
    _SPLIT_DATE(tim)
    struct tm t;

    t.tm_sec    = sec;
    t.tm_min    = min;
    t.tm_hour   = hour;
    t.tm_mday   = mday;
    t.tm_mon    = mon  -   1;
    t.tm_year   = year + 100;

    return mkgmtime(&t);
}

static int check_date(gbuint32 tim) {
    _SPLIT_DATE(tim)

    /* Sanity check the date. We don't allow years prior to 2004 because zero in
    * those bits will usually indicate that we have an altitude instead of a
    * date (i.e. that the data is the new format that uses 16 byte records).
    */
    return sec < 60 && min < 60 && hour < 24 &&
            mday > 0 && mday <= 31 && mon > 0 && mon <= 12 && year >= 4;
}

static waypoint *make_point(double lat, double lon, double alt, time_t tim, const char *fmt, int index) {
    char     wp_name[20];
	waypoint *wpt = waypt_new();

	sprintf(wp_name, fmt, index);

	wpt->latitude	    = lat;;
	wpt->longitude	    = lon;
	wpt->altitude       = alt;
	wpt->creation_time  = tim;
	wpt->shortname      = xstrdup(wp_name);
	
    return wpt;
}

static waypoint *make_waypoint(struct read_state *st, double lat, double lon, double alt, time_t tim) {
    return make_point(lat, lon, alt, tim, "WP%04d", ++st->wpn);
}

static waypoint *make_trackpoint(struct read_state *st, double lat, double lon, double alt, time_t tim) {
    return make_point(lat, lon, alt, tim, "TP%04d", ++st->tpn);
}

static int wbt200_data_chunk(struct read_state *st, const void *buf, int fmt) {
    gbuint32   tim;
    double     lat, lon, alt;
    time_t     rtim;
	waypoint   *tpt     = NULL;
	const char *bp      = buf;
    size_t     buf_used = fmt_version[fmt].reclen;

    tim = le_read32(bp + 0);

    lat = (double) ((gbint32) le_read32(bp + 4)) / 10000000;
    lon = (double) ((gbint32) le_read32(bp + 8)) / 10000000;

    /* Handle extra fields in longer records here. */
    if (buf_used >= 16) {
        alt = (double) le_read32(bp + 12) / 10;
    } else {
        alt = unknown_alt;
    }

    rtim = decode_date(tim);

    if (lat >= 100) {
        /* Start new track in the northern hemisphere */
        lat -= 100;
        st->route_head = NULL;
    } else if (lat <= -100) {
        /* Start new track in the southern hemisphere */
        /* This fix courtesy of Anton Frolich */
        lat += 100;
        st->route_head = NULL;
    }

    tpt = make_trackpoint(st, lat, lon, alt, rtim);

	if (NULL == st->route_head)	{
	    db(1, "New Track\n");
		st->route_head = route_head_alloc();
		track_add_head(st->route_head);
	}

	track_add_wpt(st->route_head, tpt);

    return 1;
}

/* Return true iff the data appears valid with the specified record length */
static int is_valid(struct buf_head *h, int fmt) {
    char buf[RECLEN_MAX];
    size_t reclen = fmt_version[fmt].reclen;

    buf_rewind(h);

    db(2, "Checking %lu bytes of data against format %d\n", h->used, fmt);

    for (;;) {
        size_t got = buf_read(h, buf, reclen);
        gbuint32 tim;
        /* Don't mind odd bytes at the end - we may
         * be examining an incomplete dataset.
         */
        if (got != reclen) {
            break;
        }

        tim = le_read32(buf + 0);
        if (!check_date(tim)) {
            return 0;
        }

        if (reclen > 12) {
#ifdef MAXALT
            gbuint32 alt = le_read32(buf + 12);
            if (alt > MAXALT * 10) {
                return 0;
            }
#endif
        }
    }

    return 1;
}

static void wbt200_process_data(struct read_state *pst, int fmt) {
    char buf[RECLEN_MAX];
    size_t reclen = fmt_version[fmt].reclen;

    buf_rewind(&pst->data);

    db(2, "Processing %lu bytes of data using format %d\n", pst->data.used, fmt);

    for (;;) {
        size_t got = buf_read(&pst->data, buf, reclen);
        if (got != reclen) {
            break;
        }
        wbt200_data_chunk(pst, buf, fmt);
    }
}

static void state_init(struct read_state *pst) {
    pst->route_head = NULL;
    pst->wpn        = 0;
    pst->tpn        = 0;

    buf_init(&pst->data, RECLEN_V1 * RECLEN_V2);
}

static void state_empty(struct read_state *pst) {
    buf_empty(&pst->data);
    state_init(pst);
}

static void file_read(void) {
    char                buf[512];
    size_t              rc;
    struct read_state   st;
    int                 fmt;

    state_init(&st);

    /* Read the whole file into the buffer */
    rc = fread(buf, 1, sizeof(buf), fl);
    while (rc != 0) {
        buf_write(&st.data, buf, rc);
        rc = fread(buf, 1, sizeof(buf), fl);
    }

    if (!feof(fl)) {
        fatal(MYNAME ": Read error\n");
    }

    /* Try to guess the data format */
    for (fmt = 0; fmt_version[fmt].reclen != 0; fmt++) {
        size_t reclen = fmt_version[fmt].reclen;
        if ((st.data.used % reclen) == 0 && is_valid(&st.data, fmt)) {
            break;
        }
    }

    if (fmt_version[fmt].reclen == 0) {
        fatal(MYNAME ": Can't autodetect data format\n");
    }

    wbt200_process_data(&st, fmt);

    state_empty(&st);
}

static void want_bytes(struct buf_head *h, size_t len) {
    char buf[512];

    db(3, "Reading %lu bytes from device\n", (unsigned long) len);

    while (len > 0) {
        size_t want = sizeof(buf);
        if (want > len) { want = len; }
        rd_buf(buf, want);
        buf_write(h, buf, want);
        len -= want;
    }
}

static void wbt200_data_read(void) {
    /* Awooga! Awooga! Statically allocated buffer danger!
     * Actually, it's OK because rd_line can read arbitrarily
     * long lines returning only the first N characters
     */
    char                line_buf[100];
    int                 fmt;
    unsigned long       count;
    struct read_state   st;

    state_init(&st);

    /* We could potentially parse the version string to find out which
     * data format to use - but it's not clear how the version string
     * will increment in the future - so just now it's more future-
     * proof to rely on analysing the data. We need to be able to do
     * that with files anyway - because they're not versioned.
     */
    do_simple("$PFST,FIRMWAREVERSION", BUFSPEC(line_buf));

    do_simple("$PFST,NORMAL",          BUFSPEC(line_buf));
    do_simple("$PFST,READLOGGER",      BUFSPEC(line_buf));

    /* Now we're into binary mode */
    rd_buf(line_buf, 6);            /* six byte header */
    count = le_read16(line_buf + 2) + 1;
    if (count == 0x10000) {
        count = 0;
    }

    db(3, "%lu points available\n", count);

    /* Loop through the known formats requesting more data from the
     * device each time. When the device contains only a single
     * point the first format will get a false positive - so we'll
     * lose the altitude data.
     */
    for (fmt = 0; fmt_version[fmt].reclen != 0; fmt++) {
        size_t reclen = fmt_version[fmt].reclen;
        size_t want   = reclen * count;

        if (want < st.data.used) {
            fatal(MYNAME ": Internal error: formats not ordered in ascending size order\n");
        }

        db(3, "Want %lu bytes of data\n", (unsigned long) want);

        /* Top up the buffer */
        want_bytes(&st.data, want - st.data.used);

        /* And see if it's valid */
        if (is_valid(&st.data, fmt)) {
            break;
        }
    }

    if (fmt_version[fmt].reclen == 0) {
        fatal(MYNAME ": Can't autodetect data format\n");
    }

    wbt200_process_data(&st, fmt);

    /* Erase data? */

    if (*erase != '0') {
        int f;
        db(1, "Erasing data\n");
        for (f = 27; f <= 31; f++) {
            sprintf(line_buf, "$PFST,REMOVEFILE,%d", f);
            do_cmd(line_buf, "$PFST,REMOVEFILE", BUFSPEC(line_buf));
        }
        db(1, "Reclaiming free space\n");
        for (f = 0; f <= 3; f++) {
            sprintf(line_buf, "$PFST,FFSRECLAIM,%d", f);
            do_cmd(line_buf, "$PFST,FFSRECLAIM", BUFSPEC(line_buf));
        }
    }

    do_simple("$PFST,NORMAL", BUFSPEC(line_buf));

    state_empty(&st);
}

static int wbt201_data_chunk(struct read_state *st, const void *buf) {
    gbuint32    tim;
    gbuint16    flags;
    double      lat, lon, alt;
    time_t      rtim;
    waypoint    *tpt     = NULL;
	const char  *bp      = buf;

    flags = le_read16(bp + 0);
    tim   = le_read32(bp + 2);
    lat   = (double) ((gbint32) le_read32(bp +  6)) / 10000000;
    lon   = (double) ((gbint32) le_read32(bp + 10)) / 10000000;
    alt   = (double) ((gbint16) le_read16(bp + 14));

    rtim = decode_date(tim);

    if ((flags & WBT201_WAYPOINT) && (global_opts.masked_objective & WPTDATAMASK)) {
        waypoint *wpt = make_waypoint(st, lat, lon, alt, rtim);
        waypt_add(wpt);
    }

    if (global_opts.masked_objective & TRKDATAMASK) {
        if (flags & WBT201_TRACK_START) {
            st->route_head = NULL;
        }

        tpt = make_trackpoint(st, lat, lon, alt, rtim);

    	if (NULL == st->route_head)	{
    	    db(1, "New Track\n");
    		st->route_head = route_head_alloc();
    		track_add_head(st->route_head);
    	}

    	track_add_wpt(st->route_head, tpt);
    }

    return 1;
}

static void wbt201_process_chunk(struct read_state *st) {
    char buf[RECLEN_WBT201];
    buf_rewind(&st->data);

    db(2, "Processing %lu bytes of data\n", st->data.used);

    for (;;) {
        size_t got = buf_read(&st->data, buf, sizeof(buf));
        if (got != sizeof(buf)) {
            break;
        }
        wbt201_data_chunk(st, buf);
    }
}

static int wbt201_read_chunk(struct read_state *st, unsigned pos, unsigned limit) {
    char cmd_buf[30];
    char line_buf[100];
    unsigned long cs;
    char *lp, *op;
    static char *cs_prefix = "@AL,CS,";

    unsigned want = limit - pos;
    if (want > WBT201CHUNK) {
        want = WBT201CHUNK;
    }

    db(3, "Reading bytes at %u (0x%x), limit = %u (0x%x), want = %u (0x%x)\n",
        pos, pos, limit, limit, want, want);

    buf_empty(&st->data);

    rd_drain();
    sprintf(cmd_buf, "@AL,5,3,%d", pos);
    wr_cmdl(cmd_buf);

    want_bytes(&st->data, want);

    /* checksum */
    rd_line(BUFSPEC(line_buf));

    if (!starts_with(line_buf, cs_prefix)) {
        db(2, "Bad checksum response\n");
        return 0;
    }

    lp = line_buf + strlen(cs_prefix);
    cs = strtoul(lp, &op, 16);
    if (*lp == ',' || *op != ',') {
        db(2, "Badly formed checksum\n");
        return 0;
    }

    if (cs != st->data.checksum) {
        db(2, "Checksums don't match. Got %02x, expected %02\n", cs, st->data.checksum);
        return 0;
    }
    
    /* ack */
    rd_line(BUFSPEC(line_buf));
    return starts_with(line_buf, cmd_buf);
}

static void wbt201_data_read(void) {
    char                line_buf[100];
    struct read_state   st;
    unsigned            tries;

    const char          *tmp;

    double              ver_hw;
    double              ver_sw;
    double              ver_fmt;

    unsigned            log_addr_start;
    unsigned            log_addr_end;
    unsigned            log_area_start;
    unsigned            log_area_end;

    /* Read various device information. We don't use much of this yet -
     * just log_addr_start and log_addr_end - but it's useful to have it
     * here for debug and documentation purposes. 
     */
    tmp = get_param("@AL,7,1", BUFSPEC(line_buf));
    db(1, "Reading device \"%s\"\n", tmp);

    ver_hw         = get_param_float("@AL,8,1");
    ver_sw         = get_param_float("@AL,8,2");
    ver_fmt        = get_param_float("@AL,8,3");

    db(2, "versions: hw=%f, sw=%f, fmt=%f\n", 
        ver_hw, ver_sw, ver_fmt);
    
    log_addr_start = get_param_int("@AL,5,1");  /* we read from here... */
    log_addr_end   = get_param_int("@AL,5,2");  /*  ...to here and ... */
    log_area_start = get_param_int("@AL,5,9");  /*  ...probably don't... */
    log_area_end   = get_param_int("@AL,5,10"); /*  ...need these. */
    
    db(2, "Log addr=(%d..%d), area=(%d..%d)\n", 
        log_addr_start, log_addr_end, 
        log_area_start, log_area_end);

    state_init(&st);

    tries = 10;
    while (log_addr_start < log_addr_end) {
        if (wbt201_read_chunk(&st, log_addr_start, log_addr_end)) {
            wbt201_process_chunk(&st);
            log_addr_start += st.data.used;
        } else {
            if (--tries <= 0) {
                fatal(MYNAME ": Too many data errors during read\n");
            }
        }
    }

    if (*erase != '0') {
        /* erase device */
        do_simple("@AL,5,6", BUFSPEC(line_buf));
    }

    state_empty(&st);
    do_simple("@AL,2,1", BUFSPEC(line_buf));
}

static void data_read(void) {
    switch (dev_type) {
        case WBT200: 
            wbt200_data_read(); 
            break;
            
        case WBT201: 
            wbt201_data_read(); 
            break;
            
        default:
            fatal(MYNAME ": Unknown device type (internal)\n");
            break;
    }
}

static arglist_t wbt_sargs[] = {
    { "erase", &erase, "Erase device data after download",
        "0", ARGTYPE_BOOL, ARG_NOMINMAX },
    ARG_TERMINATOR
};

ff_vecs_t wbt_svecs = {
    ff_type_serial,
    { ff_cap_read, ff_cap_read, ff_cap_none },
    rd_init,
    NULL,
    rd_deinit,
    NULL,
    data_read,
    NULL,
    NULL, 
    wbt_sargs,
    CET_CHARSET_UTF8, 1         /* master process: don't convert anything | CET-REVIEW */
};

static arglist_t wbt_fargs[] = {
    ARG_TERMINATOR
};

ff_vecs_t wbt_fvecs = {
    ff_type_file,
    { ff_cap_none, ff_cap_read, ff_cap_none },
    file_init,
    NULL,
    file_deinit,
    NULL,
    file_read,
    NULL,
    NULL, 
    wbt_fargs,
    CET_CHARSET_UTF8, 1         /* master process: don't convert anything | CET-REVIEW */
};
