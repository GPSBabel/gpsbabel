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

#define BAUD        9600
#define TIMEOUT     1500

/*
    A conversation looks like this

    >> $PFST,FIRMWAREVERSION
    << $PFST,FIRMWAREVERSION,WBT200,3,31,6090,R2*77
    >> $PFST,NORMAL
    << $PFST,NORMAL,*02
    >> $PFST,READLOGGER
    << $PFST,READLOGGER,*17
    << 0xFFFF, <length>, 0xFFFF
    << (length + 1) * 12 bytes of data
    << ====
    >> $PFST,NORMAL
    << $PFST,NORMAL,*02
*/

/*static gpsdevh *fd;*/
static void *fd;
static FILE *fl;
static char *port;
static char *erase;

#define MYNAME      "WBT-100/200"
#define NL          "\x0D\x0A"

struct read_state {
	route_head	*route_head; 
    double      plat, plon;     /* previous point */
    time_t      ptim;
    unsigned    wpn;
};

/* Number of lines to skip while waiting for an ACK from a command. I've seen
 * conversations with up to 30 lines of cruft before the response so 50 isn't
 * too crazy.
 */
#define RETRIES     50

static void db(int l, const char *msg, ...) {
    va_list ap;
    va_start(ap, msg);
    if (global_opts.debug_level >= l) {
        vprintf(msg, ap);
    }
    va_end(ap);
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
}

static void wr_cmd(const char *cmd) {
    int rc;
    db(3, "Sending: %s\n", cmd);
    if (rc = gbser_print(fd, cmd), gbser_OK != rc) {
        fatal(MYNAME ": Write error (%d)\n", rc);
    }
}

static void rd_init(const char *fname) {
    port = xstrdup(fname);

    db(1, "Opening port...\n");
    if ((fd = gbser_init(port), NULL == fd) ||
        gbser_set_port(fd, BAUD, 8, 0, 1)) {
        fatal(MYNAME ": Can't initialise port \"%s\"\n", port);
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
 
static void do_cmd(const char *cmd, const char *expect, char *buf, int len) {
    int try;

    rd_drain();
    wr_cmd(cmd); 
    wr_cmd(NL);

    db(2, "Cmd: %s\n", cmd);

    /* We may need to skip a load of data to start with - the unit streams
     * NMEA data all the time so it's highly likely that it'll be in the
     * middle of an NMEA sentence when we start listening.
     */
    for (try = 0; try < RETRIES; try++) {
        rd_line(buf, len);
        if (starts_with(buf, expect)) {
            db(2, "Got: %s\n", buf);
            return;
        }
        db(2, "Skip %d: %s\n", try, buf);
    }

    fatal(MYNAME ": Bad response from unit\n");
}

static void do_simple(const char *cmd, char *buf, int len) {
    do_cmd(cmd, cmd, buf, len);
}

static void data_chunk(struct read_state *st, const void *buf) {
    char        wp_name[20];
    gbuint32    tim;
    double      lat, lon;
    struct tm   t;
    time_t      rtim;
	waypoint	*wpt        = NULL;
	const char *bp = buf;
    
    tim = le_read32(bp + 0);
    
    lat = (double) ((gbint32) le_read32(bp + 4)) / 10000000;
    lon = (double) ((gbint32) le_read32(bp + 8)) / 10000000;
    
    t.tm_sec    = ((tim >>  0) & 0x3F);
    t.tm_min    = ((tim >>  6) & 0x3F);
    t.tm_hour   = ((tim >> 12) & 0x1F);
    t.tm_mday   = ((tim >> 17) & 0x1F);
    t.tm_mon    = ((tim >> 22) & 0x0F) - 1;
    t.tm_year   = ((tim >> 26) & 0x3F) + 100;
    
    rtim = mkgmtime(&t);

    if (lat >= 100) {
        /* Start new track in the northern hemisphere */
        lat -= 100;
        st->route_head = NULL;
    } else if (lat <= -100) {
        /* Start new track in the southern hemisphere */
        /* This fix courtesy of Anton Frolich */
        lat += 100;
        st->route_head = NULL;
    } else {
        double speed, gcd, dtim, rtm;
		wpt = waypt_new();
	
		wpt->latitude	    = lat;;
		wpt->longitude	    = lon;
		wpt->creation_time  = rtim;
		wpt->centiseconds   = 0;
	
		sprintf(wp_name, "WP%04d", ++st->wpn);
		wpt->shortname      = xstrdup(wp_name);
		
		/* Broken down to make it easier to find the source of rounding errors */
        gcd                 = gcdist(RAD(st->plat), RAD(st->plon), RAD(lat), RAD(lon));
        gcd                 = (double) ((long) (gcd * 1000000 + 0.5)) / 1000000;
        dtim                = rtim - st->ptim;
        rtm                 = radtometers(gcd);
        speed               = rtm / dtim;
        
        wpt->speed          = speed;
		wpt->course         = heading_true_degrees(RAD(st->plat), RAD(st->plon),
	                                      RAD(lat), RAD(lon));
		wpt->pdop	        = 0;
		wpt->fix 		    = fix_unknown;

		if (NULL == st->route_head)	{
		    db(1, "New Track\n");
			st->route_head = route_head_alloc();
			track_add_head(st->route_head);
		}

		track_add_wpt(st->route_head, wpt);
    }
	
	st->ptim = rtim;
	st->plat = lat;
	st->plon = lon;
}

static void file_read(void) {
    char                buf[12];
    int                 rc;
    struct read_state   st;
    
    st.route_head = NULL;
    st.wpn = 0;
    
    rc = fread(buf, sizeof(buf), 1, fl);
    while (rc == 1) {
        data_chunk(&st, buf);
        rc = fread(buf, sizeof(buf), 1, fl);
    }
}

static void data_read(void) {
    /* Awooga! Awooga! Statically allocated buffer danger!
     * Actually, it's OK because rd_line can read arbitrarily
     * long lines returning only the first N characters
     */
    char                line_buf[100];
    int                 count, d;
    struct read_state   st;
    
    st.route_head = NULL;
    st.wpn = 0;

    do_simple("$PFST,FIRMWAREVERSION", line_buf, sizeof(line_buf));
    do_simple("$PFST,NORMAL",          line_buf, sizeof(line_buf));
    do_simple("$PFST,READLOGGER",      line_buf, sizeof(line_buf));
    
    /* Now we're into binary mode */
    rd_buf(line_buf, 6);            /* six byte header */
    count = le_read16(line_buf + 2) + 1;
    if (count == 0x10000) {
        count = 0;
    }
    
    db(1, "Reading %d data\n", count);
    for (d = 0; d < count; d++) {
        rd_buf(line_buf, 12);       /* twelve byte record */
        data_chunk(&st, line_buf);
    }
    
    /* Erase data? */
    
    if (*erase != '0') {
        int f;
        db(1, "Erasing data\n");
        for (f = 27; f <= 31; f++) {
            sprintf(line_buf, "$PFST,REMOVEFILE,%d", f);
            do_cmd(line_buf, "$PFST,REMOVEFILE", line_buf, sizeof(line_buf));
        }
        db(1, "Reclaiming free space\n");
        for (f = 0; f <= 3; f++) {
            sprintf(line_buf, "$PFST,FFSRECLAIM,%d", f);
            do_cmd(line_buf, "$PFST,FFSRECLAIM", line_buf, sizeof(line_buf));
        }
    }

    do_simple("$PFST,NORMAL", line_buf, sizeof(line_buf));
    
}

static arglist_t wbt_sargs[] = {
    { "erase", &erase, "Erase device data after download", 
        "0", ARGTYPE_BOOL, ARG_NOMINMAX },
    ARG_TERMINATOR
};

ff_vecs_t wbt_svecs = {
    ff_type_serial,
    { ff_cap_none, ff_cap_read, ff_cap_none },
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
