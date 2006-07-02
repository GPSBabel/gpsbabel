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
#include "jeeps/gpsserial.h"
#include "grtcirc.h"
#include <errno.h>

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

static gpsdevh *fd;
static char *port;
static char *erase;

#define MYNAME      "WBT-100/200"
#define PRESTRKNAME "PRESALTTRK"
#define NL          "\x0D\x0A"

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

/* Read a single character from the serial port. Kind of gross but we do
 * it like this so we can use Jeeps (which doesn't have an equivalent
 * function). Returns -1 if no char is available, -2 on error or the
 * retrieved char.
 */
static int rd_char() {
    if (GPS_Serial_Chars_Ready(fd)) {
        unsigned char c;
        if (GPS_Serial_Read(fd, &c, 1) != 1) {
            return -2;
        } else {
            return c;
        }
    } else {
        return -1;
    }
}

/* Blocking version of above. It would be nicer to use select on the fd
 * rather than spinning in a loop here - but we're trying to stay platform
 * independent.
 */
static int rd_char_b() {
    int c = rd_char();
    while (c == -1) {
        c = rd_char();
    }
    
    return c;
}

/* Swallow any pending output from GPS */

static int rd_drain() {
    int c = rd_char();
    while (c >= 0) {
        c = rd_char();
    }
    
    return c == -1 ? 0 : c;
}

/* Read a line (up to 0x0A). Carriage returns are filtered out. Always tries to
 * read an entire line but discards any characters beyond len (because we're
 * only ever interested in fairly short lines.
 *
 * Returns the number of characters read or -2 on error. The buffer will contain
 * the (possibly truncated) string without any line terminator characters. The
 * buffer will always be null terminated.
 */
static int rd_line(char *buf, int len) {
    int c, pos = 0, nr = 0;
    c = rd_char_b();
    while (c >= 0 && c != 0x0A) {
        nr++;
        if (c != 0x0D && pos < len-1) {
            buf[pos++] = (unsigned char) c;
        }
        c = rd_char_b();
    }
    
    buf[pos] = '\0';
    
    return c < 0 ? c : nr;
}

static int wr_cmd(const char *cmd) {
    return GPS_Serial_Write(fd, cmd, strlen(cmd));
}

static void rd_init(const char *fname) {
    port = xstrdup(fname);

    db(1, "Opening port...\n");
    if (!GPS_Serial_On(port, &fd)) {
        fatal(MYNAME ": Can't initialise port '%s'\n", port);
    }
}

static void rd_deinit(void) {
    db(1, "Closing port...\n");
    if (!GPS_Serial_Off(fd)) {
        fatal(MYNAME ": Can't shut down port '%s'\n", port);
    }

    xfree(port);
}

static int starts_with(const char *buf, const char *pat) {
    return memcmp(buf, pat, strlen(pat)) == 0;
}

/* Send a command then wait for a line starting with the command string
 * to be returned.
 */
 
static void do_cmd(const char *cmd, const char *expect, char *buf, int len) {
    int rc, try;

    if (rd_drain() < 0) {
        fatal(MYNAME ": Read error\n");
    }

    wr_cmd(cmd); 
    wr_cmd(NL);

    db(2, "Cmd: %s\n", cmd);

    /* We may need to skip a load of data to start with - the unit streams
     * NMEA data all the time so it's highly likely that it'll be in the
     * middle of an NMEA sentence when we start listening.
     */
    for (try = 0; try < RETRIES; try++) {
        if (rc = rd_line(buf, len), rc < 0) {
            fatal(MYNAME ": Read error\n");
        }
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

static void hd(const void *d, int len) {
    const unsigned char *dd = d;
    while (len-- > 0) {
        db(3, "%02x ", *dd++);
    }
    db(3, "\n");
}

static void rd_buf(void *buf, int len) {
    char *bp = buf;
    
    while (len > 0) {
        int rc = GPS_Serial_Read(fd, bp, len);
        if (rc < 0) {
            fatal(MYNAME ": Read error\n");
        }
        len -= rc;
        bp  += rc;
    }
}

static void data_read(void) {
    /* Awooga! Awooga! Statically allocated buffer danger!
     * Actually, it's OK because rd_line can read arbitrarily
     * long lines returning only the first N characters
     */
    char        line_buf[100];
    int         count, d;
    gbuint32    tim, ptim;
    double      lat, lon;
    double      plat, plon;     /* previous point */
    struct tm   t;
    time_t      rtim;
	route_head	*route_head = NULL; 
	waypoint	*wpt        = NULL;

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
        tim = le_read32(line_buf + 0);
        
        lat = (double) ((gbint32) le_read32(line_buf + 4)) / 10000000;
        lon = (double) ((gbint32) le_read32(line_buf + 8)) / 10000000;
        
        t.tm_sec    = ((tim >>  0) & 0x3F);
        t.tm_min    = ((tim >>  6) & 0x3F);
        t.tm_hour   = ((tim >> 12) & 0x1F);
        t.tm_mday   = ((tim >> 17) & 0x1F);
        t.tm_mon    = ((tim >> 22) & 0x0F) - 1;
        t.tm_year   = ((tim >> 26) & 0x3F) + 100;
        
        rtim = mkgmtime(&t);

        if (lat >= 100) {
            /* Start new track */
            lat -= 100;
            route_head = NULL;
        } else {
    		wpt = waypt_new();
		
    		wpt->latitude	    = lat;;
    		wpt->longitude	    = lon;
    		wpt->creation_time  = rtim;
    		wpt->centiseconds   = 0;
		
    		/* OK to reuse buffer now */
    		sprintf(line_buf, "WP%04d", d + 1);
    		wpt->shortname      = xstrdup(line_buf);
    		
			wpt->speed          = radtometers(
		                            gcdist(RAD(plat), RAD(plon), 
		                                   RAD(lat),  RAD(lon))) /
			                      (rtim - ptim);
			wpt->course         = DEG(heading(RAD(plat), RAD(plon),
		                                      RAD(lat),  RAD(lon)));
    		wpt->pdop	        = 0;
    		wpt->fix 		    = fix_unknown;
    		wpt->sat            = 0;

    		if (NULL == route_head)	{
    		    db(1, "New Track\n");
    			route_head = route_head_alloc();
    			track_add_head(route_head);
    		}

    		track_add_wpt(route_head, wpt);
        }
		
		ptim = rtim;
		plat = lat;
		plon = lon;
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

static arglist_t wbt_args[] = {
    { "erase", &erase, "Erase device data after download", 
        "0", ARGTYPE_BOOL, ARG_NOMINMAX },
    ARG_TERMINATOR
};

ff_vecs_t wbt_vecs = {
    ff_type_serial,
    { ff_cap_none, ff_cap_read, ff_cap_none },
    rd_init,
    NULL,
    rd_deinit,
    NULL,
    data_read,
    NULL,
    NULL, 
    wbt_args,
    CET_CHARSET_UTF8, 1         /* master process: don't convert anything | CET-REVIEW */
};
