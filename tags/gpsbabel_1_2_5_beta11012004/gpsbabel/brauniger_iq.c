/*
 * Serial download of barograph data from a Brauniger IQ Variometer.
 * 
 * Copyright (C) 2004 Chris Jones
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
#include <errno.h>

static int32 fd;
static char *port;

#define MYNAME "BRAUNIGER-IQ"
#define PRESTRKNAME "PRESALTTRK"

static enum {
    st_sync,
    st_fl_num,
    st_data_len,
    st_ser_num,
    st_pilot_name,
    st_start_date,
    st_start_year,
    st_max_alt_1,
    st_max_alt_2,
    st_max_climb,
    st_flight_dur,
    st_log_ival,
    st_start_time,
    st_end_time,
    st_sample_alt,
    st_sample_spd,
    num_states
} state;

static const int reqd_bytes[num_states] = { 6, 1, 2, 2, 25, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 1 };

static void rd_init(const char *fname)
{
    port = xstrdup(fname);

    // Fortunately the instruments use the same serial settings as the Garmin
    // GPS receivers (9600-8-N) so we can use jeeps and hence we don't need
    // to worry about OS dependencies here.
    if (!GPS_Serial_On(port, &fd)) {
	fatal(MYNAME ": Can't initialise port '%s'\n", port);
    }
}

static void rd_deinit(void)
{
    if (!GPS_Serial_Off(port, fd)) {
	fatal(MYNAME ": Can't shut down port '%s'\n", port);
    }
    if (!GPS_Serial_Close(fd, port)) {
	fatal(MYNAME ": Can't close port '%s'\n", port);
    }
    xfree(port);
}

/**
 * Process a data record.
 * @return zero when all expected data has been received
 */
static int process_data(const unsigned char *data)
{
    static int remaining = 100;
    static struct tm tm;
    static time_t start, creation;
    static route_head *track;
    static unsigned char interval;
    time_t finish;
    waypoint *wpt = NULL;
    int i;

    if (global_opts.debug_level >= 3) {
	for (i = 0; i < reqd_bytes[state]; i++) {
	    printf("%.2x ", data[i]);
	}
	puts("");
    }

    remaining -= reqd_bytes[state];
    switch (state) {
    case st_sync:
	if (memcmp(data, "\x30\x31\x32\x33\x34\x35", 6) != 0) {
	    fatal(MYNAME ": Could not synchronise\n");
	}
	break;

    case st_fl_num:
	if (global_opts.debug_level >= 1) {
	    printf(MYNAME ": Flight Number: %d\n", data[0]);
	}
	break;

    case st_data_len:
	remaining = (data[0] << 8) + data[1] - 2;
	if (global_opts.debug_level >= 1) {
	    printf(MYNAME ": Data Length: %d\n", remaining);
	}
	break;

    case st_ser_num:
	if (global_opts.debug_level >= 1) {
	    printf(MYNAME ": Serial Number: %d\n", (data[0] << 8) + data[1]);
	}
	break;

    case st_pilot_name:
	if (global_opts.debug_level >= 1) {
	    printf(MYNAME ": Pilot Name: %.25s\n", data);
	}
	break;

    case st_start_date:
	i = (data[0] << 8) + data[1];
	tm.tm_mday = i / 100;
	tm.tm_mon = (i % 100) - 1;
	break;

    case st_start_year:
	tm.tm_year = ((data[0] << 8) + data[1]) - 1900;
	break;

    case st_max_alt_1:
	if (global_opts.debug_level >= 1) {
	    printf(MYNAME ": Max Altitude 1: %dm\n", (data[0] << 8) + data[1]);
	}
	break;

    case st_max_alt_2:
	if (global_opts.debug_level >= 1) {
	    printf(MYNAME ": Max Altitude 2: %dm\n", (data[0] << 8) + data[1]);
	}
	break;

    case st_max_climb:
	if (global_opts.debug_level >= 1) {
	    i = (data[0] << 8) + data[1];
	    printf(MYNAME ": Max climb: %d.%dm/s\n", i / 10, i % 10);
	}
	break;

    case st_flight_dur:
	if (global_opts.debug_level >= 1) {
	    i = (data[0] << 8) + data[1];
	    printf(MYNAME ": Flight Time: %d:%d\n", i / 100, i % 100);
	}
	break;

    case st_log_ival:
	interval = data[0];
	if (global_opts.debug_level >= 1) {
	    printf(MYNAME ": Logging Interval: %ds\n", interval);
	}
	break;

    case st_start_time:
	i = (data[0] << 8) + data[1];
	tm.tm_hour = i / 100;
	tm.tm_min = (i % 100) - 1;
	tm.tm_sec = 0;
	creation = start = mktime(&tm);
	if (global_opts.debug_level >= 1) {
	    printf(MYNAME ": Start Time: %s", ctime(&start));
	}
	break;

    case st_end_time:
	i = (data[0] << 8) + data[1];
	tm.tm_hour = i / 100;
	tm.tm_min = (i % 100) - 1;
	finish = mktime(&tm);
	if (global_opts.debug_level >= 1) {
	    printf(MYNAME ": End Time: %s", ctime(&finish));
	}
	if (remaining) {
	    track = route_head_alloc();
	    track->rte_name = xstrdup(PRESTRKNAME);
	    track->rte_desc = xstrdup("Brauniger-IQ Barograph");
	    track_add_head(track);
	} else {
	    warning(MYNAME ": No barograph recorded for this flight\n");
	}
	break;

    case st_sample_alt:
	wpt = waypt_new();
	wpt->latitude = wpt->longitude = 0.0;
	wpt->creation_time = creation;
	creation += interval;
	wpt->altitude = (data[0] << 8) + data[1];
	route_add_wpt(track, wpt);
	if (global_opts.debug_level >= 2) {
	    printf(MYNAME ": remaining=%d, Altitude=%fm, ", remaining, wpt->altitude);
	}
	break;

    case st_sample_spd:
	if (global_opts.debug_level >= 2) {
	    printf("Airspeed=%dkmh\n", data[0]);
	}
	state = st_sample_alt;
	return remaining;

    default:
	fatal(MYNAME ": Bad internal state\n");
    }
    state++;
    return remaining;
}

static void data_read(void)
{
    unsigned char ibuf[25];
    int32 rd_cnt, ofs;

    if (global_opts.debug_level >= 0) {
	puts(MYNAME ":  Select recorded flight in memo mode.");
	puts(MYNAME ":  Press Memo button for two seconds...");
    }
    // Wait until something arrives
    while (!GPS_Serial_Wait(fd));
    if (global_opts.debug_level >= 0) {
	puts(MYNAME ":  Downloading flight...");
    }
    // Read data until there is none left to read
    state = st_sync;
    ofs = 0;
    do {
	if (0 > (rd_cnt = GPS_Serial_Read(fd, ibuf + ofs, reqd_bytes[state] - ofs))) {
	    fatal(MYNAME ": Error reading port '%s', %s\n", port, strerror(errno));
	}
	if (reqd_bytes[state] == rd_cnt + ofs) {
	    if (!process_data(ibuf)) {
		if (global_opts.debug_level >= 0) {
		    puts(MYNAME "  ...Finished");
		}
		return;
	    }
	    ofs = 0;
	} else {
	    ofs += rd_cnt;
	}
    } while (GPS_Serial_Wait(fd));
    fatal(MYNAME ": Incomplete download\n");
}

static arglist_t brauniger_iq_args[] = {
    {0, 0, 0, 0, 0}
};

ff_vecs_t brauniger_iq_vecs = {
    ff_type_serial,
    rd_init,
    NULL,
    rd_deinit,
    NULL,
    data_read,
    NULL,
    NULL, 
    brauniger_iq_args
};
