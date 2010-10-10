/*
    Write points to SubRip subtitle file (for video geotagging)
 
    Copyright (C) 2010 Michael von Glasow, michael @t vonglasow d.t com

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
 
#include "defs.h"
#include <time.h>

#define MYNAME "subrip"

static char * opt_videotime;
static char * opt_gpstime;
static char * opt_gpsdate;
static time_t time_offset;
static int stnum;
static gbfile *fout;
static const waypoint * prevwpp;

/* internal helper functions */

static time_t
sync_time(time_t arg_gpstime, char * arg_videotime)
{
	static time_t videotime_t;
	static struct tm * ptm_video;
	static time_t result;

	videotime_t = 0;
	ptm_video = gmtime(&videotime_t);
	if (arg_videotime)
	{
		sscanf(arg_videotime, "%2d%2d%2d", &ptm_video->tm_hour, &ptm_video->tm_min, &ptm_video->tm_sec);
	}
	videotime_t = mkgmtime(ptm_video);
	result = (arg_gpstime - videotime_t);
	return result;
}

static time_t
gps_to_video_time(time_t arg_gpstime)
{
	static time_t result;
	/* Converts a GPS timestamp to relative time in the video stream. */
	result = arg_gpstime - time_offset;
	return result;
}

static void
subrip_write_duration(time_t starttime, time_t endtime)
{
	/* Writes start and end time for subtitle display to file. */
	struct tm * tmptime;
	
	tmptime = gmtime(&starttime);
	gbfprintf(fout, "%02d:%02d:%02d,000 --> ", tmptime->tm_hour, tmptime->tm_min, tmptime->tm_sec);
	
	tmptime = gmtime(&endtime);
	gbfprintf(fout, "%02d:%02d:%02d,000\n", tmptime->tm_hour, tmptime->tm_min, tmptime->tm_sec);
}

static void
subrip_write_time(time_t arg_time)
{
	/* Writes a timestamp to file. */
	struct tm * tmptime;
	
	tmptime = gmtime(&arg_time);
	gbfprintf(fout, "%02d:%02d:%02d", tmptime->tm_hour, tmptime->tm_min, tmptime->tm_sec);
}

static void
subrip_prevwp_pr(const waypoint *waypointp)
{
	/* Now that we have the next waypoint, we can write out the subtitle for
	 * the previous one.
	 */
	time_t starttime;
	time_t endtime;
	
	if (prevwpp->creation_time >= time_offset)
	/* if this condition is not true, the waypoint is before the beginning of 
	 * the video and will be ignored
	 */
	{
		
		starttime = gps_to_video_time(prevwpp->creation_time);
		if (!waypointp)
		{
			endtime = starttime + 1;
		}
		else
		{
			endtime = gps_to_video_time(waypointp->creation_time);
		}
		gbfprintf(fout, "%d\n", stnum);
		stnum++;
		subrip_write_duration(starttime, endtime);
		if WAYPT_HAS(prevwpp, speed) 
		{
			gbfprintf(fout, "%d km/h", 
				(int) (MPS_TO_KPH(prevwpp->speed) + 0.5));
		}
		if (prevwpp->altitude != unknown_alt)
		{
			if WAYPT_HAS(prevwpp, speed) 
			{
				gbfprintf(fout, ", ");
			}
			gbfprintf(fout, "%d m\n", 
				(int) (prevwpp->altitude + 0.5));
		}
		else if WAYPT_HAS(prevwpp, speed) 
		{
			gbfprintf(fout, "\n");
		}
		subrip_write_time(prevwpp->creation_time);
		gbfprintf(fout, " Lat=%0.5lf Lon=%0.5lf\n",
			prevwpp->latitude + .000005,
			prevwpp->longitude + .000005);
		gbfprintf(fout, "\n");
	}
}

/* callback functions */

static void
subrip_trkpt_pr(const waypoint *waypointp)
{
	/* 
	 * To determine the duration of the subtitle, we need the timestamp of the
	 * associated waypoint plus that of the following one.
	 * Since we get waypoints one at a time, the only way is to store one and
	 * defer processing until we get the next one.
	 */
	if ((stnum == 1) && (time_offset == 0))
	/* 
	 * esoteric bug: GPS tracks created on Jan 1, 1970 at midnight would cause
	 * undesirable behavior here. But if you run into this problem, I assume
	 * you are capable of time-travel as well as inventing a high-tech system
	 * some 20 years before the rest of mankind does, so finding a prettier
	 * way of solving this should be trivial to you :-)
	 */
	{
		time_offset = sync_time(waypointp->creation_time, opt_videotime);
	}
	
	if (prevwpp)
	{
		subrip_prevwp_pr(waypointp);
	}
	prevwpp = waypointp;
}

/* global callback (exported) functions */

static void
subrip_wr_init(const char *fname)
{
	time_t gpstime_t;
	struct tm * ptm_gps;

	stnum = 1;
	
	time_offset = 0;
	
	prevwpp = NULL;

	if ((opt_gpstime != NULL) && (opt_gpsdate != NULL))
	{
		time (&gpstime_t);
		ptm_gps = gmtime(&gpstime_t);
		if (opt_gpstime)
		{
			sscanf(opt_gpstime, "%2d%2d%2d", &ptm_gps->tm_hour, &ptm_gps->tm_min, &ptm_gps->tm_sec);
		}
		if (opt_gpsdate)
		{
			sscanf(opt_gpsdate, "%4d%2d%2d", &ptm_gps->tm_year, &ptm_gps->tm_mon, &ptm_gps->tm_mday);
			/* 
			 * Don't ask me why we need to do this nonsense, but it seems to be necessary:
			 * Years are two-digit since this was fashionable in the mid-1900s.
			 * For dates after 2000, just add 100 to the year.
			 * Months are zero-based (0 is January), but days are one-based.
			 * Makes sense, eh?
			 * Btw: correct dates will result in incorrect timestamps and you'll
			 * never figure out why. Suppose that's to confuse the Russians, 
			 * given that the system was developed during the Cold War. But that
			 * is true for most of Unix.
			 * Make a difference - contribute to ReactOS.
			 */
			ptm_gps->tm_year-=1900;
			ptm_gps->tm_mon--;
		}
		gpstime_t = mkgmtime(ptm_gps);
		time_offset = sync_time(gpstime_t, opt_videotime);

	}

	fout = gbfopen(fname, "wb", MYNAME);

}

static void
subrip_wr_deinit(void)
{
	gbfclose(fout);
}

static void
subrip_write(void)
{
	track_disp_all(NULL, NULL, subrip_trkpt_pr);
	
	/* 
	 * Due to the necessary hack, one waypoint is still in memory (unless we
	 * did not get any waypoints). Check if there is one and, if so, write it.
	 */
	if (prevwpp)
	{
		subrip_prevwp_pr(NULL);
	}
}

/* arguments: definitions of format-specific arguments */

arglist_t subrip_args[] = {
	// FIXME: document that gps_date and gps_time must be specified together or they will both be ignored and the timestamp of the first trackpoint will be used.
	{"video_time", &opt_videotime, "Video position for which exact GPS time is known (hhmmss, default is 0:00:00)", 0, ARGTYPE_STRING, ARG_NOMINMAX },
	{"gps_time", &opt_gpstime, "GPS time at position video_time (hhmmss, default is first timestamp of track)", 0, ARGTYPE_STRING, ARG_NOMINMAX },
	{"gps_date", &opt_gpsdate, "GPS date at position video_time (hhmmss, default is first timestamp of track)", 0, ARGTYPE_STRING, ARG_NOMINMAX },
	ARG_TERMINATOR
};

/* manifest: capabilities of this module, pointers to exported functions and others */

ff_vecs_t subrip_vecs = {
	ff_type_file,
	{ ff_cap_none, ff_cap_write, ff_cap_none }, // waypoints, track, route; for now, we just do tracks
	NULL,	
	subrip_wr_init,	
	NULL,	
	subrip_wr_deinit,	
	NULL,
	subrip_write,
	NULL,
	subrip_args,
	CET_CHARSET_ASCII, 0
};
