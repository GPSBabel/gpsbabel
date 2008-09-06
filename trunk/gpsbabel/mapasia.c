/*

    Support for MapAsia (.tr7) track file format.

    Copyright (C) 2008 Olaf Klein, o.b.klein@gpsbabel.org

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
#include <math.h>
#include <string.h>
#include <time.h>
#include "defs.h"
#include "session.h"

#define MYNAME "mapasia"

#define TR7_TRACK_MAGIC	0x223EADB

#define TR7_S_SIZE	32

#define TR7_S_YEAR	0
#define TR7_S_MONTH	2
#define TR7_S_DAY	6
#define TR7_S_HOUR	8
#define TR7_S_MIN	10
#define TR7_S_SEC	12
#define TR7_S_LON	16
#define TR7_S_LAT	20
#define TR7_S_SPEED	24
#define TR7_S_COURSE	26
#define TR7_S_VALID	28
#define TR7_S_FIX	29

static gbfile *fin, *fout;
static const waypoint *wpt_tmp;
static const route_head *trk_tmp;
static int course_tmp, speed_tmp;

static
arglist_t tr7_args[] = {
	ARG_TERMINATOR
};


/*******************************************************************************
* %%%        global callbacks called by gpsbabel main process              %%% *
*******************************************************************************/

static void
tr7_rd_init(const char *fname)
{
	fin = gbfopen_le(fname, "rb", MYNAME);
}

static void 
tr7_rd_deinit(void)
{
	gbfclose(fin);
}

static void
tr7_read(void)
{
	route_head *trk = NULL;
	unsigned int magic;
	waypoint *prev = NULL;
	
	magic = gbfgetint32(fin);
	if (magic != TR7_TRACK_MAGIC) {
		fatal(MYNAME ": Invalid magic number in header (%X, but %X expected)!\n", magic, TR7_TRACK_MAGIC);
	}

	while (! gbfeof(fin)) {
		unsigned char buff[TR7_S_SIZE];
		double lat, lon;
		struct tm tm;
		waypoint *wpt;

		gbfread(buff, 1, sizeof(buff), fin);

		memset(&tm, 0, sizeof(tm));

		lat = (double)le_read32(&buff[TR7_S_LAT]) / 1000000.0;
		lon = (double)le_read32(&buff[TR7_S_LON]) / 1000000.0;

		if ((fabs(lat) > 90) || (fabs(lon) > 180)) {	/* that really happens */
			trk = NULL;	
			continue;
		}

		tm.tm_year = le_read16(&buff[TR7_S_YEAR]);
		tm.tm_mon = buff[TR7_S_MONTH];
		tm.tm_mday = buff[TR7_S_DAY];
		tm.tm_hour = buff[TR7_S_HOUR];
		tm.tm_min = buff[TR7_S_MIN];
		tm.tm_sec = buff[TR7_S_SEC];

		wpt = waypt_new();

		wpt->latitude = lat;
		wpt->longitude = lon;

		/* create only valid timestamps */
		if (tm.tm_mday && (tm.tm_mday <= 31) && tm.tm_mon && (tm.tm_mon <= 12) && (tm.tm_year > 1970)) {
			tm.tm_year -= 1900;
			tm.tm_mon -= 1;
			wpt->creation_time = mkgmtime(&tm);
		}

		WAYPT_SET(wpt, course, 360 - le_read16(&buff[TR7_S_COURSE]));
		WAYPT_SET(wpt, speed, KPH_TO_MPS(le_read16(&buff[TR7_S_SPEED])));

#if 0		/* unsure, not validated items */
		wpt->fix = buff[TR7_S_FIX];
		if (buff[TR7_S_VALID] != 'A') {
			waypt_free(wpt);
			continue;
		}
#endif
		if (waypt_speed(prev, wpt) > 9999.9) {	/* filter out some bad trackpoints */
			waypt_free(wpt);
			continue;
		}

		if (prev) {	/* other track or bad timestamp */
			if (wpt->creation_time && (prev->creation_time > wpt->creation_time)) trk = NULL;
			else if (waypt_distance(prev, wpt) > 9999.9) trk = NULL;
		}

		if (! trk) {
			trk = route_head_alloc();
			track_add_head(trk);
		}
		track_add_wpt(trk, wpt);
		prev = wpt;
	}
}

/**************************************************************************/

static void
tr7_disp_track_head_cb(const route_head *trk)
{
	wpt_tmp = NULL;
}

static void
tr7_disp_waypt_cb(const waypoint *wpt)
{
	unsigned char buff[TR7_S_SIZE];
	struct tm tm;
	double speed, course;
	
	memset(buff, 0, sizeof(buff));
	
	le_write32(&buff[TR7_S_LON], (int)(wpt->longitude * 1000000.0));
	le_write32(&buff[TR7_S_LAT], (int)(wpt->latitude * 1000000.0));

	if WAYPT_HAS(wpt, course) course = wpt->course;
	else if (wpt_tmp != NULL) course =  waypt_course(wpt_tmp, wpt);
	else course = -1;
	if (course >= 0) le_write16(&buff[TR7_S_COURSE], (int)(360 - course));

	if (wpt->creation_time) {
		tm = *gmtime(&wpt->creation_time);

		le_write16(&buff[TR7_S_YEAR], tm.tm_year + 1900);
		buff[TR7_S_MONTH] = tm.tm_mon + 1;
		buff[TR7_S_DAY] = tm.tm_mday;
		buff[TR7_S_HOUR] = tm.tm_hour;
		buff[TR7_S_MIN] = tm.tm_min;
		buff[TR7_S_SEC] = tm.tm_sec;

		if WAYPT_HAS(wpt, speed) speed = wpt->speed;
		else if (wpt_tmp != NULL) speed = waypt_speed(wpt_tmp, wpt);
		else speed = -1;
		if (speed >= 0) le_write16(&buff[TR7_S_SPEED], (int)MPS_TO_KPH(speed));
	}
	buff[TR7_S_VALID] = 'A';	/* meaning unknown */

#if 0	/* not validated */
	if (wpt->fix != fix_unknown) buff[TR7_S_FIX] = wpt->fix;
#endif
	gbfwrite(buff, 1, sizeof(buff), fout);
	
	wpt_tmp = wpt;
}

static void
tr7_wr_init(const char *fname)
{
	fout = gbfopen_le(fname, "wb", MYNAME);
	gbfputint32(TR7_TRACK_MAGIC, fout);
}

static void
tr7_check_after_read_head_cb(const route_head *trk)
{
	trk_tmp = trk;
	course_tmp = 0;
	speed_tmp = 0;
}

static void
tr7_check_after_read_wpt_cb(const waypoint *wpt)
{
	if (wpt->speed != 0) speed_tmp = 1;
	if (wpt->course != 360.0) course_tmp = 1;	
}

static void
tr7_check_after_read_trailer_cb(const route_head *trk)
{
	queue *elem, *tmp;
	QUEUE_FOR_EACH((queue *)&trk->waypoint_list, elem, tmp) {
		waypoint *wpt = (waypoint *)elem;
		if (speed_tmp == 0) WAYPT_UNSET(wpt, speed);
		if (course_tmp == 0) {
			WAYPT_UNSET(wpt, course);
			wpt->course = 0;
		}
	}
}

static void
tr7_wr_deinit(void)
{
	track_disp_session(curr_session(), 
		tr7_check_after_read_head_cb, 
		tr7_check_after_read_trailer_cb,
		tr7_check_after_read_wpt_cb);
	gbfclose(fout);
}

static void
tr7_write(void)
{
	track_disp_all(tr7_disp_track_head_cb, NULL, tr7_disp_waypt_cb);
}

/**************************************************************************/

ff_vecs_t mapasia_tr7_vecs = {		/* we can read and write tracks */
	ff_type_file,
	{ 
		ff_cap_none 			/* waypoints */, 
	  	ff_cap_read | ff_cap_write	/* tracks */, 
	  	ff_cap_none			/* routes */
	},
	tr7_rd_init,	
	tr7_wr_init,
	tr7_rd_deinit,	
	tr7_wr_deinit,
	tr7_read,
	tr7_write,
	NULL,
	tr7_args,
	CET_CHARSET_UTF8, 1	/* FIXED - CET-REVIEW - */

};

/**************************************************************************/
