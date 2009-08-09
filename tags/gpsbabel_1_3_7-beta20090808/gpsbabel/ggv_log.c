/*

    Support for "GeoGrid Viewer" binary tracklogs (*.log)

    Copyright (C) 2007 Olaf Klein, o.b.klein@gpsbabel.org

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
#include "grtcirc.h"
#include "jeeps/gpsmath.h"

#define MYNAME "ggv_log"

static gbfile *fin, *fout;
static int ggv_log_ver;

static
arglist_t ggv_log_args[] = {
	ARG_TERMINATOR
};

/*******************************************************************************
* %%%        global callbacks called by gpsbabel main process              %%% *
*******************************************************************************/

static void
ggv_log_rd_init(const char *fname)
{
	static char magic[32];
	int len = 0;

	fin = gbfopen(fname, "rb", MYNAME);

	for (;;) {
		int cin;
		
		cin = gbfgetc(fin);
		if (cin < 0) break;

		magic[len++] = cin;
		
		if (cin == '\0') {
			double ver = 0;
			char *sver;
			if (strncmp(magic, "DOMGVGPS Logfile V", 18) != 0) break;

			sver = &magic[18];
			sscanf(sver, "%lf:", &ver);
			ggv_log_ver = ver * 10;
			if ((ggv_log_ver == 10) || (ggv_log_ver == 25)) return; /* header accepted */

			fatal(MYNAME ": Sorry, unsupported version (%s)!\n", sver);
		}
		else if (len == sizeof(magic))
			break;
	}
	fatal(MYNAME ": Invalid header. Probably no " MYNAME " file!\n");
}

static void 
ggv_log_rd_deinit(void)
{
	gbfclose(fin);
}

static void
ggv_log_read(void)
{
	signed char *buf;
	int bufsz = 0, len;
	route_head *trk = NULL;

	switch(ggv_log_ver) {
		case 10: bufsz = 0x2A; break;
		case 25: bufsz = 0x6F; break;
	}
	
	buf = xmalloc(bufsz);
	
	while ((len = gbfread(buf, 1, bufsz, fin))) {
		int deg, min;
		double xlat, xlon;
		float sec;
		struct tm tm;
		waypoint *wpt;
		
		if (len != bufsz) break;
		
		if (trk == NULL) {
			trk = route_head_alloc();
			track_add_head(trk);
		}

		memset(&tm, 0, sizeof(tm));

		wpt = waypt_new();

		deg = (gbint16) le_read16(&buf[0]);
		min = le_read16(&buf[2]);
		sec = le_read_float(&buf[4]);
		xlat = (double)deg + ((double)min / (double)60) + (sec / (double)3600.0);
		wpt->latitude = xlat;
			
		deg = (gbint16) le_read16(&buf[8]);
		min = le_read16(&buf[10]);
		sec = le_read_float(&buf[12]);
		xlon = (double)deg + ((double)min / (double)60) + (sec / (double)3600.0);
		wpt->longitude = xlon;
		
		WAYPT_SET(wpt, course, le_read16(&buf[16 + 0]));
		
		if (ggv_log_ver == 10) {
			double secs;
			
			wpt->altitude = le_read16(&buf[16 +  2]);
			WAYPT_SET(wpt, speed, le_read16(&buf[16 +  4]));
			tm.tm_year =    le_read16(&buf[16 +  8]);
			tm.tm_mon =     le_read16(&buf[16 + 10]);
			tm.tm_mday =    le_read16(&buf[16 + 12]);
			tm.tm_hour =    le_read16(&buf[16 + 14]);
			tm.tm_min =     le_read16(&buf[16 + 16]);
			secs =          le_read_double(&buf[16 + 18]);
			tm.tm_sec = (int)secs;
			wpt->microseconds = (secs - tm.tm_sec) * 1000000;
		}
		else {
			wpt->altitude = le_read16(&buf[16 + 4]);
			wpt->sat = (unsigned char)buf[16 + 14];
			
			/* other probably valid double values at offset:
			
			22: 0.0 - 20.0
			43: 0.0 - 59.0
			51: -1.0
			61: -1.0
			79: .. - 20.0 ? speed over ground ? (++)
			87: ? course ?
			95: 0.0 - 3.1 (++)
			103: -1
			
			*/
		}

		if (wpt->altitude == 0)
			wpt->altitude = unknown_alt;

		if (tm.tm_year >= 1900) {
			tm.tm_year -= 1900;
			if (tm.tm_mon > 0) {
				tm.tm_mon--;
				wpt->creation_time = mkgmtime(&tm);
			}
		}
		
		track_add_wpt(trk, wpt);
	}
	xfree(buf);
}

static void
ggv_log_wr_init(const char *fname)
{
	fout = gbfopen(fname, "wb", MYNAME);
	
	gbfputcstr("DOMGVGPS Logfile V1.0:", fout);
}

static void
ggv_log_wr_deinit(void)
{
	gbfclose(fout);
}

static void
ggv_log_track_head_cb(const route_head *trk)
{
	queue *elem, *tmp;
	waypoint *prev = NULL;
	
	QUEUE_FOR_EACH((queue *)&trk->waypoint_list, elem, tmp) {
		double  latmin, lonmin, latsec, lonsec;
		int     latint, lonint;
		double  course = 0, speed = 0;
		struct tm tm;
		waypoint *wpt = (waypoint *)elem;
		double secs = 0;
		
		latint = wpt->latitude;
		lonint = wpt->longitude;
		latmin = 60.0 * (fabs(wpt->latitude) - latint);
		lonmin = 60.0 * (fabs(wpt->longitude) - lonint);
		latsec = 60.0 * (latmin - floor(latmin));
		lonsec = 60.0 * (lonmin - floor(lonmin));
		
		if (wpt->creation_time > 0) {
			tm = *gmtime(&wpt->creation_time);
			tm.tm_mon += 1;
			tm.tm_year += 1900;
		}
		else
			memset(&tm, 0, sizeof(tm));
		
		if (prev != NULL) {
			course = heading_true_degrees(
				prev->latitude, prev->longitude,
				wpt->latitude, wpt->longitude);
			speed = waypt_speed(prev, wpt);
		}
		if (wpt->creation_time > 0)
			secs = (double)tm.tm_sec + ((double)wpt->microseconds / 1000000);

		gbfputint16((gbint16) latint, fout);
		gbfputint16((gbint16) latmin, fout);
		gbfputflt(latsec, fout);
		gbfputint16((gbint16) lonint, fout);
		gbfputint16((gbint16) lonmin, fout);
		gbfputflt(lonsec, fout);
		gbfputint16((gbint16) course, fout);
		gbfputint16((gbint16) (wpt->altitude != unknown_alt) ? wpt->altitude : 0, fout);
		gbfputint16((gbint16) speed, fout);
		gbfputint16(0, fout);
		gbfputint16(tm.tm_year, fout);
		gbfputint16(tm.tm_mon, fout);
		gbfputint16(tm.tm_mday, fout);
		gbfputint16(tm.tm_hour, fout);
		gbfputint16(tm.tm_min, fout);
		gbfputdbl(secs, fout);
		
		prev = wpt;
	}
}

static void
ggv_log_write(void)
{
	track_disp_all(ggv_log_track_head_cb, NULL, NULL);
}

/**************************************************************************/

ff_vecs_t ggv_log_vecs = {
	ff_type_file,
	{ 
		ff_cap_none, 			/* waypoints */
	  	ff_cap_read | ff_cap_write,	/* tracks */
	  	ff_cap_none			/* routes */ 
	},
	ggv_log_rd_init,	
	ggv_log_wr_init,
	ggv_log_rd_deinit,	
	ggv_log_wr_deinit,
	ggv_log_read,
	ggv_log_write,
	NULL,
	ggv_log_args,
	CET_CHARSET_ASCII, 1
};
/**************************************************************************/
