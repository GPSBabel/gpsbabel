 /*

    Support for "Suunto Track Manager" (STM) WaypointPlus files,
    see homepage "http://www.suunto.fi" for more details,

    Copyright (C) 2005 Olaf Klein, o.b.klein@t-online.de

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
#include "csv_util.h"
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>

static FILE *fin;
static FILE *fout;
static route_head *track, *route;
static waypoint *wpt;

#define MYNAME "STMwpp"

#define STM_NOTHING	0
#define STM_WAYPT 	1
#define STM_TRKPT 	2

static int track_index;
static int track_num;
static int what;

static char *index_opt = NULL;

static
arglist_t stmwpp_args[] = 
{
	{"index", &index_opt, "Index of route/track to write (if more the one in source)", 
		NULL, ARGTYPE_INT, "1", NULL },
	{0, 0, 0, 0, 0}
};


static void
stmwpp_rd_init(const char *fname)
{
	fin = xfopen(fname, "r", MYNAME);
	track = NULL;
	route = NULL;
	wpt = NULL;
}

static void
stmwpp_rd_deinit(void)
{
	fclose(fin);
}

static void
stmwpp_data_read(void)
{
	char buff[1024];
	
	what = STM_NOTHING;
	fgets(buff, sizeof(buff), fin);
	
	if (strncmp(buff, "Datum,WGS 84,WGS 84,", 20) != 0)
		fatal(MYNAME ": Invalid GPS datum or not \"WaypointPlus\"\" file!\n");
	
	while (fgets(buff, sizeof(buff), fin) != NULL)
	{
		char *c;
		int column = -1;
		struct tm time;
		
		wpt = NULL;
		memset(&time, 0, sizeof(time));
		
		c = csv_lineparse(buff, ",", "", column++);
		while (c != NULL)
		{
			int new_what;
			
			switch(column)
			{
				case 0:
					if (case_ignore_strcmp(c, "WP") == 0)
					{
						new_what = STM_WAYPT;
					}
					else if (case_ignore_strcmp(c, "TP") == 0)
					{
						new_what = STM_TRKPT;
					}
					else 
						fatal(MYNAME ": Unknown feature \"%s\"!\n", c);
						
					if ((what != STM_NOTHING) && (new_what != what))
						fatal(MYNAME ": Only one feature (route or track) is supported by STM!\n");
						
					what = new_what;
					wpt = waypt_new();
					break;
					
				case 1:
					if (what == STM_TRKPT) column++;	/* no name -> skip column two */
					break;
					
				case 2:
					wpt->shortname = xstrdup(c);
					break;
					
				case 3:
					wpt->latitude = atof(c);
					break;
					
				case 4:
					wpt->longitude = atof(c);
					break;
					
				case 5:
					sscanf(c, "%d/%d/%d", &time.tm_mon, &time.tm_mday, &time.tm_year);
					break;
					
				case 6:
					sscanf(c, "%d:%d:%d.%d", &time.tm_hour, &time.tm_min, &time.tm_sec, &wpt->centiseconds);
					if (what == STM_TRKPT)
						wpt->centiseconds /= 10;
					break;
					
				default:
					break;
			}
			c = csv_lineparse(NULL, ",", "", column++);
		}
		if (wpt != NULL)
		{
			time.tm_year -= 1900;
			time.tm_mon--;
			wpt->creation_time = mkgmtime(&time);

			switch(what)
			{
				case STM_WAYPT:
					waypt_add(waypt_dupe(wpt));
					if (route == NULL)
					{
						route = route_head_alloc();
						route_add_head(route);
					}
					route_add_wpt(route, wpt);
					break;
					
				case STM_TRKPT:
					if (track == NULL)
					{
						track = route_head_alloc();
						track_add_head(track);
					}
					route_add_wpt(track, wpt);
					break;
			}
			wpt = NULL;
		}
	}
}

static void
stmwpp_rw_init(const char *fname)
{
	fout = xfopen(fname, "w", MYNAME);
}

static void
stmwpp_rw_deinit(void)
{
	fclose(fout);
}

static void
stmwpp_track_hdr(const route_head *track)
{
	track_num++;
}

static void
stmwpp_track_tlr(const route_head *track)
{
}

static void
stmwpp_write_double(const double val)
{
	char buff[64];
	char *c;
	
	c = buff + snprintf(buff, sizeof(buff), "%3.7f", val);
	while (*--c == '0') *c = '\0';
	if (*c == '.') *c = '0';
	fprintf(fout, "%s,", buff);
}

static void
stmwpp_waypt_cb(const waypoint *wpt)
{
	char cdate[16], ctime[16];
	struct tm tm;
	
	if (track_index != track_num) return;
	
	tm = *gmtime(&wpt->creation_time);
	tm.tm_year += 1900;
	tm.tm_mon++;
	
	snprintf(cdate, sizeof(cdate), "%02d/%02d/%04d", tm.tm_mon, tm.tm_mday, tm.tm_year);
	snprintf(ctime, sizeof(ctime), "%02d:%02d:%02d", tm.tm_hour, tm.tm_min, tm.tm_sec);
	
	switch(what)
	{
		case STM_WAYPT:
			fprintf(fout, "WP,D,%s,", wpt->shortname);
			break;
			
		case STM_TRKPT:
			fprintf(fout, "TP,D,");
			break;
	}
	stmwpp_write_double(wpt->latitude);
	stmwpp_write_double(wpt->longitude);
	fprintf(fout, "%s,%s", cdate, ctime);
	switch(what)
	{
		case STM_WAYPT:
			fprintf(fout, ".%02d", wpt->centiseconds);
			break;
		case STM_TRKPT:
			fprintf(fout, ".%03d", wpt->centiseconds * 10);
			break;
	}
	fprintf(fout, ",\r\n");
}

static void
stmwpp_data_write(void)
{
	track_num = 0;
	if (index_opt != NULL)
		track_index = atoi(index_opt);
	else
		track_index = 1;
		
	fprintf(fout, "Datum,WGS 84,WGS 84,0,0,0,0,0\r\n");
	
	switch(global_opts.objective)
	{
		case wptdata:
			what = STM_WAYPT;
			track_index = track_num;
			waypt_disp_all(stmwpp_waypt_cb);
			break;
		case rtedata:
			what = STM_WAYPT;
			track_disp_all(stmwpp_track_hdr, stmwpp_track_tlr, stmwpp_waypt_cb);
			break;
		case trkdata:
			what = STM_TRKPT;
			track_disp_all(stmwpp_track_hdr, stmwpp_track_tlr, stmwpp_waypt_cb);
			break;
	}
}

ff_vecs_t stmwpp_vecs = {
	ff_type_file,
	FF_CAP_RW_ALL,
	stmwpp_rd_init,
	stmwpp_rw_init,
	stmwpp_rd_deinit,
	stmwpp_rw_deinit,
	stmwpp_data_read,
	stmwpp_data_write,
	NULL,
	stmwpp_args,
	CET_CHARSET_MS_ANSI, 0
};
