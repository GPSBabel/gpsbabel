/*
    Jeeps wrapper for Garmin serial protocol.
  
    Copyright (C) 2002, 2003, 2004, 2005, 2006  Robert Lipe, robertlipe@usa.net

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
#include <limits.h>
#include "defs.h"
#include "jeeps/gps.h"
#include "garmin_tables.h"
#include "garmin_fs.h"

#define SOON 1

#define MYNAME "GARMIN" 
static const char *portname;
static short_handle mkshort_handle;
static GPS_PWay *tx_routelist;
static GPS_PWay *cur_tx_routelist_entry;
static GPS_PTrack *tx_tracklist;
static GPS_PTrack *cur_tx_tracklist_entry;
static char *getposn = NULL;
static char *poweroff = NULL;
static char *snlen = NULL;
static char *snwhiteopt = NULL;
static char *deficon = NULL;
static char *category = NULL;

/* Technically, even this is a little loose as spaces arent allowed */
static char valid_waypt_chars[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZ 0123456789";

static
arglist_t garmin_args[] = {
	{ "snlen", &snlen, "Length of generated shortnames", NULL, 
		ARGTYPE_INT, "1", NULL },
	{ "snwhite", &snwhiteopt, "Allow whitespace synth. shortnames",
		NULL, ARGTYPE_BOOL, ARG_NOMINMAX},
	{ "deficon", &deficon, "Default icon name", NULL, ARGTYPE_STRING, ARG_NOMINMAX },
	{ "get_posn", &getposn, "Return current position as a waypoint", 
		NULL, ARGTYPE_BOOL, ARG_NOMINMAX},
	{ "power_off", &poweroff, "Command unit to power itself down", 
		NULL, ARGTYPE_BOOL, ARG_NOMINMAX},
	{ "category", &category, "Command unit to power itself down", 
		NULL, ARGTYPE_INT, "0", "15"},
	ARG_TERMINATOR
};

static const char * d103_symbol_from_icon_number(unsigned int n);
static int d103_icon_number_from_symbol(const char *s);

static void
rw_init(const char *fname)
{
	int receiver_short_length;
	int receiver_must_upper = 1;
	char * receiver_charset = NULL;

	if (!mkshort_handle)
		mkshort_handle = mkshort_new_handle();

	if (global_opts.debug_level > 0)  {
		GPS_Enable_Warning();
		GPS_Enable_User();
	}
	if (global_opts.debug_level > 1)  {
		GPS_Enable_Diagnose();
	}
	GPS_Enable_Error();

	if (poweroff) {
		GPS_Command_Off(fname);
		return;
	}

        if (GPS_Init(fname) < 0) {
		fatal(MYNAME ":Can't init %s\n", fname);
	}
	portname = fname;

	/*
	 * Grope the unit we're talking to to set setshort_length to 
	 * 	20 for  the V, 
	 * 	10 for Street Pilot, Rhino, 76
	 * 	6 for the III, 12, emap, and etrex
	 * Fortunately, getting this "wrong" only results in ugly names
	 * when we're using the synthesize_shortname path.
	 */
	receiver_short_length = 10;

	switch ( gps_waypt_type )	/* waypoint type as defined by jeeps */
	{
		case 0:
			fatal("Garmin unit %d does not support waypoint xfer.",
				 gps_save_id);

			break;
		case 100:	/* The GARMIN GPS Interface Specification, */
		case 101:	/* says these waypoint types use an ident */
		case 102:	/* length of 6.  Waypoint types 106, 108 */
		case 103:	/* and 109 are all variable  length    */
		case 104:
		case 105:
		case 107:
		case 150:
		case 151:
		case 152:
		case 154:
		case 155:
			receiver_short_length = 6;
			break;
		case 106:	/* Waypoint types with variable ident length */
		case 108: 	/* Need GPSr id to know the actual length */
		case 109:                   
		case 110:                   
			switch ( gps_save_id )
			{
				case 130:	/* Garmin Etrex (yellow) */
					receiver_short_length = 6;
					break;
				case 155:	/* Garmin V */
				case 404:	/* SP2720 */
					receiver_short_length = 20;
					break;
				case 382: 	/* C320 */
					receiver_short_length = 30;
					receiver_must_upper = 0;
					break;
				case 292: /* (60|76)C[s]X series */
				case 421: /* Vista|Legend CX */
					receiver_short_length = 14;
					snwhiteopt = xstrdup("1");
					receiver_must_upper = 0;
					/* This might be 8859-1 */
					receiver_charset = CET_CHARSET_MS_ANSI;
					break;
				case 231: /* Quest */
					receiver_must_upper = 0;
					receiver_short_length = 30;
					receiver_charset = CET_CHARSET_MS_ANSI;
					break;
				case 260: /* GPSMap 296 */
				default:
					break;
			}
			break;
		default:
			break;
			
	}
	if (global_opts.debug_level > 0)  {
		fprintf(stderr, "Waypoint type: %d\n"
			"Chosen waypoint length %d\n",
			 gps_waypt_type, receiver_short_length);
	if (gps_category_type) {
		fprintf(stderr, "Waypoint category type: %d\n",
		gps_category_type);
	}

	}
	/*
	 * If the user provided a short_length, override the calculated value.
	 */
	if (snlen)
		setshort_length(mkshort_handle, atoi(snlen));
	else
		setshort_length(mkshort_handle, receiver_short_length);

	if (snwhiteopt)
		setshort_whitespace_ok(mkshort_handle, atoi(snwhiteopt));

	/* 
	 * Until Garmins documents how to determine valid character space
	 * for the new models, we just release this safety check manually.
	 */
	if (receiver_must_upper) {
		setshort_goodchars(mkshort_handle, valid_waypt_chars);
	} else {
		setshort_badchars(mkshort_handle, "");
	}

	setshort_mustupper(mkshort_handle, receiver_must_upper);

	if (receiver_charset)
		cet_convert_init(receiver_charset, 1);
}

static void
rw_deinit(void)
{
	if (mkshort_handle) {
		mkshort_del_handle(&mkshort_handle);
	}
}

static int
waypt_read_cb(int total_ct, GPS_PWay *way)
{
	static int i;

	if (global_opts.verbose_status) {
		i++;
		waypt_status_disp(total_ct, i);
	}
	return 0;
}

static void
waypt_read(void)
{
	int i,n;
	GPS_PWay *way;

	if (getposn) {
		waypoint *wpt = waypt_new();
		wpt->latitude = gps_save_lat;
		wpt->longitude = gps_save_lon;
		wpt->shortname = xstrdup("Position");
		waypt_add(wpt);
		return;
	}

	if ((n = GPS_Command_Get_Waypoint(portname, &way, waypt_read_cb)) < 0) {
		fatal(MYNAME  ":Can't get waypoint from %s\n", portname);
	}

	for (i = 0; i < n; i++) {
		waypoint *wpt_tmp = waypt_new();

		wpt_tmp->shortname = xstrdup(way[i]->ident);
		wpt_tmp->description = xstrdup(way[i]->cmnt);
		rtrim(wpt_tmp->shortname);
		rtrim(wpt_tmp->description);
		wpt_tmp->longitude = way[i]->lon;
		wpt_tmp->latitude = way[i]->lat;
		if (gps_waypt_type == 103) {
			wpt_tmp->icon_descr = d103_symbol_from_icon_number(
					way[i]->smbl);
		} else {
			int dyn = 0;
			wpt_tmp->icon_descr = gt_find_desc_from_icon_number(
					way[i]->smbl, PCX, &dyn);
			wpt_tmp->wpt_flags.icon_descr_is_dynamic = dyn;
		}
		/*
		 * If a unit doesn't store altitude info (i.e. a D103)
		 * gpsmem will default the alt to INT_MAX.   Other units 
		 * (I can't recall if it was the V (D109) hor the Vista (D108) 
		 * return INT_MAX+1, contrary to the Garmin protocol doc which
		 * says they should report 1.0e25.   So we'll try to trap 
		 * all the cases here.     Yes, libjeeps should probably 
		 * do this and not us...
		 */
		if ((way[i]->alt == (float) (1U<<31)) || 
		     (way[i]->alt == INT_MAX) ||
		     (way[i]->alt >= (float) 1.0e20)
		     ) {
			wpt_tmp->altitude = unknown_alt;
		} else {
			wpt_tmp->altitude = way[i]->alt;
		}
		if (way[i]->time_populated) {
			wpt_tmp->creation_time = way[i]->time;
		}
#if SOON
		garmin_fs_garmin_after_read(way[i], wpt_tmp, gps_waypt_type);
#endif		
		waypt_add(wpt_tmp);
		GPS_Way_Del(&way[i]);
	}
}

static
void
track_read(void)
{
	int32 ntracks;
	GPS_PTrack *array;
	route_head *trk_head = NULL;
	int trk_num = 0;
	int i;
	int trk_seg_num = 1;
	char trk_seg_num_buf[10];
	char *trk_name = "";

	ntracks = GPS_Command_Get_Track(portname, &array, waypt_read_cb);

	if ( ntracks <= 0 )
		return;

	for(i = 0; i < ntracks; i++) {
		waypoint *wpt;

		/*
		 * This is probably always in slot zero, but the Garmin
		 * serial spec says these can appear anywhere.  Toss them
		 * out so we don't treat it as an extraneous trackpoint.
		 */ 	
		if (array[i]->ishdr) {
			trk_name = array[i]->trk_ident;
			if (!trk_name)
				trk_name = "";
			trk_seg_num = 1;
		}


		if ((trk_head == NULL) || array[i]->tnew) {
			trk_head = route_head_alloc();
			trk_head->rte_num = trk_num;
			if (trk_seg_num == 1) {
				trk_head->rte_name = xstrdup(trk_name);
			} else {
				/* name in the form TRACKNAME #n */
				snprintf(trk_seg_num_buf, sizeof(trk_seg_num_buf), "%d", trk_seg_num);
				trk_head->rte_name = xmalloc(strlen(trk_name)+strlen(trk_seg_num_buf)+3);
				sprintf(trk_head->rte_name, "%s #%s", trk_name, trk_seg_num_buf);
			}
			trk_seg_num++;
			trk_num++;
			track_add_head(trk_head);
		}

		if (array[i]->no_latlon || array[i]->ishdr) {
			continue;
		}
		wpt = waypt_new();

		wpt->longitude = array[i]->lon;
		wpt->latitude = array[i]->lat;
		wpt->altitude = array[i]->alt;
		wpt->heartrate = array[i]->heartrate;
		wpt->cadence = array[i]->cadence;
		wpt->shortname = xstrdup(array[i]->trk_ident);
		wpt->creation_time = array[i]->Time;
		
		track_add_wpt(trk_head, wpt);
	}

	while(ntracks) {
		GPS_Track_Del(&array[--ntracks]);
	}
	xfree(array);
}

static
void
route_read(void)
{
	int32 nroutepts;
	int i;
	GPS_PWay *array;
  route_head *rte_head;

	nroutepts = GPS_Command_Get_Route(portname, &array);

//	fprintf(stderr, "Routes %d\n", (int) nroutepts);
#if 1
	for (i = 0; i < nroutepts; i++) {
		if (array[i]->isrte) {
			char *csrc = NULL;
			/* What a horrible API has libjeeps for making this
			 * my problem.
			 */
			switch (array[i]->rte_prot) {
				case 201: csrc = array[i]->rte_cmnt; break;
				case 202: csrc = array[i]->rte_ident; break;
				default: break;
			}
      rte_head = route_head_alloc();
      route_add_head(rte_head);
      if (csrc) {
        rte_head->rte_name = xstrdup(csrc);
      }
		} else { 
			if (array[i]->islink)  {
				continue; 
			} else {
				waypoint *wpt_tmp = xcalloc(sizeof (*wpt_tmp), 1);
				wpt_tmp->latitude = array[i]->lat;
				wpt_tmp->longitude = array[i]->lon;
				wpt_tmp->shortname = array[i]->ident;
				route_add_wpt(rte_head, wpt_tmp);
			}
		}
	}
#else
	GPS_Fmt_Print_Route(array, nroutepts, stderr);
#endif

}

static void
data_read(void)
{
	if (poweroff) {
		return;
	}

	if (global_opts.masked_objective & WPTDATAMASK)
	  waypt_read();
	if (global_opts.masked_objective & TRKDATAMASK)
	  track_read();
	if (global_opts.masked_objective & RTEDATAMASK)
	  route_read();
	if (!(global_opts.masked_objective & 
	      (WPTDATAMASK | TRKDATAMASK | RTEDATAMASK)))
	  fatal(MYNAME ": Nothing to do.\n");
}

static GPS_PWay
sane_GPS_Way_New(void)
{
	GPS_PWay way;
	way = GPS_Way_New();
	if (!way) {
		fatal(MYNAME ":not enough memory\n");
	}

	/*
	 *  Undo less than helpful defaults from Way_New.
	 */
	way->rte_ident[0] = 0;
	way->rte_cmnt[0] = 0;
	way->rte_link_subclass[0] = 0;
	way->rte_link_ident[0] = 0;
	way->city[0] = 0;
	way->state[0] = 0;
	way->facility[0] = 0;
	way->addr[0] = 0;
	way->cross_road[0] = 0;
	way->cross_road[0] = 0;
	way->dpth = 1.0e25f;
	way->wpt_class = 0;

	return way;
}

static int 
waypt_write_cb(GPS_PWay *way)
{
	static int i;
	int n = waypt_count();

	if (global_opts.verbose_status) {
		i++;
		waypt_status_disp(n, i);
	}
	return 0;
}

/* 
 * If we're not using smart icons, try to put the cache info in the
 * description.
 */
const char *
get_gc_info(waypoint *wpt)
{
	if (global_opts.no_smart_icons) {
		if (wpt->gc_data.type == gt_virtual) return  "V ";
		if (wpt->gc_data.type == gt_unknown) return  "? ";
		if (wpt->gc_data.type == gt_multi) return  "Mlt ";
		if (wpt->gc_data.container == gc_micro) return  "M ";
		if (wpt->gc_data.container == gc_small) return  "S ";
	}
	return "";
}

static void
waypoint_write(void)
{
	int i;
	int32 ret;
	int n = waypt_count();
	queue *elem, *tmp;
	extern queue waypt_head;
	GPS_PWay *way;
	int icon;

	way = xcalloc(n,sizeof(*way));

	for (i = 0; i < n; i++) {
		way[i] = sane_GPS_Way_New();
	}

	i = 0;

	QUEUE_FOR_EACH(&waypt_head, elem, tmp) {
		waypoint *wpt;
		char *ident;
		char *src = NULL;
		char obuf[256];

		wpt = (waypoint *) elem;

		if(wpt->description) src = wpt->description;
		if(wpt->notes) src = wpt->notes;

		/* 
		 * mkshort will do collision detection and namespace 
		 * cleaning 
		 */
		ident = mkshort(mkshort_handle, 
			global_opts.synthesize_shortnames ? src : 
			wpt->shortname);
		/* Should not be a strcpy as 'ident' isn't really a C string,
		 * but rather a garmin "fixed length" buffer that's padded
		 * to the end with spaces.  So this is NOT (strlen+1).
		 */
		memcpy(way[i]->ident, ident, strlen(ident));

		if (global_opts.synthesize_shortnames) { 
			xfree(ident);
		}
		way[i]->ident[sizeof(way[i]->ident)-1] = 0;

		if (!global_opts.no_smart_icons && 
		     wpt->gc_data.diff && wpt->gc_data.terr) {
	                snprintf(obuf, sizeof(obuf), "%s%d/%d %s", 
					get_gc_info(wpt),
					wpt->gc_data.diff, wpt->gc_data.terr, 
					src);
			memcpy(way[i]->cmnt, obuf, strlen(obuf));
		} else  {
			memcpy(way[i]->cmnt, src, strlen(src));
		}
		way[i]->lon = wpt->longitude;
		way[i]->lat = wpt->latitude;

		if (deficon) {
			icon = gt_find_icon_number_from_desc(deficon, PCX);
		} else {
			if (get_cache_icon(wpt)) {
				icon = gt_find_icon_number_from_desc(get_cache_icon(wpt), PCX);
			} else {
				icon = gt_find_icon_number_from_desc(wpt->icon_descr, PCX);
			}
		}

		/* For units that support tiny numbers of waypoints, just
		 * overwrite that and go very literal.
		 */
		if (gps_waypt_type == 103) {
			icon = d103_icon_number_from_symbol(wpt->icon_descr);
		}
		way[i]->smbl = icon;
		if (wpt->altitude == unknown_alt) {
			way[i]->alt_is_unknown = 1;
			way[i]->alt = 0;
		} else {
			way[i]->alt = wpt->altitude;
		}
		if (wpt->creation_time) {
			way[i]->time = wpt->creation_time;
			way[i]->time_populated = 1;
		}
		if (category) {
			way[i]->category = 1 << atoi(category);
		}
#if SOON
		garmin_fs_garmin_before_write(wpt, way[i], gps_waypt_type);
#endif
		i++;
	}

	if ((ret = GPS_Command_Send_Waypoint(portname, way, n, waypt_write_cb)) < 0) {
		fatal(MYNAME ":communication error sending wayoints..\n");
	}

	for (i = 0; i < n; ++i) {
		GPS_Way_Del(&way[i]);
	}
	if (global_opts.verbose_status) {
		fprintf(stdout, "\r\n");
		fflush(stdout);
	}
	xfree(way);
}

static void
route_hdr_pr(const route_head *rte)
{
	(*cur_tx_routelist_entry)->rte_num = rte->rte_num;
	(*cur_tx_routelist_entry)->isrte = 1;
	if (rte->rte_name) {
		strncpy((*cur_tx_routelist_entry)->rte_ident, rte->rte_name, 
			sizeof ((*cur_tx_routelist_entry)->rte_ident));
	}
}

static void
route_waypt_pr(const waypoint *wpt)
{
	GPS_PWay rte = *cur_tx_routelist_entry;

	/*
	 * As stupid as this is, libjeeps seems to want an empty 
	 * waypoint between every link in a route that has nothing
	 * but the 'islink' member set.   Rather than "fixing" libjeeps,
	 * we just double them up (sigh) and do that here.
	 */
	rte->islink = 1;
	rte->lon = wpt->longitude;
	rte->lat = wpt->latitude;
	cur_tx_routelist_entry++;
	rte = *cur_tx_routelist_entry;

	rte->lon = wpt->longitude;
	rte->lat = wpt->latitude;
	rte->smbl = gt_find_icon_number_from_desc(wpt->icon_descr, PCX);
	if (wpt->altitude != unknown_alt) {
		rte->alt = wpt->altitude;
	}
	strncpy(rte->ident, wpt->shortname, sizeof(rte->ident));
	rte->ident[sizeof(rte->ident)-1] = 0;

	if (wpt->description) {
		strncpy(rte->cmnt, wpt->description, sizeof(rte->cmnt));
		rte->cmnt[sizeof(rte->ident)-1] = 0;
	} else  {
		rte->cmnt[0] = 0;
	}
	cur_tx_routelist_entry++;
}

static void
route_noop(const route_head *wp)
{
}

static void
route_write(void)
{
	int i;
	int n = 2 * route_waypt_count(); /* Doubled for the islink crap. */

	tx_routelist = xcalloc(n,sizeof(GPS_PWay));
	cur_tx_routelist_entry = tx_routelist;

	for (i = 0; i < n; i++) {
		tx_routelist[i] = sane_GPS_Way_New();
	}

	route_disp_all(route_hdr_pr, route_noop, route_waypt_pr);
	GPS_Command_Send_Route(portname, tx_routelist, n);
}

static void
track_hdr_pr(const route_head *trk_head)
{
	(*cur_tx_tracklist_entry)->tnew = gpsTrue;
	(*cur_tx_tracklist_entry)->ishdr = gpsTrue;
	if ( trk_head->rte_name ) {
		strncpy((*cur_tx_tracklist_entry)->trk_ident, trk_head->rte_name, sizeof((*cur_tx_tracklist_entry)->trk_ident));
		(*cur_tx_tracklist_entry)->trk_ident[sizeof((*cur_tx_tracklist_entry)->trk_ident)-1] = 0;
	}
	cur_tx_tracklist_entry++;
}

static void
track_waypt_pr(const waypoint *wpt)
{
	(*cur_tx_tracklist_entry)->lat = wpt->latitude;
	(*cur_tx_tracklist_entry)->lon = wpt->longitude;
	(*cur_tx_tracklist_entry)->alt = wpt->altitude;
	(*cur_tx_tracklist_entry)->Time = wpt->creation_time;
	if ( wpt->shortname ) {
		strncpy((*cur_tx_tracklist_entry)->trk_ident, wpt->shortname, sizeof((*cur_tx_tracklist_entry)->trk_ident));
		(*cur_tx_tracklist_entry)->trk_ident[sizeof((*cur_tx_tracklist_entry)->trk_ident)-1] = 0;
	}
	cur_tx_tracklist_entry++;
}

static void
track_write(void)
{
	int i;
	int n = track_waypt_count() + track_count();

	tx_tracklist = xcalloc(n, sizeof(GPS_PTrack));
	cur_tx_tracklist_entry = tx_tracklist;
	for (i = 0; i < n; i++) {
		tx_tracklist[i] = GPS_Track_New();
	}

	track_disp_all(track_hdr_pr, route_noop, track_waypt_pr);

	GPS_Command_Send_Track(portname, tx_tracklist, n);

	for (i = 0; i < n; i++) {
		GPS_Track_Del(&tx_tracklist[i]);
	}
	xfree(tx_tracklist);
}

static void
data_write(void)
{
	if (poweroff) {
		return;
	}

	if (global_opts.masked_objective & WPTDATAMASK)
	  waypoint_write();
	if (global_opts.masked_objective & RTEDATAMASK)
	  route_write();
	if (global_opts.masked_objective & TRKDATAMASK)
	  track_write();
}


ff_vecs_t garmin_vecs = {
	ff_type_serial,
	FF_CAP_RW_ALL,
	rw_init,
	rw_init,
	rw_deinit,
	rw_deinit,
	data_read,
	data_write,
	NULL,
	garmin_args,
	CET_CHARSET_ASCII, 0
};

static const char *d103_icons[16] = {
	"dot",
	"house",
	"gas",
	"car",
	"fish",
	"boat",
	"anchor",
	"wreck",
	"exit",
	"skull",
	"flag",
	"camp",
	"circle_x",
	"deer",
	"1st_aid",
	"back-track"
};

static const char *
d103_symbol_from_icon_number(unsigned int n)
{
	if (n  <= 15)
		return d103_icons[n];
	else
		return "unknown";
}

static int 
d103_icon_number_from_symbol(const char *s)
{
	unsigned int i;

	if (NULL == s) {
		return 0;
	}

	for (i = 0; i < sizeof(d103_icons) / sizeof(d103_icons[0]); i++) {
		if (0 == case_ignore_strcmp(s, d103_icons[i]))
			return i;
	}
	return 0;
}
