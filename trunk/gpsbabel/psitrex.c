/*
    Access to PsiTrex text files.
    Based on information provided by Ian Cowley.

    Copyright (C) 2003 Mark Bradley, mrcb.gpsb@osps.net

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

#include <stdio.h>
#include <string.h>

#include "defs.h"
#include "garmin_tables.h"
#include <ctype.h>

#define MYNAME "PSITREX" 

typedef enum {
	ltrimEOL = 1 ,		/* skip spaces & tabs to start; ends on EOL */
	EOL,				/* don't skip spaces and tabs to start; end on EOL */
	comma,				/* skip spaces & tabs to start; ends on comma or EOL */
	whitespace,			/* skip spaces & tabs to start; ends on white space or EOL */
	wscomma				/* skip spaces & tabs to start; ends on white space, comma or EOL */
} psit_tokenSep_type;

typedef struct psit_icon_mapping {
	const int	value;
	const char	*icon;
} psit_icon_mapping_t;

static FILE *psit_file_in;
static FILE *psit_file_out;
static void *mkshort_handle;

/* 2 = not written any tracks out
   1 = change of track to write out track header
   0 = in the middle of writing out track datapoints, so don't write a header */
static int	psit_track_state = 2;

static char psit_current_token[256];

char *snlen;

static
arglist_t psit_args[] = {
/*	{"snlen", &snlen, "Length of generated shortnames", ARGTYPE_INT }, */
	{0, 0, 0, 0}
};

/* Taken from PsiTrex 1.13 */
const psit_icon_mapping_t psit_icon_value_table[] = {
	{   0x00, "anchor" },
	{   0x06, "dollar" },
	{   0x07, "fish" },
	{   0x08, "fuel" },
	{   0x0a, "house" },
	{   0x0b, "knife" },
	{   0x0d, "mug" },
	{   0x0e, "skull" },
	{   0x12, "wpt_dot" },
	{   0x13, "wreck" },
	{   0x15, "mob" },
	{ 0x0096, "boat_ramp" },
	{ 0x0097, "camp" },
	{ 0x0098, "restrooms" },
	{ 0x0099, "showers" },
	{ 0x009a, "drinking_wtr" },
	{ 0x009b, "phone" },
	{ 0x009c, "1st_aid" },
	{ 0x009d, "info" },
	{ 0x009e, "parking" },
	{ 0x009f, "park" },
	{ 0x00a0, "picnic" },
	{ 0x00a1, "scenic" },
	{ 0x00a2, "skiing" },
	{ 0x00a3, "swimming" },
	{ 0x00a4, "dam" },
	{ 0x00a6, "danger" },
	{ 0x00a9, "ball" },
	{ 0x00aa, "car" },
	{ 0x00ab, "deer" },
	{ 0x00ac, "shpng_cart" },
	{ 0x00ad, "lodging" },
	{ 0x00ae, "mine" },
	{ 0x00af, "trail_head" },
	{ 0x00b0, "truck_stop" },
	{ 0x00b2, "flag" },
	{ 0x2005, "golf" },
	{ 0x2006, "sml_cty" },
	{ 0x2007, "med_cty" },
	{ 0x2008, "lrg_cty" },
	{ 0x200c, "amuse_pk" },
	{ 0x200d, "bowling" },
	{ 0x200e, "car_rental" },
	{ 0x200f, "car_repair" },
	{ 0x2010, "fastfood" },
	{ 0x2011, "fitness" },
	{ 0x2012, "movie" },
	{ 0x2013, "museum" },
	{ 0x2014, "pharmacy" },
	{ 0x2015, "pizza" },		/* how specific does this really need to be? C'mon! */
	{ 0x2016, "post_ofc" },
	{ 0x2017, "rv_park" },
	{ 0x2018, "school" },
	{ 0x2019, "stadium" },
	{ 0x201a, "store" },
	{ 0x201b, "zoo" },
	{ 0x201c, "gas_plus" },
	{ 0x201d, "faces" },
	{ 0x2022, "weigh_sttn" },
	{ 0x2023, "toll_booth" },
	{ 0x2029, "bridge" },
	{ 0x202a, "building" },
	{ 0x202b, "cemetery" },
	{ 0x202c, "church" },
	{ 0x202e, "crossing" },
	{ 0x2032, "oil_field" },
	{ 0x2033, "tunnel" },
	{ 0x2035, "forest" },
	{ 0x2036, "summit" },
	{ 0x203f, "geocache" },
	{ 0x2040, "geocache_fnd" },
	{ 0x4000, "airport" },
	{ 0x4007, "tall_tower" },
	{ 0x4008, "short_tower" },
	{ 0x4009, "glider" },
	{ 0x400a, "ultralight" },
	{ 0x400b, "parachute" },
	{ 0x4012, "seaplane" },
	{     -1, NULL }
};

const char *
psit_find_desc_from_icon_number(const int icon)
{
	const psit_icon_mapping_t *i;

	for (i = psit_icon_value_table; i->icon; i++) {
		if (icon == i->value) {
			return i->icon;
		}
	}
	return "";
}

int
psit_find_icon_number_from_desc(const char *desc)
{
	const psit_icon_mapping_t *i;
	int def_icon = 18;

	if (!desc) {
		return def_icon;
	}

	for (i = psit_icon_value_table; i->icon; i++) {
		if (case_ignore_strcmp(desc,i->icon) == 0) {
			return i->value;
		}
	}
	if (atoi(desc) > 0) return atoi(desc);
	return def_icon;
}

static void
psit_rd_init(const char *fname)
{
	psit_file_in = fopen(fname, "r");
	if (psit_file_in == NULL) {
		fatal(MYNAME ": '%s' for reading\n", fname);
	}
}

static void
psit_rd_deinit(void)
{
	fclose(psit_file_in);
}

static void
psit_wr_init(const char *fname)
{
	psit_file_out = fopen(fname, "w");
	if (psit_file_out == NULL) {
		fatal(MYNAME ": '%s' for writing\n", fname);
		exit(1);
	}
}

static void
psit_wr_deinit(void)
{
	fclose(psit_file_out);
}

/*
 * get characters until and including terminating NULL from psit_file_in 
 * and write into buf.
 */
static void
psit_readstr(FILE *psit_file, char *buf, size_t sz)
{
	int c;
	while (sz-- && (c = fgetc (psit_file)) != EOF) {
		*buf++ = c;
		if (c == 0)  {
			return;
		}
	}
}

/*
 * get characters until and including terminating NULL from psit_file_in 
 * and write into buf.
 */
static void
psit_getToken(FILE *psit_file, char *buf, size_t sz, psit_tokenSep_type delimType)
{
	int c;
char *buf2 = buf;	/* MRCB debug */

	*buf = 0;

	if (delimType != EOL) {
		while ((c = fgetc (psit_file)) != EOF) {
			if (!isspace(c)) break;
		}
	}

	if (feof(psit_file)) return;

	if (delimType == EOL) {
		c = fgetc (psit_file);
	}

	if (c == '#') {
		if (fgets(buf, sz, psit_file) == NULL) {
			*buf = 0;
			return;
		}
		/* use recursion to skip multiple comment lines or just to return the next token */
		psit_getToken(psit_file, buf, sz, delimType);
		return;
	}

	if ((delimType == EOL) || (delimType == ltrimEOL)) {
		*buf = c;
		buf++;
		fgets(buf, sz-1, psit_file);
		return;
	}

	while (sz--) {
		*buf++ = c;
		if ((c = fgetc (psit_file)) == EOF) {
			*buf = 0;
			return;
		}
		if (((c == 0) || isspace(c)) &&
			((delimType == whitespace) || (delimType == wscomma)) ) {
				*buf = 0;
				return;
		}
		if (((delimType == comma) || (delimType == wscomma)) &&
			(c == ',')) {
				*buf = 0;
				return;
		}
	}
}


/*
 * test if a token is known 
 * 
 */
static int
psit_isKnownToken(char *buf)
{
	if (strcmp(buf, "Track:") == 0) return 0;
	if (strcmp(buf, "Route:") == 0) return 0;
	if (strcmp(buf, "Waypoint:") == 0) return 0;
	if (strcmp(buf, "Map:") == 0) return 0;
	return 1;
}

/*
 * read in from file a waypoint record
 * MRCB
 */
static void
psit_waypoint_r(FILE *psit_file, waypoint **wpt)
{
	int		garmin_icon_num;

	waypoint	*thisWaypoint;
	double	psit_altitude = unknown_alt;
	double	psit_depth = unknown_alt;

	if (strlen(psit_current_token) > 0) {
		thisWaypoint = xcalloc(sizeof(*thisWaypoint), 1);

		thisWaypoint->position.latitude.degrees = atof(psit_current_token);

		psit_getToken(psit_file,psit_current_token,sizeof(psit_current_token), comma);
		thisWaypoint->position.longitude.degrees = atof(psit_current_token);

		psit_getToken(psit_file,psit_current_token,sizeof(psit_current_token), comma);
		if (psit_current_token[0] == '*') {
			thisWaypoint->position.altitude.altitude_meters = unknown_alt;
		}
		else {
			thisWaypoint->position.altitude.altitude_meters = atof(psit_current_token);
		}

		/* the name */
		psit_getToken(psit_file,psit_current_token,sizeof(psit_current_token), comma);
		rtrim(psit_current_token);
		thisWaypoint->shortname = xstrdup(psit_current_token);
		thisWaypoint->description = xstrdup("");

		psit_getToken(psit_file,psit_current_token,sizeof(psit_current_token), ltrimEOL);
		rtrim(psit_current_token);
		/* since PsiTrex only deals with Garmins, let's use the "proper" Garmin icon name */
		/* convert the PsiTrex name to the number, which is the PCX one; from there to Garmin desc */
		garmin_icon_num = psit_find_icon_number_from_desc(psit_current_token);
		thisWaypoint->icon_descr = mps_find_desc_from_icon_number(garmin_icon_num, PCX);

		waypt_add(thisWaypoint);

		psit_getToken(psit_file,psit_current_token,sizeof(psit_current_token), wscomma);
	}
}

/*
 * write out to file a waypoint record
 * MRCB
 */
static void
psit_waypoint_w(FILE *psit_file, const waypoint *wpt)
{
	int	icon;
	char *src;
	const char *ident;
	int display = 1;
	int colour = 0;			/*  (unknown colour) black is 1, white is 16 */

	fprintf(psit_file, "%11.6f,%11.6f,", 
						wpt->position.latitude.degrees,
						wpt->position.longitude.degrees);

	if (wpt->position.altitude.altitude_meters == unknown_alt) 
		fprintf(psit_file, "********,");
	else
		fprintf(psit_file, "%8.2f,",
						wpt->position.altitude.altitude_meters);

	ident = global_opts.synthesize_shortnames ?
				mkshort(mkshort_handle, src) :
				wpt->shortname;

	fprintf(psit_file, " %-6s, ", ident);
	icon = mps_find_icon_number_from_desc(wpt->icon_descr, PCX);

	if (get_cache_icon(wpt) && wpt->icon_descr && (strcmp(wpt->icon_descr, "Geocache Found") != 0)) {
		icon = mps_find_icon_number_from_desc(get_cache_icon(wpt), PCX);
	}

	ident = psit_find_desc_from_icon_number(icon);
	if (strlen(ident) == 0)
		fprintf(psit_file, "%1d\n", icon);
	else
		fprintf(psit_file, "%s\n", ident);

}

static void
psit_waypoint_w_wrapper(const waypoint *wpt)
{
	psit_waypoint_w(psit_file_out, wpt);
}

/*
 * read in from file a route record
 * MRCB
 */
static void
psit_route_r(FILE *psit_file, route_head **rte)
{
	char rtename[256];
	unsigned int rte_num;

	int		garmin_icon_num;

	time_t	dateTime = 0;
	route_head *rte_head;
	unsigned int rte_count;

	waypoint	*thisWaypoint;
	double	psit_altitude = unknown_alt;
	double	psit_depth = unknown_alt;

	psit_getToken(psit_file,psit_current_token,sizeof(psit_current_token), ltrimEOL);

	if (strlen(psit_current_token) == 0) {
		strcpy(rtename, "ROUTE");
	}
	else {
		strcpy(rtename, psit_current_token);
	}

	rtrim(rtename);

	rte_head = route_head_alloc();
	rte_head->rte_name = xstrdup(rtename);
	route_add_head(rte_head);
	*rte = rte_head;

	rte_num = 0;

	rte_count = 0;

	psit_getToken(psit_file,psit_current_token,sizeof(psit_current_token), wscomma);

	while (psit_isKnownToken(psit_current_token) != 0) {
		if (strlen(psit_current_token) > 0) {
			thisWaypoint = xcalloc(sizeof(*thisWaypoint), 1);

			thisWaypoint->position.latitude.degrees = atof(psit_current_token);

			psit_getToken(psit_file,psit_current_token,sizeof(psit_current_token), comma);
			thisWaypoint->position.longitude.degrees = atof(psit_current_token);

			psit_getToken(psit_file,psit_current_token,sizeof(psit_current_token), comma);
			if (psit_current_token[0] == '*') {
				thisWaypoint->position.altitude.altitude_meters = unknown_alt;
			}
			else {
				thisWaypoint->position.altitude.altitude_meters = atof(psit_current_token);
			}

			/* the name */
			psit_getToken(psit_file,psit_current_token,sizeof(psit_current_token), comma);
			rtrim(psit_current_token);
			thisWaypoint->shortname = xstrdup(psit_current_token);
			thisWaypoint->description = xstrdup("");

			psit_getToken(psit_file,psit_current_token,sizeof(psit_current_token), ltrimEOL);
			rtrim(psit_current_token);
			/* since PsiTrex only deals with Garmins, let's use the "proper" Garmin icon name */
			/* convert the PsiTrex name to the number, which is the PCX one; from there to Garmin desc */
			garmin_icon_num = psit_find_icon_number_from_desc(psit_current_token);
			thisWaypoint->icon_descr = mps_find_desc_from_icon_number(garmin_icon_num, PCX);

			route_add_wpt(rte_head, thisWaypoint);

			if (feof(psit_file)) break;

			psit_getToken(psit_file,psit_current_token,sizeof(psit_current_token), wscomma);
		}
		else break;
	}
}

/*
 * write out to file a route header
 * MRCB
 */
static void
psit_routehdr_w(FILE *psit_file, const route_head *rte)
{
	char		hdr[20];
	unsigned int rte_datapoints;
	char		*rname;

	waypoint	*testwpt;
	time_t		uniqueValue;
	int			allWptNameLengths;

	queue *elem, *tmp;

	/* total nodes (waypoints) this route */
	rte_datapoints = 0;
	allWptNameLengths = 0;

	if (rte->waypoint_list.next) {		/* this test doesn't do what I want i.e test if this is a valid route - treat as a placeholder for now */
		QUEUE_FOR_EACH(&rte->waypoint_list, elem, tmp) {
			testwpt = (waypoint *)elem;
			if (rte_datapoints == 0) {
				uniqueValue = testwpt->creation_time;
			}
			rte_datapoints++;
		}		

		if (uniqueValue == 0) {
			uniqueValue = time(NULL);
		}

		/* route name */
		if (!rte->rte_name) {
			sprintf(hdr, "Route%04x", uniqueValue);
			rname = xstrdup(hdr);
		}
		else
			rname = xstrdup(rte->rte_name);

		fprintf(psit_file, "Route:  %s\n",
							rname);
	}
}

static void
psit_routehdr_w_wrapper(const route_head *rte)
{
	psit_routehdr_w(psit_file_out, rte);
}


/*
 * read in from file a track record
 * MRCB
 */
static void
psit_track_r(FILE *psit_file, route_head **trk)
{
	char tbuf[100];
	char trkname[256];
	unsigned int trk_num;

	struct tm tmTime;
	time_t	dateTime = 0;
	route_head *track_head;
	unsigned int trk_count;

	waypoint	*thisWaypoint;
	double	psit_altitude = unknown_alt;
	double	psit_depth = unknown_alt;

	psit_getToken(psit_file,psit_current_token,sizeof(psit_current_token), ltrimEOL);
	if (strlen(psit_current_token) == 0) {
		strcpy(trkname, "TRACK");
	}
	else {
		strcpy(trkname, psit_current_token);
	}

	rtrim(trkname);

	trk_num = 0;

	trk_count = 0;

	psit_getToken(psit_file,psit_current_token,sizeof(psit_current_token), wscomma);

	while (psit_isKnownToken(psit_current_token) != 0) {
		if (strlen(psit_current_token) > 0) {
			thisWaypoint = xcalloc(sizeof(*thisWaypoint), 1);

			thisWaypoint->position.latitude.degrees = atof(psit_current_token);

			psit_getToken(psit_file,psit_current_token,sizeof(psit_current_token), comma);
			thisWaypoint->position.longitude.degrees = atof(psit_current_token);

			psit_getToken(psit_file,psit_current_token,sizeof(psit_current_token), comma);
			if (psit_current_token[0] == '*') {
				thisWaypoint->position.altitude.altitude_meters = unknown_alt;
			}
			else {
				thisWaypoint->position.altitude.altitude_meters = atof(psit_current_token);
			}

			/* date portion of the date time DD/MM/YY */
			psit_getToken(psit_file, psit_current_token,
					sizeof(psit_current_token), whitespace);
			sscanf(psit_current_token, "%02d/%02d/%02d", 
				&(tmTime.tm_mday) , &(tmTime.tm_mon), 
				&(tmTime.tm_year));

			/* years are less 1900 in the tm struct */
			tmTime.tm_year += (tmTime.tm_year > 50 ? 0 : 100);		
			/* months are 0 to 11 in the tm struct */
			tmTime.tm_mon--;										
			/* time portion of the date time hh:mm:ss */
			psit_getToken(psit_file,psit_current_token,
					sizeof(psit_current_token), wscomma);
			sscanf(psit_current_token, "%02d:%02d:%02d", 
					&(tmTime.tm_hour) , &(tmTime.tm_min), 
					&(tmTime.tm_sec));

			tmTime.tm_isdst = 0;
			dateTime = mktime(&tmTime) + get_tz_offset();

			psit_getToken(psit_file,psit_current_token,sizeof(psit_current_token), whitespace);

			if (strcmp(psit_current_token, "1") == 0) {
				track_head = route_head_alloc();
				/* Add a number to the track name.  With Garmins, the "first" tracklog is usually ACTIVE LOG
				   the second is ACTIVE LOG001 and so on */
				if (trk_num > 0) {
					sprintf(tbuf, "%s%03d", trkname, trk_num);
					track_head->rte_name = xstrdup(tbuf);
				}
				else {
					track_head->rte_name = xstrdup(trkname);
				}
				trk_num++;
				route_add_head(track_head);
			}

			thisWaypoint->creation_time = dateTime;
			thisWaypoint->centiseconds = 0;
			route_add_wpt(track_head, thisWaypoint);

			if (feof(psit_file)) break;

			psit_getToken(psit_file,psit_current_token,sizeof(psit_current_token), wscomma);
		}
		else break;
	}
}

/*
 * write out to file a tracklog header
 * MRCB
 */
static void
psit_trackhdr_w(FILE *psit_file, const route_head *trk)
{
	char		hdr[30];
	unsigned int trk_datapoints;
	char		*tname;
	waypoint	*testwpt;
	time_t		uniqueValue;

	queue *elem, *tmp;

	if (psit_track_state == 2) {
		/* total nodes (waypoints) this track */
		trk_datapoints = 0;
		if (trk->waypoint_list.next) {	/* this test doesn't do what I want i.e test if this is a valid track - treat as a placeholder for now */
			QUEUE_FOR_EACH(&trk->waypoint_list, elem, tmp) {
				if (trk_datapoints == 0) {
					testwpt = (waypoint *)elem;
					uniqueValue = testwpt->creation_time;
				}
				trk_datapoints++;
			}

			if (uniqueValue == 0) {
				uniqueValue = time(NULL);
			}

			/* track name */
			if (!trk->rte_name) {
				sprintf(hdr, "Track%04x", uniqueValue);
				tname = xstrdup(hdr);
			}
			else
				tname = xstrdup(trk->rte_name);

			fprintf (psit_file, "Track:  %s\n",
								tname);

		}
	}
	psit_track_state = 1;
}

static void
psit_trackhdr_w_wrapper(const route_head *trk)
{
	psit_trackhdr_w(psit_file_out, trk);
}


/*
 * write out to file a tracklog datapoint
 * MRCB
 */
static void
psit_trackdatapoint_w(FILE *psit_file, const waypoint *wpt)
{
	time_t	t = wpt->creation_time;
	struct tm *tmTime = gmtime(&t);

	double	psit_altitude = wpt->position.altitude.altitude_meters;
	double	psit_proximity = unknown_alt;
	double	psit_depth = unknown_alt;

	fprintf(psit_file, "%11.6f,%11.6f,", 
						wpt->position.latitude.degrees,
						wpt->position.longitude.degrees);

	if (wpt->position.altitude.altitude_meters == unknown_alt) 
		fprintf(psit_file, "********, ");
	else
		fprintf(psit_file, "%8.2f, ",
						wpt->position.altitude.altitude_meters);

	/* Following date time format is fixed and reveals the origin of PsiTrex (i.e. the UK) */
	fprintf(psit_file, "%02d/%02d/%02d %02d:%02d:%02d,",
						tmTime->tm_mday,
						tmTime->tm_mon+1,
						tmTime->tm_year % 100,
						tmTime->tm_hour,
						tmTime->tm_min,
						tmTime->tm_sec);

	fprintf(psit_file," %d\n", psit_track_state);
	psit_track_state = 0;
}

static void
psit_trackdatapoint_w_wrapper(const waypoint *wpt)
{
	psit_trackdatapoint_w(psit_file_out, wpt);
}


static void
psit_read(void)
{
	waypoint	*wpt;
	route_head	*rte;
	route_head	*trk;

#ifdef DUMP_ICON_TABLE
	printf("static icon_mapping_t icon_table[] = {\n");
#endif

	psit_getToken(psit_file_in, psit_current_token, sizeof(psit_current_token), whitespace);

	do {
		if (strlen(psit_current_token) == 0) break;

		if (strcmp(psit_current_token, "Track:") == 0) {
			if (global_opts.objective == trkdata) {
				psit_track_r(psit_file_in, &trk);
			}
			else break;		/* psitrex files only have one format in; */
							/* if this is a track file and we don't want them, them bail out */
		}
		else if (strcmp(psit_current_token, "Route:") == 0) {
			if (global_opts.objective == rtedata) {
				psit_route_r(psit_file_in, &rte);
			}
			else break;		/* ditto, but for routes */
		}
		else {
			/* Must be waypoints in this file */
			if (global_opts.objective == wptdata) {
				psit_waypoint_r(psit_file_in, &wpt);
#ifdef DUMP_ICON_TABLE
				printf("\t{  %4u, \"%s\" },\n", icon, wpt->shortname);
#endif

			}
			else break;
		}
	} while (!feof(psit_file_in));

	return;

#ifdef DUMP_ICON_TABLE
		printf("\t{ -1, NULL },\n");
		printf("};\n");
#endif
}

static void 
psit_noop(const route_head *wp)
{
	/* no-op */
}

void
psit_write(void)
{
	int short_length;

	if (snlen)
		short_length = atoi(snlen);
	else
		short_length = 10;

	mkshort_handle = mkshort_new_handle();

	setshort_length(mkshort_handle, short_length);
	setshort_whitespace_ok(mkshort_handle, 0);

	psit_track_state = 2;

	if (global_opts.objective == wptdata) {
		waypt_disp_all(psit_waypoint_w_wrapper);
	}
	if (global_opts.objective == rtedata) {
		route_disp_all(psit_routehdr_w_wrapper, psit_noop, psit_waypoint_w_wrapper);
	}
	if (global_opts.objective == trkdata) {
		route_disp_all(psit_trackhdr_w_wrapper, psit_noop, psit_trackdatapoint_w_wrapper);
	}

	mkshort_del_handle(mkshort_handle);

}

ff_vecs_t psit_vecs = {
	psit_rd_init,
	psit_wr_init,
	psit_rd_deinit,
	psit_wr_deinit,
	psit_read,
	psit_write,
	psit_args
};
