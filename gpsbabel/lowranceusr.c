/*
	Access to Lowrance USR files.
	Contributed to gpsbabel by Jason Rust (jrust at rustyparts.com)

	Copyright (C) 2005 Robert Lipe, robertlipe@usa.net

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

	HISTORY:

	6/21/05 - Ling Nero (rnlnero@yahoo.com)
	- Added Routes, Icons, & Tracks support
	- Fixed waypoint date/time stamp conversion
*/


#include "defs.h"
#include <string.h>
#include <math.h> /* for lat/lon conversion */

typedef struct lowranceusr_icon_mapping {
	const long int	value;
	const char		*icon;
} lowranceusr_icon_mapping_t;

#define DEF_ICON 10001

/* Taken from iFinder 1.8 */
const lowranceusr_icon_mapping_t lowranceusr_icon_value_table[] = {
	{ 10000, "diamond 1" },
	{ 10001, "diamond 2" },
	{ 10002, "diamond 3" },
	{ 10003, "x 1" },
	{ 10004, "x 2" },
	{ 10005, "x 3" },
	{ 10006, "cross" },
	{ 10007, "house" },
	{ 10008, "car" },
	{ 10009, "store" },
	{ 10010, "gas station" },
	{ 10011, "fork and spoon" },
	{ 10012, "telephone" },
	{ 10013, "airplane" },
	{ 10014, "exit sign" },
	{ 10015, "stop sign" },
	{ 10016, "exclamation" },
	{ 10017, "traffic light" },
	{ 10018, "american flag" },
	{ 10019, "person" },
	{ 10020, "restrooms" },
	{ 10021, "tree" },
	{ 10022, "mountains" },
	{ 10023, "campsite" },
	{ 10024, "picnic table" },
	{ 10025, "deer" },
	{ 10026, "deer tracks" },
	{ 10027, "turkey tracks" },
	{ 10028, "tree stand" },
	{ 10029, "bridge" },
	{ 10030, "skull and crossbones" },
	{ 10031, "fish" },
	{ 10032, "two fish" },
	{ 10033, "dive flag" },
	{ 10034, "wreck" },
	{ 10035, "anchor" },
	{ 10036, "boat" },
	{ 10037, "boat ramp" },
	{ 10038, "flag buoy" },
	{ 10039, "dam" },
	{ 10040, "swimmer" },
	{ 10041, "pier"},

	{ 10038, "Micro-Cache" },   	/* icon for "flag buoy" */
	{ 10030, "Virtual cache" }, 	/* icon for "skull and crossbones" */
	{ 10032, "Multi-Cache" },   	/* icon for "two fish" */
	{ 10003, "Unknown Cache" },   	/* icon for "x 1" */
	{ 10018, "Locationless (Reverse) Cache" }, /* Icon for "american flag" */
	{ 10007, "Post Office" },  	/* Icon for "house" */
	{ 10019, "Event Cache" }, 	/* Icon for "person" */
	{ 10020, "Webcam Cache" }, 	/* Icon for "restrooms" */

/* The following list is from TopoFusion */

	{ 10000, "Waypoint" },		/* diamond 1 */
	{ DEF_ICON, "Text Label (No Dot)" },
	{ 10018, "Trailhead" },		/* american flag */
	{ 10023, "Campground" },	/* campsite */
	{ 10022, "Summit" },		/* mountains */
	{ DEF_ICON, "Tall Tower" },
	{ DEF_ICON, "Short Tower" },
	{ 10021, "Forest" },		/* tree */
	{ DEF_ICON, "Mine" },
	{ 10038, "Geocache" },		/* flag buoy */
	{ 10016, "Geocache Found" },	/* exclamation */
	{ DEF_ICON, "Skiing Area" },
	{ 10029, "Crossing" },		/* bridge */
	{ 10007, "House" },			/* house */
	{ 10003, "Dot" },			/* x 1 */
	{ 10025, "Hunting Area" },	/* deer */
	{ 10031, "Fishing Area" },	/* fish */
	{ 10040, "Swimming Area" },	/* swimmer */
	{ 10012, "Telephone" },		/* telephone */
	{ 10024, "Rest Area" },		/* picnic table */
	{ 10021, "Park" },			/* tree */
	{ 10007, "Information" },	/* house */
	{ 10022, "Scenic Area" },	/* mountains */
	{ DEF_ICON, "Bank/Dollar" },
	{ 10009, "Hotel" },			/* store */
	{ 10011, "Restaurant" },	/* fork and spoon */
	{ 10030, "Danger Area" },	/* skull and crossbones */
	{ 10035, "Anchor" },		/* anchor */
	{ 10002, "City (Large)" },	/* diamond 3 */
	{ 10001, "City (Medium)" },	/* diamond 2 */
	{ 10000, "City (Small)" },	/* diamond 1 */
	{ DEF_ICON, "Drinking Water" },
	{ 10008, "Parking Area" },	/* car */
	{ 10023, "RV Park" },		/* campsite */
	{ 10020, "Rest Room" },		/* restroom */
	{ 10019, "Shower" },		/* person */
	{ DEF_ICON, "Tunnel" },

	{	 -1, NULL }
};

static FILE *file_in;
static FILE *file_out;
static void *mkshort_handle;

static unsigned short waypt_out_count;
static unsigned int trail_count, lowrance_route_count;
static int trail_point_count;
static int continuous = 1;
static short num_section_points;
static route_head *trk_head;
static route_head *rte_head;
static char *ignoreicons;
static char *merge;
static char *seg_break;

#define MYNAME "Lowrance USR"

#define MAXUSRSTRINGSIZE	256
#define SEMIMINOR		   6356752.3142
#define DEGREESTORADIANS	0.017453292
#define SECSTO2000			946713600
#define MAX_TRAIL_POINTS 9999

static
size_t
my_fwrite4(int *ptr, FILE *stream)
{
        int i = le_read32(ptr);
        return fwrite(&i, 4, 1, stream);
}

static
size_t
my_fwrite2(short *ptr, FILE *stream)
{
	short i = le_read16(ptr);
	return fwrite(&i, 2, 1, stream);
}

const char *
lowranceusr_find_desc_from_icon_number(const int icon)
{
	const lowranceusr_icon_mapping_t *i;

	for (i = lowranceusr_icon_value_table; i->icon; i++) {
		if (icon == i->value) {
			return i->icon;
		}
	}

	return "";
}

long int
lowranceusr_find_icon_number_from_desc(const char *desc)
{
	const lowranceusr_icon_mapping_t *i;

	if (!desc) {
		return DEF_ICON;
	}

	for (i = lowranceusr_icon_value_table; i->icon; i++) {
		if (case_ignore_strcmp(desc,i->icon) == 0) {
			return i->value;
		}
	}

	return DEF_ICON;
}
static int
lowranceusr_fread(void *buff, size_t size, size_t members, FILE * fp) 
{
	size_t br;

	br = fread(buff, size, members, fp);

	if (br != members) {
		fatal(MYNAME ": requested to read %d bytes, read %d bytes.\n", members, br);
	}

	return (br);
}

static
arglist_t lowranceusr_args[] = {
	{"ignoreicons", &ignoreicons, "Ignore event marker icons",
	 NULL, ARGTYPE_BOOL },
	{"merge", &merge, "(USR output) Merge into one segmented track",
	NULL, ARGTYPE_BOOL },
	{"break", &seg_break, "(USR input) Break segments into separate tracks",
	NULL, ARGTYPE_BOOL }
};

static void
rd_init(const char *fname)
{
	file_in = xfopen(fname, "rb", MYNAME);
}

static void
rd_deinit(void)
{
	fclose(file_in);
}

static void
wr_init(const char *fname)
{
	file_out = xfopen(fname, "wb", MYNAME);
	mkshort_handle = mkshort_new_handle();
	waypt_out_count = 0;
}

static void
wr_deinit(void)
{
	fclose(file_out);
	mkshort_del_handle(mkshort_handle);
}

/**
 * Latitude and longitude for USR coords are in the lowrance mercator meter
 * format in WGS84.  The below code converts them to degrees.
 */
static double 
lon_mm_to_deg(double x) {
	return x / (DEGREESTORADIANS * SEMIMINOR);
}

static long 
lon_deg_to_mm(double x) {
	return (long)(x * SEMIMINOR * DEGREESTORADIANS);
}

static double 
lat_mm_to_deg(double x) {
	return (2 * atan(exp(x / SEMIMINOR)) - M_PI / 2) / DEGREESTORADIANS;
}

static long
lat_deg_to_mm(double x) {
	return (long)(SEMIMINOR * log(tan((x * DEGREESTORADIANS + M_PI / 2) / 2)));
}

static void
lowranceusr_parse_waypt(waypoint *wpt_tmp)
{
	char buff[MAXUSRSTRINGSIZE + 1];
	long int TextLen;
	time_t waypt_time;
	short waypt_type;

	lowranceusr_fread(&buff[0], 4, 1, file_in);
	wpt_tmp->latitude = lat_mm_to_deg(le_read32(&buff[0]));
	lowranceusr_fread(&buff[0], 4, 1, file_in);
	wpt_tmp->longitude = lon_mm_to_deg(le_read32(&buff[0]));
	lowranceusr_fread(&buff[0], 4, 1, file_in);
	wpt_tmp->altitude = le_read32(&buff[0]);
	lowranceusr_fread(&buff[0], 4, 1, file_in);
	TextLen = buff[0];
	lowranceusr_fread(&buff[0], TextLen, 1, file_in);
	buff[TextLen] = '\0';
	wpt_tmp->shortname = xstrdup(buff);

    if (global_opts.debug_level >= 1)
	printf("LOWRANCE parse_waypt: Waypt name = %s Lat = %f Lon = %f alt = %f\n",wpt_tmp->shortname, wpt_tmp->latitude,
			wpt_tmp->longitude, wpt_tmp->altitude);

	lowranceusr_fread(&buff[0], 4, 1, file_in);
	TextLen = buff[0];
	if (TextLen) {
		lowranceusr_fread(&buff[0], TextLen, 1, file_in);
		buff[TextLen] = '\0';
		wpt_tmp->description = xstrdup(buff);
	}
	lowranceusr_fread(&buff[0], 4, 1, file_in);
	/* Time is number of seconds since Jan. 1, 2000 */
	waypt_time = le_read32(&buff[0]);
	if (waypt_time)
		wpt_tmp->creation_time = SECSTO2000 + waypt_time;

    if (global_opts.debug_level >= 2)
	{
		struct tm *tm;
		char tstr[20];

		tm = gmtime(&(wpt_tmp->creation_time));
		if (tm)
		{
			sprintf(tstr,"%d/%d/%d %d:%d:%d\n",tm->tm_mon+1, tm->tm_mday,
				tm->tm_year, tm->tm_hour, tm->tm_min, tm->tm_sec);
			printf("LOWRANCE parse_waypt: creation_time (GMT) %s\n", tstr);
			printf("LOWRANCE parse_waypt: creation time (local) %s\n", 
				ctime(&(wpt_tmp->creation_time)));
			printf("LOWRANCE parse_waypt: waypt time (local) %s\n", ctime(&waypt_time));
		}
	}

	/* Symbol ID */
	lowranceusr_fread(&buff[0], 4, 1, file_in);
	wpt_tmp->icon_descr = lowranceusr_find_desc_from_icon_number(le_read32(&buff[0]));

	/* Waypoint Type (USER, TEMPORARY, POINT_OF_INTEREST) */
	lowranceusr_fread(&buff[0], 2, 1, file_in);
	waypt_type = le_read16(&buff[0]);
    if (global_opts.debug_level >= 1)
		printf("LOWRANCEUSR parse_waypt: waypt_type = %d\n",waypt_type);

}



static void
lowranceusr_parse_routes(void)
{
	char buff[MAXUSRSTRINGSIZE + 1];
	short int num_routes, num_legs;
	int i,j;
	long int text_len;
	waypoint *wpt_tmp;

	lowranceusr_fread(&buff[0], 2, 1, file_in);
	num_routes = le_read16(&buff[0]);

    if (global_opts.debug_level >= 1)
		printf("LOWRANCAE parse_routes: Num Routes = %d\n", num_routes);

	for (i=0; i < num_routes; i++)
	{
		rte_head = route_head_alloc();
		route_add_head(rte_head);
		rte_head->rte_num = i+1;

		/* route name */
		lowranceusr_fread(&buff[0], 4, 1, file_in);
		text_len = buff[0];
		if (text_len)
		{
			lowranceusr_fread(&buff[0], text_len, 1, file_in);
			buff[text_len] = '\0';
		}
		rte_head->rte_name = xstrdup(buff);
		rte_head->rte_desc = '\0';

		/* num Legs */
		lowranceusr_fread(&buff[0], 2, 1, file_in);
		num_legs = le_read16(&buff[0]);

		/* route reversed */
		lowranceusr_fread(&buff[0], 1, 1, file_in);

		/* waypoints */
		for (j=0; j < num_legs; j++)
		{
			wpt_tmp = waypt_new();
			lowranceusr_parse_waypt(wpt_tmp);
			route_add_wpt(rte_head, wpt_tmp);
		}
	}
}

/*
 * Icons are automatically converted to waypoints unless
 * option of ignoreicons is used
 */
static void
lowranceusr_parse_icons(void)
{
	char buff[MAXUSRSTRINGSIZE + 1];
	short int num_icons;
	int i;

	lowranceusr_fread(&buff[0], 2, 1, file_in);
	num_icons = le_read16(&buff[0]);

    if (global_opts.debug_level >= 1)
		printf("LOWRANCE parse_icons: num Icons = %d\n", num_icons);

	for (i=0; i < num_icons; i++)
	{
		if (ignoreicons)
		{
			/* position coord lat & long */
			lowranceusr_fread(&buff[0], 4, 2, file_in);
			/* symbol */
			lowranceusr_fread(&buff[0], 4, 1, file_in);
		}
		else
		{
			waypoint *wpt_tmp;
			wpt_tmp = waypt_new();

			/* position coord lat & long */
			lowranceusr_fread(&buff[0], 4, 1, file_in);
			wpt_tmp->latitude = lat_mm_to_deg(le_read32(&buff[0]));
			lowranceusr_fread(&buff[0], 4, 1, file_in);
			wpt_tmp->longitude = lon_mm_to_deg(le_read32(&buff[0]));
			wpt_tmp->altitude = 0;
			sprintf(buff,"Icon %d", i+1);
			wpt_tmp->shortname = xstrdup(buff);
			/* symbol */
			lowranceusr_fread(&buff[0], 4, 1, file_in);
			wpt_tmp->icon_descr = lowranceusr_find_desc_from_icon_number(le_read32(&buff[0]));
			waypt_add(wpt_tmp);
		}
	}

}

static void
lowranceusr_parse_trails(void)
{
	char buff[MAXUSRSTRINGSIZE + 1];
	short int num_trails, num_trail_points, num_section_points;
	int i,j, trk_num;
	long int text_len;
	waypoint *wpt_tmp;
	route_head *trk_tmp;

	/* num trails */
	lowranceusr_fread(&buff[0], 2, 1, file_in);
	num_trails = le_read16(&buff[0]);

    if (global_opts.debug_level >= 1)
		printf("LOWRANCE parse_trails: num trails = %d\n", num_trails);

	for (i=trk_num=0; i < num_trails; i++)
	{
		trk_head = route_head_alloc();
		trk_head->rte_num = ++trk_num;
		track_add_head(trk_head);

		/* trail name */
		lowranceusr_fread(&buff[0], 4, 1, file_in);
		text_len = buff[0];

if (global_opts.debug_level >= 1)
	printf("LOWRANCE parse_trails: name text len = %ld\n", text_len);

		if (text_len)
			lowranceusr_fread(&buff[0], text_len, 1, file_in);

		buff[text_len] = '\0';
		trk_head->rte_name = xstrdup(buff);
		trk_head->rte_desc = '\0';

if (global_opts.debug_level >= 1)
	printf("LOWRANCE parse_trails: trail name = %s\n", trk_head->rte_name);

		/* visible */
		lowranceusr_fread(&buff[0], 1, 1, file_in);
		/* num trail points */
		lowranceusr_fread(&buff[0], 2, 1, file_in);
		num_trail_points = le_read16(&buff[0]);

if (global_opts.debug_level >= 1)
	printf("LOWRANCE parse_trails: num trail points = %d\n", num_trail_points);

		/* max trail size */
		lowranceusr_fread(&buff[0], 2, 1, file_in);

if (global_opts.debug_level >= 1)
	printf("LOWRANCE parse_trails: max trail size = %d\n", le_read16(&buff[0]));

		if (num_trail_points)
		{
			
			while (num_trail_points)
			{
			/* num section points */
			lowranceusr_fread(&buff[0], 2, 1, file_in);
			num_section_points = le_read16(&buff[0]);

if (global_opts.debug_level >= 1)
	printf("LOWRANCE parse_trails: num section points = %d\n", num_section_points);

				for (j=0; j < num_section_points; j++, num_trail_points--)
				{
				wpt_tmp = waypt_new();
				lowranceusr_fread(&buff[0], 4, 1, file_in);
				wpt_tmp->latitude = lat_mm_to_deg(le_read32(&buff[0]));
				lowranceusr_fread(&buff[0], 4, 1, file_in);
				wpt_tmp->longitude = lon_mm_to_deg(le_read32(&buff[0]));
				/* continuous */
				lowranceusr_fread(&buff[0], 1, 1, file_in);
				if (!buff[0] && seg_break && j)
				{
					trk_tmp = route_head_alloc();
					trk_tmp->rte_num = ++trk_num;
					trk_tmp->rte_name = xstrdup(trk_head->rte_name);
					trk_tmp->rte_desc = '\0';
					track_add_head(trk_tmp);
					trk_head = trk_tmp;
				}
				route_add_wpt(trk_head, wpt_tmp);
			
if (global_opts.debug_level >= 1)
	printf("LOWRANCE parse_trails: Trail pt lat %f lon %f\n", wpt_tmp->latitude, wpt_tmp->longitude);
				}
			}
		}
		/* remove the trail since it's empty */
		else track_del_head(trk_head);
	}
}

static void
data_read(void)
{
	char buff[MAXUSRSTRINGSIZE + 1];
	short int NumWaypoints, MajorVersion, MinorVersion, object_num;
	int i;

	lowranceusr_fread(&buff[0], 2, 1, file_in);
	MajorVersion = le_read16(&buff[0]);
	lowranceusr_fread(&buff[0], 2, 1, file_in);
	MinorVersion = le_read16(&buff[0]);
	
	if (global_opts.debug_level >= 1)
		printf("LOWRANCE data_read: Major Version %d Minor Version %d\n", MajorVersion, MinorVersion);

	if (MajorVersion < 2) {
		fatal(MYNAME ": input file is from an old version of the USR file and is not supported\n");
	}

	lowranceusr_fread(&buff[0], 2, 1, file_in);
	NumWaypoints = le_read16(&buff[0]);

    if (global_opts.debug_level >= 1)
	printf("LOWRANCE data_read: Num waypoints %d\n", NumWaypoints);

	for (i = 0; i < NumWaypoints; i++) {
		waypoint *wpt_tmp;

		wpt_tmp = waypt_new();

		/* Object num */
		lowranceusr_fread(&buff[0], 2, 1, file_in);
		object_num = le_read16(&buff[0]);
		if (global_opts.debug_level >= 1)
			printf("LOWRANCE data_read: object_num = %d\n", object_num);

		/* waypoint */
		lowranceusr_parse_waypt(wpt_tmp);

		waypt_add(wpt_tmp);
	}

	lowranceusr_parse_routes();
	lowranceusr_parse_icons();
	lowranceusr_parse_trails();
}

static void
lowranceusr_waypt_disp(const waypoint *wpt)
{
	int TextLen, Lat, Lon, Time, SymbolId;
	short int WayptType;
	char *name;
	char *comment;
	int alt = wpt->altitude;

	Lat = lat_deg_to_mm(wpt->latitude);
	my_fwrite4(&Lat, file_out);
	Lon = lon_deg_to_mm(wpt->longitude);
	my_fwrite4(&Lon, file_out);
	my_fwrite4(&alt, file_out);

    if (global_opts.debug_level >= 1)
	printf("LOWRANCE waypt_disp: Lat = %d\nLon = %d\nAlt = %d\n",Lat, Lon, alt);

	/* Try and make sure we have a name */
	if ((! wpt->shortname) || global_opts.synthesize_shortnames) {
		if (wpt->description && global_opts.synthesize_shortnames) {
			name = mkshort_from_wpt(mkshort_handle, wpt);
		} else if (wpt->shortname) {
			name = xstrdup(wpt->shortname);
		} else if (wpt->description) {
			name = xstrdup(wpt->description);
		} else {
			name = xstrdup("");
		}
	} else {
		name = xstrdup(wpt->shortname);
	}

	TextLen = strlen(name);
	my_fwrite4(&TextLen, file_out);
	fwrite(name, 1, TextLen, file_out);

    if (global_opts.debug_level >= 1)
	printf("LOWRANCE waypt_disp: Waypt name = %s\n",name);

	xfree(name);

	/**
	 * Comments aren't used by the iFinder yet so they just take up space...
	 */
	if (0 && wpt->description && strcmp(wpt->description, wpt->shortname) != 0) {
		comment = xstrdup(wpt->description);
		TextLen = strlen(comment);
		my_fwrite4(&TextLen, file_out);
		fwrite(comment, 1, TextLen, file_out);
		xfree(comment);
	} else {
		TextLen = 0;
		my_fwrite4(&TextLen, file_out);
	}

	if (wpt->creation_time > SECSTO2000) {
		Time = wpt->creation_time - SECSTO2000;
	} else {
		Time = 0;
	}

    if (global_opts.debug_level >= 2)
	{
		time_t wpt_time = Time;
	printf("LOWRANCE waypt_disp: waypt time (local): %s\n",ctime(&wpt_time));
	}

	my_fwrite4(&Time, file_out);

	if (get_cache_icon(wpt) && wpt->icon_descr && (strcmp(wpt->icon_descr, "Geocache Found") != 0)) {
		SymbolId = lowranceusr_find_icon_number_from_desc(get_cache_icon(wpt));
	} else {
		SymbolId = lowranceusr_find_icon_number_from_desc(wpt->icon_descr);
	}

	my_fwrite4(&SymbolId, file_out);

	/* USER waypoint type */
	WayptType = 0;
	my_fwrite2(&WayptType, file_out);
}

static void
lowranceusr_waypt_pr(const waypoint *wpt)
{

	/* our personal waypoint counter */
	my_fwrite2((short *) &waypt_out_count, file_out);

    if (global_opts.debug_level >= 1)
	printf("LOWRANCE waypt_pr: waypoint #%d ",waypt_out_count);

	waypt_out_count++;

	lowranceusr_waypt_disp(wpt);
}

/*
 * Header format:
 *	short num_trails,
 *  int trail_name text length,
 *  char *trail_name,
 *  boolean visible,
 *  short num_trail_points,
 *  short max_trail_size,
 *  short num_section_points
 *	  == don't know how many max points per section so
 *	  == use num_trail_points for now
 *	  == Once this is known then the waypoints ought to be
 *	  == broken up into sections
 */
 
static void
lowranceusr_track_hdr(const route_head *trk)
{
	int text_len;
	char *name, tmp_name[20];
	short num_trail_points, max_trail_size;
	int visible=1;

	++trail_count;
	if (trk->rte_name) {
		name = xstrdup(trk->rte_name);
	} else if (trk->rte_desc) {
			name = xstrdup(trk->rte_desc);
	} else
	{
		tmp_name[0]='\0';
		sprintf(tmp_name, "Babel %d", trail_count);
		name = xstrdup(tmp_name);
	}

	text_len = strlen(name);
    if (global_opts.debug_level >= 1)
	printf("LOWRANCE track_hdr: trail name text len = %d\n", text_len);
	my_fwrite4(&text_len, file_out);

    if (global_opts.debug_level >= 1)
	printf("LOWRANCE track_hdr: trail name = %s\n", name);

	fwrite(name, 1, text_len, file_out);

	num_trail_points = (short) trk->rte_waypt_ct;
	max_trail_size = MAX_TRAIL_POINTS;
	if (num_trail_points > max_trail_size)
		num_trail_points = max_trail_size;
	num_section_points = num_trail_points;

    if (global_opts.debug_level >= 1)
	printf("LOWRANCE track_hdr: num_trail_points = %d\nmax_trail_size = %d\nnum_section_points = %d\n",
			num_trail_points, max_trail_size, num_section_points);

	fwrite(&visible, 1, 1, file_out);
	my_fwrite2(&num_trail_points, file_out);
	my_fwrite2(&max_trail_size, file_out);
	my_fwrite2(&num_section_points, file_out);
	xfree(name);
	trail_point_count=1;
}

static void
lowranceusr_route_hdr(const route_head *rte)
{
	int text_len;
	char *name, tmp_name[20];
	short num_legs;
	int route_reversed=0;

	/* route name */
	if (rte->rte_name) {
		name = xstrdup(rte->rte_name);
	} else if (rte->rte_desc) {
			name = xstrdup(rte->rte_desc);
	} else
	{
		tmp_name[0]='\0';
		sprintf(tmp_name, "Babel R%d", ++lowrance_route_count);
		name = xstrdup(tmp_name);
	}
	text_len = strlen(name);
	my_fwrite4(&text_len, file_out);
	fwrite(name, 1, text_len, file_out);

	/* num legs */
	num_legs = (short) rte->rte_waypt_ct;
	my_fwrite2(&num_legs, file_out);
	fwrite(&route_reversed, 1, 1, file_out);

    if (global_opts.debug_level >= 1)
		printf("LOWRANCE route_hdr: route name \"%s\" num_legs = %d\n",
			rte->rte_name, num_legs);

}

static void
lowranceusr_track_disp(const waypoint *wpt)
{
	int lat, lon;

	if (++trail_point_count <= MAX_TRAIL_POINTS)
	{
		lat = lat_deg_to_mm(wpt->latitude);
		lon = lon_deg_to_mm(wpt->longitude);

    if (global_opts.debug_level >= 1)
		printf("LOWRANCE track_disp: Trail point #%d lat = %d long = %d\n",trail_point_count, lat, lon);

		my_fwrite4(&lat, file_out);
		my_fwrite4(&lon, file_out);
		fwrite(&continuous, 1, 1, file_out);
		if (!continuous)
			continuous = 1;
	}
}

static void
lowranceusr_merge_track_hdr(const route_head *trk)
{
	int text_len;
	char *name, tmp_name[20];

	if (++trail_count == 1)
	{
		if (trk->rte_name) {
			name = xstrdup(trk->rte_name);
		} else if (trk->rte_desc) {
				name = xstrdup(trk->rte_desc);
		} else
		{
			tmp_name[0]='\0';
			sprintf(tmp_name, "Babel %d", trail_count);
			name = xstrdup(tmp_name);
		}
		text_len = strlen(name);
		my_fwrite4(&text_len, file_out);

		if (global_opts.debug_level >= 1)
			printf("LOWRANCE track_hdr: trail name = %s\n", name);

		fwrite(name, 1, text_len, file_out);
	}

	trail_point_count += (short) trk->rte_waypt_ct;
}

static void
lowranceusr_merge_track_tlr(const route_head *trk)
{
	short num_trail_points, max_trail_size;
	int visible=1;

	if (trail_count == track_count())	/* last trail */
	{
		num_trail_points = trail_point_count;
		max_trail_size = MAX_TRAIL_POINTS;
		if (num_trail_points > max_trail_size)
			num_trail_points = max_trail_size;
		num_section_points = num_trail_points;

    if (global_opts.debug_level >= 1)
	printf("LOWRANCE merge_track_tlr: num_trail_points = %d\nmax_trail_size = %d\nnum_section_points = %d\n",
			num_trail_points, max_trail_size, num_section_points);

		fwrite(&visible, 1, 1, file_out);
		my_fwrite2(&num_trail_points, file_out);
		my_fwrite2(&max_trail_size, file_out);
		my_fwrite2(&num_section_points, file_out);
	}
}
static void

lowranceusr_merge_track_hdr_2(const route_head *trk)
{
	continuous = 0;
}

static void
data_write(void)
{
	short int NumWaypoints, MajorVersion, MinorVersion, NumRoutes, NumTrails, NumIcons;
	setshort_length(mkshort_handle, 15);
	MajorVersion = 2;
	MinorVersion = 0;

	NumWaypoints = waypt_count();

	my_fwrite2(&MajorVersion, file_out);
	my_fwrite2(&MinorVersion, file_out);
	my_fwrite2(&NumWaypoints, file_out);

    if (global_opts.debug_level >= 1)
	printf("LOWRANCE data_write: Num waypoints = %d\n", NumWaypoints);

	if (NumWaypoints)
		waypt_disp_all(lowranceusr_waypt_pr);

	/* Route support added 6/21/05 */
	NumRoutes = route_count();
	my_fwrite2(&NumRoutes, file_out);

    if (global_opts.debug_level >= 1)
	printf("LOWRANCE data_write: Num routes = %d\n", NumRoutes);

	if (NumRoutes)
	{
		lowrance_route_count=0;
		route_disp_all(lowranceusr_route_hdr, NULL, lowranceusr_waypt_disp);
	}

	/* no support for Icons */
	NumIcons = 0;
	my_fwrite2(&NumIcons, file_out);
	
	/* Track support added 6/21/05 */
	NumTrails = track_count();

	if (NumTrails && merge)
	{
		NumTrails = 1;
		my_fwrite2(&NumTrails, file_out);
		trail_point_count = 0;
		trail_count = 0;
		/* count the number of total track points */
		track_disp_all(lowranceusr_merge_track_hdr, lowranceusr_merge_track_tlr, NULL);
		/* write out the new track header */
		trail_point_count = 0;
		track_disp_all(lowranceusr_merge_track_hdr_2, NULL, lowranceusr_track_disp);

	}
	else
	{
	
		my_fwrite2(&NumTrails, file_out);

		if (global_opts.debug_level >= 1)
		printf("LOWRANCE data_write: Num tracks = %d\n", NumTrails);

		if (NumTrails)
		{
			trail_count=0;
			track_disp_all(lowranceusr_track_hdr, NULL, lowranceusr_track_disp);
		}
	}
}


ff_vecs_t lowranceusr_vecs = {
	ff_type_file,
	FF_CAP_RW_WPT,
	rd_init,
	wr_init,
	rd_deinit,
	wr_deinit,
	data_read,
	data_write,
	NULL, 
	lowranceusr_args
};
